GetYHat <- function(survey, joint, joint_inter=NULL, model_directory, output_prefix, laus, county_target=NULL, state_target=NULL, demo_target=NULL, starting_model=NULL) {

  # laus adjustment
  if (is.null(starting_model)) {
    message("  ", survey, ": laus adjustment")
    fips_n <- data.frame(fips=substr(joint$fips, 1, 5), n=joint$n, stringsAsFactors=FALSE) %>% 
      group_by(fips) %>% 
      summarize(cnip=sum(n))
    survey_month <- as.numeric(gsub(".+_", "", survey))
    survey_year <- as.numeric(gsub("_.+", "", survey))
    if (survey_month == 1) {
      laus_ref <- laus[laus$month == paste0(survey_year - 1, "_12"),]
    } else {
      laus_ref <- laus[laus$month == paste0(survey_year, "_", str_pad(survey_month - 1, width=2, pad="0")),]
    }
    laus_ref <- inner_join(fips_n, laus_ref, by="fips")
    laus_ref <- laus_ref[complete.cases(laus_ref),]
    laus_ref$employed <- laus_ref$employed / laus_ref$cnip
    laus_ref$laborforce <- laus_ref$laborforce / laus_ref$cnip
    laus_ref <- data.frame(laus_ref)

    joint$ix <- 1:nrow(joint)
    joint$emp_laborforce <- 1 - joint$emp_notinlaborforce
    joint_split <- lapply(c("ix", "n", "emp_laborforce", "emp_employed"), function(i) split(x=joint[,i], f=substr(joint$fips, 1, 5)))
    names(joint_split) <- c("ix", "n", "laborforce", "employed")

    joint_laus <- data.frame(ix=joint$ix)
    deltas <- NULL
    for (y in c("laborforce", "employed")) {
      registerDoMC(5)
      raw_adj <- foreach (i = 1:nrow(laus_ref)) %dopar% {
        n <- joint_split$n[[laus_ref$fips[i]]]
        x <- joint_split[[y]][[laus_ref$fips[i]]]
        delta <- FindDelta(y=x, n=n, yhat=min(1, laus_ref[i, y]))
        out <- data.frame(
          ix=joint_split$ix[[laus_ref$fips[i]]], 
          y=invlogit(logit(x) + delta) * n)
        colnames(out) <- c("ix", paste0("laus_", y))
        return(list(delta=delta, out=out))
      }
      deltas <- rbind(deltas, data.frame(
        fips=laus_ref$fips, 
        y=y, 
        delta=sapply(raw_adj, function(i) i$delta), 
        stringsAsFactors=FALSE))
      raw_adj <- lapply(raw_adj, function(i) i$out)
      joint_laus <- left_join(joint_laus, bind_rows(raw_adj), by="ix")
      ok <- is.na(joint_laus[, paste0("laus_", y)])
      joint_laus[ok, paste0("laus_", y)] <- joint[ok, paste0("emp_", y)] * joint$n[ok]
    }
    joint_laus$laus_employed <- joint_laus$laus_employed / joint_laus$laus_laborforce
    joint_laus$laus_laborforce <- joint_laus$laus_laborforce / joint$n
    for (i in colnames(joint_laus)[colnames(joint_laus) != "ix"])
      # joint[,i] <- logit(joint_laus[,i])
      joint[,i] <- joint_laus[,i]
  } 

  joint$agegrp2 <- c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4)[joint$agegrp]
  
  if (is.null(starting_model)) {
    registerDoMC(3)
    raw <- foreach (model = paste0("cps_", survey, "_", c("y_laborforce_cnip", "y_employed_laborforce", "y_atwork_employed"))) %dopar% {

      tmp <- readRDS(paste0(model_directory, "/", model, ".rds"))
      M_baseline <- tmp$M_baseline
      M_occ <- tmp$M_occ
      weekly_agg <- tmp$weekly_agg
      fix <- tmp$fix
      ran <- tmp$ran
      M_inter <- tmp$M_inter
      national_cnip <- tmp$national_cnip
      national_target <- tmp$national_target

      if (length(grep("y_laborforce_cnip", model)) == 1) {

        # baseline model
        message("  ", model, ": baseline model")
        baseline <- "laus_laborforce"
        joint$baseline <- joint[,baseline]
        if (!is.null(M_baseline))
          joint$baseline <- logit(coef(M_baseline)[1] + joint[,baseline])

        # occupation model
        message("  ", model, ": occupation model")
        coefs <- as.matrix(coef(M_occ))
        joint$occ <- joint$baseline + coefs[1]
        ok <- coefs != 0 & rownames(coefs) != "(Intercept)" & rownames(coefs) %in% colnames(joint_inter)
        if (any(ok)) {
          xs <- rownames(coefs)[ok]
          coefs <- matrix(coefs[ok])
          joint$occ <- joint$occ + (joint_inter[,xs] %*% coefs)[,1]
        }
        joint$occ <- pmin(logit(1, digits=3), pmax(logit(0, digits=3), joint$occ))

      } else if (length(grep("y_employed_laborforce", model)) == 1) {

        # baseline model
        message("  ", model, ": baseline model")
        baseline <- "laus_employed"
        joint$baseline <- joint[,baseline]
        if (!is.null(M_baseline))
          joint$baseline <- logit(coef(M_baseline)[1] + coef(M_baseline)[2] * joint[,baseline])

        # occupation model
        message("  ", model, ": occupation model")
        coefs <- as.matrix(coef(M_occ))
        joint$occ <- joint$baseline + coefs[1]
        ok <- coefs != 0 & rownames(coefs) != "(Intercept)" & rownames(coefs) %in% colnames(joint_inter)
        if (any(ok)) {
          xs <- rownames(coefs)[ok]
          coefs <- matrix(coefs[ok])
          joint$occ <- joint$occ + (joint_inter[,xs] %*% coefs)[,1]
        }
        joint$occ <- pmin(logit(1, digits=3), pmax(logit(0, digits=3), joint$occ))

      } else if (length(grep("y_atwork_employed", model)) == 1) {

        # occupation model
        message("  ", model, ": occupation model")
        coefs <- as.matrix(coef(M_occ))
        joint$occ <- coefs[1]
        ok <- coefs != 0 & rownames(coefs) != "(Intercept)" & rownames(coefs) %in% colnames(joint_inter)
        if (any(ok)) {
          xs <- rownames(coefs)[ok]
          coefs <- matrix(coefs[ok])
          joint$occ <- joint$occ + (joint_inter[,xs] %*% coefs)[,1]
        }
        joint$occ <- pmin(logit(1, digits=3), pmax(logit(0, digits=3), joint$occ))

      }

      # weekly data
      z <- weekly_agg$z_weekly
      names(z) <- weekly_agg$state
      joint$z_weekly <- z[joint$state]

      # lmer model
      message("  ", model, ": lmer model")
      if (is.null(fix)) {
        yhat_lmer <- joint$occ
      } else {
        yhat_lmer <- fix["(Intercept)"] + joint$occ
        for (i in names(fix)[names(fix) != "(Intercept)"])
          yhat_lmer <- yhat_lmer + fix[i] * joint[,i]
        for (i in names(ran))
          yhat_lmer <- yhat_lmer + NVL(ran[[i]][as.character(joint[,i]),])
      }

      # interactions model
      message("  ", model, ": interactions model")
      if (is.null(M_inter)) {
        yhat <- yhat_lmer
      } else {
        coefs <- as.matrix(coef(M_inter))
        yhat <- coefs["(Intercept)",] + yhat_lmer

        ok <- coefs != 0 & rownames(coefs) != "(Intercept)" & rownames(coefs) %in% colnames(joint_inter)
        if (any(ok)) {
          xs <- rownames(coefs)[ok]
          coefs <- matrix(coefs[ok])
          yhat <- yhat + (joint_inter[,xs] %*% coefs)[,1]
        }
      }
      yhat <- invlogit(yhat)

      return(list(national_target=national_target, national_cnip=national_cnip, yhat=yhat))
    }
    out <- data.frame(laborforce=raw[[1]]$yhat, employed=raw[[2]]$yhat, atwork=raw[[3]]$yhat)
    names(raw) <- colnames(out)
  } else {
    registerDoMC(3)
    raw <- foreach (model = paste0("cps_", survey, "_", c("y_laborforce_cnip", "y_employed_laborforce", "y_atwork_employed"))) %dopar% {
      return(readRDS(paste0(model_directory, "/", model, ".rds")))
    }
    out <- readRDS(paste0(model_directory, "/", starting_model))
    colnames(out) <- gsub(paste0("_", survey), "", colnames(out))
    out$atwork <- out$atwork / out$employed
    out$employed <- out$employed / out$laborforce
    out$laborforce <- out$laborforce / out$n
    out <- out[, c("laborforce", "employed", "atwork")]
    out[is.na(out)] <- 0
    names(raw) <- colnames(out)
  }

  # county adjustment if a target is provided
  if (!is.null(county_target)) {
    message("  ", survey, ": county deltas")
    for (i_model in colnames(out)) {
      if (i_model %in% names(county_target)) {
        if (i_model == "laborforce") {
          delta <- data.frame(fips=substr(joint$fips, 1, 5), y=round(out[,i_model], 3), n=joint$n, stringsAsFactors=FALSE)
        } else if (i_model == "employed") {
          delta <- data.frame(fips=substr(joint$fips, 1, 5), y=round(out[,i_model], 3), n=out$laborforce * joint$n, stringsAsFactors=FALSE)
        } else if (i_model == "atwork") {
          delta <- data.frame(fips=substr(joint$fips, 1, 5), y=round(out[,i_model], 3), n=out$laborforce * out$employed * joint$n, stringsAsFactors=FALSE)
        } else {
          stop("bad model (county_target)")
        }
        delta <- data.frame(delta %>% 
          group_by(fips, y) %>%
          summarize(n=sum(n)), 
          stringsAsFactors=FALSE)
        fipss <- sort(unique(delta$fips))
        fipss <- fipss[fipss %in% county_target$fips]
        delta <- delta[delta$fips %in% fipss,]
        n_split <- split(delta$n, f=delta$fips)
        y_split <- split(delta$y, f=delta$fips)
        y_target <- county_target[, i_model]
        names(y_target) <- county_target$fips
        registerDoMC(5)
        delta <- unlist(foreach(i = 1:length(fipss)) %dopar% {
          FindDelta(y=y_split[[fipss[i]]], n=n_split[[fipss[i]]], yhat=y_target[fipss[i]])
        })
        names(delta) <- fipss
        correction <- delta[substr(joint$fips, 1, 5)]
        out[,i_model] <- invlogit(logit(out[,i_model]) + NVL(correction))
      }
    }
  }

  # state adjustment if a target is provided
  if (!is.null(state_target)) {
    message("  ", survey, ": state deltas")
    for (i_model in colnames(out)) {
      if (i_model %in% names(state_target)) {
        if (i_model == "laborforce") {
          delta <- data.frame(fips=substr(joint$fips, 1, 2), y=round(out[,i_model], 3), n=joint$n, stringsAsFactors=FALSE)
        } else if (i_model == "employed") {
          delta <- data.frame(fips=substr(joint$fips, 1, 2), y=round(out[,i_model], 3), n=out$laborforce * joint$n, stringsAsFactors=FALSE)
        } else if (i_model == "atwork") {
          delta <- data.frame(fips=substr(joint$fips, 1, 2), y=round(out[,i_model], 3), n=out$laborforce * out$employed * joint$n, stringsAsFactors=FALSE)
        } else {
          stop("bad model (state_target)")
        }
        delta <- data.frame(delta %>% 
          group_by(fips, y) %>%
          summarize(n=sum(n)), 
          stringsAsFactors=FALSE)
        fipss <- sort(unique(delta$fips))
        fipss <- fipss[fipss %in% state_target$fips]
        delta <- delta[delta$fips %in% fipss,]
        n_split <- split(delta$n, f=delta$fips)
        y_split <- split(delta$y, f=delta$fips)
        y_target <- state_target[, i_model]
        names(y_target) <- state_target$fips
        registerDoMC(5)
        delta <- unlist(foreach(i = 1:length(fipss)) %dopar% {
          FindDelta(y=y_split[[fipss[i]]], n=n_split[[fipss[i]]], yhat=y_target[fipss[i]])
        })
        names(delta) <- fipss
        correction <- delta[substr(joint$fips, 1, 2)]
        out[,i_model] <- invlogit(logit(out[,i_model]) + NVL(correction))
      }
    }
  }

  # demographic adjustment if a target is provided
  if (!is.null(demo_target)) {
    demoids <- sort(unique(demo_target$demoid))
    demoids <- demoids[demoids %in% joint$demoid]
    demo_target$ok <- demo_target$demoid %in% demoids
    # n correction
    message("  ", survey, ": demographic targets (n)")
    tmp <- left_join(
      data.frame(joint %>% group_by(demoid) %>% summarize(orig=sum(n)), stringsAsFactors=fALSE), 
      demo_target[, c("demoid", "n")], 
      by="demoid")
    tmp$ratio <- tmp$n / tmp$orig
    tmp$ratio[is.na(tmp$ratio)] <- 1
    correction <- tmp$ratio
    names(correction) <- tmp$demoid
    joint$n <- joint$n * correction[joint$demoid]
    # laborforce
    message("  ", survey, ": demographic targets (laborforce)")
    x_split <- split(x=out$laborforce, f=joint$demoid)
    n_split <- split(x=joint$n, f=joint$demoid)
    yhat <- demo_target$laborforce
    names(yhat) <- demo_target$demoid
    registerDoMC(5)
    deltas <- foreach(i = 1:length(demoids)) %dopar% {
      FindDelta(y=x_split[[demoids[i]]], n=n_split[[demoids[i]]], yhat=yhat[demoids[i]])
    }
    deltas <- unlist(deltas)
    names(deltas) <- demoids
    correction <- deltas[joint$demoid]
    out$laborforce <- invlogit(logit(out$laborforce) + NVL(correction))
    # employed
    message("  ", survey, ": demographic targets (employed)")
    x_split <- split(x=out$employed, f=joint$demoid)
    n_split <- split(x=joint$n * out$laborforce, f=joint$demoid)
    yhat <- demo_target$employed
    names(yhat) <- demo_target$demoid
    registerDoMC(5)
    deltas <- foreach(i = 1:length(demoids)) %dopar% {
      FindDelta(y=x_split[[demoids[i]]], n=n_split[[demoids[i]]], yhat=yhat[demoids[i]])
    }
    deltas <- unlist(deltas)
    names(deltas) <- demoids
    correction <- deltas[joint$demoid]
    out$employed <- invlogit(logit(out$employed) + NVL(correction))
    # atwork
    message("  ", survey, ": demographic targets (atwork)")
    x_split <- split(x=out$atwork, f=joint$demoid)
    n_split <- split(x=joint$n * out$laborforce * out$employed, f=joint$demoid)
    yhat <- demo_target$atwork
    names(yhat) <- demo_target$demoid
    registerDoMC(5)
    deltas <- foreach(i = 1:length(demoids)) %dopar% {
      FindDelta(y=x_split[[demoids[i]]], n=n_split[[demoids[i]]], yhat=yhat[demoids[i]])
    }
    deltas <- unlist(deltas)
    names(deltas) <- demoids
    correction <- deltas[joint$demoid]
    out$atwork <- invlogit(logit(out$atwork) + NVL(correction))
  }

  # line it up with national number
  national_cnip <- raw[[1]]$national_cnip
  correction <- national_cnip / sum(joint$n)
  joint$n <- joint$n * correction

  message("  ", survey, ": national deltas")
  for (i_model in colnames(out)) {
    if (i_model == "laborforce") {
      delta <- data.frame(y=round(out[,i_model], 5), n=joint$n)
    } else if (i_model == "employed") {
      delta <- data.frame(y=round(out[,i_model], 5), n=out$laborforce * joint$n)
    } else if (i_model == "atwork") {
      delta <- data.frame(y=round(out[,i_model], 5), n=out$laborforce * out$employed * joint$n)
    } else {
      stop("bad model (national deltas)")
    }
    delta <- data.frame(delta %>% 
      group_by(y) %>%
      summarize(n=sum(n)), 
      stringsAsFactors=FALSE)
    delta <- FindDelta(y=delta$y, n=delta$n, yhat=raw[[i_model]]$national_target)
    out[,i_model] <- invlogit(logit(out[,i_model]) + delta)
  }

  out$laborforce <- round(out$laborforce * joint$n, 3)
  out$employed <- round(out$employed * out$laborforce, 3)
  out$atwork <- round(out$atwork * out$employed, 3)
  out$n <- joint$n

  colnames(out) <- paste0(colnames(out), "_", survey)
  saveRDS(out, file=paste0(model_directory, "/", output_prefix, "_", survey, ".rds"))

  message("********* DONE: ", survey)
  return(out)

}
