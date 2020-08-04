################################################################################################################
# helper function

SmoothSurveyRef <- function(survey_ref, xs, y_var, joint=NULL) {

  ys <- unique(survey_ref[,y_var])
  survey_ref <- sqldf(paste0("
    select 
    ", paste0(xs, collapse=", "), ", 
    sum(n) as n, 
    ", paste0("sum(case when ", y_var, " = '", ys, "' then n else 0 end) as y_", ys, collapse=", "), "
    from survey_ref
    group by ", paste0(xs, collapse=", "), "
  "))
  if (!is.null(joint))
    survey_ref <- left_join(joint[,xs], survey_ref, by=xs)

  inters <- as.data.frame(combinations(n=length(xs), r=2, v=xs), stringsAsFactors=FALSE)
  colnames(inters) <- c("x1", "x2")
  inters$full <- paste0(inters$x1, "__", inters$x2)
  xs_full <- c(xs, inters$full)
  for (i in 1:nrow(inters))
    survey_ref[, inters$full[i]] <- paste0(survey_ref[,inters$x1[i]], "__", survey_ref[,inters$x2[i]])

  drop <- sapply(xs_full, function(i) length(unique(survey_ref[,i])) < 2)
  xs_full <- xs_full[!drop]
  options(warn=-1)
  Y <- sapply(ys, function(y) {
    y_bin <- cbind(survey_ref[, paste0("y_", y)], survey_ref$n - survey_ref[, paste0("y_", y)])
    eval(parse(text=paste0("
      M <- glmer(
        y_bin ~ 
        ", paste0("(1 | ", xs_full, ")", collapse=" + "), ", 
        data=survey_ref, family='binomial', 
        control=glmerControl(optimizer='nloptwrap'))
    ")))
    out <- rep(fixef(M), nrow(survey_ref))
    for (i in xs_full)
      out <- out + NVL(ranef(M)[[i]][survey_ref[,i],])
    return(as.numeric(invlogit(out)))
  })
  options(warn=0)
  colnames(Y) <- paste0("y_", ys)

  out <- data.frame(
    survey_ref[,xs], 
    Y / rowSums(Y), 
    stringsAsFactors=FALSE)

  return(out)
}

################################################################################################################
# variable importance order
# state, age, sex are on everything to one degree or another
# then ... education, marital, race, citizen

MarginalToJoint <- function(marg, survey, state) {

  dat <- marg[marg$state == state, 3:ncol(marg)]
  # 16-17 year olds will be estimated using 15-17 year olds * 2/3
  for (i in grep("_1517_", colnames(dat), value=TRUE))
    dat[, gsub("_1517_", "_1617_", i)] <- dat[,i] * 2/3

  geos <- paste0("geo", marg$fips[marg$state == state])
  n_geo <- nrow(dat)

  # first get sex x race x citizenship for all age groups
  message(state, ": sex x race x citizenship")
  survey_ref <- survey[survey$state == state,]
  survey_ref$sex <- c("male", "female")[survey_ref$female]
  survey_ref$age <- c("1617", "1819", "2024", "2529", "3034", "3544", "3544", "4554", "4554", "5564", "5564", "6574", "6574", "75plus")[survey_ref$agegrp]
  survey_ref$race <- c("white", "black", "hispanic", "asian", "native", "other")[survey_ref$race]
  survey_ref$citizenship <- c("noncitizen", "citizen")[survey_ref$citizen + 1]

  joint1 <- sqldf(paste0("
    select 
    sex, race, citizenship
    from survey_ref
    group by sex, race, citizenship
  "))
  tmp <- t(dat[,paste0("sex_race_citizenship__", apply(joint1, 1, paste0, collapse="_"))])
  colnames(tmp) <- geos
  joint1 <- data.frame(joint1, tmp, stringsAsFactors=FALSE)
  survey_ref <- SmoothSurveyRef(survey_ref, xs=c("sex", "race", "citizenship"), y_var="age", joint=joint1)

  ys <- gsub("^y_", "", grep("^y_", colnames(survey_ref), value=TRUE))
  merged <- left_join(joint1, survey_ref, by=c("sex", "race", "citizenship"))
  joint2 <- NULL
  for (y in ys) {
    prod <- matrix(rep(merged[, paste0("y_", y)], n_geo), ncol=n_geo, byrow=FALSE)
    joint2 <- rbind(joint2, data.frame(
      merged[,c("sex", "race", "citizenship")], 
      age=y, 
      merged[,geos] * prod, 
      stringsAsFactors=FALSE))
  }
  colnames(joint2)[grep("^merged", colnames(joint2))] <- geos
  correction <- data.frame(
    joint2 %>% 
    group_by(sex, age, race) %>% 
    summarize_at(geos, sum))
  numerator <- t(dat[,paste0("sex_age_race__", apply(correction[, c("sex", "age", "race")], 1, paste0, collapse="_"))])
  correction[,geos] <- numerator / correction[,geos]
  correction[is.na(correction) | correction == Inf] <- 1

  joint2 <- left_join(joint2, correction, by=c("sex", "age", "race"))
  joint2[, paste0(geos, ".x")] <- joint2[, paste0(geos, ".x")] * joint2[, paste0(geos, ".y")]
  joint2 <- joint2[, grep("\\.y$", colnames(joint2), invert=TRUE)]
  colnames(joint2) <- gsub("\\.x$", "", colnames(joint2))

  # split out the new age years
  message(state, ": age groups")
  survey_ref <- survey[survey$state == state,]
  survey_ref$sex <- c("male", "female")[survey_ref$female]
  survey_ref$age <- c("1617", "1819", "2024", "2529", "3034", "3544", "3544", "4554", "4554", "5564", "5564", "6574", "6574", "75plus")[survey_ref$agegrp]
  survey_ref$age_end <- c("1617", "1819", "2024", "2529", "3034", "3539", "4044", "4549", "5054", "5559", "6064", "6574", "6574", "75plus")[survey_ref$agegrp]
  survey_ref$race <- c("white", "black", "hispanic", "asian", "native", "other")[survey_ref$race]
  survey_ref$citizenship <- c("noncitizen", "citizen")[survey_ref$citizen + 1]

  survey_ref_out <- left_join(joint2[, c("sex", "race", "citizenship", "age")], 
    sqldf(paste0("
      select 
      sex, age, race, citizenship
      from survey_ref
      group by sex, age, race, citizenship
    ")), by=c("sex", "race", "citizenship", "age"))
  ys <- unique(survey_ref$age_end)
  for (i in ys)
    survey_ref_out[, paste0("y_", i)] <- NA

  for (i in ys[!(ys %in% c("3544", "4554", "5564"))])
    survey_ref_out[survey_ref_out$age == i, paste0("y_", i)] <- 1

  rownames(survey_ref_out) <- apply(survey_ref_out[, c("sex", "age", "race", "citizenship")], 1, paste0, collapse="_")
  for (i in c("3544", "4554", "5564")) {
    tmp <- SmoothSurveyRef(survey_ref[survey_ref$age == i,], xs= c("sex", "age", "race", "citizenship"), y_var="age_end")
    rownames(tmp) <- apply(tmp[, c("sex", "age", "race", "citizenship")], 1, paste0, collapse="_")
    for (y in grep("^y_", colnames(tmp), value=TRUE))
      survey_ref_out[rownames(tmp), y] <- tmp[rownames(tmp), y]
    ok <- rownames(survey_ref_out) %in% grep(i, rownames(survey_ref_out), value=TRUE) & 
      apply(is.na(survey_ref_out[, grep("^y_", colnames(survey_ref_out))]), 1, all)
    y_var <- grep("^y_", colnames(tmp), value=TRUE)
    survey_ref_out[ok, y_var] <- 1 / length(y_var)
  }
  rownames(survey_ref_out) <- NULL
  survey_ref_out[is.na(survey_ref_out)] <- 0
  survey_ref <- survey_ref_out

  merged <- left_join(joint2, survey_ref, by=c("sex", "age", "race", "citizenship"))
  joint3 <- NULL
  for (y in ys) {
    prod <- matrix(rep(merged[, paste0("y_", y)], n_geo), ncol=n_geo, byrow=FALSE)
    joint3 <- rbind(joint3, data.frame(
      merged[,c("sex", "race", "citizenship")], 
      age=y, 
      merged[,geos] * prod, 
      stringsAsFactors=FALSE))
  }
  colnames(joint3)[grep("^merged", colnames(joint3))] <- geos
  joint3 <- data.frame(
    joint3 %>%
    group_by(sex, age, race, citizenship) %>% 
    summarize_at(geos, sum))

  # add marital status
  message(state, ": marital status")
  survey_ref <- survey[survey$state == state,]
  survey_ref$sex <- c("male", "female")[survey_ref$female]
  survey_ref$age <- c("1617", "1819", "2024", "2529", "3034", "3539", "4044", "4549", "5054", "5559", "6064", "6574", "6574", "75plus")[survey_ref$agegrp]
  survey_ref$race <- c("white", "black", "hispanic", "asian", "native", "other")[survey_ref$race]
  survey_ref$citizenship <- c("noncitizen", "citizen")[survey_ref$citizen + 1]
  survey_ref$marital <- c("single", "married")[survey_ref$married]
  survey_ref <- SmoothSurveyRef(survey_ref, xs=c("sex", "age", "race", "citizenship"), y_var="marital", joint=joint3)

  ys <- gsub("^y_", "", grep("^y_", colnames(survey_ref), value=TRUE))
  merged <- left_join(joint3, survey_ref, by=c("sex", "age", "race", "citizenship"))
  joint4 <- NULL
  for (y in ys) {
    prod <- matrix(rep(merged[, paste0("y_", y)], n_geo), ncol=n_geo, byrow=FALSE)
    joint4 <- rbind(joint4, data.frame(
      merged[,c("sex", "age", "race", "citizenship")], 
      marital=y, 
      merged[,geos] * prod, 
      stringsAsFactors=FALSE))
  }
  colnames(joint4)[grep("^merged", colnames(joint4))] <- geos
  correction <- data.frame(
    joint4 %>% 
    group_by(sex, age, marital) %>% 
    summarize_at(geos, sum))
  numerator <- t(dat[,paste0("sex_age_marital__", apply(correction[, c("sex", "age", "marital")], 1, paste0, collapse="_"))])
  correction[,geos] <- numerator / correction[,geos]
  correction[is.na(correction) | correction == Inf] <- 1

  joint4 <- left_join(joint4, correction, by=c("sex", "age", "marital"))
  joint4[, paste0(geos, ".x")] <- joint4[, paste0(geos, ".x")] * joint4[, paste0(geos, ".y")]
  joint4 <- joint4[, grep("\\.y$", colnames(joint4), invert=TRUE)]
  colnames(joint4) <- gsub("\\.x$", "", colnames(joint4))

  # add education
  message(state, ": education")
  survey_ref <- survey[survey$state == state,]
  survey_ref$sex <- c("male", "female")[survey_ref$female]
  survey_ref$age <- c("1617", "1819", "2024", "2529", "3034", "3539", "4044", "4549", "5054", "5559", "6064", "6574", "6574", "75plus")[survey_ref$agegrp]
  survey_ref$race <- c("white", "black", "hispanic", "asian", "native", "other")[survey_ref$race]
  survey_ref$citizenship <- c("noncitizen", "citizen")[survey_ref$citizen + 1]
  survey_ref$marital <- c("single", "married")[survey_ref$married]
  survey_ref$edu <- c("nohs", "hs", "somecollege", "college", "postgrad")[survey_ref$edu]
  survey_ref <- SmoothSurveyRef(survey_ref, xs=c("sex", "age", "race", "citizenship", "marital"), y_var="edu", joint=joint4)

  ys <- gsub("^y_", "", grep("^y_", colnames(survey_ref), value=TRUE))
  merged <- left_join(joint4, survey_ref, by=c("sex", "age", "race", "citizenship", "marital"))
  joint5 <- NULL
  for (y in ys) {
    prod <- matrix(rep(merged[, paste0("y_", y)], n_geo), ncol=n_geo, byrow=FALSE)
    joint5 <- rbind(joint5, data.frame(
      merged[,c("sex", "age", "race", "citizenship", "marital")], 
      edu=y, 
      merged[,geos] * prod, 
      stringsAsFactors=FALSE))
  }
  colnames(joint5)[grep("^merged", colnames(joint5))] <- geos
  joint5$age2 <- c("1617"="1617", "1819"="1824", "2024"="1824", "2529"="2534", "3034"="2534", "3539"="3544", "4044"="3544", "4549"="4564", "5054"="4564", "5559"="4564", "6064"="4564", "6574"="65plus", "75plus"="65plus")[joint5$age]

  correction <- data.frame(
    joint5 %>% 
    group_by(sex, age2, edu) %>% 
    summarize_at(geos, sum))
  numerator <- as.matrix(correction[,geos])
  ix <- paste0("sex_age_edu__", apply(correction[, c("sex", "age2", "edu")], 1, paste0, collapse="_"))
  ok <- ix %in% colnames(dat)
  numerator[ok,] <- t(dat[,ix[ok]])
  correction[,geos] <- numerator / correction[,geos]
  correction[is.na(correction) | correction == Inf] <- 1

  joint5 <- left_join(joint5, correction, by=c("sex", "age2", "edu"))
  joint5[, paste0(geos, ".x")] <- joint5[, paste0(geos, ".x")] * joint5[, paste0(geos, ".y")]
  joint5 <- joint5[, grep("\\.y$", colnames(joint5), invert=TRUE)]
  colnames(joint5) <- gsub("\\.x$", "", colnames(joint5))
  joint5 <- joint5[, colnames(joint5) != "age2"]

  # split out the new age years
  message(state, ": final age groups")
  survey_ref <- survey[survey$state == state,]
  survey_ref$sex <- c("male", "female")[survey_ref$female]
  survey_ref$race <- c("white", "black", "hispanic", "asian", "native", "other")[survey_ref$race]
  survey_ref$citizenship <- c("noncitizen", "citizen")[survey_ref$citizen + 1]
  survey_ref$marital <- c("single", "married")[survey_ref$married]
  survey_ref$edu <- c("nohs", "hs", "somecollege", "college", "postgrad")[survey_ref$edu]
  survey_ref$age <- c("1617", "1819", "2024", "2529", "3034", "3539", "4044", "4549", "5054", "5559", "6064", "6574", "6574", "75plus")[survey_ref$agegrp]
  survey_ref$age_end <- c("1617", "1819", "2024", "2529", "3034", "3539", "4044", "4549", "5054", "5559", "6064", "6569", "7074", "75plus")[survey_ref$agegrp]

  survey_ref_out <- left_join(joint5[, c("sex", "race", "citizenship", "age", "marital", "edu")], 
    sqldf(paste0("
      select 
      sex, age, race, citizenship, marital, edu
      from survey_ref
      group by sex, age, race, citizenship, marital, edu
    ")), by=c("sex", "race", "citizenship", "age", "marital", "edu"))
  ys <- unique(survey_ref$age_end)
  for (i in ys)
    survey_ref_out[, paste0("y_", i)] <- NA

  for (i in ys[!(ys %in% c("6574"))])
    survey_ref_out[survey_ref_out$age == i, paste0("y_", i)] <- 1

  rownames(survey_ref_out) <- apply(survey_ref_out[, c("sex", "race", "citizenship", "age", "marital", "edu")], 1, paste0, collapse="_")
  for (i in c("6574")) {
    tmp <- SmoothSurveyRef(survey_ref[survey_ref$age == i,], xs= c("sex", "race", "citizenship", "age", "marital", "edu"), y_var="age_end")
    rownames(tmp) <- apply(tmp[, c("sex", "race", "citizenship", "age", "marital", "edu")], 1, paste0, collapse="_")
    for (y in grep("^y_", colnames(tmp), value=TRUE))
      survey_ref_out[rownames(tmp), y] <- tmp[rownames(tmp), y]
    ok <- rownames(survey_ref_out) %in% grep(i, rownames(survey_ref_out), value=TRUE) & 
      apply(is.na(survey_ref_out[, grep("^y_", colnames(survey_ref_out))]), 1, all)
    y_var <- grep("^y_", colnames(tmp), value=TRUE)
    survey_ref_out[ok, y_var] <- 1 / length(y_var)
  }
  rownames(survey_ref_out) <- NULL
  survey_ref_out[is.na(survey_ref_out)] <- 0
  survey_ref <- survey_ref_out

  merged <- left_join(joint5, survey_ref, by=c("sex", "race", "citizenship", "age", "marital", "edu"))
  joint6 <- NULL
  for (y in ys) {
    prod <- matrix(rep(merged[, paste0("y_", y)], n_geo), ncol=n_geo, byrow=FALSE)
    joint6 <- rbind(joint6, data.frame(
      merged[,c("sex", "race", "citizenship", "marital", "edu")], 
      age=y, 
      merged[,geos] * prod, 
      stringsAsFactors=FALSE))
  }
  colnames(joint6)[grep("^merged", colnames(joint6))] <- geos
  joint6 <- data.frame(
    joint6 %>%
    group_by(sex, age, race, citizenship, marital, edu) %>% 
    summarize_at(geos, sum))

  # recode xs to make them compatible with CPS models
  joint6$agegrp <- c("1617"=1, "1819"=2, "2024"=3, "2529"=4, "3034"=5, "3539"=6, "4044"=7, "4549"=8, "5054"=9, "5559"=10, "6064"=11, "6569"=12, "7074"=13, "75plus"=14)[joint6$age]
  joint6$female <- c("male"=1, "female"=2)[joint6$sex]
  joint6$race <- c("white"=1, "black"=2, "hispanic"=3, "asian"=4, "native"=5, "other"=6)[joint6$race]
  joint6$edu <- c("nohs"=1, "hs"=2, "somecollege"=3, "college"=4, "postgrad"=5)[joint6$edu]
  joint6$married <- c("married"=2, "single"=1)[joint6$marital]
  joint6$citizen <- c("citizen"=1, "noncitizen"=0)[joint6$citizenship]

  skinny <- bind_rows(lapply(geos, function(i) 
    data.frame(
      state=state, 
      fips=gsub("^geo", "", i), 
      joint6[, c("agegrp", "female", "race", "edu", "married", "citizen")], 
      n=joint6[,i], 
      stringsAsFactors=FALSE)))
  skinny <- skinny[skinny$n > 0,]

  return(skinny)
  
}
