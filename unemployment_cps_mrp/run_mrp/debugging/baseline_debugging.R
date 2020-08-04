# par(mfrow=c(4,1))

i_comb <- 3

  ok <- cps$survey == combs$survey[i_comb]
  y <- combs$y[i_comb]

  tmp <- cps[ok,]
  OCC <- as.matrix(cps_occ[ok,])
  tmp$y <- tmp[,y]
  tmp$n <- tmp[, gsub("^y_", "n_", y)]
  ok <- !is.na(tmp$y)
  tmp <- tmp[ok,]
  OCC <- OCC[ok,]
  y_bin <- round(cbind(tmp$y, tmp$n - tmp$y))

#################################################################################

i <- ifelse(i_comb == 3, "laus_laborforce", "laus_employed")

ok <- is.na(OCC[,i])
occ_impute <- data.frame(x=OCC[,i], y=tmp$y / tmp$n, n=tmp$n)

occ_impute$col <- "black"
occ_impute$col[ok] <- "red"
occ_impute$col[!is.na(occ_impute$x) & !is.na(tmp$fips)] <- "blue"

#################################################################################

#     agg <- sqldf("select col, round(x, 2) as x, sum(n) as n, sum(n * y) / sum(n) as y from occ_impute where x is not null group by 1, 2")
#     plot(agg$x, agg$y, cex=sqrt(agg$n) / 10, xlim=c(0,1), ylim=c(0,1), col=agg$col, pch=20)
#     abline(a=0, b=1, col="grey")
#     abline(h=0.5, col="grey")
#     abline(v=0.5, col="grey")
#     abline(lm(y ~ x, w=n, data=agg[agg$col == "blue",]), col="blue")
#     abline(lm(y ~ x, w=n, data=agg[agg$col == "red",]), col="red")
#     abline(lm(y ~ x, w=n, data=agg), col="black")

# registerDoMC(30)
# coefs <- foreach(i_bag = 1:100) %dopar% {
#   samp <- occ_impute[sample(1:nrow(occ_impute), nrow(occ_impute), replace=TRUE),]
#   M <- lm(y ~ x, w=n, data=samp)
#   return(coef(M))
# }
# coefs <- t(sapply(coefs, function(i) i))

#################################################################################

M <- lm(x ~ y, w=n, data=occ_impute)
M_sd <- wt.sd(x=resid(M), wt=M$weight)

occ_impute$xhat <- coef(M)[1] + coef(M)[2] * occ_impute$y
occ_impute$xhat <- occ_impute$xhat + rnorm(nrow(occ_impute), mean=0, sd=M_sd)

occ_impute$final <- occ_impute$x
ok <- is.na(occ_impute$x)
occ_impute$final[ok] <- occ_impute$xhat[ok]

#################################################################################

plot(occ_impute$x, occ_impute$y, cex=sqrt(occ_impute$n), 
  pch=20, col=rgb(0, 0, 0, alpha=0.25), 
  xlim=c(0,1), ylim=c(0,1))
abline(a=0, b=1, col="grey", lwd=2)
abline(h=0.5, col="grey", lwd=2)
abline(v=0.5, col="grey", lwd=2)

M0 <- lm(y ~ x, w=n, data=occ_impute[occ_impute$col == "blue",])
M1 <- lm(y ~ x, w=n, data=occ_impute)
ok <- occ_impute$col == "black"
M2 <- lm(y ~ xhat, w=n, data=occ_impute[ok,])
M3 <- lm(y ~ xhat, w=n, data=occ_impute)
M4 <- lm(y ~ final, w=n, data=occ_impute)

out <- data.frame(coef(M0), coef(M1), coef(M2), coef(M3), coef(M4))
colnames(out) <- c("fips", "orig", "black", "both", "final")
print(out)

# abline(M1, col="red", lwd=2)
# abline(M2, col="blue", lwd=2)
# abline(M3, col="red", lwd=2)
# abline(M4, col="forest green", lwd=2)

#################################################################################

y_bin <- cbind(occ_impute$y * occ_impute$n, (1 - occ_impute$y) * occ_impute$n)

occ_impute$logit_x <- logit(occ_impute$x, digits=3)
occ_impute$logit_xhat <- logit(occ_impute$xhat, digits=3)
occ_impute$logit_final <- occ_impute$logit_x
ok <- is.na(occ_impute$logit_x)
occ_impute$logit_final[ok] <- occ_impute$logit_xhat[ok]

ok <- occ_impute$col == "blue"
M0 <- glm(y_bin[ok,] ~ logit_xhat, data=occ_impute[ok,], family="quasibinomial")
M1 <- glm(y_bin ~ logit_x, data=occ_impute, family="quasibinomial")
ok <- occ_impute$col == "black"
M2 <- glm(y_bin[ok,] ~ logit_xhat, data=occ_impute[ok,], family="quasibinomial")
M3 <- glm(y_bin ~ logit_xhat, data=occ_impute, family="quasibinomial")
M4 <- glm(y_bin ~ logit_final, data=occ_impute, family="quasibinomial")

out <- data.frame(coef(M0), coef(M1), coef(M2), coef(M3), coef(M4))
colnames(out) <- c("fips", "orig", "black", "both", "final")
print(out)

#################################################################################

agg <- sqldf("select col, round(xhat, 2) as x, sum(n) as n, sum(n * y) / sum(n) as y from occ_impute group by 1, 2")
plot(agg$x, agg$y, cex=sqrt(agg$n) / 10, xlim=c(0,1), ylim=c(0,1), col=agg$col, pch=20)
abline(a=0, b=1, col="grey")
abline(h=0.5, col="grey")
abline(v=0.5, col="grey")

plot_x <- seq(0, 1, by=0.01)
plot_y <- invlogit(coef(M0)[1] + coef(M0)[2] * logit(plot_x))
points(plot_x, plot_y, type="l", col="blue", lwd=2)
plot_y <- invlogit(coef(M4)[1] + coef(M4)[2] * logit(plot_x))
points(plot_x, plot_y, type="l", col="black")

#################################################################################

ok <- occ_impute$col == "blue"
M0 <- glm(y_bin[ok,] ~ logit_xhat - 1, data=occ_impute[ok,], family="quasibinomial")
M1 <- glm(y_bin ~ logit_x - 1, data=occ_impute, family="quasibinomial")
ok <- occ_impute$col == "black"
M2 <- glm(y_bin[ok,] ~ logit_xhat - 1, data=occ_impute[ok,], family="quasibinomial")
M3 <- glm(y_bin ~ logit_xhat - 1, data=occ_impute, family="quasibinomial")
M4 <- glm(y_bin ~ logit_final - 1, data=occ_impute, family="quasibinomial")

out <- data.frame(coef(M0), coef(M1), coef(M2), coef(M3), coef(M4))
colnames(out) <- c("fips", "orig", "black", "both", "final")
print(out)

agg <- sqldf("select col, round(xhat, 2) as x, sum(n) as n, sum(n * y) / sum(n) as y from occ_impute group by 1, 2")
plot(agg$x, agg$y, cex=sqrt(agg$n) / 10, xlim=c(0,1), ylim=c(0,1), col=agg$col, pch=20)
abline(a=0, b=1, col="grey")
abline(h=0.5, col="grey")
abline(v=0.5, col="grey")

plot_x <- seq(0, 1, by=0.01)
plot_y <- invlogit(coef(M0)[1] * logit(plot_x))
points(plot_x, plot_y, type="l", col="blue", lwd=2)
plot_y <- invlogit(coef(M4)[1] * logit(plot_x))
points(plot_x, plot_y, type="l", col="black")

#################################################################################

M0 <- lm(y ~ x, w=n, data=occ_impute[occ_impute$col == "blue",])
M1 <- lm(y ~ x, w=n, data=occ_impute)
ok <- occ_impute$col == "black"
M2 <- lm(y ~ xhat, w=n, data=occ_impute[ok,])
M3 <- lm(y ~ xhat, w=n, data=occ_impute)
M4 <- lm(y ~ final, w=n, data=occ_impute)

out <- data.frame(coef(M0), coef(M1), coef(M2), coef(M3), coef(M4))
colnames(out) <- c("fips", "orig", "black", "both", "final")
print(out)

agg <- sqldf("select col, round(xhat, 2) as x, sum(n) as n, sum(n * y) / sum(n) as y from occ_impute group by 1, 2")
plot(agg$x, agg$y, cex=sqrt(agg$n) / 10, xlim=c(0,1), ylim=c(0,1), col=agg$col, pch=20)
abline(a=0, b=1, col="grey")
abline(h=0.5, col="grey")
abline(v=0.5, col="grey")

plot_x <- seq(0, 1, by=0.01)
plot_y <- coef(M0)[1] + coef(M0)[2] * plot_x
points(plot_x, plot_y, type="l", col="blue", lwd=2)
plot_y <- coef(M4)[1] + coef(M4)[2] * plot_x
points(plot_x, plot_y, type="l", col="black")

##################################################################################################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

joint <- occ[, c("n", "fips", "emp_laborforce", "emp_employed")]
for (i in grep("_", colnames(joint)))
  joint[,i] <- joint[,i] / joint$n
survey <- combs$survey[i_comb]

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
    # joint$emp_laborforce <- 1 - joint$emp_notinlaborforce
    joint_split <- lapply(c("ix", "n", "emp_laborforce", "emp_employed"), function(i) split(x=joint[,i], f=substr(joint$fips, 1, 5)))
    names(joint_split) <- c("ix", "n", "laborforce", "employed")

    joint_laus <- data.frame(ix=joint$ix)
    deltas <- NULL
    for (y in c("laborforce", "employed")) {
      registerDoMC(5)
      raw_adj <- foreach (i = 1:nrow(laus_ref)) %dopar% {
        n <- joint_split$n[[laus_ref$fips[i]]]
        x <- joint_split[[y]][[laus_ref$fips[i]]]
        delta <- FindDelta(y=x, n=n, yhat=min(laus_ref[i, y], 1))
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
    # for (i in colnames(joint_laus)[colnames(joint_laus) != "ix"])
    #   joint[,i] <- logit(joint_laus[,i])

joint <- data.frame(occ[,1:12], joint_laus)
gc()

##################################################################################################################################

library(plotrix)

  ok <- cps$survey == combs$survey[i_comb]
  y <- combs$y[i_comb]

  tmp <- cps[ok,]
  OCC <- as.matrix(cps_occ[ok,])
  tmp$y <- tmp[,y]
  tmp$n <- tmp[, gsub("^y_", "n_", y)]
  ok <- !is.na(tmp$y)
  tmp <- tmp[ok,]
  OCC <- data.frame(OCC[ok, grep("laus", colnames(OCC))])
  y_bin <- round(cbind(tmp$y, tmp$n - tmp$y))


joint$x <- joint$laus_laborforce
tmp$x <- OCC$laus_laborforce

fipss <- sort(unique(na.exclude(tmp$fips[!is.na(OCC$x)])))
metarea_fipss <- lookup$fips[lookup$metarea %in% sort(unique(na.exclude(tmp$metarea[is.na(tmp$fips)])))]
metarea_fipss <- metarea_fipss[!(metarea_fipss %in% fipss)]

joint$type <- "3 missing"
joint$type[joint$fips %in% fipss] <- "1 fips"
joint$type[joint$fips %in% metarea_fipss] <- "2 metarea"


plot(density(joint$x[joint$fips %in% fipss]), xlim=c(0,1))
points(density(joint$x[!(joint$fips %in% fipss)]), col="red", type="l")
points(density(na.exclude(tmp$x[tmp$fips %in% fipss])), col="blue", type="l")
points(density(na.exclude(tmp$x[!(tmp$fips %in% fipss)])), col="forest green", type="l")

tmp$type <- "3 missing"
tmp$type[!is.na(tmp$x) & !is.na(tmp$metarea)] <- "2 metarea"
tmp$type[!is.na(tmp$x) & !is.na(tmp$fips)] <- "1 fips"

##################################################################################################################################

i <- "x"

    occ_impute <- data.frame(x=OCC[,i], y=tmp$y / tmp$n, n=tmp$n)
    M <- lm(x ~ y, w=n, data=occ_impute)
    M_sd <- wt.sd(x=resid(M), wt=M$weight)
    tmp$xhat <- coef(M)[1] + coef(M)[2] * occ_impute$y
    tmp$xhat <- tmp$xhat + rnorm(nrow(occ_impute), mean=0, sd=M_sd)

##################################################################################################################################

cors <- occ_impute[complete.cases(occ_impute),]
cors <- cov.wt(cors[, c("x", "y")], w=cors$n, cor=TRUE)$cor

agg <- sqldf("select round(x, 2) as x, sum(n) as n, sum(n * y) / sum(n) as y from occ_impute where x is not null group by 1")
cov.wt(agg[, c("x", "y")], w=agg$n, cor=TRUE)$cor
M <- lm(x ~ y, w=n, data=agg)
display(M)
# M_sd <- wt.sd(x=resid(M), wt=M$weight)
tmp$xhat <- coef(M)[1] + coef(M)[2] * occ_impute$y

    tmp$xhat <- pmin(1, pmax(0, tmp$xhat + rnorm(nrow(occ_impute), mean=0, sd=M_sd)))


##################################################################################################################################

types <- left_join(
  data.frame(
    tmp %>% 
     group_by(type) %>% 
     summarize(
      cps_n=sum(n), 
      cps_y=sum(y) / sum(n), 
      cps_x=sum(n * x) / sum(n), 
      cps_xhat=sum(n * xhat) / sum(n))), 
  data.frame(
    joint %>% 
     group_by(type) %>% 
     summarize(
      joint_n=sum(n), 
      joint_x=sum(n * x) / sum(n))))
print(types)

##################################################################################################################################

agg <- data.frame(
    tmp %>% 
     group_by(fips) %>% 
     summarize(
      cps_n=sum(n), 
      cps_y=sum(y) / sum(n), 
      cps_x=sum(n * x) / sum(n)))

plot(agg$cps_x, agg$cps_y, cex=sqrt(agg$cps_n) / 10, xlim=c(0,1), ylim=c(0,1))
M <- lm(cps_y ~ cps_x, w=cps_n, data=agg)
display(M)
abline(M)

plot(occ_impute$x, occ_impute$y, cex=sqrt(occ_impute$n), xlim=c(0,1), ylim=c(0,1))
M <- lm(y ~ x, w=n, data=occ_impute)
display(M)
abline(M)

M <- lm(cps_x ~ cps_y, w=cps_n, data=agg)
M <- lm(x ~ y, w=n, data=occ_impute)

tmp$xhat <- pmin(1, pmax(0, (occ_impute$y - coef(M)[1]) / coef(M)[2]))



y = 0.04 + 0.85 * x
x = (y - 0.04) / 0.85

##################################################################################################################################

occ_impute$xhat <- tmp$xhat
ok <- is.na(occ_impute$x)
plot(occ_impute$x, occ_impute$y, cex=sqrt(occ_impute$n) / 5, xlim=c(0,1))
abline(lm(y ~ x, w=n, data=occ_impute), lwd=2)
points(occ_impute$xhat[!ok], occ_impute$y[!ok], cex=sqrt(occ_impute$n[!ok]) / 5, col="red")
abline(lm(y ~ xhat, w=n, data=occ_impute[!ok,]), col="red", lwd=2)
points(occ_impute$xhat[ok], occ_impute$y[ok], cex=sqrt(occ_impute$n[ok]) / 5, col="blue")
abline(lm(y ~ xhat, w=n, data=occ_impute[ok,]), col="blue", lwd=2)

##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

ydat <- tmp[, c("fips", "metarea", "x", "y", "n")]
ydat$y <- round(ydat$y)
ydat$n <- round(ydat$n)

raw <- lapply(1:nrow(ydat), function(i) {
  if (i %% 1000 == 1)
    message(i, " of ", nrow(ydat))
  n1 <- ydat$y[i]
  n0 <- ydat$n[i] - ydat$y[i]
  out <- NULL
  if (n1 > 0)
    out <- rbind(out, data.frame(
      fips=ydat$fips[i], 
      metarea=ydat$metarea[i], 
      x=ydat$x[i], 
      y=rep(1, n1), 
      stringsAsFactors=FALSE))
  if (n0 > 0)
    out <- rbind(out, data.frame(
      fips=ydat$fips[i], 
      metarea=ydat$metarea[i], 
      x=ydat$x[i], 
      y=rep(0, n0), 
      stringsAsFactors=FALSE))
  return(out)
})
ydat <- bind_rows(raw)

M <- glm(y ~ x, data=ydat, family="binomial")

plot(density(na.exclude(ydat$x[ydat$y == 0])))
points(density(na.exclude(ydat$x[ydat$y == 1])), col="grey", type="l")


##################################################################################################################################

tmp$y_pct <- tmp$y / tmp$n
source("../helper_functions/NVL.R")


  print(system.time(eval(parse(text=paste0("
    M_lmer <- glmer(
      x ~ 1 + y_pct + ", paste0(xs_binary, collapse=" + "), " + 
      ", paste0("(1 | ", xs_cat, ")", collapse=" + "), ", 
      data=tmp, 
      verbose=TRUE)
  ")))))

fix <- fixef(M_lmer)
ran <- ranef(M_lmer)
tmp$xhat <- fix["(Intercept)"]
for (i in names(fix)[names(fix) != "(Intercept)"])
  tmp$xhat <- tmp$xhat + fix[i] * tmp[,i]
for (i in names(ran))
  tmp$xhat <- tmp$xhat + NVL(ran[[i]][as.character(tmp[,i]),])

plot(tmp$xhat, tmp$x, xlim=c(0,1), ylim=c(0,1), pch=".")

##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

i_comb <- 2

  ok <- cps$survey == combs$survey[i_comb]
  y <- combs$y[i_comb]

  tmp <- cps[ok,]
  OCC <- as.matrix(cps_occ[ok,])
  tmp$y <- tmp[,y]
  tmp$n <- tmp[, gsub("^y_", "n_", y)]
  ok <- !is.na(tmp$y)
  tmp <- tmp[ok,]
  OCC <- OCC[ok,]
  y_bin <- cbind(tmp$y, tmp$n - tmp$y)

  ok <- complete.cases(OCC)
  tmp <- tmp[ok,]
  OCC <- OCC[ok,]
  y_bin <- y_bin[ok,]


tmp$x <- OCC[, ifelse(i_comb == 3, "laus_laborforce", "laus_employed")]

tmp$type <- "3 missing"
tmp$type[!is.na(tmp$x) & !is.na(tmp$metarea)] <- "2 metarea"
tmp$type[!is.na(tmp$x) & !is.na(tmp$fips)] <- "1 fips"

agg <- sqldf("select type, round(x, 2) as x, sum(n) as n, sum(y) / sum(n) as y from tmp group by 1, 2")
agg$col <- c(
  "1 fips"=rgb(0, 0, 1, alpha=0.25), 
  "2 metarea"=rgb(1, 0, 0, alpha=0.25), 
  "3 missing"=rgb(0, 0.5, 0, alpha=0.25))[agg$type]
plot(agg$x, agg$y, cex=sqrt(agg$n) / 10, xlim=c(0,1), ylim=c(0,1), col=agg$col, pch=20, main=y, xlab="", ylab="")
abline(a=0, b=1, col="grey")
abline(h=0.5, col="grey")
abline(v=0.5, col="grey")

tmp$ypct <- tmp$y / tmp$n
logit_x <- logit(tmp$x)
M1 <- glm(y_bin ~ logit_x, family="quasibinomial")
M2 <- glm(y_bin ~ logit_x - 1, family="quasibinomial")
M3 <- lm(ypct ~ x, w=n, data=tmp)

plot_x <- seq(0, 1, by=0.01)
# points(plot_x, invlogit(coef(M1)[1] + coef(M1)[2] * logit(plot_x)), type="l", col="red")
points(plot_x, invlogit(coef(M2)[1] * logit(plot_x)), type="l", col="blue")
abline(M3)
