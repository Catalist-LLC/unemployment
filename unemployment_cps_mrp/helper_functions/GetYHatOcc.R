GetYHatOcc <- function(occ, occ_inter, models, y) {

  yhat <- rep(NA, nrow(occ))

  if (substr(y, 1, 4) == "emp_") {
    for (i_model in names(models)) {
      female <- as.numeric(substr(i_model, 1, 1))
      married <- as.numeric(substr(i_model, 2, 2))
      ok_rows <- occ$female == female & occ$married == married
      tmp <- occ[ok_rows,]
      tmp_inter <- occ_inter[ok_rows,]

      fix <- models[[i_model]]$fix
      ran <- models[[i_model]]$ran
      M_glmnet <- models[[i_model]]$M_glmnet

      message(y, ": lmer, female=", female, ", married=", married)
      yhat_model <- fix["(Intercept)"] + tmp$citizen * fix["citizen"]
      for (i in names(ran))
        yhat_model <- yhat_model + NVL(ran[[i]][as.character(tmp[,i]),])

      message(y, ": glmnet, female=", female, ", married=", married)
      coefs <- as.matrix(coef(M_glmnet))
      yhat_model <- coefs["(Intercept)",] + coefs["yhat_lmer",] * yhat_model
      coefs <- coefs[coefs != 0,]
      coefs <- coefs[names(coefs) %in% c("(Intercept)", "yhat_lmer", colnames(tmp_inter))]
      if (length(coefs) > 0) {
        xs <- names(coefs)[3:length(coefs)]
        coefs <- matrix(coefs[3:length(coefs)])
        yhat_model <- yhat_model + (tmp_inter[,xs] %*% coefs)[,1]
      }
      yhat[ok_rows] <- yhat_model
    }
  } else {
    for (i_model in names(models)) {
      female <- as.numeric(substr(i_model, 1, 1))
      ok_rows <- occ$female == female
      tmp <- occ[ok_rows,]
      tmp_inter <- occ_inter[ok_rows,]

      fix <- models[[i_model]]$fix
      ran <- models[[i_model]]$ran
      M_glmnet <- models[[i_model]]$M_glmnet

      message(y, ": lmer, female=", female)
      yhat_model <- fix["(Intercept)"] + tmp$citizen * fix["citizen"] + tmp$married * fix["married"]
      for (i in names(ran))
        yhat_model <- yhat_model + NVL(ran[[i]][as.character(tmp[,i]),])

      message(y, ": glmnet, female=", female)
      coefs <- as.matrix(coef(M_glmnet))
      yhat_model <- coefs["(Intercept)",] + coefs["yhat_lmer",] * yhat_model
      coefs <- coefs[coefs != 0,]
      coefs <- coefs[names(coefs) %in% c("(Intercept)", "yhat_lmer", colnames(tmp_inter))]
      if (!(all(names(coefs) %in% c("(Intercept)", "yhat_lmer")))) {
        xs <- names(coefs)[3:length(coefs)]
        coefs <- matrix(coefs[3:length(coefs)])
        yhat_model <- yhat_model + (tmp_inter[,xs] %*% coefs)[,1]
      }
      yhat[ok_rows] <- yhat_model
    }
  }

  return(round(invlogit(yhat), 3))

}
