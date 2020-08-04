PrepGeographicData <- function(marg) {
  # fill in blank income values
  incs <- grep("^inc__", colnames(marg), value=TRUE)
  inc_yhat <- sapply(incs, function(i1) {
    tmp <- marg[, incs]
    tmp$y <- tmp[,i1]
    tmp$n <- rowSums(marg[, grep("emp__employed", colnames(marg))])
    rng <- range(tmp$y, na.rm=TRUE)
    yhats <- sapply(incs[incs != i1], function(i2) {
      tmp$x <- tmp[,i2]
      M <- lm(y ~ x, w=n, data=tmp)
      return(pmin(rng[2], pmax(rng[1], coef(M)[1] + tmp$x * coef(M)[2])))
    })
    return(apply(yhats, 1, mean, na.rm=TRUE))
  })
  for (i in incs) {
    ok <- is.na(marg[,i])
    marg[ok, i] <- inc_yhat[ok, i]
  }

  # convert to percents
  combs <- rbind(
    expand.grid(prefix=c("occ", "ind"), suffix=c("female", "male"), stringsAsFactors=FALSE), 
    expand.grid(prefix="emp", suffix=c("female_married", "female_single", "male_married", "male_single"), stringsAsFactors=FALSE))
  registerDoMC(30)
  raw <- foreach (i_comb = 1:nrow(combs)) %dopar% {
    prefix <- combs$prefix[i_comb]
    suffix <- combs$suffix[i_comb]
    X <- marg[,grep(paste0(prefix, "__.+_", suffix, "$"), colnames(marg), value=TRUE)]
    X <- X / rowSums(X)
    return(X)
  }
  out <- data.frame(
    marg[, c("state", "fips", grep("total", incs, invert=TRUE, value=TRUE))], 
    bind_cols(raw), 
    stringsAsFactors=FALSE)

  return(out)
}

