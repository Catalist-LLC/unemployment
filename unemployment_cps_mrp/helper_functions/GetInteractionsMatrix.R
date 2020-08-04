GetInteractionsMatrix <- function(dat, inters, cores=30) {

  registerDoMC(cores)
  raw <- foreach(i_inter = 1:nrow(inters)) %dopar% {
    x <- paste0(dat[,inters$x1[i_inter]], "__", dat[,inters$x2[i_inter]])
    inter_values <- sort(unique(x))
    ix <- lapply(1:length(inter_values), function(i)
      data.frame("row"=which(x == inter_values[i]), "col"=i))
    out <- bind_rows(ix)
    return(list(out=out, inter_values=inter_values))
  }
  out <- lapply(raw, function(i) i$out)
  inter_values <- lapply(raw, function(i) i$inter_values)
  names(inter_values) <- inters$full

  num_cols <- cumsum(sapply(out, function(i) max(i$col)))
  for (i in 2:length(out))
    out[[i]]$col <- out[[i]]$col + num_cols[i - 1]
  out <- bind_rows(out)

  out <- sparseMatrix(i=out$row, j=out$col)
  cn <- lapply(names(inter_values), function(i) paste0(i, "___", inter_values[[i]]))
  colnames(out) <- unlist(cn)
  return(out)

}
