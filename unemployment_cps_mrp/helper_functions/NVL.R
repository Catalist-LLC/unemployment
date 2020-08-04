NVL <- function(x, replacement=0) {
  x[is.na(x)] <- replacement
  return(x)
}
