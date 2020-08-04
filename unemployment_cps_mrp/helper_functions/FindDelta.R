logit <- function(x, digits=5)
  return(-1 * log(1/pmin(1 - 1 * 10^(-1 * digits), pmax(1 * 10^(-1 * digits), x)) - 1))

AbsError <- function(delta, y, n, yhat) {
  y <- pmin(logit(1), pmax(logit(0), y))
  abs((sum(invlogit(logit(y) + delta) * n) / sum(n) - yhat))    
}

FindDelta <- function(y, n, yhat)
  optimize(AbsError, interval=c(-5,5), y, n, yhat)$minimum
