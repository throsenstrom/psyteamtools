#' Pairwise Direction Dependence Measures with Lag Included
#'
#' @param X1 A matrix with two time 1 variables as columns
#' @param X0 A matrix with two time 0 variables as columns
#' @param approximation This defaults to the “mxnt” estimator
#' but may be supplemented with an optional argument
#' `approximation = "mxnt"` (alternatives are mxnt, kgv, tanh, skew,
#' rskew, hsic; see the original paper for details).
#'
#' @return A list of results
#' @export
lagged_direction_dependence <- function(X1,X0, approximation = "mxnt"){
  X1 <- scale(X1); X0 <- scale(X0)
  mf <- lm(X1 ~ X0[,1] + X0[,2])
  M <- t( mf$coefficients[2:3,] )
  # mxnt
  Txy <- direction_dependence(mf$residuals[,1], mf$residuals[,2],
                              approximation = approximation)
  if (Txy >= 0){
    B0 <- matrix(c(0,0,lm(mf$residuals[,2] ~ mf$residuals[,1])$coefficients[2],0),
                 2,2,byrow=T)
  } else {
    B0 <- matrix(c(0,lm(mf$residuals[,1] ~ mf$residuals[,2])$coefficients[2],0,0),
                 2,2,byrow=T)
  }
  B1 <- (diag(2) - B0) %*% M
  return( list(B0=B0, B1=B1, Txy_I=Txy, Txy_L = abs(B1[2,1]) - abs(B1[1,2])) )
}
