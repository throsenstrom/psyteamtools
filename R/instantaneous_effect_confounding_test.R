#' HSIC-based Test for Confounding
#'
#' @description HSIC-based test for confounding in the
#' estimated instantaneous effects.
#'
#' @param X1 A matrix with two time 1 variables as columns
#' @param X0 A matrix with two time 0 variables as columns
#' @param dontprint Set to TRUE if you don't want a print out
#'
#' @return Results of the test
#' @export
instantaneous_effect_confounding_test <- function(X1, X0, dontprint = FALSE){
  X1 <- scale(X1); X0 <- scale(X0)
  mf <- lm(X1 ~ X0[,1] + X0[,2])
  # Test direction V1 -> V2
  mf2 <- lm(mf$residuals[,2] ~ mf$residuals[,1])
  v1_to_v2 <- dHSIC::dhsic.test(mf$residuals[,1],mf2$residuals)
  # Test direction V2 -> V1
  mf2 <- lm(mf$residuals[,1] ~ mf$residuals[,2])
  v2_to_v1 <- dHSIC::dhsic.test(mf$residuals[,2],mf2$residuals)
  if (!dontprint){
    print("Test results for direction dependence V1 -> V2:")
    print(v1_to_v2)
    print("Test results for direction dependence V2 -> V1:")
    print(v2_to_v1)
    print("INTERPRETATION:")
    if ((v1_to_v2$p.value <= 0.05/2) & (v2_to_v1$p.value <= 0.05/2)){
      print("Confounding or reprocal directionality was present at family-wise significance level 0.05.")
    } else {
      print("No confounding or reprocal directionality present at family-wise significance level 0.05.")
    }
  }
  return(invisible(list(v1_to_v2 = v1_to_v2, v2_to_v1 = v2_to_v1)))
}
