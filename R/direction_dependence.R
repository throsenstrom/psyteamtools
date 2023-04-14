#' Pairwise Direction of Dependence Inference
#'
#' @description Computes the direction of dependence between two
#' continuous-valued variables `x` and `y`.
#'
#' @param x A numeric vector (one variable)
#' @param y A numeric vector (the other variable)
#' @param approximation
#'
#' @return A positive scalar value when x causes y and negative when
#' other way around. Near-zero values for no direction (use bootstrapping).
#' @export
direction_dependence <- function(x, y, approximation = "mxnt"){
  Xtmp <- scale( na.omit(cbind(x, y)) )
  x <- Xtmp[,1]; y <- Xtmp[,2]
  # Maximum entropy approximation to likelihood ratio
  if (approximation == "mxnt"){
    mentappr <- function(z){
      # standardize
      z <- z - mean(z)
      zsd <- sd(z)
      z <- z/zsd
      # Constants we need
      k1 <- 36/(8*sqrt(3)-9)
      gamma <- 0.37457
      k2 <- 79.047
      gaussianEntropy <- log(2*pi)/2+1/2
      # Negentropy
      negentropy <- k2*(mean(log(cosh(z)))-gamma)^2+k1*mean(z*exp(-z^2/2))^2
      entropy <- gaussianEntropy - negentropy + log(zsd);
      return(entropy)
    }
    ratio <- mentappr(y) + mentappr(lm(x~y)$residuals) -
      mentappr(x) - mentappr(lm(y~x)$residuals)
    return(ratio)
  }
  # Kernel generalized variance approx of mutual information
  if (approximation == "kgv"){
    rx <- lm(y ~ x)$residuals
    ry <- lm(x ~ y)$residuals
    return( kgv_contrast( cbind(y, ry)) - kgv_contrast( cbind(x, rx)) )
  }
  # Tanh (kurtosis) based approximation to likelihood ratio
  if (approximation == "tanh"){
    return( cor(x,y)*mean(x*tanh(y) - tanh(x)*y) )
  }
  # Simple skewness based measure
  if (approximation == "skew"){
    x <- x*sign(moments::skewness(x)); y <- y*sign(moments::skewness(y))
    return( cor(x,y)*mean(x^2*y - x*y^2) )
  }
  # Skewness-based likelihood-ratio approximation
  if (approximation == "rskew"){
    return( cor(x,y)*mean( log(cosh(pmax(x,0)))*y - x*log(cosh(pmax(y,0))) ) )
  }
  if (approximation == "hsic"){
    rx <- lm(y ~ x)$residuals
    ry <- lm(x ~ y)$residuals
    return( dHSIC::dhsic( list(y, ry))$dHSIC -
              dHSIC::dhsic( list(x, rx))$dHSIC )
  }
}
