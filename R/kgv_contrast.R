#' Kernel Generalized Variance Constrast (KGV) Function
#'
#' @description This helper function computes the KGV contrast for
#' kernel-based causal direction dependence estimation in the
#' direction_dependence and lagged_direction_dependence functions.
#'
#' @param x A data matrix
#' @param sigma A default value
#' @param eta A default value
#' @param kappaval A default value
#'
#' @return A scalar value for the KGV contrast of the data.
#' @export
kgv_contrast <- function(x,sigma=ifelse(nrow(x)<=1000,2/100,2/1000),
                         eta=1e-04, kappaval=1e-02){
  centerpartial <- function(G1){
    return(G1 - matrix(rep(colMeans(G1),nrow(G1)),ncol = ncol(G1), byrow = T) )
  }
  N <- nrow(x) # number of data points
  m <- ncol(x) # number of components
  Us <- Drs <- vector("list", m)
  sizes <- rep(0,m)
  for (i in 1:m){
    G <- KernelICA::incomplete_cholesky(x[,i], kernel = "gaus", sigma = sigma)
    G <- centerpartial(G$L[G$perm,])
    # Regularization
    a <- eigen(t(G) %*% G, symmetric = TRUE)
    indexes <- which( (a$values >= N*eta) & sapply(a$values, is.numeric) )
    if (length(indexes)==0){ indexes <- 1 }
    D <- a$values[indexes]
    V <- G %*% (a$vectors[,indexes] %*% diag(sqrt(1/D)))
    Us[[i]] <- V
    Drs[[i]] <- D/(N*kappaval+D)
    sizes[i] <- length(D)
  }
  # Calculated Rkappa
  Rkappa <- diag(sum(sizes))
  starts <- cumsum(c(1,sizes))[-(m+1)]
  for (i in 2:m){
    for (j in 1:(i-1)){
      newbottom <- diag(Drs[[i]], nrow = sizes[i]) %*%
        (t(Us[[i]]) %*% Us[[j]]) %*% diag(Drs[[j]], nrow = sizes[j])
      Rkappa[starts[i]:(starts[i]+sizes[i]-1),starts[j]:(starts[j]+sizes[j]-1)] <-
        newbottom
      Rkappa[starts[j]:(starts[j]+sizes[j]-1),starts[i]:(starts[i]+sizes[i]-1)] <-
        t(newbottom)
    }
  }
  return(-0.5*determinant(Rkappa, logarithm = TRUE)$modulus)
}
