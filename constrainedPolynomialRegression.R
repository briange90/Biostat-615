constrainedPolynomialRegression <- function(p, y) {
  n <- length(y)
  X <- matrix(1, nrow = n, ncol = 2)
##Calculate X when only beta and beta1 exist
  for (i in 1:n) {
    i_n <- i / n
    sum_X <- 0
    sum_X <- sum((i_n^(1:p)) / factorial(1:p))
    X[i, 2] <- sum_X
  }
  print(X)
##Calculate XtX within O(p)
  XtX <- matrix(0, ncol(X), ncol(X))
  for (i in 1:n) {
    XtX[1, 1] <- XtX[1, 1] + X[i, 1] * X[i, 1]
    XtX[1, 2] <- XtX[1, 2] + X[i, 1] * X[i, 2]
    XtX[2, 2] <- XtX[2, 2] + X[i, 2] * X[i, 2]
  }
  XtX[2, 1] <- XtX[1, 2]
##Calculate the inverse of XtX
  det<-XtX[1]*XtX[4]-XtX[2]*XtX[3]
  inverse<-1/det*matrix(c(XtX[4],-XtX[2],-XtX[3],XtX[1]),nrow=2)
##Calculate Xty
  Xty <- numeric(ncol(X))
  for (j in 1:ncol(X)) {
    Xty[j] <- sum(X[, j] * y)
  }
  beta_hat <- numeric(ncol(X))
##Calculate beta_hat
  for (i in 1:ncol(X)) {
    beta_hat[i] <- inverse[i, 1] * Xty[1] + inverse[i, 2] * Xty[2]
  }
  beta<-rep(0,p+1)
  beta[1:2]<-beta_hat
  for (j in 2:p){
    beta[j+1]<-beta[j]/j
    
  }
  return(beta)
}

