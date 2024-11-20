fastRidgeRegression<-function(X,Y,lambda){
  p<-ncol(X)
  n<-nrow(X)
  ##When n>p, use computation of t(X)X since the dimension of X is n,p
  if (n>p){
    I_p<-diag(1,p,p)
    tra <- numeric(p)
    for (j in 1:p) {
      tra[j] <- sum(X[, j] * Y)
    }
    beta<-solve((t(X)%*%X+lambda*I_p))%*%tra
  } 
  ##When n<p, use computation of Xt(X)
  else if (n<p){
    I_n<-diag(1,n,n)
    Inverse_matrix<-solve(X%*%t(X)+lambda*I_n)%*%Y
    beta<-numeric(p)
    for (j in 1:p){
      beta[j]<-sum(X[,j]*Inverse_matrix)
    }
  }
  ##rounding each elements of beta
  beta<-round(beta)
  ##show all the nonzero value in beta 
  nonzero_indices <- which(beta!= 0)
  result <- data.frame(index = nonzero_indices, beta = beta[nonzero_indices])
  return(result)
}
