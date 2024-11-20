nearestNeighborInterpolation<-function(X, y, Z){
  m<-nrow(Z)
  p<-ncol(Z)
  n<-nrow(X)
  ##Build g_hat vector to restore g_hat
  g_hat<-numeric(m)
  for (i in 1:m){
##Find n distance between n X rows and each row of Z
    distances <- sqrt(rowSums((X - matrix(Z[i, ], n, p, byrow = TRUE))^2))
##Find the indices corresponding the p+1 smallest distance 
    indices<-order(distances)[1:(p+1)]
    A<-X[indices,]
    b<-y[indices]
    I<-rep(1,p+1)
    A_I<-cbind(I,A)
    beta<-solve(A_I,b)
    g_hat[i]<-beta[1]+sum(Z[i, ]*beta[-1])
  }
  return(g_hat)
}


