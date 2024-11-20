piecewiseLinearInterpolation=function(X, Y, Z){
  f.hat <- numeric(length(Z))
  f.true <- numeric(length(Z))
  sort_index<-order(X)
  X<-X[sort_index]
  Y<-Y[sort_index]
  n <- length(X)
  for (i in 1:length(Z)) {
    if (Z[i] < min(X)) {
      f.hat[i] <- Y[1] + (Y[2] - Y[1]) * (Z[i] - X[1]) / (X[2] - X[1])
    } else if (Z[i] >= max(X)) {
      f.hat[i] <- Y[n] + (Y[n] - Y[n - 1]) * (Z[i] - X[n]) / (X[n] - X[n - 1])
    } else {
    idx <- which(X <= Z[i])
    idx_2<-which(X>=Z[i])
    matched_values <- idx[(idx + 1) %in% idx_2]
    matched_values<- max(matched_values)
    f.hat[i] <- Y[matched_values] + (Y[matched_values + 1] - Y[matched_values]) * (Z[i] - X[matched_values]) / (X[matched_values + 1] - X[matched_values])
    }
  }
  X_qua<-matrix(0,nrow=3,ncol=length(X))
  X_qua[1,]<-X**2
  X_qua[2,]<-X 
  X_qua[3,]<-rep(1,length(X))
  X_qua<-t(X_qua)
  coefficient<-solve(t(X_qua)%*%X_qua)%*%t(X_qua)%*%Y
  a<-coefficient[1]
  b<-coefficient[2]
  c<-coefficient[3]
  f.true<-a*Z**2+b*Z+c
  return(data.frame(f.hat = f.hat, f.true = f.true))
}



