euclideanDistanceThreshold<-function(X,Y,thres){
  X_square=colSums(X^2)
  Y_square=colSums(Y^2)
  D_square=outer(X_square,Y_square,"+")-2*t(X)%*%Y
  num_thres=sum(D_square<=thres^2)
  return(num_thres)
}


