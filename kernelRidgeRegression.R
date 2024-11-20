kernelRidgeRegression=function(df,lambda,rho,bw){
  x_train<-df$x_train
  x_test<-df$x_test
  y_train<-df$y_train
  y_test<-df$y_test
  n<-length(x_train)
  ##Calculate the kernel
  diff_train<-outer(x_train, x_train, FUN = function(a, b) abs(a - b))
  value<-exp(-rho * (as.matrix(diff_train))^2)
  value[as.matrix(diff_train)>bw]<-0
  K<-sparseMatrix(i=row(value)[value!=0],
                  j=col(value)[value!=0],
                  x=value[value!=0])
  I<-Diagonal(n)
  ##Calculate Beta
  beta_hat<-solve(K+lambda*I,y_train)
  ##Define f with x test and x train
  f_hat<-function(x){
    dist_test<-abs(x-x_train)
    kx<-exp(-rho*dist_test^2)
    kx[dist_test>bw]<-0
    return(sum(beta_hat*kx))
  }
  y_pred<-sapply(x_test,f_hat)
  PMSE<-sum((y_test-y_pred)^2)/length(y_test)
  return (PMSE)
}

