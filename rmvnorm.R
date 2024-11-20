rmvnorm<-function(n,p,rho){
  h=seq(1:p)
  l=(h-1)/p
  covariance_matrix=matrix(0,nrow=p,ncol=p)
  for (i in 1:p){
    for (j in 1:p){
      covariance_matrix[i,j]=exp(p*log(rho)*abs(l[i]-l[j])^1.99-abs(cos(l[i]))-abs(cos(l[j])))
    }
  }
  A=t(chol(covariance_matrix))
  z=matrix(rnorm(p*n),p,n)
  x=A%*%z
  max_value=apply(x,2,max)
  E_max=mean(max_value)
  E_sqrt=mean(sqrt(colSums(x^2)))
  prop=sum(x[1,]*x[2,]>0.5*rho)/n
  vector=c(E_max,E_sqrt,prop)
  return (vector)
}

