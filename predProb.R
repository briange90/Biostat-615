predProb<-function(x){
  p<-length(x)
  ex_beta<-rep(0,5)
  for (k in 1:5){
    beta_k0<-2^(-k)
    beta_sum<-beta_k0
    for (j in 1:p){
      beta_kj<-2^abs(k-j)
      beta_sum<-beta_sum+beta_kj*x[j]}
    ex_beta[k] <- beta_sum
  }
  exp_beta<-exp(ex_beta-max(ex_beta))
  sum_exp_beta<-sum(exp_beta)
  pb <- exp_beta / sum_exp_beta
    
    
  return(pb)
}

