accurateSumOfBernoulli<-function(p){
  if (length(p) == 1) {
    return(c(1 - p, p))
  }
  midline <- floor(length(p) / 2)
  le_pmf <- accurateSumOfBernoulli(p[1:midline])
  ri_pmf <- accurateSumOfBernoulli(p[(midline+1):length(p)])
  combined_length<-length(le_pmf)+length(ri_pmf)-1
  combined_pmf<-rep(0,combined_length)
  for (i in 1:length(le_pmf)){
      combined_pmf[i:(i+length(ri_pmf)-1)]<-combined_pmf[i:(i+length(ri_pmf)-1)]+le_pmf[i]*ri_pmf
    
  }
  whole_pmf <- pmax(pmin(combined_pmf, 1), 0)
  return(whole_pmf)
}

