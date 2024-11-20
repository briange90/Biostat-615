stationaryDistribution <- function(P, tol = 1e-13, max_iter = 10000) {
  n <- nrow(P)
  pi <- matrix(c(1,rep(0,n-1)),nrow=n,ncol=1)
  for (i in 1:max_iter) {
    pi_new <- P%*%pi
    if (max(abs(pi_new - pi)) < tol) {
      return(pi_new)
      break
    }
    pi<-pi_new
  }
  return(pi_new)
}

