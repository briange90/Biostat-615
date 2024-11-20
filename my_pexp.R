my_pexp<-function(x, rate, lower.tail, log.p){
  h=x*rate
  if(lower.tail==TRUE){
    if(log.p==FALSE){
      if(h<1e-10){
        return(h-h^2/2+h^3/6-h^4/24+h^5/120)
      }
      else{
        return (1-exp(-h))}
    }
    else if(log.p==TRUE){
      if(h<1e-10){
        
        return(log(h-h^2/2+h^3/6-h^4/24+h^5/120))
      }
      else if(h>20){
        return(-exp(-h)-(exp(-h)^2)/2-(exp(-h))^3/3-(exp(-h))^4/4-(exp(-h))^5/5)
      }
      else{
        return (log(1-exp(-h)))
      }
    }}
  else if(lower.tail==FALSE){
    if(log.p==FALSE){
      return (exp(-h))
    }
    else if(log.p==TRUE){
      return (-h)
    }}
}
x= c(50)
rate= c(1)
lower.tail= c(TRUE)
log.p= c(TRUE)
my_pexp(x, rate, lower.tail , log.p)
