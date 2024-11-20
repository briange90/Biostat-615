rejectionSampling <- function(n, a, b) {
  pi_density <- function(x, a, b) {
    return(exp(-x^2) * x^(a - 1) * (1 - x)^(b - 1))
  }
  envelope_density <- function(x, a, b) {
    return(dbeta(x, a, b))
  }
  M <- optimize(function(x) pi_density(x, a, b) / envelope_density(x, a, b), interval = c(0, 1), maximum = TRUE)$objective
  accepted <- numeric(n)
  attempted <- 0
  count <- 0
  while (count < n) {
    x <- rbeta(n - count, a, b) ##Generate enough samples based on the remaining number needed
    u <- runif(n - count) ##Generate the same number with x
    attempted <- attempted + (n - count) 
    accept <- u <= pi_density(x, a, b) / (M * envelope_density(x, a, b)) 
    accepted_samples <- x[accept] 
    num_accepted <- length(accepted_samples)
    if (num_accepted > 0) {
      accepted[(count + 1):(count + num_accepted)] <- accepted_samples
      count <- count + num_accepted
    }
  }
  return(list(attempted = attempted, accepted = n, values = accepted))
}