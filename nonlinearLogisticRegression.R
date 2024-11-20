nonlinearLogisticRegression <- function(df) {
  X <- df$X
  Y <- df$Y
  Z <- df$Z
  log_likelihood <- function(a) {
    p <- exp(a * X - a^2 * Z^2) / (1 + exp(a * X - a^2 * Z^2))
    return(sum(Y * log(p) + (1 - Y) * log(1 - p)))
  }
  score_function <- function(a) {
    p <- exp(a * X - a^2 * Z^2) / (1 + exp(a * X - a^2 * Z^2))
    return(sum((Y - p) * (X-2*a*Z^2)))
  }
  a_values <- seq(-5, 5, length.out = 100)
  log_likelihood_values <- sapply(a_values, log_likelihood)
  initial_indices <- order(log_likelihood_values, decreasing = TRUE)[1:2]
  an <- a_values[initial_indices[1]]
  an_1 <- a_values[initial_indices[2]]
  max_iter <- 1000
  tol <- 1e-10
  for (i in 1:max_iter) {
    f_an <- score_function(an)
    f_an_1 <- score_function(an_1)
    an_2 <- an - f_an * (an - an_1) / (f_an - f_an_1)
    if (abs(score_function(an_2)) < tol) {
      break
    }
    an_1 <- an
    an <- an_2
  }
  return(round(an_2, 5))
}


