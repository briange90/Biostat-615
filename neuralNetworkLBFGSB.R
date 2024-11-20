neuralNetworkLBFGSB <- function(p, df) {
  GeLu <- function(x) {
    return(x * pnorm(x))
  }
  
  ##Derivative of derivative of GeLu to alpha_p+j
  dGeLu <- function(x) {
    return(pnorm(x) + x * dnorm(x))
  }
  ##Define F(alpha)
  objective_function <- function(alpha, X, Y, p) {
    n <- length(Y)
    predictions <- alpha[1] + rowSums(
      sapply(1:p, function(j) {
        alpha[j + 1] * GeLu(alpha[p + j + 1] + alpha[2 * p + j + 1] * X)
      })
    )
    return(mean((Y - predictions)^2))  # Mean squared error
  }
  ##Define the gradient function of every alpha
  gradient_function <- function(alpha, X, Y, p) {
    n <- length(Y)
    gradients <- numeric(length(alpha))
    predictions <- alpha[1] + rowSums(
      sapply(1:p, function(j) {
        alpha[j + 1] * GeLu(alpha[p + j + 1] + alpha[2 * p + j + 1] * X)
      })
    )
    residuals <- Y - predictions
    ##Derivative to alpha0
    gradients[1] <- -2 * mean(residuals)
    ##Derivatives to the other alpha
    for (j in 1:p) {
      GeLu_input <- alpha[p + j + 1] + alpha[2 * p + j + 1] * X
      dGeLu_val <- dGeLu(GeLu_input)
      gradients[j + 1] <- -2 * mean(residuals * GeLu(GeLu_input))
      gradients[p + j + 1] <- -2 * mean(residuals * alpha[j + 1] * dGeLu_val)
      gradients[2 * p + j + 1] <- -2 * mean(residuals * alpha[j + 1] * dGeLu_val * X)
    }
    return(gradients)
  }
  ##using optim to solve the question
  alpha_init <- runif(3 * p + 1, -1, 1)
  X <- df$X
  Y <- df$Y
  n <- length(Y)
  result <- optim(
    par = alpha_init,
    fn = objective_function,
    gr = gradient_function,
    X = X,
    Y = Y,
    p = p,
    method = "L-BFGS-B",
    lower = rep(-10, 3 * p + 1),
    upper = rep(10, 3 * p + 1),
    control = list(maxit = 10000)
  )
  return(result)
}



