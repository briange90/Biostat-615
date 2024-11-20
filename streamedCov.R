#' streamedCov(x, y): calculate covariance between vectors accurately while
#' streaming. The function must read each element of
#' x and y without reusing them after an iteration.
#' @param x - A numeric vector
#' @param y - Another numeric vector
#' @return covariance between x and y (with n-1 as denominator)
streamedCov <- function(x, y) {
  ## ensure that two input vectors are the same size
  stopifnot(length(x) == length(y))
  ####################################################
  n <- length(x)
  mean_x <- x[1]
  mean_y <- y[1]
  S_xy <- 0
  ###################################################
  for(i in 2:n) {
    xi = x[i]
    yi = y[i]
    S_xy<-S_xy+(i-1)/i*(xi-mean_x)*(yi-mean_y)
    mean_x <- mean_x + (xi-mean_x) / i
    mean_y <- mean_y + (yi-mean_y) / i
    ###############################################
    ## YOUR PRIMARY IMPLEMENTATION GOES HERE
    ## YOU MAY ONLY USE xi, yi, and OTHER VARIABLES
    ## YOU DEFINED ABOVE.
    ## YOU MAY NOT USE OTHER ELEMENTS OF x and y
    ## EXCEPT FOR xi=x[i] and yi=y[i]
    ## (e.g. NEVER USE mean(x) or sum(y))
    ###############################################
  }
  ####################################################
  ## YOU MAY RETURN CALCULATED COVARIANCE HERE
  covariance<-S_xy/(n-1)
  return(covariance)
  ###################################################
}

