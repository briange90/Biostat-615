fastSumOfBernoulli <- function(p) {
  if (length(p) == 1) {
    return(c(1 - p, p))
  }
  midline <- floor(length(p) / 2)
  le_pmf <- fastSumOfBernoulli(p[1:midline])
  ri_pmf <- fastSumOfBernoulli(p[(midline+1):length(p)])
  combined_pmf <- convolve(le_pmf, rev(ri_pmf), type = "open")
  whole_pmf <- pmax(pmin(combined_pmf, 1), 0)
  return(whole_pmf)
}

