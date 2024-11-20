solveMatrixEquation <- function(A, B, C) {
  n <- nrow(A)
  Schur_A <- Schur(A)
  Schur_B <- Schur(B)
  Q_A <- Schur_A$Q
  T_A <- Schur_A$T
  Q_B <- Schur_B$Q
  T_B <- Schur_B$T
  C_tilde <- t(Q_A) %*% C %*% Q_B
  I <- Diagonal(n)
  K_A <- kronecker(I, T_A, make.dimnames = FALSE)
  K_B <- kronecker(t(T_B), I, make.dimnames = FALSE)
  K <- K_A + K_B
  vec_C_tilde <- as.vector(C_tilde)
  vec_X_tilde <- solve(K, vec_C_tilde)
  X_tilde <- matrix(vec_X_tilde, n, n)
  X <- Q_A %*% X_tilde %*% t(Q_B)
  X <- round(X)
  X_sparse <- as(X, "TsparseMatrix")
  return(X_sparse)
}




