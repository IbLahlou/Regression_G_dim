mySummary <- function(X_tr, y_tr, beta) {
  # num samples
  n <- nrow(X_tr)
  # num features
  p <- ncol(X_tr)
  residual_variance <- sum((y_tr - X_tr %*% beta)^2) / (n - p)
  standard_errors <- sqrt(diag(residual_variance * solve(t(X_tr) %*% X_tr)))
  
  t_values <- beta / standard_errors
  p_values <- 2 * pt(-abs(t_values), df = n - p)
  
  result <- data.frame(beta = beta, se = standard_errors, tval = t_values, pval = p_values)
  rownames(result) <- colnames(X_tr)
  
  return(result)
}
