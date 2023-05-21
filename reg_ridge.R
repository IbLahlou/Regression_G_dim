# Ridge Regression with LOO Cross Validation
ridge_loo_regression <- function(X, y, alpha) {
  n <- nrow(X)
  p <- ncol(X)
  
  lst_beta <- matrix(0, nrow = p, ncol = n)
  for (i in 1:n) {
    X_tr <- X[-i, ]
    y_tr <- y[-i]
    p <- ncol(X_tr)
    lambda <- alpha * diag(p)
    
    beta <- solve(t(X_tr) %*% X_tr + lambda) %*% t(X_tr) %*% y_tr
    lst_beta[, i] <- beta
  }
  
  return(lst_beta)
}

# LOO Cross Validation for Ridge Regression
alphas <- 0.01:0.01:10
mse_list <- c()

for (alpha in alphas) {
  lst_beta_ridge_loo <- ridge_loo_regression(X_tr, y_tr, alpha)
  yhat <- t(lst_beta_ridge_loo[-1, ]) %*% t(X_ts)
  mse <- mean((y_ts - yhat) ^ 2) + alpha * sum(lst_beta_ridge_loo[-1, , drop = FALSE]^2)
  mse_list <- c(mse_list, mse)
}

# Best lambda and corresponding beta
lambda <- alphas[which.min(mse_list)]
lst_beta_ridge_loo <- ridge_loo_regression(X_tr, y_tr, lambda)
beta_ridge_loo <- lst_beta_ridge_loo[, -1, drop = FALSE]  # Exclude the first column

# Prediction on the test set
yhat <- X_ts %*% beta_ridge_loo

# Mean Squared Error
mse_ridge_loo <- mean((y_ts - yhat) ^ 2)

# Accuracy
acc_ridge_loo <- (1 - mse_ridge_loo / var(y_ts)) * 100

cat("Statistics:\n")
cat("Beta (Ridge LOO):\n")
print(beta_ridge_loo)
cat("MSE (Ridge LOO):", mse_ridge_loo, "\n")
cat("Accuracy (Ridge LOO):", acc_ridge_loo, "\n")
