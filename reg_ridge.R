# Ridge Regression function
ridge_regression <- function(X, y, alpha) {
  n <- nrow(X)
  p <- ncol(X)
  lambda <- alpha * diag(p)
  beta <- solve(t(X) %*% X + lambda) %*% t(X) %*% y
  return(beta)
}

# 1 RIDGE MODEL

# Grid Search for the best lambda
alphas <- 0.01:0.01:10
mse_list <- c()
for (alpha in alphas) {
  beta_ridge <- ridge_regression(X_tr, y_tr, alpha)
  yhat <- X_ts %*% beta_ridge
  mse <- mean((y_ts - yhat) ^ 2) + alpha * sum(beta_ridge[-1] ^ 2)
  mse_list <- c(mse_list, mse)
}

# beta ridge coefficient
lambda <- alphas[which.min(mse_list)]
beta_ridge <- ridge_regression(X_tr, y_tr, lambda)

# Prediction on the test set
yhat <- X_ts %*% beta_ridge

# 2 Ridge MEAN SQUARED ERROR
ridge_mse <- function(y_ts, yhat) {
  mse <- mean((y_ts - yhat) ^ 2)
  return(mse)
}
mse_ridge <- ridge_mse(y_ts, yhat)

# 3 ACCURACY
ridge_accuracy <- function(y_ts, yhat) {
  acc <- 1 - mse_ridge / var(y_ts)
  return(acc)
}
acc_ridge <- ridge_accuracy(y_ts, yhat) * 100

cat("Statistics:\n")
mySummary(X_tr, y_tr, beta_ridge)
cat("MSE:", mse_ridge, "\n")
cat("Accuracy:", acc_ridge, "\n")
