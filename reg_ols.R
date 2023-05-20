# 1. OLS MODEL
ols_model <- function(X_tr, y_tr) {
  beta <- solve(t(X_tr) %*% X_tr) %*% t(X_tr) %*% y_tr
  return(beta)
}

# 2. OLS MEAN SQUARED ERROR
ols_mse <- function(y_ts, yhat) {
  mse <- mean((y_ts - yhat) ^ 2)
  return(mse)
}

# 3. OLS ACCURACY
ols_accuracy <- function(y_ts, yhat) {
  acc <- (1 - mean((y_ts - yhat) ^ 2) / var(y_ts)) * 100
  return(acc)
}

# 4. Evaluation
beta_ols <- ols_model(X_tr, y_tr)
yhat <- X_ts %*% beta_ols
mse_ols <- ols_mse(y_ts, yhat)
acc_ols <- ols_accuracy(y_ts, yhat)

cat("Statistics:\n")
cat("beta :", beta_ols ,"\n")
cat("MSE :", mse_ols, "\n")
cat("Accuracy:", acc_ols, "\n")
