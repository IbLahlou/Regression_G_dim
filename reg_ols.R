source("featureSelection.R")

# 1 OLS MODEL
ols_model <- function(X_tr,y_tr){
beta <- solve(t(X_tr) %*% X_tr) %*% t(X_tr) %*% y_tr
return(beta)
}


# 2 OLS MEAN SQUARED ERROR
mse_ols <- function(y, yhat) {
  mse <- mean((y - yhat)^2)
  return(mse)
}

# 3 FEATURE SELECTION 
result <- feature_selection(X_tr, y_tr, X_ts, y_ts, ols_model, mse_ols)

cat("Selected Features:", result$sel_features, "\n")
cat("Beta Coefficients:", result$beta, "\n")
cat("MSE:", result$mse, "\n")
cat("Accuracy:", result$accuracy, "\n")
