library(quadprog)

# LASSO using quadprog package
lasso_regression <- function(X, y, lambda) {
  n <- nrow(X)
  p <- ncol(X)
  XtX <- t(X) %*% X
  Xty <- t(X) %*% y
  Sign <- as.matrix(expand.grid(rep(list(c(-1, 1)), p)))
  Constraint <- rep(-lambda, 2^p)
  beta_lasso <- solve.QP(Dmat = 2 * XtX, dvec = 2 * Xty, Amat = t(Sign), bvec = Constraint)
  return(beta_lasso$solution)
}

# LASSO Model
lasso_current_model <- function(X_tr, y_tr, X_ts, y_ts, lambda) {
  beta_lasso <- lasso_regression(X_tr, y_tr, lambda)
  yhat <- X_ts %*% beta_lasso
  mse <- mean((y_ts - yhat)^2)
  return(list(beta = beta_lasso, mse = mse))
}

# LOO-based lambda selection for LASSO
loo_lambda_selection <- function(X, y, lambda_list) {
  bmse <- Inf
  blambda <- NULL
  
  for (lambda in lambda_list) {
    mse_sum <- 0
    
    for (i in 1:nrow(X)) {
      X_tr <- X[-i, ]
      y_tr <- y[-i]
      X_ts <- X[i, , drop = FALSE]
      y_ts <- y[i]
      
      result <- lasso_current_model(X_tr, y_tr, X_ts, y_ts, lambda)
      mse_sum <- mse_sum + result$mse
    }
    
    mse_avg <- mse_sum / nrow(X)
    
    if (mse_avg < bmse) {
      bmse <- mse_avg
      blambda <- lambda
    }
  }
  
  return(blambda)
}

# LASSO Model with feature and hyperparameter selection
lasso_feature_selection <- function(X_tr, y_tr, X_ts, y_ts, lambda_list) {
  blambda <- loo_lambda_selection(X_tr, y_tr, lambda_list)
  
  result <- lasso_current_model(X_tr, y_tr, X_ts, y_ts, blambda)
  beta_lasso <- result$beta
  mse <- result$mse
  
  acc <- 1 - mse / var(y_ts)
  
  return(list(beta = beta_lasso, mse = mse, acc = acc))
}

# Example usage
lambda_list <- seq(1, 100, 1)

result <- lasso_feature_selection(X_tr, y_tr, X_ts, y_ts, lambda_list)

beta_lasso <- result$beta
mse_lasso <- result$mse
acc_lasso <- result$acc*100


cat("Beta Coefficients:", beta_lasso, "\n")
cat("MSE:", mse_lasso, "\n")
cat("Accuracy:", acc_lasso, "\n")
