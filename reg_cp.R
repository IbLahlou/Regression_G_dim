# 1. PCA Regression function
pca_regression <- function(X_tr, y_tr, X_ts, y_ts, opt_cmp ) {
  covX <- cov(X_tr)
  eig <- eigen(covX)
  
  eigval <- eig$values
  eigvect <- eig$vectors
  
  s_idx <- order(eigval, decreasing = TRUE)
  seigval <- eigval[s_idx]
  seigvect <- eigvect[, s_idx]
  
  QI <- cumsum(eigval) / sum(eigval)
  
  # Ajouter une autre composante principal , pour un modèle un peu plus pertinant
  cmp <- opt_cmp + 1
  scmp <- seigvect[, 1:cmp]
  
  X_tr_p <- X_tr %*% scmp
  beta_cp <- solve(t(X_tr_p) %*% X_tr_p) %*% t(X_tr_p) %*% y_tr
  
  X_ts_p <- X_ts %*% scmp
  yhat <- X_ts_p %*% beta_cp
  
  # 2.3. MSE and Accuracy
  
  mse <- mean((y_ts - yhat)^2)
  acc <- (1 - mse) / var(y_ts)
  
  
  return(list(
    beta = beta_cp,
    mse = mse,
    accuracy = acc
  ))
}

for(i in 1:7){
  # Example usage
  result <- pca_regression(X_tr, y_tr, X_ts, y_ts, opt_cmp = i)
  
  beta_cp <- result$beta
  mse_cp <- result$mse
  acc_cp <- result$accuracy*100
  
  #4.Statistics
  cat("Beta Coefficients:", beta_cp, "\n")
  cat("MSE:", mse_cp, "\n")
  cat("Accuracy:", acc_cp , "%\n")
  cat("---------------------------------------------------------------------------------------------------------------------------------\n")
}