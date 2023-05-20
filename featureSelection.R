# Features Selection Function
feature_selection <- function(X_tr, y_tr, X_ts, y_ts, model_func, mse_func) {
  best_mse <- Inf
  best_features <- NULL
  
  n_features <- ncol(X_tr)  # Number of features
  
  # Loop over all possible feature combinations
  for (num in 1:n_features) {
    combinations <- combn(ncol(X_tr), num)
    
    # Iterate over each combination
    for (i in 1:ncol(combinations)) {
      sel_features <- combinations[, i]
      
      # Calculate beta and yhat using the specified model function
      beta <- model_func(X_tr[, sel_features], y_tr)
      yhat <- X_ts[, sel_features] %*% beta
      
      # Calculate MSE using the specified MSE function
      mse <- mse_func(y_ts, yhat)
      
      if (mse < best_mse) {
        best_mse <- mse
        best_features <- sel_features
      }
    }
  }
  
  # Get the names of the best features
  feature_names <- colnames(X_tr)
  sel_features_names <- feature_names[best_features]
  
  # Calculate beta, yhat, MSE, and accuracy using the specified model function
  beta <- model_func(X_tr[, best_features], y_tr)
  yhat <- X_ts[, best_features] %*% beta
  
  mse <- mse_func(y_ts, yhat)
  acc <- (1 - mse / var(y_ts)) * 100
  
  # Return the selected feature names, beta coefficients, MSE, and accuracy
  result <- list(
    sel_features = sel_features_names,
    beta = beta,
    mse = mse,
    accuracy = acc
  )
  return(result)
}

