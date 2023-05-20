# Elastic net
elastic_net <- function(X, Y, lambda_1, lambda_2, iter){
  beta_elastic_net = matrix(rep(0, ncol(X)), nrow = ncol(X), ncol = 1, byrow = FALSE)
  
  for (i in 1:iter) {
    for (j in 1:ncol(X)) {
      X_BETA = matrix(rep(0, nrow(X)), nrow = nrow(X), ncol = 1, byrow = FALSE)
      
      for (k in 1:ncol(X)) {
        if (k != j) {
          X_BETA = X_BETA + X[, k] * beta_elastic_net[k]
        }
      }
      
      R = t(X[, j]) %*% (Y - X_BETA)
      cont = (1 - (lambda_1 / (2 * abs(R))))
      
      if (cont > 0) {
        beta_elastic_net[j] = (R / ((t(X[, j]) %*% X[, j]) + lambda_2)) * cont
      } else {
        beta_elastic_net[j] = 0
      }
    }
  }
  
  return(beta_elastic_net)
}

# Grid search for Elastic Net
grid_search_elastic_net <- function(X, Y, lambda1, lambda2, iter) {
  error <- Inf
  bparams <- list()
  
  for (l1 in lambda1) {
    for (l2 in lambda2) {
      if (l1 != 0 && l2 != 0) {
        beta_elastic_net <- elastic_net(X, Y, l1, l2, iter)
        mse <- sum((Y - X %*% beta_elastic_net)^2) / nrow(X)
        
        if (mse < error) {
          error <- mse
          bparams$lambda1 <- l1
          bparams$lambda2 <- l2
          bparams$beta_elastic_net <- beta_elastic_net
        }
      }
    }
  }
  
  bparams$error <- error
  return(bparams)
}

# Example usage
iter <- 10
lambda1 <- seq(0, 10, 0.27)
lambda2 <- seq(0, 10, 0.24)

# Le process prends bq de temps pour le bons choix des paramétre c'est pour cela que j'ai opté pour le grid search une methode moins performante malheursement
bparams <- grid_search_elastic_net(X_tr, y_tr, lambda1, lambda2, iter)
blambda1 <- bparams$lambda1
blambda2 <- bparams$lambda2
beta_elnet <- bparams$beta_elastic_net

mse_elnet <- sum((y_ts - X_ts %*% beta_elnet)^2) / nrow(X_ts)
acc_elnet <- ols_accuracy(y_ts, X_ts %*% beta_elnet)

# Print the results
cat("Optimal lambda1:", blambda1, "\n")
cat("Optimal lambda2:", blambda2, "\n")
cat("Mean Squared error:", mse_elnet, "\n")
cat("Accuracy_elnet:", acc_elnet, "\n")
cat("Optimal beta_elnet:", beta_elnet, "\n")
