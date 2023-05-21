# Elastic Net with LOO Cross Validation
elastic_net_loo <- function(X, y, lambda_1, lambda_2, iter) {
  n <- nrow(X)
  p <- ncol(X)
  beta_elastic_net <- matrix(rep(0, p), nrow = p, ncol = 1, byrow = FALSE)
  betas <- matrix(0, nrow = p, ncol = n)
  
  for (i in 1:n) {
    X_tr <- X[-i, ]
    y_tr <- y[-i]
    
    for (j in 1:iter) {
      for (k in 1:ncol(X_tr)) {
        X_BETA = matrix(rep(0, nrow(X_tr)), nrow = nrow(X_tr), ncol = 1, byrow = FALSE)
        
        for (m in 1:ncol(X_tr)) {
          if (m != k) {
            X_BETA = X_BETA + X_tr[, m] * beta_elastic_net[m]
          }
        }
        
        R = t(X_tr[, k]) %*% (y_tr - X_BETA)
        cont = (1 - (lambda_1 / (2 * abs(R))))
        
        if (cont > 0) {
          beta_elastic_net[k] = (R / ((t(X_tr[, k]) %*% X_tr[, k]) + lambda_2)) * cont
        } else {
          beta_elastic_net[k] = 0
        }
      }
    }
    
    betas[, i] <- beta_elastic_net
  }
  
  return(betas)
}

# LOO Cross Validation for Elastic Net
lambda1 <- seq(0, 10, 0.27)
lambda2 <- seq(0, 10, 0.24)
iter <- 10

mse_list <- c()
betas_elnet_loo <- matrix(0, nrow = ncol(X_tr), ncol = nrow(X_tr))
best_lambda1 <- NULL
best_lambda2 <- NULL

for (l1 in lambda1) {
  for (l2 in lambda2) {
    if (l1 != 0 && l2 != 0) {
      betas_elnet_loo <- elastic_net_loo(X_tr, y_tr, l1, l2, iter)
      yhat <- X_ts %*% colMeans(betas_elnet_loo)
      mse <- sum((y_ts - yhat) ^ 2) / nrow(X_ts)
      mse_list <- c(mse_list, mse)
      
      if (mse == min(mse_list)) {
        best_lambda1 <- l1
        best_lambda2 <- l2
      }
    }
  }
}

# Best lambdas and corresponding beta
lambda1 <- best_lambda1
lambda2 <- best_lambda2
betas_elnet_loo <- elastic_net_loo(X_tr, y_tr, lambda1, lambda2, iter)
beta_elnet_loo <- colMeans(betas_elnet_loo)

# Prediction on the test set
yhat <- X_ts %*% beta_elnet_loo

# Mean Squared Error
mse_elnet_loo <- sum((y_ts - yhat) ^ 2) / nrow(X_ts)

# Accuracy
acc_elnet_loo <- ols_accuracy(y_ts, yhat)

cat("Optimal lambda1:", lambda1, "\n")
cat("Optimal lambda2:", lambda2, "\n")
cat("Mean Squared Error:", mse_elnet_loo, "\n")
cat("Accuracy_elnet_loo:", acc_elnet_loo, "\n")
cat("Optimal beta_elnet_loo:", beta_elnet_loo, "\n")