# Régression à composante principale 



pca <- function(x, k) {
  # Centrer les données
  x <- scale(x, center = TRUE, scale = FALSE)
  
  # Calculer la matrice de covariance
  covmat <- cov(x)
  
  # Calculer les valeurs propres et les vecteurs propres
  eig <- eigen(covmat)
  values <- eig$values
  vectors <- eig$vectors
  
  # Sélectionner les k premiers vecteurs propres
  vectors <- vectors[, 1:k]
  
  # Calculer les scores des composantes principales
  scores <- x %*% vectors
  
  return(scores)
}