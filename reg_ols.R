# Régression OLS à partir de zéro
# x: matrice de prédicteurs (n x p)
# y: vecteur de réponse (n x 1)
# renvoie: vecteur de coefficients (p x 1)

ols <- function(x, y) {
  # Ajouter une colonne de 1 pour l'interception
  x <- cbind(1, x)
  
  # Calculer la matrice de Gram
  g <- t(x) %*% x
  
  # Calculer l'inverse de la matrice de Gram
  ginv <- solve(g)
  
  # Calculer les coefficients
  beta <- ginv %*% t(x) %*% y
  
  return(beta)
}