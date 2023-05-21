# Importing the Data

pros=read.table("http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/prostate.data")
#pros

# Diviser le Dataset en variable cible et feaure

X <- as.matrix(pros[, -9])
y <- pros$lpsa

#X
#y

# Trouver les index auquelle l'observation est celle du test et du train

tr_idx  <- which(pros$train == TRUE)
ts_idx  <- which(pros$train == FALSE)

X_tr <- X[tr_idx,-9]
y_tr <- y[tr_idx]

X_ts <- X[ts_idx,-9]
y_ts <- y[ts_idx]


# Visualisation de la corrélation entre les features du dataset
#install.packages("heatmaply)
library(heatmaply)
heatmaply(cor(X_tr), node_type = "scatter" , point_size_mat = cor(X_tr) )


# Appeler chaque script individuellement
source("mySummary.R")
source("reg_ols.R")
source("reg_cp.R")
#source("reg_ridge.R") Erreur en calcul matriciel LOOCV
source("reg_ridge_gs.R")
source("reg_lasso.R")
#source("reg_ElasticNet.R") Erreur en calcul matriciel LOOCV
source("reg_ElasticNet_gs.R")

# Un Graphique visualisant les meilleurs features a sélectionner
coeff <- data.frame(beta_ols, beta_cp, beta_ridge_gs, beta_lasso, beta_elnet_gs)
rownames(coeff) <- colnames(pros)
heatmaply(as.matrix(coeff), main = "Features Selection")


