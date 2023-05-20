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

X_tr <- cbind(1, X_tr)
X_ts <- cbind(1, X_ts)


# Visualisation de la corrélation entre les features du dataset
#cor(X_tr_scaled)
#library(heatmaply)
#heatmaply(cor(X_tr_scaled), node_type = "scatter" , point_size_mat = cor(X_tr_scaled) )
