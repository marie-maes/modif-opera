# When it's ready, turn it into package
#library(devtools)
#devtools::install_github("marie-maes/modif-opera")
#library(opera)



source("~/Desktop/modif-opera/R/mixture.R")
source("~/Desktop/modif-opera/R/predict-mixture.R")
source("~/Desktop/modif-opera/R/predictReal-mixture.R")
source("~/Desktop/modif-opera/R/MLPol.R")
source("~/Desktop/modif-opera/R/lossPred.R")
source("~/Desktop/modif-opera/R/loss.R")


MLpoltest <- mixture(model = "MLpol", loss.type = "linexNeg")
X_matrix <- as.matrix(X[, 2:7])
activation_matrix <- ifelse(is.na(X_matrix), 0, 1)
actual_qty_vec <- actual_qty$actual_qty
MLpol_model <- MLpoltest
for (i in 1:length(actual_qty_vec)) {
  MLpol_model <- predict(MLpol_model, newexperts = X_matrix[i,], newY = actual_qty_vec[i], online = TRUE,
                   awake = activation_matrix[i,])
}


MLpoltest2 <- mixture(model = "MLpol", loss.type = "square")
X_matrix <- as.matrix(X[, 2:7])
activation_matrix <- ifelse(is.na(X_matrix), 0, 1)
actual_qty_vec <- actual_qty$actual_qty
MLpol_model2 <- MLpoltest2
for (i in 1:length(actual_qty_vec)) {
  MLpol_model2 <- predict(MLpol_model2, newexperts = X_matrix[i,], newY = actual_qty_vec[i], online = TRUE,
                         awake = activation_matrix[i,])
}