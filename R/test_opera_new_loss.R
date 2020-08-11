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


# Data 
data(electric_load)
attach(electric_load)

# train and test set 
idx_data_test <- 620:nrow(electric_load)
data_train <- electric_load[-idx_data_test, ]
data_test <- electric_load[idx_data_test, ]

# figure 
plot(Load, type = "l", main = "The electric load")

# building the expert forecast 
# generalized additive model 
library(mgcv)
gam.fit <- gam(Load~ s(IPI)+ s(Temp) + s(Time, k=3) + 
                 s(Load1) + as.factor(NumWeek), data = data_train)
gam.forecast <- predict(gam.fit, newdata = data_test)

# medium term generalized additive model followed by an autoregressive short -term correction
# medium term model
medium.fit <- gam(Load ~ s(Time,k=3) + s(NumWeek) + s(Temp) + s(IPI), data = data_train)
electric_load$Medium <- c(predict(medium.fit), predict(medium.fit, newdata = data_test))
electric_load$Residuals <- electric_load$Load - electric_load$Medium

# autoregressive correction
ar.forecast <- numeric(length(idx_data_test))
for (i in seq(idx_data_test)) {
  ar.fit <- ar(electric_load$Residuals[1:(idx_data_test[i] - 1)])
  ar.forecast[i] <- as.numeric(predict(ar.fit)$pred) + electric_load$Medium[idx_data_test[i]]
}

# gradient boosting model 
library(caret)
gbm.fit <- train(Load ~ IPI + IPI_CVS + Temp + Temp1 + Time + Load1 + NumWeek, 
                 data = data_train, method = "gbm")
gbm.forecast <- predict(gbm.fit, newdata = data_test)

# plot of the true data and the forecasts 
Y <- data_test$Load
X <- cbind(gam.forecast, ar.forecast, gbm.forecast)
matplot(cbind(X,Y), type = "l", col = 1:16, ylab = "Weekly Load", xlab = "week", main= "Expert forecasts and observations")

# initialising the algorithm 
MLpol0 <- mixture(model = "MLpol", loss.type = "linexPos", loss.gradient = TRUE)

MLpol_model <- MLpol0
for (i in 1:length(Y)) {
  MLpol_model <- predict(MLpol_model, newexperts = X[i,], newY = Y[i])
}

summary(MLpol_model)

