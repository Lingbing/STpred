# ready for prediction
# load prelinimary results
source("./src/initial.R")
library(cutoffR)
library(forecast)




# Extrapolate (predict) the basis functions (here, two: f1 and f2)
f1 <- STmdb$trend$V1
f2 <- STmdb$trend$V2
# prediction horizon
ph = 12

# time-points at which to predict, ph months ahead
# ARMA for the first basis, and spline for the second basis
estdate <- as.Date(rownames(estdata))
T <- estdate[1:(m + ph)]
f1.pred = forecast(auto.arima(f1), h = ph)
f2.pred = spline(x = date.month[1:m], y = f2, xout = T, method = "periodic")$y
F1 <- c(f1, as.numeric(f1.pred$mean))
F2 <- f2.pred

#
pred.ind <- estdata[1:(m + ph), 1:s]
a <- pred.ind
a[a != 0] <- 1
pred.ind <- a


# create a new ST data (with st convariate)for prediction
STmdb_p <- STmdb
ST.list <- list(ind = pred.ind)
STmdb_st <- createSTdata(obs, covars, SpatioTemporal = ST.list)

# updating the trend component
STmdb_st$trend <- data.frame(F1, F2, date = T)
print(STmdb_st)
mdb.pred.st <- createSTmodel(STmdb_st, LUR = LUR, ST = "ind", cov.beta = cov.beta, 
                             cov.nu = cov.nu, locations = locations, strip = FALSE)
print(mdb.pred.st)
# make sure the trends were not overwritten
ts.plot(mdb.pred.st$trend$F1)
ts.plot(mdb.pred.st$trend$F2)

# 
print(mdb.pred.st)
loglikeSTdim(mdb.pred.st)
# Prediction (with spatio-temporal covariate) in action

system.time(pred_sp <- predict(mdb.pred.st, est.mdb.model, type = "f",
                               pred.var = TRUE))


print(pred_sp)
plot(pred_sp)
pred_sp <- pred2
# study the prediction results --------------------------------------------

# define a plot function
pred.plot <- function(i, pred = pred_sp) {
#   par(mfrow = c(2, 1), mar = c(1, 1, 1.5, 1.5))
  plot(pred, ID = i, STmodel = mdb.pred.st, pred.var = TRUE)
  plot(estdata[(m + 1):(m + ph), i], col = "steelblue", type = "l", lty = 1,
       lwd = 2)
  lines(pred$EX[(m + 1):(m + ph), i], col = 2, type = "l", lty = 2,
        lwd = 2)
  legend("topright", legend = c("Predicted", "Observed"), 
         col = c("red", "steelblue"), lty = c(2, 1),
         lwd = c(2, 2))
#   par(mfrow = c(1, 1))
}
par(mfrow = c(2, 1), mar = c(1, 1, 1.5, 1.5))
plot(pred_sp, ID = 7, STmodel = mdb.pred.st, pred.var = TRUE)
plot(pred_sp$EX[(m + 1):(m + ph), 1], col = 2, type = "l", lty = 2, lwd = 2,
     ylim = c(0, 5))
lines(estdata[(m + 1):(m + ph), 2], col = "gold", type = "l", lty = 1, 
      lwd = 1.5)
plot(pred_sp, ID = 1, STmodel = mdb.pred.st, pred.var = TRUE)

plot(pred_sp, ID = 10, STmodel = mdb.pred.st, pred.var = TRUE)
plot(pred_sp, ID = 17, STmodel = mdb.pred.st, pred.var = TRUE)
plot(pred_sp, ID = 17, STmodel = mdb.pred.st, pred.var = TRUE, lwd = 2)
plot(pred_sp, ID = 17, pred.type = "EX.mu", col = "green", add = TRUE, lwd = 2)
plot(pred_sp, ID = 17, pred.type = "EX.mu.beta", col = "blue", add = TRUE, lwd = 2)


sapply(1:26, function(i) pred.plot(i))


par(mfrow = c(4, 1))
pred.plot(i = "X041011")
pred.plot(i = "X042003")
par(mfrow = c(1, 1))