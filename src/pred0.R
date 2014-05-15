## prediction using no spatio-temporal covariates
library(plotrix, forecast)
library(forecast)
load("./data/est1.rda")
mdb.ori <- STmdb
ph = 12 # prediction horizon

T <- date.month[1:(m + ph)]

f <- calcSmoothTrends(STmdb, n.basis = 2)$trend
# extrapolating the two basis function to prediction period
f1 <- f$V1
f2 <- f$V2

V1 = spline(x = STmdb$trend$date, y = f1, xout=T, method = "natural")$y
V2 = spline(x = STmdb$trend$date, y = f2, xout=T, method = "periodic")$y
pred.v1 <- V1
pred.v1[1:m] <- NA
pred.v2 <- V2
pred.v2[1:m] <- NA

par(mfrow = c(2, 1), mar = c(4, 4, 2, 2))
plot(f1, type = "l", xlim = c(1, 264), )
lines(pred.v1, type = "l", lty = 2, , col = 2, lwd = 2)
plot(f2, type = "l", xlim = c(1, 264), )
lines(pred.v2, type = "l", lty = 2, , col = 2, lwd = 2)
par(mfrow = c(1, 1))

# try arima
arima.V1 = forecast(auto.arima(f1), h = 24)
stl.f2 <- ts(f2, frequency = 12)
stl.V2 <- forecast(stl(stl.f2, s.window = 12), h = 24)
ets.V2 <- forecast(ets(stl.f2), h = 24)
plot(arima.V1, main = "ARIMA")
plot(stl.V2, main = "STL")
plot(ets.V2, main = "Exponential smoothing state space model")
# arima for V1 and spline for V2

V1 <- c(f1, as.numeric(arima.V1$mean))
V2 <- V2
