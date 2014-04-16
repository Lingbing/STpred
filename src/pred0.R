## prediction using no spatio-temporal covariates
library(plotrix, forecast)

mdb0.ori <- STmdb
ph = 12 # prediction horizon

T <- date.month[1:(m + ph)]

f <- calcSmoothTrends(STmdb, n.basis = 2)$trend
# extrapolating the two basis function to prediction period
f1 <- f$V1
f2 <- f$V2