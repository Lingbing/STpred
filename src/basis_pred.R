# plot the obs at some locations with the fitted smooth trends
ph = 12
par(mfrow=c(4,1), mar=c(2.5,2.5,2,.5))
plot(STmdb, "obs", ID=3, col=c("black", "red"))
plot(STmdb, "obs", ID=18, col=c("black", "red"))

##also plot the residuals

plot(STmdb, "res", ID=3, col=c("black", "red"))
plot(STmdb, "res", ID=18, col=c("black", "red"))
par(mfrow=c(1, 1))

# acf plot of the residuals, its not working now...

par(mfcol=c(2,2),mar=c(2.5,2.5,3,.5))
plot(STmdb, "pacf", ID=1)
plot(STmdb, "acf", ID=5)
plot(STmdb, "acf", ID=13)
plot(STmdb, "acf", ID=18)

f1.pred1 <- forecast(hw(f1), h = ph)
accuracy(f1.pred1)
f1.pred2 <- forecast(auto.arima(f1), h = ph)
accuracy(f1.pred2)
f1.pred3 <- forecast(ets(f1), h = ph)
accuracy(f1.pred3)
f1.pred4 <- forecast(tbats(f1), h = ph)
accuracy(f1.pred4)
f1.pred5 <- forecast(bats(f1), h = ph)
accuracy(f1.pred5)
f1.pred6 <- forecast(stl(f1, s.window = "periodic"), h = ph)
accuracy(f1.pred6)

accu1 <- rbind(accuracy(f1.pred1), accuracy(f1.pred2),accuracy(f1.pred3),
              accuracy(f1.pred4), accuracy(f1.pred5), accuracy(f1.pred6))
rownames(accu1) <- c("HW", "ARIMA", "ETS", "tbats", "bats", "stl")
accu11 <- melt(accu1)
par(mfrow = c(6, 1))
plot(f1.pred1, fcol = 2, flty = 2, shadecols = "grey",  main = "HW")
plot(f1.pred2, fcol = 2, flty = 2, shadecols = "grey", main = "ARIMA")
plot(f1.pred3, fcol = 2, flty = 2, shadecols = "grey",  main = "ETS")
plot(f1.pred4, fcol = 2, flty = 2, shadecols = "grey",  main = "TBATS")
plot(f1.pred5, fcol = 2, flty = 2, shadecols = "grey", main = "BATS")
plot(f1.pred6, fcol = 2, flty = 2, shadecols = "grey",  main = "STL")
par(mfrow = c(1, 1))




f2.pred1 <- forecast(hw(f2), h = ph)
accuracy(f2.pred1)
f2.pred2 <- forecast(auto.arima(f2), h = ph)
accuracy(f2.pred2)
f2.pred3 <- forecast(ets(f2), h = ph)
accuracy(f2.pred3)
f2.pred4 <- forecast(tbats(f2), h = ph)
accuracy(f2.pred4)
f2.pred5 <- forecast(bats(f2), h = ph)
accuracy(f2.pred5)
f2.pred6 <- forecast(stl(f2, s.window = "periodic"), h = ph)
accuracy(f2.pred6)

accu2 <- rbind(accuracy(f2.pred1), accuracy(f2.pred2),accuracy(f2.pred3),
               accuracy(f2.pred4), accuracy(f2.pred5), accuracy(f2.pred6))
rownames(accu2) <- c("HW", "ARIMA", "ETS", "tbats", "bats", "stl")
accu22 <- melt(accu2)
par(mfrow = c(6, 1))
plot(f2.pred1, fcol = 2, flty = 2, shadecols = "grey",  main = "HW")
plot(f2.pred2, fcol = 2, flty = 2, shadecols = "grey", main = "ARIMA")
plot(f2.pred3, fcol = 2, flty = 2, shadecols = "grey",  main = "ETS")
plot(f2.pred4, fcol = 2, flty = 2, shadecols = "grey",  main = "TBATS")
plot(f2.pred5, fcol = 2, flty = 2, shadecols = "grey", main = "BATS")
plot(f2.pred6, fcol = 2, flty = 2, shadecols = "grey",  main = "STL")
par(mfrow = c(1, 1))

par(mfrow = c(2, 1))
plot(f1.pred2, fcol = 2, flty = 2, shadecols = "grey", 
     main = "Predictions from ARIMA(3, 0, 5)(1, 0, 2)[12]")
plot(f2.pred2, fcol = 2, flty = 2, shadecols = "grey",
     main = "Predictions from ARIMA(2, 0, 4)(2, 0, 2)[12]")
par(mfrow = c(1, 1))