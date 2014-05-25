

# predict using univariate methods
unipred <- function(f, fun = NULL) {
  fun <- match.fun(fun)
  f1 <- ts(f, start = c(1980, 1), end = c(2009, 12), frequency = 12)
  estdate <- as.Date(rownames(estdata))
  T <- estdate[1:(m + ph)]
  f1.pred = forecast(fun(f1), h = 12)
  a <- as.numeric(f1.pred$mean)
}

uni_arima <- apply(mdbdata, 2, function(i) unipred(i, fun = auto.arima))
uni_ets <- apply(mdbdata, 2, function(i) unipred(i, fun = ets))
uni_tbats <- apply(mdbdata, 2, function(i) unipred(i, fun = tbats))
uni_bats <- apply(mdbdata, 2, function(i) unipred(i, fun = bats))
uni_nnet <- apply(mdbdata, 2, function(i) unipred(i, fun = nnetar))
uni_ses <- apply(mdbdata, 2, function(i) unipred(i, fun = function(i) ses(i, h = 12)))
uni_hw <- apply(mdbdata, 2, function(i) unipred(i, fun = function(i) hw(i, h = 12)))
uni_naive <- apply(mdbdata, 2, function(i) unipred(i, fun = function(i) snaive(i, h = 12)))

compresult <- rbind(mixerror(final), mixerror(uni_arima), mixerror(uni_ets), mixerror(uni_tbats), mixerror(uni_bats),
                    mixerror(uni_nnet), mixerror(uni_ses), mixerror(uni_hw), mixerror(uni_naive))

rownames(compresult) <- c("Model-3", "ARIMA", "ETS", "TBATS", "BATS", "NNET", "SES", "HW", "NAIVE")

mixerror <- function(x) {
  err <- x - trues
  me <- mean(err)
  mse <- mean(err^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(err))
  out <- c(me, mse, rmse, mae)
  names(out) <- c("ME", "MSE", "RMSE", "MAE")
  return(out)
}
# True values
trues <- estdata[361:366, 1:26]
plot(predict1, trues)
err <- mixerror(final)
err1 <- mixerror(uni)
err2 <- mixerror(predict1)
