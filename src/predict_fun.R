# Spatio-temporal prediction function
load(file = "./data/init.rda")
load(file = "./data/estdata.rda")

m = 360
s = 26
ph = 12
pred.ind <- estdata[1:(m + ph), 1:s]
a <- pred.ind
a[a != 0] <- 1
pred.ind <- a
STpred <- function(est = est.mdb.model, STmodel = mdb.model,
                   pred.dummy = pred.ind, LUR = list(~1, ~1, ~1)) {
  

  f1 <- STmodel$trend$V1
  f2 <- STmodel$trend$V2
  
#   f1 <- ts(f1, start = c(1980, 1), end = c(2009, 12), frequency = 12)
#   f2 <- ts(f2, start = c(1980, 1), end = c(2009, 12), frequency = 12)

  estdate <- as.Date(rownames(estdata))
  T <- estdate[1:(m + ph)]
  f1.pred = forecast(bats(f1), h = ph)
  f2.pred = forecast(bats(f2),  h = ph)
  f2.pred = suppressWarnings(spline(x = estdate[1:m], 
                                    y = f2, xout = T,
                                    method = "periodic"))$y
  F1 <- c(f1, as.numeric(f1.pred$mean))
  F2 <- f2.pred
  if (is.null(STmodel$ST.list)) {
    pred.dummy <- NULL
  }
  
  # create a new ST a (with st convariate)for prediction
  
  if (!is.null(STmodel$ST.list)) {
    ST.list <- list(ind = pred.ind)
    STmdb_st <- createSTdata(obs, covars, SpatioTemporal = ST.list)
  } else {
    STmdb_st <- createSTdata(obs, covars, SpatioTemporal = NULL)
  }
  
  # updating the trend component
  STmdb_st$trend <- data.frame(F1, F2, date = T)
  if (!is.null(STmodel$ST.list)) {
  mdb.pred.st <- createSTmodel(STmdb_st, LUR = LUR, ST = "ind", cov.beta = cov.beta, 
                               cov.nu = cov.nu, locations = locations, strip = FALSE)
  } else {
    mdb.pred.st <- createSTmodel(STmdb_st, LUR = LUR, ST = NULL, cov.beta = cov.beta, 
                                 cov.nu = cov.nu, locations = locations, strip = FALSE)
  }
#   print(mdb.pred.st)
#   # make sure the trends were not overwritten
#   ts.plot(mdb.pred.st$trend$F1)
#   ts.plot(mdb.pred.st$trend$F2)
#   
#   
#   print(mdb.pred.st)
#   loglikeSTdim(mdb.pred.st)
  # Prediction (with spatio-temporal covariate) in action
  
  pred_sp <- predict(mdb.pred.st, est, type = "f",
                                 pred.var = TRUE)
  return(pred_sp)
}

# par(mfrow = c(2, 1))
# plot(f1, ylab = "First basis function")
# plot(f2, ylab = "Second basis function")
# par(mfrow = c(1, 1))

pred1 <- STpred(LUR = list(~alt, ~alt, ~alt))
pred2 <- STpred(LUR = list(~1, ~1, ~1))
pred3 <- STpred(LUR = list(~1, ~1, ~1))
predict1 <- pred1$EX[361:372, ]
HeatStruct(predict1)
# True values
tvalues <- estdata[361:372, 1:26]
plot(predict1, tvalues)
a <- sum((predict1 - tvalues)^2)/length(tvalues)
