# Spatio-temporal prediction function

STpred <- function(est = est.mdb.model, STmodel = mdb.model,
                   m = 360, ph = 12, pred.ind = NULL, LUR = NULL) {
  estdate <- STmodel$trend$date
  if (!is.null(LUR)) {
    LUR <- list(~alt, ~alt, ~alt)
  } else {
    LUR <- list(~1, ~1, ~1)
  }
  f1 <- STmdb$trend$V1
  f2 <- STmdb$trend$V2
  ####
  
  T <- estdate[1:(m + ph)]
  f1.pred = forecast(auto.arima(f1), h = ph)
  f2.pred = suppressWarnings(spline(x = date.month[1:m], 
                                    y = f2, xout = T,
                                    method = "periodic"))$y
  F1 <- c(f1, as.numeric(f1.pred$mean))
  F2 <- f2.pred
  if (!is.null(STmodel$ST.list)) {
    pred.ind = as.data.frame(pred.ind)
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
  
  pred_sp <- predict(mdb.pred.st, est.mdb.model, type = "f",
                                 pred.var = TRUE)
return(pred_sp)
}