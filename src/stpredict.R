#' Spatio-temporal prediction
STpred <- function(est = est.mdb.model, STmodel = mdb.model,
                   pred.dummy = NULL, LUR = list(~1, ~1, ~1)) {
  
  
  f1 <- STmodel$trend$V1
  f2 <- STmodel$trend$V2
  
  f1 <- ts(f1, start = c(1980, 1), end = c(2009, 12), frequency = 12)
  f2 <- ts(f2, start = c(1980, 1), end = c(2009, 12), frequency = 12)
  
  estdate <- as.Date(rownames(estdata))
  T <- estdate[1:(m + ph)]
  f1.pred = forecast(tbats(f1), h = ph)
  f2.pred = forecast(tbats(f2),  h = ph)
#   f2.pred = suppressWarnings(spline(x = estdate[1:m], 
#                                       y = f2, xout = T,
#                                       method = "periodic"))$y
  F1 <- c(f1, as.numeric(f1.pred$mean))
  F2 <- c(f2, as.numeric(f2.pred$mean))
#   F2 <- f2.pred
#   F2 <- c(f2, as.numeric(f2.pred$mean))
  if (is.null(STmodel$ST.list)) {
    pred.dummy <- NULL
  }
  
  # create a new ST a (with st convariate)for prediction
  
  if (!is.null(STmodel$ST.list)) {
    ST.list <- list(ind = pred.dummy)
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