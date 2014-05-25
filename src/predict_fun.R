# Spatio-temporal prediction function
# load(file = "./data/init.rda")
load(file = "./data/estdata.rda")

m = 360
s = 26
ph = 6
pred.ind <- estdata[1:(m + ph), 1:s]
a <- pred.ind
a[a != 0] <- 1
pred.ind <- a

load("~/Dropbox/R_dev/STpred/results/estimation3.Rdata")

fuc <- function(i) {
  try <- STpred(pred.dummy = i)
  return(try$EX[361:366, ])
}
pred100 <- mclapply(ind_list, fuc)
finalpred <- Reduce("+", pred100)/100

pred2 <- STpred(pred.dummy = pred.ind)
pred3 <- STpred(LUR = list(~1, ~1, ~1))
final <- pred_sp$EX[361:372, ]
HeatStruct(predict1)
# True values
trues <- estdata[361:372, 1:26]
plot(predict1, trues)
a <- sum((predict1 - tvalues)^2)/length(tvalues)
