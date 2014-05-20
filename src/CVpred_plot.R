####################################
## plot CV-pred. for a given site ##
####################################
# load data
# model-1
load("~/Dropbox/R_dev/STpred/results/estimation1.Rda")
model1 <- mdb.model
mdb.cv1 <- mdb.cv
est.cv1 <- est.mdb.model
pred.cv1 <- pred.cv
# model-2
load("~/Dropbox/R_dev/STpred/results/estimation_2.Rda")
model2 <- mdb.model
mdb.cv2 <- mdb.cv
est.cv2 <- est.mdb.model
pred.cv2 <- pred.cv
# model-3
load("~/Dropbox/R_dev/STpred/results/estimation3.Rdata")
model3 <- mdb.model
mdb.cv3 <- mdb.cv
est.cv3 <- est.mdb.model
pred.cv3 <- pred.cv
# model-4
load("~/Dropbox/R_dev/STpred/results/estimation4.Rdata")
model4 <- mdb.model
mdb.cv4 <- mdb.cv
est.cv4 <- est.mdb.model
pred.cv4 <- pred.cv


pred.cv <- pred.cv3
par(mar=c(3.3, 3.3, 1.5, 1), mgp = c(2, 1, 0))
layout(matrix(c(1, 1, 2, 2, 3, 4), 3, 2, byrow = TRUE))
plot(pred.cv, ID = "X024511", xlab= "", ylab = "Rainfall",
     col = c("firebrick", "gray39", "azure2"),
     main = "Predictions for X024511", lty = c(1, NA), lwd = 2,
     pch = c(NA, 19), cex = 0.75, ylim = c(-0.5, 7.5))
plot(pred.cv, ID = "X024511", pred.type = "EX.mu", lty = 3, lwd = 3, 
     col = "darkviolet", add = TRUE)
plot(pred.cv, ID = "X024511", pred.type = "EX.mu.beta", lty = 4, lwd = 3, 
     col = "deepskyblue3", add = TRUE)
legend("topright", c("Observations", "Predictions", "Contribution from beta",
                     "Contribution from mean", "95% CI"), bty = "n",
       lty = c(NA, 1, 4, 3, NA), lwd = c(NA, 2, 3, 3, NA), 
       pch = c(19, NA, NA, NA, 15), pt.cex = c(.75, NA, NA, NA, 2.5), 
       col = c("firebrick", "gray39", "deepskyblue3", "goldenrod", "azure2"))
plot(pred.cv, ID = "X043026", xlab= "", ylab = "Rainfall",
     col = c("firebrick", "gray39", "azure2"),
     main = "Predictions for X043026", lty = c(1, NA), lwd = 2,
     pch = c(NA, 19), cex = 0.75, ylim = c(-0.5, 8.5))
plot(pred.cv, ID = "X043026", pred.type = "EX.mu", lty = 3, lwd = 3, 
     col = "darkviolet", add = TRUE)
plot(pred.cv, ID = "X043026", pred.type = "EX.mu.beta", lty = 4, lwd = 3, 
     col = "deepskyblue3", add = TRUE)
legend("topright", c("Observations", "Predictions", "Contribution from beta",
                     "Contribution from mean", "95% CI"), bty = "n",
       lty = c(NA, 1, 4, 3, NA), lwd = c(NA, 2, 3, 3, NA), 
       pch = c(19, NA, NA, NA, 15), pt.cex = c(.75, NA, NA, NA, 2.5), 
       col = c("firebrick", "gray39", "deepskyblue3", "goldenrod", "azure2"))

plot(pred.cv, "obs", ID = "all", pch = c(19, NA), cex = .25, lty = c(NA, 2),
     col = c("ID", "black", "azure2"), xlab = "observations", 
     ylab = "Predictions", main = "All stations")

i.season <- as.factor(as.POSIXlt(pred.cv$pred.obs$date)$mon + 1)
levels(i.season) <- c(rep("Summer", 2), rep("Fall", 3), rep("Winter", 3), 
                      rep("Spring", 3), "Summer")
qqnorm(pred.cv, norm = TRUE, main = "Normalised residuals", col = i.season)
legend("bottomright", legend = as.character(levels(i.season)),
       pch = 1, col = 1:nlevels(i.season), bty = "n")