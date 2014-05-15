## cross-validation

# create the CV structure defining 10 different CV-groups
I.cv <- createCV(mdb.model, groups = 10, min.dist = 0.1)
table(I.cv)
I.col <- sapply(split(mdb.model$obs$ID, I.cv),unique)
print(I.col)
I.col <- apply(sapply(I.col, function(x) mdb.model$locations$ID
                      %in% x), 1, function(x) if(sum(x)==1) which(x) else 0)

##Plot the locations, colour coded by CV-grouping
# find out which location belongs to which cv group
plot(mdb.model$locations$long, mdb.model$locations$lat,
     pch = 23 + floor(I.col/max(I.col) + .5), bg=I.col,
     xlab="Longitude", ylab="Latitude")


# cv estimation

mdb.cv <- estimateCV(mdb.model, x.init, I.cv)

# study the cv results
print(mdb.cv)
mdb.cv$par.all

## boxplot of the different estimates from the cv
par(mfrow = c(1, 1), mar = c(12, 2.5, 2, .5), las = 2)
boxplot(mdb.cv, plot.type = "all", las = 2)
points(est.mdb.model$res.best$par.all$par, pch = 4, col = 2)

# cv predicition
set.seed(1234)
pred.cv <- predictCV(mdb.model, mdb.cv, I.cv, LTA = TRUE)
print(pred.cv)
names(pred.cv)
# plot observations with CV-predictions and 95% prediction intervals
par(mfcol = c(4, 1), mar = c(2.5, 2.5, 2, .5))
plot(pred.cv, ID = 7)
plot(pred.cv, ID = 6)
plot(pred.cv, ID = 12)
plot(pred.cv, ID = 17)

## plot predicted vs. observed
par(mfcol = c(1, 1), mar = c(4.5, 4.5, 2, 2))
plot(pred.cv$pred.obs$obs, pred.cv$pred.obs$EX, xlab = "observations",
     ylab = "Predictions", pch = 19, cex = .1, col = mdb.model$obs$idx)
abline(0, 1)
# 
summary(pred.cv, lta = T)