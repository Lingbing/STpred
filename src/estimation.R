# Estimation
source("./src/initial.R")
system.time(est.mdb.model <- estimate(mdb.model, x.init, hessian.all = TRUE))
print(est.mdb.model)

##compare the estimated parameters for the two starting points
est.mdb.model$summary$par.all
##and values of the likelihood (and convergence info)
est.mdb.model$summary$status

##extract the estimated parameters and approximate uncertainties
x <- coef(est.mdb.model)

##compare estimated parameters
##plot the estimated parameters with uncertainties
par(mfrow = c(1, 1), mar = c(13.5, 2.5, .5, .5))
with(x, plot(par, ylim = range(c(par - 1.96*sd, par + 1.96*sd)),
             xlab = "", xaxt = "n"))
with(x, points(par - 1.96*sd, pch=3))
with(x, points(par + 1.96*sd, pch=3))

abline(h=0, col="grey")
##add axis labels
axis(1, 1:length(x$par), rownames(x), las=2)

## MCMC estimation
x <- coef(est.mdb.model)
H <- est.mdb.model$res.best$hessian.all
set.seed(1234)
system.time(MCMC.mdb.model <- MCMC(mdb.model, x$par, N = 5000, Hessian.prop = H))
print(MCMC.mdb.model)
names(MCMC.mdb.model)
summary(MCMC.mdb.model)
plot(MCMC.mdb.model)
# MCMC tracks
par(mfrow=c(5,1),mar=c(2,2,2.5,.5))
plot(MCMC.mdb.model, ylab="", xlab="", type="l")
for(i in c(4,9,13,15)){
  plot(MCMC.mdb.model, i, ylab="", xlab="", type="l")
}
