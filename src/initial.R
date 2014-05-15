
##  setting up for modeling --------------------------------------------
# loading required packages and data
library(cutoff, SpatioTemporal)
library(ggplot2, mvtsplot)
library(reshape2)
load("./data/station.rda")
data(hqmr.data)
data(date.month)

##

## cube-transform the data and attaching date (month) tags
hqmr.cube <- data.frame((hqmr.data[, 1:78])^(1/3))
rownames(hqmr.cube) <- date.month
hqmr.cube$date <- date.month

# impute missing values by the CUTOFF method
data <- Cut(hqmr.cube)
# cut the date to the recent 30 years from 1980 - 2010
estdata <- data[(1200 - 371):1200, ]

m = 360 # number of months for modeling 
s = 26 # number of stations for modeling


subdata <- estdata[1:m, c(1:s)]
mdbdata <- subdata
rm(subdata)

# normally, two is the best
SVD.cv <- SVDsmoothCV(mdbdata, 1:5)
plot(SVD.cv)

# create a indicator matrix for zeros and non-zeros
# non-zeros are denoted by 1
zeroind <- mdbdata
zeroind[zeroind != 0]  <- 1

# creating "obs"
obs <- melt(t(mdbdata))[, c(3, 2, 1)] 
names(obs) <- c("obs", "date", "ID")
obs$date <- as.Date(as.character(obs$date))

# creating "covars" 
# calculating the xy coordinates
st <- station[1:s, ]
xcoor <- 111.13 * st$long * cos(34.021 * pi/180)
ycoor <- 111.13 * st$lat

# creating covars, with ID , x-y coordinates and alt as the only LUR covariate
covars <- data.frame(ID = colnames(mdbdata), x = xcoor, y = ycoor, 
                     long = st$long, lat = st$lat, alt = st$elav)


# create ST data object
# ST.list <- list(ind = zeroind)
ST.list <- NULL
STmdb <- createSTdata(obs, covars, n.basis = 2, SpatioTemporal = ST.list)
print(STmdb)
# fd <- calcSmoothTrends(STmdb, n.basis = 2)$trend
# STmdb$trend <- fd
# plot(fd$date, STmdb$trend[, 1], ylim = c(min(STmdb$trend[, 2]), max(STmdb$trend[, 2])), type = "l")
# lines(1:m, STmdb$trend[, 2], type = "l", lty = 2, col = 2)

LUR <- list(~alt, ~alt, ~alt)
# LUR <- processLUR(STmdb, LUR.in = NULL)
# exponential covariance for the beta-fileds and no nugget effect
cov.beta <- list(covf = "exp", nugget = FALSE)
# cov.beta <- list(covf = "gaussian", nugget = FALSE)
# exponential covariance for the nu-fileds and including nugget effect
# random.effect = FALSE, specifies the we want to use a constant nugget
cov.nu <- list(covf = "exp", nugget = TRUE, random.effect = FALSE)

# define which location to use
locations <- list(coords = c("x","y"), long.lat = c("long","lat"))


# create ST model object
# mdb.model <- createSTmodel(STmdb, ST = "ind", LUR = LUR, cov.beta = cov.beta, cov.nu = cov.nu, locations = locations)# cov.beta2 <- list(covf = c("exp", "exp2", "iid"), nugget = c(FALSE, FALSE, TRUE))
mdb.model <- createSTmodel(STmdb, ST = NULL, LUR = LUR, cov.beta = cov.beta, cov.nu = cov.nu, locations = locations)# cov.beta2 <- list(covf = c("exp", "exp2", "iid"), nugget = c(FALSE, FALSE, TRUE))

# mdb.model2 <- updateCovf(mdb.model, cov.beta = cov.beta2)
# print(mdb.model2)

## Parameter Estimation
# take a look at the important dimensions of the model
model.dim <- loglikeSTdim(mdb.model)

# setting up the initial parameter values for optimization
x.init <- cbind(rep(2, model.dim$nparam.cov), c(rep(c(1, -1), model.dim$m+1), 1))
loglikeSTnames(mdb.model, all = FALSE)
# add names to the initial values
rownames(x.init) <- loglikeSTnames(mdb.model, all = FALSE)
# names(x.init) <- loglikeSTnames(mdb.model, all = FALSE)
x.init


## double check we have the right basis functions
plot(1:m, mdb.model$trend[, 1], ylim = c(min(STmdb$trend[, 2]), max(STmdb$trend[, 2])), type = "l")
lines(1:m, mdb.model$trend[, 2], type = "l", lty = 2, col = 2)

