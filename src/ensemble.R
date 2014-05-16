
#' Aggregate monthly zero proportion for a univariate monthly time series
#' 
#' @param vec a numeric vector
zp <- function(vec) {
  n <- length(vec)
  v <- vec
  v[v == 0] <- NA
  n.na <- sum(is.na(v))
  nv <- numeric(12)
  for (i in 1:12) {
    nv[i] <- sum(is.na((v[month == i])))/n
  }
  nv
}

month <- 1 + as.POSIXlt(rownames(mdbdata))$mon

# find the proportin of zeros
pz <- sapply(as.data.frame(mdbdata), zp)
zerop <- 12*pz
rainp <- 1 - zerop
library(mvtsplot)
mvtsplot(rainp, levels = 9,  rowstat = "mean", palette = "OrRd")

funp <- Vectorize(function(prob) rbinom(prob = prob, n = 1, size = 1), "prob")

# Use the first 24 stations

funpp <- function(i) c(t(funp(i)))
ind_rain <- apply(rainp, 2, funpp)
load("./data/ori_sub.rda")
ori <- ori_subdata[1:360, ]
ori$date <- rownames(mdbdata)
id <- cutoff(ori, keep.ID = TRUE)$ID
trydata <- ori[, -27]
try2 <- matrix(NA, 12, 26)
for (i in 1:26) {
  index <- id[[i]]
  v1 <- zp(trydata[, i])
  if (is.null(dim(trydata[, index]))) {
    v2 <- zp(trydata[, index])
  } else {
    v2 <- sapply(trydata[, index], zp)
  }
  if (!is.null(dim(trydata[, index]))) {
    try2[, i] <- rowMeans(cbind(rowMeans(v2), v1))*12
  } else {
    try2[, i] <- rowMeans(cbind(v1, v2))*12
  }
}

rainp3 <- 1 - try2
colnames(rainp3) <- colnames(rainp)
mvtsplot(rainp3, levels = 9,  rowstat = "mean", palette = "OrRd")

mvtsplot(rainp3 - rainp)



