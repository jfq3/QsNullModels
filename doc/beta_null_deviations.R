## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
suppressMessages(library(vegan))
data("dune")
comm <- dune
comm <- comm[1:5, ]
comm <- comm[ , colSums(comm)>0]
comm <- comm[ , 1:8]
comm

## -----------------------------------------------------------------------------
x1 <- nullmodel(comm, "c0_ind")
x2 <- simulate(x1, nsim = 999)
x2

## -----------------------------------------------------------------------------
x2[ , , 1]

## -----------------------------------------------------------------------------
colSums(comm)
colSums(x2[ , , 1])

## -----------------------------------------------------------------------------
sum(comm)
sum(x2[ , , 1])

sum(colSums(comm)>0)
sum(colSums(x2[ , , 1])>0)

## -----------------------------------------------------------------------------
all(apply(x2, 3, colSums)>0)

## -----------------------------------------------------------------------------
rowSums(comm)
rowSums(x2[ , , 1])

## -----------------------------------------------------------------------------
bray.obs <- mean(vegdist(comm, "bray"))
bray.nulls <- apply(x2, 3, vegdist, "bray")
bray.exptd <- mean(bray.nulls)
bray.expt.sd <- sd(bray.nulls)
ses <- (bray.obs - bray.exptd)/bray.expt.sd
bray.obs
bray.exptd
bray.expt.sd
ses

## -----------------------------------------------------------------------------
hist(bray.nulls, main = "Bray-nulls Distribution")
abline(v = bray.obs, col = "red")
# Find the probablility that the observed mean bray distance
# is not significantly different from the expected mean bray distance.
rank.1 <- rank(c(bray.obs, bray.nulls))[1]
p.val <- rank.1/(length(bray.nulls)+1)
p.val

## -----------------------------------------------------------------------------
mean.bray <- function(x) mean(vegdist(x, "bray"))
rslt <- oecosimu(comm, mean.bray, method = "c0_ind", nsimul = 999)
rslt

