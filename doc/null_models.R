## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
suppressPackageStartupMessages(library(QsNullModels))
data(com)
com
rowSums(com)
colSums(com)
sum(colSums(com)>0)
sum(com)

## -----------------------------------------------------------------------------
set.seed(1)
a <- Sim4(com)
a
rowSums(a)
colSums(a)
sum(colSums(a)>0)
sum(a)

## -----------------------------------------------------------------------------
set.seed(1)
w <- c(30,0,3,2,3,1,3,3,2,1)
b <- randomize_by_rows(com, w, rename=TRUE)
b
rowSums(b)
colSums(b)
sum(colSums(b)>0)
sum(b)

## -----------------------------------------------------------------------------
set.seed(1)
c <- Sim2(com)
c
rowSums(c)
colSums(c)
sum(colSums(c)>0)
sum(c)

## -----------------------------------------------------------------------------
library(picante)
set.seed(1)
d <- randomizeMatrix(com, null.model="independentswap")
d
rowSums(d)
colSums(d)
sum(colSums(d)>0)
sum(d)

e <- randomizeMatrix(com, null.model="trialswap")
e
rowSums(e)
colSums(e)
sum(colSums(e)>0)
sum(e)

## ---- echo = FALSE, fig.align='left', fig.cap="Figure 1. Analysis of echinoid biogeographic data. Dotted outlines separate the sampling areas: the numbers indicate the INDEX OF SIMILARITY of each area with respect to an arbitrarily chosen reference area (marked with a red X : Pacific coast of Central America). Numbers enclosed in circles indicate significant similarity to the reference area; numbers in boxes indicate significant dissimilarity to the reference area. Intermediate values of the INDEX are contoured. From Raup & Crick, 1979."----
knitr::include_graphics("echinoids.jpg")

## -----------------------------------------------------------------------------
set.seed(1)
raup_crick(com, classic_metric=TRUE)

## -----------------------------------------------------------------------------
set.seed(1)
my_rc(com)

## -----------------------------------------------------------------------------
set.seed(1)
rc.t <- raupcrick(com, chase=TRUE)
rc.t
set.seed(1)
rc.f <- raupcrick(com, chase=FALSE)
rc.f
(rc.t + rc.f)/2

## -----------------------------------------------------------------------------
data(dune)
# Chase's R script:
system.time(replicate(100,(raup_crick(dune, classic_metric=TRUE))))
# My function:
system.time(replicate(100, my_rc(dune)))
# Average vegan fucntion with chase=T & F
f <- function(x) {raupcrick(x, chase=TRUE)+raupcrick(x, chase=FALSE)/2}
system.time(replicate(100, f(dune)))

## -----------------------------------------------------------------------------
vegdist(com, "raup")
set.seed(1)
raupcrick(com, null="r0", nsimul=999, chase=FALSE)

## -----------------------------------------------------------------------------
data(dune)
data(dune.env)
com.hp <- dune[dune.env$Use=="Haypastu", ]
com.hp <- decostand(com.hp, "pa")
mean.jac <- function(x) mean(vegdist(x, "jac", binary=TRUE))
set.seed(1)
rslt <- oecosimu(com.hp, mean.jac, nsimul=999, "r1")
rslt

## -----------------------------------------------------------------------------
set.seed(1)
rslt <- oecosimu(com.hp, mean.jac, nsimul=999, "quasiswap")
rslt

## -----------------------------------------------------------------------------
dune.pa <- decostand(dune, "pa")
weights <- colSums(dune.pa)
set.seed(1)
rslt <- perm_disper(com.hp, w=weights)
rslt
anova(rslt)

## ---- echo=FALSE, fig.align='left', fig.cap="Figure 2. Results of the `perm_disper` test for the Hay Pasture samples using Jaccard distances and `gamma.method=total'"----
boxplot(rslt)

## ---- echo=FALSE, fig.align="left", fig.cap="Figure 3. Ordination based on the `perm_disper` test for the Hay Pasture samples using Jaccard distances and `gamma.method=total`. For the null model, one sample appears an outlier."----
plot(rslt)

## -----------------------------------------------------------------------------
weights <- colSums(com.hp)
set.seed(1)
rslt <- perm_disper(com.hp, w=weights)
rslt
anova(rslt)

## -----------------------------------------------------------------------------
library(vegan)
data(sipoo)
set.seed(1)
rslt <- oecosimu(sipoo, shared_spp, nsimul=999, "r1")
rslt.sum <- format_oecosimu_vector(rslt, sipoo)
simberloff_sum(rslt.sum, alpha=0.05)

## -----------------------------------------------------------------------------
set.seed(1)
rslt <- oecosimu(sipoo, mean_shared_spp, nsimul=999, "r1")
rslt

