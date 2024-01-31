# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: R code for "Regression Discontinuity Designs in Education: A Practitioner's Guide" :::::
# ::: Youmi Suk ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# ::::: Libraries :::::
library(rdrobust)
library(rddensity)
library(rdd)                # for McCrary test (2008)
source('Functions.R')       # read functions for RD plot

# ::::: read data :::::
dat <- read.csv("synNJdata.csv")
dat$childgen <- factor(dat$childgen)
dat$testtyp <- factor(dat$testtyp)
dat$childethnic <- factor(dat$childethnic)
dat$flunch <- factor(dat$flunch)

# structure of data
dim(dat) 
str(dat)
summary(dat)

# ::::: description of variables :::::
# ppvt      ... PPVT score (vocabulary)
# cav       ... centered assignment variable (days from cutoff score: October 1st)
# assign    ... assignment variable according to RD assignment rule
# treat     ... treatment indicator (treatment received)
# childgen  ... child's gender
# testtyp   ... indicator of English (PPVT) and Spanish version (TVIP)
# childethnic . child's ethnicity
# flunch    ... free lunch status

# ::::::::::::::::::::::
# ::::: ITT Effect ::::: ####
# ::::::::::::::::::::::

# RD plot: version 1
rdplot(y = dat$ppvt, x = dat$cav)
  
# RD plot: version 2
BW_mse = rdbwselect(y = dat$ppvt, x = dat$cav, kernel = "tri", p = 1)
par(mar=c(5, 4, 0.5, 2) + 0.1) # c(bottom, left, top, right)
plot.RD(ppvt ~ cav, treat = 'assign', data = dat, kdens = 'triangular', h = BW_mse$bws[1], 
        cex = .3, main = '', xlim=c(-400, 400), ylim=c(0, 120), SE=FALSE,
        xlab = 'Centered Running Variable, Birth Date (Unit: Day)', ylab = 'Vocabulary Test Score')


# ITT effect
rd_ITT <- rdrobust(y = dat$ppvt, x = dat$cav, kernel = 'tri', bwselect = "mserd")
summary(rd_ITT)

# add covariates
cov_mat <- model.matrix( ~ childgen + testtyp + childethnic + flunch + 0, dat)[, -1] 

rd_ITT_covs <- rdrobust(y = dat$ppvt, x = dat$cav, covs =cov_mat ,  kernel = 'tri', h=rd_ITT$bws[1,1])
summary(rd_ITT_covs)

# change the bandwidth to the half size
rd_ITT_halfh <- rdrobust(y = dat$ppvt, x = dat$cav, kernel = 'tri', h = rd_ITT$bws[1,1]/2)
summary(rd_ITT_halfh)

ITT_rslt <- rbind(c(rd_ITT$bws[1,1], rd_ITT$Estimate[1], rd_ITT$Estimate[1]/sd(dat$ppvt), rd_ITT$ci[1, ], rd_ITT$ci[3, ]),
                  c(rd_ITT$bws[1,1], rd_ITT_covs$Estimate[1], rd_ITT_covs$Estimate[1]/sd(dat$ppvt), rd_ITT_covs$ci[1, ], rd_ITT_covs$ci[3, ]),      
                  c(rd_ITT$bws[1,1]/2, rd_ITT_halfh$Estimate[1], rd_ITT_halfh$Estimate[1]/sd(dat$ppvt), rd_ITT_halfh$ci[1, ], rd_ITT_halfh$ci[3, ]))

colnames(ITT_rslt) <- c("Bandwidth", "Estimate", "Effect Size", "CI Lower",  "CI Upper",  "Robust CI Lower",  "Robust CI Upper")
rownames(ITT_rslt) <- c("ITT (Optimal BW)", "Optimal BW w/ Covs", "Half-BW")

round(ITT_rslt, 2)

# :::::::::::::::::::::::
# ::::: LATE Effect ::::: ####
# :::::::::::::::::::::::

# treatment compliance 
with(dat, table(assign, treat))   # compliance table
(19 + 21) / nrow(dat)             # non-compliance rate: 2%

# treatment compliance: within the bandwidth
BW_mse = rdbwselect(y = dat$ppvt, x = dat$cav, fuzzy=dat$treat, kernel = "tri")
with(dat[abs(dat$cav) < BW_mse$bws[1], ], table(assign, treat))   # compliance table
(19 + 8) / nrow(dat)             # non-compliance rate: 1% within the bandwidth

# discontinuity in treatment probabilities: version 1
rdplot(y = dat$treat, x = dat$cav)

# discontinuity in treatment probabilities: version 2
par(mar=c(5, 4, 0.5, 2) + 0.1) # c(bottom, left, top, right)
plot.RD(treat ~ cav, treat = 'assign', data = dat, kdens = 'rectangular', h = BW_mse$bws[1], 
        cex = .3, main = '', xlim=c(-400, 400), SE=FALSE,
        xlab = 'Centered Running Variable, Birth Date (Unit: Day)', ylab = 'Treatmnet Proability')


# LATE effect
rd_LATE <- rdrobust(y = dat$ppvt, x = dat$cav, fuzzy = dat$treat, kernel = 'tri', bwselect = 'mserd')
summary(rd_LATE)

# add covariates
rd_LATE_covs <- rdrobust(y = dat$ppvt, x = dat$cav, fuzzy = dat$treat, covs =cov_mat , kernel = 'tri', h=rd_LATE$bws[1,1])
summary(rd_LATE_covs)

# change the bandwidth to the half size
rd_LATE_halfh <- rdrobust(y = dat$ppvt, x = dat$cav, fuzzy = dat$treat, kernel = 'tri', h = rd_LATE$bws[1,1]/2)
summary(rd_LATE_halfh)

LATE_rslt <- rbind(c(rd_LATE$bws[1,1], rd_LATE$Estimate[1], rd_LATE$Estimate[1]/sd(dat$ppvt), rd_LATE$ci[1, ], rd_LATE$ci[3, ]),
                   c(rd_LATE$bws[1,1], rd_LATE_covs$Estimate[1], rd_LATE_covs$Estimate[1]/sd(dat$ppvt), rd_LATE_covs$ci[1, ], rd_LATE_covs$ci[3, ]),      
                   c(rd_LATE$bws[1,1]/2, rd_LATE_halfh$Estimate[1], rd_LATE_halfh$Estimate[1]/sd(dat$ppvt), rd_LATE_halfh$ci[1, ], rd_LATE_halfh$ci[3, ]))

colnames(LATE_rslt) <- c("Bandwidth", "Estimate", "Effect Size", "CI Lower",  "CI Upper",  "Robust CI Lower",  "Robust CI Upper")
rownames(LATE_rslt) <- c("LATE (Optimal BW)", "Optimal BW w/ Covs", "Half-BW")

round(LATE_rslt, 2)


# ::::::::::::::::::::::::::::
# ::::: Validation Tests ::::: ####
# ::::::::::::::::::::::::::::

# ::: manipulation test
# Cattaneo et al. (2020)
den <- rddensity(X=dat$cav, vce="jackknife", massPoints=FALSE) 
summary(den) 
denplot <- rdplotdensity(den, dat$cav, plotRange=c(-367,367), plotN=50, histFillCol="gray90", histLineCol="gray80",
                         xlabel = "Centered Running Variable, Birth Date (Unit: Day)", ylabel="Density")

# McCrary test (2008)
DCdensity(dat$cav, 0, verbose = FALSE, plot = TRUE, ext.out = TRUE, htest = TRUE)

# ::: covariate imbalance at the cutoff
par(mar=c(5, 4, 1, 2) + 0.1) # c(bottom, left, top, right)
plot.RD(childgen ~ cav, treat = 'assign', data = dat, yaxt="n", linecol='black',
        kdens = 'rectangular', h = 70, cex = .4, main="", ylab="Female", xlab="Centered Running Variable, Birth Date (Unit: Day)", xlim=c(-400, 400))
axis(side=2, at=c(1, 2), labels = c(0, 1))

plot.RD(testtyp ~ cav, treat = 'assign', data = dat, yaxt="n", linecol='black',
        kdens = 'rectangular', h = 70, cex = .4, main="", ylab="Test Type", xlab="Centered Running Variable, Birth Date (Unit: Day)", xlim=c(-400, 400))

plot.RD(childethnic ~ cav, treat = 'assign', data = dat, yaxt="n", linecol='black',
        kdens = 'rectangular', h = 70, cex = .4, main="", ylab="Ethnicity", xlab="Centered Running Variable, Birth Date (Unit: Day)", xlim=c(-400, 400))

plot.RD(flunch ~ cav, treat = 'assign', data = dat, yaxt="n", linecol='black',
        kdens = 'rectangular', h = 70, cex = .4, main="", ylab="Free Lunch Status", xlab="Centered Running Variable, Birth Date (Unit: Day)", xlim=c(-400, 400))


