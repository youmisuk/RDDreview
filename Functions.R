# :::::::::::::::::::::::::::::
# ::::: functions for RDD :::::
# :::::::::::::::::::::::::::::
# Note: These functions were originally developed by Dr. Peter Steiner (University of Maryland-College Park), 
# with very minor revisions subsequently made by Dr. Youmi Suk (Teachers College, Columbia University) 
# This R script should be loaded prior to conducting the main RD analysis.

if(!require("scales")){
  install.packages("scales")
  library("scales")
}

plot.RD <- function(frml, treat, data, kdens = 'triangular', h = F, SE=TRUE, linecol="red", ...)
{
  # frml  ... Y ~ X    outcome (Y), CENTERED assignment variable (X)
  #                    polynomials of X are possible (but no other covariates)
  # treat ... name (character string) of the treatment indicator (0/1)
  # data  ... data frame of variables
  # kdens ... kernel density
  # h     ... bandwidth
  
  var.nam <- all.vars(frml)   # get names of variables in formula
  # determine plotting range left and right to cutoff (0)
  cav0 <- seq(min(data[, var.nam[2]]), 0, length = 100)
  cav1 <- seq(0, max(data[, var.nam[2]]), length = 100)
  # flip treatment indicator if treatment is left to the cutoff
  if(diff(tapply(data[, var.nam[2]], data[, treat], mean)) < 0) {
    data[, treat] <- ifelse(data[, treat] == 1, 0, 1)
  }
  # transform a dichotomous outcome into dummy coding
  y <- data[, var.nam[1]]
  if(is.factor(y)) data[, var.nam[1]] <- unclass(y); print(levels(y))
  
  # scatterplot
  plot(data[, var.nam[2]], data[, var.nam[1]], col = alpha("black", 0.15), ...)
  abline(v = 0, lty = 2)
  
  # add nonparametric regression paths
  pre0 <- loc.reg(frml, data = data[data[, treat] == 0, ], 
                  kdens = kdens, h = h, x.val = cav0)  
  pre1 <- loc.reg(frml, data = data[data[, treat] == 1, ],
                  kdens = kdens, h = h, x.val = cav1) 
  lines(cav0, pre0$fit, lwd = 2, col=linecol)
  lines(cav1, pre1$fit, lwd = 2, col=linecol)
  
  if (SE == TRUE) {
    # add 95% confidence envelopes
    lines(cav0, pre0$fit + 2*pre0$se.fit, lty = 2)
    lines(cav0, pre0$fit - 2*pre0$se.fit, lty = 2)
    lines(cav1, pre1$fit + 2*pre1$se.fit, lty = 2)
    lines(cav1, pre1$fit - 2*pre1$se.fit, lty = 2)
    
  }
}


loc.reg <- function(frml, data, kdens = 'normal', h = F, x.val = NULL)
{
  # estimate path of means using local polynomial regression
  # frml  ... Y ~ X : outcome Y regressed on polynomial of X
  # data  ... data frame including variables for the formula
  # kdens ... kernel density
  # h     ... bandwidth for kernel
  # x.val ... locations at which the means should be calculated
  
  if(is.null(x.val)) {
    av <- all.vars(frml)[2]
    x.val <- seq(min(data[, av]), max(data[, av]), length = 200)
  }
  y <- sapply(x.val, loc.regmean, frml = frml, data = data, 
              kdens = kdens, h = h)
  data.frame(x = x.val, fit = y[1, ], se.fit = y[2, ])
}


loc.regmean <- function(frml, loc, data, kdens = 'normal', h = F)
{
  # estimate mean value at 'loc' using local polynomial regression
  # frml  ... Y ~ X : outcome Y regressed on polynomial of X
  # loc   ... location where mean should be estimated
  # data  ... data frame including variables for the formula
  # kdens ... kernel density
  # h     ... bandwidth for kernel
  
  # define function for kernel density weights
  kern <- switch(kdens,
                 normal = function(x) dnorm(x),
                 epanechnikov = function(x) ifelse(abs(x) < 1, 3/4 * (1 - x^2), 0),
                 rectangular = function(x) ifelse(abs(x) < 1, 1, 0),
                 triangular = function(x) ifelse(abs(x) < 1, 1 - abs(x), 0),
                 tricube = function(x) ifelse(abs(x) < 1, (1 - abs(x)^3)^3, 0))
  
  av <- all.vars(frml)[2]                 # name of AV
  data[, av] <- av.c <- data[, av] - loc  # center AV at location loc
  data$kern.wt <- kern(av.c / h)          # weights
  out.lm <- lm(frml, data = data, weights = kern.wt)  
  summary(out.lm)$coef[1, 1:2]            # return intercept & s.e.
}
