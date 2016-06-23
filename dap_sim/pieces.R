################################################################################
# Example data
bob <- data.frame(x=rnorm(1000))
bob$y <- rnorm(1000)
bob$z <- rnorm(1000) + 0.5*bob$x - 0.5*bob$y

cor(bob)

################################################################################
# SKEPTIC
library(huge)
skeptic_cor = huge.npn(bob, npn.func="skeptic", npn.thresh=NULL, verbose=TRUE)

################################################################################
# PC
library(pcalg)
pc(suffStat=list(C=skeptic_cor, n=1000), indepTest=gaussCItest, alpha=0.05, 
   labels=names(bob), fixedGaps = NULL, fixedEdges = NULL, 
   NAdelete = TRUE, m.max = Inf, u2pd = c("relaxed"),
   skel.method = c("stable.fast"), conservative = FALSE, maj.rule = FALSE, 
   solve.confl = FALSE, verbose = FALSE)

################################################################################
# FCI
library(pcalg)
fci(suffStat=list(C=skeptic_cor, n=1000), indepTest=gaussCItest, alpha=0.05, 
    labels=c("x", "y", "z"),
    skel.method = c("stable.fast"),
    type = c("normal"),
    fixedGaps = NULL, fixedEdges = NULL,
    NAdelete = TRUE, m.max = Inf, pdsep.max = Inf,
    rules = rep(TRUE, 10), doPdsep = TRUE, biCC = FALSE,
    conservative = FALSE, maj.rule = FALSE,
    verbose = FALSE)

################################################################################
# GES
# looks like we can't use a correlation matrix with the pcalg implementation

################################################################################
# glasso
library("glasso")
glasso(s=skeptic_cor, rho=.01, zero=NULL, thr=1.0e-4, maxit=1e4,  approx=FALSE, 
       penalize.diagonal=TRUE, start=c("cold"), 
       w.init=NULL,wi.init=NULL, trace=FALSE)

library(huge)
joe = huge(x=skeptic_cor, lambda = NULL, nlambda = NULL, lambda.min.ratio = NULL, 
           method = "glasso", scr = NULL, scr.num = NULL, cov.output = FALSE, 
           sym = "or", verbose = TRUE)

################################################################################
# CLIME
library(clime)
nlambda=100
lambda.max=0.8
lambda.min=1e-4
lambda <- seq(lambda.min, lambda.max, length.out = nlambda)
ted = clime(x=skeptic_cor, lambda=,
      lambda.max=0.8, lambda.min=1e-4,
      sigma=TRUE, perturb=FALSE, standardize=FALSE, logspaced=TRUE,
      linsolver=c("simplex"), pdtol=1e-3, pdmaxiter=50)


################################################################################
# The Meinshausen-B\"uhlmann neighborhood descent estimator
library(huge)
joe = huge(x=skeptic_cor, lambda = NULL, nlambda = NULL, lambda.min.ratio = NULL, 
           method = "mb", scr = NULL, scr.num = NULL, cov.output = FALSE, 
           sym = "or", verbose = TRUE)
huge.plot(joe$path[[3]])

library("glasso")
glasso(s=skeptic_cor, rho=.01, zero=NULL, thr=1.0e-4, maxit=1e4,  approx=TRUE, 
       penalize.diagonal=TRUE, start=c("cold"), 
       w.init=NULL,wi.init=NULL, trace=FALSE)


################################################################################
# correlation thresholding 
library(huge)
joe = huge(x=skeptic_cor, lambda = NULL, nlambda = NULL, lambda.min.ratio = NULL, 
           method = "ct", scr = NULL, scr.num = NULL, cov.output = FALSE, 
           sym = "or", verbose = TRUE)

################################################################################
# graphical Dantzig selector
library(flare)

