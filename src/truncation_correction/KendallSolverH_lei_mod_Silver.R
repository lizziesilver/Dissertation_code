################################################################################
# This software was originally written by Prof Lei Nie and Prof Haitao Chu in 
# S-Plus. It implements a numerical solver to correct the bias in Kendall's tau 
# due to truncation, as described in Nie, L., Chu, H., & Korostyshevskiy, V. R. 
# (2008). "Bias reduction for nonparametric correlation coefficients under the 
# bivariate normal copula assumption with known detection limits." The Canadian 
# Journal of Statistics/La Revue Canadienne de Statistique, 36(3): 427-442.
#
# I (Elizabeth Silver) have made some small modifications. All errors are my 
# own.
#
# I've added comments relating the functions to the equations in the paper, to
# aid my own understanding, and hope these are helpful for anyone else who uses 
# this software.

################################################################################
# kendall.A returns the value of A (page 429 of Nie, Chu & Korostyshevskiy 2008) 
# given L1, L2 and r, and assuming y1 and y2 have mean 0, variance 1, and 
# correlaton r
kendall.A <- function(L1, L2, r) {
  fun <- function(y1) {
    
    # first integral of A, integrated over y2 from L2 to infinity
    A1 <- sapply(y1, function(y1){
      integrate(f = function(y2, y1) {
        
        # A1 = integrate[ phi(y1) * phi(y2) * { Phi(z1) * 
        #                                       [1 - 2*Phi(z2) + Phi(z2c)] -
        #                                       Phi(z1c) * Phi(z2c) } ]
        dnorm(y1) * dnorm(y2) * (pnorm((y1-r*y2)/sqrt(1-r^2)) * 
          (1 - 2*pnorm((y2-r*y1)/sqrt(1-r^2)) + pnorm((L2-r*y1)/sqrt(1-r^2))) -
          pnorm((L1-r*y2)/sqrt(1-r^2)) * pnorm((L2-r*y1)/sqrt(1-r^2)) )
        
      }, rel.tol=1e-6, subdivisions=1000, lower=L2, upper=Inf, y1=y1)$value				
    }
    )
    
    # Second integral of A, integrated over y2 from -infinity to L2
    A2 <- sapply(y1, function(y1){
      integrate(f = function(y2, y1) {
        
        # A2 = integrate[ phi(y1) * phi(y2) * [2 * Phi(z1) - 1] * 
        #                 [1 - Phi(z2c)] ]
        dnorm(y1) * dnorm(y2) * (2 * pnorm((y1-r*y2)/sqrt(1-r^2)) - 1) * 
          (1 - pnorm((L2-r*y1)/sqrt(1-r^2)))
        
      }, rel.tol=1e-6, subdivisions=1000, lower=-Inf, upper=L2, y1=y1)$value
    }
    )
    
    return(A1 + A2)
  }
  
  # Now integrate over y1 from L1 to infinity, return the value
  A <- integrate(f=fun, rel.tol=1e-4, subdivisions=1000, lower=L1, 
                 upper=Inf)$value	
  
  return(2 * A)
}

# Note to self:
# qnorm takes a quantile and returns x
# pnorm takes x and returns the quantile
# dnorm takes x and returns the height of the density at x

################################################################################
# start with an observed r, denoted  R.o, and bounds on the correction r.l and 
# r.u
# Perform binary search for true r.
solve.r <- function(R.o, r.l, r.u, L1, L2, accuracy=0.001) {
  
  # for correlations very close to 1 or -1, just return the observed correlation
  if (abs(r.u-r.l)<=accuracy) {
    if (abs(r.u)!=1) return(r.u) else return(r.l)
  }
  
  while (abs(r.u-r.l)>accuracy){
    # set r to be the midpoint of the upper and lower bounds
    r <- (r.l+r.u)/2
    
    # estimate R.e: what correlation would we expect to observe, given a true 
    # correlation r, and truncation bounds L1 and L2?
    tau.e <- kendall.A(L1, L2, r) / sqrt((1-pnorm(L1)^2) * (1-pnorm(L2)^2))
    R.e <- sin((pi / 2) * tau.e)
    
    # If the estimate is larger than the observed R.o, narrow the upper bound; 
    # otherwise leave it untouched
    r.u <- ifelse(R.e>R.o, r, r.u)
    # If the estimate is smaller than the observed R.o, narrow the lower bound; 
    # otherwise leave it untouched
    r.l <- ifelse(R.e>R.o, r.l, r)
  }
  
  return(r)
}

################################################################################
# Corrects Kendall correlation, given truncation points L1 and L2
kendall.corrected <- function(r, L1, L2, accuracy=0.001){

  # because the bias always attenuates tau toward zero, the absolute values of 
  # the bounds on tau.corrected are always [r, 1]. 
  r.l <- ifelse(r<0, -1, r)
  r.u <- ifelse(r<0, r, 1)
  
  # correct bias
  r.corrected <- solve.r(r, r.l, r.u, L1, L2, accuracy)
  
  return(r.corrected)
}

################################################################################
# Notes:

# If you need standard errors, you should bootstrap them.

# How long does it take to correct an observed correlation? On a quad-core 
# Macbook Pro (2012), 2.4GHz Intel Core i7, 16GB RAM, we have:
# > system.time(kendall.corrected(0.3, 2, 2, accuracy=0.001))
# user  system elapsed 
# 0.043   0.010   0.067 
