# Functions to calculate sample size for TOST
# Author: Felipe Campelo (fcampelo@ufmg.br)
# April 2015

# 1) Function to calculate sample size for 1-sample TOST.
# Based on Mathews (2010), Sample Size Calculations, p. 45, MMB.
# modified for unequal variances
calcN_tost1<-function(alpha = 0.05, # significance level
                      beta = 0.2,   # type-II error rate
                      diff_mu = 0,  # maximum real difference |mu-mu0| for 
                                    # which a power (1-beta) is desired
                      tolmargin,    # tolerance margin (>0)
                      s)            # estimated sd of population
{
    # Guarantee that diff_mu is expressed as a positive value
    if (diff_mu<0) {
        warning("Using abs(diff_mu)")
        diff_mu<-abs(diff_mu)
    }
    
    # initial values for iteration
    talpha  <- qnorm(1-alpha)
    tbeta   <- qnorm(1-beta)
    n       <- 0
    rhs     <- ((talpha+tbeta)*s/(tolmargin-diff_mu))^2
    while (n<rhs){
        n       <- rhs                  # update n
        talpha  <- qt(1-alpha,n-1)      # update t-quantile
        tbeta   <- qt(1-beta,n-1)       # update t-quantile
        rhs     <- ((talpha+tbeta)*s/(tolmargin-diff_mu))^2
    }
    return(n)
}

# 2) Function to calculate sample size for 2-sample TOST, assuming n1=n2=n
# Based on Zhang (2003), Journal of Biopharmaceutical Statistics 13(3):529-538,
# modified for unequal variances
calcN_tost2<-function(alpha = 0.05, # significance level
                     beta = 0.2,    # type-II error rate
                     diff_mu = 0,   # maximum real difference |mu1-mu2| for 
                                    # which a power (1-beta) is desired
                     tolmargin,     # tolerance margin (>0)
                     s1,            # estimated sd of first population
                     s2)            # estimated sd of second population
                                    # (defaults to s2=s1)
{
    # Function to calculate DoF for a t distribution using the Welch formula
    calc_df<-function(s1,s2,n1,n2)
    {(s1^2/n1 + s2^2/n2)^2/((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))}
    
    # Assume equality of variances if s2 is not informed
    if(missing(s2)) {
        warning("s2 not informed. Assuming s2 = s1.")
        s2<-s1
    }

    # Guarantee that diff_mu is expressed as a positive value
    if (diff_mu<0) {
        warning("Using abs(diff_mu)")
        diff_mu<-abs(diff_mu)
    }
    
    # Calculate required values based on the Zhang formula
    c       <- 0.5*exp(-7.06*diff_mu/tolmargin) # c factor
    sigma_e <- sqrt((s1^2+s2^2))                # combined variance
    talpha  <- qnorm(alpha)                     # initial value for iteration
    tbeta   <- qnorm((1-c)*beta)                # initial value for iteration
    n       <- 0                                # initial value for iteration
    rhs     <- (talpha+tbeta)^2 * (sigma_e/(tolmargin-diff_mu))^2

    while (n<rhs){
        n       <- rhs                  # update n
        nu      <- calc_df(s1,s2,n,n)   # calculate DoF (Welch)
        talpha  <- qt(alpha,nu)         # update t-quantile
        tbeta   <- qt((1-c)*beta,nu)    # update t-quantile
        rhs     <- (talpha+tbeta)^2 * (sigma_e/(tolmargin-diff_mu))^2
    }
    return(n)   # return required number of observations afor each group
}