# g.e.k is a sub-function that calculates the control cost (stands for g(e_k))
g.e.k=function(e.k, h){if(h>0){10^(h*e.k)-1} else{e.k}}

# CC.k is a function that calculates the control cost as a function of percentage reduction in exposure (e.k*100%)
CC.k=function(e.k, max.CC, h, Overhead.CC=0){
  CC.k=Overhead.CC+(max.CC-Overhead.CC)*g.e.k(e.k, h)/g.e.k(1, h)
  CC.k[which(e.k==0)]=0
  return(CC.k)
}


#----------------------------------------------------#

# u.sd calculates the uncertainty standard deviation based on the orders of magnitude (OM) and corresponding range (i.e., what proportion of the distribution does the "range" cover?)
# NOTE: this calculation is based on Normal distribution
u.sd=function(OoM, range){
  OoM/(-2*qnorm(1/2*(1-range)))
}


#----------------------------------------------------##----------------------------------------------------#

# Stands for discount rate for health cost using cumulative summation
# This function calculate sum(1/(1+r)^(y-1)) for year 1 to y.eff-1 (for pre-mitigation) and year y.eff to y.TH (post-mitigation).
# A particular row will be selected and multiplied when computing HC component for TSC

disc.rate.HC_cumsum=function(y.TH, r){
  disc.rate=(1+r)^((1:y.TH)-1)
  disc.rate.inv=1/disc.rate
  disc.rate.inv.total=sum(disc.rate.inv)
  disc.rate.inv.cumsum=cumsum(disc.rate.inv)
  pre.mit=disc.rate.inv.cumsum.NoMit=c(0, disc.rate.inv.cumsum[1:y.TH-1])
  post.mit=disc.rate.inv.cumsum.Mit=disc.rate.inv.total-disc.rate.inv.cumsum.NoMit
  
  out=NULL
  out$disc.rate.inv.cumsum=cbind(pre.mit, post.mit)
  out
}


#----------------------------------------------------##----------------------------------------------------#

# Normal_Uncertainty_Generator is a function that generates a parameter vector of length n
# NOTE: It assumes Normality with mean=parameter1 and sd=parameter2

Normal_Uncertainty_Generator=function(n=1, parameter1, parameter2){
  if(parameter2==0){
    output=raw.output=rep(parameter1, n)
  }
  if(parameter2>0){
    raw.output=rnorm(n)
    raw.output=(raw.output-mean(raw.output))/sd(raw.output)*n/(n-1) # ensure sample mean=0 and sample sd=1
    output=raw.output*parameter2+parameter1 # convert to original scale
  }
  out=NULL
  out$raw.output=raw.output
  out$output=output
  out
}


#----------------------------------------------------##----------------------------------------------------#

# Risk_Function requires a matrix of parameter inputs and calculate the risk under the assumption of distributions being Normal.
# The option "plot" shows the plot of toxicity CDF and exposure pdf.

Risk_Function=function(information){

  mu.tox=information[,1]
  sigma.tox=information[,2]
  mu.exp=information[,3]
  sigma.exp=information[,4]
    
  risk.inside.mat=(mu.exp-mu.tox)/sqrt(sigma.exp^2+sigma.tox^2)
  result.mat=pnorm(risk.inside.mat)

  result.mat=matrix(result.mat)
  risk.inside.mat=matrix(risk.inside.mat)
  colnames(result.mat)=c("risk")
  colnames(risk.inside.mat)=c("risk.inside")
  
  out=NULL
  out$results=result.mat
  out$risk.inside=risk.inside.mat
  out
  
}


#----------------------------------------------------##----------------------------------------------------#

#-----------------#
# UPDATED VERSION #
#-----------------#

# Normal_Bayesian_Updating_mean is a function that calculates the posterior mean and variance of the
# mean for toxicity or exposure distribution under the assumption of Normality.
# This function also assumes that experimantal variability is known and a constant.
# The function allows for bias (i.e., using TRUE PARAMETER rather than prior mean)

Normal_Bayesian_Updating_mean=function(n.expr=1, sigma_expr, mu_mu_prior, s_j=0, mu_sigma_prior){

  if(is.infinite(sigma_expr)){
    mean_expr=s_j
    mu_sigma_posterior=(1/mu_sigma_prior^2 + n.expr/sigma_expr^2)^(-1/2)
    mu_mu_posterior=mu_mu_prior
  }
  if(sigma_expr>0 & !is.infinite(sigma_expr)){
    mean_expr=s_j
    mu_sigma_posterior=(1/mu_sigma_prior^2 + n.expr/sigma_expr^2)^(-1/2)
    mu_mu_posterior=(sigma_expr^2/(sigma_expr^2+mu_sigma_prior^2))*mu_mu_prior+(1-(sigma_expr^2/(sigma_expr^2+mu_sigma_prior^2)))*mean_expr
  }
  if(sigma_expr==0){
    mean_expr=s_j
    mu_sigma_posterior=(1/mu_sigma_prior^2 + n.expr/sigma_expr^2)^(-1/2)
    mu_mu_posterior=(sigma_expr^2/(sigma_expr^2+mu_sigma_prior^2))*mu_mu_prior+(1-(sigma_expr^2/(sigma_expr^2+mu_sigma_prior^2)))*mean_expr
  }
  
  
  out=NULL
  out$sample.mean=mean_expr
  out$mu_mu_posterior=mu_mu_posterior
  out$mu_sigma_posterior=mu_sigma_posterior
  out
}


#----------------------------------------------------##----------------------------------------------------#

# This function calculates the standard deviation for the experiment based on the prior and posterior uncertainty
sigma_expr_function=function(sigma.posterior, u.prior=1){
  sigma_expr=sqrt(1/(1/(sigma.posterior^2)-1/(u.prior^2)))
  sigma_expr
  }


#----------------------------------------------------##----------------------------------------------------#


# This function calculates the TRDM VOI analysis
TRDM_function_RS=function(mu.tox, u.mu.tox, sigma.tox, mu.exp, sigma.exp, t.IC, t.imp, t.eff, for.range.mu.tox.raw=seq(-6, 6, by=0.1), TRL, q.UCL=0.95, q.LCL=0.05, ER.option.A=0.9){
  
  # Return error message if inappropriate values for q.UCL and q.LCL
  if(!is.na(q.UCL)){
    if(q.UCL>=1){
      stop("quantile must be between 0 to 1!")
    } 
    if(q.UCL<0){
      stop("quantile must be between 0 to 1!")
    }
  }
  if(!is.na(q.LCL)){
    if(q.LCL>=1){
      stop("quantile must be between 0 to 1!")
    } 
    if(q.LCL<0){
      stop("quantile must be between 0 to 1!")
    }
  }
  
  
  #----------------------------------------------------#
  
  # Obtain mu.tox values corresponding to q.UCL and q.LCL
  for.mu.tox.q=qnorm(1-c(q.UCL, q.LCL))
  mu.tox.q=for.mu.tox.q*u.mu.tox+mu.tox
  mu.tox.q.UCL=mu.tox.q[1]
  mu.tox.q.LCL=mu.tox.q[2]
  
  
  # Obtain R values corresponding to q.UCL and q.LCL
  risk.q.dat=Risk_Function(cbind(mu.tox.q, sigma.tox, mu.exp, sigma.exp))
  risk.q.UCL=risk.q.dat$results[1]
  risk.q.LCL=risk.q.dat$results[2]
  log10.risk.q.UCL=log10(risk.q.UCL)
  log10.risk.q.LCL=log10(risk.q.LCL)
  
  
  # print out the quantiles and target risks for debugging
  cbind(log10(risk.q.dat$results), TRL)
  
  
  # Obtain R value under TRUE parameter values
  risk.true.dat=Risk_Function(cbind(mu.tox, sigma.tox, mu.exp, sigma.exp))
  risk.true=risk.true.dat$results
  log10.risk.true=log10(risk.true)
  
  
  #----------------------------------------------------#
  
  #-------------------------#
  # Decision Rule Indicator #
  #-------------------------#
  
  # Obtain Decision Rule Indicator (DRI), where +1 means decision made to regulate, 
  # -1 means decision made NOT to regulate, and 0 means UNDECIDED due to lack of information
  DRI=0
  if(log10.risk.q.UCL<TRL){
    DRI=-1
  }
  if(log10.risk.q.UCL>TRL & log10.risk.q.LCL>TRL){
    DRI=1
  } 

  
  # Obtain DRI for TRUE risk
  DRI.true=-1
  if(log10.risk.true>TRL){
    DRI.true=1
  }
  
  
  # For TRUE risk, obtain reduced risk
  if(DRI.true==1){
    mu.exp.true.option.A=mu.exp-1
    risk.true.option.A.dat=Risk_Function(cbind(mu.tox, sigma.tox, mu.exp.true.option.A, sigma.exp))
    risk.true.option.A=risk.true.option.A.dat$results
    log10.risk.true.option.A=log10(risk.true.option.A)
  }

  #-----------#
  # For EVPPI #
  #-----------#
  
  # If DRI=0, then the HC would be greater than that would be acceptable/desired
  
  # Calculate a vector of possible risk levels without any regulatory action
  pdf=dnorm(for.range.mu.tox.raw)
  pdf=pdf/sum(pdf)
  mu.tox.vec=for.range.mu.tox.raw*u.mu.tox+mu.tox
  risk.dat=Risk_Function(cbind(mu.tox.vec, sigma.tox, mu.exp, sigma.exp))
  risk.original=risk.dat$results
  log10.risk.original=log10(risk.original)
  
  
  # Weighted Average of unregulated risk. This is used for calculation of HC (unregulated)
  risk.original.WA=sum(risk.original*pdf) 
  log10.risk.original.WA=log10(risk.original.WA)
  
  
  # Calculate a vector of possible risk levels after regulation.
  # NOTE: for original risks less than TRL will not have reduction in risk (to avoid unnecessary regulations)
  mu.tox.vec.option.A=mu.tox.vec
  loc.option.A=which(log10.risk.original>TRL) # ones that are going to be reduced
  mu.tox.vec.option.A=mu.tox.vec-log10(1-ER.option.A) # ALL risks are reduced (since we don't know if the true value is greater than TRL)
  risk.option.A.dat=Risk_Function(cbind(mu.tox.vec.option.A, sigma.tox, mu.exp, sigma.exp))
  risk.option.A=risk.option.A.dat$results
  log10.risk.option.A=log10(risk.option.A)
  
  
  # Weighted Average of regulated risk. This is used for calculation of HC (regulated)
  risk.option.A.WA=sum(risk.option.A*pdf) 
  log10.risk.option.A.WA=log10(risk.option.A.WA)
  
  
  # Calculate a vector of possibl TRUE risk levels after regulation.
  # NOTE: for original risks less than TRL will not have reduction in risk (to avoid unnecessary regulations)
  mu.tox.true.vec.option.A=mu.tox.vec
  loc.option.A=which(log10.risk.original>TRL) # ones that are going to be reduced
  mu.tox.true.vec.option.A[loc.option.A]=mu.tox.vec[loc.option.A]-log10(1-ER.option.A) # If risk is reduced only if greater than TRL
  risk.true.option.A.dat=Risk_Function(cbind(mu.tox.true.vec.option.A, sigma.tox, mu.exp, sigma.exp))
  risk.true.option.A=risk.true.option.A.dat$results
  log10.risk.true.option.A=log10(risk.true.option.A)
  
  
  # Weighted Average of regulated risk. This is used for calculation of HC (regulated)
  risk.true.option.A.WA=sum(risk.true.option.A*pdf) 
  log10.risk.true.option.A.WA=log10(risk.true.option.A.WA)
  
  
  log10.true.risks.option.A=as.data.frame(cbind(log10.risk.original, log10.risk.option.A, log10.risk.true.option.A))
  names(log10.true.risks.option.A)=c("log10.risk.true.original", "log10.risk.option.A", "log10.risk.true.option.A")
  
  # Organize output
  out=NULL
  out$DRI=DRI
  out$log10.risk.original.WA=log10.risk.original.WA
  out$log10.risk.option.A.WA=log10.risk.option.A.WA
  out$log10.risk.true.option.A.WA=log10.risk.true.option.A.WA
  out$log10.true.risks.option.A=log10.true.risks.option.A
  out$ER.option.A=ER.option.A
  out$log10.risk.q.UCL=log10.risk.q.UCL
  out$log10.risk.q.LCL=log10.risk.q.LCL
  out
  
}

