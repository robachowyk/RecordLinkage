################################################################################# 
#' glmRecordlinkage: A package for performing a time-to-event analysis with data from two files. 
#'
#' The RecLinkSurv package implements the StEM-based model. For all available functions, please use library(help="RecLinkSurv"). 
#'  
#' @author you
#' @import LaplacesDemon
#' @import coda
#' @import Matrix
#' @import Rcpp
#' @import RcppArmadillo
#' @import progress
#' 
#' @useDynLib glmRecordlinkage
#' @name glmRecordlinkage
#' @docType package
"_PACKAGE"
 


################################################################################# 

###########################################################
#' Extract the info necessary for the gibbs sampler. 
#'    
#' @author Michel H Hof, \email{m.h.hof@amsterdamumc.nl}
#' @param A x 
#' @param B x 
#' @param name.y x
#' @param PIVs x
#' @param regression.formula x
#' @param pmatch.formula x
#' @param family.Y1 x
#' @param family.Y0 x
#' @param data The observed data consisting of. Must be created with the ... function.
#' @return List of prior distributions.
#' @examples 
#' @export   
#####################################################################################
createData = function(A,B, 
                      name.y, 
                      PIVs,
                      regression.formula, 
                      pmatch.formula, 
                      family.Y1, 
                      family.Y0)
{
  #Check if the variables in the formulas are present in the data
  #Do this with the first observation to keep things simple
  check = tryCatch(get_all_vars(regression.formula, A), error = function(e) 
  {
    cat("The formula contains variables that are not present in the data\n")
    stop(e)
  })
  check = tryCatch(get_all_vars(pmatch.formula, A), error = function(e) 
  {
    cat("The formula contains variables that are not present in the data\n")
    stop(e)
  })
   
  Y = B[,name.y]
  X = as.matrix(model.matrix(regression.formula, A))
  W = as.matrix(model.matrix(pmatch.formula, A))
  GA = A[,PIVs]
  GB = B[,PIVs]
  
  levels.pivs =  list()
  for(k in 1:ncol(GA))
    levels.pivs[[k]] = sort(unique(as.character(c(GA[,k], GB[,k]))))
  
  names(levels.pivs) = PIVs

  for(k in 1:ncol(GA))
    GA[,k] = as.numeric(factor(as.character(GA[,k]), levels=levels.pivs[[k]]))
  
  for(k in 1:ncol(GB))
    GB[,k] = as.numeric(factor(as.character(GB[,k]), levels=levels.pivs[[k]]))

  list(Y=Y, X=X, W=W, 
       GA=as.matrix(GA), GB=as.matrix(GB), 
       levels.pivs=levels.pivs, 
       family.Y1=family.Y1, 
       family.Y0=family.Y0)
}
  

###########################################################
#' Set the priors for the Gibbs sampler. Possible to modify afterwards if necessary (see example)
#' 
#' @param  
#' @author Michel H Hof, \email{m.h.hof@amsterdamumc.nl}
#' 
#' @param data The observed data consisting of. Must be created with the ... function.
#' @return List of prior distributions.
#' @examples 
#' @export   
#####################################################################################
createPriors = function(data)
{
  beta.prior = list()
  beta.prior.range = list()
  for(i in 1:ncol(data$X))
  {
    beta.prior[[i]] = function(x) dnorm(x,0, 1, log=TRUE)
    beta.prior.range[[i]] = c(-Inf, Inf)
  }
  names(beta.prior) = names(beta.prior.range) = colnames(data$X)
  
  #Add residual variance parameter
  if(data$family.Y1$family=="gaussian")
  { 
    beta.prior[[i+1]] = function(x) dexp(x, 0.001, log=TRUE)
    beta.prior.range[[i+1]] = c(0, Inf)
    names(beta.prior)[i+1] = names(beta.prior.range)[i+1] = "res.sd"
  }
  
  ###############################################################################
  
  alpha.prior = list()
  alpha.prior.range = list()
  for(i in 1:ncol(data$W))
  {
    alpha.prior[[i]] = function(x) dnorm(x,0, 1, log=TRUE)
    alpha.prior.range[[i]] = c(-Inf, Inf)
  } 
  names(alpha.prior) = names(alpha.prior.range) = colnames(data$W)
  
  ###############################################################################
  
  if(data$family.Y0$family=="gaussian")
  { 
    phi.prior = list(function(x) dnorm(x,0, 1, log=TRUE), function(x) dexp(x, 0.001, log=TRUE))
    phi.prior.range = list(c(-Inf,Inf), c(0, Inf))
    names(phi.prior) = names(phi.prior.range) = c("mean", "sd")
  }
  if(data$family.Y0$family=="binomial")
  { 
    phi.prior = c(1,1)
    names(phi.prior) = c("Y=1", "Y=0")
    #Not necessary
    phi.prior.range = NULL
  }
  
  ###############################################################################
  
  mu.prior = list()
  for(i in 1:length(data$levels.pivs))
  {
    mu.prior[[i]] = c(1,20)
    names(mu.prior[[i]]) = c("error", "no.Error")
  }
  pi.prior = list()
  for(i in 1:length(data$levels.pivs))
  {
    pi.prior[[i]] = rep(1, length(data$levels.pivs[[i]]))
    names(pi.prior[[i]]) = data$levels.pivs[[i]]
  }
  names(pi.prior) = names(mu.prior) = colnames(data$GA)
  
  ###############################################################################
  
  list(alpha=list(prior=alpha.prior, range=alpha.prior.range),
       beta=list(prior=beta.prior, range=beta.prior.range), 
       phi=list(prior=phi.prior, range=phi.prior.range), 
       mu=list(prior=mu.prior, range=NULL), 
       pi=list(prior=pi.prior, range=NULL))
}

 
###########################################################
#' Create start values for the Gibbs sampler 
#' 
#' Can be modified afterwards if necessary. RANDOMNESS!
#' 
#' @param  
#' @author Michel H Hof, \email{m.h.hof@amsterdamumc.nl}
#' 
#' @param data The observed data consisting of. Must be created with the ... function.
#' @param chain Previous run of the Gibbs sampler.
#' @return List of starting values.

#' @export   
#####################################################################################
createStart = function(data, chain=NULL)
{
  if(is.null(chain))
  {
    beta = rep(0, ncol(data$X))
    #Residual variance parameter
    if(data$family.Y1$family=="gaussian")
      beta[ncol(data$X)+1] = 1
    
    if(data$family.Y0$family=="gaussian")
      phi = c(0,1)
  
    if(data$family.Y0$family=="binomial")
      phi = 0.5
    
    alpha = rep(0,ncol(data$W))
     
    #PIVS
    n.levels = sapply(data$levels.pivs, length)
    mu = rep(0.05, length(n.levels))
    pi = list()
    for(i in 1:length(n.levels))
      pi[[i]] = rep(1/n.levels[i], n.levels[i])
    }else{
    
    maxiter = nrow(chain$alpha)
    
    alpha = chain$alpha[maxiter,]
    beta = chain$beta[maxiter,]
    phi = chain$phi[maxiter,]
    mu = chain$mu[maxiter,]
    pi = lapply(chain$pi, function(x)x[maxiter,])
  }

  list(alpha=alpha, beta=beta, phi=phi, mu=mu, pi=pi)
}


###########################################################
#' Main function of the StEM estimator. 
#'
#' The data mut be prepared with the function \link{createData}. Two types of regression models can be fitted to the data 
#' linear regression and logistic regression models. Note that this version of the StEM algorithm is very limited and can only fit the model from the simulation study; 
#' i.e. the PIVs are assumed to be ACAR/DCAR and that the outcome variable is assumed to be normally distributed in observations from 
#' file B without a match in file A. 
 
#' @author Michel H Hof, \email{m.h.hof@amsterdamumc.nl}
#' @export   
#####################################################################################
runGibbs = function(data, priors, start, n.iter, w)
{ 
  ######################################################################################################
  logPossibleConfig = function(m,sumD)
  {
    return = 0
    if(sumD>0)
      return = sum( log(m:(m-sumD+1))  )
    return 
  }
  
  ######################################################################################################
  loglik = function(logFV0, logFV1, LLM, LLA, LLB, PDX, D)
  {
    sumRowD = rowSums(D)
    sumColD = colSums(D)
    matches = which(D==1, arr.ind = TRUE)
    
    logPDX = sum(log(PDX) * sumRowD + log(1-PDX) * (1-sumRowD)) - logPossibleConfig(ncol(D),sum(D))
    
    logPDX +
      sum(LLA[sumRowD==0])+
      sum(LLB[sumColD==0])+
      sum(LLM[matches])+
      sum(logFV1[matches])+
      sum(logFV0[sumColD==0])
  }
   
  ######################################################################################################
  simulateD = function()
  {
    vectrueGA = paste1(trueGA)
    vectrueGB = paste1(trueGB)
    ##################################################################
    levs = unique(c(vectrueGA, vectrueGB))
    select = F33(vectrueGA, vectrueGB, levs)
    #########W#########################################################
      
    PDX = invlogit(as.numeric(data$W %*% alpha))
    logFV1 = Matrix(0, nrow=nrow(data$X), ncol=length(data$Y))
    
    if(data$family.Y1$family=="gaussian")
    { 
      gXB = data$family.Y1$linkinv(data$X %*% beta[-length(beta)])
      logFV1[select] = dnorm(data$Y[select[,2]], mean=gXB[select[,1]] , sd=beta[length(beta)], log=TRUE)  
    }
    
    if(data$family.Y1$family=="binomial")
    {
      gXB = data$family.Y1$linkinv(data$X %*% beta)
      logFV1[select] = dbern(data$Y[select[,2]], gXB[select[,1]] , log=TRUE)  
    } 
    
    if(data$family.Y0$family=="gaussian")
      logFV0 = dnorm(data$Y, mean=phi[1], sd=phi[2], log=TRUE) 
    
    if(data$family.Y0$family=="binomial")
      logFV0 = dbern(data$Y, phi, log=TRUE) 
    
    #Dummy matrices wrt pivs
    #What should we add/substract from the loglikelihood if an observation is a match/nonmatch
    LLA = rep(0,nrow(data$X))
    for(k in 1:length(n.levels))
    {
      logpTrue = log(pi[[k]])[trueGA[,k]]
      pTypo = (1-mu[k]) / (n.levels[k]-1)
      pSame = mu[k]
      
      #Contribution to the likelihood
      contr = rep(pSame, nrow(data$X))
      contr[data$GA[,k] != trueGA[,k]] = pTypo
      
      LLA = LLA + logpTrue + log(contr)
    }  
    
    LLB = rep(0,length(data$Y))
    for(k in 1:length(n.levels))
    {
      logpTrue = log(pi[[k]])[trueGB[,k]]
      pTypo = (1-mu[k]) / (n.levels[k]-1)
      pSame = mu[k]
      
      #Contribution to the likelihood
      contr = rep(pSame, length(data$Y))
      contr[data$GB[,k] != trueGB[,k]] = pTypo
      
      LLB = LLB + logpTrue + log(contr)
    }  
    
    LLMtmp = rep(0, nrow(select))
    #Do the same for the (possible) matches
    #What do we add/ substract from the loglik if a pair is a match/nonmatch
    for(k in 1:length(n.levels))
    {
      #True pivs from the matches
      HA = trueGA[ select[,1],k ]
      HB = trueGB[ select[,2],k ]
      
      logpTrue = log(pi[[k]])[HA]
      
      pTypo = (1-mu[k]) / (n.levels[k]-1)
      pSame = mu[k]
      
      #Contribution to the likelihood of the observation from A
      contr1 = rep(pSame, length(HA))
      contr1[data$GA[select[,1],k] != HA] = pTypo
      
      #Contribution to the likelihood of the observation from B
      contr2 = rep(pSame, length(HB))
      contr2[data$GB[select[,2],k] != HB] = pTypo
      
      LLMtmp = LLMtmp + logpTrue + log(contr1) + log(contr2)
    }    
    
    LLM = Matrix(0, nrow=nrow(data$X), ncol=length(data$Y))
    LLM[select] = LLMtmp 
    
    #Complete data likelihood
    LL0 = loglik(logFV0=logFV0, logFV1=logFV1, LLM=LLM, LLA=LLA, LLB=LLB, PDX=PDX, D=D)
    
    #A single run through D
    Dsample = sampleD(niter=n.sims.run ,
                      S=as.matrix(select),
                      LLA=LLA, 
                      LLB=LLB+logFV0, 
                      LLM=LLM+logFV1,
                      PDX=as.double(PDX), 
                      loglik=LL0, D=D, nmatches=as.integer(sum(D)), 
                      sumRowD=rowSums(D)>0, sumColD=colSums(D)>0)
    
    #Dsample$loglik
    #loglik(logFV0=logFV0, logFV1=logFV1, LLM=LLM, LLA=LLA, LLB=LLB, PDX=PDX,  D=Dsample$D)
    
    #Also return previous loglikelihood
    list(loglik = LL0, D = Dsample$D)
  }
  ######################################################################################################
  
  log.SliceSampler = function(params, logP, logP.range, n.sims, w) 
  {
    #Define function to check whether proposal is in slice    
    in.slice = function(prop, y, i) { y <= logP(prop, i=i) }
    
    d = matrix(NA, n.sims, length(params))
    d[1,] = params
    s=2
    for (s in 2:n.sims)
    {
      params = d[s-1,]
      for(i in 1:ncol(d))
      {
        #log-scale: sample from [0,p(x)]
        y = log(runif(1, 0, 1)) + logP(params, i=i)
        
        l = params
        u = params
        l[i] = l[i] - w * runif(1)
        u[i] = l[i] + w
        
        #############################################
        #Expand upper and lower limits if necessary
        #Respect the lower limit
        if(l[i] < logP.range[[i]][1])
        {
          l[i] = logP.range[[i]][1]
          l.in = FALSE
        }else
        {
          l.in = in.slice(prop=l, y=y, i=i)
        }   
        
        if(l.in) 
        {
          while (l.in) 
          {
            l[i] = l[i] - w
            
            #Respect the lower limit
            if(l[i] < logP.range[[i]][1])
            {
              l[i] = logP.range[[i]][1]
              l.in = FALSE
            }else
            {
              l.in = in.slice(prop=l, y=y, i=i)
            }    
          }
        }
        #############################################
        
        
        #############################################
        #Respect the upper limit
        if(u[i] > logP.range[[i]][2])
        {
          u[i] = logP.range[[i]][2]
          u.in = FALSE
        }else
        {
          u.in = in.slice(prop=u, y=y, i=i)
        }
        
        if(u.in) 
        {
          while (u.in) 
          {
            u[i] = u[i] + w
            
            #Respect the upper limit
            if(u[i] > logP.range[[i]][2])
            {
              u[i] = logP.range[[i]][2]
              u.in = FALSE
            }else
            {
              u.in = in.slice(prop=u, y=y, i=i)
            }
          }
        }
        #############################################
        
        # sample params from y-slice and shrink
        params.old = params
        params.in = FALSE
        while (!params.in) 
        {
          params[i] = runif(1, l[i], u[i])
          # check whether new 'beta' is in slice
          params.in = in.slice(prop=params, y=y, i=i)
          
          # shrink interval    
          if (params[i] > params.old[i]) 
          {
            u[i] = params[i]
          } else 
          {
            l[i] = params[i]
          }
        }
      }
      d[s,] = params
    }
    d
  }
  
  ######################################################################################################
  #Posterior density functions
  ######################################################################################################
  
  logGauss_beta = function(beta, i)
    sum(dnorm(Ym, data$family.Y1$linkinv(Xm %*% beta[-length(beta)]), sd=beta[length(beta)], log=TRUE)) +
      priors$beta$prior[[i]](beta[i])
   
  logBinom_beta = function(beta, i)
  {
    p = data$family.Y1$linkinv(Xm %*% beta)
    sum(log(p)*Ym + log(1-p)*(1-Ym)) + priors$beta$prior[[i]](beta[i])
  } 
  
  logGauss_phi = function(phi, i)
    sum(dnorm(Ynm, phi[1], sd=phi[2], log=TRUE)) + priors$phi$prior[[i]](phi[i])
  
  logBinom_alpha = function(alpha, i)
  {
    p = 1/(1+exp(-(data$W %*% alpha)))
    sum(log(p)*matchA + log(1-p)*(1-matchA)) + priors$alpha$prior[[i]](alpha[i]) 
  }  
  
  ######################################################################################################
  
  #Transform to numbers after determining its unique values
  n.levels = sapply(data$levels.pivs, length)
  
	###############################################################################
	#STARTVALUES
	###############################################################################
  
  alpha = start$alpha
  beta = start$beta
  phi = start$phi
  mu = start$mu
  pi = start$pi
  
  ######################################################################################
	#TRACES
	######################################################################################
	
	beta.iter = array(NA, c(n.iter, length(beta)))
	phi.iter = array(NA, c(n.iter, length(phi)))    
	alpha.iter = array(NA , c(n.iter, length(alpha)))
	mu.iter = array(NA , c(n.iter, length(n.levels)))
	pi.iter = lapply(n.levels, function(x) array(NA, c(n.iter, x)))
  loglik.iter = array(NA, c(n.iter, 1))
	
	D = Matrix(0,nrow=nrow(data$X), ncol=length(data$Y), sparse=TRUE)
	Dsum = D
	trueGA = data$GA
	trueGB = data$GB
	
	pb = progress_bar$new(format = "(:spin) Gibbs sampler [:bar] :percent in :elapsed",       total = n.iter, clear = FALSE, width= 60)
	
	iter = 1
	n.sims.run = 1
	for(iter in 1:n.iter)
  { 
	  alpha.iter[iter,] = alpha
	  beta.iter[iter,] = beta
	  phi.iter[iter,] = phi
	  mu.iter[iter,] = mu
	  
	  for(i in 1:length(n.levels))
	    pi.iter[[i]][iter,] = pi[[i]]
	  
	  pb$tick()
	  
	  #Update Delta  
	  D.sim = simulateD()
	  
	  loglik.iter[iter,] = D.sim$loglik
	  D = D.sim$D 
	  Dsum = Dsum + D
	  
	  matches = which(D==1, arr.ind=TRUE)
	  matchA  = rowSums(D)
	  matchB  = colSums(D)
	  
	  ###########################################################################################################

    trueGM = sampleM(GA=data$GA[matches[,1],], GB=data$GB[matches[,2],], pi=pi, mu=mu)
    trueGA[matches[,1],] = trueGB[matches[,2],] = trueGM
    trueGA[matchA==0,] = sampleNM(GA=data$GA[matchA==0,], pi=pi, mu=mu)
    trueGB[matchB==0,] = sampleNM(GA=data$GB[matchB==0,], pi=pi, mu=mu)
    
    #####################################################################################################################
    #Update beta
    
    #Data: regression data from matches
    Xm = data$X[matches[,1],]
    Ym = data$Y[matches[,2]]
    
    if(data$family.Y1$family=="gaussian")
      beta = log.SliceSampler(params=beta, logP=logGauss_beta, logP.range=priors$beta$range,  n.sims=n.sims.run+1, w=w)[n.sims.run+1,]
    
    if(data$family.Y1$family=="binomial")
      beta = log.SliceSampler(params=beta, logP=logBinom_beta, logP.range=priors$beta$range,  n.sims=n.sims.run+1, w=w)[n.sims.run+1,]
    
    #####################################################################################################################
    #Update alpha: uses matchA
    alpha = log.SliceSampler(params=alpha, logP=logBinom_alpha, logP.range=priors$alpha$range,  n.sims=n.sims.run+1, w=w)[n.sims.run+1,]

    #####################################################################################################################
    #Update phi
 
    Ynm = data$Y[colSums(D)==0]
    if(data$family.Y0$family=="gaussian")
      phi = log.SliceSampler(params=phi, logP=logGauss_phi, logP.range=priors$phi$range,  n.sims=n.sims.run+1, w=w)[n.sims.run+1,]
    
    if(data$family.Y0$family=="binomial")
      phi = rbeta(1, priors$phi$prior[1] + sum(Ynm==1), priors$phi$prior[2] + sum(Ynm==0))
      
    #####################################################################################################################
    #Update mu
    
    nvalsG = nrow(data$X) + length(data$Y)
    for(i in 1:length(n.levels))
    {
      nagreements = sum(trueGA[,i] == data$GA[,i]) + sum(trueGB[,i] == data$GB[,i])
      a = priors$mu$prior[[i]][1] + (nvalsG - nagreements)
      b = priors$mu$prior[[i]][2] + nagreements
      mu[i] = rbeta(1, a, b)
    }
      
    #####################################################################################################################
    #Update pi
    for(i in 1:length(n.levels))
    {
      picounts = table(factor(trueGA[,i], levels=1:n.levels[i])) + 
        table(factor(trueGB[matchB==0,i], levels=1:n.levels[i]))
    
      pi[[i]] = rdirichlet(1, picounts + priors$pi$prior[[i]])  
    } 
    
    #####################################################################################################################

	}
	
	pb$terminate()
	
	colnames(alpha.iter) = colnames(data$W)
	
	#Ligt aan de parametrizatie
	if(data$family.Y1$family=="gaussian")
	  colnames(beta.iter)  = c(colnames(data$X), "res.sd.")
	
	
	if(data$family.Y1$family=="binomial")
	  colnames(beta.iter)  = colnames(data$X)
	  
	colnames(mu.iter) = colnames(data$GA)
	
	#Ligt aan de parametrizatie
	if(data$family.Y0$family=="gaussian")
	  colnames(phi.iter) = c("mean", "sd")
	
	for(i in 1:length(n.levels))
	  colnames(pi.iter[[i]]) = data$levels.pivs[[i]]
	
	#Output
	out = list(loglik = as.mcmc(loglik.iter), alpha = as.mcmc(alpha.iter), beta = as.mcmc(beta.iter),
	            phi = as.mcmc(phi.iter),  mu = as.mcmc(mu.iter), pi = list())
	  
	for(i in 1:length(n.levels))
	  out$pi[[i]] = as.mcmc(pi.iter[[i]])
	
	out
}
	