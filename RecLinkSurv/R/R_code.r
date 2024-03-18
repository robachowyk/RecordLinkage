################################################################################# 
#' RecLinkSurv: A package for performing a time-to-event analysis with data from two files. 
#'
#' The RecLinkSurv package implements the StEM-based model. For all available functions, please use library(help="RecLinkSurv"). 
#'
#'  import(Matrix) 
#' @docType package
"_PACKAGE"
################################################################################# 


################################################################################# 
#' Create the data for the StEM algorithm.
#' 
#' @param A Data.frame containing file A. The time of measurement must be registered as the variable "U" (see the example data).
#' @param B Data.frame containing file B. The time of event must be registered as the variable "V" (see the example data).
#' @param x.names Vector of variable names that refer to the covariates in file A.
#' @param rangeA Range of times at which the baseline measurements were performed. In the paper, these values are referred to as l^A and r^A.
#' @param rangeB Range of event times in file B. In the paper, these values are referred to as l^B and r^B.
#' @param pivs Vector containing the names of the partially identifying variables. 
#' @param nintervalsT Number of intervals for the piecewise constant baseline hazard function. Default value is 10.
#' @param nintervalsV Number of intervals for the piecewise linear function describing the times of events without matches. Default value is 10.
#' @param referencePIVs Vector containing the reference values for all partially identifying variables.
#' @param referenceV0 Index that indicates which value is the reference value for the piecewise linear function describing the times of events without matches.
#'  The default value is the first parameter, i.e. referenceV0=1.
#'  
#' @return 
#' A list of object that are used in the StEM algorithm. If necessary, the objects can be adjusted manually afterwards.
#' @author Michel H. Hof, \email{m.h.hof@amsterdamumc.nl}
#' 
#' @examples  
#' ## See \link{StEM} function.
#' 
#' @export 
################################################################################# 

create.data = function(A, B, x.names, rangeA, rangeB,  pivs, nintervalsT=10, nintervalsV=10, referencePIVs, referenceV0=1)
{  
  maxT = rangeB[2] - rangeA[1]
  intervalsT = seq(0,maxT, length=nintervalsT+1)
  intervalsV = seq(rangeB[1], rangeB[2], length=nintervalsV+1) 
  ######################################### 
  
  #Change reference-group to most frequently obs value
  levels_pivs =  list()
  for(k in 1:length(pivs))
  { 
      x = pivs[k]
      pivsAsFactor = factor(as.character(c(A[,x], B[,x])))
      pivsAsFactor = relevel(pivsAsFactor, referencePIVs[k])
      levels_pivs[[k]] = levels(pivsAsFactor)
  }
  names(levels_pivs) = pivs
  
  for(k in 1:length(pivs))
  {
    A[,pivs[k]] = as.numeric(factor(as.character(A[,pivs[k]]), levels=levels_pivs[[k]]))
    B[,pivs[k]] = as.numeric(factor(as.character(B[,pivs[k]]), levels=levels_pivs[[k]]))
  }
  
  #Value for missing data
  A[,pivs][ is.na(A[,pivs]) ] = 0
  B[,pivs][ is.na(B[,pivs]) ] = 0

  missing_pivsA = colSums(A[,pivs]==0)
  missing_pivsB = colSums(B[,pivs]==0)
  nvalues = sapply(levels_pivs, length)

  list( U=A$U, 
        V=B$V,
        X=as.matrix(A[,x.names]),
       
        intervalsT=intervalsT,
        maxT=maxT,
        intervalsV=intervalsV,
        
        rangeA=rangeA,
        rangeB=rangeB, 
        
        pivsA=A[,pivs], 
        pivsB=B[,pivs], 
        
        referenceV0=referenceV0,
        
        nvalues=nvalues,
        missing_pivsA=missing_pivsA,
        missing_pivsB=missing_pivsB,
        levels_pivs=levels_pivs      
      )
}

invlogit = function(x)
  exp(x)/(1+exp(x))

logit = function(x)
  log(x/(1-x))

logPossibleConfig = function(m,sumD)
{
  return = 0
  if(sumD>0)
    return = sum( log(m:(m-sumD+1))  )
  return 
}

loglik = function(logFV0, logFV1, LLM, LLA, LLB, SXc, D)
{
  sumRowD = rowSums(D)
  sumColD = colSums(D)
  matches = which(D==1, arr.ind = TRUE)
  
  logPD = sum(log(1-SXc) * sumRowD + log(SXc) * (1-sumRowD)) - logPossibleConfig(ncol(D),sum(D))
  
  logPD +
    sum(LLA[sumRowD==0])+
    sum(LLB[sumColD==0])+
    sum(LLM[matches])+
    sum(logFV1[matches])+
    sum(logFV0[sumColD==0])
}

simulateW_fast = function(data, D, mu, eta, pTrue)
{
  truepivsA = array(NA, dim(data$pivsA))
  truepivsB = array(NA, dim(data$pivsB))
  
  matches =  which(D==1, arr.ind=TRUE)
  nonmatchesA = rowSums(D)==0
  nonmatchesB = colSums(D)==0
  
  eta.index = rep(1:length(data$nvalues),each=2)
 
  #Values that the PIV can take (0 is missing)
  for(k in 1:length(data$nvalues))
  {
    goX = 0:data$nvalues[k]
    
    for(g in goX)
    {
      checkA = nonmatchesA & (data$pivsA[,k] == g)
      if(sum(checkA)>0)      
        truepivsA[checkA,k] = sampleNM2(g, mu=mu[k], eta=eta[eta.index==k][1], pTrue=pTrue[[k]],  n=sum(checkA))
    
      checkB = nonmatchesB & (data$pivsB[,k] == g)
      if(sum(checkB)>0)
        truepivsB[checkB,k] = sampleNM2(g, mu=mu[k], eta=eta[eta.index==k][2], pTrue=pTrue[[k]],  n=sum(checkB))
    }  
  } 
                
  if(nrow(matches)>0)
  {  
    for(k in 1:length(data$nvalues))
    {
      newValues = sampleM(A=data$pivsA[matches[,1],k], B=data$pivsB[matches[,2],k],  mu=mu[k], eta=eta[eta.index==k], pTrue=pTrue[[k]])
      truepivsA[matches[,1],k] = newValues
      truepivsB[matches[,2],k] = newValues
    }
  }
  list(truepivsA=truepivsA, truepivsB=truepivsB)
}




simulateD = function(data, D, truepivsA, truepivsB, mu, eta, pTrue,  alpha, beta, gamma)
{
  #####################################################################
  #Determine which observation pairs can be matches (or not) based on the true values of the PIVS
  UA = sspaste2(as.matrix(truepivsA))
  UB = sspaste2(as.matrix(truepivsB))
  
  valuesU = unique(c(UA,UB))
  
  UA = as.numeric(factor(UA,levels=valuesU))
  UB = as.numeric(factor(UB,levels=valuesU)) 
  
  tmpA = F2(UA, length(valuesU))
  tmpB = F2(UB, length(valuesU))
  select = F33(tmpA,tmpB, length(tmpA))
  
  #Check the times; if zero/negative it is also not possible to be a match
  times = data$V[select[,2]] - data$U[select[,1]]
  select = select[times>0,]
   
  ##############################################################################
  #Dummymatrices used in the gibbs sampler
  #What do we need to add/substract from the loglikelihood if we consider pairs to be matches/non-matches
  #dummymatrices wrt. time
  
  expXb = as.numeric(exp(as.matrix(data$X) %*% beta))
  
  #Proportional hazards
  Sc = exp(-Hfunc( max(data$intervalsV) - data$U , knots=data$intervalsT, params=exp(alpha)))
  SXc = Sc^expXb
  
  times = data$V[select[,2]] - data$U[select[,1]]
  S = exp(-Hfunc(times, knots=data$intervalsT, params=exp(alpha)))
  h = hfunc(times, knots=data$intervalsT, params=exp(alpha))
  hX = h * expXb[select[,1]]
  SX = S^expXb[select[,1]]
  
  FV1 = (hX*SX)/(1-SXc[select[,1]])
  logFV1 = Matrix(0, nrow=nrow(data$pivsA), ncol=nrow(data$pivsB))
  logFV1[select] = log(FV1)  
  
  #For all observations from B: density if no-match
  gammax = rep(0, length(gamma)+1)
  gammax[-data$referenceV0] = gamma
  FV0 = dpiecelin(data$V, b=data$intervalsV, w=exp(gammax))
  logFV0 = log(FV0)
  
  ##################################################################
  
  eta.index = rep(1:length(data$nvalues),each=2)
  
  #Dummy matrices wrt pivs
  #What should we add/substract from the loglikelihood if an observation is a nonmatch
  LLA = rep(0,nrow(data$pivsA))
  for(k in 1:length(data$nvalues))
  {
    logpTrue = log(pTrue[[k]])[truepivsA[,k]]
    
    mu_k = mu[k]
    eta_k = eta[eta.index==k]
    pMissingA = exp(eta_k[1]) / (1+exp(eta_k[1]));
    pTypoA = (1-pMissingA) * exp(mu_k)/(1+exp(mu_k)) / (data$nvalues[k]-1);
    pSameA = (1-pMissingA) / (1+exp(mu_k));
    
    #Contribution to the likelihood
    contr = rep(pSameA, nrow(data$pivsA))
    contr[data$pivsA[,k] != truepivsA[,k]] = pTypoA 
    contr[data$pivsA[,k] == 0] = pMissingA
    
    LLA = LLA + logpTrue + log(contr)
  }  
  
  #What should we add/substract from the loglikelihood if an observation is a nonmatch
  LLB = rep(0,nrow(data$pivsB))
  for(k in 1:length(data$nvalues))
  {
    logpTrue = log(pTrue[[k]])[truepivsB[,k]]
    
    mu_k = mu[k]
    eta_k = eta[eta.index==k]
    pMissingB = exp(eta_k[2]) / (1+exp(eta_k[2]));
    pTypoB = (1-pMissingB) * exp(mu_k)/(1+exp(mu_k)) / (data$nvalues[k]-1);
    pSameB = (1-pMissingB) / (1+exp(mu_k));
    
    #Contribution to the likelihood
    contr = rep(pSameB, nrow(data$pivsB))
    contr[data$pivsB[,k] != truepivsB[,k]] = pTypoB
    contr[data$pivsB[,k] == 0] = pMissingB
    
    LLB = LLB + logpTrue + log(contr)
  } 
  
  LLMtmp = rep(0, nrow(select))
  #Do the same for the (possible) matches
  #What do we add/ substract from the loglik if a pair is a match/nonmatch
  for(k in 1:length(data$nvalues))
  {
    #True pivs from the matches
    HA = truepivsA[ select[,1],k ]
    HB = truepivsB[ select[,2],k ]
    
    logpTrue = log(pTrue[[k]])[HA]
    
    mu_k = mu[k]
    eta_k = eta[eta.index==k]
    pMissingA = exp(eta_k[1]) / (1+exp(eta_k[1]));
    pTypoA = (1-pMissingA) * exp(mu_k)/(1+exp(mu_k)) / (data$nvalues[k]-1);
    pSameA = (1-pMissingA) / (1+exp(mu_k));
    
    pMissingB = exp(eta_k[2]) / (1+exp(eta_k[2]));
    pTypoB = (1-pMissingB) * exp(mu_k)/(1+exp(mu_k)) / (data$nvalues[k]-1);
    pSameB = (1-pMissingB) / (1+exp(mu_k));
    
    #Contribution to the likelihood of the observation from A
    contr1 = rep(pSameA, length(HA))
    contr1[data$pivsA[select[,1],k] != HA] = pTypoA
    contr1[data$pivsA[select[,1],k] == 0] = pMissingA
    
    #Contribution to the likelihood of the observation from B
    contr2 = rep(pSameB, length(HB))
    contr2[data$pivsB[select[,2],k] != HB] = pTypoB
    contr2[data$pivsB[select[,2],k] == 0] = pMissingB
    
    LLMtmp = LLMtmp + logpTrue + log(contr1) + log(contr2)
  }    
  
  LLM = Matrix(0, nrow=nrow(data$pivsA), ncol=nrow(data$pivsB))
  LLM[select] = LLMtmp 
  
  #Complete data likelihood
  LL0 = loglik(logFV0=logFV0, logFV1=logFV1, LLM=LLM, LLA=LLA, LLB=LLB, SXc=SXc, D=D)

  #A single run through D
  Dsample = sampleD(S=as.matrix(select),
                    LLA=LLA, 
                    LLB=LLB+logFV0, 
                    LLM=LLM+logFV1,
                    pi=as.double(1-SXc), 
                    loglik=LL0, D=D, nmatches=as.integer(sum(D)), 
                    sumRowD=rowSums(D)>0, sumColD=colSums(D)>0)

  #Check: Must be the same otherwise the RCPP code is wrong
  #Dsample$loglik
  #loglik(logFV0=logFV0, logFV1=logFV1, LLM=LLM, LLA=LLA, LLB=LLB, SXc=SXc, D=Dsample$D)
  
  Dsample$D
}

loglikV1 = function(params, index, X, V1, data)
{ 
  alpha = params[index]
  beta = params[-index]
  
  expXb = as.numeric(exp(as.matrix(data$X) %*% beta))
  
  h = hfunc(V1$T, knots=data$intervalsT, params=exp(alpha))  *(expXb[V1$Xindex])
  S = exp(-Hfunc(V1$T, knots=data$intervalsT, params=exp(alpha)))  ^(expXb[V1$Xindex])
  
  out = -sum((log(h)*V1$E + log(S))*V1$freq)
  out
}


loglikV0 = function(V0, gamma, b, reference)
{
  gammax = rep(0, length(gamma)+1)
  gammax[-reference] = gamma
  tmp = dpiecelin(x=V0$x, b=b, w=exp(gammax))
  -sum(V0$freq * log(tmp)) 
}

estimateRL = function(pi, freq)
  -sum(freq * (c(0,pi) - log(sum(exp(c(0,pi))))))


###########################################################
#' Main function of the StEM-based model. 
#'
#' The data must be prepared with the function \link{create.data}. The model uses the same assumptions as the simulation study in the paper:
#' a piecewise constant baseline hazard function, a piecewise linear function for the times from events without match, and independently
#' distributed partially identifying variables.  
#' 
#' @param data data created with \link{create.data}.
#' @param fitPrev Output of previous StEM run. If not previous run is available, the value should be NULL (also the default value)
#' Values can also be modified to change the staring values.
#' @param Z Number of StEM iterations; Z=Z0+Z1 if we use the notation from the paper.
#' @param M0 Number of burn-in iterations for the Gibbs sampler.
#' @param M1 Number of samples used in each StEM iteration.
#' @param trace Amount of detail that is given after performing a single StEM iteration: trace=1 gives a progressbar, trace=2 gives detailed information
#' onthe current parameter estimates, trace>2 also gives a plot to check whether the burn-in period was sufficient of not. Default value is 1.
#' @param ndigits Number of digits that is used in the output given after each StEM iteration. Default value is 2.
#' @param lower Lower bounds on the parameter estimates. Default values are list(mu=-Inf,eta=-Inf,pi=-Inf,alpha=-Inf,beta=-Inf,gamma=-Inf).
#' @param upper Upper bounds on the parameter estimates. Default values are list(mu=Inf,eta=Inf,pi=Inf,alpha=Inf,beta=Inf,gamma=Inf).
#' 
#' @return 
#' A list of matrices containing the Z parameter estimates.
#' 
#' @author Michel H. Hof, \email{m.h.hof@amsterdamumc.nl}
#' @examples 
#' \dontrun{
#' #Example data containing 500 baseline measurements
#' data(A)
#' #Example data containing 1000 events
#' data(B)
#' 
#' head(A)
#' head(B)
#' 
#' data = create.data(A,B, x.names=c("X1","X2"), pivs=c("piv1", "piv2", "piv3", "piv4"), 
#'                    rangeA=c(0,1), rangeB=c(0,1), nintervalsT=5, nintervalsV=5, 
#'                    referencePIVs = c("a","1","1","1"), referenceV0=1)
#' 
#' fit1a = StEM(data, Z=250, M0=25, M1=25, trace=2, 
#'              lower=list(mu = -5, eta = -Inf, pi = -Inf, alpha = -Inf, beta = -Inf, gamma = -Inf))
#' 
#' trace.StEM(fit1a, ask=FALSE)
#' 
#' fit1b = estimate.StEM(fit1a, Z0=200)
#' fit1c = variance.StEM(fit1b, M0=100, M1=2000, data=data, Mmax=10000)
#' }
#' @export   
#####################################################################################

StEM = function(data, fitPrev=NULL, Z, M0, M1, trace=1, ndigits=2, 
                  lower=list(mu=-Inf, eta=-Inf, pi=-Inf, alpha=-Inf, beta=-Inf, gamma=-Inf), 
                  upper=list(mu=Inf, eta=Inf, pi=Inf , alpha=Inf,  beta=Inf,  gamma=Inf))
{
  pi.index = rep(1:length(data$nvalues), data$nvalues-1)
  
  #Create starting values
  if(is.null(fitPrev))
  {
    mu = rep(-3, length(data$nvalues))
    
    #Parameter for the missings
    eta = sapply(1:length(data$nvalues), function(x)
    {  
      logit(c(data$missing_pivsA[x]/nrow(data$pivsA), data$missing_pivsB[x]/nrow(data$pivsB)))
    })
    eta = as.numeric(eta)
    pi = rep(0,length(pi.index))
    
    beta = rep(0, ncol(data$X))
    alpha = rep(-2, length(data$intervalsT)-1)  
    gamma = rep(0, length(data$intervalsV)-1)
  }else
  {
    lastIter = nrow(fitPrev$mu)
    alpha = as.numeric(fitPrev$alpha[lastIter,]) 
    beta =  as.numeric(fitPrev$beta[lastIter,])
    gamma = as.numeric(fitPrev$gamma[lastIter,]) 
    eta =   as.numeric(fitPrev$eta) 
    mu =    as.numeric(fitPrev$mu[lastIter,]) 
    pi =    as.numeric(fitPrev$pi[lastIter,])
  }
     
  ###################################################################
  
  mu.iter = array(NA, c(Z, length(mu)))
  pi.iter = array(NA, c(Z, length(pi)))
  beta.iter = array(NA, c(Z, length(beta)))
  alpha.iter = array(NA , c(Z, length(alpha)))
  gamma.iter = array(NA , c(Z, length(gamma))) 
  time.iter = 0
  
  if(trace==1)
  {
    pb_all = progress_bar$new(format = "Running StEM algorithm [:bar] :percent in :elapsed",       total = Z, clear = FALSE, width= 60)
    pb_all$tick(0)
  }  
  if(trace>1) 
    tijdM = Sys.time()
  
  iter=1 
  for(iter in 1:Z)
  {
    if(trace>1)
      cat("-------------------------------------------------------------------------\n")
    
    D = Matrix(0,nrow=nrow(data$pivsA),ncol=nrow(data$pivsB), sparse=TRUE)
    
    #Probability mass function of the true values
    pTrue = list()
    for(i in 1:length(data$nvalues))
    { 
      tmp = exp(c(0,pi[pi.index==i]))
      pTrue[[i]] = tmp/sum(tmp)
    }
    
    #############################################################################
    #BURNIN PERIOD GIBBS SAMPLER
    Dsum = c()
    for(j in 1:M0)
    { 
      newTruePivs = simulateW_fast(data=data, D=D, mu=mu, eta=eta, pTrue=pTrue) 
      
      truepivsA = newTruePivs$truepivsA
      truepivsB = newTruePivs$truepivsB
      
      D = simulateD(data=data, D=D, truepivsA=truepivsA, truepivsB=truepivsB, mu=mu, eta=eta, pTrue=pTrue, alpha=alpha, beta=beta, gamma=gamma)
      
      Dsum[j] = sum(D)
    }   
    if(trace>2) 
      plot(0:M0, c(0,Dsum), type="b", ylab="Number of matches", xlab="M0")
    
    #############################################################################
    #Save some summaries to make the M-step easier: V1/V0 contain event times and Vmu and Vpi information about the PIVs

    V1 = list()
    V0 = list()
    Vmu = list()
    Vpi = list()
    sumD = 0
    for(j in 1:M1)
    {
      newTruePivs = simulateW_fast(data=data, D=D, mu=mu, eta=eta, pTrue=pTrue)
      truepivsA = newTruePivs$truepivsA
      truepivsB = newTruePivs$truepivsB
      D = simulateD(data=data, D=D, truepivsA=truepivsA, truepivsB=truepivsB, mu=mu, eta=eta, pTrue=pTrue, alpha=alpha, beta=beta, gamma=gamma)
      
      #Save results for M-step
      matches = which(D==1, arr.ind=TRUE)
      survivalData = data.frame(T = data$rangeB[2] - data$U, E=0, Xindex=1:nrow(data$X))
      survivalData$T[matches[,1]] = data$V[matches[,2]] -  data$U[matches[,1]]
      survivalData$E[matches[,1]] = 1  
      
      V1[[j]] = survivalData
      V0[[j]] = data$V[!(1:nrow(data$pivsB) %in% matches[,2])]
      
      tmp = list()
      for(k in 1:length(data$nvalues))
      {
        facpivsA = factor(truepivsA[,k], levels=1:data$nvalues[k])
        facpivsB = factor(truepivsB[,k], levels=1:data$nvalues[k])
        
        tmp[[k]] = table(facpivsA[rowSums(D)==0]) + table(facpivsB[colSums(D)==0]) +  table(facpivsA[rowSums(D)==1])
      }
      Vpi[[j]] = tmp
      
      #Count agreements
      tmp = c()
      for(k in 1:length(data$nvalues))
        tmp[k] = sum(truepivsA[,k]==data$pivsA[,k]) + sum(truepivsB[,k]==data$pivsB[,k])
      Vmu[[j]] = tmp
      
      sumD = sumD + sum(D)
    } 
      
    meanD = sumD/M1
    
    #############################################################################  
    
    V0 = count(do.call("c", V0))
    gamma = optim(gamma*0, loglikV0,  b=data$intervalsV, V0=V0, method= "L-BFGS-B", lower=lower$gamma, upper=upper$gamma, reference=data$referenceV0)$par
    
    V1 = do.call("rbind", V1)
    #Possible to round T is necessary to speed up things
    V1 = count(V1)

    V1$freq = V1$freq / M1
    index = 1:length(alpha) 
    
    lower_tmp = c(rep(lower$alpha, length(alpha)), rep(lower$beta, length(beta)))
    upper_tmp = c(rep(upper$alpha, length(alpha)), rep(upper$beta, length(beta)))
    
    par.new = optim(c(alpha,beta), loglikV1, index=index, V1=V1, data=data,
                    lower=lower_tmp, upper=upper_tmp, method= "L-BFGS-B")$par
   
    alpha = par.new[index]
    beta = par.new[-index]
    
    for(i in 1:length(data$nvalues))
    {
      freq = rowSums(do.call("cbind", lapply(Vpi, function(tmp) tmp[[i]]))) 
      pi[pi.index==i] = nlminb(pi[pi.index==i], estimateRL, freq = freq, lower=lower$pi, upper=upper$pi)$par
    }
    
    #Amount of agreements
    N_tot = length(Vmu) * (nrow(truepivsA) + nrow(truepivsB))
    N_missing = length(Vmu) * (data$missing_pivsA + data$missing_pivsB)
    #Conditionally on not missing
    N_tot = N_tot - N_missing
    
    N_agree = colSums(do.call("rbind", Vmu))
    N_disagree = N_tot - N_agree 
    #Update only the mu's for the PIVS that contain error
    for(k in 1:length(data$nvalues))
    { 
        mu[k] =  nlminb(0, estimateRL, 
                                  freq = c(N_agree[k], N_disagree[k]),
                                  lower=lower$mu, upper=upper$mu)$par
    }
 
    #########################################
    #Administration
    mu.iter[iter,] = mu
    pi.iter[iter,] = pi
    beta.iter[iter,] = beta
    alpha.iter[iter,] = alpha
    gamma.iter[iter,] = gamma
    
    if(trace==1)
      pb_all$tick()
    
    if(trace>1)
    {
      cat("Iter:", iter, "\n")
      
      remaining = as.numeric(difftime(Sys.time(), tijdM, units="min"))/iter * (Z-iter)
      cat("approx. time remaining:\t", remaining, "mins\n")
    
      cat("beta:\t", round(beta, ndigits), "\n")
      cat("mu:\t", round(mu, ndigits), "\n")
      cat("eta:\t", round(eta, ndigits), "\n")

      for(i in 1:length(data$nvalues))
      {  
        cat("pi ", names(data$pivsA)[i],":", sep="")
        cat("\t",  round(pi[pi.index==i], ndigits), "\n")
      }
      
      cat("alpha:\t", round(alpha, ndigits), "\n") 
      cat("gamma:\t", round(gamma, ndigits), "\n")
      cat("mean(matches):\t", round(meanD, ndigits), "\n")
    }
  }
  if(trace==1)
    pb_all$terminate()
  
  ##################################################################################
  
  #Create labels for the parameter estimates
  colnames(mu.iter) = names(data$pivsA)
  
  eta = matrix(eta, nrow=1)
  colnames(eta) = paste(rep(names(data$pivsA), each=2), c("a","b"), sep="_")
  
  #First value is the reference value
  piv_labels = do.call("c", lapply(data$levels_pivs, function(x) x[-1]))
  piv_labels = paste(rep(names(data$pivsA), times=data$nvalues-1), piv_labels, sep="_")
  colnames(pi.iter) = piv_labels
  
  alpha_labels = paste("[", data$intervalsT[-length(data$intervalsT)], ",", data$intervalsT[-1], ")", sep="") 
  colnames(alpha.iter) = alpha_labels
  colnames(beta.iter) = colnames(data$X)
  
  #First value is the reference value
  colnames(gamma.iter) = data$intervalsV[-data$referenceV0]
  list(mu=mu.iter, eta=eta, pi=pi.iter, beta=beta.iter, alpha=alpha.iter, gamma=gamma.iter)
}
    
################################################################################# 
#' Plot the Markov chain produced by the StEM algorithm.
#' 
#' @param fit Result from a \link{StEM} function.
#' @param mfrow A vector of the form c(nr, nc). Subsequent figures will be drawn in an nr-by-nc array on the device by rows, respectively.
#' @param ask If TRUE (and the R session is interactive) the user is asked for input before a new figure is drawn.
#'   
#' @author Michel H. Hof, \email{m.h.hof@amsterdamumc.nl}
#' 
#' @return Traceplots of the parameter estimates. The red line is the average prediction using the last 
#' Z iterations
#' 
#' @examples  
#' ## See \link{StEM} function.
#' 
#'  
#' @export 
#################################################################################  
trace.StEM = function(fit, mfrow=c(3,2), ask=TRUE)
{
  par(mfrow=mfrow, ask=ask) 
  for(i in 1:ncol(fit$mu))
  {
    plot(fit$mu[,i], type="l", ylab=expression(mu), xlab="StEM iteration",  main=colnames(fit$mu)[i])
    est = sapply(1:nrow(fit$mu), function(j) mean(fit$mu[-1:-j,i]))
    lines(1:nrow(fit$mu), est, col=2)
  }
  
  par(mfrow=mfrow, ask=ask) 
  for(i in 1:ncol(fit$pi))
  {
    plot(fit$pi[,i], type="l", ylab=expression(pi), xlab="StEM iteration",  main=colnames(fit$pi)[i])
    est = sapply(1:nrow(fit$pi), function(j) mean(fit$pi[-1:-j,i]))
    lines(1:nrow(fit$pi), est, col=2)
  } 
  
  par(mfrow=mfrow, ask=ask) 
  for(i in 1:ncol(fit$alpha))
  {
    plot(fit$alpha[,i], type="l", ylab=expression(alpha), xlab="StEM iteration",  main=colnames(fit$alpha)[i])
    est = sapply(1:nrow(fit$alpha), function(j) mean(fit$alpha[-1:-j,i]))
    lines(1:nrow(fit$alpha), est, col=2)
  }
  
  par(mfrow=mfrow, ask=ask) 
  for(i in 1:ncol(fit$beta))
  {
    plot(fit$beta[,i], type="l", ylab=expression(beta), xlab="StEM iteration",  main=colnames(fit$beta)[i])
    est = sapply(1:nrow(fit$beta), function(j) mean(fit$beta[-1:-j,i]))
    lines(1:nrow(fit$beta), est, col=2)
  }
  
  par(mfrow=mfrow, ask=ask) 
  for(i in 1:ncol(fit$gamma))
  {
    plot(fit$gamma[,i], type="l", ylab=expression(gamma), xlab="StEM iteration",  main=colnames(fit$gamma)[i])
    est = sapply(1:nrow(fit$gamma), function(j) mean(fit$gamma[-1:-j,i]))
    lines(1:nrow(fit$gamma), est, col=2)
  }
}
  

################################################################################# 
#' Use the Markov chain produced by the StEM algorithm to obtain parameter estimates.
#' 
#' @param fit \link{StEM} function object.
#' @param Z0 Number of StEM iterations that are discarded as burn-in.
#' 
#' @return 
#' A list containing the parameter estimates.
#'   
#' @author Michel H. Hof, \email{m.h.hof@amsterdamumc.nl}
#' 
#' @examples  
#' ## See \link{StEM} function.
#'  
#' @export 
#################################################################################  
estimate.StEM = function(fit, Z0)
{  
  if(Z0 >= nrow(fit$alpha))
    stop(paste("Z0 must be smaller than ", nrow(fit), ".", sep=""))
  
  alpha = apply(fit$alpha, 2, function(x) mean(x[-1:-Z0]))  
  beta =  apply(fit$beta, 2, function(x) mean(x[-1:-Z0]))  
  gamma = apply(fit$gamma, 2, function(x) mean(x[-1:-Z0]))  
  eta =   as.numeric(fit$eta)
  names(eta) = colnames(fit$eta)
  mu =    apply(fit$mu, 2, function(x) mean(x[-1:-Z0]))  
  pi =    apply(fit$pi, 2, function(x) mean(x[-1:-Z0]))
  
  list(mu=mu, eta=eta, pi=pi, beta=beta, alpha=alpha, gamma=gamma)
}

############################################################


################################################################################# 
#' Estimate the covariance matrix for a certain set of estimates. If one of the variances is negative (due to too much simulation noise), 
#' the algorithm continues to sample more sets of missing data until a certain maximum is reached. Note that difficulties estimating 
#' the covariance matrix could also indicate identifiability issues of the model. In this case, simplifying the model (e.g. lowering the number
#' of intervals in the piecewise constant function) might solve the problem.
#' 
#' @param estimates Parameter estimates obtained with \link{estimate.StEM}.
#' @param M0 Number of burn-in iterations for the Gibbs sampler.
#' @param M1 Number of terations for the Gibbs sampler. Must be larger than M0.
#' @param data Data created by the function \link{create.data}.
#' @param Mmax Maximum number of simulated sets of missing data. 
#' 
#' @return 
#' \item{estimates}{Estimates and variance of the estimates of the model parameters.}
#' \item{check}{Did the function succeed in estimating the covariance matrix: check=1 if all values on the diagonal of the covariance matrix 
#' are larger than zero. check=2 if Mmax has been reached but the covariance contains negative (or zero) values on the diagonal.}
#' \item{covar}{The complete covariance matrix.}
#' \item{nSamples}{The number of sets of missing data used to estimate the covariance matrix.}
#'   
#' @author Michel H. Hof, \email{m.h.hof@amsterdamumc.nl}
#' 
#' @examples
#' ## See \link{StEM} function.
#'  
#' @export 
#################################################################################  
variance.StEM = function(estimates, M0, M1, data, Mmax)
{
  
  #Summarize fit
  alpha = estimates$alpha
  beta =  estimates$beta
  gamma = estimates$gamma
  mu =    estimates$mu
  eta =   estimates$eta
  pi =    estimates$pi
  
  pi.index1 = rep(1:length(data$nvalues), data$nvalues-1)  
  pi.index2 = rep(1:length(data$nvalues), times=data$nvalues)
  
  pTrue = list()
  for(i in 1:length(data$nvalues))
  { 
    tmp = exp(c(0,pi[pi.index1==i]))
    pTrue[[i]] = tmp/sum(tmp)
  }
  
  truepivsA = data$pivsA
  truepivsB = data$pivsB
  D = Matrix(0,nrow=nrow(data$pivsA),ncol=nrow(data$pivsB), sparse=TRUE)
   
  npars = sum(sapply(estimates, length))
  
  #############################################################################
  #Burnin-period
  
  pb = progress_bar$new(format = "Burn-in period [:bar] :percent in :elapsed",
                        total = M0, clear = FALSE, width= 60)
  for(j in 1:M0)
  {
    newTruePivs = simulateW_fast(data=data, D=D, mu=mu, eta=eta, pTrue=pTrue)
    truepivsA = newTruePivs$truepivsA
    truepivsB = newTruePivs$truepivsB
    D = simulateD(data=data, D=D, truepivsA=truepivsA, truepivsB=truepivsB, mu=mu, eta=eta, pTrue=pTrue, alpha=alpha, beta=beta, gamma=gamma)
    pb$tick()
  }
  
  pb$terminate()
  check = 0
  
  H = Matrix(0, npars, npars)
  J = c()
  
  while(check==0)
  {
    #############################################################################
    #Run Gibbs sampler
    
    V1 = list()
    V0 = list()
    Vmu = list()
    Vpi = list()
    
    pb = progress_bar$new(format = "Sample [:bar] :percent in :elapsed",
                          total = M1, clear = FALSE, width= 60)

    for(j in 1:M1)
    {
      newTruePivs = simulateW_fast(data=data, D=D, mu=mu, eta=eta, pTrue=pTrue)
      truepivsA = newTruePivs$truepivsA
      truepivsB = newTruePivs$truepivsB
      D = simulateD(data=data, D=D, truepivsA=truepivsA, truepivsB=truepivsB, mu=mu, eta=eta, pTrue=pTrue, alpha=alpha, beta=beta, gamma=gamma)
     
      #Save results for M-step
      matches = which(D==1, arr.ind=TRUE)
      survivalData = data.frame(T = data$rangeB[2] - data$U, E=0, Xindex=1:nrow(data$X))
      survivalData$T[matches[,1]] = data$V[matches[,2]] -  data$U[matches[,1]]
      survivalData$E[matches[,1]] = 1  
      
      V1[[j]] = survivalData
      V0[[j]] = data$V[!(1:nrow(data$pivsB) %in% matches[,2])]
      
      tmp = list()
      for(k in 1:length(data$nvalues))
      {
        facpivsA = factor(truepivsA[,k], levels=1:data$nvalues[k])
        facpivsB = factor(truepivsB[,k], levels=1:data$nvalues[k])
        
        tmp[[k]] = table(facpivsA[rowSums(D)==0]) + table(facpivsB[colSums(D)==0]) +  table(facpivsA[rowSums(D)==1])
      }
      Vpi[[j]] = tmp
      
      #Count agreements
      tmp = c()
      for(k in 1:length(data$nvalues))
        tmp[k] = sum(truepivsA[,k]==data$pivsA[,k]) + sum(truepivsB[,k]==data$pivsB[,k])
      Vmu[[j]] = tmp
      
      pb$tick()
    }   
    
    #####################################################################
    #Survival model
    
    index = 1:length(alpha) 
    H1 = hessian(loglikV1, c(alpha,beta), index=index, V1=count(do.call("rbind",V1)), data=data)
    
    J1 = t(sapply(V1, function(v1)
    {
      v1$freq = 1
      jacobian(loglikV1, c(alpha,beta), index=index, V1=v1, data=data, method="simple")
    }))
    
    #####################################################################
    #Distribution of event times in non-matches
    
    H2 = hessian(loglikV0, gamma, b=data$intervalsV, V0=count(do.call("c", V0)), reference=data$referenceV0)
    
    J2 = t(sapply(V0, function(v0)  
    {
      jacobian(loglikV0, gamma, b=data$intervalsV, V0=count(v0), reference=data$referenceV0, method="simple")
    }))
    
    #####################################################################
    #Mu: disagreement in PIVS
    
    H3 = array(0, rep(length(mu),2))
    N_agree = colSums(do.call("rbind", Vmu))
    N_tot = length(Vmu) * (nrow(truepivsA) + nrow(truepivsB) - data$missing_pivsA - data$missing_pivsB)
    
    for(k in 1:length(data$nvalues))
    {
      freq = c(N_agree[k],  N_tot[k] - N_agree[k])
      tmp = hessian(estimateRL, mu[k], freq = freq)
      H3[k,k] = tmp
    }
    
    J3 = c()
    N_agree = do.call("rbind", Vmu)
    for(k in 1:length(data$nvalues))
    {
      N_tot_k = nrow(truepivsA) + nrow(truepivsB) - data$missing_pivsA[k] - data$missing_pivsB[k]
      
      tmp = sapply(N_agree[,k], function(counts)
      { 
        freq = c(N_agree[k],  N_tot_k - counts)
        c(NA, jacobian(estimateRL, mu[k], freq = freq, method="simple"))
      })
      
      J3 = cbind(J3,  t(tmp)[,-1])
    }
    
    #####################################################################
    #Eta: missings
    
    eta.index = rep(1:length(data$nvalues),each=2)
    H4 = array(0, rep(length(eta),2))
    diag(H4) = NA
    
    N_tot = length(Vmu) * (nrow(truepivsA) + nrow(truepivsB))

    for(k in 1:length(data$nvalues))
    { 
      if(data$missing_pivsA[k]>0)
      {
        freq = c(data$missing_pivsA[k], N_tot - data$missing_pivsA[k])
        tmp = hessian(estimateRL, eta[eta.index==k][1], freq = freq)  
        tmp2 = which(eta.index==k)[1]
        H4[tmp2,tmp2] = tmp
      } 
      if(data$missing_pivsB[k]>0)
      {
        freq = c(data$missing_pivsB[k], N_tot - data$missing_pivsB[k])
        tmp = hessian(estimateRL, eta[eta.index==k][2], freq = freq)  
        tmp2 = which(eta.index==k)[2]
        H4[tmp2,tmp2] = tmp
      }
    }
     
    J4 = matrix(0, ncol=length(eta), nrow=M1)
  
    #####################################################################
    #True values of the PIVS
    
    H5 = array(0, rep(length(pi),2)) 
    start = 0 
    
    for(k in 1:length(data$nvalues))
    {
      n_pi = rowSums(sapply(Vpi, function(x) x[[k]]))
      tmp = hessian(estimateRL, pi[pi.index1==k], freq = n_pi)
      H5[(start+1):(start+ncol(tmp)), (start+1):(start+ncol(tmp))] = tmp
      start = start + ncol(tmp)
    }
    
    J5 = c()  
    n_pi = do.call("rbind", Vpi)
    for(k in 1:length(data$nvalues))
    {
      n_pi = sapply(Vpi, function(x) x[[k]])
      tmp = apply(n_pi, 2, function(counts)
      {
        c(NA, jacobian(estimateRL, pi[pi.index1==k], freq = counts, method="simple"))
      })
      J5 = cbind(J5,  t(tmp)[,-1])
    }
    
    #####################################################################
    
    start=0
    H[(start+1):(start+ncol(H1)), (start+1):(start+ncol(H1))] = H[(start+1):(start+ncol(H1)), (start+1):(start+ncol(H1))] - H1
    start=start + ncol(H1)
    H[(start+1):(start+ncol(H2)), (start+1):(start+ncol(H2))] = H[(start+1):(start+ncol(H2)), (start+1):(start+ncol(H2))] - H2
    start=start + ncol(H2)
    H[(start+1):(start+ncol(H3)), (start+1):(start+ncol(H3))] = H[(start+1):(start+ncol(H3)), (start+1):(start+ncol(H3))] - H3
    start=start + ncol(H3)
    H[(start+1):(start+ncol(H4)), (start+1):(start+ncol(H4))] = H[(start+1):(start+ncol(H4)), (start+1):(start+ncol(H4))] - H4
    start=start + ncol(H4)
    H[(start+1):(start+ncol(H5)), (start+1):(start+ncol(H5))] = H[(start+1):(start+ncol(H5)), (start+1):(start+ncol(H5))] - H5
  
    J = rbind(J, -cbind(J1, J2, J3, J4, J5))
    
    ######################################################################
  
    #Calculate variance
    #Remove the not estimated parameters
    notEstimated = is.na(diag(H)) | diag(H)==0
    
    Jx = J[,!notEstimated]
    Hx = H[!notEstimated,!notEstimated]
    
    Hmean = Hx/nrow(Jx) 
    varJ = var(Jx) *(nrow(Jx)-1)/nrow(Jx) 
    I = -Hmean - varJ
    covar = solve(I)  
    
    #All variances are positive (minimum requirement)
    if(all(diag(covar)>0))
      check = 1
     
    #Reached maximum sample size
    if(nrow(J)>=Mmax)
      check = 2
    
    #Order of parameters
    est = c(alpha,beta, gamma,mu,eta, pi)
    est = est[!notEstimated]
    out = data.frame(est, se = sqrt(diag(covar)))
    
    labels_out = c(paste("alpha", names(alpha), sep="_"),
                   paste("beta", names(beta), sep="_"),
                   paste("gamma", names(gamma), sep="_"),
                   paste("mu", names(mu), sep="_"),
                   paste("eta", names(eta), sep="_"),
                   paste("pi", names(pi), sep="_"))
    
    row.names(out) = labels_out[!notEstimated]
  }
    
  row.names(covar) = colnames(covar) = row.names(out)
  list(estimates = out, check = check, covar=covar, nSamples = nrow(J))
}







