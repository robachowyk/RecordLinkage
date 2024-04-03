
closeAllConnections()

library(LaplacesDemon)
library(splines)
library(coda)
library(parallel)
#########################################
#Example with linear regression
############################################

#Number of observations in file A
n = 1000
#Number of observations in file B
m = 1000
#Number of unique valus partially identifying variables
Nval = c(2,2,2,2,2) + 1
#Amount of typos in the partially identifying variables
typos = rep(0.02,5) 
print(n*m/prod(Nval))

#############################################################################
#Simulate the partially identifying variables
NRecords = c(n, m)	
for(i in 1:2)
{
  dataSet=c()
  for(u in 1:length(Nval))
    dataSet = cbind(dataSet,
                    sample(1:Nval[u], NRecords[i], replace=T))
  
  dataSet = as.data.frame(dataSet)
  names(dataSet) = c(paste("V", 1:(ncol(dataSet)), sep=""))
  
  assign(paste("dataSet", i, sep=""),dataSet  )
} 

library(mvtnorm)
#############################################################################
#Simulate covariates
X = rmvnorm(n, c(0,0), diag(2))
pmatch = invlogit(cbind(1,X) %*% c(-1, 0.1,-1))
match = rbern(n, pmatch)
print(sum(match))

#############################################################################
dataSet2[which(match==1),] = dataSet1[which(match==1),]

for(x in 1:ncol(dataSet2))
{ 
  biased = as.logical(rbinom(nrow(dataSet2),1,typos[x]))
  if(sum(biased)>0)
    dataSet2[,x][biased] = sapply(dataSet2[,x][biased], function(i) sample((1:Nval[x])[-c(i)], 1))
}

#simulate the outcome (continuous)
#Outcome in matches
meany =  cbind(1,X[which(match==1),]) %*% c(-0.3,0.4, -0.2)

Ym = rnorm(length(meany), meany, 0.5)
#Outcome in observations without matches
 
Y =  rnorm(m, -0.2, 1) 
Y[which(match==1)] = Ym

X = as.data.frame(X)
names(X) = c(paste("x", 1:(ncol(X)), sep=""))
A  = as.data.frame(cbind(dataSet1, X))
B  = as.data.frame(cbind(dataSet2, Y))
head(A)
head(B)

#A$V1 = letters[A$V1]
#B$V1 = letters[B$V1]

A = A[sample(1:nrow(A), nrow(A)),]
B = B[sample(1:nrow(B), nrow(B)),]
#############################################################################

library(glmRecordlinkage)
 
data = createData(A=A, B=B, 
                   name.y = "Y", 
                   PIVs = names(A)[1:5],
                   regression.formula = ~ x1+x2, 
                   pmatch.formula = ~ ns(x1, df=4) + ns(x2, df=4),
                   family.Y1 = gaussian(),  
                   family.Y0 = gaussian())

priors = createPriors(data)
start = createStart(data)
 
priors$mu$prior = lapply(priors$mu$prior, function(x) c(1,10))

x = seq(0,1,length=100)
plot(x, dbeta(x, priors$mu$prior[[1]][1], priors$mu$prior[[1]][2]), type="l")

clust = makeCluster(4)
clusterExport(clust, c("priors", "data", "start"))
chains = clusterEvalQ(clust, 
{
  library(glmRecordlinkage)
  run0 = runGibbs(data=data, priors=priors, start=start, n.iter=100, w=0.25)
  start = createStart(data, run0)
  out = runGibbs(data=data, priors=priors, start=start, n.iter=3000, w=0.25)
  start = createStart(data, out)
  out
})

#closeAllConnections()

plot(mcmc.list(lapply(chains, function(x) x$beta)))
summary(mcmc.list(lapply(chains, function(x) x$beta)))
gelman.diag(mcmc.list(lapply(chains, function(x) x$beta)))

plot(mcmc.list(lapply(chains, function(x) x$mu)))
summary(mcmc.list(lapply(chains, function(x) x$mu)))
gelman.diag(mcmc.list(lapply(chains, function(x) x$mu)))

plot(mcmc.list(lapply(chains, function(x) x$loglik)))

plot(mcmc.list(lapply(chains, function(x) x$phi)))
summary(mcmc.list(lapply(chains, function(x) x$phi)))
gelman.diag(mcmc.list(lapply(chains, function(x) x$phi)))

x = seq(0,1,length=100)
plot(density(chains[[1]]$mu[,4]), xlim=c(0,1), lty=2)
lines(x, dbeta(x, priors$mu$prior[[1]][1], priors$mu$prior[[1]][2]), col=2)


#j=3
#plot(mcmc.list(as.mcmc(chain1$pi[[j]]), as.mcmc(chain2$pi[[j]])))

