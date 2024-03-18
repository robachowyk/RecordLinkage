### IMPORTS

### OUR STEM
library(exchanger)
library(comparator)
library(clevr)
library(progress)
library(Matrix)
library(testit)
library("BRL")
source("recordlinkage.r")
Rcpp:::sourceCpp("functions.cpp")

newDirectory = sprintf("NLTCS %s", Sys.time())
dir.create(newDirectory)

### SAVE PERFORMANCE RESULTS
results = data.frame(matrix(0, nrow=8, ncol=5))
colnames(results) = c("ExchangerSelf", "Exchanger", "BRL", "FlexRL_noinstability", "Naive")
rownames(results) = c("F1Score", "Precision", "Recall", "FN", "FP", "TP", "MatrixDistance", "ExchangerDedup")

### IMPORT DATA
B = read.table("completeA1982.txt")
A = read.table("completeB1994.txt") # A has to be the smallest dataset: 1994

PIVs_config = list( sex      = list(stable = TRUE),
                    dob_yy   = list(stable = TRUE),
                    dob_mm   = list(stable = TRUE),
                    dob_dd   = list(stable = TRUE),
                    state    = list(stable = TRUE),
                    reg      = list(stable = TRUE))
PIVs = names(PIVs_config)
PIVs_stable = sapply(PIVs_config, function(x) x$stable)

### FORMAT THE MISSING VALUES
A[,PIVs][A[,PIVs] == 0] <- NA
B[,PIVs][B[,PIVs] == 0] <- NA

B$helperdo1 = rep(NA,nrow(B))
B$helperdo2 = rep(NA,nrow(B))
A$helperpresent = rep(NA,nrow(A))
linkedID = intersect(A$seq, B$seq)
Nlinks = length( linkedID )
nbrRecordsA = nrow(A)
nbrRecordsB = nrow(B)

### CREATE THE TRUE DELTA WITH TRUE IDENTIFIERS
Delta = matrix(0, nrow=nbrRecordsA, ncol=nbrRecordsB)
for (i in 1:Nlinks)
{
  id = linkedID[i]
  idA = which(A$seq == id)
  idB = which(B$seq == id)
  Delta[idA,idB] = 1
}

### MISSING IN THE WHOLE DATASETS (ENCODED WITH NA)
NA_A = ( colSums(is.na(A)) / nrow(A) )[PIVs]
NA_B = ( colSums(is.na(B)) / nrow(B) )[PIVs]

### MISSING IN THE TRUELINKS (ENCODED WITH NA)
NA_A_true = ( colSums(is.na(A[A$seq %in% linkedID,])) / Nlinks )[PIVs]
NA_B_true = ( colSums(is.na(B[B$seq %in% linkedID,])) / Nlinks )[PIVs]

### UNIQUE VALUES IN THE WHOLE DATASETS (ENCODED WITH NA)
unique_values = sapply( rbind(A[,PIVs],B[,PIVs]), function(x) length(unique(x[!(is.na(x))])) )

### AGREEMENTS IN THE TRUE LINKS
recapAgreementsTrueLinks = data.frame(matrix(0, nrow=0, ncol=length(PIVs)))
colnames(recapAgreementsTrueLinks) = PIVs
for( i in 1:Nlinks ){
  matricule_id = linkedID[i]
  entityA = A[A$seq == matricule_id,]
  entityB = B[B$seq == matricule_id,]
  if(nrow(entityA)>0){
    if(nrow(entityB)>0){
      recapAgreementsTrueLinks = rbind(recapAgreementsTrueLinks, entityA[,PIVs] == entityB[,PIVs])
    }
  }
}
recapAgreementsTrueLinks = colSums( recapAgreementsTrueLinks, na.rm = TRUE ) / nrow( recapAgreementsTrueLinks )

### STORY TELLING
df = data.frame( rbind(NA_A, NA_B, NA_A_true, NA_B_true, recapAgreementsTrueLinks, unique_values, nbrRecordsA, nbrRecordsB, Nlinks) )
colnames(df) = PIVs
rownames(df) = c("NaN in A", "NaN in B", "NaN in A true", "NaN in B true", "agreements btw A and B true links", "unique values", "size A", "size B", "Nlinks")
write.csv(df, file.path(newDirectory, "datasetNLTCS_recapstory.csv"))

### PROCESSING FOR TRUE LINKS
A$localID = 1:nrow(A)
B$localID = 1:nrow(B)

A$source = "A"
B$source = "B"

linePID = system("ps -C R")
lineV = system("ps v")
write(linePID, file="memorytrackNLTCS.txt", append=TRUE)
write(lineV, file="memorytrackNLTCS.txt", append=TRUE)

###################

encodedA = A
encodedB = B
levels_PIVs = lapply(PIVs, function(x) levels(factor(as.character(c(encodedA[,x], encodedB[,x])))))
for(i in 1:length(PIVs))
{
  encodedA[,PIVs[i]] = as.numeric(factor(as.character(encodedA[,PIVs[i]]), levels=levels_PIVs[[i]]))
  encodedB[,PIVs[i]] = as.numeric(factor(as.character(encodedB[,PIVs[i]]), levels=levels_PIVs[[i]]))
}
nvalues = sapply(levels_PIVs, length)
encodedA[,PIVs][ is.na(encodedA[,PIVs]) ] = 0
encodedB[,PIVs][ is.na(encodedB[,PIVs]) ] = 0

### CHECK THE NAIVE
### Not the same support in A and B...
sapply( encodedA[,PIVs], function(x) sort(unique(x[x!=0])) )
sapply( encodedB[,PIVs], function(x) sort(unique(x[x!=0])) )
DeltaNaive = matrix(0, nrow=nrow(A), ncol=nrow(B))
# A not missing avec all B
isNotMissingA = apply(encodedA[,PIVs]!=0, 1, all)
A_PIVs_notMissing = encodedA[isNotMissingA,PIVs]
A_PIVs_notMissing_ID = encodedA[isNotMissingA,"localID"]
UA = sspaste2(as.matrix(A_PIVs_notMissing))
UB = sspaste2(as.matrix(encodedB[,PIVs]))
valuesU = unique(c(UA,UB))
UA = as.numeric(factor(UA,levels=valuesU))
UB = as.numeric(factor(UB,levels=valuesU)) 
tmpA = F2(UA, length(valuesU))
tmpB = F2(UB, length(valuesU))
select = F33(tmpA, tmpB, length(tmpA))
if(nrow(select>0)){
  for (l in 1:nrow(select))
  {
    idxA = as.integer(select[l,1])
    idxA = A_PIVs_notMissing_ID[idxA]
    idxB = as.integer(select[l,2])
    if( (idxA <= nrow(DeltaNaive)) & (idxB <= ncol(DeltaNaive)) ){
      DeltaNaive[idxA,idxB] = 1
    }
  }
}
# A missing avec all B
for(k in 1:length(PIVs)){
  isMissingA_k = encodedA[,k]==0
  A_PIVs_k_Missing = encodedA[isMissingA_k,PIVs]
  A_PIVs_k_Missing_ID = encodedA[isMissingA_k,"localID"]
  UA = sspaste2(as.matrix(A_PIVs_k_Missing[,-k]))
  UB = sspaste2(as.matrix(encodedB[,PIVs][,-k]))
  valuesU = unique(c(UA,UB))
  UA = as.numeric(factor(UA,levels=valuesU))
  UB = as.numeric(factor(UB,levels=valuesU)) 
  tmpA = F2(UA, length(valuesU))
  tmpB = F2(UB, length(valuesU))
  select = F33(tmpA, tmpB, length(tmpA))
  if(nrow(select>0)){
    for (l in 1:nrow(select))
    {
      idxA = as.integer(select[l,1])
      idxA = A_PIVs_k_Missing_ID[idxA]
      idxB = as.integer(select[l,2])
      if( (idxA <= nrow(DeltaNaive)) & (idxB <= ncol(DeltaNaive)) ){
        DeltaNaive[idxA,idxB] = 1
      }
    }
  }
}
# B not missing avec all A
isNotMissingB = apply(encodedB[,PIVs]!=0, 1, all)
B_PIVs_notMissing = encodedB[isNotMissingB,PIVs]
B_PIVs_notMissing_ID = encodedB[isNotMissingB,"localID"]
UB = sspaste2(as.matrix(B_PIVs_notMissing))
UA = sspaste2(as.matrix(encodedA[,PIVs]))
valuesU = unique(c(UA,UB))
UA = as.numeric(factor(UA,levels=valuesU))
UB = as.numeric(factor(UB,levels=valuesU)) 
tmpA = F2(UA, length(valuesU))
tmpB = F2(UB, length(valuesU))
select = F33(tmpA, tmpB, length(tmpA))
if(nrow(select>0)){
  for (l in 1:nrow(select))
  {
    idxA = as.integer(select[l,1])
    idxB = as.integer(select[l,2])
    idxB = B_PIVs_notMissing_ID[idxB]
    if( (idxA <= nrow(DeltaNaive)) & (idxB <= ncol(DeltaNaive)) ){
      DeltaNaive[idxA,idxB] = 1
    }
  }
}
# B missing avec all A
for(k in 1:length(PIVs)){
  isMissingB_k = encodedB[,k]==0
  B_PIVs_k_Missing = encodedB[isMissingB_k,PIVs]
  B_PIVs_k_Missing_ID = encodedB[isMissingB_k,"localID"]
  UB = sspaste2(as.matrix(B_PIVs_k_Missing[,-k]))
  UA = sspaste2(as.matrix(encodedA[,PIVs][,-k]))
  valuesU = unique(c(UA,UB))
  UA = as.numeric(factor(UA,levels=valuesU))
  UB = as.numeric(factor(UB,levels=valuesU)) 
  tmpA = F2(UA, length(valuesU))
  tmpB = F2(UB, length(valuesU))
  select = F33(tmpA, tmpB, length(tmpA))
  if(nrow(select>0)){
    for (l in 1:nrow(select))
    {
      idxA = as.integer(select[l,1])
      idxB = as.integer(select[l,2])
      idxB = B_PIVs_k_Missing_ID[idxB]
      if( (idxA <= nrow(DeltaNaive)) & (idxB <= ncol(DeltaNaive)) ){
        DeltaNaive[idxA,idxB] = 1
      }
    }
  }
}
linkedpairs = which(DeltaNaive>0.5, arr.ind=TRUE)
linkedpairsA = linkedpairs[,1]
linkedpairsB = linkedpairs[,2]
linkedTP = which((DeltaNaive>0.5) & (Delta==1), arr.ind=TRUE)
linkedTPA = linkedTP[,1]
linkedTPB = linkedTP[,2]
linkedFP = which((DeltaNaive>0.5) & (Delta==0), arr.ind=TRUE)
linkedFPA = linkedFP[,1]
linkedFPB = linkedFP[,2]
linkedFN = which((DeltaNaive<0.5) & (Delta==1), arr.ind=TRUE)
linkedFNA = linkedFN[,1]
linkedFNB = linkedFN[,2]
### MISSING IN LINKED PAIRS
NA_A = ( colSums(is.na(A[linkedpairsA,])) / length(linkedpairsA) )[PIVs]
NA_B = ( colSums(is.na(B[linkedpairsB,])) / length(linkedpairsB) )[PIVs]
### AGREEMENTS IN LINKED PAIRS
recapAgreementsLinkedPairs = data.frame(matrix(0, nrow=0, ncol=length(PIVs)))
colnames(recapAgreementsLinkedPairs) = PIVs
for( i in 1:nrow(linkedpairs) ){
  entityA = A[linkedpairsA[i],]
  entityB = B[linkedpairsB[i],]
  if(nrow(entityA)>0){
    if(nrow(entityB)>0){
      recapAgreementsLinkedPairs = rbind(recapAgreementsLinkedPairs, entityA[,PIVs] == entityB[,PIVs])
    }
  }
}
recapAgreementsLinkedPairs = colSums( recapAgreementsLinkedPairs, na.rm = TRUE ) / nrow( recapAgreementsLinkedPairs )
### STORY TELLING
df = data.frame( rbind(NA_A, NA_B, recapAgreementsLinkedPairs) )
colnames(df) = PIVs
rownames(df) = c("NaN in A", "NaN in B", "agreements btw linked pairs")
write.csv(df, file.path(newDirectory, "datasetNLTCS_recaplinkedpairs_Naive.csv"))
### MISSING IN TP
NA_A = ( colSums(is.na(A[linkedTPA,])) / length(linkedTPA) )[PIVs]
NA_B = ( colSums(is.na(B[linkedTPB,])) / length(linkedTPB) )[PIVs]
### AGREEMENTS IN TP
recapAgreementsTP = data.frame(matrix(0, nrow=0, ncol=length(PIVs)))
colnames(recapAgreementsTP) = PIVs
for( i in 1:nrow(linkedTP) ){
  entityA = A[linkedTPA[i],]
  entityB = B[linkedTPB[i],]
  if(nrow(entityA)>0){
    if(nrow(entityB)>0){
      recapAgreementsTP = rbind(recapAgreementsTP, entityA[,PIVs] == entityB[,PIVs])
    }
  }
}
recapAgreementsTP = colSums( recapAgreementsTP, na.rm = TRUE ) / nrow( recapAgreementsTP )
### STORY TELLING
df = data.frame( rbind(NA_A, NA_B, recapAgreementsTP) )
colnames(df) = PIVs
rownames(df) = c("NaN in A", "NaN in B", "agreements btw TP")
write.csv(df, file.path(newDirectory, "datasetNLTCS_recaplinkedTP_Naive.csv"))
### MISSING IN FP
NA_A = ( colSums(is.na(A[linkedFPA,])) / length(linkedFPA) )[PIVs]
NA_B = ( colSums(is.na(B[linkedFPB,])) / length(linkedFPB) )[PIVs]
### AGREEMENTS IN FP
recapAgreementsFP = data.frame(matrix(0, nrow=0, ncol=length(PIVs)))
colnames(recapAgreementsFP) = PIVs
for( i in 1:nrow(linkedFP) ){
  entityA = A[linkedFPA[i],]
  entityB = B[linkedFPB[i],]
  if(nrow(entityA)>0){
    if(nrow(entityB)>0){
      recapAgreementsFP = rbind(recapAgreementsFP, entityA[,PIVs] == entityB[,PIVs])
    }
  }
}
recapAgreementsFP = colSums( recapAgreementsFP, na.rm = TRUE ) / nrow( recapAgreementsFP )
### STORY TELLING
df = data.frame( rbind(NA_A, NA_B, recapAgreementsFP) )
colnames(df) = PIVs
rownames(df) = c("NaN in A", "NaN in B", "agreements btw FP")
write.csv(df, file.path(newDirectory, "datasetNLTCS_recaplinkedFP_Naive.csv"))
### MISSING IN FN
NA_A = ( colSums(is.na(A[linkedFNA,])) / length(linkedFNA) )[PIVs]
NA_B = ( colSums(is.na(B[linkedFNB,])) / length(linkedFNB) )[PIVs]
### AGREEMENTS IN FN
recapAgreementsFN = data.frame(matrix(0, nrow=0, ncol=length(PIVs)))
colnames(recapAgreementsFN) = PIVs
for( i in 1:nrow(linkedFN) ){
  entityA = A[linkedFNA[i],]
  entityB = B[linkedFNB[i],]
  if(nrow(entityA)>0){
    if(nrow(entityB)>0){
      recapAgreementsFN = rbind(recapAgreementsFN, entityA[,PIVs] == entityB[,PIVs])
    }
  }
}
recapAgreementsFN = colSums( recapAgreementsFN, na.rm = TRUE ) / nrow( recapAgreementsFN )
### STORY TELLING
df = data.frame( rbind(NA_A, NA_B, recapAgreementsFN) )
colnames(df) = PIVs
rownames(df) = c("NaN in A", "NaN in B", "agreements btw FN")
write.csv(df, file.path(newDirectory, "datasetNLTCS_recaplinkedFN_Naive.csv"))
### PERFORMANCE METRICS
truepositive = sum( (DeltaNaive>0.5) & (Delta==1) )
falsepositive = sum( (DeltaNaive>0.5) & (Delta==0) )
falsenegative = sum( (DeltaNaive<0.5) & (Delta==1) )
precision = truepositive / (truepositive + falsepositive)
recall = truepositive / (truepositive + falsenegative)
f1score = 2 * (precision * recall) / (precision + recall)
distance = sqrt( sum( (DeltaNaive - Delta)**2 ) )
results["F1Score","Naive"] = f1score
results["Precision","Naive"] = precision
results["Recall","Naive"] = recall
results["FN","Naive"] = falsenegative
results["FP","Naive"] = falsepositive
results["TP","Naive"] = truepositive
results["MatrixDistance","Naive"] = distance

linePID = system("ps -C R")
lineV = system("ps v")
write(linePID, file="memorytrackNLTCS.txt", append=TRUE)
write(lineV, file="memorytrackNLTCS.txt", append=TRUE)

### LAUNCH FlexRL (all stable)
dataSimu = list( A             = encodedA,
                 B             = encodedB, 
                 PIVs_config   = PIVs_config,
                 PIVs          = PIVs,
                 PIVs_stable   = PIVs_stable,
                 Nvalues       = nvalues,
                 newDirectory  = newDirectory )

fit = stEM(  data               = dataSimu,
             StEMIter           = 300, 
             StEMBurnin         = 275, 
             GibbsIter          = 300, 
             GibbsBurnin        = 250 )

linkedpairs = which(fit$Delta>0.5, arr.ind=TRUE)
linkedpairsA = linkedpairs[,1]
linkedpairsB = linkedpairs[,2]
linkedTP = which((fit$Delta>0.5) & (Delta==1), arr.ind=TRUE)
linkedTPA = linkedTP[,1]
linkedTPB = linkedTP[,2]
linkedFP = which((fit$Delta>0.5) & (Delta==0), arr.ind=TRUE)
linkedFPA = linkedFP[,1]
linkedFPB = linkedFP[,2]
linkedFN = which((fit$Delta<0.5) & (Delta==1), arr.ind=TRUE)
linkedFNA = linkedFN[,1]
linkedFNB = linkedFN[,2]
### MISSING IN LINKED PAIRS
NA_A = ( colSums(is.na(A[linkedpairsA,])) / length(linkedpairsA) )[PIVs]
NA_B = ( colSums(is.na(B[linkedpairsB,])) / length(linkedpairsB) )[PIVs]
### AGREEMENTS IN LINKED PAIRS
recapAgreementsLinkedPairs = data.frame(matrix(0, nrow=0, ncol=length(PIVs)))
colnames(recapAgreementsLinkedPairs) = PIVs
for( i in 1:nrow(linkedpairs) ){
  entityA = A[linkedpairsA[i],]
  entityB = B[linkedpairsB[i],]
  if(nrow(entityA)>0){
    if(nrow(entityB)>0){
      recapAgreementsLinkedPairs = rbind(recapAgreementsLinkedPairs, entityA[,PIVs] == entityB[,PIVs])
    }
  }
}
recapAgreementsLinkedPairs = colSums( recapAgreementsLinkedPairs, na.rm = TRUE ) / nrow( recapAgreementsLinkedPairs )
### STORY TELLING
df = data.frame( rbind(NA_A, NA_B, recapAgreementsLinkedPairs) )
colnames(df) = PIVs
rownames(df) = c("NaN in A", "NaN in B", "agreements btw linked pairs")
write.csv(df, file.path(newDirectory, "datasetNLTCS_recaplinkedpairs_ours.csv"))
### MISSING IN TP
NA_A = ( colSums(is.na(A[linkedTPA,])) / length(linkedTPA) )[PIVs]
NA_B = ( colSums(is.na(B[linkedTPB,])) / length(linkedTPB) )[PIVs]
### AGREEMENTS IN TP
recapAgreementsTP = data.frame(matrix(0, nrow=0, ncol=length(PIVs)))
colnames(recapAgreementsTP) = PIVs
for( i in 1:nrow(linkedTP) ){
  entityA = A[linkedTPA[i],]
  entityB = B[linkedTPB[i],]
  if(nrow(entityA)>0){
    if(nrow(entityB)>0){
      recapAgreementsTP = rbind(recapAgreementsTP, entityA[,PIVs] == entityB[,PIVs])
    }
  }
}
recapAgreementsTP = colSums( recapAgreementsTP, na.rm = TRUE ) / nrow( recapAgreementsTP )
### STORY TELLING
df = data.frame( rbind(NA_A, NA_B, recapAgreementsTP) )
colnames(df) = PIVs
rownames(df) = c("NaN in A", "NaN in B", "agreements btw TP")
write.csv(df, file.path(newDirectory, "datasetNLTCS_recaplinkedTP_ours.csv"))
### MISSING IN FP
NA_A = ( colSums(is.na(A[linkedFPA,])) / length(linkedFPA) )[PIVs]
NA_B = ( colSums(is.na(B[linkedFPB,])) / length(linkedFPB) )[PIVs]
### AGREEMENTS IN FP
recapAgreementsFP = data.frame(matrix(0, nrow=0, ncol=length(PIVs)))
colnames(recapAgreementsFP) = PIVs
for( i in 1:nrow(linkedFP) ){
  entityA = A[linkedFPA[i],]
  entityB = B[linkedFPB[i],]
  if(nrow(entityA)>0){
    if(nrow(entityB)>0){
      recapAgreementsFP = rbind(recapAgreementsFP, entityA[,PIVs] == entityB[,PIVs])
    }
  }
}
recapAgreementsFP = colSums( recapAgreementsFP, na.rm = TRUE ) / nrow( recapAgreementsFP )
### STORY TELLING
df = data.frame( rbind(NA_A, NA_B, recapAgreementsFP) )
colnames(df) = PIVs
rownames(df) = c("NaN in A", "NaN in B", "agreements btw FP")
write.csv(df, file.path(newDirectory, "datasetNLTCS_recaplinkedFP_ours.csv"))
### MISSING IN FN
NA_A = ( colSums(is.na(A[linkedFNA,])) / length(linkedFNA) )[PIVs]
NA_B = ( colSums(is.na(B[linkedFNB,])) / length(linkedFNB) )[PIVs]
### AGREEMENTS IN FN
recapAgreementsFN = data.frame(matrix(0, nrow=0, ncol=length(PIVs)))
colnames(recapAgreementsFN) = PIVs
for( i in 1:nrow(linkedFN) ){
  entityA = A[linkedFNA[i],]
  entityB = B[linkedFNB[i],]
  if(nrow(entityA)>0){
    if(nrow(entityB)>0){
      recapAgreementsFN = rbind(recapAgreementsFN, entityA[,PIVs] == entityB[,PIVs])
    }
  }
}
recapAgreementsFN = colSums( recapAgreementsFN, na.rm = TRUE ) / nrow( recapAgreementsFN )
### STORY TELLING
df = data.frame( rbind(NA_A, NA_B, recapAgreementsFN) )
colnames(df) = PIVs
rownames(df) = c("NaN in A", "NaN in B", "agreements btw FN")
write.csv(df, file.path(newDirectory, "datasetNLTCS_recaplinkedFN_ours.csv"))
### PERFORMANCE METRICS
truepositive = sum( (fit$Delta>0.5) & (Delta==1) )
falsepositive = sum( (fit$Delta>0.5) & (Delta==0) )
falsenegative = sum( (fit$Delta<0.5) & (Delta==1) )
precision = truepositive / (truepositive + falsepositive)
recall = truepositive / (truepositive + falsenegative)
f1score = 2 * (precision * recall) / (precision + recall)
distance = sqrt( sum( (fit$Delta - Delta)**2 ) )
results["F1Score","FlexRL_noinstability"] = f1score
results["Precision","FlexRL_noinstability"] = precision
results["Recall","FlexRL_noinstability"] = recall
results["FN","FlexRL_noinstability"] = falsenegative
results["FP","FlexRL_noinstability"] = falsepositive
results["TP","FlexRL_noinstability"] = truepositive
results["MatrixDistance","FlexRL_noinstability"] = distance

fit_gamma = (fit$gamma)[ , drop=FALSE]

fit_phi = lapply(fit$phi, function(x) x[, , drop=FALSE])

write.csv(fit_gamma, file.path(newDirectory, "datasetNLTCS_results_gamma.csv"))
write.csv(fit_phi[[1]][,1], file.path(newDirectory, "datasetNLTCS_results_phi_agree_V1.csv"))
write.csv(fit_phi[[2]][,1], file.path(newDirectory, "datasetNLTCS_results_phi_agree_V2.csv"))
write.csv(fit_phi[[3]][,1], file.path(newDirectory, "datasetNLTCS_results_phi_agree_V3.csv"))
write.csv(fit_phi[[4]][,1], file.path(newDirectory, "datasetNLTCS_results_phi_agree_V4.csv"))
write.csv(fit_phi[[5]][,1], file.path(newDirectory, "datasetNLTCS_results_phi_agree_V5.csv"))
write.csv(fit_phi[[6]][,1], file.path(newDirectory, "datasetNLTCS_results_phi_agree_V6.csv"))

write.csv(results, file.path(newDirectory, "datasetNLTCS_results.csv"))

linePID = system("ps -C R")
lineV = system("ps v")
write(linePID, file="memorytrackNLTCS.txt", append=TRUE)
write(lineV, file="memorytrackNLTCS.txt", append=TRUE)

###################

RLdata = rbind(A, B)
rownames(RLdata) = 1:nrow(RLdata)
true_pairs = cbind( rownames(RLdata[(RLdata$source=="A")&(RLdata$seq %in% linkedID),]),
                    rownames(RLdata[(RLdata$source=="B")&(RLdata$seq %in% linkedID),]) )

### EXCHANGER
distort_prior <- BetaRV(1, 4)
attr_params <- list(
  sex = CategoricalAttribute(distort_prior, 
                             distort_dist_prior = DirichletProcess(GammaRV(2, 1e-4)),
                             entity_dist_prior = DirichletRV(1.0)),
  dob_yy = CategoricalAttribute(distort_prob_prior = distort_prior, 
                                distort_dist_prior = DirichletProcess(GammaRV(2, 1e-4)),
                                entity_dist_prior = DirichletRV(1.0)),
  dob_mm = CategoricalAttribute(distort_prob_prior = distort_prior, 
                                distort_dist_prior = DirichletProcess(GammaRV(2, 1e-4)),
                                entity_dist_prior = DirichletRV(1.0)),
  dob_dd = CategoricalAttribute(distort_prob_prior = distort_prior, 
                                distort_dist_prior = DirichletProcess(GammaRV(2, 1e-4)),
                                entity_dist_prior = DirichletRV(1.0)),
  state = CategoricalAttribute(distort_prior, 
                               distort_dist_prior = DirichletProcess(GammaRV(2, 1e-4)),
                               entity_dist_prior = DirichletRV(1.0)),
  reg = CategoricalAttribute(distort_prior, 
                               distort_dist_prior = DirichletProcess(GammaRV(2, 1e-4)),
                               entity_dist_prior = DirichletRV(1.0))
)
clust_prior <- PitmanYorRP(alpha = GammaRV(1, .01), d = BetaRV(1, 1))
model <- exchanger(RLdata, attr_params, clust_prior)
result <- exchanger::run_inference(model, n_samples=20000, thin_interval=10, burnin_interval=10000)
pred_clust <- smp_clusters(result)
n_records <- nrow(RLdata)
pred_pairs <- clusters_to_pairs(pred_clust)
measures <- eval_report_pairs(true_pairs, pred_pairs, num_pairs=n_records*(n_records-1)/2)
DeltaExchanger = matrix(0, nrow=nrow(A), ncol=nrow(B))
dedup = 0
for (l in 1:nrow(pred_pairs))
{
  idx_row_RLdata_A = as.integer(pred_pairs[l,1])
  idx_row_RLdata_B = as.integer(pred_pairs[l,2])
  idxA = RLdata[rownames(RLdata) == idx_row_RLdata_A, ]$localID
  idxB = RLdata[rownames(RLdata) == idx_row_RLdata_B, ]$localID
  if( (idxA <= nrow(DeltaExchanger)) & (idxB <= ncol(DeltaExchanger)) ){
    DeltaExchanger[idxA,idxB] = 1
  }else{
    dedup = dedup + 1
  }
}
linkedpairs = which(DeltaExchanger>0.5, arr.ind=TRUE)
linkedpairsA = linkedpairs[,1]
linkedpairsB = linkedpairs[,2]
linkedTP = which((DeltaExchanger>0.5) & (Delta==1), arr.ind=TRUE)
linkedTPA = linkedTP[,1]
linkedTPB = linkedTP[,2]
linkedFP = which((DeltaExchanger>0.5) & (Delta==0), arr.ind=TRUE)
linkedFPA = linkedFP[,1]
linkedFPB = linkedFP[,2]
linkedFN = which((DeltaExchanger<0.5) & (Delta==1), arr.ind=TRUE)
linkedFNA = linkedFN[,1]
linkedFNB = linkedFN[,2]
### MISSING IN LINKED PAIRS
NA_A = ( colSums(is.na(A[linkedpairsA,])) / length(linkedpairsA) )[PIVs]
NA_B = ( colSums(is.na(B[linkedpairsB,])) / length(linkedpairsB) )[PIVs]
### AGREEMENTS IN LINKED PAIRS
recapAgreementsLinkedPairs = data.frame(matrix(0, nrow=0, ncol=length(PIVs)))
colnames(recapAgreementsLinkedPairs) = PIVs
for( i in 1:nrow(linkedpairs) ){
  entityA = A[linkedpairsA[i],]
  entityB = B[linkedpairsB[i],]
  if(nrow(entityA)>0){
    if(nrow(entityB)>0){
      recapAgreementsLinkedPairs = rbind(recapAgreementsLinkedPairs, entityA[,PIVs] == entityB[,PIVs])
    }
  }
}
recapAgreementsLinkedPairs = colSums( recapAgreementsLinkedPairs, na.rm = TRUE ) / nrow( recapAgreementsLinkedPairs )
### STORY TELLING
df = data.frame( rbind(NA_A, NA_B, recapAgreementsLinkedPairs) )
colnames(df) = PIVs
rownames(df) = c("NaN in A", "NaN in B", "agreements btw linked pairs")
write.csv(df, file.path(newDirectory, "datasetNLTCS_recaplinkedpairs_Exchanger.csv"))
### MISSING IN TP
NA_A = ( colSums(is.na(A[linkedTPA,])) / length(linkedTPA) )[PIVs]
NA_B = ( colSums(is.na(B[linkedTPB,])) / length(linkedTPB) )[PIVs]
### AGREEMENTS IN TP
recapAgreementsTP = data.frame(matrix(0, nrow=0, ncol=length(PIVs)))
colnames(recapAgreementsTP) = PIVs
for( i in 1:nrow(linkedTP) ){
  entityA = A[linkedTPA[i],]
  entityB = B[linkedTPB[i],]
  if(nrow(entityA)>0){
    if(nrow(entityB)>0){
      recapAgreementsTP = rbind(recapAgreementsTP, entityA[,PIVs] == entityB[,PIVs])
    }
  }
}
recapAgreementsTP = colSums( recapAgreementsTP, na.rm = TRUE ) / nrow( recapAgreementsTP )
### STORY TELLING
df = data.frame( rbind(NA_A, NA_B, recapAgreementsTP) )
colnames(df) = PIVs
rownames(df) = c("NaN in A", "NaN in B", "agreements btw TP")
write.csv(df, file.path(newDirectory, "datasetNLTCS_recaplinkedTP_Exchanger.csv"))
### MISSING IN FP
NA_A = ( colSums(is.na(A[linkedFPA,])) / length(linkedFPA) )[PIVs]
NA_B = ( colSums(is.na(B[linkedFPB,])) / length(linkedFPB) )[PIVs]
### AGREEMENTS IN FP
recapAgreementsFP = data.frame(matrix(0, nrow=0, ncol=length(PIVs)))
colnames(recapAgreementsFP) = PIVs
for( i in 1:nrow(linkedFP) ){
  entityA = A[linkedFPA[i],]
  entityB = B[linkedFPB[i],]
  if(nrow(entityA)>0){
    if(nrow(entityB)>0){
      recapAgreementsFP = rbind(recapAgreementsFP, entityA[,PIVs] == entityB[,PIVs])
    }
  }
}
recapAgreementsFP = colSums( recapAgreementsFP, na.rm = TRUE ) / nrow( recapAgreementsFP )
### STORY TELLING
df = data.frame( rbind(NA_A, NA_B, recapAgreementsFP) )
colnames(df) = PIVs
rownames(df) = c("NaN in A", "NaN in B", "agreements btw FP")
write.csv(df, file.path(newDirectory, "datasetNLTCS_recaplinkedFP_Exchanger.csv"))
### MISSING IN FN
NA_A = ( colSums(is.na(A[linkedFNA,])) / length(linkedFNA) )[PIVs]
NA_B = ( colSums(is.na(B[linkedFNB,])) / length(linkedFNB) )[PIVs]
### AGREEMENTS IN FN
recapAgreementsFN = data.frame(matrix(0, nrow=0, ncol=length(PIVs)))
colnames(recapAgreementsFN) = PIVs
for( i in 1:nrow(linkedFN) ){
  entityA = A[linkedFNA[i],]
  entityB = B[linkedFNB[i],]
  if(nrow(entityA)>0){
    if(nrow(entityB)>0){
      recapAgreementsFN = rbind(recapAgreementsFN, entityA[,PIVs] == entityB[,PIVs])
    }
  }
}
recapAgreementsFN = colSums( recapAgreementsFN, na.rm = TRUE ) / nrow( recapAgreementsFN )
### STORY TELLING
df = data.frame( rbind(NA_A, NA_B, recapAgreementsFN) )
colnames(df) = PIVs
rownames(df) = c("NaN in A", "NaN in B", "agreements btw FN")
write.csv(df, file.path(newDirectory, "datasetNLTCS_recaplinkedFN_Exchanger.csv"))
### PERFORMANCE METRICS
truepositive = sum( (DeltaExchanger>0.5) & (Delta==1) )
falsepositive = sum( (DeltaExchanger>0.5) & (Delta==0) ) + dedup
falsenegative = sum( (DeltaExchanger<0.5) & (Delta==1) )
precision = truepositive / (truepositive + falsepositive)
recall = truepositive / (truepositive + falsenegative)
f1score = 2 * (precision * recall) / (precision + recall)
distance = sqrt( sum( (DeltaExchanger - Delta)**2 ) )
results["F1Score","Exchanger"] = f1score
results["Precision","Exchanger"] = precision
results["Recall","Exchanger"] = recall
results["FN","Exchanger"] = falsenegative
results["FP","Exchanger"] = falsepositive
results["TP","Exchanger"] = truepositive
results["MatrixDistance","Exchanger"] = distance
results["ExchangerDedup","Exchanger"] = dedup

results["F1Score","ExchangerSelf"] = measures$f1score
results["Precision","ExchangerSelf"] = measures$precision
results["Recall","ExchangerSelf"] = measures$recall

write.csv(results, file.path(newDirectory, "datasetNLTCS_results.csv"))

linePID = system("ps -C R")
lineV = system("ps v")
write(linePID, file="memorytrackNLTCS.txt", append=TRUE)
write(lineV, file="memorytrackNLTCS.txt", append=TRUE)

### BRL
Zhat <- BRL(B, A, flds=PIVs, types=c("bi","bi","bi","bi","bi","bi"), nIter=1000) # breaks = c(0, .25, .5)
n1 <- nrow(B)
idxA = which( Zhat <= n1 ) # index in A
idxB = Zhat[ Zhat <= n1 ] # index in B
DeltaBRL = matrix(0, nrow=nrow(A), ncol=nrow(B))
for (l in 1:length(idxA))
{
  DeltaBRL[idxA[l], idxB[l]] = 1
}
linkedpairs = which(DeltaBRL>0.5, arr.ind=TRUE)
linkedpairsA = linkedpairs[,1]
linkedpairsB = linkedpairs[,2]
linkedTP = which((DeltaBRL>0.5) & (Delta==1), arr.ind=TRUE)
linkedTPA = linkedTP[,1]
linkedTPB = linkedTP[,2]
linkedFP = which((DeltaBRL>0.5) & (Delta==0), arr.ind=TRUE)
linkedFPA = linkedFP[,1]
linkedFPB = linkedFP[,2]
linkedFN = which((DeltaBRL<0.5) & (Delta==1), arr.ind=TRUE)
linkedFNA = linkedFN[,1]
linkedFNB = linkedFN[,2]
### MISSING IN LINKED PAIRS
NA_A = ( colSums(is.na(A[linkedpairsA,])) / length(linkedpairsA) )[PIVs]
NA_B = ( colSums(is.na(B[linkedpairsB,])) / length(linkedpairsB) )[PIVs]
### AGREEMENTS IN LINKED PAIRS
recapAgreementsLinkedPairs = data.frame(matrix(0, nrow=0, ncol=length(PIVs)))
colnames(recapAgreementsLinkedPairs) = PIVs
for( i in 1:nrow(linkedpairs) ){
  entityA = A[linkedpairsA[i],]
  entityB = B[linkedpairsB[i],]
  if(nrow(entityA)>0){
    if(nrow(entityB)>0){
      recapAgreementsLinkedPairs = rbind(recapAgreementsLinkedPairs, entityA[,PIVs] == entityB[,PIVs])
    }
  }
}
recapAgreementsLinkedPairs = colSums( recapAgreementsLinkedPairs, na.rm = TRUE ) / nrow( recapAgreementsLinkedPairs )
### STORY TELLING
df = data.frame( rbind(NA_A, NA_B, recapAgreementsLinkedPairs) )
colnames(df) = PIVs
rownames(df) = c("NaN in A", "NaN in B", "agreements btw linked pairs")
write.csv(df, file.path(newDirectory, "datasetNLTCS_recaplinkedpairs_BRL.csv"))
### MISSING IN TP
NA_A = ( colSums(is.na(A[linkedTPA,])) / length(linkedTPA) )[PIVs]
NA_B = ( colSums(is.na(B[linkedTPB,])) / length(linkedTPB) )[PIVs]
### AGREEMENTS IN TP
recapAgreementsTP = data.frame(matrix(0, nrow=0, ncol=length(PIVs)))
colnames(recapAgreementsTP) = PIVs
for( i in 1:nrow(linkedTP) ){
  entityA = A[linkedTPA[i],]
  entityB = B[linkedTPB[i],]
  if(nrow(entityA)>0){
    if(nrow(entityB)>0){
      recapAgreementsTP = rbind(recapAgreementsTP, entityA[,PIVs] == entityB[,PIVs])
    }
  }
}
recapAgreementsTP = colSums( recapAgreementsTP, na.rm = TRUE ) / nrow( recapAgreementsTP )
### STORY TELLING
df = data.frame( rbind(NA_A, NA_B, recapAgreementsTP) )
colnames(df) = PIVs
rownames(df) = c("NaN in A", "NaN in B", "agreements btw TP")
write.csv(df, file.path(newDirectory, "datasetNLTCS_recaplinkedTP_BRL.csv"))
### MISSING IN FP
NA_A = ( colSums(is.na(A[linkedFPA,])) / length(linkedFPA) )[PIVs]
NA_B = ( colSums(is.na(B[linkedFPB,])) / length(linkedFPB) )[PIVs]
### AGREEMENTS IN FP
recapAgreementsFP = data.frame(matrix(0, nrow=0, ncol=length(PIVs)))
colnames(recapAgreementsFP) = PIVs
for( i in 1:nrow(linkedFP) ){
  entityA = A[linkedFPA[i],]
  entityB = B[linkedFPB[i],]
  if(nrow(entityA)>0){
    if(nrow(entityB)>0){
      recapAgreementsFP = rbind(recapAgreementsFP, entityA[,PIVs] == entityB[,PIVs])
    }
  }
}
recapAgreementsFP = colSums( recapAgreementsFP, na.rm = TRUE ) / nrow( recapAgreementsFP )
### STORY TELLING
df = data.frame( rbind(NA_A, NA_B, recapAgreementsFP) )
colnames(df) = PIVs
rownames(df) = c("NaN in A", "NaN in B", "agreements btw FP")
write.csv(df, file.path(newDirectory, "datasetNLTCS_recaplinkedFP_BRL.csv"))
### MISSING IN FN
NA_A = ( colSums(is.na(A[linkedFNA,])) / length(linkedFNA) )[PIVs]
NA_B = ( colSums(is.na(B[linkedFNB,])) / length(linkedFNB) )[PIVs]
### AGREEMENTS IN FN
recapAgreementsFN = data.frame(matrix(0, nrow=0, ncol=length(PIVs)))
colnames(recapAgreementsFN) = PIVs
for( i in 1:nrow(linkedFN) ){
  entityA = A[linkedFNA[i],]
  entityB = B[linkedFNB[i],]
  if(nrow(entityA)>0){
    if(nrow(entityB)>0){
      recapAgreementsFN = rbind(recapAgreementsFN, entityA[,PIVs] == entityB[,PIVs])
    }
  }
}
recapAgreementsFN = colSums( recapAgreementsFN, na.rm = TRUE ) / nrow( recapAgreementsFN )
### STORY TELLING
df = data.frame( rbind(NA_A, NA_B, recapAgreementsFN) )
colnames(df) = PIVs
rownames(df) = c("NaN in A", "NaN in B", "agreements btw FN")
write.csv(df, file.path(newDirectory, "datasetNLTCS_recaplinkedFN_BRL.csv"))
### PERFORMANCE METRICS
truepositive = sum( (DeltaBRL>0.5) & (Delta==1) )
falsepositive = sum( (DeltaBRL>0.5) & (Delta==0) )
falsenegative = sum( (DeltaBRL<0.5) & (Delta==1) )
precision = truepositive / (truepositive + falsepositive)
recall = truepositive / (truepositive + falsenegative)
f1score = 2 * (precision * recall) / (precision + recall)
distance = sqrt( sum( (DeltaBRL - Delta)**2 ) )
results["F1Score","BRL"] = f1score
results["Precision","BRL"] = precision
results["Recall","BRL"] = recall
results["FN","BRL"] = falsenegative
results["FP","BRL"] = falsepositive
results["TP","BRL"] = truepositive
results["MatrixDistance","BRL"] = distance

write.csv(results, file.path(newDirectory, "datasetNLTCS_results.csv"))

linePID = system("ps -C R")
lineV = system("ps v")
write(linePID, file="memorytrackNLTCS.txt", append=TRUE)
write(lineV, file="memorytrackNLTCS.txt", append=TRUE)