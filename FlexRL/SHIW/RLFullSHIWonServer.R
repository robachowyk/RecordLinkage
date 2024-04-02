### IMPORTS
library(exchanger)
library(comparator)
library(clevr)
library(progress)
library(Matrix)
library(testit)
library("BRL")
source("recordlinkage.r")
Rcpp:::sourceCpp("functions.cpp")

newDirectory = sprintf("SHIW %s", Sys.time())
dir.create(newDirectory)

### SAVE PERFORMANCE RESULTS
results = data.frame(matrix(0, nrow=7, ncol=4))
colnames(results) = c("Exchanger", "BRL", "FlexRL_noinstability", "Naive")
rownames(results) = c("F1Score", "Precision", "Recall", "FN", "FP", "TP", "MatrixDistance")

### IMPORT DATA
DF = read.csv("SHIWdata.csv")
DF = DF[,c("SESSO","PAR","ANASCI","STACIV","IREG","ANNO","ID")]
### CHECK IF WE CAN FILTER ON ANNO
sum( is.na(DF$ANNO) )

B = DF[DF$ANNO==2008,]
A = DF[DF$ANNO==2010,]

PIVs_config = list( SESSO      = list(stable = TRUE),
                    PAR        = list(stable = TRUE),
                    ANASCI     = list(stable = TRUE),
                    STACIV     = list(stable = TRUE),
                    IREG       = list(stable = TRUE) )

PIVs = names(PIVs_config)

PIVs_stable = sapply(PIVs_config, function(x) x$stable)

### NO MISSING VALUES
sum( is.na(A[,PIVs]) )
sum( is.na(B[,PIVs]) )

### FILTER THE DATA ON THE INTERSECTING SUPPORT OF THE PIVS
for(i in 1:length(PIVs)){
  intersect_support_piv = intersect( unique(A[,PIVs[i]]), unique(B[,PIVs[i]]) )
  A = A[A[,PIVs[i]] %in% intersect_support_piv,]
  B = B[B[,PIVs[i]] %in% intersect_support_piv,]
}

### A IS THE SMALLEST FILE
nbrRecordsA = nrow(A)
nbrRecordsB = nrow(B)
nbrRecordsA < nbrRecordsB

linkedID = intersect(A$ID, B$ID)
Nlinks = length( linkedID )

### CREATE THE TRUE DELTA WITH TRUE IDENTIFIERS
Delta = matrix(0, nrow=nbrRecordsA, ncol=nbrRecordsB)
for (i in 1:Nlinks)
{
  id = linkedID[i]
  idA = which(A$ID == id)
  idB = which(B$ID == id)
  Delta[idA,idB] = 1
}

### MISSING IN THE WHOLE DATASETS (ENCODED WITH NA)
NA_A = ( colSums(is.na(A)) / nrow(A) )[PIVs]
NA_B = ( colSums(is.na(B)) / nrow(B) )[PIVs]

### MISSING IN THE TRUELINKS (ENCODED WITH NA)
NA_A_true = ( colSums(is.na(A[A$ID %in% linkedID,])) / Nlinks )[PIVs]
NA_B_true = ( colSums(is.na(B[B$ID %in% linkedID,])) / Nlinks )[PIVs]

### UNIQUE VALUES IN THE WHOLE DATASETS (ENCODED WITH NA)
unique_values = sapply( rbind(A[,PIVs],B[,PIVs]), function(x) length(unique(x[!(is.na(x))])) )

### AGREEMENTS IN THE TRUE LINKS
recapAgreementsTrueLinks = data.frame(matrix(0, nrow=0, ncol=length(PIVs)))
colnames(recapAgreementsTrueLinks) = PIVs
for( i in 1:Nlinks ){
  matricule_id = linkedID[i]
  entityA = A[A$ID == matricule_id,]
  entityB = B[B$ID == matricule_id,]
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
write.csv(df, file.path(newDirectory, "datasetSHIW_recapstory.csv"))

### PROCESSING FOR TRUE LINKS
A$localID = 1:nrow(A)
B$localID = 1:nrow(B)

A$source = "A"
B$source = "B"

### ENCODE THE DATA FOR FLEX RL
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
sapply( encodedA[,PIVs], function(x) sort(unique(x[x!=0])) )
sapply( encodedB[,PIVs], function(x) sort(unique(x[x!=0])) )
DeltaNaive = matrix(0, nrow=nrow(A), ncol=nrow(B))
# A NOT MISSING THAT MATCH WITH B
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
# A MISSING THAT MATCH WITH B
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
# B NOT MISSING THAT MATCH WITH A
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
# B MISSING THAT MATCH WITH A
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
### PERFORMANCE METRICS
linked_pairs = data.frame(which(DeltaNaive>0.5, arr.ind=TRUE))
linked_pairs = do.call(paste, c(linked_pairs[,c("row","col")], list(sep="_")))
true_pairs = data.frame(which(Delta==1, arr.ind=TRUE))
true_pairs = do.call(paste, c(true_pairs[,c("row","col")], list(sep="_")))
truepositive = length( intersect(linked_pairs,true_pairs) )
falsepositive = length( setdiff(linked_pairs, true_pairs) )
falsenegative = length( setdiff(true_pairs, linked_pairs)  )
precision = truepositive / (truepositive + falsepositive)
recall = truepositive / (truepositive + falsenegative)
f1score = 2 * (precision * recall) / (precision + recall)
results["F1Score","Naive"] = f1score
results["Precision","Naive"] = precision
results["Recall","Naive"] = recall
results["FN","Naive"] = falsenegative
results["FP","Naive"] = falsepositive
results["TP","Naive"] = truepositive

### FLEXRL (all PIVs stable)
dataSimu = list( A             = encodedA,
                 B             = encodedB, 
                 PIVs_config   = PIVs_config,
                 PIVs          = PIVs,
                 PIVs_stable   = PIVs_stable,
                 Nvalues       = nvalues,
                 newDirectory  = newDirectory )

fit = stEM(  data               = dataSimu,
             StEMIter           = 200, 
             StEMBurnin         = 175, 
             GibbsIter          = 800, 
             GibbsBurnin        = 200 )

### PERFORMANCE METRICS
linked_pairs = data.frame(which(fit$Delta>0.5, arr.ind=TRUE))
linked_pairs = do.call(paste, c(linked_pairs[,c("row","col")], list(sep="_")))
true_pairs = data.frame(which(Delta==1, arr.ind=TRUE))
true_pairs = do.call(paste, c(true_pairs[,c("row","col")], list(sep="_")))
truepositive = length( intersect(linked_pairs,true_pairs) )
falsepositive = length( setdiff(linked_pairs, true_pairs) )
falsenegative = length( setdiff(true_pairs, linked_pairs)  )
precision = truepositive / (truepositive + falsepositive)
recall = truepositive / (truepositive + falsenegative)
f1score = 2 * (precision * recall) / (precision + recall)
results["F1Score","FlexRL_noinstability"] = f1score
results["Precision","FlexRL_noinstability"] = precision
results["Recall","FlexRL_noinstability"] = recall
results["FN","FlexRL_noinstability"] = falsenegative
results["FP","FlexRL_noinstability"] = falsepositive
results["TP","FlexRL_noinstability"] = truepositive

write.csv(results, file.path(newDirectory, "datasetSHIW_results.csv"))

fit_gamma = (fit$gamma)[ , drop=FALSE]

fit_phi = lapply(fit$phi, function(x) x[, , drop=FALSE])

write.csv(fit_gamma, file.path(newDirectory, "datasetSHIW_results_gamma.csv"))
write.csv(fit_phi[[1]][,1], file.path(newDirectory, "datasetSHIW_results_phi_agree_V1SESSO.csv"))
write.csv(fit_phi[[2]][,1], file.path(newDirectory, "datasetSHIW_results_phi_agree_V2PAR.csv"))
write.csv(fit_phi[[3]][,1], file.path(newDirectory, "datasetSHIW_results_phi_agree_V3ANASCI.csv"))
write.csv(fit_phi[[4]][,1], file.path(newDirectory, "datasetSHIW_results_phi_agree_V4STACIV.csv"))
write.csv(fit_phi[[5]][,1], file.path(newDirectory, "datasetSHIW_results_phi_agree_V5IREG.csv"))
