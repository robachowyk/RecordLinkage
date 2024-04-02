load("myEnvironment.RData")
load("myEnvironmentLocal.RData")

### IMPORTS
library(exchanger)
library(comparator)
library(clevr)
library(progress)
library(Matrix)
library(testit)
library("BRL")
source("../recordlinkage.r")
Rcpp:::sourceCpp("../functions.cpp")

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
DeltaTrue = matrix(0, nrow=nbrRecordsA, ncol=nbrRecordsB)
for (i in 1:Nlinks)
{
  id = linkedID[i]
  idA = which(A$ID == id)
  idB = which(B$ID == id)
  DeltaTrue[idA,idB] = 1
}

# linkedpairs = which(fit$Delta>0.5, arr.ind=TRUE)
# linkedpairsA = linkedpairs[,1]
# linkedpairsB = linkedpairs[,2]
# linkedTP = which((fit$Delta>0.5) & (Delta==1), arr.ind=TRUE)
# linkedTPA = linkedTP[,1]
# linkedTPB = linkedTP[,2]
# linkedFP = which((fit$Delta>0.5) & (Delta==0), arr.ind=TRUE)
# linkedFPA = linkedFP[,1]
# linkedFPB = linkedFP[,2]
# linkedFN = which((fit$Delta<0.5) & (Delta==1), arr.ind=TRUE)
# linkedFNA = linkedFN[,1]
# linkedFNB = linkedFN[,2]

linked_pairs = data.frame(which(Delta>0.5, arr.ind=TRUE))
linked_pairs = do.call(paste, c(linked_pairs[,c("row","col")], list(sep="_")))

# non_linked_pairs = data.frame(which(Delta<0.5, arr.ind=TRUE))
# non_linked_pairs = do.call(paste, c(non_linked_pairs[,c("row","col")], list(sep="_")))

true_pairs = data.frame(which(DeltaTrue==1, arr.ind=TRUE))
true_pairs = do.call(paste, c(true_pairs[,c("row","col")], list(sep="_")))

# true_not_pairs = data.frame(which(DeltaTrue==0, arr.ind=TRUE))
# true_not_pairs = do.call(paste, c(true_not_pairs[,c("row","col")], list(sep="_")))

truepositive = length( intersect(linked_pairs,true_pairs) )

falsepositive = length( setdiff(linked_pairs,true_pairs) )

falsenegative = length( setdiff(true_pairs, linked_pairs)  )

# truepositive = sum( (Delta>0.5) & (DeltaTrue==1) )
# falsepositive = sum( (Delta>0.5) & (DeltaTrue==0) )
# falsenegative = sum( (Delta<0.5) & (DeltaTrue==1) )
precision = truepositive / (truepositive + falsepositive)
recall = truepositive / (truepositive + falsenegative)
f1score = 2 * (precision * recall) / (precision + recall)
# distance = sqrt( sum( (fit$Delta - Delta)**2 ) )
results["F1Score","FlexRL_noinstability"] = f1score
results["Precision","FlexRL_noinstability"] = precision
results["Recall","FlexRL_noinstability"] = recall
results["FN","FlexRL_noinstability"] = falsenegative
results["FP","FlexRL_noinstability"] = falsepositive
results["TP","FlexRL_noinstability"] = truepositive