fullData = FALSE

# SORRY TELLING: UNIQUE VALUES IN DATA
uniquevalues = read.csv("datasetSHIW_recapstory.csv", row.names=1)["unique values",]

# SORRY TELLING: NA VALUES IN DATA
NAvalues = read.csv("datasetSHIW_recapstory.csv", row.names=1)["NaN in A",] + read.csv("datasetSHIW_recapstory.csv", row.names=1)["NaN in B",]

# MISSING: AGREEMENT IN ALL DATA? -> MAY BE ENOUGH TO HAVE UNIQUE VALUES
# STORY TELLING TRUE LINKS: AGREEMENT IN TRUE LINKS
true_agree = read.csv("datasetSHIW_recapstory.csv", row.names=1)["agreements btw A and B true links",]

# STORY TELLING TRUE LINKS: NA IN TRUE LINKS (on TP and FN)
NAtruevalues = read.csv("datasetSHIW_recapstory.csv", row.names=1)["NaN in A true",] + read.csv("datasetSHIW_recapstory.csv", row.names=1)["NaN in B true",]

# STORY TELLING TRUE LINKS: NO CHANGES

# STORY TELLING ON LINKED PAIRS (FP, TP) and NON LINKED (FN)
BRL_linked = round(read.csv("datasetSHIW_recaplinkedpairs_BRL.csv", row.names=1), 2)
BRL_TP = round(read.csv("datasetSHIW_recaplinkedTP_BRL.csv", row.names=1), 2)
BRL_FP = round(read.csv("datasetSHIW_recaplinkedFP_BRL.csv", row.names=1), 2)
BRL_FN = round(read.csv("datasetSHIW_recapnonlinkedFN_BRL.csv", row.names=1), 2)

Exchanger_linked = round(read.csv("datasetSHIW_recaplinkedpairs_Exchanger.csv", row.names=1), 2)
Exchanger_TP = round(read.csv("datasetSHIW_recaplinkedTP_Exchanger.csv", row.names=1), 2)
Exchanger_FP = round(read.csv("datasetSHIW_recaplinkedFP_Exchanger.csv", row.names=1), 2)
Exchanger_FN = round(read.csv("datasetSHIW_recapnonlinkedFN_Exchanger.csv", row.names=1), 2)

Naive_linked = round(read.csv("datasetSHIW_recaplinkedpairs_Naive.csv", row.names=1), 2)
Naive_TP = round(read.csv("datasetSHIW_recaplinkedTP_Naive.csv", row.names=1), 2)
Naive_FP = round(read.csv("datasetSHIW_recaplinkedFP_Naive.csv", row.names=1), 2)
Naive_FN = round(read.csv("datasetSHIW_recapnonlinkedFN_Naive.csv", row.names=1), 2)

ours_linked = round(read.csv("datasetSHIW_recaplinkedpairs_ours.csv", row.names=1), 2)
ours_TP = round(read.csv("datasetSHIW_recaplinkedTP_ours.csv", row.names=1), 2)
ours_FP = round(read.csv("datasetSHIW_recaplinkedFP_ours.csv", row.names=1), 2)
ours_FN = round(read.csv("datasetSHIW_recapnonlinkedFN_ours.csv", row.names=1), 2)

# RESULT: METRICS

# DISTANCE
distance = read.csv("datasetSHIW_results.csv", row.names=1)["MatrixDistance",]
# F1SCORE
f1score = read.csv("datasetSHIW_results.csv", row.names=1)["F1Score",]
# PRECISION
precision = read.csv("datasetSHIW_results.csv", row.names=1)["Precision",]
# RECALL
recall = read.csv("datasetSHIW_results.csv", row.names=1)["Recall",]
# FN
FN = read.csv("datasetSHIW_results.csv", row.names=1)["FN",]
# FP
FP = read.csv("datasetSHIW_results.csv", row.names=1)["FP",]
# TP
TP = read.csv("datasetSHIW_results.csv", row.names=1)["TP",]

### OUR METHOD: FLEXRL
#   RESULTS:
#   NO INSTABILITY BUT MISTAKES FOR ALL PIVs

#### PHI MISTAKE
phiV1_data = read.csv("datasetSHIW_results_phi_agree_V1SESSO.csv", row.names=1)
phiV2_data = read.csv("datasetSHIW_results_phi_agree_V2PAR.csv", row.names=1)
phiV3_data = read.csv("datasetSHIW_results_phi_agree_V3ANASCI.csv", row.names=1)
phiV4_data = read.csv("datasetSHIW_results_phi_agree_V4STACIV.csv", row.names=1)
if(fullData){
  phiV5_data = read.csv("datasetSHIW_results_phi_agree_V5IREG.csv", row.names=1)
}

par(mfrow=c(2,3))

true_mistake_V1_mean = 1 - true_agree[,"SESSO"]
phiV1_mean_line = 1 - unlist(phiV1_data)
plot(phiV1_mean_line, ylim=c(0,1), xlab="StEM iterations", ylab=sprintf("Mistakes in PIV V1 SESSO"), type="l")
title(main=c("Parameter for mistakes,", "FlexRL"), outer=TRUE, line=-2)
abline(h = true_mistake_V1_mean, col="red")

true_mistake_V2_mean = 1 - true_agree[,"PAR"]
phiV2_mean_line = 1 - unlist(phiV2_data)
plot(phiV2_mean_line, type="l", ylim=c(0,1), xlab="StEM iterations", ylab=sprintf("Mistakes in PIV V2 PAR"))
abline(h = true_mistake_V2_mean, col="red")

true_mistake_V3_mean = 1 - true_agree[,"ANASCI"]
phiV3_mean_line = 1 - unlist(phiV3_data)
plot(phiV3_mean_line, type="l", ylim=c(0,1), xlab="StEM iterations", ylab=sprintf("Mistakes in PIV V3 ANASCI"))
abline(h = true_mistake_V3_mean, col="red")

true_mistake_V4_mean = 1 - true_agree[,"STACIV"]
phiV4_mean_line = 1 - unlist(phiV4_data)
plot(phiV4_mean_line, type="l", ylim=c(0,1), xlab="StEM iterations", ylab=sprintf("Mistakes in PIV V4 STACIV"))
abline(h = true_mistake_V4_mean, col="red")

if(fullData){
  true_mistake_V5_mean = 1 - true_agree[,"IREG"]
  phiV5_mean_line = 1 - unlist(phiV5_data)
  plot(phiV5_mean_line, type="l", ylim=c(0,1), xlab="StEM iterations", ylab=sprintf("Mistakes in PIV V5 IREG"))
  abline(h = true_mistake_V5_mean, col="red")
}

#### GAMMA 
par(mfrow=c(1,1))
Nlinks = read.csv("datasetSHIW_recapstory.csv", row.names=1)["Nlinks","SESSO"]
sizeA = read.csv("datasetSHIW_recapstory.csv", row.names=1)["size A","SESSO"]
gamma_data = read.csv("datasetSHIW_results_gamma.csv", row.names=1)
plot(unlist(gamma_data), type="l", ylim=c(0,1), xlab="StEM iterations", ylab="gamma")
abline(h = Nlinks/sizeA, col="red")
title(main=c("Proportion of links", "FlexRL"), outer=TRUE, line=-2)