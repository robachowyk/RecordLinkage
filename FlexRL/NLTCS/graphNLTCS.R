

# SORRY TELLING: UNIQUE VALUES IN DATA
uniquevalues = read.csv("datasetNLTCS_recapstory.csv", row.names=1)["unique values",]

# SORRY TELLING: NA VALUES IN DATA
NAvalues = read.csv("datasetNLTCS_recapstory.csv", row.names=1)["NaN in A",] + read.csv("datasetNLTCS_recapstory.csv", row.names=1)["NaN in B",]

# MISSING: AGREEMENT IN ALL DATA? -> MAY BE ENOUGH TO HAVE UNIQUE VALUES
# STORY TELLING TRUE LINKS: AGREEMENT IN TRUE LINKS
true_agree = read.csv("datasetNLTCS_recapstory.csv", row.names=1)["agreements btw A and B true links",]

# STORY TELLING TRUE LINKS: NA IN TRUE LINKS (on TP and FN)
NAtruevalues = read.csv("datasetNLTCS_recapstory.csv", row.names=1)["NaN in A true",] + read.csv("datasetNLTCS_recapstory.csv", row.names=1)["NaN in B true",]

# STORY TELLING TRUE LINKS: NO CHANGES

# STORY TELLING ON LINKED PAIRS (FP, TP) and NON LINKED (FN)
BRL_linked = round(read.csv("datasetNLTCS_recaplinkedpairs_BRL.csv", row.names=1), 2)
BRL_TP = round(read.csv("datasetNLTCS_recaplinkedTP_BRL.csv", row.names=1), 2)
BRL_FP = round(read.csv("datasetNLTCS_recaplinkedFP_BRL.csv", row.names=1), 2)
BRL_FN = round(read.csv("datasetNLTCS_recapnonlinkedFN_BRL.csv", row.names=1), 2)

Exchanger_linked = round(read.csv("datasetNLTCS_recaplinkedpairs_Exchanger.csv", row.names=1), 2)
Exchanger_TP = round(read.csv("datasetNLTCS_recaplinkedTP_Exchanger.csv", row.names=1), 2)
Exchanger_FP = round(read.csv("datasetNLTCS_recaplinkedFP_Exchanger.csv", row.names=1), 2)
Exchanger_FN = round(read.csv("datasetNLTCS_recapnonlinkedFN_Exchanger.csv", row.names=1), 2)

Naive_linked = round(read.csv("datasetNLTCS_recaplinkedpairs_Naive.csv", row.names=1), 2)
Naive_TP = round(read.csv("datasetNLTCS_recaplinkedTP_Naive.csv", row.names=1), 2)
Naive_FP = round(read.csv("datasetNLTCS_recaplinkedFP_Naive.csv", row.names=1), 2)
Naive_FN = round(read.csv("datasetNLTCS_recapnonlinkedFN_Naive.csv", row.names=1), 2)

ours_linked = round(read.csv("datasetNLTCS_recaplinkedpairs_ours.csv", row.names=1), 2)
ours_TP = round(read.csv("datasetNLTCS_recaplinkedTP_ours.csv", row.names=1), 2)
ours_FP = round(read.csv("datasetNLTCS_recaplinkedFP_ours.csv", row.names=1), 2)
ours_FN = round(read.csv("datasetNLTCS_recapnonlinkedFN_ours.csv", row.names=1), 2)

# RESULT: METRICS

# DISTANCE
distance = read.csv("datasetNLTCS_results.csv", row.names=1)["MatrixDistance",]
# F1SCORE
f1score = read.csv("datasetNLTCS_results.csv", row.names=1)["F1Score",]
# PRECISION
precision = read.csv("datasetNLTCS_results.csv", row.names=1)["Precision",]
# RECALL
recall = read.csv("datasetNLTCS_results.csv", row.names=1)["Recall",]
# FN
FN = read.csv("datasetNLTCS_results.csv", row.names=1)["FN",]
# FP
FP = read.csv("datasetNLTCS_results.csv", row.names=1)["FP",]
# TP
TP = read.csv("datasetNLTCS_results.csv", row.names=1)["TP",]

### OUR METHOD: FLEXRL
#   RESULTS:
#   NO INSTABILITY BUT MISTAKES FOR ALL PIVs

#### PHI MISTAKE
phiV1_data = read.csv("datasetNLTCS_results_phi_agree_V1sex.csv", row.names=1)
phiV2_data = read.csv("datasetNLTCS_results_phi_agree_V2dob_yy.csv", row.names=1)
phiV3_data = read.csv("datasetNLTCS_results_phi_agree_V3dob_mm.csv", row.names=1)
phiV4_data = read.csv("datasetNLTCS_results_phi_agree_V4dob_dd.csv", row.names=1)
phiV5_data = read.csv("datasetNLTCS_results_phi_agree_V5state.csv", row.names=1)
phiV6_data = read.csv("datasetNLTCS_results_phi_agree_V6reg.csv", row.names=1)

par(mfrow=c(2,3))

true_mistake_V1_mean = 1 - true_agree[,"sex"]
phiV1_mean_line = 1 - unlist(phiV1_data)
plot(phiV1_mean_line, ylim=c(0,1), xlab="StEM iterations", ylab=sprintf("Mistakes in PIV V1 sex"), type="l")
title(main=c("Parameter for mistakes,", "FlexRL"), outer=TRUE, line=-2)
abline(h = true_mistake_V1_mean, col="red")

true_mistake_V2_mean = 1 - true_agree[,"dob_yy"]
phiV2_mean_line = 1 - unlist(phiV2_data)
plot(phiV2_mean_line, type="l", ylim=c(0,1), xlab="StEM iterations", ylab=sprintf("Mistakes in PIV V2 dob yy"))
abline(h = true_mistake_V2_mean, col="red")

true_mistake_V3_mean = 1 - true_agree[,"dob_mm"]
phiV3_mean_line = 1 - unlist(phiV3_data)
plot(phiV3_mean_line, type="l", ylim=c(0,1), xlab="StEM iterations", ylab=sprintf("Mistakes in PIV V3 dob mm"))
abline(h = true_mistake_V3_mean, col="red")

true_mistake_V4_mean = 1 - true_agree[,"dob_dd"]
phiV4_mean_line = 1 - unlist(phiV4_data)
plot(phiV4_mean_line, type="l", ylim=c(0,1), xlab="StEM iterations", ylab=sprintf("Mistakes in PIV V4 dob dd"))
abline(h = true_mistake_V4_mean, col="red")

true_mistake_V5_mean = 1 - true_agree[,"state"]
phiV5_mean_line = 1 - unlist(phiV5_data)
plot(phiV5_mean_line, type="l", ylim=c(0,1), xlab="StEM iterations", ylab=sprintf("Mistakes in PIV V5 state"))
abline(h = true_mistake_V5_mean, col="red")

true_mistake_V6_mean = 1 - true_agree[,"reg"]
phiV6_mean_line = 1 - unlist(phiV6_data)
plot(phiV6_mean_line, type="l", ylim=c(0,1), xlab="StEM iterations", ylab=sprintf("Mistakes in PIV V6 reg"))
abline(h = true_mistake_V6_mean, col="red")

#### GAMMA 
par(mfrow=c(1,1))
Nlinks = read.csv("datasetNLTCS_recapstory.csv", row.names=1)["Nlinks","sex"]
sizeA = read.csv("datasetNLTCS_recapstory.csv", row.names=1)["size A","sex"]
gamma_data = read.csv("datasetNLTCS_results_gamma.csv", row.names=1)
plot(unlist(gamma_data), type="l", ylim=c(0,1), xlab="StEM iterations", ylab="gamma")
abline(h = Nlinks/sizeA, col="red")
title(main=c("Proportion of links", "FlexRL"), outer=TRUE, line=-2)