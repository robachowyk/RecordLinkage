# STORY TELLING: UNIQUE VALUES IN DATA
uniquevalues = read.csv("datasetSimu_uniquevalues.csv", row.names=1)

# sometimes: 0
non_zero = apply(uniquevalues, 1, function(x){ any(x!=0) })

uniquevalues = uniquevalues[non_zero, ]
apply(uniquevalues, 2, mean)
apply(uniquevalues, 2, stats::sd)

# SORRY TELLING: NA VALUES IN DATA
NAvalues = read.csv("datasetSimu_NA_A.csv", row.names=1) + read.csv("datasetSimu_NA_B.csv", row.names=1)
round( apply(NAvalues[non_zero,], 2, mean), 2)
round( apply(NAvalues[non_zero,], 2, stats::sd), 2)

# MISSING: AGREEMENT IN ALL DATA? -> MAY BE ENOUGH TO HAVE UNIQUE VALUES

# STORY TELLING TRUE LINKS: AGREEMENT IN TRUE LINKS
true_agree = read.csv("datasetSimu_truelinks_agree.csv", row.names=1)
round( apply(true_agree[non_zero,], 2, mean), 2)
round( apply(true_agree[non_zero,], 2, stats::sd), 2)

# STORY TELLING TRUE LINKS: NA IN TRUE LINKS (on TP and FN)
NAtruevalues = read.csv("datasetSimu_truelinks_NA_A.csv", row.names=1) + read.csv("datasetSimu_truelinks_NA_B.csv", row.names=1)
round(apply(NAtruevalues[non_zero,], 2, mean),2)
round(apply(NAtruevalues[non_zero,], 2, stats::sd),2)

# STORY TELLING TRUE LINKS: CHANGES ON V5
changes = read.csv("datasetSimu_truelinks_unstable_change.csv", row.names=1)[,'V5']
round(mean(changes[non_zero]),2)
round(stats::sd(changes[non_zero]),2)

# STORY TELLING ON LINKED PAIRS (FP, TP) and NON LINKED (FN)
BRL_linked_agree = read.csv("datasetSimuBRL_agree_linked.csv", row.names=1)
round(apply(BRL_linked_agree[non_zero,], 2, mean),2)
round(apply(BRL_linked_agree[non_zero,], 2, stats::sd),2)
BRL_TP_agree = read.csv("datasetSimuBRL_agree_TP.csv", row.names=1)
round(apply(BRL_TP_agree[non_zero,], 2, mean),2)
round(apply(BRL_TP_agree[non_zero,], 2, stats::sd),2)
BRL_FP_agree = read.csv("datasetSimuBRL_agree_FP.csv", row.names=1)
round(apply(BRL_FP_agree[non_zero,], 2, mean),2)
round(apply(BRL_FP_agree[non_zero,], 2, stats::sd),2)
BRL_FN_agree = read.csv("datasetSimuBRL_agree_FN.csv", row.names=1)
round(apply(BRL_FN_agree[non_zero,], 2, mean),2)
round(apply(BRL_FN_agree[non_zero,], 2, stats::sd),2)
# STORY TELLING ON NA 
BRL_linked_NA = read.csv("datasetSimuBRL_NAA_linked.csv", row.names=1) + read.csv("datasetSimuBRL_NAB_linked.csv", row.names=1)
round(apply(BRL_linked_NA[non_zero,], 2, mean),2)
round(apply(BRL_linked_NA[non_zero,], 2, stats::sd),2)
BRL_TP_NA = read.csv("datasetSimuBRL_NAA_TP.csv", row.names=1) + read.csv("datasetSimuBRL_NAB_TP.csv", row.names=1)
round(apply(BRL_TP_NA[non_zero,], 2, mean),2)
round(apply(BRL_TP_NA[non_zero,], 2, stats::sd),2)
BRL_FP_NA = read.csv("datasetSimuBRL_NAA_FP.csv", row.names=1) + read.csv("datasetSimuBRL_NAB_FP.csv", row.names=1)
round(apply(BRL_FP_NA[non_zero,], 2, mean),2)
round(apply(BRL_FP_NA[non_zero,], 2, stats::sd),2)
BRL_FN_NA = read.csv("datasetSimuBRL_NAA_FN.csv", row.names=1) + read.csv("datasetSimuBRL_NAB_FN.csv", row.names=1)
round(apply(BRL_FN_NA[non_zero,], 2, mean),2)
round(apply(BRL_FN_NA[non_zero,], 2, stats::sd),2)
# STORY TELLING ON CHANGES IN V5 
BRL_linked_changes_V5 = read.csv("datasetSimuBRL_unstable_change_linked.csv", row.names=1)[,'V5']
round(mean(BRL_linked_changes_V5[non_zero]),2)
round(stats::sd(BRL_linked_changes_V5[non_zero]),2)
BRL_TP_changes_V5 = read.csv("datasetSimuBRL_unstable_change_TP.csv", row.names=1)[,'V5']
round(mean(BRL_TP_changes_V5[non_zero]),2)
round(stats::sd(BRL_TP_changes_V5[non_zero]),2)
BRL_FP_changes_V5 = read.csv("datasetSimuBRL_unstable_change_FP.csv", row.names=1)[,'V5']
round(mean(BRL_FP_changes_V5[non_zero]),2)
round(stats::sd(BRL_FP_changes_V5[non_zero]),2)
BRL_FN_changes_V5 = read.csv("datasetSimuBRL_unstable_change_FN.csv", row.names=1)[,'V5']
round(mean(BRL_FN_changes_V5[non_zero]),2)
round(stats::sd(BRL_FN_changes_V5[non_zero]),2)

# STORY TELLING ON LINKED PAIRS (FP, TP) and NON LINKED (FN)
Exchanger_linked_agree = read.csv("datasetSimuExchanger_agree_linked.csv", row.names=1)
round(apply(Exchanger_linked_agree[non_zero,], 2, mean),2)
round(apply(Exchanger_linked_agree[non_zero,], 2, stats::sd),2)
Exchanger_TP_agree = read.csv("datasetSimuExchanger_agree_TP.csv", row.names=1)
round(apply(Exchanger_TP_agree[non_zero,], 2, mean),2)
round(apply(Exchanger_TP_agree[non_zero,], 2, stats::sd),2)
Exchanger_FP_agree = read.csv("datasetSimuExchanger_agree_FP.csv", row.names=1)
round(apply(Exchanger_FP_agree[non_zero,], 2, mean),2)
round(apply(Exchanger_FP_agree[non_zero,], 2, stats::sd),2)
Exchanger_FN_agree = read.csv("datasetSimuExchanger_agree_FN.csv", row.names=1)
round(apply(Exchanger_FN_agree[non_zero,], 2, mean),2)
round(apply(Exchanger_FN_agree[non_zero,], 2, stats::sd),2)
# STORY TELLING ON NA 
Exchanger_linked_NA = read.csv("datasetSimuExchanger_NAA_linked.csv", row.names=1) + read.csv("datasetSimuExchanger_NAB_linked.csv", row.names=1)
round(apply(Exchanger_linked_NA[non_zero,], 2, mean),2)
round(apply(Exchanger_linked_NA[non_zero,], 2, stats::sd),2)
Exchanger_TP_NA = read.csv("datasetSimuExchanger_NAA_TP.csv", row.names=1) + read.csv("datasetSimuExchanger_NAB_TP.csv", row.names=1)
round(apply(Exchanger_TP_NA[non_zero,], 2, mean),2)
round(apply(Exchanger_TP_NA[non_zero,], 2, stats::sd),2)
Exchanger_FP_NA = read.csv("datasetSimuExchanger_NAA_FP.csv", row.names=1) + read.csv("datasetSimuExchanger_NAB_FP.csv", row.names=1)
round(apply(Exchanger_FP_NA[non_zero,], 2, mean),2)
round(apply(Exchanger_FP_NA[non_zero,], 2, stats::sd),2)
Exchanger_FN_NA = read.csv("datasetSimuExchanger_NAA_FN.csv", row.names=1) + read.csv("datasetSimuExchanger_NAB_FN.csv", row.names=1)
round(apply(Exchanger_FN_NA[non_zero,], 2, mean),2)
round(apply(Exchanger_FN_NA[non_zero,], 2, stats::sd),2)
# STORY TELLING ON CHANGES IN V5 
Exchanger_linked_changes_V5 = read.csv("datasetSimuExchanger_unstable_change_linked.csv", row.names=1)[,'V5']
round(mean(Exchanger_linked_changes_V5[non_zero]),2)
round(stats::sd(Exchanger_linked_changes_V5[non_zero]),2)
Exchanger_TP_changes_V5 = read.csv("datasetSimuExchanger_unstable_change_TP.csv", row.names=1)[,'V5']
round(mean(Exchanger_TP_changes_V5[non_zero]),2)
round(stats::sd(Exchanger_TP_changes_V5[non_zero]),2)
Exchanger_FP_changes_V5 = read.csv("datasetSimuExchanger_unstable_change_FP.csv", row.names=1)[,'V5']
round(mean(Exchanger_FP_changes_V5[non_zero]),2)
round(stats::sd(Exchanger_FP_changes_V5[non_zero]),2)
Exchanger_FN_changes_V5 = read.csv("datasetSimuExchanger_unstable_change_FN.csv", row.names=1)[,'V5']
round(mean(Exchanger_FN_changes_V5[non_zero]),2)
round(stats::sd(Exchanger_FN_changes_V5[non_zero]),2)

# STORY TELLING ON LINKED PAIRS (FP, TP) and NON LINKED (FN)
FlexRL_instability4V5_linked_agree = read.csv("datasetSimuFlexRL_instability4V5_agree_linked.csv", row.names=1)
round(apply(FlexRL_instability4V5_linked_agree[non_zero,], 2, mean),2)
round(apply(FlexRL_instability4V5_linked_agree[non_zero,], 2, stats::sd),2)
FlexRL_instability4V5_TP_agree = read.csv("datasetSimuFlexRL_instability4V5_agree_TP.csv", row.names=1)
round(apply(FlexRL_instability4V5_TP_agree[non_zero,], 2, mean),2)
round(apply(FlexRL_instability4V5_TP_agree[non_zero,], 2, stats::sd),2)
FlexRL_instability4V5_FP_agree = read.csv("datasetSimuFlexRL_instability4V5_agree_FP.csv", row.names=1)
round(apply(FlexRL_instability4V5_FP_agree[non_zero,], 2, mean),2)
round(apply(FlexRL_instability4V5_FP_agree[non_zero,], 2, stats::sd),2)
FlexRL_instability4V5_FN_agree = read.csv("datasetSimuFlexRL_instability4V5_agree_FN.csv", row.names=1)
round(apply(FlexRL_instability4V5_FN_agree[non_zero,], 2, mean),2)
round(apply(FlexRL_instability4V5_FN_agree[non_zero,], 2, stats::sd),2)
# STORY TELLING ON NA 
FlexRL_instability4V5_linked_NA = read.csv("datasetSimuFlexRL_instability4V5_NAA_linked.csv", row.names=1) + read.csv("datasetSimuFlexRL_instability4V5_NAB_linked.csv", row.names=1)
round(apply(FlexRL_instability4V5_linked_NA[non_zero,], 2, mean),2)
round(apply(FlexRL_instability4V5_linked_NA[non_zero,], 2, stats::sd),2)
FlexRL_instability4V5_TP_NA = read.csv("datasetSimuFlexRL_instability4V5_NAA_TP.csv", row.names=1) + read.csv("datasetSimuFlexRL_instability4V5_NAB_TP.csv", row.names=1)
round(apply(FlexRL_instability4V5_TP_NA[non_zero,], 2, mean),2)
round(apply(FlexRL_instability4V5_TP_NA[non_zero,], 2, stats::sd),2)
FlexRL_instability4V5_FP_NA = read.csv("datasetSimuFlexRL_instability4V5_NAA_FP.csv", row.names=1) + read.csv("datasetSimuFlexRL_instability4V5_NAB_FP.csv", row.names=1)
round(apply(FlexRL_instability4V5_FP_NA[non_zero,], 2, mean),2)
round(apply(FlexRL_instability4V5_FP_NA[non_zero,], 2, stats::sd),2)
FlexRL_instability4V5_FN_NA = read.csv("datasetSimuFlexRL_instability4V5_NAA_FN.csv", row.names=1) + read.csv("datasetSimuFlexRL_instability4V5_NAB_FN.csv", row.names=1)
round(apply(FlexRL_instability4V5_FN_NA[non_zero,], 2, mean),2)
round(apply(FlexRL_instability4V5_FN_NA[non_zero,], 2, stats::sd),2)
# STORY TELLING ON CHANGES IN V5 
FlexRL_instability4V5_linked_changes_V5 = read.csv("datasetSimuFlexRL_instability4V5_unstable_change_linked.csv", row.names=1)[,'V5']
round(mean(FlexRL_instability4V5_linked_changes_V5[non_zero]),2)
round(stats::sd(FlexRL_instability4V5_linked_changes_V5[non_zero]),2)
FlexRL_instability4V5_TP_changes_V5 = read.csv("datasetSimuFlexRL_instability4V5_unstable_change_TP.csv", row.names=1)[,'V5']
round(mean(FlexRL_instability4V5_TP_changes_V5[non_zero]),2)
round(stats::sd(FlexRL_instability4V5_TP_changes_V5[non_zero]),2)
FlexRL_instability4V5_FP_changes_V5 = read.csv("datasetSimuFlexRL_instability4V5_unstable_change_FP.csv", row.names=1)[,'V5']
round(mean(FlexRL_instability4V5_FP_changes_V5[non_zero]),2)
round(stats::sd(FlexRL_instability4V5_FP_changes_V5[non_zero]),2)
FlexRL_instability4V5_FN_changes_V5 = read.csv("datasetSimuFlexRL_instability4V5_unstable_change_FN.csv", row.names=1)[,'V5']
round(mean(FlexRL_instability4V5_FN_changes_V5[non_zero]),2)
round(stats::sd(FlexRL_instability4V5_FN_changes_V5[non_zero]),2)

# STORY TELLING ON LINKED PAIRS (FP, TP) and NON LINKED (FN)
FlexRL_noinstability_linked_agree = read.csv("datasetSimuFlexRL_noinstability_agree_linked.csv", row.names=1)
round(apply(FlexRL_noinstability_linked_agree[non_zero,], 2, mean),2)
round(apply(FlexRL_noinstability_linked_agree[non_zero,], 2, stats::sd),2)
FlexRL_noinstability_TP_agree = read.csv("datasetSimuFlexRL_noinstability_agree_TP.csv", row.names=1)
round(apply(FlexRL_noinstability_TP_agree[non_zero,], 2, mean),2)
round(apply(FlexRL_noinstability_TP_agree[non_zero,], 2, stats::sd),2)
FlexRL_noinstability_FP_agree = read.csv("datasetSimuFlexRL_noinstability_agree_FP.csv", row.names=1)
round(apply(FlexRL_noinstability_FP_agree[non_zero,], 2, mean),2)
round(apply(FlexRL_noinstability_FP_agree[non_zero,], 2, stats::sd),2)
FlexRL_noinstability_FN_agree = read.csv("datasetSimuFlexRL_noinstability_agree_FN.csv", row.names=1)
round(apply(FlexRL_noinstability_FN_agree[non_zero,], 2, mean),2)
round(apply(FlexRL_noinstability_FN_agree[non_zero,], 2, stats::sd),2)
# STORY TELLING ON NA 
FlexRL_noinstability_linked_NA = read.csv("datasetSimuFlexRL_noinstability_NAA_linked.csv", row.names=1) + read.csv("datasetSimuFlexRL_noinstability_NAB_linked.csv", row.names=1)
round(apply(FlexRL_noinstability_linked_NA[non_zero,], 2, mean),2)
round(apply(FlexRL_noinstability_linked_NA[non_zero,], 2, stats::sd),2)
FlexRL_noinstability_TP_NA = read.csv("datasetSimuFlexRL_noinstability_NAA_TP.csv", row.names=1) + read.csv("datasetSimuFlexRL_noinstability_NAB_TP.csv", row.names=1)
round(apply(FlexRL_noinstability_TP_NA[non_zero,], 2, mean),2)
round(apply(FlexRL_noinstability_TP_NA[non_zero,], 2, stats::sd),2)
FlexRL_noinstability_FP_NA = read.csv("datasetSimuFlexRL_noinstability_NAA_FP.csv", row.names=1) + read.csv("datasetSimuFlexRL_noinstability_NAB_FP.csv", row.names=1)
round(apply(FlexRL_noinstability_FP_NA[non_zero,], 2, mean),2)
round(apply(FlexRL_noinstability_FP_NA[non_zero,], 2, stats::sd),2)
FlexRL_noinstability_FN_NA = read.csv("datasetSimuFlexRL_noinstability_NAA_FN.csv", row.names=1) + read.csv("datasetSimuFlexRL_noinstability_NAB_FN.csv", row.names=1)
round(apply(FlexRL_noinstability_FN_NA[non_zero,], 2, mean),2)
round(apply(FlexRL_noinstability_FN_NA[non_zero,], 2, stats::sd),2)
# STORY TELLING ON CHANGES IN V5 
FlexRL_noinstability_linked_changes_V5 = read.csv("datasetSimuFlexRL_noinstability_unstable_change_linked.csv", row.names=1)[,'V5']
round(mean(FlexRL_noinstability_linked_changes_V5[non_zero]),2)
round(stats::sd(FlexRL_noinstability_linked_changes_V5[non_zero]),2)
FlexRL_noinstability_TP_changes_V5 = read.csv("datasetSimuFlexRL_noinstability_unstable_change_TP.csv", row.names=1)[,'V5']
round(mean(FlexRL_noinstability_TP_changes_V5[non_zero]),2)
round(stats::sd(FlexRL_noinstability_TP_changes_V5[non_zero]),2)
FlexRL_noinstability_FP_changes_V5 = read.csv("datasetSimuFlexRL_noinstability_unstable_change_FP.csv", row.names=1)[,'V5']
round(mean(FlexRL_noinstability_FP_changes_V5[non_zero]),2)
round(stats::sd(FlexRL_noinstability_FP_changes_V5[non_zero]),2)
FlexRL_noinstability_FN_changes_V5 = read.csv("datasetSimuFlexRL_noinstability_unstable_change_FN.csv", row.names=1)[,'V5']
round(mean(FlexRL_noinstability_FN_changes_V5[non_zero]),2)
round(stats::sd(FlexRL_noinstability_FN_changes_V5[non_zero]),2)

# STORY TELLING ON LINKED PAIRS (FP, TP) and NON LINKED (FN)
Naive_linked_agree = read.csv("datasetSimuNaive_agree_linked.csv", row.names=1)
round(apply(Naive_linked_agree[non_zero,], 2, mean),2)
round(apply(Naive_linked_agree[non_zero,], 2, stats::sd),2)
Naive_TP_agree = read.csv("datasetSimuNaive_agree_TP.csv", row.names=1)
round(apply(Naive_TP_agree[non_zero,], 2, mean),2)
round(apply(Naive_TP_agree[non_zero,], 2, stats::sd),2)
Naive_FP_agree = read.csv("datasetSimuNaive_agree_FP.csv", row.names=1)
round(apply(Naive_FP_agree[non_zero,], 2, mean),2)
round(apply(Naive_FP_agree[non_zero,], 2, stats::sd),2)
Naive_FN_agree = read.csv("datasetSimuNaive_agree_FN.csv", row.names=1)
round(apply(Naive_FN_agree[non_zero,], 2, mean),2)
round(apply(Naive_FN_agree[non_zero,], 2, stats::sd),2)
# STORY TELLING ON NA 
Naive_linked_NA = read.csv("datasetSimuNaive_NAA_linked.csv", row.names=1) + read.csv("datasetSimuNaive_NAB_linked.csv", row.names=1)
round(apply(Naive_linked_NA[non_zero,], 2, mean),2)
round(apply(Naive_linked_NA[non_zero,], 2, stats::sd),2)
Naive_TP_NA = read.csv("datasetSimuNaive_NAA_TP.csv", row.names=1) + read.csv("datasetSimuNaive_NAB_TP.csv", row.names=1)
round(apply(Naive_TP_NA[non_zero,], 2, mean),2)
round(apply(Naive_TP_NA[non_zero,], 2, stats::sd),2)
Naive_FP_NA = read.csv("datasetSimuNaive_NAA_FP.csv", row.names=1) + read.csv("datasetSimuNaive_NAB_FP.csv", row.names=1)
round(apply(Naive_FP_NA[non_zero,], 2, mean),2)
round(apply(Naive_FP_NA[non_zero,], 2, stats::sd),2)
Naive_FN_NA = read.csv("datasetSimuNaive_NAA_FN.csv", row.names=1) + read.csv("datasetSimuNaive_NAB_FN.csv", row.names=1)
round(apply(Naive_FN_NA[non_zero,], 2, mean),2)
round(apply(Naive_FN_NA[non_zero,], 2, stats::sd),2)
# STORY TELLING ON CHANGES IN V5 
Naive_linked_changes_V5 = read.csv("datasetSimuNaive_unstable_change_linked.csv", row.names=1)[,'V5']
round(mean(Naive_linked_changes_V5[non_zero]),2)
round(stats::sd(Naive_linked_changes_V5[non_zero]),2)
Naive_TP_changes_V5 = read.csv("datasetSimuNaive_unstable_change_TP.csv", row.names=1)[,'V5']
round(mean(Naive_TP_changes_V5[non_zero]),2)
round(stats::sd(Naive_TP_changes_V5[non_zero]),2)
Naive_FP_changes_V5 = read.csv("datasetSimuNaive_unstable_change_FP.csv", row.names=1)[,'V5']
round(mean(Naive_FP_changes_V5[non_zero]),2)
round(stats::sd(Naive_FP_changes_V5[non_zero]),2)
Naive_FN_changes_V5 = read.csv("datasetSimuNaive_unstable_change_FN.csv", row.names=1)[,'V5']
round(mean(Naive_FN_changes_V5[non_zero]),2)
round(stats::sd(Naive_FN_changes_V5[non_zero]),2)

# RESULT: METRICS

# DISTANCE
distance = read.csv("datasetSimu_results_MatrixDistance.csv", row.names=1)
round(apply(distance[non_zero,], 2, mean),2)
round(apply(distance[non_zero,], 2, stats::sd),2)
# F1SCORE
f1score = read.csv("datasetSimu_results_f1score.csv", row.names=1)
meanF1Score = round(apply(f1score[non_zero,], 2, mean),2)
meanF1Score
sdF1Score = round(apply(f1score[non_zero,], 2, stats::sd),2)
sdF1Score
# PLOT F1SCORE
par(mfrow=c(1,1))
plot(density(f1score[non_zero,1]), main = "Distribution of F1Score", xlim = c(0,1), ylim = c(-2,50), type="l", col=1, xlab = "F1Score", ylab = "Density")
text(x = meanF1Score[1], y = max(density(f1score[non_zero,1])$y), label = sprintf("%s", meanF1Score[1]))
vec = c( sprintf("%s", colnames(f1score)[1]) )
for(method in 2:ncol(f1score)){
  lines( density(f1score[non_zero,method]), col=method )
  vec = append(vec, sprintf("%s", colnames(f1score)[method]))
  text(x = meanF1Score[method], y = max(density(f1score[non_zero,method])$y), label = sprintf("%s", meanF1Score[method]))
}
legend("topleft", legend=vec, col=seq_len(ncol(f1score)), lty=1, cex=0.8)
# PRECISION
precision = read.csv("datasetSimu_results_precision.csv", row.names=1)
round(apply(precision[non_zero,], 2, mean),2)
round(apply(precision[non_zero,], 2, stats::sd),2)
# RECALL
recall = read.csv("datasetSimu_results_recall.csv", row.names=1)
round(apply(recall[non_zero,], 2, mean),2)
round(apply(recall[non_zero,], 2, stats::sd),2)
# FN
FN = read.csv("datasetSimu_results_FN.csv", row.names=1)
round(apply(FN[non_zero,], 2, mean),0)
round(apply(FN[non_zero,], 2, stats::sd),0)
# FP
FP = read.csv("datasetSimu_results_FP.csv", row.names=1)
round(apply(FP[non_zero,], 2, mean),0)
round(apply(FP[non_zero,], 2, stats::sd),0)
# TP
TP = read.csv("datasetSimu_results_TP.csv", row.names=1)
round(apply(TP[non_zero,], 2, mean),0)
round(apply(TP[non_zero,], 2, stats::sd),0)

# disagreements = {missings, mistakes, changes} vs. agreements
# mistakes = 1 - agreements - missings - changes

true_mistakes_mean  = 1 - apply(true_agree[non_zero,], 2, mean)
true_mistakes_sd    = 1 - apply(true_agree[non_zero,], 2, stats::sd)

true_mistake_V1_mean = true_mistakes_mean["V1"]
true_mistake_V1_sd = true_mistakes_sd["V1"]

true_mistake_V2_mean = true_mistakes_mean["V2"]
true_mistake_V2_sd = true_mistakes_sd["V2"]

true_mistake_V3_mean = true_mistakes_mean["V3"]
true_mistake_V3_sd = true_mistakes_sd["V3"]

true_mistake_V4_mean = true_mistakes_mean["V4"]
true_mistake_V4_sd = true_mistakes_sd["V4"]

mistakes_V5 = 1 - true_agree[non_zero,"V5"] - changes[non_zero]
true_mistake_V5_mean = mean(unlist(mistakes_V5))
true_mistake_V5_sd = stats::sd(unlist(mistakes_V5))

### OUR METHOD: FLEXRL
#   RESULTS:
#   WITH INSTABILITY

#### PHI MISTAKE
phiV1_data = read.csv("datasetSimu_results_unstable_phi_agree_V1.csv", row.names=1)
phiV2_data = read.csv("datasetSimu_results_unstable_phi_agree_V2.csv", row.names=1)
phiV3_data = read.csv("datasetSimu_results_unstable_phi_agree_V3.csv", row.names=1)
phiV4_data = read.csv("datasetSimu_results_unstable_phi_agree_V4.csv", row.names=1)
phiV5_data = read.csv("datasetSimu_results_unstable_phi_agree_V5.csv", row.names=1)
phiV1_data = phiV1_data[non_zero,]
phiV2_data = phiV2_data[non_zero,]
phiV3_data = phiV3_data[non_zero,]
phiV4_data = phiV4_data[non_zero,]
phiV5_data = phiV5_data[non_zero,]

par(mfrow=c(2,3))
phiV1_mean_line = 1 - apply(phiV1_data, 2, mean)
phiV1_sd_shape = apply(phiV1_data, 2, stats::sd)
plot(phiV1_mean_line, type="l", ylim=c(0,1), xlab="StEM iterations", ylab=sprintf("Mistakes in PIV V1"))
title(main=c("Parameter for mistakes,", "FlexRL: instability and mistakes"), outer=TRUE, line=-2)
polygon( c(1:length(phiV1_mean_line), rev(1:length(phiV1_mean_line))), c(phiV1_mean_line-phiV1_sd_shape, rev(phiV1_mean_line+phiV1_sd_shape)), col=adjustcolor("gray",alpha.f=0.6), border=NA)
abline(h = true_mistake_V1_mean, col="red")
polygon( c(1:length(true_mistake_V1_mean), rev(1:length(true_mistake_V1_mean))), c(true_mistake_V1_mean-true_mistake_V1_sd, rev(true_mistake_V1_mean+true_mistake_V1_sd)), col=adjustcolor("red",alpha.f=0.4), border=NA)
phiV2_mean_line = 1 - apply(phiV2_data, 2, mean)
phiV2_sd_shape = apply(phiV2_data, 2, stats::sd)
plot(phiV2_mean_line, type="l", ylim=c(0,1), xlab="StEM iterations", ylab=sprintf("Mistakes in PIV V2"))
polygon( c(1:length(phiV2_mean_line), rev(1:length(phiV2_mean_line))), c(phiV2_mean_line-phiV2_sd_shape, rev(phiV2_mean_line+phiV2_sd_shape)), col=adjustcolor("gray",alpha.f=0.6), border=NA)
abline(h = true_mistake_V2_mean, col="red")
polygon( c(1:length(true_mistake_V2_mean), rev(1:length(true_mistake_V2_mean))), c(true_mistake_V2_mean-true_mistake_V2_sd, rev(true_mistake_V2_mean+true_mistake_V2_sd)), col=adjustcolor("red",alpha.f=0.4), border=NA)
phiV3_mean_line = 1 - apply(phiV3_data, 2, mean)
phiV3_sd_shape = apply(phiV3_data, 2, stats::sd)
plot(phiV3_mean_line, type="l", ylim=c(0,1), xlab="StEM iterations", ylab=sprintf("Mistakes in PIV V3"))
polygon( c(1:length(phiV3_mean_line), rev(1:length(phiV3_mean_line))), c(phiV3_mean_line-phiV3_sd_shape, rev(phiV3_mean_line+phiV3_sd_shape)), col=adjustcolor("gray",alpha.f=0.6), border=NA)
abline(h = true_mistake_V3_mean, col="red")
polygon( c(1:length(true_mistake_V3_mean), rev(1:length(true_mistake_V3_mean))), c(true_mistake_V3_mean-true_mistake_V3_sd, rev(true_mistake_V3_mean+true_mistake_V3_sd)), col=adjustcolor("red",alpha.f=0.4), border=NA)
phiV4_mean_line = 1 - apply(phiV4_data, 2, mean)
phiV4_sd_shape = apply(phiV4_data, 2, stats::sd)
plot(phiV4_mean_line, type="l", ylim=c(0,1), xlab="StEM iterations", ylab=sprintf("Mistakes in PIV V4"))
polygon( c(1:length(phiV4_mean_line), rev(1:length(phiV4_mean_line))), c(phiV4_mean_line-phiV4_sd_shape, rev(phiV4_mean_line+phiV4_sd_shape)), col=adjustcolor("gray",alpha.f=0.6), border=NA)
abline(h = true_mistake_V4_mean, col="red")
polygon( c(1:length(true_mistake_V4_mean), rev(1:length(true_mistake_V4_mean))), c(true_mistake_V4_mean-true_mistake_V4_sd, rev(true_mistake_V4_mean+true_mistake_V4_sd)), col=adjustcolor("red",alpha.f=0.4), border=NA)
phiV5_mean_line = 1 - apply(phiV5_data, 2, mean)
phiV5_sd_shape = apply(phiV5_data, 2, stats::sd)
plot(phiV5_mean_line, type="l", ylim=c(0,1), xlab="StEM iterations", ylab=sprintf("Mistakes in PIV V5"))
polygon( c(1:length(phiV5_mean_line), rev(1:length(phiV5_mean_line))), c(phiV5_mean_line-phiV5_sd_shape, rev(phiV5_mean_line+phiV5_sd_shape)), col=adjustcolor("gray",alpha.f=0.6), border=NA)
abline(h = true_mistake_V5_mean, col="red")
polygon( c(1:length(true_mistake_V5_mean), rev(1:length(true_mistake_V5_mean))), c(true_mistake_V5_mean-true_mistake_V5_sd, rev(true_mistake_V5_mean+true_mistake_V5_sd)), col=adjustcolor("red",alpha.f=0.4), border=NA)

#### GAMMA 
par(mfrow=c(1,1))
gamma_data = read.csv("datasetSimu_results_unstable_gamma.csv", row.names=1)
gamma_mean_line = apply(gamma_data[non_zero,], 2, mean)
gamma_sd_shape = apply(gamma_data[non_zero,], 2, stats::sd)
plot(gamma_mean_line, type="l", ylim=c(0,1), xlab="StEM iterations", ylab="gamma")
polygon( c(1:length(gamma_mean_line), rev(1:length(gamma_mean_line))), c(gamma_mean_line-gamma_sd_shape, rev(gamma_mean_line+gamma_sd_shape)), col=adjustcolor("gray",alpha.f=0.6), border=NA)
abline(h = 5/8, col="red")
title(main=c("Proportion of links", "FlexRL: instability for V5 and mistakes"), outer=TRUE, line=-2)

#### ALPHA
par(mfrow=c(1,2))
alpha_data = read.csv("datasetSimu_results_unstable_alpha_param.csv", row.names=1)
alpha_mean_line = apply(alpha_data[non_zero,], 2, mean)
alpha_sd_shape = apply(alpha_data[non_zero,], 2, stats::sd)
plot(alpha_mean_line, type="l", ylim=c(0,1), xlab="StEM iterations", ylab="exp alpha")
title(main=c("PIVs Dynamics", "FlexRL: instability for V5 and mistakes"), outer=TRUE, line=-2)
polygon( c(1:length(alpha_mean_line), rev(1:length(alpha_mean_line))), c(alpha_mean_line-alpha_sd_shape, rev(alpha_mean_line+alpha_sd_shape)), col=adjustcolor("gray",alpha.f=0.6), border=NA)
abline(h = 0.28, col="red")
proba_same_H = read.csv("datasetSimu_results_unstable_alpha_probaTrue.csv", row.names=1)
proba_same_H = t(apply(proba_same_H[non_zero,], 1, function(x){sort(x, decreasing=TRUE)}))
proba_same_H_mean_line = apply(proba_same_H, 2, mean)
proba_same_H_sd_shape = apply(proba_same_H, 2, stats::sd)
time_difference = read.csv("datasetSimu_results_unstable_alpha_timesTrue.csv", row.names=1)
time_difference = t(apply(time_difference[non_zero,], 1, sort))
time_difference_mean_line = apply(time_difference, 2, mean) 
time_difference_sd_shape = apply(time_difference, 2, stats::sd)
probaEstimate = read.csv("datasetSimu_results_unstable_alpha_probaEstimate.csv", row.names=1)
probaEstimate = t(apply(probaEstimate[non_zero,], 1, function(x){sort(x, decreasing=TRUE)}))
probaEstimate_mean_line = apply(probaEstimate, 2, function(x){ mean(x[x!=0]) })
probaEstimate_mean_line = probaEstimate_mean_line[!is.na(probaEstimate_mean_line)]
probaEstimate_mean_line = sort(probaEstimate_mean_line, decreasing=TRUE)
probaEstimate_sd_shape = apply(probaEstimate, 2, function(x){ stats::sd(x[x!=0]) } )
probaEstimate_sd_shape = probaEstimate_sd_shape[!is.na(probaEstimate_sd_shape)]
len = min(length(probaEstimate_mean_line), length(probaEstimate_sd_shape))
probaEstimate_mean_line = probaEstimate_mean_line[1:len]
probaEstimate_sd_shape = probaEstimate_sd_shape[1:len]
timesEstimate = read.csv("datasetSimu_results_unstable_alpha_timesEstimate.csv", row.names=1)
timesEstimate = t(apply(timesEstimate[non_zero,], 1, sort))
timesEstimate_mean_line = apply(timesEstimate, 2, function(x){ mean(x[x!=0]) })
timesEstimate_mean_line = timesEstimate_mean_line[!is.na(timesEstimate_mean_line)]
timesEstimate_mean_line = sort(timesEstimate_mean_line)
timesEstimate_sd_shape = apply(timesEstimate, 2, function(x){ stats::sd(x[x!=0]) })
timesEstimate_sd_shape = timesEstimate_sd_shape[!is.na(timesEstimate_sd_shape)]
timesEstimate_mean_line = timesEstimate_mean_line[1:len]
timesEstimate_sd_shape = timesEstimate_sd_shape[1:len]

# TRUTH
plot( time_difference_mean_line, proba_same_H_mean_line, ylim=c(0,1), type="l", xlab = "time difference", ylab = sprintf("true model for proba of no change in V5") )
polygon( c(time_difference_mean_line, rev(time_difference_mean_line)), c(proba_same_H_mean_line-proba_same_H_sd_shape, rev(proba_same_H_mean_line+proba_same_H_sd_shape)), col=adjustcolor("red",alpha.f=0.4), border=NA)
# ESTIMATION
lines( timesEstimate_mean_line, probaEstimate_mean_line, ylim=c(0,1), xlab = "time difference", ylab = sprintf("estimated model for proba of no change in V5") )
polygon( c(timesEstimate_mean_line, rev(timesEstimate_mean_line)), c(probaEstimate_mean_line-probaEstimate_sd_shape, rev(probaEstimate_mean_line+probaEstimate_sd_shape)), col=adjustcolor("gray",alpha.f=0.6), border=NA)

# # TRUTH
# plot( proba_same_H_mean_line, type="l", ylim=c(0,1), xlab = "true records pairs", ylab = sprintf("true model for proba of no change in V5") )
# polygon( c(1:length(proba_same_H_mean_line), rev(1:length(proba_same_H_mean_line))), c(proba_same_H_mean_line-proba_same_H_sd_shape, rev(proba_same_H_mean_line+proba_same_H_sd_shape)), col=adjustcolor("red",alpha.f=0.4), border=NA)
# # ESTIMATION
# lines( probaEstimate_mean_line, type="l", ylim=c(0,1), xlab = "true records pairs", ylab = sprintf("estimated model for proba of no change in V5") )
# polygon( c(1:length(probaEstimate_mean_line), rev(1:length(probaEstimate_mean_line))), c(probaEstimate_mean_line-probaEstimate_sd_shape, rev(probaEstimate_mean_line+probaEstimate_sd_shape)), col=adjustcolor("gray",alpha.f=0.6), border=NA)
# title(main=c("PIVs Dynamics", "FlexRL 2: instability and typos"))

### OUR METHOD: FLEXRL
#   RESULTS:
#   WITHOUT INSTABILITY

#### PHI MISTAKE
phiV1_data = read.csv("datasetSimu_results_stable_phi_agree_V1.csv", row.names=1)
phiV2_data = read.csv("datasetSimu_results_stable_phi_agree_V2.csv", row.names=1)
phiV3_data = read.csv("datasetSimu_results_stable_phi_agree_V3.csv", row.names=1)
phiV4_data = read.csv("datasetSimu_results_stable_phi_agree_V4.csv", row.names=1)
phiV5_data = read.csv("datasetSimu_results_stable_phi_agree_V5.csv", row.names=1)
phiV1_data = phiV1_data[non_zero,]
phiV2_data = phiV2_data[non_zero,]
phiV3_data = phiV3_data[non_zero,]
phiV4_data = phiV4_data[non_zero,]
phiV5_data = phiV5_data[non_zero,]

par(mfrow=c(2,3))
phiV1_mean_line = 1 - apply(phiV1_data, 2, mean)
phiV1_sd_shape = apply(phiV1_data, 2, stats::sd)
plot(phiV1_mean_line, type="l", ylim=c(0,1), xlab="StEM iterations", ylab=sprintf("Mistakes in PIV V1"))
title(main=c("Parameter for mistakes,", "FlexRL: all stable and mistakes"), outer=TRUE, line=-2)
polygon( c(1:length(phiV1_mean_line), rev(1:length(phiV1_mean_line))), c(phiV1_mean_line-phiV1_sd_shape, rev(phiV1_mean_line+phiV1_sd_shape)), col=adjustcolor("gray",alpha.f=0.6), border=NA)
abline(h = true_mistake_V1_mean, col="red")
polygon( c(1:length(true_mistake_V1_mean), rev(1:length(true_mistake_V1_mean))), c(true_mistake_V1_mean-true_mistake_V1_sd, rev(true_mistake_V1_mean+true_mistake_V1_sd)), col=adjustcolor("red",alpha.f=0.4), border=NA)
phiV2_mean_line = 1 - apply(phiV2_data, 2, mean)
phiV2_sd_shape = apply(phiV2_data, 2, stats::sd)
plot(phiV2_mean_line, type="l", ylim=c(0,1), xlab="StEM iterations", ylab=sprintf("Mistakes in PIV V2"))
polygon( c(1:length(phiV2_mean_line), rev(1:length(phiV2_mean_line))), c(phiV2_mean_line-phiV2_sd_shape, rev(phiV2_mean_line+phiV2_sd_shape)), col=adjustcolor("gray",alpha.f=0.6), border=NA)
abline(h = true_mistake_V2_mean, col="red")
polygon( c(1:length(true_mistake_V2_mean), rev(1:length(true_mistake_V2_mean))), c(true_mistake_V2_mean-true_mistake_V2_sd, rev(true_mistake_V2_mean+true_mistake_V2_sd)), col=adjustcolor("red",alpha.f=0.4), border=NA)
phiV3_mean_line = 1 - apply(phiV3_data, 2, mean)
phiV3_sd_shape = apply(phiV3_data, 2, stats::sd)
plot(phiV3_mean_line, type="l", ylim=c(0,1), xlab="StEM iterations", ylab=sprintf("Mistakes in PIV V3"))
polygon( c(1:length(phiV3_mean_line), rev(1:length(phiV3_mean_line))), c(phiV3_mean_line-phiV3_sd_shape, rev(phiV3_mean_line+phiV3_sd_shape)), col=adjustcolor("gray",alpha.f=0.6), border=NA)
abline(h = true_mistake_V3_mean, col="red")
polygon( c(1:length(true_mistake_V3_mean), rev(1:length(true_mistake_V3_mean))), c(true_mistake_V3_mean-true_mistake_V3_sd, rev(true_mistake_V3_mean+true_mistake_V3_sd)), col=adjustcolor("red",alpha.f=0.4), border=NA)
phiV4_mean_line = 1 - apply(phiV4_data, 2, mean)
phiV4_sd_shape = apply(phiV4_data, 2, stats::sd)
plot(phiV4_mean_line, type="l", ylim=c(0,1), xlab="StEM iterations", ylab=sprintf("Mistakes in PIV V4"))
polygon( c(1:length(phiV4_mean_line), rev(1:length(phiV4_mean_line))), c(phiV4_mean_line-phiV4_sd_shape, rev(phiV4_mean_line+phiV4_sd_shape)), col=adjustcolor("gray",alpha.f=0.6), border=NA)
abline(h = true_mistake_V4_mean, col="red")
polygon( c(1:length(true_mistake_V4_mean), rev(1:length(true_mistake_V4_mean))), c(true_mistake_V4_mean-true_mistake_V4_sd, rev(true_mistake_V4_mean+true_mistake_V4_sd)), col=adjustcolor("red",alpha.f=0.4), border=NA)
phiV5_mean_line = 1 - apply(phiV5_data, 2, mean)
phiV5_sd_shape = apply(phiV5_data, 2, stats::sd)
plot(phiV5_mean_line, type="l", ylim=c(0,1), xlab="StEM iterations", ylab=sprintf("Mistakes in PIV V5"))
polygon( c(1:length(phiV5_mean_line), rev(1:length(phiV5_mean_line))), c(phiV5_mean_line-phiV5_sd_shape, rev(phiV5_mean_line+phiV5_sd_shape)), col=adjustcolor("gray",alpha.f=0.6), border=NA)
abline(h = true_mistake_V5_mean, col="red")
polygon( c(1:length(true_mistake_V5_mean), rev(1:length(true_mistake_V5_mean))), c(true_mistake_V5_mean-true_mistake_V5_sd, rev(true_mistake_V5_mean+true_mistake_V5_sd)), col=adjustcolor("red",alpha.f=0.4), border=NA)

#### GAMMA 
par(mfrow=c(1,1))
gamma_data = read.csv("datasetSimu_results_stable_gamma.csv", row.names=1)
gamma_mean_line = apply(gamma_data[non_zero,], 2, mean)
gamma_sd_shape = apply(gamma_data[non_zero,], 2, stats::sd)
plot(gamma_mean_line, type="l", ylim=c(0,1), xlab="StEM iterations", ylab="gamma")
polygon( c(1:length(gamma_mean_line), rev(1:length(gamma_mean_line))), c(gamma_mean_line-gamma_sd_shape, rev(gamma_mean_line+gamma_sd_shape)), col=adjustcolor("gray",alpha.f=0.6), border=NA)
abline(h = 5/8, col="red")
title(main=c("Proportion of links", "FlexRL: all stable and mistakes"), outer=TRUE, line=-2)
