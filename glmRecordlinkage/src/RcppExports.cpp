// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// paste1
CharacterVector paste1(IntegerMatrix G);
RcppExport SEXP _glmRecordlinkage_paste1(SEXP GSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type G(GSEXP);
    rcpp_result_gen = Rcpp::wrap(paste1(G));
    return rcpp_result_gen;
END_RCPP
}
// listVals
List listVals(CharacterVector GX, CharacterVector levs);
RcppExport SEXP _glmRecordlinkage_listVals(SEXP GXSEXP, SEXP levsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type GX(GXSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type levs(levsSEXP);
    rcpp_result_gen = Rcpp::wrap(listVals(GX, levs));
    return rcpp_result_gen;
END_RCPP
}
// F33
IntegerMatrix F33(CharacterVector GA, CharacterVector GB, CharacterVector levs);
RcppExport SEXP _glmRecordlinkage_F33(SEXP GASEXP, SEXP GBSEXP, SEXP levsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type GA(GASEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type GB(GBSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type levs(levsSEXP);
    rcpp_result_gen = Rcpp::wrap(F33(GA, GB, levs));
    return rcpp_result_gen;
END_RCPP
}
// sampleD
List sampleD(int niter, IntegerMatrix S, NumericVector LLA, NumericVector LLB, arma::sp_mat LLM, NumericVector PDX, double loglik, arma::sp_mat D, int nmatches, LogicalVector sumRowD, LogicalVector sumColD);
RcppExport SEXP _glmRecordlinkage_sampleD(SEXP niterSEXP, SEXP SSEXP, SEXP LLASEXP, SEXP LLBSEXP, SEXP LLMSEXP, SEXP PDXSEXP, SEXP loglikSEXP, SEXP DSEXP, SEXP nmatchesSEXP, SEXP sumRowDSEXP, SEXP sumColDSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type niter(niterSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type S(SSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type LLA(LLASEXP);
    Rcpp::traits::input_parameter< NumericVector >::type LLB(LLBSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type LLM(LLMSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type PDX(PDXSEXP);
    Rcpp::traits::input_parameter< double >::type loglik(loglikSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type D(DSEXP);
    Rcpp::traits::input_parameter< int >::type nmatches(nmatchesSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type sumRowD(sumRowDSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type sumColD(sumColDSEXP);
    rcpp_result_gen = Rcpp::wrap(sampleD(niter, S, LLA, LLB, LLM, PDX, loglik, D, nmatches, sumRowD, sumColD));
    return rcpp_result_gen;
END_RCPP
}
// sampleM
IntegerVector sampleM(IntegerMatrix GA, IntegerMatrix GB, List pi, NumericVector mu);
RcppExport SEXP _glmRecordlinkage_sampleM(SEXP GASEXP, SEXP GBSEXP, SEXP piSEXP, SEXP muSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type GA(GASEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type GB(GBSEXP);
    Rcpp::traits::input_parameter< List >::type pi(piSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type mu(muSEXP);
    rcpp_result_gen = Rcpp::wrap(sampleM(GA, GB, pi, mu));
    return rcpp_result_gen;
END_RCPP
}
// sampleNM
IntegerVector sampleNM(IntegerMatrix GA, List pi, NumericVector mu);
RcppExport SEXP _glmRecordlinkage_sampleNM(SEXP GASEXP, SEXP piSEXP, SEXP muSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type GA(GASEXP);
    Rcpp::traits::input_parameter< List >::type pi(piSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type mu(muSEXP);
    rcpp_result_gen = Rcpp::wrap(sampleNM(GA, pi, mu));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_glmRecordlinkage_paste1", (DL_FUNC) &_glmRecordlinkage_paste1, 1},
    {"_glmRecordlinkage_listVals", (DL_FUNC) &_glmRecordlinkage_listVals, 2},
    {"_glmRecordlinkage_F33", (DL_FUNC) &_glmRecordlinkage_F33, 3},
    {"_glmRecordlinkage_sampleD", (DL_FUNC) &_glmRecordlinkage_sampleD, 11},
    {"_glmRecordlinkage_sampleM", (DL_FUNC) &_glmRecordlinkage_sampleM, 4},
    {"_glmRecordlinkage_sampleNM", (DL_FUNC) &_glmRecordlinkage_sampleNM, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_glmRecordlinkage(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}