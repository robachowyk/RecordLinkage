#include <random>
#include <chrono>

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;  

static std::random_device rd; 
// initialize Mersennes' twister using rd to generate the seed
static std::mt19937 gen{rd()}; 
std::uniform_real_distribution<double> dist(0, 1);

//-----------------------------------------------------------
// [[Rcpp::export]] 
CharacterVector paste1(IntegerMatrix G)
{
  int i = 0, j = 0;
  int sz = G.nrow();
  CharacterVector res(sz);
  
  for (std::ostringstream oss; i < sz; i++, oss.str("")) 
  {
    oss << G(i,0);
    for (j = 1; j < G.ncol(); j++)
    {
      oss << "_" << G(i,j);
    }
    res[i] = oss.str();
  }
  return res;
}
//-----------------------------------------------------------



//-----------------------------------------------------------
// [[Rcpp::export]] 
List listVals(CharacterVector GX, CharacterVector levs)
{
  IntegerVector levelX = match(GX, levs);
  int nvals = levs.length();
   
  List out(nvals);
  for (int i = 0; i < nvals; i++)
  { 
   IntegerVector tmp;
   out[i] = tmp;
  }
  
  //Which value does observation i have. Assign it to the appropriate "bucket" in the list
  for (int i = 0; i < levelX.length(); i++)
  {
   IntegerVector tmpX = out[levelX[i]-1];
   tmpX.push_back(i+1);
   out[levelX[i]-1] = tmpX;
  } 
  return out;
}  




//-----------------------------------------------------------
// Cartesian product of both lists. Which combos are possible based on the true values?
// [[Rcpp::export]] 
IntegerMatrix F33(CharacterVector GA, CharacterVector GB, CharacterVector levs)
{
   
  List A = listVals(GA, levs);
  List B = listVals(GB, levs);  
  int nvals = levs.length();
  int ntotal = 0;
  
  for (int k = 0; k < nvals; k++)
  {
    //Check a bucket to see whether and how many combos are possible
    IntegerVector tmpA = A[k];
    IntegerVector tmpB = B[k];
    ntotal += tmpA.length() * tmpB.length();
  }
  IntegerMatrix tmpC(ntotal,2);
  
  int counter = 0;
  for (int k = 0; k < nvals; k++)
  {
    IntegerVector tmpA = A[k];
    IntegerVector tmpB = B[k];
    for (int i = 0; i < tmpA.length(); i++)
    {
      for (int j = 0; j < tmpB.length(); j++)
      {
        tmpC(counter,0) = tmpA[i];
        tmpC(counter,1) = tmpB[j];
        counter += 1;
      }
    }
  } 
  return tmpC;
}
//-----------------------------------------------------------
 
// [[Rcpp::export]] 
List sampleD(int niter, 
             IntegerMatrix S,
             NumericVector LLA, 
             NumericVector LLB, 
             arma::sp_mat LLM, 
             NumericVector PDX, 
             double loglik, 
             arma::sp_mat D, 
             int nmatches,
             LogicalVector sumRowD,
             LogicalVector sumColD)
{
  for (int iter = 0; iter < niter; iter++)
  {
    for (int q = 0; q < S.nrow(); q++)
    {
      int i = S(q,0)-1;
      int j = S(q,1)-1;
      
      //////////////////////////////////////////////////////////
      // If non match and possible match -> check if match
      if((sumRowD(i)==false) && (sumColD(j)==false) && LLM(i,j) < 10000000)
      {					
        double loglikNew = loglik  
        // Comparison vectors
        - LLB(j) - LLA(i) 
        + LLM(i,j)
        
        // Bipartite matching
        - log(1-PDX(i)) + log(PDX(i))
        - log(LLB.length() - nmatches);
        
        double sumlogdensity = log(1 + exp(loglik-loglikNew)) + loglikNew;
        double pchange = exp(loglikNew - sumlogdensity);		
        
        // Random  number smaller than prob -> generate binomial value
        bool match = dist(gen) < pchange;
        
        if(match)
        {
          loglik = loglikNew;
          D(i,j) = true;
          sumRowD(i) = true;
          sumColD(j) = true;
          nmatches = nmatches + 1; 
        }
        
      }else if(D(i,j)==true)
      {
        //////////////////////////////////////////////////////////
        //If match -> check if nonmatch
        
        double loglikNew = loglik
        // Comparison vectors
        + LLB(j) + LLA(i) 
        - LLM(i,j)
        
        // Bipartite matching
        + log(1-PDX(i)) - log(PDX(i))
        + log(LLB.length() - nmatches+1);
        
        double sumlogdensity = log(1 + exp(loglik-loglikNew)) + loglikNew;	
        double pchange = exp(loglikNew - sumlogdensity);		
        
        bool nonmatch = dist(gen) < pchange;
        
        if(nonmatch)
        {
          loglik = loglikNew;
          D(i,j) = false;
          sumRowD(i) = false;
          sumColD(j) = false;
          nmatches = nmatches - 1; 
        }			
      }
    }
  }
  
  // Return to R
  List ret;
  ret["D"] = D;
  ret["sumRowD"] = sumRowD;	
  ret["sumColD"] = sumColD;
  ret["loglik"] = loglik;
  ret["nmatches"] = nmatches;
  return ret;
} 







// [[Rcpp::export]]
IntegerVector sampleM(IntegerMatrix GA, IntegerMatrix GB, List pi, NumericVector mu)
{
  //Empty matrices for the result
  IntegerMatrix trueG(GA.nrow(), mu.length()); 
  
  for(int i = 0; i < GA.nrow(); i++) 
  {
    for(int j = 0; j < mu.length(); j++)
    {
      NumericVector pTrue = pi[j];
      int nval = pTrue.length();  
      IntegerVector choice_set = seq_len(nval);
      
      //Three possible actions for file A
      double pTypoA = mu[j] / (pTrue.length()-1);
      double pSameA = 1-mu[j];
      
      //Three possible actions for file B
      double pTypoB = mu[j] / (pTrue.length()-1);
      double pSameB = 1-mu[j];
      
      NumericVector helpA(nval, pTypoA);   
      NumericVector helpB(nval, pTypoB);   
      //What if they correspond
      helpA(GA(i,j)-1) = pSameA; 
      helpB(GB(i,j)-1) = pSameB; 
      
      //Joint probability to have the registered and true value
      NumericVector prob = pTrue * helpA * helpB; 
      trueG(i,j) = Rcpp::sample(choice_set, 1, false, prob)[0];
    }
  }
  return trueG;
}

// [[Rcpp::export]]
IntegerVector sampleNM(IntegerMatrix GA, List pi, NumericVector mu)
{
  //Empty matrices for the result
  IntegerMatrix trueG(GA.nrow(), mu.length()); 
  
  for(int i = 0; i < GA.nrow(); i++) 
  {
    for(int j = 0; j < mu.length(); j++)
    {
      NumericVector pTrue = pi[j];
      int nval = pTrue.length();  
      IntegerVector choice_set = seq_len(nval);
      
      //Three possible actions for file A
      double pTypoA = mu[j] / (pTrue.length()-1);
      double pSameA = 1-mu[j];
      
      NumericVector helpA(nval, pTypoA);    
      //What if they correspond
      helpA(GA(i,j)-1) = pSameA;  
      
      //Joint probability to have the registered and true value
      NumericVector prob = pTrue * helpA; 
      trueG(i,j) = Rcpp::sample(choice_set, 1, false, prob)[0];
    }
  }
  return trueG;
}
