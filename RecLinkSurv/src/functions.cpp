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

//std::vector<int>! 
// [[Rcpp::export]] 
List F2(IntegerVector U, int nvals)
{
  List out(nvals);
  for (int i = 0; i < nvals; i++)
  { 
    IntegerVector tmp;
    out[i] = tmp;
  }
  
  //Which value does observation i have. Assign it to the appropriate "bucket" in the list
  for (int i = 0; i < U.length(); i++)
  {
    IntegerVector tmpX = out[U[i]-1];
    tmpX.push_back(i+1);
    out[U[i]-1] = tmpX;
  } 
  return out;
}

// Cartesian product of both lists. Which combos are possible based on the true values?
// [[Rcpp::export]] 
IntegerMatrix F33(List A, List B, int nvals)
{
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


// Fast paste function
// [[Rcpp::export]] 
CharacterVector sspaste2(IntegerMatrix A)
{
  int i = 0, j = 0;
  int sz = A.nrow();
  CharacterVector res(sz);
  
  for (std::ostringstream oss; i < sz; i++, oss.str("")) 
  {
    oss << A(i,0);
    for (j = 1; j < A.ncol(); j++)
    {
      oss << "_" << A(i,j);
    }
    res[i] = oss.str();
  }
  return res;
}






// [[Rcpp::export]] 
List sampleD(IntegerMatrix S,
             NumericVector LLA, 
             NumericVector LLB, 
             arma::sp_mat LLM, 
             NumericVector pi, 
             double loglik, 
             arma::sp_mat D, 
             int nmatches,
             LogicalVector sumRowD,
             LogicalVector sumColD)
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
      - log(1-pi(i)) + log(pi(i))
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
      + log(1-pi(i)) - log(pi(i))
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
IntegerVector sampleNM(IntegerVector X, double mu, double eta, NumericVector pTrue)
{
  IntegerVector Y(X.length());
  
  //Number of possible values c_k
  int nval = pTrue.length(); 
  
  //Create the possible values to sample from
  IntegerVector choice_set = seq_len(nval);
  
  //Possible registration errors
  double pMissing = exp(eta) / (1+exp(eta));
  double pTypo = (1-pMissing) * exp(mu)/(1+exp(mu)) / (pTrue.length()-1);
  double pSame = (1-pMissing) / (1+exp(mu));
 
  //Iterate over all elements
  for(int i = 0; i < X.length(); i++) 
  {
    // Create a vector indicating P(Registered=X|True)
    // First value is for the missings
    
    //What happens if missing:
    if(X(i) == 0)
    { 
      //All equally likely
      Y(i) = sample(choice_set, 1, false, pTrue)[0];
    }else
    {
      NumericVector help1(nval, pTypo);   
      help1(X(i)-1) = pSame; 
      
      //Joint probability to have the registered and true value
      NumericVector prob = pTrue * help1;  
      Y(i) = sample(choice_set, 1, false, prob)[0];
    }
  }
  return Y;
}


// Fast version sampling for non-matches
// [[Rcpp::export]]
IntegerVector sampleNM2(int X, double mu, double eta, NumericVector pTrue, int n)
{
  IntegerVector Y(n);
  //Number of possible values c_k
  int nval = pTrue.length(); 
  
  //Create the possible values to sample from
  IntegerVector choice_set = seq_len(nval);
  
  //Possible registration errors
  double pMissing = exp(eta) / (1+exp(eta));
  double pTypo = (1-pMissing) * exp(mu)/(1+exp(mu)) / (pTrue.length()-1);
  double pSame = (1-pMissing) / (1+exp(mu));
   
  //What happens if missing:
  if(X == 0)
  { 
    NumericVector prob = pTrue;  
    Y = Rcpp::sample(choice_set, n, true, prob);
  }else
  {
    NumericVector help1(nval, pTypo);   
    help1(X-1) = pSame; 
    
    //Joint probability to have the registered and true value
    NumericVector prob = pTrue * help1;  
    Y =  Rcpp::sample(choice_set, n, true, prob);
  }
  return Y;
}
//////////////////////////////////////////////////////////////////////////////////////////////////


// [[Rcpp::export]]
IntegerVector sampleM(IntegerVector A, IntegerVector B, NumericVector pTrue, double mu, NumericVector eta)
{
  IntegerVector Y(A.length());
  int nval = pTrue.length();  
  IntegerVector choice_set = seq_len(nval);

  //Three possible actions for file A
  double pMissingA = exp(eta[0]) / (1+exp(eta[0]));
  double pTypoA = (1-pMissingA) * exp(mu)/(1+exp(mu)) / (pTrue.length()-1);
  double pSameA = (1-pMissingA) / (1+exp(mu));
  
  //Three possible actions for file B
  double pMissingB = exp(eta[1]) / (1+exp(eta[1]));
  double pTypoB = (1-pMissingB) * exp(mu)/(1+exp(mu)) / (pTrue.length()-1);
  double pSameB = (1-pMissingB) / (1+exp(mu));
  
  //Iterate over all elements
  for(int i = 0; i < A.length(); i++) 
  {
    //There are four options possible 
     
    //Both missing
    if(A(i)==0 && B(i)==0)
    {
      NumericVector prob = pTrue;
      Y(i) = Rcpp::sample(choice_set, 1, false, prob)[0];
      
    }else if(A(i)>0 && B(i)==0)
    {
      //Z missing
      NumericVector help1(nval, pTypoA);   
      help1(A(i)-1) = pSameA; 
      //Joint probability to have the registered and true value
      NumericVector prob = pTrue * help1;  
      Y(i) = Rcpp::sample(choice_set, 1, false, prob)[0];
      
    }else if(A(i)==0 && B(i)>0)
    {
      //X missing
      NumericVector help1(nval, pTypoB);   
      help1(B(i)-1) = pSameB; 
      //Joint probability to have the registered and true value
      NumericVector prob = pTrue  * help1;  
      Y(i) = Rcpp::sample(choice_set, 1, false, prob)[0];
      
    }else if(A(i)>0 && B(i)>0)
    {  //None missing
      // Create vectors indicating P(Registered=X|True)
      NumericVector helpA(nval, pTypoA);   
      NumericVector helpB(nval, pTypoB);   
      //What if they correspond
      helpA(A(i)-1) = pSameA; 
      helpB(B(i)-1) = pSameB; 
      
      //Joint probability to have the registered and true value
      NumericVector prob = pTrue * helpA * helpB; 
      Y(i) = Rcpp::sample(choice_set, 1, false, prob)[0];
    }
  }
  return Y;
}

// Determine in which interval [L,R) the value t is located
// Function returns index starting from 0 and requires knots to be unique
//Also no extrapolation!
// [[Rcpp::export]]
int left(double t, NumericVector knots)
{
  int res = std::distance(knots.begin(), std::upper_bound(knots.begin(), knots.end(), t))-1;
  return res;
}
//--------------------------------------------------------------------

// [[Rcpp::export]]
NumericVector dpiecelin(NumericVector x, NumericVector w, NumericVector b)
{
  NumericVector pi(w.length());
  NumericVector out(x.length());
  
  double S = 0;
  for(int i = 0; i < (b.length()-1); i++)
  {
    S += ((w[i+1] + w[i]) * (b[i+1] - b[i]))/2; 
  }
  
  for(int i = 0; i < b.length(); i++)
  {
    pi[i] = w[i]/S;
  }
  
  for(int i = 0; i < x.length(); i++)
  {
    double interval = left(x[i], b);
    double tmp1 = pi[interval]   * ((b[interval+1] - x[i])/ (b[interval+1] - b[interval]));
    double tmp2 = pi[interval+1] * ((x[i]-b[interval])/ (b[interval+1] - b[interval]));
    out[i] = tmp1+tmp2;
  }
  return out;
}

//--------------------------------------------------------------------
// [[Rcpp::export]]
NumericVector Hfunc(NumericVector t, NumericVector knots, NumericVector params)
{
  //Calculate the cumulative sum for all intervals
  //This safes time
  NumericVector cumsum(knots.length()); 
  cumsum[0] = 0;
  
  for(int i = 1; i < knots.length(); i++)
    cumsum[i] = cumsum[i-1] + params[i-1] * (knots[i] - knots[i-1]);
  
  NumericVector out(t.length()); 
  for(int i = 0; i < t.length(); i++)
  {
    int index = left(t[i], knots);
    out[i] = cumsum[index] + params[index] * (t[i] - knots[index]);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector hfunc(NumericVector t, NumericVector knots, NumericVector params)
{
  NumericVector out(t.length()); 
  for(int i = 0; i < t.length(); i++)
  {
    int index = left(t[i], knots);
    out[i] = params[index];
  }
  return out;
}



