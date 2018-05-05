#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double getDistance(NumericVector z, NumericVector x, double p)
{
  double ret = -1.0;
  if(p == R_PosInf)
  {
    for(int i=0;  i<z.size(); i++)
    {
      if(fabs(x[i]-z[i])> ret)
        ret = fabs(x[i]-z[i]);
    }
    return ret;
  }
  else
  {
    ret = 0;
    for(int i=0; i<z.size(); i++)
      ret += pow(fabs(x[i]-z[i]), p);
    return pow(ret, 1.0/p);
  }
}

// [[Rcpp::export]]
double KthSmallest(NumericVector x, int k)
{
  x = clone(x);
  k = k - 1;
  int l=0;
  int r=x.size()-1;
  while(true)
  {
    if(l == r)
      return x[l];
    int pi = l + rand() % (r - l + 1);
    double pv = x[pi];
    x[pi] = x[r];
    x[r] = pv;
    int si = l;
    for(int i=l; i<r; i++)
    {
      if(x[i]<pv)
      {
        double temp = x[si];
        x[si] = x[i];
        x[i] = temp;
        si++;
      }
    }
    double temp = x[r];
    x[r] = x[si];
    x[si] = temp;
    pi = si;
    if(k == pi)
      return x[k];
    if(k < pi)
      r = pi - 1;
    else
      l = pi + 1;
  } 
}

// [[Rcpp::export]]
NumericMatrix knn(NumericMatrix X, NumericVector y, NumericMatrix Z, int k, double p=2) 
{
  if(X.nrow() != y.size())
    stop("X and y length mismatch.");
  if(X.ncol() != Z.ncol())
    stop("X and Z dimensions mismatch.");
  if(k < 1 || k > X.nrow())
    stop("Invalid value of k.");
  NumericMatrix out(Z.nrow(), k);
  for(int i=0; i<Z.nrow(); i++)
  {
    NumericVector dist(X.nrow());
    for(int j=0; j<X.nrow(); j++)
    {
      dist[j] = getDistance(Z(i,_), X(j,_), p);
    }
    double kth = KthSmallest(dist, k);
    NumericVector smallest(k);
    int idx = 0;
    for(int j=0; j<X.nrow(); j++)
    {
      if(dist[j] <= kth && idx < k)
      {
        smallest[idx] = dist[j];
        out(i,idx) = y[j];
        int b = idx;
        while(b > 0)
        {
          if(smallest[b-1] > smallest[b])
          {
            double tempL = out(i, b);
            double tempD = smallest[b];
            smallest[b] = smallest[b-1];
            out(i, b) = out(i, b-1);
            smallest[b-1] = tempD;
            out(i, b-1) = tempL;
          }
          b--;
        }
        idx++;
      }
    }
  }
  return out;
}


