#include <Rcpp.h>
#include <algorithm>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix p2pDistance(NumericMatrix dataFrom, NumericMatrix dataTo){


  int nrows = dataFrom.nrow();
  int ncols = dataTo.nrow();

  NumericMatrix outputData(nrows, ncols);

  for(int j = 0; j < ncols + 1; j++){
    for(int i = 0; i < nrows + 1; i++){

      double xValue = dataFrom(i, 0) - dataTo(j, 0);
      double yValue = dataFrom(i, 1) - dataTo(j, 1);

      outputData(i, j) = sqrt(xValue*xValue + yValue*yValue);
    }
  }

  return outputData;
}
