#include <Rcpp.h>
#include <algorithm>
#include <math.h>
using namespace Rcpp;

//' @useDynLib ruisu
//' @importFrom Rcpp evalCpp
// [[Rcpp::export(name="p2pDistance")]]
NumericMatrix p2pDistance(NumericMatrix dataFrom, NumericMatrix dataTo, bool greatCircle = false){

  double pi = 3.141592653589793;
  double earthRadius = 6371.0090667;
  int nrows = dataFrom.nrow();
  int ncols = dataTo.nrow();
  double tempValue;

  NumericMatrix outputData(nrows, ncols);

  for(int j = 0; j < ncols + 1; j++){
    for(int i = 0; i < nrows + 1; i++){

      if(greatCircle == false){
        double xValue = dataFrom(i, 0) - dataTo(j, 0);
        double yValue = dataFrom(i, 1) - dataTo(j, 1);

        tempValue = sqrt(xValue*xValue + yValue*yValue);
      }else{

        double lon1 = dataFrom(i, 0)/180*pi;
        double lon2 = dataFrom(j, 0)/180*pi;
        double lat1 = dataFrom(i, 1)/180*pi;
        double lat2 = dataFrom(j, 1)/180*pi;

        tempValue = acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2)*cos(lon1-lon2))*earthRadius;
      }

      outputData(i, j) = tempValue;
    }
  }

  return outputData;
}
