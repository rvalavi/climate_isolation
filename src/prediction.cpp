#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector categorise_cpp(NumericMatrix m, int n) {
    int rows = m.nrow();
    NumericVector x = m(_, 0);
    NumericVector y = m(_, 1);
    
    double x_min = min(x);
    double x_max = max(x);
    double y_min = min(y);
    double y_max = max(y);
    
    // Correct way to create sequence in Rcpp
    NumericVector xr(n);
    NumericVector yr(n);
    
    double x_step = (x_max - x_min) / n;
    double y_step = (y_max - y_min) / n;
    
    for (int i = 0; i < n; i++) {
        xr[i] = x_min + (i + 1) * x_step;
        yr[i] = y_min + (i + 1) * y_step;
    }
    
    IntegerVector out(rows);
    
    for (int v = 0; v < rows; v++) {
        int i = 1;
        int j = 1;
        
        while (i < n && x[v] >= xr[i]) i++;
        while (j < n && y[v] >= yr[j]) j++;
        
        out[v] = (i - 1) * n + j;
    }
    
    return out;
}

