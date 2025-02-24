#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector haversine_cpp(Rcpp::NumericVector lat1, Rcpp::NumericVector lon1, 
                            Rcpp::NumericVector lat2, Rcpp::NumericVector lon2) {
    int n = lat1.size();
    Rcpp::NumericVector distances(n);
    
    double R = 6371.0; // Earth's radius in kilometers
    double to_rad = M_PI / 180.0; // Degrees to radians conversion factor
    
    for (int i = 0; i < n; i++) {
        // Convert degrees to radians
        double phi1 = lat1[i] * to_rad;
        double phi2 = lat2[i] * to_rad;
        double dphi = (lat2[i] - lat1[i]) * to_rad;
        double dlambda = (lon2[i] - lon1[i]) * to_rad;
        
        // Haversine formula
        double a = sin(dphi / 2.0) * sin(dphi / 2.0) +
            cos(phi1) * cos(phi2) * 
            sin(dlambda / 2.0) * sin(dlambda / 2.0);
        double c = 2.0 * atan2(sqrt(a), sqrt(1 - a));
        
        distances[i] = R * c;
    }
    
    return distances;
}

