#include <iostream>
#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;
using std::sqrt;

//' Running Standard Deviation assuming Mean = 0
//'
//' @param x numeric vector
//' @param w integer, windows size, results are right aligned if w < 0, left aligned if w > 0
//' @param min_size if number of non-NA elements is fewer than min_size, return NA
//'
//' @export
// [[Rcpp::export]]
NumericVector running_sd0(NumericVector x, int w, int min_size=2) {
    size_t sz  = x.size();
    size_t cnt = 0;  // non_na counts
    NumericVector res(sz, NA_REAL);

    if (w == 0) throw std::range_error("window size w must be non-zero");
    
    bool left_aligned = w < 0;
    w = w < 0 ? -w : w;

    if (w > sz) throw std::range_error("Invalid window size");

    if (min_size < 2) {
        Rcout << "[Warn] min_size must be at least 2, set to 2" << std::endl;
        min_size = 2;
    }

    size_t i   = 0;
    double sxx = 0;
    if (left_aligned) {
        for (i=0; i<sz; ++i) {
            if (!NumericVector::is_na(x[i])) {
                ++cnt;
                sxx += x[i] * x[i];
            }
            if (i >= w && !NumericVector::is_na(x[i-w])) {
                --cnt;
                sxx -= x[i-w] * x[i-w];
            }
            if (cnt >= min_size) {
                double var = sxx / cnt;
                if (var < 0) { // precision safe
                    res[i] = 0.0;
                } else {
                    res[i] = sqrt(var);
                }
            }
        }
    } else {
        // right aligned
        for (i=0; i<w; ++i) {
            if (!NumericVector::is_na(x[i])) {
                ++cnt;
                sxx += x[i] * x[i];
            }
        }

        for (; i<sz+w; ++i) {
            if (cnt >= min_size) {
                double var = sxx / cnt;
                if (var < 0) { // precision safe
                    res[i-w] = 0.0;
                } else {
                    res[i-w] = sqrt(var);
                }
            }
            if (i<sz && !NumericVector::is_na(x[i])) {
                ++cnt;
                sxx += x[i] * x[i];
            }
            if (!NumericVector::is_na(x[i-w])) {
                --cnt;
                sxx -= x[i-w] * x[i-w];
            }
        }
    }

    return res;

}