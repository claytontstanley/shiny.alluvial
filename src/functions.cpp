#include <Rcpp.h>
using namespace Rcpp;

//' @export
// [[Rcpp::export]]
CharacterVector accumpaste(CharacterVector x, NumericVector sid) {
            int n = x.size();
            CharacterVector s(n);
            int sidC = -1;
            String sUptoLabel = "";
            for(int i = 0; i < n; ++i) {
                    if (sid[i] != sidC) {
                            sUptoLabel = "";
                    }
                    sidC = sid[i];
                    if (sUptoLabel != "") {
                            sUptoLabel += ",";
                    }
                    sUptoLabel += x[i];
                    s[i] = sUptoLabel;
            }
            return s;
}


