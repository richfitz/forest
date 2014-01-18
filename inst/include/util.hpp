#ifndef _FOREST_UTIL_HPP_
#define _FOREST_UTIL_HPP_

#include <Rcpp.h>

namespace forest {
namespace util {

// Adapted from the C++ FAQ
inline double string_to_double(const std::string& str) {
  std::istringstream i(str);
  double x;
  if (!(i >> x))
    Rcpp::stop("failed to convert " + str + " to double");
  return x;
}

}
}

#endif
