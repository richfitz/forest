#include "util.hpp"
#include <Rcpp.h>

// NOTE: I suspect that when I'm done I can push this into tree_rcpp.h
// as an inline function?
namespace forest {
void stop(const std::string& message) {
  Rcpp::stop(message);
}
}
