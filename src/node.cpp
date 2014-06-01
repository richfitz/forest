#include <forest.h>

// [[Rcpp::export]]
forest::forest_node forest_node__ctor(std::string label, double length,
                                      Rcpp::RObject data) {
  return forest::forest_node(label, length, data);
}
