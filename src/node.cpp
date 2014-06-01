#include <forest.h>

// NOTE: I'm using the prefix 'forest_node' here, because that's what
// forest::rnode (or forest::node<Rcpp::RObject>) objects become on
// the R side.

// [[Rcpp::export]]
forest::rnode forest_node__ctor(std::string label, double length,
                                Rcpp::RObject data) {
  return forest::rnode(label, length, data);
}
