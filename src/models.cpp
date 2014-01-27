#include "models.hpp"

forest::node<forest::models::gaussian>
test_convert(forest::rnode::node_type nd);
forest::node<forest::models::gaussian>
test_convert(forest::rnode::node_type nd) {
  forest::node<forest::models::gaussian> ret =
    forest::duplicate_node<forest::models::gaussian>(nd);
  return ret;
}

#ifdef __clang__
#pragma clang diagnostic push
// These I have no control over because they're Rcpp issues.
#pragma clang diagnostic ignored "-Wglobal-constructors"
#pragma clang diagnostic ignored "-Wexit-time-destructors"
#pragma clang diagnostic ignored "-Wmissing-prototypes"
#endif
RCPP_MODULE(models) {
#ifdef __clang__
#pragma clang diagnostic pop
#endif
  Rcpp::class_<forest::models::gaussian>("gaussian")
    .constructor()
    .constructor<double, double, double>()
    .constructor<std::vector<double> >()
    .method("times",             &forest::models::gaussian::operator*)
    .property("valid",           &forest::models::gaussian::valid)
    // Might make these read/write?  Not sure.
    .field_readonly("mean",      &forest::models::gaussian::mean)
    .field_readonly("variance",  &forest::models::gaussian::variance)
    .field_readonly("log_scale", &forest::models::gaussian::log_scale)
    ;

  Rcpp::class_<forest::models::brownian_motion>("brownian_motion")
    .constructor()
    .property("parameters",
              &forest::models::brownian_motion::parameters,
              &forest::models::brownian_motion::set_parameters)
    .method("forward",
            &forest::models::brownian_motion::forward)
    .method("backward",
            &forest::models::brownian_motion::backward)
    .method("combine",
            &forest::models::brownian_motion::combine)
    ;

  Rcpp::function("test_convert", &test_convert);
}
