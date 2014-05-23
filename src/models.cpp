#include <forest.h>

// Testing only
typedef forest::node<forest::models::gaussian> gnode_test;

// Actually used
typedef forest::models::branch_pair<forest::models::gaussian> gpair;
typedef forest::node<gpair>   gnode;
typedef treetree::tree<gnode> gtree;

// For now:
typedef forest::models::calculator<forest::models::brownian_motion>
calculator_bm;

// Also requiring module export below.
RCPP_EXPOSED_CLASS_NODECL(forest::tree_wrapped<gnode>)

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
    .method("times",             &forest::models::gaussian::operator*)
    .property("valid",           &forest::models::gaussian::valid)
    // Might make these read/write?  Not sure.
    .field_readonly("mean",      &forest::models::gaussian::mean)
    .field_readonly("variance",  &forest::models::gaussian::variance)
    .field_readonly("log_scale", &forest::models::gaussian::log_scale)
    ;

  Rcpp::class_<forest::models::discrete>("discrete")
    .constructor()
    .constructor<std::vector<double>, double>()
    .method("times",             &forest::models::discrete::operator*)
    .property("valid",           &forest::models::discrete::valid)
    .property("size",            &forest::models::discrete::size)
    .method("resize",            &forest::models::discrete::resize)
    .method("[[",                &forest::models::discrete::r_at)
    .field_readonly("probabilities",
                    &forest::models::discrete::probabilities)
    .field_readonly("log_scale", &forest::models::discrete::log_scale)
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

  Rcpp::class_<forest::models::mk2>("mk2")
    .constructor()
    .property("parameters",
              &forest::models::mk2::parameters,
              &forest::models::mk2::set_parameters)
    .method("forward",
            &forest::models::mk2::forward)
    .method("backward",
            &forest::models::mk2::backward)
    .method("combine",
            &forest::models::mk2::combine)
    ;

  // Just one type for now, but eventually this will be generic.
  Rcpp::class_<calculator_bm>("calculator_bm")
    .constructor<forest::models::brownian_motion, gtree>()
    .method("all_branches", &calculator_bm::all_branches)
    .method("set_parameters", &calculator_bm::set_parameters)
    ;

  // Also requiring EXPOSED_CLASS above.
  Rcpp::class_<forest::tree_wrapped<gnode> >("gtree")
    .method("to_rtree", &forest::tree_wrapped<gnode>::to_rtree)
    ;

  Rcpp::function("build_gaussian_tree",
                 &forest::models::build_model_tree<forest::models::gaussian>);
  Rcpp::function("all_branches_bm",
                 &forest::models::all_branches<forest::models::brownian_motion>);

}
