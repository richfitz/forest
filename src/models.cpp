#include "models.hpp"

// Testing only
typedef forest::node<forest::models::gaussian> gnode;

// Actually used
typedef forest::models::branch_pair<forest::models::gaussian> gpair;
typedef forest::node<gpair>                    gpnode;
typedef treetree::tree<gpnode>                 gptree;

// For now:
typedef forest::models::calculator<forest::models::brownian_motion>
calculator_bm;

// Also requiring module export below.
RCPP_EXPOSED_CLASS_NODECL(forest::tree_wrapped<gpnode>)

gnode test_convert_node(forest::rnode::node_type nd);
gnode test_convert_node(forest::rnode::node_type nd) {
  return forest::duplicate_node<forest::models::gaussian>(nd);
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

  // Just one type for now, but eventually this will be generic.
  Rcpp::class_<calculator_bm>("calculator_bm")
    .constructor<forest::models::brownian_motion, gptree>()
    .method("all_branches", &calculator_bm::all_branches)
    .method("set_parameters", &calculator_bm::set_parameters)
    ;

  // Also requiring EXPOSED_CLASS above.
  Rcpp::class_<forest::tree_wrapped<gpnode> >("gptree")
    .method("to_rtree", &forest::tree_wrapped<gpnode>::to_rtree)
    ;

  Rcpp::function("test_convert_node",  &test_convert_node);
  Rcpp::function("build_gaussian_tree",
                 &forest::models::build_gaussian_tree<forest::rnode::node_type>);
  Rcpp::function("all_branches_bm",
                 &forest::models::all_branches<forest::models::brownian_motion>);

}
