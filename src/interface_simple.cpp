#include "node.hpp"
#include "node_wrapper.hpp"

#include "tree_rcpp.hpp"

#include <Rcpp.h>

// These three are silly little testing functions: they take a node as
// an argument, return as a return value and do both.  They're purely
// designed to exercise the as/wrap magic.
template <typename T>
T node_data(forest::node<T> obj) {
  return obj.data_;
}
template <typename T>
forest::node<T> node_with_twice_length(double len) {
  return forest::node<T>("test_only", len * 2, static_cast<T>(len * 2));
}
template <typename T>
forest::node<T> node_with_twice_length2(double len) {
  return forest::node<T>("test_only", len * 2);
}
template <typename T>
forest::node<T> combine(const forest::node<T>& a,
                        const forest::node<T>& b) {
  return forest::node<T>(a.label_  + b.label_,
                         a.length_ + b.length_,
                         a.data_   + b.data_);
}

typedef forest::node_wrapped<Rcpp::RObject> rnode;

// This is identical to the xnode description.
#ifdef __clang__
#pragma clang diagnostic push
// These I have no control over because they're Rcpp issues.
#pragma clang diagnostic ignored "-Wglobal-constructors"
#pragma clang diagnostic ignored "-Wexit-time-destructors"
#pragma clang diagnostic ignored "-Wmissing-prototypes"
#endif
RCPP_MODULE(simple) {
#ifdef __clang__
#pragma clang diagnostic pop
#endif
  Rcpp::class_<rnode>("rnode")
    .constructor()
    .constructor<std::string>()
    .constructor<std::string, double>()
    .constructor<std::string, double, rnode::data_type>()

    .property("label",
              &rnode::get_label,
              &rnode::set_label)
    .property("length",
              &rnode::get_length,
              &rnode::set_length)
    .property("data",
              &rnode::get_data,
              &rnode::set_data)

    .property("has_label",    &rnode::has_label)
    .property("has_length",   &rnode::has_length)

    .property("height",       &rnode::height)
    .property("depth",        &rnode::depth)

    .method("copy",           &rnode::copy)
    .method("equals",         &rnode::equals)
    ;

  Rcpp::function("node_with_twice_length_int",
                 &node_with_twice_length<int>);
  Rcpp::function("node_with_twice_length_double",
                 &node_with_twice_length<double>);
  Rcpp::function("node_with_twice_length_general",
                 &node_with_twice_length2<Rcpp::RObject>);

  Rcpp::function("node_data_int",
                 &node_data<int>);
  Rcpp::function("node_data_double",
                 &node_data<double>);

  Rcpp::function("combine_int",
                 &combine<int>);
  Rcpp::function("combine_double",
                 &combine<double>);
}
