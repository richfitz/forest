#include <Rcpp.h>

#include "node.h"

typedef forest::node<int> xnode;
RCPP_EXPOSED_CLASS_NODECL(xnode)

#ifdef __clang__
#pragma clang diagnostic push
// These I have no control over because they're Rcpp issues.
#pragma clang diagnostic ignored "-Wglobal-constructors"
#pragma clang diagnostic ignored "-Wexit-time-destructors"
#pragma clang diagnostic ignored "-Wmissing-prototypes"
#endif
RCPP_MODULE(forest) {
#ifdef __clang__
#pragma clang diagnostic pop
#endif
  Rcpp::class_<xnode>("xnode")
    .constructor<xnode::value_type>()
    .constructor<xnode::value_type, std::string>()
    .field("data",    &xnode::data_)
    .field("label",   &xnode::label_)
    .method("copy",   &xnode::copy)
    .method("equals", &xnode::operator==)
    ;
}
