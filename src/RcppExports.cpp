// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/forest.h"
#include <Rcpp.h>

using namespace Rcpp;

// to_newick_string
std::string to_newick_string(treetree::tree<forest::forest_node> tr, int digits);
RcppExport SEXP forest_to_newick_string(SEXP trSEXP, SEXP digitsSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< treetree::tree<forest::forest_node> >::type tr(trSEXP );
        Rcpp::traits::input_parameter< int >::type digits(digitsSEXP );
        std::string __result = to_newick_string(tr, digits);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// from_newick_node
forest::forest_node from_newick_node(std::string x);
RcppExport SEXP forest_from_newick_node(SEXP xSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< std::string >::type x(xSEXP );
        forest::forest_node __result = from_newick_node(x);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// from_newick_string
treetree::tree<forest::forest_node> from_newick_string(const std::vector<std::string>& tokens_str);
RcppExport SEXP forest_from_newick_string(SEXP tokens_strSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const std::vector<std::string>& >::type tokens_str(tokens_strSEXP );
        treetree::tree<forest::forest_node> __result = from_newick_string(tokens_str);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// from_ape_internal
treetree::tree<forest::forest_node> from_ape_internal(const std::vector<size_t>& order, const std::vector<size_t>& from, const std::vector<size_t>& to, const std::vector<std::string>& label, const std::vector<double>& length);
RcppExport SEXP forest_from_ape_internal(SEXP orderSEXP, SEXP fromSEXP, SEXP toSEXP, SEXP labelSEXP, SEXP lengthSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const std::vector<size_t>& >::type order(orderSEXP );
        Rcpp::traits::input_parameter< const std::vector<size_t>& >::type from(fromSEXP );
        Rcpp::traits::input_parameter< const std::vector<size_t>& >::type to(toSEXP );
        Rcpp::traits::input_parameter< const std::vector<std::string>& >::type label(labelSEXP );
        Rcpp::traits::input_parameter< const std::vector<double>& >::type length(lengthSEXP );
        treetree::tree<forest::forest_node> __result = from_ape_internal(order, from, to, label, length);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// to_ape_internal
Rcpp::List to_ape_internal(const treetree::tree<forest::forest_node>& tr);
RcppExport SEXP forest_to_ape_internal(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const treetree::tree<forest::forest_node>& >::type tr(trSEXP );
        Rcpp::List __result = to_ape_internal(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// drain_tree
Rcpp::List drain_tree(const treetree::tree<forest::forest_node>& tr);
RcppExport SEXP forest_drain_tree(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const treetree::tree<forest::forest_node>& >::type tr(trSEXP );
        Rcpp::List __result = drain_tree(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// classify
Rcpp::IntegerVector classify(const treetree::tree<forest::forest_node>& tr, const std::vector<std::string>& labels);
RcppExport SEXP forest_classify(SEXP trSEXP, SEXP labelsSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const treetree::tree<forest::forest_node>& >::type tr(trSEXP );
        Rcpp::traits::input_parameter< const std::vector<std::string>& >::type labels(labelsSEXP );
        Rcpp::IntegerVector __result = classify(tr, labels);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// plotting_coordinates
treetree::tree<forest::node<forest::plotting::plot_info> > plotting_coordinates(const treetree::tree<forest::node<Rcpp::RObject> >& tree);
RcppExport SEXP forest_plotting_coordinates(SEXP treeSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const treetree::tree<forest::node<Rcpp::RObject> >& >::type tree(treeSEXP );
        treetree::tree<forest::node<forest::plotting::plot_info> > __result = plotting_coordinates(tree);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// plotting_coordinates_clade
treetree::tree<forest::node<forest::plotting::plot_info> > plotting_coordinates_clade(const treetree::tree<forest::node<Rcpp::RObject> >& tree, const std::vector<double>& n_taxa, double p);
RcppExport SEXP forest_plotting_coordinates_clade(SEXP treeSEXP, SEXP n_taxaSEXP, SEXP pSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const treetree::tree<forest::node<Rcpp::RObject> >& >::type tree(treeSEXP );
        Rcpp::traits::input_parameter< const std::vector<double>& >::type n_taxa(n_taxaSEXP );
        Rcpp::traits::input_parameter< double >::type p(pSEXP );
        treetree::tree<forest::node<forest::plotting::plot_info> > __result = plotting_coordinates_clade(tree, n_taxa, p);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_node__ctor
forest::forest_node forest_node__ctor(std::string label, double length, Rcpp::RObject data);
RcppExport SEXP forest_forest_node__ctor(SEXP labelSEXP, SEXP lengthSEXP, SEXP dataSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< std::string >::type label(labelSEXP );
        Rcpp::traits::input_parameter< double >::type length(lengthSEXP );
        Rcpp::traits::input_parameter< Rcpp::RObject >::type data(dataSEXP );
        forest::forest_node __result = forest_node__ctor(label, length, data);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__root_subtree
forest::forest_subtree forest_tree__root_subtree(Rcpp::RObject x);
RcppExport SEXP forest_forest_tree__root_subtree(SEXP xSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        forest::forest_subtree __result = forest_tree__root_subtree(x);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_subtree__root_subtree
forest::forest_subtree forest_subtree__root_subtree(Rcpp::RObject x);
RcppExport SEXP forest_forest_subtree__root_subtree(SEXP xSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        forest::forest_subtree __result = forest_subtree__root_subtree(x);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__set_root_subtree
void forest_tree__set_root_subtree(Rcpp::RObject x, forest::forest_tree value);
RcppExport SEXP forest_forest_tree__set_root_subtree(SEXP xSEXP, SEXP valueSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        Rcpp::traits::input_parameter< forest::forest_tree >::type value(valueSEXP );
        forest_tree__set_root_subtree(x, value);
    }
    return R_NilValue;
END_RCPP
}
// forest_subtree__set_root_subtree
void forest_subtree__set_root_subtree(Rcpp::RObject x, forest::forest_tree value);
RcppExport SEXP forest_forest_subtree__set_root_subtree(SEXP xSEXP, SEXP valueSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        Rcpp::traits::input_parameter< forest::forest_tree >::type value(valueSEXP );
        forest_subtree__set_root_subtree(x, value);
    }
    return R_NilValue;
END_RCPP
}
// forest_tree__root_node
forest::forest_node forest_tree__root_node(Rcpp::RObject x);
RcppExport SEXP forest_forest_tree__root_node(SEXP xSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        forest::forest_node __result = forest_tree__root_node(x);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_subtree__root_node
forest::forest_node forest_subtree__root_node(Rcpp::RObject x);
RcppExport SEXP forest_forest_subtree__root_node(SEXP xSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        forest::forest_node __result = forest_subtree__root_node(x);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__set_root_node
void forest_tree__set_root_node(Rcpp::RObject x, forest::forest_node value);
RcppExport SEXP forest_forest_tree__set_root_node(SEXP xSEXP, SEXP valueSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        Rcpp::traits::input_parameter< forest::forest_node >::type value(valueSEXP );
        forest_tree__set_root_node(x, value);
    }
    return R_NilValue;
END_RCPP
}
// forest_subtree__set_root_node
void forest_subtree__set_root_node(Rcpp::RObject x, forest::forest_node value);
RcppExport SEXP forest_forest_subtree__set_root_node(SEXP xSEXP, SEXP valueSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        Rcpp::traits::input_parameter< forest::forest_node >::type value(valueSEXP );
        forest_subtree__set_root_node(x, value);
    }
    return R_NilValue;
END_RCPP
}
// forest_tree__child_subtree
forest::forest_subtree forest_tree__child_subtree(Rcpp::RObject x, forest::util::index idx);
RcppExport SEXP forest_forest_tree__child_subtree(SEXP xSEXP, SEXP idxSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        Rcpp::traits::input_parameter< forest::util::index >::type idx(idxSEXP );
        forest::forest_subtree __result = forest_tree__child_subtree(x, idx);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_subtree__child_subtree
forest::forest_subtree forest_subtree__child_subtree(Rcpp::RObject x, forest::util::index idx);
RcppExport SEXP forest_forest_subtree__child_subtree(SEXP xSEXP, SEXP idxSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        Rcpp::traits::input_parameter< forest::util::index >::type idx(idxSEXP );
        forest::forest_subtree __result = forest_subtree__child_subtree(x, idx);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__set_child_subtree
void forest_tree__set_child_subtree(Rcpp::RObject x, forest::util::index idx, forest::forest_tree value);
RcppExport SEXP forest_forest_tree__set_child_subtree(SEXP xSEXP, SEXP idxSEXP, SEXP valueSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        Rcpp::traits::input_parameter< forest::util::index >::type idx(idxSEXP );
        Rcpp::traits::input_parameter< forest::forest_tree >::type value(valueSEXP );
        forest_tree__set_child_subtree(x, idx, value);
    }
    return R_NilValue;
END_RCPP
}
// forest_subtree__set_child_subtree
void forest_subtree__set_child_subtree(Rcpp::RObject x, forest::util::index idx, forest::forest_tree value);
RcppExport SEXP forest_forest_subtree__set_child_subtree(SEXP xSEXP, SEXP idxSEXP, SEXP valueSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        Rcpp::traits::input_parameter< forest::util::index >::type idx(idxSEXP );
        Rcpp::traits::input_parameter< forest::forest_tree >::type value(valueSEXP );
        forest_subtree__set_child_subtree(x, idx, value);
    }
    return R_NilValue;
END_RCPP
}
// forest_tree__child_node
forest::forest_node forest_tree__child_node(Rcpp::RObject x, forest::util::index idx);
RcppExport SEXP forest_forest_tree__child_node(SEXP xSEXP, SEXP idxSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        Rcpp::traits::input_parameter< forest::util::index >::type idx(idxSEXP );
        forest::forest_node __result = forest_tree__child_node(x, idx);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_subtree__child_node
forest::forest_node forest_subtree__child_node(Rcpp::RObject x, forest::util::index idx);
RcppExport SEXP forest_forest_subtree__child_node(SEXP xSEXP, SEXP idxSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        Rcpp::traits::input_parameter< forest::util::index >::type idx(idxSEXP );
        forest::forest_node __result = forest_subtree__child_node(x, idx);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__set_child_node
void forest_tree__set_child_node(Rcpp::RObject x, forest::util::index idx, forest::forest_node value);
RcppExport SEXP forest_forest_tree__set_child_node(SEXP xSEXP, SEXP idxSEXP, SEXP valueSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        Rcpp::traits::input_parameter< forest::util::index >::type idx(idxSEXP );
        Rcpp::traits::input_parameter< forest::forest_node >::type value(valueSEXP );
        forest_tree__set_child_node(x, idx, value);
    }
    return R_NilValue;
END_RCPP
}
// forest_subtree__set_child_node
void forest_subtree__set_child_node(Rcpp::RObject x, forest::util::index idx, forest::forest_node value);
RcppExport SEXP forest_forest_subtree__set_child_node(SEXP xSEXP, SEXP idxSEXP, SEXP valueSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        Rcpp::traits::input_parameter< forest::util::index >::type idx(idxSEXP );
        Rcpp::traits::input_parameter< forest::forest_node >::type value(valueSEXP );
        forest_subtree__set_child_node(x, idx, value);
    }
    return R_NilValue;
END_RCPP
}
// forest_tree__subtree
forest::forest_subtree forest_tree__subtree(Rcpp::RObject x, std::string label);
RcppExport SEXP forest_forest_tree__subtree(SEXP xSEXP, SEXP labelSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        Rcpp::traits::input_parameter< std::string >::type label(labelSEXP );
        forest::forest_subtree __result = forest_tree__subtree(x, label);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_subtree__subtree
forest::forest_subtree forest_subtree__subtree(Rcpp::RObject x, std::string label);
RcppExport SEXP forest_forest_subtree__subtree(SEXP xSEXP, SEXP labelSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        Rcpp::traits::input_parameter< std::string >::type label(labelSEXP );
        forest::forest_subtree __result = forest_subtree__subtree(x, label);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__ctor_empty
forest::forest_tree forest_tree__ctor_empty();
RcppExport SEXP forest_forest_tree__ctor_empty() {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        forest::forest_tree __result = forest_tree__ctor_empty();
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__ctor_node
forest::forest_tree forest_tree__ctor_node(forest::forest_node nd);
RcppExport SEXP forest_forest_tree__ctor_node(SEXP ndSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< forest::forest_node >::type nd(ndSEXP );
        forest::forest_tree __result = forest_tree__ctor_node(nd);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__empty
bool forest_tree__empty(const forest::forest_tree& tr);
RcppExport SEXP forest_forest_tree__empty(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_tree& >::type tr(trSEXP );
        bool __result = forest_tree__empty(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__size
size_t forest_tree__size(const forest::forest_tree& tr);
RcppExport SEXP forest_forest_tree__size(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_tree& >::type tr(trSEXP );
        size_t __result = forest_tree__size(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__arity
size_t forest_tree__arity(const forest::forest_tree& tr);
RcppExport SEXP forest_forest_tree__arity(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_tree& >::type tr(trSEXP );
        size_t __result = forest_tree__arity(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__childless
bool forest_tree__childless(const forest::forest_tree& tr);
RcppExport SEXP forest_forest_tree__childless(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_tree& >::type tr(trSEXP );
        bool __result = forest_tree__childless(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__representation
std::string forest_tree__representation(const forest::forest_tree& tr);
RcppExport SEXP forest_forest_tree__representation(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_tree& >::type tr(trSEXP );
        std::string __result = forest_tree__representation(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__clear
void forest_tree__clear(Rcpp::RObject x);
RcppExport SEXP forest_forest_tree__clear(SEXP xSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        forest_tree__clear(x);
    }
    return R_NilValue;
END_RCPP
}
// forest_tree__count_tips
size_t forest_tree__count_tips(const forest::forest_tree& tr);
RcppExport SEXP forest_forest_tree__count_tips(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_tree& >::type tr(trSEXP );
        size_t __result = forest_tree__count_tips(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__count_nodes
size_t forest_tree__count_nodes(const forest::forest_tree& tr);
RcppExport SEXP forest_forest_tree__count_nodes(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_tree& >::type tr(trSEXP );
        size_t __result = forest_tree__count_nodes(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__tip_labels
std::vector<std::string> forest_tree__tip_labels(const forest::forest_tree& tr);
RcppExport SEXP forest_forest_tree__tip_labels(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_tree& >::type tr(trSEXP );
        std::vector<std::string> __result = forest_tree__tip_labels(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__node_labels
std::vector<std::string> forest_tree__node_labels(const forest::forest_tree& tr);
RcppExport SEXP forest_forest_tree__node_labels(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_tree& >::type tr(trSEXP );
        std::vector<std::string> __result = forest_tree__node_labels(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__heights
std::vector<double> forest_tree__heights(const forest::forest_tree& tr);
RcppExport SEXP forest_forest_tree__heights(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_tree& >::type tr(trSEXP );
        std::vector<double> __result = forest_tree__heights(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__depths
std::vector<double> forest_tree__depths(const forest::forest_tree& tr);
RcppExport SEXP forest_forest_tree__depths(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_tree& >::type tr(trSEXP );
        std::vector<double> __result = forest_tree__depths(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__is_binary
bool forest_tree__is_binary(const forest::forest_tree& tr);
RcppExport SEXP forest_forest_tree__is_binary(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_tree& >::type tr(trSEXP );
        bool __result = forest_tree__is_binary(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__has_branch_lengths
bool forest_tree__has_branch_lengths(const forest::forest_tree& tr);
RcppExport SEXP forest_forest_tree__has_branch_lengths(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_tree& >::type tr(trSEXP );
        bool __result = forest_tree__has_branch_lengths(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__is_ultrametric
bool forest_tree__is_ultrametric(const forest::forest_tree& tr, double eps);
RcppExport SEXP forest_forest_tree__is_ultrametric(SEXP trSEXP, SEXP epsSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_tree& >::type tr(trSEXP );
        Rcpp::traits::input_parameter< double >::type eps(epsSEXP );
        bool __result = forest_tree__is_ultrametric(tr, eps);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__update_heights
void forest_tree__update_heights(Rcpp::RObject x);
RcppExport SEXP forest_forest_tree__update_heights(SEXP xSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        forest_tree__update_heights(x);
    }
    return R_NilValue;
END_RCPP
}
// forest_tree__collapse_singles
void forest_tree__collapse_singles(Rcpp::RObject x);
RcppExport SEXP forest_forest_tree__collapse_singles(SEXP xSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        forest_tree__collapse_singles(x);
    }
    return R_NilValue;
END_RCPP
}
// forest_tree__ladderise
void forest_tree__ladderise(Rcpp::RObject x, bool right);
RcppExport SEXP forest_forest_tree__ladderise(SEXP xSEXP, SEXP rightSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        Rcpp::traits::input_parameter< bool >::type right(rightSEXP );
        forest_tree__ladderise(x, right);
    }
    return R_NilValue;
END_RCPP
}
// forest_tree__drop_tips_by_label
void forest_tree__drop_tips_by_label(Rcpp::RObject x, const std::vector<std::string>& labels);
RcppExport SEXP forest_forest_tree__drop_tips_by_label(SEXP xSEXP, SEXP labelsSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        Rcpp::traits::input_parameter< const std::vector<std::string>& >::type labels(labelsSEXP );
        forest_tree__drop_tips_by_label(x, labels);
    }
    return R_NilValue;
END_RCPP
}
// forest_tree__rotate
void forest_tree__rotate(Rcpp::RObject x, std::string label);
RcppExport SEXP forest_forest_tree__rotate(SEXP xSEXP, SEXP labelSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        Rcpp::traits::input_parameter< std::string >::type label(labelSEXP );
        forest_tree__rotate(x, label);
    }
    return R_NilValue;
END_RCPP
}
// forest_tree__check_names
bool forest_tree__check_names(const forest::forest_tree& tr, const std::vector<std::string>& labels, bool tip, bool node);
RcppExport SEXP forest_forest_tree__check_names(SEXP trSEXP, SEXP labelsSEXP, SEXP tipSEXP, SEXP nodeSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_tree& >::type tr(trSEXP );
        Rcpp::traits::input_parameter< const std::vector<std::string>& >::type labels(labelsSEXP );
        Rcpp::traits::input_parameter< bool >::type tip(tipSEXP );
        Rcpp::traits::input_parameter< bool >::type node(nodeSEXP );
        bool __result = forest_tree__check_names(tr, labels, tip, node);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__associate_data
void forest_tree__associate_data(Rcpp::RObject x, SEXP data, bool tip, bool node);
RcppExport SEXP forest_forest_tree__associate_data(SEXP xSEXP, SEXP dataSEXP, SEXP tipSEXP, SEXP nodeSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::RObject >::type x(xSEXP );
        Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP );
        Rcpp::traits::input_parameter< bool >::type tip(tipSEXP );
        Rcpp::traits::input_parameter< bool >::type node(nodeSEXP );
        forest_tree__associate_data(x, data, tip, node);
    }
    return R_NilValue;
END_RCPP
}
// forest_tree__copy_structure
forest::forest_tree forest_tree__copy_structure(const forest::forest_tree& tr);
RcppExport SEXP forest_forest_tree__copy_structure(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_tree& >::type tr(trSEXP );
        forest::forest_tree __result = forest_tree__copy_structure(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_tree__copy
forest::forest_tree forest_tree__copy(const forest::forest_tree& tr);
RcppExport SEXP forest_forest_tree__copy(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_tree& >::type tr(trSEXP );
        forest::forest_tree __result = forest_tree__copy(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_subtree__empty
bool forest_subtree__empty(const forest::forest_subtree& tr);
RcppExport SEXP forest_forest_subtree__empty(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_subtree& >::type tr(trSEXP );
        bool __result = forest_subtree__empty(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_subtree__size
size_t forest_subtree__size(const forest::forest_subtree& tr);
RcppExport SEXP forest_forest_subtree__size(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_subtree& >::type tr(trSEXP );
        size_t __result = forest_subtree__size(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_subtree__arity
size_t forest_subtree__arity(const forest::forest_subtree& tr);
RcppExport SEXP forest_forest_subtree__arity(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_subtree& >::type tr(trSEXP );
        size_t __result = forest_subtree__arity(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_subtree__childless
bool forest_subtree__childless(const forest::forest_subtree& tr);
RcppExport SEXP forest_forest_subtree__childless(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_subtree& >::type tr(trSEXP );
        bool __result = forest_subtree__childless(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_subtree__representation
std::string forest_subtree__representation(const forest::forest_subtree& tr);
RcppExport SEXP forest_forest_subtree__representation(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_subtree& >::type tr(trSEXP );
        std::string __result = forest_subtree__representation(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_subtree__count_tips
size_t forest_subtree__count_tips(const forest::forest_subtree& tr);
RcppExport SEXP forest_forest_subtree__count_tips(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_subtree& >::type tr(trSEXP );
        size_t __result = forest_subtree__count_tips(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_subtree__count_nodes
size_t forest_subtree__count_nodes(const forest::forest_subtree& tr);
RcppExport SEXP forest_forest_subtree__count_nodes(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_subtree& >::type tr(trSEXP );
        size_t __result = forest_subtree__count_nodes(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_subtree__tip_labels
std::vector<std::string> forest_subtree__tip_labels(const forest::forest_subtree& tr);
RcppExport SEXP forest_forest_subtree__tip_labels(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_subtree& >::type tr(trSEXP );
        std::vector<std::string> __result = forest_subtree__tip_labels(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_subtree__node_labels
std::vector<std::string> forest_subtree__node_labels(const forest::forest_subtree& tr);
RcppExport SEXP forest_forest_subtree__node_labels(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_subtree& >::type tr(trSEXP );
        std::vector<std::string> __result = forest_subtree__node_labels(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// forest_subtree__subtree_to_tree
forest::forest_tree forest_subtree__subtree_to_tree(const forest::forest_subtree& tr);
RcppExport SEXP forest_forest_subtree__subtree_to_tree(SEXP trSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const forest::forest_subtree& >::type tr(trSEXP );
        forest::forest_tree __result = forest_subtree__subtree_to_tree(tr);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
