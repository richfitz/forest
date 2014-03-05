#ifndef _FOREST_TREE_MISC_POST_RCPP_HPP_
#define _FOREST_TREE_MISC_POST_RCPP_HPP_

namespace forest {
// NOTE: I'm not sure why node needs to be fully qualified in the
// typename statement below (and not in the argument list) but it
// apparently does.
template <typename T>
void associate_data(treetree::tree<node<T> >& tr, Rcpp::List data,
                    bool tip, bool node) {
  const std::vector<std::string> names = data.names();
  // NOTE: Add a more informative error here.  See Rcpp sugar's setdiff.
  if (!check_names(tr, names, tip, node))
    stop("Not all tips/nodes are represented in 'data'");
  for (typename treetree::tree<forest::node<T> >::sub_pre_iterator
         it = tr.begin(); it != tr.end(); ++it)
    if ((tip  && it->childless()) || (node && !it->childless()))
      it->begin()->data_ =
        Rcpp::as<T>(data[node_label(it->root())]);
    else
      it->begin()->data_ = Rcpp::as<T>(R_NilValue);
}

template <typename T>
void associate_data(treetree::tree<node<T> >& tr, SEXP data,
                    bool tip, bool node) {
  associate_data(tr, Rcpp::as<Rcpp::List>(data), tip, node);
}

template <typename T>
SEXP to_rtree(const treetree::const_subtree<node<T> >& tr) {
  return Rcpp::wrap(copy_convert<node<Rcpp::RObject> >(tr));
}

// Tree classification.  See misc.hpp for the actual meat here; this
// just wraps it up so that we end up with a named integer vector,
// rather than a bunch of numbers in pre-order traversal order.
template <typename T>
Rcpp::IntegerVector r_classify(const treetree::tree<T>& tr,
                               const std::vector<std::string>& labels) {
  treetree::tree<forest::node<int> > tmp = classify(tr, labels);
  Rcpp::IntegerVector ret;
  std::vector<std::string> names;
  names.reserve(tr.size());
  for (treetree::tree<forest::node<int> >::const_pre_iterator
         it = tmp.begin(); it != tmp.end(); ++it) {
    ret.push_back(it->data_);
    names.push_back(it->label_);
  }
  ret.attr("names") = names;
  return ret;
}

}

#endif
