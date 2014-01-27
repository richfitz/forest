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
  typedef node<Rcpp::RObject> node_target;
  typedef node<T>             node_source;
  treetree::tree<node_target> ret = duplicate_topology<node_target>(tr);
  typename treetree::tree<node_target>::pre_iterator it_to = ret.begin();
  typename treetree::tree<node_source>::const_pre_iterator it = tr.begin();
  while (it != tr.end()) {
    (it_to++)->data_ = Rcpp::wrap((it++)->data_);
  }
  return Rcpp::wrap(ret);
}

}

#endif
