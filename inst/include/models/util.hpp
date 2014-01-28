#ifndef _FOREST_MODELS_UTIL_HPP_
#define _FOREST_MODELS_UTIL_HPP_

namespace forest {
namespace models {
// Branches have a beginning and an end; we want to have both
// beginning and end values for a branch (so the value of the
// calculation at the tip and at the root).
//
// An alternative way would be storing just the tip calculation until
// the the branch is computed at which point compute the root, but
// that would require storing the tip values somewhere else and
// re-initialising them every time.
//
// Note that this is basically a std::pair, but homogeneous in type
// and with first/second renamed tipward/rootward).
template <typename T>
struct branch_pair {
  typedef T value_type;
  branch_pair() : rootward(), tipward() {}
  branch_pair(const T& rootward_, const T& tipward_)
    : rootward(rootward_), tipward(tipward_) {}
  T rootward;
  T tipward;
};

// This assumes that the data are held in a branch_pair and that we
// have iterators to (say) the child nodes.  Possibly there are
// std::product algorithm things that do this better?
//
// Reference semantics could be better here (also something that I
// wondered about with gaussian).
//
// It may also be good to have special cases for 1 or 2 elements?  Can
// use std::distance to work out if that applies.
template <typename Model, typename ForwardIterator>
typename Model::data combine(const Model& model,
                             ForwardIterator first,
                             ForwardIterator last) {
  typename Model::data ret = (first++)->data_.rootward;
  while (first != last)
    ret = model.combine(ret, (first++)->data_.rootward);
  return ret;
}

// Run Felsenstein's post-order traversal algorithm on a model.  The
// model needs to provide a 'combine' method (to be used by
// models::combine) and a 'backward' method (propagating the
// likelihood down a branch from root to tip).
//
// The name here is taken from diversitree as it does still seem apt.
// But (like everything else) it may still change.
//
// This will work for the simplest cases only -- no time dependence,
// no state dependence.  Those can come later.
//
// NOTE: this will propagate the probability distrbution along the
// root edge too, even though we don't need that to happen; we could
// try and intercept that by remembering where the root and not
// calculating anything for it.  That could be as simple as
//
//   typename tree_type::sub_post_iterator root =
//         boost::prior(tree.end_sub_post());
//   if (sub != root)
//     node->data_.rootward =
//       model.backward(node->data_.tipward, node->length_);
//
// Alternatively, we can rely on the calculations being fast enough,
// especially for a zero root length.
template <typename Model>
typename Model::data
all_branches(treetree::tree<node<branch_pair<typename Model::data> > >& tree,
                  const Model& model) {
  typedef treetree::tree<node<branch_pair<typename Model::data> > > tree_type;

  typename tree_type::sub_post_iterator sub = tree.begin_sub_post();
  while (sub != tree.end_sub_post()) {
    typename tree_type::post_iterator node = sub;
    if (!sub->childless()) {
      node->data_.tipward =
        combine(model, sub->begin_child(), sub->end_child());
    }
    node->data_.rootward =
      model.backward(node->data_.tipward, node->length_);
    ++sub;
  }
  return tree.root().data_.tipward;
}

}
}

#endif
