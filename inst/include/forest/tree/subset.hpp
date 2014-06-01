#ifndef _FOREST_TREE_SUBSET_HPP_
#define _FOREST_TREE_SUBSET_HPP_

#include <forest/tree/node.hpp>
#include <forest/treetree.hpp>
#include <forest/util.hpp> // util::stop
#include <forest/tree/support.hpp> // locate_node_by_label

// Functions involved with returning partial bits of the tree.
//
// We have
//
// 0. root_subtree / set_root_subtree
//    -> get / set the root subtree of a tree / subtree
// 1. root_node / set_root_node
//    -> get / set the root node of a tree / subtree
// 2. child_subtree / set_child_subtree
//    -> get / set one of the child *subtrees*.
// 3. child_node / set_child_node
//    -> get / set one of the child *nodes*.
// 4. subtree / set_subtree
//    -> get / set a subtree by label
// 5. node / set_node
//    -> get / set a node by label

namespace forest {

// TODO: With these functions here, should the functions return
// reference and therefore allow assignment by doing root_node(tr) =
// value?  That would be nice.

// 0. root_subtree: get / set root subtree of a tree / subtree
template <typename T>
treetree::subtree<T> root_subtree(treetree::subtree<T>& tr) {
  return tr.root_sub(); // noop?
}
template <typename T>
treetree::subtree<T> root_subtree(treetree::tree<T>& tr) {
  return tr.root_sub(); // noop?
}

// NOTE: Here, and elsewhere, passing the tree by value always creates
// a copy.
template <typename T>
void set_root_subtree(treetree::subtree<T>& tr,
                      treetree::tree<T> value) {
  tr.root_sub() = value;
}
template <typename T>
void set_root_subtree(treetree::tree<T>& tr,
                      treetree::tree<T> value) {
  tr.root_sub() = value;
}

// 1. root_node: get / set root node of a tree / subtree
template <typename T>
T& root_node(treetree::subtree<T>& tr) {
  if (tr.size() == 0) {
    util::stop("Cannot extract root node of empty tree");
  }
  return tr.root();
}

template <typename T>
T& root_node(treetree::tree<T>& tr) {
  treetree::subtree<T> sub(tr);
  return root_node(sub);
}

// TODO: Not 100% certain this is the correct treetree way of doing
// this.  Might be simply
//   root_node(tr) = value
//
// TODO: Assignment of a root node to an empty tree should create the
// node.  That should be fairly easy -- probably
//   tr.insert(tr.end(), value);
template <typename T>
void set_root_node(treetree::tree<T>& tr, const T& value) {
  *tr.begin() = value;
}
template <typename T>
void set_root_node(treetree::subtree<T>& tr, const T& value) {
  *tr.begin() = value;
}

// 2. child_subtree: get / set one of the child *subtrees*.
//
// NOTE: No range checing here - assume that the index is valid.
// Checking will be done on the R side and that has nicer error
// messages (and will report the correct base of the index).
//
// NOTE: All the int idx will become base1_index objects.
template <typename T>
treetree::subtree<T> child_subtree(treetree::subtree<T>& tr,
                                   util::index idx) {
  return tr[idx.as_size(tr.arity())];
}
template <typename T>
treetree::subtree<T> child_subtree(treetree::tree<T>& tr,
                                   util::index idx) {
  treetree::subtree<T> subtr(tr);
  return child_subtree<T>(subtr, idx);
  // Not sure if the above actually works properly (references!) but
  // it might.  If not, use this code:
  // return tr[idx];
  // If it *does* work right, then we might not need references at
  // all...
}

template <typename T>
void set_child_subtree(treetree::subtree<T>& tr, util::index idx,
                       treetree::tree<T> value) {
  tr[idx.as_size(tr.arity())] = value;
}
template <typename T>
void set_child_subtree(treetree::tree<T>& tr, util::index idx,
                       treetree::tree<T> value) {
  tr[idx.as_size(tr.arity())] = value;
}

// 3. child_node: get / set one of the child *nodes*.
template <typename T>
T child_node(treetree::subtree<T>& tr, util::index idx) {
  return child_subtree(tr, idx).root();
}
template <typename T>
T child_node(treetree::tree<T>& tr, util::index idx) {
  return child_subtree(tr, idx).root();
}

template <typename T>
void set_child_node(treetree::subtree<T>& tr, util::index idx,
                    const T& value) {
  tr[idx.as_size(tr.arity())].root() = value;
}
template <typename T>
void set_child_node(treetree::tree<T>& tr, util::index idx,
                    const T& value) {
  tr[idx.as_size(tr.arity())].root() = value;
}

// 4. subtree: get / set a subtree by label
template <typename T>
treetree::subtree<T> subtree(treetree::subtree<T>& tr,
                             const std::string& label) {
  return *locate_node_by_label<T>(tr.begin_sub(), tr.end_sub(),
                                  label);
}
template <typename T>
treetree::subtree<T> subtree(treetree::tree<T>& tr,
                             const std::string& label) {
  // Or:
  //   return subtree_at_label(treetree::subtree<T>(tr), label);
  return *locate_node_by_label<T>(tr.begin_sub(), tr.end_sub(),
                                  label);
}

// template <typename T>
// void set_subtree(treetree::subtree<T>& tr,
//                  const std::string& label,
//                  treetree::tree<T>& value) {
//   locate_node_by_label<T>(tr.begin_sub(), tr.end_sub(), label)
// }

// 5. node: get / set a node by label
//
// These are not yet implemented, but will be fairly simple.  They can
// actually be done in R code on top of the above functions.

}

#endif
