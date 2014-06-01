#ifndef _FOREST_MANIPULATION_HPP_
#define _FOREST_MANIPULATION_HPP_

#include <forest/util.hpp>

namespace forest {

// Collapse singleton nodes; nodes that have just one descendant
// subtree (i.e., nodes with arity one).
//
// With the root to the left, we have this situation:
//
//             /                         /
//   root ----+-----------+-------------+------ tips
//            ^           ^             ^
//         parent     singleton     descendant
//
// In the case when there are no branch lengths, we simply remove
// 'singleton' from the tree.  When there are branch lengths, we take
// the branch length of 'singleton' (i.e., between 'parent' and
// 'singleton')  and add it onto the length for 'descendant'.  This
// gives the tree:
//
//             /                         /
//   root ----+-------------------------+------ tips
//            ^                         ^
//         parent                   descendant
//
// For now, we also simply drop 'singleton's label/data; we retain
// things only from descendant.
//
// This might want to be more flexible; perhaps taking the label from
// singleton rather than from the descendant, especially if it is
// blank for the descendant.  More generally we might allow a function
// to be passed in that would allow merging of the data elements.  For
// now though these are modifications for the future.  I believe
// (though have not checked) that this this is the same as what ape
// does.
//
// TODO: This would be nice if it worked on subtrees too (just need to
// class it appropriately, etc).  Then we could apply different rules
// to different parts of the tree.
//
// NOTE: collapse_node does not check that a node is actually
// collapsable -- it just does it.
template <typename T>
void collapse_node(treetree::tree<T>& tr,
                   typename treetree::tree<T>::sub_post_iterator singleton) {
  typedef typename treetree::tree<T>::sub_post_iterator sub_post_iterator;
  sub_post_iterator descendant = boost::prior(singleton);
  descendant->begin()->length_ += singleton->begin()->length_;
  singleton = tr.flatten(singleton);
  tr.erase(singleton);
}

// Seek out and remove all singletons in a tree:
template <typename T>
void collapse_singles(treetree::tree<T>& tr) {
  typedef typename treetree::tree<T>::sub_post_iterator sub_post_iterator;
  for (sub_post_iterator it = tr.begin_sub_post();
       it != tr.end_sub_post(); ++it)
    if (it->arity() == 1)
      collapse_node(tr, it);
}

// Drop a tip -- if this results in a singleton node, collapse it.
//
// NOTE: Though untested, this should actually work perfectly well for
// nodes too; it will drop the node, all descendants, and resolve if
// any singles should be resolved.  However, if it is used this way we
// should also change the name.  `drop_node` is probably more generic.
template <typename T>
void drop_tip(treetree::tree<T>& tr,
              typename treetree::tree<T>::sub_post_iterator it) {
  typename treetree::tree<T>::sub_post_iterator parent = treetree::parent(it);
  it = tr.erase(it);
  if (parent->arity() == 1)
    collapse_node(tr, parent);
}

// TODO: This will chance once I get a better way of addressing nodes
// from the R side.
//
// TODO: Should be able to drop nodes this way, too.
template <typename T>
void drop_tip_by_label(treetree::tree<T>& tr, const std::string& label) {
  typename treetree::tree<T>::sub_pre_iterator it =
    locate_tip_by_label<T>(tr.begin_sub(), tr.end_sub(), label);
  drop_tip(tr, it);
}

template <typename T>
void drop_tips_by_label(treetree::tree<T>& tr,
                        const std::vector<std::string>& labels) {
  std::vector<std::string>::const_iterator it = labels.begin();
  while (it != labels.end()) {
    drop_tip_by_label(tr, *it);
    ++it;
  }
}

// Rotate:
//
// Reorder the order of child subtrees.  There are two approaches that
// will give identical results on a bifurcating node that would be
// plausible with a multifurcation, std::rotate and std::reverse.
//
// A more general approach here would be to use a predicate on the
// nodes or something; that then generalise nicely (sort by species
// richness, some data element, etc).
//
// NOTE: the arity 1 case is unclear; could throw here instead?
template <typename T>
void rotate(typename treetree::tree<T>::sub_pre_iterator node) {
  if (node->arity() == 1)
    return;
  typename treetree::tree<T>::sub_child_iterator
    new_first = node->begin_sub_child();
  ++new_first;
  util::rotate(node->begin_sub_child(), new_first, node->end_sub_child());
}

template <typename T>
void rotate(treetree::tree<T>& tr, const std::string& label) {
  rotate<T>(locate_internal_by_label<T>(tr.begin_sub(), tr.end_sub(), label));
}

template <typename T>
void rotate(treetree::subtree<T>& tr, const std::string& label) {
  rotate<T>(locate_internal_by_label<T>(tr.begin_sub(), tr.end_sub(), label));
}

template <typename T>
void ladderise(treetree::tree<T>& tr, bool right) {
  for (typename treetree::tree<T>::sub_pre_iterator
         it = tr.begin_sub(); it != tr.end_sub(); ++it) {
    if (!it->childless() && it->arity() > 1) {
      if (it->arity() > 2) {
        util::stop("Can't (yet) ladderize a polytomy");
      }
      const size_t nl = (*it)[0].size(), nr = (*it)[1].size();
      if ((right && nr > nl) || (!right && nl > nr))
        rotate<T>(it);
    }
  }
}


}

#endif
