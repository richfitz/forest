#ifndef _FOREST_MANIPULATION_HPP_
#define _FOREST_MANIPULATION_HPP_

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
//
// NOTE: I am not 100% sure that the assignment here causes a polite
// cleanup: erase seems more what I'd want to do, but it cleans up too
// much.  This should do the whole subtree assignment thing so I guess
// the Right Thing is being done.
template <typename T>
void collapse_node(typename treetree::tree<T>::sub_post_iterator singleton) {
  typedef typename treetree::tree<T>::sub_post_iterator sub_post_iterator;
  sub_post_iterator descendant = boost::prior(singleton);
  descendant->begin()->length_ += singleton->begin()->length_;
  *singleton = *descendant;
}

template <typename T>
void collapse_singles(treetree::tree<T>& tr) {
  typedef typename treetree::tree<T>::sub_post_iterator sub_post_iterator;
  for (sub_post_iterator it = tr.begin_sub_post();
       it != tr.end_sub_post(); ++it)
    if (it->arity() == 1)
      collapse_node<T>(it);
}

}

#endif
