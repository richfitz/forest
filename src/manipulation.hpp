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
template <typename T>
void collapse_singles(treetree::tree<T>& tr) {
  typedef typename treetree::tree<T>::sub_post_iterator sub_post_iterator;
  for (sub_post_iterator it = tr.begin_sub_post();
       it != tr.end_sub_post(); ++it) {
    if (it->arity() == 1) {
      sub_post_iterator singleton = it, descendant = boost::prior(it);
      descendant->begin()->length_ += singleton->begin()->length_;
      *singleton = *descendant;
    }
  }
}

}

#endif
