// -*-c++-*-
#ifndef _FOREST_ITERATOR_WRAPPER_ALGORITHM_H_
#define _FOREST_ITERATOR_WRAPPER_ALGORITHM_H_

#include "iterator_wrapper.h"

// Wrappers to get things to work with STL algorithms.  There are
// rather a lot of these, and I don't know how much this will cause
// code bloat or slow down loading of the R module; particularly if
// they are not used often it may not be a good idea to go through and
// blindly implement everything possible.
//
// Some of these require that the contained elements have the '=='
// operator, '<' or '>' operators; these we can do fairly easily.
//
// Some require extra things that will be much harder to get right.
//
// I've just implemented find as a demo for now, and can come back and
// easily add others as needed.

// The full list is:
//
// adjacent_find
// all_of
// any_of
// binary_search
// copy
// copy_backward
// copy_if
// copy_n
// count
// count_if
// equal
// equal_range
// fill
// fill_n
// find
// find_end
// find_first_of
// find_if
// find_if_not
// for_each
// generate
// generate_n
// includes
// inplace_merge
// is_heap
// is_heap_until
// is_partitioned
// is_permutation
// is_sorted
// is_sorted_until
// iter_swap
// lexicographical_compare
// lower_bound
// make_heap
// max
// max_element
// merge
// min
// minmax
// minmax_element
// min_element
// mismatch
// move
// move_backward
// next_permutation
// none_of
// nth_element
// partial_sort
// partial_sort_copy
// partition
// partition_copy
// partition_point
// pop_heap
// prev_permutation
// push_heap
// random_shuffle
// remove
// remove_copy
// remove_copy_if
// remove_if
// replace
// replace_copy
// replace_copy_if
// replace_if
// reverse
// reverse_copy
// rotate
// rotate_copy
// search
// search_n
// set_difference
// set_intersection
// set_symmetric_difference
// set_union
// shuffle
// sort
// sort_heap
// stable_partition
// stable_sort
// swap
// swap_ranges
// transform
// unique
// unique_copy
// upper_bound

namespace forest {

template <typename Iterator>
Iterator find(Iterator first, Iterator last,
	      const typename Iterator::value_type& val) {
  return std::find(first, last, val);
}

// "distance" comes from <iterator>, not <algorithm>, but it seems to
// belong here.  It is potentially dangerous for users to use on
// arbitrary iterators.

}

// Helper to export with standard names:
#define FOREST_ITERATOR_MODULE_ALGORITHM(type, name)     \
  Rcpp::function("find_" name, &forest::find<type>);     \
  Rcpp::function("distance_" name, &std::distance<type>) \

#endif
