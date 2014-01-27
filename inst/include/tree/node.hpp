#ifndef _FOREST_TREE_NODE_HPP_
#define _FOREST_TREE_NODE_HPP_

// This is the thing that will be a generic node type.  The things
// that we need are going to be
//
//   * label (unique over the tree?)
//   * branch length
//
// and possibly some computed things
//
//   * distance above the root
//   * distance below the tallest tip
//
// but we can save that for later.
//
// The slot "data_" will hold something special: for R based trees
// that will be a RObject, so that it can hold any R object at all (so
// a list for example).
//
// However, on the C++ side we might want to make a node where the
// data type is a numeric vector, or a boost::array in order to make
// an efficient calculator (e.g., boost::array<double,3> would be
// perfect for a BM calculator).

#include <R.h>     // NA_REAL, ISNA
#include <string>
#include <ostream>
#include <cmath>   // std::abs

namespace forest {

// NOTE: The constructor dance below is a bit annoying; I assume that
// it makes the least sense to initialise a node without a label, then
// an edge length, and then some data.  That's not always going to be
// the case of course, but values can be directly initialised (though
// the public data fields) so where inappropriate it's easy enough to
// deal with.
template <typename T>
struct node {
  typedef T data_type;
  node()
    : label_(""),    length_(NA_REAL), data_(),
      height_(NA_REAL), depth_(NA_REAL) {}
  node(const std::string& label)
    : label_(label), length_(NA_REAL), data_(),
      height_(NA_REAL), depth_(NA_REAL) {}
  node(const std::string& label, double length)
    : label_(label), length_(length),  data_(),
      height_(NA_REAL), depth_(NA_REAL) {}
  node(const std::string& label, double length, const data_type& data)
    : label_(label), length_(length),  data_(data),
      height_(NA_REAL), depth_(NA_REAL) {}

  bool operator==(const node<data_type>& rhs) const {
    return (label_       == rhs.label_       &&
            has_length() == rhs.has_length() &&
	    (!has_length() || !(std::abs(length_ - rhs.length_) > 0)) &&
            data_   == rhs.data_);}
  node copy() const {return *this;}
  bool has_label()  const {return label_ != "";}
  bool has_length() const {return !ISNA(length_);}
  std::string label_;
  double      length_;
  data_type   data_;

  double height_; // Height above the root
  double depth_;  // Depth below the highest tip
};

template<typename T>
std::ostream& operator<<(std::ostream& out, const node<T>& nd) {
  out << nd.label_;
  return out;
}

// TODO: do this with enable_if and proper SFINAE
template <typename T>
std::string node_label(const forest::node<T>& nd) {
  return nd.label_;
}

}

#endif
