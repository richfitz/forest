// -*-c++-*-
#ifndef _FOREST_NODE_H_
#define _FOREST_NODE_H_

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
// that will (eventually) by a RObject, so that it can hold any R
// object at all (so a list for example).
//
// However, on the C++ side we might want to make a node where the
// data type is a numeric vector, or a boost::array in order to make
// an efficient calculator (e.g., boost::array<double,3> would be
// perfect for a BM calculator).

#include <string>
#include <ostream>

namespace forest {

template <typename T>
struct node {
  typedef T value_type;
  node(const value_type& data)
    : data_(data), label_("") {}
  node(const value_type& data, const std::string& label)
    : data_(data), label_(label) {}
  bool operator==(const node<value_type>& rhs) const {
    return (data_  == rhs.data_   &&
	    label_ == rhs.label_);}
  node copy() const {return *this;}
  value_type  data_;
  std::string label_;
};

template<typename T>
std::ostream& operator<<(std::ostream& out, const node<T>& nd) {
  out << nd.label_;
  return out;
}

}

#endif
