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

// This is needed to deal with the conversion of 'data' elements.
// However, the actual definition is deferred until after Rcpp is
// loaded because we're going to make use of that code there quite
// extensively.
//
// NOTE: This might be better in a utility file?
template <typename T_out, typename T_in>
T_out data_convert(const T_in& obj);

// NOTE: The constructor dance below is a bit annoying; I assume that
// it makes the least sense to initialise a node without a label, then
// an edge length, and then some data.  That's not always going to be
// the case of course, but values can be directly initialised (though
// the public data fields) so where inappropriate it's easy enough to
// deal with.
//
// TODO: Check that all the mess below is still needed.  We might need
// a default constructor but when do we use the others?
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

  // Note that these are not done via a runtime argument because that
  // would require that T *is* convertable to U, and I don't know that
  // is the case for sure.  One of these might be slightly better as a
  // copy constructor though (probably the with-data version).
  template <typename U>
  node<U> copy_structure() const {
    node<U> ret(label_, length_);
    ret.height_ = height_;
    ret.depth_  = depth_;
    return ret;
  }
  template <typename U>
  node<U> copy_convert() const {
    node<U> ret(label_, length_, data_convert<U>(data_));
    ret.height_ = height_;
    ret.depth_  = depth_;
    return ret;
  }

  bool operator==(const node<data_type>& rhs) const {
    return (label_       == rhs.label_       &&
            has_length() == rhs.has_length() &&
	    (!has_length() || !(std::abs(length_ - rhs.length_) > 0)) &&
            data_   == rhs.data_);}
  node copy() const {return *this;}
  // TODO: These can possibly be dropped now.
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
