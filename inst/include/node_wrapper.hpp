#ifndef _FOREST_NODE_WRAPPER_HPP_
#define _FOREST_NODE_WRAPPER_HPP_

// This is going to exist to deal with passing back and forth between
// R and C++.  It should immediately work for any as/wrappable type T
// within the node's data field.  It's a slightly weird class because
// this only really makes sense to parametrise on Rcpp::RObject, but I
// want to define this without referencing Rcpp so that I can get the
// as/wrap magic to work.

// So, with this, plus the bits in tree_rcpp.hpp we can write
// functions that return objects of type forest::node<T> for any type
// T.  On conversion to R the node get put into one of these wrapped
// classes, with the data slot (of type T) converted to an
// Rcpp::Object.

#include "node.hpp"

namespace forest {

template <typename T>
struct node_wrapped {
  typedef T data_type;
  node_wrapped() : node_() {}
  node_wrapped(const std::string& label)
    : node_(label) {}
  node_wrapped(const std::string& label, double length)
    : node_(label, length) {}
  node_wrapped(const std::string& label, double length, const T& data)
   : node_(label, length, data) {}

  // This is the way that the wrapper will do construction.
  node_wrapped(const node<T>& node) : node_(node) {}

  node_wrapped copy() const {return *this;}
  bool equals(const node_wrapped<T>& rhs) const { return node_ == rhs.node_; }

  bool has_label()  const {return node_.has_label();}
  bool has_length() const {return node_.has_length();}
  double height()   const {return node_.height_;}
  double depth()    const {return node_.depth_;}

  std::string get_label() const {return node_.label_;}
  void set_label(std::string label) {node_.label_ = label;}
  double get_length() const {return node_.length_;}
  void set_length(double length) {node_.length_ = length;}
  data_type get_data() const {return node_.data_;}
  void set_data(data_type data) {node_.data_ = data;}

  node<T> node_;
};

}

#endif
