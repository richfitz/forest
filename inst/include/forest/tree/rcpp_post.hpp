#ifndef _FOREST_TREE_RCPP_POST_HPP_
#define _FOREST_TREE_RCPP_POST_HPP_

#include <forest/tree/rcpp_pre.hpp>

#include <Rcpp.h>

namespace forest {
// TODO: This should probably be a forest_node to match the R side?
// It's not used very often, so that's not that much of a big deal.
typedef forest::node<Rcpp::RObject>    forest_node;
typedef treetree::tree<forest_node>    forest_tree;
typedef treetree::subtree<forest_node> forest_subtree;

// This is all we're defining for now.  Will grow over time though.
// Really this one should only work for T_in = Rcpp::RObject and
// Rcpp::List.

// NOTE: This does not quite do what is wanted yet; I think that it is
// being over-eager in what is being converted.  In particular, I'd
// like to specialise this so that we only go down this route when
// T_in is Rcpp::RObject, but using the definition
//
//   template <typename T_out>
//   T_out data_convert(const Rcpp::RObject& obj) {
//     return Rcpp::as<T_out>(obj);
//   }
//
// gives a linker error.
//
// At the least it would be nice to specialise the T_out = T_in case
// to avoid a bunch of allocations.  This will have to do for now
// though (also, without careful tests I don't know when or if we use
// this without an Rcpp type as T_in).
//
// There is also the real possibility that we'll end up running into
// issues with having return-value overloading.  I might have to
// overload something like the exporter interface within Rcpp to make
// this work nicely.  This is actually a totally non-trivial piece of
// code.  The only thing guaranteed is that it is going to copy.
template <typename T_out, typename T_in>
T_out data_convert(const T_in& obj) {
  return Rcpp::as<T_out>(Rcpp::wrap(obj));
}

namespace util {

// Shared with rodeint, though not sure what it will take to get an
// equivalent rebuild working.
template <typename T>
void check_ptr_valid(Rcpp::XPtr<T> p) {
  T* test = p;
  if (test == NULL) {
    util::stop("Pointer is NULL");
  }
}

}

// These are little bits of code that will be useful in keeping the
// Rcpp::as functions tidy.  This is here just so that we have an
// internal namespace and to keep things organised, and to keep the
// exporter code as simple as possible (as it's a bit of a weird bit
// of code).
namespace exporters {
// All these functions, given a SEXP, or something that can be
// implicitly constructed from a SEXP, create an object of some type.
// They'll have to be called with template arguments as the templated
// thing is the return argument.

template <typename T>
node<T> node_from_R(Rcpp::RObject x) {
  const std::string cl = Rcpp::as<std::string>(x.attr("class"));
  if (cl != "forest_node") {
    util::stop("Expected forest_node, recieved " + cl);
  }
  Rcpp::List xl = Rcpp::as<Rcpp::List>(x);
  return node<T>(Rcpp::as<std::string>(xl["label"]),
                 Rcpp::as<double>(xl["length"]),
                 Rcpp::as<T>(xl["data"]));
}

// There is a bit of a trick here: sometimes we are going to get trees
// and sometimes we are going to get their pointers.  This little
// wrapper is going to cut back on some of the repetition here.  We
// don't keep full as/wrap transitiveness though because it's way more
// convenient to pass in the pointer directly from the containing
// class.
//
// Basically, we check the class -- if it's a forest_tree then we'll
// coerce it to a reference class object and grab the pointer field.
// Otherwise we assume it's a pointer and just cast it to the right
// type.  This second part could be improved by (a) checking that
// there is *no* class attribute.  (b) checking the "type" attribute
// of the pointer.
//
// Note that if we use a different class type (wch classes or
// something) this will not work and we'll have to use some other
// magic.
template <typename T>
Rcpp::XPtr<treetree::tree<T> > tree_ptr_from_R(Rcpp::RObject x) {
  typedef Rcpp::XPtr<treetree::tree<T> > ptr;
  if (x.hasAttribute("class") &&
      Rcpp::as<std::string>(x.attr("class")) == "forest_tree") {
    Rcpp::Reference obj = Rcpp::as<Rcpp::Reference>(x);
    ptr xp = Rcpp::as<ptr>(obj.field("ptr"));
    forest::util::check_ptr_valid(xp);
    return xp;
  } else {
    ptr xp = Rcpp::as<ptr>(x);
    forest::util::check_ptr_valid(xp);
    return xp;
  }
}

template <typename T>
Rcpp::XPtr<treetree::subtree<T> > subtree_ptr_from_R(Rcpp::RObject x) {
  typedef Rcpp::XPtr<treetree::subtree<T> > ptr;
  if (x.hasAttribute("class") &&
      Rcpp::as<std::string>(x.attr("class")) == "forest_subtree") {
    Rcpp::Reference obj = Rcpp::as<Rcpp::Reference>(x);
    ptr xp = Rcpp::as<ptr>(obj.field("ptr"));
    forest::util::check_ptr_valid(xp);
    return xp;
  } else {
    ptr xp = Rcpp::as<ptr>(x);
    forest::util::check_ptr_valid(xp);
    return xp;
  }
}

template <typename T>
treetree::tree<T>& tree_from_R(Rcpp::RObject x) {
  return *tree_ptr_from_R<T>(x);
}

template <typename T>
treetree::subtree<T>& subtree_from_R(Rcpp::RObject x) {
  return *subtree_ptr_from_R<T>(x);
}

}
}

namespace Rcpp {

template <>
inline SEXP wrap(const forest::util::index& obj) {
  return wrap(obj.i);
}

// For both trees and subtrees, we'll pop a copy of the tree into an
// external pointer and return that with a "type" attribute that means
// we can work with it.

// TODO: Would be useful to be able to say what the internal type is
// here, but I don't think it's actually necessary.  And R users may
// not care.  Options for doing this nicely include:
// http://stackoverflow.com/questions/12877521/human-readable-type-info-name
// http://ideone.com/L3T3p
// http://stackoverflow.com/questions/81870/print-variable-type-in-c
// http://stackoverflow.com/a/1055563
// the result of which we could store in some attribute.

// TODO: The approach of wrapping things up here could possibly be
// improved on, and will need to be if we're going to use the a non-RC
// class.  One approach would be to create a function in
// package:forest and use that -- could simply be forest::forest_tree
// and forest::forest_subtree, which is quite nice actually.  My guess
// is that the lookup cost here is not that terrible and should be
// cached between uses.
template <typename T>
SEXP wrap(const treetree::tree<T>& obj) {
  XPtr<treetree::tree<T> > ptr(new treetree::tree<T>(obj), true);
  ptr.attr("type") = "forest_tree";

  Environment methods("package:methods");
  Function Rnew = methods["new"];
  return Rnew("forest_tree", ptr);
}

template <typename T>
SEXP wrap(const treetree::subtree<T>& obj) {
  XPtr<treetree::subtree<T> > ptr(new treetree::subtree<T>(obj), true);
  ptr.attr("type") = "forest_subtree";

  Environment methods("package:methods");
  Function Rnew = methods["new"];
  return Rnew("forest_subtree", ptr);
}

// Node level wrapping is different to the tree level wrapping because
// we'll actually copy completely into an R list.  Then on the R side
// we lock down the accessors a bit to give relative type-safety.
// This might change to become a pointer, but given that we're only
// going to work with copies of objects the reference semantics are
// not hugely useful.
template <typename T>
SEXP wrap(const forest::node<T>& obj) {
  Rcpp::List ret = Rcpp::List::create(_["label"]  = obj.label_,
                                      _["length"] = obj.length_,
                                      _["data"]   = obj.data_);
  ret.attr("height") = obj.height_;
  ret.attr("depth")  = obj.depth_;
  ret.attr("class")  = "forest_node";
  return ret;
}

// I am not sure who uses this, or why it exists.  Might try and
// remove it soon.  It looks like it converts an
// template<>
// inline SEXP wrap(const treetree::tree<forest::node<Rcpp::List> >& obj) {
//   typedef forest::node<Rcpp::List>    T_in;
//   typedef forest::node<Rcpp::RObject> T_out;
//   return Rcpp::wrap(forest::copy_convert<T_out,T_in>(obj));
// }

template <>
inline forest::util::index as(SEXP obj) {
  return forest::util::index(Rcpp::as<int>(obj));
}

namespace traits {
template <typename T>
class Exporter<forest::node<T> > {
public:
  Exporter (SEXP x) : nd(forest::exporters::node_from_R<T>(x)) {}
  inline forest::node<T> get() { return nd; }
private:
  forest::node<T> nd;
};


// TODO: Check that the reference bits here work OK.
template <typename T>
class Exporter<treetree::tree<T> > {
public:
  Exporter (SEXP x) : t(forest::exporters::tree_from_R<T>(x)) {}
  inline const treetree::tree<T>& get() const { return t; }
  inline treetree::tree<T>& get() { return t; }
private:
  treetree::tree<T>& t;
};

template <typename T>
class Exporter<treetree::subtree<T> > {
public:
  Exporter (SEXP x) : t(forest::exporters::subtree_from_R<T>(x)) {}
  inline const treetree::subtree<T>& get() const { return t; }
  inline treetree::subtree<T>& get() { return t; }
private:
  treetree::subtree<T>& t;
};

}
}

#endif
