// -*-c++-*-
#ifndef _FOREST_ITERATOR_WRAPPER_H_
#define _FOREST_ITERATOR_WRAPPER_H_

#include <Rcpp.h>

// Methods that iterators have that we want to duplicate:
//   "*x"     --> value
//   "*x = y" --> assign
//   "x == y" --> equals  // also R generic?
//   "x != y" --> differs // also R generic?
//   "x++"    --> increment or plus1
//   "x--"    --> decrement or minus1
//   advance(x) (is actually generic in C++)
//
// I should probably offer three forms of the increment/decrement
// operators -- increase or decrease by one (using pre-increment), and
// the versions that also return the value.
//
// I'd really really like some way of determining if the iterator has
// been made invalid, but that does not appear to be easily possible.
// So these need to be used with extreme care.  I feel like I could
// possibly grab an Xptr to the original data structure (which should
// protect it from garbage collection) which would prevent crashes,
// but that replaces a catastrophic bug with an unpredictible one.
//
// One option could be to always store the sequence end, plus a
// pointer to the sequence.  Then on passing back from R we check if
// our stored end is equal to the end from the pointer.  Not sure if
// that will always work though.

namespace forest {

// This psuedo-class exists only to simplify translation between C++
// and R names and syntaxes.  Things might get more useful later if I
// work out how to enable iterator validity checking though.  It's
// templated by the iterator, not the container, because I want to be
// able to have a const and non-const version of these.
template <typename Iterator>
class iterator_wrapper {
public:
  typedef typename Iterator::value_type value_type;
  iterator_wrapper(Iterator it_) : it(it_) {}
  iterator_wrapper copy() const {
    iterator_wrapper ret = *this;
    return ret;
  }
  value_type value() const { return *it; }
  void assign(value_type x) { *it = x; } // -- but not for const_iterators
  bool equals(const iterator_wrapper<Iterator>& rhs) const {
    return it == rhs.it;
  }
  bool differs(const iterator_wrapper<Iterator>& rhs) const {
    return it != rhs.it;
  }

  void increment() {++it;}
  void decrement() {--it;}
  void advance(int n) {std::advance(it, n);}

  Iterator iterator() const {return it;}

  static iterator_wrapper create(Iterator it_) {
    iterator_wrapper ret(it_);
    return ret;
  }
private:
  Iterator it;
};

// Not sure if a macro here can be avoided but may help.  The other
// way around would be to define a generic type and then have Rcpp do
// it's inheritance thing.  But that won't work well with `$value`,
// for which we need to define the correct type.
#define FOREST_ITERATOR_MODULE_WRAPPED(wrapped_type, name) \
  Rcpp::class_<wrapped_type>(name)	         \
  .method("copy",      &wrapped_type::copy)      \
  .property("value",   &wrapped_type::value)     \
  .method("assign",    &wrapped_type::assign)    \
  .method("equals",    &wrapped_type::equals)    \
  .method("differs",   &wrapped_type::differs)	 \
  .method("increment", &wrapped_type::increment) \
  .method("decrement", &wrapped_type::decrement) \
  .method("advance",   &wrapped_type::advance)	 \
  ;

// This organises building the big export string:
#define FOREST_ITERATOR_MODULE(type, name) \
  FOREST_ITERATOR_MODULE_WRAPPED(forest::iterator_wrapper<type>, name)

#define FOREST_ITERATOR_EXPORT(type) \
  RCPP_EXPOSED_CLASS_NODECL(forest::iterator_wrapper<type>)

}

#endif
