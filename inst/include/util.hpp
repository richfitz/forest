#ifndef _FOREST_UTIL_HPP_
#define _FOREST_UTIL_HPP_

#include <string>
#include <algorithm> // std::swap
#include <sstream>

namespace forest {

void stop(const std::string& message);

namespace util {

// Adapted from the C++ FAQ
inline double string_to_double(const std::string& str) {
  std::istringstream i(str);
  double x;
  if (!(i >> x))
    stop("failed to convert " + str + " to double");
  return x;
}

template<typename T>
std::string to_string(T x) {
  std::ostringstream o;
  if (!(o << x))
    stop("String conversion failure");
  return o.str();
}

// Adapted from http://www.cplusplus.com/reference/algorithm/rotate/
//
// Needed because sub_child_iterator doesn't seem to work with
// std::rotate or std::reverse.
template <class ForwardIterator>
void rotate(ForwardIterator first, ForwardIterator middle,
            ForwardIterator last) {
  ForwardIterator next = middle;
  while (first!=next) {
    std::swap(*first++,*next++);
    if (next==last) next=middle;
    else if (first==middle) middle=next;
  }
}

inline void check_length(size_t received, size_t expected) {
  if (expected != received)
    stop("Incorrect length input; expected " +
         util::to_string(expected) + ", received " +
         util::to_string(received));
}

// Both check bounds and convert base-1 to base-0.  For use in R
// indexing.
inline size_t check_bounds_r(size_t idx, size_t size) {
  // We don't check size < 0 or idx < 0, as not possible with size_t
  if (size == 0)
    stop("Index " + util::to_string(idx) +
         " out of bounds: container is empty");
  else if (idx < 1 || idx > size)
    stop("Index " + util::to_string(idx) +
         " out of bounds: must be in [1," + util::to_string(size) + "]");
  return idx - 1;
}

}
}

#endif
