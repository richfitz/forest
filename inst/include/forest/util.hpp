#ifndef _FOREST_UTIL_HPP_
#define _FOREST_UTIL_HPP_

#include <string>
#include <algorithm> // std::swap
#include <sstream>
#include <vector>
#include <set>
#include <RcppCommon.h> // Rcpp::stop

namespace forest {
namespace util {

inline void stop(const std::string& message) {
  Rcpp::stop(message);
}

// Adapted from the C++ FAQ
inline double string_to_double(const std::string& str) {
  std::istringstream i(str);
  double x;
  if (!(i >> x)) {
    stop("failed to convert " + str + " to double");
  }
  return x;
}

// From http://stackoverflow.com/questions/2769174/determining-if-an-unordered-vectort-has-all-unique-elements
template <class T>
bool is_unique(const std::vector<T>& x) {
  std::set<T> y(x.begin(), x.end());
  return x.size() == y.size();
}

template<typename T>
std::string to_string(T x) {
  std::ostringstream o;
  if (!(o << x)) {
    stop("String conversion failure");
  }
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
  if (expected != received) {
    stop("Incorrect length input; expected " +
         util::to_string(expected) + ", received " +
         util::to_string(received));
  }
}

// Both check bounds and convert base-1 to base-0.  For use in R
// indexing.
inline size_t check_bounds_r(size_t idx, size_t size) {
  // We don't check size < 0 or idx < 0, as not possible with size_t
  if (size == 0) {
    stop("Index " + util::to_string(idx) +
         " out of bounds: container is empty");
  } else if (idx < 1 || idx > size) {
    stop("Index " + util::to_string(idx) +
         " out of bounds: must be in [1," + util::to_string(size) + "]");
  }
  return idx - 1;
}

// Similar, but check indices in base-0.
inline size_t check_bounds(size_t idx, size_t size, bool base0) {
  if (size == 0) {
    stop("Index " + util::to_string(idx) +
         " out of bounds: container is empty");
  }

  if (base0) {
    if (idx >= size) {
      stop("Index " + util::to_string(idx) +
           " out of bounds: must be in [0," + util::to_string(size-1) + "]");
    }
  } else {
    if (idx < 1 || idx > size) {
      stop("Index " + util::to_string(idx) +
           " out of bounds: must be in [1," + util::to_string(size) + "]");
    }
    idx--;
  }
  return idx;
}

inline size_t safe_size_t_from_r(int n) {
  if (n < 0) {
    stop("Required a non-negative value");
  }
  return static_cast<size_t>(n);
}

// Range checked indices from R.
inline size_t safe_index_from_r(int i, size_t size) {
  return check_bounds(safe_size_t_from_r(i), size, false);
}

// Improve the logic here to not accept negative values on creation,
// rather than down the track.
struct index {
  int i; // in base0
  index(size_t ui) : i(static_cast<int>(ui + 1)) {}
  index(int i_)    : i(i_)                       {}
  // As a UI, these could work as operator()?
  size_t as_size(size_t size) {
    return safe_index_from_r(i, size);
  }
  size_t as_size() {
    return safe_size_t_from_r(i - 1);
  }
};

}
}

#endif
