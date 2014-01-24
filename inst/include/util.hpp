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

// Branches have a beginning and an end; we want to have both
// beginning and end values for a branch (so the value of the
// calculation at the tip and at the root).
//
// An alternative way would be storing just the tip calculation until
// the the branch is computed at which point compute the root, but
// that would require storing the tip values somewhere else and
// re-initialising them every time.
//
// Note that this is basically a std::pair, but homogeneous in type
// and with first/second renamed tipward/rootward).
template <typename T>
struct branch_pair {
  typedef T value_type;
  branch_pair(const T& tipward_, const T& rootward_)
    : tipward(tipward_), rootward(rootward_) {}
  T tipward;
  T rootward;
};

}
}

#endif
