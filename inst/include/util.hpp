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


}
}

#endif
