#ifndef _FOREST_MODELS_UTIL_HPP_
#define _FOREST_MODELS_UTIL_HPP_

namespace forest {
namespace models {
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
  branch_pair() : tipward(), rootward() {}
  branch_pair(const T& tipward_, const T& rootward_)
    : tipward(tipward_), rootward(rootward_) {}
  T tipward;
  T rootward;
};

}
}

#endif
