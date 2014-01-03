// Copyright 2007-2008 Google Inc. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License")
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an AS IS BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// Author: madscience@google.com (Moshe Looks)

// convenience foreach used by tree tests (but not by the library itself)

#ifndef _TREE_FOREACH_HPP_
#define _TREE_FOREACH_HPP_

#include <boost/iterator/counting_iterator.hpp>
#include <boost/range.hpp>

namespace boost { 

inline counting_iterator<int> begin(const int&) { 
  return make_counting_iterator(0);
}
inline counting_iterator<int> end(const int& i) {
  return make_counting_iterator(i);
}
inline counting_iterator<std::size_t> begin(const std::size_t&) {
  return make_counting_iterator(std::size_t(0));
}
inline counting_iterator<std::size_t> end(const std::size_t& i) {
  return make_counting_iterator(i);
}
//} //namespace range_detail

template<>
struct range_iterator<int> { typedef counting_iterator<int> type; };
template<>
struct range_const_iterator<int> { typedef counting_iterator<int> type; };
template<>
struct range_iterator<std::size_t> { 
  typedef counting_iterator<std::size_t> type; 
};
template<>
struct range_const_iterator<std::size_t> { 
  typedef counting_iterator<std::size_t> type; 
};

} //namespace boost

#ifndef BOOST_FOREACH
#include <boost/foreach.hpp>
#endif
#define foreach BOOST_FOREACH

#endif //_TREE_FOREACH_HPP_
