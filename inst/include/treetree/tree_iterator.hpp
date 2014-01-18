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

/****
     Generic iterators for appending and prepending to trees and subtrees.
****/

#ifndef _TREE_TREE_ITERATOR_HPP_
#define _TREE_TREE_ITERATOR_HPP_

namespace TREE_TREE_NAMESPACE {

namespace _tree_iterator_private {
template<typename Container,typename Out>
struct out_iter
    : public boost::iterator_facade<Out,Out,std::output_iterator_tag> {

  out_iter(Container& c) : _c(&c) {}
 protected:
  Container* _c;

  friend class boost::iterator_core_access;
  const Out& dereference() const { return *static_cast<const Out*>(this); }
};
} //namespace _tree_iterator_private

template<typename Container>
struct append_iterator
    : public _tree_iterator_private::out_iter<Container,
                                              append_iterator<Container> > {

  append_iterator(Container& c)
      : _tree_iterator_private::out_iter<Container,
                                         append_iterator<Container> >(c) {}

  append_iterator& operator=(typename Container::const_reference v) {
    this->_c->append(v);
    return *this;
  }
};
template<typename Container>
struct prepend_iterator
    : public _tree_iterator_private::out_iter<Container,
                                              prepend_iterator<Container> > {

  prepend_iterator(Container& c)
      : _tree_iterator_private::out_iter<Container,
                                         prepend_iterator<Container> >(c) {}

  prepend_iterator& operator=(typename Container::const_reference v) {
    this->_c->prepend(v);
    return *this;
  }
};

template<typename Container>
append_iterator<Container>
appender(Container& c) { 
  return append_iterator<Container>(c);
}

template<typename Container>
prepend_iterator<Container>
prepender(Container& c) {
  return prepend_iterator<Container>(c);
}

} //namespace TREE_TREE_NAMESPACE
#endif //_TREE_TREE_ITERATOR_HPP_
