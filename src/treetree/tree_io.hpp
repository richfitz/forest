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
     Basic output and input on trees, format is e.g. f(x g(y z)). To output a
     tree as an s-expression, e.g. (f x (g y z)) do 
     ostream << sexpr_format(tree), likewise to read do 
     istream >> sexpr_format(tree).
****/

#ifndef _TREE_TREE_IO_HPP_
#define _TREE_TREE_IO_HPP_

#define BOOST_SPIRIT_USE_OLD_NAMESPACE	1

#include <algorithm>
#include <istream>
#include <ostream>
#include <string>
#include <stdexcept>
#include <boost/spirit/include/classic.hpp>
#include <boost/lexical_cast.hpp>
#include "tree.hpp"

namespace TREE_TREE_NAMESPACE {

template<typename>
struct sexpr_io_wrapper;

template<typename T> 
struct sexpr_io_wrapper<subtree<T> > {
  explicit sexpr_io_wrapper(subtree<T> t) : _tr(t) {}
  subtree<T> expr() { return _tr; }
  const_subtree<T> expr() const { return _tr; }
  typedef subtree<T> tree_t;
 private:
  subtree<T> _tr;
};
template<typename T> 
struct sexpr_io_wrapper<const_subtree<T> > {
  explicit sexpr_io_wrapper(const_subtree<T> t) : _tr(t) {}
  const_subtree<T> expr() const { return _tr; }
  typedef const_subtree<T> tree_t;
 private:
  const_subtree<T> _tr;
};
template<typename T>
struct sexpr_io_wrapper<tree<T>*> {
  explicit sexpr_io_wrapper(tree<T>& t) : _tr(&t) {}
  tree<T>& expr() { return *_tr; }
  const tree<T>& expr() const { return *_tr; }
  typedef tree<T> tree_t;
 private:
  tree<T>* _tr;
};
template<typename T>
struct sexpr_io_wrapper<const tree<T>*> {
  explicit sexpr_io_wrapper(const tree<T>& t) : _tr(&t) {}
  const tree<T>& expr() const { return *_tr; }
  typedef tree<T> tree_t;
 private:
  const tree<T>* _tr;
};

template<typename T>
sexpr_io_wrapper<tree<T> > sexpr_format(subtree<T> t) {
  return sexpr_io_wrapper<subtree<T> >(t);
}
template<typename T>
sexpr_io_wrapper<const_subtree<T> > sexpr_format(const_subtree<T> t) {
  return sexpr_io_wrapper<const_subtree<T> >(t); 
}
template<typename T>
sexpr_io_wrapper<tree<T>*> sexpr_format(tree<T>& t) {
  return sexpr_io_wrapper<tree<T>*>(t);
}
template<typename T>
sexpr_io_wrapper<const tree<T>*> sexpr_format(const tree<T>& t) {
  return sexpr_io_wrapper<const tree<T>*>(t);
}

// output
template<typename T>
std::ostream& operator<<(std::ostream& out,const_subtree<T> tr) {
  if (tr.childless())
    return out << tr.root();

  out << tr.root() << "(";
  for (typename const_subtree<T>::const_sub_child_iterator 
           it=tr.begin_sub_child();it!=tr.end_sub_child();++it)
    out << *it << (boost::next(it)==tr.end_sub_child() ? ")" : " ");
  return out;
}
template<typename T>
std::ostream& operator<<(std::ostream& out,subtree<T> tr) {
  return out << const_subtree<T>(tr);
}
template<typename T>
std::ostream& operator<<(std::ostream& out,const tree<T>& tr) {
  if (!tr.empty())
    out << const_subtree<T>(tr);
  return out;
}

// sexpr output
template<typename Tree>
std::ostream& operator<<(std::ostream& out,sexpr_io_wrapper<Tree> s) {
  if (s.expr().empty())
    return out << "()"; //empty tree
  else if (s.expr().childless())
    return out << s.expr().root();

  out << "(" << s.expr().root() << " ";
  for (typename sexpr_io_wrapper<Tree>::tree_t::const_sub_child_iterator 
           it=s.expr().begin_sub_child();it!=s.expr().end_sub_child();++it)
    out << sexpr_format(*it)
        << (boost::next(it)==s.expr().end_sub_child() ? ")" : " ");
  return out;
}

// input
namespace _tree_io_private {
using namespace boost::spirit;
using std::string;
struct tree_grammar : public boost::spirit::grammar<tree_grammar> {
  mutable tree<string>* tr;
  mutable tree<string>::sub_child_iterator at;
  bool sexpr_io;
  void begin_internal(const char* from, const char* to) const {
    if (sexpr_io)
      ++from;
    else
      --to;
    at=tr->insert(at,string(from,to))->begin_child();
  }
  void end_internal(const char) const { ++++at; }
  void add_leaf(const char* from, const char* to) const {
    tr->insert(at,string(from,to));
  }
  template<typename ScannerT>
  struct definition {
    definition(const tree_grammar& g) {
      term=lexeme_d[(+( anychar_p - ch_p('(') - ch_p(')') - space_p))]
          [boost::bind<void>(&tree_grammar::add_leaf,&g,_1,_2)];
      if (g.sexpr_io)
        beg=lexeme_d['(' >> (+( anychar_p - ch_p('(') - ch_p(')') - space_p))];
      else
        beg=lexeme_d[(+( anychar_p - ch_p('(') - ch_p(')') - space_p)) >> '('];
      expr=(beg[boost::bind(&tree_grammar::begin_internal,&g,_1,_2)]
            >> +expr >> ch_p(')')
            [boost::bind(&tree_grammar::end_internal,&g,_1)]) |
          term;
    }
    rule<ScannerT> expr,beg,term;
    
    const rule<ScannerT>& start() const { return expr; }
  };
};
inline bool lparen(char c) { return (c=='(' || c=='[' || c=='{'); }
inline bool rparen(char c) { return (c==')' || c==']' || c=='}'); }
inline bool whitespace(char c) { return (c==' ' || c=='\t' || c=='\n'); }
inline bool all_whitespace(const std::string& s) {
  return std::find_if(s.begin(),s.end(),!boost::bind(&whitespace,_1))==s.end();
}
inline std::string chomp(const std::string& s) {
  std::size_t i=0,j=s.length();
  while (i<j && whitespace(s[i]))
    ++i;
  do {
    --j;
  } while (i<j && whitespace(s[j]));
  return s.substr(i,++j);
}

inline void read_string_tree(std::istream& in,tree<std::string>& dst,
                             bool sexpr_io) {
  std::string str;
  std::string::size_type nparen=0;
  char c;
  bool started=false;
  do {
    c=in.get();
    if (lparen(c)) {
      started=true;
      ++nparen;
    } else if (rparen(c)) {
      --nparen;
    } else if (whitespace(c) && !started && !all_whitespace(str)) {
      dst=tree<std::string>(chomp(str)); //a single node
      return;
    }
    str.push_back(c);
  } while (in && c!=EOF && (nparen>0 || !started));
  if (c==EOF || whitespace(c)) {
    str.erase(--str.end());
    in.putback(c);
  }
  str=chomp(str);
  if (nparen!=0)
    throw std::runtime_error("paren mismatch reading: '"+str+"'");
  tree_grammar g;
  g.tr=&dst;
  g.at=dst.begin();
  g.sexpr_io=sexpr_io;
  parse(str.c_str(),g,space_p);
}
template<typename T>
void rec_copy(subtree<std::string> str,subtree<T> dst) {
  for (typename tree<std::string>::sub_child_iterator i=str.begin_sub_child();
       i!=str.end_sub_child();++i)
    rec_copy(*i,*dst.insert(dst.end_sub_child(),
                            boost::lexical_cast<T>(i->root())));
}
template<typename T>
std::istream& read_tree(std::istream& in,tree<T>& dst,bool sexpr_io) {
  tree<std::string> str;
  _tree_io_private::read_string_tree(in,str,sexpr_io);

  dst.clear();
  if (!str.empty()) {
    rec_copy(subtree<std::string>(str),
             *dst.insert(dst.end_sub(),boost::lexical_cast<T>(str.root())));
  }
  
  return in;
}
template<typename T>
std::istream& read_tree(std::istream& in,subtree<T> dst,bool sexpr_io) {
  tree<std::string> str;
  _tree_io_private::read_string_tree(in,str,sexpr_io);

  if (str.empty())
    throw std::runtime_error("can't read empty input into a subtree");

  dst.prune();
  dst.root()=boost::lexical_cast<T>(str.root());
  rec_copy(str,dst);
  
  return in;
}
} //namespace _tree_io_private

template<typename T>
std::istream& operator>>(std::istream& in,tree<T>& dst) { 
  return _tree_io_private::read_tree(in,dst,false);
}

template<typename T>
std::istream& operator>>(std::istream& in,subtree<T> dst) {
  return _tree_io_private::read_tree(in,dst,false);
}

template<typename Tree>
std::istream& operator>>(std::istream& in,sexpr_io_wrapper<Tree> s) {
  return _tree_io_private::read_tree(in,s.expr(),true);
}

} //namespace TREE_TREE_NAMESPACE
#endif //_TREE_TREE_IO_HPP_
