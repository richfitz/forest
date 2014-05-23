#ifndef _FOREST_TREE_NEWICK_HPP_
#define _FOREST_TREE_NEWICK_HPP_

#include <ostream>
#include <iomanip>

#include <forest/tree.hpp>
#include <forest/util.hpp>

namespace forest {

// Output:
template <typename T>
void to_newick(std::ostream& out, const node<T>& nd) {
  out << nd.label_;
  if (nd.has_length())
    out << ":" << nd.length_;
}

template <typename T>
void to_newick(std::ostream& out, treetree::const_subtree<T> tr) {
  if (!tr.childless()) {
    out << "(";
    for (typename treetree::const_subtree<T>::const_sub_child_iterator
           it=tr.begin_sub_child(); it != tr.end_sub_child(); ++it) {
      to_newick(out, *it);
      out << (boost::next(it) == tr.end_sub_child() ? ")" : ",");
    }
  }
  to_newick(out, tr.root());
}

template <typename T>
void to_newick(std::ostream& out, treetree::subtree<T> tr) {
  to_newick(out, treetree::const_subtree<T>(tr));
}
template <typename T>
void to_newick(std::ostream& out,const treetree::tree<T>& tr) {
  if (!tr.empty())
    to_newick(out, treetree::const_subtree<T>(tr));
}

// NOTE:
//
// I can either declare/export this way:
//   template <typename T> std::string f(const T& tr)
//   Rcpp::function("f", &forest::f<xtree>);
//
// Or this way:
//
//   template <typename T> std::string f(const treetree::tree<T>& tr)
//   Rcpp::function("f", &forest::f<xtree::value_type>);
template <typename T>
std::string to_newick_string(treetree::tree<T> tr, int digits) {
  std::stringstream out;
  out << std::setprecision(digits);
  to_newick(out, tr);
  out << ";";
  return out.str();
}

// Input:

// First, parse a node.
//    label          -> label
//    label[:length] -> label, length
template <typename T>
T from_newick_node(std::string x) {
  const size_t i = x.rfind(":");
  if (i != std::string::npos) { // We do have a branch length here.
    const double length = util::string_to_double(x.substr(i + 1, x.size()));
    x = x.substr(0, i);
    T nd(x, length);
    return nd;
  } else {
    T nd(x);
    return nd;
  }
}

enum newick_token_type {PAREN_OPEN, PAREN_CLOSE, COMMA, SEMICOLON, NODE};
typedef std::pair<newick_token_type, std::string> newick_token;

inline newick_token_type categorise_newick_token(const std::string& s) {
  newick_token_type t;
  if (s == "(")
    t = PAREN_OPEN;
  else if (s == ")")
    t = PAREN_CLOSE;
  else if (s == ",")
    t = COMMA;
  else if (s == ";")
    t = SEMICOLON;
  else
    t = NODE;
  return t;
}

template <typename T>
std::vector<newick_token>::iterator
from_newick(treetree::tree<T>& tr,
            std::vector<newick_token>::iterator it,
            std::vector<newick_token>::iterator last) {
  if (it->first != PAREN_OPEN)
    Rcpp::stop("Parse error -- expected opening parenthesis");
  ++it;

  while (it != last) {
    if (it->first == PAREN_CLOSE) {
      ++it;
      break;
    } else if (it->first == PAREN_OPEN) {
      T sub_root_node;
      treetree::tree<T> sub(sub_root_node);
      it = from_newick<T>(sub, it, last);
      tr.insert(tr.end_child(), sub);
    } else if (it->first == NODE) {
      tr.insert(tr.end_child(), from_newick_node<T>(it->second));
      ++it;
    } else { // COMMA
      ++it;
    }
  }

  if (it != last && it->first == NODE) {
    T root = from_newick_node<T>(it->second);
    *tr.begin() = root;
    ++it;
  }

  return it;
}

template <typename T>
treetree::tree<T>
from_newick_string(const std::vector<std::string>& tokens_str) {
  std::vector<newick_token> tokens;
  for (std::vector<std::string>::const_iterator
         el = tokens_str.begin(); el != tokens_str.end(); ++el)
    tokens.push_back(newick_token(categorise_newick_token(*el), *el));

  // Destination tree:
  T root_node;
  treetree::tree<T> tr(root_node);
  std::vector<newick_token>::iterator it =
    from_newick(tr, tokens.begin(), tokens.end());

  if (it == tokens.end() || it->first != SEMICOLON)
    Rcpp::stop("Parse error -- expected terminating semicolon");

  return tr;
}

}

#endif
