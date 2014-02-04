#include "tree.hpp"

#include "tree/ape.hpp"
#include "tree/newick.hpp"

// Temporary -- used in treeapply
template <typename T>
Rcpp::List drain_tree(const treetree::tree<T>& tree) {
  Rcpp::List ret;
  for (typename treetree::tree<T>::const_pre_iterator
         it = tree.begin(); it != tree.end(); ++it)
    ret.push_back(Rcpp::wrap(*it));
  return ret;
}

using forest::rnode;
using forest::rtree;
using forest::rsubtree;

// This is identical to the xnode description.
#ifdef __clang__
#pragma clang diagnostic push
// These I have no control over because they're Rcpp issues.
#pragma clang diagnostic ignored "-Wglobal-constructors"
#pragma clang diagnostic ignored "-Wexit-time-destructors"
#pragma clang diagnostic ignored "-Wmissing-prototypes"
#endif
RCPP_MODULE(forest) {
#ifdef __clang__
#pragma clang diagnostic pop
#endif
  Rcpp::class_<rnode>("rnode")
    .constructor()
    .constructor<std::string>()
    .constructor<std::string, double>()
    .constructor<std::string, double, rnode::data_type>()

    .field("label",              &rnode::label_)
    .field("length",             &rnode::length_)
    .field("data",               &rnode::data_)

    .property("has_label",       &rnode::has_label)
    .property("has_length",      &rnode::has_length)

    .field_readonly("height",    &rnode::height_)
    .field_readonly("depth",     &rnode::depth_)

    .method("copy",              &rnode::copy<Rcpp::RObject>)
    .method("equals",            &rnode::operator==)
    ;

  Rcpp::class_<rtree>("rtree")
    .constructor()
    .constructor<rtree::node_type>()

    .method("copy",              &rtree::copy)
    .method("clear",             &rtree::clear)

    // 1. Basic interrogation
    .property("empty",           &rtree::empty)
    .property("size",            &rtree::size)
    .property("arity",           &rtree::arity)
    .property("childless",       &rtree::childless)
    .property("representation",  &rtree::representation)

    // 2. Accessors: Provide access to root node.  I wonder if we
    // should let '[[' access children?  And should allow root
    // assignment.  But with [[, how to distinguish between subtrees
    // and nodes of subtrees?  Probably return subtrees by default.
    //
    // TODO: Refuse to return root node from empty tree.
    .property("root_node",       &rtree::root,
              &rtree::set_root_node)
    .method("[[",                &rtree::r_at)
    .method("[[<-",              &rtree::r_insert_at)

    .method("equals",            &rtree::operator==)

    .property("tips",            &rtree::tips)
    .property("nodes",           &rtree::nodes)
    .property("tip_labels",      &rtree::tip_labels)
    .property("node_labels",     &rtree::node_labels)
    .property("heights",         &rtree::heights)
    .property("depths",          &rtree::depths)
    .property("is_binary",       &rtree::is_binary)
    .property("has_branch_lengths",
              &rtree::has_branch_lengths)
    .method("is_ultrametric",    &rtree::is_ultrametric)
    .method("update_heights",    &rtree::update_heights)

    .method("collapse_singles",  &rtree::collapse_singles)
    .method("drop_tips",         &rtree::drop_tips)
    .method("rotate",            &rtree::rotate)
    .method("ladderise",         &rtree::ladderise)
    .method("get_subtree",       &rtree::get_subtree)

    .method("check_names",       &rtree::check_names)
    .method("associate_data",    &rtree::associate_data)
    .method("duplicate_topology",&rtree::duplicate_topology)
    ;

  Rcpp::class_<rsubtree>("rsubtree")
    // NOTE: no constructor, copy, clear
    // 1. Basic interrogation
    .property("empty",           &rsubtree::empty)
    .property("size",            &rsubtree::size)
    .property("arity",           &rsubtree::arity)
    .property("childless",       &rsubtree::childless)
    .property("representation",  &rsubtree::representation)

    // 2. Accessors: Provide access to root node.  I wonder if we
    // should let '[[' access children?  And should allow root
    // assignment.  But with [[, how to distinguish between subtrees
    // and nodes of subtrees?  Probably return subtrees by default.
    .property("root_node",       &rsubtree::root,
              &rsubtree::set_root_node)
    .method("[[",                &rsubtree::r_at)
    .method("[[<-",              &rsubtree::r_insert_at)

    .method("equals",            &rsubtree::operator==)

    // Extra
    .property("tips",            &rsubtree::tips)
    .property("nodes",           &rsubtree::nodes)
    .property("tip_labels",      &rsubtree::tip_labels)
    .property("node_labels",     &rsubtree::node_labels)
    .property("heights",         &rsubtree::heights)
    .property("depths",          &rsubtree::depths)

    .method("rotate",            &rsubtree::rotate)
    .method("get_subtree",       &rsubtree::get_subtree)
    .method("to_tree",           &rsubtree::to_tree)
    ;

  Rcpp::function("to_newick_string",
                 &forest::to_newick_string<rtree::node_type>);
  Rcpp::function("from_newick_node",
                 &forest::from_newick_node<rtree::node_type>);
  Rcpp::function("from_newick_string",
                 &forest::from_newick_string<rtree::node_type>);
  Rcpp::function("from_ape_internal",
                 &forest::from_ape_internal<rtree::node_type>);
  Rcpp::function("to_ape_internal",
                 &forest::to_ape_internal<rtree::node_type>);

  Rcpp::function("drain_tree",   &drain_tree<rtree::node_type>);
}
