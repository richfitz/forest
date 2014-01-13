#ifndef _FOREST_TREE_HPP_
#define _FOREST_TREE_HPP_

#include <Rcpp.h>
#include "treetree.hpp"

// Iterators
//
// Pointing at nodes (node iterators)
//   pre_iterator       -- in preorder
//   post_iterator      -- in preorder
//   child_iterator     -- over daughters of a node
//
// Pointing  at subtrees (subtree iterators)
//   sub_pre_iterator   -- in preorder
//   sub_post_iterator  -- in preorder
//   sub_child_iterator -- over daughters of a node

namespace forest {

// Forward reference
template <typename T> class subtree_wrapped;

template <typename T>
class tree_wrapped {
public:
  typedef treetree::tree<T>                          tree_type;
  typedef treetree::subtree<T>                       subtree_type;
  typedef subtree_wrapped<T>                         subtree_wrapped_type;
  typedef typename tree_type::value_type             value_type;

  // Iterator types:
  typedef typename tree_type::pre_iterator           pre_iterator;
  typedef typename tree_type::post_iterator          post_iterator;
  typedef typename tree_type::child_iterator         child_iterator;
  typedef typename tree_type::sub_pre_iterator       sub_pre_iterator;
  typedef typename tree_type::sub_post_iterator      sub_post_iterator;
  typedef typename tree_type::sub_child_iterator     sub_child_iterator;

  // Used internally, only.
private:
  typedef typename tree_type::const_pre_iterator     const_pre_iterator;
  typedef typename tree_type::const_sub_pre_iterator const_sub_pre_iterator;

public:
  tree_wrapped() {}
  tree_wrapped(const T& t) : tree_(t) {}

  // Constructor for the wrapper code:
  tree_wrapped(const tree_type& tree) : tree_(tree) {}

  tree_wrapped<T> copy() const {return *this;}
  void clear() {tree_.clear();}

  // 1. Basic interrogation:
  bool empty()     const {return tree_.empty();    }
  size_t size()    const {return tree_.size();     }
  size_t arity()   const {return tree_.arity();    }
  bool childless() const {return tree_.childless();}
  std::string representation() const {
    return boost::lexical_cast<std::string>(tree_);}

  // 2. Accessors
  value_type root()        {return tree_.root();     }
  value_type front()       {return tree_.front();    }
  value_type back()        {return tree_.back();     }
  subtree_type root_sub()  {return tree_.root_sub(); }
  subtree_type front_sub() {return tree_.front_sub();}
  subtree_type back_sub()  {return tree_.back_sub(); }

  subtree_type at(size_t idx) {return tree_[idx];}
  void insert_at(size_t idx, const subtree_type& value) {
    tree_[idx] = value;}
  // NOTE: Base-1 access/set methods for R use.
  //
  // TODO: Need range checks here -- will crash R if out-of-bounds
  // iterators are used.
  subtree_type r_at(size_t idx) {return at(idx-1);}
  void r_insert_at(size_t idx, const subtree_type& value) {
    insert_at(idx-1, value);}

  // 3. Iterators
  pre_iterator       begin()           {return tree_.begin();          }
  pre_iterator       end()             {return tree_.end();            }
  post_iterator      begin_post()      {return tree_.begin_post();     }
  post_iterator      end_post()        {return tree_.end_post();       }
  child_iterator     begin_child()     {return tree_.begin_child();    }
  child_iterator     end_child()       {return tree_.end_child();      }

  sub_pre_iterator   begin_sub()       {return tree_.begin_sub();      }
  sub_pre_iterator   end_sub()         {return tree_.end_sub();        }
  sub_post_iterator  begin_sub_post()  {return tree_.begin_sub_post(); }
  sub_post_iterator  end_sub_post()    {return tree_.end_sub_post();   }
  sub_child_iterator begin_sub_child() {return tree_.begin_sub_child();}
  sub_child_iterator end_sub_child()   {return tree_.end_sub_child();  }

  // 4. Insert
  //
  // NOTE: In theory, this is all that is needed, but we need to be
  // able to deal with all the different iterator types, as they will
  // silently convert from one to the other (this is instantiated in
  // the module code as a pre_iterator).
  void insert(pre_iterator i, const value_type& v) {
    tree_.insert(i, v);}
  void insert_subtree(pre_iterator i, const subtree_type& s) {
    tree_.insert(i, s);}
  void insert_n(pre_iterator i, size_t n, const value_type& v) {
    tree_.insert(i, n, v);}
  void insert_subtree_n(pre_iterator i, size_t n, const subtree_type& s) {
    tree_.insert(i, n, s);}

  // NOTE: insert_above and insert_below should return iterators.  But
  // getting the correct type back out may be tricky, so I'm skipping
  // this for now.
  void insert_above(pre_iterator i, const value_type& v) {
    tree_.insert_above(i, v);}
  void insert_below(pre_iterator i, const value_type& v) {
    tree_.insert_below(i, v);}

  // 5 Append + Prepend
  void append(const value_type& v)  {tree_.append(v); }
  void prepend(const value_type& v) {tree_.prepend(v);}
  void append_subtree(const subtree_type s)  {tree_.append(s); }
  void prepend_subtree(const subtree_type s) {tree_.prepend(s);}

  void append_n(size_t n, const value_type& v) {
    tree_.append(n, v);}
  void prepend_n(size_t n, const value_type& v) {
    tree_.prepend(n, v);}
  void append_subtree_n(size_t n, const subtree_type s) {
    tree_.append(n, s);}
  void prepend_subtree_n(size_t n, const subtree_type s) {
    tree_.prepend(n, s);}

  // 6. Splice
  void splice(pre_iterator i, subtree_type s) {
    tree_.splice(i, s);}
  void splice_pair(pre_iterator i,
		   sub_child_iterator fi, sub_child_iterator li) {
    tree_.splice(i, fi, li); }

  // 7. Destructive modification
  // NOTE: flatten should return iterator, but does not (c.f. insert_above)
  void prune()                 {tree_.prune();   }
  void flatten(pre_iterator i) {tree_.flatten(i);}
  void erase(pre_iterator i)   {tree_.erase(i);  }
  // NOTE: Dangerous if wrong iterators given
  void erase_pair(child_iterator f, child_iterator l) {
    tree_.erase(f, l);}

  // 8. Equality testing
  bool operator==(const tree_wrapped<T>& rhs) const {
    return this->tree_ == rhs.tree_;}

  // NOTE: This is the only method here that actually depends on
  // Rcpp.h being included, and I'm not sure it's actually useful.
  bool is_node_type(SEXP obj) const {
    try {
#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
#endif
      value_type tmp = Rcpp::as<value_type>(obj);
      return true;
#ifdef __clang__
#pragma clang diagnostic pop
#endif
    } catch (...) {
      return false;
    }
  }

  // Public for the 'as' method
  tree_type tree_;
};

template <typename T>
class subtree_wrapped {
public:
  // NOTE: Going through tree_wrapped_type here so that is the only
  // place where the type relations are defined.
  typedef tree_wrapped<T>                           tree_wrapped_type;
  typedef typename tree_wrapped_type::tree_type     tree_type;
  typedef typename tree_wrapped_type::value_type    value_type;
  typedef typename tree_wrapped_type::subtree_type  subtree_type;

  typedef typename tree_type::pre_iterator          pre_iterator;
  typedef typename tree_type::post_iterator         post_iterator;
  typedef typename tree_type::child_iterator        child_iterator;
  typedef typename tree_type::sub_pre_iterator      sub_pre_iterator;
  typedef typename tree_type::sub_post_iterator     sub_post_iterator;
  typedef typename tree_type::sub_child_iterator    sub_child_iterator;

  // NOTE: No exported constructor, copy, clear in interface.

  // Constructor for the wrapper code:
  subtree_wrapped(const subtree_type& subtree) : subtree_(subtree) {}

  // 1. Basic interrogation
  bool   empty()     const {return subtree_.empty();    }
  size_t size()      const {return subtree_.size();     }
  size_t arity()     const {return subtree_.arity();    }
  bool   childless() const {return subtree_.childless();}
  std::string representation() const {
    return boost::lexical_cast<std::string>(subtree_);}

  // 2. Accessors
  value_type root()        {return subtree_.root();     }
  value_type front()       {return subtree_.front();    }
  value_type back()        {return subtree_.back();     }
  subtree_type root_sub()  {return subtree_.root_sub(); }
  subtree_type front_sub() {return subtree_.front_sub();}
  subtree_type back_sub()  {return subtree_.back_sub(); }

  subtree_type at(size_t idx) { return subtree_[idx]; }
  void insert_at(size_t idx, const subtree_type& value) {
    subtree_[idx] = value; }
  // NOTE: Base-1 access/set methods for R use.
  // TODO: Need range checks here -- will crash R if out-of-bounds
  // iterators are used.
  // TODO: insert is unexported and untested.
  subtree_type r_at(size_t idx) { return at(idx-1); }
  void r_insert_at(size_t idx, const subtree_type& value) {
    insert_at(idx-1, value); }

  // 3. Iterators
  pre_iterator       begin()           {return subtree_.begin();      }
  pre_iterator       end()             {return subtree_.end();        }
  post_iterator      begin_post()      {return subtree_.begin_post(); }
  post_iterator      end_post()        {return subtree_.end_post();   }
  child_iterator     begin_child()     {return subtree_.begin_child();}
  child_iterator     end_child()       {return subtree_.end_child();  }

  sub_pre_iterator   begin_sub()       {return subtree_.begin_sub();      }
  sub_pre_iterator   end_sub()         {return subtree_.end_sub();        }
  sub_post_iterator  begin_sub_post()  {return subtree_.begin_sub_post(); }
  sub_post_iterator  end_sub_post()    {return subtree_.end_sub_post();   }
  sub_child_iterator begin_sub_child() {return subtree_.begin_sub_child();}
  sub_child_iterator end_sub_child()   {return subtree_.end_sub_child();  }

  // 4. Insert
  void insert(pre_iterator i, const value_type& v) {
    subtree_.insert(i, v);}
  void insert_subtree(pre_iterator i, const subtree_type& s) {
    subtree_.insert(i, s);}
  void insert_n(pre_iterator i, size_t n, const value_type& v) {
    subtree_.insert(i, n, v);}
  void insert_subtree_n(pre_iterator i, size_t n, const subtree_type& s) {
    subtree_.insert(i, n, s);}

  // NOTE: returning void -- see tree.insert_above().
  void insert_above(pre_iterator i, const value_type& v) {
    subtree_.insert_above(i, v);}
  void insert_below(pre_iterator i, const value_type& v) {
    subtree_.insert_below(i, v);}

  // 5 Append + Prepend
  void append(const value_type& v)  {subtree_.append(v); }
  void prepend(const value_type& v) {subtree_.prepend(v);}
  void append_subtree(const subtree_type s)  {subtree_.append(s); }
  void prepend_subtree(const subtree_type s) {subtree_.prepend(s);}

  void append_n(size_t n, const value_type& v) {
    subtree_.append(n, v);}
  void prepend_n(size_t n, const value_type& v) {
    subtree_.prepend(n, v);}
  void append_subtree_n(size_t n, const subtree_type s) {
    subtree_.append(n, s);}
  void prepend_subtree_n(size_t n, const subtree_type s) {
    subtree_.prepend(n, s);}

  // 6. Splice
  void splice(pre_iterator i, subtree_type s) {
    subtree_.splice(i, s);}
  void splice_pair(pre_iterator i,
		   sub_child_iterator fi, sub_child_iterator li) {
    subtree_.splice(i, fi, li);}

  // 7. Destructive modification
  // NOTE: flatten should return iterator, but does not (c.f. insert_above)
  void prune()                 {subtree_.prune();   }
  void flatten(pre_iterator i) {subtree_.flatten(i);}
  void erase(pre_iterator i)   {subtree_.erase(i);  }
  void erase_pair(child_iterator f, child_iterator l) {
    subtree_.erase(f, l); }

  // 8. Equality testing
  bool operator==(const subtree_wrapped& rhs) const {
    return this->subtree_ == rhs.subtree_; }

  // Public for the 'as' method
  subtree_type subtree_;
};

}

#endif
