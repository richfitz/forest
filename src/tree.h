// -*-c++-*-
#ifndef _FOREST_TREE_H_
#define _FOREST_TREE_H_

#ifdef __clang__
#pragma clang diagnostic push
// These I have no control over because they're treetree issues.
#pragma clang diagnostic ignored "-Wshadow"
#pragma clang diagnostic ignored "-Wdocumentation"
#pragma clang diagnostic ignored "-Wconversion"
#pragma clang diagnostic ignored "-Wsign-conversion"
#endif
#include "treetree/tree.hpp"
#include "treetree/tree_io.hpp"
#ifdef __clang__
#pragma clang diagnostic pop
#endif

namespace forest {

template <typename T>
struct node {
public:
  node(const T& data_) : data(data_) {}
  bool operator==(const node<T>& rhs) const {return data == rhs.data;}
  T data;
};

template<typename T>
std::ostream& operator<<(std::ostream& out, const forest::node<T>& nd) {
  out << nd.data;
  return out;
}

// Forward reference
template <typename T> class subtree_wrapped;

template <typename T>
class tree {
public:
  typedef node<T>                                    node_type;
  typedef TREE_TREE_NAMESPACE::tree<node_type>       tree_type;
  typedef TREE_TREE_NAMESPACE::subtree<node_type>    subtree_type;
  typedef subtree_wrapped<T>                         subtree_wrapped_type;

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
  tree() {}
  tree(const T& t) : tree_(node_type(t)) {}
  tree<T> copy() const {return *this;}
  void clear() {tree_.clear();}

  // 1. Basic interrogation:
  bool empty()     const {return tree_.empty();    }
  size_t size()    const {return tree_.size();     }
  size_t arity()   const {return tree_.arity();    }
  bool childless() const {return tree_.childless();}
  std::string representation() const {
    return boost::lexical_cast<std::string>(tree_);}

  // 2. Accessors
  //
  // NOTE: Given nature of iterators, etc, this could either return
  // the node or the node contents.  Returning the contents is more
  // like tree_runner, but returning the node is more like the
  // iterator.  If I scrap the node structure entirely (quite likely)
  // then this problem goes away.
  node_type root()         {return tree_.root();     }
  node_type front()        {return tree_.front();    }
  node_type back()         {return tree_.back();     }
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
  void insert(pre_iterator i, const T& v) {
    tree_.insert(i, node_type(v));}
  void insert_subtree(pre_iterator i, const subtree_type& s) {
    tree_.insert(i, s);}
  void insert_n(pre_iterator i, size_t n, const T& v) {
    tree_.insert(i, n, node_type(v));}
  void insert_subtree_n(pre_iterator i, size_t n, const subtree_type& s) {
    tree_.insert(i, n, s);}

  // NOTE: insert_above and insert_below should return iterators.  But
  // getting the correct type back out may be tricky, so I'm skipping
  // this for now.
  void insert_above(pre_iterator i, const T& v) {
    tree_.insert_above(i, node_type(v));}
  void insert_below(pre_iterator i, const T& v) {
    tree_.insert_below(i, node_type(v));}

  // 5 Append + Prepend
  void append_node(const T& v)  {tree_.append(node_type(v)); }
  void prepend_node(const T& v) {tree_.prepend(node_type(v));}
  void append_subtree(const subtree_type s)  {tree_.append(s); }
  void prepend_subtree(const subtree_type s) {tree_.prepend(s);}

  void append_node_n(size_t n, const T& v) {
    tree_.append(n, node_type(v));}
  void prepend_node_n(size_t n, const T& v) {
    tree_.prepend(n, node_type(v));}
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
  bool is_equal_to(const tree<T>& rhs) const {
    return this->tree_ == rhs.tree_;}

private:
  tree_type tree_;
};

template <typename T>
class subtree_wrapped {
public:
  typedef tree<T> tree_type;

  // Pull some typedefs in from the parent tree type:
  typedef typename tree_type::node_type       node_type;
  typedef typename tree_type::subtree_type    subtree_type;

  typedef typename tree_type::pre_iterator        pre_iterator;
  typedef typename tree_type::post_iterator       post_iterator;
  typedef typename tree_type::child_iterator      child_iterator;
  typedef typename tree_type::sub_pre_iterator    sub_pre_iterator;
  typedef typename tree_type::sub_post_iterator   sub_post_iterator;
  typedef typename tree_type::sub_child_iterator  sub_child_iterator;

  // NOTE: No exported constructor, copy, clear in interface.
  subtree_wrapped(const subtree_type& subtree) : subtree_(subtree) {}

  // 1. Basic interrogation
  bool   empty()     const {return subtree_.empty();    }
  size_t size()      const {return subtree_.size();     }
  size_t arity()     const {return subtree_.arity();    }
  bool   childless() const {return subtree_.childless();}
  std::string representation() const {
    return boost::lexical_cast<std::string>(subtree_);}

  // 2. Accessors (NOTE: most missing)
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
  pre_iterator   begin()       {return subtree_.begin();      }
  pre_iterator   end()         {return subtree_.end();        }
  post_iterator  begin_post()  {return subtree_.begin_post(); }
  post_iterator  end_post()    {return subtree_.end_post();   }
  child_iterator begin_child() {return subtree_.begin_child();}
  child_iterator end_child()   {return subtree_.end_child();  }

  // NOTE: (missing all _sub iterators?)

  // 4. Insert
  void insert(pre_iterator i, const T& v) {
    subtree_.insert(i, node_type(v));}
  void insert_subtree(pre_iterator i, const subtree_type& s) {
    subtree_.insert(i, s);}
  void insert_n(pre_iterator i, size_t n, const T& v) {
    subtree_.insert(i, n, node_type(v));}
  void insert_subtree_n(pre_iterator i, size_t n, const subtree_type& s) {
    subtree_.insert(i, n, s);}

  // NOTE: returning void -- see tree.insert_above().
  void insert_above(pre_iterator i, const T& v) {
    subtree_.insert_above(i, node_type(v));}
  void insert_below(pre_iterator i, const T& v) {
    subtree_.insert_below(i, node_type(v));}

  // 5 Append + Prepend
  void append_node(const T& v)  {subtree_.append(node_type(v));}
  void prepend_node(const T& v) {subtree_.prepend(node_type(v));}
  void append_subtree(const subtree_type s)  {subtree_.append(s); }
  void prepend_subtree(const subtree_type s) {subtree_.prepend(s);}

  void append_node_n(size_t n, const T& v) {
    subtree_.append(n, node_type(v));}
  void prepend_node_n(size_t n, const T& v) {
    subtree_.prepend(n, node_type(v));}
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
  bool is_equal_to(const subtree_wrapped& rhs) const {
    return this->subtree_ == rhs.subtree_; }

  // This could be private, if not for the as templated function.
  subtree_type subtree_;
};

}

#endif
