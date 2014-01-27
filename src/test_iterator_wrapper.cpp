#include "iterator/iterator_wrapper.hpp"

namespace forest {
namespace iterator {
namespace test {

// Convenience typedefs:
typedef std::vector<double> vector_double;

void vector_double_resize(vector_double *obj, size_t size);
void vector_double_assign(vector_double* obj, Rcpp::NumericVector data);
double vector_double_at(vector_double* obj, size_t position);
void vector_double_insert(vector_double* obj, int position,
			  Rcpp::NumericVector data);
Rcpp::NumericVector vector_double_asR(vector_double* obj);
void vector_double_set(vector_double* obj, size_t i, double value);
vector_double::iterator vector_double_begin(vector_double* obj);
vector_double::iterator vector_double_end(vector_double* obj);

void vector_double_resize(vector_double *obj, size_t size) {
  obj->resize(size);
}
void vector_double_assign(vector_double* obj, Rcpp::NumericVector data) {
  obj->assign(data.begin(), data.end());
}
double vector_double_at(vector_double* obj, size_t position) {
  return obj->at(position - 1);
}
void vector_double_insert(vector_double* obj, int position,
			  Rcpp::NumericVector data) {
  vector_double::iterator it = obj->begin();
  std::advance(it, position - 1);
  obj->insert(it, data.begin(), data.end());
}
Rcpp::NumericVector vector_double_asR(vector_double* obj) {
  return Rcpp::wrap(*obj);
}
void vector_double_set(vector_double* obj, size_t i, double value) {
  obj->at(i - 1) = value;
}
// These incredibly simple functions (methods for proper classes) do
// seem to be needed to avoid an ambiguity in method resolution
// (possibly const vs non-const templates?).  I think it's around the
// return type of begin(), which can't be inferred if you set this up
// directly in the module with
//   .method("begin", &vector_double::begin)
vector_double::iterator vector_double_begin(vector_double* obj) {
  return obj->begin();
}
vector_double::iterator vector_double_end(vector_double* obj) {
  return obj->end();
}

}
}
}

// This line is required to make the iterators available to R --
// though that will pretty much always be the idea when creating a
// wrapped iterator!
FOREST_ITERATOR_EXPORT(forest::iterator::test::vector_double::iterator)

#ifdef __clang__
#pragma clang diagnostic push
// These I have no control over because they're Rcpp issues.
#pragma clang diagnostic ignored "-Wglobal-constructors"
#pragma clang diagnostic ignored "-Wexit-time-destructors"
#pragma clang diagnostic ignored "-Wmissing-prototypes"
#endif
RCPP_MODULE(test_iterator_wrapper) {
#ifdef __clang__
#pragma clang diagnostic pop
#endif
  Rcpp::class_<forest::iterator::test::vector_double>("vector_double")
    .constructor()
    .constructor<int>()

    .property("size",      &forest::iterator::test::vector_double::size)
    .property("max_size",  &forest::iterator::test::vector_double::max_size)
    .method("resize",      &forest::iterator::test::vector_double_resize)
    .property("capacity",  &forest::iterator::test::vector_double::capacity)
    .property("empty",     &forest::iterator::test::vector_double::empty)
    .method("reserve",     &forest::iterator::test::vector_double::reserve)
    .method("push_back",   &forest::iterator::test::vector_double::push_back)
    .method("pop_back",    &forest::iterator::test::vector_double::pop_back)
    .method("clear",       &forest::iterator::test::vector_double::clear)

    .const_method("back",  &forest::iterator::test::vector_double::back)
    .const_method("front", &forest::iterator::test::vector_double::front)
    .method("at",          &forest::iterator::test::vector_double_at)

    .method("assign",      &forest::iterator::test::vector_double_assign)
    .method("insert",      &forest::iterator::test::vector_double_insert)
    .method("as.vector",   &forest::iterator::test::vector_double_asR)

    // special methods for indexing
    .method("[[",          &forest::iterator::test::vector_double_at)
    .method("[[<-",        &forest::iterator::test::vector_double_set)

    // Iterators
    .method("begin",       &forest::iterator::test::vector_double_begin)
    .method("end",         &forest::iterator::test::vector_double_end)
    ;

  FOREST_ITERATOR_MODULE(forest::iterator::test::vector_double::iterator,
    "vector_double_iterator")
    ;
}
