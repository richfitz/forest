#include "iterator_wrapper.h"
#include "iterator_wrapper_algorithm.h"

#include "iterator_wrapper_test.h"

namespace forest {
namespace test {

void vector_double_assign(vector_double* obj, Rcpp::NumericVector data) {
  obj->assign(data.begin(), data.end());
}
double vector_double_at(vector_double* obj, size_t position) {
  return obj->at(position - 1);
}
void vector_double_insert(vector_double* obj, size_t position,
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
vector_double_iterator vector_double_begin(vector_double* obj) {
  return vector_double_iterator::create(obj->begin());
}
vector_double_iterator vector_double_end(vector_double* obj) {
  return vector_double_iterator::create(obj->end());
}

}
}

FOREST_ITERATOR_EXPORT(forest::test::vector_double::iterator)

#ifdef __clang__
#pragma clang diagnostic push
// These I have no control over because they're Rcpp issues.
#pragma clang diagnostic ignored "-Wglobal-constructors"
#pragma clang diagnostic ignored "-Wexit-time-destructors"
#pragma clang diagnostic ignored "-Wmissing-prototypes"
#endif
RCPP_MODULE(mod_test) {
#ifdef __clang__
#pragma clang diagnostic pop
#endif
  Rcpp::class_<forest::test::vector_double>("vector_double")
    .constructor()
    .constructor<int>()

    .property("size",      &forest::test::vector_double::size)
    .property("max_size",  &forest::test::vector_double::max_size)
    .method("resize",      &forest::test::vector_double::resize)
    .property("capacity",  &forest::test::vector_double::capacity)
    .property("empty",     &forest::test::vector_double::empty)
    .method("reserve",     &forest::test::vector_double::reserve)
    .method("push_back",   &forest::test::vector_double::push_back)
    .method("pop_back",    &forest::test::vector_double::pop_back)
    .method("clear",       &forest::test::vector_double::clear)

    .const_method("back",  &forest::test::vector_double::back)
    .const_method("front", &forest::test::vector_double::front)
    .method("at",          &forest::test::vector_double_at)

    .method("assign",      &forest::test::vector_double_assign)
    .method("insert",      &forest::test::vector_double_insert)
    .method("as.vector",   &forest::test::vector_double_asR)

    // special methods for indexing
    .method("[[",          &forest::test::vector_double_at)
    .method("[[<-",        &forest::test::vector_double_set)

    // Iterators
    .method("begin",       &forest::test::vector_double_begin)
    .method("end",         &forest::test::vector_double_end)
    ;

  FOREST_ITERATOR_MODULE(forest::test::vector_double::iterator,
    "vector_double_iterator")
    ;
  // Make algorithms available:
  FOREST_ITERATOR_MODULE_ALGORITHM(forest::test::vector_double::iterator,
    "vector_double_iterator")
    ;
}
