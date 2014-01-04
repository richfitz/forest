// -*-c++-*-
#ifndef _FOREST_ITERATOR_WRAPPER_TEST_H_
#define _FOREST_ITERATOR_WRAPPER_TEST_H_

#include <Rcpp.h>

// To test the iterator wrapping, let's wrap up the vector class,
// following Rcpp-modules example.  The only difference is that we'll
// take care of the indexing so that it appears 1-based from R.

namespace forest {
namespace test {

// Convenience typedefs:
typedef std::vector<double> vector_double;
typedef iterator_wrapper<vector_double::iterator> vector_double_iterator;

void vector_double_assign(vector_double* obj, Rcpp::NumericVector data);
double vector_double_at(vector_double* obj, size_t position);
void vector_double_insert(vector_double* obj, size_t position, Rcpp::NumericVector data);
Rcpp::NumericVector vector_double_asR(vector_double* obj);
void vector_double_set(vector_double* obj, size_t i, double value);
vector_double_iterator vector_double_begin(vector_double* obj);
vector_double_iterator vector_double_end(vector_double* obj);

}
}

#endif
