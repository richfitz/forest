#ifndef _FOREST_MODELS_TRANSLATION_HPP_
#define _FOREST_MODELS_TRANSLATION_HPP_

// Support for models of continuous trait evolution that involve
// Gaussian distributions at nodes.

// NOTE: Because this requires some use of Rcpp types, this is
// something that will need to be included in models.hpp, not in
// common.hpp.
#include <forest/tree/common.hpp>
#include <forest/models/common.hpp>
#include <Rcpp.h>

namespace forest {
namespace models {

template <typename ModelData>
treetree::tree<node<branch_pair<ModelData> > >
build_model_tree(const treetree::tree<node<Rcpp::RObject> >& tree) {
  typedef node<Rcpp::RObject>           rnode;
  typedef node<branch_pair<ModelData> > mnode;

  treetree::tree<mnode> ret = copy_structure<mnode, rnode>(tree);
  typename treetree::tree<rnode>::const_pre_iterator it = tree.begin();
  typename treetree::tree<mnode>::pre_iterator it_to = ret.begin();
  while (it != tree.end()) {
    if (it->data_ != R_NilValue)
      it_to->data_ =
        branch_pair<ModelData>(ModelData(), // rootward, tipward
                               ModelData(ModelData::from_R(it->data_)));
    ++it_to;
    ++it;
  }
  return ret;
}

}
}

#endif
