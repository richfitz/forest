# Forest

[![Build Status](https://travis-ci.org/richfitz/forest.png?branch=master)](https://travis-ci.org/richfitz/forest)

A new tree data structure for phylogenetics in R

This package arises from my frustration with dealing with edge
matrices to work with phylogenetic trees in R.  The idea is to use a
nice tree data structure instead of manually manipulating branch
indices.

This package will not have any phylogenetic models (such as those in
diversitree, geiger, ape, etc).  However, it will be a basis for
building fast calculators and simulators.

This is my side project, so don't expect great things quickly.
Currently, I'm focussing on wrapping the underlying data structure
nicely with Rcpp.
