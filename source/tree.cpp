/*******************************************************
* Random projection tree - build trees & query in trees
* Ville Hyvönen
* HIIT
* ville.o.hyvonen<at>helsinki.fi 
* 14.10.2015
********************************************************/
 
#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

// Node of an RP-tree
struct Node {
  double split_point;
  Node* left;
  Node* right;
};


// Build RP tree from data matrix X
// Arguments:
// X = data matrix, one row is one data point & one column is one dimension
// n_0 = maximum leaf size of tree
// n_tree = how manieth of one set of multiple trees this tree is

// [[Rcpp::export]]
List buildTree(arma::mat X, int n_0, int n_tree) {
  List result;
  int n = X.n_rows;
  int dim = X.n_cols;
  int depth = ceil(log2(n / n_0));
  int first_idx = (n_tree - 1) * depth;
  Node root;
  root.split_point = 1.5;
  
  
  std::cout << "n: " << n << ", dim: "<< dim << ", depth: " << depth << ", first_idx: " << first_idx << std::endl; 
  std::cout << "split point: " << root.split_point << std::endl;
  
  
  result["data"] = X; 
  return result;
}

