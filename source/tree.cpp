/*******************************************************
* Random projection tree - build trees & query in trees
* Ville Hyvönen
* HIIT
* ville.o.hyvonen<at>helsinki.fi 
* 14.10.2015
********************************************************/
 
#include <RcppArmadillo.h>
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]


// Node of an RP-tree
class Node {
private:
  
public:
  double split_point;
  int cluster;
  Node* left;
  Node* right;
  
  Node() {
    split_point = 0.0;
    cluster = 0;
    left = nullptr;
    right = nullptr;
  }
  
  ~Node() {
    // std::cout << "Poistetaan Node, split point: " << split_point << std::endl;
    delete left;
    delete right;
  }
};

// One RP-tree + metadata
class RP_tree {
private:
  
public:
  Node* tree;
  int depth;
  int dim;
  uvec clusters;
  uword first_idx;
  
  RP_tree(Node* tree_n, int depth_n, int dim_n, uvec clusters_n, uword first_idx_n) : 
  tree(tree_n),
  depth(depth_n),
  dim(dim_n),
  clusters(clusters_n),
  first_idx(first_idx_n) {}
  
  ~RP_tree() {
    // std::cout << "Poistetaan puu. " << std::endl;
    delete tree;
  }

  };

// Grow an RP-tree recursively 
// Arguments:
// projected_data = data matrix projected into random vectors
// n_0 = maximum leaf size
// n_tree = how manieth of one set of multiple trees this tree is
// indices = indices of original data matrix handled
// clusters = container for leaf_labels of the original data points
// current_cluster = counter for leaf labels
// tree_level = which level of the tree we are on
// random_indices = indices of rows we use from projected data
// Return value:
// node = node of a tree with 
//  split = split_point (0 if this this is a leaf)
//  cluster = leaf label if this is a leaf
//  left = pointer to the left child
//  right = pointer to the right child

Node* grow_subtree(const mat& projected_data, int n_0, int n_tree, const uvec &indices, uvec &clusters, uword &current_cluster, uword tree_level, uword first_idx) {
  int n = indices.size();
  Node* node = new Node;
  
  if(n <= n_0) {
    node->cluster = current_cluster;
    clusters.elem(indices) = zeros<uvec>(n) + current_cluster++;
    // if(current_cluster < 100) std::cout << "indices:\n" << indices << std::endl << std::endl;
    return node;
  }
  
  // std::cout << "projection not yet allocated" << std::endl;
  mat temp = projected_data.rows(indices);
  vec projection = temp.col(first_idx + tree_level);
  // std::cout << "projection allocated" << std::endl;
  uvec ordered = sort_index(projection);

  
  int split_point = n % 2 ? n / 2 : n / 2 - 1;  // median split
  int idx_split_point = ordered(split_point);
  int idx_split_point2 = ordered(split_point + 1);
  // std::cout << "n: " << n << ", split point: " << split_point << ", n % 2: " << n % 2 << ", idx_split_point: " << idx_split_point << std::endl;
  
  node->split_point = n % 2 ? projection(idx_split_point) : (projection(idx_split_point) + projection(idx_split_point2)) / 2;
  // std::cout << "node->split_point allocated."  << std::endl;
  uvec left_indices = ordered.subvec(0, split_point);
  uvec right_indices = ordered.subvec(split_point + 1, n - 1);
  
  // std::cout << "ready for recursive calls."  << std::endl;
  node->left = grow_subtree(projected_data, n_0, n_tree, indices.elem(left_indices), clusters, current_cluster, tree_level + 1, first_idx);
  // std::cout << "back from recursion, n: " << n  << std::endl << std::endl;
  node->right = grow_subtree(projected_data, n_0, n_tree, indices.elem(right_indices), clusters, current_cluster, tree_level + 1, first_idx);
  
  return node;
}


void print_tree(Node* node) {
  std::cout << "split point: " << node->split_point << ", leaf label: " << node->cluster << std::endl;
  if(node->cluster) {
    std::cout << std::endl;
    return; 
  }
  print_tree(node->left);
  print_tree(node->right);  
}


// Build RP tree from data matrix X
// Arguments:
// X = data matrix, one row is one data point & one column is one dimension
// n_0 = maximum leaf size of tree
// n_tree = how manieth of one set of multiple trees this tree is

RP_tree* grow_tree(const mat& projected_data, int dim, int n_0, int n_tree, bool print_tr) {  
  int n = projected_data.n_rows;
  int depth = ceil(log2(n / n_0));
  uword first_idx = (n_tree - 1) * depth;
  uword current_cluster = 1;

  uvec clusters(n);
  clusters.zeros();
  uvec indices = linspace<uvec>(0, n - 1, n);
  
  std::cout << "n: " << n << ", dim: "<< dim << ", depth: " << depth << ", first_idx: " << first_idx << std::endl; 
  std::cout << "current cluster: " << current_cluster << std::endl;

  Node* tree = grow_subtree(projected_data, n_0, n_tree, indices, clusters, current_cluster, 0, first_idx);
  if(print_tr) print_tree(tree);
  
  RP_tree* result = new RP_tree(tree, depth, dim, clusters, first_idx); 

  std::cout << "clusters:\n" << result->clusters.subvec(0,20) << std::endl;
  std::cout << "depth: " << result->depth << ", dim: " << result->dim << ", first_idx: " << result->first_idx << std::endl;
  
  return result;
}

// // [[Rcpp::export]]
// int tree_query(Node* tree) {
//   
// }


// // [[Rcpp::export]]
// Rcpp::List grow_multiple_trees(mat X, int n_0, int n_trees) {
//   Rcpp::List result;
//   int dim = X.n_cols;
//   int n = X.n_rows;
//   int depth = ceil(log2(n / n_0));
//   int n_pool = n_trees * depth;
//   
//   // generate random vectors for all the trees and project data onto them
//   mat random_matrix(dim, n_pool);
//   random_matrix.randn();
//   mat projected_data = X * random_matrix;
//   
//   
//   
//   return result;
// }


// [[Rcpp::export]]
void test(const mat& projected_data, int dim, int n_0, int n_tree, bool print_tr) {
  RP_tree* tree = grow_tree(projected_data, dim, n_0, n_tree, print_tr);
  delete tree;
}

// [[Rcpp::export]]
double test_nm( Rcpp::NumericMatrix X ) {
  return 0.0 ;
}

// [[Rcpp::export]]
double test_arma( mat X ) {
  return 0.0 ;
}

// [[Rcpp::export]]
double test_const_arma( const mat& X ) { 
  return 0.0 ;
}

