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
struct Node {
  double split_point;
  double cluster;
  Node* left;
  Node* right;
};


// Node* grow_subtree(const mat &projected_data, int n_0, int n_tree, const uvec &indices, uvec &clusters, int &current_cluster, int tree_level, uvec random_indices) {
Node* grow_subtree(int n_0, int n_tree, const uvec &indices, uvec &clusters, int &current_cluster, int tree_level, int depth) {
  int n = indices.size();
  Node* node = new Node;
  node->cluster = 0;
  node->split_point = tree_level;
  
  if(n <= n_0 || tree_level == depth - 1) {
    node->cluster = current_cluster;
    clusters.elem(indices) += current_cluster++;
    return node;
  }
  
  // vec projection = projected_data.submat(indices, random_indices);
  
  node->left = grow_subtree(n_0, n_tree, indices, clusters, current_cluster, tree_level + 1, depth);
  node->right = grow_subtree(n_0, n_tree, indices, clusters, current_cluster, tree_level + 1, depth);
  
  return node;
}

void print_tree(Node* node) {
  std::cout << "tree_level: " << node->split_point << ", cluster: " << node->cluster << std::endl;
  if(node->cluster) {
    std::cout << std::endl;
    return; 
  }
  print_tree(node->left);
  print_tree(node->right);  
}

void delete_tree(Node* node) {
  std::cout << "tree_level: " << node->split_point << ", cluster: " << node->cluster << std::endl;
  if(node->cluster) {
    std::cout << std::endl;
    return; 
  }
  delete_tree(node->left);
  delete node->left;
  delete_tree(node->right);
  delete node->right;
}



// Build RP tree from data matrix X
// Arguments:
// X = data matrix, one row is one data point & one column is one dimension
// n_0 = maximum leaf size of tree
// n_tree = how manieth of one set of multiple trees this tree is

// [[Rcpp::export]]
Rcpp::List grow_tree(mat X, int n_0, int n_tree) {
  Rcpp::List result;
  int n = X.n_rows;
  int dim = X.n_cols;
  int depth = ceil(log2(n / n_0));
  int first_idx = (n_tree - 1) * depth;
  int last_idx = first_idx + depth -1;
  int current_cluster = 1;

  uvec random_indices = linspace<uvec>(first_idx, last_idx, depth);
  uvec clusters(n);
  clusters.zeros();
  uvec indices = linspace<uvec>(0, n - 1, n);
  
  std::cout << "n: " << n << ", dim: "<< dim << ", depth: " << depth << ", first_idx: " << first_idx << ", last_idx: " << last_idx << std::endl; 
  std::cout << "current cluster: " << current_cluster << std::endl;
  std::cout << "random_indices:\n" << random_indices << std::endl;
  
  Node* tree = grow_subtree(n_0, n_tree, indices, clusters, current_cluster, 0, depth);
  print_tree(tree);
  
  delete_tree(tree);
  delete tree;
  tree = nullptr;
  
  result["data"] = X; 
  return result;
}


