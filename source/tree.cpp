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
  int cluster;
  Node* left;
  Node* right;
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

Node* grow_subtree(const mat &projected_data, int n_0, int n_tree, const uvec &indices, uvec &clusters, int &current_cluster, int tree_level, uvec random_indices) {
  int n = indices.size();
  Node* node = new Node;
  node->cluster = 0;

  if(n <= n_0) {
    node->cluster = current_cluster;
    clusters.elem(indices) += current_cluster++;
    return node;
  }
  
  // std::cout << "projection not yet allocated" << std::endl;
  mat temp = projected_data.rows(indices);
  vec projection = temp.col(random_indices(tree_level));
  // std::cout << "projection allocated" << std::endl;
  uvec ordered = sort_index(projection);

  
  int split_point = n % 2 ? n / 2 : n / 2 - 1;  // median split
  int idx_split_point = ordered(split_point);
  int idx_split_point2 = ordered(split_point + 1);
  // std::cout << "n: " << n << ", split point: " << split_point << ", n % 2: " << n % 2 << ", idx_split_point: " << idx_split_point << std::endl;
  // std::cout << "idx_split"
  node->split_point = n % 2 ? projection(idx_split_point) : (projection(idx_split_point) + projection(idx_split_point2)) / 2;
  // std::cout << "node->split_point allocated."  << std::endl;
  uvec left_indices = ordered.subvec(0, split_point);
  uvec right_indices = ordered.subvec(split_point + 1, n - 1);
  
  // std::cout << "ready for recursive calls."  << std::endl << std::endl;
  node->left = grow_subtree(projected_data, n_0, n_tree, left_indices, clusters, current_cluster, tree_level + 1, random_indices);
  node->right = grow_subtree(projected_data, n_0, n_tree, right_indices, clusters, current_cluster, tree_level + 1, random_indices);
  
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

void delete_tree(Node* node) {
  // std::cout << "split point: " << node->split_point << ", leaf label: " << node->cluster << std::endl;
  if(node->cluster) {
    // std::cout << std::endl;
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
Rcpp::List grow_tree(mat projected_data, int dim, int n_0, int n_tree) {  // projected_data referenssiksi!
  Rcpp::List result;
  int n = projected_data.n_rows;
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

  Node* tree = grow_subtree(projected_data, n_0, n_tree, indices, clusters, current_cluster, 0, random_indices);
  // print_tree(tree);
  
  delete_tree(tree);
  delete tree;
  tree = nullptr;
  
  result["data"] = projected_data; 
  return result;
}


