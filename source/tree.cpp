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
  double split_point;  //  split point (0 if this this is a leaf)
  int leaf_label;         //  leaf label if this is a leaf
  Node* left;          
  Node* right;         
  
  Node() {
    split_point = 0.0;
    leaf_label = 0;
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
  const mat& projected_data;  // data matrix projected into random vectors
  uword current_leaf_label;  // counter for leaf labels
  uvec leaf_labels;          // container for leaf_labels of the original data points
  uword first_idx;        // first col index of this tree in the projected_data
  int n_0;                // maximum leaf size
  int n_rows;
  int depth;              
  Node* tree;
  
  // Grow an RP-tree recursively 
  // Arguments:
  // indices = indices of original data matrix handled
  // tree_level = which level of the tree we are on
  Node* grow_subtree(const uvec &indices, uword tree_level) {
    int n = indices.size();
    Node* node = new Node;
    
    if(n <= n_0) {
      node->leaf_label = current_leaf_label;
      leaf_labels.elem(indices) = zeros<uvec>(n) + current_leaf_label++;
      // if(current_leaf_label < 100) std::cout << "indices:\n" << indices << std::endl << std::endl;
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
    node->left = grow_subtree(indices.elem(left_indices), tree_level + 1);
    // std::cout << "back from recursion, n: " << n  << std::endl << std::endl;
    node->right = grow_subtree(indices.elem(right_indices), tree_level + 1);
    
    return node;
  }
  
  
  
public:
  
  RP_tree(const mat& projected, int n_tree, int n_stop) : projected_data(projected), current_leaf_label(1), n_0(n_stop) {
    n_rows = projected_data.n_rows;
    depth = ceil(log2(n_rows / n_0));
    first_idx = (n_tree - 1) * depth;
    leaf_labels = zeros<uvec>(n_rows);
    tree = nullptr;
  }
  
  ~RP_tree() {
    // std::cout << "Poistetaan puu. " << std::endl;
    delete tree;
  }
  
  static void print(Node* node) {
    std::cout << "split point: " << node->split_point << ", leaf label: " << node->leaf_label << std::endl;
    if(node->leaf_label) {
      std::cout << std::endl;
      return; 
    }
    print(node->left);
    print(node->right);  
  }
  
  
  // Grow an RP-tree from data matrix X
  // Arguments:
  // X = data matrix, one row is one data point & one column is one dimension
  // n_0 = maximum leaf size of tree
  // n_tree = how manieth of one set of multiple trees this tree is
  
  void grow(bool print_tr) {  
    
    uvec indices = linspace<uvec>(0, n_rows - 1, n_rows);
    
    std::cout << "n_rows: " << n_rows << ", depth: " << depth << ", first_idx: " << first_idx << std::endl; 
    std::cout << "current leaf_label: " << current_leaf_label << std::endl;
    
    tree = grow_subtree(indices, 0);
    if(print_tr) print(tree);
    
    std::cout << "leaf_labels:\n" << leaf_labels.subvec(0,20) << std::endl;
    std::cout << "depth: " << depth << ", first_idx: " << first_idx << std::endl;
  }
  

  };





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
void test(const mat& projected_data, int n_0, int n_tree, bool print_tr) {
  RP_tree* tree = new RP_tree(projected_data, n_tree, n_0);
  (*tree).grow(print_tr);
  delete tree;
}


