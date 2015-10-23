/*******************************************************
* Random projection tree - build trees & query in trees
* Ville Hyvönen
* HIIT
* ville.o.hyvonen<at>helsinki.fi 
* 14.10.2015
********************************************************/
 
#include <RcppArmadillo.h>
#include <array>
#include <unordered_set>
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

public:
  RP_tree(const mat& projected, int n_tree, int n_stop) : projected_data(projected), current_leaf_label(1), n_0(n_stop) {
    n_rows = projected_data.n_rows;
    depth = ceil(log2(n_rows / n_0));
    first_idx = n_tree * depth;
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
  

  void grow() {  
    tree = grow_subtree(linspace<uvec>(0, n_rows - 1, n_rows), 0);  // all rows of data, level of the tree
  }
  
  // Query in a RP-tree
  // projected_query = the query point projected into all vectors of in all the RP-trees
  // return : label of the leaf the query point is routed into in this RP-tree
  int query(const vec& projected_query) {
    Node* node = tree;
    int i = first_idx;
    
    while(!node->leaf_label)  
      node = projected_query(i++) <= node->split_point ? node->left : node->right;   
    
    return node->leaf_label;     
  }
  
  
  Node* get_tree() {
    return tree;
  }
  
  uvec get_leaf_labels() {
    return leaf_labels;
  }
  
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
      return node;
    }
    
    mat temp = projected_data.rows(indices);
    vec projection = temp.col(first_idx + tree_level);
    uvec ordered = sort_index(projection);
    
    int split_point = n % 2 ? n / 2 : n / 2 - 1;  // median split
    int idx_split_point = ordered(split_point);
    int idx_split_point2 = ordered(split_point + 1);

    node->split_point = n % 2 ? projection(idx_split_point) : (projection(idx_split_point) + projection(idx_split_point2)) / 2;
    uvec left_indices = ordered.subvec(0, split_point);
    uvec right_indices = ordered.subvec(split_point + 1, n - 1);
    
    node->left = grow_subtree(indices.elem(left_indices), tree_level + 1);
    node->right = grow_subtree(indices.elem(right_indices), tree_level + 1);
    
    return node;
  }


};



class Mrpt {
public:
  
  Mrpt(const mat& X_, int n_trees_, int n_0_) : X(X_), n_trees(n_trees_), n_0(n_0_) {
    n_rows = X.n_rows;
    dim = X.n_cols;
    depth = ceil(log2(n_rows / n_0));
    n_pool = n_trees * depth;
  }
  
  ~Mrpt() {
    for(int i = 0; i < n_trees; i++)
      delete trees[i];
    delete[] trees;
  }
  
  void grow() {
    trees = new RP_tree*[n_trees];
    random_matrix = randn(dim, n_pool);
    projected_data = X * random_matrix;
    
    for(int i = 0; i < n_trees; i++) {
      trees[i] = new RP_tree(projected_data, i, n_0);
      trees[i]->grow();
    }
  }
  
  
  std::unordered_set<int> query(const vec& q) {  
    mat projected_query = q * random_matrix;  // query vector q is passed as a reference to row vector
    std::unordered_set<int> idx_canditates;
    
    for(int i = 0; i < n_trees; i++) {
      uvec leaf_labels = trees[i]->get_leaf_labels();
      uword query_label = trees[i]->query(projected_query);
      uvec idx_one_tree =  leaf_labels(find(leaf_labels == query_label)); 
      idx_canditates.insert(idx_one_tree.begin(), idx_one_tree.end());
    }
    
    return idx_canditates;
    
    
    

  }
  
  RP_tree** get_trees() {
    return trees;
  }
  
  
private:
  mat X;        // data matrix, row = data point, col = variable
  int n_trees;  // number of RP-trees
  int n_0;      // maximum leaf size of all the RP-trees
  int n_rows;   // sample size of data
  int dim;      // dimension of data
  int depth;    // depth of an RP-tree with median split
  int n_pool;   // amount of random vectors needed for all the RP-trees
  mat random_matrix;    // random vectors needed for all the RP-trees
  mat projected_data;   // data matrix projected onto all the random vectors
  RP_tree** trees;      // all the RP-trees
  };



uvec knn(const vec& q, const mat& X, int k) {
  int n_rows = X.n_rows;
  int dim = X.n_cols;
  vec distances = vec(n_rows);
  for(int i = 0; i < n_rows; i++)
    distances[i] = sum(pow((X.row(i) - q), 2));

  uvec sorted_indices = sort_index(distances);
  return sorted_indices.subvec(1, k);
}


// [[Rcpp::export]]
Rcpp::List test(const mat& X, int n_trees, int n_0, bool print_tr, const mat& test_points) {
  Mrpt* mrpt = new Mrpt(X, n_trees, n_0);
  mrpt->grow();
  if(print_tr) for(int i = 0; i < n_trees; i++) RP_tree::print(mrpt->get_trees()[i]->get_tree());
  
  RP_tree* tree = mrpt->get_trees()[0];
  uvec clusters = tree->get_leaf_labels();
  int n_test = test_points.n_cols;
  uvec query_results(n_test);
  
  for(int i = 0; i < n_test; i++){
    std::cout << "i: " << i << std::endl;
    query_results[i] = tree->query(test_points.col(i).t());
  }

  Rcpp::List res = Rcpp::List::create(
    Rcpp::_["leaf_labels"] = tree->get_leaf_labels(), 
    Rcpp::_["query_results"] = query_results
  );

  delete tree;
  return res;
}


