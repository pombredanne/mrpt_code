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
#include <ctime>
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]


// find k nearest neighbors from data for the query point
// X = data matrix, row = data point, col = dimension
// q = query point as a row matrix
// k = number of neighbors searched for
// return : indices of nearest neighbors in data matrix X as a column vector
// [[Rcpp::export]]
uvec knnCpp_indices(const mat& X, const rowvec& q, uword k, uvec indices) {
  // std::cout << "q.head(5): " << q.head(5);
  int n_rows = indices.size();
  // std::cout << "n_rows: " << n_rows << std::endl;
  vec distances = vec(n_rows);
  for(int i = 0; i < n_rows; i++)
    distances[i] = sum(pow((X.row(indices(i)) - q), 2));
  
  uvec sorted_indices = indices(sort_index(distances));
  // std::cout << "sorted_indices:\n" << sorted_indices; 
  return sorted_indices.size() > k ? sorted_indices.head(k) + 1 : sorted_indices;
}


// find k nearest neighbors from data for the query point
// X = data matrix, row = data point, col = dimension
// q = query point as a row matrix
// k = number of neighbors searched for
// return : indices of nearest neighbors in data matrix X as a column vector
// [[Rcpp::export]]
uvec knnCpp(const mat& X, const rowvec& q, int k) {
  int n_rows = X.n_rows;
  vec distances = vec(n_rows);
  for(int i = 0; i < n_rows; i++)
    distances[i] = sum(pow((X.row(i) - q), 2));
  
  uvec sorted_indices = sort_index(distances);
  return sorted_indices.head(k) + 1;
}


// find k nearest neighbors from data for the query point
// X = *transposed* data matrix, row = dimension, col = data point
// q = query point as a column matrix
// k = number of neighbors searched for
// return : indices of nearest neighbors (cols of transposed data matrix X) as a column vector
// [[Rcpp::export]]
uvec knnCppT(const mat& X, const vec& q, int k) {
  int n_cols = X.n_cols;
  vec distances = vec(n_cols);
  for(int i = 0; i < n_cols; i++)
    distances[i] = sum(pow((X.col(i) - q), 2));
  
  uvec sorted_indices = sort_index(distances);
  return sorted_indices.subvec(0, k - 1) + 1;
}


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
  int query(const rowvec& projected_query) {
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
    trees = nullptr;
  }
  
  ~Mrpt() {
    if(trees)
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
  
  
  uvec query(const rowvec& q, int k) { 
    // std::cout << "q.head(5): "<< q.head(5);
    rowvec projected_query = q * random_matrix;  // query vector q is passed as a reference to row vector
    std::unordered_set<int> idx_canditates;
    // std::cout << "projected_query.head(5): "<< projected_query.head(5);
    
    
    for(int i = 0; i < n_trees; i++) {
      uvec leaf_labels = trees[i]->get_leaf_labels();
      uword query_label = trees[i]->query(projected_query);
      
      // std::cout << "leaf_labels.head(5):\n" << leaf_labels.head(5);
      // std::cout << "query_label: "<< query_label << std::endl;
      
      uvec idx_one_tree =  find(leaf_labels == query_label);
      // std::cout << "i: " << i << ", idx_one_tree:\n" << idx_one_tree << std::endl;
      idx_canditates.insert(idx_one_tree.begin(), idx_one_tree.end());
    }
    
    std::vector<int> idx_vector(idx_canditates.begin(), idx_canditates.end());
    // std::cout << "idx_canditates.size(): " << idx_canditates.size() << std::endl;
    // std::cout << "idx_vector.size(): " << idx_vector.size() << std::endl;
    return knnCpp_indices(X, q, k, conv_to<uvec>::from(idx_vector));
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

class Contour {
public:
  Contour(const mat& X_, uvec n_trees_, uvec n_0_) : X(X_), n_trees(n_trees_), n_0(n_0_) {
    n_mrpts = n_trees.size();
    mrpts = nullptr;
  }
  
  ~Contour() {
    if(mrpts)
      for(int i = 0; i < n_mrpts; i++)
        delete mrpts[i];
    delete[] mrpts;
  }
  
  void grow() {
    mrpts = new Mrpt*[n_mrpts];
    growing_times = std::vector<double>(n_mrpts);
    
    for(int i = 0; i < n_mrpts; i++) {
      std::cout << "n_trees: " << n_trees[i] << ", n_0: " << n_0[i] << std::endl;
      
      clock_t begin = clock();
      mrpts[i] = new Mrpt(X, n_trees[i], n_0[i]);
      mrpts[i]->grow();
      clock_t end = clock();
      
      growing_times[i] = (end - begin) / static_cast<double>(CLOCKS_PER_SEC);
     }
    
    std::cout << std::endl;
  }
  
  void query(const mat& Q, int k_, umat true_knn) {
    k = k_;
    n_points = Q.n_rows;
    nn_found = zeros<vec>(n_mrpts);
    // n_search_space = zeros<vec>(n_mrpts);
    std::vector<uvec> approximate_knn(n_points);
    times_query = std::vector<double>(n_mrpts);
    
    for(int i = 0; i < n_mrpts; i++) {
      Mrpt* mrpt = mrpts[i];
      
      clock_t begin = clock();
      for(int j = 0; j < n_points; j++) 
        approximate_knn[j] = mrpt->query(Q.row(j), k);
      clock_t end = clock();
      times_query[i] = (end - begin) / static_cast<double>(CLOCKS_PER_SEC);
      
      for(int j = 0; j < n_points; j++) {
        int n_knn = approximate_knn[j].size();
        
        for(int l = 0; l < n_knn; l++)
          if(any(true_knn.col(j) == approximate_knn[j][l]))
            nn_found[i]++;
      }
    }
    
    nn_found /= n_points;
    
  }
  
  
  Rcpp::List results(double time_exact) {
    Rcpp::List ret = Rcpp::List::create(
      Rcpp::_["nn_found"] = std::vector<double>(nn_found.begin(), nn_found.end()),
      Rcpp::_["k"] = k,
      Rcpp::_["n_trees"] = std::vector<int>(n_trees.begin(), n_trees.end()),
      Rcpp::_["n_0"] = std::vector<int>(n_0.begin(), n_0.end()),
      Rcpp::_["times_knn"] = std::vector<double>(n_mrpts, 0.0),
      Rcpp::_["times_query"] = times_query,
      Rcpp::_["growing_times"] = growing_times,
      Rcpp::_["time_exact"] = time_exact,
      Rcpp::_["n_points"] = n_points
    );
    ret.attr("class") = "contour"; 
    
    return ret;
  }
  
private:
  const mat& X;  // original data matrix
  int n_mrpts;   // number of Mrpt:s
  uvec n_trees;  // vector of number of trees
  uvec n_0;      // vector of maximum leaf sizes
  Mrpt** mrpts;  // all the Mrpt:s
  vec nn_found;
  // vec n_search_space;
  int k;
  std::vector<double> times_query;
  std::vector<double> growing_times;
  int n_points;
};


class Contours {
public:
  Contours(const mat& X_, int n_contours_, std::vector<uvec> n_trees_, std::vector<uvec> n_0_) :
  X(X_), n_contours(n_contours_), n_trees(n_trees_), n_0(n_0_) {
    contours = nullptr;
  }
  
  
  // min_S = log_2 of minimum search space size
  // max_S = log_2 of maximum search space size
  // min_leaf = smallest minimum leaf size n_0 used

  Contours(const mat& X_, int min_S, int max_S, int min_leaf) : X(X_) {
    contours = nullptr;
    n_contours = max_S - min_S + 1;
    n_trees = std::vector<uvec>(n_contours);
    n_0 = std::vector<uvec>(n_contours);
    for(int i = min_S; i <= max_S; i++) {
      uvec temp(i - min_leaf + 1);
      for(int j = 0; j <= i - min_leaf; j++)
        temp[j] = pow(2, j);
      n_trees[i - min_S] = temp; 
      
      int c = 0;
      for(int j = i; j >= min_leaf; j--) 
        temp[c++] = pow(2, j);
      n_0[i - min_S] = temp;
      
     // std::cout << "i: " << i << ", n_trees[i - min_S]:\n" << n_trees[i - min_S];
     // std::cout << "i: " << i << ", n_0[i - min_S]:\n" << n_0[i - min_S];
     
    }
  }
  
  ~Contours() {
    if(contours)
      for(int i = 0; i < n_contours; i++)
        delete contours[i];
    delete[] contours;
  }
  
  void grow() {
    contours = new Contour*[n_contours];
    for(int i = 0; i < n_contours; i++) {
      contours[i] = new Contour(X, n_trees[i], n_0[i]);
      contours[i]->grow();
    }
  }
  
  void query(const mat& Q, int k) {
    int n_points = Q.n_rows;
    umat true_knn(k, n_points);
    
    clock_t begin = clock();
    for(int i = 0; i < n_points; i++)
      true_knn.col(i) = knnCpp(X, Q.row(i), k);
    clock_t end = clock();
    time_exact = (end - begin) / static_cast<double>(CLOCKS_PER_SEC);
    
    for(int i = 0; i < n_contours; i++)
      contours[i]->query(Q, k, true_knn);
    }
  
  Rcpp::List results() {
    Rcpp::List ret(n_contours);
    
    for(int i = 0; i < n_contours; i++) 
      ret[i] = contours[i]->results(time_exact);
    
    ret.attr("class") = "mrpts";
    
    return ret;
  }
  
  
  
private:
  const mat& X;  // original data matrix
  int n_contours;
  std::vector<uvec> n_trees;  // vector of number of trees
  std::vector<uvec> n_0;      // vector of maximum leaf sizes
  Contour** contours;  // all the Contours
  double time_exact;   // time it takes to do exact knn search for n_points points
};



// [[Rcpp::export]]
Rcpp::List test(const mat& X, int n_trees, int n_0, const mat& test_points, int k) {
  Mrpt* mrpt = new Mrpt(X, n_trees, n_0);
  mrpt->grow();
  // if(print_tr) for(int i = 0; i < n_trees; i++) RP_tree::print(mrpt->get_trees()[i]->get_tree());
  
  int n_test = test_points.n_rows;
  umat query_results(k, n_test);
  
  for(int i = 0; i < n_test; i++){
    // std::cout << "i: " << i << std::endl;
    query_results.col(i) = mrpt->query(test_points.row(i), k);
  }
  
  Rcpp::List res = Rcpp::List::create(Rcpp::_["query_results"] = query_results);

  delete mrpt;
  mrpt = nullptr;
  return res;
}


// [[Rcpp::export]]
Rcpp::List test_contoursCpp(const mat& X, int min_S, int max_S, int min_leaf, const mat& Q, int k) {
  
  std::cout << "Nyt mennaan!" << std::endl;
  Contours* contours = new Contours(X, min_S, max_S, min_leaf);
  contours->grow();
  std::cout << "Puut rakennettu." << std::endl;
  
  contours->query(Q, k);
  std::cout << "Kysely tehty" << std::endl;
  
  Rcpp::List ret = contours->results();
  std::cout << "Tulokset haettu" << std::endl;
  
  delete contours;
  contours = nullptr;
  std::cout << "Puut poistettu." << std::endl;
  
  return ret;
}