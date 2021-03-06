/*******************************************************
* Random projection tree - build trees & query in trees
* Ville Hyv�nen
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
uvec knnCpp_T_indices(const mat& X, const vec& q, uword k, uvec indices) {
  // std::cout << "q.head(5): " << q.head(5);
  int n_cols = indices.size();
  // std::cout << "n_rows: " << n_rows << std::endl;
  vec distances = vec(n_cols);
  for(int i = 0; i < n_cols; i++)
    distances[i] = sum(pow((X.col(indices(i)) - q), 2));
  
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

class Mrpt {
public:
  
  Mrpt(const mat& X_, int n_trees_, int n_0_) : X(X_), n_trees(n_trees_), n_0(n_0_) {
    n_rows = X.n_cols;  // X is transposed
    dim = X.n_rows;
    depth = ceil(log2(n_rows / n_0));
    n_pool = n_trees * depth;
    n_array = pow(2, depth + 1);
  }
  
  ~Mrpt() {
  }
  
  std::vector<double> grow() {
    trees = zeros<mat>(n_array, n_trees);
    leaf_labels = umat(n_rows, n_trees);
    std::vector<double> times(2);
    uvec indices = linspace<uvec>(0, n_rows - 1, n_rows);
    
    // generate the random matrix and project the data set onto it 
    clock_t begin = clock();
    random_matrix = randn(n_pool, dim);
    projected_data = random_matrix * X;
    // std::cout << "n_pool: " << n_pool << ", n_rows: " << n_rows << ", dim: " << dim << std::endl;
    // std::cout << "projected_data.n_rows: " << projected_data.n_rows << ", projected_data.n_cols: " << projected_data.n_cols << std::endl;
    clock_t end = clock();
    times[0] = (end - begin) / static_cast<double>(CLOCKS_PER_SEC);
    
    // grow the trees
    begin = clock();
    for(int n_tree = 0; n_tree < n_trees; n_tree++){
      first_idx = n_tree * depth;
      grow_subtree(indices, 0, 0, n_tree);  // all rows of data, 0 = level of the tree, 0 = first index in the array that stores the tree, n_tree:th tree
      
    } 
    end = clock();
    times[1] = (end - begin) / static_cast<double>(CLOCKS_PER_SEC);
    
    return times;
  }
  
  
  uvec query(const vec& q, int k) { 
    vec projected_query = random_matrix * q;   // query vector q is passed as a reference to a col vector
    std::vector<int> idx_canditates(n_trees * n_0);
    int j = 0;
    
    
    for(int n_tree = 0; n_tree < n_trees; n_tree++) {
      const uvec& col_leaf_labels = leaf_labels.unsafe_col(n_tree);
      const vec& tree = trees.unsafe_col(n_tree);
      
      double split_point = tree[0];
      int idx_left, idx_right;
      int idx_tree = 0;
      
      while(split_point) {
        idx_left = 2 * idx_tree + 1;
        idx_right = idx_left + 1;
        idx_tree = projected_query(j++) <= split_point ? idx_left : idx_right;   
        split_point = tree[idx_tree];
      }  
      
      uvec idx_one_tree =  find(col_leaf_labels == idx_tree);
      idx_canditates.insert(idx_canditates.begin(), idx_one_tree.begin(), idx_one_tree.end()) ;
    }
    
    auto last = std::unique(idx_canditates.begin(), idx_canditates.end());
    idx_canditates.erase(last, idx_canditates.end());
    
    return knnCpp_T_indices(X, q, k, conv_to<uvec>::from(idx_canditates));
  }
  
  uvec query_canditates(const vec& q, int k) { 
    vec projected_query = random_matrix * q;   // query vector q is passed as a reference to a col vector
    std::vector<int> idx_canditates(n_trees * n_0);
    int j = 0;
    
    // std::cout << "projected_query.size(): " << projected_query.size() << ", idx_canditates.size(): " << idx_canditates.size() << std::endl;
    for(int n_tree = 0; n_tree < n_trees; n_tree++) {
      // std::cout << "n_tree: " << n_tree << ", n_trees: " << n_trees << ", j: " << j << std::endl; 
      
      const uvec& col_leaf_labels = leaf_labels.unsafe_col(n_tree);
      const vec& tree = trees.unsafe_col(n_tree);
      
      // std::cout << "tree[0]: " << tree[0] << std::endl;
      
      double split_point = tree[0];
      int idx_left, idx_right;
      int idx_tree = 0;
      
      while(split_point) {
        idx_left = 2 * idx_tree + 1;
        idx_right = idx_left + 1;
        idx_tree = projected_query(j++) <= split_point ? idx_left : idx_right;   
        split_point = tree[idx_tree];
        // std::cout << "idx_left: " << idx_left << ", idx_right: " << idx_right << ", split_point: " << split_point << std::endl;
        // bool temp = split_point == 0;
        // std::cout << "split_point == 0: " <<  temp << std::endl; 
      }  
      
      uvec idx_one_tree =  find(col_leaf_labels == idx_tree);
      idx_canditates.insert(idx_canditates.begin(), idx_one_tree.begin(), idx_one_tree.end()) ;
    }
    
    auto last = std::unique(idx_canditates.begin(), idx_canditates.end());
    idx_canditates.erase(last, idx_canditates.end());
    return  conv_to<uvec>::from(idx_canditates);
  }

  
  void matrix_multiplication(const vec& q) { 
    vec projected_query = random_matrix * q;
  }
    
  
  
private:
  void grow_subtree(const uvec &indices, int tree_level, int i, uword n_tree) {
    int n = indices.size();
    int idx_left = 2 * i + 1;
    int idx_right = idx_left + 1; 
    
    if(n <= n_0) {
      uvec idx_tree = {n_tree};
      leaf_labels(indices, idx_tree) = zeros<uvec>(n) + i;
      return;
    }
    
    uvec level = {first_idx + tree_level};
    rowvec projection = projected_data(level, indices);
    uvec ordered = sort_index(projection);  // indices??
    
    int split_point = n % 2 ? n / 2 : n / 2 - 1;  // median split
    int idx_split_point = ordered(split_point);
    int idx_split_point2 = ordered(split_point + 1);
    
    trees(i, n_tree) = n % 2 ? projection(idx_split_point) : (projection(idx_split_point) + projection(idx_split_point2)) / 2;
    uvec left_indices = ordered.subvec(0, split_point);
    uvec right_indices = ordered.subvec(split_point + 1, n - 1);
    
    grow_subtree(indices.elem(left_indices), tree_level + 1, idx_left, n_tree);
    grow_subtree(indices.elem(right_indices), tree_level + 1, idx_right, n_tree);
    
  }
  
  
  mat X;        // data matrix, col = observation, row = dimension
  int n_trees;  // number of RP-trees
  int n_0;      // maximum leaf size of all the RP-trees
  int n_rows;   // sample size of data
  int dim;      // dimension of data
  int depth;    // depth of an RP-tree with median split
  int n_pool;   // amount of random vectors needed for all the RP-trees
  mat random_matrix;    // random vectors needed for all the RP-trees
  mat projected_data;   // data matrix projected onto all the random vectors
  mat trees;            // all the RP-trees, col = tree, row = node
  umat leaf_labels;     // leaf labels of all the data points, col = tree, row = data point
  int n_array;          // length of the one RP-tree as array
  uword first_idx;        // first col index of this tree in the projected_data
};



class Contour {
public:
  Contour(const mat& X_, uvec n_trees_, uvec n_0_) : X(X_), n_trees(n_trees_), n_0(n_0_) {
    n_mrpts = n_trees.size();
    mrpts = nullptr;
    times_matrix = std::vector<double>(n_mrpts);
    times_trees = std::vector<double>(n_mrpts);
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
    std::vector<double> times;
    
    for(int i = 0; i < n_mrpts; i++) {
      std::cout << "n_trees: " << n_trees[i] << ", n_0: " << n_0[i] << std::endl;
      
      clock_t begin = clock();
      mrpts[i] = new Mrpt(X, n_trees[i], n_0[i]);
      times = mrpts[i]->grow();
      clock_t end = clock();
      growing_times[i] = (end - begin) / static_cast<double>(CLOCKS_PER_SEC);
      times_matrix[i] = times[0];
      times_trees[i] = times[1];
     }
    
    std::cout << std::endl;
  }
  
  void query(const mat& Q, int k_, umat true_knn) {
    k = k_;
    n_points = Q.n_cols;
    nn_found = zeros<vec>(n_mrpts);
    // n_search_space = zeros<vec>(n_mrpts);
    std::vector<uvec> approximate_knn(n_points);
    std::vector<uvec> idx_canditates(n_points);
    times_query = std::vector<double>(n_mrpts);
    times_knn = std::vector<double>(n_mrpts);
    times_total = std::vector<double>(n_mrpts);
    times_multi = std::vector<double>(n_mrpts);
    
    for(int i = 0; i < n_mrpts; i++) {
      Mrpt* mrpt = mrpts[i];
      
      clock_t begin = clock();
      for(int j = 0; j < n_points; j++) 
        idx_canditates[j] = mrpt->query_canditates(Q.unsafe_col(j), k);
      clock_t end = clock();
      times_query[i] = (end - begin) / static_cast<double>(CLOCKS_PER_SEC);
      
      begin = clock();
      for(int j = 0; j < n_points; j++)
        approximate_knn[j] = knnCpp_T_indices(X, Q.unsafe_col(j), k, idx_canditates[j]);
      end = clock();
      times_knn[i] = (end - begin) / static_cast<double>(CLOCKS_PER_SEC);
      
      begin = clock();
      for(int j = 0; j < n_points; j++)
        mrpt->query(Q.unsafe_col(j), k);
      end = clock();
      times_total[i] = (end - begin) / static_cast<double>(CLOCKS_PER_SEC);
      
      begin = clock();
      for(int j = 0; j < n_points; j++)
        mrpt->matrix_multiplication(Q.unsafe_col(j));
      end = clock();
      times_multi[i] = (end - begin) / static_cast<double>(CLOCKS_PER_SEC);
      
      
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
      Rcpp::_["times_knn"] = times_knn,
      Rcpp::_["times_query"] = times_query,
      Rcpp::_["growing_times"] = growing_times,
      Rcpp::_["time_exact"] = time_exact,
      Rcpp::_["n_points"] = n_points,
      Rcpp::_["times_matrix"] = times_matrix,
      Rcpp::_["times_trees"] = times_trees,
      Rcpp::_["times_total"] = times_total,
      Rcpp::_["times_multi"] = times_multi
    );
    
    ret.attr("class") = "contour"; 
    
    return ret;
  }
  
private:
  const mat& X;  // original data matrix, col = observation, row = dimension
  int n_mrpts;   // number of Mrpt:s
  uvec n_trees;  // vector of number of trees
  uvec n_0;      // vector of maximum leaf sizes
  Mrpt** mrpts;  // all the Mrpt:s
  vec nn_found;  // average true nearest neighbors for all the test points 
  // vec n_search_space;
  int k;         // number of nearest neighbors searched for
  std::vector<double> times_query;    // query times in trees 
  std::vector<double> times_knn;      // knn times in a final search spaxe
  std::vector<double> times_total;    // total query times
  std::vector<double> growing_times;  // growing times for the trees
  int n_points;  // number of test points
  std::vector<double> times_matrix;
  std::vector<double> times_trees;
  std::vector<double> times_multi;
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
    int n_points = Q.n_cols;
    umat true_knn(k, n_points);
    
    std::cout << "Haetaan todelliset naapurit." << std::endl;
    
    clock_t begin = clock();
    for(int i = 0; i < n_points; i++)
      true_knn.col(i) = knnCppT(X, Q.col(i), k);
    clock_t end = clock();
    time_exact = (end - begin) / static_cast<double>(CLOCKS_PER_SEC);
    
    std::cout << "Todelliset " << k << " naapuria haettu" << std::endl;
    
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
  const mat& X;               // original data matrix, col = observation, row = dimension
  int n_contours;             // number of Contour objects in contours
  std::vector<uvec> n_trees;  // vector of number of trees
  std::vector<uvec> n_0;      // vector of maximum leaf sizes
  Contour** contours;         // all the Contours
  double time_exact;          // time it takes to do exact knn search for n_points points
};




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