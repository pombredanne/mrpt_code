##################################################
# Random tree - test code
# Ville Hyvönen
# HIIT
# ville.o.hyvonen@helsinki.fi 
# 4.6.2015
# implemented with RStudio using R version 3.2.0 

source('../source/knn.R')
source('../source/tree_matrix_same2.R')

# compute exact k nearest neighbours for test data points x from data set X_test
# returns matrix whose i:s row has k nearest neighbor for data point x[i,]
compute_exact_nn <- function(x, X_test, k) {
  exact_nn <- t(apply(x, 1, function(point) knn(X_test, point, k)))
  if(k==1) return(t(exact_nn)) else return(exact_nn)
}


# builds n_trees[1], ..., n_trees[n] trees with maximum leaf sizes n_0[1], ... , n_0[n] from data set X_test
# returns n-length list of multiple random projection trees (MRPT:s)
build_contour <- function(X_test, n_trees, n_0, save_vectors, n_pool) {
  res <- list()
  res$trees <- list()
  res$growing_times <- numeric()
  for(i in seq_along(n_trees)) {
    res$growing_times[i] <- system.time(res$trees[[i]] <- build_multiple_trees(X_test, n_stop = n_0[i], n_trees = n_trees[i], save_vectors, n_pool))['elapsed']
    cat('n_trees: ', n_trees[i], ', n_0: ', n_0[i], '\n', sep='')
  }
  cat('\n')
  res$n_trees <- n_trees
  res$n_0 <- n_0
  res$n_pool <- n_pool
  res
}

# build contours of mrpts with S_max = 2^min_trees, ... , 2^max_trees
# X_test = data set, rows are observations
# min_S = log_2 of minimum search space size
# max_S = log_2 of maximum search space size
# min_leaf = smallest minimum leaf size n_0 used
build_contours_power2 <- function(X_test, min_S, max_S, min_leaf = 0, save_vectors = TRUE, n_pool='full')
  lapply(min_S:max_S, function(i) build_contour(X_test, 2^(0:(i-min_leaf)), 2^(i:min_leaf), save_vectors, n_pool))


# make queries for test data points x[1,], ..., x[n_points, ] and calculate averages numbers of true knn found
# and average sizes of true search space
# compute also times it takes to make queries in T trees and search in a final search space S
test_contour <- function(x, X_test, contour, k, exact_nn, time_exact) {
  con_trees <- contour$trees
  n_con <- length(con_trees)
  n_trees <- contour$n_trees
  n_points <- nrow(x)
  nn_found <- n_search_space <- times_query <- times_knn <- numeric(n_con)
  
  for(j in seq_along(con_trees)) {
    mrpt <- con_trees[[j]]
    idx_neighbors <- nearest_neighbors <- vector('list', length=n_points)
    
    times_query[j] <- system.time(for(i in 1:n_points) {
      idx_neighbors[[i]] <- multiple_trees_query(x[i, ], mrpt)
      })['elapsed']
    
    times_knn[j] <- system.time(for(i in 1:n_points) {
      nearest_neighbors[[i]] <- knn(X_test, x[i, ], k, idx_neighbors[[i]])
    })['elapsed']
    
    for(i in 1:n_points) {
      nn_found[j] <- nn_found[j] + sum(nearest_neighbors[[i]] %in% exact_nn[i,])
      n_search_space[j] <- n_search_space[j] + length(idx_neighbors[[i]])
    }
     
  }    

  res <- list(nn_found = nn_found / n_points, n_search_space = n_search_space / n_points, k = k, n_trees = contour$n_trees, n_0 = contour$n_0,
       times_knn = times_knn, times_query = times_query, growing_times = contour$growing_times, time_exact = time_exact, n_points = n_points, n_pool = contour$n_pool)
  class(res) <- 'contour'
  res
} 

# make queries for test data points x[1,], ..., x[n_points, ] and calculate averages numbers of true knn found
# and average sizes of true search space
# compute also times it takes to make queries in T trees and search in a final search space S
test_contours <- function(x, X_test, contours, k) {
  time_exact <- system.time(exact_nn <- compute_exact_nn(x, X_test, k))['elapsed']
  res <- lapply(contours, function(contour) test_contour(x, X_test, contour, k, exact_nn, time_exact))
  class(res) <- 'mrpts'
  res
}


# make queries for test data points x[1,], ..., x[n_points, ] and calculate averages numbers of true knn found
# and average sizes of true search space
# compute also times it takes to make queries in T trees and search in a final search space S
# for single set of T RP-trees
test_mrpt <- function(x, X_test, mrpt, k, exact_nn) {
  n_trees <- length(mrpt)
  n_points <- nrow(x)
  nn_found <- n_search_space <- numeric(1)
  
    idx_neighbors <- nearest_neighbors <- vector('list', length=n_points)
    
    times_query <- system.time(for(i in 1:n_points) {
      x_clusters <- vapply(mrpt, function(rpt) tree_query(x[i, ], rpt), numeric(1))
      for(l in seq_along(mrpt))
        idx_neighbors[[i]] <- union(idx_neighbors[[i]], which(mrpt[[l]]$clusters == x_clusters[l]))
    })['elapsed']
    
    times_knn <- system.time(for(i in 1:n_points) {
      nearest_neighbors[[i]] <- knn(X_test, x[i, ], k, idx_neighbors[[i]])
    })['elapsed']
    
    for(i in 1:n_points) {
      nn_found <- nn_found + sum(nearest_neighbors[[i]] %in% exact_nn[i,])
      n_search_space <- n_search_space + length(idx_neighbors[[i]])
    }

  
  list(nn_found = nn_found / n_points, n_search_space = n_search_space / n_points, k = k, n_trees = n_trees, times_knn = times_knn,
       times_query = times_query, n_points = n_points, per_point = (times_query + times_knn) / n_points)
} 




