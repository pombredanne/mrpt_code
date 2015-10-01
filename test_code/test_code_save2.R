##################################################
# Random tree - test code
# Saves every multiple RP-trees (mrpt) object into a separate file to save memory
# Ville Hyvönen
# HIIT
# ville.o.hyvonen@helsinki.fi 
# 11.7.2015
# implemented with RStudio using R version 3.2.0 

source('tree_matrix2.R')
source('knn.R')

# compute exact k nearest neighbours for test data points x from data set X_test
# returns matrix whose i:s row has k nearest neighbor for data point x[i,]
compute_exact_nn <- function(x, X_test, k) {
  exact_nn <- t(apply(x, 1, function(point) knn(X_test, point, k)))
  if(k==1) return(t(exact_nn)) else return(exact_nn)
}


# builds n_trees[1], ..., n_trees[n] trees with maximum leaf sizes n_0[1], ... , n_0[n] from data set X_test
# returns n-length list of multiple random projection trees (MRPT:s)
build_contour <- function(X_test, n_trees, n_0, idx_contour, id, save_vectors) {
  contour <- list()
  contour$growing_times <- numeric()
  
  for(i in seq_along(n_trees)) {
    contour$growing_times[i] <- system.time(mrpt <- build_multiple_trees(X_test, n_stop = n_0[i], n_trees = n_trees[i], save_vectors))['elapsed']
    save(mrpt, file=paste(id, 'trees', idx_contour, i, sep='_'))
    cat('n_trees: ', n_trees[i], ', n_0: ', n_0[i], '\n', sep='')
  }
  rm(mrpt)
  cat('\n')
  
  contour$n_trees <- n_trees
  contour$n_0 <- n_0
  contour$id <- id
  contour
}


# build contours of mrpts with S_max = 2^min_trees, ... , 2^max_trees
# X_test = data set, rows are observations
# min_trees = log_2 of minimum search space size
# max_trees = log_2 of maximum search space size
# min_leaf = smallest minimum leaf size n_0 used
build_contours_power2 <- function(X_test, min_S, max_S, min_leaf, id, save_vectors = TRUE)
  lapply(min_S:max_S, function(i) build_contour(X_test, 2^(0:(i-min_leaf)), 2^(i:min_leaf), i - min_S + 1, id, save_vectors))


# make queries for test data points x[1,], ..., x[n_points, ] and calculate averages numbers of true knn found
# and average sizes of true search space
# compute also times it takes to make queries in T trees and search in a final search space S
test_contour <- function(x, X_test, contour, k, exact_nn, time_exact, idx_contour) {
  # con_trees <- contour$trees
  n_trees <- contour$n_trees
  n_con <- length(n_trees)
  n_points <- nrow(x)
  nn_found <- n_search_space <- times_query <- times_knn <- numeric(n_con)
  
  for(j in seq_along(n_trees)) {
    cat('n_trees:', n_trees[j], 'n_0:', contour$n_0[j], '\n')
    load(paste(contour$id, 'trees', idx_contour, j, sep='_'))  # load multiple RP-trees object created by build_contour() and saved as 'mrpt'
    idx_neighbors <- nearest_neighbors <- vector('list', length=n_points)
    
    times_query[j] <- system.time(for(i in 1:n_points) {
      x_clusters <- vapply(mrpt, function(rpt) tree_query(x[i, ], rpt), numeric(1))
      for(l in seq_along(mrpt))
        idx_neighbors[[i]] <- union(idx_neighbors[[i]], which(mrpt[[l]]$clusters == x_clusters[l]))
      })['elapsed']
    
    times_knn[j] <- system.time(for(i in 1:n_points) {
      nearest_neighbors[[i]] <- knn(X_test, x[i, ], k, idx_neighbors[[i]])
    })['elapsed']
    
    for(i in 1:n_points) {
      nn_found[j] <- nn_found[j] + sum(nearest_neighbors[[i]] %in% exact_nn[i,])
      n_search_space[j] <- n_search_space[j] + length(idx_neighbors[[i]])
    }
    
    rm(mrpt)
     
  }
  
  cat('\n')
  
  res <- list(nn_found = nn_found / n_points, n_search_space = n_search_space / n_points, k = k, n_trees = contour$n_trees, n_0 = contour$n_0,
       times_knn = times_knn, times_query = times_query, growing_times = contour$growing_times, time_exact = time_exact, n_points = n_points)
  class(res) <- 'contour'
  res
} 

# make queries for test data points x[1,], ..., x[n_points, ] and calculate averages numbers of true knn found
# and average sizes of true search space
# compute also times it takes to make queries in T trees and search in a final search space S
test_contours <- function(x, X_test, contours, k) {
  time_exact <- system.time(exact_nn <- compute_exact_nn(x, X_test, k))['elapsed']
  res <- lapply(seq_along(contours), function(i) test_contour(x, X_test, contours[[i]], k, exact_nn, time_exact, i))
  class(res) <- 'mrpts'
  res
}

aggregate_contours <- function(id, n_sim, result_name = 'tmt') {
  load(paste(id, 1, sep='_'))
  result <- get(result_name)
  for(i in 2:n_sim) {
    load(paste(id, i, sep='_'))
    current <- get(result_name)
    for(j in seq_along(result))
      for(k in seq_along(result[[j]]))
        result[[j]][[k]] <- result[[j]][[k]] + current[[j]][[k]]
  }
  tmt_aggregate <- lapply(result, function(contour) lapply(contour, function(field) field / n_sim))
  
  for(i in 1:length(tmt_aggregate)) class(tmt_aggregate[[i]]) <- 'contour'
  class(tmt_aggregate) <- 'mrpts'
  save(tmt_aggregate, file=paste(id, 'aggregate', n_sim, sep='_'))
}



