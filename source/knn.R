################################################
# Exact search for k nearest neighbours 
# Ville Hyvönen
# HIIT
# ville.o.hyvonen@helsinki.fi 
# 2.6.2015
# implemented with RStudio using R version 3.2.0

# distance matrix between data point x and all points in data
euclidean <- function(X, x) sqrt(colSums((t(X)-x)^2))

# search for k nearest neighbours for data point x from data
# returns indices of k nearest neighbours in the original data
# optimized version
knn <- function(X, x, k=1, idx_neighbors=NULL) {
  if(is.null(idx_neighbors)) {
    dist <- euclidean(X, x)
    return(order(dist)[1:k])
  } else {
    dist <- euclidean(X[idx_neighbors, ], x)
    if(length(idx_neighbors) >= k) idx_neighbors[order(dist)[1:k]] else idx_neighbors[order(dist)]
  }
}


# search for k nearest neighbours for data point x from data
# returns indices of k nearest neighbours in the original data
# old (slow) version
knn2 <- function(X, x, k=1, idx_neighbors=NULL) {
  idx_leaf <- if(is.null(idx_neighbors)) 1:nrow(X) else idx_neighbors
  dist <- euclidean(X[idx_leaf, ], x) 
  if(length(idx_leaf) >= k) idx_leaf[order(dist)[1:k]] else idx_leaf[order(dist)]
}

# search for k nearest neighbours for data point x from data
# returns indices of k nearest neighbours in the original data
# rpts = list of random projection trees
multiple_trees_knn <- function(X, x, rpts, k) {
  x_clusters <- sapply(rpts, function(rpt) tree_query(x, rpt))
  idx_neighbors <- NULL
  
  for(i in 1:length(rpts)) 
    idx_neighbors <- union(idx_neighbors, which(rpts[[i]]$clusters == x_clusters[i]))

  nn <- knn(X, x, k, idx_neighbors)
  
  return(list(nearest_neighbors=nn, n_search_space=length(idx_neighbors)))
}




