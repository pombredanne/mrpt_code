##################################################
# Random projection tree - build trees & query in trees
# Ville Hyvönen
# HIIT
# ville.o.hyvonen@helsinki.fi 
# 10.9.2015
# implemented with RStudio using R version 3.2.0. 

# version where on one level of the tree same projection is used at each branch

# tree structure is implemented as 3-column matrix, where:
# 1. column: value of the split point in a projected space
# 2. column: index of a left child node
# 3. column: index of a right child node

# initial matrix has 128 rows, and this is doubled at every allocation

if(!exists('POSSIBLE_SEEDS')) POSSIBLE_SEEDS <- 1:10e6

# build a random projection tree
# data = data set from which nearest neighbors are searched
# n_stop = maximum leaf size, building of branch is stopped, when it has <= n_stop points
# split_type = not implemented yet
build_tree <- function(projected_data, dim, n_stop, n_tree) {
  n_sim <- nrow(projected_data)
  depth <- ceiling(log2(n_sim/n_stop))
  first_idx <- (n_tree - 1) * depth + 1
  random_indices <- first_idx:(first_idx + depth - 1)
  
  # recursive function to build tree
  build_subtree <- function(idx_org, index, tree_level) {
    force(index)
    idx_next <<- idx_next + 1
    if(idx_next > nrow(tree_cutpoint)) # double size of the matrix allocated for tree
      tree_cutpoint <<- rbind(tree_cutpoint, matrix(numeric(3 * nrow(tree_cutpoint)), ncol=3))
    
    n_points <- length(idx_org)
    if(n_points <= n_stop) {
      clusters[idx_org] <<- index 
      return()
    } 
    
    projection <- projected_data[idx_org, random_indices[tree_level]]
    
    ordered <- order(projection)
    cutpoint <- if(n_points %% 2 == 0) n_points/2 else ceiling(n_points/2)
    idx_cutpoint <- if(n_points %% 2 == 0) ordered[c(cutpoint, cutpoint+1)] else ordered[cutpoint]
    data_cutpoint <- mean(projection[idx_cutpoint])
    tree_cutpoint[index, 1] <<- data_cutpoint
    idx_left <- ordered[1:cutpoint]
    idx_right <- ordered[(cutpoint+1):n_points]
    
    tree_cutpoint[index, 2] <<- idx_next
    build_subtree(idx_org[idx_left], idx_next, tree_level = tree_level + 1) # build left subtree 
    tree_cutpoint[index, 3] <<- idx_next
    build_subtree(idx_org[idx_right], idx_next, tree_level = tree_level + 1) # build right subtree
    return(index)
  }
  
  # init global variables
  idx_next <- 1
  init_depth <- 7
  clusters <- numeric(n_sim)
  tree_cutpoint <- matrix(numeric(3 * 2^init_depth), ncol=3)
  
  build_subtree(1:n_sim, index=1, tree_level=1)
  return(list(tree=tree_cutpoint, depth=depth, dim=dim, clusters=clusters, random_indices = random_indices))
}

# build n_trees random projection trees 
build_multiple_trees <- function(data, n_stop, n_trees=1, save_vectors, n_pool=NULL) {
  seed <- sample(POSSIBLE_SEEDS, size=1)
  set.seed(seed)
  dim <- ncol(data)
  n <- nrow(data)
  depth <- ceiling(log2(n/n_stop))
  n_pool <- n_trees * depth
  trees <- vector('list', length = n_trees)
  
  # generate pool of random vectors used by all of the trees and project the data set into all of them.
  random_matrix <- matrix(rnorm(n = dim * n_pool), nrow = dim)
  projected_data <- data %*% random_matrix
  
  for(i in 1:n_trees) 
    trees[[i]] <- build_tree(projected_data, dim, n_stop, n_tree = i)
  
  if(save_vectors) {
    trees[[1]]$random_matrix <- random_matrix
  } else {
    trees[[1]]$seed <- seed
  } 
  trees[[1]]$n_pool <- n_pool
  trees
}

# Rout the query point down in all T trees, 
# and find all the points in all the leaves the query point ended up to / the final search set
# query = query point 
# rpts = multiple random projection trees object
# returns indices of the points in a final search set
multiple_trees_query <- function(query, rpts) {
  
  # function to search for leaf which the query point falls into in one RP-tree
  # rpt = RP-tree
  # returns the leaf label of the leaf the query point is routed into
  tree_query <- function(rpt) {
    tree <- rpt$tree
    projected_query <- projection_pool[rpt$random_indices]
    
    i <- tree_level <- 1
    while(tree[i,2] != 0) {
      projection <- projected_query[tree_level]
      if(projection <= tree[i,1]) {
        i <- tree[i,2]
      } else {
        i <- tree[i,3]
      }
      tree_level <- tree_level + 1
    }
    return(i)
  }
  
  # create a pool of projections by projecting the query point into a random matrix
  if(!is.null(rpts[[1]]$random_matrix)) {
    random_matrix <- rpts[[1]]$random_matrix
  } else {
    set.seed(rpts[[1]]$seed)
    random_matrix <- matrix(rnorm(n=rpts[[1]]$dim * rpts[[1]]$n_pool), nrow=rpts[[1]]$dim)
  }
  projection_pool <- as.vector(query %*% random_matrix)
  
  # find for each of a tree a leaf the query point falls into
  leaf_labels <- sapply(rpts, function(rpt) tree_query(rpt))
  
  # collect all the points in all the leaves the query point ended up to / the final search set 
  idx_neighbors <- NULL
  for(i in 1:length(rpts)) 
    idx_neighbors <- union(idx_neighbors, which(rpts[[i]]$clusters == leaf_labels[i]))
  
  idx_neighbors
}  

