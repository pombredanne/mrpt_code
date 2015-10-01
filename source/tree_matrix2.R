##################################################
# Random projection tree - build trees & query in trees
# Ville Hyvönen
# HIIT
# ville.o.hyvonen@helsinki.fi 
# 21.7.2015
# implemented with RStudio using R version 3.2.0. 

# tree structure is implemented as 3-column matrix, where:
# 1. column: value of the split point in a projected space
# 2. column: index of a left child node
# 3. column: index of a right child node

# initial matrix has 128 rows, and this is doubled at every allocation

source('knn.R')
if(!exists('POSSIBLE_SEEDS')) POSSIBLE_SEEDS <- 1:10e6
if(!exists('PLUS_SEED')) PLUS_SEED <- c(353,863)


# build a random projection tree
# data = data set from which nearest neighbors are searched
# n_stop = maximum leaf size, building of branch is stopped, when it has <= n_stop points
# split_type = not implemented yet
build_tree <- function(data, n_stop, split_type='median') {
  seed <- sample(POSSIBLE_SEEDS, size=1)
  n_sim <- nrow(data)
  dim <- ncol(data)
  
  # init global variables
  tree_cutpoint <- numeric()
  clusters <- numeric(n_sim)
  
  # recursive function to build tree
  build_subtree <- function(X, idx_org, index, seed) {
    force(index)
    idx_next <<- idx_next + 1
    if(idx_next > nrow(tree_cutpoint)) # double size of the matrix allocated for tree
      tree_cutpoint <<- rbind(tree_cutpoint, matrix(numeric(3 * nrow(tree_cutpoint)), ncol=3))
    
    n_points <- nrow(X)
    if(n_points <= n_stop) {
      clusters[idx_org] <<- index 
      return()
    } 
    
    set.seed(seed)
    random_vector <- rnorm(n=dim)
    projection <- X %*% random_vector
    
    ordered <- order(projection)
    cutpoint <- if(n_points %% 2 == 0) n_points/2 else ceiling(n_points/2)
    idx_cutpoint <- if(n_points %% 2 == 0) ordered[c(cutpoint, cutpoint+1)] else ordered[cutpoint]
    data_cutpoint <- mean(projection[idx_cutpoint])
    tree_cutpoint[index, 1] <<- data_cutpoint
    idx_left <- ordered[1:cutpoint]
    idx_right <- ordered[(cutpoint+1):n_points]
    
    tree_cutpoint[index, 2] <<- idx_next
    build_subtree(X[idx_left,], idx_org[idx_left], idx_next, seed + PLUS_SEED[1]) # build left subtree 
    tree_cutpoint[index, 3] <<- idx_next
    build_subtree(X[idx_right,], idx_org[idx_right], idx_next, seed + PLUS_SEED[2]) # build right subtree
    return(index)
  }
  
  idx_next <- 1
  init_depth <- 7
  tree_cutpoint <- matrix(numeric(3 * 2^init_depth), ncol=3)

  build_subtree(data, 1:n_sim, index=1, seed=seed)
  return(list(seed=seed, tree=tree_cutpoint, clusters=clusters))
}

# build n_trees random projection trees 
build_multiple_trees <- function(data, n_stop, n_trees=1) replicate(n_trees, build_tree(data, n_stop), simplify=F)


# funtion to search for leaf which point x falls into 
tree_query <- function(x, rpt) {
  tree <- rpt$tree
  seed <- rpt$seed
  i <- 1
  while(tree[i,2] != 0) {
    set.seed(seed)
    random_vector <- rnorm(n = length(x)) 
    projection <- random_vector %*% x
    if(projection <= tree[i,1]) {
      i <- tree[i,2]
      seed <- seed + PLUS_SEED[1]
    } else {
      i <- tree[i,3]
      seed <- seed + PLUS_SEED[2]
    }
  }
  return(i)
}





