##################################################
# Random tree - test and visualize Rcpp version with 2D data
# Ville Hyvönen
# HIIT
# ville.o.hyvonen@helsinki.fi 
# 23.10.2015
# RStudio with R 3.2.2 

# install.packages("Rcpp")
# install.packages("microbenchmark")
# install.packages("RcppArmadillo")

library(Rcpp)
library(microbenchmark)
library(RcppArmadillo)

source('../test_code/visualize_tree.R')

# create 2Dtest data
set.seed(666)
n_sim <- 32
x1 <- runif(n_sim)
x2 <- runif(n_sim)
x <- cbind(x1,x2)
n_test <- 10
test_points <- matrix(runif(2 * n_test), ncol=2)

# generate random matrix and compute projected data
n_0 <- 5
n <- nrow(x)
dim <- ncol(x)
depth <- ceiling(log2(n/n_0))
n_pool <- depth  
random_matrix <- matrix(rnorm(n = dim * n_pool), nrow = dim)
projected_data <- x %*% random_matrix
projected_query <- test_points %*% random_matrix

# source Rcpp version, grow RP-tree and visualize results
sourceCpp('../source/tree.cpp')
res <- test(x, 2, n_0, print_tr = T, t(projected_query))
visualise_clusters(x, res$leaf_labels)
visualise_clusters(test_points, res$query_results, cex = 1.5, add = T, old_clusters =  res$leaf_labels)








