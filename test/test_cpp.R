##################################################
# Random tree - test Rcpp version with MNIST data
# Ville Hyvönen
# HIIT
# ville.o.hyvonen@helsinki.fi 
# 14.10.2015
# RStudio with R 3.2.1 

# install.packages("Rcpp")
# install.packages("microbenchmark")
# install.packages("RcppArmadillo")

library(Rcpp)
library(microbenchmark)
library(RcppArmadillo)

source('../test_code/loadmnist.R')

# load mnist data
mnist <- loadmnist()
X_mnist <- mnist$X[1:(2^12+100), ]
x_idx <- 1:100
x <- X_mnist[x_idx,]
n_points <- nrow(x)
X_test <- X_mnist[-x_idx,]

# set parameters
n_0 <- 8

x <- X_test
# generate random matrix and compute projected data
n <- nrow(x)
dim <- ncol(x)
depth <- ceiling(log2(n/n_0))
n_pool <- depth  
set.seed(667)
random_matrix <- matrix(rnorm(n = dim * n_pool), nrow = dim)
projected_data <- x %*% random_matrix

sourceCpp('../source/tree.cpp')
test(projected_data, dim, n_0, 1, print_tr = F)



sourceCpp('../test/testia.cpp')
struct_test()

# test different ways of converting R matrix into arma::mat
microbenchmark(test_nm(X_mnist), test_arma(X_mnist), test_const_arma(X_mnist))

