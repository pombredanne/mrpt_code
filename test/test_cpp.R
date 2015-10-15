##################################################
# Random tree - plots with MNIST data
# Ville Hyvönen
# HIIT
# ville.o.hyvonen@helsinki.fi 
# 7.7.2015
# implemented with RStudio using R version 3.2.0 

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

sourceCpp('../source/tree.cpp')
a <- buildTree(x, n_0, 2)

