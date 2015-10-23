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
test_points <- X_mnist[x_idx,]
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
projected_query <- test_points %*% random_matrix

sourceCpp('../source/tree.cpp')
res <- test(x, 2, n_0, print_tr = F, t(projected_query))
str(res)

sourceCpp('../test/testia.cpp')

a <- 1
while((a <- a + 1)  < 5)
  print(a)


noppa <- function(n) {
  dice7 <- 1:n
  while(length(dice7) > 1) {
    mask_dice7 <- rep(TRUE, length(dice7))
    for(i in 1:length(dice7))
      while((dice5 <- sample(1:5, 1)) != 3)
        if(dice5 < 3)
          mask_dice7[i] <- FALSE;
        if (sum(mask_dice7)) dice7 <- dice7[mask_dice7]    
  }
  dice7
}
        


table(replicate(1e4, noppa(7)))
table(sample(1:7, 1e4, replace = T))
