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
source('../source/knn.R')
source('../test_code/test_code.R')
source('../test_code/test_code_plot2.R')


# load mnist data
mnist <- loadmnist()
X_mnist <- mnist$X[1:(2^12+100), ]
x_idx <- 1:100
x <- X_mnist[x_idx,]
X_test <- X_mnist[-x_idx,]

# set parameters
n_0 <- 8
k <- 8
set.seed(667)
min_S <- 3
max_S <- 9
min_leaf <- 3

# build contours of trees for data set X_test
system.time(rp3 <- build_contours_power2(X_test, min_S=min_S, max_S=max_S, min_leaf=min_leaf))

# make queries for x test points
system.time(tmt_old <- test_contours(x, X_test, rp3, k))

# test C++ version
sourceCpp('../source/tree.cpp')
test_contoursCpp(X_test, min_S, max_S, min_leaf)



res <- test(X_test, 16, n_0, test_points, k)
res




# plot knn found against #trees T with different search space sizes 
plot(tmt)
plot(tmt_old)



# knn-searchin nopeuden testailua
X_test_t <- t(X_test)
x_1_t <- t(test_points[1,])
knn(X_test, test_points[1, ], k = 8)
knnCpp(X_test, test_points[1, ], k = 8)
knnCppT(X_test_t, x_1_t, k = 8)

# almost 4 times faster to go through the matrix by cols than rows!
microbenchmark(knn(X_test, test_points[1, ], k = 8), knnCpp(X_test, test_points[1, ], k = 8), knnCppT(X_test_t, x_1_t, k = 8)) 



sourceCpp('../test/testia.cpp')

