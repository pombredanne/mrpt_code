##################################################
# Random tree - plots with MNIST data
# Ville Hyvönen
# HIIT
# ville.o.hyvonen@helsinki.fi 
# 7.7.2015
# implemented with RStudio using R version 3.2.0 

source('../test_code/loadmnist.R')
source('../test_code/test_code.R')
source('../test_code/test_code_plot2.R')

# load mnist data
mnist <- loadmnist()
X_mnist <- mnist$X[1:(2^12+100), ]
x_idx <- 1:100
x <- X_mnist[x_idx,]
n_points <- nrow(x)
X_test <- X_mnist[-x_idx,]

# set parameters
set.seed(667)

k <- 8
max_S <- 9


# build contours of trees for data set X_test
system.time(rp3 <- build_contours_power2(X_test, min_S=3, max_S=max_S, min_leaf=3))

# make queries for x test points
system.time(tmt_old <- test_contours(x, X_test, rp3, k))


# build contours of trees for data set X_test
source('../alt_source/tree_matrix_same_pool.R')
system.time(rp4 <- build_contours_power2(X_test, min_S=3, max_S=max_S, min_leaf=3, n_pool=32))

# make queries for x test points
system.time(tmt32 <- test_contours(x, X_test, rp4, k))

# build contours of trees for data set X_test
source('tree_matrix_same_pool_PCA.R')
system.time(rp <- build_contours_power2(X_test, min_S=3, max_S=max_S, min_leaf=3, n_pool=.05))

# make queries for x test points
system.time(tmt_PCA <- test_contours(x, X_test, rp, k))
tmt_old <- tmt
tmt <- tmt_PCA


# build contours of trees for data set X_test
source('tree_matrix_same2.R')
system.time(rp_old <- build_contours_power2(X_test, min_S=3, max_S=max_S, min_leaf=3))

# make queries for x test points
system.time(tmt_old <- test_contours(x, X_test, rp_old, k))



# aggregate_contours('news_1024_16384_same', n_sim=5)
load('./Results/news_1024_16384_pooled_128')
tmt_old <- tmt
tmt <- tmt_pooled_8


# plot knn found against #trees T with different search space sizes 
plot(tmt)
plot(tmt_old)

str(rp4[[2]])

# plot knn found against log( # trees) with different search space sizes
plot(tmt, log=T)

# plot true size of search space against # trees with 2^(0:n_log_search_space) search space sizes
plot(tmt, search_space=T)
plot(tmt_old, search_space=T)
plot(tmt, search_space=T, log=T)

# plot running times against accuracy
plot(tmt, times=T, per_point = T)
plot(tmt_old, times=T, per_point = T)
plot(tmt, times=T, per_point = T, xlim=c(0,11), exact=T)

# plot growing times against accuracy
plot(tmt, growing_times=T, xlim=c(0,2500))
plot(tmt_old, growing_times=T)

# compare accuracy of the original and the same projection version
for(i in 1:length(tmt)) {
  cat("orig: ", tmt_old[[i]]$nn_found, '\n')
  cat("same: ", tmt[[i]]$nn_found, '\n')
}

# ratio of query times of the original and the same projection version
for(i in 1:length(tmt)) 
  cat("ratio (old/new): ", (tmt_old[[i]]$times_query + tmt_old[[i]]$times_knn) / (tmt[[i]]$times_query + tmt[[i]]$times_knn), '\n')


# ratio of growing times of the original and the same projection version
for(i in 1:length(tmt)) 
  cat("ratio (old/new): ", tmt_old[[i]]$growing_times / tmt[[i]]$growing_times, '\n')




# compare query times of the original and the same projection version
for(i in 1:length(tmt)) {
  cat("orig: ", (tmt_old[[i]]$times_query + tmt_old[[i]]$times_knn) / tmt_old[[1]]$n_points, '\n')
  cat("same: ", (tmt[[i]]$times_query + tmt[[i]]$times_knn) / tmt[[1]]$n_points, '\n')
}


# compare tree traversal times of the original and the same projection version
for(i in 1:length(tmt)) {
  cat("orig: ", tmt_old[[i]]$times_query, '\n')
  cat("same: ", tmt[[i]]$times_query, '\n')
}

# compare growing times of the original and the same projection version
for(i in 1:length(tmt)) {
  cat("orig: ", tmt_old[[i]]$growing_times, '\n')
  cat("same: ", tmt[[i]]$growing_times, '\n')
}

tmt_comp <- compound(7, tmt4, tmt8, tmt16, tmt32)

tmt_comp[[2]]$n_pool <- 10

plot_compound(tmt_comp)



#############################################################
# Save image as pdf

setwd('C:\\HY-Data\\HYVI\\HIIT\\Fig')
pdf(file = 'news_1024_16384_search_spaces.pdf', width = 9)
plot(tmt, search_space = T, main='Size of the pool 2.5%')
plot(tmt_old, search_space = T, main='Original')
dev.off()


#############################################################



#############################################################
# animation stuff
# take partial data set without first contour
tmt_part <- tmt[2:5]
class(tmt_part) <- 'mrpts'

# plot running time against averages number of neighbors found with partial data set
plot(tmt_part, times=T, per_point = T)
plot(tmt, times=T, per_point = T, xlim=c(0,.1), exact=T)
plot(tmt_old, times=T, per_point = T, xlim=c(0,.1), exact=T)
plot(tmt, times=T, per_point = T, xlim=c(0,15), exact=T)

# save figure of running times
# pdf(file = 'news_1024_16384_sparse5.pdf', width = 9)
plot(tmt_part, times=T, per_point=T, lwd=2)
# dev.off()

animate(tmt, times=T, per_point=T, lwd=2)

############################################################
# some microbenchmarking stuff...

library(microbenchmark)
n <- 1e6
rand <- matrix(rnorm(n), nrow = 1000)
r <- rand[1:10, ]
microbenchmark(rand[1,])


str(rp4[[3]])
str(rp4[[5]][[1]][[1]][[1]]$n_pool)
