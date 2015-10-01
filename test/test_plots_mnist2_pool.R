##################################################
# Random tree - plots with MNIST data
# Ville Hyvönen
# HIIT
# ville.o.hyvonen@helsinki.fi 
# 7.7.2015
# implemented with RStudio using R version 3.2.0 

setwd('C:/HY-Data/HYVI/HIIT')

source('loadmnist.R')
source('test_code.R')
source('test_animation.R')

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
source('tree_matrix_same_pool.R')
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
load('./Results/news1024_16384_pooled_10')
tmt_old <- tmt
tmt <- tmt_aggregate


# plot knn found against #trees T with different search space sizes 
plot(tmt)
plot(tmt_old)

# plot knn found against log( # trees) with different search space sizes
plot(tmt, log=T)

# plot true size of search space against # trees with 2^(0:n_log_search_space) search space sizes
plot(tmt, search_space=T)
plot(tmt_old, search_space=T)
plot(tmt, search_space=T, log=T)

# plot running times against accuracy
plot(tmt, times=T, per_point = T, xlim=c(0, 3.5))
plot(tmt_old, times=T, per_point = T)
plot(tmt, times=T, per_point = T, xlim=c(0,11), exact=T)

# plot growing times against accuracy
plot(tmt, growing_times=T, xlim=c(0,2000))
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

##################################################


tmt[[5]]$n_pool <- 'normal'
tmt_pooled_8[[5]]$n_pool <- 8 
tmt_pooled_16[[5]]$n_pool <- 16
tmt_pooled_32[[5]]$n_pool <- 32
tmt_pooled_64[[5]]$n_pool <- 64
tmt_pooled_128[[5]]$n_pool <- 128
tmt_pooled_256[[5]]$n_pool <- 256 
tmt_pooled_512[[5]]$n_pool <- 512

tmt_pooled_025[[5]]$n_pool <- 0.025 
tmt_pooled_05[[5]]$n_pool <- 0.05
tmt_pooled_1[[5]]$n_pool <- 0.1
tmt_pooled_25[[5]]$n_pool <- 0.25
tmt_pooled_5[[5]]$n_pool <- 0.5
tmt_pooled_75[[5]]$n_pool <- 0.75 
tmt_pooled_10[[5]]$n_pool <- 1


 

tmt_comp <- compound(5, tmt, tmt_pooled_512, tmt_pooled_256,
                     tmt_pooled_128, tmt_pooled_64, tmt_pooled_32, tmt_pooled_16, tmt_pooled_8)

plot_compound(tmt_comp, main='S=16384, news data set')


tmt_comp_proportion <- compound(5, tmt, tmt_pooled_1, tmt_pooled_05, tmt_pooled_025)

plot_compound(tmt_comp_proportion, main='S=16384, news data set')

#############################################################
# Save image as pdf

pdf(file = './Fig/news_16384_pool.pdf', width = 9)
plot_compound(tmt_comp, main='S=16384, news data set')
dev.off()


#############################################################

