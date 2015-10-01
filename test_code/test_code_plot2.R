##################################################
# Random tree - build plots for test code
# Ville Hyvönen
# HIIT
# ville.o.hyvonen@helsinki.fi 
# 8.7.2015 
# implemented with RStudio using R version 3.2.0 

get_times <- function(contours, per_point, growing_times) {
  n <- contours[[1]]$n_points
  times <- if(growing_times) lapply(contours, function(contour) contour$growing_times) else lapply(contours, function(contour) contour$times_knn + contour$times_query)
  if(per_point && !growing_times) lapply(times, '/', n) else times
} 

  
get_n_trees <- function(contours, log) {
  n_trees <- lapply(contours, function(contour) contour$n_trees)
  if(log) lapply(n_trees, log, base=2) else n_trees
}

get_search_spaces <- function(contours) lapply(contours, function(contour) contour$n_search_space)

get_nn_found <- function(contours, prop) {
  k <- contours[[1]]$k
  nn_found <- lapply(contours, function(contour) contour$nn_found)
  if(prop) lapply(nn_found, '/', k) else nn_found
}

get_S_max <- function(contours) vapply(contours, function(contour) contour$n_trees[1] * contour$n_0[1], numeric(1))

get_times_exact <- function(contour, per_point) {
  n <- contour$n_points
  times_exact <- contour$time_exact
  if(per_point) lapply(times_exact, '/', n) else times_exact
}

get_x <- function(contours, times=F, per_point=F, log=F, growing_times=F, ...) {
  k <- contours[[1]]$k
  res <- list()
  
  if(times || growing_times) {
    n <- contours[[1]]$n_points
    res$times_exact <- get_times_exact(contours[[1]], per_point)
    res$x <- get_times(contours, per_point, growing_times)
    if(growing_times) {
      res$xlab <- 'Growing time (seconds) of trees' 
    } 
      else {
        res$xlab <- if(per_point) 'Running time (seconds) for 1 query point' else paste('Running time (seconds) for', n, 'query points.')
      }
  } else  {
    res$x <- get_n_trees(contours, log) 
    res$xlab <- if(log) 'log(T)' else 'Number of trees (T)'
  }
  
  res$xlim <- c(min(unlist(res$x)), max(unlist(res$x)))
  res
}


get_y <- function(contours, search_space=F, prop=T, ...) {
  k <- contours[[1]]$k
  res <- list()
  
  if(search_space == TRUE) {
    res$y <- get_search_spaces(contours)
    res$ylab <- 'Avg. size of the search space S'
    res$ylim <- c(0, max(unlist(res$y))) 
  } else {
    res$y <- get_nn_found(contours, prop)
    res$ylab <- if(prop == TRUE) paste('Avg. proportion of knn found (of', k, ')') else paste('Avg. number of knn found (of', k, ')')
    res$ylim <- if(prop == TRUE) c(0,1) else c(0,k)
  }
  res
}


# save plot produced by function f as pdf
# f = function producing the plot
# file = filename as string (remeber .pdf)
# width, width of the pdf file
save_pdf <- function(f, file, width = 9) {
  function(...) {
    pdf(file = file, width = width)
    on.exit(dev.off())
    f(...)
  }
}


# plot number of trees (x-axis) against knn found (y-axis) with different search space sizes (color) of mrpts
# contours = contours of mrpts 
# prop = plot proportion or number of knn found on y-axis
# times = plot times or number of knn found on y-axis
# growing = plot growing times
# search_space = plot size of the true search space S or knn found on y-axis
# log = log x-axis on log scale
# per_point = show time per one query point
# exact = draw vertical line at time of exact knn search
plot.mrpts <- function(contours, prop = TRUE, times = FALSE, growing_times = FALSE, search_space=FALSE, log = FALSE, per_point = FALSE, exact=TRUE, xlim = NULL, ylim = NULL,
                       type='b', lwd=3, inset = 0, adj_legend=0, file=NULL, pdf_width=9, ...) {
  
  if(!is.null(file)) {
    pdf(file = file, width = pdf_width)
    on.exit(dev.off())
  }
  
  X <- get_x(contours, times = times, per_point = per_point, log = log, growing_times = growing_times)
  if(is.null(xlim)) xlim <- X$xlim
  Y <- get_y(contours, search_space = search_space, prop = prop)
  if(is.null(ylim)) ylim <- Y$ylim
  
  S_max <- get_S_max(contours)
  
  plot(NULL, xlim=xlim, ylim=ylim, xlab=X$xlab, ylab=Y$ylab, ...)
  for(i in seq_along(contours))
    lines(X$x[[i]], Y$y[[i]], type=type, col = i + 1, lwd=lwd, ...)
  if(times && exact) abline(v=X$times_exact, col = 'blue', lty=2) 
  legend("bottomright", legend=S_max, lwd=lwd, col=seq_along(contours)+1, title = expression(S[max]), inset = inset, adj = adj_legend)
}  


# plot number of trees or query times (x-axis) against knn found or search set size (y-axis) for one contour (set of mrpt objects with fixed 
# maximum search set size S_max)
# contour = contour of mrpts with constant S_max
# prop = plot proportion or number of knn found on y-axis
# times = plot times or number of knn found on y-axis
# growing = plot growing times
# search_space = plot size of the true search space S or knn found on y-axis
# log = log x-axis on log scale
# per_point = show time per one query point
# exact = draw vertical line at time of exact knn search
# add = add to existing plot
# col = color of the curve
plot.contour <- function(contour, prop = TRUE, times = FALSE, growing_times = FALSE, search_space=FALSE, log = FALSE, per_point = FALSE, exact=TRUE, xlim = NULL,
                         ylim = NULL, add=FALSE, col = 2, type = 'b', lwd=3, idx=1:length(contour), labels=FALSE, adj = c(-2,2), ...) {
  k <- contour$k
  if(times == TRUE) {
    n <- contour$n_points
    times_exact <- get_times_exact(contour, per_point)
    x <- if(per_point) (contour$times_knn + contour$times_query) / n else contour$times_knn + contour$times_query
    xlab <- if(per_point) 'Running time (seconds) for 1 query point' else paste('Running time (seconds) for', n, 'query points.')
  } else if(growing_times)  { 
    x <- contour$growing_times
    xlab <- 'Growing time (seconds) of trees'
  } else {
    x <- if(log) log(contour$n_trees, base=2) else contour$n_trees
    xlab <- if(log) 'log(T)' else 'Number of trees (T)'
  }
  if(is.null(xlim)) xlim <- c(min(x), max(x))
  
  if(search_space == TRUE) {
    y <- contour$n_search_space
    ylab <- 'Avg. size of the search space S'
    if(is.null(ylim)) ylim <- c(0, max(y))
  } else {
    y <- if(prop == TRUE) contour$nn_found / k else contour$nn_found
    ylab <- if(prop == TRUE) paste('Avg. proportion of knn found (of', k, ')') else paste('Avg. number of knn found (of', k, ')')
    if(is.null(ylim)) ylim <- if(prop == TRUE) c(0,1) else c(0,k)
  } 
  
  if(!add) {
    plot(x[idx], y[idx], type=type, col=col, lwd=lwd, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, ...)
  } else {
    lines(x[idx], y[idx], type=type, col=col, lwd=lwd, ...)
  }
  
  if(labels) text(x[idx], y[idx], labels=contour$n_trees[idx], adj = adj, col=col)
  if(times && exact) abline(v=times_exact, col = 'blue', lty=2) 
}  


##########################################################################
# stuff for plotting same contours with different random vector pool sizes

# compound same contours from different mrpts objects
compound <- function(idx, ...) {
  tmts <- list(...)
  lapply(tmts, function(tmt) tmt[[idx]])
}

get_n_pools <- function(compound) sapply(compound, function(contour) contour$n_pool)

# plot compound of contours
plot_compound <- function(tmt_compound, lwd = 3, inset = 0, adj_legend = 0, ...) {
  tmt_length <- length(tmt_compound)
  n_pools <- get_n_pools(tmt_compound)
  
  plot(tmt_compound[[1]], col=2, ...)
  if(tmt_length > 1)
    for(i in 2:tmt_length) 
      plot(tmt_compound[[i]], col = i+1, add=T, ...)
  legend("bottomright", legend=n_pools, lwd=lwd, col=seq_along(tmt_compound)+1, title = 'L', inset = inset, adj = adj_legend)
  
}






# plot.mrpts <- function(tmt, prop = TRUE, times=FALSE, search_space=FALSE, log = FALSE, per_point = FALSE, exact=TRUE, xlim = NULL, ylim = NULL) {
#   idx_last <- length(tmt)
#   plot(tmt[[idx_last]], prop=prop, times=times, search_space=search_space, log=log, per_point=per_point, exact=exact, xlim=xlim, ylim=ylim, add=FALSE, col=idx_last+1)
#   if(idx_last > 1)
#     for(i in 1:idx_last) 
#       plot(tmt[[i]], prop=prop, times=times, search_space=search_space, log=log, per_point=per_point, exact=exact, add=TRUE, col = i + 1)
#   
#   S_max <- vapply(tmt, function(contour) contour$n_trees[1] * contour$n_0[1], numeric(1))
#   legend("bottomright", legend=S_max, lwd=2, col=seq_along(tmt)+1, title = expression(S[max]))
# } 
