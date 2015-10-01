##################################################
# Random tree - animated plots with MNIST data
# Ville Hyvönen
# HIIT
# ville.o.hyvonen@helsinki.fi 
# 21.7.2015
# implemented with RStudio using R version 3.2.0 

source('../test_code/test_code_plot2.R')

library(animation)
ani.options(ffmpeg = 'C:\\HY-Data\\HYVI\\ffmpeg\\bin\\ffmpeg.exe')  # set path for video converter
ani.options(nmax=100)


# generic function for animating contour and mrpts objects
animate <- function(x, ...) UseMethod('animate')

# animate one contour 
animate.contour <- function(contour, interval=1, nmax=50, labels = FALSE, adj=c(-2,2), ...) {
  # ani.options(interval = interval, nmax = nmax)
  for (i in seq_along(contour$n_trees)) {
    plot(contour, idx=1:i, labels=labels, adj=adj, ...)
    ani.pause() ## pause for a while ('interval')
  }
}





animate.mrpts <- function(contours, interval=1, xlim=NULL, ylim=NULL, xlab=NULL, ylab=NULL, labels=FALSE, adj=c(-2,2), adj_legend=0, inset=0, ...) {
  X <- get_x(contours, ...)
  Y <- get_y(contours, ...)
  
  if(is.null(xlim)) xlim <- X$xlim
  if(is.null(ylim)) ylim <- Y$ylim
  if(is.null(xlab)) xlab <- X$xlab
  if(is.null(ylab)) ylab <- Y$ylab
  
  plot(NULL, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab)
  for(i in seq_along(contours)) {
    contour <- contours[[i]]
    S_max <- get_S_max(contours[1:i])
    for (j in seq_along(contour$n_trees)) {
      plot(NULL, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab)
      if(i > 1) 
        for(k in 1:(i-1)) plot(contours[[k]], col = k + 1, labels=labels, adj=adj, add=T, ...)
      plot(contour, idx=1:j, labels=labels, adj=adj, add=T, col=i+1, ...)
      legend("bottomright", legend=S_max, lwd=3, col=seq_along(contours)+1, title = expression(S[max]), adj=adj_legend, inset=inset)
      ani.pause() ## pause for a while ('interval')
    }
    
  }
  plot(NULL, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab)
  for(i in seq_along(contours)) plot(contours[[i]], labels=labels, col=i+1, add=T, ...)
  S_max <- get_S_max(contours)
  legend("bottomright", legend=S_max, lwd=3, col=seq_along(contours)+1, title = expression(S[max]))
  ani.pause()
}


animate_pdf <- function(contours, file, interval=1, xlim=NULL, ylim=NULL, xlab=NULL, ylab=NULL, labels=FALSE, adj=c(-2,2), adj_legend=0, inset=0, ...) {
  X <- get_x(contours, ...)
  Y <- get_y(contours, ...)
  
  on.exit(dev.off())  # close .pdf file written to on exit
  
  if(is.null(xlim)) xlim <- X$xlim
  if(is.null(ylim)) ylim <- Y$ylim
  if(is.null(xlab)) xlab <- X$xlab
  if(is.null(ylab)) ylab <- Y$ylab
  
  plot(NULL, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab)
  for(i in seq_along(contours)) {
    contour <- contours[[i]]
    S_max <- get_S_max(contours[1:i])
    for (j in seq_along(contour$n_trees)) {
      pdf(file = paste(paste(file, i, j, sep='_'), 'pdf', sep='.'), width = 9)
      plot(NULL, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab)
      if(i > 1) 
        for(k in 1:(i-1)) plot(contours[[k]], col = k + 1, labels=labels, adj=adj, add=T, ...)
      plot(contour, idx=1:j, labels=labels, adj=adj, add=T, col=i+1, ...)
     legend("bottomright", legend=S_max, lwd=3, col=seq_along(contours)+1, title = expression(S[max]), adj=adj_legend, inset=inset)
     dev.off()
    }
    
  }
}


animate_mrpts_old <- function(contours, interval=1, xlim=NULL, ylim=NULL, xlab=NULL, ylab=NULL, labels=FALSE, adj=c(-2,2), ...) {
  X <- get_x(contours, ...)
  Y <- get_y(contours, ...)
  
  if(is.null(xlim)) xlim <- X$xlim
  if(is.null(ylim)) ylim <- Y$ylim
  if(is.null(xlab)) xlab <- X$xlab
  if(is.null(ylab)) ylab <- Y$ylab
  
  plot(NULL, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab)
  for(i in seq_along(contours)) {
    S_max <- get_S_max(contours[1:i])
    legend("bottomright", legend=S_max, lwd=3, col=seq_along(contours)+1, title = expression(S[max]))
    if(i > 1) {
      plot(NULL, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab)
      for(j in 1:(i-1)) plot(contours[[j]], col = j + 1, labels=labels, adj=adj, add=T, ...)
    }
    animate(contours[[i]], interval = interval, col = i + 1, labels=labels, add=T, adj=adj, ...)
  }
      
}


