##################################################
# Visualize RP_tree with 2D data
# Ville Hyvönen
# HIIT
# ville.o.hyvonen@helsinki.fi 
# 23.10.2015
# RStudio with R 3.2.2 


# function to visualise clusters with 2D data
# data = data matrix; row = data point, col = variable
# clusters = leaf labels of each data row
# cex = size of the text in the plot
# add = add query results to plot of original data points?
# old_clusters = if add = T, leaf labels of the original data points
visualise_clusters <- function(data, clusters, cex = 1, add = F, old_clusters = clusters) {
  idx_clusters <- unique(old_clusters)  
  
  if(!add) {
    xlim <- c(min(data[,1])-.2, max(data[,1])+.2)
    ylim <- c(min(data[,2])-.2, max(data[,2])+.2)
    plot(x=0,y=0, xlab='', ylab='', type='n', xlim=xlim, ylim=ylim)
  }
  
    for(i in 1:length(idx_clusters))
      if(idx_clusters[i] %in% clusters)
        text(x=data[clusters == idx_clusters[i], 1], y=data[clusters == idx_clusters[i], 2], labels = idx_clusters[i], col=i, lwd=2, cex = cex)
}
