#' draw a clustered graph
#'
#' Function \code{draw} employs the output of \code{lclust(A, n)} or any arbitrary grouping
#' to plot a nice clustered graph.
#'
#' @param A symmetric adjacency matrix
#' @param groups list where each element denotes the cluster, possibly the result of \code{lclust(A, n)}
#' @param colors palette which consist of at least \code{length(groups)} colors, default is 
#' \code{terrain.colors(legth(group))}
#' 
#' @export 
#' @examples 
#' draw(A, )
draw <- function(A = matrix(), groups = list(), colors = terrain.colors(length(groups)), main = "") {
  if (! "package:igraph" %in% search() ) library("igraph") 
  
  cols <- rep(0, nrow(A))
  for (i in 1:length(groups)) {
    cols[groups[[i]]] <- colors[i]
  }
  g <- graph.adjacency(A, mode = "undirected")
  
  plot.igraph(g, vertex.color = cols, vertex.size = 20,
              vertex.label.color = "black", main = main)
  legend(x = "bottomleft", legend = paste("N =", length(groups)), bty = "n")
}