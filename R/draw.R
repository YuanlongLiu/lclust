#' draw a clustered graph
#'
#' Function \code{draw} employs the output of \code{lclust(A, n)} or any arbitrary list of groups
#' to plot a nice clustered graph.
#'
#' @param A symmetric adjacency matrix
#' @param groups list where each element denotes the cluster, possibly \code{lclust(A, n)}
#' @param colors palette which consist of at least as much as number of groups colors, default is 
#' \code{terrain.colors(legth(group))}
#' 
#' @export 
#' @examples 
#' draw(A, lclust(A, 2))
#' B <- matrix(c(0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0), 4, 4)
#' groups <- list(c(1, 3), c(2, 4))
#' draw(B, groups)
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
#' Data on international trade
#' 
#' 24 selected representative countries exchanging crude materials
#' 
#' @format Non-symmetric bunary matrix, trade goes from rows to columns.
#' @docType data
#' @keywords datasets
#' @name crude
#' @usage data(crude)
#' @source \url{http://moreno.ss.uci.edu/data.html#trade}
"crude"