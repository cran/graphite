# Copyright 2011 Gabriele Sales <gabriele.sales@unipd.it>
#
#
# This file is part of graphite.
#
# graphite is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License
# version 3 as published by the Free Software Foundation.
#
# graphite is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public
# License along with graphite. If not, see <http://www.gnu.org/licenses/>.


pathway.graph <- function(pathways, name) {
  if (!require(graph))
    stop("the \"graph\" package is missing")

  nodes <- pathways$nodes[[name]]
  edges <- pathways$edges[[name]]
  degenerate <- is.null(edges)
  
  if (!degenerate) {
    edges.sym <- .gen.symmetric(edges)
    edgeL <- sapply(nodes, function(n) list(edges=edges.sym[edges.sym[,1]==n,2]), simplify=F, USE.NAMES=T)
  } else {
    edgeL <- list()
  }

  g <- new("graphNEL", nodes, edgeL, "directed")

  if (!degenerate) {
    edgeDataDefaults(g, "type") <- "unknown"
    edgeData(g, edges.sym[,1], edges.sym[,2], "type") <- edges.sym[,3]
  }

  g
}

.gen.symmetric <- function(m) {
  undirected <- m[m[,3]=="undirected",c(2,1,4)]
  m2 <- m[,-3]
  names(undirected) <- names(m2)
  rbind(m2, undirected)
}
