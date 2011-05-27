\name{pathway.graph}
\alias{pathway.graph}
\title{Graph representing the topology of a pathway}
\description{
  Builds a graphNEL object representing the topology of a pathway.
}
\usage{
pathway.graph(pathways, name)
}
\arguments{
  \item{pathways}{one of \link{biocarta.pathways}, \link{kegg.pathways},
    \link{nci.pathways} and \link{reactome.pathways}.}
  \item{name}{the name of the pathway to be converted to a graph.}
}
\value{
  A graphNEL object.
}
\seealso{
  \code{\link{biocarta.pathways}}
  
  \code{\link{kegg.pathways}}

  \code{\link{nci.pathways}}

  \code{\link{reactome.pathways}}

  \code{\link[graph:graphNEL-class]{graphNEL}}
}
\examples{
gn <- pathway.graph(reactome.pathways, "Unwinding of DNA")
}
