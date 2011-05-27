\name{pathway.names}
\alias{pathway.names}
\title{Pathway names}
\description{
  The names of the pathways in a database.
}
\usage{
pathway.names(pathways)
}
\arguments{
  \item{pathways}{one of \link{biocarta.pathways}, \link{kegg.pathways},
    \link{nci.pathways} and \link{reactome.pathways}.}
}
\value{
  A vector of names.
}
\seealso{
  \code{\link{biocarta.pathways}}
  
  \code{\link{kegg.pathways}}

  \code{\link{nci.pathways}}

  \code{\link{reactome.pathways}}
}
\examples{
ns <- pathway.names(reactome.pathways)
}
