\name{runTopologyGSA}
\alias{runTopologyGSA}
\docType{data}
\title{
Run topological analysis on expression dataset using topologyGSA. See topologyGSA.
}
\description{
  Use graphical models to test the pathway components highlighting those
  involved in its deregulation.
}
\usage{
runTopologyGSA(pathway, test, exp1, exp2, ...)
}
\arguments{
  \item{pathway}{
One of the pathways contained in \link{biocarta}, \link{kegg},\link{nci} or \link{reactome}.
    }
  \item{test}{
Either \code{"var"} and \code{"mean"}. Determine the type of test used by topologyGSA.
    }
  \item{exp1}{
Experiment matrix of the first class, genes in columns.
}
 \item{exp2}{
Experiment matrix of the second class, genes in columns.
    }
 \item{...}{
   Additional parameters. 
 }
}
\details{
  This function produces a warning and returns NULL when the number of
  genes in common between the expression matrices and the pathway is
  less than 3.
}
\references{
Massa MS, Chiogna M, Romualdi C. Gene set analysis exploiting the topology of a pathway. BMC System Biol. 2010 Sep 1;4:121.
}
\examples{
library(topologyGSA)
data(examples)

p <- convertIdentifiers(kegg[["hsa04664"]], "symbol")
runTopologyGSA(p, "var", exp1, exp2, 0.05)
}
\seealso{
\code{\link[topologyGSA:pathway.var.test]{pathway.var.test}}
\code{\link[topologyGSA:pathway.mean.test]{pathway.mean.test}}

}
\keyword{topology}
\keyword{analysis}
\keyword{topologyGSEA}