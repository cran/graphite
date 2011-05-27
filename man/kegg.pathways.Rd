\name{kegg.pathways}
\alias{kegg.pathways}
\title{Gene lists and topologies of KEGG pathways}
\description{
  This object is a list with three elements.

  \itemize{
    \item nodes: a list of vectors, one for each pathway.

    Each element of the list is named according to the pathway it refers
    to. A vector contains the names of all the genes belonging to a
    pathway.

    \item edges: a list of matrices, one for each pathway.

    Each element of the list is named according to the pathway it refers
    to. A matrix describes all the edges of the network corresponding to
    a pathway. It contains four columns: the ID of the source node, the
    ID of the destination node, the direction (either "directed" or
    "undirected"), the type (for example, "binding").

    \item timestamp: the date at which the pathway data was retrieved.
  }

Ogata H, Goto S, Sato K, Fujibuchi W, Bono H, Kanehisa M. KEGG: Kyoto Encyclopedia of Genes and Genomes. Nucleic Acids Res. 1999 Jan 1;27(1):29-34.

  \url{http://www.genome.jp/kegg/}
}
