\name{reactome.pathways}
\alias{reactome.pathways}
\title{Gene lists and topologies of Reactome pathways}
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
 
  Matthews L, Gopinath G, Gillespie M, Caudy M, Croft D, de Bono B, Garapati P, 
Hemish J, Hermjakob H, Jassal B, Kanapin A, Lewis S, Mahajan S, May B, Schmidt E,
Vastrik I, Wu G, Birney E, Stein L, D'Eustachio P. Reactome knowledgebase of
human biological pathways and processes. Nucleic Acids Res. 2009 Jan;37(Database 
issue):D619-22. Epub 2008 Nov 3.
  \url{http://www.reactome.org}
}
