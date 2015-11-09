\name{tetrad2graph}
\alias{tetrad2graph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{tetrad2graph
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tetrad2graph(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (filename) 
{
    gesout <- readLines(filename)
    startnodes <- grep("Graph Nodes:", gesout, fixed = TRUE)
    startedges <- grep("Graph Edges:", gesout, fixed = TRUE)
    gesnodes <- gesout[(startnodes + 1):(startedges - 1)]
    gesedges <- gesout[(startedges + 1):length(gesout)]
    nodesvec <- unlist(strsplit(gesnodes, split = " "))
    edgelist <- gsub("[0-9]+\\.\\s", "", gesedges)
    edgelist <- edgelist[nchar(edgelist) != 0]
    edgemat <- matrix(unlist(strsplit(edgelist, split = " ")), 
        nrow = length(edgelist), byrow = TRUE)
    undir <- which(edgemat[, 2] == "---")
    edgemat <- rbind(edgemat[, c(1, 3)], edgemat[undir, c(3, 
        1)])
    colnames(edgemat) <- c("Parent", "Child")
    edgel <- list()
    for (i in 1:length(nodesvec)) {
        edgel[[i]] <- edgemat[which(edgemat[, 1] == nodesvec[i]), 
            2]
    }
    names(edgel) <- nodesvec
    outputgraph <- graphNEL(nodes = nodesvec, edgeL = edgel, 
        edgemode = "directed")
    return(outputgraph)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
