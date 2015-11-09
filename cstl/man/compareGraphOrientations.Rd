\name{compareGraphOrientations}
\alias{compareGraphOrientations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{compareGraphOrientations
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
compareGraphOrientations(gl, gt)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gl}{
%%     ~~Describe \code{gl} here~~
}
  \item{gt}{
%%     ~~Describe \code{gt} here~~
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
function (gl, gt) 
{
    ml <- wgtMatrix(ugraph(gl))
    mt <- wgtMatrix(ugraph(gt))
    p <- dim(ml)[2]
    mt[mt != 0] <- rep(1, sum(mt != 0))
    ml[ml != 0] <- rep(1, sum(ml != 0))
    diffm <- ml - mt
    nmbTrueGaps <- (sum(mt == 0) - p)/2
    if (nmbTrueGaps == 0) {
        afpr <- 1
    }
    else {
        afpr <- (sum(diffm > 0)/2)/nmbTrueGaps
    }
    diffm2 <- mt - ml
    nmbTrueEdges <- (sum(mt == 1)/2)
    if (nmbTrueEdges == 0) {
        atpr <- 0
    }
    else {
        atpr <- 1 - (sum(diffm2 > 0)/2)/nmbTrueEdges
    }
    trueEstEdges <- (nmbTrueEdges - sum(diffm2 > 0)/2)
    if (sum(ml == 1) == 0) {
        if (trueEstEdges == 0) {
            atdr <- 1
        }
        else {
            atdr <- 0
        }
    }
    else {
        atdr <- trueEstEdges/(sum(ml == 1)/2)
    }
    dml <- wgtMatrix(gl)
    dmt <- wgtMatrix(dag2cpdag(gt))
    tadml <- dml
    tadml[mt == 0] <- rep(0, sum(mt == 0))
    tadmt <- dmt
    tadmt[ml == 0] <- rep(0, sum(ml == 0))
    orient <- tadmt + t(tadmt)
    orientable <- tadmt
    orientable[orient == 2] <- rep(0, sum(orient == 2))
    unorientable <- tadmt
    unorientable[orient == 1] <- rep(0, sum(orient == 1))
    orientl <- tadml + t(tadml)
    oriented <- tadml
    oriented[orientl == 2] <- rep(0, sum(orientl == 2))
    unoriented <- tadml
    unoriented[orientl == 1] <- rep(0, sum(orientl == 1))
    numUnorientable <- (sum(unorientable == 1))/2
    otn <- sum((unoriented == unorientable)[unorientable == 1])/2
    if (numUnorientable > 0) {
        otnr <- otn/numUnorientable
    }
    else {
        otnr <- 1
    }
    numOrientable <- (sum(orientable == 1))
    otp <- sum((oriented == orientable)[orientable == 1])
    if (numOrientable > 0) {
        otpr <- otp/numOrientable
    }
    else {
        otpr <- 1
    }
    ofp <- sum((orientable - oriented) == -1)
    numMisorientable <- numOrientable + numUnorientable
    if (numMisorientable > 0) {
        ofpr <- ofp/numMisorientable
    }
    else {
        ofpr <- 1
    }
    ofn <- sum((unorientable - unoriented) == -1)/2
    if (numOrientable > 0) {
        ofnr <- ofn/numOrientable
    }
    else {
        ofnr <- 1
    }
    if (sum(oriented) > 0) {
        otdr <- otp/sum(oriented)
    }
    else {
        otdr = 1
    }
    return(c(atpr = atpr, afpr = afpr, atdr = atdr, otpr = otpr, 
        ofpr = ofpr, otnr = otnr, ofnr = ofnr, otdr = otdr))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
