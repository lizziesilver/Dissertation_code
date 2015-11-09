compareGraphOrientations <-
function (gl, gt) {
	
	# gl <- tet.out
	# gt <- target2 
		
    # extract undirected adjacency matrices
    ml <- wgtMatrix(ugraph(gl))
    mt <- wgtMatrix(ugraph(gt))
    
    # p is number of nodes
    p <- dim(ml)[2]
    
    # make sure edges aren't weighted
    mt[mt != 0] <- rep(1, sum(mt != 0))
    ml[ml != 0] <- rep(1, sum(ml != 0))
    
    # difference matrix
    diffm <- ml - mt
    
    # true gaps, below the diagonal
    nmbTrueGaps <- (sum(mt == 0) - p)/2
    # every time you learn an edge in a true gap, it's a fp
    if (nmbTrueGaps == 0) {
    	afpr <- 1
    } else {
    	afpr <- (sum(diffm > 0)/2)/nmbTrueGaps
    }
    
    # same for true edges
    diffm2 <- mt - ml
    nmbTrueEdges <- (sum(mt == 1)/2)
    if (nmbTrueEdges == 0) {
    	atpr <- 0
    	} else {
    	atpr <- 1 - (sum(diffm2 > 0)/2)/nmbTrueEdges
    }
    
    # true discovery rate: true learned edges / learned edges
    trueEstEdges <- (nmbTrueEdges - sum(diffm2 > 0)/2)
    if (sum(ml == 1) == 0) { 
        if (trueEstEdges == 0) {atdr <- 1} else {atdr <- 0}
    } else {
    	atdr <- trueEstEdges/(sum(ml == 1)/2)
    }
    
    # directed adjacency matrix of learned graph:
    dml <- wgtMatrix(gl)
    dmt <- wgtMatrix(dag2cpdag(gt))
    
    # looking only at edges that are true adjacencies
    tadml <- dml
    tadml[mt == 0] <- rep(0, sum(mt == 0))
    tadmt <- dmt
    tadmt[ml == 0] <- rep(0, sum(ml == 0))
    
    # get matrices of edges that are unorientable (undirected in 
    # the cpdag of the true dag) or orientable
    orient <- tadmt + t(tadmt)
    orientable <- tadmt
    orientable[orient == 2] <- rep(0, sum(orient == 2))
    unorientable <- tadmt
    unorientable[orient == 1] <- rep(0, sum(orient == 1))
    
    # get matrices of edges that are unoriented (undirected in 
    # the learned cpdag) or oriented
    orientl <- tadml + t(tadml)
    oriented <- tadml
    oriented[orientl == 2] <- rep(0, sum(orientl == 2))
    unoriented <- tadml
    unoriented[orientl == 1] <- rep(0, sum(orientl == 1))
    
    # true negative rate
    numUnorientable <- (sum(unorientable == 1))/2
    otn <- sum((unoriented==unorientable)[unorientable==1])/2
    if (numUnorientable > 0){
    		otnr <- otn/numUnorientable
    } else {
    		otnr <- 1
    }
	
	# true positive rate
    numOrientable <- (sum(orientable == 1))
    otp <- sum((oriented==orientable)[orientable==1])
    if (numOrientable > 0){
    		otpr <- otp/numOrientable
    } else {
    		otpr <- 1
    }
    
    # false positive rate
    ofp <- sum((orientable - oriented) == -1)
    numMisorientable <- numOrientable + numUnorientable
    if (numMisorientable > 0){
    		ofpr <- ofp/numMisorientable
    } else {
    		ofpr <- 1
    }
    
    # false negative rate
    ofn <- sum((unorientable - unoriented) == -1) / 2
    if (numOrientable > 0){
    		ofnr <- ofn/numOrientable
    } else {
    		ofnr <- 1
    }
    
    # true discovery rate
    if (sum(oriented)>0){
    	otdr <- otp / sum(oriented)
    } else {
    	otdr = 1
    }
    
    return( c(atpr = atpr, afpr = afpr, atdr = atdr, otpr = otpr, 
    		  ofpr = ofpr, otnr = otnr, ofnr = ofnr, otdr = otdr))
}
