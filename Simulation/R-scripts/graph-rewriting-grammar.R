
########################################################
# Plugin distribution for removing a node
probRemoveNode <- function(graphobject){
	undirect <- ugraph(graphobject)
	weightlist <- 1/(degree(undirect)+1)
	normweights <- weightlist/sum(weightlist)
	return(normweights)
}

# Modification: remove one node
modRemoveNode <- function(graphobject){
	nodelist <- nodes(graphobject)
	normweights <- probRemoveNode(graphobject)
	chosennode <- sample(nodelist, size=1, 
						 prob=normweights)
	newgraph <- removeNode(chosennode, graphobject)
	mod=paste("removing node: ", chosennode)
	cat(mod, "\n")
	return(list(newgraph,mod))
}

########################################################
# Plugin distribution for removing an edge
probRemoveEdge <- function(graphobject, edgeframe){
	# edgeframe must have a "from" and a "to" column
	nodelist <- nodes(graphobject)
	undirect <- ugraph(graphobject)
	nodeweightlist <- 1/(degree(undirect)+1)
	edgeframe$fromweight <- nodeweightlist[edgeframe$from]
	edgeframe$toweight <- nodeweightlist[edgeframe$to]
	edgeframe$edgeweight <- edgeframe$fromweight + edgeframe$toweight
	edgeframe$normedgeweightlist <- edgeframe$edgeweight/
									sum(edgeframe$edgeweight)
	return(edgeframe$normedgeweightlist)
}

# Modification: remove one edge
modRemoveEdge <- function(graphobject){
	nodelist <- nodes(graphobject)
	edgeframe <- as.data.frame(t(edgeMatrix(graphobject)))
	normedgeweightlist <- probRemoveEdge(graphobject, 
									   edgeframe)
	chosenedge <- sample(nrow(edgeframe), size=1, 
						prob=normedgeweightlist)
	newgraph <- removeEdge(from=nodelist[edgeframe$from[chosenedge]], 
						  to=nodelist[edgeframe$to[chosenedge]], 
						  graph=graphobject)
	mod=paste("removing edge: ", nodelist[edgeframe$from[chosenedge]], "-->", 
		nodelist[edgeframe$to[chosenedge]])
	cat(mod, "\n")
	return(list(newgraph,mod))
}



########################################################
# Plugin distribution for adding an edge
# Uniform over pairs of non-adjacent nodes
# Assumes graph is not complete!
probAddEdge <- function(graphobject, newedgeframe){
	n <- nrow(newedgeframe)
	probs <- rep(1/n, times=n)
	return(probs)
}

# Modification: add an edge
# Now does check that acyclicity is maintained
modAddEdge <- function(graphobject){
	nodelist <- nodes(graphobject)
	undirect <- ugraph(graphobject)
	nonadjacent <- 1-(as(undirect, "graphAM")@adjMat)
	diag(nonadjacent) <- 0
	newedgeframe <- which(nonadjacent!=0, arr.ind=T)
	probs <- probAddEdge(graphobject, newedgeframe)
  topo1 <- c()
  while (length(topo1)==0) {
    chosen.new.edge <- newedgeframe[sample(nrow(newedgeframe
    ), size=1, prob=probs), ]
    from <- nodelist[chosen.new.edge[1]]
    to <- nodelist[chosen.new.edge[2]]
    newgraph <- addEdge(from, to, graphobject)
    topo1 <- tsort(newgraph)
  }
  mod=paste("adding edge: ", from, "-->", to)
  cat(mod, "\n")
	return(list(newgraph,mod))
}


########################################################
# Plugin distribution for duplicating a node
probDupNode <- function(graphobject){
	n <- length(nodes(graphobject))
	probs <- rep(1/n, times=n)
	return(probs)
}

# Modification: add a node
modDupNode <- function(graphobject){
	nodelist <- nodes(graphobject)
	probs <- probDupNode(graphobject)
	node.to.duplicate <- sample(nodelist, size=1, prob=probs)
	pa <- unlist(inEdges(node.to.duplicate, graphobject))
	ch <- unlist(adj(graphobject, node.to.duplicate))
	newname <- paste(node.to.duplicate, "_dup", sep="")
	# Bug fix: prevents name collision if the same node has previously been duplicated 
	while (newname %in% nodelist) {
		newname <- paste(newname, "_2", sep="")
	}
	
	newgraph <- addNode(newname, graphobject)
	for (i in seq_along(pa)){
		newgraph <- addEdge(from=pa[i], to=newname,
							newgraph)
	}
	for (i in seq_along(ch)){
		newgraph <- addEdge(from=newname, to=ch[i],
							newgraph)
	}
	mod=paste("duplicating node: ", node.to.duplicate)
	cat(mod, "\n")
	return(list(newgraph,mod))
}

# ########################################################
# ########################################################
# # Examples
# # Example graph
   # V <- LETTERS[1:10]
   # edL <- vector("list", length=10)
   # names(edL) <- V
      # edL[[1]] <- list(edges=c(2,3))
      # edL[[2]] <- list(edges=c(4,6))
      # edL[[3]] <- list(edges=c(4,10))
      # edL[[4]] <- list(edges=c())
      # edL[[5]] <- list(edges=c(6,7))
      # edL[[6]] <- list(edges=c(8))
      # edL[[7]] <- list(edges=c(8,10))
      # edL[[8]] <- list(edges=c(9))
      # edL[[9]] <- list(edges=c())
      # edL[[10]] <- list(edges=c())
   # gR <- graphNEL(nodes=V, edgeL=edL, edgemode="directed")

# plot(gR)

# gR2 <- modRemoveNode(gR)
# par(mfrow=c(1,2))
# plot(gR)
# plot(gR2)
# gR2 <- modRemoveEdge(gR)
# par(mfrow=c(1,2))
# plot(gR)
# plot(gR2)
# gR2 <- modAddEdge(gR)
# par(mfrow=c(1,2))
# plot(gR)
# plot(gR2)
# gR2 <- modDupNode(gR)
# par(mfrow=c(1,2))
# plot(gR)
# plot(gR2)


########################################################
# converter: DAG to GaussParDAG
# figuring out how to parameterize things

# want: new("GaussParDAG", nodes, in.edges, params)

#rtest <- r.gauss.pardag(10, .2)
#rtesta <- randomDAG(10, .2)

dag2gausspardag <- function(dag, normalize = FALSE, lbe = 0.1, 
						    ube = 1, neg.coef = TRUE, lbv = 0.5, 
						    ubv = 1){
	V <- nodes(dag)
	edL <- inEdges(object=dag)
	for (i in 1:length(edL)){
		edL[[i]] <- match(edL[[i]], V)
	}
	p <- length(V)
    pars <- as.list(runif(p, min = lbv, max = ubv))
    names(pars) <- V
    for (i in 1:p) {
        parentCount <- length(edL[[i]])
        weights <- runif(parentCount, min = lbe, max = ube)
        if (neg.coef) 
            weights <- weights * sample(c(-1, 1), parentCount, 
                replace = TRUE)
        pars[[i]] <- c(pars[[i]], 0, weights)
    }
    result <- new("GaussParDAG", nodes = V, in.edges = edL, params = pars)
    if (normalize) {
        H <- diag(result$cov.mat())
        result$set.err.var(result$err.var()/H)
        H <- sqrt(H)
        for (i in 1:p) if (length(edL[[i]]) > 0) 
            result$.params[[i]][-c(1, 2)] <- pars[[i]][-c(1, 
                2)] * H[edL[[i]]]/H[i]
    }
    validObject(result)
    return(result)
}


# check whether cyclic graphs introduce any new complexity - can PC recover them?

# addd comments about plugin weighting function


########################################################
# distance metric
shnd <- function(g1,g2){
	nodestoadd1 <- setdiff(nodes(g2), nodes(g1))
	nodestoadd2 <- setdiff(nodes(g1), nodes(g2))
	for (i in seq_along(nodestoadd1)){
		g1 <- addNode(nodestoadd1[i], g1)
	}
	for (i in seq_along(nodestoadd2)){
		g2 <- addNode(nodestoadd2[i], g2)
	}
	shdist <- shd(g1,g2)
	fulldist <- shdist + length(c(nodestoadd1,nodestoadd2))
	return(fulldist)
}

########################################################
# # not using evolveset any more
# evolveset <- function(nmods, kgraphs, starter=NULL, n=NULL, prob=NULL, ...){
	# stopifnot("!"(is.null(c(starter, n))), 
			  # "!"(is.null(c(starter, prob))),
			  # (nmods > 0),
			  # (kgraphs > 0))
	# if (is.null(starter)) {
		# starter <- randomDAG(n, prob)
	# }
	# graphlist <- list(starter)
	# for (i in 1:kgraphs){
		# graphlist[[i+1]] <- evolveDAG(nmods, startgraph=starter, ...)[[2]]
	# }
	# return(graphlist)
# }

evolveDAG <- function(nmods, startgraph){
	stopifnot("!"(is.null(startgraph)), 
			  (nmods > 0))
	evgraph <- startgraph
	mods = c()
	for (i in 1:nmods){
		whichmod <- sample(4, size=1)
		if (whichmod==1){
#			cat("removing a node\n")
			evgraphlist <- modRemoveNode(evgraph)
			evgraph <- evgraphlist[[1]]
			mods <- c(mods, evgraphlist[[2]])
		} else if (whichmod==2){
#			cat("removing an edge\n")
			evgraphlist <- modRemoveEdge(evgraph)
			evgraph <- evgraphlist[[1]]
			mods <- c(mods, evgraphlist[[2]])
		} else if (whichmod==3){
#			cat("duplicating a node\n")
			evgraphlist <- modDupNode(evgraph)
			evgraph <- evgraphlist[[1]]
			mods <- c(mods, evgraphlist[[2]])
		} else if (whichmod==4){
#			cat("adding an edge\n")
			evgraphlist <- modAddEdge(evgraph)
			evgraph <- evgraphlist[[1]]
			mods <- c(mods, evgraphlist[[2]])
		}
	}
	cat("\n")
	return(list(evgraph, mods))
}

########################################################
# # overall loop
# # OBSOLETE
# #gR <- randomDAG(10, prob=.2)
# #nummods <- 8
# #graphlist <- list(gR)
# #igR <- igraph.from.graphNEL(gR)
# #l1 <- layout.fruchterman.reingold(igR)

# setwd("/Users/lizzie/Dropbox/Prospectus/synthetic_data")
# numnodes <- c(20, 200, 2000)
# nummods <- c(5, 15, 45)
# densities <- c(1,2,4)
# id <- gsub("\\s|:", "-", Sys.time())
# for (i in 1:length(numnodes)){
	# cat("-- numnodes =", numnodes[i], "\n")
	# for (j in 1:length(nummods)){
		# cat("---- nummods = ", nummods[j], "\n")
		# for (den in 1:length(densities)){
			# cat("---- density = ", densities[den], "\n")
			
			# # create random dag and set of evolved dags
			# newdags <- evolveset(nmods=nummods[j], kgraphs=10, starter=NULL, 
								 # n=numnodes[i], prob=den*2/numnodes[i])
			
			# # parameterize them (with default param ranges)
			# newpardags <- list()
			# for (k in 1:length(newdags)) {
				# newpardags[[k]] <- dag2gausspardag(newdags[[k]])
			# }
			
			# # create name to save DAGs  (as R object)
			# nameofobject <- paste(id, "-id_", "pardags_", "mods-", nummods[j], 
								  # "_nodes-", numnodes[i], "_den-", den,
								   # ".R", sep="")
			
			# # save parameterized dags in case we want to examine them
			# save(newpardags, file= nameofobject)
			
			# # generate datasets
			# for (h in 1:length(newdags)) {
				
				# dataset <- rmvnorm.ivent(1000, newpardags[[h]])
				
				# # create dataset name
				# if (h==1) graphname="original" else graphname=as.character(h)
				# nameofdata <- paste(id, "-id_", "data_dag-", graphname, 
									# "_mods-", nummods[j], 
									# "_nodes-", numnodes[i], 
									# "_den-", den, 
									# ".csv", sep="")
				
				# # write data to file
				# write.table(dataset, nameofdata, sep="\t", row.names=FALSE)
			# }
		# }
	# }
# }

# ########################################################
# # Evaluation:
# datanames <- list()
# k <- 1
# for (i in 1:length(numnodes)){
	# for (j in 1:length(nummods)){
		# for (den in 1:length(densities)){
			# datnam <- c()
			# for (h in 1:length(newdags)) {
				# if (h==1) graphname="original" else graphname=as.character(h)
				# nameofdata <- paste(id, "-id_", "data_dag-", graphname, 
									# "_mods-", nummods[j], 
									# "_nodes-", numnodes[i], 
									# "_den-", den, 
									# ".csv", sep="")
				 # datnam <- c(datnam, nameofdata)
			# }
			# datanames[[k]] <- datnam
			# k <- k+1
		# }
	# }
# }

# # created pooled data:
# # 100 samples from each of 3 related graphs
# # for each of the simulation conditions
# datasets <- list()
# for (i in 1:length(datanames)){
	# dataseti <- datanames[[i]]
	# dataiunion <- NULL
	# for (j in 2:4){
		# bob <- read.table(dataseti[j], header=TRUE,sep="\t",  stringsAsFactors=FALSE)
		# if (is.null(dataiunion)) dataiunion <- bob else {
			# bobnames <- names(bob)[which(names(bob) %in% names(dataiunion))]
			# unnames <- names(dataiunion)[which(names(dataiunion) %in% names(bob))]
			# dataiunion <- rbind(dataiunion[1:(100*(j-2)), unnames], bob[1:100, bobnames])
		# }
	# }
	# datasets[[i]] <- dataiunion
# }

# for (i in 1:length(datasets)){
	# write.table(datasets[[i]], paste("pooled_",datanames[[i]][2],sep=""), 
				# row.names=FALSE, sep="\t")
# }

# for (i in 1:length(datasets)){
	# bob <- read.table(datanames[[i]][2], header=TRUE,sep="\t",  stringsAsFactors=FALSE)
	# write.table(bob[1:100,], paste("solo_",datanames[[i]][2],sep=""), 
				# row.names=FALSE, sep="\t")
# }





# # for (i in 1:nummods){
	# # whichmod <- sample(4, size=1)
	# # if (whichmod==1){
		# # cat("removing a node\n")
		# # graphlist[[i+1]] <- modRemoveNode(graphlist[[i]])
	# # } else if (whichmod==2){
		# # cat("removing an edge\n")
		# # graphlist[[i+1]] <- modRemoveEdge(graphlist[[i]])
	# # } else if (whichmod==3){
		# # cat("duplicating a node\n")
		# # graphlist[[i+1]] <- modDupNode(graphlist[[i]])
	# # } else if (whichmod==4){
		# # cat("adding an edge\n")
		# # graphlist[[i+1]] <- modAddEdge(graphlist[[i]])
	# # }
# # }
# #par(mfrow=c(3,3))
# #for (i in 1:(nummods+1)) plot(graphlist[[i]])


