tetrad2graph <-
function(filename){
	gesout <- readLines(filename)
	
	# find the line numbers where the nodes and edge lists begin:
	startnodes <- grep("Graph Nodes:",gesout, fixed=TRUE)
	startedges <- grep("Graph Edges:",gesout, fixed=TRUE)
	
	# extract node and edge list text:
	gesnodes <- gesout[(startnodes+1):(startedges-1)]
	gesedges <- gesout[(startedges+1):length(gesout)]
	
	# split space-delimited list of nodes into character vector
	nodesvec <- unlist(strsplit(gesnodes, split=" "))
	
	# Erase number at the start of each line of the edge list
	edgelist <- gsub("[0-9]+\\.\\s", "", gesedges)
	# Get rid of empty lines
	edgelist <- edgelist[nchar(edgelist)!=0]
	
	# split edges into three element vectors, i.e. 
	# "node" "-->" "node" (or "node" "---" "node"),
	# and package into a matrix
	edgemat <- matrix(unlist(strsplit(edgelist, split=" ")),
					  nrow=length(edgelist), byrow=TRUE)
	
	# find undirected edge indices
	undir <- which(edgemat[,2]=="---")
	
	# for each undirected edge, create a new edge with the two variables 
	# in reversed order. Also, remove the edge column, but name the columns
	edgemat <- rbind(edgemat[,c(1,3)], edgemat[undir,c(3,1)])
	colnames(edgemat) <- c("Parent", "Child")
	
	# create edge list for graphNEL format
	edgel <- list()
	for (i in 1:length(nodesvec)){
		edgel[[i]] <- edgemat[which(edgemat[,1]==nodesvec[i]),2]
	}
	names(edgel) <- nodesvec
	
	outputgraph <- graphNEL(nodes=nodesvec, edgeL=edgel, edgemode="directed")
	return(outputgraph)
}
