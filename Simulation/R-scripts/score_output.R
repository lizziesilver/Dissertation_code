true1 <- matrix(c(0,1,0,1,0,1,0,1,0),ncol=3)
true2 <- matrix(c(0,0,0,1,0,1,0,0,0),ncol=3)
true3 <- matrix(c(0,1,0,0,1,0,0,0,0,1,0,1,0,0,0,0),ncol=4)

scoreorient <- function(out, truth){
	trushort <- compressedges(truth)
	outshort <- compressedges(out)
	tp <- 0
	tn <- 0
	fn <- 0
	fp <- 0
	td <- 0
	orientableedges <- sum(trushort[,1]=="directed")
	nonorientable <- sum(trushort[,1]=="undirected")
	directedout <- sum(outshort[,1]=="directed")
	truedges <- nrow(trushort)
	foa <- 0
	fua <- 0
	if ("!"(is.null(outshort))){
		outedges <- nrow(outshort)
		for (i in 1:nrow(outshort)){
			node1 <- outshort[i,2]
			node2 <- outshort[i,3]
			if (outshort[i,1]=="undirected"){
				for (i in 1:nrow(trushort)){
					if (trushort[i,2]==node1){
						if (trushort[i,3]==node2){
							if (trushort[i,1]=="undirected"){
								tn <- tn + 1
								fua <- fua + 1
							} else if (trushort[i,1]=="directed"){
								fn <- fn + 1
								foa <- foa + 1
							}
						}
					} else if (trushort[i,2]==node2){
						if (trushort[i,3]==node1){
							if (trushort[i,1]=="undirected"){
								tn <- tn + 1
								fua <- fua + 1
							} else if (trushort[i,1]=="directed"){
								fn <- fn + 1
								foa <- foa + 1
							}
						}
					}
				} 
			} else if (outshort[i,1]=="directed") {
				for (i in 1:nrow(trushort)){
					if (trushort[i,2]==node1){
						if (trushort[i,3]==node2){
							if (trushort[i,1]=="undirected"){
								fp <- fp + 1
								fua <- fua + 1
							} else if (trushort[i,1]=="directed"){
								tp <- tp + 1
								foa <- foa + 1
							}
						}
					} else if (trushort[i,2]==node2){
						if (trushort[i,3]==node1){
							if (trushort[i,1]=="undirected"){
								fp <- fp + 1
								fua <- fua + 1
							} else if (trushort[i,1]=="directed"){
								fp <- fp + 1
								foa <- foa + 1
							}
						}
					}
				} 
			}
		}
		if (orientableedges != 0){
			tpr <- tp/orientableedges
			fnr <- fn/orientableedges
		} else {
			tpr <- 1
			fnr <- 0
		}
		if (nonorientable != 0){
			fpr <- fp/truedges
			tnr <- tn/nonorientable
		} else {
			fpr <- fp/truedges
			tnr <- 1
		}
		if (directedout != 0){
			tdr <- tp/directedout
		} else {
			tdr <- 1
		}
	} else {
		outedges <- 0
		tpr <- 1
		fpr <- 0
		tnr <- 1
		fnr <- 0
		tdr <- 1
	}
	scores <- c(tpor=tpr, fpor=fpr, tnor=tnr, fnor=fnr, tdor=tdr, orientable=orientableedges, numedges=outedges, oriented=directedout, tpo=tp, fpo=fp, tno=tn, fno=fn, foundorientableadj=foa, foundunorientableadj=fua)
	return(scores)
}

compressedges <- function(graph){
	edges <- listEdges(graph)
	outedges <- NULL
	if ("!"(is.null(edges))){
		for (i in 1:length(edges)){
			newedge <- c()
			if (length(edges[[i]])==2){
				newedge[1] <- "undirected"
				newedge[2] <- edges[[i]][[1]][[1]]@bNode
				newedge[3] <- edges[[i]][[1]][[1]]@eNode
			} else {
				newedge[1] <- "directed"
				newedge[2] <- edges[[i]][[1]]@bNode
				newedge[3] <- edges[[i]][[1]]@eNode
			}
			outedges <- rbind(outedges, newedge)
		}
		row.names(outedges) <- NULL
		outedges <- as.data.frame(outedges)
		outedges[,1]<- as.character(outedges[,1])
		outedges[,2]<- as.character(outedges[,2])
		outedges[,2]<- as.numeric(outedges[,2])
		outedges[,3]<- as.character(outedges[,3])
		outedges[,3]<- as.numeric(outedges[,3])
	} else {
		outedges <- NULL
	}
	return(outedges)
}



V <- as.character(1:4)
edL2 <- vector("list", length=4)
names(edL2) <- V
edL2[[1]] <- list(edges= 2)
edL2[[2]] <- list(edges= c(1,3))
edL2[[3]] <- list(edges= c())
edL2[[4]] <- list(edges= c(3))
trueg3 <- new("graphNEL", nodes=V, edgeL=edL2, edgemode="directed")

V <- as.character(1:3)
edL2 <- vector("list", length=3)
names(edL2) <- V
edL2[[1]] <- list(edges= 2)
edL2[[2]] <- list(edges= c(1,3))
edL2[[3]] <- list(edges= c(2))
trueg1 <- new("graphNEL", nodes=V, edgeL=edL2, edgemode="undirected")

V <- as.character(1:3)
edL2 <- vector("list", length=3)
names(edL2) <- V
edL2[[1]] <- list(edges= c(2))
edL2[[2]] <- list(edges= c())
edL2[[3]] <- list(edges= c(2))
trueg2 <- new("graphNEL", nodes=V, edgeL=edL2, edgemode="directed")

model1.n150.out
model1.n1k.out

scoregraphlist <- function(glist, tru){
	scoredf <- NULL
	for (i in 1:length(glist)){
		score.i1 <- compareGraphs(glist[[i]]@graph, tru)
		score.i2 <- scoreorient(glist[[i]]@graph, tru)
		score.i <- c(score.i1, score.i2)
		scoredf <- rbind(scoredf, score.i)
	}
	return(scoredf)
}

score1.cont.n150 <- scoregraphlist(model1.n150.out, trueg1)
score1.cont.n1k <- scoregraphlist(model1.n1k.out, trueg1)
score2.cont.n150 <- scoregraphlist(model2.n150.out, trueg2)
score2.cont.n1k <- scoregraphlist(model2.n1k.out, trueg2)
score3.cont.n150 <- scoregraphlist(model3.n150.out, trueg3)
score3.cont.n1k <- scoregraphlist(model3.n1k.out, trueg3)
score4.cont.n150 <- scoregraphlist(model4.n150.out, trueg1)
score4.cont.n1k <- scoregraphlist(model4.n1k.out, trueg1)
score5.cont.n150 <- scoregraphlist(model5.n150.out, trueg2)
score5.cont.n1k <- scoregraphlist(model5.n1k.out, trueg2)
score6.cont.n150 <- scoregraphlist(model6.n150.out, trueg3)
score6.cont.n1k <- scoregraphlist(model6.n1k.out, trueg3)

score1.trunc.n150 <- scoregraphlist(poly1.n150.out, trueg1)
score1.trunc.n1k <- scoregraphlist(poly1.n1k.out, trueg1)
score2.trunc.n150 <- scoregraphlist(poly2.n150.out, trueg2)
score2.trunc.n1k <- scoregraphlist(poly2.n1k.out, trueg2)
score3.trunc.n150 <- scoregraphlist(poly3.n150.out, trueg3)
score3.trunc.n1k <- scoregraphlist(poly3.n1k.out, trueg3)
score4.trunc.n150 <- scoregraphlist(poly4.n150.out, trueg1)
score4.trunc.n1k <- scoregraphlist(poly4.n1k.out, trueg1)
score5.trunc.n150 <- scoregraphlist(poly5.n150.out, trueg2)
score5.trunc.n1k <- scoregraphlist(poly5.n1k.out, trueg2)
score6.trunc.n150 <- scoregraphlist(poly6.n150.out, trueg3)
score6.trunc.n1k <- scoregraphlist(poly6.n1k.out, trueg3)





