# evaluate output on simulated data


# 1: load target graph (and other sources)
library(pcalg)
library(graph)
setwd("/Users/lizzie/Dropbox/Prospectus/synthetic_data")

# 2: load Tetrad output and convert to graphnel object


# 3: compare false positive and false negative ardjacencies and orientations using the pcalg package
# 4: Get 'covariate' information: SHND distance of target to sources; sample size? Number of sources? Number of samples per source? Ratio of target to other source data?
# 5: Save everything in a text file (csv)

fileConn <-file("output.txt")
writeLines(paste(names(result), collapse=", "), fileConn)
close(fileConn)



#1###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-5_nodes-20_den-1.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-5_nodes-20_den-1.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=5, nodes=20, den=1, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#2###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-15_nodes-20_den-1.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-15_nodes-20_den-1.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=15, nodes=20, den=1, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#3###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-45_nodes-20_den-1.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-45_nodes-20_den-1.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=45, nodes=20, den=1, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#4###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-5_nodes-200_den-1.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-5_nodes-200_den-1.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=5, nodes=200, den=1, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#5###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-15_nodes-200_den-1.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-15_nodes-200_den-1.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=15, nodes=200, den=1, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#6###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-45_nodes-200_den-1.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-45_nodes-200_den-1.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=45, nodes=200, den=1, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#7###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-5_nodes-2000_den-1.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-5_nodes-2000_den-1.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=5, nodes=2000, den=1, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#8###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-15_nodes-2000_den-1.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-15_nodes-2000_den-1.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=15, nodes=2000, den=1, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#9###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-45_nodes-2000_den-1.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-45_nodes-2000_den-1.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=45, nodes=2000, den=1, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#10###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-5_nodes-20_den-2.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-5_nodes-20_den-2.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=5, nodes=20, den=2, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#11###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-15_nodes-20_den-2.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-15_nodes-20_den-2.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=15, nodes=20, den=2, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#12###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-45_nodes-20_den-2.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-45_nodes-20_den-2.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=45, nodes=20, den=2, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#13###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-5_nodes-200_den-2.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-5_nodes-200_den-2.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=5, nodes=200, den=2, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#14###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-15_nodes-200_den-2.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-15_nodes-200_den-2.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=15, nodes=200, den=2, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#15###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-45_nodes-200_den-2.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-45_nodes-200_den-2.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=45, nodes=200, den=2, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#16###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-5_nodes-2000_den-2.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-5_nodes-2000_den-2.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=5, nodes=2000, den=2, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#17###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-15_nodes-2000_den-2.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-15_nodes-2000_den-2.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=15, nodes=2000, den=2, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#18###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-45_nodes-2000_den-2.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-45_nodes-2000_den-2.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=45, nodes=2000, den=2, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#19###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-5_nodes-20_den-3.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-5_nodes-20_den-3.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=5, nodes=20, den=3, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#20###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-15_nodes-20_den-3.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-15_nodes-20_den-3.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=15, nodes=20, den=3, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#21###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-45_nodes-20_den-3.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-45_nodes-20_den-3.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=45, nodes=20, den=3, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#22###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-5_nodes-200_den-3.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-5_nodes-200_den-3.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=5, nodes=200, den=3, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#23###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-15_nodes-200_den-3.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-15_nodes-200_den-3.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=15, nodes=200, den=3, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#24###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-45_nodes-200_den-3.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-45_nodes-200_den-3.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=45, nodes=200, den=3, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#25###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-5_nodes-2000_den-3.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-5_nodes-2000_den-3.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=5, nodes=2000, den=3, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#26###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-15_nodes-2000_den-3.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-15_nodes-2000_den-3.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=15, nodes=2000, den=3, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)

#27###################################################
load(	 "2014-11-19-00-11-08-id_pardags_mods-45_nodes-2000_den-3.R")
tet.out <- tetrad2graph(
"ges_solo_2014-11-19-00-11-08-id_data_dag-2_mods-45_nodes-2000_den-3.txt")
target <- newpardags[[2]]
nod <- target$.nodes
#edg <- target$.in.edges
adjmat <- wgtMatrix(target)
out.edg <- list()
for (i in 1:length(nod)){
	out.edg[[nod[i]]] <- nod[which(adjmat[i,]==TRUE)]
}
target <- graphNEL(nodes=nod, edgeL=out.edg, edgemode='directed') 
target <- dag2cpdag(target)
result <- compareGraphOrientations(tet.out, target)
result <- c(result, mods=45, nodes=2000, den=3, solo=TRUE)
write(paste(result, collapse=", "), file="output.txt", append=TRUE)





