################################################################################
create_nodes <- function(numNodes){
  node_list <- .jnew("java/util/ArrayList")
  for (i in 1:numNodes){
    nodi <- .jnew("edu/cmu/tetrad/data/ContinuousVariable", 
                  paste("X", i, sep=""))
    node_list$add(nodi)
  }
  node_list <- .jcast(node_list, "java.util.List")
  return(node_list)
}

################################################################################
create_graph <- function(node_list, graphGenMethod, numLatentConfounders,  
                         maxNumEdges, maxDegree, maxIndegree, maxOutdegree, 
                         connected, alpha, beta, delta_in, delta_out){
  
  if (graphGenMethod == "uniform"){
    # uniform graph
    newgraph <- .jcall("edu/cmu/tetrad/graph/GraphUtils", 
                       "Ledu/cmu/tetrad/graph/Graph;", "randomGraphUniform", 
                       node_list, 
                       as.integer(numLatentConfounders), 
                       as.integer(maxNumEdges), 
                       as.integer(maxDegree),
                       as.integer(maxIndegree), 
                       as.integer(maxOutdegree), 
                       connected)
    
  } else if (graphGenMethod == "scalefree"){
    # scalefree
    newgraph <- .jcall("edu/cmu/tetrad/graph/GraphUtils", 
                       "Ledu/cmu/tetrad/graph/Graph;", "scaleFreeGraph",
                       node_list, as.integer(numLatentConfounders), 
                       alpha, beta, delta_in, delta_out)
    
  } else if (graphGenMethod == "forwardedges"){
    # random forward edges
    # todo: fix this in Tetrad so it works when connected=TRUE, and submit 
    #a pull request 
    newgraph <- .jcall("edu/cmu/tetrad/graph/GraphUtils", 
                       "Ledu/cmu/tetrad/graph/Graph;", "randomGraph",
                       node_list, as.integer(numLatentConfounders), 
                       as.integer(maxNumEdges), as.integer(maxDegree),
                       as.integer(maxIndegree), as.integer(maxOutdegree), 
                       connected)
    
  } else {
    stop("Graph generation method not one of 'uniform', 'scalefree', or 
       'forwardedges'.")
  }
  
  return(newgraph)
}

################################################################################
get_degree_stats <- function(newgraph, node_list, numNodes, 
                             numLatentConfounders){
  
  degreemat <- matrix(nrow=numNodes, ncol=4)
  colnames(degreemat) <- c("degree", "indegree", "outdegree", "latent")
  for (node_i in 1:numNodes){
    actualNode <- as.list(node_list)[[node_i]]
    degreemat[node_i, "degree"] = newgraph$getNumEdges(actualNode)
    degreemat[node_i, "indegree"] = newgraph$getIndegree(actualNode)
    degreemat[node_i, "outdegree"] = newgraph$getOutdegree(actualNode)
    degreemat[node_i, "latent"] = actualNode$getNodeType()$toString()=="Latent"
  }
  
  addname <- function(prefix, vec){
    names(vec) <- paste(prefix, names(vec), sep="")
    return(vec)
  }
  
  mean_degrees <- colMeans(degreemat[,1:3])
  mean_degrees <- addname("mean_", mean_degrees)
  median_degrees <- apply(degreemat[,1:3], 2, "median")
  median_degrees <- addname("median_", median_degrees)
  max_degrees <- apply(degreemat[,1:3], 2, "max")
  max_degrees <- addname("max_", max_degrees)
  
  latents <- which(degreemat[,"latent"]==TRUE)
  if (numLatentConfounders > 0){
    latent_mean_degrees <- colMeans(degreemat[latents, 1:3, drop=FALSE])
    latent_median_degrees <- apply(degreemat[latents, 1:3, drop=FALSE], 2, 
                                   "median")
    latent_max_degrees <- apply(degreemat[latents, 1:3, drop=FALSE], 2, "max")
  } else {
    latent_mean_degrees <- c(0,0,0)
    latent_median_degrees <- c(0,0,0)
    latent_max_degrees <- c(0,0,0)
  }
  
  names(latent_mean_degrees) <- paste("latent_", names(mean_degrees), sep="")
  names(latent_median_degrees) <- paste("latent_",names(median_degrees),sep="")
  names(latent_max_degrees) <- paste("latent_", names(max_degrees), sep="")
  
  realNumEdges <- newgraph$getNumEdges()
  degree_stats <- c(mean_degrees, median_degrees, max_degrees, 
                    latent_mean_degrees, latent_median_degrees, 
                    latent_max_degrees, realNumEdges)
  names(degree_stats)[19] <- "realNumEdges"
  
  return(degree_stats)
}

################################################################################
get_true_graph <- function(newgraph, numLatentConfounders, maxPathLength){
  if (numLatentConfounders == 0){
    # get true CPDAG in Tetrad
    truegraph <- .jcall("edu/cmu/tetrad/search/SearchGraphUtils",
                        "Ledu/cmu/tetrad/graph/Graph;", 
                        "patternForDag", newgraph)
    truegraph <- .jcast(truegraph, "edu/cmu/tetrad/graph/Graph", check=TRUE)
    
  } else if (numLatentConfounders > 0){
    # get PAG of true generating DAG with latents in Tetrad
    dagtopag <- .jnew("edu/cmu/tetrad/search/DagToPag", newgraph)
    dagtopag$setMaxPathLength(as.integer(maxPathLength))
    truegraph <- dagtopag$convert()
    truegraph <- .jcast(truegraph, "edu/cmu/tetrad/graph/Graph", check = TRUE)
  }
  return(truegraph)
}

################################################################################
get_true_ug <- function(truegraph){
  trueug <- .jcall("edu/cmu/tetrad/graph/GraphUtils", 
                   "Ledu/cmu/tetrad/graph/Graph;", 
                   "undirectedMoralizedGraph", truegraph)
  trueug <- .jcast(trueug, "edu/cmu/tetrad/graph/Graph", check=TRUE)
  return(trueug)
}

################################################################################
remove_latents <- function(rdata, latent_nodes){
  latent_node_names <- strsplit(gsub("\\[|\\]", "", latent_nodes$toString()), 
                                split=", ")[[1]]
  rdata <- rdata[,setdiff(names(rdata), latent_node_names)]
  return(rdata)
}

################################################################################
ind_test_fisherz <- function(newcovmat, alpha=0.05){
  newindtest_fisherz <- .jnew("edu/cmu/tetrad/search/IndTestFisherZ", newcovmat, 
                              alpha)
  newindtest_fisherz <- .jcast(newindtest_fisherz, 
                               "edu/cmu/tetrad/search/IndependenceTest", 
                               check=TRUE)
  return(newindtest_fisherz)
}

################################################################################
# start FCI from given UG
ug_fci <- function(newug_tetrad, newindtest_fisherz, maxPathLength){
  newfci_tetrad <- .jnew("edu/cmu/tetrad/graph/EdgeListGraphSingleConnections", 
                         newug_tetrad)
  newfci_tetrad <- .jcast(newfci_tetrad, "edu/cmu/tetrad/graph/Graph", 
                          check=TRUE)
  nullsepsets <- .jnull(class = "edu/cmu/tetrad/search/SepsetMap")
  
  # sepsets is created with newug_tetrad (analogous to gesGraph, as is 
  # done in line 240 of GFci), rather than newfci_tetrad.
  sepsets <- .jnew("edu/cmu/tetrad/search/SepsetsMaxPValue",newug_tetrad, 
                   newindtest_fisherz, nullsepsets, as.integer(maxPathLength))
  
  ug_edges <- as.list(newfci_tetrad$getEdges())
  for (m in 1:length(ug_edges)){
    i <- ug_edges[[m]]$getNode1()
    k <- ug_edges[[m]]$getNode2()
    j <- newfci_tetrad$getAdjacentNodes(i)
    j$retainAll(newfci_tetrad$getAdjacentNodes(k))
    if ("!"(j$isEmpty())){
      sepsets$getSepset(i,k) # this sets the p-value for the sepsets object
      if (sepsets$getPValue() > newindtest_fisherz$getAlpha()) {
        newfci_tetrad$removeEdge(ug_edges[[m]])
      }
    }
  }
  
  sepsets <- .jcast(sepsets, "edu/cmu/tetrad/search/SepsetProducer", check=TRUE)
  nullfgs <- .jnull(class="edu/cmu/tetrad/search/Fgs")
  
  # can't call ruleR0Special without instantiating a GFci object
  bob <- .jnew("edu/cmu/tetrad/search/GFci", newindtest_fisherz)
  .jcall(bob, returnSig="V", method="ruleR0Special", newfci_tetrad, 
         newug_tetrad, sepsets, nullfgs)
  
  fciOrient <- .jnew("edu/cmu/tetrad/search/FciOrient", sepsets)
  fciOrient$doFinalOrientation(newfci_tetrad)
  
  learnedgraph <- .jcast(newfci_tetrad, "edu/cmu/tetrad/graph/Graph")
  return(learnedgraph)
}

################################################################################
LearnUg <- function(specificAlgorithms, skeptic_cor, node_list, lambda=NULL){
  
  if (specificAlgorithms %in% c("glasso", "glasso_ges", "glasso_fci")){
    # use glasso to generate UG
    newug <- huge(x=skeptic_cor, lambda = NULL, nlambda = NULL, 
                  lambda.min.ratio = NULL, method = "glasso", scr = NULL, 
                  scr.num = NULL, cov.output = FALSE, sym = "or", 
                  verbose = TRUE)
    
    # todo: pick lambda according to convergence proof?
    # for now, pick an arbitrary graph in the path
    ugmat <- newug$path[[5]]
    
  } else if (specificAlgorithms %in% c("clime", "clime_ges", "clime_fci")){
    newug <- clime(x=skeptic_cor, lambda=lambda,
                   sigma=TRUE, perturb=FALSE, standardize=FALSE,
                   linsolver=c("primaldual"), pdtol=1e-3, pdmaxiter=50)
    # todo: find principled way of getting ugmat
    ugmat <- 1*(round(newug$Omegalist[[5]],4)!=0)
    
  } else if (specificAlgorithms %in% c("mb", "mb_ges", "mb_fci")){
    newug <- huge(x=skeptic_cor, lambda = NULL, nlambda = NULL, 
                  lambda.min.ratio = NULL, method = "mb", scr = NULL, 
                  scr.num = NULL, cov.output = FALSE, sym = "or", 
                  verbose = TRUE)
    # todo: pick lambda according to convergence proof?
    # for now, pick an arbitrary graph in the path
    ugmat <- newug$path[[5]]
    
  }
  
  # translate adjacency matrix in R into edge list graph in Tetrad
  newug_tetrad <- ugraphToTetradGraph(ugmat, node_list)
  newug_tetrad <- .jcall("edu/cmu/tetrad/graph/GraphUtils", 
                         "Ledu/cmu/tetrad/graph/Graph;", 
                         "replaceNodes", newug_tetrad, 
                         node_list)
  
  return(newug_tetrad)
}


################################################################################
LearnDirected <- function(specificAlgorithms, newindtest_fisherz, newcovmat,
                          maxPathLength){
  # PC
  if (specificAlgorithms == "pc"){
    # run PC
    pcstable <- .jnew("edu/cmu/tetrad/search/PcStable", newindtest_fisherz)
    learnedgraph <- pcstable$search()
  }
  
  # GES
  if (specificAlgorithms == "ges"){
    # run GES
    newges <- .jnew("edu/cmu/tetrad/search/Ges", newcovmat)
    learnedgraph <- newges$search()
  }
  
  # FCI
  if (specificAlgorithms == "fci"){
    # run FCI
    newfci <- .jnew("edu/cmu/tetrad/search/Fci", newindtest_fisherz)
    newfci$setMaxPathLength(as.integer(maxPathLength))
    learnedgraph <- newfci$search()
  }
  
  # GFCI
  if (specificAlgorithms == "gfci"){
    # run GFCI
    newgfci <- .jnew("edu/cmu/tetrad/search/GFci", newindtest_fisherz)
    newgfci$setMaxPathLength(as.integer(maxPathLength))
    learnedgraph <- newgfci$search()
  }
  
  return(learnedgraph)
}

################################################################################
GetResults <- function(truegraph, learnedgraph){
  newgraphutils <- .jnew("edu/cmu/tetrad/graph/GraphUtils")
  nulloutput <- .jnull("java/io/PrintStream")
  adj_results <- AdjacencyResults(truegraph, learnedgraph)
  orient_results <- OrientationResults(truegraph, learnedgraph, newgraphutils, 
                                         nulloutput)
  return(c(adj_results, orient_results))
}

# not sure what these do:
#newGraphComparison$getArrowptCorrect()
#newGraphComparison$getArrowptFp()
#newGraphComparison$getArrowptFn()

################################################################################
AdjacencyResults <- function(truegraph, learnedgraph){
  graphComparison <- .jcall("edu/cmu/tetrad/search/SearchGraphUtils", 
                            "Ledu/cmu/tetrad/graph/GraphUtils$GraphComparison;", 
                            "getGraphComparison", 
                            .jcast(learnedgraph, "edu/cmu/tetrad/graph/Graph", 
                                   check=TRUE), 
                            truegraph)
  adjacency_result <- c(graphComparison$getAdjFp(), 
                        graphComparison$getAdjFn(), 
                        graphComparison$getAdjCorrect(), 
                        learnedgraph$getNumEdges(),
                        truegraph$getNumEdges())
  names(adjacency_result) <- c("AdjFp", "AdjFn", "AdjCorrect", "EstEdges", 
                               "TrueEdges")
  return(adjacency_result)
}

################################################################################
OrientationResults <- function(truegraph, learnedgraph, newgraphutils, 
                               nulloutput){
  result_counts <- newgraphutils$edgeMisclassificationCounts(truegraph, 
                                                             learnedgraph, 
                                                             nulloutput)
  
  edge_result_counts_r <- .jevalArray(result_counts, simplify=TRUE)
  colnames(edge_result_counts_r) <- c("---", "o-o", "o->", "-->", "<->", 
                                      "No Edge")
  rownames(edge_result_counts_r) <- c("---", "o-o", "o->", "<-o", "-->", "<--", 
                                      "<->", "No Edge")
  
  edge_results_vector <- as.vector(edge_result_counts_r)
  resnames <- expand.grid(paste("True:", rownames(edge_result_counts_r)), 
                          paste("Estd:", colnames(edge_result_counts_r)), 
                          stringsAsFactors = FALSE)
  resnames <- paste(resnames$Var1, resnames$Var2, sep=" ; ")
  names(edge_results_vector) <- resnames
  return(edge_results_vector)
}

################################################################################
cor_compare <- function(a, b, type="mse"){
  if (type=="max"){
    return(max(abs(a - b)))
  } else if (type=="mse"){
    return(sum((a - b)^2)/length(a))
  }
}

################################################################################
