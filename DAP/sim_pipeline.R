################################################################################
# set up 

# allocate 4 gigs of memory to java
options( java.parameters = "-Xmx2g" )

# load packages
library(stringr)
library(graph)
library(RBGL)
library(rJava)
library(huge)
library(clime)
require(R.utils)


# todo: make code more modular. This loop is to big and hard to reason about.
# todo: run through scale free graph example

# source config file
source("/Users/lizzie/Dissertation_code/DAP/config.R")

# source utility functions
source(path_to_utils)

# initialize JVM, add all necessary jars to the classpath
.jinit(path_to_tetrad_jar) 
for (pat in other_tetrad_paths){
  .jaddClassPath(pat)
}

# check you're running the right version of Java
java_version <- .jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
if (substr(java_version, 1, 3) != "1.8"){
  stop("R is not using Java 8.")
}

################################################################################
# Set values of the parameters using functions in config.
for (i in 1:length(numNodesVec)){
  numNodes <- numNodesVec[i]
  
  maxDegreeVec <- setMaxDegree(numNodes)
  for (q in 1:length(maxDegreeVec)){
    maxDegree <- maxDegreeVec[q]
    maxIndegree <- setMaxIndegree(numNodes, maxDegree)
    maxOutdegree <- setMaxOutdegree(numNodes, maxDegree)
    
    numLatentConfoundersVec <- setNumLatentConfounders(numNodes)
    for (j in 1:length(numLatentConfoundersVec)){
      numLatentConfounders <- numLatentConfoundersVec[j]
      
      maxNumEdgesVec <- setMaxNumEdges(numNodes)
      for (k in 1:length(maxNumEdgesVec)){
        maxNumEdges <- maxNumEdgesVec[k]
        
        sampleSizeVec <- setSampleSize(numNodes)
        for (l in 1:length(sampleSizeVec)){
          sample_size <- sampleSizeVec[l]
          
          for (m in 1:length(graphGenMethodVec)){
            graphGenMethod <- graphGenMethodVec[m]
            
            ################################################################################
            # start timing data generation and processing
            startTimeData <- Sys.time()
            newgraphlist <- NULL
            attempt <- 1
            while( is.null(newgraphlist) && attempt <= 3 ) {
              attempt <- attempt + 1
              potentialError <- tryCatch(
                expr = {
                  evalWithTimeout({
                    
                    ################################################################################
                    # Generate DAGs using Tetrad
                    startTimeGraphGen <- Sys.time()
                    # create list of nodes using the ContinuousVariable class
                    node_list <- .jnew("java/util/ArrayList")
                    for (i in 1:numNodes){
                      nodi <- .jnew("edu/cmu/tetrad/data/ContinuousVariable", paste("X", i, sep=""))
                      node_list$add(nodi)
                    }
                    node_list <- .jcast(node_list, "java.util.List")
                    
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
                      newgraph <- .jcall("edu/cmu/tetrad/graph/GraphUtils", 
                                         "Ledu/cmu/tetrad/graph/Graph;", "scaleFreeGraph",
                                         node_list, as.integer(numLatentConfounders), 
                                         alpha, beta, delta_in, delta_out)
                      
                    } else if (graphGenMethod == "forwardedges"){
                      # random forward edges
                      # todo: fix this in Tetrad so it works when connected=TRUE, and submit a pull request 
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
                    endTimeGraphGen <- Sys.time()
                    graphGenTime <- difftime(endTimeGraphGen, startTimeGraphGen, units="secs")
                    
                    degreemat <- matrix(nrow=numNodes, ncol=4)
                    colnames(degreemat) <- c("degree", "indegree", "outdegree", "latent")
                    for (node_i in 1:numNodes){
                      degreemat[node_i, "degree"] = newgraph$getNumEdges(as.list(node_list)[[node_i]])
                      degreemat[node_i, "indegree"] = newgraph$getIndegree(as.list(node_list)[[node_i]])
                      degreemat[node_i, "outdegree"] = newgraph$getOutdegree(as.list(node_list)[[node_i]])
                      degreemat[node_i, "latent"] = as.list(node_list)[[node_i]]$getNodeType()$toString()=="Latent"
                    }
                    
                    mean_degrees <- colMeans(degreemat[,1:3])
                    names(mean_degrees) <- paste("mean_", names(mean_degrees), sep="")
                    median_degrees <- apply(degreemat[,1:3], 2, "median")
                    names(median_degrees) <- paste("median_", names(median_degrees), sep="")
                    max_degrees <- apply(degreemat[,1:3], 2, "max")
                    names(max_degrees) <- paste("max_", names(max_degrees), sep="")
                    if (numLatentConfounders > 0){
                      latent_mean_degrees <- colMeans(degreemat[degreemat[,"latent"]==TRUE,1:3, 
                                                                drop=FALSE])
                      names(latent_mean_degrees) <- paste("latent_mean_", names(latent_mean_degrees), 
                                                          sep="")
                      latent_median_degrees <- apply(degreemat[degreemat[,"latent"]==TRUE,1:3, 
                                                               drop=FALSE], 2, "median")
                      names(latent_median_degrees) <- paste("latent_median_", 
                                                            names(latent_median_degrees), sep="")
                      latent_max_degrees <- apply(degreemat[degreemat[,"latent"]==TRUE,1:3, 
                                                            drop=FALSE], 2, "max")
                      names(latent_max_degrees) <- paste("latent_max_", 
                                                         names(latent_max_degrees), sep="")
                    } else {
                      latent_mean_degrees <- c(0,0,0)
                      names(latent_mean_degrees) <- paste("latent_", names(mean_degrees), sep="")
                      latent_median_degrees <- c(0,0,0)
                      names(latent_median_degrees) <- paste("latent_", names(median_degrees), sep="")
                      latent_max_degrees <- c(0,0,0)
                      names(latent_max_degrees) <- paste("latent_", names(max_degrees), sep="")
                    }
                    
                    realNumEdges <- newgraph$getNumEdges()
                    realMaxDegree <- .jcall("edu/cmu/tetrad/graph/GraphUtils", 
                                            "I", "getDegree", newgraph)
                    
                    ################################################################################
                    # get true PAG, Pattern and UG for evaluation phase
                    # todo: go through all this in detail and make sure that the
                    # various graph manipulations aren't changing the structure. 
                    
                    startTimeDagToCpdagOrPag <- Sys.time()
                    
                    if (numLatentConfounders == 0){
                      # get true CPDAG in Tetrad
                      truegraph <- .jcall("edu/cmu/tetrad/search/SearchGraphUtils",
                                          "Ledu/cmu/tetrad/graph/Graph;", "patternForDag", newgraph)
                      truegraph <- .jcast(truegraph, "edu/cmu/tetrad/graph/Graph", check=TRUE)
                      
                    } else if (numLatentConfounders > 0){
                      # get PAG of true generating DAG with latents in Tetrad
                      dagtopag <- .jnew("edu/cmu/tetrad/search/DagToPag", newgraph)
                      dagtopag$setMaxPathLength(as.integer(maxPathLength))
                      truegraph <- dagtopag$convert()
                      truegraph <- .jcast(truegraph, "edu/cmu/tetrad/graph/Graph", check = TRUE)
                    }
                    endTimeDagToCpdagOrPag <- Sys.time()
                    dagToCpdagOrPagTime <- difftime(endTimeDagToCpdagOrPag, startTimeDagToCpdagOrPag, units = "secs")
                    
                    # get true UG (moralize the true CPDAG or PAG)
                    startTimeMoralize <- Sys.time()
                    trueug <- .jcall("edu/cmu/tetrad/graph/GraphUtils", "Ledu/cmu/tetrad/graph/Graph;", 
                                     "undirectedMoralizedGraph", truegraph)
                    trueug <- .jcast(trueug, "edu/cmu/tetrad/graph/Graph", check=TRUE)
                    endTimeMoralize <- Sys.time()
                    moralizeTime <- difftime(endTimeMoralize, startTimeMoralize, units="secs") 
                    
                    newgraphlist <- list(node_list=node_list,
                                         newgraph=newgraph, 
                                         truegraph=truegraph,
                                         trueug=trueug,
                                         graphGenTime=graphGenTime,
                                         dagToCpdagOrPagTime=dagToCpdagOrPagTime,
                                         moralizeTime=moralizeTime,
                                         realNumEdges=realNumEdges,
                                         realMaxDegree=realMaxDegree)
                    cat("Attempt at generating graphs:", attempt-1, "\n")
                  }, 
                  timeout = 180)
                }, 
                TimeoutException = function(ex) {cat("Timeout. Skipping.\n")}
              )
              
              if(inherits(potentialError, "error")) {
                next
              } else {
                
                node_list <- newgraphlist$node_list
                newgraph <- newgraphlist$newgraph
                truegraph <- newgraphlist$truegraph
                trueug <- newgraphlist$trueug
                graphGenTime <- newgraphlist$graphGenTime
                dagToCpdagOrPagTime <- newgraphlist$dagToCpdagOrPagTime
                moralizeTime <- newgraphlist$moralizeTime
                realNumEdges <- newgraphlist$realNumEdges
                realMaxDegree <- newgraphlist$realMaxDegree
                
                ################################################################################
                # parameterize DAGs using Tetrad
                newsem <- .jnew("edu/cmu/tetrad/sem/LargeSemSimulator", newgraph)
                
                # generate data using Tetrad
                newdata <- .jcall(newsem, "Ledu/cmu/tetrad/data/DataSet;", 
                                  "simulateDataAcyclic", as.integer(sample_size))
                
                ################################################################################
                # load data into R
                rdata <- read.table(text=newdata$toString(), header=TRUE)
                
                # if latent nodes exist, remove them from R dataset
                if (numLatentConfounders > 0){
                  latent_nodes <- .jcall("edu/cmu/tetrad/graph/GraphUtils","Ljava/util/List;", 
                                         "getLatents", newgraph)
                  latent_node_names <- strsplit(gsub("\\[|\\]", "", latent_nodes$toString()), 
                                                split=", ")[[1]]
                  rdata <- rdata[,setdiff(names(rdata), latent_node_names)]
                  
                  # remove latent nodes from node_list, so they don't screw us up
                  node_list$removeAll(latent_nodes)
                  
                }
                
                # SKEPTIC recovers cov matrix in R
                # Note that there is no need to explicitly transform the data to a nonparanormal
                # distribution, because monotonic transforms do not change the rank of the data,
                # and the SKEPTIC relies only on the ranks.
                skeptic_cor <- huge.npn(rdata, npn.func="skeptic", npn.thresh=NULL, 
                                        verbose=TRUE)
                
                # compare with correlation matrix of untransformed data
                # todo: comparison, evaluation measure of skeptic
                true_cor <- cor(rdata)
                
                # pass skeptic cov mat back into Tetrad, for use with independence test
                newcovmat <- rCovMatrix2TetradCovMatrix(skeptic_cor, node_list, sample_size)
                
                #           # In Tetrad, create covariance matrix from raw data:
                #           truecovmat <- .jnew("edu/cmu/tetrad/data/CovarianceMatrixOnTheFly", newdata)
                #           truecovmat <- .jcast(newcovmat, "edu/cmu/tetrad/data/ICovarianceMatrix", 
                #                               check=TRUE)
                
                # Use covariance matrix to create independence test:
                # todo: choose alpha programmatically using sampleSize
                newindtest_fisherz <- .jnew("edu/cmu/tetrad/search/IndTestFisherZ", newcovmat, 
                                            0.05)
                newindtest_fisherz <- .jcast(newindtest_fisherz, 
                                             "edu/cmu/tetrad/search/IndependenceTest", 
                                             check=TRUE)
                
                
                ################################################################################
                # finish timing data generation & processing
                endTimeData <- Sys.time()
                
                dataTime <- difftime(endTimeData, startTimeData, units="secs") 
                
                ################################################################################
                # Set/loop over learning methods
                learningMethodVec <- setLearningMethod(numLatentConfounders)
                for (n in 1:length(learningMethodVec)){
                  learningMethod <- learningMethodVec[n]
                  
                  specificAlgorithmsVec <- setSpecificAlgorithms(learningMethod)
                  for (p in 1:length(specificAlgorithmsVec)){
                    specificAlgorithms <- specificAlgorithmsVec[p]
                    
                    ################################################################################
                    # begin timing specific algorithm
                    startTimeAlg <- Sys.time()
                    
                    ################################################################################
                    # learn graphs: UGs and methods starting from UGs
                    
                    # UG learners
                    if (learningMethod %in% c("ug", "ug_to_cpdag", "ug_to_pag")){
                      if (specificAlgorithms %in% c("glasso", "glasso_ges", "glasso_fci")){
                        # use glasso to generate UG
                        newug <- huge(x=skeptic_cor, lambda = NULL, nlambda = NULL, 
                                      lambda.min.ratio = NULL, method = "glasso", scr = NULL, 
                                      scr.num = NULL, cov.output = FALSE, sym = "or", verbose = TRUE)
                        
                        # todo: pick lambda according to convergence proof
                        # for now, pick an arbitrary graph in the path
                        ugmat <- newug$path[[5]]
                        
                      } else if (specificAlgorithms %in% c("clime", "clime_ges", "clime_fci")){
                        newug <- clime(x=skeptic_cor, lambda=lambda,
                                       sigma=TRUE, perturb=FALSE, standardize=FALSE,
                                       linsolver=c("primaldual"), pdtol=1e-3, pdmaxiter=50)
                        # todo: find principled way of getting ugmat
                        ugmat <- 1*(round(newug$Omegalist[[5]],4)!=0)
                        
                      } else if (specificAlgorithms %in% c("mb", "mb_ges", "mb_fci")){
                        newug <- huge(x=skeptic_cor, lambda = NULL, nlambda = NULL, lambda.min.ratio = NULL, 
                                      method = "mb", scr = NULL, scr.num = NULL, cov.output = FALSE, 
                                      sym = "or", verbose = TRUE)
                        # todo: pick lambda according to convergence proof
                        # for now, pick an arbitrary graph in the path
                        ugmat <- newug$path[[5]]
                        
                      }
                      
                      # translate adjacency matrix in R into edge list graph in Tetrad
                      newug_tetrad <- ugraphToTetradGraph(ugmat, node_list)
                      newug_tetrad <- .jcall("edu/cmu/tetrad/graph/GraphUtils", 
                                             "Ledu/cmu/tetrad/graph/Graph;", 
                                             "replaceNodes", newug_tetrad, 
                                             .jcast(newindtest_fisherz$getVariables(), "java/util/List", check = TRUE))
                      learnedgraph <- .jcast(newug_tetrad, "edu/cmu/tetrad/graph/Graph", check=TRUE)
                      
                    }
                    
                    # UG to GES
                    if (specificAlgorithms %in% c("glasso_ges", "clime_ges", "mb_ges")){
                      # feed UG to GES
                      # todo: ask Peter about this
                    }
                    
                    # UG to FCI
                    if (specificAlgorithms %in% c("glasso_fci", "clime_fci", "mb_fci")){
                      # start FCI from given UG
                      newfci_tetrad <- .jnew("edu/cmu/tetrad/graph/EdgeListGraphSingleConnections", 
                                             newug_tetrad)
                      newfci_tetrad <- .jcast(newfci_tetrad, "edu/cmu/tetrad/graph/Graph", check=TRUE)
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
                      
                      sepsets <- .jcast(sepsets, "edu/cmu/tetrad/search/SepsetProducer", check = TRUE)
                      nullfgs <- .jnull(class="edu/cmu/tetrad/search/Fgs")
                      
                      # can't call ruleR0Special without instantiating a GFci object
                      bob <- .jnew("edu/cmu/tetrad/search/GFci", newindtest_fisherz)
                      .jcall(bob, returnSig="V", method="ruleR0Special", newfci_tetrad, newug_tetrad, sepsets, nullfgs)
                      
                      fciOrient <- .jnew("edu/cmu/tetrad/search/FciOrient", sepsets)
                      fciOrient$doFinalOrientation(newfci_tetrad)
                      learnedgraph <- .jcast(newfci_tetrad, "edu/cmu/tetrad/graph/Graph")
                    }
                    
                    ################################################################################
                    # Learn graph: non-UG methods (PC, GES, FCI, GFCI)
                    
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
                    
                    
                    ################################################################################
                    # evaluation
                    
                    # edge confusion matrix:
                    newgraphutils <- .jnew("edu/cmu/tetrad/graph/GraphUtils")
                    nulloutput <- .jnull("java/io/PrintStream")
                    if (learningMethod=="ug"){
                      result_counts <- newgraphutils$edgeMisclassificationCounts(trueug, newug_tetrad, nulloutput)
                      # adjacency errors:
                      newGraphComparison <- .jcall("edu/cmu/tetrad/search/SearchGraphUtils", 
                                                   "Ledu/cmu/tetrad/graph/GraphUtils$GraphComparison;", 
                                                   "getGraphComparison", 
                                                   .jcast(learnedgraph, "edu/cmu/tetrad/graph/Graph", check=TRUE), trueug)
                      
                    } else {
                      result_counts <- newgraphutils$edgeMisclassificationCounts(truegraph, learnedgraph, nulloutput)
                      # adjacency errors:
                      newGraphComparison <- .jcall("edu/cmu/tetrad/search/SearchGraphUtils", 
                                                   "Ledu/cmu/tetrad/graph/GraphUtils$GraphComparison;", 
                                                   "getGraphComparison", 
                                                   .jcast(learnedgraph, "edu/cmu/tetrad/graph/Graph", check=TRUE) , truegraph)
                      
                    }
                    
                    
                    result_string <- newgraphutils$edgeMisclassifications(result_counts)
                    cat(result_string)
                    
                    
                    # not sure what these do:
                    #newGraphComparison$getArrowptCorrect()
                    #newGraphComparison$getArrowptFp()
                    #newGraphComparison$getArrowptFn()
                    
                    endTimeAlg <- Sys.time()
                    algTime <- difftime(endTimeAlg, startTimeAlg, units="secs") 
                    
                    ################################################################################
                    # results into R, save to file
                    timeComplete <- format(Sys.time(), "%a %b %d %X %Y")
                    
                    searchparams <- c(timeComplete, graphGenMethod, learningMethod, specificAlgorithms, 
                                      numNodes, numLatentConfounders, 
                                      mean_degrees, median_degrees, max_degrees, 
                                      latent_mean_degrees, latent_median_degrees, latent_max_degrees,
                                      realNumEdges, maxNumEdges, sample_size, 
                                      algTime, dataTime, graphGenTime, moralizeTime, dagToCpdagOrPagTime)
                    names(searchparams) <- c("timeComplete", "graphGenMethod", "learningMethod", "specificAlgorithms",
                                             "numNodes", "numLatentConfounders", 
                                             "mean_degrees", "median_degrees", "max_degrees", 
                                             "latent_mean_degrees", "latent_median_degrees", "latent_max_degrees",
                                             "realNumEdges", "maxNumEdges", "sample_size", 
                                             "algTime", "dataTime", "graphGenTime", "moralizeTime", "dagToCpdagOrPagTime")
                    
                    #edges
                    edge_result_counts_r <- .jevalArray(result_counts, simplify=TRUE)
                    colnames(edge_result_counts_r) <- c("---", "o-o", "o->", "-->", "<->", "No Edge")
                    rownames(edge_result_counts_r) <- c("---", "o-o", "o->", "<-o", "-->", "<--", "<->", "No Edge")
                    
                    edge_results_vector <- as.vector(edge_result_counts_r)
                    resnames <- expand.grid(paste("True:",rownames(edge_result_counts_r)), 
                                            paste("Estd:", colnames(edge_result_counts_r)), 
                                            stringsAsFactors = FALSE)
                    resnames <- paste(resnames$Var1, resnames$Var2, sep=" ; ")
                    names(edge_results_vector) <- resnames
                    
                    # adjacencies
                    adjacency_result <- c(newGraphComparison$getAdjFp(), 
                                          newGraphComparison$getAdjFn(), 
                                          newGraphComparison$getAdjCorrect(), 
                                          learnedgraph$getNumEdges(),
                                          truegraph$getNumEdges())
                    names(adjacency_result) <- c("AdjFp", "AdjFn", "AdjCorrect", "EstEdges", "TrueEdges")
                    
                    
                    #write out
                    allresults <- c(searchparams, adjacency_result, edge_results_vector)
                    if ("!"(file.exists(output_filename))){
                      write(names(allresults), file=output_filename, ncolumns = length(allresults), sep="\t")
                    }
                    write(allresults, file=output_filename, ncolumns=length(allresults), sep="\t", append=TRUE)
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}