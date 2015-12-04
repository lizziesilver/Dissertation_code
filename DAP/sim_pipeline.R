################################################################################
# set up 
# load packages
library(stringr)
library(graph)
library(RBGL)
library(rJava)
library(huge)

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
  
  maxDegree <- setMaxDegree(numNodes)
  maxIndegree <- setMaxIndegree(numNodes)
  maxOutdegree <- setMaxOutdegree(numNodes)
  
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
          
          ################################################################################
          # Generate DAGs using Tetrad
          
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
          
          ################################################################################
          # parameterize DAGs using Tetrad
          newsem <- .jnew("edu/cmu/tetrad/sem/LargeSemSimulator", newgraph)
          
          # generate data using Tetrad
          newdata <- .jcall(newsem, "Ledu/cmu/tetrad/data/DataSet;", 
                            "simulateDataAcyclic", as.integer(sample_size))
          
          # In Tetrad, create covariance matrix from data:
          newcovmat <- .jnew("edu/cmu/tetrad/data/CovarianceMatrixOnTheFly", newdata)
          newcovmat <- .jcast(newcovmat, "edu/cmu/tetrad/data/ICovarianceMatrix", 
                              check=TRUE)
          
          # Use covariance matrix to create independence test:
          # todo: choose alpha programmatically using sampleSize
          newindtest_fisherz <- .jnew("edu/cmu/tetrad/search/IndTestFisherZ", newcovmat, 
                                      0.05)
          newindtest_fisherz <- .jcast(newindtest_fisherz, 
                                       "edu/cmu/tetrad/search/IndependenceTest", 
                                       check=TRUE)
          
          # todo URGENT: pass skeptic cov mat back into Tetrad, for use with independence test
          
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
          
          ################################################################################
          # finish timing data generation & processing
          endTimeData <- Sys.time()
          
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
                  # todo: use clime to generate UG, make sure it translates to Tetrad
                } else if (specificAlgorithms %in% c("mb", "mb_ges", "mb_fci")){
                  # todo: use MB estimator to generate UG, make sure it translates to Tetrad
                }
                
                # translate adjacency matrix in R into edge list graph in Tetrad
                newug_tetrad <- ugraphToTetradGraph(ugmat)
                newug_tetrad <- .jcall("edu/cmu/tetrad/graph/GraphUtils", 
                                       "Ledu/cmu/tetrad/graph/Graph;", 
                                       "replaceNodes", newug_tetrad, node_list)
              }
              
              # UG to GES
              if (learningMethod %in% c("glasso_ges", "clime_ges", "mb_ges")){
                # feed UG to GES
              }
              
              # UG to FCI
              if (learningMethod %in% c("glasso_fci", "clime_fci", "mb_fci")){
                # start FCI from given UG
                newfci_tetrad <- .jnew("edu/cmu/tetrad/graph/EdgeListGraphSingleConnections", 
                                       newug_tetrad)
                newfci_tetrad <- .jcast(newfci_tetrad, "edu/cmu/tetrad/graph/Graph", check=TRUE)
                nullsepsets <- .jnull(class = "edu/cmu/tetrad/search/SepsetMap")
                
                # sepsets is created with newug_tetrad (analogous to gesGraph, as is 
                # done in line 240 of GFci), rather than newfci_tetrad.
                sepsets <- .jnew("edu/cmu/tetrad/search/SepsetsMaxPValue",newug_tetrad, 
                                 newindtest_fisherz, nullsepsets, as.integer(3))
                
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
                newfci_tetrad <- .jcast(newfci_tetrad, "edu/cmu/tetrad/graph/Graph")
              }
              
              ################################################################################
              # Learn graph: non-UG methods (PC, GES, FCI, GFCI)
              
              # PC
              if (learningMethod == "pc"){
                # run PC
              }
              
              # GES
              if (learningMethod == "ges"){
                # run GES
              }
              
              # FCI
              if (learningMethod == "fci"){
                # run FCI
              }
              
              # GFCI
              if (learningMethod == "gfci"){
                # run GFCI
              }
              
              
              ################################################################################
              # evaluation
              # todo: go through all this in detail and make sure that the
              # various graph manipulations aren't changing the structure. 
              
              if (numLatentConfounders == 0){
                # get true CPDAG in Tetrad
              } else if (numLatentConfounders > 0){
                # get PAG of true generating DAG with latents in Tetrad
                dagtopag <- .jnew("edu/cmu/tetrad/search/DagToPag", newgraph)
                truepag <- dagtopag$convert()
                truepag <- .jcast(truepag, "edu/cmu/tetrad/graph/Graph")
              }
              
              # eval UG methods
              if (learningMethod == "ug"){
                if (numLatentConfounders == 0) {
                  # eval causally sufficient UG and write out
                } else if (numLatentConfounders > 0) {
                  # eval causally insufficient UG and write out
                }
              }
              
              
              
              
              # edge confusion matrix:
              newgraphutils <- .jnew("edu/cmu/tetrad/graph/GraphUtils")
              nulloutput <- .jnull("java/io/PrintStream")
              result_counts <- newgraphutils$edgeMisclassificationCounts(truepag, newfci_tetrad, nulloutput)
              result_string <- newgraphutils$edgeMisclassifications(result_counts)
              cat(result_string)
              
              # adjacency errors:
              newGraphComparison <- .jcall("edu/cmu/tetrad/search/SearchGraphUtils", 
                                           "Ledu/cmu/tetrad/graph/GraphUtils$GraphComparison;", 
                                           "getGraphComparison", newfci_tetrad, truepag)
              
              # not sure what these do:
              #newGraphComparison$getArrowptCorrect()
              #newGraphComparison$getArrowptFp()
              #newGraphComparison$getArrowptFn()
              
              ################################################################################
              # results into R, save to file
              
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
                                    newfci_tetrad$getNumEdges(),
                                    truepag$getNumEdges())
              names(adjacency_result) <- c("AdjFp", "AdjFn", "AdjCorrect", "EstEdges", "TrueEdges")
              
              #write out
              allresults <- c(adjacency_result, edge_results_vector)
              if ("!"(file.exists(output_filename))){
                write(names(allresults), output_filename, ncolumns = length(allresults), sep="\t")
              }
              write(allresults, file=output_filename, ncolumns=length(allresults), sep="\t", append=TRUE)
            }
          }
        }
      }
    }
  }
}
