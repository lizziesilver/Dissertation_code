################################################################################
# set up 
# load packages
library(stringr)
library(graph)
library(RBGL)
library(rJava)
library(huge)
library(clime)

# source config file
source("/Users/lizzie/Dissertation_code/DAP/dtpconfig.R")

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
  for (l in seq_along(maxDegreeVec)){
    maxDegree <- maxDegreeVec[l]
    maxIndegree <- setMaxIndegree(numNodes, maxDegree)
    maxOutdegree <- setMaxOutdegree(numNodes, maxDegree)
    
    numLatentConfoundersVec <- setNumLatentConfounders(numNodes)
    for (j in 1:length(numLatentConfoundersVec)){
      numLatentConfounders <- numLatentConfoundersVec[j]
      
      maxNumEdgesVec <- setMaxNumEdges(numNodes)
      for (k in 1:length(maxNumEdgesVec)){
        maxNumEdges <- maxNumEdgesVec[k]
        
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
          
          startTimeGraphGen <- Sys.time()
          
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
          graphGenTime <- endTimeGraphGen - startTimeGraphGen
          
          realNumEdges <- newgraph$getNumEdges()
          realMaxDegree <- .jcall("edu/cmu/tetrad/graph/GraphUtils", 
                                  "I", "getDegree", newgraph)
          
          ################################################################################
          # get true PAG, Pattern and UG for evaluation phase
          # todo: go through all this in detail and make sure that the
          # various graph manipulations aren't changing the structure. 
          
          if (numLatentConfounders == 0){
            # get true CPDAG in Tetrad
            truegraph <- .jcall("edu/cmu/tetrad/search/SearchGraphUtils",
                                "Ledu/cmu/tetrad/graph/Graph;", "patternForDag", newgraph)
            truegraph <- .jcast(truegraph, "edu/cmu/tetrad/graph/Graph", check=TRUE)
            
            # get true UG (moralize the true DAG)
            trueug <- .jcall("edu/cmu/tetrad/graph/GraphUtils", "Ledu/cmu/tetrad/graph/Graph;", "undirectedGraph", newgraph)
            colliders <- .jcall("edu/cmu/tetrad/graph/GraphUtils", "Ljava/util/LinkedList;", "listColliderTriples", newgraph)
            print(paste("Number of colliders:", length(as.list(colliders))))
            
          } else if (numLatentConfounders > 0){
            # get PAG of true generating DAG with latents in Tetrad
            startTimeDagToPag <- Sys.time()
            
            dagtopag <- .jnew("edu/cmu/tetrad/search/DagToPag", truegraph)
            dagtopag$setMaxPathLength(as.integer(maxPathLength))
            
            truegraph <- dagtopag$convert()
            
            endTimeDagToPag <- Sys.time()
            dagToPagTime <- endTimeDagToPag - startTimeDagToPag
            
            truegraph <- .jcast(truegraph, "edu/cmu/tetrad/graph/Graph", check = TRUE)
            # get true UG (moralize the true PAG)
#             startTimeMoralize <- Sys.time()
#             trueug <- .jcall("edu/cmu/tetrad/graph/GraphUtils", "Ledu/cmu/tetrad/graph/Graph;", "undirectedGraph", truegraph)
#             colliders <- .jcall("edu/cmu/tetrad/graph/GraphUtils", "Ljava/util/LinkedList;", "listColliderTriples", truegraph)
          }
          
          # now do the moralizing
          startTimeMoralize <- Sys.time()
          
#           colliders <- as.list(colliders)
#           for (colli in seq_along(colliders)){
#             X <- colliders[[colli]]$getX()
#             Z <- colliders[[colli]]$getZ()
#             if ("!"(newgraph$isAdjacentTo(X, Z))){
#               newedge <- .jnew("edu/cmu/tetrad/graph/Edge", .jcast(X, "edu/cmu/tetrad/graph/Node"), 
#                                .jcast(Z, "edu/cmu/tetrad/graph/Node"), .jnew("edu/cmu/tetrad/graph/Endpoint", "Tail")$TAIL,
#                                .jnew("edu/cmu/tetrad/graph/Endpoint", "Tail")$TAIL)
#               trueug$addEdge(newedge)
#             }
#           }
          
          trueug <- .jcall("edu/cmu/tetrad/graph/GraphUtils", "Ledu/cmu/tetrad/graph/Graph;", 
                           "undirectedMoralizedGraph", newgraph)
          
          trueug <- .jcast(trueug, "edu/cmu/tetrad/graph/Graph", check=TRUE)
          
          endTimeMoralize <- Sys.time()
          
          moralizeTime <- endTimeMoralize - startTimeMoralize
          
          
          ################################################################################
          # finish timing data generation & processing
          endTimeData <- Sys.time()
          
          dataTime <- endTimeData - startTimeData
          
          
          
          #write out
          allresults <- c(numNodes, maxDegree, maxIndegree, maxOutdegree, 
                          numLatentConfounders, maxNumEdges, graphGenMethod,
                          dataTime, dagToPagTime, realNumEdges, realMaxDegree,
                          graphGenTime, moralizeTime)
          names(allresults) <- c("numNodes", "maxDegree", "maxIndegree", "maxOutdegree", 
                                 "numLatentConfounders", "maxNumEdges", "graphGenMethod",
                                 "dataTime", "dagToPagTime", "realNumEdges", "realMaxDegree", 
                                 "graphGenTime", "moralizeTime")
            if ("!"(file.exists(output_filename))){
              write(names(allresults), file=output_filename, ncolumns = length(allresults), sep="\t")
            }
          write(allresults, file=output_filename, ncolumns=length(allresults), sep="\t", append=TRUE)
          
        }
      }
    }
  }
}
