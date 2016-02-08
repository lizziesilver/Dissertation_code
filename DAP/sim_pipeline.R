################################################################################
# set up 

# allocate 4 gigs of memory to java
options( java.parameters = "-Xmx4g" )

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
source(path_to_tetrad_utils)
source(path_to_simulation_utils)

# initialize JVM, add all necessary jars to the classpath
.jinit(path_to_tetrad_jar) 
for (pat in other_tetrad_paths){
  .jaddClassPath(pat)
}

# check you're running the right version of Java
checkJavaVersion(version="1.8")

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
        
        for (m in 1:length(graphGenMethodVec)){
          graphGenMethod <- graphGenMethodVec[m]
          
          ######################################################################
          # Generate DAGs using Tetrad
          startTimeGraphGen <- Sys.time()
          node_list <- create_nodes(numNodes)
          newgraph <- create_graph(node_list, graphGenMethod, 
                                   numLatentConfounders, maxNumEdges, maxDegree, 
                                   maxIndegree, maxOutdegree, connected, alpha, 
                                   beta, delta_in, delta_out)
          endTimeGraphGen <- Sys.time()
          graphGenTime <- difftime(endTimeGraphGen, startTimeGraphGen, 
                                   units="secs")
          
          degree_stats <- get_degree_stats(newgraph, node_list, numNodes, 
                                           numLatentConfounders)
          
          ######################################################################
          # get true PAG or CPDAG for evaluation phase
          startTimeDagToPag <- Sys.time()
          trueCpdagOrPag <- get_true_graph(newgraph, numLatentConfounders, 
                                           maxPathLength)
          endTimeDagToPag <- Sys.time()
          dagToCpdagOrPagTime <- difftime(endTimeDagToPag, startTimeDagToPag, 
                                          units="secs")
          
          # get true UG (moralize the true CPDAG or PAG)
          startTimeMoral <- Sys.time()
          trueug <- get_true_ug(trueCpdagOrPag)
          endTimeMoral <- Sys.time()
          moralizeTime <- difftime(endTimeMoral, startTimeMoral, units="secs") 
          
          # remove latent nodes from node_list if they exist
          latent_nodes <- .jcall("edu/cmu/tetrad/graph/GraphUtils", 
                                 "Ljava/util/List;", "getLatents", newgraph)
          node_list$removeAll(latent_nodes)
          
          ######################################################################
          # parameterize DAGs using Tetrad
          newsem <- .jnew("edu/cmu/tetrad/sem/LargeSemSimulator", newgraph)
          
          # start loop over sample sizes
          sampleSizeVec <- setSampleSize(numNodes)
          for (l in 1:length(sampleSizeVec)){
            sample_size <- sampleSizeVec[l]
            
            # generate data using Tetrad
            newdata <- .jcall(newsem, "Ledu/cmu/tetrad/data/DataSet;", 
                              "simulateDataAcyclic", as.integer(sample_size))
            
            ####################################################################
            # load data into R
            rdata <- read.table(text=newdata$toString(), header=TRUE)
            
            # if latent nodes exist, remove them from R dataset
            rdata <- remove_latents(rdata, latent_nodes)
            
            # make sure data columns are ordered same as node_list
            varnames <- strsplit(gsub("\\[|\\]", "", node_list$toString()), 
                                 split=", ")[[1]]
            rdata <- rdata[,varnames]
            
            # SKEPTIC recovers cov matrix in R
            skeptic_cor <- huge.npn(rdata, npn.func="skeptic", npn.thresh=NULL, 
                                    verbose=TRUE)
            
            # compare with correlation matrix of untransformed data
            true_cor <- cor(rdata)
            
            # compare with correlation matrices of data with transformations
            npn_cor_1 <- cor(tanh(rdata))
            npn_cor_2 <- cor(rdata^3 + 0.5*rdata)
            npn_cor_3 <- cor(log(rdata - min(rdata) + 0.00001))
            
            cor_list <- list(true_cor=true_cor, skeptic_cor=skeptic_cor,
                             npn_cor_1=npn_cor_1, npn_cor_2=npn_cor_2,
                             npn_cor_3=npn_cor_3)
            
            cor_max <- sapply(cor_list, cor_compare, true_cor, "max")
            cor_mse <- sapply(cor_list, cor_compare, true_cor, "mse")
            names(cor_max) <- paste(names(cor_max), "_max_error", sep="")
            names(cor_mse) <- paste(names(cor_mse), "_mse", sep="")
            
            for (h in 1:length(cor_list)){
              cormat <- cor_list[[h]]
              cormethod <- names(cor_list[h])
              
              # pass skeptic cov mat back into Tetrad, for use with ind test
              newcovmat <- rCovMatrix2TetradCovMatrix(cormat, node_list, 
                                                      sample_size)
              newindtest_fisherz <- ind_test_fisherz(newcovmat, alpha=0.05)
              
              ##################################################################
              # Set/loop over learning methods
              learningMethodVec <- setLearningMethod(numLatentConfounders)
              for (n in 1:length(learningMethodVec)){
                learningMethod <- learningMethodVec[n]
                
                specificAlgorithmsVec <- setSpecificAlgorithms(learningMethod)
                for (p in 1:length(specificAlgorithmsVec)){
                  specificAlgorithms <- specificAlgorithmsVec[p]
                  
                  ##############################################################
                  startTimeAlg <- Sys.time()
                  # learn graphs: UGs and methods starting from UGs
                  
                  # UG learners
                  if (learningMethod %in% c("ug", "ug_to_cpdag", "ug_to_pag")){
                    newug_tetrad <- LearnUg(specificAlgorithms, 
                                            cormat, node_list, 
                                            lambda=NULL)
                  }
                  
                  # UG to GES. todo: ask Peter about how to do this
                  if (specificAlgorithms %in% c("glasso_ges", "clime_ges", 
                                                "mb_ges")){
                  }
                  
                  # UG to FCI
                  if (specificAlgorithms %in% c("glasso_fci", "clime_fci", 
                                                "mb_fci")){
                    learnedgraph <- ug_fci(newug_tetrad, newindtest_fisherz, 
                                           maxPathLength)
                  }
                  
                  ##############################################################
                  # Learn graph: non-UG methods (PC, GES, FCI, GFCI)
                  if (specificAlgorithms %in% c("pc", "ges", "fci", "gfci")){
                    learnedgraph <- LearnDirected(specificAlgorithms, 
                                                  newindtest_fisherz, 
                                                  newcovmat, maxPathLength)
                  }
                  
                  endTimeAlg <- Sys.time()
                  algTime <- difftime(endTimeAlg, startTimeAlg, units="secs") 
                  
                  ##############################################################
                  # evaluation
                  if (learningMethod=="ug"){
                    learnedgraph <- .jcast(newug_tetrad, 
                                           "edu/cmu/tetrad/graph/Graph", 
                                           check=TRUE)
                    truegraph <- trueug
                  } else {
                    truegraph <- trueCpdagOrPag
                  }
                  results <- GetResults(truegraph, learnedgraph)
                  
                  ##############################################################
                  # results into R, save to file
                  timeComplete <- format(Sys.time(), "%a %b %d %X %Y")
                  
                  searchparams <- c(timeComplete, numNodes, maxDegree, 
                                    maxIndegree, maxOutdegree,  
                                    numLatentConfounders, maxNumEdges, 
                                    graphGenMethod, sample_size, cormethod,
                                    cor_max[h], cor_mse[h],
                                    learningMethod, 
                                    specificAlgorithms, graphGenTime, 
                                    moralizeTime, dagToCpdagOrPagTime, algTime, 
                                    degree_stats)
                  names(searchparams) <- c("timeComplete", "numNodes", 
                                           "maxDegree", "maxIndegree", 
                                           "maxOutdegree", 
                                           "numLatentConfounders", 
                                           "maxNumEdges", "graphGenMethod", 
                                           "sample_size", "corMethod",
                                           "corMaxError", "corMSE",
                                           "learningMethod", 
                                           "specificAlgorithms", "graphGenTime", 
                                           "moralizeTime", 
                                           "dagToCpdagOrPagTime", 
                                           "algTime", names(degree_stats))
                  
                  #write out
                  allresults <- c(searchparams, results)
                  if ("!"(file.exists(output_filename))){
                    write(names(allresults), file=output_filename, 
                          ncolumns=length(allresults), sep="\t")
                  }
                  write(allresults, file=output_filename, 
                        ncolumns=length(allresults), sep="\t", append=TRUE)
                }
              }
            }
          }
        }
      }
    }
  }
}