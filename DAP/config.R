# configuration file for simulation  study 

# input and output files
path_to_tetrad_jar = "/Users/lizzie/Dissertation_code/R-Tetrad_interface/tetrad-lib-5.3.0-SNAPSHOT-tetradcmd.jar"
path_to_utils = "/Users/lizzie/Dissertation_code/R-Tetrad_interface/tetrad_utils.R"
other_tetrad_paths = c("/Users/lizzie/Dissertation_code/R-Tetrad_interface/tetrad-gui-5.3.0-SNAPSHOT.jar")
output_filename = "/Users/lizzie/Dissertation_code/DAP/DAP_sim_results.txt"

# the methods for setting parameters return vectors of options, which we iterate over

# graph generation method will vary independently of the other things
graphGenMethodVec = c("forwardedges", "uniform")
#numNodesVec = c(50, 500, 5000)
numNodesVec = c(50, 500)

# learning method depends on whether there are latents or not
setLearningMethod = function(numLatentConfounders){
  if (numLatentConfounders==0) {
    return(c("ug", "cpdag"))#return(c("ug", "cpdag", "ug_to_cpdag"))
  } else if (numLatentConfounders > 0) {
    return(c("pag", "cpdag_to_pag", "ug_to_pag", "ug"))
  }
}

# algorithm that we can use depends on the learning type
setSpecificAlgorithms = function(learningMethod){
  if (learningMethod=="ug"){
    return(c("glasso", "mb")) #"clime", 
  } else if (learningMethod=="cpdag"){
    return(c("pc", "ges"))
  } else if (learningMethod=="ug_to_cpdag"){
    return(c("glasso_ges", "mb_ges")) #"clime_ges", 
  } else if (learningMethod=="pag"){
    return(c("fci"))
  } else if (learningMethod=="cpdag_to_pag"){
    return(c("gfci"))
  } else if (learningMethod=="ug_to_pag"){
    return(c("glasso_fci", "mb_fci"))#"clime_fci", 
  }
}

# All other features are functions of the number of nodes
setNumLatentConfounders = function(numNodes){
  #return(c(0, ceiling(0.01*numNodes)))#, ceiling(0.11*numNodes)))
  return(c(0, ceiling(0.01*numNodes), ceiling(0.11*numNodes)))
}
setMaxNumEdges = function(numNodes){
  return(c(1*numNodes, 2*numNodes))
}
setSampleSize = function(numNodes){
  if (numNodes==50){
    return(c(100, 1000)) # 2, 20 
  } else if (numNodes==500) {
    return(c(100, 1000)) # .2, 2
  } else if (numNodes==5000) {
    return(c(100, 1000, 10000)) # .02, .2, 2
  }
}

# these features return only a single value
setMaxDegree = function(numNodes){
  return(unique(c(#min(numNodes - 1, 3), 
    min(numNodes - 1, 3), 
    min(numNodes - 1, 5))))
}
setMaxIndegree = function(numNodes, maxDegree){
  return(min(numNodes - 1, maxDegree))
}
setMaxOutdegree = function(numNodes, maxDegree){
  return(min(numNodes - 1, maxDegree))
}
connected = FALSE

# for scale free graphs:
alpha = 0.05 #double  
beta = 0.05 # alpha + beta must be less than 1
delta_in = 0.05
delta_out = 3

# for clime:
nlambda=10
lambda.max=0.8
lambda.min=1e-4
lambda <- seq(lambda.min, lambda.max, length.out = nlambda)

# for DagToPag and FCI:
maxPathLength = as.integer(3)