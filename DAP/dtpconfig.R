# configuration file for simulation  study 

# input and output files
path_to_tetrad_jar = "/Users/lizzie/Dissertation_code/DAP/tetrad-lib-5.3.0-SNAPSHOT-tetradcmd.jar"
path_to_utils = "/Users/lizzie/Dissertation_code/R-Tetrad_interface/tetrad_utils.R"
other_tetrad_paths = c("/Users/lizzie/Dissertation_code/DAP/tetrad-gui-5.3.0-SNAPSHOT.jar")
output_filename = "/Users/lizzie/Dissertation_code/DAP/dagToPag_timing_results.txt"

# the methods for setting parameters return vectors of options, which we iterate over

# graph generation method will vary independently of the other things
graphGenMethodVec = c("uniform")
#numNodesVec = c(50, 500, 5000)
#numNodesVec = c(4,5,6,7,8,9,10,11,12,13,14,15)
#numNodesVec = c(16, 20, 30, 40, 50, 75)
numNodesVec = 75

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
  numlats <- unique(ceiling(seq(from=1, to=0.3*numNodes, length.out=5)))
  return(numlats)#, ceiling(0.11*numNodes)))
}
setMaxNumEdges = function(numNodes){
  upperlimit <- min(0.5*numNodes*(numNodes - 1), numNodes*2)
  length_out <- min(upperlimit - numNodes + 1, 4)
  return(floor(seq(from=numNodes, to=upperlimit, length.out=length_out)))
}

# these features return only a single value
setMaxDegree = function(numNodes){
  return(unique(c(#min(numNodes - 1, 3), 
                  min(numNodes - 1, 4), 
                  min(numNodes - 1, 6), 
                  numNodes - 1)))
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