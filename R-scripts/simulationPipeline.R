# Simulation pipeline:

# load required libraries
library(graph)
library(RBGL)
#library(gRbase)
library(pcalg)
library(stringr)
# library(dplyr)
# load configuration file
source("simulation_config.R")
setwd("/Users/lizzie/Dissertation_code/R-scripts")

################################################################################
# Load unique ids of last run, if this is not the first run:
if (file.exists(last_id_log_file)){
	last_run_ids = read.table(last_id_log_file, header=TRUE, sep=",")
} else {
	last_run_ids = data.frame("run_id"=0, "progenitor_graph_id"=0, "evolved_graph_id"=0, 
 							  "dataset_id"=0, "pooled_dataset_id"=0, "search_id"=0)
}
new_run_ids = last_run_ids + 1

################################################################################
# file to log every run of the pipeline
# should contain: run id, date, list of progenitor graph ids, list of evolved graph ids, list of dataset ids, list of pooled data ids, 
#				  list of search ids, list of evaluation set ids, start time, end time, duration
sim_run_log_file = paste("sim_run_log_", as.character(new_run_ids$run_id), ".txt", sep="")


################################################################################
# Graph generation:
# 	Generate small number of random graphs, the progenitors. 
# 		Choices: number of nodes per graph, number of edges per graph (or connection probability)
# 		Log: features of graphs, plus a unique id, plus params of generation process
# 		Save graph object
pgid = new_run_ids$progenitor_graph_id
progenitor <- randomDAG(num_nodes, prob_edges)
# save and log graph here


################################################################################
# 	Take progenitor and evolve it for a few steps into a descendant. Repeat. 
# 		Choices: number of descendants, number of steps of evolution.
# 		Log: features of graphs, plus a unique id, plus id of progenitor, plus params of evolution process, plus specific modifications made
# 		Save graph object
# 		At end of repeat: remove progenitor from memory

evolved_list = list()
for (i in 1:num_evolved){
	evolved_list[[i]] <- evolveDAG(num_mods, progenitor)
}
# save and log evolved graphs here


################################################################################
# 	Generate data from each descendant graph. 
# 		Choices: parameterization, number of data points to generate.
# 		Log: data filename, number of rows and columns, params of data generation, id of generator graph
# 		Save data
# 		At end of each data generation: remove graph from memory

data_list = list()
for (i in 1:num_evolved){
	if (parameterization=="linear_gaussian"){
		evolved_list[[i]] <- dag2gausspardag(evolved_list[[i]])
	} else {
		cat("error: parameterization not listed or not recognized\n")
		break
	}
	data_list[[i]] <- rmvnorm.ivent(sample_size, evolved_list[[i]])
}
# save and log datasets here
# rm(evolved_list)


################################################################################
# Data pooling:

# 	Pool data from graphs descended from the same progenitor. 
# 		Choices: amount of data from each graph to include; number of graphs to include
# 		Log: unique id for pooled dataset, plus the unique dataset IDs and row numbers from the source data that were used in pooling, 

vars = colnames(data_list[[1]])
rows_included_list = list()
for (i in 1: num_evolved){
	vars = intersect(vars, colnames(data_list[[i]]))
	rows_included = sample(pooled_sample_sizes[i])
	rows_included_list[[i]] <- 1:sample_size %in% rows_included
}
# save and log the rows included and the vars included 
pool_data_list <- list()
for (i in 1: num_evolved){
	pool_data_list[[i]] <- subset(data_list[[i]], subset=rows_included_list[[i]], select=vars)
}
pool_data_frame = do.call(rbind, pool_data_list)
write.table(pool_data_frame,file="pooldata.txt", sep="\t")

################################################################################

# Graph search:

# 	Choose one graph from the descendants in each pool to be the "target". 
# 		Choice: which graph? Or repeat for all graphs?
# 		Log: choice of target

# 	Run search algorithm on the pooled data
# 		Save recovered graph object, and keep in memory for evaluation stage
# 		Log: unique id for recovered graph, id of dataset, id of algorithm and all parameters

library(rJava)
.jinit("/Users/lizzie/Dissertation_code/Tetrad-jars/tetrad-5.2.1-3.jar") 
# here it's annoying that I had to write the data to file and read it back in.
filename = .jnew("java/lang/String", "pooldata.txt")
datafile = .jnew("java/io/File", filename)
reader = .jnew("edu/cmu/tetrad/data/DataReader")
delim = J("edu/cmu/tetrad/data/DelimiterType")
reader$setDelimiter(delim$TAB)
dataset = .jcall(reader, "Ledu/cmu/tetrad/data/DataSet;", "parseTabular", datafile)

# trying to create the dataset directly ... this doesn't work yet. 
mt = matrix(head(pool_data_frame), ncol = ncol(pool_data_frame))
afg <- .jarray(mt, dispatch=TRUE)
.jevalArray(afg, simplify=TRUE)
.jnew("edu/cmu/tetrad/data/ColtDataSet", .jnew("I", 6)(int rows, List<Node> variables, double[][] data)
.jnew("edu/cmu/tetrad/data/ColtDataSet", 6, List<Node> variables, double[][] data)


gesinstance = .jnew("edu/cmu/tetrad/search/FastGes", dataset)
.jcall(gesinstance, "V", "setPenaltyDiscount", 1.0)
.jcall(gesinstance, "V", "setSamplePrior", 10.0)
.jcall(gesinstance, "V", "setStructurePrior", 1.0)
resultGraph = .jcall(gesinstance, "Ledu/cmu/tetrad/graph/Graph;", "search")

out_graph = tetradPattern2graphNEL(resultGraph)



 
# 	Run search algorithm on data from individual target graph:
# 	(1) using just the rows that went into the pooled data
# 		Save recovered graph object, and keep in memory for evaluation stage
# 		Log: unique id for recovered graph, id of dataset, id of algorithm and all parameters
# 	(2) using a superset, including the pooled data but also additional rows, so that the sample size is equal to the sample size of the pooled data.
# 		Save recovered graph object, and keep in memory for evaluation stage
# 		Log: unique id for recovered graph, id of dataset, id of algorithm and all parameters


# Evaluation:
