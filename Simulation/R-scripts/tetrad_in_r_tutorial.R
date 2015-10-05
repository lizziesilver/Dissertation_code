################################################################################
# Interfacing Tetrad with R: a tutorial

# load required libraries
library(stringr)
library(graph)
library(RBGL)
library(rJava)

# set the paths to whatever they should be for your machine:
path_to_tetrad_jar = "/Users/lizzie/Dissertation_code/Tetrad-jars/tetrad-5.2.1-3.jar"
path_to_data = "/Users/lizzie/Dissertation_code/Simulation/R-scripts/charity.txt"
path_to_utils = "/Users/lizzie/Dissertation_code/Simulation/R-scripts/tetrad_utils.R"

# load the R functions we'll need from this source file
source(path_to_utils)

# read in some data (I'm using the Charity data for this example)
mydata = read.table(path_to_data, sep="\t", header=TRUE)

# start the Java Virtual Machine with the Tetrad jar:
.jinit(path_to_tetrad_jar) 

# check you're running the right Java version. This should say 1.8.something:
.jcall("java/lang/System", "S", "getProperty", "java.runtime.version")

# convert dataframe to tetrad dataset:
# (see function in other R file)
tetrad_data <- dataFrame2TetradDataset(mydata)

# initialize GES with your data:
ges_instance = .jnew("edu/cmu/tetrad/search/FastGes", tetrad_data)

# you can now tweak the parameters of GES if you want. For example:
#.jcall(gesinstance, "V", "setPenaltyDiscount", 1.0)
#.jcall(gesinstance, "V", "setSamplePrior", 10.0)
#.jcall(gesinstance, "V", "setStructurePrior", 1.0)

# search using GES:
tetrad_graph = .jcall(ges_instance, "Ledu/cmu/tetrad/graph/Graph;", "search")

# convert output of GES into an R object (graphNEL)
ges_graph = tetradPattern2graphNEL(tetrad_graph)

# you can now plot the graph in R:
plot(ges_graph)

# And you're done! 