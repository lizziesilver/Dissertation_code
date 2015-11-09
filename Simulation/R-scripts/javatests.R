# Using the command line:
cmd <- "java -jar my/path/.jar" 
system(cmd, wait = FALSE)

# Set up
#setwd("wherever/you/put/the/data/")
library(rJava)
#.jinit("path/to/tetrad-5.2.1-3.jar")
.jinit("/Users/lizzie/Dissertation_code/Tetrad-jars/tetrad-5.2.1-3.jar") # this starts the JVM
setwd("/Users/lizzie/Dissertation_code/R-scripts")

# All of this works:
filename = .jnew("java/lang/String", "charity.txt")
datafile = .jnew("java/io/File", filename)
reader = .jnew("edu/cmu/tetrad/data/DataReader")
delim = J("edu/cmu/tetrad/data/DelimiterType")
.jcall(reader, "V", "setDelimiter", delim$TAB)
dataset = .jcall(reader, "Ledu/cmu/tetrad/data/DataSet;", "parseTabular", datafile)
gesinstance = .jnew("edu/cmu/tetrad/search/Ges", dataset)

# # obsolete
# dataset = reader$parseTabular(datafile)
# reader = .jcall(reader, "Ledu/cmu/tetrad/data/DataReader;", "setDelimiter", delim$TAB)


# Using rJava:
setwd("/Users/lizzie/Dissertation_code/Tetrad-jars/")
library(rJava)
.jinit("/Users/lizzie/Dissertation_code/Tetrad-jars/tetrad-5.2.1-3.jar") # this starts the JVM

# Check the JRE is version 1.8, or the same as was used to compile the jar:
.jcall("java/lang/System", "S", "getProperty", "java.runtime.version")

# Now create an empty DAG:
jobject <- .jnew("edu/cmu/tetrad/graph/Dag")  ## call the constructor
#.jcall(jobject ,"I",method="YOUR_METHOD") ## call a method
#.jcall(object,"Ljava/lang/String;" return class,"concat" method, argument)
.jmethods(jobject)

# Now we want to run GES.
# First, what does GES need as input? A dataset or a covariance matrix. Let's try with a dataset.
# Create a new dataset:
# Create a new GES instance:
# Call search on the GES instance:

# File file = new File(dataFileName);
setwd("/Users/lizzie/Dissertation_code/R-scripts")
filename = .jnew("java/lang/String", "charity.txt")
datafile = .jnew("java/io/File", filename)

# DataReader reader = new DataReader();
# reader = .jnew("/Users/lizzie/tetrad/svn/src/edu/cmu/tetrad/data/DataReader")
reader = .jnew("edu/cmu/tetrad/data/DataReader")

# if (whitespace) {
    # reader.setDelimiter(DelimiterType.WHITESPACE);
# } else {
    # reader.setDelimiter(DelimiterType.TAB);
# }
delim = J("edu/cmu/tetrad/data/DelimiterType")
reader$setDelimiter(delim$TAB)
bob = reader$parseTabular(datafile)

# reader.setMaxIntegralDiscrete(Integer.MAX_VALUE);

# if (useCovariance) {
    # ICovarianceMatrix cov = reader.parseCovariance(file);
    # this.covarianceMatrix = cov;
# } else {
    # DataSet data = reader.parseTabular(file);
    # out.println("# variables = " + data.getNumColumns() +
                   # ", # cases = " + data.getNumRows());
    # this.data = data;
# }

filename = .jnew("java/lang/String", "charity.txt")
datafile = .jnew("java/io/File", filename)
reader = .jnew("edu/cmu/tetrad/data/DataReader")
delim = J("edu/cmu/tetrad/data/DelimiterType")
reader$setDelimiter(delim$TAB)
dataset = .jcall(reader, "Ledu/cmu/tetrad/data/DataSet;", "parseTabular", datafile)

# ges = new FastGes(new SemBicScore(new CovarianceMatrixOnTheFly(data)));
gesinstance = .jnew("edu/cmu/tetrad/search/FastGes", dataset)

    # private double samplePrior = 10.0;
    # private double structurePrior = 1.0;
    # private double penaltyDiscount = 1.0;
    # ges.setPenaltyDiscount(penaltyDiscount);
    # ges.setSamplePrior(samplePrior);
    # ges.setStructurePrior(structurePrior);

.jcall(gesinstance, "V", "setPenaltyDiscount", 1.0)
.jcall(gesinstance, "V", "setSamplePrior", 10.0)
.jcall(gesinstance, "V", "setStructurePrior", 1.0)

# # Check this worked:
#.jcall(gesinstance, "D", "getPenaltyDiscount")

        # ges.setKnowledge(getKnowledge());

        # // Convert back to Graph..
        # Graph resultGraph = ges.search();

resultGraph = .jcall(gesinstance, "Ledu/cmu/tetrad/graph/Graph;", "search")

        # // PrintUtil outputStreamPath problem and graphs.
        # out.println("\nResult graph:");
        # out.println(resultGraph);

        # writeGraph(resultGraph);

# [2] "public java.lang.String edu.cmu.tetrad.graph.EdgeListGraphSingleConnections.toString()"
  # [5] "public edu.cmu.tetrad.graph.Node edu.cmu.tetrad.graph.EdgeListGraphSingleConnections.getNode(java.lang.String)"
# [6] "public boolean edu.cmu.tetrad.graph.EdgeListGraphSingleConnections.removeNode(edu.cmu.tetrad.graph.Node)"                                                                                                   
# [7] "public java.util.List edu.cmu.tetrad.graph.EdgeListGraphSingleConnections.getEdges(edu.cmu.tetrad.graph.Node,edu.cmu.tetrad.graph.Node)"                                                                    
# [8] "public synchronized java.util.List edu.cmu.tetrad.graph.EdgeListGraphSingleConnections.getEdges(edu.cmu.tetrad.graph.Node)"                                                                                 
  # [9] "public java.util.Set edu.cmu.tetrad.graph.EdgeListGraphSingleConnections.getEdges()"                                                                                                                        
 # [10] "public java.util.List edu.cmu.tetrad.graph.EdgeListGraphSingleConnections.getNodes()"                                                                                                                       
library(plyr)

nods = resultGraph$getNodes()
.jfields(resultGraph, name="Ljava/util/List", as.obj=TRUE)
#$toString()
#as.list(resultGraphStuff)

# Search in Tetrad
# Convert to R graph
# compare other methods

# here it's annoying that I had to write the data to file and read it back in.
filename = .jnew("java/lang/String", "pooldata.txt")
datafile = .jnew("java/io/File", filename)
reader = .jnew("edu/cmu/tetrad/data/DataReader")
delim = J("edu/cmu/tetrad/data/DelimiterType")
reader$setDelimiter(delim$TAB)
dataset = .jcall(reader, "Ledu/cmu/tetrad/data/DataSet;", "parseTabular", datafile)



# extract nodes: DONE
nods = resultGraph$getNodes()
# these two lines are equivalent - ought to time them and pick the faster:
sapply(as.list(nods), .jrcall, "toString")
sapply(as.list(nods), with, toString())

# extract edges
eds = resultGraph$getEdges()
sapply(as.list(eds), .jrcall, "toString")

new("graphNEL", nodes = V, edgeL = edL, edgemode = "directed")
.jmethods(resultGraph)
# need function for converting from all pcalg to all Tetrad graph types (and vice versa).
# Get nodes
# Get edges
# Get edge types
# Get graph class (PAG, MAG, CPDAG, etc.)
mt = matrix(c(0,2,3,1), nrow=2)
mat = .jnew("[[D", mt)
# # # Ilari's comment:
# # Good point, Elizabeth Silver - interesting - it looks like the $-style call returns a "jobjRef" with a jclass of its concrete class:
# # > dput(dataset)
# # new("jobjRef"
# # , jobj = <pointer: 0x00000000380102f8>
# # , jclass = "edu/cmu/tetrad/data/ColtDataSet"
# # )
# 
# # whereas the one from the .jcall where we gave it the return type gives that interface as its type:
# # new("jobjRef"
# # , jobj = <pointer: 0x000000003a036878>
# # , jclass = "edu/cmu/tetrad/data/DataSet"
# # )
# 
# # I didn't stumble through rJava dispatch logic much but a wild guess is that they're choosing the constructor without considering whether arguments are polymorphically "compatible" - that seems like an rJava bug to me.

