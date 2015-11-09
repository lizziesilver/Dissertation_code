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

