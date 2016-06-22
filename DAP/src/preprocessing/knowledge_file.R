args = commandArgs(trailingOnly=TRUE)
#setwd("~/Dissertation_code/DAP/Data/")
setwd(args[1])

library(rJava)

# start jvm with Tetrad in the classpath
# path_to_tetrad_jar = paste0("/Users/lizzie/Dissertation_code/", 
#                             "R-Tetrad_interface/", 
#                             "tetrad-lib-5.3.0-SNAPSHOT-tetradcmd.jar")

path_to_tetrad_jar <- args[2]
.jinit(path_to_tetrad_jar)

# load TFs and gene subset
tfs <- readLines("Processed/tf_list.txt") 
gene_subset <- readLines("Processed/gene_subset.txt")

# create empty knowledge object
knowledge <- .jnew("edu/cmu/tetrad/data/Knowledge2")

# add TFs to first tier
first_tier <- intersect(gene_subset, tfs)
for (i in 1:length(first_tier)) {
  knowledge$addToTier(as.integer(1), first_tier[i])
}

# add other genes to second tier
second_tier <- setdiff(gene_subset, tfs)
for (i in 1:length(second_tier)) {
  knowledge$addToTier(as.integer(2), second_tier[i])
}

# forbid edges between non-TFs
knowledge$setTierForbiddenWithin(as.integer(2), TRUE)

# cast to IKnowledge
knowledge <- .jcast(knowledge, "edu/cmu/tetrad/data/IKnowledge", check=TRUE)

# write to file
write(knowledge$toString(), "Processed/knowledge_tetrad_format_subset.txt")

# Afterwards, I have to go and manually delete Tier 0 because for some reason 
# that gets added, but it makes Tetrad choke when it tries to read the file. :(
