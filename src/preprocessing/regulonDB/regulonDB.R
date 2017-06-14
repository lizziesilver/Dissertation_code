# go to data directory
setwd("~/Dissertation_code/RealData/Raw/Ecoli/RegulonDB")

# read in data:
# transcription factors (used for linking TF names to gene names)
tfs <- readLines("TFSet.txt")
tfs <- tfs[37:length(tfs)]
tfs <- strsplit(tfs, split="\t")
lengths <- sapply(tfs, length)
for (i in which(lengths==7)) {
  tfs[[i]] <- c(tfs[[i]], "")
}
tfs <- matrix(unlist(tfs), ncol=8, byrow=T)
tfs <- as.data.frame(tfs)
names(tfs) <- c("regulonDB.ID", "name", "gene", "active.conformations", 
                "inactive.conformations", "evidence.type", 
                "PMIDs", "evidence.level")
# tfs[,c("active.conformations", "inactive.conformations", "PMIDs")] <- NULL

# TF-gene interactions
tf.gene <- read.table("network_tf_gene.txt", sep="\t", header=F, 
                      stringsAsFactors=F)
names(tf.gene) <- c("tf", "gene", "effect", "evidence.type", 
                    "evidence.strength")
tf.gene[,6] <- NULL

# 









