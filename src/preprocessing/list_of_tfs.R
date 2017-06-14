args = commandArgs(trailingOnly=TRUE)
#setwd("~/Dissertation_code/DAP/Data/")
setwd(args[1])

# list of TFs from Gene Ontology
tfs <- readLines("Raw/MESCs/GeneOntology/GO_transcription_factors.txt")
tfs <- strsplit(tfs, split="\t")
# not all genes have synonyms, so some lines of the file are too short:
lengths <- sapply(tfs, length)
for (i in 1:length(lengths)){
  if (lengths[i]==3) tfs[[i]] <- c(tfs[[i]], "")
}
tfs <- matrix(unlist(tfs), ncol=4, byrow=TRUE)
colnames(tfs) <- c("Name", "Description", "ID", "Synonyms")
tfs <- data.frame(tfs, stringsAsFactors=F)

# list of chromatin modifiers from Gene Ontology
chrom <- read.table("Raw/MESCs/GeneOntology/GO_chromatin_modifiers.txt", sep="\t", stringsAsFactors=F)
names(chrom) <- c("Name", "Description", "ID", "Synonyms")

# list of regulators from gold standards
chipx <- read.csv("Processed/MESCs/chipx.csv", stringsAsFactors=F, header=T)
logof <- read.csv("Processed/MESCs/logof.csv", stringsAsFactors=F, header=T)

# combine into one list of TFs and chromatin modifiers
tf_list <- unique(c(chipx$source, 
                    logof$source, 
                    tolower(chrom$Name), 
                    tolower(tfs$Name)))

# write to file
writeLines(tf_list, "Processed/MESCs/tf_list.txt")

