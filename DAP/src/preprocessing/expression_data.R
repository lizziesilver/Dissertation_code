setwd("~/Dissertation_code/DAP/Data/")

# load the huge package, which has the SKEPTIC
library(huge)

################################################################################
# load sample of 933 cells, 24175 genes
mus1 <- read.csv("Raw/GSE65525_RAW/GSM1599494_ES_d0_main.csv", header=FALSE)
row.names(mus1) <- tolower(mus1[,1])
mus1 <- mus1[,2:ncol(mus1)]
mus1 <- t(mus1)
row.names(mus1) <- NULL

# load gene subset list
gene_subset <- readLines("Processed/gene_subset.txt")

# subset data by selected genes
mus_subset_data <- mus1[, colnames(mus1) %in% gene_subset]
rm(mus1)
mus_subset_data <- as.data.frame(mus_subset_data)
write.csv(mus_subset_data, "Processed/mus_subset_data.csv", row.names=F)

################################################################################
# make correlation matrix: Pearson correlation
sample_size <- nrow(mus_subset_data)
cor_pearson_subset <- cor(mus_subset_data)
# create the header that Tetrad needs: sample size and col names
write(sample_size, "Processed/cor_pearson_subset_with_header.txt", append=TRUE)
write(gene_subset, "Processed/cor_pearson_subset_with_header.txt", 
      ncolumns=length(gene_subset), sep="\t", append=TRUE)
# Append the correlation matrix to file
write(cor_pearson_subset, "Processed/cor_pearson_subset_with_header.txt", 
      ncolumns=length(gene_subset), sep="\t", append=TRUE)

# make correlation matrix: SKEPTIC estimator of correlation
cor_skeptic_subset <- huge.npn(mus_subset_data, npn.func = "skeptic")
# create the header that Tetrad needs: sample size and col names
write(sample_size, "Processed/cor_skeptic_subset_with_header.txt", append=TRUE)
write(gene_subset, "Processed/cor_skeptic_subset_with_header.txt", 
      ncolumns=length(gene_subset), sep="\t", append=TRUE)
# Append the correlation matrix to file
write(cor_skeptic_subset, "Processed/cor_skeptic_subset_with_header.txt", 
      ncolumns=length(gene_subset), sep="\t", append=TRUE)

# ################################################################################
# # make a pretty plot
# pic <- which(colnames(mus_subset_data) %in% c("stmn2", "tyw1", "hspb1", "atp6v0e"))
# pairs(mus_subset_data[,pic], pch=16, col=rgb(0, 0, 0, 0.03), cex.labels = 1)