args = commandArgs(trailingOnly=TRUE)
#setwd("~/Dissertation_code/DAP/Data/")
setwd(args[1])

# What is the maximum number of genes we want to include?
max_genes <- 500

# If the top variable genes don't include many TFs, how many TFs is enough?
min_tfs <- 120

# Variable genes in gene expression dataset:
vargenes <- read.csv("Raw/mmc3-sheet1-variable-genes-in-es-cells.csv", 
                     stringsAsFactors=F)
vargenes$Gene.symbol <- tolower(vargenes$Gene.symbol)

# # check that ordering by q value is the same as ordering by v score
# all.equal(vargenes[order(vargenes$q.value..BH.FDR.corrected.p.value.), 
#                    "Gene.symbol"],
#           vargenes[order(vargenes$v.score, decreasing = T), "Gene.symbol"])
# # [1] TRUE

# list of TFs
tf_list <- readLines("Processed/tf_list.txt")

# which of the top 2047 variable genes are TFs?
vargenes$TF <- vargenes$Gene.symbol %in% tf_list

# First check whether the top variable genes already contain enough TFs:
if (sum(vargenes$TF[1:max_genes]) >= min_tfs) {
  genes_included <- vargenes$Gene.symbol[1:max_genes]
} else {
  # If they don't, we first select our TFs, then pull enough of the variable
  # genes to make up the numbers:
  tfs_included <- which(vargenes$TF)[1:min_tfs]
  non_tfs_included <- which(!vargenes$TF)[1:(max_genes - min_tfs)]
  genes_included <- vargenes$Gene.symbol[c(tfs_included, non_tfs_included)]
}

# write to file
write(genes_included, file="Processed/gene_subset.txt")


