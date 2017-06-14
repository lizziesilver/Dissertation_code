args = commandArgs(trailingOnly=TRUE)
#setwd("~/Dissertation_code/DAP/Data/")
setwd(args[1])

# load TFs and gene subset
tfs <- readLines("Processed/MESCs/tf_list.txt") 
gene_subset <- readLines("Processed/MESCs/gene_subset.txt")

# add TFs to first tier
first_tier <- intersect(gene_subset, tfs)
first_tier <- paste(first_tier, collapse=" ")

# add other genes to second tier
second_tier <- setdiff(gene_subset, tfs)
second_tier <- paste(second_tier, collapse=" ")

# the asterisk means edges within the second tier are forbidden
knowledge <- paste0("/knowledge\naddtemporal\n\n1 ",
                    first_tier, " \n2* ", second_tier,
                    " \n\nforbiddirect\n\nrequiredirect\n\n\n")

# write to file
write(knowledge, "Processed/MESCs/knowledge_tetrad_format_subset.txt")
