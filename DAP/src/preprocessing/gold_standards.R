setwd("~/Dissertation_code/DAP/Data/")

# gold standard 1: xu et al
chipx1 <- read.table("Raw/xu2013escape_datasets/chip_x.txt", sep="\t", 
                     header = TRUE, stringsAsFactors = FALSE)
names(chipx1)[c(1,3)] <- c("source", "target")

logof1 <- read.table("Raw/xu2013escape_datasets/logof.txt", sep="\t", 
                     header = TRUE, stringsAsFactors = FALSE)
logof1 <- logof1[logof1$cellType=="MESC",]
names(logof1)[c(1,3)] <- c("source", "target")

# gold standard 2: correa-cerro et al
logof2 <- read.table(paste0("Raw/correa2011generation_suppl/", 
                            "GSE31381_processed-data_Table_S2.txt"), 
                     sep="\t", header = TRUE, stringsAsFactors = FALSE)
names(logof2)[c(1,2)] <- c("source", "target")

# gold standard 3: nishiyama et al
logof3 <- read.table("Raw/nishiyama2013suppl/srep01390-s1.csv", sep=",", 
                     header = TRUE, stringsAsFactors = FALSE)
names(logof3)[c(1,2)] <- c("source", "target")

# gold standard 4: regulatorynetworks.org
chipx2 <- read.table(paste0("Raw/buffer.5000.mm9-120313/mCJ7-DS13320/",
                            "genes.regulate.genes"), sep="\t", 
                     stringsAsFactors = FALSE)
chipx3 <- read.table(paste0("Raw/buffer.5000.mm9-120313/ZhBTc4-DS15236/",
                            "genes.regulate.genes"), sep="\t", 
                     stringsAsFactors = FALSE)
chipx4 <- read.table(paste0("Raw/buffer.5000.mm9-120313/ZhBTc4-DS17562/",
                            "genes.regulate.genes"), sep="\t", 
                     stringsAsFactors = FALSE)
chipx5 <- read.table(paste0("Raw/buffer.5000.mm9-120313/ZhBTc4-DS17616/",
                            "genes.regulate.genes"), sep="\t", 
                     stringsAsFactors = FALSE)
chipx2 <- rbind(chipx2, chipx3, chipx4, chipx5)
names(chipx2) <- c("chrom", "start", "stop", "target", "source", "evidence")

# combine into chip-x and logof gold standards
chipx <- rbind(chipx1[, c("source", "target")], 
               chipx2[, c("source", "target")])
logof <- rbind(logof1[, c("source", "target")], 
               logof2[, c("source", "target")], 
               logof3[, c("source", "target")])

# make lower case
chipx$source <- tolower(chipx$source)
chipx$target <- tolower(chipx$target)
logof$source <- tolower(logof$source)
logof$target <- tolower(logof$target)

# remove duplicates
chipx <- chipx["!"(duplicated(chipx)),]
logof <- logof["!"(duplicated(logof)),]

# remove autoregulation
chipx <- chipx[chipx$source != chipx$target, ]
logof <- logof[logof$source != logof$target, ]

# write to file
write.csv(chipx, "Processed/chipx.csv", quote=F, row.names=F)
write.csv(logof, "Processed/logof.csv", quote=F, row.names=F)
