################################################################################
# setup
library(stringr)
library(reshape2)
# library(dplyr)
setwd("~/Dissertation_code/RealData/Raw/Ecoli_and_Salmonella/")

################################################################################
# load salmonella expression data, put in matrix
sfile <- paste("COLOMBOS/meta_sente_compendium_data/", 
               "colombos_meta_sente_exprdata_20151030.txt", sep="")
salexpr <- readLines(sfile)
salexpr <- strsplit(salexpr, split="\t")
salexpr <- matrix(unlist(salexpr), ncol=1069, byrow=T)
salexpr.meta <- salexpr[c(1:6),]
rownames(salexpr.meta) <- salexpr.meta[, 3]
colnames(salexpr) <- salexpr[7,]
salexpr <- salexpr[-c(1:7),]

################################################################################
# load e coli expression data, put in matrix
efile <- "COLOMBOS/ecoli_compendium_data/colombos_ecoli_exprdata_20151029.txt"
ecoexpr <- readLines(efile)
ecoexpr <- strsplit(ecoexpr, split="\t")
ecoexpr <- matrix(unlist(ecoexpr), ncol=4080, byrow=T)
ecoexpr.meta <- ecoexpr[c(1:6),]
rownames(ecoexpr.meta) <- ecoexpr.meta[, 3]
colnames(ecoexpr) <- ecoexpr[7,]
ecoexpr <- ecoexpr[-c(1:7),]

################################################################################
# Match genes between species, make sure rows are in standard order
mfile <- paste("meysman2013expression_Supplementary_Data/", 
               "Supplemental_dataset_1_EC_score.csv", sep="")
genematch <- read.csv(mfile, stringsAsFactors=F)

# First extract Salmonella gene names. Because this is a cross-train compendium,
# the gene name column includes info about the name in several strains. We just
# need to capture the name for strain Salmonella enterica sl1344.
reg <- "STM[:digit:]{4}[^\\(\\<]*"
salgenes <- str_match_all(salexpr[,2], reg)
lengths <- sapply(salgenes, nrow)
shortgenes <- vector(mode="character", length=length(salgenes))
for (i in 1:length(salgenes)) {
  shortgenes[i] <- ifelse(lengths[i] == 1, salgenes[[i]][1, 1], "")
}
salrows <- match(genematch[, 2], shortgenes)
salexpr <- salexpr[salrows, ]
rownames(salexpr) <- genematch[, 2]

# E. coli is easier - just use the b-number
ecorows <- match(genematch[, 1], ecoexpr[, 1])
ecoexpr <- ecoexpr[ecorows, ]
rownames(ecoexpr) <- genematch[, 1]

################################################################################
# subset to large experiments that share contrast class

# E coli expts #1 & #2
e.expt1 <- which(ecoexpr.meta[3, ] == "ref_GSE6836_luc_0.075")
e.expt2 <- which(ecoexpr.meta[3, ] == 
                   "ref_GSE20305_MG1655_control_timepoint2_rep1")
ecoexpr1 <- ecoexpr[, e.expt1]
ecoexpr2 <- ecoexpr[, e.expt2]
ecoexpr.meta1 <- ecoexpr.meta[, e.expt1]
ecoexpr.meta2 <- ecoexpr.meta[, e.expt2]
rm(ecoexpr, ecoexpr.meta)

# Salmonell expt #3
# GEO accession: GSE38378
# Contrast samples: 54 (3 technical replicates of each, must be averaged)
s.expt <- which(salexpr.meta[3, ]=="ref_GSE38378_LB_stationary_phase_18h")
salexpr <- salexpr[, s.expt]
salexpr.meta <- salexpr.meta[, s.expt]

# Notes on other salmonella experiments I'm not using right now:

# Expt #4
# "ref_GSE1625_swarmer_ref"
# GEO accession: GSE1625
# contrast samples: 60 or 11, depending on whether the reps are bio or technical
# Related:
# "ref_GSE1631_swarmer_ref"

# Expt #5
# "ref_GSE2456_M9_pH7.0_Glc_37_OD0.71_5D664_1"
# GEO accession: GSE2456
# contrast samples: 77 or 22, depending on whether the reps are bio or technical
# I've contacted the contributor, but the same guy has other GEO datasets from 
# the same time that only have tech reps, so they're probably technical.

# the following expts only have technical reps:
# "ref_GSE806_pretreatment"
# "ref_GSE622_pretreatment"

################################################################################
# Capture experimental conditions and encode them as continuous variables

#########################################
# Extract condition annotations
# E coli:
efile <- "COLOMBOS/ecoli_compendium_data/colombos_ecoli_testannot_20151029.txt"
ecocond <- read.table(efile, sep="\t", header=T, stringsAsFactors=F)

# subset to relevant experiment
ecocond1 <- ecocond[ecocond$ContrastName %in% ecoexpr.meta1[1, ], ]
ecocond2 <- ecocond[ecocond$ContrastName %in% ecoexpr.meta2[1, ], ]

# extract annotations
ecocondval1 <- strsplit(ecocond1[,2], split=":")
ecocondval2 <- strsplit(ecocond2[,2], split=":")

lengths1 <- sapply(ecocondval1, length)
for (i in 1:length(ecocondval1)) { 
  if (length(ecocondval1[[i]])==1) ecocondval1[[i]] <- c("", "")
}
ecocondval1 <- matrix(unlist(ecocondval1), ncol=2, byrow=T)

lengths2 <- sapply(ecocondval2, length)
for (i in 1:length(ecocondval2)) { 
  if (length(ecocondval2[[i]])==1) ecocondval2[[i]] <- c("", "")
}
ecocondval2 <- matrix(unlist(ecocondval2), ncol=2, byrow=T)

ecocond1 <- cbind(ecocond1, ecocondval1[, 1], ecocondval1[, 2]) 
ecocond2 <- cbind(ecocond2, ecocondval2[, 1], ecocondval2[, 2]) 
ecocond1$TestAnnotation <- NULL
ecocond2$TestAnnotation <- NULL

# convert from long to wide format
names(ecocond1)[c(2, 3)] <- c("Variable", "Value")
names(ecocond2)[c(2, 3)] <- c("Variable", "Value")
ecocond1 <- dcast(ecocond1, ContrastName ~ Variable)
ecocond2 <- dcast(ecocond2, ContrastName ~ Variable)

rm(ecocondval1, ecocondval2, ecocond)

# Salmonella:
sfile <- paste("COLOMBOS/meta_sente_compendium_data/", 
               "colombos_meta_sente_testannot_20151030.txt", sep="")
salcond <- read.table(sfile, sep="\t", header=T, stringsAsFactors=F)

# subset to relevant experiment
salcond <- salcond[salcond$ContrastName %in% salexpr.meta[1, ], ]

# extract annotations
salcondval <- strsplit(salcond[,2], split=":")
lengths <- sapply(salcondval, length)
for (i in 1:length(salcondval)) { 
  if (length(salcondval[[i]])==1) salcondval[[i]] <- c("", "")
}
salcondval <- matrix(unlist(salcondval), ncol=2, byrow=T)
salcond <- cbind(salcond, salcondval[, 1], salcondval[, 2]) 
salcond$TestAnnotation <- NULL

# convert from long to wide format
names(salcond)[c(2, 3)] <- c("Variable", "Value")
salcond <- dcast(salcond, ContrastName ~ Variable)

rm(salcondval)

#########################################
# Clean up condition variables

# E coli expt 2:
# Discarding because 0 variance: "STRAIN.MG1655", "MEDIUM.MOPS", "BIOREACTOR". 
#   "GLUCOSE" and LACTOSE don't vary given LACTOSE.TIME.
#   "OD600" refers to optical density at 600nm. It contains very little info and
#   doesn't seem consistent with the growth phase variables so I discard it.
# Keeping: "LACTOSE.TIME", "H2O2", "TEMPERATURE", "TIME"
#   "GROWTH.EXPONENTIAL", "GROWTH.LAG", & "GROWTH.STATIONARY" give growth phase. 
#   GROWTH.LAG occurs because of stress during exponential phase; I will
#   consider it to be a mid-point between exponential and stationary growth.
# Adding: MEDIUM, since it appears in Salmonella expt

ecocond2$MEDIUM <- 0
ecocond2$GROWTH.PHASE <- ifelse(is.na(ecocond2$GROWTH.EXPONENTIAL), 
                                ifelse(is.na(ecocond2$GROWTH.LAG), 0, 0.5), 1)
ecocond2$LACTOSE.TIME <- as.numeric(gsub("min", "", ecocond2$LACTOSE.TIME))
ecocond2$LACTOSE.TIME[which(is.na(ecocond2$LACTOSE.TIME))] <- 0
ecocond2$H2O2 <- as.numeric(gsub("mM", "", ecocond2$H2O2))
ecocond2$H2O2[which(is.na(ecocond2$H2O2))] <- 0
ecocond2$TEMPERATURE <- gsub("°C", "", ecocond2$TEMPERATURE)
ecocond2$TEMPERATURE <- gsub("37", "", ecocond2$TEMPERATURE)
ecocond2$TEMPERATURE <- as.numeric(ecocond2$TEMPERATURE)
ecocond2$TEMPERATURE[which(is.na(ecocond2$TEMPERATURE))] <- 0
ecocond2$TIME <- as.numeric(gsub("min", "", ecocond2$TIME))
ecocond2$TIME[which(is.na(ecocond2$TIME))] <- 0

# E coli expt 1:
# lots of overexpression vars that I will ignore
# "BIOREACTOR", "STRAIN.MG1655", "TEMPERATURE" and "MEDIUM.LB" don't vary at all
# All other vars are gene overexpression experiments, which I discard as they're
# not fat hands. Could use for validation though.
# So, just need to add the vars that occur in other expts.

ecocond1[, c("GROWTH.PHASE", "MEDIUM", "TIME", "TEMPERATURE", "LACTOSE.TIME", 
             "H2O2")] <- 0

# salmonella expt:
# FLASK doesn't vary at all
# exponential is the inverse of stationary, & refers to growth phase
# the three MEDIUM vars are a categorical variable. LB (Luria broth) is the 
# richest. AMM stands for Acidic Minimal Media. From the paper, AMM2 meant 
# "grown in LB then transferred to AMM" whereas AMM1 meant "grown in AMM the 
# whole time", so I'll encode them as LB = 1, AMM1 = 0, AMM2 = 0.1.
# all other vars are deletions, each affecting a small number of rows. For now 
# I will ignore them because they're not fat hand interventions.
# Add "TIME", "TEMPERATURE", "LACTOSE.TIME", "H2O2" for e coli expt 2

salcond$MEDIUM <- ifelse(is.na(salcond$MEDIUM.LB), 
                         ifelse(is.na(salcond$MEDIUM.AMM2), 0, 0.1), 
                         1)
salcond$GROWTH.PHASE <- ifelse(is.na(salcond$STATIONARY), 1, 0)
salcond[, c("TIME", "TEMPERATURE", "LACTOSE.TIME", "H2O2")] <- 0

# Restrict to final set of condition vars:
condvars <- c("ContrastName", "GROWTH.PHASE", "MEDIUM", "TIME", "TEMPERATURE", 
              "LACTOSE.TIME", "H2O2")
ecocond2 <- ecocond2[, condvars]
ecocond1 <- ecocond1[, condvars]
salcond <- salcond[, condvars]

# Match order to expression dataset:
# Salmonella and e coli expt 1 are already in order, as you can verify like so:
# all.equal(match(ecoexpr.meta1[1, ], ecocond1$ContrastName), 
#           match(ecocond1$ContrastName, ecoexpr.meta1[1, ]))
# all.equal(match(salexpr.meta[1, ], salcond$ContrastName), 
#           match(salcond$ContrastName, salexpr.meta[1, ]))

# but the 2nd e coli experiment is out of order:
ecocond2 <- ecocond2[match(ecoexpr.meta2[1, ], ecocond2$ContrastName), ]

# Now you can verify the matching worked:
# all.equal(match(ecoexpr.meta2[1, ], ecocond2$ContrastName), 
#           match(ecocond2$ContrastName, ecoexpr.meta2[1, ]))

################################################################################
# Transpose, turn into data frames, and write processed datasets to file

ecoexpr1 <- t(ecoexpr1)
ecoexpr1 <- apply(ecoexpr1, 2, as.numeric) 
ecoexpr1 <- as.data.frame(ecoexpr1)
ecoexpr1 <- cbind(ecocond1, ecoexpr1)
for (i in 1:ncol(ecoexpr1)) ecoexpr1[which(is.na(ecoexpr1[, i])), i] <- 0
ecoexpr1$ContrastName <- NULL

ecoexpr2 <- t(ecoexpr2)
ecoexpr2 <- apply(ecoexpr2, 2, as.numeric) 
ecoexpr2 <- as.data.frame(ecoexpr2)
ecoexpr2 <- cbind(ecocond2, ecoexpr2)
which(is.na(ecoexpr2))
for (i in 1:ncol(ecoexpr2)) ecoexpr2[which(is.na(ecoexpr2[, i])), i] <- 0
ecoexpr2$ContrastName <- NULL

salexpr <- t(salexpr)
salexpr <- apply(salexpr, 2, as.numeric) 
salexpr <- as.data.frame(salexpr)
salexpr <- cbind(salcond, salexpr)
for (i in 1:ncol(salexpr)) salexpr[which(is.na(salexpr[, i])), i] <- 0
salexpr$ContrastName <- NULL

outfilepath <- "~/Dissertation_code/RealData/Processed/Ecoli_and_Salmonella/"
write.table(salexpr, file=paste(outfilepath, "senterica_express.txt", sep=""), 
            sep="\t", row.names=F, col.names=T, quote=F)
write.table(ecoexpr1, file=paste(outfilepath, "ecoli_express_1.txt", sep=""), 
            sep="\t", row.names=F, col.names=T, quote=F)
write.table(ecoexpr2, file=paste(outfilepath, "ecoli_express_2.txt", sep=""), 
            sep="\t", row.names=F, col.names=T, quote=F)


# scratchpad - testing GEST on small subset of data
# salexpr.sub <- salexpr[,1:20]
# ecoexpr1.sub <- ecoexpr1[,1:20]
# ecoexpr2.sub <- ecoexpr2[,1:20]
# 
# salvar <- apply(salexpr.sub, 2, var)
# ecovar1 <- apply(ecoexpr1.sub, 2, var)
# ecovar2 <- apply(ecoexpr2.sub, 2, var)
# for (i in 1:20) {
#   if (salvar[i]==0) salexpr.sub[,i] <- salexpr.sub[,i] + rnorm(nrow(salexpr.sub))
#   if (ecovar1[i]==0) ecoexpr1.sub[,i] <- ecoexpr1.sub[,i] + rnorm(nrow(ecoexpr1.sub))
#   if (ecovar2[i]==0) ecoexpr2.sub[,i] <- ecoexpr2.sub[,i] + rnorm(nrow(ecoexpr2.sub))
# }
# 
# 
# write.table(salexpr.sub, file=paste(outfilepath, "subset_senterica_express.txt", sep=""), 
#             sep="\t", row.names=F, col.names=T, quote=F)
# write.table(ecoexpr1.sub, file=paste(outfilepath, "subset_ecoli_express_1.txt", sep=""), 
#             sep="\t", row.names=F, col.names=T, quote=F)
# write.table(ecoexpr2.sub, file=paste(outfilepath, "subset_ecoli_express_2.txt", sep=""), 
#             sep="\t", row.names=F, col.names=T, quote=F)
