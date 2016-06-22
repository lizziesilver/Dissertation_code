The raw data folder contains the following:

1. Klein et al. (2015)'s single-cell RNA-seq gene expression data: 
located in subdirectory GSE65525_RAW 
Originally downloaded from http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE65525

2. A list of significantly variable genes within the Klein et al. data: 
In the Excel workbook mmc3.xlsx and the CSV file mmc3-sheet1-variable-genes-in-es-cells.csv 
Originally downloaded from http://dx.doi.org/10.1016/j.cell.2015.04.044 

3. A list of transcription factors and a list of chromatin modifiers:
GO_transcription_factors.txt and GO_chromatin_modifiers.txt 
Downloaded from the Gene Ontology, using the following searches:

Transcription factors search terms:
    taxon_label: Mus musculus
    regulates_closure: GO:0006355
Search URL:
    http://amigo.geneontology.org/amigo/search/bioentity?q=*:*&fq=regulates_closure:%22GO:0006355%22&fq=taxon_label:%22Mus%20musculus%22&sfq=document_category:%22bioentity%22

Chromatin modifiers search terms:
    taxon_label: Mus musculus
    regulates_closure: GO:0016568
Search URL:
    http://amigo.geneontology.org/amigo/search/bioentity?q=*:*&fq=regulates_closure:%22GO:0016568%22&fq=taxon_label:%22Mus%20musculus%22&sfq=document_category:%22bioentity%22

Saved fields for each search are:
    Label (bioentity_label)
    Name (bioentity_name)
    Acc (bioentity)
    Synonyms (synonym)

4. Gold standard data:

Gold Standard 1: Both ChIP-x and LOGOF data from Xu et al. (2013)'s ESCAPE database.
located in xu2013escape_datasets
downloaded from http://www.maayanlab.net/ESCAPE/download.php

Gold Standard 2: LOGOF data from Correa-Cerro et al. (2011)
Located in correa2011generation_suppl
Downloaded from http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE31381

Gold Standard 3: LOGOF experimental data from Nishiyama et al. (2013) 
Located in folder nishiyama2013suppl
Downloaded from: http://www.nature.com/articles/srep01390#supplementary-information

Gold standard 4: ChIP-x regulatory relationships downloaded from http://www.regulatorynetworks.org/results/networks/mm9/networks.v12032013.tgz
Located in folder buffer.5000.mm9-120313
Readme for the data can be found here: http://www.regulatorynetworks.org/results/networks/mm9/readme.v12032013.txt
Subfolders denote cell lines. The relevant cell lines for ES cells are:
    mCJ7-DS13320
    ZhBTc4-DS15236
    ZhBTc4-DS17562
    ZhBTc4-DS17616
Each subfolder contains a file named "genes.regulate.genes.txt" with a list of regulatory relationships.

The processed data folder contains:
1. Two gold standard files (chipx.txt and logof.txt)
2. A list of transcription factors from GO and the gold standards (tf_list.txt)
3. A subset of genes for analysis (gene_subset.txt)
4. A data frame of gene expression over that subset (mus_subset_data.csv)
5. Two correlation matrices for that data subset, with headers including the sample size and variable names, in the correct format for input to tetradcmd. One is estimated using Pearson correlation and one using the SKEPTIC (cor_pearson_subset_with_header.txt and cor_skeptic_subset_with_header.txt)
6. A file with the background knowledge used as input to Tetrad (knowledge_tetrad_format_subset.txt)

To produce the processed data from the raw data, navigate to the directory:
    DAP/src/preprocessing
Then run the bash script that runs all the R scripts:
    bash preprocess.sh

You will need to alter the paths at the top of each R script so that the program can locate things on your computer.