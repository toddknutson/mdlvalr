## code to prepare `refseq_gff_unnest` dataset goes here


library(tidyverse)
library(glue)



# Download files from NCBI
url1 <- "ftp://ftp.ncbi.nlm.nih.gov/genomes/all/annotation_releases/9606/105.20220307/GCF_000001405.25_GRCh37.p13/GCF_000001405.25_GRCh37.p13_genomic.gff.gz"
download.file(url1, destfile = basename(url1), method = "wget")
gff_filename <- "GCF_000001405.25_GRCh37.p13_genomic.gff"
system(glue("gunzip -c {gff_filename}.gz > {gff_filename}"))



# Get the columns specifications
# Use them when reading in the whole file
# This is important because some rows have no values in the columns
# and read_tsv() cannot properly guess the column type
col_spec <- read_tsv(gff_filename, col_names = FALSE, comment = "#", n_max = 1) %>%
    readr::spec()

# Ignore any quotes found in the file
gff_raw <- read_tsv(gff_filename, quote = "", comment = "#", col_names = FALSE, col_types = col_spec)


refseq_gff <- gff_raw %>%
	dplyr::rename(seqid = "X1", 
		source = "X2",
		type = "X3",
		start = "X4",
		end = "X5",
		score = "X6",
		strand = "X7",
		phase = "X8",
		attributes = "X9") %>%
	dplyr::mutate(attributes_list = lapply(str_split(attributes, ";"), function(x) {
        curr_split <- str_split(x, "=", simplify = TRUE)
        keys <- curr_split[, 1]
        values <- curr_split[, 2]
        tibble(keys, values)}))

refseq_gff_unnest <- refseq_gff %>% 
    unnest(cols = attributes_list, keep_empty = TRUE) %>%
    tidyr::pivot_wider(names_from = "keys", values_from = "values")




# ---------------------------------------------------------------------
# Create clean gene name and chromosome band table
# ---------------------------------------------------------------------


# Some GFF files use the NCBI accession names (or something else) for the sequence id
# name, instead of the chromosome name. To capture the chromosome names, create a small 
# conversion table that can be merged with your final table later.

seqid_to_chrom_shortlist <- refseq_gff_unnest %>% 
    dplyr::filter(genome == "chromosome") %>%
    dplyr::select(seqid, chromosome) %>%
    dplyr::rename(chr = "chromosome")
    
refseq_gff_unnest_with_chrom <- refseq_gff_unnest %>%
    dplyr::left_join(seqid_to_chrom_shortlist, by = "seqid") %>%
    dplyr::filter(!is.na(chr)) %>%
    dplyr::filter(chr != "Unknown")

refseq_gff_unnest_with_chrom_cds <- refseq_gff_unnest_with_chrom %>%
    dplyr::filter(type == "CDS")



# ---------------------------------------------------------------------
# Use this data in package
# ---------------------------------------------------------------------



# This will write the output file to PACKAGE/data dir. 
usethis::use_data(refseq_gff_unnest_with_chrom_cds, overwrite = TRUE)

