## code to prepare `samples_tbl` dataset goes here

library(mdlvalr)
library(tidyverse)
library(glue)





# Find a set of files (tarballs) that were outputs from the pipeline. Untar and uncompress the data if necessary.
# Find example data included with mdlvalr package
extdata_dir <- system.file("extdata", package = "mdlvalr", mustWork = TRUE)
tar_files <- list.files(extdata_dir, pattern = "*.tar.gz", full.names = TRUE, recursive = TRUE)


# Untar and uncompress files (if not already available)
for (i in seq_along(tar_files)) {
    curr_tar_path <- tar_files[i]
    # Find the top level dir name inside the tar.gz
	curr_top_level_dirname <- basename(untar(tarfile = curr_tar_path, list = TRUE)[1])
	
	if (!dir.exists(curr_top_level_dirname)) {
        untar(tarfile = curr_tar_path)
	} else {
	    message(glue("'{curr_top_level_dirname}' dir already exists - will not untar again."))
	}
}


cwd <- getwd()

samples_tbl <- tibble(tar_path = tar_files) %>%
    dplyr::mutate(tar_filename = basename(tar_path)) %>%
    dplyr::mutate(out_name = str_remove(tar_filename, ".tar.gz")) %>%
    dplyr::mutate(out_name = str_remove(out_name, "^output_")) %>%
    dplyr::mutate(sample_name = str_split(out_name, "_", simplify = TRUE)[, 2]) %>%
    dplyr::mutate(sample_name_short = str_remove(sample_name, "-[^-]*$")) %>%
    dplyr::mutate(sample_name_short = factor(sample_name_short)) %>%
    dplyr::mutate(group = str_extract(sample_name, "[^-]*$")) %>%
    dplyr::mutate(group = glue("{group}ng")) %>%
    dplyr::mutate(group = factor(group, levels = c("100ng", "150ng"))) %>%
    dplyr::mutate(run_name = str_split(out_name, "_", simplify = TRUE)[, 1]) %>%
    dplyr::mutate(run_ver = str_remove(out_name, fixed(as.character(glue("{run_name}_{sample_name}_"))))) %>%
    dplyr::mutate(filtered_path = glue("{cwd}/{out_name}/{sample_name}_{run_ver}_filtered_integrated_variant_output_summary.xlsx")) %>%
    dplyr::mutate(coverage_path = glue("{cwd}/{out_name}/{sample_name}_{run_ver}_coverage_summary.xlsx"))



# ---------------------------------------------------------------------
# Clean up
# ---------------------------------------------------------------------

# Delete untar dirs
for (i in seq_along(tar_files)) {
    curr_tar_path <- tar_files[i]
    # Find the top level dir name inside the tar.gz
	curr_top_level_dirname <- basename(untar(tarfile = curr_tar_path, list = TRUE)[1])
	
	if (dir.exists(curr_top_level_dirname)) {
        unlink(curr_top_level_dirname, recursive = TRUE)
	}
}





# ---------------------------------------------------------------------
# Use this data in package
# ---------------------------------------------------------------------



# This will write the output file to PACKAGE/data dir. 
usethis::use_data(samples_tbl, overwrite = TRUE)

