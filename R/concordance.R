#' Read Excel files into R tibbles
#'
#' The function reads the variant list and exon coverage Excel files and returns tibbles. The is an internal function used by `all_sample_compare()`.
#'
#' @param var_file File path to the variant file. File path supplied to `openxlsx::read.xlsx()`.
#' @param cov_file File path to the exon coverage file, sheet = 2. File path supplied to `openxlsx::read.xlsx()`.
#'
#' @return Returns a named list of tibbles (length = 2). The first is the variant table and the second is the exon coverage table. 
#'
#'
#'
read_files <- function(var_file, cov_file) {
    var_tbl <- openxlsx::read.xlsx(xlsxFile = var_file) %>% 
        as_tibble()
    cov_tbl <- openxlsx::read.xlsx(xlsxFile = cov_file, sheet = 2) %>% 
        as_tibble()

    return(list(
        var_tbl = var_tbl,
        cov_tbl = cov_tbl
    ))
}





#' Compare two variant call sets
#'
#' This function takes two samples as input and compares them using `dplyr::*_join()` functions. The results of the joins are captured and returned in a list. This is an internal function. 
#'
#' @param sample_1_name String for sample_1 name. 
#' @param sample_1_var Tibble of sample_1 variants.
#' @param sample_1_cov Tibble of sample_1 exon coverage.
#' @param sample_2_name String for sample_2 name. 
#' @param sample_2_var Tibble of sample_2 variants.
#' @param sample_2_cov Tibble of sample_2 exon coverage.
#' @param min_vaf Variant tables are filtered based on minimum variant allele frequency (VAF). Variants with VAF less than or equal to this value are not used in comparison.  [default: 0.05].
#' @param min_fraction_125x Exon tables are filtered based on the minimum fraction of bases covered at 125x. [default: 0.9].
#' @param match_by_colnames Vector of column names to use for matching. Passed to `by = ` parameter in join functions. [default: c("CHROM", "POS", "REF", "ALT", "GENE", "EXON")]. 
#' @param suffix_names Suffix names added to column names after running join functions. Vector of length two, [default: c(".sample_1", ".sample_2")].
#'
#' @return Returns a named list of tibbles or integers tibble sizes.
#'
#'
two_sample_compare <- function(sample_1_name, sample_1_var, sample_1_cov, sample_2_name, sample_2_var, sample_2_cov,
    min_vaf = 0.5, min_fraction_125x = 0.9,
    match_by_colnames = c("CHROM", "POS", "REF", "ALT", "GENE", "EXON"),
    suffix_names = c(".sample_1", ".sample_2")) {
    
    
    # Filter tibbles
    sample_1_pass <- sample_1_var %>%
        dplyr::filter(VAF >= min_vaf)

    sample_1_notpass <- sample_1_var %>%
        dplyr::filter(VAF < min_vaf)
        
    sample_2_pass <- sample_2_var %>%
        dplyr::filter(VAF >= min_vaf)

    sample_2_notpass <- sample_2_var %>%
        dplyr::filter(VAF < min_vaf)
    
    # Compare tables
    vars_in_common <- dplyr::inner_join(sample_1_pass, sample_2_pass, by = match_by_colnames, suffix = suffix_names)
    vars_in_common_from_sample_1 <- dplyr::semi_join(sample_1_pass, sample_2_pass, by = match_by_colnames)
    vars_in_common_from_sample_2 <- dplyr::semi_join(sample_2_pass, sample_1_pass, by = match_by_colnames)
    vars_in_sample_1_missing_from_sample_2 <- dplyr::anti_join(sample_1_pass, sample_2_pass, by = match_by_colnames)
    vars_in_sample_2_missing_from_sample_1 <- dplyr::anti_join(sample_2_pass, sample_1_pass, by = match_by_colnames)
    
    # What variants in "sample_1_pass", did not overlap with "sample_2_pass" (i.e. vars_in_sample_1_missing_from_sample_2), but
    # did overlap with sample_2_nopass? How many?
    vars_in_common_1pass_2notpass <- dplyr::inner_join(vars_in_sample_1_missing_from_sample_2, sample_2_notpass, by = match_by_colnames, suffix = suffix_names)
    vars_in_common_2pass_1notpass <- dplyr::inner_join(vars_in_sample_2_missing_from_sample_1, sample_1_notpass, by = match_by_colnames, suffix = suffix_names)

    
    
    # Get coverage info
    sample_1_cov_fail <- sample_1_cov %>%
        dplyr::filter(fraction_125x < min_fraction_125x)

    sample_2_cov_fail <- sample_2_cov %>%
        dplyr::filter(fraction_125x < min_fraction_125x)

    out <- list(
        # Names
        sample_1_name = sample_1_name,
        sample_2_name = sample_2_name,
        # Var tables
        sample_1_var = sample_1_var, 
        sample_2_var = sample_2_var,
        sample_1_pass = sample_1_pass, 
        sample_2_pass = sample_2_pass,
        vars_in_common = vars_in_common,
        vars_in_common_from_sample_1 = vars_in_common_from_sample_1,
        vars_in_common_from_sample_2 = vars_in_common_from_sample_2,
        vars_in_sample_1_missing_from_sample_2 = vars_in_sample_1_missing_from_sample_2,
        vars_in_sample_2_missing_from_sample_1 = vars_in_sample_2_missing_from_sample_1,
        vars_in_common_1pass_2notpass = vars_in_common_1pass_2notpass,
        vars_in_common_2pass_1notpass = vars_in_common_2pass_1notpass,
        # Variant stats
        n_vars_sample_1 = nrow(sample_1_var),
        n_vars_sample_2 = nrow(sample_2_var),
        n_vars_sample_1_pass = nrow(sample_1_pass),
        n_vars_sample_2_pass = nrow(sample_2_pass),
        n_vars_in_common = nrow(vars_in_common),
        n_vars_in_sample_1_missing_from_sample_2 = nrow(vars_in_sample_1_missing_from_sample_2),
        n_vars_in_sample_2_missing_from_sample_1 = nrow(vars_in_sample_2_missing_from_sample_1),
        n_vars_in_common_1pass_2notpass = nrow(vars_in_common_1pass_2notpass),
        n_vars_in_common_2pass_1notpass = nrow(vars_in_common_2pass_1notpass),
        # Var tables
        sample_1_cov = sample_1_cov,
        sample_2_cov = sample_2_cov,
        sample_1_cov_fail = sample_1_cov_fail,
        sample_2_cov_fail = sample_2_cov_fail,
        # Coverage stats
        n_exons_in_sample_1 = nrow(sample_1_cov),
        n_exons_in_sample_1_fail_coverage = nrow(sample_1_cov_fail),
        n_exons_in_sample_2 = nrow(sample_2_cov),
        n_exons_in_sample_2_fail_coverage = nrow(sample_2_cov_fail)
    )
    return(out)
}



 
#' Compare a set of samples for concordance
#'
#' This function takes a tibble of sample info (filename paths, sample names, etc.) and runs a concordance analysis. 
#'
#'
#' @param samples_tbl Tibble of sample info. 
#' @param sample_group String of column name in `samples_tbl` that represents the sample group for comparison. This column in samples_tbl needs to be a factor.
#' @param comparison_group String of column name in `samples_tbl` that represents the comparison group. This column in samples_tbl needs to be a factor with exactly two levels. For example, levels might be c("ver1", "ver2") or c("20ng", "50ng").
#' @param var_path String of column name in `samples_tbl` that represents the variant file path.
#' @param cov_path String of column name in `samples_tbl` that represents the coverage file path.
#' @param min_vaf Variant tables are filtered based on minimum variant allele frequency (VAF). Variants with VAF less than or equal to this value are not used in comparison.  [default: 0.05].
#' @param min_fraction_125x Exon tables are filtered based on the minimum fraction of bases covered at 125x. [default: 0.9].
#'
#'
#'
#'
#' @return Returns a named list of lists. The length is equal to the number of comparisons made. 
#'
#'
#' @export
all_sample_compare <- function(samples_tbl, sample_group, comparison_group, var_path, cov_path, min_vaf = 0.05, min_fraction_125x = 0.9) {
    tbl <- samples_tbl %>%
        dplyr::select(all_of(c(sample_group, comparison_group, var_path, cov_path))) %>%
        magrittr::set_colnames(c("sample_group", "comparison_group", "var_path", "cov_path")) %>%
        dplyr::mutate(comparison_name = glue("{sample_group}_{comparison_group}"))
       

    if (!is.factor(tbl$sample_group)) {
        stop("'sample_group' is not a factor variable")
    }
    if (!is.factor(tbl$comparison_group)) {
        stop("'comparison_group' is not a factor variable")
    }
    
    # Add integer based levels for each comparison group
    levels_int <- tibble(comparison_group = levels(tbl$comparison_group),
        comparison_group_int = seq(from = 1, to = length(levels(tbl$comparison_group))))
    
    tbl2 <- tbl %>%
        dplyr::left_join(levels_int, by = "comparison_group")
    
    tbl_wide <- tbl2 %>%
        tidyr::pivot_wider(id_cols = sample_group, names_from = "comparison_group_int", values_from = c("var_path", "cov_path", "comparison_name"))
    
    

    two_sample_compare_list <- list()
    for (i in seq_len(nrow(tbl_wide))) {
        sample_1_files <- read_files(tbl_wide$var_path_1[i], tbl_wide$cov_path_1[i])
        sample_2_files <- read_files(tbl_wide$var_path_2[i], tbl_wide$cov_path_2[i])
        
        two_sample_compare_list[[i]] <- two_sample_compare(
            sample_1_name = tbl_wide$comparison_name_1[i],
            sample_2_name = tbl_wide$comparison_name_2[i],
            sample_1_var = sample_1_files$var_tbl,
            sample_2_var = sample_2_files$var_tbl, 
            sample_1_cov = sample_1_files$cov_tbl,
            sample_2_cov = sample_2_files$cov_tbl, 
            min_vaf = min_vaf, 
            min_fraction_125x = min_fraction_125x)
        names(two_sample_compare_list)[i] <- as.character(glue("{tbl_wide$comparison_name_1[i]}_vs_{tbl_wide$comparison_name_2[i]}"))
    }
    return(two_sample_compare_list)
}




#' Fix a vector of names to fit into Excel tabs
#'
#' This function takes a vector of string names and reduces their length to be less than 31 characters (the max for Excel tabs). 
#'
#'
#' @param vector_of_strings Vector of string names. 
#'
#'
#' @return Returns a vector of shortened strings.
#'
fix_names <- function(vector_of_strings) {
     new_vector_of_strings <- tibble(name = vector_of_strings) %>%
        dplyr::mutate(name = if_else(str_length(name) >= 31, str_replace_all(name, "_", ""), name)) %>%
        dplyr::mutate(name = if_else(str_length(name) >= 31, str_replace_all(name, "^vars", ""), name)) %>%
        pull(name)
    return(new_vector_of_strings)
}




#' Create output files after concordance was performed
#'
#' This function takes the output list from `two_sample_compare()` as input and writes output Excel files. 
#'
#'
#' @param two_sample_compare_list List of data generated by `two_sample_compare()`.
#' @param filename_prefix String or path that is prepended to exported filenames. [default = ""].
#'
#' @return Returns a vector of shortened strings.
#'
#' @export
out_files <- function(two_sample_compare_list, filename_prefix = "") {
    names_all_samples <- names(two_sample_compare_list)

    comparison_summary_tbl <- list()
    for (h in seq_along(names_all_samples)) {
        curr_comparison <- names_all_samples[h]
        
        curr_list <- two_sample_compare_list[[h]]
        names(curr_list) <- fix_names(names(curr_list))


        tbl_idx <- unlist(lapply(curr_list, FUN = function(x) is_tibble(x)))

        
        comparison_summary <- list(summary = bind_cols(tibble(comparison = curr_comparison), curr_list[!tbl_idx]))
        comparison_summary_tbl[[h]] <- comparison_summary[[1]]
        
        tbls <- curr_list[tbl_idx]
        all_tbls <- c(comparison_summary, tbls)
        wb <- write.xlsx(all_tbls, file = glue("{filename_prefix}{curr_comparison}.xlsx"), overwrite = TRUE) 
    }
    tbl_summary_all <- bind_rows(comparison_summary_tbl)
    wb <- write.xlsx(list(all_samples = tbl_summary_all), file = glue("{filename_prefix}all.xlsx"), overwrite = TRUE) 
}



