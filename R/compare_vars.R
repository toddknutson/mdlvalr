#' Generic function
#' 
#' This generic function can take only the first argument as input and use the correct "method" automatically based on the class of the argument. 
#' @export
compare_vars <- function(mdlvalr_list, ...) {
    UseMethod(generic = "compare_vars", object = mdlvalr_list)
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
#' @export
compare_vars.hybcap <- function(
    mdlvalr_list,
    pipeline = "hybcap",
    match_by_colnames = c("CHROM", "POS", "REF", "ALT", "GENE", "EXON"),
    suffix_names = c(".sample1", ".sample2")) {

    for (i in seq_along(names(mdlvalr_list$comparisons))) {
        var_1 <- mdlvalr_list$comparisons[[i]]$flagged_data$var_1
        var_2 <- mdlvalr_list$comparisons[[i]]$flagged_data$var_2

        # Compare tables
        vars_in_common <- dplyr::inner_join(var_1, var_2, by = match_by_colnames, suffix = suffix_names)
        vars_in_common_from_s1 <- dplyr::semi_join(var_1, var_2, by = match_by_colnames)
        vars_in_common_from_s2 <- dplyr::semi_join(var_2, var_1, by = match_by_colnames)
        vars_in_s1_not_in_s2 <- dplyr::anti_join(var_1, var_2, by = match_by_colnames)
        vars_in_s2_not_in_s1 <- dplyr::anti_join(var_2, var_1, by = match_by_colnames)

        mdlvalr_list$comparisons[[i]]$compared_data <- list(
            vars_in_common = vars_in_common, 
            vars_in_common_from_s1 = vars_in_common_from_s1, 
            vars_in_common_from_s2 = vars_in_common_from_s2, 
            vars_in_s1_not_in_s2 = vars_in_s1_not_in_s2,
            vars_in_s2_not_in_s1 = vars_in_s2_not_in_s1
        )
    } 
    class(mdlvalr_list) <- class(mdlvalr_list)
    return(mdlvalr_list)
}

