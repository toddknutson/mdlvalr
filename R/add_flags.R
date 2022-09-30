
#' Add a column indicating whether variant overlaps with a CDS region
#'
#' @param var_tbl Tibble of variants. Must have columns named: chr, start, end, type.
#' @param cds_table Tibble RefSeq gff that includes CDS regions. Must have column name: "type" that indicates the CDS or not.
#'
#' @return Returns the same var_tbl that was input, but includes additional column for CDS.
#'
add_cds_column <- function(var_tbl, cds_table) {
    cds_table_granges <- cds_table %>%
        dplyr::select(chr, start, end, type) %>%
        dplyr::mutate(start = as.numeric(start)) %>%
        dplyr::mutate(end = as.numeric(end)) %>%
        dplyr::rename(seqnames = "chr") %>%
        dplyr::relocate(seqnames) %>%
        dplyr::mutate(seqnames = glue("chr{seqnames}")) %>%
        plyranges::as_granges()

    var_tbl_granges <- var_tbl %>%
        dplyr::mutate(start = as.numeric(POS)) %>%
        dplyr::mutate(end = as.numeric(POS)) %>%
        dplyr::mutate(seqnames = CHROM) %>%
        dplyr::relocate(seqnames) %>%
        plyranges::as_granges()

    var_tbl_granges_cds <- plyranges::join_overlap_left(var_tbl_granges, cds_table_granges) %>%
        as_tibble() %>%
        distinct() %>%
        dplyr::select(-seqnames, -start, -end, -width, -strand) %>%
        dplyr::rename(within_refseq_cds = "type") %>%
        dplyr::mutate(within_refseq_cds = if_else(is.na(within_refseq_cds), "no", "yes"))
    return(var_tbl_granges_cds)
}



#' Generic function
#' 
#' This generic function can take only the first argument as input and use the correct "method" automatically based on the class of the argument. 
add_flags <- function(mdlvalr_list, ...) {
    UseMethod(generic = "add_flags", object = mdlvalr_list)
}




#' Hybrid capture method function
#'
#' @param mdlvalr_list Name of the list to flag input data
#' @param pipeline [string] Name of pipeline that data were derived from. 
#'
#' @return Returns a named list of tibbles 
#'
#' @export
add_flags.hybcap <- function(mdlvalr_list, 
        pipeline = "hybcap",
        cds_table = NULL,
        var_pass_fail_logic = "TRUE", 
        cov_pass_fail_logic = "TRUE") {
    for (i in seq_along(names(mdlvalr_list$comparisons))) {
        # Add some helpful flag columns to indicate common VAF thresholds
        var_1 <- mdlvalr_list$comparisons[[i]]$input_data$var_1 %>%
            dplyr::mutate(vaf_gt_0.03 = if_else(VAF >= 0.03, "yes", "no")) %>%
            dplyr::mutate(vaf_gt_0.05 = if_else(VAF >= 0.05, "yes", "no"))
        var_2 <- mdlvalr_list$comparisons[[i]]$input_data$var_2 %>%
            dplyr::mutate(vaf_gt_0.03 = if_else(VAF >= 0.03, "yes", "no")) %>%
            dplyr::mutate(vaf_gt_0.05 = if_else(VAF >= 0.05, "yes", "no"))
                
        if (!is.null(cds_table)) {
            var_1 <- add_cds_column(var_1, cds_table)
            var_2 <- add_cds_column(var_2, cds_table)
        }
        
        # Add some helpful flag columns to indicate common coverage metrics
        cov_1 <- mdlvalr_list$comparisons[[i]]$input_data$cov_1 %>%
            dplyr::mutate(fraction_125x_gt_0.9 = if_else(fraction_125x >= 0.9, "yes", "no"))
            
        cov_2 <- mdlvalr_list$comparisons[[i]]$input_data$cov_2 %>%
            dplyr::mutate(fraction_125x_gt_0.9 = if_else(fraction_125x >= 0.9, "yes", "no"))
        
        # Add variant table pass/fail flag column depending on input logic
        var_1 <- var_1 %>%
            dplyr::mutate(var_pass_fail = case_when(
                eval(parse(text = var_pass_fail_logic)) ~ "pass",
                TRUE ~ "fail"
                ))
        var_2 <- var_2 %>%
            dplyr::mutate(var_pass_fail = case_when(
                eval(parse(text = var_pass_fail_logic)) ~ "pass",
                TRUE ~ "fail"
                ))
        # Add coverage table pass/fail flag column depending on input logic
        cov_1 <- cov_1 %>%
            dplyr::mutate(cov_pass_fail = case_when(
                eval(parse(text = cov_pass_fail_logic)) ~ "pass",
                TRUE ~ "fail"
                ))
        cov_2 <- cov_2 %>%
            dplyr::mutate(cov_pass_fail = case_when(
                eval(parse(text = cov_pass_fail_logic)) ~ "pass",
                TRUE ~ "fail"
                ))

        mdlvalr_list$comparisons[[i]]$flagged_data <- list(
            var_1 = var_1,
            var_2 = var_2,
            cov_1 = cov_1,
            cov_2 = cov_2
        )
    } 
    class(mdlvalr_list) <- class(mdlvalr_list)
    return(mdlvalr_list)
}

