#' Validate a table of sample info @param samples_tbl Tibble of sample info. Requires five columns (listed below).
#' @param sample_name Column name that specifies the sample names. [string].
#' @param sample_group Column name that specifies the sample_group. [string].
#' @param comparison_group Column name that specifies the comparison_group. [string].
#' @param var_path Column name that specifies the variants table file path. [string].
#' @param cov_path Column name that specifies the coverage table file path. [string].
#' @param pipeline Name of pipeline that produced the data. One of c("hybcap", "exome", "germline"). 
#' 
#' 
#' @examples
#' invisible(samples_tbl)
#' sample_sheet <- validate_samples(samples_tbl, sample_name = "sample_name", sample_group = "sample_name_short", comparison_group = "group", var_path = "filtered_path", cov_path = "coverage_path", pipeline = "hybcap")
#' 
#' @export 
validate_samples <- function(samples_tbl, sample_name, sample_group, comparison_group, var_path, cov_path, pipeline) {
    tbl <- samples_tbl %>%
        dplyr::select(all_of(c(sample_name, sample_group, comparison_group, var_path, cov_path))) %>%
        magrittr::set_colnames(c("sample_name", "sample_group", "comparison_group", "var_path", "cov_path")) %>%
        dplyr::mutate(comparison_name = glue("{sample_group}_{comparison_group}"))
       

    if (!is.factor(tbl$sample_group)) {
        tbl$sample_group <- factor(tbl$sample_group)
    }
    if (!is.factor(tbl$comparison_group)) {
        tbl$comparison_group <- factor(tbl$comparison_group)
    }

    # Add integer based levels for each comparison group
    levels_int <- tibble(comparison_group = levels(tbl$comparison_group),
        comparison_group_int = seq(from = 1, to = length(levels(tbl$comparison_group))))

    tbl2 <- tbl %>%
        dplyr::left_join(levels_int, by = "comparison_group")

    tbl_wide <- tbl2 %>%
        tidyr::pivot_wider(id_cols = sample_group, names_from = "comparison_group_int", values_from = c("var_path", "cov_path", "comparison_name")) %>%
        dplyr::mutate(pipeline = pipeline) %>%
        dplyr::mutate(comparison_name = glue("{comparison_name_1}_vs_{comparison_name_2}"))

    class(tbl_wide) <- c(pipeline, class(tbl_wide))
    return(tbl_wide)
}



