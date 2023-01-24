#' Read pipeline files into a list
#'
#' @export
#' @param sample_sheet Name of the tibble or data.frame that contains sample
#' info. One set of comparisons per row of table. There are five required
#' column names: (1) `comparison_name` (e.g. "sample1_run1_vs_sample1_run2").
#' (2, 3) `var_path_1` and `var_path_2` that provide complete paths to the
#' variant files. (4, 5) `cov_path_1` and `cov_path_2` that provide complete
#' paths to the coverage files.
#'
#' @param pipeline Standard name of the pipeline from which the data were derived
#'
#' @return Returns a standard `mdlvalr_list` object (i.e. a regular R list of lists) that contains tables as named list elements.
#' The class of the retuned object is updated to include the name of the pipeline. This facilitates use of alternative methods in
#' downstream functions.
#'
#' @importFrom openxlsx read.xlsx
#'
#' @examples
#' \dontrun{
#' mdlvalr_list <- get_data(sample_sheet, pipeline = "hybcap")
#'}
get_data <- function(sample_sheet, pipeline) {
    if (!pipeline %in% c("hybcap", "germline")) {
        stop("ERROR: pipeline arg supplied to `get_data()` must be only one of: c('hybcap', 'germline')")
    }

    comparisons <- list()

    if (pipeline == "hybcap") {
        for (i in seq_len(nrow(sample_sheet))) {
            if (file.exists(sample_sheet$var_path_1[i])) {
                var_1 <- openxlsx::read.xlsx(xlsxFile = sample_sheet$var_path_1[i]) %>%
                    as_tibble()
            } else {
                stop(glue("ERROR: cannot find var_path_1 file: {sample_sheet$var_path_1[i]}"))
            }

            if (file.exists(sample_sheet$var_path_2[i])) {
                var_2 <- openxlsx::read.xlsx(xlsxFile = sample_sheet$var_path_2[i]) %>%
                    as_tibble()
            } else {
                stop(glue("ERROR: cannot find var_path_2 file: {sample_sheet$var_path_2[i]}"))
            }

            if (file.exists(sample_sheet$cov_path_1[i])) {
                cov_1 <- openxlsx::read.xlsx(xlsxFile = sample_sheet$cov_path_1[i], sheet = 2) %>%
                    as_tibble()
            } else {
                stop(glue("ERROR: cannot find cov_path_1 file: {sample_sheet$cov_path_1[i]}"))
            }

            if (file.exists(sample_sheet$cov_path_2[i])) {
                cov_2 <- openxlsx::read.xlsx(xlsxFile = sample_sheet$cov_path_2[i], sheet = 2) %>%
                    as_tibble()
            } else {
                stop(glue("ERROR: cannot find cov_path_2 file: {sample_sheet$cov_path_2[i]}"))
            }

            comparisons[[i]] <- list(input_data = list(var_1 = var_1, var_2 = var_2, cov_1 = cov_1, cov_2 = cov_2))
        }
    } else if (pipeline == "germline") {
        for (i in seq_len(nrow(sample_sheet))) {
            if (file.exists(sample_sheet$var_path_1[i])) {
                var_1 <- readr::read_tsv(sample_sheet$var_path_1[i], comment = "#", col_types = cols(.default = "c"),
                    col_names = c("CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT", "SAMPLE")) %>%
                    as_tibble()
            } else {
                stop(glue("ERROR: cannot find var_path_1 file: {sample_sheet$var_path_1[i]}"))
            }

            if (file.exists(sample_sheet$var_path_2[i])) {
                var_2 <- readr::read_tsv(sample_sheet$var_path_2[i], comment = "#", col_types = cols(.default = "c"),
                    col_names = c("CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT", "SAMPLE")) %>%
                    as_tibble()
            } else {
                stop(glue("ERROR: cannot find var_path_2 file: {sample_sheet$var_path_2[i]}"))
            }

            if (file.exists(sample_sheet$cov_path_1[i])) {
                cov_1 <- readr::read_tsv(sample_sheet$cov_path_1[i], comment = "#", col_types = cols(.default = "c"),
                    col_names = c("chr", "start", "end", "bases_15x", "bases_20x", "exons_15x", "exons_20x", "exon")) %>%
                    as_tibble()
            } else {
                stop(glue("ERROR: cannot find cov_path_1 file: {sample_sheet$cov_path_1[i]}"))
            }

            if (file.exists(sample_sheet$cov_path_2[i])) {
                cov_2 <- readr::read_tsv(sample_sheet$cov_path_2[i], comment = "#", col_types = cols(.default = "c"),
                    col_names = c("chr", "start", "end", "bases_15x", "bases_20x", "exons_15x", "exons_20x", "exon")) %>%
                    as_tibble()
            } else {
                stop(glue("ERROR: cannot find cov_path_2 file: {sample_sheet$cov_path_2[i]}"))
            }

            comparisons[[i]] <- list(input_data = list(var_1 = var_1, var_2 = var_2, cov_1 = cov_1, cov_2 = cov_2))
        }
    } else {
        stop(glue("ERROR: There is no method for get_data for pipeline: {pipeline}"))
    }

    names(comparisons) <- sample_sheet$comparison_name

    mdlvalr_list <- list(pipeline = pipeline, sample_sheet = sample_sheet, summary = NULL, comparisons = comparisons)
    class(mdlvalr_list) <- c(pipeline, class(mdlvalr_list))
    return(mdlvalr_list)
}
