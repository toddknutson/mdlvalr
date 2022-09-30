#' Read pipeline files into a list
#'
#' This generic function can take only the first argument as input and use the correct "method"
#' automatically based on the class of the argument. 
#'
#'
#' @export
#' @param sample_sheet Name of the tibble or data.frame that contains sample
#' info. The class of this object can be used to automatically determine which 
#' funcion methods will be used.  
#' @param pipeline Standard name of the pipeline from which the data were derived.
#'
#' @section Methods:
#' This function is a **generic**, which means that different
#' implementations (methods) can be executed for different classes. The class
#' of the first argument will determine which method is used. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' @return Returns a named list of tibbles (length = 2). The first is the variant table and the second is the exon coverage table. 
#'
#' @examples
#' class(sample_sheet)
#' # [1] "hybcap" "tbl_df" "data.frame"
#' 
#' mdlvalr_list <- get_data(sample_sheet)
#'
get_data <- function(sample_sheet, pipeline = NULL) {
    UseMethod("get_data")
}





#' @export
#' @rdname get_data
get_data.hybcap <- function(sample_sheet, pipeline = "hybcap") {
    comparisons <- list()
    for (i in seq_len(nrow(sample_sheet))) {
        var_1 <- openxlsx::read.xlsx(xlsxFile = sample_sheet$var_path_1[i]) %>% 
            as_tibble()
  
        var_2 <- openxlsx::read.xlsx(xlsxFile = sample_sheet$var_path_2[i]) %>% 
            as_tibble()

        cov_1 <- openxlsx::read.xlsx(xlsxFile = sample_sheet$cov_path_1[i], sheet = 2) %>% 
            as_tibble()
        
        cov_2 <- openxlsx::read.xlsx(xlsxFile = sample_sheet$cov_path_2[i], sheet = 2) %>% 
            as_tibble()

        comparisons[[i]] <- list(input_data = list(var_1 = var_1, var_2 = var_2, cov_1 = cov_1, cov_2 = cov_2))
    } 
    names(comparisons) <- sample_sheet$comparison_name

    mdlvalr_list <- list(pipeline = pipeline, sample_sheet = sample_sheet, summary = NULL, comparisons = comparisons)
    class(mdlvalr_list) <- c(pipeline, class(mdlvalr_list))
    return(mdlvalr_list)
}

