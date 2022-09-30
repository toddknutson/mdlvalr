#' Generic function
#' 
#' This generic function can take only the first argument as input and use the correct "method" automatically based on the class of the argument. 
export_excel <- function(mdlvalr_list, ...) {
    UseMethod(generic = "export_excel", object = mdlvalr_list)
}




#' Export data to Excel files using tabs
#'
#'
#'
export_excel.hybcap <- function(mdlvalr_list, pipeline = "hybcap", filename_prefix = "") {
    for (i in seq_along(names(mdlvalr_list$comparisons))) {
        curr_comparison <- names(mdlvalr_list$comparisons)[i]

        flagged_names <- names(mdlvalr_list$comparisons[[i]]$flagged_data)
        flagged <- mdlvalr_list$comparisons[[i]]$flagged_data
        #filtered_names <- names(mdlvalr_list$comparisons[[i]]$filtered_data)
        #filtered <- mdlvalr_list$comparisons[[i]]$filtered_data
        compared_names <- names(mdlvalr_list$comparisons[[i]]$compared_data)
        compared <- mdlvalr_list$comparisons[[i]]$compared_data
        labeled_names <- names(mdlvalr_list$comparisons[[i]]$labeled_data)
        labeled <- mdlvalr_list$comparisons[[i]]$labeled_data
        summary_tbl <- mdlvalr_list$comparisons[[i]]$summary %>%
              bind_rows()

        out_list <- c(list(summary = summary_tbl), flagged, labeled)
        wb <- write.xlsx(out_list, file = glue("{filename_prefix}{curr_comparison}.xlsx"), overwrite = TRUE)
    }
    wb <- write.xlsx(list(all_samples = mdlvalr_list$summary), file = glue("{filename_prefix}all.xlsx"), overwrite = TRUE)
}

