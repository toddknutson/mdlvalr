
#' Generic function
#' 
#' This generic function can take only the first argument as input and use the correct "method" automatically based on the class of the argument. 
filter_data <- function(mdlvalr_list, ...) {
    UseMethod(generic = "filter_data", object = mdlvalr_list)
}




#' Hybrid capture method function
#'
#' @param mdlvalr_list Name of the list to flag input data
#' @param pipeline [string] Name of pipeline that data were derived from. 
#'
#' @return Returns a named list of tibbles 
#'
#' @export
filter_data.hybcap <- function(
    mdlvalr_list,
    pipeline = "hybcap",
    var_flag_cols = c("vaf_gt_0.03"),
    cov_flag_cols = c("fraction_125x_gt_0.9")) {

    for (i in seq_along(names(mdlvalr_list$comparisons))) {
        var_1_pass <- mdlvalr_list$comparisons[[i]]$flagged_data$var_1 %>%
            dplyr::filter(if_all(all_of(var_flag_cols), ~ str_detect(.x, "^yes$")))

        var_1_notpass <- mdlvalr_list$comparisons[[i]]$flagged_data$var_1 %>%
            dplyr::filter(if_any(all_of(var_flag_cols), ~ str_detect(.x, "^no$")))

        var_2_pass <- mdlvalr_list$comparisons[[i]]$flagged_data$var_2 %>%
            dplyr::filter(if_all(all_of(var_flag_cols), ~ str_detect(.x, "^yes$")))
             
        var_2_notpass <- mdlvalr_list$comparisons[[i]]$flagged_data$var_2 %>%
            dplyr::filter(if_any(all_of(var_flag_cols), ~ str_detect(.x, "^no$")))
       
        cov_1_pass <- mdlvalr_list$comparisons[[i]]$flagged_data$cov_1 %>%
            dplyr::filter(if_all(all_of(cov_flag_cols), ~ str_detect(.x, "^yes$")))
            
        cov_1_notpass <- mdlvalr_list$comparisons[[i]]$flagged_data$cov_1 %>%
            dplyr::filter(if_any(all_of(cov_flag_cols), ~ str_detect(.x, "^no$")))

        cov_2_pass <- mdlvalr_list$comparisons[[i]]$flagged_data$cov_2 %>%
            dplyr::filter(if_all(all_of(cov_flag_cols), ~ str_detect(.x, "^yes$")))

        cov_2_notpass <- mdlvalr_list$comparisons[[i]]$flagged_data$cov_2 %>%
            dplyr::filter(if_any(all_of(cov_flag_cols), ~ str_detect(.x, "^no$")))


        mdlvalr_list$comparisons[[i]]$filtered_data <- list(
            var_1_pass = var_1_pass,
            var_1_notpass = var_1_notpass,
            var_2_pass = var_2_pass,
            var_2_notpass = var_2_notpass,
            cov_1_pass = cov_1_pass,
            cov_1_notpass = cov_1_notpass,
            cov_2_pass = cov_2_pass,
            cov_2_notpass = cov_2_notpass
        )
    } 
    class(mdlvalr_list) <- class(mdlvalr_list)
    return(mdlvalr_list)
}

