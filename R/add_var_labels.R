
#' Generic function
#' 
#' This generic function can take only the first argument as input and use the correct "method" automatically based on the class of the argument. 
add_var_labels <- function(mdlvalr_list, ...) {
    UseMethod(generic = "add_var_labels", object = mdlvalr_list)
}




#' Hybrid capture method function
#'
#' @param mdlvalr_list Name of the list to flag input data
#' @param pipeline [string] Name of pipeline that data were derived from. 
#'
#' @return Returns a named list of tibbles 
#'
#' @export
add_var_labels.hybcap <- function(
    mdlvalr_list,
    pipeline = "hybcap") {
    for (i in seq_along(names(mdlvalr_list$comparisons))) {
        vars_in_common <- mdlvalr_list$comparisons[[i]]$compared_data$vars_in_common %>%
            dplyr::mutate(vars_in_common_s1pass_s2pass = case_when(
                var_pass_fail.sample1 == "pass" & var_pass_fail.sample2 == "pass" ~ "yes",
                TRUE ~ "no"
                )) %>%
            dplyr::mutate(vars_in_common_s1pass_s2fail = case_when(
                var_pass_fail.sample1 == "pass" & var_pass_fail.sample2 == "fail" ~ "yes",
                TRUE ~ "no"
                )) %>%
            dplyr::mutate(vars_in_common_s1fail_s2pass = case_when(
                var_pass_fail.sample1 == "fail" & var_pass_fail.sample2 == "pass" ~ "yes",
                TRUE ~ "no"
                )) %>%
            dplyr::mutate(vars_in_common_s1fail_s2fail = case_when(
                var_pass_fail.sample1 == "fail" & var_pass_fail.sample2 == "fail" ~ "yes",
                TRUE ~ "no"
                ))

        vars_in_s1_not_in_s2  <- mdlvalr_list$comparisons[[i]]$compared_data$vars_in_s1_not_in_s2 %>%
            dplyr::mutate(vars_in_s1pass_not_in_s2 = case_when(
                var_pass_fail == "pass" ~ "yes",
                TRUE ~ "no"
                )) %>%
            dplyr::mutate(vars_in_s1fail_not_in_s2 = case_when(
                var_pass_fail == "fail" ~ "yes",
                TRUE ~ "no"
                ))

        vars_in_s2_not_in_s1  <- mdlvalr_list$comparisons[[i]]$compared_data$vars_in_s2_not_in_s1 %>%
            dplyr::mutate(vars_in_s2pass_not_in_s1 = case_when(
                var_pass_fail == "pass" ~ "yes",
                TRUE ~ "no"
                )) %>%
            dplyr::mutate(vars_in_s2fail_not_in_s1 = case_when(
                var_pass_fail == "fail" ~ "yes",
                TRUE ~ "no"
                ))

        mdlvalr_list$comparisons[[i]]$labeled_data <- list(
            vars_in_common = vars_in_common,
            vars_in_s1_not_in_s2 = vars_in_s1_not_in_s2,
            vars_in_s2_not_in_s1 = vars_in_s2_not_in_s1
        )
    } 
    class(mdlvalr_list) <- class(mdlvalr_list)
    return(mdlvalr_list)
}

