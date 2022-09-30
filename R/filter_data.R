
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
    pipeline = "hybcap") {

    for (i in seq_along(names(mdlvalr_list$comparisons))) {
        # Samples that pass
        vars_in_s1pass <- mdlvalr_list$comparisons[[i]]$flagged_data$var_1 %>%
            dplyr::filter(var_pass_fail == "pass")
        vars_in_s2pass <- mdlvalr_list$comparisons[[i]]$flagged_data$var_2 %>%
            dplyr::filter(var_pass_fail == "pass")


        # Vars common to both samples
        vars_in_common_s1pass_s2pass <- mdlvalr_list$comparisons[[i]]$labeled_data$vars_in_common %>%
            dplyr::filter(vars_in_common_s1pass_s2pass == "yes")
        vars_in_common_s1pass_s2fail <- mdlvalr_list$comparisons[[i]]$labeled_data$vars_in_common %>%
            dplyr::filter(vars_in_common_s1pass_s2fail == "yes")
        vars_in_common_s1fail_s2pass <- mdlvalr_list$comparisons[[i]]$labeled_data$vars_in_common %>%
            dplyr::filter(vars_in_common_s1fail_s2pass == "yes")
        vars_in_common_s1fail_s2fail <- mdlvalr_list$comparisons[[i]]$labeled_data$vars_in_common %>%
            dplyr::filter(vars_in_common_s1fail_s2fail == "yes")


        # Vars in one sample but not the other
        vars_in_s1pass_not_in_s2 <- mdlvalr_list$comparisons[[i]]$labeled_data$vars_in_s1_not_in_s2 %>%
            dplyr::filter(vars_in_s1pass_not_in_s2 == "yes")
        vars_in_s1fail_not_in_s2 <- mdlvalr_list$comparisons[[i]]$labeled_data$vars_in_s1_not_in_s2 %>%
            dplyr::filter(vars_in_s1fail_not_in_s2 == "yes")

        vars_in_s2pass_not_in_s1 <- mdlvalr_list$comparisons[[i]]$labeled_data$vars_in_s2_not_in_s1 %>%
            dplyr::filter(vars_in_s2pass_not_in_s1 == "yes")
        vars_in_s2fail_not_in_s1 <- mdlvalr_list$comparisons[[i]]$labeled_data$vars_in_s2_not_in_s1 %>%
            dplyr::filter(vars_in_s2fail_not_in_s1 == "yes")


        exons_in_s1pass <- mdlvalr_list$comparisons[[i]]$flagged_data$cov_1 %>%
            dplyr::filter(cov_pass_fail == "pass")
        exons_in_s2pass <- mdlvalr_list$comparisons[[i]]$flagged_data$cov_2 %>%
            dplyr::filter(cov_pass_fail == "pass")
        exons_in_s1fail <- mdlvalr_list$comparisons[[i]]$flagged_data$cov_1 %>%
            dplyr::filter(cov_pass_fail == "fail")
        exons_in_s2fail <- mdlvalr_list$comparisons[[i]]$flagged_data$cov_2 %>%
            dplyr::filter(cov_pass_fail == "fail")


        mdlvalr_list$comparisons[[i]]$filtered_data <- list(
            vars_in_s1pass = vars_in_s1pass,
            vars_in_s2pass = vars_in_s2pass,
            vars_in_common_s1pass_s2pass = vars_in_common_s1pass_s2pass,
            vars_in_common_s1pass_s2fail = vars_in_common_s1pass_s2fail,
            vars_in_common_s1fail_s2pass = vars_in_common_s1fail_s2pass,
            vars_in_common_s1fail_s2fail = vars_in_common_s1fail_s2fail,
            vars_in_s1pass_not_in_s2 = vars_in_s1pass_not_in_s2,
            vars_in_s1fail_not_in_s2 = vars_in_s1fail_not_in_s2,
            vars_in_s2pass_not_in_s1 = vars_in_s2pass_not_in_s1,
            vars_in_s2fail_not_in_s1 = vars_in_s2fail_not_in_s1,
            exons_in_s1pass = exons_in_s1pass,
            exons_in_s2pass = exons_in_s2pass,
            exons_in_s1fail = exons_in_s1fail,
            exons_in_s2fail = exons_in_s2fail 
        )
    } 
    class(mdlvalr_list) <- class(mdlvalr_list)
    return(mdlvalr_list)
}

