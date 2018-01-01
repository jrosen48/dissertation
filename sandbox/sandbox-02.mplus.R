library(tidyverse)
library(MplusAutomation)

select_ancillary_functions <- function(df, ...){
    if (!is.data.frame(df)) stop("df must be a data.frame (or tibble)")
    df <- tibble::as_tibble(df)
    df_ss <- dplyr::select(df, ...)
    cases_to_keep <- stats::complete.cases(df_ss) # to use later for comparing function to index which cases to keep
    d <- df_ss[cases_to_keep, ] # removes incomplete cases
    names(d) <- stringr::str_replace(names(d), "\\.", "_")
    return(d)
}

extract_mplus_summary <- function(x) {
    x$summaries[c("LL", "BIC", "Entropy")]
}

extract_mplus_paramaters <- function(x) {
    y <- x$parameters[[1]]
    y <- y[-nrow(y), ]
    dplyr::select(y, param_name = paramHeader, var_name = param, est, se, class = LatentClass)
}

extract_means <- function(x) {
    calculate_centroids_mclust(x) %>%
        rownames_to_column("var_name") %>%
        rename(class_1 = Profile1, class_2 = Profile2) %>%
        mutate(param_name = "Means",
               class1 = round(class_1, 3),
               class2 = round(class_2, 3)) %>%
        select(param_name, var_name, class_1, class_2)
}

create_profiles_mplus <- function(df,
                                  ...,
                                  n_profiles,
                                  out_filename,
                                  the_title = "test",
                                  data_filename = "d.dat",
                                  script_filename = "t.inp",
                                  model = 1) {
    
    d <- select_ancillary_functions(df, ...)
    suppressWarnings(MplusAutomation::prepareMplusData(d, data_filename))
    
    unquoted_variable_name <- paste0(names(d), collapse = " ")

    var_list <- list()
    for (i in 1:length(names(d))) {
        var_list[[i]] <- names(d)[i]
    }
    
    TITLE <- paste0("TITLE: ", the_title)
    
    DATA <- paste0("DATA: File is ", data_filename, ";")
    
    VARIABLE_line0 <- "VARIABLE:"
    VARIABLE_line1 <- paste0("Names are ", unquoted_variable_name, ";")
    VARIABLE_line2 <- paste0("Classes = c(", n_profiles, ");")
    
    ANALYSIS_line0 <- "ANALYSIS: Type is mixture;"
    
    MODEL_overall_line00 <- paste0("MODEL:")
    MODEL_overall_line0 <- paste0("%overall%")
    MODEL_overall_line1 <- paste0("[", unquoted_variable_name, "];")
    MODEL_overall_line2 <- paste0(unquoted_variable_name, ";")
    
    OUTPUT_line0 <- "OUTPUT: TECH1 TECH11;"
    
    if (model == 1) {
        overall_collector <- list()
        for (j in 1:length(var_list)) {
            for (k in j:length(var_list)) {
                if (var_list[[j]] != var_list[[k]]) {
                    the_index <- length(overall_collector)
                    overall_collector[[the_index + 1]] <- paste0(var_list[[j]], " WITH ", var_list[[k]], "@0;")
                }
            }
        }
        the_index <- 0
        class_collector <- list()
        for (i in 1:n_profiles) {
            class_collector[[the_index + 1]] <- paste0("%c#", i, "%")
            class_collector[[the_index + 2]] <- paste0("[", unquoted_variable_name, "];")
            class_collector[[the_index + 3]] <- paste0(unquoted_variable_name, "(", 1, "-", length(var_list), ");")
            for (j in 1:length(var_list)) {
                for (k in j:length(var_list)) {
                    if (var_list[[j]] != var_list[[k]]) {
                        the_index <- length(class_collector)
                        class_collector[[the_index + 1]] <- paste0(var_list[[j]], " WITH ", var_list[[k]], "@0;") }}}}
    } 
    
    else if (model == 2) {
        overall_collector <- list()
        for (j in 1:length(var_list)) {
            for (k in j:length(var_list)) {
                if (var_list[[j]] != var_list[[k]]) {
                    the_index <- length(overall_collector)
                    overall_collector[[the_index + 1]] <- paste0(var_list[[j]], " WITH ", var_list[[k]], "@0;")
                }
            }
        }
        the_index <- 0
        class_collector <- list()
        for (i in 1:n_profiles) {
            class_collector[[the_index + i]] <- paste0("%c#", i, "%")
            class_collector[[the_index + i + 1]] <- paste0("[", unquoted_variable_name, "];")
            class_collector[[the_index + i + 2]] <- paste0(unquoted_variable_name, "(", 1, "-", length(var_list), ");")
            for (j in 1:length(var_list)) {
                for (k in j:length(var_list)) {
                    if (var_list[[j]] != var_list[[k]]) {
                        the_index <- length(class_collector)
                        class_collector[[the_index + 1]] <- paste0(var_list[[j]], " WITH ", var_list[[k]], "@0;") }}}}
    } else if (model == 3) {
        overall_collector <- list()
        for (j in 1:length(var_list)) {
            for (k in j:length(var_list)) {
                if (var_list[[j]] != var_list[[k]]) {
                    the_index <- length(overall_collector)
                    overall_collector[[the_index + 1]] <- paste0(var_list[[j]], " WITH ", var_list[[k]], "@0;")
                }
            }
        }
        the_index <- 0
        class_collector <- list()
        for (i in 1:n_profiles) {
            class_collector[[the_index + i]] <- paste0("%c#", i, "%")
            class_collector[[the_index + i + 1]] <- paste0("[", unquoted_variable_name, "];")
            class_collector[[the_index + i + 2]] <- paste0(unquoted_variable_name, "(", 1, "-", length(var_list), ");")
            for (j in 1:length(var_list)) {
                for (k in j:length(var_list)) {
                    if (var_list[[j]] != var_list[[k]]) {
                        the_index <- length(class_collector)
                        class_collector[[the_index + 1]] <- paste0(var_list[[j]], " WITH ", var_list[[k]], "@0;") }}}}
    }
    
    readr::write_lines(c(TITLE,
                         DATA, 
                         VARIABLE_line0, VARIABLE_line1, VARIABLE_line2,
                         MODEL_overall_line00, MODEL_overall_line0, MODEL_overall_line1, MODEL_overall_line2,
                         overall_collector,
                         class_collector,
                         ANALYSIS_line0,
                         OUTPUT_line0), 
                       script_filename)
    
}