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

create_profiles_mplus <- function(df,
                                  ...,
                                  n_profiles,
                                  out_filename,
                                  the_title = "test",
                                  data_filename = "d",
                                  script_filename) {
    
    d <- select_ancillary_functions(df, ...)
    prepareMplusData(d, paste0(data_filename, ".data"))
    
    un
    
    # TESTING!!! 
    the_title <- "test"
    the_filename <- "iris.dat"
    var1 <- "Sepal_Length"
    var2 <- "Sepal_Width"
    var3 <- "Petal_Length"
    var4 <- "Petal_Width"
    unquoted_variable_name <- paste(var1, var2, var3, var4)
    var_list <- list(var1, var2, var3, var4)
    
    unquoted_variable_name
    n_profiles <- 2
    model <- 1
    
    TITLE <- paste0("TITLE: ", the_title, ";")
    
    DATA <- paste0("DATA: File is ", the_filename, ";")
    
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
            class_collector[[the_index + i]] <- paste0("[", unquoted_variable_name, "];")
            class_collector[[the_index + i + 1]] <- paste0(unquoted_variable_name, "(", 1, "-", length(var_list), ");")
            for (j in 1:length(var_list)) {
                for (k in j:length(var_list)) {
                    if (var_list[[j]] != var_list[[k]]) {
                        the_index <- length(class_collector)
                        print(paste("i:", i, "j:", j, "k:", k))
                        class_collector[[the_index + 1]] <- paste0(var_list[[j]], " WITH ", var_list[[k]], "@0;")
                        print(length(class_collector)) }}}}
    } else if (model == 2) {
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
            class_collector[[the_index + i]] <- paste0("[", unquoted_variable_name, "];")
            class_collector[[the_index + i + 1]] <- paste0(unquoted_variable_name, "(", 1, "-", length(var_list), ");")
            for (j in 1:length(var_list)) {
                for (k in j:length(var_list)) {
                    if (var_list[[j]] != var_list[[k]]) {
                        the_index <- length(class_collector)
                        print(paste("i:", i, "j:", j, "k:", k))
                        class_collector[[the_index + 1]] <- paste0(var_list[[j]], " WITH ", var_list[[k]], "@0;")
                        print(length(class_collector)) }}}}
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
            class_collector[[the_index + i]] <- paste0("[", unquoted_variable_name, "];")
            class_collector[[the_index + i + 1]] <- paste0(unquoted_variable_name, "(", 1, "-", length(var_list), ");")
            for (j in 1:length(var_list)) {
                for (k in j:length(var_list)) {
                    if (var_list[[j]] != var_list[[k]]) {
                        the_index <- length(class_collector)
                        print(paste("i:", i, "j:", j, "k:", k))
                        class_collector[[the_index + 1]] <- paste0(var_list[[j]], " WITH ", var_list[[k]], "@0;")
                        print(length(class_collector)) }}}}
    }
    
    readr::write_lines(c(TITLE,
                         DATA, 
                         VARIABLE_line0, VARIABLE_line1, VARIABLE_line2,
                         MODEL_overall_line00, MODEL_overall_line0, MODEL_overall_line1, MODEL_overall_line2,
                         overall_collector,
                         class_collector,
                         ANALYSIS_line0,
                         OUTPUT_line0), 
                       "sandbox/mplus/test_1118.inp")
    
}

runModels(target = paste0(getwd(), "/sandbox/mplus/test_1231.inp"))