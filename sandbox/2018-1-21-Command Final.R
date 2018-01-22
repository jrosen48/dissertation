# command_to_submit

#############################################
### 0. Functions (helper and main) ##########
#############################################

#' centers the data
#'
#' @export

centering_function <- function(data, method_of_centering, grouping_vector, to_standardize = F){
    center_this <- function(x){
        x - mean(x, na.rm = T)
    }
    
    scale_this <- function(x) {
        if (stats::sd(x, na.rm = T) == 0){
            x - mean(x, na.rm = T)
        } else {
            (x - mean(x)) / stats::sd(x)
        }
    }
    if (method_of_centering == "grand" & to_standardize == F) {
        out <- sapply(data, function(x) center_this(x))
        out <- as.data.frame(out)
    }
    if (method_of_centering == "group" & to_standardize == F) {
        out <- data %>%
            cbind(grouping_vector) %>%
            dplyr::group_by(grouping_vector) %>%
            dplyr::mutate_each(dplyr::funs(center_this))
        out <- as.data.frame(out[, 1:ncol(data)])
    }
    if (method_of_centering == "grand" & to_standardize == T) {
        out <- sapply(data, function(x) scale_this(x))
        out <- as.data.frame(out)
    }
    if (method_of_centering == "group" & to_standardize == T) {
        out <- data %>%
            cbind(grouping_vector) %>%
            dplyr::group_by(grouping_vector) %>%
            dplyr::mutate_each(dplyr::funs(scale_this))
        out <- as.data.frame(out[, 1:ncol(data)])
    }
    if (method_of_centering == "raw") {
        out <- as.data.frame(data)
    }
    return(out)
}

#' calculates distance
#'
#' @export

distance_function <- function(x, distance_metric = "squared_euclidean"){
    if (distance_metric != "squared_euclidean") {
        distance <- stats::dist(x, method = distance_metric)
    } else {
        distance <- stats::dist(x, method = "euclidean")
        distance <- distance ^ 2
    }
    return(distance)
}

#' converts hclust output to start values for kmeans function
#'
#' @export

hclust_to_kmeans_function <- function(data, out, n_clusters){
    # This function processes the output from the hierarchical clustering to be used as starting points for the kmeans clustering
    cut_hclust <- stats::cutree(out, n_clusters) # cuts the results of the hierarchical cluster at the specified # of clusters
    clusters_list <- list()
    for (i in seq(n_clusters)){
        clusters_list[[i]] <- data[cut_hclust == i,]
    }
    ordered_clusters <- list()
    cluster_freqs <- list()
    for (i in seq(length(clusters_list))){
        ordered_clusters[[i]] <- colSums(as.matrix(clusters_list[[i]]) / nrow(clusters_list[[i]]))
        cluster_freqs[[i]] <- ordered_clusters[[i]]
    }
    return(cluster_freqs)
}

#' carries out kmeans cluster analysis
#'
#' @export

kmeans_function <- function(data, cluster_freqs){
    start <- data.frame(matrix(unlist(cluster_freqs), nrow=length(cluster_freqs[[1]]), byrow = T), stringsAsFactors = F)
    start <- as.matrix(start)
    start <- t(start)
    return(stats::kmeans(data, start))
}

#' calculates dissimilarity matrix
#'
#' @export

dissim_function <- function(hc){
    data.frame(row.names = paste0("Cluster", seq_along(hc$height)),
               height = hc$height,
               component_1 = paste0("Cluster", hc$merge[, 1]),
               component_2 = paste0("Cluster", hc$merge[, 2]),
               stringsAsFactors=FALSE)
}

#' standardizes raw data
#'
#' @export

standardize_function <- function(data){
    standardized_data <- scale(data, center = F, scale = T)
    return(standardized_data)
}

#' calculates cluster centroids
#'
#' @export

cluster_freq_function <- function(data, n_clusters, kfit, variable_names){
    clusters <- list()
    for (i in 1:n_clusters){
        clusters[[i]] <- data[kfit$cluster == i, ]
    }
    cluster_freqs <- list()
    for (i in seq(n_clusters)){
        cluster_freqs[[i]] <- colSums(clusters[[i]]) / nrow(clusters[[i]]) # Need to fix - will want to add group freqs
    }
    cluster_freqs <- as.data.frame(matrix(unlist(cluster_freqs), nrow = n_clusters, byrow = T))
    names(cluster_freqs) <- variable_names
    cluster_freqs$Cluster <- paste0("Cluster ", 1:n_clusters, ": ", table(kfit$cluster), " Obs.")
    return(cluster_freqs)
}

#' creates plot of cluster centroids
#'
#' @export

cluster_plot_function <- function(cluster_freqs, font_size, fill_order){
    cluster_freqs_tmp <- tidyr::gather(cluster_freqs, Var, Value, -Cluster)
    if (!is.null(fill_order)){
        cluster_freqs_tmp$Var <- factor(cluster_freqs_tmp$Var, levels = fill_order)
        #cluster_freqs_tmp <- cluster_freqs_tmp[match(fill_order, cluster_freqs_tmp$Var), ]
    }
    clusters_p <- ggplot2::ggplot(cluster_freqs_tmp, ggplot2::aes(x = Cluster, y = Value, fill = Var)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ylab("") +
        xlab("") +
        ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ggplot2::theme(legend.position = "top") +
        ggplot2::theme(text=element_text(size = font_size, family = "Times"))
    return(clusters_p)
}

#' carries out Tukey HSD test
#'
#' @export

testing_the_tukey <- function(data){
    tukey_list <- list()
    for (i in 1:(ncol(data) - 2)){
        fit <- aov(data[, i] ~ as.character(data$cluster_assignment))
        tukey_list[[i]] <- stats::TukeyHSD(fit)
    }
    names(tukey_list) <- names(data[, 1:3])
    return(tukey_list)
}

#' carries out MANOVA test
#'
#' @export

try_manova <- function(data, cluster_assignment, variable_names){
    
    manova_function <- function(data, cluster_assignment, variable_names){
        out <- list()
        data$DV <- as.matrix(data)
        data <- cbind(data, cluster_assignment)
        mv_out <- stats::manova(DV ~ cluster_assignment, data = data)
        out[[1]] <- summary(mv_out, test = "Wilks")
        out[[2]] <- summary.aov(mv_out)
        out[[3]] <- testing_the_tukey(data)
        names(out[[3]]) <- variable_names
        return(out)
    }
    
    out <- tryCatch(
        {
            manova_function(data, cluster_assignment, variable_names)  
        },
        error = function(cond){
            return(cond)
        }
    )
    return(out)
}

#' merge assignments with selected factors
#'
#' @export

merge_assignments_and_factors <- function(cluster_assignments, cases_to_keep, factor_data_frame){
    factor_data_frame_ss <- factor_data_frame[cases_to_keep, ]
    data <- data.frame(cluster = cluster_assignments, factor_data_frame_ss)
    return(data)
}

#' create crosstabs
#'
#' @export

create_crosstab <- function(data, factor_to_explore){
    if (length(factor_to_explore) == 1) {
        which_variable <- which(names(data) == factor_to_explore)
        out <- table(data$cluster, data[, which_variable])
    } else if (length(factor_to_explore) == 2) {
        which_variable_1 <- which(names(data) == factor_to_explore[1])
        which_variable_2 <- which(names(data) == factor_to_explore[2])
        out <- table(data$cluster, data[, which_variable_1], data[, which_variable_2])
    } else if (length(factor_to_explore) == 3) {
        which_variable_1 <- which(names(data) == factor_to_explore[1])
        which_variable_2 <- which(names(data) == factor_to_explore[2])
        which_variable_3 <- which(names(data) == factor_to_explore[3])
        out <- table(data$cluster, data[, which_variable_1], data[, which_variable_2], data[, which_variable_3])
    }
    return(out)
}

#' dummy code cluster assignments
#'
#' @export

dummmy_code_cluster_assignments <- function(data){
    data$cluster <- as.factor(data$cluster)
    tmp <- as.data.frame(model.matrix( ~ cluster - 1, data = data))
    out <- data.frame(data[, 2:ncol(data)], tmp)
    return(out)
}

#' create raw data for ANOVA and subsequent processing
#'
#' @export

create_raw_data <- function(dummy_coded_data, factor_to_explore, variable_to_find_proportion){
    
    for_one <- function(dummy_coded_data, factor_to_explore, variable_to_find_proportion){
        if (!is.null(variable_to_find_proportion)) {
            raw_data <- dummy_coded_data %>%
                dplyr::select(dplyr::matches(variable_to_find_proportion), dplyr::matches(factor_to_explore), dplyr::contains("cluster")) %>%
                dplyr::group_by_(variable_to_find_proportion, factor_to_explore) %>%
                dplyr::summarize_each(dplyr::funs(mean)) %>%
                dplyr::ungroup()
        } else {
            raw_data <- dummy_coded_data %>%
                dplyr::select(dplyr::matches(factor_to_explore), dplyr::contains("cluster")) %>%
                dplyr::ungroup()
        }
        return(raw_data)
    }
    
    for_two <- function(dummy_coded_data, factor_to_explore, variable_to_find_proportion){
        if (!is.null(variable_to_find_proportion)) {
            raw_data <- dummy_coded_data %>%
                dplyr::select(dplyr::matches(variable_to_find_proportion), dplyr::matches(factor_to_explore[1]), dplyr::matches(factor_to_explore[2]), dplyr::contains("cluster")) %>%
                dplyr::group_by_(variable_to_find_proportion, factor_to_explore) %>%
                dplyr::summarize_each(dplyr::funs(mean)) %>%
                ungroup()
        } else {
            raw_data <- dummy_coded_data %>%
                dplyr::select(dplyr::matches(factor_to_explore[1]), dplyr::matches(factor_to_explore[2]), dplyr::contains("cluster")) %>%
                dplyr::ungroup()
        }
        return(raw_data)
    }
    
    for_three <- function(dummy_coded_data, factor_to_explore, variable_to_find_proportion){
        if (!is.null(variable_to_find_proportion)) {
            raw_data <- dummy_coded_data %>%
                dplyr::select(dplyr::matches(variable_to_find_proportion), dplyr::matches(factor_to_explore[1]), dplyr::matches(factor_to_explore[2]), dplyr::matches(factor_to_explore[3]), dplyr::contains("cluster")) %>%
                dplyr::group_by_(variable_to_find_proportion, factor_to_explore) %>%
                dplyr::summarize_each(dplyr::funs(mean)) %>%
                dplyr::ungroup()
        } else {
            raw_data <- dummy_coded_data %>%
                dplyr::select(dplyr::matches(factor_to_explore), dplyr::matches(factor_to_explore[1]), dplyr::matches(factor_to_explore[2]), dplyr::matches(factor_to_explore[3]), dplyr::contains("cluster")) %>%
                dplyr::ungroup()
        }
        return(raw_data)
    }
    
    if (length(factor_to_explore) == 1) {
        out <- for_one(dummy_coded_data, factor_to_explore, variable_to_find_proportion)
    } else if (length(factor_to_explore) == 2) {
        out <- for_two(dummy_coded_data, factor_to_explore, variable_to_find_proportion)
    } else if (length(factor_to_explore) == 3) {
        out <- for_three(dummy_coded_data, factor_to_explore, variable_to_find_proportion)
    }
    
    return(out)
}

#' finds n by condition 
#'
#' @export

find_n <- function(raw_data, factor_to_explore){
    
    if (length(factor_to_explore) == 1) {
        out <- 
            raw_data %>%
            dplyr::group_by_(factor_to_explore) %>%
            dplyr::summarize(n = n())
    } else if (length(factor_to_explore) == 2) {
        out <- 
            raw_data %>%
            dplyr::group_by_(factor_to_explore[1], factor_to_explore[2]) %>%
            dplyr::summarize(n = n())    
    } else if (length(factor_to_explore) == 3) {
        out <- 
            raw_data %>%
            dplyr::group_by_(factor_to_explore[1], factor_to_explore[2], factor_to_explore[3]) %>%
            dplyr::summarize(n = n())
    }
    
}

#' creates process data for summary table and plot
#'
#' @export

create_processed_data <- function(raw_data, factor_to_explore, variable_to_find_proportion){
    
    for_one <- function(dummy_coded_data, factor_to_explore, variable_to_find_proportion){
        if (!is.null(variable_to_find_proportion)) {
            processed_data <- raw_data %>%
                dplyr::select(-dplyr::matches(variable_to_find_proportion)) %>%
                dplyr::group_by_(factor_to_explore) %>%
                dplyr::summarize_each(dplyr::funs(mean))
        } else {
            processed_data <- raw_data %>%
                dplyr::group_by_(factor_to_explore) %>%
                dplyr::summarize_each(dplyr::funs(mean))
        }
        return(processed_data)
    }
    
    for_two <- function(dummy_coded_data, factor_to_explore, variable_to_find_proportion){
        if (!is.null(variable_to_find_proportion)) {
            processed_data <- raw_data %>%
                dplyr::select(-dplyr::matches(variable_to_find_proportion)) %>%
                dplyr::group_by_(factor_to_explore[1], factor_to_explore[2]) %>%
                dplyr::summarize_each(dplyr::funs(mean))
        } else {
            processed_data <- raw_data %>%
                dplyr::group_by_(factor_to_explore[1], factor_to_explore[2]) %>%
                dplyr::summarize_each(dplyr::funs(mean))
        }
        return(processed_data)
    }
    
    for_three <- function(dummy_coded_data, factor_to_explore, variable_to_find_proportion){
        if (!is.null(variable_to_find_proportion)) {
            processed_data <- raw_data %>%
                dplyr::select(-dplyr::matches(variable_to_find_proportion)) %>%
                dplyr::group_by_(factor_to_explore[1], factor_to_explore[2], factor_to_explore[3]) %>%
                dplyr::summarize_each(dplyr::funs(mean))
        } else {
            processed_data <- raw_data %>%
                dplyr::group_by_(factor_to_explore[1], factor_to_explore[2], factor_to_explore[3]) %>%
                dplyr::summarize_each(dplyr::funs(mean))
        }
        return(processed_data)
    }
    
    if (length(factor_to_explore) == 1) {
        out <- for_one(dummy_coded_data, factor_to_explore, variable_to_find_proportion)
    } else if (length(factor_to_explore) == 2) {
        out <- for_two(dummy_coded_data, factor_to_explore, variable_to_find_proportion)
    } else if (length(factor_to_explore) == 3) {
        out <- for_three(dummy_coded_data, factor_to_explore, variable_to_find_proportion)
    }
    
    return(out)
    
}

#' create plot to explore factors
#'
#' @export

create_plot_to_explore_factors <- function(processed_data, factor_to_explore, cluster_names){
    processed_data <- processed_data[complete.cases(processed_data), ]
    if (length(factor_to_explore) == 1) {
        to_plot <- tidyr::gather(processed_data, cluster, mean, -dplyr::matches(factor_to_explore))
        # to_plot <- to_plot[!is.na(dplyr::matches(factor_to_explore)), ]
        out <- ggplot(dplyr::arrange(to_plot, desc(cluster)), ggplot2::aes(y = mean, fill = cluster)) +
            ggplot2::aes_string(x = factor_to_explore) +
            ggplot2::geom_bar(stat = "identity", color = "black") +
            xlab("") +
            ylab("Proportion of Responses") +
            ggtitle("") +
            ggplot2::theme(legend.position = "top") +
            ggplot2::theme(legend.title = element_blank()) +
            ggplot2::theme(text=element_text(size = 12, family = "Times")) +
            ggplot2::theme(axis.text.x = element_text(angle = 45)) +
            ggplot2::theme(legend.position = "right") +
            ggplot2::theme(legend.title = element_blank()) +
            ggplot2::scale_fill_discrete(name = "Cluster", labels = cluster_names)
    } else if (length(factor_to_explore) == 2) {
        to_plot <- tidyr::gather(processed_data, cluster, mean, -dplyr::matches(factor_to_explore[1]), -dplyr::matches(factor_to_explore[2]))
        out <- ggplot2::ggplot(dplyr::arrange(to_plot, desc(cluster)), ggplot2::aes(y = mean, fill = cluster)) +
            ggplot2::aes_string(x = factor_to_explore[1]) +
            ggplot2::facet_wrap(as.formula(paste("~", factor_to_explore[2]))) +
            ggplot2::geom_bar(stat = "identity", color = "black") +
            ggplot2::xlab("") +
            ggplot2::ylab("Proportion of Responses") +
            ggplot2::ggtitle("") +
            ggplot2::theme(legend.position = "top") +
            ggplot2::theme(legend.title = element_blank()) +
            ggplot2::theme(text=element_text(size = 12, family = "Times")) +
            ggplot2::theme(axis.text.x = element_text(angle = 45)) +
            ggplot2::theme(legend.position = "right") +
            ggplot2::theme(legend.title = element_blank()) +
            ggplot2::scale_fill_discrete(name = "Cluster", labels = cluster_names)
    } else if (length(factor_to_explore) == 3) {
        to_plot <- tidyr::gather(processed_data, cluster, mean, -matches(factor_to_explore[1]), -matches(factor_to_explore[2]), -matches(factor_to_explore[3]))
        out <- ggplot2::ggplot(dplyr::arrange(to_plot, desc(cluster)), ggplot2::aes(y = mean, fill = cluster, order =)) +
            ggplot2::aes_string(x = factor_to_explore[1]) +
            ggplot2::facet_wrap(as.formula(paste("~", factor_to_explore[2], " + ", factor_to_explore[3]))) +
            ggplot2::geom_bar(stat = "identity", color = "black") +
            ggplot2::xlab("") +
            ggplot2::ylab("Proportion of Responses") +
            ggplot2::ggtitle("") +
            ggplot2::theme(legend.position = "top") +
            ggplot2::theme(legend.title = element_blank()) +
            ggplot2::theme(text=element_text(size = 12, family = "Times")) +
            ggplot2::theme(axis.text.x = element_text(angle = 45)) +
            ggplot2::theme(legend.position = "right") +
            ggplot2::theme(legend.title = element_blank()) +
            ggplot2::scale_fill_discrete(name = "Cluster", labels = cluster_names)
    }
    return(out)
}

# create_compare_manova <- function(processed_data, factor_to_explore){
#     factor_to_explore <- "race"
#     processed_data <- explored_factors[[2]]
#     df <- as.data.frame(processed_data)
#     df$DV <- as.matrix(cbind(df[, 3:ncol(df)]))
#     x <- paste0("DV ~ ", factor_to_explore, sep = "")
#     out <- manova(as.formula(x), data = df)
#     return(out)
# }

#' compares between levels of factor using ANOVA
#'
#' @export

create_compare_anova <- function(processed_data, variable_to_find_proportion, cluster_names, factor_to_explore){
    df <- processed_data
    out <- list()
    out_tukey <- list()
    
    for_one <- function(processed_data, variable_to_find_proportion, cluster_names){
        if (!is.null(variable_to_find_proportion)){
            names(df)[2] <- "DV"
            for (i in 1:(ncol(processed_data) - 2)){
                x <- paste0("cluster", i, " ~ DV", sep = "")
                out[[i]] <- summary(aov(as.formula(x), data = df))
                out_tukey[[i]] <- stats::TukeyHSD(aov(as.formula(x), data = df))
            }
        } else {
            names(df)[1] <- "DV"
            for (i in 1:(ncol(processed_data) - 1)){
                x <- paste0("cluster", i, " ~ DV", sep = "")
                out[[i]] <- summary(aov(as.formula(x), data = df))
                out_tukey[[i]] <- stats::TukeyHSD(aov(as.formula(x), data = df))
            }
        }
        out <- list(out, out_tukey)
        names(out[[1]]) <- cluster_names
        names(out[[2]]) <- cluster_names
        return(out)
    }
    
    for_two <- function(processed_data, variable_to_find_proportion, cluster_names){
        if (!is.null(variable_to_find_proportion)){
            names(df)[2] <- "DV1"
            names(df)[3] <- "DV2"
            for (i in 1:(ncol(processed_data) - 3)){
                x <- paste0("cluster", i, " ~ DV1*DV2", sep = "")
                out[[i]] <- summary(aov(as.formula(x), data = df))
                out_tukey[[i]] <- stats::TukeyHSD(aov(as.formula(x), data = df))
            }
        } else {
            names(df)[1] <- "DV1"
            names(df)[2] <- "DV2"
            for (i in 1:(ncol(processed_data) - 2)){
                x <- paste0("cluster", i, " ~ DV1*DV2", sep = "")
                out[[i]] <- summary(aov(as.formula(x), data = df))
                out_tukey[[i]] <- stats::TukeyHSD(aov(as.formula(x), data = df))
            }
        }
        out <- list(out, out_tukey)
        names(out[[1]]) <- cluster_names
        names(out[[2]]) <- cluster_names
        return(out)
    }
    
    for_three <- function(processed_data, variable_to_find_proportion, cluster_names){
        if (!is.null(variable_to_find_proportion)){
            names(df)[2] <- "DV1"
            names(df)[3] <- "DV2"
            names(df)[4] <- "DV2"
            for (i in 1:(ncol(processed_data) - 4)){
                x <- paste0("cluster", i, " ~ DV1*DV2*DV3", sep = "")
                out[[i]] <- summary(aov(as.formula(x), data = df))
                out_tukey[[i]] <- stats::TukeyHSD(aov(as.formula(x), data = df))
            }
        } else {
            names(df)[1] <- "DV"
            names(df)[2] <- "DV2"
            names(df)[3] <- "DV3"
            for (i in 1:(ncol(processed_data) - 3)){
                x <- paste0("cluster", i, " ~ DV1*DV2*DV3", sep = "")
                out[[i]] <- summary(aov(as.formula(x), data = df))
                out_tukey[[i]] <- stats::TukeyHSD(aov(as.formula(x), data = df))
            }
        }
        out <- list(out, out_tukey)
        names(out[[1]]) <- cluster_names
        names(out[[2]]) <- cluster_names
        return(out)
    }
    
    if (length(factor_to_explore) == 1) {
        out <- for_one(processed_data, variable_to_find_proportion, cluster_names)
    } else if (length(factor_to_explore) == 2) {
        out <- for_two(processed_data, variable_to_find_proportion, cluster_names)
    } else if (length(factor_to_explore) == 3) {
        out <- for_three(processed_data, variable_to_find_proportion, cluster_names)
    }
    
    # names(out) <- cluster_names
    return(out)
}

#' removes incomplete cases
#'
#' @export

removed_obs_df_maker <- function(raw_data_matrix, cases_to_keep){
    removed_obs_df <- data.frame(row = row.names(raw_data_matrix), raw_data_matrix, stringsAsFactors = F)
    removed_obs_df$reason_removed <- NA
    removed_obs_df$reason_removed[!cases_to_keep] <- "incomplete_case"
    return(removed_obs_df)
}

#' detects univariate outliers
#'
#' @export

uv_outlier_detector <- function(x, na.rm = T, ...) {
    # need to figure out where this came from - from a SO question, can probably re-write
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    return(y)
}

#' remove uv outliers
#'
#' @export

remove_uv_out_func <- function(data){
    x <- sapply(data, uv_outlier_detector)
    return(x)
}

#' remove mv outliers
#'
#' @export

remove_mv_out_func <- function(data){
    mvout <- chemometrics::Moutlier(data, quantile = 0.99, plot = F)
    the_index <- which(mvout$md > mvout$cutoff)
    if (any(the_index) == T){
        return(the_index)
    } else{
        return(data)
    }
}

#' main function to remove mv outliers
#'
#' @export

remove_mv_main_func <- function(data, removed_obs_df, cases_to_keep, found_uv_outlier_bool = F, uv_outliers = NULL, print_status){
    out_tmp <- remove_mv_out_func(data)
    if(print_status == T){
        print(paste0("### Note: ", length(out_tmp), " cases with multivariate outliers out of ", nrow(data), " cases removed, so ", nrow(data) - length(out_tmp), " used in subsequent analysis ###"))
    }
    x <- removed_obs_df[cases_to_keep, ]
    if(found_uv_outlier_bool == T){ 
        y <- x[-uv_outliers, ]
        z <- as.numeric(y$row[out_tmp])
    } else{
        z <- as.numeric(x$row[out_tmp])
    }
    removed_obs_df$reason_removed[z] <- "multivariate_outlier"
    data_out <- data[-out_tmp, ] # this is the first list item (data with mv outliers removed), second is the cases to be output as an attribute returned from prepare_data()
    data_out <- list(data_out, removed_obs_df)
    return(data_out)
}

#' compare cluster fit index statistics
#'
#' @export

comparision_of_statistics_plot <- function(data, lower_num, upper_num){
    ggplot2::ggplot(data, aes(x = number_of_clusters, y = proportion_of_variance_explained)) +
        ggplot2::geom_point() +
        # scale_x_continuous(breaks = lower_num:upper_num) +
        ggplot2::ylab("Proportion of Variance Explained (R^2)") +
        ggplot2::xlab("Number of Clusters")  
}

#' split halves
#'
#' @export

splitting_halves <- function(x){
    x <- data.frame(matrix(unlist(x), ncol = length(x), byrow = F))
    if (nrow(x) %/% 2 == 0){
        y1 <- nrow(x) / 2
        y2 <- y1
    } else{
        y1 <- nrow(x) %/% 2
        y2 <- y1 + 1
    }
    y <- c(rep(1, y1), rep(0, y2))
    z <- rnorm(nrow(x))
    df <- dplyr::arrange(data.frame(y, z), z)
    y_shuffled <- df$y
    half_one <- as.data.frame(x[y_shuffled == 0, ])
    half_two <- as.data.frame(x[y_shuffled == 1, ])
    out <- list(half_one, half_two)
    return(out)
}

#' try to cluster halves
#'
#' @export

try_to_cluster_halves <- function(prepared_data, args){
    out <- tryCatch({
        create_profiles(prepared_data, args[[2]], args[[3]], args[[4]], print_status = F)
    },
    error = function(cond){
        #warning("Did not properly converge, trying again.")
        return(NA)
    },
    finally = {
        
    }
    )
    return(out)
}

#' clustering the halves functions
#'
#' @export

cluster_the_halves <- function(split_halves, args){
    df <- data.frame(matrix(unlist(prepared_data), ncol = length(prepared_data), byrow = F))
    
    clustered_half_one <- try_to_cluster_halves(split_halves[[1]], args)
    if(!is.character(clustered_half_one)){
        clustered_half_one <- calculate_stats(clustered_half_one, print_status = F)[[5]]
        print(paste0("### Proportion of variance explained (R^2) = ", round(clustered_half_one, 3)))
    } else{
        clustered_half_one<- NA
    }
    
    clustered_half_two <- try_to_cluster_halves(split_halves[[2]], args)
    if(!is.character(clustered_half_two)){
        clustered_half_two <- calculate_stats(clustered_half_two, print_status = F)[[5]]
        print(paste0("### Proportion of variance explained (R^2) = ", round(clustered_half_two, 3)))
    } else{
        clustered_half_two <- NA
    }
    
    out <- list(clustered_half_one, clustered_half_two)
    return(out)
}

#' try to cluster
#'
#' @export

try_to_cluster <- function(prepared_data, args, i){
    out <- tryCatch(
        {
            tmp <- create_profiles(prepared_data, i, args[[3]], args[[4]], print_status = F)
        },
        error = function(cond){
            return(warning("Did not properly converge, try a different lower_num or upper_num."))
        },
        finally = {
            # print(paste0("### Processed cluster solution with ", i, " clusters"))
        }
    )
    return(out)
}

#' calculate stats for halves
#'
#' @export

calculate_the_stats <- function(clustered_halves, variable_names){ #fix
    half_one_stats <- calculate_stats(clustered_halves[[1]], variable_names, print_status = F)
    half_two_stats <- calculate_stats(clustered_halves[[2]], variable_names, print_status = F)
    out <- list(half_one_stats, half_two_stats)
    return(out)
}

#' finding the nearest centroids
#'
#' @export

find_nearest_centroid <- function(split_halves, calculated_stats){
    a <- split_halves[[1]] # keep
    a_assign <- calculated_stats[[1]][[3]] # keep
    b_centroid <- calculated_stats[[2]][[7]][, 1:3] # keep
    z <- fields::rdist(a, b_centroid)
    a_assign_star <- apply(z, 1, function(x) which.min(x))
    return(a_assign_star)
}

#' calculate agreement and kappa
#'
#' @export

calculate_agreement <- function(a_assign_star, a_assign){
    out <- list()
    tab <- table(a_assign_star, a_assign[[1]])
    res <- lpSolve::lp.assign(-tab)
    l <- apply(res$solution > 0.5, 1, which)
    a_assign_star_recode <- l[a_assign_star]
    tmp_mat <- data.frame(a_assign_star_recode, a_assign)
    out[[1]] <- irr::kappa2(tmp_mat)
    out[[2]] <- irr::agree(tmp_mat)
    return(out)
}

#' Pre-processing function to prepare data for subsequent analysis
#'@param raw_data_matrix data frame or matrix of any dimensions with numeric data
#'@param method_of_centering string indicating variable-wise centering, options include "grand" (for grand mean centering), "group" (for group mean centering; requires a grouping vector, described next) and "raw", which does not center the variables
#'@param grouping_vector a vector indicating how the cases are to be grouped for group mean centering
#'@param to_standardize boolean indicating whether to standardize (TRUE) or not (FALSE)
#'@param remove_uv_outliers boolean indicating whether to remove (TRUE) univariate outlier or not (FALSE)
#'@param remove_mv_outliers boolean indicating whether to remove (TRUE) multivariate outlier or not (FALSE)
#'@param print_status boolean indicating whether to print information about the output (TRUE) or to not print information about the output (FALSE)
#'@export

prepare_data <- function(raw_data_matrix, method_of_centering = "raw", grouping_vector = NULL, to_standardize = F, remove_uv_outliers = F, remove_mv_outliers = F, print_status = F){
    cases_to_keep <- complete.cases(raw_data_matrix) # to use later for comparing function to index which cases to keep
    removed_obs_df <- removed_obs_df_maker(raw_data_matrix, cases_to_keep)
    data_tmp <- raw_data_matrix[cases_to_keep, ] # removes incomplete cases
    print("### Created the following output ... ")
    print("### 1. Prepared data ###")
    if(print_status == T){
        if(length(table(cases_to_keep)) > 1){
            print(paste0("### Note: ", table(cases_to_keep)[1], " incomplete cases out of ", sum(table(cases_to_keep)), " total cases removed, so ", sum(table(cases_to_keep)) - table(cases_to_keep)[1], " used in subsequent analysis ###"))
        } else{
            print(paste0("### Note: 0 incomplete cases out of ", nrow(data_tmp), " total cases removed, so ", nrow(data_tmp), " used in subsequent analysis ###"))
        }
    }
    if (remove_uv_outliers == T){
        tmp1 <- remove_uv_main_func(data_tmp, removed_obs_df, cases_to_keep, print_status)
        data_tmp <- tmp1[[1]]
        removed_obs_df <- tmp1[[2]]
    }
    if(any(as.character(removed_obs_df$reason_removed) == "univariate_outlier", na.rm = T)){
        found_uv_outlier_bool <- T
    } else{
        found_uv_outlier_bool <- F
    }
    if (remove_mv_outliers == T){
        tmp2 <- remove_mv_main_func(data_tmp, removed_obs_df, cases_to_keep, found_uv_outlier_bool, uv_outliers = tmp1, print_status)
        data_tmp <- tmp2[[1]]
        removed_obs_df <- tmp2[[2]]
    }
    grouping_vector <- grouping_vector[cases_to_keep]
    out <- centering_function(as.data.frame(data_tmp), method_of_centering, grouping_vector, to_standardize)
    cases_to_keep = row.names(raw_data_matrix) %in% removed_obs_df$row[is.na(removed_obs_df$reason_removed)]
    attributes(out) <- list(uncentered_cleaned_data = data_tmp, 
                            method_of_centering = method_of_centering, cases_to_keep = cases_to_keep, 
                            cases_removed_df = removed_obs_df[, 2:5], variable_names = names(raw_data_matrix))
    if(print_status == T){
        print("### Note. Print the cases_removed_df attribute to view cases removed ###")
    }
    return(out)
}

#' Create profiles function
#'@param prepared_data output from the prepare_data() function
#'@param n_clusters the number of clusters; specified a priori
#'@param distance_metric metric for calculating the distance matrix used in hierarchical clustering, options include "euclidean", "squared_euclidean", and others (see ?dist() for more details)
#'@param linkage method for combining clusters in hierarchical clustering, options include "complete", "average", and others (see ?hclust() for details)
#'@param print_status boolean indicating whether to print information about the output (TRUE) or to not print information about the output (FALSE)
#'@export

create_profiles <- function(prepared_data,
                            n_clusters,
                            distance_metric = "squared_euclidean",
                            linkage = "complete", 
                            print_status = T) {
    df <- data.frame(matrix(unlist(prepared_data), ncol = length(prepared_data), byrow = F))
    names(df) <- names(prepared_data)
    args <- list(prepared_data, n_clusters, distance_metric, linkage)
    out <- list() # this collects the output
    distance_matrix <- distance_function(df, distance_metric)
    out[[1]] <- hclust(distance_matrix, method = linkage) # hierarhical clustering
    starting_points <- hclust_to_kmeans_function(df, out[[1]], n_clusters)
    out[[2]] <- kmeans_function(df, starting_points) # Fits k-means algorithm using results from hierarchical algorithm as start value
    attributes(out) <- list(cleaned_data = df,
                            uncentered_cleaned_data = attributes(prepared_data)$uncentered_cleaned_data, 
                            n_clusters_attr = n_clusters, data_attr = df, args_attr = args, 
                            cases_to_keep_attr = attributes(prepared_data)$cases_to_keep, 
                            variable_names = attributes(prepared_data)$variable_names)
    if(print_status == T){
        print("### Created the following output ... ")
        print("### 1. Hierarchical cluster analysis output ###")
        print("### 2. K-means cluster analysis output ###")
    }
    invisible(out)
}

#' Function to calculate statistics about cluster solution found via cluster_data()
#'@param clustering_output output from cluster_data() function
#'@param cluster_names optional names for clusters, useful for creating plot
#'@param print_status boolean indicating whether to print information about the output (TRUE) or to not print information about the output (FALSE)
#'@param to_standardize boolean indicating whether to standardize the raw data used to create cluster centroid plots
#'@param plot_uncentered_data boolean indicating whether to plot cluster centroid plots with raw data
#'@param fill_order optional order for clustered variables
#'@param the_order optional order for cluster profiles
#'@param legend_title optional legend title
#'@param font_size optional font size
#'@export

calculate_stats <- function(clustering_output, 
                            cluster_names = NULL, 
                            to_standardize = F,
                            print_status = T,
                            plot_uncentered_data = F,
                            fill_order = NULL,
                            the_order = NULL,
                            legend_title = NULL,
                            font_size = 14){
    out <- list()
    variable_names <- attributes(clustering_output)$variable_names
    # this function takes a list, clustering output, from the cluster_data function
    options(max.print = 100000)
    
    out[[1]] <- dissim_function(clustering_output[[1]]) # agglomeration schedule - currently out of order
    out[[2]] <- clustering_output[[1]] # dendrogram
    out[[3]] <- cutree(clustering_output[[1]], attributes(clustering_output)$n_clusters_attr) # hclust assignment
    out[[4]] <- clustering_output[[2]]$cluster # kmeans assignment
    out[[5]] <- (clustering_output[[2]]$totss - sum(clustering_output[[2]]$withinss)) / clustering_output[[2]]$totss # proportion of variance explained
    out[[6]] <- try_manova(attributes(clustering_output)$data_attr, out[[4]], variable_names)
    if (to_standardize == T){
        tmp <- standardize_function(attributes(clustering_output)$data_attr)
        out[[7]] <- cluster_freq_function(tmp, attributes(clustering_output)$n_clusters_attr, clustering_output[[2]], variable_names)
    } else {
        out[[7]] <- cluster_freq_function(attributes(clustering_output)$data_attr, attributes(clustering_output)$n_clusters_attr, clustering_output[[2]], variable_names)
    }
    
    if (!is.null(the_order)){
        out[[7]]$the_order <- the_order
        out[[7]] <- dplyr::arrange(out[[7]], the_order)
        out[[7]]$the_order <- NULL
    }
    
    if (!is.null(cluster_names)){
        out[[7]]$Cluster <- factor(cluster_names, levels = cluster_names)
    }
    
    out[[8]] <- cluster_plot_function(out[[7]], font_size, fill_order)
    
    if (plot_uncentered_data == T){
        tmp <- cluster_freq_function(attributes(clustering_output)$uncentered_cleaned_data, attributes(clustering_output)$n_clusters_attr, clustering_output[[2]], variable_names)
        
        if (!is.null(the_order)){
            tmp$the_order <- the_order
            tmp <- dplyr::arrange(tmp, the_order)
            tmp$Cluster <- factor(cluster_names, levels = cluster_names)
            tmp$the_order <- NULL
        } 
        
        if (!is.null(cluster_names)){
            out[[7]]$Cluster <- factor(cluster_names, levels = cluster_names)
        }
        
        out[[9]] <- cluster_plot_function(tmp, font_size, fill_order)
    }
    
    attributes(out) <- list(cleaned_data = attributes(clustering_output)$cleaned_data,
                            cluster_names = out[[7]]$Cluster, n_clusters_attr = attributes(clustering_output)$n_clusters_attr, 
                            args_attr = args, 
                            cases_to_keep = attributes(clustering_output)$cases_to_keep,
                            variable_names = attributes(clustering_output)$variable_names)
    if(print_status == T){
        print("### Created the following output ... ")
        print("### 1. Hierarchical cluster analysis diagnostics: Agglomeration schedule ###")
        print("### 2. Hierarchical cluster analysis diagnostics: hclust object to coerce using as.dendrogram then to plot() ###")
        print("### 3. Hierarchical cluster analysis assignments ###")
        print("### 4. K-means cluster analysis assignments ###")
        print("### 5. K-means cluster analysis diagnostics: Proportion of variance explained (R^2) ###")
        print("### 6. Overall diagnostics: MANOVA ###")
        print("### 7. Overall output: Cluster centroids ###")
        print("### 8. Overall output: ggplot2 object for plot of cluster centroids with centered data ###")
        if(plot_uncentered_data == T){
            print("### 9. Overall output: ggplot2 object for plot of cluster centroids with raw data ###")
        }
    }
    invisible(out)
}

#' Function to explore frequency of clusters across select factors
#'@param statistics from create_statistics() function
#'@param factor_data_frame data frame of select factors
#'@param factor_to_explore specific factor to explore
#'@param variable_to_find_proportion variable to normalize clusters as a unit of analysis
#'@param cluster_names optional names for clusters, useful for interpreting findings
#'@param print_status boolean indicating whether to print information about the output (TRUE) or to not print information about the output (FALSE)
#'@details To explore the frequency of clusters across factors
#'  and cleaning the corpus, deviationalizing and clustering.
#'@export

explore_factors <- function(statistics,
                            factor_data_frame, 
                            factor_to_explore, 
                            variable_to_find_proportion = NULL, 
                            cluster_names = NULL,
                            print_status = T){
    cluster_assignments = statistics[[4]]
    cases_to_keep = attributes(statistics)$cases_to_keep
    variable_names = attributes(statistics)$variable_names
    out <- list()
    factor_data_frame[] <- lapply(factor_data_frame, factor)
    data <- merge_assignments_and_factors(cluster_assignments, cases_to_keep, factor_data_frame)
    data_for_descriptive_stats <- data.frame(attributes(statistics)$cleaned_data, data)
    dummy_coded_data <- dummmy_code_cluster_assignments(data)
    #dummy_coded_data <- dummy_coded_data[!is.nan(factor_to_explore), ]
    out[[1]] <- create_crosstab(data, factor_to_explore)
    out[[2]] <- create_raw_data(dummy_coded_data, factor_to_explore, variable_to_find_proportion)
    out[[3]] <- create_processed_data(out[[2]], factor_to_explore, variable_to_find_proportion)
    out[[4]] <- create_plot_to_explore_factors(out[[3]], factor_to_explore, cluster_names)
    out[[5]] <- find_n(out[[2]], factor_to_explore)
    out[[6]] <- create_compare_anova(out[[2]], variable_to_find_proportion, cluster_names, factor_to_explore)
    # out[[7]] <- create_compare_manova()
    
    if(print_status == T){
        print("### Created the following output ... ")
        print("### 1. Comparison table ###")
        print("### 2. Processed data: Raw ###")
        print("### 3. Processed data: Summary ###")
        print("### 4. ggplot2 object  ###")
        print("### 5. Number by factor  ###")
        print("### 6. ANOVA [[1]] and Tukey HSD [[2]] ###")
        # print("### 7. MANOVA ###")
    }
    attributes(out) <- list(cleaned_df = data_for_descriptive_stats, data_attr = data)
    invisible(out)
    
}

#' Function to compare the proportion of variance explained for cluster solutions with varying number of clusters
#'@param prepared_data output from the prepare_data() function
#'@param args list of arguments passed to the create_profiles() function, which is returned as an attribute of its output (i.e., attributes(output)$args_attr)
#'@param lower_num integer representing the lower bound of the range of the number of clusters in the cluster solution
#'@param upper_num integer representing the lower bound of the range of the number of clusters in the cluster solution
#'@details Function to compare the proportion of variance explained for cluster solutions with varying number of clusters
#'@export

compare_cluster_statistics <- function(prepared_data, args, lower_num, upper_num){ # can also be method_of_centering (and grouping vector) and to_standardize for now
    if(lower_num == 1) {
        lower_num <- 2
        warning("Cannot find cluster solution with 1 cluster; skipped calculation of cluster solution with 1 cluster")
    }
    tmp_vec <- vector(length = upper_num)
    for (i in lower_num:upper_num){
        tmp <- try_to_cluster(prepared_data, args, i)
        if(!is.character(tmp)){
            tmp <- calculate_stats(tmp, print_status = F)[[5]]
            # print(paste0("### Proportion of variance explained (R^2) = ", round(tmp, 3)))
        } else{
            tmp <- NA
        }
        tmp_vec[i] <- tmp
    }
    number_of_clusters <- 1:upper_num
    proportion_of_variance_explained <- tmp_vec
    number_of_clusters <- number_of_clusters[proportion_of_variance_explained != 0]
    proportion_of_variance_explained <- proportion_of_variance_explained[proportion_of_variance_explained != 0]
    if(any(is.na(proportion_of_variance_explained))) {
        number_of_clusters <- number_of_clusters[!is.na(proportion_of_variance_explained)]
        proportion_of_variance_explained <- proportion_of_variance_explained[!is.na(proportion_of_variance_explained)]
    }
    out <- data.frame(number_of_clusters, proportion_of_variance_explained)
    out_plot <- comparision_of_statistics_plot(out, min(out$number_of_clusters), max(out$number_of_clusters))
    out <- list(out, out_plot)
    print("### Created the following output ... ")
    print("### 1. Data frame of proportion of variance explained by number of clusters ###")
    print("### 2. ggplot object of of proportion of variance explained by number of clusters ###")
    return(out)
}

#' Function to cross-validate the cluster solution using split half or other cross validation 
#'@param prepared_data output from the prepare_data() function
#'@param output output from the create_profiles() function
#'@param k integer for the number of cross-validation attempts
#'@param variable_names optional names for clustered variables
#'@details Function to cross-validate the cluster solution using split half or other cross validation 
#'@export

cross_validate <- function(prepared_data, output, variable_names, k){
    kappa_collector <- vector()
    agree_collector <- vector()
    for (i in 1:k){
        # print(paste0("### Processing cross validation iteration ", i))
        x <- splitting_halves(prepared_data)
        y <- cluster_the_halves(x, attributes(output)$args_attr)
        test <- all(!is.na(y[[1]]) & !is.na(y[[2]]))
        if(test){
            z <- calculate_the_stats(y, variable_names)
            a_assign_star <- find_nearest_centroid(split_halves = x, calculated_stats = z)
            zzz <- calculate_agreement(a_assign_star, z[[1]][4])
            kappa_collector[[i]] <- round(zzz[[1]]$value, 3)
            agree_collector[[i]] <- round(zzz[[2]]$value * .01, 3)
        } else{
            kappa_collector[[i]] <- NA
            agree_collector[[i]] <- NA
        }
        # print(paste0("Kappa: ", kappa_collector[[i]]))
        # print(paste0("Agreement: ", agree_collector[[i]]))
    }
    mean_kappa <- paste0("Mean Kappa for ", k, " attempts: ", round(mean(kappa_collector, na.rm = T), 3))
    mean_agree <- paste0("Mean agreement for ", k, " attempts: ", round(mean(agree_collector, na.rm = T), 3))    
    out <- list(kappa_collector, agree_collector, mean_kappa, mean_agree)
    print("### Created the following output ... ")
    print(paste0("### 1. Vector of Cohen's Kappa of length ", k, " ###"))
    print(paste0("### 2. Vector of agreement of length ", k, " ###"))
    print(paste0("### 3. Mean Cohen's Kappa for ", k, " attempts ###"))
    print(paste0("### 4. Mean agreement for ", k, " attempts  ###"))
    return(out)
}

# Wrapper function to perform all analyses at once - still in development
# prcr <- function(raw_data_matrix, method_of_centering = "raw", grouping_vector = NULL, 
#                  to_standardize = T, remove_uv_outliers = T, remove_mv_outliers = T, print_status = F,
#                  n_clusters, distance_metric = "squared_euclidean", linkage = "complete",
#                  variable_to_find_proportion = NULL, factor_data_frame, factor_to_explore, 
#                  compare_statistics = F, cross_validate = F)

#############################################
### 1. Setting up, loading data #############
#############################################

# change this directory to the folder with the data files in them

setwd("/Users/joshuarosenberg/Dropbox/1_Research/SciMo/JRST\ MEP\ Paper\ Final\ Docs")

library(ggplot2) # need to install with install.packages('ggplot2') if not yet installed
library(multcomp) # need to install with install.packages('multcomp') if not yet installed 
library(tidyverse) # need to install with install.packages('tidyverse') if not yet installed
library(MASS) # need to install with install.packages('MASS') if not yet installed
library(haven) # need to install with install.packages('haven') if not yet installed

esm_df <- haven::read_sav('esm_data.sav')

to_remove <- esm_df$stud_ID == "AXR050594" & esm_df$month == 10 & esm_df$day == 20 & esm_df$year == 8 & esm_df$signal == 2 # removing 10/20/8, ID = "AXR050594", second beeper
table(to_remove)
nrow(esm_df)
esm_df <- esm_df[!to_remove, ]
nrow(esm_df) # - 1 case

data <-
    esm_df %>%
    mutate(behavioral_scale = ifelse(!is.na(conc) & !is.na(hardwk), (conc + hardwk) / 2, 
                                     ifelse(is.na(conc), hardwk, conc)),
           cognitive_scale = ifelse(!is.na(imp_y) & !is.na(imp_fut), (imp_y + imp_fut) / 2, 
                                    ifelse(is.na(imp_y), imp_fut, imp_y)),
           affective_scale = ifelse(!is.na(enjoy) & !is.na(interest), (enjoy + interest) / 2, 
                                    ifelse(is.na(enjoy), interest, enjoy))) %>%
    dplyr::select(behavioral_scale,
                  cognitive_scale,
                  affective_scale)

cor(as.matrix(data)[, 1], as.matrix(data)[, 2], use = 'complete.obs')
cor.test(as.matrix(data)[, 2], as.matrix(data)[, 3], use = 'complete.obs')

#############################################
### 2. Cluster analysis #####################
#############################################

names(data) <- c("Behavioral Engagement ", "Cognitive Engagement ", "Affective Engagement ")

prepared_data <- prepare_data(data, "grand", grouping_vector = NULL, 
                              to_standardize = T, remove_uv_outliers = F,
                              remove_mv_outliers = T, print_status = T) # this won't work when no cases are removed

created_profiles <- create_profiles(prepared_data, 6, "squared_euclidean", "complete")

cluster_names = c("Full (n = 428)",
                  "Moderately Full (n = 694)",
                  "Rational (n = 497)",
                  "Pleasurable (n = 751)",
                  "Reluctant (n = 722)",
                  "Universally Low (n = 871)")

statistics <- calculate_stats(created_profiles, to_standardize = F, 
                              cluster_names = rev(cluster_names), plot_uncentered_data = T,
                              the_order = c(5, 4, 6, 2, 1, 3),
                              font_size = 17) 

table(statistics[[4]])

statistics[[8]] + 
    geom_col(position = position_dodge(.9)) +
    ylab("Z-score") + 
    #theme(legend.position = "none") +
    # scale_fill_discrete("") + # for scale_fill_grey
    theme(axis.title.y=element_text(margin=margin(0, 25, 0, 0))) +
    #labs(caption = "Cluster centroids are significantly different from one another as assessed using a multivariate analysis of variance (MANOVA).") +
    theme(legend.position = "none") +
    theme(legend.title = element_blank()) +
    theme(text=element_text(size = 13, family = "Helvetica Neue")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1.05)) +
    theme(panel.background = element_blank()) +
    theme(panel.grid.major.y=element_line(color="black", size=.25)) +
    theme(panel.grid.minor.y=element_line(color="black", size=.15)) +
    theme(panel.grid.major.x=element_blank()) +
    theme(axis.ticks.y = element_blank()) +
    guides(fill = guide_legend(reverse = F))  +
    scale_fill_brewer(palette="Set2") +
    scale_y_continuous(labels = scales::comma) +
    theme(axis.text.x=element_blank()) +
    scale_x_discrete(limit = "Universally Low (n = 871)") +
    # scale_x_discrete(limit = "Reluctant (n = 722)")
    #scale_x_discrete(limit = "Pleasurable (n = 751)")
    #scale_x_discrete(limit = "Rational (n = 497)")
    #scale_x_discrete(limit = "Moderately Full (n = 694)")
    # scale_x_discrete(limit = "Full (n = 428)")
    

ggsave("2018-1-21-mep_final-f.png", width = 3, height = 6)


# ggsave("2018-1-18-mep_final_bw.png", width = 12, height = 8.5)

#############################################
### 3. IP ###################################
#############################################

factor_df <- dplyr::select(esm_df, instructional_practice)

factor_df$ip <- as.character(haven::as_factor(factor_df$instructional_practice))

factor_df$ip <- car::recode(factor_df$ip,
                            "c('1 Lecture') = 'Lecture';
                            c('2 Seatwk-Ind', '3 Review-Ind') = 'Individual Work';
                            c('4 Seatwk-Grp', '5 Review-Grp') = 'Group Work';
                            c('6 QuizPrep', '8 TestTaking', '7 QuizRev') = 'Quiz and Test';
                            c('9 Discussion') = 'Discussion';
                            c('11 PresElab', '10 Presentation') = 'Presentation';
                            c('12 Video-Watch', '13 Video-Elab') = 'Video';
                            c('15 Labwork', '14 Lab-Prep', '16 Lab-Review') = 'Laboratory';
                            c('17 NonInstTime') = 'Non-instructional';
                            c('18 OffTask') = NA")

factor_df$ip <- ifelse((factor_df$ip == "Discussion" | factor_df$ip == "Non-instructional" |
                            factor_df$ip == "Presentation" | factor_df$ip == "Video" | factor_df$ip == "Group Work"), "Other", factor_df$ip)

explored_factors <- explore_factors(statistics, 
                                    factor_df, 
                                    factor_to_explore = c("ip"), # first is x, second is faceted
                                    variable_to_find_proportion = NULL, 
                                    cluster_names = cluster_names,
                                    print_status = T)

explored_factors[[2]]$ip <- as.character(explored_factors[[2]]$ip)

explored_factors[[2]]$ip[explored_factors[[2]]$ip == "Individual Work"] <- "Individual Work (n = 651)"
explored_factors[[2]]$ip[explored_factors[[2]]$ip == "Laboratory"] <- "Laboratory (n = 991)"
explored_factors[[2]]$ip[explored_factors[[2]]$ip == "Lecture"] <- "Lecture (n = 542)"
explored_factors[[2]]$ip[explored_factors[[2]]$ip == "Other"] <- "Other (n = 1091)"
explored_factors[[2]]$ip[explored_factors[[2]]$ip == "Quiz and Test"] <- "Quiz and Test (n = 639)"

explored_factors[[2]]$ip <- factor(explored_factors[[2]]$ip,
                                   levels = c("Laboratory (n = 991)",
                                              "Individual Work (n = 651)",
                                              "Lecture (n = 542)",
                                              "Quiz and Test (n = 639)",
                                              "Other (n = 1091)"))

explored_factors[[2]] <- dplyr::select(explored_factors[[2]], 
                                       ip,
                                       `Full` = cluster3,
                                       `Moderately Full` = cluster1,
                                       `Rational` = cluster2,
                                       `Pleasurable` = cluster6,
                                       `Reluctant` = cluster4,
                                       `Universally Low` = cluster5)

out <- explored_factors[[2]] %>% 
    tidyr::gather(key, val, -ip) %>% 
    dplyr::group_by(key, ip) %>% 
    dplyr::filter(!is.na(ip)) %>% 
    dplyr::summarize(sum = sum(val)) 

out <- out %>% filter(ip != "Other (n = 1091)")

out1 <- out %>% tidyr::spread(ip, sum)
lsr::cramersV(out1[, 2:5])
y <- chisq.test(out1[, 2:5])
y <- as.data.frame(y$stdres)

row.names(y) <- out1$key
the_vec <- as.vector(as.matrix(t(y)))

out$label <- ifelse(the_vec > 1.96, "+",
                    ifelse(the_vec < -1.96, "-", ""))

ggplot(out, aes(x = key, y = sum, fill = ip, label = label)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(position = position_stack(.5), size = 6.5) +
    theme(legend.position = "right") +
    theme(legend.title = element_blank()) +
    theme(text=element_text(size = 22, family = "Helvetica Neue")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1.05)) +
    ylab("Number of Responses") +
    xlab(NULL) +
    theme(panel.background = element_blank()) +
    theme(panel.grid.major.y=element_line(color="black", size=.25)) +
    theme(panel.grid.minor.y=element_line(color="black", size=.15)) +
    theme(panel.grid.major.x=element_blank()) +
    theme(axis.ticks.y = element_blank()) +
    guides(fill = guide_legend(reverse = F))  +
    scale_fill_brewer(palette="Set2") +
    scale_y_continuous(labels = scales::comma)

ggsave("2018-1-21-ip_final_bw.png", width = 12, height = 8.33)

#############################################
### 3. All vs. No Choice (Overall) ###########
#############################################

factor_df <- dplyr::select(esm_df, contains("ch"))

factor_df$ch_who[is.na(factor_df$ch_who)] <- 0
factor_df$ch_howdo[is.na(factor_df$ch_howdo)] <- 0
factor_df$ch_mat[is.na(factor_df$ch_mat)] <- 0
factor_df$ch_time[is.na(factor_df$ch_time)] <- 0
factor_df$ch_doing[is.na(factor_df$ch_doing)] <- 0
factor_df$ch_topic[is.na(factor_df$ch_topic)] <- 0
factor_df$ch_defin[is.na(factor_df$ch_defin)] <- 0
factor_df$ch_other[is.na(factor_df$ch_other)] <- 0
factor_df$ch_none[is.na(factor_df$ch_none)] <- 0

factor_df$ch_none <- ifelse(((factor_df$ch_who == 0 & factor_df$ch_howdo == 0 & factor_df$ch_mat == 0 & factor_df$ch_time == 0 & factor_df$ch_doing == 0 &
                                  factor_df$ch_defin == 0 & factor_df$ch_topic == 0 & factor_df$ch_other == 0) & factor_df$ch_none == 0), 1, factor_df$ch_none)

factor_df$ch_none <- ifelse(((factor_df$ch_who == 1 | factor_df$ch_howdo == 1 | factor_df$ch_mat == 1 | factor_df$ch_time == 1 | factor_df$ch_doing |
                                  factor_df$ch_defin == 1 | factor_df$ch_topic == 1 | factor_df$ch_other == 1) & factor_df$ch_none == 1), 0, factor_df$ch_none)

factor_df$ch_doing_defining_topic <- ifelse(factor_df$ch_defin == 1 | factor_df$ch_topic == 1 | factor_df$ch_doing, 1, 0)

filter <- attributes(statistics)$cases_to_keep

factor_df$ch_defining_topic <- ifelse(factor_df$ch_defin == 1 | factor_df$ch_topic == 1, 1, 0)
factor_df$ch_doing_defining_topic <- ifelse(factor_df$ch_defin == 1 | factor_df$ch_topic == 1 | factor_df$ch_doing, 1, 0)

aov_df <- data.frame(who = factor_df$ch_who[filter], mat = factor_df$ch_mat[filter],
                     time = factor_df$ch_time[filter], howdo = factor_df$ch_howdo[filter],
                     doing = factor_df$ch_doing[filter], define = factor_df$ch_defin[filter],
                     topic = factor_df$ch_topic[filter], other = factor_df$ch_other[filter], 
                     defining_topic = factor_df$ch_defining_topic[filter], doing_defining_topic = factor_df$ch_doing_defining_topic[filter],
                     none = factor_df$ch_none[filter],
                     cluster = statistics[[4]])

plot <-
    aov_df %>% 
    dplyr::select(who, mat, time, howdo, other, doing_defining_topic, none, cluster) %>% 
    dplyr::mutate(any = ifelse(who == 1 | mat == 1 | howdo == 1 | other == 1 | doing_defining_topic == 1, 1, 0)) %>% 
    group_by(cluster) %>% 
    summarize(any = sum(any),
              none = sum(none))

plot$cluster <- factor(plot$cluster)
levels(plot$cluster) <- c("Moderately Full", "Rational", "Full", "Reluctant", "Universally Low", "Pleasurable")
plot$cluster <- factor(plot$cluster, levels = c("Full", "Moderately Full", "Rational", "Pleasurable", "Reluctant", "Universally Low"))

to_plot <- plot %>% tidyr::gather(key, val, -cluster)
to_plot$key <- factor(to_plot$key)
to_plot$key <- forcats::fct_recode(to_plot$key,
                                   c("Any Choice" = "any",
                                     "No Choice" = "none"))

to_plot <- dplyr::arrange(to_plot, cluster)
plot_o <- to_plot %>% tidyr::spread(key, val)
lsr::cramersV(plot_o[, 2:3])
y <- chisq.test(plot_o[, 2:3])
y <- as.data.frame(y$stdres)
# clipr::write_clip(y)
row.names(y) <- plot_o$cluster

the_vec <- as.vector(as.matrix(t(y)))

to_plot$label <- ifelse(the_vec > 1.96, "+",
                        ifelse(the_vec < -1.96, "-", ""))

to_plot %>% group_by(cluster) %>% summarize(sum = sum(val))

ggplot(to_plot, aes(x = key, y = val, fill = cluster, label = label)) +
    geom_bar(stat = "identity") +
    geom_text(position = position_stack(.5)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1.05)) +
    theme(text=element_text(size = 16, family = "Times")) +
    theme(legend.position = "right") +
    # scale_fill_discrete("") + for bw
    scale_fill_brewer("", palette="OrRd") +
    ylab("Number of Responses") +
    xlab(NULL) +
    theme(panel.background = element_blank()) +
    theme(panel.grid.major.y=element_line(color="black", size=.25)) +
    theme(panel.grid.minor.y=element_line(color="black", size=.15)) +
    theme(panel.grid.major.x=element_blank()) +
    theme(axis.ticks.y = element_blank()) + 
    guides(fill = guide_legend(reverse = F)) +
    scale_y_continuous(labels = scales::comma)

ggsave("choices_final_bw.png", height = 5, width = 6)

#############################################
### 4. Choice logistic regression ###########
#############################################

factor_df$ch_defining_topic <- ifelse(factor_df$ch_defin == 1 | factor_df$ch_topic == 1, 1, 0)
factor_df$ch_doing_defining_topic <- ifelse(factor_df$ch_defin == 1 | factor_df$ch_topic == 1 | factor_df$ch_doing, 1, 0)

aov_df <- data.frame(who = factor_df$ch_who[filter], mat = factor_df$ch_mat[filter],
                     time = factor_df$ch_time[filter], howdo = factor_df$ch_howdo[filter],
                     doing = factor_df$ch_doing[filter], define = factor_df$ch_defin[filter],
                     topic = factor_df$ch_topic[filter], other = factor_df$ch_other[filter], 
                     defining_topic = factor_df$ch_defining_topic[filter], doing_defining_topic = factor_df$ch_doing_defining_topic[filter],
                     none = factor_df$ch_none[filter],
                     cluster = statistics[[4]])

df <-
    aov_df %>% 
    dplyr::select(who, mat, time, howdo, other, doing_defining_topic, none, cluster) 

df$cluster1 <- ifelse(df$cluster == 1, 1, 0)
df$cluster2 <- ifelse(df$cluster == 2, 1, 0)
df$cluster3 <- ifelse(df$cluster == 3, 1, 0)
df$cluster4 <- ifelse(df$cluster == 4, 1, 0)
df$cluster5 <- ifelse(df$cluster == 5, 1, 0)
df$cluster6 <- ifelse(df$cluster == 6, 1, 0)

model1 <- glm(cluster1 ~ who + mat + time + howdo + doing_defining_topic + other, family = binomial(link = "logit"), data = df)
arm::display(model1)
jmRtools::convert_log_odds(coef(model1))
summary(model1)

model2 <- glm(cluster2 ~ who + mat + time + howdo + doing_defining_topic + other, family = binomial(link = "logit"), data = df)
arm::display(model2)
jmRtools::convert_log_odds(coef(model2))
summary(model2)

model3 <- glm(cluster3 ~ who + mat + time + howdo + doing_defining_topic + other, family = binomial(link = "logit"), data = df)
arm::display(model3)
jmRtools::convert_log_odds(coef(model3))
summary(model3)

model4 <- glm(cluster4 ~ who + mat + time + howdo + doing_defining_topic + other, family = binomial(link = "logit"), data = df)
arm::display(model4)
jmRtools::convert_log_odds(coef(model4))
summary(model4)

model5 <- glm(cluster5 ~ who + mat + time + howdo + doing_defining_topic + other, family = binomial(link = "logit"), data = df)
arm::display(model5)
jmRtools::convert_log_odds(coef(model5))
summary(model5)

model6 <- glm(cluster6 ~ who + mat + time + howdo + doing_defining_topic + other, family = binomial(link = "logit"), data = df)
arm::display(model6)
jmRtools::convert_log_odds(coef(model6))
summary(model6)

#############################################
### 5 Choice (any vs. none just for lab) ####
#############################################

factor_df <- dplyr::select(esm_df, instructional_practice, contains("ch"))

factor_df$ip <- as.character(haven::as_factor(factor_df$instructional_practice))

factor_df$ip <- car::recode(factor_df$ip,
                            "c('1 Lecture') = 'Lecture';
                            c('2 Seatwk-Ind', '3 Review-Ind') = 'Individual Work';
                            c('4 Seatwk-Grp', '5 Review-Grp') = 'Group Work';
                            c('6 QuizPrep', '8 TestTaking', '7 QuizRev') = 'Quiz and Test';
                            c('9 Discussion') = 'Discussion';
                            c('11 PresElab', '10 Presentation') = 'Presentation';
                            c('12 Video-Watch', '13 Video-Elab') = 'Video';
                            c('15 Labwork', '14 Lab-Prep', '16 Lab-Review') = 'Laboratory';
                            c('17 NonInstTime') = 'Non-instructional';
                            c('18 OffTask') = NA")

factor_df$ip <- ifelse((factor_df$ip == "Discussion" | factor_df$ip == "Non-instructional" |
                            factor_df$ip == "Presentation" | factor_df$ip == "Video" | factor_df$ip == "Group Work"), "Other", factor_df$ip)

factor_df$ch_who[is.na(factor_df$ch_who)] <- 0
factor_df$ch_howdo[is.na(factor_df$ch_howdo)] <- 0
factor_df$ch_mat[is.na(factor_df$ch_mat)] <- 0
factor_df$ch_time[is.na(factor_df$ch_time)] <- 0
factor_df$ch_doing[is.na(factor_df$ch_doing)] <- 0
factor_df$ch_topic[is.na(factor_df$ch_topic)] <- 0
factor_df$ch_defin[is.na(factor_df$ch_defin)] <- 0
factor_df$ch_other[is.na(factor_df$ch_other)] <- 0
factor_df$ch_none[is.na(factor_df$ch_none)] <- 0

factor_df$ch_defining_topic <- ifelse(factor_df$ch_defin == 1 | factor_df$ch_topic == 1, 1, 0)
factor_df$ch_doing_defining_topic <- ifelse(factor_df$ch_defin == 1 | factor_df$ch_topic == 1 | factor_df$ch_doing, 1, 0)

filter <- attributes(statistics)$cases_to_keep

aov_df <- data.frame(who = factor_df$ch_who[filter], mat = factor_df$ch_mat[filter],
                     time = factor_df$ch_time[filter], howdo = factor_df$ch_howdo[filter],
                     doing = factor_df$ch_doing[filter], define = factor_df$ch_defin[filter],
                     topic = factor_df$ch_topic[filter], other = factor_df$ch_other[filter], 
                     framing = factor_df$ch_doing_defining_topic[filter],
                     none = factor_df$ch_none[filter],
                     cluster = statistics[[4]],
                     ip = factor_df$ip[filter])

aov_df$ip <- as.character(aov_df$ip)

aov_df$any_choice <- ifelse(aov_df$framing == 1 | aov_df$mat == 1 | aov_df$howdo == 1 | aov_df$time == 1 |
                                aov_df$who == 1 | aov_df$other == 1, 1, 0)

plot <-
    aov_df %>% 
    dplyr::select(any_choice, none, cluster, ip) %>% 
    group_by(cluster, ip) %>% 
    summarize(Any = sum(any_choice),
              None = sum(none)) %>% 
    arrange(ip, cluster)

plot$cluster <- as.factor(plot$cluster)
levels(plot$cluster) <- c("Moderately Full", "Rational", "Full", "Reluctant", "Universally Low", "Pleasurable")
plot$cluster <- factor(plot$cluster,
                       levels = rev(c("Full", "Moderately Full", "Rational", "Pleasurable", "Reluctant", "Universally Low")))

plot_ss <- 
    plot %>% 
    filter(ip == "Laboratory") %>% 
    gather(key, val, -cluster, -ip) %>% 
    select(-ip)

plot_ss$sig <- c("", "-", "+", "+", "", "+",
                 "", "-", "", "-", "-", "")

ggplot(plot_ss, aes(x = cluster, y = val, fill = key, label = sig)) +
    # facet_grid( ~ ip) + 
    geom_col(position = "stack") +
    #ggtitle("Choices for Laboratory") +
    scale_fill_brewer("", palette="Set2") +
    ylab("Number of Responses") +
    xlab(NULL) +
    theme(panel.background = element_blank()) +
    theme(panel.grid.major.y=element_line(color="black", size=.25)) +
    theme(panel.grid.minor.y=element_line(color="black", size=.15)) +
    theme(panel.grid.major.y=element_blank()) +
    #theme(axis.ticks.y = element_blank()) + 
    guides(fill = guide_legend(reverse = F)) +
    scale_y_continuous(labels = scales::comma) +
    theme(text = element_text(size = 16, family = "Times")) +
    geom_text(position = position_stack(.5)) +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave('choices_by_lab.png', width = 7, height = 5.5)

######################################################
# X. Start here - log lm  ############################
######################################################

convert_odds_less_than_one <- function(x){
    1 / x
}

plot <-
    aov_df %>% 
    dplyr::select(who, mat, time, howdo, other, framing, none, cluster, ip) %>% 
    group_by(cluster, ip) %>% 
    summarize_each(funs(sum)) %>% 
    arrange(ip, cluster)

plot

the_list <- list(yind, ylab, ylec, yqt)
arr <- abind::abind(the_list, along = 3)
tab <- as.table(arr)
names(dimnames(tab)) = c("cluster", "choice", "ip")
dimnames(tab)[[1]] = c("Moderately Full", "Rational", "Full", "Reluctant", "Universally Low", "Pleasurable")
dimnames(tab)[[2]] = c("any", "none")
dimnames(tab)[[3]] = c("Individual Work", "Laboratory", "Lecture", "Quiz and Test")

m.sat <- MASS::loglm( ~ cluster + choice + ip, data = tab)
summary(m.sat)


plot <- filter(plot, !is.na(ip) & ip != "NaN")

plot %>% spread()

plot %>% gather(key, val, -cluster, -ip)

y <- chisq.test(plot[1:6, 3:4])
yind <- plot[1:6, 3:4]
y_ind <- as.data.frame(y$stdres)
the_vec1 <- as.vector(as.matrix(t(y_ind)))

y <- chisq.test(plot[7:12, 3:4])
ylab <- plot[7:12, 3:4]
y_lab <- as.data.frame(y$stdres)
the_vec2 <- as.vector(as.matrix(t(y_lab)))

y <- chisq.test(plot[13:18, 3:4])
ylec <- plot[13:18, 3:4]
y_lec <- as.data.frame(y$stdres)
the_vec3 <- as.vector(as.matrix(t(y_lec)))

y <- chisq.test(plot[19:24, 3:4])
yother <- plot[19:24, 3:4]
y_other <- as.data.frame(y$stdres)
the_vec4 <- as.vector(as.matrix(t(y_other)))

y <- chisq.test(plot[25:30, 3:4])
yqt <- plot[25:30, 3:4]
y_qt <- as.data.frame(y$stdres)
the_vec5 <- as.vector(as.matrix(t(y_qt)))

the_vec <- c(the_vec1, the_vec2, the_vec3, the_vec4, the_vec5)
plot$cluster <- factor(plot$cluster)
levels(plot$cluster) <- c("Moderately Full", "Rational", "Full", "Reluctant", "Universally Low", "Pleasurable")

to_plot <-
    plot %>%
    select(cluster:None) %>%
    tidyr::gather(key, val, -cluster, -ip) %>%
    arrange(ip, cluster)

to_plot %>% group_by(ip) %>% summarize(sum = sum(val))
to_plot$cluster <- factor(to_plot$cluster, levels = c("Full", "Moderately Full", "Rational", "Pleasurable", "Reluctant", "Universally Low"))
to_plot

the_list <- list(yind, ylab, ylec, yqt)
arr <- abind::abind(the_list, along = 3)
tab <- as.table(arr)
names(dimnames(tab)) = c("cluster", "choice", "ip")
dimnames(tab)[[1]] = c("Moderately Full", "Rational", "Full", "Reluctant", "Universally Low", "Pleasurable")
dimnames(tab)[[2]] = c("any", "none")
dimnames(tab)[[3]] = c("Individual Work", "Laboratory", "Lecture", "Quiz and Test")

m.sat <- MASS::loglm( ~ cluster + choice + ip, data = tab)
summary(m.sat)

x <- as.data.frame(resid(m.sat))

x %>%
    spread(cluster, Freq) %>%
    arrange(ip) %>%
    select(ip, choice, `Universally Low`, `Reluctant`, `Pleasurable`, `Rational`, `Moderately Full`, `Full`) %>%
    clipr::write_clip()