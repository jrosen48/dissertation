library(tidyverse)

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


the_dir <- getwd()

prepareMplusData(iris[, -5], paste0(the_dir, "sandbox/mplus/iris.dat"))

runModels(paste0(the_dir, "/sandbox/mplus/modelA.inp"), showOutput = T)
runModels(paste0(the_dir, "/sandbox/mplus/modelB.inp"), showOutput = T)
runModels(paste0(the_dir, "/sandbox/mplus/modelC.inp"), showOutput = T)
runModels(paste0(the_dir, "/sandbox/mplus/modelD.inp"), showOutput = T)
runModels(paste0(the_dir, "/sandbox/mplus/modelE.inp"), showOutput = T)

mA <- readModels(paste0(the_dir, "/sandbox/mplus/modelA.out"))
mB <- readModels(paste0(the_dir, "/sandbox/mplus/modelB.out"))
mC <- readModels(paste0(the_dir, "/sandbox/mplus/modelC.out"))
mD <- readModels(paste0(the_dir, "/sandbox/mplus/modelD.out"))
mE <- readModels(paste0(the_dir, "/sandbox/mplus/modelE.out"))

extract_mplus_summary(mA) # matches model A
extract_mplus_summary(mB) # matches model B
extract_mplus_summary(mC) # matches model C
extract_mplus_summary(mD) # new model!
extract_mplus_summary(mE) # matches model D

library(tidyLPA)

mA_mclust <- create_profiles_lpa(iris,
                                 Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
                                 n_profiles = 2,
                                 model = 1,
                                 to_return = "mclust")

summary(mA_mclust)

mB_mclust <- create_profiles_lpa(iris,
                                 Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
                                 n_profiles = 2,
                                 model = 2,
                                 to_return = "mclust")

summary(mB_mclust)

mC_mclust <- create_profiles_lpa(iris,
                                 Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
                                 n_profiles = 2,
                                 model = 3,
                                 to_return = "mclust")

summary(mC_mclust)

mD_mclust <- create_profiles_lpa(iris,
                                 Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
                                 n_profiles = 2,
                                 model = 4,
                                 to_return = "mclust")

summary(mA_mclust)

# Testing
d <- tbl_df(iris[, -5])
create_profiles_mplus(d, 
                      Sepal.Length, Sepal.Width, Petal.Length,
                      n_profiles = 2)
runModels(target = paste0(getwd(), "/t.inp"))
m1_mplus_out <- readModels((target = paste0(getwd(), "/t.out")))
extract_mplus_summary(m1_mplus_out)