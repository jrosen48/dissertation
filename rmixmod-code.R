### Rmixmod

library(Rmixmod)
iris

m <- mixmodCluster(iris[, -5], nbCluster = 2:3, models = mixmodGaussianModel())
summary(m)

d <- select(df, contains("dm"))
d <- d[complete.cases(d), ]
d <- mutate_all(d, scale)

my_models <- c("Gaussian_pk_L_B",
               "Gaussian_pk_L_C",
               "Gaussian_pk_Lk_Bk",
               "Gaussian_pk_Lk_Ck"
)

list.models <- mixmodGaussianModel(listModels = my_models)

m <- mixmodCluster(d, nbCluster = 3:6, 
                   models = list.models)

extract_criterion_value <- function(x) {
    criterion <- m["results"][x][[1]]@criterion
    criterion_value <- m["results"][x][[1]]@criterionValue
    model <- m["results"][x][[1]]@model
    n_profiles <- m["results"][x][[1]]@nbCluster
    data.frame(model, n_profiles, criterion, criterion_value)
}

extract_criterion_value(1)

1:length(m["results"]) %>% 
    purrr::map_df(extract_criterion_value) %>% 
    arrange(desc(criterion_value)) %>% 
    ggplot(aes(x = n_profiles, y = criterion_value, color = model, group = model)) +
    geom_line() +
    geom_point()
```