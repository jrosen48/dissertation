library(tidyLPA)

f <- function(x) {
    x <- x[x!=""]
    data.frame(LL = x[1], seed = x[2], m_iterations = x[3])
}


x <- create_profiles_mplus(df,
                           dm_cog_eng, dm_beh_eng, dm_aff_eng, dm_challenge, dm_competence, 
                           n_profiles=6, model = 2, optseed = 68985)

# 18338.421  68985
plot_profiles_mplus(x)

# -17098.434    344422           296
# -17216.914    226322           478

starts = c(20, 4)
xx <- readr::read_lines("i.out")
start <- which(stringr::str_detect(xx, "Final stage loglikelihood")) + 2
stop <- start + (starts[2] - 1)   
y <- xx[start:stop]
y <- stringr::str_trim(y)
z <- stringr::str_split(y, " ")
o <- purrr::map_df(z, f)
o
