library(tidyLPA)

extract_stats <- function(x) {
    x <- x[x!=""]
    data.frame(LL = x[1], seed = x[2], m_iterations = x[3])
}

x <- estimate_profiles_mplus(df,
                             dm_cog_eng, dm_beh_eng, dm_aff_eng, dm_challenge, dm_competence, 
                             n_profiles=6, model = 2, remove_tmp_files=F, starts=c(100, 20))

extract_LL <- function(output_filename = "i.out") {
    raw_text <- readr::read_lines(output_filename)
    start <- which(stringr::str_detect(raw_text, "Final stage loglikelihood")) + 2
    start_vals <- raw_text[str_detect(raw_text, "start =")]
    start_vals <- stringr::str_trim(start_vals)
    start_vals <- stringr::str_sub(start_vals, end = -2L)
    start_vals <- strsplit(start_vals, "[^[:digit:]]")
    start_vals <- as.numeric(unlist(start_vals))
    start_vals <- unique(start_vals[!is.na(start_vals)])
    stop <- start + (start_vals[2] - 1)   
    subset_text <- raw_text[start:stop]
    trimmed_text <- stringr::str_trim(subset_text)
    fin_text <- stringr::str_split(trimmed_text, " ")
    o <- suppressWarnings(purrr::map_df(fin_text, extract_stats))
    o$seed <- suppressWarnings(as.numeric(o$seed))
    o <- o[!is.na(o$seed), ]
    dplyr::tbl_df(o)
}
