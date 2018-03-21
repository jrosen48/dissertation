# helpers.R

get_kr_df <- function(model_object, var_name) {
  L <- diag(rep(1, length(lme4::fixef(model_object))))
  L <- as.data.frame(L)
  out <- purrr::map_dbl(L, pbkrtest::get_Lb_ddf, object = model_object)
  names(out) <- names(lme4::fixef(model_object))
  out
}

wald_p_sig <- function(x) {
  (1 - pnorm(x))
}

t_dist_p <- function(x, df) {
  (1 - pt(x, df))
}

tidy_model <- function(model) {

  fixef_names <- c("(Intercept)", "dm_ask", "dm_ask:overall_pre_interest", "dm_com",
                   "dm_gen", "dm_mod", "dm_obs", "overall_pre_interest", "overall_pre_interest:dm_com",
                   "overall_pre_interest:dm_gen", "overall_pre_interest:dm_mod",
                   "overall_pre_interest:dm_obs", "gender_female", "urm", "dm_composite")

  d <- as.data.frame(matrix(rep(NA, 15), ncol = 15))
  names(d) <- fixef_names

  kr_df <- get_kr_df(model)
  kr_df <- kr_df %>% as.data.frame() %>% rownames_to_column()
  names(kr_df) <- c("term", "df")

  # fixefs
  fixef_vals <- tidy(model) %>%
    filter(effect == "fixed") %>%
    mutate(est = round(estimate, 3),
           se = round(std.error, 3)) %>%
    left_join(kr_df) %>%
    mutate(p_val = t_dist_p(statistic, df),
           est_se = ifelse(p_val < .001,
                           str_c(est, " (", se, ")", " (p < .001)"),
                           str_c(est, " (", se, ")", " (p = ", round(p_val, 3), ")"))) %>%
    select(term, est_se) %>%
    spread(term, est_se)

  dd <- bind_rows(d, fixef_vals)[-1, ]

  # ICCs
  icc_vals <- sjstats::icc(model) %>%
    as.numeric() %>%
    round(3)

  # To return
  names(icc_vals) <- str_c(names(sjstats::icc(model)), "_ICC")
  ddd <- cbind(dd, as.data.frame(t(icc_vals)))
  ddd
}

paste_stats <- function(stat, paren_statement) {
  if (paren_statement < .001) {
    stringr::str_c(stat, " (< .001)")
  }
  stringr::str_c(stat, " (", paren_statement, ")")
}
