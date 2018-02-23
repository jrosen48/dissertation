#m1
m3 <- estimate_profiles_mplus(df,
                              dm_cog_eng, dm_beh_eng, dm_aff_eng, dm_challenge, dm_competence, model = 1, n_profiles=3, remove_tmp_files=F, starts = c(500, 50), n_processors=8)

m4 <- estimate_profiles_mplus(df,
                              dm_cog_eng, dm_beh_eng, dm_aff_eng, dm_challenge, dm_competence, model = 1, n_profiles=4, remove_tmp_files=F, starts = c(500, 50), n_processors=8)
m5 <- estimate_profiles_mplus(df,
                              dm_cog_eng, dm_beh_eng, dm_aff_eng, dm_challenge, dm_competence, model = 1, n_profiles=5, remove_tmp_files=F, starts = c(500, 50), n_processors=8)
m6 <- estimate_profiles_mplus(df,
                              dm_cog_eng, dm_beh_eng, dm_aff_eng, dm_challenge, dm_competence, model = 1, n_profiles=6, remove_tmp_files=F, starts = c(500, 50), n_processors=8)
m7 <- estimate_profiles_mplus(df,
                              dm_cog_eng, dm_beh_eng, dm_aff_eng, dm_challenge, dm_competence, model = 1, n_profiles=7, remove_tmp_files=F, starts = c(500, 50), n_processors=8)
#m2
m3i <- estimate_profiles_mplus(df,
                               dm_cog_eng, dm_beh_eng, dm_aff_eng, dm_challenge, dm_competence, model = 2, n_profiles=3, remove_tmp_files=F, starts = c(500, 50), n_processors=8)

m4i <- estimate_profiles_mplus(df,
                               dm_cog_eng, dm_beh_eng, dm_aff_eng, dm_challenge, dm_competence, model = 2, n_profiles=4, remove_tmp_files=F, starts = c(500, 50), n_processors=8)
m5i <- estimate_profiles_mplus(df,
                               dm_cog_eng, dm_beh_eng, dm_aff_eng, dm_challenge, dm_competence, model = 2, n_profiles=5, remove_tmp_files=F, starts = c(500, 50), n_processors=8)

m6i <- estimate_profiles_mplus(df,
                               dm_cog_eng, dm_beh_eng, dm_aff_eng, dm_challenge, dm_competence, model = 2, n_profiles=6, 
                               remove_tmp_files=F, starts = c(500, 100), n_processors=8)
m7i <- estimate_profiles_mplus(df,
                               dm_cog_eng, dm_beh_eng, dm_aff_eng, dm_challenge, dm_competence, model = 2, n_profiles=7, 
                               remove_tmp_files=F, starts = c(500, 100), n_processors=8)

library(MplusAutomation)

x <- readModels()
extract_LL_mplus()
m5i %>% plot_profiles_mplus()

microbenchmark::microbenchmark(m7i <- estimate_profiles_mplus(df,
                                                              dm_cog_eng, dm_beh_eng, dm_aff_eng, dm_challenge, dm_competence, model = 2, n_profiles=7, 
                                                              remove_tmp_files=F, starts = c(500, 100), n_processors=8),
                               m7ii <- estimate_profiles_mplus(df,
                                                              dm_cog_eng, dm_beh_eng, dm_aff_eng, dm_challenge, dm_competence, model = 2, n_profiles=7, 
                                                              remove_tmp_files=F, starts = c(500, 100), n_processors=1),
                               times = 3)
