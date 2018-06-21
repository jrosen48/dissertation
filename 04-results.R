## ---- loading-packages-1-------------------------------------------------
library(tidyverse)
library(lme4)
library(corrr)
library(jmRtools)
library(tidyLPA)
library(kableExtra)
library(sjPlot)
library(broom)
library(broom.mixed)
library(konfound)
source("helpers.R")

## ---- loading-data, eval = F---------------------------------------------
## esm <- read_csv("/Volumes/SCHMIDTLAB/PSE/data/STEM-IE/STEM-IE-esm.csv")
## pre_survey_data_processed <- read_csv("/Volumes/SCHMIDTLAB/PSE/data/STEM-IE/STEM-IE-pre-survey.csv")
## post_survey_data_partially_processed <- read_csv("/Volumes/SCHMIDTLAB/PSE/data/STEM-IE/STEM-IE-post-survey.csv")
## video <- read_csv("/Volumes/SCHMIDTLAB/PSE/data/STEM-IE/STEM-IE-video.csv")
## pqa <- read_csv("/Volumes/SCHMIDTLAB/PSE/data/STEM-IE/STEM-IE-pqa.csv")
## attendance <- read_csv("/Volumes/SCHMIDTLAB/PSE/data/STEM-IE/STEM-IE-attendance.csv")
## class_data <- read_csv("/Volumes/SCHMIDTLAB/PSE/data/STEM-IE/STEM-IE-class-video.csv")
## demographics <- read_csv("/Volumes/SCHMIDTLAB/PSE/data/STEM-IE/STEM-IE-demographics.csv")
## pm <- read_csv("/Volumes/SCHMIDTLAB/PSE/Data/STEM-IE/STEM-IE-program-match.csv")

## ---- loading-rdata------------------------------------------------------
# save.image("~/desktop/sandbox-01.Rdata")
load("~/desktop/sandbox-01.Rdata")

## ---- processing-attendance-demo-esm-data--------------------------------
attendance <- rename(attendance, participant_ID = ParticipantID)
attendance <- mutate(attendance, prop_attend = DaysAttended / DaysScheduled,
                     participant_ID = as.integer(participant_ID))
attendance <- select(attendance, participant_ID, prop_attend)

demographics <- filter(demographics, participant_ID!= 7187)
demographics <- left_join(demographics, attendance)

esm$overall_engagement <- jmRtools::composite_mean_maker(esm, hard_working, concentrating, enjoy, interest)

## ---- joining-to-df------------------------------------------------------
df <- left_join(esm, pre_survey_data_processed, by = "participant_ID") # df & post-survey
df <- left_join(df, video, by = c("program_ID", "response_date", "sociedad_class", "signal_number")) # df & video
df <- left_join(df, demographics, by = c("participant_ID", "program_ID")) # df and demographics

## ---- proc-beep-actvariables, echo = F-----------------------------------
df$participant_ID <- as.factor(df$participant_ID)
df$program_ID <- as.factor(df$program_ID)
df$beep_ID <- as.factor(df$beep_ID)
df$beep_ID_new <- as.factor(df$beep_ID_new)

df$youth_activity_rc <- ifelse(df$youth_activity == "Off Task", "Not Focused", df$youth_activity)

df$youth_activity_rc <- ifelse(df$youth_activity_rc == "Student Presentation" | df$youth_activity_rc == "Problem Solving", "Creating Product", df$youth_activity_rc)

df$youth_activity_rc <- ifelse(df$youth_activity_rc == "Showing Video", "Program Staff Led", df$youth_activity_rc)

df$youth_activity_rc <- as.factor(df$youth_activity_rc)

df$youth_activity_rc <- forcats::fct_relevel(df$youth_activity_rc, "Not Focused")

df$relevance <- jmRtools::composite_mean_maker(df, use_outside, future_goals, important)

## ----proc-demographics---------------------------------------------------
df$urm <- ifelse(df$race %in% c("White", "Asian"), 0, 1)
df$race <- as.factor(df$race)
df$race <- fct_lump(df$race, n = 2)
df$race_other <- fct_relevel(df$race, "Other")
df$gender_female <- as.factor(df$gender) # female is comparison_group
df$gender_female <- ifelse(df$gender_female == "F", 1,
                           ifelse(df$gender_female == "M", 0, NA))

## ---- proc-pqa-data------------------------------------------------------
pqa <- mutate(pqa,
              active = active_part_1 + active_part_2,
              ho_thinking = ho_thinking_1 + ho_thinking_2 + ho_thinking_3,
              belonging = belonging_1 + belonging_2,
              agency = agency_1 + agency_2 + agency_3 + agency_4,
              youth_development_overall = active_part_1 + active_part_2 + ho_thinking_1 + ho_thinking_2 + ho_thinking_3 + belonging_1 + belonging_2 + agency_1 + agency_2 + agency_3 + agency_4,
              making_observations = stem_sb_8,
              data_modeling = stem_sb_2 + stem_sb_3 + stem_sb_9,
              interpreting_communicating = stem_sb_6,
              generating_data = stem_sb_4,
              asking_questions = stem_sb_1,
              stem_sb = stem_sb_1 + stem_sb_2 + stem_sb_3 + stem_sb_4 + stem_sb_5 + stem_sb_6 + stem_sb_7 + stem_sb_8 + stem_sb_9)

pqa$sociedad_class <- ifelse(pqa$eighth_math == 1, "8th Math",
                             ifelse(pqa$seventh_math == 1, "7th Math",
                                    ifelse(pqa$sixth_math == 1, "6th Math",
                                           ifelse(pqa$robotics == 1, "Robotics",
                                                  ifelse(pqa$dance == 1, "Dance", NA)))))

pqa <- rename(pqa,
              program_ID = SiteIDNumeric,
              response_date = resp_date,
              signal_number = signal)

pqa$program_ID <- as.character(pqa$program_ID)

df <- left_join(df, pqa, by = c("response_date", "program_ID", "signal_number", "sociedad_class"))

## ---- proc-vars-for-modeling---------------------------------------------
df <- df %>%
  mutate(dm_cog_eng = learning,
         dm_beh_eng = hard_working,
         dm_aff_eng = enjoy,
         dm_challenge = challenge,
         dm_competence = good_at) %>%
  rename(ssb_predict = stem_sb_1,
         ssb_model = stem_sb_2 ,
         ssb_analyze = stem_sb_3,
         ssb_measure = stem_sb_4,
         ssb_tools = stem_sb_5,
         ssb_precision = stem_sb_6,
         ssb_vocabulary = stem_sb_7,
         ssb_classification = stem_sb_8,
         ssb_symbols = stem_sb_9) %>%
  mutate(dm_ask = ssb_predict,
         dm_obs = ssb_classification,
         dm_gen = ifelse(ssb_measure == 1 | ssb_precision == 1, 1, 0),
         dm_mod = ssb_model,
         dm_com = ifelse(ssb_symbols == 1 | ssb_analyze == 1, 1, 0)) %>%
  mutate(ov_cog_eng = (important + future_goals) / 2,
         ov_beh_eng = (hard_working + concentrating) / 2,
         ov_aff_eng = (enjoy + interest) / 2) %>%
  mutate(dm_composite = dm_ask + dm_obs + dm_gen + dm_mod + dm_com,
         dm_composite_di = ifelse(dm_ask == 1 | dm_obs == 1 | dm_gen == 1 | dm_mod == 1 | dm_com == 1, 1, 0))

df$dm_overall_eng <- composite_mean_maker(df, dm_cog_eng, dm_beh_eng, dm_aff_eng)

df <- mutate(df, inquiry_based = ifelse(youth_activity_rc == "Creating Product" | youth_activity_rc == "Lab Activity", 1, 0),
             inquiry_based_three = ifelse(youth_activity_rc == "Creating Product" | youth_activity_rc == "Lab Activity", "inquiry-based",
                                          ifelse(youth_activity_rc == "Not Focused", "not-focused", "other")))

## ------------------------------------------------------------------------
d_red <- df %>%
  group_by(participant_ID) %>%
  mutate(rownum = row_number()) %>%
  mutate(overall_pre_interest = ifelse(rownum == 1, overall_pre_interest, NA)) %>%
  ungroup() %>%
  select(-participant_ID)

# For the number of codes
# d <- googlesheets::gs_title("New Overall Coding Sheet") %>%
#   googlesheets::gs_read()
#
# write_csv(d, "data/new_overall_coding_sheet.csv")

read_csv("data/new_overall_coding_sheet.csv") %>%
  filter(!is.na(program_name)) %>%
  select(Asking, Observe, Generate, Model, Communicate) %>%
  mutate_all(replace_na, 0) %>%
  mutate(row_num = row_number()) %>%
  gather(key, val, -row_num) %>%
  group_by(row_num) %>%
  summarize(sum_val = sum(val)) %>%
  count(sum_val) %>%
  mutate(n_prop = n / sum(n))

## ---- m1_6p-ll, eval = FALSE, cache = FALSE------------------------------
## extract_LL_mplus() %>% slice(1:10) %>% knitr::kable()

m1_6 <- read_rds("data/models/m1_6.rds")
m1_6b <- read_rds("data/models/m1_6.rds")

## ------------------------------------------------------------------------
df <- read_csv("data/for-seven-profiles.csv")
cc <- df %>% select(dm_cog_eng:dm_competence) %>% complete.cases()
C <- select(m1_6, C)

C_p_m <- select(m1_6, CPROB1:CPROB6) %>%
  apply(MARGIN = 1, FUN = max)

C_p <- select(m1_6, CPROB1:CPROB6)

df_ss <- df[cc, ]

df_ss <- bind_cols(df_ss, C_p)
df_ss$profile <- C
df_ss$profile_p <- C_p_m

d <- df_ss %>% select(contains("dm"), participant_ID, program_ID, beep_ID = beep_ID_new, profile, profile_p, overall_pre_interest, youth_activity_rc, inquiry_based, inquiry_based_three, classroom_versus_field_enrichment, gender_female, urm, contains("cprob"), dm_composite_di)

d <- d %>%
  mutate(
    profile_1 = ifelse(profile == 1, 1, 0),
    profile_2 = ifelse(profile == 2, 1, 0),
    profile_3 = ifelse(profile == 3, 1, 0),
    profile_4 = ifelse(profile == 4, 1, 0),
    profile_5 = ifelse(profile == 5, 1, 0),
    profile_6 = ifelse(profile == 6, 1, 0),
    profile_1_p = CPROB1,
    profile_2_p = CPROB2,
    profile_3_p = CPROB3,
    profile_4_p = CPROB4,
    profile_5_p = CPROB5,
    profile_6_p = CPROB6
  )

d$profile <- as.vector(unlist(d$profile))
d$profile_1 <- as.vector(d$profile_1)
d$profile_2 <- as.vector(d$profile_2)
d$profile_3 <- as.vector(d$profile_3)
d$profile_4 <- as.vector(d$profile_4)
d$profile_5 <- as.vector(d$profile_5)
d$profile_6 <- as.vector(d$profile_6)

d <- as.tibble(as.data.frame(d))

d_out <- d

# ## ---- rq2-0-null, cache = FALSE, eval = TRUE-----------------------------
# m1 <- lmer(profile_1_p ~ 1 +
#              (1 | participant_ID) +
#              (1 | beep_ID) +
#              (1 | program_ID),
#            data = d)
#
# m2 <- lmer(profile_2_p ~ 1 +
#              (1 | participant_ID) +
#              (1 | beep_ID) +
#              (1 | program_ID),
#            data = d)
#
# m3 <- lmer(profile_3_p ~ 1 +
#              (1 | participant_ID) +
#              (1 | beep_ID) +
#              (1 | program_ID),
#            data = d)
#
# m4 <- lmer(profile_4_p ~ 1 +
#              (1 | participant_ID) +
#              (1 | beep_ID) +
#              (1 | program_ID),
#            data = d)
#
# m5 <- lmer(profile_5_p ~ 1 +
#              (1 | participant_ID) +
#              (1 | beep_ID) +
#              (1 | program_ID),
#            data = d)
#
# m6 <- lmer(profile_6_p ~ 1 +
#              (1 | participant_ID) +
#              (1 | beep_ID) +
#              (1 | program_ID),
#            data = d)
# #
# ## ---- rq2-1-all-vars-com-keep, cache = FALSE-----------------------------
# m1a <- lmer(profile_1_p ~ 1 +
#               # gender_female +
#               # urm +
#               dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
#               (1 | participant_ID) +
#               (1 | beep_ID) +
#               (1 | program_ID),
#             data = d)
#
# m2a <- lmer(profile_2_p ~ 1 +
#               # gender_female +
#               # urm +
#               dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
#               (1 | participant_ID) +
#               (1 | beep_ID) +
#               (1 | program_ID),
#             data = d)
#
# m3a <- lmer(profile_3_p ~ 1 +
#               # gender_female +
#               # urm +
#               dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
#               (1 | participant_ID) +
#               (1 | beep_ID) +
#               (1 | program_ID),
#             data = d)
#
# m4a <- lmer(profile_4_p ~ 1 +
#               # gender_female +
#               # urm +
#               dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
#               (1 | participant_ID) +
#               (1 | beep_ID) +
#               (1 | program_ID),
#             data = d)
#
# m5a <- lmer(profile_5_p ~ 1 +
#               # gender_female +
#               # urm +
#               dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
#               (1 | participant_ID) +
#               (1 | beep_ID) +
#               (1 | program_ID),
#             data = d)
#
# m6a <- lmer(profile_6_p ~ 1 +
#               # gender_female +
#               # urm +
#               dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
#               (1 | participant_ID) +
#               (1 | beep_ID) +
#               (1 | program_ID),
#             data = d)
#
# ## ---- rq3-2-all-vars-sep-interaction-keep, eval = TRUE-------------------
# m1c <- lmer(profile_1_p ~ 1 +
#               overall_pre_interest +
#               gender_female +
#               urm +
#               (1 | participant_ID) +
#               (1 | beep_ID) +
#               (1 | program_ID),
#             data = d)
#
# m2c <- lmer(profile_2_p ~ 1 +
#               overall_pre_interest +
#               gender_female +
#               urm +
#               (1 | participant_ID) +
#               (1 | beep_ID) +
#               (1 | program_ID),
#             data = d)
#
# m3c <- lmer(profile_3_p ~ 1 +
#               overall_pre_interest +
#               gender_female +
#               urm +
#               (1 | participant_ID) +
#               (1 | beep_ID) +
#               (1 | program_ID),
#             data = d)
#
# m4c <- lmer(profile_4_p ~ 1 +
#               overall_pre_interest +
#               gender_female +
#               urm +
#               (1 | participant_ID) +
#               (1 | beep_ID) +
#               (1 | program_ID),
#             data = d)
#
# m5c <- lmer(profile_5_p ~ 1 +
#               overall_pre_interest +
#               gender_female +
#               urm +
#               (1 | participant_ID) +
#               (1 | beep_ID) +
#               (1 | program_ID),
#             data = d)
#
# m6c <- lmer(profile_6_p ~ 1 +
#               overall_pre_interest +
#               gender_female +
#               urm +
#               (1 | participant_ID) +
#               (1 | beep_ID) +
#               (1 | program_ID),
#             data = d)
#
# ## ind all but no interactions yet
# m1d <- lmer(profile_1_p ~ 1 +
#               overall_pre_interest +
#               gender_female +
#               urm +
#               dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
#               (1 | participant_ID) +
#               (1 | beep_ID) +
#               (1 | program_ID),
#             data = d)
#
# m2d <- lmer(profile_2_p ~ 1 +
#               overall_pre_interest +
#               gender_female +
#               urm +
#               dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
#               (1 | participant_ID) +
#               (1 | beep_ID) +
#               (1 | program_ID),
#             data = d)
#
# m3d <- lmer(profile_3_p ~ 1 +
#               overall_pre_interest +
#               gender_female +
#               urm +
#               dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
#               (1 | participant_ID) +
#               (1 | beep_ID) +
#               (1 | program_ID),
#             data = d)
#
# m4d <- lmer(profile_4_p ~ 1 +
#               overall_pre_interest +
#               gender_female +
#               urm +
#               dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
#               (1 | participant_ID) +
#               (1 | beep_ID) +
#               (1 | program_ID),
#             data = d)
#
# m5d <- lmer(profile_5_p ~ 1 +
#               overall_pre_interest +
#               gender_female +
#               urm +
#               dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
#               (1 | participant_ID) +
#               (1 | beep_ID) +
#               (1 | program_ID),
#             data = d)
#
# m6d <- lmer(profile_6_p ~ 1 +
#               overall_pre_interest +
#               gender_female +
#               urm +
#               dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
#               (1 | participant_ID) +
#               (1 | beep_ID) +
#               (1 | program_ID),
#             data = d)
#
# r2glmm::r2beta(m1d)
# r2glmm::r2beta(m2d)
# r2glmm::r2beta(m3d)
# r2glmm::r2beta(m4d)
# r2glmm::r2beta(m5d)
# r2glmm::r2beta(m6d)

# ## Spec interactions
#
# m1e <- lmer(profile_1_p ~ 1 +
#               overall_pre_interest +
#               gender_female +
#               urm +
#               dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
#               overall_pre_interest:dm_com +
#               gender_female:dm_com +
#               urm:dm_com +
#               (1 | participant_ID) +
#               (1 | beep_ID) +
#               (1 | program_ID),
#             data = d)
#
# # m2d <- lmer(profile_2_p ~ 1 +
# #               overall_pre_interest +
# #               gender_female +
# #               urm +
# #               dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
# #               (1 | participant_ID) +
# #               (1 | beep_ID) +
# #               (1 | program_ID),
# #             data = d)
# #
# # m3d <- lmer(profile_3_p ~ 1 +
# #               overall_pre_interest +
# #               gender_female +
# #               urm +
# #               dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
# #               (1 | participant_ID) +
# #               (1 | beep_ID) +
# #               (1 | program_ID),
# #             data = d)
# #
# # m4d <- lmer(profile_4_p ~ 1 +
# #               overall_pre_interest +
# #               gender_female +
# #               urm +
# #               dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
# #               (1 | participant_ID) +
# #               (1 | beep_ID) +
# #               (1 | program_ID),
# #             data = d)
# #
# # m5d <- lmer(profile_5_p ~ 1 +
# #               overall_pre_interest +
# #               gender_female +
# #               urm +
# #               dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
# #               (1 | participant_ID) +
# #               (1 | beep_ID) +
# #               (1 | program_ID),
# #             data = d)
#
# m6ei <- lmer(profile_6_p ~ 1 +
#                overall_pre_interest +
#                gender_female +
#                urm +
#                dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
#                overall_pre_interest:dm_gen +
#                gender_female:dm_gen +
#                urm:dm_gen +
#                (1 | participant_ID) +
#                (1 | beep_ID) +
#                (1 | program_ID),
#              data = d)
#
# m6eii <- lmer(profile_6_p ~ 1 +
#                overall_pre_interest +
#                gender_female +
#                urm +
#                dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
#                overall_pre_interest:dm_mod +
#                gender_female:dm_mod +
#                urm:dm_mod +
#                (1 | participant_ID) +
#                (1 | beep_ID) +
#                (1 | program_ID),
#              data = d)

## tidying models

# ### m1-6 - null
# l <- list(m1, m2, m3, m4, m5, m6)
# o <- map_df(l, tidy_model)
# write_rds(o, "data/m1-6.rds")
#
# ### m1a-6a - work with data
# l <- list(m1a, m2a, m3a, m4a, m5a, m6a)
# o <- map_df(l, tidy_model)
# write_rds(o, "data/m1a-6a.rds")
#
# ### m1b-6b - ind chars
# l <- list(m1b, m2b, m3b, m4b, m5b, m6b)
# o <- map_df(l, tidy_model)
# write_rds(o, "data/m1b-6b.rds")
#
### m1d-6d - all vars
# l <- list(m1d, m2d, m3d, m4d, m5d, m6d)
# o <- map_df(l, tidy_model)
# write_rds(o, "data/m1d-6d.rds")
#
# ### m1e, m6ei, m6eii - select interactions
# l <- list(m1e, m6ei, m6eii)
# o <- map_df(l, tidy_model)
# write_rds(o, "data/m1e-m6ei-m6eii.rds")

## ---- sensitivity-analysis-for-rq2c, eval = FALSE, cache = FALSE---------
## konfound::konfound(m6e, `dm_composite:gender_female`)
## konfound::konfound(m6e, `dm_composite:gender_female`)

ds3 <- data_frame(profile = c("Only behavioral",
                              "Universally low",
                              "Engaged and competent but not challenged",
                              "Only affective",
                              "All moderate",
                              "Full"),
                  asking_questions = c(NA, NA, NA, NA, NA, NA),
                  observing = c(NA, NA, NA, NA, NA, NA),
                  generating = c(NA, NA, NA, NA, NA, 0.029),
                  modeling = c(NA, NA, NA, NA, NA, 0.035),
                  communicating = c(0.025, NA, NA, NA, NA, NA),
                  composite = c(0.007, NA, NA, NA, NA, NA))

ds4 <- data_frame(profile = c("Only behavioral",
                              "Universally low",
                              "Engaged and competent but not challenged",
                              "Only affective",
                              "All moderate",
                              "Full"),
                  pre_interest = c(NA, NA, 0.033, NA, NA, NA),
                  gender_female = c(NA, NA, NA, NA, NA, NA),
                  urm = c(NA, NA, NA, NA, NA, NA),
                  pre_interest_X_composite = c(NA, NA, NA, NA, NA, NA),
                  gender_female_X_composite = c(NA, NA, NA, NA, NA, 0.012),
                  composite = c(NA, NA, NA, NA, NA, NA))

os <- read_csv("data/new-overall-coding.csv")

os %>%
  filter(Asking == 1)
