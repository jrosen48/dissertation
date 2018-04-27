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


## ---- rq2-1-corr-p, eval = F---------------------------------------------
## p_vals <- d %>%
##   select(dm_ask:dm_com, dm_composite,
##          profile_1_p:profile_6_p,
##          overall_pre_interest) %>%
##   psych::corr.test() %>%
##   pluck(4) %>%
##   round(3)

# dfss <- d_red %>% distinct(beep_ID, sociedad_class, .keep_all=T)
# dfss %>%
#   select(dm_composite) %>%
#   summarize(m_dm_composite = mean(dm_composite, na.rm = T),
#             sd_dm_composite = sd(dm_composite, na.rm = T))
#
# # A tibble: 1 x 2
# m_dm_composite sd_dm_composite
# <dbl>           <dbl>
#   1           1.86            1.61

## ---- eval = FALSE-------------------------------------------------------
## dfs <- df %>%
##   distinct(beep_ID, sociedad_class, .keep_all=T)
##
## dfs %>%
##   count(dm_ask) %>%
##   filter(!is.na(dm_ask)) %>%
##   mutate(prop = n / sum(n))
##
## dfs %>%
##   count(dm_obs) %>%
##   filter(!is.na(dm_obs)) %>%
##   mutate(prop = n / sum(n))
##
## dfs %>%
##   count(dm_gen) %>%
##   filter(!is.na(dm_gen)) %>%
##   mutate(prop = n / sum(n))
##
## dfs %>%
##   count(dm_mod) %>%
##   filter(!is.na(dm_mod)) %>%
##   mutate(prop = n / sum(n))
##
## dfs %>%
##   count(dm_com) %>%
##   filter(!is.na(dm_com)) %>%
##   mutate(prop = n / sum(n))
##
## dfs %>%
##   count(dm_composite) %>%
##   filter(!is.na(dm_composite)) %>%
##   mutate(prop = n / sum(n))

## ---- eval = F-----------------------------------------------------------
## library(googlesheets)
## library(dplyr)
##
## d <- gs_title("New Overall Coding Sheet")
## d <- gs_read(d, ws = 1)
##
## d %>%
##   select(program_name, Asking, Observe, Generate, Model, Communicate) %>%
##   group_by(program_name) %>%
##   summarize_all(funs(sum), na.rm = T)
##
## d %>%
##   dplyr::select(Asking:com_jr) %>%
##   summarize_all(sum, na.rm = T) %>%
##   gather(key, val)
##
## # ask
## 36/90 # .4
## # observe
## 49/57 # .85
## # gen
## 48/102 # .47
## # mod
## 49/68 # .72
## # com
## 49/103 # .47
##
## # d %>%
## #   filter(ask_jr == 1)
## #
## # d %>%
## #   dplyr::select(Asking, Observe, Generate, Model, Communicate) %>%
## #   summarize_all(sum, na.rm = T)
## #
## # d %>%
## #   group_by(program_name) %>%
## #   dplyr::select(Asking, Observe, Generate, Model, Communicate) %>%
## #   summarize_all(sum, na.rm = T)
##
## # d %>%
## #   filter(!is.na(program_ID))
##
## d %>%
##   dplyr::select(dplyr::contains("jr")) %>%
##   dplyr::summarize(sum, na.rm = T)
##
## d %>%
##   dplyr::select(dplyr::contains("jr")) %>%
##   dplyr::summarize_all(sum, na.rm = T) / 239
##
## # d %>%
## #   group_by(program_name) %>%
## #   dplyr::select(dplyr::contains("jr")) %>%
## #   dplyr::summarize_all(sum, na.rm = T)
## # g1 <- gs_title("USE THIS! New Coding Frame - KMS")
## # g2 <- gs_title("USE THIS! New Coding Frame - HM")
## # g3 <- gs_title("USE THIS! New Coding Frame - KS")
## #
## # d1 <- gs_read(g1, ws = 1)
## # d2 <- gs_read(g2, ws = 1)
## # d3 <- gs_read(g3, ws = 1)
##
## # d1 <- readr::read_csv("KMS - Codes.csv")
## # d2 <- readr::read_csv("HM - Codes.csv")
## # d3 <- readr::read_csv("KS - Codes.csv")
## #
## # d1 <- rename(d1, KMS_codes = `Qualitative Coding`)
## # d2 <- select(d2, program_name, response_date, signal_number, HM_codes = `Qualitative Coding`)
## # d3 <- select(d3, program_name, response_date, signal_number, KS_codes = `Qualitative Coding`)
## #
## # dc <- d1 %>%
## #   left_join(d2, by = c("program_name", "response_date", "signal_number")) %>%
## #   left_join(d3, by = c("program_name", "response_date", "signal_number"))
## #
## # # d1 <- dplyr::select(d1, program_name, response_date, signal_number, KMS_qual = `Qualitative Coding`)
## # # d2 <- dplyr::select(d2, program_name, response_date, signal_number, HM_qual = `Qualitative Coding`)
## # # d3 <- rename(d3, KLS_qual = `Qualitative Coding`)
## # #
## # # d3 <- d3 %>%
## # #   left_join(d2) %>%
## # #   left_join(d1)
## # #
## # # d_proc <- d3 %>%
## # #   select(everything(), contains("qual"), -`Josh notes`, -Initials)
## # #
## # # # d1i <- dplyr::pull(d1, `Qualitative Coding`)[!d1_na]
## # # # d2i <- dplyr::pull(d2, `Qualitative Coding`)[!d2_na]
## # # # d3i <- dplyr::pull(d3, `Qualitative Coding`)[!d3_na]
## #
## # readr::write_csv(select(dc, -Initials, -`Josh notes`), "qual-coding.csv")
## dc <- readr::read_csv("qual-coding.csv")
##
## library(poLCA)
##
##

## ---- spec-solutions-m1_6, cache = FALSE, eval = FALSE, fig.width = 6, out.width = "100%"----
## m1_6 <- estimate_profiles_mplus(df,
##                                 dm_cog_eng, dm_beh_eng, dm_aff_eng, dm_challenge, dm_competence,
##                                 starts = c(600, 120),
##                                 model = 1,
##                                 n_profiles = 6,
##                                 include_BLRT=TRUE,
##                                 n_processors = 6, remove_tmp_files = FALSE)
## write_rds(m1_6, "data/models/m1_6.rds")

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

## ---- rq2-0-null, cache = FALSE, eval = TRUE-----------------------------
m1 <- lmer(profile_1_p ~ 1 +
             (1 | participant_ID) +
             (1 | beep_ID) +
             (1 | program_ID),
           data = d)

m2 <- lmer(profile_2_p ~ 1 +
             (1 | participant_ID) +
             (1 | beep_ID) +
             (1 | program_ID),
           data = d)

m3 <- lmer(profile_3_p ~ 1 +
             (1 | participant_ID) +
             (1 | beep_ID) +
             (1 | program_ID),
           data = d)

m4 <- lmer(profile_4_p ~ 1 +
             (1 | participant_ID) +
             (1 | beep_ID) +
             (1 | program_ID),
           data = d)

m5 <- lmer(profile_5_p ~ 1 +
             (1 | participant_ID) +
             (1 | beep_ID) +
             (1 | program_ID),
           data = d)

m6 <- lmer(profile_6_p ~ 1 +
             (1 | participant_ID) +
             (1 | beep_ID) +
             (1 | program_ID),
           data = d)

## ---- rq2-1-all-vars-com-keep, cache = FALSE-----------------------------
m1a <- lmer(profile_1_p ~ 1 +
              # gender_female +
              # urm +
              dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

m2a <- lmer(profile_2_p ~ 1 +
              # gender_female +
              # urm +
              dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

m3a <- lmer(profile_3_p ~ 1 +
              # gender_female +
              # urm +
              dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

m4a <- lmer(profile_4_p ~ 1 +
              # gender_female +
              # urm +
              dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

m5a <- lmer(profile_5_p ~ 1 +
              # gender_female +
              # urm +
              dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

m6a <- lmer(profile_6_p ~ 1 +
              # gender_female +
              # urm +
              dm_ask + dm_obs + dm_gen + dm_mod + dm_com +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

## ---- rq2-2-composite-keep, cache = FALSE--------------------------------
m1b <- lmer(profile_1_p ~ 1 +
              dm_composite +
              # gender_female +
              # urm +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

m2b <- lmer(profile_2_p ~ 1 +
              dm_composite +
              # gender_female +
              # urm +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

m3b <- lmer(profile_3_p ~ 1 +
              dm_composite +
              # gender_female +
              # urm +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

m4b <- lmer(profile_4_p ~ 1 +
              dm_composite +
              # gender_female +
              # urm +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

m5b <- lmer(profile_5_p ~ 1 +
              dm_composite +
              # gender_female +
              # urm +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

m6b <- lmer(profile_6_p ~ 1 +
              dm_composite +
              # gender_female +
              # urm +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

## ---- rq2-2-composite-keep-2, cache = FALSE------------------------------
m1bi <- lmer(profile_1_p ~ 1 +
               dm_composite_di +
               (1 | participant_ID) +
               (1 | beep_ID) +
               (1 | program_ID),
             data = d)

m2bi <- lmer(profile_2_p ~ 1 +
               dm_composite_di +
               # gender_female +
               # urm +
               (1 | participant_ID) +
               (1 | beep_ID) +
               (1 | program_ID),
             data = d)

m3bi <- lmer(profile_3_p ~ 1 +
               dm_composite_di +
               # gender_female +
               # urm +
               (1 | participant_ID) +
               (1 | beep_ID) +
               (1 | program_ID),
             data = d)

m4bi <- lmer(profile_4_p ~ 1 +
               dm_composite_di +
               # gender_female +
               # urm +
               (1 | participant_ID) +
               (1 | beep_ID) +
               (1 | program_ID),
             data = d)

m5bi <- lmer(profile_5_p ~ 1 +
               dm_composite_di +
               # gender_female +
               # urm +
               (1 | participant_ID) +
               (1 | beep_ID) +
               (1 | program_ID),
             data = d)

m6bi <- lmer(profile_6_p ~ 1 +
               dm_composite_di +
               # gender_female +
               # urm +
               (1 | participant_ID) +
               (1 | beep_ID) +
               (1 | program_ID),
             data = d)

## -----r2-1, eval = FALSE-------------------------------------------------
## MuMIn::r.squaredGLMM(m1) # .108
## MuMIn::r.squaredGLMM(m2) # .295
## MuMIn::r.squaredGLMM(m3) # .324
## MuMIn::r.squaredGLMM(m4) # .109
## MuMIn::r.squaredGLMM(m5) # .268
## MuMIn::r.squaredGLMM(m6) # .482
##
## MuMIn::r.squaredGLMM(m1a) # .115
## MuMIn::r.squaredGLMM(m2a) # .300
## MuMIn::r.squaredGLMM(m3a) # .321
## MuMIn::r.squaredGLMM(m4a) # .101
## MuMIn::r.squaredGLMM(m5a) # .270
## MuMIn::r.squaredGLMM(m6a) # .50

## ---- rq2-1-tab, eval = FALSE--------------------------------------------
## l <- list(m1a, m2a, m3a, m4a, m5a, m6a)
## o <- map_df(l, tidy_model)
## write_rds(o, "data/rq2-1-tab.rds")

## ---- sensitivity-analysis-for-rq1, eval = FALSE, cache = FALSE----------
## konfound::konfound(m6a, dm_gen)
## konfound::konfound(m6a, dm_mod)
#konfound::konfound(m1a, dm_com)
## konfound::konfound(m6a, gender_female) # need to add

## ---- sens-2, eval = FALSE, cache = FALSE--------------------------------
## konfound(m1b, dm_composite)

## -----r2-2, eval = FALSE-------------------------------------------------
## MuMIn::r.squaredGLMM(m1) # .108
## MuMIn::r.squaredGLMM(m2) # .295
## MuMIn::r.squaredGLMM(m3) # .324
## MuMIn::r.squaredGLMM(m4) # .109
## MuMIn::r.squaredGLMM(m5) # .268
## MuMIn::r.squaredGLMM(m6) # .482
##
## MuMIn::r.squaredGLMM(m1b) # .113
## MuMIn::r.squaredGLMM(m2b) # .298
## MuMIn::r.squaredGLMM(m3b) # .320
## MuMIn::r.squaredGLMM(m4b) # .100
## MuMIn::r.squaredGLMM(m5b) # .269
## MuMIn::r.squaredGLMM(m6b) # .502

## ---- just-composite-mod, eval = FALSE-----------------------------------
## l <- list(m1b, m2b, m3b, m4b, m5b, m6b)
## o <- map_df(l, tidy_model)
## write_rds(o, "data/comp-l.rds")

## ---- just-composite-mod-2, eval = FALSE---------------------------------
## l <- list(m1bi, m2bi, m3bi, m4bi, m5bi, m6bi)
## o <- map_df(l, tidy_model)
## write_rds(o, "data/comp-l-2.rds")

## ---- just-composite-red-2, eval = FALSE---------------------------------
## o <- read_rds("data/comp-l-2.rds")
## o <- mutate(o, model = c(
##   str_c("profile_", 1:6)))
##
## o %>%
##   select(model,
##          intercept = `(Intercept)`,
##          dm_composite_di,
##          beep_ID_ICC = beep_ID_ICC,
##          participant_ID_ICC,
##          program_ID_ICC) %>%
##   mutate(model = c("Only behavioral",
##                    "Universally low",
##                    "Engaged and competent but not challenged",
##                    "Only affective",
##                    "All moderate",
##                    "Full")) %>%
##   knitr::kable(format = "latex", booktabs = TRUE, caption = "Results of mixed effects models for the composite", linesep = "") %>%
##   kableExtra::kable_styling(latex_options = "scale_down") %>%
##   kableExtra::landscape()

## ---- rq3-2-all-vars-sep-interaction-keep, eval = TRUE-------------------
m1c <- lmer(profile_1_p ~ 1 +
              overall_pre_interest +
              gender_female +
              urm +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

m2c <- lmer(profile_2_p ~ 1 +
              overall_pre_interest +
              gender_female +
              urm +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

m3c <- lmer(profile_3_p ~ 1 +
              overall_pre_interest +
              gender_female +
              urm +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

m4c <- lmer(profile_4_p ~ 1 +
              overall_pre_interest +
              gender_female +
              urm +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

m5c <- lmer(profile_5_p ~ 1 +
              overall_pre_interest +
              gender_female +
              urm +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

m6c <- lmer(profile_6_p ~ 1 +
              overall_pre_interest +
              gender_female +
              urm +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

## ---- rq3-2-all-vars-interaction-inq-keep, eval = TRUE-------------------
m1e <- lmer(profile_1_p ~ 1 +
              overall_pre_interest*dm_composite +
              gender_female*dm_composite +
              urm*dm_composite +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

m2e <- lmer(profile_2_p ~ 1 +
              overall_pre_interest*dm_composite +
              gender_female*dm_composite +
              urm*dm_composite +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

m3e <- lmer(profile_3_p ~ 1 +
              overall_pre_interest*dm_composite +
              gender_female*dm_composite +
              urm*dm_composite +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

m4e <- lmer(profile_4_p ~ 1 +
              overall_pre_interest*dm_composite +
              gender_female*dm_composite +
              urm*dm_composite +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

m5e <- lmer(profile_5_p ~ 1 +
              overall_pre_interest*dm_composite +
              gender_female*dm_composite +
              urm*dm_composite +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

m6e <- lmer(profile_6_p ~ 1 +
              overall_pre_interest*dm_composite +
              gender_female*dm_composite +
              urm*dm_composite +
              (1 | participant_ID) +
              (1 | beep_ID) +
              (1 | program_ID),
            data = d)

## -----r2-3, eval = FALSE-------------------------------------------------
## MuMIn::r.squaredGLMM(m1) # .108
## MuMIn::r.squaredGLMM(m2) # .295
## MuMIn::r.squaredGLMM(m3) # .324
## MuMIn::r.squaredGLMM(m4) # .109
## MuMIn::r.squaredGLMM(m5) # .268
## MuMIn::r.squaredGLMM(m6) # .482
##
## MuMIn::r.squaredGLMM(m1c) # .112
## MuMIn::r.squaredGLMM(m2c) # .302
## MuMIn::r.squaredGLMM(m3c) # .328
## MuMIn::r.squaredGLMM(m4c) # .112
## MuMIn::r.squaredGLMM(m5c) # .275
## MuMIn::r.squaredGLMM(m6c) # .487

## ---- md-block-for-rq4, eval = FALSE-------------------------------------
## l <- list(m1c, m2c, m3c, m4c, m5c, m6c)
## o <- map_df(l, tidy_model)
## write_rds(o, "data/md-block-for-rq4")

## ---- sensitivity-analysis-for-rq2a, eval = FALSE, cache = FALSE---------
# konfound::konfound(m3c, overall_pre_interest)
## konfound::konfound(m2c, gender_female)

## -----r2-2ii, eval = FALSE-----------------------------------------------
## MuMIn::r.squaredGLMM(m1) # .108
## MuMIn::r.squaredGLMM(m2) # .295
## MuMIn::r.squaredGLMM(m3) # .324
## MuMIn::r.squaredGLMM(m4) # .109
## MuMIn::r.squaredGLMM(m5) # .268
## MuMIn::r.squaredGLMM(m6) # .482
##
## MuMIn::r.squaredGLMM(m1e) # .118
## MuMIn::r.squaredGLMM(m2e) # .307
## MuMIn::r.squaredGLMM(m3e) # .327
## MuMIn::r.squaredGLMM(m4e) # .102
## MuMIn::r.squaredGLMM(m5e) # .276
## MuMIn::r.squaredGLMM(m6e) # .510

## ---- pre-int-interactions, eval = FALSE---------------------------------
## l <- list(m1e, m2e, m3e, m4e, m5e, m6e)
## o <- map_df(l, tidy_model)
## write_rds(o, "data/pre-int-interaction.rds")

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
