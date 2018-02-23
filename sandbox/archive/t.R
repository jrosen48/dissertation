# Plots

d %>% 
    select(dm_ask:dm_com) %>% 
    summarize_all(funs(safe_sum, n())) %>% 
    select(contains("sum")) %>% 
    gather(key, val) %>% 
    ggplot(aes(x = key, y = val)) +
    geom_col()

d %>% 
    select(profile, dm_ask:dm_com) %>% 
    group_by(profile) %>% 
    summarize_all(funs(safe_sum, n())) %>% 
    select(profile, contains("sum"), n = dm_ask_n) %>% 
    gather(key, val, -n, -profile) %>% 
    mutate(prop = val / n) %>% 
    mutate(key = str_sub(key, end = 6)) %>% 
    select(-n) %>% 
    ggplot(aes(x = profile, y = prop, fill = key)) +
    geom_col(position = "dodge")

d %>% 
    select(profile, dm_ask:dm_com) %>% 
    group_by(profile) %>% 
    summarize_all(funs(safe_sum, n())) %>% 
    select(profile, contains("sum"), n = dm_ask_n) %>% 
    gather(key, val, -profile, -n) %>% 
    mutate(prop = val/n) %>% 
    select(profile, key, prop) %>% 
    mutate(key = str_sub(key, end = 6))

ggplot(x, aes(x = profile, y = prop, fill = key)) +
    geom_col(position = "dodge")

x <- d %>% 
    select(profile, dm_ask:dm_com) %>% 
    group_by(profile) %>% 
    summarize_all(funs(safe_sum, n())) %>% 
    select(profile, contains("sum"), n = dm_ask_n) %>% 
    gather(key, val, -profile, -n)

ggplot(x, aes(x = profile, y = val, fill = key)) +
    geom_col()

# Models

m3a <- glm(profile_1 ~ 1 +
               dm_ask + dm_obs + dm_gen + dm_mod + dm_com,
           #dm_obs +
           # (1 | participant_ID) +
           # (1 | beep_ID) +
           # (1 | program_ID),
           family = binomial(link = "logit"),
           data = d)

summary(m3a)
sjstats::icc(m3a)

m4a <- glm(profile_4 ~ 1 +
               dm_ask + dm_obs + dm_gen + dm_mod + dm_com,
           # (1 | participant_ID),
           #(1 | beep_ID) +
           #(1 | program_ID),
           family = binomial(link = "logit"),
           data = d)

summary(m4a)
sjstats::icc(m4a)
konfound::konfound(m4a)
