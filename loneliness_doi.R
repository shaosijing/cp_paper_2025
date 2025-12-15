library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(brms)
library(lme4)
library(Hmisc)
library(patchwork)
library(viridis)
library(openxlsx)

## ------------------------------------------------------------------
## Load data
## ------------------------------------------------------------------

lonely_EMA <- read.csv("ema_wide_clean_data_W1_2025-04-21.csv")
baseline   <- read.csv("baseline_wide_clean_data_W1_2025-04-21.csv")

remove_SIDs <- c(88310, 92284)
lonely_EMA <- lonely_EMA %>% filter(!SID %in% remove_SIDs)

## ------------------------------------------------------------------
## Compliance filtering
## ------------------------------------------------------------------

df <- lonely_EMA

df <- df %>%
  group_by(SID) %>%
  filter(n() >= 50) %>%
  ungroup()

df_day_count <- df %>%
  group_by(SID, Day) %>%
  summarise(assessments_per_day = n(), .groups = "drop")

sids_to_remove <- df_day_count %>%
  filter(assessments_per_day < 3) %>%
  count(SID) %>%
  filter(n >= 10) %>%
  pull(SID)

df <- df %>% filter(!SID %in% sids_to_remove)

df_compliance <- df %>%
  count(SID, name = "attended_sessions") %>%
  mutate(compliance = attended_sessions / 100)

df <- df %>% filter(!SID %in% df_compliance$SID[df_compliance$compliance < 0.5])

## ------------------------------------------------------------------
## Session completion
## ------------------------------------------------------------------

df <- df %>%
  mutate(
    Date_interruptions = ymd_hms(Date_interruptions),
    sh.iso_when = as.numeric(sh.iso_when)
  ) %>%
  filter(!is.na(session))

df <- df %>%
  group_by(SID) %>%
  complete(session = full_seq(session, 1)) %>%
  arrange(SID, session) %>%
  ungroup()

## ------------------------------------------------------------------
## Social interaction construction
## ------------------------------------------------------------------

df_SI <- df %>%
  group_by(SID) %>%
  arrange(Date_interruptions) %>%
  mutate(
    sh.iso_when_lead = lead(sh.iso_when),
    time_gap_hours   = as.numeric(difftime(lead(Date_interruptions),
                                           Date_interruptions,
                                           units = "hours")),
    time_gap_cat = case_when(
      time_gap_hours <= 1 ~ "0-1",
      time_gap_hours <= 3 ~ "1-3",
      time_gap_hours <= 6 ~ "3-6",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() %>%
  mutate(
    social_interaction_any = case_when(
      is.na(sh.iso_when_lead) ~ NA_real_,
      time_gap_cat == "0-1" & sh.iso_when_lead == 5 ~ 1,
      time_gap_cat == "1-3" & sh.iso_when_lead %in% c(4, 5) ~ 1,
      time_gap_cat == "3-6" & sh.iso_when_lead %in% c(3, 4, 5) ~ 1,
      TRUE ~ 0
    )
  )

df_SI <- df_SI %>%
  group_by(SID, Day) %>%
  arrange(Date_interruptions) %>%
  mutate(last_assessment_day = row_number() == max(row_number())) %>%
  ungroup()

df_SI <- df_SI %>%
  group_by(SID) %>%
  arrange(Date_interruptions) %>%
  mutate(
    sh.iso_when_next_day =
      if_else(last_assessment_day, lead(sh.iso_when), NA_real_)
  ) %>%
  ungroup()

df_SI <- df_SI %>%
  mutate(
    social_interaction_across = case_when(
      sh.iso_when_next_day %in% c(4, 5) ~ 1,
      sh.iso_when_next_day %in% 0:3 ~ rbinom(n(), 1, runif(n(), 0.5, 1)),
      TRUE ~ NA_real_
    ),
    SI = coalesce(social_interaction_any, social_interaction_across)
  )

df_SI <- df_SI %>%
  select(SID, Date_interruptions, sh.iso_when, Day, session, SI)

EMA_dat <- df_SI %>%
  left_join(df, by = c("SID", "Date_interruptions", "Day", "session", "sh.iso_when"))

EMA_dat <- EMA_dat %>%
  mutate(
    SI_human = ifelse(SI == 1 & iso.type_person == 1, 1, 0)
  )

## ------------------------------------------------------------------
## Loneliness scoring
## ------------------------------------------------------------------

EMA_dat <- EMA_dat %>%
  arrange(SID, session) %>%
  mutate(
    lonely_companion_rev = 5 - lonely_companion,
    lonely_isolated_rev  = 5 - lonely_isolated,
    lonely_leftout_rev   = 5 - lonely_leftout,
    lonely = lonely_companion_rev +
      lonely_isolated_rev +
      lonely_leftout_rev
  )

## ------------------------------------------------------------------
## Baseline processing
## ------------------------------------------------------------------

baseline <- baseline %>%
  mutate(
    across(
      starts_with("sh.loneliness_") &
        ends_with(c("18","38","39","42","43","48","49","52","53")),
      ~ 5 - .
    ),
    lon_TL = rowMeans(select(., starts_with("sh.loneliness_")), na.rm = TRUE)
  ) %>%
  filter(!is.na(lon_TL))

## ------------------------------------------------------------------
## Mplus dataset
## ------------------------------------------------------------------

EMA_dat <- EMA_dat %>%
  mutate(
    lonely = replace_na(lonely, -999),
    ss_rejected = replace_na(ss_rejected, -999)
  )

ema_mplus <- EMA_dat %>%
  select(SID, session, lonely, ss_rejected) %>%
  filter(!SID %in% c(80552, 71650, 58286, 11765, 33775, 16963, 17890)) %>%
  left_join(
    baseline %>% select(SID, demo_gender, demo_education, demo_age, lon_TL),
    by = "SID"
  )

ema_mplus <- ema_mplus %>%
  filter(
    !is.na(demo_gender),
    !is.na(demo_education),
    demo_gender != -999,
    demo_education != -999
  )

ema_mplus <- ema_mplus %>%
  mutate(
    lon_bin = ifelse(lonely == -999, NA, ifelse(lonely == 0, 0, 1)),  
    ss_bin = ifelse(ss_rejected == -999, NA, ifelse(ss_rejected == 0, 0, 1)),  
    lon_c = ifelse(lonely == -999 | lonely == 0, NA, lonely),  
    ss_c = ifelse(ss_rejected == -999 | ss_rejected == 0, NA, ss_rejected)  
  )

write.table(
  ema_mplus,
  "ema_mplus_clean.dat",
  sep = " ",
  row.names = FALSE,
  col.names = FALSE,
  na = "-999"
)

## ------------------------------------------------------------------
## Hypothesis 1: ICC
## ------------------------------------------------------------------

icc_model <- lmer(lonely ~ (1 | SID), data = ema_mplus, REML = TRUE)

var_between <- as.numeric(VarCorr(icc_model)$SID[1])
var_within  <- attr(VarCorr(icc_model), "sc")^2
ICC_lonely  <- var_between / (var_between + var_within)

ICC_lonely


## ------------------------------------------------------------------
## Hypothesis 1(c): Trait loneliness descriptive stats + distribution
## ------------------------------------------------------------------

EMA_dat <- EMA_dat %>%
  left_join(
    baseline %>% select(SID, demo_gender, demo_education, demo_age, lon_TL),
    by = "SID"
  )

df_lon <- EMA_dat %>%
  group_by(SID) %>%
  summarise(lon_TL = first(lon_TL), .groups = "drop")

df_lon %>%
  summarise(
    M   = mean(lon_TL, na.rm = TRUE),
    SD  = sd(lon_TL, na.rm = TRUE),
    Min = min(lon_TL, na.rm = TRUE),
    Max = max(lon_TL, na.rm = TRUE)
  )

ggplot(df_lon, aes(x = lon_TL)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "gray70", color = "black") +
  labs(x = "Trait Loneliness", y = "Density") +
  theme_minimal(base_size = 14)


## ------------------------------------------------------------------
## Hypothesis 1(c): SD/MSSD associations (Bayesian)
## ------------------------------------------------------------------

EMA_dat <- EMA_dat %>%
  mutate(across(c(lonely, ss_rejected), ~ na_if(., -999)))

ema_summary <- EMA_dat %>%
  group_by(SID, lon_TL) %>%
  summarise(
    SD_lonely        = sd(lonely, na.rm = TRUE),
    MSSD_lonely      = mean(diff(lonely, lag = 1)^2, na.rm = TRUE),
    SD_ss_rejected   = sd(ss_rejected, na.rm = TRUE),
    MSSD_ss_rejected = mean(diff(ss_rejected, lag = 1)^2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  drop_na()

sd_model_bayes <- brm(
  SD_ss_rejected ~ SD_lonely + lon_TL + (1 + SD_lonely | SID),
  data = ema_summary,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b"),
    prior(normal(0, 10), class = "Intercept"),
    prior(cauchy(0, 2), class = "sd")
  ),
  iter = 5000, warmup = 3000, chains = 4, cores = 4,
  control = list(adapt_delta = 0.95)
)

summary(sd_model_bayes)
saveRDS(sd_model_bayes, "Ho1_c_results_sd.rds")

mssd_model_bayes <- brm(
  MSSD_ss_rejected ~ MSSD_lonely + lon_TL + (1 + MSSD_lonely | SID),
  data = ema_summary,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b"),
    prior(normal(0, 10), class = "Intercept"),
    prior(cauchy(0, 2), class = "sd")
  ),
  iter = 5000, warmup = 3000, chains = 4, cores = 4,
  control = list(adapt_delta = 0.95)
)

summary(mssd_model_bayes)
saveRDS(mssd_model_bayes, "Ho1_c_results_mssd.rds")


## ------------------------------------------------------------------
## Hypothesis 2(a): Social interaction (Bernoulli, Bayesian)
## ------------------------------------------------------------------

EMA_dat <- EMA_dat %>%
  mutate(across(everything(), ~ na_if(., -999))) %>%
  group_by(SID) %>%
  mutate(
    lonely_bp = mean(lonely, na.rm = TRUE),
    lonely_wi = lonely - lonely_bp
  ) %>%
  ungroup()

SI_model_main <- brm(
  SI ~ lonely_wi + lon_TL + lonely_bp + (1 + lonely_wi | SID),
  data = EMA_dat,
  family = bernoulli(),
  prior = c(
    prior(normal(0, 2), class = "b"),
    prior(normal(0, 5), class = "Intercept"),
    prior(cauchy(0, 2), class = "sd")
  ),
  iter = 2000, warmup = 1000, chains = 2, cores = 2,
  control = list(adapt_delta = 0.95)
)

summary(SI_model_main)

SI_model <- brm(
  SI ~ lonely_wi * lon_TL + lonely_bp + (1 + lonely_wi | SID),
  data = EMA_dat,
  family = bernoulli(),
  prior = c(
    prior(normal(0, 2), class = "b"),
    prior(normal(0, 5), class = "Intercept"),
    prior(cauchy(0, 2), class = "sd")
  ),
  iter = 2000, warmup = 1000, chains = 2, cores = 2,
  control = list(adapt_delta = 0.95)
)

summary(SI_model)
saveRDS(SI_model, "Ho2_a_results_SI.rds")

effects_data <- conditional_effects(SI_model, effects = "lonely_wi:lon_TL")
plot_data_interaction <- effects_data$`lonely_wi:lon_TL` %>%
  mutate(lon_TL = formatC(lon_TL, format = "f", digits = 2))


## ------------------------------------------------------------------
## Hypothesis 2(b): Self-disclosure (Gaussian, Bayesian)
## ------------------------------------------------------------------

EMA_dat <- EMA_dat %>%
  mutate(
    SS_Total = ss_disclose + ss_understood
  ) %>%
  group_by(SID) %>%
  mutate(
    lonely_bp = mean(lonely, na.rm = TRUE),
    lonely_wi = lonely - lonely_bp
  ) %>%
  ungroup()

SS_model_main <- brm(
  SS_Total ~ lonely_wi + lon_TL + lonely_bp + (1 | SID),
  data = EMA_dat,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b"),
    prior(normal(0, 10), class = "Intercept"),
    prior(cauchy(0, 2), class = "sd")
  ),
  iter = 2000, warmup = 1000, chains = 2, cores = 2,
  control = list(adapt_delta = 0.95)
)

round(summary(SS_model_main)$fixed, 3)

SS_model <- brm(
  SS_Total ~ lonely_wi * lon_TL + lonely_bp + (1 | SID),
  data = EMA_dat,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b"),
    prior(normal(0, 10), class = "Intercept"),
    prior(cauchy(0, 2), class = "sd")
  ),
  iter = 2000, warmup = 1000, chains = 2, cores = 2,
  control = list(adapt_delta = 0.95)
)

saveRDS(SS_model, "Ho2_b_results_SS.rds")
round(summary(SS_model)$fixed, 3)

effects_data <- conditional_effects(SS_model, effects = "lonely_wi:lon_TL")
plot_data_disclosure <- effects_data$`lonely_wi:lon_TL` %>%
  mutate(lon_TL = formatC(lon_TL, format = "f", digits = 2))


## ------------------------------------------------------------------
## Interaction plots for Hypothesis 2(a) and 2(b)
## ------------------------------------------------------------------

p1 <- ggplot(plot_data_interaction,
             aes(x = lonely_wi, y = estimate__, color = factor(lon_TL), group = factor(lon_TL))) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = factor(lon_TL)), alpha = 0.2) +
  scale_color_viridis_d(option = "plasma") +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  labs(x = "Momentary Loneliness", y = "Predicted Social Interaction") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(size = 14))

p2 <- ggplot(plot_data_disclosure,
             aes(x = lonely_wi, y = estimate__, color = factor(lon_TL), group = factor(lon_TL))) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = factor(lon_TL)), alpha = 0.2) +
  scale_color_viridis_d(option = "plasma", name = "Trait Loneliness") +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  labs(x = "Momentary Loneliness", y = "Predicted Self-Disclosure") +
  theme_minimal() +
  theme(legend.position = "right", text = element_text(size = 14))

combined_plot <- p1 + p2 + plot_layout(ncol = 2)
ggsave("combined_loneliness_effects.png", combined_plot, width = 12, height = 6, dpi = 300)