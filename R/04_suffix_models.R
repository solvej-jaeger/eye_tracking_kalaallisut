## skipping model

df_skip_model <- df_affix %>%
  mutate(
    skipped = as.integer(FIRST_PASS_FIXATION_COUNT == 0)    # 1 = skipped, 0 = fixated
  )

model_skipping <- glmer(
  skipped ~ affix_type + affix_frequency_log + affix_length + predictability_log +
    (1 | participant_id), # random intercepts for participants
  data = df_skip_model,
  family = "binomial"
)

## ffd model

# trials where target affix is skipped are excluded from the ffd and gd analyses
df_affix_without_skipped <- df_affix %>%
  filter(FIRST_PASS_FIXATION_COUNT != 0) %>%
  mutate(FIRST_FIXATION_DURATION_log = log(FIRST_FIXATION_DURATION),
         GAZE_DURATION_log = log(GAZE_DURATION))

model_ffd <- lmer(
  FIRST_FIXATION_DURATION_log ~ affix_type + affix_frequency_log + affix_length + predictability_log +
    (1 | participant_id),
  data = df_affix_without_skipped
)

## gd model

model_gd <- lmer(
  GAZE_DURATION_log ~ affix_type + affix_frequency_log + affix_length + predictability_log +
    (1 | participant_id), # random intercept
  data = df_affix_without_skipped
)