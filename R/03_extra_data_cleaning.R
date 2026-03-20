# Trials are excluded from the affix analysis if number of fixations per character on the 
# target word (word containing target affix) is more than 3 SDs from the participant mean
# (because this could indicate that the participant had trouble understanding the target word).

# first convert CURRENT_FIX_INTEREST_AREA_ID to integer
fix_report <- fix_report %>%
  mutate(CURRENT_FIX_INTEREST_AREA_ID = as.integer(ifelse(CURRENT_FIX_INTEREST_AREA_ID == ".", NA, CURRENT_FIX_INTEREST_AREA_ID)))

# add column containing length of target word
fix_report <- fix_report %>%
  mutate(target_word_length = nchar(target_word))

# get number of fixations on target word for each trial
fixations_on_target <- fix_report %>%
  filter(CURRENT_FIX_INTEREST_AREA_ID >= crit_root_start &
           CURRENT_FIX_INTEREST_AREA_ID <= rest_word_end) %>%
  group_by(trial_key, participant_id, item_no, target_word_length) %>%
  summarise(n_fix_target_word = n()) %>%
  ungroup()

# make column with fix per char for each trial
fixations_on_target <- fixations_on_target %>%
  mutate(n_fix_per_char = n_fix_target_word / target_word_length)

# calculate participant means and SDs
fixations_on_target_means <- fixations_on_target %>%
  group_by(participant_id) %>%
  mutate(
    mean_fix_char = mean(n_fix_per_char, na.rm = TRUE),
    sd_fix_char = sd(n_fix_per_char, na.rm = TRUE),
    z_score = (n_fix_per_char - mean_fix_char) / sd_fix_char,
    is_outlier = abs(z_score) > 3
  ) %>%
  ungroup()

# get list of outliers (trials where n_fix_target_word is more than 3 SDs from the mean)
outlier_trials <- fixations_on_target_means %>%
  filter(is_outlier == TRUE)

# exclude outliers
df_affix_cleaned <- df_affix %>%
  filter(!trial_key %in% outlier_trials$trial_key)
df_root_cleaned <- df_root %>%
  filter(!trial_key %in% outlier_trials$trial_key)
df_fill_cleaned <- df_fill %>%
  filter(!trial_key %in% outlier_trials$trial_key)
df_inflection_cleaned <- df_inflection %>%
  filter(!trial_key %in% outlier_trials$trial_key)