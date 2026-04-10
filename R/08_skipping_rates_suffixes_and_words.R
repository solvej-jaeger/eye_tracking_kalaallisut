## merging the data frames

# first define columns to exclude from renaming
exclude_cols <- c("trial_key", "participant_id")

# rename df_root: add _root suffix
df_root_renamed <- df_root
names(df_root_renamed) <- ifelse(
  names(df_root_renamed) %in% exclude_cols,
  names(df_root_renamed),
  paste0(names(df_root_renamed), "_root")
)

# rename df_affix: add _target suffix
df_affix_renamed <- df_affix
names(df_affix_renamed) <- ifelse(
  names(df_affix_renamed) %in% exclude_cols,
  names(df_affix_renamed),
  paste0(names(df_affix_renamed), "_target")
)

# rename df_baseline: add _word suffix
df_baseline_renamed <- df_baseline
names(df_baseline_renamed) <- ifelse(
  names(df_baseline_renamed) %in% exclude_cols,
  names(df_baseline_renamed),
  paste0(names(df_baseline_renamed), "_word")
)

# rename df_fill: add _fill suffix
df_fill_renamed <- df_fill
names(df_fill_renamed) <- ifelse(
  names(df_fill_renamed) %in% exclude_cols,
  names(df_fill_renamed),
  paste0(names(df_fill_renamed), "_fill")
)

# rename df_inflection: add _inflection suffix
df_inflection_renamed <- df_inflection
names(df_inflection_renamed) <- ifelse(
  names(df_inflection_renamed) %in% exclude_cols,
  names(df_inflection_renamed),
  paste0(names(df_inflection_renamed), "_inflection")
)

# merge on trial_key and participant_id
df_merged <- list(
  df_root_renamed,
  df_affix_renamed,
  df_baseline_renamed,
  df_fill_renamed,
  df_inflection_renamed
) %>%
  reduce(full_join, by = c("trial_key", "participant_id"))

# create length columns
df_merged <- df_merged %>%
  mutate(
    length_root = crit_root_end_root - crit_root_start_root + 1,
    length_target = crit_affix_end_target - crit_affix_start_target + 1,
    length_fill = fill_suffix_end_fill - fill_suffix_start_fill + 1,
    length_inflection = inflection_end_inflection - inflection_start_inflection + 1,
    length_word = crit_word_end_word - crit_word_start_word + 1
  )

# reshape into long format
df_merged_long <- df_merged %>%
  select(
    participant_id, trial_key,
    length_root, length_target, length_fill, length_inflection, length_word,
    FIRST_FIXATION_DURATION_root, FIRST_FIXATION_DURATION_target,
    FIRST_FIXATION_DURATION_fill, FIRST_FIXATION_DURATION_inflection,
    FIRST_FIXATION_DURATION_word,
    GAZE_DURATION_root, GAZE_DURATION_target, GAZE_DURATION_fill,
    GAZE_DURATION_inflection, GAZE_DURATION_word,
    FIRST_PASS_FIXATION_COUNT_root, FIRST_PASS_FIXATION_COUNT_target,
    FIRST_PASS_FIXATION_COUNT_fill, FIRST_PASS_FIXATION_COUNT_inflection,
    FIRST_PASS_FIXATION_COUNT_word,
    REGRESSION_IN_COUNT_root, REGRESSION_IN_COUNT_target,
    REGRESSION_IN_COUNT_fill, REGRESSION_IN_COUNT_inflection,
    REGRESSION_IN_COUNT_word,
    root_frequency_log_root, affix_frequency_log_target,
    baseline_word_frequency_log_word, fill_affix_frequency_log_fill
  ) %>%
  pivot_longer(
    cols = c(
      length_root, length_target, length_fill, length_inflection, length_word,
      FIRST_FIXATION_DURATION_root, FIRST_FIXATION_DURATION_target,
      FIRST_FIXATION_DURATION_fill, FIRST_FIXATION_DURATION_inflection,
      FIRST_FIXATION_DURATION_word,
      GAZE_DURATION_root, GAZE_DURATION_target, GAZE_DURATION_fill,
      GAZE_DURATION_inflection, GAZE_DURATION_word,
      FIRST_PASS_FIXATION_COUNT_root, FIRST_PASS_FIXATION_COUNT_target,
      FIRST_PASS_FIXATION_COUNT_fill, FIRST_PASS_FIXATION_COUNT_inflection,
      FIRST_PASS_FIXATION_COUNT_word,
      REGRESSION_IN_COUNT_root, REGRESSION_IN_COUNT_target,
      REGRESSION_IN_COUNT_fill, REGRESSION_IN_COUNT_inflection,
      REGRESSION_IN_COUNT_word,
      root_frequency_log_root, affix_frequency_log_target,
      baseline_word_frequency_log_word, fill_affix_frequency_log_fill
    ),
    names_to = c(".value", "type"),
    names_pattern = "(.*)_(root|target|fill|inflection|word)"
  ) %>%
  mutate(
    frequency_log = case_when(
      type == "target" ~ affix_frequency_log,
      type == "fill" ~ fill_affix_frequency_log,
      type == "root" ~ root_frequency_log,
      type == "inflection" ~ NA_real_,
      TRUE ~ baseline_word_frequency_log
    )
  ) %>%
  select(
    participant_id, trial_key, type, length,
    FIRST_FIXATION_DURATION, GAZE_DURATION,
    FIRST_PASS_FIXATION_COUNT, REGRESSION_IN_COUNT, frequency_log
  )

# delete rows with NAs
df_merged_long <- df_merged_long %>%
  filter(!is.na(FIRST_FIXATION_DURATION))

# add column indicating if region was skipped
df_merged_long <- df_merged_long %>%
  mutate(skipped = as.integer(FIRST_PASS_FIXATION_COUNT == 0))

# filter df to only include target suffixes and baseline words
df_suffixes_and_words <- df_merged_long %>%
  mutate(length = as.numeric(length)) %>%
  filter(type == "target" | type == "word")

df_skipping_suffixes_and_words <- df_suffixes_and_words %>%
  group_by(length, type) %>%
  summarise(
    n = n(),
    skipped = sum(FIRST_PASS_FIXATION_COUNT == 0),
    skip_proportion = skipped / n,
    se = sqrt(skip_proportion * (1 - skip_proportion) / n),
    .groups = "drop"
  )


## how often are inflections skipped?

inflection_skipping_check <- df_merged_long %>%   
  group_by(type, participant_id) %>%   
  summarise(skipping_proportion = mean(skipped)) %>%
  filter(type == "inflection")

mean_skipping_rate <- mean(inflection_skipping_check$skipping_proportion)


## skipping model target suffixes and baseline words

model_suffix_baseline_skipping <- glmer(
  skipped ~ type + length +
    (1 | participant_id), 
  data = df_suffixes_and_words,
  family = "binomial"
)

