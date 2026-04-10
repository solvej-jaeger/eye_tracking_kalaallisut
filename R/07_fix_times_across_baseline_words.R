# divide into fourths
fix_report_target_word_and_baseline <- fix_report_target_word_and_baseline %>%
  mutate(
    fix_position_normalized_fourth = cut(
      fix_position_normalized,
      breaks = c(0, 1/4, 2/4, 3/4, 1),
      labels = c(1, 2, 3, 4),
      include.lowest = TRUE,
      right = TRUE
    )
  )

# compute fixation duration per bin
fixations_within_baseline_words2 <- fix_report_target_word_and_baseline %>%
  group_by(fixation_position, fix_position_normalized_fourth) %>%
  summarise(
    mean_duration =
      sum(CURRENT_FIX_DURATION) / length(df_baseline$trial_key),
    .groups = "drop"
  ) %>%
  left_join(region_lengths, by = "fixation_position") %>%
  mutate(
    mean_duration_per_letter =
      mean_duration / (mean_region_length / 4)
  )

# keep only baseline words
fixations_within_baseline_words2 <- fixations_within_baseline_words2 %>%
  filter(fixation_position == "word")
