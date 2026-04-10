# add columns to fix_report containing target word ids and baseline word ids
fix_report <- fix_report %>%
  rowwise() %>%
  mutate(
    target_word_ids = list(crit_root_start:rest_word_end),
    baseline_word_ids = list(crit_word_start:crit_word_end)
  ) %>%
  ungroup()

# extract rows where fixation falls within baseline or target word
fix_report_target_word_and_baseline <- fix_report %>%
  rowwise() %>%
  filter(
    CURRENT_FIX_INTEREST_AREA_ID %in% target_word_ids |
      CURRENT_FIX_INTEREST_AREA_ID %in% baseline_word_ids
  ) %>%
  ungroup()

# add columns containing ids for root, target suffix, fill suffix, inflection
fix_report_target_word_and_baseline <- fix_report_target_word_and_baseline %>%
  rowwise() %>%
  mutate(
    root_ids = list(crit_root_start:crit_root_end),
    affix_ids = list(crit_affix_start:crit_affix_end),
    fill_ids = list(fill_suffix_start:fill_suffix_end),
    inflection_ids = list(inflection_start:inflection_end),
    root_len = length(root_ids),
    affix_len = length(affix_ids),
    fill_len = length(fill_ids),
    inflection_len = length(inflection_ids),
    word_len = length(baseline_word_ids)
  ) %>%
  ungroup()

# indicate fixation region
fix_report_target_word_and_baseline <- fix_report_target_word_and_baseline %>%
  rowwise() %>%
  mutate(
    fixation_position = case_when(
      CURRENT_FIX_INTEREST_AREA_ID %in% root_ids ~ "root",
      CURRENT_FIX_INTEREST_AREA_ID %in% affix_ids ~ "affix",
      CURRENT_FIX_INTEREST_AREA_ID %in% fill_ids ~ "fill",
      CURRENT_FIX_INTEREST_AREA_ID %in% inflection_ids ~ "inflection",
      CURRENT_FIX_INTEREST_AREA_ID %in% baseline_word_ids ~ "word",
      TRUE ~ "other"
    )
  ) %>%
  ungroup()

# add region start and end
fix_report_target_word_and_baseline <- fix_report_target_word_and_baseline %>%
  mutate(
    region_start = ifelse(
      fixation_position == "root", crit_root_start,
      ifelse(
        fixation_position == "affix", crit_affix_start,
        ifelse(
          fixation_position == "fill", fill_suffix_start,
          ifelse(
            fixation_position == "inflection", inflection_start,
            ifelse(fixation_position == "word", crit_word_start, NA)
          )
        )
      )
    ),
    region_end = ifelse(
      fixation_position == "root", crit_root_end,
      ifelse(
        fixation_position == "affix", crit_affix_end,
        ifelse(
          fixation_position == "fill", fill_suffix_end,
          ifelse(
            fixation_position == "inflection", inflection_end,
            ifelse(fixation_position == "word", crit_word_end, NA)
          )
        )
      )
    )
  )

# convert to integer
fix_report_target_word_and_baseline <- fix_report_target_word_and_baseline %>%
  mutate(
    region_start = as.integer(region_start),
    region_end = as.integer(region_end)
  )

# region length
fix_report_target_word_and_baseline <- fix_report_target_word_and_baseline %>%
  mutate(region_length = region_end - region_start + 1)

# exclude regions that are only two letters
fix_report_target_word_and_baseline <- fix_report_target_word_and_baseline %>%
  filter(region_length != 2)

# fixation id within region
fix_report_target_word_and_baseline <- fix_report_target_word_and_baseline %>%
  mutate(fix_id_within_region = CURRENT_FIX_INTEREST_AREA_ID - region_start + 1)

# normalized fixation position
fix_report_target_word_and_baseline <- fix_report_target_word_and_baseline %>%
  mutate(
    fix_position_normalized =
      (fix_id_within_region - 0.5) / region_length
  )

# divide into thirds
fix_report_target_word_and_baseline <- fix_report_target_word_and_baseline %>%
  mutate(
    fix_position_normalized_third = cut(
      fix_position_normalized,
      breaks = c(0, 1/3, 2/3, 1),
      labels = c(1, 2, 3),
      include.lowest = TRUE,
      right = TRUE
    )
  )

# mean region length
region_lengths <- fix_report_target_word_and_baseline %>%
  group_by(fixation_position) %>%
  summarise(
    mean_region_length = mean(region_length, na.rm = TRUE),
    .groups = "drop"
  )

# fixation duration per bin
fixations_within_regions_grouped4 <- fix_report_target_word_and_baseline %>%
  group_by(fixation_position, fix_position_normalized_third) %>%
  summarise(
    mean_duration =
      sum(CURRENT_FIX_DURATION) / length(df_affix$trial_key),
    .groups = "drop"
  ) %>%
  left_join(region_lengths, by = "fixation_position") %>%
  mutate(
    mean_duration_per_letter =
      mean_duration / (mean_region_length / 3)
  )

# remove baseline word
fixations_within_regions_grouped4 <- fixations_within_regions_grouped4 %>%
  filter(fixation_position != "word")

# relabel factors
fixations_within_regions_grouped4$fixation_position <- factor(
  fixations_within_regions_grouped4$fixation_position,
  levels = c("root", "affix", "fill", "inflection"),
  labels = c("root", "target suffix", "fill suffix", "inflection")
)
