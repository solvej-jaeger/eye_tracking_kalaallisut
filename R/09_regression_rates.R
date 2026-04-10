# add column indicating if morpheme/word was regressed to
df_merged_long <- df_merged_long %>%
  mutate(regressed_to = as.integer(REGRESSION_IN_COUNT != 0))

# preparations for plot showing regression proportion for words and morphemes

# group by subject and type
df_regressions_morphemes_and_words <- df_merged_long %>%
  group_by(type, participant_id) %>%
  summarise(
    n = n(),
    regression_proportion = sum(REGRESSION_IN_COUNT != 0) / n,
    .groups = "drop"
  )

# get group-level mean and SE across subjects
df_regressions_morphemes_and_words2 <- df_regressions_morphemes_and_words %>%
  group_by(type) %>%
  summarise(
    n_subjects = n(),
    mean_regression_prop = mean(regression_proportion),
    se_across_subjects = sd(regression_proportion) / sqrt(n_subjects),
    .groups = "drop"
  )

# relabel type factor
df_regressions_morphemes_and_words2$type <- factor(
  df_regressions_morphemes_and_words2$type,
  levels = c("root", "target", "fill", "inflection", "word"),
  labels = c("root", "target suffix", "fill suffix", "inflection", "baseline word")
)

# include only roots, target suffixes, and baseline words
df_regressions_morphemes_and_words2 <- df_regressions_morphemes_and_words2 %>%
  filter(type != "fill suffix", type != "inflection")

## regression model

df_root_suffix_word <- df_merged_long %>%
  filter(type != "fill", type != "inflection")

df_root_suffix_word <- df_root_suffix_word %>%
  mutate(
    type = factor(type, levels = c("target", "root", "word"))
  )

regression_model_root_suffix_word <- glmer(
  (REGRESSION_IN_COUNT != 0) ~ type + skipped + (1 | participant_id),
  data = df_root_suffix_word,
  family = binomial
)
