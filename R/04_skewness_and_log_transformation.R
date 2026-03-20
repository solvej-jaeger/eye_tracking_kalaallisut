# checking if frequency and predictability are skewed
skewness(df_affix_cleaned$affix_frequency_absolute)
skewness(df_affix_cleaned$predictability)
# they are both moderately to strongly skewed (>1) and will therefore be log10-transformed for analysis

# log-transforming predictability (there is already a column with log10-transformed frequency)
df_affix_cleaned <- df_affix_cleaned %>%
  mutate(predictability_log = log10(predictability + 1e-7))