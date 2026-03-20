

# Model for fixation density on the four regions (line 758 in extra_analyses)

## add columns containing inflection frequency (both absolute and log10-transformed)

# add inflection column
fix_report_target_word_and_baseline <- 
  fix_report_target_word_and_baseline %>%
  mutate(
    inflection = pmap_chr(
      list(sentence_2_kal, sentence_2_ids, inflection_ids),
      function(sent, sent_ids_raw, infl_ids) {
        
        # 1. Turn sentence_2_ids into a vector
        sent_ids <- str_split(sent_ids_raw, ",\\s*")[[1]]
        
        # 2. Match inflection IDs to sentence IDs
        positions <- match(infl_ids, sent_ids)
        
        # 3. Split sentence into characters
        chars <- str_split(sent, "")[[1]]
        
        # 4. Extract characters
        paste0(chars[positions], collapse = "")
      }
    )
  )

# make df containing inflection frequency
df_inflection_frequency <- fix_report_target_word_and_baseline %>%
  group_by(inflection) %>%
  summarise("inflection_frequency_absolute" = NA)

df_inflection_frequency <- df_inflection_frequency %>% 
  mutate(inflection_frequency_absolute = recode(
    inflection,
    "aangat" = 9454,
    "aat" = 606457,
    "ami" = 250904,
    "amiuk" = 18667,
    "aq" = 699499,
    "at" = 1473379,
    "gaa" = 79801,
    "gami" = 19840,
    "gamik" = 13697,
    "li" = 286947,
    "lluni" = 165895,
    "llutik" = 93348,
    "luni" = 346093,
    "lutik" = 196583,
    "mat" = 145798,
    "mata" = 51333,
    "mmat" = 71007,
    "mmata" = 28507,
    "paa" = 104292,
    "paat" = 85273,
    "pai" = 39738,
    "poq" = 673407,
    "pput" = 291821,
    "put" = 628027,
    "soq" = 349321,
    "sut" = 550045,
    "vaa" = 48810,
    "vaat" = 44830,
    "vai" = 21200,
    "voq" = 289743,
    "vunga" = 12537,
    .default = NA_real_
  ))

# note: for the inflection frequencies, we used the "words ends with" search function in the corpus
# so some of the hits may not actually be the same inflection but the ending of something else
# that just happens to be spelled the same

# merge with fix_report
fix_report_target_word_and_baseline <- 
  fix_report_target_word_and_baseline %>%
  left_join(df_inflection_frequency, by = "inflection")

# add column containing log10-transformed inflection frequency
fix_report_target_word_and_baseline <- 
  fix_report_target_word_and_baseline %>%
  mutate(
    inflection_frequency_log = log10(inflection_frequency_absolute)
  )

## create column region_frequency_log
fix_report_target_word_and_baseline <- 
  fix_report_target_word_and_baseline %>%
  mutate(
    region_frequency_log = case_when(
      fixation_position == "root" ~ root_frequency_log,
      fixation_position == "affix" ~ affix_frequency_log,
      fixation_position == "fill" ~ fill_affix_frequency_log,
      fixation_position == "inflection" ~ inflection_frequency_log,
      fixation_position == "word" ~ baseline_word_frequency_log,
      TRUE ~ NA_real_
    )
  )

## calculate fixation density on each region
fix_density_df <- 
  fix_report_target_word_and_baseline %>%
  group_by(trial_key, participant_id, fixation_position) %>%
  summarise(
    n_fixations = n(),
    region_length = first(region_length),
    region_frequency_log = first(region_frequency_log),
    fixation_density = n_fixations / region_length,
    .groups = "drop"
  )

## fit a lmer model

fix_density_df$fixation_position <- 
  factor(fix_density_df$fixation_position,
         levels = c("affix", "root", "fill", "inflection", "word"))

model_fix_dens <- lmer(
  fixation_density ~ fixation_position + 
    region_frequency_log + 
    region_length + 
    (1 | participant_id),
  data = fix_density_df
)

summary(model_fix_dens)