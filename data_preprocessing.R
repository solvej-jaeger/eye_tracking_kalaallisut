library(dplyr)
library(tidyr)
library(purrr)
library(zoo)
library(stringr)

# read fixation report
fix_report_original <- read.delim("raw_data/fix_report_complete.txt", fileEncoding = "UTF-8-BOM")
fix_report_clean <- fix_report_original

# read reading-measures files
df_affix_clean <- read.csv("raw_data/rm_target_affix.csv", sep = "\t", fileEncoding = "UTF-16LE")
df_baseline_clean <- read.csv("raw_data/rm_baseline.csv", sep = "\t", fileEncoding = "UTF-16LE")
# baseline word is including the space before the word
df_root_clean <- read.csv("raw_data/rm_root.csv", sep = "\t", fileEncoding = "UTF-16LE")
df_fill_clean <- read.csv("raw_data/rm_fill.csv", sep = "\t", fileEncoding = "UTF-16LE")
df_inflection_clean <- read.csv("raw_data/rm_inflection.csv", sep = "\t", fileEncoding = "UTF-16LE")

# rename participant_id column
names(df_affix_clean)[names(df_affix_clean) == 'Session_Name_'] <- 'participant_id'
names(df_baseline_clean)[names(df_baseline_clean) == 'Session_Name_'] <- 'participant_id'
names(df_root_clean)[names(df_root_clean) == 'Session_Name_'] <- 'participant_id'
names(df_fill_clean)[names(df_fill_clean) == 'Session_Name_'] <- 'participant_id'
names(df_inflection_clean)[names(df_inflection_clean) == 'Session_Name_'] <- 'participant_id'
names(fix_report_clean)[names(fix_report_clean) == 'RECORDING_SESSION_LABEL'] <- 'participant_id'
names(fix_report_original)[names(fix_report_original) == 'RECORDING_SESSION_LABEL'] <- 'participant_id'

# add trial key
df_affix_clean <- df_affix_clean %>%
  mutate(trial_key = paste(participant_id, item_no, sep = "_"))
df_baseline_clean <- df_baseline_clean %>%
  mutate(trial_key = paste(participant_id, item_no, sep = "_"))
df_root_clean <- df_root_clean %>%
  mutate(trial_key = paste(participant_id, item_no, sep = "_"))
df_fill_clean <- df_fill_clean %>%
  mutate(trial_key = paste(participant_id, item_no, sep = "_"))
df_inflection_clean <- df_inflection_clean %>%
  mutate(trial_key = paste(participant_id, item_no, sep = "_"))
fix_report_clean <- fix_report_clean %>%
  mutate(trial_key = paste(participant_id, item_no, sep = "_"))
fix_report_original <- fix_report_original %>%
  mutate(trial_key = paste(participant_id, item_no, sep = "_"))

## adding column with log10-transformed predictability to df_affix_clean
########################################################################

df_affix_clean <- df_affix_clean %>%
  mutate(predictability_log = log10(predictability + 1e-7))

###################
## data cleaning ##
###################

# remove data from participants whose accuracy on comprehension questions is at chance level
#############################################################################################

accuracy_count <- df_affix_clean %>%
  group_by(participant_id) %>%
  summarise(correct = sum(ACCURACY == 1),
            incorrect = sum(ACCURACY == 0),
            accuracy_percentage = (correct / (correct + incorrect)) * 100)

# binomial test to determine cutoff point
min_number_correct <- min(which(sapply(0:40, function(x) binom.test(x, 40, 0.5)$p.value) < 0.05 & 0:40 > 20))
# which accuracy percentage does this correspond to?
min_accuracy <- min_number_correct / 40 * 100

# participants to discard on this basis
participants_to_remove <- accuracy_count %>%
  filter(accuracy_percentage < min_accuracy)
# data from 3 participants must be discarded 

# discarding data from these participants
df_affix_clean <- df_affix_clean %>%
  filter(!participant_id %in% participants_to_remove$participant_id)
df_baseline_clean <- df_baseline_clean %>%
  filter(!participant_id %in% participants_to_remove$participant_id)
df_root_clean <- df_root_clean %>%
  filter(!participant_id %in% participants_to_remove$participant_id)
df_fill_clean <- df_fill_clean %>%
  filter(!participant_id %in% participants_to_remove$participant_id)
df_inflection_clean <- df_inflection_clean %>%
  filter(!participant_id %in% participants_to_remove$participant_id)
fix_report_clean <- fix_report_clean %>%
  filter(!participant_id %in% participants_to_remove$participant_id)

# remove trials where there was a disturbance (noted in log) or participant pressed space too soon
###################################################################################################

# read csv with these trials
df_trials_noted_in_log <- read.csv("raw_data/trials_noted_in_log.csv", sep = ";", fileEncoding = "UTF-8")

# get item_no for all trials and create trial_key
df_trials_noted_in_log <- df_trials_noted_in_log %>%
  left_join(
    df_affix_clean %>% select(participant_id, TRIAL_INDEX, item_no_affix = item_no),
    by = c("participant_id", "TRIAL_INDEX")
  ) %>%
  mutate(
    # filling missing item_no from df_affix_clean if necessary
    item_no = ifelse(is.na(item_no), item_no_affix, item_no)
  ) %>%
  select(-item_no_affix) %>%  # removing temporary column
  mutate(
    # creating trial_key
    trial_key = paste0(participant_id, "_", item_no)
  )

# remove trials 
df_affix_clean <- df_affix_clean %>%
  filter(!trial_key %in% df_trials_noted_in_log$trial_key)
df_baseline_clean <- df_baseline_clean %>%
  filter(!trial_key %in% df_trials_noted_in_log$trial_key)
df_root_clean <- df_root_clean %>%
  filter(!trial_key %in% df_trials_noted_in_log$trial_key)
df_fill_clean <- df_fill_clean %>%
  filter(!trial_key %in% df_trials_noted_in_log$trial_key)
df_inflection_clean <- df_inflection_clean %>%
  filter(!trial_key %in% df_trials_noted_in_log$trial_key)
fix_report_clean <- fix_report_clean %>%
  filter(!trial_key %in% df_trials_noted_in_log$trial_key)

# exclude rereading from fix_report_clean
###########################################

# if they go back to fixating on sentence 1 and 2 after reading sentence 3, exclude those fixations

# calculate the IA id ranges for each sentence
fix_report_clean <- fix_report_clean %>%
  rowwise() %>%
  mutate(
    len1 = nchar(sentence_1_kal),
    len2 = nchar(sentence_2_kal),
    len3 = nchar(sentence_3_kal),
    sentence_1_ids = list(1:len1),
    sentence_2_ids = list((len1 + 1):(len1 + len2)),
    sentence_3_ids = list((len1 + len2 + 1):(len1 + len2 + len3))
  ) %>%
  ungroup()

# function to identify if CURRENT_INTEREST_AREA is in any sentence
get_sentence_label <- function(interest_area, s1, s2, s3) {
  if (interest_area %in% s1) return(1)
  if (interest_area %in% s2) return(2)
  if (interest_area %in% s3) return(3)
  return(NA)
}

# applying sentence label to each fixation
fix_report_clean <- fix_report_clean %>%
  rowwise() %>%
  mutate(
    sentence_label = get_sentence_label(CURRENT_FIX_INTEREST_AREA_ID, sentence_1_ids, sentence_2_ids, sentence_3_ids)
  ) %>%
  ungroup()

# modifying fix_report so it contains only initial pass fixations per trial (no rereading)

# step-by-step overview:
# group by trial.
# for each trial, extract the sequence of sentence_labels.
# find the first point where there are 3 consecutive fixations on sentence 3.
# from that point onward, search for 3 consecutive fixations on sentence 1 or 2.
# if found, exclude all fixations from that point onward in the trial.

# function to find rereading start index
find_rereading_start <- function(sentence_seq) {
  # Find the first index where there are 3 consecutive 3s (reading of sentence 3)
  runs_3 <- rollapply(sentence_seq, width = 3, FUN = function(x) all(x == 3), fill = NA, align = "left")
  first_s3_idx <- which(runs_3)[1]
  if (is.na(first_s3_idx)) return(Inf)  # No proper sentence 3 reading found
  
  # Search for 3 consecutive 1s or 2s after first_s3_idx
  post_s3_seq <- sentence_seq[(first_s3_idx + 3):length(sentence_seq)]
  runs_12 <- rollapply(post_s3_seq, width = 3, FUN = function(x) all(x %in% c(1, 2)), fill = NA, align = "left")
  reread_start_idx <- which(runs_12)[1]
  
  if (!is.na(reread_start_idx)) {
    return(first_s3_idx + 3 + reread_start_idx - 1)
  } else {
    return(Inf)  # No rereading found
  }
}

# apply to fix_report_clean
fix_report_clean <- fix_report_clean %>%
  group_by(trial_key, participant_id) %>%
  mutate(row_index = row_number()) %>%
  mutate(rereading_start = {
    reread_idx <- find_rereading_start(sentence_label)
    ifelse(row_index >= reread_idx, TRUE, FALSE)
  }) %>%
  filter(!rereading_start) %>%
  select(-rereading_start, -row_index)

# remove all trials where there was not a fixation on every line
##################################################################

# for each trial, check if each sentence was fixated
incomplete_trials <- fix_report_clean %>%
  group_by(participant_id, item_no, trial_key) %>%
  summarise(
    fixated_s1 = any(CURRENT_FIX_INTEREST_AREA_ID %in% unlist(sentence_1_ids)),
    fixated_s2 = any(CURRENT_FIX_INTEREST_AREA_ID %in% unlist(sentence_2_ids)),
    fixated_s3 = any(CURRENT_FIX_INTEREST_AREA_ID %in% unlist(sentence_3_ids)),
    all_lines_fixated = fixated_s1 & fixated_s2 & fixated_s3
  ) %>%
  filter(!all_lines_fixated)

# remove incomplete trials
df_affix_clean <- df_affix_clean %>%
  filter(!trial_key %in% incomplete_trials$trial_key)
df_baseline_clean <- df_baseline_clean %>%
  filter(!trial_key %in% incomplete_trials$trial_key)
df_root_clean <- df_root_clean %>%
  filter(!trial_key %in% incomplete_trials$trial_key)
df_fill_clean <- df_fill_clean %>%
  filter(!trial_key %in% incomplete_trials$trial_key)
df_inflection_clean <- df_inflection_clean %>%
  filter(!trial_key %in% incomplete_trials$trial_key)
fix_report_clean <- fix_report_clean %>%
  filter(!trial_key %in% incomplete_trials$trial_key)

# remove trials where number of fixations pr. character is more than 3 SDs from the participant's mean
###########################################################################

# get number of fixations and number of interest areas per trial
fixation_density <- fix_report_clean %>%
  group_by(participant_id, trial_key, item_no) %>%
  summarise(
    n_fixations_trial = n(),
    n_chars_trial = max(unlist(sentence_3_ids)),  # total number of characters in this trial
    fix_per_char_trial = n_fixations_trial / n_chars_trial,
    .groups = "drop"
  )

# add these columns to dfs for later use
df_affix_clean <- merge(df_affix_clean, fixation_density[, c("trial_key", "n_chars_trial", "n_fixations_trial", "fix_per_char_trial")], by = "trial_key")
df_baseline_clean <- merge(df_baseline_clean, fixation_density[, c("trial_key", "n_chars_trial", "n_fixations_trial", "fix_per_char_trial")], by = "trial_key")
df_root_clean <- merge(df_root_clean, fixation_density[, c("trial_key", "n_chars_trial", "n_fixations_trial", "fix_per_char_trial")], by = "trial_key")
df_fill_clean <- merge(df_fill_clean, fixation_density[, c("trial_key", "n_chars_trial", "n_fixations_trial", "fix_per_char_trial")], by = "trial_key")
df_inflection_clean <- merge(df_inflection_clean, fixation_density[, c("trial_key", "n_chars_trial", "n_fixations_trial", "fix_per_char_trial")], by = "trial_key")
fix_report_clean <- merge(fix_report_clean, fixation_density[, c("trial_key", "n_chars_trial", "n_fixations_trial", "fix_per_char_trial")], by = "trial_key")

# compute participant-level mean and SD
fix_density_stats <- fixation_density %>%
  group_by(participant_id) %>%
  mutate(
    mean_fix_char = mean(fix_per_char_trial, na.rm = TRUE),
    sd_fix_char = sd(fix_per_char_trial, na.rm = TRUE),
    z_score = (fix_per_char_trial - mean_fix_char) / sd_fix_char,
    is_outlier = abs(z_score) > 3
  ) %>%
  ungroup()

# keep only trials within 3 SDs
clean_trials <- fix_density_stats %>%
  filter(!is_outlier) %>%
  select(participant_id, trial_key)

# filter accordingly
fix_report_clean <- fix_report_clean %>%
  semi_join(clean_trials, by = c("participant_id", "trial_key"))
df_affix_clean <- df_affix_clean %>%
  semi_join(clean_trials, by = c("participant_id", "trial_key"))
df_baseline_clean <- df_baseline_clean %>%
  semi_join(clean_trials, by = c("participant_id", "trial_key"))
df_root_clean <- df_root_clean %>%
  semi_join(clean_trials, by = c("participant_id", "trial_key"))
df_fill_clean <- df_fill_clean %>%
  semi_join(clean_trials, by = c("participant_id", "trial_key"))
df_inflection_clean <- df_inflection_clean %>%
  semi_join(clean_trials, by = c("participant_id", "trial_key"))


## discarding problematic trials
#####################################

# problematic trials = trials where the 3 lines of fixations do not follow the 3 lines of text

# reading csv with problematic trials
problematic_trials <- read.csv("raw_data/trials_to_discard.csv", sep = ";", fileEncoding = "UTF-8")
# adding trial key
problematic_trials <- problematic_trials %>%
  mutate(trial_key = paste(participant_id, item_no, sep = "_"))

# adding column indicating if trial is problematic
df_affix_clean <- df_affix_clean %>%
  mutate(is_problematic = ifelse(trial_key %in% problematic_trials$trial_key, 1, 0))
df_baseline_clean <- df_baseline_clean %>%
  mutate(is_problematic = ifelse(trial_key %in% problematic_trials$trial_key, 1, 0))
df_root_clean <- df_root_clean %>%
  mutate(is_problematic = ifelse(trial_key %in% problematic_trials$trial_key, 1, 0))
df_fill_clean <- df_fill_clean %>%
  mutate(is_problematic = ifelse(trial_key %in% problematic_trials$trial_key, 1, 0))
df_inflection_clean <- df_inflection_clean %>%
  mutate(is_problematic = ifelse(trial_key %in% problematic_trials$trial_key, 1, 0))

# removing problematic trials
df_affix_clean <- df_affix_clean %>%
  filter(is_problematic == 0)
df_baseline_clean <- df_baseline_clean %>%
  filter(is_problematic == 0)
df_root_clean <- df_root_clean %>%
  filter(is_problematic == 0)
df_fill_clean <- df_fill_clean %>%
  filter(is_problematic == 0)
df_inflection_clean <- df_inflection_clean %>%
  filter(is_problematic == 0)
fix_report_clean <- fix_report_clean %>%
  filter(!trial_key %in% problematic_trials$trial_key)


## add column containing reading time per character for each trial
####################################################################

# get rt per character for each trial
reading_time_items <- fix_report_clean %>%
  group_by(trial_key, item_no) %>%
  summarise(
    total_initial_fixations_trial = n(),
    total_initial_duration_trial = sum(CURRENT_FIX_DURATION, na.rm = TRUE),
    n_chars_trial = max(unlist(sentence_3_ids)),  # total number of characters in this trial
    rt_per_char_trial = total_initial_duration_trial / n_chars_trial
  )

# add these columns to dfs for later use
df_affix_clean <- merge(df_affix_clean, reading_time_items[, c("trial_key", "total_initial_duration_trial", "rt_per_char_trial")], by = "trial_key")
df_baseline_clean <- merge(df_baseline_clean, reading_time_items[, c("trial_key", "total_initial_duration_trial", "rt_per_char_trial")], by = "trial_key")
df_root_clean <- merge(df_root_clean, reading_time_items[, c("trial_key", "total_initial_duration_trial", "rt_per_char_trial")], by = "trial_key")
df_fill_clean <- merge(df_fill_clean, reading_time_items[, c("trial_key", "total_initial_duration_trial", "rt_per_char_trial")], by = "trial_key")
df_inflection_clean <- merge(df_inflection_clean, reading_time_items[, c("trial_key", "total_initial_duration_trial", "rt_per_char_trial")], by = "trial_key")
fix_report_clean <- merge(fix_report_clean, reading_time_items[, c("trial_key", "total_initial_duration_trial", "rt_per_char_trial")], by = "trial_key")


######################
### save clean dfs ###
######################

write.table(df_affix_clean, "data/rm_target_affix_clean.tsv", sep = "\t", row.names = FALSE, quote = FALSE)
write.table(df_baseline_clean, "data/rm_baseline_clean.tsv", sep = "\t", row.names = FALSE, quote = FALSE)
write.table(df_root_clean, "data/rm_root_clean.tsv", sep = "\t", row.names = FALSE, quote = FALSE)
write.table(df_fill_clean, "data/rm_fill_clean.tsv", sep = "\t", row.names = FALSE, quote = FALSE)
write.table(df_inflection_clean, "data/rm_inflection_clean.tsv", sep = "\t", row.names = FALSE, quote = FALSE)

# convert list columns to strings in order to save fix_report_clean as a .tsv file
fix_report_clean[] <- lapply(fix_report_clean, function(col) {
  if (is.list(col)) sapply(col, toString) else col
})
write.table(fix_report_clean, "data/fix_report_clean.tsv", sep = "\t", row.names = FALSE, quote = FALSE)

