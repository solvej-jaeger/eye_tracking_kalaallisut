## keep only fixations on sentence 2 and 3 
df_fixations <- fix_report %>%   
  filter(sentence_label == 2 | sentence_label == 3)  

## divide each sentence into words (by spaces)  

# this function splits a sentence into words and returns a list of interest area ID groups (one group per word) 
get_word_ia_groups <- function(sentence_text, ia_ids_str) {   
  # split sentence into characters and words   
  chars <- str_split(sentence_text, pattern = "")[[1]]   
  words <- str_split(sentence_text, pattern = " ")[[1]]   
  # get the length of each word   
  word_lengths <- nchar(words)   
  # split the interest area ids   
  ia_ids <- str_split(ia_ids_str, pattern = ",\\s*")[[1]]   
  # assign IAs to words   
  word_ia_groups <- list()   
  start <- 1   
  for (i in seq_along(word_lengths)) {     
    len <- word_lengths[i]     
    word_ia_groups[[i]] <- ia_ids[start:(start + len - 1)]     
    start <- start + len + 1  
    # +1 to skip space   
  }   
  return(word_ia_groups) }  

# use the function to tag each fixation with its word index 
df_words <- df_fixations %>%   
  rowwise() %>%   
  mutate(     
    word_ia_groups = list(       
      if (sentence_label == 2) {         
        get_word_ia_groups(sentence_2_kal, sentence_2_ids)       
      } 
      else {         
        get_word_ia_groups(sentence_3_kal, sentence_3_ids)       
      }     
    )   ) %>%   
  ungroup()  
df_words <- df_words %>%   
  rowwise() %>%   
  mutate(     
    word_index = {       
      ia_to_word <- setNames(         
        rep(seq_along(word_ia_groups),             
            lengths(word_ia_groups)),         
        unlist(word_ia_groups)         
      )       
      # look up the fixation's IA ID in the mapping       
      ia_to_word[as.character(CURRENT_FIX_INTEREST_AREA_ID)] %||% NA_integer_     
    }   
  ) %>%   
  ungroup()  

# add a column containing the current sentence 
df_words <- df_words %>%   
  mutate(current_sentence = ifelse(sentence_label == 2, sentence_2_kal, sentence_3_kal))  

# also add a column containing the current word 
df_words <- df_words %>%   
  rowwise() %>%   
  mutate(     
    current_word = {       
      words <- str_split(current_sentence, pattern = " ")[[1]]       
      if (!is.na(word_index) && word_index <= length(words)) {         
        words[[word_index]]       
      } else {         
        NA_character_       
      }     
    }   
  ) %>%   
  ungroup()  

## exclude the first and last word in each sentence 
df_words_without_first_and_last <- df_words %>%   
  rowwise() %>%   
  mutate(last_word_index = length(word_ia_groups)) %>%   
  ungroup() %>%   
  filter(word_index != 1 & word_index != last_word_index)  

## add a column containing length of current word 
df_words_without_first_and_last <- df_words_without_first_and_last %>%   
  mutate(current_word_length = nchar(current_word))  

## add a column containing word key (trial_key + sentence_label + word_index) 
df_words_without_first_and_last <- df_words_without_first_and_last %>%   
  mutate(word_key = paste(trial_key, sentence_label, word_index, sep = "_"))  

## including skipped words  

# create a complete list of all words in sentences 2 and 3 (excluding first and last) 
df_all_words <- df_fixations %>%   
  select(participant_id, trial_key, sentence_label, sentence_2_kal, sentence_3_kal, sentence_2_ids, sentence_3_ids) %>%   
  distinct() %>%   
  rowwise() %>%   
  mutate(     
    word_ia_groups = list(       
      if (sentence_label == 2) {         
        get_word_ia_groups(sentence_2_kal, sentence_2_ids)       
      } 
      else {         
        get_word_ia_groups(sentence_3_kal, sentence_3_ids)       
      }     
    ),     
    words = list(str_split(if (sentence_label == 2) sentence_2_kal else sentence_3_kal, " ")[[1]])   
  ) %>%   
  ungroup() %>%   
  mutate(num_words = lengths(word_ia_groups)) %>%   
  mutate(word_index_list = map(word_ia_groups, seq_along)) %>%   
  unnest(cols = c(word_index_list, word_ia_groups)) %>%   
  rename(word_index = word_index_list) %>%   
  rowwise() %>%   
  mutate(current_word = words[[word_index]]) %>%   
  ungroup() %>%   
  filter(word_index != 1 & word_index != num_words) %>%   
  mutate(current_word_length = nchar(current_word),          
         word_key = paste(trial_key, sentence_label, word_index, sep = "_"))  

# discard the two 2-letter words because they are not actually words but numbers 
df_all_words <- df_all_words %>%   
  filter(current_word_length != 2) 
df_words_without_first_and_last <- df_words_without_first_and_last %>%   
  filter(current_word_length != 2)  

## now look at number of fixations pr character by word length  

# group by word key and calculate fixations per char 
fixations_per_char_words <- df_words_without_first_and_last %>%   
  group_by(participant_id, word_key, current_word_length) %>%   
  summarise(fixations = n()) %>%   
  mutate(fixations_per_char = fixations / current_word_length)  

# merge complete word list with fixation counts 
fixations_all_words <- df_all_words %>%   
  select(participant_id, word_key, current_word_length, current_word) %>%   
  left_join(fixations_per_char_words, by = c("participant_id", "word_key", "current_word_length")) %>%   
  mutate(     
    fixations = replace_na(fixations, 0),     
    fixations_per_char = replace_na(fixations_per_char, 0)   
  )  

# group by participant and current_word_length and calculate mean no fix per char 
fixations_per_char_words_by_participant <- fixations_all_words %>%   
  group_by(participant_id, current_word_length) %>%   
  summarise(mean_fix_per_char = mean(fixations_per_char))  

# group by word length and calculate mean fixation density 
fix_dens_by_word_length <- fixations_all_words %>%   
  group_by(current_word_length) %>%   
  summarise(mean_fix_density = mean(fixations_per_char), n_words = n())  

# fit model and calculate turning point

model_fix_per_char_quadratic <- lmer(   
  fixations_per_char ~ current_word_length + I(current_word_length^2) + (1|participant_id),   
  data = fixations_per_char_words)  

# calculate turning point 
coefs <- fixef(model_fix_per_char_quadratic) 
a <- coefs["I(current_word_length^2)"] 
b <- coefs["current_word_length"] 
turning_point <- -b / (2 * a) 
# turning point is when word length is 9.2 letters}
