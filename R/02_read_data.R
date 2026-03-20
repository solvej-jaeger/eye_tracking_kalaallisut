# read clean fix report
fix_report <- fread("data/fix_report_clean.tsv")

# read clean reading-measures files
df_affix <- fread("data/rm_target_affix_clean.tsv")
df_baseline <- fread("data/rm_baseline_clean.tsv")
df_root <- fread("data/rm_root_clean.tsv")
df_fill <- fread("data/rm_fill_clean.tsv")
df_inflection <- fread("data/rm_inflection_clean.tsv")

# read demographics
df_demographics <- na.omit(readr::read_delim("data/demographics.csv", delim = ";", locale = readr::locale(encoding = "UTF-8")))

