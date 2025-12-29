library(readr)
library(dplyr)
library(stringr)


extract_cdc_meta <- function(file_path){

  print(file_path)

# 1. Read the whole file
lines <- suppressWarnings(readLines(file_path))

# 2. Find where the metadata ("---") starts
separator_line <- which(lines == "\"---\"")[1]
second_separator_line <- which(lines == "\"---\"")[2]

# 3. Split data section and metadata section
data_lines <- lines[1:(separator_line - 1)]
meta_lines <- lines[(separator_line + 1):second_separator_line - 1]

# 4. Read the main data table (first part) using read_csv
data <- readr::read_csv(paste(data_lines, collapse = "\n"))

# 5. Extract query parameters from the bottom section
# Remove quotes and blank lines
meta_lines_clean <- meta_lines |>
  str_replace_all("^\"|\"$", "") |>
  str_trim() |>
  discard(~ .x == "")

# Keep only lines that look like "Key: Value"
query_params <- meta_lines_clean[str_detect(meta_lines_clean, ":")]

# Split into key-value pairs
param_df <- tibble(raw = query_params) |>
  separate(raw, into = c("key", "value"), sep = ":", extra = "merge") |>
  mutate(across(everything(), str_trim))

# 6. Convert to named list
params_list <- setNames(param_df$value, make.names(param_df$key))

# 7. Add these as columns to the main data frame
data_with_meta <- data |>
  mutate(across(everything(), as.character)) |>  # make compatible
  mutate(!!!params_list)

data_with_meta <-
  data_with_meta |>
  rename_with(~ str_replace_all(.x, " ", "_")) |>
  rename_with(~ str_replace_all(.x, "\\.", "_")) |>
  rename_with(~ str_to_lower(.x))

data_with_meta <-
  data_with_meta |>
  mutate(single_race_6 = ifelse(is.na(single_race_6) &
                                  notes == "Total",
                                "Total",
                                single_race_6)
  )

data_with_meta <-
  data_with_meta |>
  select(-notes)

replace_icd10_names <-
  c(icd_10_codes = "icd_10_113_cause_list")

data_with_meta <-
  data_with_meta |>
  rename(any_of(replace_icd10_names))

return(data_with_meta)

}

cdc_files =
  paste0(here::here("data/cdc/underlying_cod_race_"),
         c("I20-I25.csv",
           "E11.2.csv",
           "E10.2.csv",
           "E11-E11.11_E11.3-E11.9.csv",
           "E10-E10.11_E10.3-E10.9.csv",
           "E10-E14(minus E10.2, E11.2, E12.2, E13.2, E14.2).csv",
           "A00-A01_A03-A04_A06-A09.csv",
           "A15-A19_B90.csv",
           "B20-B24.csv",
           "C00-C08.csv",
           "C09-C10_C12-C14.csv",
           "C11.csv",
           "C16.csv",
           "C22.csv",
           "C32.csv",
           "C33-C34.csv",
           "C53.csv",
           "J09-J22_P23_U04_U07.csv",
           "P00-P96(minus P23, P37.3, P37.4).csv",
           "I01_I02_I05-I09.csv",
           "I60-I60.9_I67.0-I67.1.csv",
           "I61-I62_I69.0-I69.2.csv",
           "J40-J44.csv",
           "K70_K74.csv",
           "O00-O99.csv")
         )

cdc_meta_data =
  lapply(cdc_files,
         extract_cdc_meta) |>
  bind_rows()


unique(cdc_meta_data$icd_10_codes)

cdc_meta_data |>
  group_by(icd_10_codes_group) |>
  filter(age_adjusted_rate != "Unreliable") |>
  mutate(age_adjusted_rate = as.numeric(age_adjusted_rate)) |>
  summarise(max = max(age_adjusted_rate),
            min = min(age_adjusted_rate),
            mean = mean(age_adjusted_rate),
            median = median(age_adjusted_rate),
            sd = sd(age_adjusted_rate),
            diff = max - min
            )

cdc_meta_data |>
  group_by(icd_10_codes) |>
  filter(age_adjusted_rate != "Unreliable") |>
  mutate(age_adjusted_rate = as.numeric(age_adjusted_rate)) |>
  slice_max(age_adjusted_rate) |>
  select(single_race_6, age_adjusted_rate)

cdc_meta_data |>
  group_by(icd_10_codes) |>
  filter(age_adjusted_rate != "Unreliable") |>
  mutate(age_adjusted_rate = as.numeric(age_adjusted_rate)) |>
  slice_min(age_adjusted_rate) |>
  select(single_race_6,
         age_adjusted_rate)

cdc_meta_data |>
  mutate(total_age_adjusted = ifelse(single_race_6 == "Total",
                                     age_adjusted_rate,
                                     NA)
         ) |>
  tidyr::fill(total_age_adjusted,
              .direction = "up") |>
  filter(single_race_6 != "Total") |>
  filter(age_adjusted_rate != "Unreliable") |>
  group_by(icd_10_codes) |>
  mutate(ratio_age_adjusted = as.numeric(age_adjusted_rate) / as.numeric(total_age_adjusted)) |>
  select(single_race_6,
         age_adjusted_rate,
         total_age_adjusted,
         ratio_age_adjusted)

cdc_meta_data  =
cdc_meta_data |>
  mutate(icd_10_codes = as.factor(icd_10_codes)) |>
  mutate(icd_10_codes_group = case_when(
    grepl("I20-I25", icd_10_codes) ~ "Ischemic heart disease",
    grepl("E10.2", icd_10_codes) ~ "Chronic kidney disease due to diabetes mellitus type 1",
    grepl("E11.2", icd_10_codes) ~ "Chronic kidney disease due to diabetes mellitus type 2",
    grepl("B20-B24", icd_10_codes) ~ "HIV/AIDS",
    grepl("A00", icd_10_codes) ~ "Diarrheal diseases",
    grepl("E14", icd_10_codes) ~ "Diabetes mellitus",
    grepl("E11.1", icd_10_codes) ~ "Diabetes mellitus type 2",
    grepl("E10.1", icd_10_codes) ~ "Diabetes mellitus type 1",
    grepl("A16", icd_10_codes) ~ "Tuberculosis",
    grepl("C00", icd_10_codes) ~ "Lip and oral cavity cancer",
    grepl("C09", icd_10_codes) ~ "Other pharynx cancer",
    grepl("C11", icd_10_codes) ~ "Nasopharynx cancer",
    grepl("C16", icd_10_codes) ~ "Stomach cancer",
    grepl("C22", icd_10_codes) ~ "Liver cancer",
    grepl("C32", icd_10_codes) ~ "Larynx cancer",
    grepl("C33", icd_10_codes) ~ "Tracheal, bronchus, and lung cancer",
    grepl("C53", icd_10_codes) ~ "Cervical cancer",
    grepl("J09", icd_10_codes) ~ "Lower respiratory infections",
    grepl("J40", icd_10_codes) ~ "Chronic obstructive pulmonary disease",
    grepl("K70", icd_10_codes) ~ "Liver cirrhosis",
    grepl("P20", icd_10_codes) ~ "Neonatal disorders",
    grepl("O00-O99", icd_10_codes) ~ "Maternal disorders",
    grepl("I01", icd_10_codes) ~ "Rheumatic heart disease",
    grepl("I60", icd_10_codes) ~ "Subarachnoid hemorrhage",
    grepl("I61", icd_10_codes) ~ "Intracerebral hemorrhage",
    TRUE ~ as.character(icd_10_codes)
  )
           )

cdc_meta_data |>
  filter(age_adjusted_rate != "Unreliable") |>
  mutate(age_adjusted_rate = as.numeric(age_adjusted_rate)) |>
  ggplot(aes(x = single_race_6, y = age_adjusted_rate)) +
  geom_bar(stat="identity") +
  facet_wrap(~icd_10_codes_group) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
