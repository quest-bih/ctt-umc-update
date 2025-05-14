##### Extracting and cleaning TRNs, cross-registration prep

library(tidyverse)
library(here)
library(yaml)
library(janitor)
library(furrr)
library(progressr)

#----------------------------------------------------------------------------------------------------------------------
# extract and clean secondary trns for drks, euctr, and aliases (secondary ctgov trns)
#----------------------------------------------------------------------------------------------------------------------

# AACT_folder <- "C:/Datenablage/AACT/AACT_dataset_240927"
AACT_folder <- here("data", "raw", "AACT", "AACT_dataset_250513")

id_info <- file.path(AACT_folder, "id_information.txt") |> 
  read_delim(delim = "|")

regexes <- get_registry_regex(c("DRKS", "ClinicalTrials.gov", "EudraCT"))

drks_ids <- read_csv(here("data", "raw", "drks_ids.csv")) |> 
  pull(drksId)

id_info <- id_info |>
  mutate(id_value = str_squish(id_value) |>
           str_remove_all("\\s"),
         drks_clean = case_when(
           str_detect(id_type_description, "DRKS") &
             !str_detect(id_value, "DRKS") ~ paste0("DRKS", id_value),
           
           .default = str_replace(id_value, "DRKSID", "DRKS") |>
             str_remove("DRKS-ID:") |>
             str_extract(regexes$DRKS)
         ),
         euctr_clean = case_when(
           id_type == "EUDRACT_NUMBER" ~ id_value,
           .default = id_value |>
             str_extract(regexes$EudraCT)
         ),
         ctgov_clean = case_when(
           id_source == "nct_alias" ~ id_value,
           # if the "secondary ID" just repeats the trial number return NA
           id_value |>
             str_extract(regexes$ClinicalTrials.gov) == nct_id ~ NA_character_,
           .default = id_value |>
             str_extract(regexes$ClinicalTrials.gov) |> 
             str_remove_all("[:punct:]")
         ),
         ctgov_exists = ctgov_clean %in% nct_id,
         drks_exists = drks_clean %in% drks_ids)

# these are all malformed trial ids:
qa_euctr <- id_info |>
  filter(is.na(euctr_clean),
         str_detect(id_value, "\\d-\\d"),
         !str_detect(id_value, "20\\d{2}-5"),
         # is.na(id_type_description) |
         str_detect(id_type_description, "EUDRA|Eudra|CTIS|EU(?!DAM)"))


id_info |> 
  mutate(has_ctgov = !is.na(ctgov_clean)) |> 
  count(id_source, has_ctgov, ctgov_exists)

###### so now a table of cross-regs from secondary ids:
id_crossreg <- id_info |>
  mutate(is_alias = id_source == "nct_alias") |>
  # filter(!is.na(ctgov_clean) | !is.na(drks_clean) | !is.na(euctr_clean)) |>
  group_by(nct_id) |>
  summarise(has_ctgov = any(!is.na(ctgov_clean), na.rm = TRUE),
            has_alias = any(is_alias, na.rm = TRUE),
            has_secondary_id = any(id_source == "secondary_id", na.rm = TRUE),
            has_org_study_id = any(id_source == "org_study_id", na.rm = TRUE),
            has_crossreg_drks = any(!is.na(drks_clean), na.rm = TRUE),
            has_crossreg_euctr = any(!is.na(euctr_clean), na.rm = TRUE),
            ctgov_ids = deduplicate_collapsed(ctgov_clean),
            drks_ids = deduplicate_collapsed(drks_clean),
            euctr_ids = deduplicate_collapsed(euctr_clean),
            id_sources = deduplicate_collapsed(id_source)
  ) |> 
  mutate(across(where(is.character), \(x) na_if(x, "")))

id_crossreg |>
  count(has_ctgov, has_crossreg_drks, has_crossreg_euctr) |> 
  mutate(prop = n / sum(n))

id_crossreg |>
  filter(has_ctgov) |> 
  count(has_alias, has_secondary_id, has_org_study_id) |> 
  mutate(prop = n / sum(n))

id_crossreg |> 
  count()

qa_crossreg <- id_crossreg |> 
  mutate(has_multiple_drks = str_detect(drks_ids, ";"),
         n_drks = str_count(drks_ids, ";") + 1,
         has_multiple_euctr = str_detect(euctr_ids, ";"),
         n_euctr = str_count(euctr_ids, ";") + 1,
         has_multiple_ctgov = str_detect(ctgov_ids, ";"),
         n_ctgov = str_count(ctgov_ids, ";") + 1) |> 
  # filter(has_crossreg_drks | has_crossreg_euctr | has_alias) |> 
  mutate(across(where(is.logical), \(x) ifelse(is.na(x), FALSE, x)))

qa_crossreg |> 
  count(has_multiple_drks)
qa_crossreg |> 
  count(has_multiple_euctr)
qa_crossreg |> 
  count(has_multiple_ctgov)

qa_aliases <- id_crossreg |> 
  filter(has_ctgov)

ctgov_aliases <- id_info |> 
  filter(
    # nct_id %in% inclusion_trns, # inclusion criteria from sample generation!
    !is.na(ctgov_clean)
    ) |> 
  mutate(is_alias = id_source == "nct_alias",
         alias_id_exists = ctgov_clean %in% id_info) |> 
  group_by(nct_id) |>
  summarise(has_ctgov = any(!is.na(ctgov_clean), na.rm = TRUE),
            has_alias = any(is_alias, na.rm = TRUE),
            has_secondary_id = any(id_source == "secondary_id", na.rm = TRUE),
            has_org_study_id = any(id_source == "org_study_id", na.rm = TRUE),
            has_crossreg_drks = any(!is.na(drks_clean), na.rm = TRUE),
            has_crossreg_euctr = any(!is.na(euctr_clean), na.rm = TRUE),
            ctgov_ids = paste(na.omit(ctgov_clean) |> unique(), collapse = ";"),
            drks_ids = paste(na.omit(drks_clean) |> unique(), collapse = ";"),
            euctr_ids = paste(na.omit(euctr_clean) |> unique(), collapse = ";"),
            id_sources = paste(na.omit(id_source) |> unique(), collapse = ";")
  ) |> 
  mutate(across(where(is.character), \(x) na_if(x, "")))

id_aliases <- ctgov_aliases |> 
  filter(has_alias == TRUE) |> 
  select(nct_id, alias = ctgov_ids) |> 
  mutate(alias = strsplit(alias, ";")) |> 
  unnest(alias) |> 
  write_excel_csv(here("data", "processed", "ctgov_aliases.csv"))

ctgov_aliases |> 
  count(id_sources) |> 
  mutate(prop = n / sum(n))

# the very few secondary ids that are not aliases seem to be other related studies,
# ignore for now, potentially verify as number is small,
# especially after applying inclusion criteria
qa_org_study <- ctgov_aliases |> 
  filter(id_sources == "org_study_id") |> 
  mutate(secondary_exists = ctgov_ids %in% id_info$nct_id) |> 
  filter(secondary_exists == FALSE)

id_info |> 
  filter(!is.na(ctgov_clean), na.rm = TRUE) |> 
  count(id_source, ctgov_exists) |> 
  mutate(prop = n / sum(n))

qa_trn_length <- id_info |> 
  filter(!is.na(ctgov_clean), na.rm = TRUE) |> 
  mutate(alias_id_exists = ctgov_clean %in% id_info$nct_id,
         trn_too_long = str_detect(id_value, "NCT\\d{9,}"))

qa_trn_length |> 
  count(id_source, trn_too_long, alias_id_exists)


# TODO: include secondary trn info into CTgov_sample before final export