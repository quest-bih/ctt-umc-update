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
source(here("scripts", "utils.R"))

regexes <- get_registry_regex(c("DRKS", "ClinicalTrials.gov", "EudraCT"))

euctr_combined <- readRDS(here("data", "raw", "euctr_combined.rds"))

drks_ids <- read_csv(here("data", "raw", "drks_ids.csv")) |> 
  pull(drksId)

handlers(global = TRUE)
plan(multisession)

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
           str_detect(id_value, regexes$EudraCT) &
             !str_detect(str_extract(id_value, regexes$EudraCT), "[:punct:]") ~
             str_extract(id_value, regexes$EudraCT) |> 
             (\(x) paste0(str_sub(x, 1, 4),
                    "-",
                    str_sub(x, 5, 10),
                    "-",
                    str_sub(x, 11, 13)))(),
           .default = str_extract(id_value, regexes$EudraCT)
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
         drks_exists = drks_clean %in% drks_ids, # non-exisent ones were almost all removed dupes
         # with the exception of this one DRKS00005436
         euctr_exists = euctr_clean %in% euctr_combined$eudract_number)

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


ctgov_aliases <- id_info |> 
  filter(
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
            ctgov_ids = deduplicate_collapsed(ctgov_clean),
            drks_ids = deduplicate_collapsed(drks_clean),
            euctr_ids = deduplicate_collapsed(euctr_clean),
            id_sources = deduplicate_collapsed(id_source)
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


crossreg_euctr_drks <- read_csv(here("data", "processed", "crossreg_euctr_drks.csv"))
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
            ctgov_all = deduplicate_collapsed(ctgov_clean),
            drks_all = deduplicate_collapsed(drks_clean),
            euctr_all = deduplicate_collapsed(euctr_clean),
            id_sources = deduplicate_collapsed(id_source)
  ) |> 
  mutate(across(where(is.character), \(x) na_if(x, "")))


crossreg_ctgov <- id_info |> 
  filter(euctr_exists | drks_exists | ctgov_exists) |> 
  group_by(nct_id) |>
  mutate(ctgov_all = deduplicate_collapsed(ctgov_clean),
         drks_all = deduplicate_collapsed(drks_clean),
         euctr_all = deduplicate_collapsed(euctr_clean),
         has_crossreg_drks = any(drks_exists, na.rm = TRUE),
         has_crossreg_euctr = any(euctr_exists, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(across(where(is.character), \(x) na_if(x, ""))) |> 
  pivot_longer(cols = contains("_all"), values_to = "linked_id") |> 
  filter(linked_id != "", !is.na(linked_id)) |> 
  mutate(linked_id = na_if(linked_id, ""),
         triad = has_crossreg_drks & has_crossreg_euctr,
         many_to_many = str_detect(linked_id, ";|NCT"),
         via_id = TRUE, bidirectional = NA) |> 
  select(trial_id = nct_id, linked_id, triad, many_to_many, via_id, bidirectional) |> 
  rowwise() |> 
  mutate(binary_id = case_when(
           str_detect(linked_id, "NCT") ~ paste(c(trial_id, linked_id), collapse = "_"),
           .default = paste(c(linked_id, trial_id), collapse = "_")
           ), .before = 1) |> 
  ungroup()

crossreg_ctgov |> 
  write_excel_csv(here("data", "processed", "crossreg_ctgov.csv"))

crossreg_euctr_drks <- read_csv(here("data", "processed", "crossreg_euctr_drks.csv")) 

bidirectional_euctr_drks <- crossreg_euctr_drks |> 
  semi_join(crossreg_ctgov, by = "binary_id") |> 
  pull(binary_id)

new_crossregs_ctgov <- crossreg_ctgov |> 
  filter(!binary_id %in% bidirectional_euctr_drks) |> 
  mutate(bidirectional = FALSE)

crossreg_euctr_drks_ctgov <- crossreg_euctr_drks |>
  bind_rows(new_crossregs_ctgov) |> 
  mutate(bidirectional = case_when(
    binary_id %in% bidirectional_euctr_drks ~ TRUE,
    str_detect(linked_id, "NCT") ~ FALSE,
    .default = bidirectional
  )) |> 
  distinct(trial_id, linked_id, .keep_all = TRUE) |> 
  group_by(linked_id) |> 
  mutate(triad = case_when(
    str_detect(linked_id, "-") ~ any(str_detect(trial_id, "DRKS")) &
      any(str_detect(trial_id, "NCT")),
    str_detect(linked_id, "DRKS") ~ any(str_detect(trial_id, "-")) &
      any(str_detect(trial_id, "NCT")),
    str_detect(linked_id, "NCT") ~ any(str_detect(trial_id, "-")) &
      any(str_detect(trial_id, "DRKS")),
    .default = triad
  ),
  many_to_many = ifelse(any(many_to_many == TRUE), TRUE, n() > 1 & triad == FALSE)) |> 
  group_by(trial_id) |> 
  mutate(triad = case_when(
    any(triad == TRUE) ~ TRUE,
    str_detect(trial_id, "-") ~ any(str_detect(linked_id, "DRKS")) &
      any(str_detect(linked_id, "NCT")),
    str_detect(trial_id, "DRKS") ~ any(str_detect(linked_id, "-")) &
      any(str_detect(linked_id, "NCT")),
    str_detect(trial_id, "NCT") ~ any(str_detect(linked_id, "-")) &
      any(str_detect(linked_id, "DRKS")),
    .default = triad
  ),
  many_to_many = ifelse(any(many_to_many == TRUE), TRUE, n() > 1 & triad == FALSE)) |> 
  ungroup()

assertthat::are_equal(crossreg_euctr_drks_ctgov |> 
                        filter(is.na(bidirectional)) |> 
                        nrow(), 0) 

assertthat::are_equal(crossreg_euctr_drks_ctgov |>
  get_dupes(trial_id) |> 
  filter(triad == FALSE,
         many_to_many == FALSE) |> 
    nrow(), 0)

assertthat::are_equal(crossreg_euctr_drks_ctgov |>
                        get_dupes(linked_id) |> 
                        filter(triad == FALSE,
                               many_to_many == FALSE) |> 
                        nrow(), 0)

# 
# dupes_trial <- crossreg_euctr_drks_ctgov |> 
#   get_dupes(trial_id)

crossreg_euctr_drks_ctgov |> 
  write_excel_csv(here("data", "processed", "crossreg_euctr_drks_ctgov.csv"))
