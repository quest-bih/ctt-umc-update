#----------------------------------------------------------------------------------------------------------------------
#
# The following script creates the sample of trials with contributing German university medical centers (UMC)
# from the DRKS database. Due to size, the raw registry data is stored in Zenodo and downloaded into the local project via a separate script.
#
# The script searches the DRKS dataset for affiliations of the sponsor/PI/responsible party/recruitment locations
# associated with the different UMCs (keywords are loaded from city_search_terms.csv).
#
# The script saves a filtered version of the dataset, containing only relevant trials. Please be
# aware that the filtered dataset still contains false positives (i.e. trials that were found with the
# keywords but that were not associated with the UMCs - e.g. when a communal hospital in Berlin was found
# by the keyword "Berlin"). All trial affiliations were checked during the manual publication search to
# remove false positives.
#
#----------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(furrr)
library(progressr)
library(jsonlite)
library(yaml)
library(readxl)

#----------------------------------------------------------------------------------------------------------------------
# Data loading and transformation
#----------------------------------------------------------------------------------------------------------------------

# Get registry data if not already downloaded/unzipped
# source(here::here("code", "0_get_registry_data.R"))

# DRKS_sample <- read_csv2(here::here("data", "raw", "2020-06-03_drks.csv"))
# DRKS_full <- read_json(here("data", "raw", "DRKS_search_20250303.json"))

drks_tib <- fromJSON(here("data", "raw", "DRKS_search_20250513.json"))

# save the existent DRKS TRNs for easier existence and cross-reference checks later
drks_tib |> 
  select(drksId) |> 
  write_excel_csv(here("data", "raw", "drks_ids.csv"))

source(here("scripts", "utils.R"))
# regexes <- yaml::read_yaml(here("inst", "extdata", "keywords_patterns.yaml"))
regexes <- get_registry_regex(c("DRKS", "ClinicalTrials.gov", "EudraCT"))

drks_material_support <- drks_tib |> 
  select(drksId, materialSupports) |> 
  unnest(materialSupports) |> 
  unnest(contact) |> 
  mutate(across(where(is.character), \(x) na_if(x, "")))

drks_sponsors <- drks_tib |> 
  select(drksId, trialContacts) |> 
  unnest(trialContacts) |> 
  unnest(c(idContactIdType, contact))

drks_trial_contacts <- drks_tib |> 
  select(drksId, trialContacts) |> 
  unnest(trialContacts) |> 
  unnest(c(contact, idContactIdType)) |> 
  mutate(across(where(is.character), \(x) na_if(x, "")))

drks_recruitment_umcs <- drks_tib |> 
  select(drksId, recruitment) |> 
  unnest(recruitment) |> 
  unnest(countries) |> 
  mutate(country_code = unlist(idCountry)) |> 
  unnest(institutes) |> 
  filter(type == "UNI_MEDICAL_CENTER") |> 
  mutate(across(where(is.character), \(x) na_if(x, ""))) |> 
  select(drksId, country_code, name, status, actualCompletionDate)

################ UMC fields to search according to protocol

umc_search_terms <- get_umc_terms()

plan(multisession)
handlers(global = TRUE)
### 1. Trial contacts

umc_drks_sponsors <- drks_trial_contacts |> 
  filter(type == "PRIMARY_SPONSOR",
    str_detect(city, umc_search_terms) |
      str_detect(affiliation, umc_search_terms)) |> 
  unite("raw_affil", c(affiliation, city), sep = ", ") |> 
  mutate(field = "primary_sponsor_affil_city",
         raw_affil = str_squish(raw_affil))

umc_drks_pcis <- drks_trial_contacts |> 
  filter(type == "PRINCIPAL_COORDINATING_INVESTIGATOR" |
           otherType == "OTHER_PRINCIPAL_COORDINATING_INVESTIGATOR",
         str_detect(city, umc_search_terms) |
           str_detect(affiliation, umc_search_terms)) |> 
  unite("raw_affil", c(affiliation, city), sep = ", ") |> 
  mutate(field = "pci_affil_city",
         raw_affil = str_squish(raw_affil))

drks_study_characteristic <- drks_tib |> 
  select(drksId, studyCharacteristic) |> 
  unnest(studyCharacteristic)

drks_interventional <- drks_tib |> 
  select(drksId, studyCharacteristic) |> 
  unnest(studyCharacteristic) |> 
  mutate(is_interventional = type == "INTERVENTIONAL") |> 
  select(drksId, is_interventional)

drks_recruitment <- drks_tib |> 
  select(drksId, recruitment, registration_date = registrationDrks,
         last_updated = lastUpdate) |> 
  unnest(recruitment)

drks_2018_2021 <- drks_recruitment |> 
  filter(between(as_date(actualCompletionDate), as_date("2018-01-01"), as_date("2021-12-31"))) |> 
  pull(drksId)

validation_umcs_drks <- umc_drks_sponsors |> 
  bind_rows(umc_drks_pcis) |>
  filter(drksId %in% (drks_interventional |> filter(is_interventional) |> pull(drksId)), # apply interventional and time filter here
         drksId %in% drks_2018_2021) |> 
  mutate(umc = which_umcs(raw_affil),
         validation = NA,
         search_needed = NA,
         correction = NA,
         comments = "") |> 
  select(id = drksId, umc, raw_affil, field, validation, search_needed, correction, comments)

validation_umcs_drks_deduplicated <- validation_umcs_drks |> 
  group_by(raw_affil, umc) |> 
  summarise(across(everything(), first),
            n = n()) |>
  ungroup() |> 
  arrange(umc, desc(n)) |> 
  relocate(id, .before = everything()) |> 
  relocate(n, .before = validation)

validation_umcs_drks_deduplicated |>
  write_excel_csv(here("data", "processed", "validation_umcs_drks.csv"))

combined_validations <- validation_umcs_ctgov_deduplicated |> 
  bind_rows(validation_umcs_drks_deduplicated) |> 
  arrange(umc, desc(n))
combined_validations |> 
  write_excel_csv(here("data", "processed", "validation_umcs_ctgov_drks.csv"))

qa_umc_terms <- drks_sponsors |> 
  filter(str_detect(affiliation, umc_search_terms),
         type %in% c("PRIMARY_SPONSOR", "PRINCIPAL_COORDINATING_INVESTIGATOR",
                     "SECONDARY_SPONSOR"),
         str_detect(affiliation, " GmbH"),
         str_detect(affiliation, " Uni")) |> # space to exclude gGmbH
  distinct(affiliation, .keep_all = TRUE)


drks_recruitment_vs_sponsor_affil <- drks_recruitment_umcs |> 
  filter(str_detect(name, umc_search_terms)) |> 
  left_join(drks_trial_contacts |> 
              filter(type %in% c("PRIMARY_SPONSOR", "PRINCIPAL_COORDINATING_INVESTIGATOR") |
                       otherType == "OTHER_SECONDARY_SPONSOR",
                     str_detect(affiliation, umc_search_terms) |
                       str_detect(city, umc_search_terms))) |> 
  filter(country_code == "DE", is.na(affiliation))

drks_recruitment_vs_sponsor_affil |> 
  distinct(drksId) |> 
  nrow()


qa_city <- drks_trial_contacts |> 
  filter(type %in% c("PRIMARY_SPONSOR", "PRINCIPAL_COORDINATING_INVESTIGATOR") |
           otherType == "OTHER_SECONDARY_SPONSOR",
         !str_detect(affiliation, umc_search_terms),
         str_detect(city, umc_search_terms))

# add validated umc info

umc_validations_1 <- read_xlsx(here("data", "raw", "affil_review_extract_df_20250627.xlsx")) |> 
  select(id, raw_affil, umc, validation, correction, n)
umc_validations_2 <- read_xlsx(here("data", "raw", "affil_review_extract_mmp_20250627.xlsx")) |> 
  select(id, raw_affil, umc, validation, correction, n)

umc_validations <- umc_validations_1 |> 
  bind_rows(umc_validations_2) |> 
  rename(umc_estimated = umc) |> 
  mutate(raw_affil = str_squish(raw_affil),
    umc = case_when(
    validation != 1 ~ "false positive",
    correction != "" ~ correction,
    .default = umc_estimated
  ))

# prop umc false positives from the unique affiliations ~ 
umc_validations |> 
  count(umc == "false positive")  |> 
  mutate(prop = n / sum(n))
# prop umc false positives from total affiliations ~ approx 1/3

qa_cases |> 
  count(umc) |> 
  mutate(prop = n / sum(n))

umc_validations |> 
  write_csv(here("data", "processed", "umc_validations.csv"))

umc_validations <- read_csv(here("data", "processed", "umc_validations.csv"))

validated_umc_drks <- umc_validations |> 
  filter(str_detect(id, "DRKS")) |>
  select(raw_affil, umc)

validated_umc_drks <- umc_drks_sponsors |> 
  bind_rows(umc_drks_pcis) |>
  left_join(validated_umc_drks, by = "raw_affil", relationship = "many-to-many") |> 
  select(drksId, contains("type"), raw_affil, umc, everything())

validated_umc_drks_deduplicated <- validated_umc_drks |>  
  filter(umc != "false positive") |> 
  mutate(type = case_when(
    type == "PRIMARY_SPONSOR" ~ "umc_sponsor",
    otherType == "OTHER_PRINCIPAL_COORDINATING_INVESTIGATOR" |
      type == "PRINCIPAL_COORDINATING_INVESTIGATOR" ~ "umc_pi",
    .default = NA
  )) |> 
  group_by(drksId, type) |> 
  summarise(umc = deduplicate_collapsed(umc)) |> 
  pivot_wider(id_cols = drksId, names_from = type, values_from = umc) |>
  mutate(umc = deduplicate_collapsed(c(umc_pi, umc_sponsor)))

validated_exclusions_drks <- validated_umc_drks |>  
  filter(umc == "false positive", 
         !drksId %in% validated_umc_drks_deduplicated$drksId,
         drksId %in% drks_interventional$drksId,
         drksId %in% drks_2018_2021) |> 
  mutate(type = case_when(
           type == "PRIMARY_SPONSOR" ~ "umc_sponsor",
           otherType == "OTHER_PRINCIPAL_COORDINATING_INVESTIGATOR" |
             type == "PRINCIPAL_COORDINATING_INVESTIGATOR" ~ "umc_pi",
           .default = NA
         ))
validated_exclusions_drks |> 
  write_csv(here("data", "processed", "validated_exclusions_drks.csv"))

qa_validated_umc_drks <- validated_umc_drks |>  
  filter(!is.na(umc)) |> 
  group_by(drksId) |> 
  summarise(validated_affils = case_when(
    all(umc == "false positive", na.rm = TRUE) ~ "falsely included",
    any(umc == "false positive", na.rm = TRUE) ~ "falsely assigned",
    .default = deduplicate_collapsed(umc)
  )) |> 
  ungroup()

qa_validated_umc_drks |> 
  filter(drksId %in% drks_interventional$drksId, # apply interventional and time filter here
         drksId %in% drks_2018_2021) |> 
  count(validated_affils, sort = TRUE) |>
  mutate(total = sum(n),
         prop = n / total) |> 
  filter(str_detect(validated_affils, "false"))


drks_inex <- drks_recruitment |>
  left_join(drks_interventional, by = "drksId") |> 
  mutate(is_completed_2018_2021 = between(as_date(actualCompletionDate),
                                          as_date("2018-01-01"), as_date("2021-12-31")),
         is_german_umc = drksId %in% validated_umc_drks_deduplicated$drksId) |> 
  select(drksId, status, last_updated, registration_date, is_interventional, 
         is_completed_2018_2021, is_german_umc, completion_date = actualCompletionDate, estimated_completion_date = scheduledCompletionDate)
drks_inex |> filter(is_completed_2018_2021, is_german_umc, is_interventional) |> count(status)
drks_inex |> 
  rename(trial_id = drksId) |> 
  write_excel_csv(here("data", "processed", "inclusion_exclusion_drks.csv"))

DRKS_sample_save <- drks_tib |> 
  left_join(drks_inex, by = "drksId") |> 
  filter(is_interventional == TRUE, # apply interventional and time filter here
         is_completed_2018_2021 == TRUE,
         is_german_umc == TRUE) |> 
  select(drksId:url) |> 
  left_join(validated_umc_drks_deduplicated, by = "drksId")

DRKS_sample_save |> 
  summarise(total = n(),
            n_umc_sponsor = sum(!is.na(umc_sponsor)),
            n_umc_pi = sum(!is.na(umc_pi))) |>
  pivot_longer(-total, names_to = "definition", values_to = "n") |> 
  mutate(definition = str_remove(definition, "n_"),
         prop = round(n / total, 2)) |> 
  select(definition, everything())
  

write_excel_csv(DRKS_sample_save, here("data", "processed", "DRKS_sample.csv"), na = "")
# DRKS_sample <- read_csv(here("data", "processed", "DRKS_sample.csv"))

qa_excluded <- drks_tib |> 
  filter(drksId %in% drks_interventional$drksId, # apply interventional and time filter here
         drksId %in% drks_2018_2021,
         !drksId %in% validated_umc_drks_deduplicated$drksId) |> 
  select(drksId, trialContacts) |> 
  # unnest(trialContacts) |> 
  left_join(validated_umc_drks |> select(drksId, raw_affil, umc), by = "drksId") |> 
  # filter(!is.na(umc)) |> 
  unnest(trialContacts) |> 
  unnest(contact)

qa_na_excluded <- qa_excluded |> 
  filter(is.na(umc),
         str_detect(city, umc_search_terms) |
           str_detect(affiliation, umc_search_terms))

drks_results |> 
  filter(drksId == "DRKS00007181")


drks_results <- drks_tib |> 
  select(drksId, trialResults) |> 
  unnest(trialResults) |> 
  select(drksId, publications, trialResultsDescriptions) |> 
  unnest(publications) |> 
  unnest(trialResultsDescriptions) |> 
  unnest(idLocale)

drks_results_reporting <- drks_results |> 
  group_by(drksId) |> 
  summarise(results_reporting = 
              any(str_detect(description, "Ergebnisbericht|Abschlussbericht|(?<!keine )Studienergebnisse|Studienergebnisbericht|study results"),
                  na.rm = TRUE)) |> 
  select(trial_id = drksId, everything())

drks_results |> 
  ungroup() |> 
  count(type, sort = TRUE)

############# prepare export with crossreg data and additional transparency practices measures
validated_crossreg_ids <- read_csv(here("data", "processed", "crossreg_ids.csv"))
drks_export <- read_csv(here("data", "processed", "DRKS_sample.csv"))

drks_recruitment_dates <- drks_recruitment |> 
  select(trial_id = drksId, contains("actual"), status)

drks_export <- drks_export |> 
  rename(trial_id = drksId) |> 
  left_join(validated_crossreg_ids, by = "trial_id") |> 
  left_join(drks_recruitment_dates, by = "trial_id") |> 
  left_join(drks_results_reporting, by = "trial_id") |> 
  mutate(across(contains("Date"), ymd),
    is_prospective =
           (floor_date(registrationDrks, unit = "month") <=
              floor_date(actualStartDate, unit = "month")),
    results_reporting = replace_na(results_reporting, FALSE)) |> 
  select(trial_id, contains("umc"), contains("Date"),
         results_reporting, last_updated = lastUpdate, status)

write_excel_csv(drks_export, here("data", "processed", "DRKS_sample.csv"), na = "")

drks_recruitment |> 
  left_join(drks_inex) |> 
  filter(is_interventional,
         drksId %in% umc_drks_sponsors$drksId | drksId %in% umc_drks_pcis$drksId,
         !is.na(actualCompletionDate),
         !is.na(scheduledCompletionDate),
         between(as_date(scheduledCompletionDate), as_date("2018-01-01"), as_date("2021-12-31"))) |> 
  # filter(is_interventional, is_german_umc) |> 
  distinct(drksId, .keep_all = TRUE) |>
  count(is.na(actualCompletionDate), is.na(scheduledCompletionDate))


