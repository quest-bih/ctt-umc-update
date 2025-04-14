#----------------------------------------------------------------------------------------------------------------------
#
# The following script creates the sample of trials with contributing German university medical centers (UMC)
# from the clinicaltrials.gov registry. Due to size, the raw registry data 
# will be stored in Zenodo and downloaded into the local project via a separate script.
# 
# The script searches the AACT dataset for affiliations of the sponsor/PI/responsible party/facilities
# associated with the different UMCs (keywords are loaded from city_search_terms.csv). It also filters
# the relevant completion years and study status (Completed, Terminated, Suspended, Unknown status).
#
# The script saves a filtered version of the dataset, containing only relevant trials. 
# Please be
# aware that the filtered dataset still contains false positives (i.e. trials that were found with the
# keywords but that were not associated with the UMCs - e.g. when a communal hospital in Berlin was found
# by the keyword "Berlin"). 
# This sentence above does not hold true with the current regular expressions, we should rephrase
#All trial affiliations were checked during the manual publication search to
# remove false positives.
#
#----------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(here)
#----------------------------------------------------------------------------------------------------------------------
# Loading of AACT dataset
#----------------------------------------------------------------------------------------------------------------------

# the complete AACT was downloaded with the timestamp given and saved in the data/raw folder

# AACT_folder <- "C:/Datenablage/AACT/AACT_dataset_240927"
AACT_folder <- here("data", "raw", "AACT", "AACT_dataset_240927")
#AACT filenames that we need to load
AACT_dataset_names <- c("studies", "overall_officials", "sponsors", "responsible_parties",
                        "facilities", "interventions", "calculated_values", "id_information")

AACT_dataset_files <- file.path(AACT_folder, paste0(AACT_dataset_names, ".txt"))
AACT_datasets <- AACT_dataset_files |> 
  map(read_delim, delim = "|")
names(AACT_datasets) <- AACT_dataset_names


#----------------------------------------------------------------------------------------------------------------------
# Load search terms for the affiliations/cities
#----------------------------------------------------------------------------------------------------------------------

#different seach terms for each university medical center are stored loaded from this csv
city_search_terms <- readLines(here("data", "umc_search_terms", "city_search_terms.csv"), encoding = "UTF-8") |> 
  str_split(";")
cities <- city_search_terms |> map_chr(1)
city_search_terms <- city_search_terms  |> 
  map(\(x) paste0("\\b", x, "\\b", collapse = "|"))
names(city_search_terms) <- cities


#----------------------------------------------------------------------------------------------------------------------
#  search for studies affiliated with a german medical faculty and get NCTs of those studies
#----------------------------------------------------------------------------------------------------------------------

#we want to find the names of the different universities in the
#PI/sponsor/responsible_party/facilities columns
grep_fast <- function(pattern, x)
{
  return(which(str_detect(x, pattern)))
}

get_nct <- function(affil_indices, dataset)
{
  ncts <- dataset |> 
    slice(affil_indices) |> 
    select(nct_id)
  return(ncts[[1]])
}

city_grep <- function(dataset, colname, grep_terms)
{
  indices <- map(grep_terms, grep_fast, x=dataset[[colname]])
  city_ncts <- map(indices, get_nct, dataset=dataset)
  return(city_ncts)
}

city_grep_indices <- function(dataset, colname, grep_terms)
{
  indices <- map(grep_terms, grep_fast, x=dataset[[colname]])
  return(indices)
}

#search the different affilation datasets for the city search terms
grep_PI <- city_grep(AACT_datasets$overall_officials, "affiliation", city_search_terms)
grep_sponsor <- city_grep(AACT_datasets$sponsors |>
                            filter(lead_or_collaborator == "lead"), "name", city_search_terms)
grep_resp_party_org <- city_grep(AACT_datasets$responsible_parties, "organization", city_search_terms)
grep_resp_party_affil <- city_grep(AACT_datasets$responsible_parties, "affiliation", city_search_terms)

pi_umcs <- enframe(grep_PI, name = "umc", value = "nct_id") |> 
  unnest(nct_id)
sponsor_umcs <- enframe(grep_sponsor, name = "umc", value = "nct_id") |> 
  unnest(nct_id)
resp_party_org_umcs <- enframe(grep_resp_party_org, name = "umc", value = "nct_id") |> 
  unnest(nct_id)
qa_pi <- AACT_datasets$overall_officials |> 
  inner_join(pi_umcs, by = "nct_id") |> 
  select(umc, affiliation, nct_id, everything())
qa_sponsor <- AACT_datasets$sponsors |>
  filter(lead_or_collaborator == "lead") |> 
  inner_join(sponsor_umcs, by = "nct_id") |> 
  select(umc, name, nct_id, everything())
gmbh_sponsor <- qa_sponsor |> 
  filter(str_detect(name, "GmbH"))
qa_resp_org <- AACT_datasets$responsible_parties |> 
  inner_join(resp_party_org_umcs, by = "nct_id") |> 
  select(umc, organization, nct_id, everything())
gmbh_resp_org <- qa_resp_org |> 
  filter(str_detect(organization, "GmbH"))

#joining of the different grep results
affil_join <- function(affil_nct_list)
{
  affil_indices_joined <- affil_nct_list  |> 
    pmap(c) |> 
    map(unique) |> 
    map(sort)
}

#combine the results for the different columns to get the studies with a match for
#a lead (PI/sponsor/responsible_party) or facility affiliation
grep_results_lead <- list(grep_PI, grep_sponsor, grep_resp_party_org, grep_resp_party_affil)

#for each study we want to know which city has
#a lead (PI/sponsor/responsible_party) or facility affiliation or any affil
affil_ncts_lead <- affil_join(grep_results_lead)

#get the unique study IDs
unique_ncts_lead <- unique(unlist(affil_ncts_lead))

#----------------------------------------------------------------------------------------------------------------------
# reduce the CTgov dataset to those studies that are indeed affiliated
# and filter for lead completion years & study status
#----------------------------------------------------------------------------------------------------------------------

CTgov_sample <- AACT_datasets$studies

completion_years <- 2013:2019 |> 
  paste(collapse="|")
study_status <- c("COMPLETED" , "TERMINATED" , "SUSPENDED", "UNKNOWN") |> 
  paste(collapse="|")

#filter cases for affiliation, years, study status, and study type
CTgov_sample <- CTgov_sample |>
  filter(nct_id %in% unique_ncts_lead,
         grepl(completion_years, completion_date),
         grepl(study_status, overall_status),
         study_type == "INTERVENTIONAL")

qa_gmbh <- CTgov_sample |> 
  filter(nct_id %in% gmbh_sponsor$nct_id)
qa_gmbh_resp_org <- CTgov_sample |> 
  filter(nct_id %in% gmbh_resp_org$nct_id)

unique_gmbh <- gmbh_sponsor |> 
  distinct(name, .keep_all = TRUE) |> 
  bind_rows(gmbh_resp_org |> 
              distinct(name, .keep_all = TRUE))

#----------------------------------------------------------------------------------------------------------------------
# create for each study a list of affiliated cities and add to main table
#----------------------------------------------------------------------------------------------------------------------

get_city_per_NCT <- function(cities_nct_list, unique_ncts)
{
  cities_col <- vector("list", length(unique_ncts))
  names(cities_col) <- unique_ncts
  for (city in names(cities_nct_list)) {
    cities_col[cities_nct_list[[city]]] <-
      paste(cities_col[cities_nct_list[[city]]], city, sep = " ")
  }
  cities_col <- substring(cities_col, first = 6)
  names(cities_col) <- unique_ncts
  return(cities_col)
}

#create columns that list which cities are affiliated with the studies
nct_cities_lead <- get_city_per_NCT(affil_ncts_lead, unique_ncts_lead)

#prepare for joining with main table
nct_cities_lead_tbl <- tibble(nct_id = unique_ncts_lead, cities_lead = nct_cities_lead)

#add columns to main table
CTgov_sample <- CTgov_sample |>
  left_join(nct_cities_lead_tbl, by = "nct_id")



#add PI affil info to main table
#first get only affils of relevant PIs from full table
grep_PI_indices <- city_grep_indices(AACT_datasets$overall_officials, "affiliation", city_search_terms) |> 
  unlist() |> unique() |> sort() 
PI_affils_table_filtered <- AACT_datasets$overall_officials[grep_PI_indices,] |>
  distinct(nct_id, .keep_all = TRUE) #only take first relevant PI for each study to allow a clean join

CTgov_sample <- CTgov_sample |>
  left_join(PI_affils_table_filtered, by = "nct_id")

#add intervantion name
interventions_combined <- AACT_datasets$interventions |>
  group_by(nct_id) |>
  summarise(intervention_names_comb = paste(name, collapse=" | "))

CTgov_sample <- CTgov_sample |>
  left_join(interventions_combined, by = "nct_id")


#add calculated values
CTgov_sample <- CTgov_sample |>
  left_join(AACT_datasets$calculated_values, by = "nct_id")

CTgov_sample_save <- CTgov_sample |>
  rename(PI_name = name,
         PI_affiliation = affiliation,
         interventions = intervention_names_comb) |> 
  select(nct_id, cities_lead, brief_title, official_title,
         study_first_submitted_date, start_date, start_date_type,
         completion_date, completion_date_type, PI_name,
         PI_affiliation, interventions, overall_status,
         phase, enrollment, enrollment_type, were_results_reported)

#----------------------------------------------------------------------------------------------------------------------
# extract and clean secondary trns for drks, euctr, and aliases (secondary ctgov trns)
#----------------------------------------------------------------------------------------------------------------------

regexes <- get_registry_regex(c("DRKS", "ClinicalTrials.gov", "EudraCT"))
# drks_tib <- fromJSON(here("data", "raw", "DRKS_search_20250303.json"))

#drks_ids <- read_csv() 
drks_ids <- drks_tib$drksId

id_info <- AACT_datasets$id_information |>
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
             str_extract(regexes$ClinicalTrials.gov)
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
            ctgov_ids = paste(na.omit(ctgov_clean) |> unique(), collapse = ";"),
            drks_ids = paste(na.omit(drks_clean) |> unique(), collapse = ";"),
            euctr_ids = paste(na.omit(euctr_clean) |> unique(), collapse = ";"),
            id_sources = paste(na.omit(id_source) |> unique(), collapse = ";")
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

qa_aliases <- id_info |> 
  filter(!is.na(ctgov_clean), na.rm = TRUE) |> 
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

qa_aliases |> 
  count(id_sources) |> 
  mutate(prop = n / sum(n))

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

#save CT.gov trial sample
#please be aware that not all associations of the trials to the cites are correct (there are still false positives)
#such that the city associations had to be checked manually during publication search
write_excel_csv2(CTgov_sample_save, here("data", "processed", "CTgov_sample.csv"), na = "")

