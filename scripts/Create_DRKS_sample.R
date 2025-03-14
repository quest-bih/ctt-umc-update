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
library(jsonlite)
library(yaml)

#----------------------------------------------------------------------------------------------------------------------
# Data loading and transformation
#----------------------------------------------------------------------------------------------------------------------

# Get registry data if not already downloaded/unzipped
# source(here::here("code", "0_get_registry_data.R"))

# DRKS_sample <- read_csv2(here::here("data", "raw", "2020-06-03_drks.csv"))
# DRKS_full <- read_json(here("data", "raw", "DRKS_search_20250303.json"))

drks_tib <- fromJSON(here("data", "raw", "DRKS_search_20250303.json"))

regexes <- yaml::read_yaml(here("inst", "extdata", "keywords_patterns.yaml"))

drks_sponsors <- drks_tib |> 
  select(drksId, materialSupports) |> 
  unnest(materialSupports) |> 
  unnest(contact) |> 
  mutate(across(where(is.character), \(x) na_if(x, "")))

drks_affils <- drks_tib |> 
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

drks_secondary_ids <- drks_tib |> 
  select(drksId, secondaryIds) |> 
  unnest(secondaryIds) |> 
  mutate(across(where(is.character), \(x) na_if(x, "") |> 
                  str_remove_all("\\s") |> 
                  na_if("-"))) |> 
  mutate(otherPrimaryRegisterName = case_when(
    str_detect(otherPrimaryRegisterId, "(?<!S)NCT") ~ "ClinicalTrials.gov",
    str_detect(otherPrimaryRegisterId, regexes$euctr) ~ "EUCTR",
    str_detect(otherPrimaryRegisterId, regexes$drks) ~ "DRKS",
    .default = otherPrimaryRegisterName
  ),
  euctr_clean = case_when(
    !is.na(eudraCtNumber) ~ eudraCtNumber,
    .default = otherPrimaryRegisterId |>
      str_extract(regexes$euctr) 
  ),
  ctgov_clean = otherPrimaryRegisterId |>
    str_extract(regexes$ctgov),
  drks_clean = case_when(
    # if the "secondary ID" just repeats the trial number return NA
    otherPrimaryRegisterId |>
      str_extract(regexes$drks) == drksId ~ NA_character_,
    .default = otherPrimaryRegisterId |>
      str_extract(regexes$drks)
  ),
  has_alias = !is.na(drks_clean))

drks_other_secondary_ids <- drks_tib |>
  select(drksId, otherSecondaryIds) |> 
  unnest(otherSecondaryIds) |> 
  rename(secondaryId = value, secondaryRegister = description) |> 
  filter(!is.na(secondaryId),
         str_length(secondaryId) > 1) |> 
  mutate(euctr_clean2 = secondaryId |>
      str_extract(regexes$euctr),
         ctgov_clean2 = secondaryId |>
           str_extract(regexes$ctgov),
         drks_clean2 = case_when(
           # if the "secondary ID" just repeats the trial number return NA
           secondaryId |>
             str_extract(regexes$drks) == drksId ~ NA_character_,
           .default = secondaryId |>
             str_extract(regexes$drks)
         ),
         has_alias2 = !is.na(drks_clean2),
         # drks2_exists = drks_clean2 %in% drks_tib$drksId,
         secondaryRegisterName = case_when(
           str_detect(secondaryId, regexes$ctgov) ~ "ClinicalTrials.gov",
           str_detect(secondaryId, regexes$euctr) ~ "EUCTR",
           str_detect(secondaryId, regexes$drks) ~ "DRKS",
           .default = secondaryRegister
         ))


drks_secondary_ids <- drks_secondary_ids |> 
  left_join(drks_other_secondary_ids |> 
              filter(!is.na(ctgov_clean2) | !is.na(drks_clean2) | !is.na(euctr_clean2)), by = "drksId") |> 
  mutate(euctr_clean2 = if_else(euctr_clean2 == euctr_clean, NA_character_, euctr_clean2),
         ctgov_clean2 = if_else(ctgov_clean2 == ctgov_clean, NA_character_, ctgov_clean2),
         drks_alias_exists = drks_clean %in% drksId)

drks_secondary_ids |> 
  filter(euctr_clean != euctr_clean2) |> 
  select(drksId, contains("euctr"))

drks_secondary_ids |> 
  # filter(drks_clean != drksId) |> 
  mutate(has_any_euctr = !is.na(euctr_clean)  | !is.na(euctr_clean2),
         has_any_ctgov = !is.na(ctgov_clean) | !is.na(ctgov_clean2),
         has_any_alias = !is.na(drks_clean) | !is.na(drks_clean2),
         has_multiple_euctr = !is.na(euctr_clean) & !is.na(euctr_clean2),
         has_multiple_ctgov = !is.na(ctgov_clean) & !is.na(ctgov_clean2)) |> 
  count(has_any_alias) |> 
  mutate(prop = n / sum(n))



qa_euctr <- drks_secondary_ids |> 
  mutate(extracted_euctr = str_extract(otherPrimaryRegisterId, regexes$euctr)) |> 
  select(drksId, otherPrimaryRegisterId, extracted_euctr, eudraCtNumber, otherPrimaryRegisterName) |> 
  filter(str_detect(otherPrimaryRegisterId, regexes$euctr),
         eudraCtNumber != extracted_euctr)

### All of these extracted euctr do not resolve! 

qa_ctgov <- drks_secondary_ids |> 
  mutate(extracted_ctgov = coalesce(str_extract(otherPrimaryRegisterId, regexes$ctgov),
                                    str_extract(otherPrimaryRegisterName, regexes$ctgov))) |> 
  select(drksId, otherPrimaryRegisterId, extracted_ctgov, otherPrimaryRegisterName) |> 
  filter(extracted_ctgov != otherPrimaryRegisterId)

### clean extraction of trns achieved for ctgov

##### now clean secondary id tibble

drks_secondary_ids <- drks_secondary_ids |> 
  mutate(ctgovNumber = coalesce(str_extract(otherPrimaryRegisterId, regexes$ctgov),
                                    str_extract(otherPrimaryRegisterName, regexes$ctgov))) |> 
  select(drksId, ctgovNumber, euctrNumber = eudraCtNumber) |> 
  filter(!is.na(ctgovNumber) | !is.na(euctrNumber))


# cross-registrations from DRKS to the two other registries (in the full DRKS registry!):
drks_secondary_ids |> 
  mutate(has_ctgovNumber = !is.na(ctgovNumber),
         has_euctrNumber = !is.na(euctrNumber)) |> 
  count(has_ctgovNumber, has_euctrNumber) |> 
  mutate(prop = n / nrow(drks_tib))

### 444 have euctr, 206 have ctgov, 129 have both

# drks_tib_filtered <- drks_tib |> 
#   arrange(drksId) |>
#   rowwise() |> 
#   mutate(recruitment_status = pluck(recruitment, "status"),
#          recruitment_countries = pluck(recruitment, "countries") |>
#            map_chr(\(x) unlist(x) |> paste(collapse = ";")),
#          has_umc_info = pluck(recruitment, "institutes") |>
#            map_lgl(\(x) nrow(x) > 0),
#          umcs_institutes = ifelse(has_umc_info == FALSE, "",
#                        pluck(recruitment, "institutes") |>
#                          map_chr(\(x) x |>
#                                    filter(type == "UNI_MEDICAL_CENTER") |> 
#                                    pull(city) |> 
#                                    paste(collapse = ";")))) |> 
#   filter(grepl(completion_years, pluck(recruitment, "actualCompletionDate"))) |> 
#   # mutate(recruitment_status = pluck(recruitment, "status")) |> 
#   filter(pluck(recruitment, "status") %in% c("COMPLETE_FOLLOW_UP_COMPLETE", "DISCONTINUED",
#                                              "SUSPENDED", "NA"))
  

  
# NOTE: Warning appears when reading in `DRKS_sample`, which seems to have some misaligned cells
# Run `problems(DRKS_sample)` to see problems listed
# Note: `DRKS_sample |> filter(drksId == "DRKS00008870") |> pull(studyEnd)` is not corrupted data but rather an error on the registry: https://drks.de/search/en/trial/DRKS00008870

completion_years <- 2013:2019 |> 
  paste(collapse="|")

#### updated until here (old code that needs to be updated follows)

# DRKS_sample <- DRKS_sample |>
DRKS_sample <- drks_recruitment_umcs |> 
  arrange(drksId) |>
  filter(grepl(completion_years, actualCompletionDate))
names(DRKS_sample)
drks_recruitment_umcs |> filter(drksId == "DRKS00008870") |> pull(actualCompletionDate)
#----------------------------------------------------------------------------------------------------------------------
# Affiliation search
#----------------------------------------------------------------------------------------------------------------------

get_drks_id <- function(affil_indices, dataset)
{
  drks_id <- dataset |>
    slice(affil_indices) |>
    select(drksId)
  return(drks_id[[1]])
}

#affiliation columns used for the search
affil_columns <- paste0("address.affiliation",0:4)

#affiliations of different columns pasted together to simplify search
affiliations <- apply(DRKS_sample[affil_columns], 1, paste, collapse = " ")

# Load search terms for the affiliations/cities
city_search_terms <- readLines(here("data", "umc_search_terms", "city_search_terms.csv"), encoding = "UTF-8") |>
  str_split(";")
cities <- city_search_terms |> map_chr(1)
city_search_terms <- city_search_terms |>
  map(function(x) paste0("\\b", x, "\\b", collapse = "|"))
names(city_search_terms) <- cities


#actual search
affil_grep_idx <- map(city_search_terms, grep, x=affiliations)
affil_grep <- map(affil_grep_idx, get_drks_id, dataset=DRKS_sample)


drks_affils_res <- drks_affils |> 
  mutate(umc = map_chr(affiliation, \(affil) names(city_search_terms)
                       [map_lgl(city_search_terms, \(x) str_detect(affil, x))] |> 
                         paste(collapse = ";")))
    
#affiliation <- drks_affils$affiliation[21]
affiliation <- drks_affils$affiliation[1]
map_chr(affiliation, \(affil) names(city_search_terms)
        [map_lgl(city_search_terms, \(x) str_detect(affil, x))] |> 
          paste(collapse = ";"))

names(city_search_terms)[]
city_search_terms[names(city_search_terms)]

map_chr(affiliation, \(affil) names(city_search_terms)[map_lgl(city_search_terms, \(x) str_detect(affiliation, x))] |> 
          paste(collapse = ";"))
#for each study we want to know which city has
#a lead (PI/sponsor/responsible_party) or facility affiliation or any affil
affil_drks_ids_lead <- affil_grep

unique_drks_ids_lead <- unique(unlist(affil_drks_ids_lead))

#filter cases for affiliation
DRKS_sample <- DRKS_sample |>
  filter(drksId %in% unique_drks_ids_lead)


#----------------------------------------------------------------------------------------------------------------------
# create for each study a list of affiliated cities and add to main table
#----------------------------------------------------------------------------------------------------------------------

get_city_per_drks_id <- function(cities_drks_id_list, unique_drks_ids)
{
  cities_col <- vector("list", length(unique_drks_ids))
  names(cities_col) <- unique_drks_ids
  for (city in names(cities_drks_id_list)) {
    cities_col[cities_drks_id_list[[city]]] <-
      paste(cities_col[cities_drks_id_list[[city]]], city, sep = " ")
  }
  cities_col <- substring(cities_col, first = 6)
  names(cities_col) <- unique_drks_ids
  return(cities_col)
}

#create columns that list which cities are affiliated with the studies
drks_id_cities_lead <- get_city_per_drks_id(affil_drks_ids_lead, unique_drks_ids_lead)

#prepare for joining with main table
drks_id_cities_lead_tbl <- as_tibble(cbind(unique_drks_ids_lead, drks_id_cities_lead))
names(drks_id_cities_lead_tbl) <- c("drksId", "cities_lead")

#add columns to main table
DRKS_sample <- DRKS_sample |>
  left_join(drks_id_cities_lead_tbl, by = "drksId")


#----------------------------------------------------------------------------------------------------------------------
# Comparison with CT.gov dataset to get double entries
#----------------------------------------------------------------------------------------------------------------------

#check the columns for the secondary IDs for NCT ids
id_columns <- DRKS_sample[,grep("secId.id",colnames(DRKS_sample))]
nct_entries_columns <- apply(id_columns, 2, str_detect, pattern = "NCT")
nct_entries <- apply(nct_entries_columns, 1, any, na.rm = TRUE)

#extract the NCTs
nct_pos <- apply(nct_entries_columns, 1, which)
nct_pos[sapply(nct_pos, length) ==  0] <- NA
nct_pos <- unlist(nct_pos)

entry_num <- dim(DRKS_sample)[1]
ncts <- rep("", entry_num)
for(i in 1:entry_num) {
  if(is.na(nct_pos[i])) {
    ncts[i] <- NA
  } else {
    ncts[i] <- id_columns[[i, nct_pos[i]]]
  }
}

# All NCTs in DRKS sample (n = 29)
ncts_exist <- ncts[complete.cases(ncts)]

#add information on NCT-ids and filter studies with NCT (and thus are already registered on CT.gov)
DRKS_sample <- DRKS_sample |>
  add_column(has_nct_id = nct_entries) |>
  add_column(nct_id = ncts) 

#add information on whether NCTs in ctgov sample
ctgov_sample <- readr::read_csv2(here::here("data", "1_sample_generation", "IntoValue2_CTgov_sample.csv"))

DRKS_sample <- DRKS_sample |>
  mutate(in_ctgov_sample = if_else(nct_id %in% ctgov_sample$nct_id, TRUE, FALSE))

#get counts of drks trials with NCTs and those in ctgov sample
# 29 trials in drks with NCTs in drks secondary identifiers
# 1 of these not in ctgov sample, but remove from drks anyways
count(DRKS_sample, has_nct_id, in_ctgov_sample)


#remove trials with NCT from drks sample
DRKS_sample <- DRKS_sample |>
  filter(has_nct_id == FALSE)

DRKS_sample_save <- DRKS_sample |>
  rename(startDate_plannedActual = plannedActual) |> 
  mutate(
    startDate = as.Date(startDate),
    studyEnd = as.Date(studyEnd),
    targetSize = as.numeric(targetSize)
  ) |> 
  select(drksId, cities_lead, title,
         firstDrksPublishDate, startDate,
         startDate_plannedActual, studyEnd,
         intervention.category0, intervention.value0,
         intervention.category1, intervention.value1,
         intervention.category2, intervention.value2,
         address.type0, address.affiliation0, 
         address.firstname0, address.lastname0,
         address.type1, address.affiliation1, 
         address.firstname1, address.lastname1,
         targetSize, recruitmentStatus,
         investorInitiated, monoMultiCentric,
         publication.category0, publication.type0, publication.value0,
         publication.category1, publication.type1, publication.value1,
         publication.category2, publication.type2, publication.value2,
         publication.category3, publication.type3, publication.value3,
         publication.category4, publication.type4, publication.value4)


#save DRKS trial sample
write_csv2(DRKS_sample_save, here::here("data", "1_sample_generation", "IntoValue2_DRKS_sample.csv"), na = "")

# NOTE: For manual searches, additional columns later added
# [1] "publication_PMID"                    
# [2] "publication_DOI"                     
# [3] "publication_URL"                     
# [4] "article_yes_no"                      
# [5] "publication_date"                    
# [6] "unsure_about_publ_yes_no"            
# [7] "reason"                              
# [8] "other_comments"                      
# [9] "Publication identified in which step"
# [10] "manual_validation_cities_lead"   