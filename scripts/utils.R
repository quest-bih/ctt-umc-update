library(tidyverse)
library(ctregistries)
library(here)
library(httr2)

load_AACT_datasets <- function(AACT_folder, AACT_dataset_names) {
  #AACT filenames that we need to load
  file.path(AACT_folder, paste0(AACT_dataset_names, ".txt")) |> 
    map(read_delim, delim = "|") |> 
    set_names(AACT_dataset_names)
}

# extract list of regexes for given registry names
get_registry_regex <- function(registry_names_vec) {
  registry_tib <- ctregistries::registries |> 
    filter(registry %in% registry_names_vec) |> 
    select(registry, trn_regex)
  registry_tib |> 
    pull(trn_regex) |> 
    set_names(registry_tib$registry) |> 
    as.list()
}
# regexes <- get_registry_regex(c("DRKS", "ClinicalTrials.gov", "EudraCT"))
# str_vec <- "Düsseldorf;Köln;Magdeburg"
# exclude <- "Düsseldorf;Magdeburg"

vectorize_clean <- function(str_vec, sep = ";") {
  str_vec |>
    strsplit(split = sep) |> 
    unlist() |> 
    unique() |> 
    sort() |> 
    dplyr::na_if("") |>
    dplyr::na_if("-") |>
    dplyr::na_if("NA") |> 
    stats::na.omit()
}

# combine separator-listed vectors into a single collapsed, deduplicated string
deduplicate_collapsed <- function(str_vec, collapse = ";", exclude = NULL) {
  
  vectorized_input <- vectorize_clean(str_vec)
  if (!is.null(exclude)) {
    vectorized_exclusions <- vectorize_clean(exclude)
    vectorized_input <- vectorized_input[!vectorized_input %in% vectorized_exclusions]
  }
  
  vectorized_input |> 
    paste(collapse = collapse) |> 
    dplyr::na_if("")
}

get_umc_terms <- function(collapsed = TRUE) {
  regex_list <- yaml::read_yaml(here::here("data",
                                           "umc_search_terms",
                                           "umc_search_terms.yaml"))
  
  if (collapsed == FALSE) {
    return(
      regex_list |> 
        map(\(x) paste0("\\b(", x, ")\\b"))
    )
    
  } else {
    return(
      regex_list |> 
        (\(x) paste0("\\b", x, "\\b", collapse = "|"))()
    )
  }
}


# extract and collapse all umc terms from a string
which_umc <- function(umc_str, regexes = NULL, collapse = ";") {
  
  stopifnot("which_umc takes only a single string as input and you supplied a vector" =
              length(umc_str) == 1)
  if (is.null(regexes)) {
    regexes <- yaml::read_yaml(here::here("data", "umc_search_terms", "umc_search_terms.yaml"))
  }
  
  hits <- purrr::map_lgl(regexes, \(x) str_detect(umc_str, x))
  return(
    regexes[hits] |>
      names() |>
      paste(collapse = collapse)
  )
}

# vectorized version of which_umc
which_umcs <- function(umc_vec, collapse = ";") {

  regexes <- yaml::read_yaml(here::here("data", "umc_search_terms", "umc_search_terms.yaml"))
  
  p <- progressr::progressor(along = umc_vec)
  furrr::future_map_chr(umc_vec, \(x) {
    p()
    which_umc(x, regexes = regexes)
  })
}

# examples:
# which_umc(umc_str)
# umc_vec <- c("Munich", "Wuerzburg", "Ulm and Rostock")
# umc_str <- "Ulm and Rostock"
# 
# tibble(umc_str1 = umc_vec) |>
#   mutate(umcs = which_umcs(umc_str1))
# 
# tibble(umc_str1 = umc_vec, umc_str2 = c("Jena", "Kiel and Charite", "Regensburg")) |>
#   rowwise() |>
#   mutate(umcs = which_umcs(umc_str1),
#          umcs_all = which_umcs(c(umc_str1, umc_str2)) |> 
#            paste(collapse = ";"))
# 
# 
# 
# 
# validation_umcs_ctgov <- bind_rows(list(umc_ctgov_sponsors,
#                                         umc_resp_party,
#                                         umc_ctgov_pi_host)) |> 
#   filter(id %in% inclusion_trns) |>  # apply inclusion filter here
#   rowwise() |> 
#   mutate(umc = which_umcs(raw_affil)) |> 
#   ungroup()

# update obsolete alias TRNs with up-to-date TRNs, if available
update_ctgov_alias <- function(trn, trn_tib) {
  if (trn %in% trn_tib$alias) {
    trn_updated <- trn_tib |> 
      dplyr::filter(alias == trn) |> 
      dplyr::pull(nct_id)
  } else {
    trn_updated <- trn
  }
 return(trn_updated) 
}

# usage example
# update_ctgov_alias("NCT00006065", id_aliases)

# process title for title matching by removing indefinite articles, punctuation and spaces
process_title <- function(str_title) {
  stringr::str_remove_all(str_title,
                          stringr::regex("\\b(an?)\\b(?!\\-)|\\bN\\/?A\\b", ignore_case = TRUE)) |> 
    tolower() |> 
    stringr::str_remove_all("[:punct:]|(^aa\\s)|\\W") |> 
    stringr::str_squish()
}

# Create a graph where each TRN is a node, and each TRN pair is an edge
get_cluster_names_from_pairs <- function(pairs_tib) {
  
  # pairs_tib <- crossreg_only_pairs
  trial_graph <- igraph::graph_from_data_frame(pairs_tib, directed = FALSE)
  
  # Find clusters (connected components) in the graph
  clusters <- igraph::components(trial_graph)
  # Extract the membership, i.e., which trials belong to which clusters
  cluster_membership <- clusters$membership
  
  # Create a table of trial IDs and their corresponding cluster IDs
  trial_clusters <- data.frame(
    trial_id = names(cluster_membership),
    cluster_id = cluster_membership
  )
  # Group by cluster_id and create a unique identifier by concatenating trial IDs (TRNs) with underscores, similar to MDR ID
  trial_clusters_unique <- trial_clusters |>
    dplyr::group_by(cluster_id) |>
    dplyr::summarise(
      cluster_unique_id = paste(sort(trial_id), collapse = "_")  # Sort TRNs for consistency and concatenate
    )
  
  trial_clusters |>
    dplyr::left_join(trial_clusters_unique, by = "cluster_id") |>
    dplyr::select(trial_id, cluster_unique_id)
}

# check if a cluster contains all three registry strings
is_triad <- function(cluster_str) {
  stringr::str_detect(cluster_str, "-") &
    stringr::str_detect(cluster_str, "DRKS") &
    stringr::str_detect(cluster_str, "NCT")
}

# check if a cluster contains more than one link to the same registry
is_mtm <- function(cluster_str) {
  stringr::str_count(cluster_str, "-") > 3 |
    stringr::str_count(cluster_str, "DRKS") > 1 |
    stringr::str_count(cluster_str, "NCT") > 1
}

get_binary_id <- function(name_vec) {
  name_vec |>
    unique() |>
    sort() |>
    paste0(collapse = "_")
  
}

get_registry_name <- function(name_str) {
  dplyr::case_when(
    stringr::str_detect(name_str, "-") ~ "EUCTR",
    stringr::str_detect(name_str, "DRKS") ~ "DRKS",
    stringr::str_detect(name_str, "NCT") ~ "ClinicalTrials.gov"
  )
}

get_registry_names <- function(name_str) {
  strsplit(name_str, "_") |> 
    unlist() |> 
    which_registries() |> 
    unique() |> 
    sort() |> 
    paste(collapse = "_")
}

get_registry_url <- function(name_str) {
  dplyr::case_when(
    stringr::str_detect(name_str, "-") ~ paste0("https://www.clinicaltrialsregister.eu/ctr-search/search?query=", name_str),
    stringr::str_detect(name_str, "DRKS") ~ paste0("https://drks.de/search/en/trial/", name_str),
    stringr::str_detect(name_str, "NCT") ~ paste0("https://clinicaltrials.gov/study/", name_str)
  )
}

get_cluster_num_reg <- function(cluster_str, reg_regex) {
  stringr::str_count(cluster_str, reg_regex)
}


get_hierarchical_completion_date <- function(crossreg_tib) {
  
  stopifnot("Input must be a data frame with complete metadata on crossregistrations" =
              all(c("crossreg_id", "trial_id", "results_completion_date",
                    "estimated_completion_date", "actual_completion_date",
                    "last_updated",
                    "registration_date",
                    "completion_date",
                    "completion_date_type")
                  %in% names(crossreg_tib))) |> 
    try()
  
  hierarchical_tib <- crossreg_tib |> 
    dplyr::mutate(
      filled_actual_completion_date = dplyr::case_when(
        !is.na(actual_completion_date) ~ actual_completion_date,
        .default = NA_Date_),
      filled_euctr_completion_date = dplyr::if_else(has_euctr_results == TRUE, results_completion_date, NA_Date_)) |>
    dplyr::group_by(crossreg_id) |> 
    dplyr::arrange(crossreg_id, dplyr::desc(last_updated), dplyr::desc(registration_date)) |> 
    fill(c(filled_actual_completion_date, filled_euctr_completion_date), .direction = "up") |> 
    mutate(has_euctr_results = any(has_euctr_results, na.rm = TRUE),
           has_interventional = any(is_interventional, na.rm = TRUE),
           has_german_umc = any(is_german_umc, na.rm = TRUE),
           registries = get_registry_names(crossreg_id),
           has_cluster_de_protocol = any(has_trial_de_protocol, na.rm = TRUE),
           hierarchical_completion_date = dplyr::case_when(
             stringr::str_detect(crossreg_id, "-") &
               any(has_euctr_results == TRUE, na.rm = TRUE) &
               !is.na(dplyr::first(filled_euctr_completion_date)) ~
               dplyr::first(filled_euctr_completion_date),
             any(!is.na(filled_actual_completion_date), na.rm = TRUE) ~
               dplyr::first(filled_actual_completion_date),
             all(is.na(filled_actual_completion_date), na.rm = TRUE) ~
               max(estimated_completion_date, na.rm = TRUE),
             .default = NA_Date_
           ),
           last_updated_actual = dplyr::if_else(!is.na(actual_completion_date),
                                                last_updated, NA_Date_),
           only_estimated = all(is.na(results_completion_date), na.rm = TRUE) &
             all(is.na(actual_completion_date), na.rm = TRUE),
           is_last_updated = last_updated_actual ==
             max(last_updated_actual, na.rm = TRUE),
           n_last_updated = sum(is_last_updated, na.rm = TRUE),
           is_latest_actual = actual_completion_date ==
             max(actual_completion_date, na.rm = TRUE),
           is_latest_estimated = estimated_completion_date ==
             max(estimated_completion_date, na.rm = TRUE),
           n_latest_estimated = sum(is_latest_estimated, na.rm = TRUE),
           is_index_reg = dplyr::case_when(
             !is.na(results_completion_date) ~ TRUE,
             all(is.na(results_completion_date)) & 
               n_last_updated == 1 &
               is_last_updated & only_estimated == FALSE ~ TRUE,
             all(is.na(results_completion_date)) & 
               n_last_updated > 1 &
               is_latest_actual & only_estimated == FALSE ~ TRUE,
             only_estimated & is_latest_estimated & n_latest_estimated == 1 ~ TRUE,
             only_estimated & is_last_updated & n_latest_estimated > 1 ~ TRUE,
             .default = FALSE
           ),
           n_index_reg = sum(is_index_reg),
           has_completion_2018_2021 = any(dplyr::between(hierarchical_completion_date,
                                                         lubridate::as_date("2018-01-01"),
                                                         lubridate::as_date("2021-12-31")),
                                          na.rm = TRUE),
           recent_status = dplyr::first(status),
           has_withdrawn_status = any(stringr::str_detect(status, regex("withdrawn", ignore_case = TRUE))),
           many_to_many = is_mtm(crossreg_id),
           trns_in_cluster = stringr::str_count(crossreg_id, "_") + 1) |> 
    ungroup()
  
  return(hierarchical_tib)
  
}



update_bidirectionality <- function(crossreg_tib) {
  crossreg_tib |>
    dplyr::mutate(trial_registry = get_registry_name(trial_id),
                  linked_registry = get_registry_name(linked_id),
                  selfref = trial_registry == linked_registry) |>
    dplyr::group_by(binary_id, cluster_unique_id) |>
    dplyr::summarise(across(c(trial_id, linked_id), first),
                     across(where(is.logical), any),
                     bidirectional = n() > 1 & selfref == FALSE | bidirectional)
}

# split a pub_table by reviewer and create individual files for each reviewer
prep_and_print <- function(tib_pub_s, target_folder) {
  filename <- tib_pub_s |> 
    slice_head(n = 1) |> 
    pull(rev_nr)
  
  filename <- file.path(target_folder, paste0("pub_search_table_rev_", filename, ".csv"))
  
  write_csv(tib_pub_s, filename)
  
}

# ping a url and return the response (to see if it resolves)
url_extract_response <- function(url_str) {

  us_ag <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:147.0) Gecko/20100101 Firefox/147.0"

  if (is.na(url_str) | url_str == "") return(NA)

  try(
    url_str |>
        httr2::request() |>
        httr2::req_user_agent(us_ag) |>
        httr2::req_headers(
          `Connection` = "keep-alive",
          `Upgrade-Insecure-Requests` = "1") |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_perform(verbosity = 3)

  )
  # 
  if (is.null(httr2::last_response())) return(0)
  # 
  httr2::last_response() |>
    httr2::resp_status()
}
# 
# url_str <- ""
# 
# url_str <- "https://doi.org/10.1200/JCO.2017.35.15_suppl.6016"
# 
# dois_ls <- extractions |> 
#   filter(!is.na(earliest_pub_doi)) |> 
#   slice(1:10) |> 
#   pull(earliest_pub_doi)
# 
# ping_dois <- function(doi_ls) {
#   request_base <- httr2::request("https://doi.org/") |> 
#     httr2::req_throttle(capacity = 100, fill_time_s = 60) |> 
#     httr2::req_retry(backoff = ~ 10)
#   
#   reqs <- purrr::map(dois_ls, \(x) httr2::req_url_path_append(request_base, x))
#   resps <- httr2::req_perform_parallel(reqs, on_error = "continue")
#   
#   successes <- tibble::tibble()
#   resps[[5]] |> 
#     # httr2::resps_successes() |> 
#     # httr2::resps_requests() |> 
#     httr2::resp_status()
# 
#   resps |> httr2::resps_failures() |> 
#     httr2::resps_requests()
# }
# 
# respp <- request("https://doi.org/10.1200/JCO.2017.35.15_suppl.6016") |> 
#   httr2::req_throttle(capacity = 100, fill_time_s = 60) |> 
#   httr2::req_retry(backoff = ~ 10, max_tries = 5) |> 
#   req_perform(verbosity = 3)
# 
# respp |> resp_status()
# 
# 
# extract_response <- function(url_str) {
#   try(url_str |>
#         httr2::request() |>
#         httr2::req_perform(verbosity = 3) |>
#         httr2::req_retry(backoff = ~ 10)
#   )
#   
#   if (is_null(httr2::last_response())) return(0)
#   
#   httr2::last_response() |>
#     httr2::resp_status()
# }
#  


# dois_ls <- extractions |>
#   filter(!is.na(earliest_pub_doi)) |>
#   # slice(1:10) |>
#   pull(earliest_pub_doi)

# ping a vector of DOIs (no urls, but pure DOIs!) using the handles API endpoint
# and collect response statuses
ping_dois <- function(doi_ls) {
  request_base <- httr2::request("https://doi.org/api/handles/") |>
    httr2::req_throttle(capacity = 100, fill_time_s = 60) |>
    httr2::req_retry(backoff = ~ 10) |> 
    httr2::req_error(is_error = \(resp) resp_status(resp) == 500)

  reqs <- purrr::map(doi_ls, \(x) httr2::req_url_path_append(request_base, x))
  resps <- httr2::req_perform_parallel(reqs, on_error = "continue")

  statuses <- tibble::tibble(
    doi = unlist(doi_ls),
    status = resps |>
      purrr::map_dbl(httr2::resp_status)
  )
  return(statuses)
}
url_ls <- letters
ping_url <- function(url_ls) {
  
  reqs <- 
  request_base <- url_ls |> 
    httr2::request(url_i) |>
    httr2::req_throttle(capacity = 100, fill_time_s = 60) |>
    httr2::req_retry(backoff = ~ 10) |> 
    httr2::req_error(is_error = \(resp) resp_status(resp) == 500)
  
  reqs <- purrr::map(doi_ls, \(x) httr2::req_url_path_append(request_base, x))
  resps <- httr2::req_perform_parallel(reqs, on_error = "continue")
  
  statuses <- tibble::tibble(
    doi = unlist(doi_ls),
    status = resps |>
      purrr::map_dbl(httr2::resp_status)
  )
  return(statuses)
}


# url_check

update_error_log <- function(error_logs, qa_tib, rulename, colname) {
  if (nrow(qa_tib) > 0) {
    error_entry <- qa_tib |> 
      dplyr::group_by(crossreg_id) |> 
      dplyr::summarise(dplyr::across(dplyr::everything(), dplyr::first)) |> 
      dplyr::ungroup() |> 
      dplyr::select(trial_id, crossreg_id, extractor) |> 
      dplyr::mutate(rule = rulename,
                    col = colname)
    
    error_logs <- error_logs |> 
      dplyr::bind_rows(error_entry)
  }
  return(error_logs)
}

# Function to extract sponsor classification from `sponsors` field in EUCTR table
extract_sponsor_classifications <- function(json_text) {
  
  if (is.na(json_text)) return(json_text)
  
  # Parse the JSON into a list
  sponsors <- jsonlite::fromJSON(json_text, flatten = TRUE)
  
  if (!("status_of_the_sponsor" %in% colnames(sponsors))) {
    return(NA)
  } else {
    # Fallback to product_code if product_name is missing
    sponsor_classifications <- sponsors |>
      filter(!is.na(status_of_the_sponsor)) |>
      summarize(combined_names = paste(status_of_the_sponsor, collapse = "; ")) |>
      pull(combined_names)
  }
  
  # Return the product names as a single string, or NA if empty
  if (sponsor_classifications == "") {
    return(NA)
  } else {
    return(sponsor_classifications)
  }
}
# sponsors <- euctr_sp[1] |> jsonlite::fromJSON(flatten = TRUE)
# Function to extract sponsor name from `sponsors` field in EUCTR table
extract_sponsor_name <- function(json_text) {
  # Parse the JSON into a list
  sponsors <- jsonlite::fromJSON(json_text, flatten = TRUE)
  
  if (!("name_of_sponsor" %in% colnames(sponsors))) {
    return(NA)
  } else {
    # Fallback to product_code if product_name is missing
    sponsor_names <- sponsors |>
      filter(!is.na(name_of_sponsor)) |>
      summarize(combined_names = paste(name_of_sponsor, collapse = "; ")) |>
      pull(combined_names)
  }
  
  # Return the product names as a single string, or NA if empty
  if (sponsor_names == "") {
    return(NA)
  } else {
    return(sponsor_names)
  }
}
# str_date <- "2024-03-11"
# Helper function to add day 1 to a date missing the day
str_date_clean_floor <- function(str_date) {
  
  if (is.na(str_date)) return(str_date)
  if (stringr::str_detect(str_date, "NA|N\\WA")) return(NA_character_)
  
  if (stringr::str_detect(str_date, "-")) {
    str_year <- lubridate::year(str_date)
    str_day <- lubridate::day(str_date)
    str_month <- lubridate::month(str_date, label = TRUE)
    return(paste(str_day, str_month, str_year))
  }
  
  first_digit <- suppressWarnings(
    stringr::str_sub(str_date, 1, 1) |> 
      as.numeric()
  )  
  
  if (is.na(first_digit)) {
    return(paste("1", str_date))
  } else {
    return(str_date)
  }
}


# Helper function to calculate duration in days
duration_days <- function(start, end){
  as.numeric(lubridate::as.duration(lubridate::interval(start, end)), "days")
}

# Helper function to harmonize trial status across registries
recode_status <- function(status) {
  case_when(
    status %in% c("COMPLETE_FOLLOW_UP_COMPLETE",
                  "COMPLETED",
                  "Completed") ~ "Completed",
    status %in% c("INVITE_ONLY",
                  "PENDING",
                  "COMPLETE_FOLLOW_UP_CONTINUING",
                  "RECRUITING",
                  "Ongoing") ~ "Ongoing",
    status %in% c("DISCONTINIUED",
                  "Prematurely Ended",
                  "TERMINATED") ~ "Terminated",
    status == "WITHDRAWN" ~ "Withdrawn",
    str_detect(status,
               regex("suspended",
                     ignore_case = TRUE)
    ) ~ "Suspended",
    status == "UNKNOWN" ~ "Unknown",
    # status == "\\N" |
    #    ~ "Other",
    .default = NA
  )
}
# manual_tib <- manual_crossreg_info
# tib <- inex_combined
# correct crossregistration status from manual extractions
update_crossreg_manual <- function(tib, manual_tib) {
  
  new_links <-  manual_tib |> 
    filter(
      str_detect(action, "link")
    ) |> 
    rowwise() |> 
    mutate(crossreg_id = if_else(!is.na(new_crossreg),
                                 paste(c(trial_id, new_crossreg) |> sort(),
                                       collapse = "_"),
                                 trial_id)) |> 
    ungroup()
  
  missed_crossregs <- manual_tib |> 
    filter(str_detect(action, "add")) |> 
    select(trial_id, missed_crossreg = new_crossreg)
  
  manual_crossreg <- new_links |> 
    select(trial_id, new_crossreg, crossreg_id) |> 
    pivot_longer(-crossreg_id, values_to = "trial_id") |> 
    select(-name) |> 
    filter(!is.na(trial_id))
  
  manual_unlinked_crossreg <- manual_tib |> 
    filter(action == "unlink")
  partially_unlinked <- manual_unlinked_crossreg  |> 
    mutate(trial_id = purrr::map_chr(comments, ctregistries::which_trn)) |> 
    pull(trial_id)
  manual_removal_crossreg <- manual_unlinked_crossreg |> 
    filter(is_index_reg == FALSE) |> 
    pull(trial_id)
  
  manual_unlinked <- c(partially_unlinked,
                                manual_removal_crossreg) |> 
    unique()

  
  results_tib <- tib |> 
    # remove unlinked registrations
    # filter(!trial_id %in% manual_unlinked_crossreg) |>
    # flag unlinked registrations |> 
    mutate(is_unlinked = if_else(trial_id %in% manual_unlinked,
                                 TRUE,
                                 FALSE)
           ) |> 
    # update crossreg_id of newly linked registrations
    rows_upsert(manual_crossreg, by = "trial_id") |> 
    mutate(missed_crossreg = NA_character_) |> 
    rows_upsert(missed_crossregs, by = "trial_id")
  
  if (results_tib |> select(contains("index")) |> nrow() > 1) {
    index_info <- manual_tib |>
      select(trial_id, is_index_reg)
    
    
   index_reg <-  manual_crossreg |> 
      left_join(index_info, by = "trial_id") |> 
      group_by(crossreg_id) |> 
      mutate(is_index_reg = if_else(
        is.na(is_index_reg),
        !any(is_index_reg, na.rm = TRUE),
        is_index_reg
      )) |> 
     ungroup()
   
   results_tib <- results_tib |>
     rows_upsert(index_reg, by = c("trial_id", "crossreg_id")) |> 
     mutate(is_crossreg = str_detect(crossreg_id, "_"))
   
  }
    
  return(results_tib)
}
