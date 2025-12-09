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

# combine separator-listed vectors into a single collapsed, deduplicated string
deduplicate_collapsed <- function(str_vec, collapse = ";") {
  str_vec |>
    strsplit(split = collapse) |> 
    unlist() |> 
    unique() |> 
    sort() |> 
    dplyr::na_if("") |>
    dplyr::na_if("-") |>
    dplyr::na_if("NA") |> 
    stats::na.omit() |> 
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
# url_extract_response <- function(url_str) {
#   
#   if (is.na(url_str) | url_str == "") return(NA)
#   
#   try(url_str |>
#         httr2::request() |>
#         httr2::req_retry(backoff = ~ 10) |> 
#         httr2::req_perform(verbosity = 3)
#         
#   )
#   
#   if (is.null(httr2::last_response())) return(0)
#   
#   httr2::last_response() |>
#     httr2::resp_status()
# }
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
