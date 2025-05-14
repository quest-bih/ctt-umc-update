library(tidyverse)
library(ctregistries)

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
which_umc <- function(umc_str, collapse = ";") {
  
  stopifnot("which_umc takes only a single string as input and you supplied a vector" =
              length(umc_str) == 1)
  
  regexes <- yaml::read_yaml(here::here("data", "umc_search_terms", "umc_search_terms.yaml"))
  
  hits <- purrr::map_lgl(regexes, \(x) str_detect(umc_str, x))
  return(
    regexes[hits] |>
      names() |>
      paste(collapse = collapse)
  )
}

# vectorized version of which_umc
which_umcs <- function(umc_vec, collapse = ";") {

  p <- progressr::progressor(along = umc_vec)
  furrr::future_map_chr(umc_vec, \(x) {
    p()
    which_umc(x)
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