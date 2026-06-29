primaryOutput <- function(id, ...) {
  
  tagList(
    plotlyOutput(NS(id, "plot"), ...)
  )
}

overallOutput <- function(id, ...) {
  tagList(
    uiOutput(NS(id, "valuebox"))
  )
}

# screened_data <- shiny_table
primaryServer <- function(id, results, route, role,
                          status_filter, timing, selected_years,
                          estimated_cd,
                          euctr_involvement,
                          sponsor_status,
                          color_fill,
                          title) {
  moduleServer(id, function(input, output, session) {
    
    print(class(results))
    print(str(results))
    

    
    # 
    # if (sbs() == FALSE) {
    #   plot_data <- plot_data |>
    #     filter(str_detect(.data[[role]], label()))
    # }
    
    # if (role == "umc") {
    #   
    #   
    #   
    # } else if (role == "umc_sponsor") {
    #   plot_data <- 
    # }
    output$plot <- renderPlotly({
      
      if (shiny::is.reactive(results)) {
        sbs <- reactive(str_detect(results()$label, "side"))
        
        results <- results()$data
      }
      
      plot_data <- results |> 
        get_prop_measure(route = route(),
                         role = role,
                         status_filter = status_filter(),
                         timing = timing(),
                         selected_years = selected_years(),
                         estimated_cd = estimated_cd(),
                         euctr_involvement = euctr_involvement(),
                         sponsor_status = sponsor_status())
      
      
      gg_plot <- plot_data |> 
        ggplot(aes(.data[[role]], perc,
                   text = paste("prop:", n, "out of", total))) +
        geom_col(fill = color_fill,
                 color = color_fill,
                 linewidth = 0.15) +
        ylim(0, 100) +
        labs(x = "", y = "% Results available", title = title) +
        theme_minimal() +
        scale_x_discrete(drop = FALSE) +
        theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))
      
      ggplotly(gg_plot)
    })
    
    output$valuebox <- renderUI({
      
      if (shiny::is.reactive(results)) {
        sbs <- reactive(str_detect(results()$label, "side"))
        
        results <- results()$data
      }
      
      valuebox_data <- results |> 
        get_prop_measure(route = route(),
                         role = role,
                         status_filter = status_filter(),
                         timing = timing(),
                         selected_years = selected_years(),
                         estimated_cd = estimated_cd(),
                         euctr_involvement = euctr_involvement(),
                         sponsor_status = sponsor_status()) |> 
        ungroup() |> 
        summarise(n = sum(n), total = sum(total)) |> 
        mutate(perc = round(n / total, 2) * 100) |> 
        pull(perc)
      
      route_text <- case_when(
        route() == "has_any" ~ "Overall proportion of any publications",
        route() == "has_sumres" ~ "Overall proportion of summary results on any registry",
        route() == "has_sumres_all" ~ "Overall proportion of summary results on all registries",
        route() == "has_pub" ~ "Overall proportion of journal publications",
        .default = NA_character_
      )
      
      # valuebox_data <- analysis_results |>
      #   get_prop_measure(route = "has_any",
      #                    role = "umc_sponsor",
      #                    status_filter = "any",
      #                    timing = "6",
      #                    selected_years = 2018:2021) |>

      bslib::value_box(
        title = route_text,
        value = paste0(valuebox_data, "%"),
        showcase = shiny::icon("list"),
        theme = color_fill 
      )
      # flexdashboard::valueBox(
      #   paste0(valuebox_data, "%"),
      #   route_text,
      #   icon = icon("list"),
      #   color = "purple"
      # )
      
    })
  })
  
}

#----------------------------------------------------------------------
# Help functions
#----------------------------------------------------------------------

get_prop_measure <- function(tib, route = c("has_sumres", "has_sumres_all", "has_pub", "has_any"),
                             role = c("umc_sponsor", "umc"),
                             status_filter,
                             timing = c("1", "2", "5", "6"),
                             selected_years,
                             estimated_cd,
                             euctr_involvement = c("all_trials",
                                                   "only_euctr_trials",
                                                   "no_euctr_trials"),
                             sponsor_status = c("all_trials",
                                                "only_comm",
                                                "non_comm")) {
  route <- match.arg(route)
  role <- match.arg(role)
  
  # status_filter <- match.arg(status_filter)
  timing <- match.arg(timing)
  euctr_involvement <- match.arg(euctr_involvement)
  sponsor_status <- match.arg(sponsor_status)
  
  stopifnot(all(selected_years %in% 2018:2021))
  stopifnot(all(status_filter %in% c("Completed",
                                     "Ongoing",
                                     "Terminated",
                                     "Suspended",
                                     "Unknown")))
  
  route_col <- case_when(
    timing == 1 & route == "has_sumres" ~ "has_summary_results_1y",
    timing == 2 & route == "has_sumres" ~ "has_summary_results_2y",
    timing == 5 & route == "has_sumres" ~ "has_summary_results_5y",
    timing == 1 & route == "has_sumres_all" ~ "has_sumres_all_1y",
    timing == 2 & route == "has_sumres_all" ~ "has_sumres_all_2y",
    timing == 5 & route == "has_sumres_all" ~ "has_sumres_all_5y",
    timing == 1 & route == "has_pub" ~ "has_publication_1y",
    timing == 2 & route == "has_pub" ~ "has_publication_2y",
    timing == 5 & route == "has_pub" ~ "has_publication_5y",
    timing == 1 & route == "has_any" ~ "has_any_pub_1y",
    timing == 2 & route == "has_any" ~ "has_any_pub_2y",
    timing == 5 & route == "has_any" ~ "has_any_pub_5y",
    .default = route
  )
  timing_col <- case_when(
      timing == 5 & route == "has_sumres" ~ "has_followup_sumres_5y",
      timing == 5 & route == "has_sumres_all" ~ "has_followup_sumres_all_5y",
      timing == 5 & route == "has_pub" ~ "has_followup_pub_5y",
      timing == 5 & route == "has_any" ~ "has_followup_any_5y",
      .default = NA_character_
  )
  

  
  tib <- tib |>
    filter(!is.na(.data[[role]]), .preserve = TRUE) |> 
    separate_longer_delim(all_of(role), delim = ";") |>
    mutate(has_any = has_sumres | has_pub,
           has_any_pub_1y = has_summary_results_1y | has_publication_1y,
           has_any_pub_2y = has_summary_results_2y | has_publication_2y,
           has_any_pub_5y = has_summary_results_5y | has_publication_5y,
           # this is equivalent to taking the earlier of the two followup dates
           has_followup_any_5y = has_followup_pub_5y & has_followup_sumres_5y,
           result = .data[[route_col]] == TRUE,
           across(all_of(c(role, "result")), factor)
    ) 
  # print(timing_col)
  
  if (!is.na(timing_col)) {
    tib <- tib |> 
      filter(.data[[timing_col]] == TRUE, .preserve = TRUE)
  }
  
  if (length(status_filter) < 5) {
    tib <- tib |> 
      filter(status %in% status_filter, .preserve = TRUE)
  }
  #### TODO: check if this actually is FALSE
  if (estimated_cd != TRUE) {
    tib <- tib |> 
      filter(completion_date_type == TRUE | is.na(completion_date_type),
             .preserve = TRUE)
  }
  
  if (length(selected_years) < 4) {
    tib <- tib |> 
      mutate(compl_year = year(cluster_completion_date)) |> 
      filter(compl_year %in% selected_years, .preserve = TRUE)
  }
  

  
  if (euctr_involvement != "all_trials") {
    if (euctr_involvement == "only_euctr_trials") {
      tib <- tib |> 
        filter(str_detect(crossreg_id, "-"), .preserve = TRUE)
    } else if (euctr_involvement == "no_euctr_trials") {
      tib <- tib |> 
        filter(!str_detect(crossreg_id, "-"), .preserve = TRUE)
    }
  }
  
  if (sponsor_status != "all_trials") {
    if (sponsor_status == "only_comm") {
      tib <- tib |> 
        filter(has_commercial == TRUE, .preserve = TRUE)
    } else if (sponsor_status == "non_comm") {
      tib <- tib |> 
        filter(has_commercial == FALSE, .preserve = TRUE)
    }
  }
  
  
  tib |> 
    count(.data[[role]], result, .drop = FALSE) |> 
    group_by(.data[[role]]) |> 
    mutate(total = sum(n),
           perc = round(n / total, 2) * 100) |> 
    filter(result == TRUE, .preserve = TRUE)
    # count(umc, umc_sponsor_bool, result, .drop = FALSE) |> 
    # # filter(umc == "Charite") |>
    # # group_by(umc) |>
    # summarise(total = sum(n),
    #           n = sum(result == TRUE),
    #           # n_sponsor = sum(umc_sponsor_bool),
    #        perc = round(n / total, 2) * 100)
}
