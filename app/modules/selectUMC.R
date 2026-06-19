
selectUMCInput <- function(id, label, choices, ...) {
  
  selectInput(NS(id, "selectUMC"),
              label,
              choices = choices,
              ...
  )
}

selectUMCServer <- function(id, datasource, umc_type) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(data = switch(input$selectUMC,
                         "None" = NULL,
                         "All" = datasource,
                         "All (cumulative, side-by-side)" = datasource |> 
                           separate_longer_delim(.data[[umc_type]], ";"),
                         datasource |>
                           separate_longer_delim(.data[[umc_type]], ";") |> 
                           filter(str_detect(.data[[umc_type]], input$selectUMC))),
           label = switch(input$selectUMC, 
                          "None" = "",
                          "All" = paste(tolower(input$selectUMC), "University Medical Centers"),
                          "All (cumulative, side-by-side)" = "all University Medical Centers (side-by-side)",
                          input$selectUMC)
      )
    })
    
  })
}
