#' international_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_table_input_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        p("Region:"),
        shinyWidgets::pickerInput(
          inputId = ns("map_int_table_region"),
          label = NULL,
          choices = c("Europa", "OECD", "weltweit"),
          selected = "Europa",
          multiple = FALSE
        )
      ),
      column(
        width = 3,
        p("Land:"),
        shinyWidgets::pickerInput(
          inputId = ns("map_int_table_land"),
          choices = international_ui_country(type = "all"),
          selected = "Deutschland",
          multiple = TRUE,
          options =  list(
            "max-options" = 3,
            "max-options-text" = "Bitte nur maximal 3 Länder auswählen"
          )
        )
      )
    ),
    fluidRow(
      column(width = 3, p("Indikator")),
      column(width = 3, p("Fachbereich")),
      column(width = 3, p("Gruppe")),
      column(width = 3, p("Jahr"))
    ),
    # TODO loop here over filter settings
    # TODO add conditionals
    fluidRow(
      column(
        width = 3,
        shinyWidgets::pickerInput(
          inputId = ns("map_int_table_indikator_1"),
          label = NULL,
          choices = c("Studium", "Arbeitsmarkt"),
          selected = "Studium",
          multiple = FALSE
        )
      ),
      column(
        width = 3,
        shinyWidgets::pickerInput(
          inputId = ns("map_int_table_fachbereich_1"),
          label = NULL,
          choices = c("MINT", "nicht MINT"), # TODO use function
          selected = "MINT",
          multiple = FALSE
        )
      ),
      column(
        width = 3,
        shinyWidgets::pickerInput(
          inputId = ns("map_int_table_gruppe_1"),
          label = NULL,
          choices = c("A", "B"), # TODO use function
          selected = "A",
          multiple = FALSE
        )
      ),
      column(
        width = 3,
        shinyWidgets::pickerInput(
          inputId = ns("map_int_table_year_1"),
          label = NULL,
          choices = c("2020", "2022", "2024"), # TODO use function
          selected = "2020",
          multiple = FALSE
        )
      )
    )
  )
}

#' international_table Server Functions
#'
#' @noRd
mod_international_table_input_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_international_table_input_ui("international_table_input_1")

## To be copied in the server
# mod_international_table_input_server("international_table_input_1")
