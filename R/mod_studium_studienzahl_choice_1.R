#' studium_studienzahl_choice_1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_choice_1_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("waffle_y"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"),
      selected = "2022"
    ),
    p("Indikatoren (max. 2):"),
    shinyWidgets::pickerInput(
      inputId = ns("waffle_l"),
      choices = c("Studienanfänger:innen (1.Fachsemester)",
                  "Studienanfänger:innen (1.Hochschulsemester)",
                  "Studienanfänger:innen (Fachhochschulen, 1.Fachsemester)",
                  "Studienanfänger:innen (Fachhochschulen, 1.Hochschulsemester)",
                  "Studienanfänger:innen (Lehramt, Universität, 1.Fachsemester)",
                  "Studienanfänger:innen (Lehramt, Universität, 1.Hochschulsemester)",
                  "Studienanfänger:innen (Universität, 1.Fachsemester)",
                  "Studienanfänger:innen (Universität, 1.Hochschulsemester)",
                  "Studierende",
                  "Studierende (Fachhochschulen)",
                  "Studierende (Lehramt, Universität)",
                  "Studierende (Universität)"
      ),
      selected = c("Studierende"
                   , "Studienanfänger:innen (1.Fachsemester)"
      ),
      multiple = TRUE,
      options =  list(
        "max-options" = 2,
        "max-options-text" = "Maximal 2 Indikatoren auswählen")
    ),
    br(),
    shinyBS::bsPopover(id="ih_studium_mint_2", title="",
                       content = paste0("In der ersten Darstellung zeigt die rechte Grafik: 37 % der Studierenden lernen eine MINT-Disziplin. Der Großteil der MINT-Studierenden (26 %) studiert dabei eine Ingenieurwissenschaft, wozu auch ein Informatik-Studium zählt."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_mint_2")
  )

}

#' studium_studienzahl_choice_1 Server Functions
#'
#' @noRd
mod_studium_studienzahl_choice_1_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    # ns <- session$ns

    observeEvent(input$waffle_y, {
      r$waffle_y <- input$waffle_y
    })

    observeEvent(input$waffle_l, {
      r$waffle_l <- input$waffle_l
    })


    # observeEvent(input$hochschulform_studierende_1, {
    #   r$hochschulform_studierende_1 <- input$hochschulform_studierende_1
    # })
    #
    # observeEvent(input$hochschulform_studierende_2, {
    #   r$hochschulform_studierende_2 <- input$hochschulform_studierende_2
    # })


  })
}

## To be copied in the UI
# mod_studium_studienzahl_choice_1_ui("studium_studienzahl_choice_1_1")

## To be copied in the server
# mod_studium_studienzahl_choice_1_server("studium_studienzahl_choice_1_1")
