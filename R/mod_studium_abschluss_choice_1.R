#' studium_abschluss_choice_1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_abschluss_choice_1_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::column(
      width = 6,
      shinyWidgets::prettyRadioButtons(
        inputId = ns("date_abschluss"),
        label = "Wähle ein Jahr:",
        choices = c(2012, 2015, 2018, 2020),
        selected = 2012,
        inline = TRUE,
        fill = TRUE
      ),
      shinyWidgets::radioGroupButtons(
        inputId = ns("durchgefallen"),
        label = "Welchen Status soll der Abschluss haben ?",
        choices = c("bestanden", "durchgefallen"),
        justified = TRUE,
        checkIcon = list(yes = icon("ok",
                                    lib = "glyphicon"))
      ),
      shinyWidgets::radioGroupButtons(
        inputId = ns("geschlecht_abschluss"),
        label = "Soll nach Geschlecht getrennt werden ?",
        choices = c("Ja", "Nein"),
        justified = TRUE,
        checkIcon = list(yes = icon("ok",
                                    lib = "glyphicon"))
      ),
      shinyWidgets::radioGroupButtons(
        inputId = ns("ing_natwi"),
        label = "Wähle ein Fach oder gesamt ?",
        choices = c("Ingenieur" = "ingenieur", "Mathe & Natwi" = "mathe_natwi", "Gesamt"),
        justified = TRUE,
        checkIcon = list(yes = icon("ok",
                                    lib = "glyphicon"))
      )
    )
  )
}

#' studium_abschluss_choice_1 Server Functions
#'
#' @noRd
mod_studium_abschluss_choice_1_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    #ns <- session$ns

    observeEvent(input$geschlecht_abschluss, {
      r$geschlecht_abschluss <- input$geschlecht_abschluss
    })

    observeEvent(input$date_abschluss, {
      r$date_abschluss <- input$date_abschluss
    })

    observeEvent(input$durchgefallen, {
      r$durchgefallen <- input$durchgefallen
    })

    observeEvent(input$ing_natwi, {
      r$ing_natwi <- input$ing_natwi
    })

  })
}


## To be copied in the UI
# mod_studium_abschluss_choice_1_ui("studium_abschluss_choice_1_1")

## To be copied in the server
# mod_studium_abschluss_choice_1_server("studium_abschluss_choice_1_1")
