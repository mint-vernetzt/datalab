#' studium_compare_choice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_compare_choice_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::column(
      width = 4,
      shinyWidgets::prettyRadioButtons(
        inputId = ns("date_compare"),
        label = "Wähle ein Jahr:",
        choices = c(2012, 2015, 2018, 2020),
        selected = 2012,
        inline = TRUE,
        fill = TRUE
      ),
      hr(),
      h4("Abschlusszahlen"),
      shinyWidgets::pickerInput(
        inputId = ns("indikator_compare_1"),
        label = "Wähle Indikators",
        choices = c("Bachelor" = "bachelor",
                    "Master" = "master",
                    "Lehramt" = "lehramt",
                    "Promotion" = "promotion"),
        multiple = TRUE
      ),
      hr(),
      h4("Studierendenzahlen"),
      shinyWidgets::pickerInput(
        inputId = ns("indikator_compare_2"),
        label = "Studierendenzahl",
        choices = c("Eingeschrieben" = "eingeschrieben",
                    "1. HS-Semester" = "1hs",
                    "1. FS-Semester" = "1fs"),
        multiple = TRUE
      ),
      hr(),
      h4("Habilitationszahlen"),
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("ing_natwi_compare_1"),
        label = "Wähle ein Fach oder gesamt ?",
        choices = c("Ingenieur" = "ingenieur", "Mathe & Natwi" = "mathe_natwi"),
        justified = TRUE,
        checkIcon = list(yes = icon("ok",
                                    lib = "glyphicon"))
      )
    ),
    br(),br(),br(),br(),
    p(" ", style = "margin-bottom: 31px;"),
    shiny::column(
      width = 4,
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("durchgefallen_compare"),
        label = "Welchen Status soll der Abschluss haben ?",
        choices = c("bestanden", "durchgefallen"),
        justified = TRUE,
        checkIcon = list(yes = icon("ok",
                                    lib = "glyphicon"))
      ),
      br(),br(),br(),br(),
      #p(" ", style = "margin-bottom: 5px;"),
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("ing_natwi_compare_2"),
        label = "Wähle ein Fach oder gesamt ?",
        choices = c("Ingenieur" = "ingenieur", "Mathe & Natwi" = "mathe_natwi"),
        justified = TRUE,
        checkIcon = list(yes = icon("ok",
                                    lib = "glyphicon"))
      )
    ),
    shiny::column(
      width = 4,
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("ing_natwi_compare_3"),
        label = "Wähle ein Fach oder gesamt ?",
        choices = c("Ingenieur" = "ingenieur", "Mathe & Natwi" = "mathe_natwi"),
        justified = TRUE,
        checkIcon = list(yes = icon("ok",
                                    lib = "glyphicon"))
      )
    )
  )
}

#' studium_compare_choice Server Functions
#'
#' @noRd
mod_studium_compare_choice_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    #ns <- session$ns

    observeEvent(input$date_compare, {
      r$date_compare <- input$date_compare
    })

    observeEvent(input$indikator_compare_1, {
      r$indikator_compare_1 <- input$indikator_compare_1
    })

    observeEvent(input$indikator_compare_2, {
      r$indikator_compare_2 <- input$indikator_compare_2
    })

    observeEvent(input$ing_natwi_compare_1, {
      r$ing_natwi_compare_1 <- input$ing_natwi_compare_1
    })

    observeEvent(input$durchgefallen_compare, {
      r$durchgefallen_compare <- input$durchgefallen_compare
    })

    observeEvent(input$ing_natwi_compare_2, {
      r$ing_natwi_compare_2 <- input$ing_natwi_compare_2
    })

    observeEvent(input$ing_natwi_compare_3, {
      r$ing_natwi_compare_3 <- input$ing_natwi_compare_3
    })

  })
}

## To be copied in the UI
# mod_studium_compare_choice_ui("studium_compare_choice_1")

## To be copied in the server
# mod_studium_compare_choice_server("studium_compare_choice_1")
