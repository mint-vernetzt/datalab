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
        label = "Wähle ein oder mehrere Indikatoren:",
        choices = c("Bachelor" = "bachelor",
                    "Master" = "master",
                    "Lehramt" = "lehramt",
                    "Promotion" = "promotion"),
        selected = c("Bachelor" = "bachelor",
                     "Master" = "master"),
        multiple = TRUE
      ),
      hr(),
      h4("Studierendenzahlen"),
      shinyWidgets::pickerInput(
        inputId = ns("indikator_compare_2"),
        label = "Wähle ob eingeschrieben oder Studienanfänger:innen:",
        choices = c("Eingeschrieben" = "eingeschrieben",
                    "1.Hochschulemester" = "1hs",
                    "1.Fachsemester" = "1fs"),
        multiple = TRUE
      ),
      hr(),
      h4("Habilitationszahlen"),
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("ing_natwi_compare_1"),
        label = "Wähle ein Fach oder mehrere Fächer:",
        choices = c("Ingenieur" = "ingenieur", "Mathe & Natwi" = "mathe_natwi"),
        justified = TRUE,
        checkIcon = list(yes = icon("ok",
                                    lib = "glyphicon"))
      )
    ),
    br(),br(),br(),br(),
    p(" ", style = "margin-bottom: 36.5px;"),
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
      hr(),
      p(" ", style = "margin-bottom: 68.5px;"),
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("ing_natwi_compare_2"),
        label = "Wähle ein Fach oder mehrere Fächer:",
        choices = c("Ingenieur" = "ingenieur", "Mathe & Natwi" = "mathe_natwi"),
        justified = TRUE,
        checkIcon = list(yes = icon("ok",
                                    lib = "glyphicon"))
      ),hr()
    ),
    shiny::column(
      width = 4,
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("ing_natwi_compare_3"),
        label = "Wähle ein Fach oder mehrere Fächer:",
        choices = c("Ingenieur" = "ingenieur", "Mathe & Natwi" = "mathe_natwi"),
        justified = TRUE,
        checkIcon = list(yes = icon("ok",
                                    lib = "glyphicon"))
      ),hr()
    )
  )
}

#' studium_compare_choice Server Functions
#'
#' @noRd
mod_studium_compare_choice_server <- function(id, r, r_abschluss,
                                              r_studienzahl, r_habil){
  moduleServer( id, function(input, output, session){
   ns <- session$ns

   # control the hide of inputs

    observe({

      if(!isTruthy(input$indikator_compare_1)) {

        shinyWidgets::updateCheckboxGroupButtons(session=session,
                                                 inputId = "durchgefallen_compare",
                                                 disabled=T)

        shinyWidgets::updateCheckboxGroupButtons(session=session,
                                                 inputId = "ing_natwi_compare_3",
                                                 disabled=T)
      } else{

        shinyWidgets::updateCheckboxGroupButtons(session=session,
                                                 inputId = "durchgefallen_compare",
                                                 disabled=F)

        shinyWidgets::updateCheckboxGroupButtons(session=session,
                                                 inputId = "ing_natwi_compare_3",
                                                 disabled=F)
      }

      if(!isTruthy(input$indikator_compare_2)) {

        shinyWidgets::updateCheckboxGroupButtons(session=session,
                                                 inputId = "ing_natwi_compare_2",
                                                 disabled=T)
      } else{

        shinyWidgets::updateCheckboxGroupButtons(session=session,
                                                 inputId = "ing_natwi_compare_2",
                                                 disabled=F)

      }

    })


    observeEvent(input$date_compare, {
      r$date_compare <- input$date_compare
    })

    observeEvent(input$indikator_compare_1, {
      r_abschluss$indikator_compare_1 <- input$indikator_compare_1
    })

    observeEvent(input$indikator_compare_2, {
      r_studienzahl$indikator_compare_2 <- input$indikator_compare_2
    })

    observeEvent(input$ing_natwi_compare_1, {
      r_habil$ing_natwi_compare_1 <- input$ing_natwi_compare_1
    })

    observeEvent(input$durchgefallen_compare, {
      r_abschluss$durchgefallen_compare <- input$durchgefallen_compare
    })

    observeEvent(input$ing_natwi_compare_2, {
      r_studienzahl$ing_natwi_compare_2 <- input$ing_natwi_compare_2
    })

    observeEvent(input$ing_natwi_compare_3, {
      r_abschluss$ing_natwi_compare_3 <- input$ing_natwi_compare_3
    })




  })
}

## To be copied in the UI
# mod_studium_compare_choice_ui("studium_compare_choice_1")

## To be copied in the server
# mod_studium_compare_choice_server("studium_compare_choice_1")
