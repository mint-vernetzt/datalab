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
      hr(),
      br(),
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
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("durchgefallen_compare"),
        label = "Welchen Status soll der Abschluss haben ?",
        choices = c("bestanden", "durchgefallen"),
        justified = TRUE,
        checkIcon = list(yes = icon("ok",
                                    lib = "glyphicon"))
      ),
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("ing_natwi_compare_3"),
        label = "Wähle ein Fach oder mehrere Fächer:",
        choices = c("Ingenieur" = "ingenieur", "Mathe & Natwi" = "mathe_natwi"),
        justified = TRUE,
        checkIcon = list(yes = icon("ok",
                                    lib = "glyphicon"))
      )
    ),
    shiny::column(
      width = 4,
      br(),br(),br(),
      p(" ", style = "margin-bottom: 26px;"),
      h4("Studierendenzahlen"),
      hr(),
      br(),
      p(" ", style = "margin-bottom: -20px;"),
      shinyWidgets::pickerInput(
        inputId = ns("indikator_compare_2"),
        label = "Wähle ob eingeschrieben oder Studienanfänger:innen:",
        choices = c("Eingeschrieben" = "eingeschrieben",
                    "1.Hochschulemester" = "1hs",
                    "1.Fachsemester" = "1fs"),
        multiple = TRUE
      ),
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("ing_natwi_compare_2"),
        label = "Wähle ein Fach oder mehrere Fächer:",
        choices = c("Ingenieur" = "ingenieur", "Mathe & Natwi" = "mathe_natwi"),
        justified = TRUE,
        checkIcon = list(yes = icon("ok",
                                    lib = "glyphicon"))
      )),
    shiny::column(
      width = 4,
      p(" ", style = "margin-bottom: 83px;"),
      h4("Habilitationszahlen"),
      hr(),
      br(),
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("ing_natwi_compare_1"),
        label = "Wähle ein Fach oder mehrere Fächer:",
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
     } else{

       shinyWidgets::updateCheckboxGroupButtons(session=session,
                                                inputId = "durchgefallen_compare",
                                                disabled=F)
     }

     if(!isTruthy(input$durchgefallen_compare)) {


       shinyWidgets::updateCheckboxGroupButtons(session=session,
                                                inputId = "ing_natwi_compare_3",
                                                disabled=T)
     } else{


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



    observe({
      r$date_compare <- input$date_compare
    })

    observe({
      r_abschluss$indikator_compare_1 <- input$indikator_compare_1
    })

    observe({
      r_studienzahl$indikator_compare_2 <- input$indikator_compare_2
    })

    observe({
      r_habil$ing_natwi_compare_1 <- input$ing_natwi_compare_1
    })

    observe({
      r_abschluss$durchgefallen_compare <- input$durchgefallen_compare
    })

    observe({
      r_studienzahl$ing_natwi_compare_2 <- input$ing_natwi_compare_2
    })

    observe({
      r_abschluss$ing_natwi_compare_3 <- input$ing_natwi_compare_3
    })




  })
}

## To be copied in the UI
# mod_studium_compare_choice_ui("studium_compare_choice_1")

## To be copied in the server
# mod_studium_compare_choice_server("studium_compare_choice_1")
