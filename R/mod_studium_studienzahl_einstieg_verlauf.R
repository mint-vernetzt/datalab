#' studium_studienzahl_einstieg_verlauf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_einstieg_verlauf_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Jahre:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_studienzahl_einstieg_verlauf"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020,2021),
      selected = c(2015,2021)
    ),
    p("Indikatoren:"),
    shinyWidgets::pickerInput(
      inputId = ns("studienzahl_einstieg_verlauf_indi"),
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
        "max-options-text" = "Maximal 3 Indikatoren auswählen")
      # options = list(`actions-box` = TRUE,
      #                `deselect-all-text` = "Alle abwählen",
      #                `select-all-text` = "Alle auswählen")
    ),
    p("Betrachtung:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_zahlen_einstieg_verlauf_indi"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    br(),
    shinyBS::bsPopover(id="ih_studium_mint_3", title="",
                       content = paste0("Der Zeitverlauf zeigt, dass der Anteil von MINT-Studierenden an allen Studierenden sowie die absolute Anzahl der Studierenden in MINT bis zuletzt über die Jahre relativ konstant bleibt."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_mint_3")
    )


}


#' studium_studienzahl_einstieg_verlauf Server Functions
#'
#' @noRd
mod_studium_studienzahl_einstieg_verlauf_server <- function(id, r){
  moduleServer( id, function(input, output, session){


    observeEvent(input$date_studienzahl_einstieg_verlauf, {
      r$date_studienzahl_einstieg_verlauf <- input$date_studienzahl_einstieg_verlauf
    })

    observeEvent(input$studienzahl_einstieg_verlauf_indi, {
      r$studienzahl_einstieg_verlauf_indi <- input$studienzahl_einstieg_verlauf_indi
    })

    observeEvent(input$abs_zahlen_einstieg_verlauf_indi, {
      r$abs_zahlen_einstieg_verlauf_indi <- input$abs_zahlen_einstieg_verlauf_indi
    })


    observeEvent(input$abs_zahlen1, {
      r$abs_zahlen1 <- input$abs_zahlen1
    })


    observeEvent(input$abs_zahlen2, {
      r$abs_zahlen2 <- input$abs_zahlen2
    })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_einstieg_verlauf_ui("studium_studienzahl_einstieg_verlauf_1")

## To be copied in the server
# mod_studium_studienzahl_einstieg_verlauf_server("studium_studienzahl_einstieg_verlauf_1")
