#' international_top10_mint_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_top10_mint_gender_ui <- function(id){

  ns <- NS(id)
  tagList(
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("map_l"),
      choices = c("EU", "OECD"),
      selected = "EU",
      multiple = FALSE#,
      # options =  list(
      #   "max-options" = 2,
      #   "max-options-text" = "Maximal 2 Indikatoren auswählen")
    ),

    #Conditional Panel, um für Lehramt nur sinnvollere Fächer auswählen zu lassen

    conditionalPanel(condition = "input.map_l == 'EU'",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_eu"),
                       label = NULL,
                       choices = international_ui_years(region = "EU"),
                       selected = "2021"
                     ),

                     p("Fachbereich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_f_eu"),
                       choices = international_ui_faecher(region = "EU"),
                       selected = c("Alle MINT-Fächer"),
                       multiple = FALSE
                     )),
    conditionalPanel(condition = "input.map_l == 'OECD' ",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_oecd"),
                       label = NULL,
                       choices = international_ui_years(region = "OECD"),
                       selected = "2020"
                     ),

                     p("Fachbereich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_f_oecd"),
                       choices = international_ui_faecher(region = "OECD"),
                       selected = c("MINT"),
                       multiple = FALSE
                     )),

    p("Betrachtungsart:"),
    shinyWidgets::pickerInput(
      inputId = ns("art"),
      choices = c("höchster Frauenanteil in MINT", "meisten Frauen wählen MINT"),
      selected = "höchster Frauenanteil in MINT",
      multiple = FALSE
    ),

    p("Durchschnitt anzeigen:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("show_avg_top10_mint_gender_line"),
      choices = c("Ja", "Nein"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),


    br(),

    # TODO extract into own module, since this is repeated on a lot of modules

    shinyBS::bsPopover(id="dh_international_map", title = "",
                       content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_international_map"),
    br(),
    br(),
    shinyBS::bsPopover(id="ih_international_map", title="",
                       content = paste0("Die linke Karte der ersten Einstellung zeigt, dass die beiden Bundesländer mit dem höchsten Anteil von Informatik-Studierenden Bayern und Schleswig-Holstein mit jeweils 10 % sind."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_international_map")
  )
}

#' international_top10_mint_gender Server Functions
#'
#' @noRd
mod_international_top10_mint_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$show_avg_top10_mint_gender_line, {
      r$show_avg_g <- input$show_avg_top10_mint_gender_line
    })

    observeEvent(input$map_l, {
      r$map_l_g <- input$map_l
      r$show_avg_g <- input$show_avg_top10_mint_gender_line
      r$art_g <- input$art
      if (input$map_l == "EU") {
        r$map_y_g <- input$map_y_eu
        r$map_f_g <- input$map_f_eu
      }
      if (input$map_l == "OECD") {
        r$map_y_g <- input$map_y_oecd
        r$map_f_g <- input$map_f_oecd
      }
    })

    observeEvent(input$map_y_oecd, {
      r$map_y_g <- input$map_y_oecd
    })

    observeEvent(input$map_f_oecd, {
      r$map_f_g <- input$map_f_oecd
    })

    # eu check should be after oecd check, since it is the default and will
    # otherwise be overwritten on initial load up
    observeEvent(input$map_y_eu, {
      r$map_y_g <- input$map_y_eu
    })

    observeEvent(input$map_f_eu, {
      r$map_f_g <- input$map_f_eu
    })

    observeEvent(input$art, {
      r$art_g <- input$art
    })


  })
}

## To be copied in the UI
# mod_international_top10_mint_gender_ui("international_top10_mint_gender_1")

## To be copied in the server
# mod_international_top10_mint_gender_server("international_top10_mint_gender_1")