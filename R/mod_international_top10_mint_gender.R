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
  logger::log_debug("start mod_international_top10_mint_gender_ui")

  ns <- NS(id)
  tagList(
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("top10_l_gender"),
      choices = c("EU", "OECD"),
      selected = "EU",
      multiple = FALSE#,
      # options =  list(
      #   "max-options" = 2,
      #   "max-options-text" = "Maximal 2 Indikatoren auswählen")
    ),

    #Conditional Panel, um für Lehramt nur sinnvollere Fächer auswählen zu lassen

    conditionalPanel(condition = "input.top10_l_gender == 'EU'",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("top10_y_eu_gender"),
                       label = NULL,
                       choices = international_ui_years(region = "EU"),
                       selected = "2021"
                     ),

                     p("Fachbereich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("top10_f_eu_gender"),
                       choices = international_ui_faecher(region = "EU"),
                       selected = c("Alle MINT-Fächer"),
                       multiple = FALSE
                     )),
    conditionalPanel(condition = "input.top10_l_gender == 'OECD' ",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("top10_y_oecd_gender"),
                       label = NULL,
                       choices = international_ui_years(region = "OECD"),
                       selected = "2020"
                     ),

                     p("Fachbereich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("top10_f_oecd_gender"),
                       choices = international_ui_faecher(region = "OECD"),
                       selected = c("Alle MINT-Fächer"),
                       multiple = FALSE
                     )),

    p("Betrachtungsart:"),
    shinyWidgets::pickerInput(
      inputId = ns("top10_art_gender"),
      choices = c("Fachbereich mit höchstem Frauenanteil" = "höchster Frauenanteil in MINT",
                  "Fachbereich, den die meisten Frauen wählen" = "meisten Frauen wählen MINT"),
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
  logger::log_debug("start mod_international_top10_mint_gender_server")

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$show_avg_top10_mint_gender_line, {
      r$show_avg_int_studium_gender <- input$show_avg_top10_mint_gender_line
    })

    observeEvent(input$top10_l_gender, {
      r$top10_l_int_studium_gender <- input$top10_l_gender
      r$show_avg_int_studium_gender <- input$show_avg_top10_mint_gender_line
      r$top10_art_int_studium_gender <- input$top10_art_gender

      if (input$top10_l_gender == "EU") {
        r$top10_y_int_studium_gender <- input$top10_y_eu_gender
        r$top10_f_int_studium_gender <- input$top10_f_eu_gender
      }
      if (input$top10_l_gender == "OECD") {
        r$top10_y_int_studium_gender <- input$top10_y_oecd_gender
        r$top10_f_int_studium_gender <- input$top10_f_oecd_gender
      }
    })

    observeEvent(input$top10_y_oecd_gender, {
      r$top10_y_int_studium_gender <- input$top10_y_oecd_gender
    })

    observeEvent(input$top10_f_oecd, {
      r$top10_f_int_studium_gender <- input$top10_f_oecd_gender
    })

    # eu check should be after oecd check, since it is the default and will
    # otherwise be overwritten on initial load up
    observeEvent(input$top10_y_eu_gender, {
      r$top10_y_int_studium_gender <- input$top10_y_eu_gender
    })

    observeEvent(input$top10_f_eu_gender, {
      r$top10_f_int_studium_gender <- input$top10_f_eu_gender
    })

    observeEvent(input$art_gender, {
      r$art_int_studium_gender <- input$art_gender
    })


  })
}

## To be copied in the UI
# mod_international_top10_mint_gender_ui("international_top10_mint_gender_1")

## To be copied in the server
# mod_international_top10_mint_gender_server("international_top10_mint_gender_1")
