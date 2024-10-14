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
      inputId = ns("map_l_top10_mint_gender_ui"),
      choices = c("Europa" = "EU", "OECD"),
      selected = "Europa",
      multiple = FALSE#,
      # options =  list(
      #   "max-options" = 2,
      #   "max-options-text" = "Maximal 2 Indikatoren auswählen")
    ),

    #Conditional Panel, um für Lehramt nur sinnvollere Fächer auswählen zu lassen

    conditionalPanel(condition = "input.map_l_top10_mint_gender_ui == 'EU'",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_eu_top10_mint_gender_ui"),
                       label = NULL,
                       choices = international_ui_years(region = "EU"),
                       selected = "2021"
                     ),

                     p("Fachbereich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_f_eu_top10_mint_gender_ui"),
                       choices = international_ui_faecher(region = "EU"),
                       selected = c("Alle MINT-Fächer"),
                       multiple = FALSE
                     )),
    conditionalPanel(condition = "input.map_l_top10_mint_gender_ui == 'OECD' ",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_oecd_top10_mint_gender_ui"),
                       label = NULL,
                       choices = international_ui_years(region = "OECD"),
                       selected = "2020"
                     ),

                     p("Fachbereich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_f_oecd_top10_mint_gender_ui"),
                       choices = international_ui_faecher(region = "OECD"),
                       selected = c("MINT"),
                       multiple = FALSE
                     )),

    p("Darstellungsart:"),
    shinyWidgets::pickerInput(
      inputId = ns("art"),
      choices = c("Frauenanteil in MINT" ="höchster Frauenanteil in MINT",
                  "Anteil MINT-Studierende unter Frauen" ="meisten Frauen wählen MINT"),
      selected = "Frauenanteil in MINT",
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

    # # TODO extract into own module, since this is repeated on a lot of modules
    #
    # shinyBS::bsPopover(id="dh_international_map", title = "",
    #                    content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
    #                    placement = "top",
    #                    trigger = "hover"),
    # tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_international_map"),
    # br(),
    # br(),
    shinyBS::bsPopover(id="ih_international_map4", title="",
                       content = paste0("Hier sieht man, dass Lichtenstein mit über 60 % einen außergewöhnlich hohen Frauenanteil aufweist. Deutschland dagegen hat den neuntkleinsten Frauenanteil in MINT (29 %)."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_international_map4")
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

    observeEvent(input$map_l_top10_mint_gender_ui, {
      r$map_l_g <- input$map_l_top10_mint_gender_ui
      r$show_avg_g <- input$show_avg_top10_mint_gender_line
      r$art_g <- input$art
      if (input$map_l_top10_mint_gender_ui == "EU") {
        r$map_y_g <- input$map_y_eu_top10_mint_gender_ui
        r$map_f_g <- input$map_f_eu_top10_mint_gender_ui
      }
      if (input$map_l_top10_mint_gender_ui == "OECD") {
        r$map_y_g <- input$map_y_oecd_top10_mint_gender_ui
        r$map_f_g <- input$map_f_oecd_top10_mint_gender_ui
      }
    })

    observeEvent(input$map_y_oecd_top10_mint_gender_ui, {
      r$map_y_g <- input$map_y_oecd_top10_mint_gender_ui
    })

    observeEvent(input$map_f_oecd_top10_mint_gender_ui, {
      r$map_f_g <- input$map_f_oecd_top10_mint_gender_ui
    })

    # eu check should be after oecd check, since it is the default and will
    # otherwise be overwritten on initial load up
    observeEvent(input$map_y_eu_top10_mint_gender_ui, {
      r$map_y_g <- input$map_y_eu_top10_mint_gender_ui
    })

    observeEvent(input$map_f_eu_top10_mint_gender_ui, {
      r$map_f_g <- input$map_f_eu_top10_mint_gender_ui
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
