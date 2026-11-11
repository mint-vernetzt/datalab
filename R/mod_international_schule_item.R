#' international_schule_item UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_schule_item_ui <- function(id){


  ns <- NS(id)
  tagList(

    p("Darstellung:"),

    shinyWidgets::radioGroupButtons(
      inputId = ns("darstellung_timss_int_schule"),
      choices = c(
        "Scatterplot",
        "Karte"
      ),
      selected = "Scatterplot",
      justified = TRUE
    ),

    p("Fach:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("item_f_timss_int_schule"),
      choices = c(
        "Mathematik",
        "Naturwissenschaften"
      ),
      selected = "Mathematik",
      justified = TRUE
    ),

    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("item_y_timss_int_schule"),
      label = NULL,
      choices = international_ui_years(region = "TIMSS"),
      selected = "2023"
    ),

    conditionalPanel(condition = "input.darstellung_timss_int_schule == 'Scatterplot'",
                     ns = ns,
                     p("Länder beschriften:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("label_laender_timss"),
                       choices = DBI::dbGetQuery(
                         con,
                         "SELECT DISTINCT land FROM schule_timss WHERE indikator = 'Insgesamt' ORDER BY land"
                       )$land,
                       selected = "Deutschland",
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE,
                         `live-search` = TRUE
                       )
                     )
                    ),

  br(),
  shinyBS::bsPopover(id="ih_international_schule_item", title="",
                     content = paste0("Deutschland wird farblich abgehoben dargestellt", "<br> <br>Die Darstellung zeigt Unterschiede von Mädchen und Jungen im Kompetenztest von TIMSS. Z. B. Schneiden in Deutschland und weiteren 25 Ländern Jungen im Mathematiktest signifikant besser ab als Mädchen."),
                     placement = "top",
                     trigger = "hover"),
  tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_international_schule_item")
  )

}

#' international_schule_item Server Functions
#'
#' @noRd
mod_international_schule_item_server <- function(id, r){

  # logger::log_debug("start mod_international_schule_item_server")

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$darstellung_timss_int_schule, {
      r$darstellung_timss_int_schule <- input$darstellung_timss_int_schule
    })

    observeEvent(input$item_f_timss_int_schule, {
      r$item_f_int_schule <- input$item_f_timss_int_schule
    })

    observeEvent(input$item_y_timss_int_schule, {
      r$item_y_int_schule <- input$item_y_timss_int_schule
    })

    observeEvent(input$label_laender_timss, {
      r$label_laender_timss <- input$label_laender_timss
    })


  })
}

## To be copied in the UI
# mod_international_schule_item_ui("international_schule_item_1")

## To be copied in the server
# mod_international_schule_item_server("international_schule_item_1")
