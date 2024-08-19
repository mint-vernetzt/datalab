#' #' studium_studienzahl_einstieg_comparison UI Function
#' #'
#' #' @description A shiny Module.
#' #'
#' #' @param id,input,output,session Internal parameters for {shiny}.
#' #'
#' #' @noRd
#' #'
#' #' @importFrom shiny NS tagList
#' mod_studium_studienzahl_einstieg_comparison_ui <- function(id){
#'   ns <- NS(id)
#'   tagList(
#'
#'     p("Jahr:"),
#'     shinyWidgets::sliderTextInput(
#'       inputId = ns("date_kurse_einstieg_comparison"),
#'       label = NULL,
#'       choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021,2022),
#'       selected = 2022
#'     ),
#'     br(),
#'     shinyBS::bsPopover(id="ih_studium_mint_6", title="",
#'                        content = paste0("Über die Studierendengruppen hinweg liegt der Anteil an MINT-Studierenden in Deutschland 2021 zwischen 35 % - 39 %. Die einzige Ausnahme hierbei sind Lehramtstudierende: Weniger als ein Drittel der Lehramtstudierenden deutschlandweit belegen ein MINT-Fach als Hauptfach."),
#'                        trigger = "hover"),
#'     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_mint_6")
#'
#'   )
#' }
#'
#' #' studium_studienzahl_einstieg_comparison Server Functions
#' #'
#' #' @noRd
#' mod_studium_studienzahl_einstieg_comparison_server <- function(id, r){
#'   moduleServer( id, function(input, output, session){
#'
#'     observeEvent(input$date_kurse_einstieg_comparison, {
#'       r$date_kurse_einstieg_comparison <- input$date_kurse_einstieg_comparison
#'     })
#'
#'
#'   })
#' }
#'
#' ## To be copied in the UI
#' # mod_studium_studienzahl_einstieg_comparison_ui("studium_studienzahl_einstieg_comparison_1")
#'
#' ## To be copied in the server
#' # mod_studium_studienzahl_einstieg_comparison_server("studium_studienzahl_einstieg_comparison_1")
