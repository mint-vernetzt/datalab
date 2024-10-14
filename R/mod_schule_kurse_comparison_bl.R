#' #' schule_kurse_comparison_bl UI Function
#' #'
#' #' @description A shiny Module.
#' #'
#' #' @param id,input,output,session Internal parameters for {shiny}.
#' #'
#' #' @noRd
#' #'
#' #' @importFrom shiny NS tagList
#' mod_schule_kurse_comparison_bl_ui <- function(id){
#'   ns <- NS(id)
#'   tagList(
#'
#'     p("Jahr:"),
#'     shinyWidgets::sliderTextInput(
#'       inputId = ns("date_comparison_bl"),
#'       label = NULL,
#'       choices = c("2013","2014", "2015", "2016", "2017",
#'                   "2018","2019", "2020", "2021", "2022"),
#'       selected = "2022"),
#'
#'     p("Kursart:"),
#'     shinyWidgets::pickerInput(
#'       inputId = ns("indikator_comparison_bl"),
#'       choices = c("Grundkurse", "Leistungskurse"),
#'       selected = "Leistunskurse"
#'     ),
#'
#'     p("Fach/Fächergruppe:"),
#'     conditionalPanel(condition = "input.indikator_comparison_bl=='Grundkurse'",
#'     ns= ns,
#'     shinyWidgets::pickerInput(
#'       inputId = ns("subject_comparison_bl1"),
#'       choices =  c("MINT-Fächer (gesamt)",
#'                   "Mathematik",
#'                   "Informatik",
#'                   "Physik",
#'                   "Chemie",
#'                   "Biologie",
#'                   "andere Fächer (gesamt)",
#'                   "Deutsch",
#'                   "Fremdsprachen",
#'                   "Gesellschaftswissenschaften",
#'                   "Musik/Kunst",
#'                   "Religion/Ethik",
#'                   "Sport"))),
#'     conditionalPanel(condition = "input.indikator_comparison_bl=='Leistungskurse'",
#'                      ns= ns,
#'                      shinyWidgets::pickerInput(
#'                        inputId = ns("subject_comparison_bl2"),
#'                        choices =  c("MINT-Fächer (gesamt)",
#'                                     "Mathematik",
#'                                     "Informatik",
#'                                     "Physik",
#'                                     "Chemie",
#'                                     "Biologie",
#'                                     "andere Fächer (gesamt)",
#'                                     "Deutsch",
#'                                     "Fremdsprachen",
#'                                     "Gesellschaftswissenschaften",
#'                                     "Musik/Kunst",
#'                                     "Sport"))),
#'     br(),
#'     shinyBS::bsPopover(id="ih_schule_fach_6", title="",
#'                        content = paste0("Betrachtet man die Grundkursbelegungen sieht man, dass der Anteil von MINT-Fächern über alle Bundesländer hinweg zwischen 19 - 29 % liegt. Für die Leistungskursbelegungen in MINT ist der Unterschied zwischen den Bundesländern deutlicher: In Hamburg machen MINT-Fächer 24 %, in Sachsen-Anhalt 50 % aller Leistungskursbelegungen aus."),
#'                        placement = "top",
#'                        trigger = "hover"),
#'     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_fach_6")
#'
#'     )
#'
#' }
#'
#' #' schule_kurse_comparison_bl Server Functions
#' #'
#' #' @noRd
#' mod_schule_kurse_comparison_bl_server <- function(id, r){
#'   moduleServer( id, function(input, output, session){
#'
#'     observeEvent(input$date_comparison_bl, {
#'       r$date_comparison_bl <- input$date_comparison_bl
#'     })
#'
#'     observeEvent(input$indikator_comparison_bl, {
#'       r$indikator_comparison_bl <- input$indikator_comparison_bl
#'     })
#'
#'     observeEvent(input$subject_comparison_bl1, {
#'       r$subject_comparison_bl1 <- input$subject_comparison_bl1
#'     })
#'
#'     observeEvent(input$subject_comparison_bl2, {
#'       r$subject_comparison_bl2 <- input$subject_comparison_bl2
#'     })
#'
#'
#'
#'
#'
#'   })
#' }
#'
#' ## To be copied in the UI
#' # mod_schule_kurse_comparison_bl_ui("schule_kurse_comparison_bl_1")
#'
#' ## To be copied in the server
#' # mod_schule_kurse_comparison_bl_server("schule_kurse_comparison_bl_1")
