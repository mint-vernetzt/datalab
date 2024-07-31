#' #' schule_kurse_map_gender UI Function
#' #'
#' #' @description A shiny Module.
#' #'
#' #' @param id,input,output,session Internal parameters for {shiny}.
#' #'
#' #' @noRd
#' #'
#' #' @importFrom shiny NS tagList
#' mod_schule_kurse_map_gender_ui <- function(id){
#'   ns <- NS(id)
#'   tagList(
#'
#'     p("Jahr:"),
#'     shinyWidgets::sliderTextInput(
#'       inputId = ns("date_map_gender"),
#'       label = NULL,
#'       choices =2013:2022,
#'       selected = 2022
#'     ),
#'     p("Fach/Fächergruppe:"),
#'     shinyWidgets::pickerInput(
#'       inputId = ns("subject_map_gender"),
#'       choices = c("MINT-Fächer (gesamt)",
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
#'                   "Sport"),
#'       selected = "MINT-Fächer (gesamt)"
#'     ),
#'     p("Kursart:"),
#'     shinyWidgets::pickerInput(
#'       inputId = ns("kurs_map_gender"),
#'       choices = c("Grundkurse",
#'                   "Leistungskurse",
#'                   "Oberstufenbelegungen"),
#'       selected = "Leistungskurse"
#'     ),
#'     br(),
#'     shinyBS::bsPopover(id="dh_schule_mint_5", title = "",
#'                        content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
#'                        trigger = "hover"),
#'     tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_schule_mint_5"),
#'     br(),
#'     br(),
#'     shinyBS::bsPopover(id="ih_schule_mint_5", title="",
#'                        content = paste0("Betrachtet man die Karten für Leistungskurse sieht man, dass 2021 über alle Bundesländer hinweg eher Jungen als Mädchen MINT-Leistungskurse wählen. Beispielsweise sind in Sachsen 54 % aller Leistungskursbelegungen von Jungen in MINT. Dagegen entfallen nur 33 % aller Belegungen von Mädchen auf ein MINT-Fach."),
#'                        trigger = "hover"),
#'     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_mint_5")
#'
#'   )
#' }
#'
#' #' schule_kurse_map_gender Server Functions
#' #'
#' #' @noRd
#' mod_schule_kurse_map_gender_server <- function(id, r){
#'   moduleServer( id, function(input, output, session){
#'
#'     observeEvent(input$date_map_gender, {
#'       r$date_map_gender <- input$date_map_gender
#'     })
#'
#'     observeEvent(input$subject_map_gender, {
#'       r$subject_map_gender <- input$subject_map_gender
#'     })
#'
#'     observeEvent(input$kurs_map_gender, {
#'       r$kurs_map_gender <- input$kurs_map_gender
#'     })
#'
#'   })
#' }
#'
#' ## To be copied in the UI
#' # mod_schule_kurse_map_gender_ui("schule_kurse_map_gender_1")
#'
#' ## To be copied in the server
#' # mod_schule_kurse_map_gender_server("schule_kurse_map_gender_1")
