#' home_start_comparison_mint_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' mod_home_start_comparison_mint_gender_ui <- function(id){
#'   ns <- NS(id)
#'   tagList(
#'     p("Jahr:"),
#'     shinyWidgets::sliderTextInput(
#'       inputId = ns("date_start_comparison_mint_gender"),
#'       label = NULL,
#'       choices = c("2013", "2014","2015","2016","2017", "2018", "2019", "2020", "2021", "2022"),
#'       selected = "2022"
#'     ),
#'     br(),
#'     shinyBS::bsPopover(id="ih_alle_frauen_3", title="",
#'                        content = paste0("Der Frauenanteil nimmt entlang der Bildungskette ab. Der Anteil von Mädchen, die in der Schule einen MINT-Leistungskurs belegen, liegt noch bei fast der Hälfte (48 %). In MINT-Studiengängen ist der Frauenanteil mit ca. einem Drittel bereits geringer. In Ausbildung und Beruf sind nur noch 13 % bzw. 17 % der Beschäftigte weiblich."),
#'                        trigger = "hover"),
#'     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_alle_frauen_3")
#'   )
#' }
#'
#' #' home_start_comparison_mint_gender Server Functions
#' #'
#' #' @noRd
#' mod_home_start_comparison_mint_gender_server <- function(id, r){
#'   moduleServer( id, function(input, output, session){
#'
#'     observeEvent(input$date_start_comparison_mint_gender, {
#'       r$date_start_comparison_mint_gender <- input$date_start_comparison_mint_gender
#'     })
#'
#'   })
#' }

## To be copied in the UI
# mod_home_start_comparison_mint_gender_ui("home_start_comparison_mint_gender_1")

## To be copied in the server
# mod_home_start_comparison_mint_gender_server("home_start_comparison_mint_gender_1")
