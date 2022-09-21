#' datenschutz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_datenschutz_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' kontakt Server Functions
#'
#' @noRd
mod_datenschutz_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

mod_datenschutz_ui <- function(id){
  ns <- NS(id)
  tagList(

    # Banner
    # fluidRow(
    #   shinydashboard::box(
    #     width = 12,
    #     img(src='www/Banner_Kontakt.jpg',
    #         class = "img-responsive",
    #         #height = "150px", width = "150px",
    #         alt = "Banner Schule",
    #         style="display: block; margin-left: auto; margin-right: auto;"
    #     ))),



    fluidRow(
      shinydashboard::box(
        title = "Platzhalter: Datenschutz",
        width = 12,
        column(width = 9,

               p("....lorem ipsum usw...")))))}
