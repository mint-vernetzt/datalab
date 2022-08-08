#' impressum UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_impressum_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' impressum Server Functions
#'
#' @noRd
mod_impressum_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}


mod_impressum_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
        width = 12,
        tags$h2("Impressum"),
        p(style = "text-align: justify; font-size = 16px",
          "lorem ipsum"),

      )))

}

## To be copied in the UI
# mod_impressum_ui("impressum_1")

## To be copied in the server
# mod_impressum_server("impressum_1")