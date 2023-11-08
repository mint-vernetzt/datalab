#' fachkraft_start UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fachkraft_start_ui <- function(id){

  logger::log_debug("start mod_fachkraft_start_ui")

  ns <- NS(id)
  tagList(
    p("COMING SOON - mod_fachkraft_start_ui")
  )
}

#' fachkraft_start Server Functions
#'
#' @noRd
mod_fachkraft_start_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_fachkraft_start_ui("fachkraft_start_1")

## To be copied in the server
# mod_fachkraft_start_server("fachkraft_start_1")
