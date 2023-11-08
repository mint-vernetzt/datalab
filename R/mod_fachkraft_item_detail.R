#' fachkraft_item_detail UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fachkraft_item_detail_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("COMING SOON - mod_fachkraft_item_detail_ui")
  )
}

#' fachkraft_item_detail Server Functions
#'
#' @noRd
mod_fachkraft_item_detail_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_fachkraft_item_detail_ui("fachkraft_item_detail_1")

## To be copied in the server
# mod_fachkraft_item_detail_server("fachkraft_item_detail_1")
