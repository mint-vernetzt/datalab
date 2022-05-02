#' studium_compare_choice_updates UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_compare_choice_updates_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' studium_compare_choice_updates Server Functions
#'
#' @noRd
mod_studium_compare_choice_updates_server <- function(id, r, compare_session){
  moduleServer( id, function(input, output, session){
 #   ns <- session$ns



  })
}

## To be copied in the UI
# mod_studium_compare_choice_updates_ui("studium_compare_choice_updates_1")

## To be copied in the server
# mod_studium_compare_choice_updates_server("studium_compare_choice_updates_1")
