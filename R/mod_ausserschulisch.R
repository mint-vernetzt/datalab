#' ausserschulisch UI Functions
#'
#' @noRd
mod_ausserschulisch_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        mod_ausserschulisch_start_ui("mod_ausserschulisch_start_ui_1")
      )
    )
  )
}

#' ausserschulisch Server Functions
#'
#' @noRd
mod_ausserschulisch_server <- function(id, r){

  r <- reactiveValues()

  mod_ausserschulisch_start_server("mod_ausserschulisch_start_ui_1", r)

  # Box 1 - CP
  mod_ausserschulisch_cp_orgas_server("mod_ausserschulisch_cp_orgas_ui",r)
  mod_ausserschulisch_cp_projekte_server("mod_ausserschulisch_cp_projekte_ui",r)
  mod_ausserschulisch_cp_profile_server("mod_ausserschulisch_cp_profile_ui",r)


  # Box 2 - MV-Befragungne
  mod_ausserschulisch_mvb_akteursbefragung_server("mod_ausserschulisch_mvb_akteursbefragung_ui_1", r)
  mod_ausserschulisch_mvb_stimmungsb_server("mod_ausserschulisch_mvb_stimmungsb_ui", r)
  mod_ausserschulisch_mvb_genderb_server("mod_ausserschulisch_mvb_genderb_ui")

  # #Box SKf
  # mod_ausserschulisch_skf_einrichtungen_server("mod_ausserschulisch_skf_einrichtungen_ui_1", r)
  # mod_ausserschulisch_skf_personal_server("mod_ausserschulisch_skf_personal_ui_1", r)

}

