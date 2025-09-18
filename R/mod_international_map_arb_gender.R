#' international_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_map_arb_gender_ui <- function(id) {

  ns <- NS(id)
  tagList(


    p("Personengruppe:"),
    shinyWidgets::pickerInput(
    inputId = ns("map_pers_eu_arb_gender"),
    choices = c("Ausgebildete", "Naturwissenschaftler*innen und Ingenieur*innen"),
    selected = c("Ausgebildete" ),
    multiple = FALSE

    ),
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
     inputId = ns("map_y_eu_arb_gender"),
     label = NULL,
     choices = c("2013", "2014", "2015", "2016", "2017",
       "2018", "2019", "2020", "2021", "2022", "2023" ),
     selected = "2023"
     ),


    # conditionalPanel(condition = "input.map_l_arb_gender == 'OECD' ",
    #                  ns = ns,
    #                  p("Personengruppe:"),
    #                  shinyWidgets::pickerInput(
    #                    inputId = ns("map_pers_oecd_arb_gender"),
    #                    choices = c("Ausbildung (ISCED 45)",
    #                                "kurzes tertiäres Bildungsprogramm (ISCED 5)",
    #                                "Bachelor oder vergleichbar (ISCED 6)",
    #                                "Master oder vergleichbar (ISCED 7)",
    #                                "Promotion (ISCED 8)",
    #                                "tertiäre Bildung (gesamt)"
    #                                ),
    #                    selected = c("Ausbildung (ISCED 45)"),
    #                    multiple = FALSE#,
    #
    #
    #                  ),
    #                  p("Jahr:"),
    #                  shinyWidgets::sliderTextInput(
    #                    inputId = ns("map_y_oecd_arb_gender"),
    #                    label = NULL,
    #                    choices = c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"),
    #                    selected = "2023"
    #                  ),
    #
    #                  p("Fachbereich:"),
    #                  shinyWidgets::pickerInput(
    #                    inputId = ns("map_f_oecd_arb_gender"),
    #                    choices = c( "MINT",
    #                                 "---Informatik & Kommunikationstechnologie"="Informatik & Kommunikationstechnologie",
    #                                 "---Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe"="Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
    #                                 "---Naturwissenschaften, Mathematik und Statistik" = "Naturwissenschaften, Mathematik und Statistik"),
    #                    selected = c("MINT"),
    #                    multiple = FALSE#,
    #
    #                  ),
    #                  )
     #                ,


    br(),

    shinyBS::bsPopover(id="ih_international_arbeitsmarkt_map2", title="",
                       content = paste0("Die erste Karte zeigt beispielsweise, dass der Anteil an Frauen unter den MINT-Ausgebildeten in den skandinavischen Ländern höher liegt als in Deutschland oder Südosteuropa."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_international_arbeitsmarkt_map2")
  )

}

#' international_map Server Functions
#'
#' @noRd
mod_international_map_arb_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

   # region change updates respective sub inputs, which will otherwise
  #  still be the last values.
    # observeEvent(input$map_l_arb_gender, {
    #   r$map_l_arb_gender <- input$map_l_arb_gender
    #   if (input$map_l_arb_gender == "EU") {
    #     r$map_y_arb_gender <- input$map_y_eu_arb_gender
    #     r$map_pers_arb_gender <- input$map_pers_eu_arb_gender
    #   } else if (input$map_l_arb_gender == "OECD") {
    #     r$map_pers_arb_gender <- input$map_pers_oecd_arb_gender
    #
    #       r$map_y_arb_gender <- input$map_y_oecd_arb_gender
    #       r$map_f_arb_gender<- input$map_f_oecd_arb_gender
    #
    #     # else if(input$map_pers_oecd_arb_gender %in% c("Auszubildende (ISCED 45)",
    #     #                                                "Auszubildende in Erstausbildung (ISCED 35)",
    #     #                                                "In Meisterlehre (< 880 Std. Vorbereitung, ISCED 55)",
    #     #                                                "In Meister-/Technikerlehre (> 880 Std. Vorbereitung, ISCED 65)")){
    #     #
    #     #   #r$map_y_arb_gender <- input$map_y_oecd2_arb_gender
    #     #   #r$map_f_arb_gender <- input$map_f_oecd2_arb_gender
    #     #   #r$map_betr_oecd_arb_gender <- input$map_betr_oecd_arb_gender
    #     #
    #     # }
    #
    #
    #   }
    # })


    # #das gabs unten bereits und braucht net
    # observeEvent(input$map_pers_oecd_arb_gender, {
    #   r$map_y_arb_gender <- input$map_y_eu_arb_gender
    #   r$map_pers_arb_gender <- input$map_pers_eu_arb_gender
    #
    # })



    # observeEvent(input$map_pers_oecd_arb_gender, {
    #   r$map_pers_arb_gender <- input$map_pers_oecd_arb_gender
    # })

    # observeEvent(input$map_y_oecd_arb_gender, {
    #   r$map_y_arb_gender <- input$map_y_oecd_arb_gender
    # })

    # observeEvent(input$map_f_oecd_arb_gender, {
    #   r$map_f_arb_gender <- input$map_f_oecd_arb_gender
    # })

    # observeEvent(input$map_y_oecd2_arb_gender, {
    #   r$map_y_arb_gender <- input$map_y_oecd2_arb_gender
    # })

    # observeEvent(input$map_f_oecd2_arb_gender, {
    #   r$map_f_arb_gender <- input$map_f_oecd2_arb_gender
    # })

    # observeEvent(input$map_betr_oecd_arb_gender, {
    #   r$map_betr_oecd_arb_gender <- input$map_betr_oecd_arb_gender
    # })


#
    # # otherwise be overwritten on initial load up
    observeEvent(input$map_y_eu_arb_gender, {
       r$map_y_arb_gender_eu <- input$map_y_eu_arb_gender
     })

    observeEvent(input$map_pers_eu_arb_gender, {
       r$map_pers_arb_gender_eu <- input$map_pers_eu_arb_gender
     })


  })
}

## To be copied in the UI
# mod_international_map_ui("international_map_1")

## To be copied in the server
# mod_international_map_server("international_map_1")
