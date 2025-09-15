#' international_top10_mint_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_top10_mint_arb_ui <- function(id){

  ns <- NS(id)
  tagList(
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("map_l_top10_mint_arb"),
      choices = c("Europa" = "EU", "OECD"),
      selected = "Europa",
      multiple = FALSE

    ),



    conditionalPanel(condition = "input.map_l_top10_mint_arb == 'EU'",
                     ns = ns,


                     p("Personengruppe:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_pers_eu_top10_mint_arb"),
                       choices = c("Ausgebildete", "Naturwissenschaftler*innen und Ingenieur*innen"),
                       selected = c("Ausgebildete" ),
                       multiple = FALSE#,

                     ),
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_eu_top10_mint_arb"),
                       label = NULL,
                       choices = c("2013", "2014", "2015", "2016", "2017",
                                   "2018", "2019", "2020", "2021", "2022", "2023"),
                       selected = "2023"
                     )),


    conditionalPanel(condition = "input.map_l_top10_mint_arb == 'OECD' ",
                     ns = ns,
                     p("Personengruppe:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_pers_oecd_top10_mint_arb"),
                       choices = c("Anfänger*innen Ausbildung (ISCED 45)",
                                   "Anfänger*innen Erstausbildung (ISCED 35)",
                                   "Absolvent*innen Ausbildung (ISCED 45)",
                                   "Absolvent*innen Erstausbildung (ISCED 35)",
                                   "Auszubildende (ISCED 45)",
                                   "Auszubildende in Erstausbildung (ISCED 35)",
                                   "In Meisterlehre (< 880 Std. Vorbereitung, ISCED 55)",
                                   "In Meister-/Technikerlehre (> 880 Std. Vorbereitung, ISCED 65)"),
                       selected = c("Anfänger*innen Ausbildung (ISCED 45)"),
                       multiple = FALSE#,


                     ),
                     conditionalPanel(condition = "input.map_pers_oecd_top10_mint_arb == 'Anfänger*innen Ausbildung (ISCED 45)'|
                                      input.map_pers_oecd_top10_mint_arb == 'Anfänger*innen Erstausbildung (ISCED 35)'|
                                      input.map_pers_oecd_top10_mint_arb =='Absolvent*innen Ausbildung (ISCED 45)'|
                                      input.map_pers_oecd_top10_mint_arb == 'Absolvent*innen Erstausbildung (ISCED 35)' ",
                                      ns = ns,
                                      p("Jahr:"),
                                      shinyWidgets::sliderTextInput(
                                        inputId = ns("map_y_oecd_top10_mint_arb"),
                                        label = NULL,
                                        choices = c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"),
                                        selected = "2023"
                                      ),

                                      p("Fachbereich:"),
                                      shinyWidgets::pickerInput(
                                        inputId = ns("map_f_oecd_top10_mint_arb"),
                                        choices = c( "MINT",
                                                     "---Informatik & Kommunikationstechnologie"="Informatik & Kommunikationstechnologie",
                                                     "---Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe"="Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                                     "---Naturwissenschaften, Mathematik und Statistik" = "Naturwissenschaften, Mathematik und Statistik"),
                                        selected = c("MINT"),
                                        multiple = FALSE#,
                                      )),
                     conditionalPanel(condition = "input.map_pers_oecd_top10_mint_arb == 'Auszubildende (ISCED 45)'|
                                      input.map_pers_oecd_top10_mint_arb == 'Auszubildende in Erstausbildung (ISCED 35)'|
                                      input.map_pers_oecd_top10_mint_arb =='In Meisterlehre (< 880 Std. Vorbereitung, ISCED 55)'|
                                      input.map_pers_oecd_top10_mint_arb == 'In Meister-/Technikerlehre (> 880 Std. Vorbereitung, ISCED 65)' ",
                                      ns = ns,
                                      p("Jahr:"),
                                      shinyWidgets::sliderTextInput(
                                        inputId = ns("map_y_oecd2_top10_mint_arb"),
                                        label = NULL,
                                        choices = c("2015", "2016", "2017", "2018", "2019", "2020"),
                                        selected = "2020"
                                      ),

                                      p("Fachbereich:"),
                                      shinyWidgets::pickerInput(
                                        inputId = ns("map_f_oecd2_top10_mint_arb"),
                                        choices = c( "MINT",
                                                     "---Informatik & Kommunikationstechnologie"="Informatik & Kommunikationstechnologie",
                                                     "---Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe"="Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                                     "---Naturwissenschaften, Mathematik und Statistik" = "Naturwissenschaften, Mathematik und Statistik"),
                                        selected = c("MINT"),
                                        multiple = FALSE#,
                                      ))),

                     p("Durchschnitt anzeigen:"),
                     shinyWidgets::radioGroupButtons(
                       inputId = ns("show_avg_top10_mint_int_line_top10_mint_arb"),
                       choices = c("Ja", "Nein"),
                       justified = TRUE,
                       checkIcon = list(yes = icon("ok",
                                                   lib = "glyphicon"))
                     ),

    br(),
    darstellung(id="dh_map_international_4"),
    br(),
    br(),
    shinyBS::bsPopover(id="ih_international_arbeitsmarkt_tab3", title="",
                       content = paste0("Aus dieser Darstellung heraus liest sich, dass Deutschland im europäischen Vergleich zu den 10 Ländern zählt, die den geringsten Anteil an MINT-Ausgebildeten an allen Ausgebildeten aufweisen."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_international_arbeitsmarkt_tab3")
  )

}

#' international_top10_mint_gender Server Functions
#'
#' @noRd
mod_international_top10_mint_arb_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$map_l_top10_mint_arb, {
      r$map_l_top10_mint_arb <- input$map_l_top10_mint_arb
    })

    observeEvent(input$map_pers_eu_top10_mint_arb, {
      r$map_pers_eu_top10_mint_arb <- input$map_pers_eu_top10_mint_arb
    })

    observeEvent(input$map_y_eu_top10_mint_arb, {
      r$map_y_eu_top10_mint_arb <- input$map_y_eu_top10_mint_arb
    })

    observeEvent(input$map_pers_oecd_top10_mint_arb, {
      r$map_pers_oecd_top10_mint_arb <- input$map_pers_oecd_top10_mint_arb
    })

    observeEvent(input$map_y_oecd_top10_mint_arb, {
      r$map_y_oecd_top10_mint_arb <- input$map_y_oecd_top10_mint_arb
    })

    observeEvent(input$map_f_oecd_top10_mint_arb, {
      r$map_f_oecd_top10_mint_arb <- input$map_f_oecd_top10_mint_arb
    })

    observeEvent(input$map_y_oecd2_top10_mint_arb, {
      r$map_y_oecd2_top10_mint_arb <- input$map_y_oecd2_top10_mint_arb
    })

    observeEvent(input$map_f_oecd2_top10_mint_arb, {
      r$map_f_oecd2_top10_mint_arb <- input$map_f_oecd2_top10_mint_arb
    })

    observeEvent(input$map_betr_oecd_top10_mint_arb, {
      r$map_betr_oecd_top10_mint_arb <- input$map_betr_oecd_top10_mint_arb
    })

    observeEvent(input$show_avg_top10_mint_int_line_top10_mint_arb, {
      r$show_ave <- input$show_avg_top10_mint_int_line_top10_mint_arb
    })

  })
}
## To be copied in the UI
# mod_international_top10_mint_gender_ui("international_top10_mint_gender_1")

## To be copied in the server
# mod_international_top10_mint_gender_server("international_top10_mint_gender_1")
