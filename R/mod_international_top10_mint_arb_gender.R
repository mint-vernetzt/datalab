#' international_top10_mint_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_top10_mint_arb_gender_ui <- function(id){


  ns <- NS(id)
  tagList(
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("map_l_top10_mint_arb_gender"),
      choices = c("Europa" = "EU", "OECD"),
      selected = "Europa",
      multiple = FALSE#,
    ),

    #Conditional Panel, um für Lehramt nur sinnvollere Fächer auswählen zu lassen

    conditionalPanel(condition = "input.map_l_top10_mint_arb_gender == 'EU'",
                     ns = ns,


                     p("Personengruppe:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_pers_eu_top10_mint_arb_gender"),
                       choices = c("Ausgebildete", "Naturwissenschaftler*innen und Ingenieur*innen"),
                       selected = c("Ausgebildete" ),
                       multiple = FALSE#,

                     ),
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_eu_top10_mint_arb_gender"),
                       label = NULL,
                       choices = c("2013", "2014", "2015", "2016", "2017",
                                   "2018", "2019", "2020", "2021", "2022" ),
                       selected = "2022"
                     )),


    conditionalPanel(condition = "input.map_l_top10_mint_arb_gender == 'OECD' ",
                     ns = ns,
                     p("Personengruppe:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_pers_oecd_top10_mint_arb_gender"),
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
                     conditionalPanel(condition = "input.map_pers_oecd_top10_mint_arb_gender == 'Anfänger*innen Ausbildung (ISCED 45)'|
                                      input.map_pers_oecd_top10_mint_arb_gender == 'Anfänger*innen Erstausbildung (ISCED 35)'|
                                      input.map_pers_oecd_top10_mint_arb_gender =='Absolvent*innen Ausbildung (ISCED 45)'|
                                      input.map_pers_oecd_top10_mint_arb_gender == 'Absolvent*innen Erstausbildung (ISCED 35)' ",
                                      ns = ns,
                                      p("Jahr:"),
                                      shinyWidgets::sliderTextInput(
                                        inputId = ns("map_y_oecd_top10_mint_arb_gender"),
                                        label = NULL,
                                        choices = c("2015", "2016", "2017", "2018", "2019", "2020"),
                                        selected = "2020"
                                      ),

                                      p("Fachbereich:"),
                                      shinyWidgets::pickerInput(
                                        inputId = ns("map_f_oecd_top10_mint_arb_gender"),
                                        choices = c( "MINT",
                                                     "---Informatik & Kommunikationstechnologie"="Informatik & Kommunikationstechnologie",
                                                     "---Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe"="Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                                     "---Naturwissenschaften, Mathematik und Statistik" = "Naturwissenschaften, Mathematik und Statistik"),
                                        selected = c("MINT"),
                                        multiple = FALSE#,
                                      )),
                     conditionalPanel(condition = "input.map_pers_oecd_top10_mint_arb_gender == 'Auszubildende (ISCED 45)'|
                                      input.map_pers_oecd_top10_mint_arb_gender == 'Auszubildende in Erstausbildung (ISCED 35)'|
                                      input.map_pers_oecd_top10_mint_arb_gender =='In Meisterlehre (< 880 Std. Vorbereitung, ISCED 55)'|
                                      input.map_pers_oecd_top10_mint_arb_gender == 'In Meister-/Technikerlehre (> 880 Std. Vorbereitung, ISCED 65)' ",
                                      ns = ns,
                                      p("Jahr:"),
                                      shinyWidgets::sliderTextInput(
                                        inputId = ns("map_y_oecd2_top10_mint_arb_gender"),
                                        label = NULL,
                                        choices = c("2015", "2016", "2017", "2018", "2019", "2020"),
                                        selected = "2020"
                                      ),

                                      p("Fachbereich:"),
                                      shinyWidgets::pickerInput(
                                        inputId = ns("map_f_oecd2_top10_mint_arb_gender"),
                                        choices = c( "MINT",
                                                     "---Informatik & Kommunikationstechnologie"="Informatik & Kommunikationstechnologie",
                                                     "---Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe"="Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                                     "---Naturwissenschaften, Mathematik und Statistik" = "Naturwissenschaften, Mathematik und Statistik"),
                                        selected = c("MINT"),
                                        multiple = FALSE#,
                                      ),
                                      p("Darstellungsart:"),
                                      shinyWidgets::pickerInput(
                                        inputId = ns("map_betr_oecd_top10_mint_arb_gender"),
                                        choices = c("Anteil von Frauen an Allen", "Anteil an Frauen von Frauen"),
                                        selected = c("Anteil von Frauen an Allen"),
                                        multiple = FALSE))),

    p("Durchschnitt anzeigen:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("show_avg_top10_mint_arb_gender"),
      choices = c("Ja", "Nein"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    )
    ,


    br(),

    darstellung(id="dh_international_map_6"),
    br(),
    br(),
    shinyBS::bsPopover(id="ih_international_arbeitsmarkt_tap4", title="",
                       content = paste0("In der ersten Einstellung ist zu sehen, dass Deutschland mit knapp 31 % einen der niedrigsten Frauenanteile von MINT-Ausgebildeten im europäischen Vergleich aufweist."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_international_arbeitsmarkt_tap4")
  )

}

#' international_map Server Functions
#'
#' @noRd
mod_international_top10_mint_arb_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # region change updates respective sub inputs, which will otherwise
    # still be the last values.
    observeEvent(input$map_l_top10_mint_arb_gender, {
      r$map_l_top10_mint_arb_gender <- input$map_l_top10_mint_arb_gender
      if (input$map_l_top10_mint_arb_gender == "EU") {
        r$map_y_eu_top10_mint_arb_gender <- input$map_y_eu_top10_mint_arb_gender
        r$map_pers_top10_mint_arb_gender <- input$map_pers_eu_top10_mint_arb_gender
        r$show_avg_top10_mint_arb_gender <- input$show_avg_top10_mint_arb_gender

        #r$map_f_arb <- input$map_f_eu
      }
      if (input$map_l_top10_mint_arb_gender == "OECD") {
        r$map_pers_top10_mint_arb_gender <- input$map_pers_oecd_top10_mint_arb_gender

        if(input$map_pers_oecd_top10_mint_arb_gender %in% c("Anfänger*innen Ausbildung (ISCED 45)",
                                                 "Anfänger*innen Erstausbildung (ISCED 35)",
                                                 "Absolvent*innen Ausbildung (ISCED 45)",
                                                 "Absolvent*innen Erstausbildung (ISCED 35)")){

          r$map_y_top10_mint_arb_gender <- input$map_y_oecd_top10_mint_arb_gender
          r$map_f_top10_mint_arb_gender<- input$map_f_oecd_top10_mint_arb_gender
          r$show_avg_top10_mint_arb_gender <- input$show_avg_top10_mint_arb_gender

        }else if(input$map_pers_oecd_top10_mint_arb_gender %in% c("Auszubildende (ISCED 45)",
                                                       "Auszubildende in Erstausbildung (ISCED 35)",
                                                       "In Meisterlehre (< 880 Std. Vorbereitung, ISCED 55)",
                                                       "In Meister-/Technikerlehre (> 880 Std. Vorbereitung, ISCED 65)")){

          r$map_y_top10_mint_arb_gender <- input$map_y_oecd2_top10_mint_arb_gender
          r$map_f_top10_mint_arb_gender <- input$map_f_oecd2_top10_mint_arb_gender
          r$map_betr_oecd_top10_mint_arb_gender <- input$map_betr_oecd_top10_mint_arb_gender
          r$show_avg_top10_mint_arb_gender <- input$show_avg_top10_mint_arb_gender

        }


      }
    })

    observeEvent(input$map_pers_oecd_top10_mint_arb_gender, {
      r$map_pers_top10_mint_arb_gender <- input$map_pers_oecd_top10_mint_arb_gender
    })

    observeEvent(input$map_y_oecd_top10_mint_arb_gender, {
      r$map_y_top10_mint_arb_gender <- input$map_y_oecd_top10_mint_arb_gender
    })

    observeEvent(input$map_f_oecd_top10_mint_arb_gender, {
      r$map_f_top10_mint_arb_gender <- input$map_f_oecd_top10_mint_arb_gender
    })

    observeEvent(input$map_y_oecd2_top10_mint_arb_gender, {
      r$map_y_top10_mint_arb_gender <- input$map_y_oecd2_top10_mint_arb_gender
    })

    observeEvent(input$map_f_oecd2_top10_mint_arb_gender, {
      r$map_f_top10_mint_arb_gender <- input$map_f_oecd2_top10_mint_arb_gender
    })

    observeEvent(input$map_betr_oecd_top10_mint_arb_gender, {
      r$map_betr_oecd_top10_mint_arb_gender <- input$map_betr_oecd_top10_mint_arb_gender
    })

    observeEvent(input$input$map_y_eu_top10_mint_arb_gender, {
      r$map_y_eu_top10_mint_arb_gender <- input$map_y_eu_top10_mint_arb_gender
    })






    # eu check should be after oecd check, since it is the default and will
    # otherwise be overwritten on initial load up


    observeEvent(input$map_y_eu_top10_mint_arb_gender, {
      r$map_y_eu_top10_mint_arb_gender_eu <- input$map_y_eu_top10_mint_arb_gender
    })

    observeEvent(input$map_pers_eu_top10_mint_arb_gender, {
      r$map_pers_top10_mint_arb_gender_eu <- input$map_pers_eu_top10_mint_arb_gender
    })


    observeEvent(input$show_avg_top10_mint_arb_gender, {
     r$show_avg_top10_mint_arb_gender <- input$show_avg_top10_mint_arb_gender
    })


  })
}
