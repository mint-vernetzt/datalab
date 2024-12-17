#' international_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_map_arb_ui <- function(id) {

  ns <- NS(id)
  tagList(
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("map_l_arb"),
      choices = c("Europa" = "EU", "OECD"),
      selected = "Europa",
      multiple = FALSE#,
      # options =  list(
      #   "max-options" = 2,
      #   "max-options-text" = "Maximal 2 Indikatoren auswählen")
    ),

    #Conditional Panel, um für Lehramt nur sinnvollere Fächer auswählen zu lassen

    conditionalPanel(condition = "input.map_l_arb == 'EU'",
                     ns = ns,


                     p("Personengruppe:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_pers_eu_arb"),
                       choices = c("Ausgebildete", "Naturwissenschaftler*innen und Ingenieur*innen"),
                       selected = c("Ausgebildete" ),
                       multiple = FALSE#,

                     ),
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_eu_arb"),
                       label = NULL,
                       choices = c("2013", "2014", "2015", "2016", "2017",
                                   "2018", "2019", "2020", "2021", "2022" ),
                       selected = "2022"
                     )),


    conditionalPanel(condition = "input.map_l_arb == 'OECD' ",
                     ns = ns,
                     p("Personengruppe:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_pers_oecd_arb"),
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

                     )),
                     conditionalPanel(condition = "input.map_l_arb == 'OECD' && input.map_pers_oecd_arb == 'Anfänger*innen Ausbildung (ISCED 45)'||input.map_pers_oecd_arb == 'Anfänger*innen Erstausbildung (ISCED 35)'||input.map_pers_oecd_arb =='Absolvent*innen Ausbildung (ISCED 45)'||input.map_pers_oecd_arb == 'Absolvent*innen Erstausbildung (ISCED 35)' ",
                                      ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_oecd_arb"),
                       label = NULL,
                       choices = c("2015", "2016", "2017", "2018", "2019", "2020"),
                       selected = "2020"
                     ),

                     p("Fachbereich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_f_oecd_arb"),
                       choices = c( "MINT",
                                    "---Informatik & Kommunikationstechnologie"="Informatik & Kommunikationstechnologie",
                                    "---Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe"="Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                    "---Naturwissenschaften, Mathematik und Statistik" = "Naturwissenschaften, Mathematik und Statistik"),
                       selected = c("MINT"),
                       multiple = FALSE#,
                       # options =  list(
                       #   "max-options" = 2,
                       #   "max-options-text" = "Maximal 2 Indikatoren auswählen")
                     )),
                     conditionalPanel(condition = "input.map_l_arb == 'OECD' && input.map_pers_oecd_arb == 'Auszubildende (ISCED 45)'||input.map_pers_oecd_arb == 'Auszubildende in Erstausbildung (ISCED 35)'||input.map_pers_oecd_arb =='In Meisterlehre (< 880 Std. Vorbereitung, ISCED 55)'||input.map_pers_oecd_arb == 'In Meister-/Technikerlehre (> 880 Std. Vorbereitung, ISCED 65)' ",
                                      ns = ns,
                                      p("Jahr:"),
                                      shinyWidgets::sliderTextInput(
                                        inputId = ns("map_y_oecd2_arb"),
                                        label = NULL,
                                        choices = c("2015", "2016", "2017", "2018", "2019", "2020"),
                                        selected = "2020"
                                      ),

                                      p("Fachbereich:"),
                                      shinyWidgets::pickerInput(
                                        inputId = ns("map_f_oecd2_arb"),
                                        choices = c( "MINT",
                                                     "---Informatik & Kommunikationstechnologie"="Informatik & Kommunikationstechnologie",
                                                     "---Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe"="Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                                     "---Naturwissenschaften, Mathematik und Statistik" = "Naturwissenschaften, Mathematik und Statistik"),
                                        selected = c("MINT"),
                                        multiple = FALSE#,
                                        # options =  list(
                                        #   "max-options" = 2,
                                        #   "max-options-text" = "Maximal 2 Indikatoren auswählen")
                                      ))
                     ,


    br(),

    # # TODO extract into own module, since this is repeated on a lot of modules
    #
    # shinyBS::bsPopover(id="dh_international_map", title = "",
    #                    content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
    #                    placement = "top",
    #                    trigger = "hover"),
    # tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_international_map"),
    # br(),
    # br(),
    shinyBS::bsPopover(id="ih_international_arbeitsmarkt_map1", title="",
                       content = paste0("Die erste Einstellung zeigt unter anderem, dass europaweit der Anteil von MINT-Ausgebildeten an allen ausgebildeten Beschäftigten in Irland und Belgien über 50 % liegt. In Deutschland macht ihr Anteil rund 32 % aus."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_international_arbeitsmarkt_map1")
  )

}

#' international_map Server Functions
#'
#' @noRd
mod_international_map_arb_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # region change updates respective sub inputs, which will otherwise
    # still be the last values.
    observeEvent(input$map_l_arb, {
      r$map_l_arb <- input$map_l_arb
      if (input$map_l_arb == "EU") {
        r$map_y_arb <- input$map_y_eu_arb
        r$map_pers_arb <- input$map_pers_eu_arb
        #r$map_f_arb <- input$map_f_eu
      }
      if (input$map_l_arb == "OECD") {
        r$map_pers_arb <- input$map_pers_oecd_arb
        if(input$map_pers_oecd_arb %in% c("Anfänger*innen Ausbildung (ISCED 45)",
                              "Anfänger*innen Erstausbildung (ISCED 35)",
                              "Absolvent*innen Ausbildung (ISCED 45)",
                              "Absolvent*innen Erstausbildung (ISCED 35)")){

          r$map_y_arb <- input$map_y_oecd_arb
          r$map_f_arb <- input$map_f_oecd_arb

        }else{

          r$map_y_arb <- input$map_y_oecd2_arb
          r$map_f_arb <- input$map_f_oecd2_arb

        }


      }
    })

    observeEvent(input$map_pers_oecd_arb, {
      r$map_pers_arb <- input$map_pers_oecd_arb
    })

    observeEvent(input$map_y_oecd_arb, {
      r$map_y_arb <- input$map_y_oecd_arb
    })

    observeEvent(input$map_f_oecd_arb, {
      r$map_f_arb <- input$map_f_oecd_arb
    })

    observeEvent(input$map_y_oecd2_arb, {
      r$map_y_arb <- input$map_y_oecd2_arb
    })

    observeEvent(input$map_f_oecd2_arb, {
      r$map_f_arb <- input$map_f_oecd2_arb
    })






    # eu check should be after oecd check, since it is the default and will
    # otherwise be overwritten on initial load up
    observeEvent(input$map_y_eu_arb, {
      r$map_y_arb <- input$map_y_eu_arb
    })

    observeEvent(input$map_pers_eu_arb, {
      r$map_pers_arb <- input$map_pers_eu_arb
    })


  })
}

## To be copied in the UI
# mod_international_map_ui("international_map_1")

## To be copied in the server
# mod_international_map_server("international_map_1")
