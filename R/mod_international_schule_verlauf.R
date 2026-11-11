


mod_international_schule_verlauf_ui <- function(id){

  ns <- NS(id)
  tagList(
  p("Erhebung:"),
  shinyWidgets::radioGroupButtons(
    inputId = ns("verlauf_l_int_schule"),
    choices = c("TIMSS", "PISA"),
    selected = "PISA",
    justified = TRUE
  ),

  conditionalPanel(
    condition = "input.verlauf_l_int_schule == 'TIMSS'",
    ns = ns,

    p("Fachbereich:"),
    shinyWidgets::pickerInput(
      inputId = ns("verlauf_f_timss_int_schule"),
      choices = c("Mathematik", "Naturwissenschaften"),
      selected = "Mathematik"
    ),

    p("Leistungsindikator:"),
    shinyWidgets::pickerInput(
      inputId = ns("verlauf_li_timss_int_schule"),
      choices = c(
        "Test-Punktzahl",
        "Mittlerer internationaler Standard" = "Mittlerer Standard erreicht"
      ),
      selected = "Test-Punktzahl"
    ),
    p("Länder:"),
    shinyWidgets::pickerInput(
      inputId = ns("verlauf_land_timss_int_schule"),
      choices = DBI::dbGetQuery(
        con,
        "SELECT DISTINCT land FROM schule_timss WHERE indikator = 'Insgesamt' ORDER BY land"
      )$land,
      selected = c("Deutschland", "Japan", "Frankreich"),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE
      )
    )),

  conditionalPanel(
    condition = "input.verlauf_l_int_schule == 'PISA'",
    ns = ns,

    p("Fachbereich:"),
    shinyWidgets::pickerInput(
      inputId = ns("verlauf_f_pisa_int_schule"),
      choices = c("Mathematik", "Naturwissenschaften"),
      selected = "Mathematik"
    ),
    p("Länder:"),
    shinyWidgets::pickerInput(
      inputId = ns("verlauf_land_int_schule"),
      choices = c(
        "Australien",
        "Belgien",
        "Chile",
        "Costa Rica",
        "Dänemark",
        "Deutschland",
        "Estland",
        "Finnland",
        "Frankreich",
        "Griechenland",
        "Großbritannien",
        "Irland",
        "Island",
        "Israel",
        "Italien",
        "Japan",
        "Kanada",
        "Kolumbien",
        "Korea, Republik von",
        "Lettland",
        "Litauen",
        "Mexiko",
        "Neuseeland",
        "Niederlande",
        "Norwegen",
        "OECD Durchschnitt",
        "Österreich",
        "Polen",
        "Portugal",
        "Schweden",
        "Schweiz",
        "Slowakei",
        "Slowenien",
        "Spanien",
        "Tschechische Republik",
        "Türkei",
        "Ungarn",
        "Vereinigte Staaten"
    ),
    selected = c(
      "Deutschland",
      "OECD Durchschnitt"
    ),
    multiple = TRUE,
    options = list(
      `actions-box` = TRUE,
      `max-options` = 5
    ))
  )
)
}



mod_international_schule_verlauf_server <- function(id, r){

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # observe({
    #   timss_laender <- DBI::dbGetQuery(
    #     con,
    #     "SELECT DISTINCT land FROM schule_timss ORDER BY land"
    #   )$land
    #
    #   shinyWidgets::updatePickerInput(
    #     session,
    #     inputId = "verlauf_land_timss_int_schule",
    #     choices = timss_laender,
    #     selected = intersect(
    #       c("Deutschland", "Interantionaler Durchschnitt"),
    #       timss_laender
    #     )
    #   )
    # })

    observe({

      req(input$verlauf_l_int_schule)

      r$verlauf_l_int_schule  <- input$verlauf_l_int_schule

      r$verlauf_land_int_schule <- if (input$verlauf_l_int_schule == "TIMSS") {
        input$verlauf_land_timss_int_schule
      } else {
        input$verlauf_land_int_schule
      }

      if (input$verlauf_l_int_schule == "TIMSS") {

        r$verlauf_f_int_schule  <- input$verlauf_f_timss_int_schule
        r$verlauf_li_int_schule <- input$verlauf_li_timss_int_schule

      } else {

        r$verlauf_f_int_schule <- input$verlauf_f_pisa_int_schule

      }
    })
    })
}





