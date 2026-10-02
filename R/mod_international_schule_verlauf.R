


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
      choices = sort(
        DBI::dbGetQuery(
          con,
          "SELECT DISTINCT land FROM schule_timss"
        )$land
      ),
      selected = c("Deutschland"),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `max-options` = 5
      )
    )

    ),

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
      inputId = ns("verlauf_land_pisa_int_schule"),
      choices = sort(
        DBI::dbGetQuery(
          con,
          "SELECT DISTINCT land FROM schule_pisa"
        )$land
      ),
      selected = c(
        "Deutschland",
        "OECD Durchschnitt"
      ),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `max-options` = 5
      )
    )



  ))

}



mod_international_schule_verlauf_server <- function(id, r){

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$verlauf_l_int_schule, {
      r$verlauf_l_int_schule <- input$verlauf_l_int_schule
      })

    observeEvent(input$verlauf_f_pisa_int_schule, {
      r$verlauf_f_int_schule <- input$verlauf_f_pisa_int_schule
    })

    observeEvent(input$verlauf_li_timss_int_schule, {
      r$verlauf_li_int_schule <- input$verlauf_li_timss_int_schule
    })

    observeEvent(input$verlauf_f_timss_int_schule, {
      r$verlauf_f_int_schule <- input$verlauf_f_timss_int_schule
    })

    observeEvent(input$verlauf_land_timss_int_schule, {
      r$verlauf_land_timss_int_schule <- input$verlauf_land_timss_int_schule
    })

    observeEvent(input$verlauf_land_pisa_int_schule, {
      r$verlauf_land_pisa_int_schule <- input$verlauf_land_pisa_int_schule
    })

    })
}





