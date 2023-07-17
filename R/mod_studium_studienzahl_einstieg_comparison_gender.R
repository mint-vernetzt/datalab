#' studium_studienzahl_einstieg_comparison_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_einstieg_comparison_gender_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("gen_f_y"),
      label = NULL,
      choices = c("2018", "2019", "2020", "2021"),
      selected = "2021"
    ),
    p("Fach/Fächergruppe:"),
    shinyWidgets::pickerInput(
      inputId = ns("gen_f"),

      choices = c("Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin",
                  "Biologie",
                  "Geowissenschaften und Geographie",
                  "Informatik",
                  "Maschinenbau/Verfahrenstechnik",
                  "Nicht MINT",
                  "MINT (Gesamt)",
                  "Vermessungswesen",
                  "Architektur, Innenarchitektur",
                  "Bauingenieurwesen",
                  "Chemie",
                  "Mathematik",
                  "Materialwissenschaft und Werkstofftechnik",
                  "Humanmedizin/Gesundheitswissenschaften",
                  "Geisteswissenschaften",
                  "Ingenieurwissenschaften",
                  "Ingenieurwissenschaften ohne Informatik",
                  "Physik, Astronomie",
                  "Rechts-, Wirtschafts- und Sozialwissenschaften",
                  "Mathematik, Naturwissenschaften",
                  "Naturwissenschaften",
                  "Pharmazie",
                  "Raumplanung",
                  "Sport",
                  "Verkehrstechnik, Nautik",
                  "Wirtschaftsingenieurwesen mit ingenieurwissenschaftlichem Schwerpunkt",
                  "Kunst, Kunstwissenschaft",
                  "Elektrotechnik und Informationstechnik"),

      selected = "MINT"
    ),
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("gen_states"),
      choices = c("Deutschland",
                  "Baden-Württemberg",
                  "Bayern",
                  "Berlin",
                  "Brandenburg",
                  "Bremen",
                  "Hamburg",
                  "Hessen",
                  "Mecklenburg-Vorpommern",
                  "Niedersachsen",
                  "Nordrhein-Westfalen",
                  "Rheinland-Pfalz",
                  "Saarland",
                  "Sachsen",
                  "Sachsen-Anhalt",
                  "Schleswig-Holstein",
                  "Thüringen"
                  ,
                  "Westdeutschland (o. Berlin)",
                  "Ostdeutschland (inkl. Berlin)"
      ),
      selected = "Hamburg"
    ))


}

#' studium_studienzahl_einstieg_comparison_gender Server Functions
#'
#' @noRd
mod_studium_studienzahl_einstieg_comparison_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$gen_f_y, {
      r$gen_f_y <- input$gen_f_y
    })

    observeEvent(input$gen_f, {
      r$gen_f <- input$gen_f
    })

    observeEvent(input$gen_states, {
      r$gen_states <- input$gen_states
    })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_einstieg_comparison_gender_ui("studium_studienzahl_einstieg_comparison_gender_1")

## To be copied in the server
# mod_studium_studienzahl_einstieg_comparison_gender_server("studium_studienzahl_einstieg_comparison_gender_1")
