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
      choices = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"),
      selected = "2022"
    ),
#
#     p("Fach/Fächergruppe:"),
#     shinyWidgets::pickerInput(
#       inputId = ns("gen_f"),
#
#       choices = c("Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin",
#                   "Biologie",
#                   "Geowissenschaften und Geographie",
#                   "Informatik",
#                   "Maschinenbau/Verfahrenstechnik",
#                   "Alle Nicht MINT-Fächer",
#                   "Alle MINT-Fächer",
#                   "Vermessungswesen",
#                   "Architektur, Innenarchitektur",
#                   "Bauingenieurwesen",
#                   "Chemie",
#                   "Mathematik",
#                   "Materialwissenschaft und Werkstofftechnik",
#                   "Humanmedizin/Gesundheitswissenschaften",
#                   "Geisteswissenschaften",
#                   "Ingenieurwissenschaften (inkl. Informatik)",
#                   "Ingenieurwissenschaften ohne Informatik",
#                   "Physik, Astronomie",
#                   "Rechts-, Wirtschafts- und Sozialwissenschaften",
#                   "Mathematik, Naturwissenschaften",
#                   "Naturwissenschaften",
#                   "Pharmazie",
#                   "Raumplanung",
#                   "Sport",
#                   "Verkehrstechnik, Nautik",
#                   "Wirtschaftsingenieurwesen mit ingenieurwissenschaftlichem Schwerpunkt",
#                   "Kunst, Kunstwissenschaft",
#                   "Elektrotechnik und Informationstechnik"),
#
#       selected = "Alle MINT-Fächer"
#     ),
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
    ),
    p("Fach/Fächergruppe:"),

    #Conditonal Panel, dass keine leeren Plots kommen
    conditionalPanel(condition = "input.gen_states == 'Deutschland' |
      input.gen_states == 'Baden-Württemberg' |
      input.gen_states == 'Bayern' |
      input.gen_states == 'Berlin' |
      input.gen_states == 'Hamburg' |
      input.gen_states == 'Hessen' |
      input.gen_states == 'Nordrhein-Westfalen' |
                     input.gen_states == 'Rheinland-Pfalz' |
                     input.gen_states == 'Sachsen' |
                     input.gen_states == 'Westdeutschland (o. Berlin)' |
                     input.gen_states == 'Ostdeutschland (inkl. Berlin)'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("gen1_f"),

                       choices = studi_det_ui_faecher(spezif_r = c('Deutschland', 'Baden-Württemberg',
                                                        'Bayern', 'Berlin','Hamburg', 'Hessen',
                                                        'Nordrhein-Westfalen', 'Rheinland-Pfalz','Sachsen',
                                                        'Westdeutschland (o. Berlin)', 'Ostdeutschland (inkl. Berlin)' )),
                       selected = "Alle MINT-Fächer",
                       multiple = FALSE
                     )),
    conditionalPanel(condition = "input.gen_states == 'Brandenburg'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("gen2_f"),
                       choices = studi_det_ui_faecher(spezif_r ='Brandenburg'),
                       selected = "Alle MINT-Fächer",
                       multiple = FALSE
                     )),
    conditionalPanel(condition = "input.gen_states == 'Bremen'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("gen3_f"),
                       choices = studi_det_ui_faecher(spezif_r ='Bremen'),
                       selected = "Alle MINT-Fächer",
                       multiple = FALSE
                     )),
    conditionalPanel(condition = "input.gen_states == 'Mecklenburg-Vorpommern'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("gen4_f"),
                       choices = studi_det_ui_faecher(spezif_r ='Mecklenburg-Vorpommern'),
                       selected = "Alle MINT-Fächer",
                       multiple = FALSE
                     )),
    conditionalPanel(condition = "input.gen_states == 'Niedersachsen'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("gen5_f"),
                       choices = studi_det_ui_faecher(spezif_r ='Niedersachsen'),
                       selected = "Alle MINT-Fächer",
                       multiple = FALSE
                     )),
    conditionalPanel(condition = "input.gen_states == 'Saarland'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("gen6_f"),
                       choices = studi_det_ui_faecher(spezif_r ='Saarland'),
                       selected = "Alle MINT-Fächer",
                       multiple = FALSE
                     )),


    conditionalPanel(condition = "input.gen_states == 'Sachsen-Anhalt'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("gen7_f"),
                       choices = studi_det_ui_faecher(spezif_r ='Sachsen-Anhalt'),
                       selected = "Alle MINT-Fächer",
                       multiple = FALSE
                     )),
    conditionalPanel(condition = "input.gen_states == 'Schleswig-Holstein'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("gen8_f"),
                       choices = studi_det_ui_faecher(spezif_r ='Schleswig-Holstein'),
                       selected = "Alle MINT-Fächer",
                       multiple = FALSE
                     )),
    conditionalPanel(condition = "input.gen_states == 'Thüringen'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("gen9_f"),
                       choices = studi_det_ui_faecher(spezif_r ='Thüringen'),
                       selected = "Alle MINT-Fächer",
                       multiple = FALSE
                     )),
    br(),
    shinyBS::bsPopover(id="ih_studium_frauen_3", title="",
                       content = paste0("In der ersten Einstellung ist zu sehen, dass in Hamburg 2021 der Frauenanteil von Lehramtstudierenden mit einem Hauptfach in MINT mit 54 % über der Hälfte liegt. Dagegen sind nur ein Drittel der allgemeinen MINT-Studierenden in Hambrug weiblich."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_frauen_3")
    )


}

#' studium_studienzahl_einstieg_comparison_gender Server Functions
#'
#' @noRd
mod_studium_studienzahl_einstieg_comparison_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$gen_f_y, {
      r$gen_f_y <- input$gen_f_y
    })

    observeEvent(input$gen_states, {
      r$gen_states <- input$gen_states
    })

    observeEvent(input$gen1_f, {
      r$gen1_f <- input$gen1_f
    })

    observeEvent(input$gen2_f, {
      r$gen2_f <- input$gen2_f
    })
    observeEvent(input$gen3_f, {
      r$gen3_f <- input$gen3_f
    })

    observeEvent(input$gen4_f, {
      r$gen4_f <- input$gen4_f
    })
    observeEvent(input$gen5_f, {
      r$gen5_f <- input$gen5_f
    })

    observeEvent(input$gen6_f, {
      r$gen6_f <- input$gen6_f
    })
    observeEvent(input$gen7_f, {
      r$gen7_f <- input$gen7_f
    })

    observeEvent(input$gen8_f, {
      r$gen8_f <- input$gen8_f
    })

    observeEvent(input$gen9_f, {
      r$gen9_f <- input$gen9_f
    })


  })
}

## To be copied in the UI
# mod_studium_studienzahl_einstieg_comparison_gender_ui("studium_studienzahl_einstieg_comparison_gender_1")

## To be copied in the server
# mod_studium_studienzahl_einstieg_comparison_gender_server("studium_studienzahl_einstieg_comparison_gender_1")
