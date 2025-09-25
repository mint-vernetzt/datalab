mod_studium_studienzahl_mint_anteile_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahre:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("anteile_jahr"),
      label = NULL,
      choices = 2013:2024,
      selected = c(2017, 2024)
    ),

    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("anteile_states"),
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
                  "Ostdeutschland (inkl. Berlin)",
                  "Westdeutschland (o. Berlin)"
      ),
      selected = "Deutschland"
    ),

    p("Studierendengruppen:"),
    shinyWidgets::pickerInput(
      inputId = ns("anteile_indi"),
      choices = c("Studierende",
                  "Studienanfänger:innen (1. Hochschulsemester)",
                  "Absolvent:innen",
                  "internationale Studierende",
                  "internationale Studienanfänger:innen (1. Hochschulsemester)",
                  "internationale Absolvent:innen",
                  "Studierende im Lehramt" = "Studierende (Lehramt)",
                  "Absolvent:innen im Lehramt" ="Absolvent:innen (Lehramt)"),
      selected = "Studierende"),

    p("Fächer-Ebene:"),
    shinyWidgets::pickerInput(
      inputId = ns("anteile_order"),
      choices = c("MINT-Fächergruppen",
                  "MINT-Fachbereiche"),
      multiple = FALSE,
      selected = "MINT-Fachbereiche"
    ),

    conditionalPanel(condition = "input.anteile_order == 'MINT-Fächergruppen'",
                     ns = ns,
                     p("Fächergruppen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("anteile_faecher_mint"),
                       choices = studi_det_ui_faecher(),
                       selected = c("Informatik", "Elektrotechnik und Informationstechnik",
                                    "Physik, Astronomie", "Mathematik", "Chemie", "Maschinenbau/Verfahrenstechnik"),
                       multiple = TRUE,
                       options = list(`actions-box` = TRUE,
                                      `deselect-all-text` = "Alle abwählen",
                                      `select-all-text` = "Alle auswählen")
                     )
    ),

    p("Darstellungsart:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("anteile_betrachtung"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))),
    br(),

    shinyBS::bsPopover(id="ih_studium_fach_4", title="",
                       content = paste0("Diese Grafik zeigt, wie sich die Anteile der einzelnen MINT-Disziplinen über die Jahre verändern. So sieht man z. B. in der ersten Einstellung, dass deutschlandweit von 2017 bis 2023 der Anteil an Informatik-Studierenden zunimmt, der Anteil an Studierenden in anderen Ingenieurwissenschaften dagegen rückläufig ist."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_fach_4")

    )

}

#' studium_studienzahl_ranking_bl_subject Server Functions
#'
#' @noRd
mod_studium_studienzahl_mint_anteile_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    observeEvent(input$anteile_jahr, {
      r$anteile_jahr<- input$anteile_jahr
    })


    observeEvent(input$anteile_indi, {
      r$anteile_indi<- input$anteile_indi
    })

    observeEvent(input$anteile_states, {
      r$anteile_states <- input$anteile_states
    })

    observeEvent(input$anteile_order, {
      r$anteile_order <- input$anteile_order
    })

    observeEvent(input$anteile_faecher_mint, {
      r$anteile_faecher_mint <- input$anteile_faecher_mint
    })

    observeEvent(input$anteile_betrachtung, {
      r$anteile_betrachtung <- input$anteile_betrachtung
    })


  })
}

## To be copied in the UI
# mod_studium_studienzahl_ranking_bl_subject_ui("studium_studienzahl_ranking_bl_subject_1")

## To be copied in the server
# mod_studium_studienzahl_ranking_bl_subject_server("studium_studienzahl_ranking_bl_subject_1")
