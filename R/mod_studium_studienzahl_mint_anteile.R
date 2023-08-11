mod_studium_studienzahl_mint_anteile_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Auswahl des Indikators:"),
    shinyWidgets::pickerInput(
      inputId = ns("anteile_indi"),
      choices = c("Internationale Studienanfänger:innen (1. Hochschulsemester)",
                  "Studienanfänger:innen (1. Fachsemester)",
                  "Studierende",
                  "Internationale Studierende",
                  "Studienanfänger:innen (1. Hochschulsemester)",
                  "Studierende (Lehramt)"
      ),
      selected = "Studierende"),

    p("Auswahl des Bundeslands:"),
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
      selected = "Nordrhein-Westfalen"
    ),
    p("Betrachtung:"),
    shinyWidgets::pickerInput(
      inputId = ns("anteile_order"),
      choices = c("MINT-Fächer", "MINT-Aggregate"),
      multiple = FALSE,
      selected = "MINT-Fächer"
    ),
    br(),
    shinyBS::bsPopover(id="ih_studium_fach_4", title="",
                       content = paste0("Diese Grafik zeigt, wie sich die Anteile der einzelnen MINT-Diszipline über die Jahre verändern. So sieht man z. B. in der ersten Einstellung, dass in Nordrheinwestfalen von 2018 bis 2021 der Anteil an Informatik-Studierenden zunimmt, der Anteil an Studierenden in Maschinenbau/Verfahrenstechnik nimmt dagegen ab."),
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

    observeEvent(input$anteile_indi, {
      r$anteile_indi<- input$anteile_indi
    })

    observeEvent(input$anteile_states, {
      r$anteile_states <- input$anteile_states
    })

    observeEvent(input$anteile_order, {
      r$anteile_order <- input$anteile_order
    })




  })
}

## To be copied in the UI
# mod_studium_studienzahl_ranking_bl_subject_ui("studium_studienzahl_ranking_bl_subject_1")

## To be copied in the server
# mod_studium_studienzahl_ranking_bl_subject_server("studium_studienzahl_ranking_bl_subject_1")
