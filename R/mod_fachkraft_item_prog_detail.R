#' fachkraft_item_prog_detail UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fachkraft_item_prog_detail_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Wirkhebel:"),
    shinyWidgets::pickerInput(
      inputId = ns("fachkraft_item_prog_detail_wirkhebel"),
       choices = c("Basis-Szenario", fachkraft_ui_wirkhebel()),
      # selected = "Basis-Szenario",
      #choices = fachkraft_ui_wirkhebel(),
      selected = "Basis-Szenario",
      multiple = FALSE
    ),

    p("Szenario:"),
    shinyWidgets::pickerInput(
      inputId = ns("fachkraft_item_prog_detail_scenario"),
      choices = NULL,
      # selected = "Basis-Szenario",
      multiple = FALSE
    ),

    # uiOutput(ns("prog_detail_scenario_picker_ui")),

    p("Beschäftigte betrachtet nach:"),
    shinyWidgets::pickerInput(
      inputId = ns("fachkraft_item_prog_detail_gruppe"),
      choices = fachkraft_ui_prognose_gruppen(),
      selected = c("Nationalität"),
      multiple = FALSE
    ),

    br(),

    uiOutput(ns("dynamic_popover_detail")),

    br(),

    shinyBS::bsPopover(id="ih_fachkraft_prog_detail", title="",
                       content = paste0(
                       "Die erste Einstellung zeigt das Basisszenario - also die zukünftige Entwicklung, auf die wir uns aktuell zubewegen. Die Darstellung zeigt, dass sich die Struktur der MINT-Fachkräfte ändern wird. Der Anteil internationaler Fachkräfte ohne deutsche Staatsangehörigkeit wird steigen und so den demographischen Wandel etwas abfedern."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_fachkraft_prog_detail")

  )
}

#' fachkraft_item_prog_detail Server Functions
#'
#' @noRd
mod_fachkraft_item_prog_detail_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$fachkraft_item_prog_detail_wirkhebel, {
      r$fachkraft_item_prog_detail_wirkhebel <- input$fachkraft_item_prog_detail_wirkhebel
    })

    observeEvent(input$fachkraft_item_prog_detail_scenario, {
      r$fachkraft_item_prog_detail_scenario <- input$fachkraft_item_prog_detail_scenario
    })

    observeEvent(input$fachkraft_item_prog_detail_gruppe, {
      r$fachkraft_item_prog_detail_gruppe <- input$fachkraft_item_prog_detail_gruppe
    })

    selected_prog_detail_wirkhebel <- reactive({
      input$fachkraft_item_prog_detail_wirkhebel
    })

    observeEvent(input$fachkraft_item_prog_detail_wirkhebel, {
      if (input$fachkraft_item_prog_detail_wirkhebel == "Basis-Szenario") {
        shinyWidgets::updatePickerInput(
          session,
          "fachkraft_item_prog_detail_scenario",
          choices = fachkraft_ui_scenario(wirkhebel = selected_prog_detail_wirkhebel()),
          selected = "Status-quo",
        )
      } else {
        wirkhebel <- selected_prog_detail_wirkhebel()
        shinyWidgets::updatePickerInput(
          session,
          "fachkraft_item_prog_detail_scenario",
          choices = fachkraft_ui_scenario(wirkhebel = selected_prog_detail_wirkhebel()),
          selected = ifelse(wirkhebel == "Frauen in MINT", "starke Verbesserung",
                            "Verbesserung")
        )
      }
    })

    output$dynamic_popover_detail <- renderUI({
      # r$fachkraft_item_prog_wirkhebel <- input$fachkraft_item_prog_wirkhebel
      wirkhebel <- selected_prog_detail_wirkhebel()

      popup_content <- dplyr::case_when(
        wirkhebel == "Basis-Szenario" ~ paste0("Basis-Szenario = Fortschreiben der aktuellen Entwicklungen bzw. der aktuellen Verhältnisse."),
        wirkhebel == "Gesamteffekt" ~ paste0("Positives Szenario = positiver Zuwachs im MINT-Nachwuchs der letzten 10 Jahre setzt sich fort und verdoppelt sich bei den Mädchen und jungen Frauen. Auch der positive Zuwachs in der Weiterbeschäftigung von Arbeitskräften über 60 der letzten 10 Jahre setzt sich fort. Für ausländische MINT-Fachkräfte zeigt sich das „hohe Zuwanderung“-Szenario der 15. koordinierten Bevölkerungsvorausberechnung des statistischen Bundesamts.", br(), br(), "Negatives Szenario = Umkehr des Wachstumstrends der letzten 10 Jahre für den MINT-Nachwuchs, jungen Frauen in MINT und in der Weiterbeschäftigung von Arbeitskräften über 60. Für ausländische MINT-Fachkräfte zeigt sich das „niedrige Zuwanderung“-Szenario der 15. koordinierten Bevölkerungsvorausberechnung des statistischen Bundesamts."),
        wirkhebel == "MINT-Bildung" ~ paste0("Positives Szenario = Es wird angenommen, dass sich die Zunahme von MINT-Fachkräften unter 35 zwischen 2012-2022 so in den nächsten Jahren fortsetzt", br(), br(), "Negatives Szenario = Es wird angenommen, dass sich die Zunahme von MINT-Fachkräften unter 35 zwischen 2012-2022 umkehrt und es zu einer Abnahme kommt."),
        wirkhebel == "Frauen in MINT" ~ paste0("Positives Szenario = Es wird angenommen, dass sich die Zunahme von weiblichen MINT-Fachkräften unter 35 zwischen 2012-2022 so in den nächsten Jahren fortsetzt.", br(), br(), "Kombiniertes Szenario = Es wird angenommen, dass sich die Zunahme von MINT-Fachkräften unter 35 zwischen 2012-2022 so in den nächsten Jahren fortsetzt und unter den jungen Frauen sogar doppelt so stark anwächst."),
        wirkhebel == "Internationale MINT-Fachkräfte" ~ paste0("Positives Szenario = „hohe Zuwanderung“-Szenario der 15. koordinierten Bevölkerungsvorausberechnung des statistischen Bundesamts.", br(), br(), "Rückgang im Positivtrend der Zuwanderung = „geringe Zuwanderung“-Szenario der 15. koordinierten Bevölkerungsvorausberechnung des statistischen Bundesamts.", br(), br(), "Vollständiger Stillstand der Zuwanderung = Fortschreibung der aktuellen MINT-Fachkräftezahlen ohne Wanderungsbewegungen in der Bevölkerung u40."),
        wirkhebel == "Beteiligung älterer MINT-Fachkräfte" ~ "Positives Szenario = der Anteil an erwerbstätigen MINT-Fachkräften unter den 55-59-, 60-64-, und 65-69-Jährigen wächst weiterhin so an wie zwischen 2012-2022.",
        T ~ "Fehler - passender Text kann nicht angezeigt werden."
      )

      tagList(
        shinyBS::bsPopover(id="erkl_fachkraft_prog_detail", title="",
                           content = popup_content,
                           placement = "bottom",
                           trigger = "hover"),
        tags$a(paste0("Was bedeuten die Szenarien"), icon("info-circle"), id="erkl_fachkraft_prog_detail")

      )

    })

  })
}

## To be copied in the UI
# mod_fachkraft_item_prog_detail_ui("fachkraft_item_prog_detail_1")

## To be copied in the server
# mod_fachkraft_item_prog_detail_server("fachkraft_item_prog_detail_1")

