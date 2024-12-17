#' fachkraft_item_prog_alle UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fachkraft_item_prog_alle_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Wirkhebel:"),
    shinyWidgets::pickerInput(
      inputId = ns("fachkraft_item_prog_alle_wirkhebel"),
      choices = fachkraft_ui_wirkhebel(),#"kurz"
      #selected = "MINT-Bildung",
      multiple = FALSE
    ),

    br(),

    uiOutput(ns("dynamic_popover_alle")),

    br(),

    shinyBS::bsPopover(id="ih_fachkraft_prog_alle_2", title="",
                       content = paste0("Die erste Darstellung zeigt: Wenn sich die Zahl an jungen MINT-Fachkräften, z. B. durch erfolgreiche MINT-Bildungsangebote, noch einmal positiver weiterentwickelt als in den letzten 10 Jahren, können rund 700.000 zusätzliche MINT-Fachkräfte gewonnen werden. Ein Einbruch in der Zunahme junger Menschen in MINT könnte dagegen einen Verlust von rund 800.000 Fachkräften bis 2037 bedeuten."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_fachkraft_prog_alle_2")

  )
}

#' fachkraft_item_prog Server Functions
#'
#' @noRd
mod_fachkraft_item_prog_alle_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    observeEvent(input$fachkraft_item_prog_alle_wirkhebel, {
      r$fachkraft_item_prog_alle_wirkhebel <- input$fachkraft_item_prog_alle_wirkhebel

    })

    selected_prog_wirkhebel <- reactive({
      input$fachkraft_item_prog_alle_wirkhebel
    })

    output$dynamic_popover_alle <- renderUI({
      # r$fachkraft_item_prog_wirkhebel <- input$fachkraft_item_prog_wirkhebel
      wirkhebel <- selected_prog_wirkhebel()

      popup_content <- dplyr::case_when(
        wirkhebel == "Gesamteffekt" ~ paste0("Positives Szenario = positiver Zuwachs im MINT-Nachwuchs der letzten 10 Jahre setzt sich fort und verdoppelt sich bei den Mädchen und jungen Frauen. Auch der positive Zuwachs in der Weiterbeschäftigung von Arbeitskräften über 60 der letzten 10 Jahre setzt sich fort. Für ausländische MINT-Fachkräfte zeigt sich das „hohe Zuwanderung“-Szenario der 15. koordinierten Bevölkerungsvorausberechnung des statistischen Bundesamts.", br(), br(), "Negatives Szenario = Umkehr des Wachstumstrends der letzten 10 Jahre für den MINT-Nachwuchs, jungen Frauen in MINT und in der Weiterbeschäftigung von Arbeitskräften über 60. Für ausländische MINT-Fachkräfte zeigt sich das „niedrige Zuwanderung“-Szenario der 15. koordinierten Bevölkerungsvorausberechnung des statistischen Bundesamts."),
        wirkhebel == "MINT-Bildung" ~ paste0("Positives Szenario = Es wird angenommen, dass sich die Zunahme von MINT-Fachkräften unter 35 zwischen 2012-2022 so in den nächsten Jahren fortsetzt", br(), br(), "Negatives Szenario = Es wird angenommen, dass sich die Zunahme von MINT-Fachkräften unter 35 zwischen 2012-2022 umkehrt und es zu einer Abnahme kommt."),
        wirkhebel == "Frauen in MINT" ~ paste0("Positives Szenario = Es wird angenommen, dass sich die Zunahme von weiblichen MINT-Fachkräften unter 35 zwischen 2012-2022 so in den nächsten Jahren fortsetzt.", br(), br(), "Kombiniertes Szenario = Es wird angenommen, dass sich die Zunahme von MINT-Fachkräften unter 35 zwischen 2012-2022 so in den nächsten Jahren fortsetzt und unter den jungen Frauen sogar doppelt so stark anwächst."),
        wirkhebel == "Internationale MINT-Fachkräfte" ~ paste0("Positives Szenario = „hohe Zuwanderung“-Szenario der 15. koordinierten Bevölkerungsvorausberechnung des statistischen Bundesamts.", br(), br(), "Rückgang im Positivtrend der Zuwanderung = „geringe Zuwanderung“-Szenario der 15. koordinierten Bevölkerungsvorausberechnung des statistischen Bundesamts.", br(), br(), "Vollständiger Stillstand der Zuwanderung = Fortschreibung der aktuellen MINT-Fachkräftezahlen ohne Wanderungsbewegungen in der Bevölkerung u40."),
        wirkhebel == "Beteiligung älterer MINT-Fachkräfte" ~ "Positives Szenario = der Anteil an erwerbstätigen MINT-Fachkräften unter den 55-59-, 60-64-, und 65-69-Jährigen wächst weiterhin so an wie zwischen 2012-2022.",
        T ~ "Fehler - passender Text kann nicht angezeigt werden."
      )

      tagList(
        shinyBS::bsPopover(id="erkl_fachkraft_prog_alle_1", title="",
                           content = popup_content,
                           placement = "bottom",
                           trigger = "hover"),
        tags$a(paste0("Was bedeuten die Szenarien"), icon("info-circle"), id="erkl_fachkraft_prog_alle_1")

      )

    })

  })
}

## To be copied in the UI
# mod_fachkraft_item_prog_alle_ui("fachkraft_item_prog_alle_1")

## To be copied in the server
# mod_fachkraft_item_prog_alle_server("fachkraft_item_prog_alle_1")

