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

    # shinyBS::bsPopover(id="erkl_fachkraft_prog_1", title="",
    #                    content = paste0("Positives Szenario = positiver Zuwachs im MINT-Nachwuchs der letzten 10 Jahre setzt sich fort und verdoppelt sich bei den Mädchen und jungen Frauen. Auch der positive Zuwachs in der Weiterbeschäftigung von Arbeitskräften ü60 der letzten 10 Jahre setzt sich fort. Für ausländische MINT-Fachkräfte zeigt sich das „hohe Zuwanderung“-Szenario der 15. Koordinierten Bevölkerungsvorausberechnung des statistischen Bundesamts.", br(), br(), "Negatives Szenario = Umkehr des Wachstumstrends im MINT-Nachwuchs, von jungen Frauen in MINT und in der Weiterbeschäftigung von Arbeitskräften ü60 der letzten 10 Jahre. Für ausländische MINT-Fachkräfte zeigt sich das „niedrige Zuwanderung“-Szenario der 15. Koordinierten Bevölkerungsvorausberechnung des statistischen Bundesamts."),
    #                    placement = "top",
    #                    trigger = "hover"),
    # tags$a(paste0("Was bedeuten die Szenarien"), icon("info-circle"), id="erkl_fachkraft_prog_1"),

    # uiOutput(ns("dynamic_popover")),
    # tags$a(paste0("Was bedeuten die Szenarien"), icon("info-circle"), id="dynamic_popover"),

    br(), br(),

    shinyBS::bsPopover(id="ih_fachkraft_prog_alle_1", title="",
                       content = paste0("Die erste Darstellung zeigt: wenn sich die Zahl an jungen MINT-Fachkräften, z. B. durch erfolgreiche MINT-Bildungsangebote, noch einmal positiver weiterentwickelt, als in den letzten 10 Jahren, rund 700.000 zusätzliche MINT-Fachkräfte gewonnen werden können. Ein Einbruch im Zustorm junger MINT'ler könnte dagegen einen Verlust von rund 800.000 Fachkräften in MINT bis 2037 bedeuten."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_fachkraft_prog_alle_1")

  )
}

#' fachkraft_item_prog Server Functions
#'
#' @noRd
mod_fachkraft_item_prog_alle_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # observeEvent(input$fachkraft_item_prog_wirkhebel, {
    #   r$fachkraft_item_prog_wirkhebel <- input$fachkraft_item_prog_wirkhebel
    #
    #   new_popup_content <- switch(input$fachkraft_item_prog_wirkhebel,
    #                               "MINT-Bildung" = paste0("Positives Szenario = Es wird angenommen, dass sich die Zunahme von MINT-Beschäftigten u35 zwischen 2012-2022 so in den nächsten Jahren fortsetzt", br(), br(), "Negatives Szenario = Es wird angenommen, dass sich die Zunahme von MINT-Beschäftigten u35 zwischen 2012-2022 umkehrt und es zu einer Abnahme kommt."),
    #                               "Frauen in MINT" = "Test-Text",
    #                               "Internationale MINT-Fachkräfte" = "Test-Text2",
    #                               "Beteiligung älterer MINT-Fachkräfte" = "Test-Text3")
    #
    #   runjs(sprintf('$("#%s").attr("data-content", "%s");', ns("erkl_fachkraft_prog_1"), new_popup_content))
    #   runjs(sprintf('$("#%s").popover("hide");', ns("erkl_fachkraft_prog_1")))
    #   runjs(sprintf('$("#%s").popover("show");', ns("erkl_fachkraft_prog_1")))
    # })


    observeEvent(input$fachkraft_item_prog_alle_wirkhebel, {
      r$fachkraft_item_prog_alle_wirkhebel <- input$fachkraft_item_prog_alle_wirkhebel

      # new_popup_content <- switch(input$fachkraft_item_prog_alle_wirkhebel,
      #                             "Gesamteffekt" = paste0("Positives Szenario = positiver Zuwachs im MINT-Nachwuchs der letzten 10 Jahre setzt sich fort und verdoppelt sich bei den Mädchen und jungen Frauen. Auch der positive Zuwachs in der Weiterbeschäftigung von Arbeitskräften ü60 der letzten 10 Jahre setzt sich fort. Für ausländische MINT-Fachkräfte zeigt sich das „hohe Zuwanderung“-Szenario der 15. Koordinierten Bevölkerungsvorausberechnung des statistischen Bundesamts.", br(), br(), "Negatives Szenario = Umkehr des Wachstumstrends im MINT-Nachwuchs, von jungen Frauen in MINT und in der Weiterbeschäftigung von Arbeitskräften ü60 der letzten 10 Jahre. Für ausländische MINT-Fachkräfte zeigt sich das „niedrige Zuwanderung“-Szenario der 15. Koordinierten Bevölkerungsvorausberechnung des statistischen Bundesamts."),
      #                             "MINT-Bildung" = paste0("Positives Szenario = Es wird angenommen, dass sich die Zunahme von MINT-Beschäftigten u35 zwischen 2012-2022 so in den nächsten Jahren fortsetzt", br(), br(), "Negatives Szenario = Es wird angenommen, dass sich die Zunahme von MINT-Beschäftigten u35 zwischen 2012-2022 umkehrt und es zu einer Abnahme kommt."),
      #                             "Frauen in MINT" = "Test-Text",
      #                             "Internationale MINT-Fachkräfte" = "Test-Text2",
      #                             "Beteiligung älterer MINT-Fachkräfte" = "Test-Text3")

      # # Use shinyjs to run JavaScript and update the popover content
      # runjs(sprintf('$("#%s").attr("data-content", `%s`);', ns("erkl_fachkraft_prog_1"), new_popup_content))
      # runjs(sprintf('$("#%s").popover("dispose").popover({content: `%s`});', ns("erkl_fachkraft_prog_1"), new_popup_content))

      # output$dynamic_popover <- renderUI({
      #   tagList(
      #     shinyBS::bsPopover(id=ns("dynamic_popover"), title="",
      #                        content = new_popup_content,
      #                        placement = "top",
      #                        trigger = "hover")
      #   )
      # })

      # runjs(sprintf('$("#%s").popover("show");', ns("dynamic_popover_link")))

    })

  })
}

## To be copied in the UI
# mod_fachkraft_item_prog_alle_ui("fachkraft_item_prog_alle_1")

## To be copied in the server
# mod_fachkraft_item_prog_alle_server("fachkraft_item_prog_alle_1")

