#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'

# app_server <- function(input, output, session) {
#   # Your application server logic
#
#   # Alte Logik
#
#
#    mod_startseite_server("startseite_ui_1")
#
#   mod_argumentation_server("argumentationshilfe_ui_1")
#
#
#   # callModule(mod_home_server, "home_ui_1", data=example_data)
#   mod_home_server("home_ui_1")
#
#
#   # callModule(mod_schule_server, "schule_ui_1", data=example_data, filter_name="schule")
#   mod_schule_server("schule_ui_1")
#
#
#   mod_studium_server("studium_ui_1")
#
#
#   mod_beruf_server("beruf_ui_1")
#
#
#   mod_international_server("international_ui_1")
#
#
#   mod_fachkraft_server("fachkraft_ui_1")
#
#
#   mod_ausserschulisch_server("ausserschulisch_ui_1")
#
#
# }

# app_server <- function(input, output, session) {
#
#   observeEvent(input$tabs, {
#     if (input$tabs == "startseite") {
#       mod_startseite_server("startseite_ui_1")
#     } else if (input$tabs == "argumentationshilfe") {
#       mod_argumentation_server("argumentationshilfe_ui_1")
#     } else if (input$tabs == "home") {
#       mod_home_server("home_ui_1")
#     } else if (input$tabs == "schule") {
#       mod_schule_server("schule_ui_1")
#     } else if (input$tabs == "studium") {
#       mod_studium_server("studium_ui_1")
#     } else if (input$tabs == "beruf") {
#       mod_beruf_server("beruf_ui_1")
#     } else if (input$tabs == "international") {
#       mod_international_server("mod_international_ui_1")
#     } else if (input$tabs == "fachkraft") {
#       mod_fachkraft_server("fachkraft_ui_1")
#     } else if (input$tabs == "ausserschulisch") {
#       mod_ausserschulisch_server("ausserschulisch_ui_1")
#     } else if (input$tabs == "quellen") {
#       mod_quellen_server("quellen_ui_1")
#     } else if (input$tabs == "kontakt") {
#       mod_kontakt_server("kontakt_ui_1")
#     } else if (input$tabs == "impressum") {
#       mod_impressum_server("impressum_ui_1")
#     } else if (input$tabs == "datenschutz") {
#       mod_datenschutz_server("datenschutz_ui_1")
#     }
#   }, ignoreInit = FALSE)  # sorgt dafür, dass Startseite sofort geladen wird
# }
#


##geht
# app_server <- function(input, output, session) {
#   # Your application server logic
#
#   # Alte Logik
#
#   if (is.null(input$tabs)) {
#     updateTabItems(session, "tabs", "startseite")
#   }
#
#   output$main_tab_ui <- renderUI({
#     req(input$tabs)
#     switch(input$tabs,
#            "startseite" = mod_startseite_ui("startseite_ui_1"),
#            "argumentationshilfe" = mod_argumentation_ui("argumentationshilfe_ui_1"),
#            "home" = mod_home_ui("home_ui_1"),
#            "schule" = mod_schule_ui("schule_ui_1"),
#            "studium" = mod_studium_ui("studium_ui_1"),
#            "beruf" = mod_beruf_ui("beruf_ui_1"),
#            "ausserschulisch" = mod_ausserschulisch_ui("ausserschulisch_ui_1"),
#            "international" = mod_international_ui("mod_international_ui_1"),
#            "fachkraft" = mod_fachkraft_ui("fachkraft_ui_1"),
#            "quellen" = mod_quellen_ui("quellen_ui_1"),
#            "kontakt" = mod_kontakt_ui("kontakt_ui_1"),
#            "impressum" = mod_impressum_ui("impressum_ui_1"),
#            "datenschutz" = mod_datenschutz_ui("datenschutz_ui_1")
#     )
#   })
#
#   observeEvent(input$tabs, {
#     if (input$tabs == "startseite") {
#       mod_startseite_server("startseite_ui_1")
#     } else if (input$tabs == "argumentationshilfe") {
#       mod_argumentation_server("argumentationshilfe_ui_1")
#     } else if (input$tabs == "home") {
#       mod_home_server("home_ui_1")
#     } else if (input$tabs == "schule") {
#       mod_schule_server("schule_ui_1")
#     } else if (input$tabs == "studium") {
#       mod_studium_server("studium_ui_1")
#     } else if (input$tabs == "beruf") {
#       mod_beruf_server("beruf_ui_1")
#     } else if (input$tabs == "ausserschulisch") {
#       mod_ausserschulisch_server("ausserschulisch_ui_1")
#     } else if (input$tabs == "international") {
#       mod_international_server("mod_international_ui_1")
#     } else if (input$tabs == "fachkraft") {
#       mod_fachkraft_server("fachkraft_ui_1")
#     } else if (input$tabs == "quellen") {
#       mod_quellen_server("quellen_ui_1")
#     } else if (input$tabs == "kontakt") {
#       mod_kontakt_server("kontakt_ui_1")
#     } else if (input$tabs == "impressum") {
#       mod_impressum_server("impressum_ui_1")
#     } else if (input$tabs == "datenschutz") {
#       mod_datenschutz_server("datenschutz_ui_1")
#     }
#   }, ignoreInit = FALSE)
#
#
#
# }

##UI persistent
app_server <- function(input, output, session) {

  # Tabspeicher: Merkt sich, welche Tabs bereits gerendert wurden
  tab_loaded <- reactiveValues()


# Initiale Tab-Auswahl auf "startseite", wenn keine Auswahl da
observe({
  if (is.null(input$tabs)) {
    updateTabItems(session, "tabs", "startseite")
  }
})

  # Initialisiere Serverlogik EINMALIG beim App-Start
  mod_startseite_server("startseite_ui_1")
  mod_argumentation_server("argumentationshilfe_ui_1")
  mod_home_server("home_ui_1")
  mod_schule_server("schule_ui_1")
  mod_studium_server("studium_ui_1")
  mod_beruf_server("beruf_ui_1")
  mod_ausserschulisch_server("ausserschulisch_ui_1")
  mod_international_server("mod_international_ui_1")
  mod_fachkraft_server("fachkraft_ui_1")

  # Render einmalig je Tab UI-Ausgabe (Caching)
  output$ui_startseite <- renderUI({ mod_startseite_ui("startseite_ui_1") })
  output$ui_argumentationshilfe <- renderUI({ mod_argumentation_ui("argumentationshilfe_ui_1") })
  output$ui_home <- renderUI({ mod_home_ui("home_ui_1") })
  output$ui_schule <- renderUI({ mod_schule_ui("schule_ui_1") })
  output$ui_studium <- renderUI({ mod_studium_ui("studium_ui_1") })
  output$ui_beruf <- renderUI({ mod_beruf_ui("beruf_ui_1") })
  output$ui_ausserschulisch <- renderUI({ mod_ausserschulisch_ui("ausserschulisch_ui_1") })
  output$ui_international <- renderUI({ mod_international_ui("mod_international_ui_1") })
  output$ui_fachkraft <- renderUI({ mod_fachkraft_ui("fachkraft_ui_1") })
  output$ui_quellen <- renderUI({ mod_quellen_ui("quellen_ui_1") })
  output$ui_kontakt <- renderUI({ mod_kontakt_ui("kontakt_ui_1") })
  output$ui_impressum <- renderUI({ mod_impressum_ui("impressum_ui_1") })
  output$ui_datenschutz <- renderUI({ mod_datenschutz_ui("datenschutz_ui_1") })

  # Dynamisches UI mit Caching der geladenen Tabs
  output$main_tab_ui <- renderUI({
    req(input$tabs)

    # Nur das erste Mal markieren und generieren
    if (is.null(tab_loaded[[input$tabs]])) {
      tab_loaded[[input$tabs]] <- TRUE
    }
    # 👇 Loader ausblenden, nachdem UI erstellt wurde
    session$sendCustomMessage("tabDone", list())

    # Immer zurückgeben: Die jeweils einmal erzeugte UI-Ausgabe
    switch(input$tabs,
           "startseite" = uiOutput("ui_startseite"),
           "argumentationshilfe" = uiOutput("ui_argumentationshilfe"),
           "home" = uiOutput("ui_home"),
           "schule" = uiOutput("ui_schule"),
           "studium" = uiOutput("ui_studium"),
           "beruf" = uiOutput("ui_beruf"),
           "ausserschulisch" = uiOutput("ui_ausserschulisch"),
           "international" = uiOutput("ui_international"),
           "fachkraft" = uiOutput("ui_fachkraft"),
           "quellen" = uiOutput("ui_quellen"),
           "kontakt" = uiOutput("ui_kontakt"),
           "impressum" = uiOutput("ui_impressum"),
           "datenschutz" = uiOutput("ui_datenschutz"),
           div("Unbekannter Tab – bitte Navigation nutzen.")
    )
  })
}


