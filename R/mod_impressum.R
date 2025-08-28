#' impressum UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_impressum_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' impressum Server Functions
#'
#' @noRd
mod_impressum_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}


mod_impressum_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
        width = 12,
        tags$h2("Impressum"),
        p(style = "text-align:left; font-size = 16px",
          span("Angaben erfolgen nach § 5 TMG", br(),
               br(),
              "Stifterverband für die Deutsche Wissenschaft e.V.", br(),
              "Baedekerstraße 1", br(),
              "45128 Essen", br(),
              "T 0201 8401-0", br(),
              br(),
              "Vorstand (i.S.d.V.)", br(),
              "Prof. Dr. Michael Kaschke", br(),
              "Dr. Simone Bagel-Trah", br(),
              "Dr. Martin Brudermüller", br(),
              "Dr. phil. Nicola Leibinger-Kammüller", br(),
              "Dr. Cornelius Riese", br(),
              "Dr. rer. oec. Reinhard Christian Zinkann", br(),
              "Dr. Volker Meyer-Guckel", br(),
              br(),
              "Eingetragen beim Amtsgericht Essen VR 5776", br(),
              "St.-Nr.: 112/5950/0747", br(),
              "USt-IdNr.: DE 119 692 167", br(),
              br(),
              tags$b(span("Verantwortlich im Sinne des Medienrechts")), br(),
              "Dr. Pascal Hetze", br(),
              "T 030 322982-506", br(),
              "E-Mail: pascal.hetze[at]stifterverband.de", br(),
              br(),
              tags$b(span("Haftungsausschluss")), br(),
              "Die Zusammenstellung der Informationen erfolgte mit der gebotenen Sorgfalt. Gleichwohl Übernehmen wir keinerlei Haftung, aus welchem Rechtsgrund auch immer, für die Richtigkeit, Aktualität und Vollständigkeit der übermittelten Informationen. Diese Internetseite enthält Verweise auf Internetseiten, die von Dritten eingerichtet wurden. Der Stifterverband für die Deutsche Wissenschaft e.V. (Stifterverband) hat keinerlei Kontrolle über die Internetseiten und die dort angebotenen Informationen, Waren oder Dienstleistungen. Der Stifterverband übernimmt daher keinerlei Verantwortung, aus welchem Rechtsgrund auch immer, für den Inhalt der Internetseiten Dritter. Das Herstellen einer Verbindung zu diesen Internetseiten geschieht auf eigene Gefahr des Benutzers. Der Stifterverband behält sich das Recht vor, die auf dieser Internetseite angebotenen Informationen, Produkte oder Dienstleistungen ohne gesonderte Ankündigung jederzeit zu verändern oder zu aktualisieren. Für gegebenenfalls bestehende oder künftig entstehende Rechtsverhältnisse ist ausschließlich deutsches Recht anwendbar und sind nur deutsche Gerichte zuständig.", br(),
              br(),
              tags$b(span("Initiale Programmierung")), br(),
              "Statworx GmbH ", br(),
              "Hanauer Landstraße 150", br(),
              "60314 Frankfurt am Main", br(),
              br(),
              tags$b(span("Konzeption, Weiterentwicklung und Ausbau des MINT-DataLabs")), br(),
              "Stifterverband für die Deutsche Wissenschaft e.V."
          )
        ))),
    funct_footer())

}

## To be copied in the UI
# mod_impressum_ui("impressum_1")

## To be copied in the server
# mod_impressum_server("impressum_1")
