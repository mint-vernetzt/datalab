#' quellen UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_quellen_ui <- function(id){
  ns <- NS(id)
  tagList  (
    fluidRow(
      shinydashboard::box(
        #title = span("Quellen und Hinweise", style = "color:#154194; font-size: 50px"),
        width = 12,
        # column(width = 9,
        tags$h1("QUELLEN & HINWEISE"),
        p(style = "text-align: justify; font-size = 16px",
          "Hier finden Sie Verweise zu den den Institutionen, wo die im MINT-Datalab verwendeten Daten angefragt wurden. Darüber hinaus listen wir Links zu interesanten weiterführenden Quellen auf.")
      )),
    fluidRow(
      shinydashboard::box(
        title = "Quellen",
        width = 12,
        # p(style = "text-align: justify; font-size = 16px",
        #   span(tags$b(span("Quelle der Daten:", style = "color:#b16fab")))),

        p(style = "text-align: justify; font-size = 16px",
          tags$ul(
            tags$li(tags$a(href="https://www.destatis.de/DE/Home/_inhalt.html", "Statisitisches Bundesamt", target="_blank"), " : Zahlen zum Studium"),
            tags$li(tags$a(href="https://www.destatis.de/DE/Home/_inhalt.html", "Kultusministerkonferenz", target="_blank"), " : Zahlen zur Schulbildung"),
            tags$li(tags$a(href="https://statistik.arbeitsagentur.de/DE/Home/home_node.html;jsessionid=B2B7423A23D5B6A5A4C301096D0ABDAB", "Bundesagentur für Arbeit", target="_blank"), " : Arbeitsmarktdaten")
          )
        )

      )),

    fluidRow(
      shinydashboard::box(
        title = "Hinweise",
        width = 12,
        # p(style = "text-align: justify; font-size = 16px","Test"),

        p(style = "text-align: justify; font-size = 16px",
          tags$ul(
            tags$li(tags$a(href="https://informatik-monitor.de/", "Informatik-Monitor", target="_blank"), " : Daten Zum Status von Informatik als Pflichtfach"),
            tags$li(tags$a(href="https://www.iqb.hu-berlin.de/bt/BT2018/", "IQB-Bildungstrend 2018", target="_blank"), " : Erhebung und Vergelich von Kompetenzen in Mathe und Naturwissenschaften von Schülern "),
            tags$li("")
          )
        )

      )))

}

## To be copied in the UI
# mod_quellen_ui("quellen_1")

## To be copied in the server
# mod_quellen_server("quellen_1")
