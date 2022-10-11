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

    # Banner
    fluidRow(
      shinydashboard::box(
        width = 12,
        img(src='www/Banner_breiter_Quellen.jpg',
            class = "img-responsive",
            #height = "150px", width = "150px",
            alt = "Banner Quellen",
            style="display: block; margin-left: auto; margin-right: auto;"
        ))),

    fluidRow(
      shinydashboard::box(
        title = "Datenpool und Quellen",
        width = 12,
        p(style = "text-align: justify; font-size = 16px",
          "Ziel dieses Projektes ist es, die vorhandenen Statistiken über MINT in Deutschland in einem Datenpool zu bündeln
          und über das MINT-DataLab zur weiteren Nutzung zur Verfügung zu stellen."
        ),

        p(style = "text-align: justify; font-size = 16px",
          span("Unser", tags$b(span("Datenpool", style = "color:#b16fab")), "besteht aktuell aus Statistiken der Bundesagentur für Arbeit, des
                         Statistischen Bundesamtes und der Kulturministerkonferenz. Weitere Datenquellen werden im Laufe
                         der Zeit integriert.")
        ),

        p(br(),
          tags$b(span("Schulbildung", style = "color:#154194")),
          br(),
          tags$a(href="https://www.kmk.org/", "Seite der Kultusministerkonferenz", target = "_blank"), br(),
          tags$a(href="https://www.kmk.org/themen/allgemeinbildende-schulen/unterrichtsfaecher/mathematik-informatik-naturwissenschaften-technik-mint.html", "Definition MINT-Fächer an Schulen", target = "_blank"),
          br(),
          "Zu den MINT-Fächern in der Schule gehören Mathematik, Biologie, Chemie, Physik, Informatik und andere naturwissenschaftlich-technische Fächer.", br(), br()
            ),

        p(tags$b(span("Studium", style = "color:#154194")),
          br(),
          tags$a(href="https://www.destatis.de/DE/Home/_inhalt.html", "Seite des Statistischen Bundesamtes ", target = "_blank"),
          br(),
          tags$a(href="https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bildung-Forschung-Kultur/Hochschulen/Tabellen/studierende-mint-faechern.html", "Definition MINT-Studienfächer", target = "_blank"),
          br(),
          "Zu den MINT-Studienfächern werden folgende Fächergruppen bzw. Studienbereiche der Hoschulstatistik gezählt:",
          br(),
          tags$ul(
            tags$li("Ingenieurwissenschaften"),
            "Ingenieurwissenschaften allgemein, Bergbau / Hüttenwesen, Maschinenbau / Verfahrenstechnik, Elektrotechnik,
          Verkehrstechnik / Nautik, Architektur / Innenarchitektur, Raumplanung, Bauingenieurwesen, Vermessungswesen,
          Wirtschaftsingenieurwesen mit ingenieurwissenschaftlichem. Schwerpunkt",
            tags$li("Informatik"),
            tags$li("Mathematik & Naturwissenschaften"),
            "Mathematik / Naturwissenschaften allgemein, Mathematik, Informatik, Physik / Astronomie, Chemie, Pharmazie, Biologie, Geowissenschaften, Geographie"
          ), br()
        ),

        p(tags$b(span("Berufsbildung und Arbeitsmarkt", style = "color:#154194")),
          br(),
          icon = icon("arrow-right"),
          tags$a(href="https://statistik.arbeitsagentur.de/DE/Home/home_node.html;jsessionid=B2B7423A23D5B6A5A4C301096D0ABDAB", "Seite der Bundesagentur für Arbeit", target="_blank"), br(),
          tags$a(href="https://statistik.arbeitsagentur.de/DE/Statischer-Content/Statistiken/Themen-im-Fokus/Berufe/Generische-Publikationen/Broschuere-MINT.pdf?__blob=publicationFile", "Definition der MINT-Berufe", target = "_blank"), br(),
          "Das spezifische Berufsaggregat 'MINT-Berufe' umfasst alle Tätigkeiten, für deren Ausübung ein hoher Anteil an Kenntnissen
          aus den Bereichen Mathematik, Informatik, Naturwissenschaften und/oder Technik erforderlich ist. Dabei wird auch das Bauen und
          Instandhalten technischer Anlagen und Geräte als zentraler Bestandteil einer Tätigkeit zu den MINT Qualifikationen gezählt. Das Berufsaggregat 'MINT-Berufe'
          umfasst neben den hoch qualifizierten MINT-Berufen auch die sogenannten mittelqualifizierten MINT-Berufe.", br(),
          tags$a(href="https://statistik.arbeitsagentur.de/DE/Statischer-Content/Grundlagen/Methodik-Qualitaet/Methodenberichte/Uebergreifend/Generische-Publikationen/Hintergrundinfo-Anpassung-Berufsaggregat-MINT-Berufe.pdf?__blob=publicationFile", "Weitere Informationen zur Definition der MINT-Berufe", target ="_blank"),
          br(), br(), br()),

        # p(style = "text-align: justify; font-size = 16px",
        #   tags$ul(
        #     tags$li(tags$a(href="https://www.destatis.de/DE/Home/_inhalt.html", "Statistisches Bundesamt", target="_blank"), " : Zahlen zum Studium"),
        #     tags$li(tags$a(href="https://www.destatis.de/DE/Home/_inhalt.html", "Kultusministerkonferenz", target="_blank"), " : Zahlen zur Schulbildung"),
        #     tags$li(tags$a(href="https://statistik.arbeitsagentur.de/DE/Home/home_node.html;jsessionid=B2B7423A23D5B6A5A4C301096D0ABDAB", "Bundesagentur für Arbeit", target="_blank"), " : Arbeitsmarktdaten")
        #   )),

         p(style = "text-align: justify; font-size = 16px",
          "Die Nutzungsbedingungen der Datengeber erlauben die Verwendung der Daten und grafische Aufbereitung sowie die auszugsweise Weitergabe. Eine vollständige Weitergabe der überlassenen Datensätze ist nicht gestattet. Bei Interesse an den kompletten Datensätzen müssen die Datengeber direkt kontaktiert werden."
        ),

      ))


    # fluidRow(
    #   shinydashboard::box(
    #     #title = span("Quellen und Hinweise", style = "color:#154194; font-size: 50px"),
    #     width = 12,
    #     # column(width = 9,
    #     tags$h2("Quellen & Hinweise"),
    #     p(style = "text-align: justify; font-size = 16px",
    #       "Hier finden Sie Verweise zu den den Institutionen, wo die im MINT-Datalab verwendeten Daten angefragt wurden. Darüber hinaus listen wir Links zu interesanten weiterführenden Quellen auf.")
    #   )),
    # fluidRow(
    #   shinydashboard::box(
    #     title = "Quellen",
    #     width = 12,
    #     # p(style = "text-align: justify; font-size = 16px",
    #     #   span(tags$b(span("Quelle der Daten:", style = "color:#b16fab")))),
    #
    #     p(style = "text-align: justify; font-size = 16px",
    #       tags$ul(
    #         tags$li(tags$a(href="https://www.destatis.de/DE/Home/_inhalt.html", "Statisitisches Bundesamt", target="_blank"), " : Zahlen zum Studium"),
    #         tags$li(tags$a(href="https://www.destatis.de/DE/Home/_inhalt.html", "Kultusministerkonferenz", target="_blank"), " : Zahlen zur Schulbildung"),
    #         tags$li(tags$a(href="https://statistik.arbeitsagentur.de/DE/Home/home_node.html;jsessionid=B2B7423A23D5B6A5A4C301096D0ABDAB", "Bundesagentur für Arbeit", target="_blank"), " : Arbeitsmarktdaten")
    #       )
    #     )
    #
    #   )),
    #
    # fluidRow(
    #   shinydashboard::box(
    #     title = "Hinweise",
    #     width = 12,
    #     # p(style = "text-align: justify; font-size = 16px","Test"),
    #
    #     p(style = "text-align: justify; font-size = 16px",
    #       tags$ul(
    #         tags$li(tags$a(href="https://informatik-monitor.de/", "Informatik-Monitor", target="_blank"), " : Daten Zum Status von Informatik als Pflichtfach"),
    #         tags$li(tags$a(href="https://www.iqb.hu-berlin.de/bt/BT2018/", "IQB-Bildungstrend 2018", target="_blank"), " : Erhebung und Vergelich von Kompetenzen in Mathe und Naturwissenschaften von Schülern "),
    #         tags$li("")
    #       )
    #     )
    #
    #   ))

    ,
    tags$footer(style="text-align: justify;background-color:white",

                div(style="display: inline-block;position: relative;padding: 1em;",

                    tags$a(href="https://mint-vernetzt.de/",
                           img(src='www/MINTv_tranparent.png',
                               class = "img-responsive",
                               height = "100px", width = "100px",
                               alt = "Logo MINT", target="_blank",
                               style="display: inline-block; margin-left: auto; margin-right:10%;"))),

                div(style="display: inline-block;position: relative;padding: 1em;",

                    p(tags$a("Impressum", href="#shiny-tab-impressum", "data-toggle" = "tab")," | ",
                      tags$a("Kontakt", href="#shiny-tab-kontakt", "data-toggle" = "tab")," | ",
                      tags$a("Datenschutz", href="#shiny-tab-datenschutz", "data-toggle"="tab"),HTML('&nbsp;'),HTML('&nbsp;'),
                      "Copyright © 2022. Alle Rechte vorbehalten Stifterverband")),

                div(style="display: inline-block;position: relative;padding: 1em;",

                    tags$a(href="https://www.bmbf.de/bmbf/de/home/home_node.html",
                           img(src='www/BMBF-Logo_transp1.png',

                               class = "img-responsive",

                               height = "200px", width = "200px",

                               alt = "Logo BMBF", target="_blank",

                               style="display: inline-block; margin-left: auto; margin-right: auto;"))),

                div(style="display: inline-block;width: 100%;",

                    " ")


    ))

}

## To be copied in the UI
# mod_quellen_ui("quellen_1")

## To be copied in the server
# mod_quellen_server("quellen_1")
