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
        img(src='www/Banner_Hinweise.jpg',
            class = "img-responsive",
            #height = "150px", width = "150px",
            alt = "Banner Quellen",
            style="display: block; margin-left: auto; margin-right: auto;"
        ))),

    fluidRow(
      shinydashboard::box(
        width = 9,
        title = "Definition MINT",
                p(style = "text-align: justify; font-size = 16px",
          "MINT ist eine Abkürzung aus den Anfangsbuchstaben bestimmter Schul- und Studienfächer bzw. Berufe.
          Es steht als Sammelbegriff für die Felder Mathematik, Informatik, Naturwissenschaften und Technik."
        ), br(), br(),

        img(src='www/Definition_MINT.jpg',
            class = "img-responsive",
            #height = "150px", width = "150px",
            alt = "Def MINT",
            style="display: block; margin-left: auto; margin-right: auto;"
        ), br(),
        tags$a(href="https://www.kmk.org/themen/allgemeinbildende-schulen/unterrichtsfaecher/mathematik-informatik-naturwissenschaften-technik-mint.html", "Mehr Infos zur Definition MINT-Fächer in der Schule", target = "_blank"),
        br(),
        tags$a(href="https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bildung-Forschung-Kultur/Hochschulen/Tabellen/studierende-mint-faechern.html", "Mehr Infos zur Definition MINT-Studienfächer", target = "_blank"),
        br(),
        tags$a(href="https://statistik.arbeitsagentur.de/DE/Statischer-Content/Statistiken/Themen-im-Fokus/Berufe/Generische-Publikationen/Broschuere-MINT.pdf?__blob=publicationFile", "Mehr Infos zur Definition der MINT-Berufe", target = "_blank"), br(),
        tags$a(href="https://statistik.arbeitsagentur.de/DE/Statischer-Content/Grundlagen/Methodik-Qualitaet/Methodenberichte/Uebergreifend/Generische-Publikationen/Hintergrundinfo-Anpassung-Berufsaggregat-MINT-Berufe.pdf?__blob=publicationFile", "Aktuelle Infos zur Anpassung der Definition der MINT-Berufe", target ="_blank"),

    ),

      shinydashboard::box(
          width = 3,
          title = "Unsere Datengeber:innen",
          img(src='www/Logo_BA.png',
              class = "img-responsive",
              #height = "150px", width = "150px",
              alt = "Logo BA",
              style="display: block; margin-left: auto; margin-right: auto;"
          ), br(),
          tags$a(href="https://statistik.arbeitsagentur.de/DE/Home/home_node.html;jsessionid=B2B7423A23D5B6A5A4C301096D0ABDAB",
                 "Seite der Bundesagentur für Arbeit", target="_blank"), br(),
          br(),
          br(),
          img(src='www/Logo_Destatis.png',
              class = "img-responsive",
              #height = "150px", width = "150px",
              alt = "Logo Destatis",
              style="display: block; margin-left: auto; margin-right: auto;"
          ),
          br(),
          tags$a(href="https://www.destatis.de/DE/Home/_inhalt.html", "Seite des Statistischen Bundesamtes ", target = "_blank"),
          br(),

          br(), br(),
          img(src='www/Logo_KMK.png',
              class = "img-responsive",
              #height = "150px", width = "150px",
              alt = "Logo KMK",
              style="display: block; margin-left: auto; margin-right: auto;"
          ),br(),
          tags$a(href="https://www.kmk.org/", "Seite der Kultusministerkonferenz", target = "_blank"),
          br(),br(),
          img(src='www/Logo_IQB.png',
              class = "img-responsive",
              #height = "150px", width = "150px",
              alt = "Logo IQB",
              style="display: block; margin-left: auto; margin-right: auto;"
          ),


          )
          ),

    fluidRow(
      shinydashboard::box(
        width = 10,
        title = "Datenpool und Quellen",
        p(style = "text-align: justify; font-size = 16px",
          "Ziel dieses Projektes ist es, die vorhandenen Statistiken über MINT in Deutschland in einem Datenpool zu bündeln
          und über das MINT-DataLab zur weiteren Nutzung zur Verfügung zu stellen. Unser Datenpool besteht aktuell aus Statistiken der Bundesagentur für Arbeit, des
                         Statistischen Bundesamtes und der Kultusministerkonferenz. Weitere Datenquellen werden im Laufe
                         der Zeit integriert."
        ))),

    fluidRow(
      shinydashboard::box(
        width = 10,
        title = "Nutzungsbedingungen der Daten",
        p(style = "text-align: justify; font-size = 16px",
          "Die Nutzungsbedingungen der Datengeber erlauben die Verwendung der Daten und die grafische Aufbereitung
          sowie die auszugsweise Weitergabe. Eine vollständige Weitergabe der überlassenen Datensätze ist nicht gestattet.
          Bei Interesse an den kompletten Datensätzen müssen die Datengeber:innen direkt kontaktiert werden."
        )
      )),







    funct_footer())

}



# "Zu den MINT-Studienfächern werden folgende Fächergruppen bzw. Studienbereiche der Hoschulstatistik gezählt:",
# br(),
# tags$ul(
#   tags$li("Ingenieurwissenschaften"),
#   "Ingenieurwissenschaften allgemein, Bergbau/Hüttenwesen, Maschinenbau/Verfahrenstechnik, Elektrotechnik,
#           Verkehrstechnik/Nautik, Architektur/Innenarchitektur, Raumplanung, Bauingenieurwesen, Vermessungswesen,
#           Wirtschaftsingenieurwesen mit ingenieurwissenschaftlichem. Schwerpunkt",
#   tags$li("Informatik"),
#   tags$li("Mathematik & Naturwissenschaften"),
#   "Mathematik/Naturwissenschaften allgemein, Mathematik, Informatik, Physik/Astronomie, Chemie, Pharmazie, Biologie, Geowissenschaften, Geographie"
# ), br()

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

# p(style = "text-align: justify; font-size = 16px",
#   tags$ul(
#     tags$li(tags$a(href="https://www.destatis.de/DE/Home/_inhalt.html", "Statistisches Bundesamt", target="_blank"), " : Zahlen zum Studium"),
#     tags$li(tags$a(href="https://www.destatis.de/DE/Home/_inhalt.html", "Kultusministerkonferenz", target="_blank"), " : Zahlen zur Schulbildung"),
#     tags$li(tags$a(href="https://statistik.arbeitsagentur.de/DE/Home/home_node.html;jsessionid=B2B7423A23D5B6A5A4C301096D0ABDAB", "Bundesagentur für Arbeit", target="_blank"), " : Arbeitsmarktdaten")
#   )),


## To be copied in the UI
# mod_quellen_ui("quellen_1")

## To be copied in the server
# mod_quellen_server("quellen_1")
