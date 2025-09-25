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
      div(class = "clean-box",
          column(
            width = 12,
            img(src='www/Banner_Hinweise.avif',
                class = "img-responsive",
                alt = "Banner Quellen",
                style="display: block; margin-left: auto; margin-right: auto; margin-bottom: 20px;"
            )))),

    fluidRow(
      shinydashboard::box(
        width = 9,
        h2("Definition MINT"),
                p(style = "text-align: left; font-size = 16px",
          "MINT ist eine Abkürzung aus den Anfangsbuchstaben bestimmter Schul- und Studienfächer bzw. Berufe.
          Sie steht als Sammelbegriff für die Felder Mathematik, Informatik, Naturwissenschaften und Technik.",
          br(),br(),
          "Die genaue Definition davon, was als 'MINT' zählt und was nicht, variiert vor allem für Beschäftigte.
          Die folgende Darstellung versucht, eine Übersicht zu geben."
        ), br(),
        img(src='www/Definition_MINT_Bild.png',
            class = "img-responsive",
            alt = "Definition MINT",
            style="display: block; margin-left: auto; margin-right: auto;"
        )
      ),
      shinydashboard::box(
        width = 3,
        title = "Unsere Datengeber:innen",
        img(src='www/Logo_BA.png',
            class = "img-responsive",
            alt = "Logo BA",
            style="display: block; margin-left: auto; margin-right: auto;"
        ), br(),
        tags$a(href="https://statistik.arbeitsagentur.de/DE/Home/home_node.html;jsessionid=B2B7423A23D5B6A5A4C301096D0ABDAB",
               "Seite der Bundesagentur für Arbeit", target="_blank"), br(),
        br(),
        br(),
        img(src='www/Logo_Destatis.png',
            class = "img-responsive",
            alt = "Logo Destatis",
            style="display: block; margin-left: auto; margin-right: auto;"
        ),
        br(),
        tags$a(href="https://www.destatis.de/DE/Home/_inhalt.html", "Seite des Statistischen Bundesamtes ", target = "_blank"),
        br(),

        br(), br(),
        img(src='www/Logo_KMK.png',
            class = "img-responsive",
            alt = "Logo KMK",
            style="display: block; margin-left: auto; margin-right: auto;"
        ),br(),
        tags$a(href="https://www.kmk.org/", "Seite der Kultusministerkonferenz", target = "_blank"),
        br(),br()
        ),



      shinydashboard::box(
        width = 9,
        title = "Links zu den Definitionsangaben",
        column(4,
        tags$a(href="https://www.kmk.org/themen/allgemeinbildende-schulen/unterrichtsfaecher/mathematik-informatik-naturwissenschaften-technik-mint.html", "Mehr Infos zur Definition MINT-Fächer in der Schule", target = "_blank"),
        br(),
        tags$a(href="https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bildung-Forschung-Kultur/Hochschulen/Tabellen/studierende-mint-faechern.html", "Mehr Infos zur Definition MINT-Studienfächer", target = "_blank"),
        br(),
        tags$a(href="https://web.arbeitsagentur.de/berufenet/ergebnisseite/mint-berufe?mint=alle-mint&page=0", "Mehr Infos zur Definition der MINT-Berufe", target = "_blank"), br(),
        ),
        column(4,
        tags$a(href="https://statistik.arbeitsagentur.de/DE/Statischer-Content/Grundlagen/Methodik-Qualitaet/Methodenberichte/Uebergreifend/Generische-Publikationen/Hintergrundinfo-Anpassung-Berufsaggregat-MINT-Berufe.pdf?__blob=publicationFile", "Aktuelle Infos zur Anpassung der Definition der MINT-Berufe", target ="_blank"),
        br(),
        tags$a(href="https://ec.europa.eu/eurostat/statistics-explained/index.php?title=International_Standard_Classification_of_Education_(ISCED)#ISCED_1997_.28fields.29_and_ISCED-F_2013", "Internationale Klassifikation von Ausbildungen (ISCED-F)", target ="_blank"),
        br(),
        tags$a(href="https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Glossary:Scientists_and_engineers", "Eurostat Einteilung von Naturwissenschaftler:innen und Ingenieur:innen", target ="_blank"),
        )
    ),

    shinydashboard::box(
      width = 3,
      title = "Weitere Datengeber:innen",
      tags$a(href="https://www.bibb.de/", "Bundesinstitut für Berufsbildung", target = "_blank"),
      br(),
      tags$a(href="https://www.iqb.hu-berlin.de/", "Seite des Instituts zur Qualitätsentwicklung im Bildungswesen.(IQB)", target = "_blank"),
      br(),
      tags$a(href="https://www.oecd.org/pisa/", "Seite des Pisa-Programms der OECD", target = "_blank"),
      br(),
      tags$a(href="https://timss2019.org/reports/", "Seite des TIMSS-Reports 2019 der IEA", target = "_blank"),
      br(),
      tags$a(href="https://ec.europa.eu/eurostat/databrowser/explore/all/all_themes?lang=en&display=list&sort=category", "Datenportal von Eurostat", target = "_blank"),
      br(),
      tags$a(href="https://www.oecd-ilibrary.org/statistics", "Datenportal der OECD", target = "_blank"),

    )
    ),

    fluidRow(
      shinydashboard::box(
        width = 9,
        h2("Datenpool und Quellen"),
        p(style = "text-align: left; font-size = 16px",
          "Ziel dieses Projektes ist es, die vorhandenen Statistiken über MINT in Deutschland in einem Datenpools zu bündeln
          und über das MINT-DataLab zur weiteren Nutzung zur Verfügung zu stellen. Die Basis unseres Datenpool sind die amtlichen Statistiken aus Deutschland,
          also die Statistiken der Bundesagentur für Arbeit, des Statistischen Bundesamtes (Destatis) und der Kultusministerkonferenz (KMK).
          Weitere Datenquellen aus dem internationalen Bereich sind Eurostat, die OECD und die IEA.", br(),
          "Im außerschulischen Bereich zeigen wir außerdem Daten von MINTvernetzt und der Stiftung Kinder forschen.
          Für die themenseite 'MINT-Fachkräfte' hat MINTvernetzt durch das IW Köln basierend auf amtlichen Statistiken
          Zukunftsszenarien für die MINT-Fachkräfteentwicklung berechnen lassen.", br(),
          "Wir bauen dabei unsere Datengrundlage stetig weiter aus."
        ))),


    fluidRow(
      shinydashboard::box(
        width = 9,
        h2("Zitationshinweis"),
        p(style = "text-align: left; font-size = 16px",
          "Inhalte des MINT-DataLab können mit folgender Quellenangabe weitergenutzt werden:
          \"MINT-DataLab von MINTvernetzt. https://www.mint-vernetzt.de/mint-datalab/\" (mit Abrufdatum).
          Unter den interaktiven Grafiken sind jeweils eigene Quellenangaben
          mit Verweis auf die Datenquelle angegeben.")
        )
      ),

    fluidRow(
      shinydashboard::box(
        width = 9,
        h2("Nutzungsbedingungen der Daten und Quellenangabe"),
        p(style = "text-align: left; font-size = 16px",
          "Die Nutzungsbedingungen der Datengeber:innen erlauben die Verwendung der Daten und die grafische Aufbereitung
          sowie die auszugsweise Weitergabe. Eine vollständige Weitergabe der überlassenen Datensätze ist nicht gestattet.
          Bei Interesse an den kompletten Datensätzen müssen die Datengeber:innen direkt kontaktiert werden."

        ))),








    funct_footer())

}






## To be copied in the UI
# mod_quellen_ui("quellen_1")

## To be copied in the server
# mod_quellen_server("quellen_1")
