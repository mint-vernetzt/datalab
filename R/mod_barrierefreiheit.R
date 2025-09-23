#' datenschutz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.


#' kontakt Server Functions
#'
#' @noRd
mod_barrierefreiheit_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}


#' @noRd
#'
#' @importFrom shiny NS tagList
mod_barrierefreiheit_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
        width = 12,
        column(
          width = 8,
        tags$h1("Barrierefreiheitserklärung für die Website des MINT-DataLab"),
        p("Stand: 23.09.2025"),

        tags$h2("1. Geltungsbereich dieser Erklärung"),
        p("Diese Erklärung zur Barrierefreiheit gilt für die unter ",
          tags$a(href="https://mint-vernetzt.shinyapps.io/datalab/",
                 "https://mint-vernetzt.shinyapps.io/datalab/",
                 target = "_blank"),
          " erreichbare Website des MINT-DataLab von MINTvernetzt."),

        tags$h2("2. Rechtsgrundlagen"),
        p("Diese Erklärung basiert auf den Vorgaben des Barrierefreiheitsstärkungsgesetzes
          (BFSG) gemäß EU-Richtlinie 2016/2102 und entsprechenden nationalen Rechtsvorschriften."),

        tags$h2("3. Stand der Vereinbarkeit mit den Anforderungen"),
        p("Die Website des MINT-DataLabs ist bestrebt, ihre digitale Infrastruktur
        barrierefrei zugänglich zu machen. Aktuell ist die Website noch nicht vollständig
        barrierefrei. Wir arbeiten kontinuierlich daran, die Zugänglichkeit weiter zu verbessern.",
          br(), br(),
        "Es bestehen insbesondere Einschränkungen in folgenden Bereichen:", br(), br(),
        "Interaktive Grafiken erstellt durch das Visualisierungspaket \"highcharter\"
        sowie Navigationselemente der interaktiven Grafiken erstellt mit dem Paket
        \"shiny\" sind zum Teil nicht barrierefrei."),

        tags$strong("Dazu gehören:"),br(),br(),
        tags$ul(
          tags$li("Alternative Informationen vergleichbar mit ALT-Texten für
                  die interaktiven Grafiken fehlen und werden ergänzt werden."),
          tags$li("Eine Navigation über die Tastaturbedienung in der
                  Dropdown-Auswahloptionen mit Mehrfachauswahl zur Anpassung
                  der interaktiven Grafiken ermöglichen kein Verlassen der
                  Dropdown-Umgebung mehr."),
          tags$li("Eine Ansteuerung der interaktiven Grafiken, z. B. für die
                  Nutzung der Download-Optionen, ist bei einer Navigation über
                  die Tastaturbedienung aktuell nicht möglich."),
          tags$li("Eine HTML- und ARIA-Auszeichnungen der interaktiven Elemente
                  der Seite fehlt aktuell oder liegt nur unvollständig bzw.
                  fehlerhaft vor, was Screenreader-Nutzenden Probleme bereitet.")
        ),

        tags$strong("Weitere nicht barrierefreie Inhalte:"),br(),br(),
        p(tags$strong("In seltenen Fällen"), " sind Inhalte derzeit nicht barrierefrei,
          darunter:"),
        tags$ul(
          tags$li("Fehlende Textalternativen: Einige Bilder und Nicht-Text-Inhalte
                  haben ggf. unzureichende oder keine beschreibenden Alternativtexte."),
          tags$li("Schwacher Kontrast: Erhöhter Kontrast und Kontraste von
                  Grafiken und Bedienelementen sind nicht immer ausreichend."),
          tags$li("Fokus-Markierung bei Tastaturbedienung ggf. eingeschränkt:
                  Die Fokus-Markierung kann u.U. in Teilen der Website fehlen."),
          tags$li("Unklare oder fehlende Seitentitel in den Überblicksseiten:
                  Der Titel der Webseiten ist nicht immer sinnvoll gewählt oder fehlt."),
          tags$li("Linkzwecke teils nicht verständlich: Links sind ohne zusätzlichen
                  Kontext oft nicht eindeutig erkennbar.")
        ),

        tags$h2("4. Maßnahmen zur Verbesserung der Barrierefreiheit"),
        p("Wir setzen kontinuierlich Maßnahmen um, um die Barrierefreiheit der Website zu verbessern:",
          br(), br(),
          tags$strong("Regelmäßige Überprüfung:"), " Durchführung von Barrierefreiheitstests
          gemäß den Richtlinien der Barrierefreien Informationstechnik-Verordnung (BITV 2.0).",
          br(),
          tags$strong("Sensibilisierung:"), " Das Entwicklungsteam wird für die Erhöhung
          der Barrierefreiheit geschult und arbeitet aktuell an einem
          barrierefreundlicheren Umgang für die interaktiven Grafiken
          (explizit highcharter-Grafiken und Shiny-Bedienungselemente).",
          br(),
          tags$strong("Nutzer:innenfeedback"), " Integration von Feedback von
          Nutzer:innen zur Identifikation und Behebung von Barrieren."),

        tags$h2("5. Feedback und Kontakt – Barrieren melden"),
        p("Sollten Mängel in Bezug auf die barrierefreie Gestaltung unserer
        Plattform auffallen oder benötigen Sie Informationen in barrierefreiem Format,
        kontaktiere Sie uns bitte:",
          br(), br(),
          tags$strong("E-Mail: "),
          tags$a(href = "mailto:katharina.brunner@mint-vernetzt.de?subject= Barrierefreiheit MINT-Datalab",
                 "katharina.brunner@mint-vernetzt.de"),
          br(), br(),
          "Wir bemühen uns, Ihre Anfragen zeitnah zu bearbeiten."),

        tags$h2("6. Durchsetzungsverfahren"),
        p("Falls Sie keine zufriedenstellende Antwort auf Ihre Anfrage zur
        Barrierefreiheit erhaltet, können Sie sich an die zuständige
        Durchsetzungsstelle wenden:",
          br(), br(),
          tags$b("Schlichtungsstelle BGG "), "beim Beauftragten der Bundesregierung
          für die Belange von Menschen mit Behinderungen",
          br(), br(),
          tags$b("Adresse: "), "Mauerstraße 53, 10117 Berlin", br(),
          tags$b("Telefon: "), "030 18 527-2805", br(),
          tags$b("E-Mail: "), tags$a(href = "mailto:info@schlichtungsstelle-bgg.de",
                                     "info@schlichtungsstelle-bgg.de"), br(),
          tags$b("Internet: "), tags$a(href = "www.schlichtungsstelle-bgg.de",
                                       target = "_blank",
                                       "www.schlichtungsstelle-bgg.de")
        ),

        tags$h2("7. Weitere Informationen"),
        p("Das MINT-DataLab ist ein Projekt von MINTvernetzt, das darauf abzielt,
          die MINT-Community in Deutschland durch das Bündeln und Aufbereiten
          von MINT-Statistiken in ihrer Arbeit zu unterstützen.
          Weitere Informationen finden Sie auf der MINTvernetzt-Website: ",
          tags$a(href="https://www.mint-vernetzt.de"),
          br(),  br(),
          tags$b("Hinweis: "), "Diese Erklärung wird regelmäßig überprüft und aktualisiert,
          um den aktuellen Stand der Barrierefreiheit unserer Plattform widerzuspiegeln.")
      )
    )
    ),
    funct_footer()
  )
}
