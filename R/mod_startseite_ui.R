#' Startseite UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList


mod_startseite_start_ui <- function(id){
  ns <- NS(id)
  tagList(


    fluidRow(
      div(class = "clean-box",
          column(
            width = 12,
        img(src='www/Banner_Willkommen.avif',
            class = "img-responsive",
            height = "300px",
            alt = "Banner Start",
            style="display: block; margin-left: auto; margin-right: auto;"
        )))),

    # Einleitungstext ----
    fluidRow(
      div(class = "clean-box",
          column(
            width = 8,
            h1("Willkommen im MINT-DataLab von MINTvernetzt!"),
            p(
              "Im MINT-DataLab von MINTvernetzt präsentieren wir statistische Kennzahlen rund um MINT in den Bereichen Schule, Hochschule, Ausbildung und
          Arbeitsmarkt in Deutschland.", br(),
          "Unser Ziel ist es, mit dem MINT-DataLab einen zentralen Ort
          für die wichtigsten Statistiken rund um MINT zu schaffen und mittels interaktiver
          Diagramme einen intuitiven und informativen Zugang zu gewähren. Dabei entwickeln wir das MINT-DataLab stetig weiter.",
          style = "margin-bottom: 40px;"  ),

          p(),
          hr(),
          p(),

    # Kurzanalysen ----

            h2("Kurzanalysen: Aktuelle Entwicklungen auf einen Blick",
               style = "margin-top: 40px;"),
            p(
              "Unsere Kurzanalysen ordnen ausgewählte MINT-Kennzahlen ein,
              greifen aktuelle Entwicklungen auf und zeigen mögliche Handlungsansätze.
              Die Analysen basieren auf Daten aus dem MINT-DataLab und wissenschaftlichen Erkenntnissen.",
              br())

      # Button erstmal raus - vllt. zukünftig zur Übersicht der KA
              # tags$a(href = "https://www.mint-vernetzt.de/blogbeitraege/",
              #        target = "_blank", "Weitere Schwerpunktthemen von MINTvernetzt",
              #        class = "btn btn-default",
              #        style = "margin-bottom: 30px; margin-top: 10px; font-size: 18px")
    ),
    column(
      width = 12,

      tags$div(

        style = "
      display:flex;
      overflow-x:auto;
      gap:24px;
      padding:10px 0;
      align-items:stretch;
      margin-left: 30px;
      margin-right: 30px;
        ",

        # Kachel 1
        tags$div(
          style = "min-width:400px; max-width:400px; background:white;border-radius:12px;padding:20px;display:flex;flex-direction:column;flex-shrink:0;",
          tags$img(src="www/Visual KA schulisch-außerschulisch.png",
                   loading = "lazy",
                   decoding = "async",
                   style="width:100%;height:220px;object-fit:cover;"),
          tags$h3("MINT-Kooperationen machen Schule"),
          tags$p("Kooperationen zwischen Schulen und außerschulischen Bildungsakteur:innen sind zentral für eine zukunftsfähige MINT-Bildung, nicht zuletzt im Kontext des Ganztags. Die Ergebnisse verschiedener MINTvernetzt-Befragungen zeigen auf einen Blick, welche Voraussetzungen und Bedarfe Schulen und außerschulische Partner für eine gelingende Zusammenarbeit sehen.",
                 style = "flex-grow:1;"),
          tags$a(href="https://www.mint-vernetzt.de/content/uploads/2026/06/MINTvernetzt_Kurzanalyse_schulisch_ausserschulisch.pdf",
                 class="btn btn-primary",
                 target = "_blank",
                 style = "background-color: #154194 ;border:1px solid #154194;
                 margin-top:auto; width:fit-content; margin-top: 20px;",
                 "Download Kurzanalyse")
        ),

        # Kachel 2
        tags$div(
          style = "min-width:400px; max-width:400px; background:white;border-radius:12px;padding:20px;display:flex;flex-direction:column;flex-shrink:0;",
          tags$img(src="www/M I N T Fachkräftemangel.jpg",
                   loading = "lazy",
                   decoding = "async",
                   style="width:100%;height:220px;object-fit:cover;"),
          tags$h3("Fachkräftemangel in MINT: Eine Frage der Disziplin?"),
          tags$p("Informatik boomt, aber es gibt immer weniger junge Leute, die sich für Technik und Naturwissenschaften interessieren. Gleichzeitig fehlen in der Technik besonders viele Fachkräfte. Hier schauen wir uns den Fachkräftemangel in den MINT-Bereichen genauer an, und geben Ideen, wie gezielt gegengesteuert werden kann.",
                 style = "flex-grow:1;"),
          tags$a(href="https://www.mint-vernetzt.de/content/uploads/2026/01/MINTvernetzt_Kurzanalyse_Fachkraeftemangel_MINT-Disziplinen.pdf",
                 class="btn btn-primary",
                 target = "_blank",
                 style = "background-color: #154194 ;border:1px solid #154194;
                 margin-top:auto; width:fit-content; margin-top: 20px;",
                 "Download Kurzanalyse")
        ),

        # Kachel 3
        tags$div(
          style = "min-width:400px; max-width:400px; background:white;border-radius:12px;padding:20px;display:flex;flex-direction:column;flex-shrink:0;",
          tags$img(src="www/Zuwanderung_MINT.jpg",
                   loading = "lazy",
                   decoding = "async",
                   style="width:100%;height:220px;object-fit:cover;"),
          tags$h3("Szenarien: Zuwanderung für MINT"),
          tags$p("Die MINT-Branche wird immer internationaler. In dieser Kurzanalyse werfen wir einen gezielten Blick darauf, welche Rolle Zuwanderung in den kommenden 15 Jahren für die Zahl der MINT-Fachkräfte spielen könnte. Die möglichen Szenarien hat für uns das IW Köln berechnet.",
                 style = "flex-grow:1;"),
          tags$a(href="https://www.mint-vernetzt.de/content/uploads/2026/01/MINTvernetzt_Kurzanalyse_Zukunftsszenarien_Zuwanderung_MINT.pdf",
                 class="btn btn-primary",
                 target = "_blank",
                 style = "background-color: #154194 ;border:1px solid #154194;
                 margin-top:auto; width:fit-content; margin-top: 20px;",
                 "Download Kurzanalyse")
        ),

        # Kachel 4
        tags$div(
          style = "min-width:400px; max-width:400px; background:white;border-radius:12px;padding:20px;display:flex;flex-direction:column;flex-shrink:0;",
          tags$img(src="www/Zukunftsszenarien.jpg",
                   loading = "lazy",
                   decoding = "async",
                   style="width:100%;height:220px;object-fit:cover;"),
          tags$h3("Szenarien: So könnte sich die Zahl der MINT-Fachkräfte entwickeln"),
          tags$p("Projektion bis 2037: Wird es 1,4 Mio. mehr oder 1,1 Mio. weniger MINT-Fachkräfte geben als heute? Das IW Köln hat für uns Szenarien berechnet und wir fassen hier zusammen, wie sehr sich MINT-Bildung, Frauenförderung, der Verbleib älterer Fachkräfte und Zuwanderung auf die MINT-Fachkräftezahlen auswirken können.",
                 style = "flex-grow:1;"),
          tags$a(href="https://www.mint-vernetzt.de/content/uploads/2026/01/MINTvernetzt_Kurzanalyse_Zukunftsszenarien_MINT-Fachkraefte.pdf",
                 class="btn btn-primary",
                 target = "_blank",
                 style = "background-color: #154194 ;border:1px solid #154194;
                 margin-top:auto; width:fit-content; margin-top: 20px;",
                 "Download Kurzanalyse")
        ),

        # Kachel 5
        tags$div(
          style = "min-width:400px; max-width:400px; background:white;border-radius:12px;padding:20px;display:flex;flex-direction:column;flex-shrink:0;",
          tags$img(src="www/Mädchen_MINT.jpg",
                   loading = "lazy",
                   decoding = "async",
                   style="width:100%;height:220px;object-fit:cover;"),
          tags$h3("Mathe-Förderung für Mädchen: Notwendig und möglich"),
          tags$p("Die aktuellen IQB-Bildungstrend-Ergebnisse zeigen: Mädchen schneiden im Mathematik-Kompetenztest erneut weniger gut ab als Jungen. Mögliche Ursachen könnten ein geringeres Interesse und Vertrauen in die eigenen Fähigkeiten in diesem Fach sein. Hier gehen wir darauf ein, wie Mathe-Förderung für Mädchen und Frauen gelingt.",
                 style = "flex-grow:1;"),
          tags$a(href="https://www.mint-vernetzt.de/content/uploads/2026/01/MINTvernetzt_Kurzanalyse_Maedchen_Mathematik.pdf",
                 class="btn btn-primary",
                 target = "_blank",
                 style = "background-color: #154194 ;border:1px solid #154194;
                 margin-top:auto; width:fit-content; margin-top: 20px;",
                 "Download Kurzanalyse")
        ),

        # Kachel 6
        tags$div(
          style = "min-width:400px; max-width:400px; background:white;border-radius:12px;padding:20px;display:flex;flex-direction:column;flex-shrink:0;",
          tags$img(src="www/Chancengerechtigkeit.jpg",
                   loading = "lazy",
                   decoding = "async",
                   style="width:100%;height:220px;object-fit:cover;"),
          tags$h3("Mit MINT-Bildung zu mehr Chancengerechtigkeit"),
          tags$p("Die soziale Herkunft von Kindern beeinflusst deren Chancen entlang ihres Bildungswegs. Das gilt auch für den Erwerb von MINT-Kompetenzen. Wir erklären die Zusammenhänge und zeigen, welche Schritte für mehr Chancengerechtigkeit in der (MINT-)Bildung sorgen.",
                 style = "flex-grow:1;"),
          tags$a(href="https://www.mint-vernetzt.de/content/uploads/2026/01/MINTvernetzt_Kurzanalyse_Chancengerechtigkeit_mit_MINT.pdf",
                 class="btn btn-primary",
                 target = "_blank",
                 style = "background-color: #154194 ;border:1px solid #154194;
                 margin-top:auto; width:fit-content; margin-top: 20px;",
                 "Download Kurzanalyse")
        ),

        # Kachel 7
        tags$div(
          style = "min-width:400px; max-width:400px; background:white;border-radius:12px;padding:20px;display:flex;flex-direction:column;flex-shrink:0;",
          tags$img(src="www/Frauen_verlassen_MINT.jpg",
                   loading = "lazy",
                   decoding = "async",
                   style="width:100%;height:220px;object-fit:cover;"),
          tags$h3("Frauen verlassen MINT - vier Lösungsansätze"),
          tags$p("Frauen entscheiden sich seltener für eine MINT-Ausbildung oder einen MINT-Beruf. Während in MINT-Leistungskursen das Geschlechterverhältnis nahezu ausgeglichen ist, nimmt der Frauenanteil über die Ausbildung bis zum Beruf ab. Wir zeigen Ansätze aus der Wissenschaft, wie wir Frauen in MINT stärken.",
                 style = "flex-grow:1;"),
          tags$a(href="https://www.mint-vernetzt.de/content/uploads/2026/01/MINTvernetzt_Kurzanalyse_Frauen_in_MINT-Berufen.pdf",
                 class="btn btn-primary",
                 target = "_blank",
                 style = "background-color: #154194 ;border:1px solid #154194;
                 margin-top:auto; width:fit-content; margin-top: 20px;",
                 "Download Kurzanalyse")
        ),

        # Kachel 8
        tags$div(
          style = "min-width:400px; max-width:400px; background:white;border-radius:12px;padding:20px;display:flex;flex-direction:column;flex-shrink:0;",
          tags$img(src="www/Fachkräfte_von_morgen.jpg",
                   loading = "lazy",
                   decoding = "async",
                   style="width:100%;height:220px;object-fit:cover;"),
          tags$h3("Wo die MINT-Fachkräfte von morgen herkommen"),
          tags$p("Während der Bedarf an MINT-Fachkräften stetig wächst, sinkt die Zahl an MINT-Auszubildenden und MINT-Studienanfänger:innen seit einigen Jahren. Wir gehen auf diese Entwicklung ein und tragen Anregungen von Bildungsexpert:innen zusammen, die diesem Trend entgegenwirken sollen.",
                 style = "flex-grow:1;"),
          tags$a(href="https://www.mint-vernetzt.de/content/uploads/2026/01/MINTvernetzt_Kurzanalyse_Interesse_an_MINT.pdf",
                 class="btn btn-primary",
                 target = "_blank",
                 style = "background-color: #154194 ;border:1px solid #154194;
                 margin-top:auto; width:fit-content; margin-top: 20px;",
                 "Download Kurzanalyse")
        )
      ),

      p(),
      hr(style = "margin-top: 40px;"),


      # Lernvideo ----

      h2("Lernvideo zu den MINT-Daten",
         style = "margin-top: 40px;"),
      p("Auf dem MINT-Campus haben wir ein Video veröffentlich, in dem wir Statistiken
        zum Thema Frauen in MINT zeigen und einordnen."),
      div(
        style = "display: flex; justify-content: center;",
        tags$iframe(
          width = "800", height = "450",
          src = "https://www.youtube.com/embed/cFd8ZvegIhg",
          frameborder = "0",
          allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
          allowfullscreen = NA,
          style = "margin: 40px;"
        )
      )
    ),


    column(
      width = 8,

      p(),
      hr(),

      # Updates ----
      h2(style = "color: #008F68; margin-top: 40px;",
         "Was ist Neu?"),
      tags$ul(
        tags$li("Aktualisierte Daten für das Berichtsjahr 2025 der Bundesagentur für Arbeit auf den Unterseiten \"Ausbildung & Beruf\" und
                \"MINT-Fachkräfte\""),
        tags$li("Grafiken in neuem Design"),
        tags$li("Alle Kurzanalysen auf einem Blick auf der Startseite des MINT-DataLab")
      ),
      h2(style = "color: #008F68;",
         "Woran wir aktuell arbeiten:"),
      tags$ul(
        tags$li("Weiterentwicklung des Angebots \"Datenanalyse mit KI\" mit statistischen Berichten für jedes Bundesland zum Download"),
        tags$li("Weitere Erhöhung der Barrierefreiheit")),
      p(),      p(),
      p("Bei Fragen oder Anregungen, melden Sie sich jederzeit gerne ",
        tags$a(href = "mailto:katharina.brunner@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per E-Mail"),
        " bei uns."),


       # Abbinder ----

      p(),
      hr(style = "margin-top: 40px;"),

      h1("Entdecken Sie jetzt die verschiedenen MINT-Bereiche!",
         style = "margin-bottom: 40px; margin-top: 40px;")
      )
    )
  ),


    # Footer
    fluidRow(
      shinydashboard::box(
        style = "margin-top: 20px",
        width = 12,
        funct_footer()
      )
    )

)
}

#' Startseite Server Functions
#'
#' @noRd
mod_startseite_start_server <- function(id){
  moduleServer( id, function(input, output, session){


  })
}


## To be copied in the UI
# mod_startseite_ui("startseite_1")

## To be copied in the server
# mod_startseite_server("startseite_1")
