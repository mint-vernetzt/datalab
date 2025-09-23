#' kontakt UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_kontakt_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' kontakt Server Functions
#'
#' @noRd
mod_kontakt_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

mod_kontakt_ui <- function(id){
  ns <- NS(id)
  tagList(


    fluidRow(
      shinydashboard::box(
        width = 12,
        tags$h1("Kontakt und Feedback"),
        column(width = 9,
               tags$h2("Wir freuen uns über Feedback!"),
               br(),
                p(style = "text-align: left; font-size = 16px",
                 "Das MINT-DataLab wird in den nächsten Jahren schrittweise weiterentwickelt.
          Die Wünsche und Bedarfe unserer Zielgruppen werden über direkten Austausch,
          eine fortlaufende Umfrage und verschiedene Veranstaltungsformate kontinuierlich erfasst und bestmöglich berücksichtigt.",
                 br(),
                 br(),
                 "Ansprachpartnerin für dieses Projekt ist Katharina Brunner."),

               p(style = "text-align: left; font-size = 16px",
                 span("Wir freuen uns über Rückfragen oder Feedback ", tags$a(href = "mailto:katharina.brunner@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per Email"),"oder über unsere kurze",
                      tags$a(href="https://survey.lamapoll.de/MINT-DataLab_Feedback/", "Umfrage", target="_blank"), "!"
                 )),
                 br(),
                 br(),
                 br(),
                 br(),
               tags$h2("Über das MINT-DataLab"),
               p(style = "text-align: left; font-size = 16px",
                 "Im MINT-DataLab präsentieren wir statistische Kennzahlen rund um MINT in den Bereichen Schule, Hochschule,
                 Ausbildung und Arbeitsmarkt in Deutschland. Unser Ziel ist es, mit dem MINT-DataLab einen zentralen Ort
                für die wichtigsten Statistiken rund um MINT zu schaffen und mittels interaktiver Diagramme einen intuitiven und informativen Zugang zu gewähren."
               ),
               p(style = "text-align: left; font-size = 16px",
                 "Das MINT-DataLab ist ein Angebot aus dem Projekt ", tags$a(href="https://mint-vernetzt.de/", "MINTvernetzt", target="_blank"), "."
               ),

               p(style = "text-align: left; font-size = 16px",
                   "Die MINT-Vernetzungsstelle, kurz MINTvernetzt, ist die Service- und Anlaufstelle für MINT-Akteur:innen in Deutschlanddas und das
                      Dach für die außerschulische MINT-Bildung in Deutschland.
                  MINTvernetzt wird vom Bundesministerium für Bildung und Forschung gefördert und von Mitarbeitenden der Körber-Stiftung, der matrix gGmbH,
                  dem Nationalen MINTForum e.V., dem Stifterverband und der Universität Regensburg als Verbund gemeinsam umgesetzt."
                 )),

      column(width = 3,
               img(src='www/mint_logo_gross.jpg',
                   class = "img-responsive",
                   height = "150px", width = "150px",
                   alt = "Logo MINT",
                   style="display: block; margin-left: auto; margin-right: auto;"
               ))
      )),



funct_footer())

#tagList zu

}


## To be copied in the UI
# mod_kontakt_ui("kontakt_1")

## To be copied in the server
# mod_kontakt_server("kontakt_1")
