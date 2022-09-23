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

    # Banner
    fluidRow(
      shinydashboard::box(
        width = 12,
        img(src='www/Banner_Kontakt.jpg',
            class = "img-responsive",
            #height = "150px", width = "150px",
            alt = "Banner Schule",
            style="display: block; margin-left: auto; margin-right: auto;"
        ))),



    fluidRow(
      shinydashboard::box(
        title = "Über das MINT-DataLab",
        width = 12,
        column(width = 9,

               p(style = "text-align: justify; font-size = 16px",
                 "Im MINT-DataLab präsentieren wir statistische Kennzahlen rund um MINT in den Bereichen Schule, Hochschule,
                 Ausbildung und Arbeitsmarkt in Deutschland. Unser Ziel ist es, mit dem MINT-DataLab einen zentralen Ort
                für die wichtigsten Statistiken rund um MINT zu schaffen und mittels interaktiver Diagramme einen intuitiven und informativen Zugang zu gewähren."
               ),
               p(style = "text-align: justify; font-size = 16px",
                 "Das MINT-DataLab ist ein Angebot aus dem Projekt ", tags$a(href="https://mint-vernetzt.de/", "MINTvernetzt", target="_blank"), "."
               ),

               p(style = "text-align: justify; font-size = 16px",
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



    fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Kontakt & Feedback",
        p(style = "text-align: justify; font-size = 16px",
          "Das MINT-DataLab wird in den nächsten Jahren schrittweise weiterentwickelt.
          Die Wünsche und Bedarfe unserer Zielgruppen werden über direkten Austausch,
          eine fortlaufende Umfrage und verschiedene Veranstaltungsformate kontinuierlich erfasst und bestmöglich berücksichtigt.",
          br(),
          br(),
          "Ansprachpartnerin für dieses Projekt ist Antonia Kröger."),

          p(style = "text-align: justify; font-size = 16px",
            span("Wir freuen uns über Rückfragen oder Feedback ", tags$a(href = "mailto:antonia.kroeger@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per Email"),"oder über unsere kurze",
                 tags$a(href="https://survey.lamapoll.de/MINT-DataLab_Feedback/", "Umfrage", target="_blank"), "!"
                 )
          )
        )),



    # Fußzeile

    # fluidRow(
    #   shinydashboard::box(
    #     width = 12,
    #     column(width = 2,
    #            img(src='www/BMBF-Logo.jpg',
    #                class = "img-responsive",
    #                height = "100px", width = "100px",
    #                alt = "Logo BMBF",
    #                style="display: block; margin-left: auto; margin-right: auto;"
    #            )),
    #
    #     column(width = 2, p(style = "text-align: justify; font-size = 16px",
    #            " Impressum (Link)"
    #            )),
    #
    #     column(width = 2, p(style = "text-align: justify; font-size = 16px",
    #           " Datenschutz (Link)"
    #     )),
    #
    #     column(width = 2, p(style = "text-align: justify; font-size = 16px",
    #            " Kontakt (Link)"
    #     )),
    #
    #     column(width = 2,
    #             p(style = "text-align: justify; font-size = 16px",
    #             "Copyright © 2022. Alle Rechte vorbehalten Stifterverband xxx "
    #            )),
    #
    #   column(width = 2,
    #            img(src='www/mint_logo_gross.jpg',
    #                class = "img-responsive",
    #                height = "50px", width = "50px",
    #                alt = "Logo MINT",
    #                style="display: block; margin-left: auto; margin-right: auto;"
    #            ))
      # ))),



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
    #   )),


    # fluidRow(
    #   shinydashboard::box(
    #     title = "Über das Projekt MINTvernetzt",
    #     width = 12,
    #     column(width = 9,
    #            p(style = "text-align: justify; font-size = 16px",
    #              span(tags$b(span("MINTvernetzt: Die Service- und Anlaufstelle für MINT-Akteur:innen in Deutschland", style = "color:#b16fab")),br(),
    #                   "Die MINT-Vernetzungsstelle, kurz MINTvernetzt, ist das Dach für die außerschulische MINT-Bildung in Deutschland.
    #               MINTvernetzt wird vom Bundesministerium für Bildung und Forschung gefördert und von Mitarbeitenden der Körber-Stiftung, der matrix gGmbH,
    #               dem Nationalen MINTForum e.V., dem Stifterverband und der Universität Regensburg als Verbund gemeinsam umgesetzt."))),
    #     column(width = 3,
    #            img(src='www/BMBF-Logo.jpg',
    #                class = "img-responsive",
    #                height = "150px", width = "150px",
    #                alt = "Logo BMBF",
    #                style="display: block; margin-left: auto; margin-right: auto;"
    #            ))
    #   )
    # ) #Row zu


tags$footer(style="text-align: justify;",

            div(style="display: inline-block;position: relative;top: 1.2em;",

                tags$a(href="https://mint-vernetzt.de/",
                       img(src='www/MINTv_tranparent.png',
                           class = "img-responsive",
                           height = "100px", width = "100px",
                           alt = "Logo MINT", target="_blank",
                           style="display: inline-block; margin-left: auto; margin-right:10%;"))),

            div(style="display: inline-block;position: relative;top: 1.2em;",

                p(tags$a("Impressum", href="#shiny-tab-impressum", "data-toggle" = "tab")," | ",
                  tags$a("Kontakt", href="#shiny-tab-kontakt", "data-toggle" = "tab")," | ",
                  tags$a("Datenschutz", href="#shiny-tab-datenschutz", "data-toggle"="tab"),HTML('&nbsp;'),HTML('&nbsp;'),
                  "Copyright © 2022. Alle Rechte vorbehalten Stifterverband")),

            div(style="display: inline-block;position: relative;top: 1.2em;",

                tags$a(href="https://www.bmbf.de/bmbf/de/home/home_node.html",
                       img(src='www/BMBF-Logo_transp1.png',

                           class = "img-responsive",

                           height = "200px", width = "200px",

                           alt = "Logo BMBF", target="_blank",

                           style="display: inline-block; margin-left: auto; margin-right: auto;"))),

            div(style="display: inline-block;width: 100%;",

                " ")

))

#tagList zu

}

# fluidRow(
# shinydashboard::box(
#   #title = span("Willkommen im MINT-DataLab von MINTvernetzt", style = "color:#154194; font-size: 50px"),
#   width = 12,
#   column(width = 9,
#          tags$h2("Infos xx"),
#          #tags$h1("Willkommen im MINT-DataLab"),
#          #p(style = "color:#154194; font-size: 50px", "Willkommen im MINT-DataLab"),
#          p(style = "text-align: justify; font-size = 16px",
#            "Im MINT-DataLab zeigen wir statistische Kennzahlen rund um MINT in den Bereichen Schule, Hochschule,
#                     Ausbildung und Arbeitsmarkt in Deutschland."
#          ),
#
#          p(style = "text-align: justify; font-size = 16px",
#            span("Auf dieser", tags$b(span("Startseite", style = "color:#b16fab")), "geben wir einen ersten Überblick über die Daten. Auf den bereichsspezifischen", tags$b(span("Unterseiten", style = "color:#b16fab")),
#                 " gehen wir mehr ins Detail und bieten zusätzlich Vergleiche auf Fächer- und Bundeslandebene.")
#          ),
#
#   ),
#   #solidHeader = TRUE,
#   #collapsible = FALSE,
#   #br(),
#   column(width = 3, href = "https://mint-vernetzt.de/", rel="noopener", target="_blank",
#          img(src='www/mint_logo_gross.jpg',
#              class = "img-responsive",
#              height = "180px", width = "180px",
#              alt = "Logo MINT",
#              style="display: block; margin-left: auto; margin-right: auto;"
#
#          ))
# ))










## To be copied in the UI
# mod_kontakt_ui("kontakt_1")

## To be copied in the server
# mod_kontakt_server("kontakt_1")
