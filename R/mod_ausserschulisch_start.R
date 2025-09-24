#' Ausserschulisch UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList


mod_ausserschulisch_start_ui <- function(id){
  ns <- NS(id)
  tagList(

    # Banner
    fluidRow(
      div(class = "clean-box",
          column(
            width = 12,
        img(src='www/Banner_Außerschulisch.avif',
            class = "img-responsive",
            #height = "150px", width = "150px",
            alt = "Banner Außerschulisch",
            style="display: block; margin-left: auto; margin-right: auto; margin-bottom: 20px;"
        )))),


    # Info-Texte

    fluidRow(
      shinydashboard::box(
        h2("Auf dieser Seite"),
        width = 7,
        p(style = "text-align: left; font-size = 16px",
          "Auf dieser Seite zeigen wir Daten aus dem außerschulischen MINT-Bildungsbereich.
          Amtliche Statistiken für den außerschulischen Bereich liegen nicht vor. Deshalb wollen wir hier Daten
          aus der außerschulischen MINT-Bildungs-Community sammeln und so schrittweise die weißen Flecken der außerschulischen MINT-Bildungslandschaft füllen."),
        p("Wir zeigen zum einen Daten von MINTvernetzt. Hierzu nutzen wir die Daten der MINTvernetzt-Community-Plattform und
          stellen dar, wie viele Organsiationen, Projekte und Personen in welchen Bereichen der außerschulischen MINT-Bildung
          tätig sind. Außerdem zeigen wir eine Auswahl an Ergebnisse der Befragungen von MINTvernetzt."),
        p("Zum anderen zeigen wir Daten aus der außerschulischen MINT-Bildungscommunity. Aktuell sind das Zahlen der Stiftung
          Kinder forschen. Liegen in Ihrem Projekt Daten vor, die für die außerschulische MINT-Bildungslandschaft interessant
          sein könnten, kommen Sie gerne per ",
          tags$a(href = "mailto:katharina.brunner@mint-vernetzt.de?subject= ausserschulische Daten MINT-Datalab", "E-Mail"),
          " auf uns zu.")
      ),

      shinydashboard::box(
        title = "Fragen oder Feedback?",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          "Sind alle Zahlen und Grafiken verständlich dargestellt? Haben Sie eigene Daten, die für Akteur:innen in der MINT-Bildung interessant sein könnten?", br(),
          "Kontaktieren Sie uns gern! Wir freuen uns über Rückfragen, Vorschläge oder Feedback ", tags$a(href = "mailto:katharina.brunner@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per E-Mail"),"oder über eine Teilnahme an unserer kurzen",
          tags$a(href="https://survey.lamapoll.de/MINT-DataLab_Feedback/", "Umfrage", target="_blank"), "!"
        )
      )
    ),

    fluidRow(
      shinydashboard::box(
        h2("Themenübersicht"),
        width = 7,
        p(style = "text-align: left; font-size = 16px",tags$a(href="#ausserschulisch_community_plattform",
                                                              span(tags$b(span("→ Außerschulische MINT-Akteur:innen:")))),"Unsere MINTverentzt-Community in Zahlen."
        ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#ausserschulisch_mintvernetzt_befragung",
                                                              span(tags$b(span("→ MINTvernetzt Befragungen:")))),"Was sagt die Community über sich und MINT in Deutschland."

        ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#ausserschulisch_skf",
                                                              span(tags$b(span("→ Frühkindliche Bildung:")))),"Zahl der MINT-aktiven Einrichtungen bei Stiftung Kinder forschen wächst stetig."
        )
        # ,
        # p(style = "text-align: left; font-size = 16px",tags$a(href="#ausserschulisch_community_daten",
        #                                                       span(tags$b(span("Daten aus der Community:")))),"Daten und Ergebnisse aus MINT-Projekten."
        # )
        ),

      shinydashboard::box(
        title = "Datenquellen",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          span(tags$b(span("Daten zu den außerschulische MINT-Akteur:innen und MINTvernetzt Befragungen: Quelle MINTvernetzt, Stand 14. April 20-25.")))),
        p(style = "text-align: left; font-size = 16px",
          span(tags$b(span("Daten zu frühklindlicher Bildung: Quelle Stiftung Kinder forschen, 2023."))))
      )
    ),


    # Trennlinie
    h1("Daten von MINTvernetzt", style = "color: #00A87A;"), #font-size: 22px; font-family: 'SourceSans3-Bold'; margin-top: 20px;"
    hr(style = "border-top: 3px solid #00A87A; margin-top: 5px;"),

    # Community Plattform ----

    fluidRow( id="ausserschulisch_community_plattform",
              shinydashboard::box(
                h2("Außerschulische MINT-Akteur:innen: Unsere Community in Zahlen."),
                width = 12,
                column(
                  width = 8,
                  p("Die Community-Plattform von MINTvernetzt ist eine Plattform für
                    das Vernetzen, Austauschen und zum Teilen von Wissen zwischen MINT-Bildungsakteur:innen. Die Plattform
                    wurde im Herbst 2022 livegeschaltet und wird seitdem stetig weiterentwickelt.
                    Auf der Community Plattform können Profile für Personen, Projekte, Organisationen oder
                    Netzwerke angelegt werden, die Informationen über Themenbereiche, Zielgruppen, Aktivitätsgebiete und vielem mehr
                    enthalten."),
                  br(),
                  p("Für die folgenden Darstellungen wurden die Daten der MINTvernetzt-Community-Plattform anonymisiert ausgewertet.
                    Entsprechend bilden die folgenden Grafiken nur die Akteur:innen, Projekte und Organsiationen ab, die
                    Teil des MINTvernetzt-Netzwerks sind. Diese Angaben sind freiwillig und können nicht das vollständige Angebot der außerschulischen
                    MINT-Bildung abdecken. Sie können allerdings einen Einblick geben, in welchen Themenbereichen, für welche Zielgruppen und in welchen Regionen
                    MINT-Akteur:innen besonders aktiv sind."),
                  br(),
                ),
                column(
                  width = 12,

                  tabsetPanel(type = "tabs",
                              # Tab 1
                              tabPanel("Organisationen der MINTvernetzt-Community", br(),

                                 shiny::sidebarPanel(
                                   width = 3,
                                   tags$style(".well {background-color:#FFFFFF;}"),
                                   tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                                   mod_ausserschulisch_cp_orgas_ui("mod_ausserschulisch_cp_orgas_ui"),

                                 ),
                                 shiny::mainPanel(
                                   width = 9,
                                   shinycssloaders::withSpinner(
                                     highcharter::highchartOutput(ns("plot_cp_orgas"), height = "500px"),
                                                                color = "#154194"),

                                 )
                              ),
                              tabPanel("Projekte der MINTvernetzt-Community", br(),

                                       shiny::sidebarPanel(
                                         width = 3,
                                         tags$style(".well {background-color:#FFFFFF;}"),
                                         tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                                         mod_ausserschulisch_cp_projekte_ui("mod_ausserschulisch_cp_projekte_ui"),

                                       ),
                                       shiny::mainPanel(
                                         width = 9,
                                         shinycssloaders::withSpinner(
                                           highcharter::highchartOutput(ns("plot_cp_projekte"), height = "500px"),
                                                                      color = "#154194"),
    )
                              ),
                              tabPanel("Profile der MINTvernetzt-Community", br(),

                                       shiny::sidebarPanel(
                                         width = 3,
                                         tags$style(".well {background-color:#FFFFFF;}"),
                                         tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                                         mod_ausserschulisch_cp_profile_ui("mod_ausserschulisch_cp_profile_ui"),

                                       ),
                                       shiny::mainPanel(
                                         width = 9,
                                         shinycssloaders::withSpinner(
                                           highcharter::highchartOutput(ns("plot_cp_profile"), height = "500px"),
                                                                      color = "#154194"),

                                       )
                              ),
                              tabPanel("Informationen zu den Daten", br(),
                                       column(
                                         width = 8,
                                         p("Die hier gezeigten Daten haben keinen Anspruch darauf, den MINT-Bildungsbereich vollständig abzubilden.
                                         Wir geben hier einen Einblick in die Daten, die MINTvernetzt vorliegen."),
                                         div(
                                           style = "display: flex; justify-content: center; align-items: center; margin-bottom: 25px;",
                                           img(src='www/Ausserschulisch_CP_Zahlen.png',
                                               class = "img-responsive",
                                               alt = "Bild Zahlen Nutzung Community Plattform",
                                               style = "max-width: 55%;")
                                         ),
                                         p("Nicht alle Profile sind vollständig ausgefüllt. In den hier gezeigten Darstellungen werden diejenigen Profile berücksichtigt,
                                         die zu der betrachteten Eigenschaft Angaben gemacht haben."),
                                         div(
                                           style = "display: flex; justify-content: center; align-items: center; margin-bottom: 25px;
                                           margin-top: 20px;",
                                           img(src='www/Ausserschulisch_CP_Ausfuellstatus.png',
                                               class = "img-responsive",
                                               alt = "Bild Ausfuellstatus Community Plattform",
                                               style = "max-width: 60%;"
                                           )
                                         )
                                       )

                                      )

                            )
                )
              )
    ),

    div(
      class = "linked-image",
      tags$a(
        href = "https://community.mint-vernetzt.de/",
        target = "_blank",
        tags$img(
          src = "www/Ausserschulisch_CP_Absprung.png",
          alt = "Community Plattform Aufruf",
          style = "max-width: 100%; height: auto; cursor: pointer;
          margin-bottom: 20px; margin-top: 20px;"
        )
      )
    ),

    # MV-Befragungen ----

    fluidRow( id="ausserschulisch_mintvernetzt_befragung",
              shinydashboard::box(
                h2("MINTvernetzt Befragungen: Was sagt die MINTvernetzt-Community?"),
                width = 12,
                column(
                  width = 8,
                  p("MINTvernetzt führt verschiedene Befragungen unter MINT-Akteur:innen durch.
                    Hier wird eine Auswahl dieser Befragungsergebnisse dargestellt. Die vollständigen Berichte
                    zu den Befragungen finden sich auf der MINTvernetzt-Website. Weitere Hinweise und Verlinkungen finden
                    sich unter den jeweiligen Befragungsergebnissen und unter dem Reiter \"Informationen zu den Befragungen\"."),
                  br(),
                  p("Die Ergebnisse der MINTvernetzt-Akteursbefragung zeigen, dass z.B. vier von fünf der Befragten
                    MINT-Bildungsakteur:innen hauptberuflich tätig sind. Etwas mehr als ein Drittel haben, trotz ihres
                    Engagements in MINT, keinen MINT-Bildungshintergrund.",
                    "Eine der hier dargestellten Fragen des MINT-Stimmungsbarometers zeigt, dass die Befragten
                    das Ganztagsangebot an Schulen besonders als Bildungsort unter Einbezug außerschulischer Angebote nutzen wollen würden.",
                    "In der MINTvernetzt-Genderbefragung von 2023 wurde ein Blick in die Vernetzung außerschulischer MINT-Bildungsakteur:innen
                    geworfen, die sich für Mädchen- und Frauenförderung in MINT interessieren oder in diesem Bereich aktiv sind.
                    Hier zeigt sich, dass Bildungsnetzwerke in diesem Bereich vor allem durch Aktivität und hohe Vernetzungswünsche geprägt sind."),

                ),
                p(),
                column(
                  width = 12,

                  tabsetPanel(type = "tabs",
                              # Tab 1
                              tabPanel("MINTvernetzt-Akteursbefragung: Außerschulisch MINT-Akteur:innen", br(),

                                       shiny::sidebarPanel(
                                         width = 3,
                                         tags$style(".well {background-color:#FFFFFF;}"),
                                         tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                                         mod_ausserschulisch_mvb_akteursbefragung_ui("mod_ausserschulisch_mvb_akteursbefragung_ui_1"),
                                       ),
                                       shiny::mainPanel(
                                         width = 9,
                                         shinycssloaders::withSpinner(
                                           highcharter::highchartOutput(ns("plot_mvb_akteursbefragung"), height = "500px"),
                                                                      color = "#154194"),


                                         p(style = "font-size:16px; font-weight: 600;",
                                           br(),
                                           "Die MINTvernetzt-Akteursbefragung fragt ab, wer die MINTvernetzt-Community ist und welche Bedrüfnisse
                                           und Wünsche sie hat. Weitere Ergebnisse und Informationen finden sich ",
                                           tags$a(style = "color: #008F68; font-size: 16px;", href = "https://www.mint-vernetzt.de/studien-und-umfragen/#akteursbefragung", target = "_blank", "hier, auf der MINTvernetzt-Website"),
                                           "."), br(),

                                       )
                              ),
                              # Tab 2
                              tabPanel("MINT-Stimmungsbarometer: Nutzung des Ganztags", br(),
                                       shiny::sidebarPanel(
                                         width = 3,
                                         tags$style(".well {background-color:#FFFFFF;}"),
                                         tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                                         mod_ausserschulisch_mvb_stimmungsb_ui("mod_ausserschulisch_mvb_stimmungsb_ui"),
                                         # br(),br(),
                                       ),
                                       shiny::mainPanel(
                                         width = 9,
                                         shinycssloaders::withSpinner(
                                           highcharter::highchartOutput(ns("plot_mv_stimmung"), height = "500px"),
                                           color = "#154194"),

                                         p(style = "font-size:16px; font-weight: 600;",
                                           br(),
                                           "Im MINT-Stimmungsbarometer schätzen jährlich Vertreter:innen aus Bildung, Wissenschaft und Wirtschaft
                                           die Qualität der MINT-Bildung in Deutschland ein. Jedes Jahr werden dazu Fragen eines neuen Themenfokus,
                                           z. B. zur Nutzung des Ganztags in Schulen oder Kooperationen abgefragt. Weitere Ergebnisse finden sich ",
                                           tags$a(style = "color: #008F68; font-size: 16px;", href = "https://www.mint-vernetzt.de/studien-und-umfragen/#stimmungsbarometer",
                                                  target = "_blank", "hier, auf der MINTvernetzt-Website"),
                                           "."), br(),
                                       )
                              ),
                              # Tab 3
                              tabPanel("MINTvernetzt-Genderbefragung: Aktive Vernetzung", br(),
                                       column(
                                         width = 3,
                                         tags$img(src = "www/Ausserschulisch_Gender_Tabelle2.png",
                                                  style = "margin-top: 90px;")
                                       ),
                                       column(
                                         width = 9,
                                         shinycssloaders::withSpinner(
                                           highcharter::highchartOutput(ns("plot_mv_gender"), height = "600px"),
                                           color = "#154194"),


                                         p(style = "font-size:16px; font-weight: 600;",
                                           br(),
                                           "Die MINTvernetzt-Genderbefragungen untersuchen, inwieweit MINT-Bildungsangebote
                                           verschiedene Aspekte einer nachhaltig erfolgreichen MINT-Förderung für Mädchen umsetzen
                                           und wie sie dabei gezielt unterstützt werden können. Auf Basis der erhobenen Daten werden
                                           praxisnahe Handlungsempfehlungen und Tipps für die Bildungsarbeit entwickelt.
                                           Diese sowie weiteren Ergebnisse sind ",
                                           tags$a(style = "color: #008F68; font-size: 16px;", href = "https://www.mint-vernetzt.de/studien-und-umfragen/#gender",
                                                  target = "_blank", "auf der MINTvernetzt-Website verfügbar.")), br(),
                                       )
                              ),

                              # Info-Tab mit allen drei Befragungen
                              shiny::tabPanel(
                                "Informationen zu den Befragungen",
                                # Erste Befragung
                                    fluidRow(
                                      style= "top: 15px;",
                                      column(
                                        style = "top: 15px;",
                                        width = 8,
                                        div(
                                          p(strong("MINTvernetzt-Akteursbefragung 2024:", style = "color: #FCC433;")),
                                          p(tags$b("Ziel der Umfrage:"), "Übersicht über die MINT-Bildungsakteur:innen und ihre Bedarfe und Herausforderungen herstellen.", br(),
                                            tags$b("Teilnehmendenzahl:"), "221 Personen haben an der Umfrage teilgenommen.",br(),
                                            tags$b("Befragung:"), "Durchgeführt zwischen Juni und Juli 2024. Die Umfrage wurde über Kanäle von MINTvernetzt geteilt.")
                                        )
                                      ),
                                      column(
                                        style = "top: 15px;",
                                        width = 2,
                                        tags$a(
                                          href = "https://www.mint-vernetzt.de/studien-und-umfragen/#akteursbefragung",
                                          target = "_blank",
                                          tags$img(src = "www/Außerschulisch_Befragung_Akteur.png",
                                                   style = "position: relative; float: inline-end;
                                                 right: 15px; height: auto; width: 200px; padding: 5px;")
                                        )
                                      )
                                ),
                                # Zweite Befragung
                                fluidRow(
                                  style= "top: 15px;",
                                  column(
                                    style = "top: 15px;",
                                    width = 8,
                                    div(
                                      p(strong("MINT-Stimmungsbarometer 2024:", style = "color: #00A87A;")),
                                      p(tags$b("Ziel der Umfrage:"), "Einblicke in die Stimmung zur Qualität der MINT-Bildung in Deutschland erlangen.", br(),
                                        tags$b("Teilnehmendenzahl:"), "454 Personen haben an der Umfrage teilgenommen.",br(),
                                        tags$b("Befragung:"), "Durchgeführt zwischen August und September 2024. Die Umfrage wurde über Kanäle von MINTvernetzt und
                                                              dem Stifterverband an Vertreter:innen von Bildung, Wirtschaft und Wissenschaft versandt.")
                                    )
                                  ),
                                  column(
                                    style = "top: 15px;",
                                    width = 2,
                                    tags$a(
                                      href = "https://www.mint-vernetzt.de/studien-und-umfragen/#stimmungsbarometer",
                                      target = "_blank",
                                      tags$img(src = "www/Außerschulisch_Befragung_Stimmung.png",
                                               style = "position: relative; float: inline-end;
                                                 right: 15px; height: auto; width: 200px; padding: 5px;")
                                    )
                                  )
                                ),
                                # Dritte Befragung
                                fluidRow(
                                  style= "top: 15px;",
                                  column(
                                    style = "top: 15px;",
                                    width = 8,
                                    div(
                                      p(strong("MINTvernetzt-Genderbefragung 2023:", style = "color: #b16fab;")),
                                      p(tags$b("Ziel der Umfrage:"), "Synergien und nachhaltige Fördereffekte für Mädchen in MINT schaffen, aufbauend auf den
                                                                      Erfahrungen der MINT-Bildungsanbieter:innen.", br(),
                                        tags$b("Teilnehmendenzahl:"), "456 Personen haben an der Umfrage teilgenommen.",br(),
                                        tags$b("Befragung:"), "Durchgeführt zwischen November und Dezember 2023. Die Umfrage wurde über die Kanäle von MINTvernetzt geteilt.")
                                    )
                                  ),
                                  column(
                                    style = "top: 15px;",
                                    width = 2,
                                    tags$a(
                                      href = "https://www.mint-vernetzt.de/studien-und-umfragen/#gender",
                                      target = "_blank",
                                      tags$img(src = "www/Außerschulisch_Befragung_Gender.png",
                                               style = "position: relative; float: inline-end;
                                                 right: 15px; height: auto; width: 200px; padding: 5px;")
                                    )
                                  )
                                )

                               )
                  )
                )
              )
    ),

    # Trennlinie
    h1("Daten von MINT-Bildungsakteur:innen", style = "color: #00A87A;"), #font-size: 22px; font-family: 'SourceSans3-Bold'; margin-top: 20px;"
    hr(style = "border-top: 3px solid #00A87A; margin-top: 5px;"),

    # SKf ----
    fluidRow(id="ausserschulisch_skf",
             shinydashboard::box(
               h2("Frühkindliche Bildung: Zahl der MINT-aktiven Einrichtungen bei Stiftung Kinder forschen wächst stetig."),
               width = 12,
               column(
                 width = 8,
                 p("In diesem Abschnitt betrachten wir die Entwicklung der außerschulischen, frühkindlichen MINT-Bildung.
               Die interaktiven Grafiken basieren auf den Daten der 'Stiftung Kinder forschen'
               (kurz SKf; früher: 'Haus der kleinen Forscher')."),

                 p("Die Anzahl an Kitas, Grundschulen und Horte, die durch die Stiftung Kinder forschen für ihr MINT-Bildungsengagement
               zertifiziert wurden oder deren Personal durch die SKf fortgebildet wurde, wächst. Während der Corona-Pandemie 2020-2021
               hat sich das Wachstum drastisch verlangsamt. Das spiegelt sich auch in den Zahlen der neu fortgebildeten Personen wider.
               Während zwischen 2013 und 2018 jährlich zwischen 6.000 und 8.000 Personen hinzukamen, sind es im Jahr 2022 nur 2.000."), br(),
                 p("Dies sind bislang die einzigen Darstellungen aus dem Bereich der außerschulischen MINT-Bildung. Hier wird in Zukunft noch mehr hinzukommen.")
               ),
               column(
                 width = 12,

                 tabsetPanel(type = "tabs",

                             tabPanel("SKf-zertifizierte und aktive Einrichtungen", br(),

                                      tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                                      shiny::sidebarPanel(
                                        width = 3,
                                        mod_ausserschulisch_skf_einrichtungen_ui("mod_ausserschulisch_skf_einrichtungen_ui_1"),

                                      ),
                                      shiny::mainPanel(
                                        width = 9,
                                        shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_skf_einrichtungen"), height = "500px"),
                                                                     color = "#154194"),


                                        shinyBS::bsPopover(id="h_schule_ausserschulisch_1", title = "",
                                                           content = paste0("Zertifizierte Einrichtungen = Einrichtungen, die mindestens einmal als &quotHaus der Kleinen Forscher&quot ausgewiesen wurden.", "<br> <br> Einrichtungen mit SKf-Fortbildung = Einrichtungen, von welchen Fach- oder Lehrkräfte Fortbildungen der SKf besucht haben."),
                                                           placement = "top",
                                                           trigger = "hover"),
                                        tags$a(paste0("Definition der Begriffe"), icon("info-circle"), id = "h_schule_ausserschulisch_1")

                                      )
                             ),
                             tabPanel("Fach- und Lehrkräfte mit SKf-Fortbildung", br(),

                                      tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                                      shiny::sidebarPanel(
                                        width = 3,
                                        mod_ausserschulisch_skf_personal_ui("mod_ausserschulisch_skf_personal_ui_1"),

                                      ),
                                      shiny::mainPanel(
                                        width = 9,
                                        shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_skf_personal"), height = "500px"),
                                                                     color = "#154194"),


                                        br(),
                                        shinyBS::bsPopover(id="h_schule_ausserschulisch_2", title = "",
                                                           content = paste0("Die Teilnehmendenzahlen sind von der SKf geschätzt und auf 1.000er-Stellen gerundet."),
                                                           placement = "top",
                                                           trigger = "hover"),
                                        tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_ausserschulisch_2")
                                      )
                             )
                 )
               )
             )
    ),


    funct_footer()
  )
}


#' beruf_arbeitsmarkt Server Functions
#'
#' @noRd
mod_ausserschulisch_start_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Community Plattform ----

    output$plot_cp_orgas <- highcharter::renderHighchart({
      plot_cp_orgas(r)
    })

    output$plot_cp_projekte <- highcharter::renderHighchart({
      plot_cp_projekte(r)
    })

    output$plot_cp_profile <- highcharter::renderHighchart({
      plot_cp_profile(r)
    })
    # MV-Befragungen ----

    output$plot_mvb_akteursbefragung <- highcharter::renderHighchart(
      plot_mv_akteursb(r)
    )

    output$plot_mv_stimmung <- highcharter::renderHighchart(
      plot_mv_stimmung(r)
    )

    output$plot_mv_gender <- highcharter::renderHighchart(
      plot_mv_genderb()
    )

    # SKf ----

    # Tab 1
    output$plot_skf_einrichtungen <- highcharter::renderHighchart({
      skf_einrichtungen(r)
    })


    #
    # Tab 2

    output$plot_skf_personal <- highcharter::renderHighchart({
      skf_personal(r)
    })
    #




  })
}
