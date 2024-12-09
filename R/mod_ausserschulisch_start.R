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
      shinydashboard::box(
        width = 12,
        img(src='www/Banner_Außerschulisch.jpg',
            class = "img-responsive",
            #height = "150px", width = "150px",
            alt = "Banner Außerschulisch",
            style="display: block; margin-left: auto; margin-right: auto;"
        ))),


    # Info-Texte

    fluidRow(
      shinydashboard::box(
        title = "Auf dieser Seite",
        width = 7,
        p(style = "text-align: left; font-size = 16px",
          "Auf dieser Seite zeigen wir Daten aus dem außerschulischen MINT-Bildungsbereich.
          Amtliche Statistiken für den außerschulischen Bereich liegen nicht vor. Hier wollen wir
          schrittweise immer mehr weiße Flecken der außerschulischen MINT-Bildungslandschaft füllen."),
        p("Erstens zeigen wir Daten von MINTvernetzt. Hier nutzen wir die Daten der Community Plattform und
          stellen dar, wie viele Organsiationen, Projekte und Personen in welchen Bereichen der außerschulischen MINT-Bildung
          tätig sind. Außerdem zeigen wir eine Auswahl an Ergebnisse von Befragungen von MINTvernetzt."),
        p("Zweitens zeigen wir Daten aus der außerschulischen MINT-Bildungs-Community. Aktuell sind das Zahlen der Stiftung
          Kinder forschen.")
      ),

      shinydashboard::box(
        title = "Fragen oder Feedback?",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          "Sind alle Zahlen und Grafiken verständlich dargestellt? Haben Sie eigene Daten, die für Akteur:innen in der MINT-Bildung interessant sein könnten?", br(),
          "Kontaktieren Sie uns gern! Wir freuen uns über Rückfragen, Vorschläge oder Feedback ", tags$a(href = "mailto:katharina.brunner@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per E-Mail"),"oder über unsere kurze",
          tags$a(href="https://survey.lamapoll.de/MINT-DataLab_Feedback/", "Umfrage", target="_blank"), "!"
        ))
    ),

    fluidRow(
      shinydashboard::box(
        title = "Links zu den Themen dieser Seite",
        width = 7,
        p(style = "text-align: left; font-size = 16px",tags$a(href="#ausserschulisch_community_plattform",
                                                              span(tags$b(span("Außerschulische MINT-Aktreur:innen:")))),"Unsere MINTverentzt-Community in Zahlen."
        ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#ausserschulisch_befragung",
                                                              span(tags$b(span("MINTvernetzt Befragungen:")))),"Was sagt die Community über sich und MINT in Deutschland."

        ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#ausserschulisch_skf",
                                                              span(tags$b(span("Frühkindliche Bildung:")))),"Zahl der MINT-aktiven Einrichtungen bei Stiftung Kinder forschen wächst stetig."
        ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#ausserschulisch_community_daten",
                                                              span(tags$b(span("Daten aus der Community:")))),"Daten und Ergebnisse aus MINT-Projekten."
        )),

      shinydashboard::box(
        title = "Datenquellen",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          "Daten zu den außerschulische MINT-Akteur:innen und Befragungen: Quelle MINTvernetzt, Stand November 2024."),
        p(style = "text-align: left; font-size = 16px",
          "Daten zu frühklindlicher Bildung: Quelle Stiftung Kinder forschen, 2023."),
      )
    ),


    # Trennlinie
    p("Daten von MINTvernetzt", style = "color: #00A87A;
      font-size: 22px; font-family: 'SourceSans3-Bold';"),
    hr(style = "border-top: 3px solid #00A87A; margin-top: 5px"),

    # Community Plattform ----

    fluidRow( id="ausserschulisch_community_plattform",
              shinydashboard::box(
                title = "Außerschulische MINT-Akteur:innen: Unsere Community in Zahlen.",
                width = 12,
                column(
                  width = 8,
                  p("Text kommt noch...")
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
                                   # br(),br(),
                                   # downloadButton(
                                   #   outputId = ns("download_btn_plot___"),
                                   #   label = "Download",
                                   #   icon = icon("download")),
                                 ),
                                 shiny::mainPanel(
                                   width = 9,
                                   shinycssloaders::withSpinner(
                                     highcharter::highchartOutput(ns("plot_cp_orgas"), height = "600px"),
                                                                color = "#154194"),

                                   p(style="font-size:12px;color:grey", "Quelle der Daten: MINTvernetzt Community Plattform, Stand November 2024."),
                                   # shinyBS::bsPopover(id = "h_beruf_mint_3", title = "",
                                   #                    content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot"),
                                   #                    placement = "top",
                                   #                    trigger = "hover"),
                                   # tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_mint_3")
                                 )
                              ),
                              tabPanel("Projekte der MINTvernetzt-Community", br(),

                                       shiny::sidebarPanel(
                                         width = 3,
                                         tags$style(".well {background-color:#FFFFFF;}"),
                                         tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                                         mod_ausserschulisch_cp_projekte_ui("mod_ausserschulisch_cp_projekte_ui"),
                                         # br(),br(),
                                         # downloadButton(
                                         #   outputId = ns("download_btn_plot___"),
                                         #   label = "Download",
                                         #   icon = icon("download")),
                                       ),
                                       shiny::mainPanel(
                                         width = 9,
                                         shinycssloaders::withSpinner(
                                           highcharter::highchartOutput(ns("plot_cp_projekte"), height = "600px"),
                                                                      color = "#154194"),

                                         p(style="font-size:12px;color:grey", "Quelle der Daten: MINTvernetzt Community Plattform, Stand November 2024."),
                                         # shinyBS::bsPopover(id = "h_beruf_mint_3", title = "",
                                         #                    content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot"),
                                         #                    placement = "top",
                                         #                    trigger = "hover"),
                                         # tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_mint_3")
                                       )
                              ),
                              tabPanel("Profile der MINTvernetzt-Community", br(),

                                       shiny::sidebarPanel(
                                         width = 3,
                                         tags$style(".well {background-color:#FFFFFF;}"),
                                         tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                                         mod_ausserschulisch_cp_profile_ui("mod_ausserschulisch_cp_profile_ui"),
                                         # br(),br(),
                                         # downloadButton(
                                         #   outputId = ns("download_btn_plot___"),
                                         #   label = "Download",
                                         #   icon = icon("download")),
                                       ),
                                       shiny::mainPanel(
                                         width = 9,
                                         shinycssloaders::withSpinner(
                                           highcharter::highchartOutput(ns("plot_cp_profile"), height = "600px"),
                                                                      color = "#154194"),

                                         p(style="font-size:12px;color:grey", "Quelle der Daten: MINTvernetzt Community Plattform, Stand November 2024."),
                                         # shinyBS::bsPopover(id = "h_beruf_mint_3", title = "",
                                         #                    content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot"),
                                         #                    placement = "top",
                                         #                    trigger = "hover"),
                                         # tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_mint_3")
                                       )
                              ),
                              tabPanel("Informationen zu den Daten", br(),
                                       column(
                                         width = 8,
                                         p("Die hier gezeigten Daten haben keinen Anspruch darauf, den MINT-Bildungsbereich vollständig abzubilden.
                                         Wir geben hier einen Einblick in die Daten, welche MINTvernetzt vorliegen."),
                                         div(
                                           style = "display: flex; justify-content: center; align-items: center; margin-bottom: 25px;",
                                           img(src='www/Ausserschulisch_cp_Zahlen.png',
                                               class = "img-responsive",
                                               #height = "150px", width = "150px",
                                               alt = "Bild Zahlen Nutzung Community Plattform")
                                         ),
                                         p("Nicht alle Profile sind vollständig ausgefüllt. In den hier gezeigten Darstellungen werden alle die Profile berücksichtigt,
                                         die zu der betrachteten Eigenschaft Angaben gemacht haben."),
                                         div(
                                           style = "display: flex; justify-content: center; align-items: center; margin-bottom: 25px;",
                                           img(src='www/Ausserschulisch_cp_Ausfuellstatus.png',
                                               class = "img-responsive",
                                               width = "80%",
                                               alt = "Bild Ausfuellstatus Community Plattform"
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
          src = "www/Banner_CP_Absrpung.png",
          alt = "Community Plattform Aufruf",
          style = "max-width: 100%; height: auto; cursor: pointer;
          margin-bottom: 20px;"
        )
      )
    ),

    # MV-Befragungen ----

    fluidRow( id="ausserschulisch_mint-vernetzt_befragungen",
              shinydashboard::box(
                title = "Was sagt die MINTvernetzt-Community?",
                width = 12,
                column(
                  width = 8,
                  p("Text kommt noch.")
                ),
                column(
                  width = 12,

                  tabsetPanel(type = "tabs",
                              # Tab 1
                              tabPanel("Akteursbefragung: Wer sind die außerschulisch MINT-Akteur:innen", br(),

                                       shiny::sidebarPanel(
                                         width = 3,
                                         tags$style(".well {background-color:#FFFFFF;}"),
                                         tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                                         mod_ausserschulisch_mvb_akteursbefragung_ui("mod_ausserschulisch_mvb_akteursbefragung_ui_1"),
                                         # br(),br(),
                                         # downloadButton(
                                         #   outputId = ns("download_btn_plot___"),
                                         #   label = "Download",
                                         #   icon = icon("download")),
                                       ),
                                       shiny::mainPanel(
                                         width = 9,
                                         shinycssloaders::withSpinner(
                                           highcharter::highchartOutput(ns("plot_mvb_akteursbefragung"), height = "600px"),
                                                                      color = "#154194"),

                                         p(style="font-size:12px;color:grey", "Quelle der Daten: MINTvernetzt Community Plattform, Stand November 2024."),
                                         # shinyBS::bsPopover(id = "h_beruf_mint_3", title = "",
                                         #                    content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot"),
                                         #                    placement = "top",
                                         #                    trigger = "hover"),
                                         # tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_mint_3")
                                       )
                              ),
                              # Tab 2
                              tabPanel("Stimmungabarometer: Wie soll der Ganztag gestaltet werden?", br(),
                                       shiny::sidebarPanel(
                                         width = 3,
                                         tags$style(".well {background-color:#FFFFFF;}"),
                                         tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                                         mod_ausserschulisch_mvb_stimmungsb_ui("mod_ausserschulisch_mvb_stimmungsb_ui"),
                                         # br(),br(),
                                         # downloadButton(
                                         #   outputId = ns("download_btn_plot___"),
                                         #   label = "Download",
                                         #   icon = icon("download")),
                                       ),
                                       shiny::mainPanel(
                                         width = 9,
                                         shinycssloaders::withSpinner(
                                           highcharter::highchartOutput(ns("plot_mv_stimmung"), height = "600px"),
                                           color = "#154194"),

                                         p(style="font-size:12px;color:grey", "Quelle der Daten: MINTvernetzt Community Plattform, Stand November 2024."),
                                         # shinyBS::bsPopover(id = "h_beruf_mint_3", title = "",
                                         #                    content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot"),
                                         #                    placement = "top",
                                         #                    trigger = "hover"),
                                         # tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_mint_3")
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
                                        width = 6,
                                        div(
                                          p(strong("MINTvernetzt-Akteursbefragung:")),
                                          p(tags$i("Ziel der Umfrage:"), "Übersicht über die MINT-Bildungsakteur:innen und ihre Bedarfe und Herausforderungen herstellen.", br(),
                                            tags$i("Teilnehmendenzahl:"), "221 Personen haben an der Umfrage teilgenommen.",br(),
                                            tags$i("Befragung:"), "Durchgeführt zwischen Juni und Juli 2024. Die Umfrage wurde über Kanäle von MINTvernetzt geteilt.")
                                        )
                                      ),
                                      column(
                                        style = "top: 15px;",
                                        width = 3,
                                        tags$a(
                                          href = "https://www.mint-vernetzt.de/studien-und-umfragen/",
                                          target = "_blank",
                                          tags$img(src = "www/Außerschulisch_Befragung_Akteur.png",
                                                   style = "position: relative; float: inline-end;
                                                 right: 15px; height: auto; width: 200px;")
                                        )
                                      )
                                ),
                                # Zweite Befragung
                                fluidRow(
                                  style= "top: 15px;",
                                  column(
                                    style = "top: 15px;",
                                    width = 6,
                                    div(
                                      p(strong("MINT-Stimmungsbarometer:")),
                                      p(tags$i("Ziel der Umfrage:"), "Einblicke in die Stimmung zur Qualität der MINT-Bildung in Deutschland erlangen.", br(),
                                        tags$i("Teilnehmendenzahl:"), "454 Personen haben an der Umfrage teilgenommen.",br(),
                                        tags$i("Befragung:"), "Durchgeführt zwischen August und September 2024. Die Umfrage wurde über Kanäle von MINTvernetzt und
                                                              des Stifterverbands an Vertreter:innen von Bildung, Wirtschaft und Wissenschaft versandt.")
                                    )
                                  ),
                                  column(
                                    style = "top: 15px;",
                                    width = 3,
                                    tags$a(
                                      href = "https://www.mint-vernetzt.de/studien-und-umfragen/#stimmungsbarometer",
                                      target = "_blank",
                                      tags$img(src = "www/Außerschulisch_Befragung_Stimmung.png",
                                               style = "position: relative; float: inline-end;
                                                 right: 15px; height: auto; width: 200px;")
                                    )
                                  )
                                ),
                                # Dritte Befragung
                                fluidRow(
                                  style= "top: 15px;",
                                  column(
                                    style = "top: 15px;",
                                    width = 6,
                                    div(
                                      p(strong("MINTvernetzt-Genderbefragung:")),
                                      p(tags$i("Ziel der Umfrage:"), "Synergien und nachhaltige Fördereffekte für Mädchen in MINT schaffen, aufbauend auf den
                                                                      Erfahrungen der MINT-Bildungsanbieter:innen.", br(),
                                        tags$i("Teilnehmendenzahl:"), "456 Personen haben an der Umfrage teilgenommen.",br(),
                                        tags$i("Befragung:"), "Durchgeführt zwischen November und Dezember 2023. Die Umfrage wurde über die Kanäle von MINTvernetzt geteilt.")
                                    )
                                  ),
                                  column(
                                    style = "top: 15px;",
                                    width = 3,
                                    tags$a(
                                      href = "https://www.mint-vernetzt.de/studien-und-umfragen/",
                                      target = "_blank",
                                      tags$img(src = "www/Außerschulisch_Befragung_Gender.png",
                                               style = "position: relative; float: inline-end;
                                                 right: 15px; height: auto; width: 200px;")
                                    )
                                  )
                                )

                               )
                  )
                )
              )
    ),

    # Trennlinie
    p("Daten von MINT-Bildungsakteur:innen", style = "color: #00A87A;
      font-size: 22px; font-family: 'SourceSans3-Bold';"),
    hr(style = "border-top: 3px solid #00A87A; margin-top: 5px"),

    # SKf ----

    fluidRow(id="ausserschulisch_skf",
             shinydashboard::box(
               title = "Frühkindliche Bildung: Zahl der MINT-aktiven Einrichtungen bei Stiftung Kinder forschen wächst stetig.",
               width = 12,
               column(
                 width = 8,
                 p("In diesem Abschnitt betrachten wir die Entwicklung der außerschulischen, frühkindlichen MINT-Bildung.
               Die interaktiven Grafiken basieren auf den Daten der 'Stiftung Kinder forschen'
               (kurz SKf; früher: 'Haus der kleinen Forscher')."),

                 p("Die Anzahl an Kitas, Grundschulen und Horte, die durch die Stiftung Kinder forschen für ihr MINT-Bildungsengagement
               zertifiziert wurden oder deren Personal durch die SKf fortgebildet wurde, wächst. Allerdings hat sich während der Jahre,
               die akut von der Corona-Pandemie betroffen waren, verlangsamt. Das spiegelt sich auch in den Zahlen der neu fortgebildeten Personen wider.
               Während zwischen 2013 und 2018 jährlich zwischen 6.000 und 8.000 Personen dazukamen, sind es im Jahr 2022 nur 2.000."), br(),
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
                                        # br(), br(),
                                        # downloadButton(
                                        #   outputId = ns("download_btn_plot_skf_einrichtungen"),
                                        #   label = "Download",
                                        #   icon = icon("download"))
                                      ),
                                      shiny::mainPanel(
                                        width = 9,
                                        shinycssloaders::withSpinner(htmlOutput(ns("plot_skf_einrichtungen")),
                                                                     color = "#154194"),

                                        p(style="font-size:12px;color:grey",
                                          "Quelle der Daten: Stiftung Kinder forschen, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
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
                                        # br(), br(),
                                        # downloadButton(
                                        #   outputId = ns("download_btn_plot_skf_personal"),
                                        #   label = "Download",
                                        #   icon = icon("download"))
                                      ),
                                      shiny::mainPanel(
                                        width = 9,
                                        shinycssloaders::withSpinner(htmlOutput(ns("plot_skf_personal")),
                                                                     color = "#154194"),

                                        p(style="font-size:12px;color:grey",
                                          "Quelle der Daten: Stiftung Kinder forschen, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
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


    # SKf ----

    # Tab 1
    # output$plot_skf_einrichtungen <- highcharter::renderHighchart({
    #   skf_einrichtungen(r)
    # })

    output$plot_skf_einrichtungen <- renderUI({
      plot_list <- skf_einrichtungen(r)
      r$plot_skf_einrichtungen <- plot_list

      r$plot_skf_einrichtungen_title <- get_plot_title(
        plot = r$plot_skf_einrichtungen
      )

      plot_list

    })

    output$download_btn_plot_skf_einrichtungen <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_skf_einrichtungen_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_skf_einrichtungen,
          filename =  r$plot_skf_einrichtungen_title,
          width = 700,
          height = 400)

        file.copy(r$plot_skf_einrichtungen_title, file)
        file.remove(r$plot_skf_einrichtungen_title)
      }
    )

    # Tab 2

    # output$plot_skf_personal <- highcharter::renderHighchart({
    #   skf_personal(r)
    # })

    output$plot_skf_personal <- renderUI({
      plot_list <- skf_personal(r)
      r$plot_skf_personal <- plot_list

      r$plot_skf_personal_title <- get_plot_title(
        plot = r$plot_skf_personal
      )

      plot_list

    })

    output$download_btn_plot_skf_personal <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_skf_personal_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_skf_personal,
          filename =  r$plot_skf_personal_title,
          width = 700,
          height = 400)

        file.copy(r$plot_skf_personal_title, file)
        file.remove(r$plot_skf_personal_title)
      }
    )



  })
}
