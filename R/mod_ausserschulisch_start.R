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
          "Auf dieser Seite ...")
      ),

      shinydashboard::box(
        title = "Fragen oder Feedback?",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          "Sind alle Zahlen und Grafiken verständlich dargestellt?", br(), "Wir freuen uns über Rückfragen oder Feedback ", tags$a(href = "mailto:katharina.brunner@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per E-Mail"),"oder über unsere kurze",
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
          "Daten zu den außerschulische MINT-Akteur:innen und Befragungen: Quelle MINTvernetzt."),
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
                  p("Text kommt noch.")
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
                                   shinycssloaders::withSpinner(htmlOutput(ns("plot_cp_orgas")),
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
                                         shinycssloaders::withSpinner(htmlOutput(ns("plot_cp_projekte")),
                                                                      color = "#154194"),

                                         p(style="font-size:12px;color:grey", "Quelle der Daten: MINTvernetzt Community Plattform, Stand November 2024."),
                                         # shinyBS::bsPopover(id = "h_beruf_mint_3", title = "",
                                         #                    content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot"),
                                         #                    placement = "top",
                                         #                    trigger = "hover"),
                                         # tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_mint_3")
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
          style = "max-width: 100%; height: auto; cursor: pointer;"
        )
      )
    ),
    # div(class = "reference-box",
    #     style = "display: flex; align-items: stretch; padding: 0px; margin-bottom: 10px;
    #     color: #154194; background-color: #efe8e6",
    #     width = 12,
    #     column(
    #       style = "padding: 0;",
    #       width = 4,
    #       img(src='www/CP_screenshot.png',
    #           class = "responsive-image",
    #           # height = "800px",
    #           #width = "150px",
    #           alt = "Community Plattform Screenshot",
    #           style="max-width: 100%; heigth: auto; object-fit: cover;"
    #       )
    #     ),
    #     column(
    #       width = 8,
    #       div(class = "inner-box",
    #           style = "margin: 0; padding: 0px; background-color: #efe8e6;
    #           display: flex; flex-direction:column; justify-content: center;",
    #
    #           p(br(),strong("Vervollständige die Daten der außerschulischen MINT-Bildung"), br()),
    #           p(
    #             strong("Legen Sie Ihre Organisation auf der Community Plattform von MINTvernetzt an
    #                  oder füllen Sie Ihre Projekt-Informationen weiter aus.
    #                  So können Sie dabei mitwirken, die Datenlücke der außerschulischen
    #                  MINT-Bildung zu reduzieren."),
    #             br(), br(),
    #             tags$a(href = "https://community.mint-vernetzt.de/",
    #                    target = "_blank", "Link zur Community Plattform",
    #                    style = "color: #154194"),
    #             br(), br())
    #
    #       )
    #     )
    # ),

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

    output$plot_cp_orgas <- renderUI({
      plot_cp_orgas(r)
    })

    output$plot_cp_projekte <- renderUI({
      plot_cp_projekte(r)
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
