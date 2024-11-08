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
          "Daten zu den außerschulische MINT-Akteur:innen und Befragungen: Qulle MINTvernetzt."),
        p(style = "text-align: left; font-size = 16px",
          "Daten zu den außerschulische MINT-Akteur:innen und Befragungen: Qulle MINTvernetzt."),
      )
    ),


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
                              )
                            )
                )
              )
    ),

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
