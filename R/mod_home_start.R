#' home_start UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_start_ui <- function(id){
  ns <- NS(id)
  tagList(

    # Banner

       fluidRow(
         shinydashboard::box(
           width = 12,
           img(src='www/Banner_Alle_Bereiche.jpg',
               class = "img-responsive",
               #height = "300px",
               #width = "150px",
               alt = "Banner MINT entlang der Bildungskette",
               style="display: block; margin-left: auto; margin-right: auto;"
           )
           )),

 # Info-Texte

    fluidRow(
      shinydashboard::box(
        title = "Auf dieser Seite",
        width = 7,
        p(style = "text-align: left; font-size = 16px",
           "Auf dieser Überblicksseite geben wir einen ersten Einblick in die vorhandenen Daten und vergleichen die
             Bildungsbereiche miteinander. Auf den folgenden bereichsspezifischen Unterseiten gehen wir je Bildungsbereich
             mehr ins Detail und bieten zusätzlich Vergleiche auf Fächer- und Bundeslandebene.")),
      shinydashboard::box(
        title = "Fragen oder Feedback?",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          "Sind alle Zahlen und Grafiken verständlich?", br(),
          "Wir freuen uns über Rückfragen oder Feedback ", tags$a(href = "mailto:katharina.brunner@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per Email"),"oder über unsere kurze",
          tags$a(href="https://survey.lamapoll.de/MINT-DataLab_Feedback/", "Umfrage", target="_blank"), "!"
        ))
    ),

    fluidRow(
      shinydashboard::box(
        title = "Übersicht Fragestellungen",
        width = 7,
        p(style = "text-align: left; font-size = 16px",tags$a(href="#alle_mint",
        span(tags$b(span("Fächerwahl MINT:")))),"Wie hoch ist der Anteil von MINT entlang der Bildungskette?"
        ),

        p(style = "text-align: left; font-size = 16px",tags$a(href="#alle_frauen",
        span(tags$b(span("Frauen in MINT:")))),"Anteil von Frauen und Mädchen innerhalb von MINT in den verschiedenen Bildungsbereichen"))

        ,

      shinydashboard::box(
        title = "Datenquellen",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          "Studierendenzahlen: Destatis 2022, auf Anfrage"),
         p("Schüler:innenzahlen: KMK 2022, auf Anfrage"),
          p("Auszubildenden- und Beschäftigtenzahlen: Bundesagentur für Arbeit 2022, auf Anfrage")
        )

   ),

   # Box 1 ----

    fluidRow(id="alle_mint",
      shinydashboard::box(
        title = "Fächerwahl MINT: Wie hoch ist der Anteil von MINT entlang der Bildungskette?",
        width = 12,
        p("Die folgende interaktive Grafik gibt Antworten auf die Fragen: Wie hoch ist der Anteil von MINT-Fächern in der Schule? Wie hoch ist der Anteil von Studierenden, die MINT-Fächer belegen?
          Wie hoch ist der Anteil von Auszubildenden, die eine Ausbildung in MINT machen? Wie hoch ist der Anteil von Beschäftigten, die im MINT-Bereich arbeiten?"),

        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich Bereiche", br(),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_home_start_einstieg_ui("mod_home_start_einstieg_ui_1"),

                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_mint_rest_einstieg_1")),
                                                            color = "#154194"),
                               br(),
                               p(style="font-size:12px;color:grey",
                                 "Quellen: Statistisches Bundesamt,2022; Bundesagentur für Arbeit,2022; KMK, 2022, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt."),

                        shinyBS::bsPopover(id="h_alle_mint_1", title = "",
                                           content = paste0("Anders als z. B. bei Studierenden wählen Schüler:innen mehrere Grund- und Leistungskurse. Um dennoch einen Anteil von &quotMINT&quot vs. &quotnicht MINT&quot angeben zu können, nutzen wir die Kursbelegungszahlen der Schüler:innen."),
                                           placement = "top",
                                           trigger = "hover"),
                         tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_alle_mint_1")
                        )
                            ),
                    tabPanel("Zeitverlauf", br(),
                        shiny::sidebarPanel(
                          width = 3,
                          mod_home_start_multiple_ui("mod_home_start_multiple_ui_1"),
                          # br(),br(),
                          # downloadButton(
                          #   outputId = ns("download_btn_plot_mint_1"),
                          #   label = "Download",
                          #   icon = icon("download")),
                          ),
                        shiny::mainPanel(
                          width = 9,
                          shinycssloaders::withSpinner(htmlOutput(ns("plot_mint_1")),
                                                       color = "#154194"),
                          br(),
                          p(style="font-size:12px;color:grey", "Quellen: Statistisches Bundesamt,2023; Bundesagentur für Arbeit,2023; KMK, 2023, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                          shinyBS::bsPopover(id="h_alle_mint_2", title = "",
                                             content = paste0("Anders als z. B. bei Studierenden wählen Schüler:innen mehrere Grund- und Leistungskurse. Um dennoch einen Anteil von &quotMINT&quot vs. &quotnicht MINT&quot angeben zu können, nutzen wir die Kursbelegungszahlen der Schüler:innen."),
                                             placement = "top",
                                             trigger = "hover"),
                          tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_alle_mint_2")
                          )

                        )

                   ,
                    tabPanel("Überblick", br(),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_home_start_comparison_mint_ui("mod_home_start_comparison_mint_ui_1"),
                               # br(),br(),
                               # downloadButton(
                               #   outputId = ns("download_btn_plot_comparison_mint"),
                               #   label = "Download",
                               #   icon = icon("download"))
                               ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_comparison_mint")),
                                                            color = "#154194"),
                               br(),
                               p(style="font-size:12px;color:grey", "Quellen: Statistisches Bundesamt,2023; Bundesagentur für Arbeit,2023; KMK, 2023, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_alle_mint_3", title = "",
                                                  content = paste0("Anders als z. B. bei Studierenden wählen Schüler:innen mehrere Grund- und Leistungskurse. Um dennoch einen Anteil von &quotMINT&quot vs. &quotnicht MINT&quot angeben zu können, nutzen wir die Kursbelegungszahlen der Schüler:innen."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_alle_mint_3")
                             )

         ))
         )),
 # Box 2 ----
    fluidRow(id="alle_frauen",
      shinydashboard::box(
        title = "Frauen in MINT: Anteil von Frauen und Mädchen innerhalb von MINT in den verschiedenen Bildungsbereichen",
        width = 12,
        p("Die folgende interaktive Grafik gibt Antworten auf die Fragen: Wie hoch ist der Anteil von Mädchen in MINT-Leistungskursen?
          Wie hoch ist der Anteil von Frauen in MINT-Studienfächern? Wie hoch ist der Anteil von Frauen in MINT-Ausbildungsgängen?
          Wie hoch ist der Anteil von Frauen in MINT-Berufen?", br(),
        "Zum Vergleich zeigen wir jeweils auch, wie hoch der Anteil von Frauen in den anderen, nicht-MINT-Fächern oder -Berufszweigen ist."),

        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich Bereiche", br(),  # Verlgeich
                             shiny::sidebarPanel(
                               width = 3,
                               mod_home_start_einstieg_gender_ui("mod_home_start_einstieg_gender_ui_1")
                               ),

                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_pie_mint_gender")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey",
                                  "Quellen: Statistisches Bundesamt,2023; Bundesagentur für Arbeit,2023; KMK, 2023, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_alle_frauen_1", title = "",
                                                  content = paste0("Anders als z. B. bei Studierenden wählen Schüler:innen mehrere Grund- und Leistungskurse. Um dennoch einen Anteil von &quotMINT&quot vs. &quotnicht MINT&quot angeben zu können, nutzen wir die Kursbelegungszahlen der Schüler:innen.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_alle_frauen_1")
                               )
                            )
                    ,
                    tabPanel("Zeitverlauf", br(),
                        shiny::sidebarPanel(
                          width = 3,
                          mod_home_start_comparison_ui("mod_home_start_comparison_ui_1"),
                          # br(),br(),
                          # downloadButton(
                          #   outputId = ns("download_btn_plot_verlauf_mint"),
                          #   label = "Download",
                          #   icon = icon("download"))
                          ),
                        shiny::mainPanel(
                          width = 9,
                          shinycssloaders::withSpinner(htmlOutput(ns("plot_verlauf_mint")),
                                                       color = "#154194"),

                          p(style="font-size:12px;color:grey", "Quellen: Statistisches Bundesamt,2023; Bundesagentur für Arbeit,2023; KMK, 2023, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                          shinyBS::bsPopover(id="h_alle_frauen_2", title = "",
                                             content = paste0("Anders als z. B. bei Studierenden wählen Schüler:innen mehrere Grund- und Leistungskurse. Um dennoch einen Anteil von &quotMINT&quot vs. &quotnicht MINT&quot angeben zu können, nutzen wir die Kursbelegungszahlen der Schüler:innen.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden."),
                                             placement = "top",
                                             trigger = "hover"),
                          tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_alle_frauen_2")
                                        )
                            ),
                    tabPanel("Überblick", br(),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_home_start_comparison_mint_gender_ui("mod_home_start_comparison_mint_gender_ui_1"),
                             # br(),br(),
                             # downloadButton(
                             #   outputId = ns("download_btn_plot_comparison_gender"),
                             #   label = "Download",
                             #   icon = icon("download"))
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_comparison_gender")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey", "Quellen: Statistisches Bundesamt,2023; Bundesagentur für Arbeit,2023; KMK, 2023, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_alle_frauen_3", title = "",
                                                  content = paste0("Anders als z. B. bei Studierenden wählen Schüler:innen mehrere Grund- und Leistungskurse. Um dennoch einen Anteil von &quotMINT&quot vs. &quotnicht MINT&quot angeben zu können, nutzen wir die Kursbelegungszahlen der Schüler:innen.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_alle_frauen_3"))

                             )
                    )
        )
    )


  ,


# Footer
funct_footer())
}

#' home_start Server Functions
#'
#' @noRd
mod_home_start_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # Box 1, Tab1 ----


    output$plot_mint_rest_einstieg_1 <- renderUI({
      home_einstieg_pie( r)
    })


    # output$plot_verlauf_mint <- highcharter::renderHighchart({
    #   home_comparison_line(r)
    # })

    output$plot_comparison_gender <- highcharter::renderHighchart({
      home_stacked_comparison_gender(r)
    })

    # output$plot_mint_1 <- highcharter::renderHighchart({
    #   home_rest_mint_verlauf(r)
    # })

    # output$plot_comparison_mint <- highcharter::renderHighchart({
    #   home_stacked_comparison_mint( r)
    # })

    output$plot_pie_mint_gender <- renderUI({
      home_einstieg_pie_gender( r)
    })

    ### Downloads ----

    #### Box 1 ----

    #tab 2
    output$plot_mint_1 <- renderUI({
      plot_list <- home_rest_mint_verlauf(r)
      r$plot_mint_1 <- plot_list

      r$plot_mint_1_title <- get_plot_title(
        plot = r$plot_mint_1
      )

      plot_list
    })

    output$download_btn_plot_mint_1 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_mint_1_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_mint_1,
          filename =  r$plot_mint_1_title,
          width = 700,
          height = 400)

        file.copy(r$plot_mint_1_title, file)
        file.remove(r$plot_mint_1_title)
      }
    )

    # tab 3

    output$plot_comparison_mint <- renderUI({
      plot_list <-home_stacked_comparison_mint( r)
      r$plot_comparison_mint <- plot_list

      r$plot_comparison_mint_title <- get_plot_title(
        plot = r$plot_comparison_mint
      )

      plot_list
    })

    output$download_btn_plot_comparison_mint <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_comparison_mint_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_comparison_mint,
          filename =  r$plot_comparison_mint_title,
          width = 700,
          height = 400)

        file.copy(r$plot_comparison_mint_title, file)
        file.remove(r$plot_comparison_mint_title)
      }
    )


    #### Box 2 ----



    # tab 2

    output$plot_verlauf_mint <- renderUI({
      plot_list <- home_comparison_line(r)
      r$plot_verlauf_mint <- plot_list

      r$plot_verlauf_mint_title <- get_plot_title(
        plot = r$plot_verlauf_mint
      )

      plot_list
    })

    output$download_btn_plot_verlauf_mint <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_verlauf_mint_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_verlauf_mint,
          filename =  r$plot_verlauf_mint_title,
          width = 700,
          height = 400)

        file.copy(r$plot_verlauf_mint_title, file)
        file.remove(r$plot_verlauf_mint_title)
      }
    )

    # tab 3
    output$plot_comparison_gender <- renderUI({
      plot_list <-home_stacked_comparison_gender(r)
      r$plot_comparison_gender <- plot_list

      r$plot_comparison_gender_title <- get_plot_title(
        plot = r$plot_comparison_gender
      )

      plot_list
    })

    output$download_btn_plot_comparison_gender <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_comparison_gender_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_comparison_gender,
          filename =  r$plot_comparison_gender_title,
          width = 700,
          height = 400)

        file.copy(r$plot_comparison_gender_title, file)
        file.remove(r$plot_comparison_gender_title)
      }
    )




  })
}

## To be copied in the UI
# mod_home_start_ui("home_start_1")

## To be copied in the server
# mod_home_start_server("home_start_1")







# Alte Start-Box


# fluidRow(
#   shinydashboard::box(
#     #title = span("Willkommen im MINT-DataLab von MINTvernetzt", style = "color:#154194; font-size: 50px"),
#     width = 12,
#     column(width = 9,
#            tags$h1("WILLKOMMEN IM MINT-DATALAB"),
#            #tags$h1("Willkommen im MINT-DataLab"),
#            #p(style = "color:#154194; font-size: 50px", "Willkommen im MINT-DataLab"),
#            p(style = "text-align: justify; font-size = 16px",
#              "Im MINT-DataLab zeigen wir statistische Kennzahlen rund um MINT in den Bereichen Schule, Hochschule,
#                     Ausbildung und Arbeitsmarkt in Deutschland."
#            ),
#            p(style = "text-align: justify; font-size = 16px",
#              span("Unser", tags$b(span("Datenpool", style = "color:#b16fab")), "besteht aktuell aus Statistiken der Bundesagentur für Arbeit, des
#                          Statistischen Bundesamtes und der Kulturministerkonferenz. Weitere Datenquellen werden im Laufe
#                          der Zeit integriert. (Weitere Infos unter: Quellen & Hinweise)")
#            ),
#
#            p(style = "text-align: justify; font-size = 16px",
#              span("Wir freuen uns über ", tags$a(href = "mailto:antonia.kroeger@mint-vernetzt.de?subject=MINT-Datalab", "Feedback"),"!")
#            ),
#     ),
#     #solidHeader = TRUE,
#     #collapsible = FALSE,
#     #br(),
#     column(width = 3, href = "https://mint-vernetzt.de/", rel="noopener", target="_blank",
#            img(src='www/mint_logo_gross.jpg',
#                class = "img-responsive",
#                height = "180px", width = "180px",
#                alt = "Logo MINT",
#                style="display: block; margin-left: auto; margin-right: auto;"
#
#            ))
#   )
# )












