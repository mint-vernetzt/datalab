#' schule_kurse UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_ui <- function(id){
  ns <- NS(id)
  tagList(

    # Banner
    fluidRow(
      shinydashboard::box(
        width = 12,
        img(src='www/Banner_Schule_BB.jpg',
            class = "img-responsive",
            #height = "150px", width = "150px",
            alt = "Banner Schule",
            style="display: block; margin-left: auto; margin-right: auto;"
        ))),

    fluidRow(
      shinydashboard::box(
        width = 9,
        titel = "MINT in der Schule",
        p(style = "text-align: justify; font-size = 16px",
          span(tags$b(span("Kurzbeschreibung der Seite:", style = "color:#b16fab")),
          "Auf dieser Seite zeigen wir, wie hoch der Anteil von MINT-Fächern gemessen an allen gewählten Grund- und Leistungskursen ist.
          Je nach Bundesland wählen alle Oberstufen-Schülerinnen und -Schüler mehrere Grund- und Leistungskurse.
          Anhand dieser Belegungszahlen haben wir den Anteil von MINT-Fächern in der Schule berechnet."),
          br(),
          p(style = "text-align: justify; font-size = 16px",
          span(tags$b(span("Quelle der Daten:", style = "color:#b16fab")), "KMK, 2021, auf Anfrage, eigene Berechnungen.")),
        p(style = "text-align: justify; font-size = 16px",
          span(tags$b(span("Methodische Hinweise:", style = "color:#b16fab")),
               "Anders als bei Studierenden oder Auszubildenden wählen Schüler:innen mehrere Grund- und Leistungskurse und können entsprechend nicht
               eindeutig als \"MINT\" oder \"nicht MINT\" eingruppiert werden. Um dennoch einen Anteil von MINT versus nicht MINT angeben zu können,
               nutzen wir die Kursbelegungszahlen der Schüler:innen. Auf die Ausweisung absoluter Zahlen verzichten wir, da aus den Belegungszahlen
               nicht die Gesamtzahl aller Schüler:innen abgeleitet werden kann. Der Vergleich auf dieser Seite erfolgt entsprechend den Belegungszahlen
               der verschiedenen Kurse.", br(), br(),
               "Für Baden-Würtemberg liegen die Belegungszahlen nur für Mädchen und Jungen aggregiert vor.", br(),
               "In Bayern gibt keine frei wählbaren Leistungskurse: Die Grundlagenfächer Deutsch, Mathematik und eine fortgeführte Fremdsprache
                sind für alle Schülerinnen und Schüler an Gymnasien in Bayern verpflichtende Abiturprüfungsfächer und werden hier als Leistungskurse gezählt.
               Die Grundlagenfächer können nur an anderen Schulformen als Grundkurse gewählt werden und entsprechend sind
               die Anteile der Grundlagenfächer an den Grundkursen sehr gering.", br(), br(),
               "Weitere Statistiken über die Belegung von MINT-Fächern in anderen Klassenstufen liegen uns derzeit nicht vor.",
               br(),br(),
               "Die Rundung der berechneten Werte kann zu minimalen Abweichungen zwischen den Grafiken führen."
               ))
      )),
      shinydashboard::box(
        title = "Auf dieser Seite",
        width = 3,
        p(style = "text-align: justify; font-size = 16px",
          span(tags$b(span("#MINT:")), "Wie hoch ist der Anteil von MINT-Fächern in der Oberstufe? Und wie unterscheidet sich die Fächerwahl zwischen Mädchen und Jungen?"
          )),
        p(style = "text-align: justify; font-size = 16px",
          span(tags$b(span("#MINT im Detail:")), "Vergleich der einzelnen MINT-Fächer"
          )),
        p(style = "text-align: justify; font-size = 16px",
          span(tags$b(span("#Mädchen in MINT:")), "Wie hoch ist der Anteil von Mädchen in den MINT-Fächern?"
        )),

     )),

    fluidRow(
      shinydashboard::box(
        title = "#MINT: Wie hoch ist der Anteil von MINT-Fächern in der Oberstufe?",
        width = 12,
        p("In diesen interaktiven Diagrammen beleuchten wir den Anteil davon, wie häufig MINT-Fächer im Vergleich zu anderen Fächern in der Oberstufe in Deutschland belegt werden.",
          br(),
          br(),
        "Interpretationshilfe: In der ersten Einstellung ist zu sehen, dass in Deutschland 23 % aller gewählten Grundkurse aus dem Bereich MINT sind. Bei Leistungskursen liegt der Anteil im Jahr 2020 bei 34 %."),
        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich", br(),

                      tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                      shiny::sidebarPanel(
                        width = 3,
                        tags$style(".well {background-color:#FFFFFF;}"),
                        tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                        mod_schule_kurse_einstieg_ui("mod_schule_kurse_einstieg_ui_1")),
                      shiny::mainPanel(
                        width = 9,
                        htmlOutput(ns("plot_einstieg_pie")),p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2021, auf Anfrage, eigene Berechnungen."))
                            ),

                    tabPanel("Zeitverlauf", br(),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_einstieg_verlauf_ui("mod_schule_kurse_einstieg_verlauf_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_einstieg_verlauf")),p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2021, auf Anfrage, eigene Berechnungen."))
                             ),
                    tabPanel("Überblick", br(),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_einstieg_comparison_ui("mod_schule_kurse_einstieg_comparison_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_einstieg_comparison"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2021, auf Anfrage, eigene Berechnungen."))
                    ),


                    tabPanel("Vergleich: Fächerwahl Mädchen & Jungen", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_multiple_ui("mod_schule_kurse_multiple_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               plotOutput(ns("plot_waffle"))
                               ,p(style="font-size:12px;color:grey", br(),"Quelle der Daten: KMK, 2021, auf Anfrage, eigene Berechnungen."))
                    ),

                    tabPanel("Karte: Vergleich Mädchen & Jungen", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_map_gender_ui("mod_schule_kurse_map_gender_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               htmlOutput(ns("plot_map_kurse_gender"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2021, auf Anfrage, eigene Berechnungen."))
                    ),

                    # tabPanel("Vergleich (Bundesländer)", br(),
                    #
                    #          shiny::sidebarPanel(
                    #            mod_schule_kurse_verlauf_ui("mod_schule_kurse_verlauf_ui_1")),
                    #          shiny::mainPanel(
                    #
                    #            highcharter::highchartOutput(ns("plot_verlauf_kurse")))
                    # ),
                    tabPanel("Überblick Bundesländer (Mädchen & Jungen)", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_ranking_gender_ui("mod_schule_kurse_ranking_gender_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               plotOutput(ns("plot_ranking_gender"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2021, auf Anfrage, eigene Berechnungen."))

                    )
                    #,
                    #  tabPanel("Datensatz", br(),
                    #
                    #   tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                    #            .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                    #   shiny::sidebarPanel(
                    #     tags$style(".well {background-color:#FFFFFF;}"),
                    #     tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                    #     mod_schule_kurse_einstieg_ui("mod_schule_kurse_einstieg_ui_1")),
                    #   shiny::mainPanel(
                    #     div(DT::dataTableOutput(ns("data_table_einstieg")),
                    #         style = "font-size: 75%; width: 75%"),
                    #     shiny::downloadButton(ns("download_data_box1"), label = "",
                    #                           class = "butt",
                    #                           icon = shiny::icon("download")))
                    #         )
      ))),

    fluidRow(
      shinydashboard::box(
        title = "#MINT im Detail: Vergleich der einzelnen MINT-Fächer und Bundesländer",
        width = 12,
        p("Hier zeigen wir die Anteile einzelner MINT-Fächer in Deutschland sowie die Unterschiede in den Bundesländern. Berechnungsgrundlage sind wieder die Belegungszahlen aller Grund- und Leistungskurse.",
        br(), br(),
        "Interpretationshilfe: Auf der ersten Seite ist zu sehen, dass im Jahr 2020 deutschlandweit 23 % der Grundkurse aus dem MINT Bereich sind. Dabei sind Naturwissenschaften mit 14 %
        die am häufigsten belegte MINT-Disziplin. Bei den Leistungskursen sind 2020 34 % der Belegungen im MINT-Bereich, wobei Mathematik 17 % an allen Leistungskursbelegungen ausmacht."),
        tabsetPanel(type = "tabs",

                    tabPanel("Vergleich", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_multiple_mint_ui("mod_schule_kurse_multiple_mint_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               plotOutput(ns("plot_waffle_mint"))
                               ,p(style="font-size:12px;color:grey", br(), "Quelle der Daten: KMK, 2021, auf Anfrage, eigene Berechnungen."))
                    ),


                    tabPanel("Karte", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_map_ui("mod_schule_kurse_map_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               htmlOutput(ns("plot_map_kurse"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2021, auf Anfrage, eigene Berechnungen."))
                    ),
                    tabPanel("Vergleich Bundesländer", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_verlauf_multiple_ui("mod_schule_kurse_verlauf_multiple_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_verlauf_multiple"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2021, auf Anfrage, eigene Berechnungen."))
                    ),
                    tabPanel("Vergleich Fächer", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_verlauf_bl_subjects_ui("mod_schule_kurse_verlauf_bl_subjects_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_verlauf_kurse_bl_subjects"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2021, auf Anfrage, eigene Berechnungen.")))
                    ,
                    tabPanel("Überblick Fächer", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_comparison_subjects_ui("mod_schule_kurse_comparison_subjects_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_comparison_subjects"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2021, auf Anfrage, eigene Berechnungen.")

                             )),
                    tabPanel("Überblick Bundesländer", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_comparison_bl_ui("mod_schule_kurse_comparison_bl_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_comparison_bl"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2021, auf Anfrage, eigene Berechnungen.")))

        ))),
    fluidRow(
      shinydashboard::box(
        title = "#Mädchen in MINT: Wie hoch ist der Anteil von Mädchen in den MINT-Fächern?",
        width = 12,
        p("Hier schauen wir uns die Verteilung von Mädchen und Jungen innerhalb der MINT-Fächer in Deutschland an. Zum Vergleich
          zeigen wir auch den Anteil in den anderen, nicht-MINT-Fächern. Die verschiedenen Diagramme bieten außerdem
          Fächer- und Bundeslandvergleiche.",
          br(), br(),
        "Interpretationshilfe: Die erste Darstellung zeigt, dass der Anteil von Mädchen bzw. Frauen in allen MINT-Grundkursen
        in Deutschland 2020 53 % beträgt. In den MINT-Leistungskursen beträgt dieser Anteil 48 %. Im Vergleich
        mit anderen, nicht-MINT Fächern sieht man, dass in ihnen der Anteil an Mädchen bzw. Frauen etwas höher ist: In Grundkursen anderer Fächer machen Frauen 55 % aus, in Leistungskursen sogar 58 %."),
        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_schule_kurse_pie_gender_ui("mod_schule_kurse_pie_gender_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               htmlOutput(ns("plot_pie_gender"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2021, auf Anfrage, eigene Berechnungen."))
                    ),

                    # tabPanel("Zeitverlauf MINT", br(), #kann raus
                    #
                    #          tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                    #                        .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                    #          shiny::sidebarPanel(
                    #            tags$style(".well {background-color:#FFFFFF;}"),
                    #            tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                    #            mod_schule_kurse_verlauf_gender_ui("mod_schule_kurse_verlauf_gender_ui_1")),
                    #          shiny::mainPanel(
                    #            highcharter::highchartOutput(ns("plot_verlauf_gender")))
                    # ),

                    # Fehler drin: erstmal raus:
                    # tabPanel("Zeitverlauf", br(),
                    #
                    #          shiny::sidebarPanel(
                    #            mod_schule_kurse_verlauf_bl_ui("mod_schule_kurse_verlauf_bl_ui_1")),
                    #          shiny::mainPanel(
                    #            highcharter::highchartOutput(ns("plot_verlauf_kurse_bl"))
                    #            ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2021, auf Anfrage, eigene Berechnungen."))
                    # ),
                    tabPanel("Vergleich Fächer", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_ranking_ui("mod_schule_kurse_ranking_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               plotOutput(ns("plot_ranking_2"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2021, auf Anfrage, eigene Berechnungen."))
                             ),

                    tabPanel("Überblick", br(),


                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_schule_kurse_comparison_gender_ui("mod_schule_kurse_comparison_gender_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_comparison_gender"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: KMK, 2021, auf Anfrage, eigene Berechnungen."))
                    )
                    ))),


    # Footer

    tags$footer(style="text-align: justify;background-color:white",

                div(style="display: inline-block;position: relative;padding: 1em;",

                    tags$a(href="https://mint-vernetzt.de/",
                           img(src='www/MINTv_tranparent.png',
                               class = "img-responsive",
                               height = "100px", width = "100px",
                               alt = "Logo MINT", target="_blank",
                               style="display: inline-block; margin-left: auto; margin-right:10%;"))),

                div(style="display: inline-block;position: relative;padding: 1em;",

                    p(tags$a("Impressum", href="#shiny-tab-impressum", "data-toggle" = "tab")," | ",
                      tags$a("Kontakt", href="#shiny-tab-kontakt", "data-toggle" = "tab")," | ",
                      tags$a("Datenschutz", href="#shiny-tab-datenschutz", "data-toggle"="tab"),HTML('&nbsp;'),HTML('&nbsp;'),
                      "Copyright © 2022. Alle Rechte vorbehalten Stifterverband")),

                div(style="display: inline-block;position: relative;padding: 1em;",

                    tags$a(#href="https://www.bmbf.de/bmbf/de/home/home_node.html",
                           img(src='www/BMBF-Logo_transp1.png',

                               class = "img-responsive",

                               height = "200px", width = "200px",

                               alt = "Logo BMBF", target="_blank",

                               style="display: inline-block; margin-left: auto; margin-right: auto;"))),

                div(style="display: inline-block;width: 100%;",

                    " ")


  ))
  #)
}

#' schule_kurse Server Functions
#'
#' @noRd
mod_schule_kurse_server <- function(id, data_kurse, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Box 2
    output$plot_einstieg_pie <- renderUI({
      kurse_einstieg_pie(data_kurse,r)
    })

    output$plot_einstieg_verlauf <- highcharter::renderHighchart({
      kurse_verlauf_single(data_kurse,r)
    })

    output$plot_einstieg_comparison <- highcharter::renderHighchart({
      kurse_einstieg_comparison(data_kurse,r)
    })

    data_table_einstieg_react <- reactive({
      data_einstieg_kurse(data_kurse, r)
    })

    output$data_table_einstieg <- DT::renderDT({
      data_table_einstieg_react()
    })

    # Box 3
    output$plot_pie_gender <- renderUI({
      kurse_einstieg_pie_gender(data_kurse,r)
    })

    output$plot_verlauf_gender <- highcharter::renderHighchart({
      kurse_verlauf_gender(data_kurse,r)
    })

    output$plot_comparison_gender <- highcharter::renderHighchart({
      kurse_comparison_gender(data_kurse,r)
    })

    # Box 4
    output$plot_waffle_mint <- renderPlot({
      kurse_waffle_mint(data_kurse,r)
    })

    output$plot_verlauf_kurse_bl_subjects <- highcharter::renderHighchart({
      kurse_verlauf_subjects_bl(data_kurse,r)
    })

    output$plot_comparison_subjects <- highcharter::renderHighchart({
      kurse_mint_comparison(data_kurse,r)
    })

    # Box 5
    plot_waffle_react <- reactive({
      kurse_waffle(data_kurse,r)
    })

    output$plot_waffle <- renderPlot({
      plot_waffle_react()
    })

    output$plot_verlauf_kurse_bl <- highcharter::renderHighchart({
      kurse_verlauf_single_bl(data_kurse,r)
    })

    plot_ranking_react <- reactive({
      kurse_ranking(data_kurse,r, type="other")
    })

    output$plot_ranking_2 <- renderPlot({
      plot_ranking_react()
    })

    # Box 6
    output$plot_map_kurse <- renderUI({
      kurse_map(data_kurse,r)
    })

    output$plot_verlauf_multiple <- highcharter::renderHighchart({
      kurse_verlauf_multiple_bl(data_kurse,r)
    })

    output$plot_comparison_bl <- highcharter::renderHighchart({
      kurse_mint_comparison_bl(data_kurse,r)
    })

    # output$plot_comparison_bl <- renderPlot({
    #   kurse_mint_comparison_bl(data_kurse,r)
    # })

    # Box 7
    output$plot_map_kurse_gender <- renderUI({
      kurse_map_gender(data_kurse,r)
    })

    output$plot_verlauf_kurse <- highcharter::renderHighchart({
      kurse_verlauf(data_kurse,r)
    })

    output$plot_ranking_gender <- renderPlot({
      kurse_ranking_gender(data_kurse,r)
    })

    # downloader
    output$download_data_box1 <- shiny::downloadHandler(
      filename = function() {
        paste("data_kurse", "csv", sep = ".")
      },
      content = function(file){
        write.csv(data_table_einstieg_react(), file)
      }
    )

  })
}

## To be copied in the UI
# mod_schule_kurse_ui("schule_kurse_1")

## To be copied in the server
# mod_schule_kurse_server("schule_kurse_1")
