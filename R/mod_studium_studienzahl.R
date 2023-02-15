#' studium_studienzahl UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#'

tab1_name <- "Vergleich"

mod_studium_studienzahl_ui <- function(id){
  ns <- NS(id)
  tagList(

    # Banner
    fluidRow(
      shinydashboard::box(
        width = 12,
        img(src='www/Banner_Studium.jpg',
            class = "img-responsive",
            #height = "150px", width = "150px",
            alt = "Banner Studium",
            style="display: block; margin-left: auto; margin-right: auto;"
        ))),

     fluidRow(
    shinydashboard::box(
      titel = "Studium in MINT",
      width = 9,
      p(style = "text-align: justify; font-size = 16px",
      span(tags$b(span("Kurzbeschreibung der Seite:", style = "color:#b16fab")),
             "Auf dieser Seite zeigen wir statistische Kennzahlen zum Thema MINT-Fächer studieren. Wir zeigen, wie hoch der Anteil
           von MINT-Fächern gemessen an allen gewählten Studienfächern ist. Dazu zeigen wir Vergleiche nach männlichen und
           weiblichen Studierenden und nach Bundesländern. Studierende können außerdem nach Hochschultyp unterschieden werden
           oder ob sie auf Lehramt oder im ersten Fachsemester studieren."
           )),
       # br(),
      p(style = "text-align: justify; font-size = 16px",
        span(tags$b(span("Quelle der Daten:", style = "color:#b16fab")), "Destatis, 2021, auf Anfrage, eigene Berechnungen.")),
      p(style = "text-align: justify; font-size = 16px",
        span(tags$b(span("Methodische Hinweise: ", style = "color:#b16fab")),
             "Die aktuellen Berechnungen erfolgen auf Basis der Studierendenzahlen an allen deutschen Hochschulen. ",
             br(),br(),
             "Die Rundung der berechneten Werte kann zu minimalen Abweichungen zwischen den Grafiken führen."
             ))
    ),
    shinydashboard::box(
      title = "Auf dieser Seite",
      width = 3,
      p(style = "text-align: justify; font-size = 16px",
        span(tags$b(span("#MINT:")), "Wie hoch ist der Anteil von Studierenden, die ein MINT-Fach studieren?"
        )),
      p(style = "text-align: justify; font-size = 16px",
        span(tags$b(span("#MINT im Detail:")), "Vergleich der einzelnen MINT-Fächer und Bundesländer"
        )),
      p(style = "text-align: justify; font-size = 16px",
        span(tags$b(span("#Frauen in MINT:")), "Wie hoch ist der Anteil von Frauen in den MINT-Fächern?"
      )),

      p(style = "text-align: justify; font-size = 16px",
        span(tags$b(span("#Fächerwahl von Frauen:")), "Wie unterscheidet sich die Fächerwahl von Frauen und Männern?"
      ),
      )

    )),

    fluidRow(
      shinydashboard::box(
        title = "#MINT: Wie hoch ist der Anteil von Studierenden, die ein MINT-Fach studieren?",
        width = 12,
        p("In diesen interaktiven Diagrammen beleuchten wir den Anteil von MINT-Fächern an allen Studienfächern in Deutschland.
          Dabei betrachten wir sowohl Studienanfänger:innen als auch Studierende allgemein. ",
          br(),
          br(),
          "Interpretationshilfe: In der ersten Einstellung ist zu sehen, dass in Deutschland 37 % der Studienanfänger:innen ein MINT-Fach studieren, bei den Studierenden sind es im Jahr 2020 ebenfalls 37 %."),
        tabsetPanel(type = "tabs",
                    tabPanel("Überblick", br(),

                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_test_ui("mod_studium_studienzahl_test_ui_1")),
                             shiny::mainPanel(
                               htmlOutput(ns("test")),
                               p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2021, auf Anfrage, eigene Berechnungen."))


                    ),

                    tabPanel("Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_verlauf_ui("mod_studium_studienzahl_einstieg_verlauf_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_einstieg_verlauf")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2021, auf Anfrage, eigene Berechnungen."))

                    ),
                    tabPanel("Überblick", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_comparison_ui("mod_studium_studienzahl_einstieg_comparison_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_einstieg_comparison")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2021, auf Anfrage, eigene Berechnungen."))
                    )

                  # ,
                  #
                  #
                  # tabPanel("Datensatz", br(),
                  #
                  #          tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                  #              .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                  #          shiny::sidebarPanel(
                  #            tags$style(".well {background-color:#FFFFFF;}"),
                  #            tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                  #            mod_studium_studienzahl_einstieg_ui("mod_schule_kurse_einstieg_ui_1")),
                  #          shiny::mainPanel(
                  #            div(DT::dataTableOutput(ns("data_table_einstieg")),
                  #                style = "font-size: 75%; width: 75%"),
                  #            shiny::downloadButton(ns("download_data_box1"), label = "",
                  #                                  class = "butt",
                  #                                  icon = shiny::icon("download")))
                  # )
        ))),

    fluidRow(
      shinydashboard::box(
        title = "#MINT im Detail: Vergleich der einzelnen MINT-Fächer und Bundesländer",
        width = 12,
        p("Hier zeigen wir, wie häufig MINT-Fächer im Vergleich zu anderen Studienfächern in Deutschland gewählt werden.
          Außerdem kann man den Anteil von MINT-Fächern zwischen den Bundesländern vergleichen. ",
          br(), br(),
          "Interpretationshilfe: In der ersten Darstellung sieht man, dass in Deutschland 2020 Ingenieurwissenschaften den Großteil der MINT-Fächer ausmachen:
          Unter den Studienanfänger:innen wählen 26 % und unter den Studierenden 27 % eine Ingenieurwissenschaft."),

        tabsetPanel(type = "tabs",

                    tabPanel("Vergleich", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_choice_1_ui("mod_studium_studienzahl_choice_ui_1_1")),
                             shiny::mainPanel(
                               width = 9,
                               plotOutput(ns("plot_waffle")),p(style="font-size:12px;color:grey", br(), "Quelle der Daten: Destatis, 2021, auf Anfrage, eigene Berechnungen."))
                    ),

                    tabPanel("Karte", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_bl_map_ui("mod_studium_studienzahl_bl_map")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               htmlOutput(ns("plot_studienzahl_map")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2021, auf Anfrage, eigene Berechnungen.")
                             )
                    ),

                    tabPanel("Vergleich (Fächer)", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_verlauf_bl_subject_ui("mod_studium_studienzahl_verlauf_bl_subject_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_verlauf_studienzahl_bl_subject"))
                               ,p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2021, auf Anfrage, eigene Berechnungen.")

                             )
                    ),

                    tabPanel("Vergleich (Bundesländer)", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_bl_verlauf_ui("mod_studium_studienzahl_bl_verlauf")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_studienzahl_bl_verlauf")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2021, auf Anfrage, eigene Berechnungen.")
                             )
                    ),

                    tabPanel("Überblick (Fächer)", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_ranking_bl_subject_ui("mod_studium_studienzahl_ranking_bl_subject_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_verlauf_studienzahl_bl_subject1")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2021, auf Anfrage, eigene Berechnungen.")
                               #plotOutput(ns("plot_ranking_bl_subject")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2021, auf Anfrage, eigene Berechnungen.")
                             )
                    ),

                    tabPanel("Überblick (Bundesländer)", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_bl_vergleich_ui("studium_studienzahl_bl_vergleich")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_vergleich_bl1")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2021, auf Anfrage, eigene Berechnungen.")
                               #plotOutput(ns("plot_vergleich_bl")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2021, auf Anfrage, eigene Berechnungen.")
                             )
                    )
        ))),

    fluidRow(
      shinydashboard::box(
        title = "#Frauen in MINT: Wie hoch ist der Anteil von Frauen in den MINT-Fächern?",
        width = 12,
        p("Hier schauen wir uns die Verteilung von Frauen und Männern in Deutschland innerhalb der MINT-Studienfächer an.
          Zum Vergleich zeigen wir auch den Anteil in den anderen, nicht-MINT-Fächern.
          Die verschiedenen Diagramme bieten außerdem Fächer- und Bundeslandvergleiche.",
          br(), br(),
          "Interpretationshilfe: In der ersten interaktiven Grafik ist beispielsweise zu sehen,
          dass deutschlandweit 2020 der Anteil von Frauen unter den Studienanfänger:innen in MINT-Fächern 34 % ausmacht.
          Unter den Studierenden liegt der Frauenanteil in MINT-Fächern bei 32 %. In nicht-MINT-Studienfächern
          machen Frauen dagegen die Mehrheit aus: 63 % der Studienanfänger:innen und 61 % der Studierenden sind hier in Deutschland 2020 weiblich."),
        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_gender_ui("mod_studium_studienzahl_einstieg_gender_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               htmlOutput(ns("plot_einstieg_pie_gender")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2021, auf Anfrage, eigene Berechnungen."))
                    ),

                    tabPanel("Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_verlauf_gender_ui("mod_studium_studienzahl_einstieg_verlauf_gender_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_einstieg_verlauf_gender")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2021, auf Anfrage, eigene Berechnungen."))

                  ),

                  tabPanel("Überblick", br(),

                           shiny::sidebarPanel(
                             width = 3,
                             tags$style(".well  {background-color:#FFFFFF;}"),
                             tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                             mod_studium_studienzahl_einstieg_comparison_gender_ui("mod_studium_studienzahl_einstieg_comparison_gender_ui_1")),
                           shiny::mainPanel(
                             width = 9,
                             highcharter::highchartOutput(ns("plot_einstieg_comparison_gender")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2021, auf Anfrage, eigene Berechnungen."))
                  )
        ))),
    fluidRow(
      shinydashboard::box(
        title = "#Fächerwahl Frauen: Wie unterscheidet sich die Fächerwahl von Frauen und Männern?",
        width = 12,
        p("Hier zeigen wir die Unterschiede zwischen Männern und Frauen aus einer anderen Perspektive. Welchen Anteil machen MINT-Fächer bei der Studienfachwahl von Frauen und Männern in Deutschland aus?"),
       br(),
        p("Interpretationshilfe: Weibliche Studierende wählen 2020 zu 24 % ein MINT-Studienfach. Bei den männlichen Studierenden wählen 51 % MINT."),

        tabsetPanel(type = "tabs",

                    tabPanel("Vergleich", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_choice_gender_ui("mod_studium_studienzahl_choice_gender_ui")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               plotOutput(ns("plot_waffle_choice_gender")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis 2021, auf Anfrage, eigene Berechnungen.")
                             )
                    ),

                    tabPanel("Zeitverlauf", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_verlauf_bl_subject_gender_ui("mod_studium_studienzahl_verlauf_bl_subject_gender_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_verlauf_studienzahl_bl_subject_gender")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis 2021, auf Anfrage, eigene Berechnungen.")
                             )
                    ),
                    tabPanel("Überblick", br(),

                               tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                               shiny::sidebarPanel(
                                 width = 3,
                                 mod_studium_studienzahl_ranking_bl_subject_gender_ui("mod_studium_studienzahl_ranking_bl_subject_gender_ui_1")
                               ),
                               shiny::mainPanel(
                                 width = 9,
                                 #highcharter::highchartOutput(ns("plot_ranking_studienzahl_bl_subject_gender1")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis 2021, auf Anfrage, eigene Berechnungen.")
                                 plotOutput(ns("plot_ranking_studienzahl_bl_subject_gender")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis 2021, auf Anfrage, eigene Berechnungen.")
                               )
                    )
        ))),
    fluidRow(shinydashboard::box(
      title = "#XXX?",
      width = 12,
      p("XXX?"),
      br(),
      p("Interpretationshilfe: XXX."),

      tabsetPanel(
        type = "tabs",
        tags$head(
          tags$style(
            ".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}"
          )
        ),
        shiny::sidebarPanel(
          width = 3,
          mod_studium_top_faecher_ui("mod_studium_top_faecher")
        ),
        shiny::mainPanel(
          width = 9,
          htmlOutput(ns("plot_top_faecher")),
          p(
            style = "font-size:12px;color:grey",
            "Quelle der Daten: Destatis 2021, auf Anfrage, eigene Berechnungen."
          )
        )
      )
    )),



    #Footer
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
    # fluidRow(
    #   shinydashboard::box(
    #     title = "Nicht zuordbar",
    #     width = 12,
    #     p("Hier finden Sie den Anteil an Belegungen von Frauen und Männern in MINT-Fächern für die Bundesländer im Vergleich. "),
    #     tabsetPanel(type = "tabs",
    #                 tabPanel("Karte", br(),
    #
    #                          shiny::sidebarPanel(
    #                            mod_studium_studienzahl_bl_map_gender_ui("mod_studium_studienzahl_bl_map_gender")
    #                          ),
    #                          shiny::mainPanel(
    #                            htmlOutput(ns("plot_studienzahl_map_gender"))
    #                          )
    #                 ),
    #                 tabPanel("Vergleich (Bundesländer)", br(),
    #
    #                          shiny::sidebarPanel(
    #                            mod_studium_studienzahl_bl_verlauf_gender_ui("mod_studium_studienzahl_bl_verlauf_gender")
    #                          ),
    #                          shiny::mainPanel(
    #                            highcharter::highchartOutput(ns("plot_studienzahl_bl_verlauf_gender"))
    #                          )
    #                 ),
    #
    #                 tabPanel("Überblick (Bundsländer)", br(),
    #
    #                          shiny::sidebarPanel(
    #                            mod_studium_studienzahl_bl_vergleich_gender_ui("mod_studium_studienzahl_bl_vergleich_gender_ui")
    #                          ),
    #                          shiny::mainPanel(
    #                            plotOutput(ns("plot_ranking_studienzahl_bl_vergleich_gender"))
    #                          )
    #                 ),
    #
    #
    #
    #                 # zeigt Anteile MINT und nicht Gender
    #
    #                 tabPanel("Überblick (doppelt)", br(),
    #
    #                          shiny::sidebarPanel(
    #                            tags$style(".well {background-color:#FFFFFF;}"),
    #                            tags$head(tags$style(HTML(".small-box {height: 140px}"))),
    #                            mod_studium_studienzahl_einstieg_comparison_gender_ui("mod_studium_studienzahl_einstieg_comparison_gender_ui_1")),
    #                          shiny::mainPanel(
    #                            plotOutput(ns("plot_einstieg_comparison_gender")))
    #
    #                 ),
    #
    #
    #     ))),

    # fluidRow(
    #   shinydashboard::box(
    #     width = 12,
    # p(style = "text-align: justify; font-size = 16px",
    #   span(tags$b(span("Quelle der Daten:", style = "color:#b16fab")), "Hochschul-Statistiken des Statistischen Bundesamtes, 2021: auf Anfrage.")),
    # p(style = "text-align: justify; font-size = 16px",
    #   span(tags$b(span("Methodische Hinweise: ", style = "color:#b16fab")),
    #        " "))

}

#' studium_studienzahl Server Functions
#'
#' @noRd
mod_studium_studienzahl_server <- function(id, data_studierende, data_studierende2, data_studierende_faecher, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    # Box 2
    output$plot_einstieg_pie <- renderUI({
      studienzahl_einstieg_pie(data_studierende,r)
    })

    output$plot_einstieg_verlauf <- highcharter::renderHighchart({
      studienzahl_verlauf_single(data_studierende2,r)
    })

    # all_mint_23_react <- reactive({
    #   studienzahl_all_mint_23(data_studierende2, r)
    # })



    output$plot_einstieg_comparison <- highcharter::renderHighchart({
      studienzahl_einstieg_comparison(data_studierende2,r)
    })

    data_table_einstieg_react <- reactive({
      data_einstieg(data_studierende, r)
    })

    # output$data_table_einstieg <- DT::renderDT({
    #   data_table_einstieg_react()
    # })

    # Box 3
    output$plot_einstieg_pie_gender <- highcharter::renderHighchart({
      studienzahl_einstieg_pie_gender(data_studierende,r)
    })

    output$plot_einstieg_verlauf_gender <- highcharter::renderHighchart({
      studienzahl_verlauf_single_gender(data_studierende,r)
    })

    output$plot_einstieg_comparison_gender <- highcharter::renderHighchart({
      studienzahl_einstieg_comparison_gender(data_studierende,r)
    })

    output$plot_verlauf_studienzahl_bl_subject1 <- highcharter::renderHighchart({
      ranking_bl_subject(data_studierende,r)
    })


    output$plot_verlauf_studienzahl_bl1 <- highcharter::renderHighchart({
      ranking_bl_subject(data_studierende,r)
    })

    output$test <- renderUI({
      studienzahl_test(data_studierende2, r)
    })

    # Box 4
    plot_waffle_react <- reactive({
      studienzahl_waffle_mint(data_studierende2,r)
    })

    output$plot_waffle <- renderPlot({
      plot_waffle_react()
    })

    output$plot_verlauf_studienzahl_bl_subject <- highcharter::renderHighchart({
      studienzahl_verlauf_bl_subject(data_studierende2,r)
    })

    output$plot_ranking_bl_subject <- renderPlot({
      ranking_bl_subject(data_studierende,r)
    })

    # Box 5
    plot_waffle_choice_gender_react <- reactive({
      studienzahl_waffle_choice_gender(data_studierende,r)
    })

    output$plot_waffle_choice_gender <- renderPlot({
      plot_waffle_choice_gender_react()
    })

    output$plot_verlauf_studienzahl_bl_subject_gender <- highcharter::renderHighchart({
      studierende_verlauf_single_bl_gender(data_studierende,r)
    })

    plot_ranking_studienzahl_bl_subject_gender_react <- reactive({
      studienfaecher_ranking(data_studierende, r, type="other")
    })

    output$plot_ranking_studienzahl_bl_subject_gender <- renderPlot({
      plot_ranking_studienzahl_bl_subject_gender_react()
    })

    output$plot_ranking_studienzahl_bl_subject_gender1 <- highcharter::renderHighchart({
      plot_ranking_studienzahl_bl_subject_gender_react(data_studierende,r)
    })



    # Box 6
    output$plot_studienzahl_map <- renderUI({
      studierende_map(studierende_faecher_alle_indi,r)
    })

    output$plot_studienzahl_bl_verlauf <- highcharter::renderHighchart({
      studierende_verlauf_multiple_bl(data_studierende,r)
    })


     output$plot_vergleich_bl <-  renderPlot({
      studierende_mint_vergleich_bl(data_studierende,r)
    })

     output$plot_vergleich_bl1 <- highcharter::renderHighchart({
       studierende_mint_vergleich_bl(data_studierende,r)
     })


    # Box 7
    output$plot_studienzahl_map_gender <- renderUI({
      studierende_map_gender(data_studierende,r)
    })

    output$plot_studienzahl_bl_verlauf_gender <- highcharter::renderHighchart({
      studierende_verlauf_multiple_bl_gender(data_studierende,r)
    })

    plot_ranking_studienzahl_bl_vergleich_gender_react <- reactive({
      bundeslaender_ranking(data_studierende, r, type="other")
    })

    output$plot_ranking_studienzahl_bl_vergleich_gender <- renderPlot({
      plot_ranking_studienzahl_bl_vergleich_gender_react()
    })

    # Box 8
    output$plot_top_faecher <-  renderUI({
      plot_ranking_top_faecher(data_studierende_faecher, r)
    })


    # downloader
    output$download_data_box1 <- shiny::downloadHandler(
      filename = function() {
        paste("data_studium", "csv", sep = ".")
      },
      content = function(file){
        write.csv(data_table_einstieg_react(), file)
      }
    )

  })
}

## To be copied in the UI
# mod_studium_studienzahl_ui("studium_studienzahl_1")

## To be copied in the server
# mod_studium_studienzahl_server("studium_studienzahl_1")
