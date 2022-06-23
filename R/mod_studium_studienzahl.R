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
    fluidRow(
    shinydashboard::box(
      tags$h1("Studium und MINT"),
      width = 12,
      p(style = "text-align: justify; font-size = 16px",
        "Auf dieser Seite finden Sie statistische Kennzahlen rund um MINT im Bereich Hochschule. Sie finden
        Informationen zum Anteil von MINT-Fächern und nicht-MINT-Fächern an allen Studienfächern im Zeitraum
        2013 bis 2020. Zudem können Sie sich den Frauen- und Männeranteil in einzelnen MINT-Fächern für
        Studierende und Studienanfänger*innen ausgeben lassen. Des weiteren haben wir alle Ergebnisse
        auch für einzelne Bundesländer aufbereitet. ")
    )),
    fluidRow(
      shinydashboard::box(
        title = "Anteil von MINT-Studienfächern",
        width = 12,
        p("Hier können Sie sich den Anteil von MINT-Studienfächern anschauen."),
        tabsetPanel(type = "tabs",
                    tabPanel(tab1_name[1], br(),

                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_ui("mod_studium_studienzahl_einstieg_ui_1")),
                             shiny::mainPanel(
                               htmlOutput(ns("plot_einstieg_pie")))
                    ),
                    tabPanel("Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_verlauf_ui("mod_studium_studienzahl_einstieg_verlauf_ui_1")),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_einstieg_verlauf")))


                    ),
                    tabPanel("Überblick", br(),

                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_comparison_ui("mod_studium_studienzahl_einstieg_comparison_ui_1")),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_einstieg_comparison")))




                  ),
                    tabPanel("Datensatz", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                               .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_ui("mod_schule_kurse_einstieg_ui_1")),
                             shiny::mainPanel(
                               div(DT::dataTableOutput(ns("data_table_einstieg")),
                                   style = "font-size: 75%; width: 75%"),
                               shiny::downloadButton(ns("download_data_box1"), label = "",
                                                     class = "butt",
                                                     icon = shiny::icon("download")))
                    )
        ))),
    fluidRow(
      shinydashboard::box(
        title = "Anteil von Frauen an MINT-Fächern und nicht-MINT-Fächern",
        width = 12,
        p("Hier können Sie sich anschauen, wie hoch der Frauen- und Männeranteil unter
        Studierenden und
          Studienanfänger*innen in MINT- und nicht-MINT-Fächern ist."),
        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich", br(),

                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_gender_ui("mod_studium_studienzahl_einstieg_gender_ui_1")),
                             shiny::mainPanel(
                               htmlOutput(ns("plot_einstieg_pie_gender")))
                    ),
                    tabPanel("Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_verlauf_gender_ui("mod_studium_studienzahl_einstieg_verlauf_gender_ui_1")),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_einstieg_verlauf_gender")))

                  ),
                  tabPanel("Überblick", br(),

                           shiny::sidebarPanel(
                             tags$style(".well {background-color:#FFFFFF;}"),
                             tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                             mod_studium_studienzahl_einstieg_comparison_gender_ui("mod_studium_studienzahl_einstieg_comparison_gender_ui_1")),
                           shiny::mainPanel(
                             plotOutput(ns("plot_einstieg_comparison_gender")))

                )
        ))),
    fluidRow(
      shinydashboard::box(
        title = "Einzelne MINT-Fächer im Vergleich",
        width = 12,
        p("Hier finden Sie Analysen für einzelne Studienfächer im Vergleich."),
        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               mod_studium_studienzahl_choice_1_ui("mod_studium_studienzahl_choice_ui_1_1")),
                             shiny::mainPanel(
                               plotOutput(ns("plot_waffle")))
                    ),
                    tabPanel("Zeitverlauf", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               mod_studium_studienzahl_verlauf_bl_subject_ui("mod_studium_studienzahl_verlauf_bl_subject_ui_1")),
                             shiny::mainPanel(

                               highcharter::highchartOutput(ns("plot_verlauf_studienzahl_bl_subject"))

                             )
                    ),
                    tabPanel("Überblick", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               mod_studium_studienzahl_ranking_bl_subject_ui("mod_studium_studienzahl_ranking_bl_subject_ui_1")),
                             shiny::mainPanel(
                               plotOutput(ns("plot_ranking_bl_subject")),
                             )
                    ),
        ))),
    fluidRow(
      shinydashboard::box(
        title = "Anteil von Frauen in den einzelnen MINT-Fächern",
        width = 12,
        p("Hier finden Sie den Frauen- und Männeranteil in einzelnen Studienfächern im Vergleich."),
        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               mod_studium_choice_gender_ui("mod_studium_studienzahl_choice_gender_ui")
                               ),
                             shiny::mainPanel(
                               plotOutput(ns("plot_waffle_choice_gender"))
                               )
                    ),
                    tabPanel("Zeitverlauf", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               mod_studium_studienzahl_verlauf_bl_subject_gender_ui("mod_studium_studienzahl_verlauf_bl_subject_gender_ui_1")
                               ),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_verlauf_studienzahl_bl_subject_gender"))
                             )
                    ),
                    tabPanel("Überblick", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               mod_studium_studienzahl_ranking_bl_subject_gender_ui("mod_studium_studienzahl_ranking_bl_subject_gender_ui_1")
                               ),
                             shiny::mainPanel(
                               plotOutput(ns("plot_ranking_studienzahl_bl_subject_gender")),
                             )
                    ),
        ))),
    fluidRow(
      shinydashboard::box(
        title = "Bundesländer im Vergleich",
        width = 12,
        p("Hier finden Sie Analysen für einzelne Bundesländer."),
        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich", br(),

                             shiny::sidebarPanel(
                               mod_studium_studienzahl_bl_map_ui("mod_studium_studienzahl_bl_map")
                               ),
                             shiny::mainPanel(
                               htmlOutput(ns("plot_studienzahl_map"))
                               )
                    ),
                    tabPanel("Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               mod_studium_studienzahl_bl_verlauf_ui("mod_studium_studienzahl_bl_verlauf")
                               ),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_studienzahl_bl_verlauf"))
                               )
                    ),
                    tabPanel("Überblick", br(),

                             shiny::sidebarPanel(
                               mod_studium_studienzahl_bl_vergleich_ui("studium_studienzahl_bl_vergleich")
                               ),
                             shiny::mainPanel(
                               plotOutput(ns("plot_vergleich_bl"))
                               )
                    )
        ))),
    fluidRow(
      shinydashboard::box(
        title = "Anteil von Studentinnen in den Bundesländern",
        width = 12,
        p("Hier finden Sie den Anteil an Belegunden von Frauen und Männern in MINT-Fächern für die Bundesländer im Vergleich. "),
        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich", br(),

                             shiny::sidebarPanel(
                               mod_studium_studienzahl_bl_map_gender_ui("mod_studium_studienzahl_bl_map_gender")
                             ),
                             shiny::mainPanel(
                               htmlOutput(ns("plot_studienzahl_map_gender"))
                             )
                    ),
                    tabPanel("Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               mod_studium_studienzahl_bl_verlauf_gender_ui("mod_studium_studienzahl_bl_verlauf_gender")
                             ),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_studienzahl_bl_verlauf_gender"))
                             )
                    ),
                    tabPanel("Überblick", br(),

                             shiny::sidebarPanel(
                               mod_studium_studienzahl_bl_vergleich_gender_ui("mod_studium_studienzahl_bl_vergleich_gender_ui")
                             ),
                             shiny::mainPanel(
                               plotOutput(ns("plot_ranking_studienzahl_bl_vergleich_gender"))
                             )
                    )
        ))),
    fluidRow(
      shinydashboard::box(
        width = 12,
    p(style = "text-align: justify; font-size = 16px",
      span(tags$b(span("Quelle der Daten:", style = "color:#b16fab")), "Hochschul-Statistiken des Statistischen Bundesamtes, 2021: auf Anfrage.")),
    p(style = "text-align: justify; font-size = 16px",
      span(tags$b(span("Methodische Hinweise: ", style = "color:#b16fab")),
           " "))
      )
    )
  )
}

#' studium_studienzahl Server Functions
#'
#' @noRd
mod_studium_studienzahl_server <- function(id, data_studierende, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Box 2
    output$plot_einstieg_pie <- renderUI({
      studienzahl_einstieg_pie(data_studierende,r)
    })

    output$plot_einstieg_verlauf <- highcharter::renderHighchart({
      studienzahl_verlauf_single(data_studierende,r)
    })

    output$plot_einstieg_comparison <- highcharter::renderHighchart({
      studienzahl_einstieg_comparison(data_studierende,r)
    })

    data_table_einstieg_react <- reactive({
      data_einstieg(data_studierende, r)
    })

    output$data_table_einstieg <- DT::renderDT({
      data_table_einstieg_react()
    })

    # Box 3
    output$plot_einstieg_pie_gender <- renderUI({
      studienzahl_einstieg_pie_gender(data_studierende,r)
    })

    output$plot_einstieg_verlauf_gender <- highcharter::renderHighchart({
      studienzahl_verlauf_single_gender(data_studierende,r)
    })

    output$plot_einstieg_comparison_gender <- renderPlot({
      studienzahl_einstieg_comparison_gender(data_studierende,r)
    })

    # Box 4
    plot_waffle_react <- reactive({
      studienzahl_waffle_mint(data_studierende,r)
    })

    output$plot_waffle <- renderPlot({
      plot_waffle_react()
    })

    output$plot_verlauf_studienzahl_bl_subject <- highcharter::renderHighchart({
      studienzahl_verlauf_bl_subject(data_studierende,r)
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

    # Box 6
    output$plot_studienzahl_map <- renderUI({
      studierende_map(data_studierende,r)
    })

    output$plot_studienzahl_bl_verlauf <- highcharter::renderHighchart({
      studierende_verlauf_multiple_bl(data_studierende,r)
    })

    output$plot_vergleich_bl <- renderPlot({
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
