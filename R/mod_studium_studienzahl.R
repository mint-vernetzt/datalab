#' studium_studienzahl UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
    shinydashboard::box(
      title = "Box 1",
      width = 12,
      p(style = "text-align: justify; font-size = 16px",
        "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor
        invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua"),
      br(),
      p(style = "text-align: justify; font-size = 16px",
        span("17%", style = "color:#b16fab; font-size: 50px"),
        "Lorem ipsum dolor sit amet, consetetur sadipscing elitr,
        sed diam nonumy eirmod tempor invidunt ut labore et dolore magna
        aliquyam erat, sed diam voluptua."),
      br(),
      p(style = "text-align: justify; font-size = 16px",
        span("38%", style = "color:#f5adac; font-size: 50px"),
        "Lorem ipsum dolor sit amet, consetetur sadipscing elitr,
        sed diam nonumy eirmod tempor invidunt ut labore et dolore magna
        aliquyam erat, sed diam voluptua.")
    )),
    fluidRow(
      shinydashboard::box(
        title = "Box 2",
        width = 12,
        p("Lorem ipsum dolor sit amet"),
        tabsetPanel(type = "tabs",
                    tabPanel("MINT-Anteile", br(),

                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_ui("mod_studium_studienzahl_einstieg_ui_1")),
                             shiny::mainPanel(
                               htmlOutput(ns("plot_einstieg_pie")))
                    ),
                    tabPanel("Jahresvergleich", br(),

                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_verlauf_ui("mod_studium_studienzahl_einstieg_verlauf_ui_1")),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_einstieg_verlauf")))


                    ),
                    tabPanel("Vergleich", br(),

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
        title = "Box 3",
        width = 12,
        p("Lorem ipsum dolor sit amet"),
        tabsetPanel(type = "tabs",
                    tabPanel("MINT-Anteile", br(),

                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_gender_ui("mod_studium_studienzahl_einstieg_gender_ui_1")),
                             shiny::mainPanel(
                               htmlOutput(ns("plot_einstieg_pie_gender")))
                    ),
                    tabPanel("Jahresvergleich", br(),

                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_verlauf_gender_ui("mod_studium_studienzahl_einstieg_verlauf_gender_ui_1")),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_einstieg_verlauf_gender")))

                  ),
                  tabPanel("Vergleich", br(),

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
        title = "Box 4",
        width = 12,
        p("Lorem ipsum dolor sit amet"),
        tabsetPanel(type = "tabs",
                    tabPanel("Fächervergleich", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               mod_studium_studienzahl_choice_1_ui("mod_studium_studienzahl_choice_ui_1_1")),
                             shiny::mainPanel(
                               plotOutput(ns("plot_waffle")))
                    ),
                    tabPanel("Jahresvergleich", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               mod_studium_studienzahl_verlauf_bl_subject_ui("mod_studium_studienzahl_verlauf_bl_subject_ui_1")),
                             shiny::mainPanel(

                               highcharter::highchartOutput(ns("plot_verlauf_studienzahl_bl_subject"))

                             )
                    ),
                    tabPanel("Vergleich", br(),

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
        title = "Box 5",
        width = 12,
        p("Fächervergleich Geschlechter"),
        tabsetPanel(type = "tabs",
                    tabPanel("Fächervergleich", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               mod_studium_choice_gender_ui("mod_studium_studienzahl_choice_gender_ui")
                               ),
                             shiny::mainPanel(
                               plotOutput(ns("plot_waffle_choice_gender"))
                               )
                    ),
                    tabPanel("Jahresvergleich", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               mod_studium_studienzahl_verlauf_bl_subject_gender_ui("mod_studium_studienzahl_verlauf_bl_subject_gender_ui_1")
                               ),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_verlauf_studienzahl_bl_subject_gender"))
                             )
                    ),
                    tabPanel("Vergleich", br(),

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
        title = "Box 6",
        width = 12,
        p("Karte"),
        tabsetPanel(type = "tabs",
                    tabPanel("Fächerbelegung", br(),

                             shiny::sidebarPanel(
                               mod_studium_studienzahl_bl_map_ui("mod_studium_studienzahl_bl_map")
                               ),
                             shiny::mainPanel(
                               htmlOutput(ns("plot_studienzahl_map"))
                               )
                    ),
                    tabPanel("Jahresvergleich", br(),

                             shiny::sidebarPanel(
                               mod_studium_studienzahl_bl_verlauf_ui("mod_studium_studienzahl_bl_verlauf")
                               ),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_studienzahl_bl_verlauf"))
                               )
                    ),
                    tabPanel("Vergleich", br(),

                             shiny::sidebarPanel(
                               # mod_schule_kurse_comparison_bl_ui("mod_schule_kurse_comparison_bl_ui_1")
                               ),
                             shiny::mainPanel(
                               # plotOutput(ns("plot_comparison_bl"))
                               )
                    )
        ))),
    fluidRow(
      shinydashboard::box(
        title = "Box 7",
        width = 12,
        p("Karte"),
        tabsetPanel(type = "tabs",
                    tabPanel("Fächerbelegung", br(),

                             shiny::sidebarPanel(
                               mod_studium_studienzahl_bl_map_gender_ui("mod_studium_studienzahl_bl_map_gender")
                             ),
                             shiny::mainPanel(
                               htmlOutput(ns("plot_studienzahl_map_gender"))
                             )
                    ),
                    tabPanel("Jahresvergleich", br(),

                             shiny::sidebarPanel(
                               mod_studium_studienzahl_bl_verlauf_gender_ui("mod_studium_studienzahl_bl_verlauf_gender")
                             ),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_studienzahl_bl_verlauf_gender"))
                             )
                    ),
                    tabPanel("Vergleich", br(),

                             shiny::sidebarPanel(
                               # mod_schule_kurse_comparison_bl_ui("mod_schule_kurse_comparison_bl_ui_1")
                             ),
                             shiny::mainPanel(
                               # plotOutput(ns("plot_comparison_bl"))
                             )
                    )
        ))),
  )
}

#' studium_studienzahl Server Functions
#'
#' @noRd
mod_studium_studienzahl_server <- function(id, data_studierende, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    output$plot_einstieg_pie <- renderUI({
      studienzahl_einstieg_pie(data_studierende,r)

    })

    output$plot_einstieg_pie_gender <- renderUI({
      studienzahl_einstieg_pie_gender(data_studierende,r)

    })

    data_table_einstieg_react <- reactive({
      data_einstieg(data_studierende, r)
    })

    output$data_table_einstieg <- DT::renderDT({
      data_table_einstieg_react()
    })

    plot_absolut_react <- reactive({
      studienzahl_absolut(data_studierende,r)
    })

    output$plot_absolut <- renderPlot({
      plot_absolut_react()
    })

    plot_waffle_react <- reactive({
      studienzahl_waffle_mint(data_studierende,r)
    })

    output$plot_waffle <- renderPlot({
      plot_waffle_react()
    })

    output$plot_ranking_bl_subject <- renderPlot({
      ranking_bl_subject(data_studierende,r)
    })

    plot_waffle_choice_gender_react <- reactive({
      studienzahl_waffle_choice_gender(data_studierende,r)
    })

    output$plot_waffle_choice_gender <- renderPlot({
      plot_waffle_choice_gender_react()
    })



    output$plot_map_studienzahl <-renderUI({
      studienzahl_map(data_studierende,r)
    })

    output$plot_einstieg_verlauf <- highcharter::renderHighchart({
      studienzahl_verlauf_single(data_studierende,r)
    })

    output$plot_einstieg_verlauf_gender <- highcharter::renderHighchart({
      studienzahl_verlauf_single_gender(data_studierende,r)
    })

    output$plot_einstieg_comparison <- highcharter::renderHighchart({
      studienzahl_einstieg_comparison(data_studierende,r)
    })

    output$plot_einstieg_comparison_gender <- renderPlot({
      studienzahl_einstieg_comparison_gender(data_studierende,r)
    })



    output$plot_verlauf_studienzahl <- highcharter::renderHighchart({
      studienzahl_verlauf(data_studierende,r)
    })

    output$plot_verlauf_studienzahl_bl <- highcharter::renderHighchart({
      studienzahl_verlauf_bl(data_studierende,r)
    })

    output$plot_verlauf_studienzahl_bl_subject <- highcharter::renderHighchart({
      studienzahl_verlauf_bl_subject(data_studierende,r)
    })

    output$plot_verlauf_studienzahl_bl_subject_gender <- highcharter::renderHighchart({
      studierende_verlauf_single_bl_gender(data_studierende,r)
    })


    data_table_mix_react <- reactive({
      data_mix_studium(data_studierende, r)
    })

    output$data_table_mix <- DT::renderDT({
      data_table_mix_react()
    })

   data_table_verlauf_react <- DT::renderDT({
      data_verlauf_studium(data_studierende, r)
    })

    output$data_table_verlauf <- DT::renderDT({
      data_table_verlauf_react()
    })

    plot_ranking_studienzahl_bl_subject_gender_react <- reactive({
      studienfaecher_ranking(data_studierende, r, type="other")
    })

    output$plot_ranking_studienzahl_bl_subject_gender <- renderPlot({
      plot_ranking_studienzahl_bl_subject_gender_react()
    })

    output$plot_studienzahl_map <- renderUI({
      studierende_map(data_studierende,r)
    })

    output$plot_studienzahl_bl_verlauf <- highcharter::renderHighchart({
      studierende_verlauf_multiple_bl(data_studierende,r)
    })

    output$plot_studienzahl_map_gender <- renderUI({
      studierende_map_gender(data_studierende,r)
    })

    output$plot_studienzahl_bl_verlauf_gender <- highcharter::renderHighchart({
      studierende_verlauf_multiple_bl_gender(data_studierende,r)
    })

    # save histogram using downloadHandler and plot output type
    output$download_waffle <- shiny::downloadHandler(
      filename = function() {
        paste("plot_studium", "png", sep = ".")
      },
      content = function(file){
        ggplot2::ggsave(file, plot = plot_waffle_react(), device = "png",
                        dpi = 300, width = 10, height = 6)
      }
    )

    # save histogram using downloadHandler and plot output type
    output$download_absolut <- shiny::downloadHandler(
      filename = function() {
        paste("plot_studium", "png", sep = ".")
      },
      content = function(file){
        ggplot2::ggsave(file, plot = plot_absolut_react(), device = "png",
                        dpi = 300, width = 10, height = 6)
      }
    )


    output$download_data_box1 <- shiny::downloadHandler(
      filename = function() {
        paste("data_studium", "csv", sep = ".")
      },
      content = function(file){
        write.csv(data_table_einstieg_react(), file)
      }
    )


    # save histogram using downloadHandler and plot output type
    output$data_table_mix_box3 <- shiny::downloadHandler(
      filename = function() {
        paste("data_studium", "csv", sep = ".")
      },
      content = function(file){
        write.csv(data_table_mix_react(), file)
      }
    )

    output$download_data_box5 <- shiny::downloadHandler(
      filename = function() {
        paste("data_studium", "csv", sep = ".")
      },
      content = function(file){
        write.csv(data_table_verlauf_react(), file)
      }
    )
  })
}

## To be copied in the UI
# mod_studium_studienzahl_ui("studium_studienzahl_1")

## To be copied in the server
# mod_studium_studienzahl_server("studium_studienzahl_1")
