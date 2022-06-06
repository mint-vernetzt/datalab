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

                      tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                      shiny::sidebarPanel(
                        tags$style(".well {background-color:#FFFFFF;}"),
                        tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                        mod_schule_kurse_einstieg_ui("mod_schule_kurse_einstieg_ui_1")),
                      shiny::mainPanel(
                        htmlOutput(ns("plot_einstieg_pie")))
                            ),
                    tabPanel("Zeitverlauf", br(),
                             shiny::sidebarPanel(
                               mod_schule_kurse_einstieg_verlauf_ui("mod_schule_kurse_einstieg_verlauf_ui_1")),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_einstieg_verlauf")))
                             ),
                    tabPanel("Vergleich", br(),
                             shiny::sidebarPanel(
                               mod_schule_kurse_einstieg_comparison_ui("mod_schule_kurse_einstieg_comparison_ui_1")),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_einstieg_comparison")))
                    ),
                    tabPanel("Datensatz", br(),

                      tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                               .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                      shiny::sidebarPanel(
                        tags$style(".well {background-color:#FFFFFF;}"),
                        tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                        mod_schule_kurse_einstieg_ui("mod_schule_kurse_einstieg_ui_1")),
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
                    tabPanel("Frauenanteile", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_schule_kurse_pie_gender_ui("mod_schule_kurse_pie_gender_ui_1")),
                             shiny::mainPanel(
                               htmlOutput(ns("plot_pie_gender")))
                    ),
                    tabPanel("Zeitverlauf", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_schule_kurse_verlauf_gender_ui("mod_schule_kurse_verlauf_gender_ui_1")),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_verlauf_gender")))
                    ),
                    tabPanel("Vergleich", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_schule_kurse_comparison_gender_ui("mod_schule_kurse_comparison_gender_ui_1")),
                             shiny::mainPanel(
                               plotOutput(ns("plot_comparison_gender")))
                    )

        ))),
    fluidRow(
      shinydashboard::box(
        title = "Box 4",
        width = 12,
        p("Lorem ipsum dolor sit amet"),
        tabsetPanel(type = "tabs",
                    tabPanel("Frauenanteile", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                                 shiny::sidebarPanel(
                                   mod_schule_kurse_multiple_mint_ui("mod_schule_kurse_multiple_mint_ui_1")),
                             shiny::mainPanel(
                               plotOutput(ns("plot_waffle_mint")))
                    ),
                    tabPanel("Jahresvergleich", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                                 shiny::sidebarPanel(
                                   mod_schule_kurse_verlauf_bl_subjects_ui("mod_schule_kurse_verlauf_bl_subjects_ui_1")),
                                 shiny::mainPanel(
                                   highcharter::highchartOutput(ns("plot_verlauf_kurse_bl_subjects"))

                                 )
                    ),
                    tabPanel("Vergleich", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               mod_schule_kurse_comparison_subjects_ui("mod_schule_kurse_comparison_subjects_ui_1")),
                             shiny::mainPanel(
                               plotOutput(ns("plot_comparison_subjects"))

                             )
                    )
        ))),
    fluidRow(
      shinydashboard::box(
        title = "Box 5",
        width = 12,
        p("Lorem ipsum dolor sit amet"),
        tabsetPanel(type = "tabs",
                    tabPanel("Fächerbelegung", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               mod_schule_kurse_multiple_ui("mod_schule_kurse_multiple_ui_1")),
                             shiny::mainPanel(
                               plotOutput(ns("plot_waffle")))
                    ),
                    tabPanel("Jahresvergleich", br(),

                             shiny::sidebarPanel(
                               mod_schule_kurse_verlauf_bl_ui("mod_schule_kurse_verlauf_bl_ui_1")),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_verlauf_kurse_bl")))
                    ),
                    tabPanel("Vergleich", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               mod_schule_kurse_ranking_ui("mod_schule_kurse_ranking_ui_1")),
                             shiny::mainPanel(
                               plotOutput(ns("plot_ranking_2")),
                               shiny::downloadButton(ns("download_ranking"), label = "",
                                                     class = "butt",
                                                     icon = shiny::icon("download"))
                             )
                  )
        ))),
    fluidRow(
      shinydashboard::box(
        title = "Box 6",
        width = 12,
        p("Lorem ipsum dolor sit amet"),
        tabsetPanel(type = "tabs",
                    tabPanel("Fächerbelegung", br(),

                             shiny::sidebarPanel(
                               mod_schule_kurse_map_ui("mod_schule_kurse_map_ui_1")),
                             shiny::mainPanel(
                               htmlOutput(ns("plot_map_kurse")))
                    ),
                    tabPanel("Jahresvergleich", br(),

                             shiny::sidebarPanel(
                               mod_schule_kurse_verlauf_multiple_ui("mod_schule_kurse_verlauf_multiple_ui_1")),
                             shiny::mainPanel(
                               highcharter::highchartOutput(ns("plot_verlauf_multiple")))
                  ),
                  tabPanel("Vergleich", br(),

                           shiny::sidebarPanel(
                             mod_schule_kurse_comparison_bl_ui("mod_schule_kurse_comparison_bl_ui_1")),
                           shiny::mainPanel(
                             plotOutput(ns("plot_comparison_bl")))
                 )
        ))),
    fluidRow(
      shinydashboard::box(
        title = "Box 7",
        width = 12,
        p("Lorem ipsum dolor sit amet"),
        tabsetPanel(type = "tabs",
                    tabPanel("Fächerbelegung", br(),

                             shiny::sidebarPanel(
                               mod_schule_kurse_map_gender_ui("mod_schule_kurse_map_gender_ui_1")),
                             shiny::mainPanel(
                               htmlOutput(ns("plot_map_kurse_gender")))
                    ),
                    tabPanel("Jahresvergleich", br(),

                             shiny::sidebarPanel(
                               mod_schule_kurse_verlauf_ui("mod_schule_kurse_verlauf_ui_1")),
                             shiny::mainPanel(

                                    highcharter::highchartOutput(ns("plot_verlauf_kurse"))

                             )
                   ),
                   tabPanel("Vergleich", br(),

                            tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                            shiny::sidebarPanel(
                              mod_schule_kurse_ranking_gender_ui("mod_schule_kurse_ranking_gender_ui_1")),
                            shiny::mainPanel(
                              plotOutput(ns("plot_ranking_gender")),
                              # shiny::downloadButton(ns("download_ranking_gender"), label = "",
                              #                       class = "butt",
                              #                       icon = shiny::icon("download"))
                            )

                  )
        )))
  )
}

#' schule_kurse Server Functions
#'
#' @noRd
mod_schule_kurse_server <- function(id, data_kurse, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$plot_einstieg_pie <- renderUI({
      kurse_einstieg_pie(data_kurse,r)
    })

    data_table_einstieg_react <- reactive({
      data_einstieg_kurse(data_kurse, r)
    })

    output$data_table_einstieg <- DT::renderDT({
      data_table_einstieg_react()
    })

    plot_waffle_react <- reactive({
      kurse_waffle(data_kurse,r)
    })

    output$plot_waffle <- renderPlot({
      plot_waffle_react()
    })

    plot_absolut_react <- reactive({
      kurse_absolut(data_kurse,r)
    })

    output$plot_absolut <- renderPlot({
      plot_absolut_react()
    })

    plot_ranking_react <- reactive({
      kurse_ranking(data_kurse,r, type="other")
    })

    output$plot_ranking_2 <- renderPlot({
      plot_ranking_react()
    })

    output$plot_map_kurse <- renderUI({
      kurse_map(data_kurse,r)
    })

    output$plot_map_kurse_gender <- renderUI({
      kurse_map_gender(data_kurse,r)
    })

    output$plot_verlauf_kurse <- highcharter::renderHighchart({
      kurse_verlauf(data_kurse,r)
    })

    output$plot_verlauf_multiple <- highcharter::renderHighchart({
      kurse_verlauf_multiple_bl(data_kurse,r)
    })

    output$plot_verlauf_kurse_bl <- highcharter::renderHighchart({
      kurse_verlauf_single_bl(data_kurse,r)
    })

    output$plot_verlauf_kurse_bl_subjects <- highcharter::renderHighchart({
      kurse_verlauf_subjects_bl(data_kurse,r)
    })

    output$plot_einstieg_verlauf <- highcharter::renderHighchart({
      kurse_verlauf_single(data_kurse,r)
    })

    output$plot_einstieg_comparison <- highcharter::renderHighchart({
      kurse_einstieg_comparison(data_kurse,r)
    })

    output$plot_verlauf_gender <- highcharter::renderHighchart({
      kurse_verlauf_gender(data_kurse,r)
    })

    output$plot_comparison_gender <- renderPlot({
      kurse_comparison_gender(data_kurse,r)
    })

    output$plot_ranking_gender <- renderPlot({
      kurse_ranking_gender(data_kurse,r)
    })


    output$plot_waffle_mint <- renderPlot({
      kurse_waffle_mint(data_kurse,r)
    })

    output$plot_comparison_subjects <- renderPlot({
      kurse_mint_comparison(data_kurse,r)
    })

    output$plot_comparison_bl <- renderPlot({
      kurse_mint_comparison_bl(data_kurse,r)
    })

    output$plot_pie_gender <- renderUI({
      kurse_einstieg_pie_gender(data_kurse,r)
    })


    data_table_mix_react <- reactive({
      data_mix_kurse(data_kurse, r)
    })

    output$data_table_mix <- DT::renderDT({
      data_table_mix_react()
    })

    data_table_verlauf_react <- reactive({
      data_verlauf_kurse(data_kurse, r)
    })

    output$data_table_verlauf <- DT::renderDT({
      data_table_verlauf_react()
    })


    # save histogram using downloadHandler and plot output type
    output$download_waffle <- shiny::downloadHandler(
      filename = function() {
        paste("plot_kurse", "png", sep = ".")
      },
      content = function(file){
        ggplot2::ggsave(file, plot = plot_waffle_react(), device = "png",
                        dpi = 300, width = 10, height = 6)
      }
    )

    # save histogram using downloadHandler and plot output type
    output$download_absolut <- shiny::downloadHandler(
      filename = function() {
        paste("plot_kurse", "png", sep = ".")
      },
      content = function(file){
        ggplot2::ggsave(file, plot = plot_absolut_react(), device = "png",
                        dpi = 300, width = 10, height = 6)
      }
    )


    output$download_ranking <- shiny::downloadHandler(
      filename = function() {
        paste("plot_kurse", "png", sep = ".")
      },
      content = function(file){
        ggplot2::ggsave(file, plot = plot_ranking_react(), device = "png",
                        dpi = 300, width = 10, height = 6)
      }
    )


    # save histogram using downloadHandler and plot output type
    output$download_data_box3 <- shiny::downloadHandler(
      filename = function() {
        paste("data_kurse", "csv", sep = ".")
      },
      content = function(file){
        write.csv(data_table_mix_react(), file)
      }
    )

    # save histogram using downloadHandler and plot output type
    output$download_data_box1 <- shiny::downloadHandler(
      filename = function() {
        paste("data_kurse", "csv", sep = ".")
      },
      content = function(file){
        write.csv(data_table_einstieg_react(), file)
      }
    )

    output$download_data_box7 <- shiny::downloadHandler(
      filename = function() {
        paste("data_kurse", "csv", sep = ".")
      },
      content = function(file){
        write.csv(data_table_verlauf_react(), file)
      }
    )
  })
}

## To be copied in the UI
# mod_schule_kurse_ui("schule_kurse_1")

## To be copied in the server
# mod_schule_kurse_server("schule_kurse_1")
