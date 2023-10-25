#' international_start UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_start_ui <- function(id){
  ns <- NS(id)
  tagList(

    # Banner
    fluidRow(
      shinydashboard::box(
        width = 12,
        img(src='www/Banner_Studium_BB.jpg',
            class = "img-responsive",
            #height = "150px", width = "150px",
            alt = "Banner Studium",
            style="display: block; margin-left: auto; margin-right: auto;"
        ))),


    # Info-Texte

    fluidRow(
      shinydashboard::box(
        title = "Auf dieser Seite",
        width = 7,
        p(style = "text-align: left; font-size = 16px",
          "LOREM IPSUM")
      ),

      shinydashboard::box(
        title = "Fragen oder Feedback?",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          "Sind alle Zahlen und Grafiken verständlich dargestellt?", br(), "Wir freuen uns über Rückfragen oder Feedback ", tags$a(href = "mailto:katharina.brunner@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per Email"),"oder über unsere kurze",
          tags$a(href="https://survey.lamapoll.de/MINT-DataLab_Feedback/", "Umfrage", target="_blank"), "!"
        ))
    ),

    fluidRow(
      shinydashboard::box(
        title = "Übersicht Fragestellungen",
        width = 7,
        p(
          style = "text-align: left; font-size = 16px",tags$a(href="#studium_mint",
                                                              span(tags$b(span("Fächerwahl MINT:")))), "Wie hoch ist der Anteil von Studierenden, die ein MINT-Fach studieren?"
        ),

        p(style = "text-align: left; font-size = 16px",tags$a(href="#studium_fach",
                                                              span(tags$b(span("M-I-N-T:")))), "Blick auf die einzelnen Fächer und Fachbereiche."

        ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#studium_frauen",
                                                              span(tags$b(span("Frauen in MINT:")))), "Wie hoch ist der Anteil von Frauen in den MINT-Fächern?"
        ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#studium_international",
                                                              span(tags$b(span("Internationale Studierende in MINT:")))), "Wie hoch ist der Anteil von internationalen Studierenden in den MINT-Fächern?"
        )),

      shinydashboard::box(
        title = "Datenquellen",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          "Studierendenzahlen in Deutschland: Destatis 2022, auf Anfrage")

      )
    ),


    # Box 1

    fluidRow(id="international_maps",
             shinydashboard::box(
               title = "LOREM IPSUM MAPS",
               width = 12,
               p("LOREM IPSUM INFO"),
               tabsetPanel(type = "tabs",
                           tabPanel("Vergleich Fächer (Karte)", br(),

                             #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      p("LOREM"),
                                      mod_international_map_ui("mod_international_map_ui_1")

                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      htmlOutput(ns("plot_international_studienzahl_map_1")),
                                      p(style="font-size:12px;color:grey",
                                        "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),

                                      shinyBS::bsPopover(id="h_international_1", title="",
                                                         content = paste0("POPUP INFO TEXT HERE"),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_1")
                                    )
                           ),

                           tabPanel("Vergleich Frauenanteil (Karte)", br(),

                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      p("LOREM"),
                                      mod_international_map_fem_ui("international_map_fem_ui_1")

                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      htmlOutput(ns("plot_international_map_fem_1")),
                                      # p(style="font-size:12px;color:grey",
                                      #   "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                                      #
                                      # shinyBS::bsPopover(id="h_international_1", title="",
                                      #                    content = paste0("POPUP INFO TEXT HERE"),
                                      #                    placement = "top",
                                      #                    trigger = "hover"),
                                      # tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_1")
                                    )
                           ),

                           tabPanel("Vergleiche Top 10", br(),

                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      p("LOREM"),
                                      mod_international_top10_mint_ui("international_top10_mint_1")

                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      htmlOutput(ns("plot_international_top10_mint_1")),
                                      p(style="font-size:12px;color:grey",
                                        "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),

                                      shinyBS::bsPopover(id="h_international_1", title="",
                                                         content = paste0("POPUP INFO TEXT HERE"),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_2")
                                    )
                           ),

                           tabPanel("Vergleiche Top 10 (Frauen)", br(),

                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      p("LOREM"),
                                      mod_international_top10_mint_gender_ui("international_top10_mint_gender_1")

                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      htmlOutput(ns("plot_international_top10_mint_gender_1")),
                                      p(style="font-size:12px;color:grey",
                                        "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),

                                      shinyBS::bsPopover(id="h_international_1", title="",
                                                         content = paste0("POPUP INFO TEXT HERE"),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_2")
                                    )
                           ),
                           tabPanel("Vergleiche Top 10 - Internationale in MINT", br(),

                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      p("LOREM"),
                                      mod_international_top10_mint_gender_ui("international_top10_mint_gender_1")

                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      htmlOutput(ns("plot_international_top10_mint_gender_1")),
                                      p(style="font-size:12px;color:grey",
                                        "Quelle der Daten: Destatis, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),

                                      shinyBS::bsPopover(id="h_international_1", title="",
                                                         content = paste0("POPUP INFO TEXT HERE"),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_2")
                                    )
                           )


               )))
  )
}

#' international_start Server Functions
#'
#' @noRd
mod_international_start_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Box 1
    output$plot_international_studienzahl_map_1 <- renderUI({
      plot_international_map(r)
    })

    output$plot_international_top10_mint_1 <- renderUI({
      plot_international_top10(r)
    })

    output$plot_international_top10_mint_gender_1 <- renderUI({
      plot_international_top10_gender(r)
    })

    output$plot_international_map_fem_1 <- renderUI({
    plot_international_map_fem(r)
    })


  })
}

## To be copied in the UI
# mod_international_start_ui("international_start_1")

## To be copied in the server
# mod_international_start_server("international_start_1")
