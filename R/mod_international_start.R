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

  logger::log_debug("start mod_international_start_ui")

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


    # Box 1 - Studium

    fluidRow(id="international_maps",
             shinydashboard::box(

               title = "Studium International",

               width = 12,
               p("LOREM IPSUM INFO"),
               tabsetPanel(type = "tabs",
                           tabPanel("Vergleich MINT-Anteil (Karte)", br(),

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

                                        "Quelle der Daten: Eurostat, 2023; OECD, 2023; UNESCO, 2023; eigene Berechnungen durch MINTvernetzt."),

                                      # shinyBS::bsPopover(id="h_international_1", title="",
                                      #                    content = paste0("POPUP INFO TEXT HERE"),
                                      #                    placement = "top",
                                      #                    trigger = "hover"),
                                      # tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_1")
                                    )
                           ),

                           tabPanel("Vergleich Frauen in MINT (Karte)", br(),

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
                                      p(style="font-size:12px;color:grey",
                                        "Quelle der Daten: Eurostat, 2023; OECD, 2023; eigene Berechnungen durch MINTvernetzt."),

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

                                        "Quelle der Daten: Eurostat, 2023; OECD, 2023; UNESCO, 2023; eigene Berechnungen durch MINTvernetzt."),

                                      # shinyBS::bsPopover(id="h_international_1", title="",
                                      #                    content = paste0("POPUP INFO TEXT HERE"),
                                      #                    placement = "top",
                                      #                    trigger = "hover"),
                                      # tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_2")

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

                                        "Quelle der Daten: Eurostat, 2023; OECD, 2023; eigene Berechnungen durch MINTvernetzt."),

                                      # shinyBS::bsPopover(id="h_international_1", title="",
                                      #                    content = paste0("POPUP INFO TEXT HERE"),
                                      #                    placement = "top",
                                      #                    trigger = "hover"),
                                      # tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_2")
                                    )
                           ),
                           tabPanel("Vergleiche Top 10 - Internationale in MINT", br(),
                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      p("LOREM"),
                                    mod_international_top10_mint_intl_ui("mod_international_top10_mint_intl_ui_1")),


                                    shiny::mainPanel(
                                    width = 9,
                                    htmlOutput(ns("plot_international_mint_top_10_1")),
                                      p(style="font-size:12px;color:grey",
                                        "Quelle der Daten: Eurostat, 2023; eigene Berechnungen durch MINTvernetzt."),


                                      # shinyBS::bsPopover(id="h_international_1", title="",
                                      #                    content = paste0("POPUP INFO TEXT HERE"),
                                      #                    placement = "top",
                                      #                    trigger = "hover"),
                                      # tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_2")
                                    )
                           )


               )),



    # Box 2 - Schule
    fluidRow(id="international_schule",
             shinydashboard::box(
               title = "INTERNATIONAL - SCHULE",
               width = 12,
               p("LOREM IPSUM INFO"),
               tabsetPanel(type = "tabs",
                           tabPanel("Vergleich Länder (Karte)", br(),

                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      p("LOREM"),
                                      mod_international_schule_map_ui("international_schule_map_1")

                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      htmlOutput(ns("plot_international_schule_map_1")),
                                      p(style="font-size:12px;color:grey",
                                        "Quelle der Daten: IEA, 2023; OECD, 2023, als Download, eigene Berechnungen durch MINTvernetzt."),

                                      shinyBS::bsPopover(id="h_international_schule_1", title="",
                                                         content = paste0("POPUP INFO TEXT HERE"),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_schule_1")
                                    )
                           ),

                           tabPanel("Ländervergleich Item", br(),


                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      p("LOREM"),

                                      mod_international_schule_item_ui("international_schule_item_1")


                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                                      htmlOutput(ns("plot_international_schule_item_1")),
                                      p(style="font-size:12px;color:grey",
                                        "Quelle der Daten: IEA, 2023, als Download, eigene Berechnungen durch MINTvernetzt."),

                                      shinyBS::bsPopover(id="h_international_schule_2", title="",
                                                         content = paste0("POPUP INFO TEXT HERE"),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_schule_2")
                                    )
                           ),

                           tabPanel("alle Länder im Vergleich", br(),

                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      p("LOREM"),
                                      mod_international_schule_migration_ui("international_schule_migration_1")

                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      htmlOutput(ns("plot_international_schule_migration_1")),
                                      p(style="font-size:12px;color:grey",
                                        "Quelle der Daten: IEA, 2023; OECD, 2023, als Download, eigene Berechnungen durch MINTvernetzt."),

                                      shinyBS::bsPopover(id="h_international_schule_3", title="",
                                                         content = paste0("POPUP INFO TEXT HERE"),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_schule_3")
                                    )
                           )
               )
             )
    ),




  # Box 3

  fluidRow(id="Arbeitsmarkt_International",
           shinydashboard::box(
             title = "AUSBILDUNG UND BERUF: MINT-Auszubildende und -Beschäftigte im Ländervergleich",
             width = 12,
             p("Diese Box zeigt eine Übersicht von MINT-Statistiken aus dem Bereich des Arbeitsmarkts für den internationalen Vergleich. Die Grafiken basieren auf öffentlichen Statistiken, die durch die EU und die OECD gesammelt wurden. Zum einen zeigen wir, wie groß der Anteil von MINT-Auszubildenden und Beschäftigten in verschiedenen Ländern ist. Außerdem ist zu sehen, in welchen Ländern der Frauenanteil besonders groß oder klein ist. Darüber hinaus werfen wir einen Blick auf Studiums- bzw. Ausbildungs-Anfänger*innen und Absolvent*innen in MINT im Ländervergleich."),
             tabsetPanel(type = "tabs",
                         tabPanel("Vergleich MINT-Anteil (Karte)", br(),

                                  #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                  # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                  shiny::sidebarPanel(
                                    width = 3,
                                    p("LOREM"),
                                    mod_international_map_arb_ui("mod_international_map_arb_ui_1")



                                  ),
                                  shiny::mainPanel(
                                    width = 9,
                                    htmlOutput(ns("plot_international_studienzahl_map_arb_1")),
                                    p(style="font-size:12px;color:grey",
                                      "Quelle der Daten: Eurostat, 2023; OECD, 2023; eigene Berechnungen durch MINTvernetzt."),


                                  )
                         ),
                         tabPanel("Vergleich Frauen in MINT (Karte)", br(),

                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      p("LOREM"),
                                      mod_international_map_arb_gender_ui("mod_international_map_arb_gender_ui_1")



                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      htmlOutput(ns("plot_international_map_arb_gender_1")),
                                      p(style="font-size:12px;color:grey",
                                        "Quelle der Daten: Eurostat, 2023; OECD, 2023; eigene Berechnungen durch MINTvernetzt."),


                                    )
                         ),tabPanel("Top 10 MINT-Länder", br(),

                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      p("LOREM"),
                                      mod_international_top10_mint_arb_ui("mod_international_top10_mint_arb_ui_1")



                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      htmlOutput(ns("plot_international_top10_mint_arb_1")),
                                      p(style="font-size:12px;color:grey",
                                        "Quelle der Daten: Eurostat, 2023; OECD, 2023; eigene Berechnungen durch MINTvernetzt."),


                                    )
                         ),tabPanel("Top 10 Länder Frauen in MINT", br(),

                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      p("LOREM"),
                                      mod_international_top10_mint_arb_gender_ui("mod_international_top10_mint_arb_gender_ui_1")



                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      htmlOutput(ns("plot_international_top10_mint_arb_gender_1")),
                                      p(style="font-size:12px;color:grey",
                                        "Quelle der Daten: Eurostat, 2023; OECD, 2023; eigene Berechnungen durch MINTvernetzt."),


                                    )
                         ),tabPanel("Vergleiche", br(),

                                    #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                    # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

                                    shiny::sidebarPanel(
                                      width = 3,
                                      p("LOREM"),
                                      mod_international_arbeitsmarkt_vergleich_ui("international_arbeitsmarkt_vergleich_1")

                                    ),
                                    shiny::mainPanel(
                                      width = 9,
                                      htmlOutput(ns("plot_international_arbeitsmarkt_vergleiche_1")),
                                      p(style="font-size:12px;color:grey",
                                        "Quelle der Daten: IEA, 2023; OECD, 2023, als Download, eigene Berechnungen durch MINTvernetzt."),

                                      shinyBS::bsPopover(id="h_international_arbeit_3", title="",
                                                         content = paste0("POPUP INFO TEXT HERE"),
                                                         placement = "top",
                                                         trigger = "hover"),
                                      tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_international_arbeit_3")
                                    )
                           )


             )))))

}

#' international_start Server Functions
#'
#' @noRd
mod_international_start_server <- function(id, r){

  logger::log_debug("start mod_international_start_server")

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Box 1 - Studium
    output$plot_international_studienzahl_map_1 <- renderUI({
      logger::log_debug("plot_international_map")
      plot_international_map(r)
    })

    output$plot_international_top10_mint_1 <- renderUI({
      logger::log_debug("plot_international_top10")
      plot_international_top10(r)
    })

    output$plot_international_top10_mint_gender_1 <- renderUI({
      logger::log_debug("plot_international_top10_gender")
      plot_international_top10_gender(r)
    })


    output$plot_international_map_fem_1 <- renderUI({
    plot_international_map_fem(r)
    })

    output$plot_international_mint_top_10_1 <- renderUI({
      plot_international_mint_top_10(r)
    })

    # Box 3
    output$plot_international_studienzahl_map_arb_1 <- renderUI({
      plot_international_map_arb(r)
    })

    output$plot_international_map_arb_gender_1 <- renderUI({
      plot_international_map_arb_gender(r)
    })

    output$plot_international_top10_mint_arb_1 <- renderUI({
      plot_international_top10_mint_arb(r)
    })

    output$plot_international_top10_mint_arb_gender_1 <- renderUI({
    plot_international_top10_mint_arb_gender(r)
    })


    # Box 2 - Schule
    output$plot_international_schule_map_1 <- renderUI({
      logger::log_debug("plot_international_schule_map")
      plot_international_schule_map(r)
    })

    output$plot_international_schule_item_1 <- renderUI({
      logger::log_debug("plot_international_schule_item")
      plot_international_schule_item(r)
    })

    output$plot_international_schule_migration_1 <- renderUI({
      logger::log_debug("plot_international_schule_migration")
      plot_international_schule_migration(r)
    })


    # Box 3 - Arbeitsmarkt
    output$plot_international_arbeitsmarkt_map_1 <- renderUI({
      logger::log_debug("plot_international_arbeitsmarkt_map_1")
      plot_international_arbeitsmarkt_map(r)
    })

    output$plot_international_arbeitsmakrt_top10_1 <- renderUI({
      logger::log_debug("plot_international_arbeitsmakrt_top10_1")
      plot_international_arbeitsmakrt_top10(r)
    })

    output$plot_international_arbeitsmarkt_vergleiche_1 <- renderUI({
      logger::log_debug("plot_international_arbeitsmarkt_vergleiche_1")
      plot_international_arbeitsmarkt_vergleiche(r)
    })


  })
}

## To be copied in the UI
# mod_international_start_ui("international_start_1")

## To be copied in the server
# mod_international_start_server("international_start_1")
