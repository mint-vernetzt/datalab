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


    # Info-Texte ----

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
          style = "text-align: left; font-size = 16px",tags$a(href="#international_schule",
                                                              span(tags$b(span("Schule:")))), "MINT-Kompetenzen von Schüler*innen im internationalen Vergleich"
        ),

        p(style = "text-align: left; font-size = 16px",tags$a(href="#international_studium",
                                                              span(tags$b(span("Studium:")))), "MINT-Studium im internationalen Vergleich"

        ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#international_arbeitsmarkt",
                                                              span(tags$b(span("Ausbildung und Beruf:")))), "MINT-Auszubildende und -Beschäftigte im Ländervergleich"
        )
        #,
        # p(style = "text-align: left; font-size = 16px",tags$a(href="#studium_international",
        #                                                       span(tags$b(span("Internationale Studierende in MINT:")))), "Wie hoch ist der Anteil von internationalen Studierenden in den MINT-Fächern?"
        #
        # )
      ),

      shinydashboard::box(
        title = "Datenquellen",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          "Studierendenzahlen in Deutschland: Destatis 2022, auf Anfrage")

      )
    ),

    # Box 1 - Schule ----
    fluidRow(
      id="international_schule",
      shinydashboard::box(
        title = "SCHULE: MINT-Kompetenzen von Schüler*innen im internationalen Vergleich",
        width = 12,
        p("In dieser Box blicken wir auf die Kompetenzdaten der Pisa-Studie und der TIMSS-Erhebung.
               Bei beiden Erhebungen werden unter anderem Kompetenztests in Mathematik und Naturwissenschaften in 4. und 9. Klassen weltweit durchgeführt.
               X Schüler*innen haben in der letzten Befragung teilgenommen. Außerdem betrachten die Grafiken Kompetenz-Unterschiede zwischen Jungen und Mädchen und Unterschiede
                 in Abhängigkeit der Herkunft der Kinder."),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "MINT-Kompetenz im Ländervergleich", br(),

            #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
            # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

            shiny::sidebarPanel(
              width = 3,
              #p("LOREM"),
              #mod_international_map_ui("mod_international_map_ui_1")
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

          tabPanel(
            "MINT-Kompetenz von Jungen und Mädchen", br(),


            #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
            # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

            shiny::sidebarPanel(
              width = 3,
              #p("LOREM"),

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

          tabPanel(
            "MINT-Kompetenz im Gruppenvergleich", br(),

            #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
            # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

            shiny::sidebarPanel(
              width = 3,
              #p("LOREM"),
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


    # Box 2 - Studium ----

    fluidRow(
      id="international_studium",
      shinydashboard::box(

        title = "STUDIUM: MINT-Studium im internationalen Vergleich",

        width = 12,
        p("Diese Box zeigt eine Übersicht von MINT-Statistiken aus dem Bereich Studium für den internationalen Vergleich.
               Die Grafiken basieren auf öffentlichen Statistiken,die durch die EU, die OECD und die UNESCO gesammelt wurden.
                 Zum einen zeigen wir, wie groß der Anteil von MINT-Studierenden an allen Studierenden in verschiedenen Ländern ist. Außerdem ist zu sehen,
                 in welchen Ländern der Frauenanteil oder der Anteil an internationalen Studierenden in MINT-Studiengängen besonders groß oder klein ist."),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Vergleich MINT-Anteil (Karte)", br(),

            #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
            # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

            shiny::sidebarPanel(
              width = 3,
              #p("LOREM"),
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

          tabPanel(
            "Vergleich Frauen in MINT (Karte)", br(),

            #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
            # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

            shiny::sidebarPanel(
              width = 3,
              #p("LOREM"),
              mod_international_map_fem_ui("international_map_fem_ui_1")

            ),
            shiny::mainPanel(
              width = 9,
              htmlOutput(ns("plot_international_map_fem_1")),
              p(style="font-size:12px;color:grey",
                "Quelle der Daten: Eurostat, 2023; OECD, 2023; eigene Berechnungen durch MINTvernetzt."),

            )
          ),

          tabPanel(
            "Top 10 MINT-Länder", br(),

            #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
            # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

            shiny::sidebarPanel(
              width = 3,
              #p("LOREM"),
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

          tabPanel(
            "Top 10 Länder Frauen in MINT", br(),


            #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
            # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

            shiny::sidebarPanel(
              width = 3,
              #p("LOREM"),
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
          tabPanel(
            "Vergleich internationale Studierede", br(),
            #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
            # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

            shiny::sidebarPanel(
              width = 3,
              #p("LOREM"),
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


        )
      ),








      # Box 3 - Arbeitsmarkt ----

      fluidRow(
        id="international_arbeitsmarkt",
        shinydashboard::box(
          title = "AUSBILDUNG UND BERUF: MINT-Auszubildende und -Beschäftigte im Ländervergleich",
          width = 12,
          p("Diese Box zeigt eine Übersicht von MINT-Statistiken aus dem Bereich des Arbeitsmarkts für den internationalen Vergleich. Die Grafiken basieren auf öffentlichen Statistiken, die durch die EU und die OECD gesammelt wurden. Zum einen zeigen wir, wie groß der Anteil von MINT-Auszubildenden und Beschäftigten in verschiedenen Ländern ist. Außerdem ist zu sehen, in welchen Ländern der Frauenanteil besonders groß oder klein ist. Darüber hinaus werfen wir einen Blick auf Studiums- bzw. Ausbildungs-Anfänger*innen und Absolvent*innen in MINT im Ländervergleich."),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Vergleich MINT-Anteil (Karte)", br(),

              #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
              # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

              shiny::sidebarPanel(
                width = 3,
                #p("LOREM"),
                mod_international_map_arb_ui("mod_international_map_arb_ui_1")



              ),
              shiny::mainPanel(
                width = 9,
                htmlOutput(ns("plot_international_studienzahl_map_arb_1")),
                p(style="font-size:12px;color:grey",
                  "Quelle der Daten: Eurostat, 2023; OECD, 2023; eigene Berechnungen durch MINTvernetzt."),


              )
            ),
            tabPanel(
              "Vergleich Frauen in MINT (Karte)", br(),

              #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
              # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

              shiny::sidebarPanel(
                width = 3,
                #p("LOREM"),
                mod_international_map_arb_gender_ui("mod_international_map_arb_gender_ui_1")



              ),
              shiny::mainPanel(
                width = 9,
                htmlOutput(ns("plot_international_map_arb_gender_1")),
                p(style="font-size:12px;color:grey",
                  "Quelle der Daten: Eurostat, 2023; OECD, 2023; eigene Berechnungen durch MINTvernetzt."),


              )
            ),
            tabPanel(
              "Top 10 MINT-Länder", br(),

              #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
              # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

              shiny::sidebarPanel(
                width = 3,
                #p("LOREM"),
                mod_international_top10_mint_arb_ui("mod_international_top10_mint_arb_ui_1")



              ),
              shiny::mainPanel(
                width = 9,
                htmlOutput(ns("plot_international_top10_mint_arb_1")),
                p(style="font-size:12px;color:grey",
                  "Quelle der Daten: Eurostat, 2023; OECD, 2023; eigene Berechnungen durch MINTvernetzt."),


              )
            ),
            tabPanel(
              "Top 10 Länder Frauen in MINT", br(),

              #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
              # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

              shiny::sidebarPanel(
                width = 3,
                #p("LOREM"),
                mod_international_top10_mint_arb_gender_ui("mod_international_top10_mint_arb_gender_ui_1")



              ),
              shiny::mainPanel(
                width = 9,
                htmlOutput(ns("plot_international_top10_mint_arb_gender_1")),
                p(style="font-size:12px;color:grey",
                  "Quelle der Daten: Eurostat, 2023; OECD, 2023; eigene Berechnungen durch MINTvernetzt."),

              )
            ),
            tabPanel(
              "Vergleiche", br(),

              #        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
              # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

              shiny::sidebarPanel(
                width = 3,
                #p("LOREM"),
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
          )
        )
      ),


      # Box 4 - Fachkräfte because it does not work in an extra page...?!? ----
      fluidRow(
        id="fachkraft_plots",
        shinydashboard::box(
          title = "FACHKRAFT - EPA",
          width = 12,
          p("LOREM IPSUM INFO"),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              title = "EPA nach MINT", br(),

              # tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
              # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

              shiny::sidebarPanel(
                width = 3,
                mod_fachkraft_item_epa_ui("fachkraft_item_epa_1"),
                br(),
                downloadButton(
                  outputId = ns("download_btn_plot_fachkraft_epa_item_1"),
                  label = "Download (links)",
                  icon = icon("download")),
                downloadButton(
                  outputId = ns("download_btn_plot_fachkraft_epa_item_2"),
                  label = "Download (rechts)",
                  icon = icon("download")),
              ),
              shiny::mainPanel(
                width = 9,
                htmlOutput(ns("plot_fachkraft_epa_item_1")),
                p(style="font-size:12px;color:grey",
                  "hier Quellen"),
                shinyBS::bsPopover(
                  id="h_fachkraft_arbeitsmarkt_1", title="",
                  content = paste0("POPUP INFO TEXT HERE"),
                  placement = "top",
                  trigger = "hover"),
                tags$a(paste0("Hinweis zu den Daten"),
                       icon("info-circle"),
                       id = "h_fachkraft_arbeitsmarkt_1")
              )
            ),
            tabPanel(
              "MINT nach EPA", br(),

              # tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
              # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

              shiny::sidebarPanel(
                width = 3,
                mod_fachkraft_item_mint_ui("fachkraft_item_mint_1")

              ),
              shiny::mainPanel(
                width = 9,
                htmlOutput(ns("plot_fachkraft_mint_item_1")),
                p(style="font-size:12px;color:grey",
                  "hier Quellen"),
                shinyBS::bsPopover(id="h_fachkraft_arbeitsmarkt_2", title="",
                                   content = paste0("POPUP INFO TEXT HERE"),
                                   placement = "top",
                                   trigger = "hover"),
                tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_fachkraft_arbeitsmarkt_2")
              )
            ),

            tabPanel(
              "Detailansicht", br(),

              # tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
              # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

              shiny::sidebarPanel(
                width = 3,
                mod_fachkraft_item_detail_ui("fachkraft_item_detail_1"),
                downloadButton(
                  outputId = ns("download_btn_plot_fachkraft_item_detail_1"),
                  label = "Download",
                  icon = icon("download"))
              ),
              shiny::mainPanel(
                width = 9,
                htmlOutput(ns("plot_fachkraft_detail_item_1")),
                p(style="font-size:12px;color:grey",
                  "hier Quellen"),

                shinyBS::bsPopover(id="h_fachkraft_arbeitsmarkt_3", title="",
                                   content = paste0("POPUP INFO TEXT HERE"),
                                   placement = "top",
                                   trigger = "hover"),
                tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_fachkraft_arbeitsmarkt_3")
              )
            ),

            tabPanel(
              "Arbeitslosen-Stellen-Relation und Vakanzzeit in MINT", br(),

              # tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
              # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),

              shiny::sidebarPanel(
                width = 3,
                mod_fachkraft_bar_vakanz_ui("fachkraft_bar_vakanz_1"),
                downloadButton(
                  outputId = ns("download_btn_plot_fachkraft_bar_vakanz_1"),
                  label = "Download",
                  icon = icon("download"))

              ),
              shiny::mainPanel(
                width = 9,
                htmlOutput(ns("plot_fachkraft_bar_vakanz_1")),
                p(style="font-size:12px;color:grey",
                  "hier Quellen"),
                shinyBS::bsPopover(id="h_fachkraft_arbeitsmarkt_4", title="",
                                   content = paste0("POPUP INFO TEXT HERE"),
                                   placement = "top",
                                   trigger = "hover"),
                tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_fachkraft_arbeitsmarkt_4")
              )
            ),
          )
        )
      ),

      # Box 5 - Table for user selection ----
      fluidRow(
        id="international_table_box",
        shinydashboard::box(
          title = "INTERNATIONAL - TABLLE",
          width = 12,
          p("LOREM IPSUM INFO"),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              title = "Tabelle", br(),

              shiny::sidebarPanel(
                width = 12,
                mod_international_table_input_ui("international_table_input_1"),
              ),
              shiny::mainPanel(
                width = 12,
                DT::dataTableOutput(outputId = ns("international_table_1")),
                br(),
                downloadButton(
                  outputId = ns("download_btn_png_international_table_1"),
                  label = "Download Tabelle (png)",
                  icon = icon("download")),
                downloadButton(
                  outputId = ns("download_btn_csv_international_table_1"),
                  label = "Download Daten (csv)",
                  icon = icon("download")),
                # quellen sind schon in der Tabelle enthalten
                # p(style="font-size:12px;color:grey",
                #   "hier Quellen"),
                # shinyBS::bsPopover(
                #   id="h_fachkraft_arbeitsmarkt_1", title="",
                #   content = paste0("POPUP INFO TEXT HERE"),
                #   placement = "top",
                #   trigger = "hover"),
                # tags$a(paste0("Hinweis zu den Daten"),
                #        icon("info-circle"),
                #        id = "h_fachkraft_arbeitsmarkt_1")
              )
            )
          )
        )
      )
    )
  )

}

  # Server -------

#' international_start Server Functions
#'
#' @noRd
mod_international_start_server <- function(id, r){

  logger::log_debug("start mod_international_start_server")

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Box 1 - Schule
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

    # Box 2 - Studium
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

    # Box 3 - Arbeitsmarkt
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

    # Box 3 - Arbeitsmarkt (Jakob)
    # output$plot_international_arbeitsmarkt_map_1 <- renderUI({
    #   logger::log_debug("plot_international_arbeitsmarkt_map_1")
    #   plot_international_arbeitsmarkt_map(r)
    # })
    #
    # output$plot_international_arbeitsmakrt_top10_1 <- renderUI({
    #   logger::log_debug("plot_international_arbeitsmakrt_top10_1")
    #   plot_international_arbeitsmakrt_top10(r)
    # })

    output$plot_international_arbeitsmarkt_vergleiche_1 <- renderUI({
      logger::log_debug("plot_international_arbeitsmarkt_vergleiche_1")
      plot_international_arbeitsmarkt_vergleiche(r)
    })

    # Box 4 - Fachkraft

    ## EPA nach MINT
    output$plot_fachkraft_epa_item_1 <- renderUI({
      plot_list <- plot_fachkraft_epa_item(r)
      r$plot_fachkraft_epa_item_1_left <- plot_list[[1]]
      r$plot_fachkraft_epa_item_1_right <- plot_list[[2]]

      r$plot_fachkraft_epa_item_1_left_title <- get_plot_title(
        plot = r$plot_fachkraft_epa_item_1_left
      )
      r$plot_fachkraft_epa_item_1_right_title <- get_plot_title(
        plot = r$plot_fachkraft_epa_item_1_right
      )

      # return plots
      out <- highcharter::hw_grid(
        plot_list,
        ncol = 2)
      out

    })

    output$download_btn_plot_fachkraft_epa_item_1 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_fachkraft_epa_item_1_left_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_fachkraft_epa_item_1_left,
          filename =  r$plot_fachkraft_epa_item_1_left_title,
          width = 700,
          height = 400)

        file.copy(r$plot_fachkraft_epa_item_1_left_title, file)
        file.remove(r$plot_fachkraft_epa_item_1_left_title)
      }
    )

    output$download_btn_plot_fachkraft_epa_item_2 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_fachkraft_epa_item_1_right_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download
        add_caption_and_download(
          hc = r$plot_fachkraft_epa_item_1_right,
          filename =  r$plot_fachkraft_epa_item_1_right_title,
          width = 700,
          height = 400)

        file.copy(r$plot_fachkraft_epa_item_1_right_title, file)
        file.remove(r$plot_fachkraft_epa_item_1_right_title)
      }
    )

    ## MINT an EPA
    output$plot_fachkraft_mint_item_1 <- renderUI({
      plot_fachkraft_mint_item(r)
    })

    ## Detail Berufe
    output$plot_fachkraft_detail_item_1 <- renderUI({
      plot_list <- plot_fachkraft_detail_item(r)
      r$plot_fachkraft_detail_item_1_left <- plot_list[[1]]
      r$plot_fachkraft_detail_item_1_right <- plot_list[[2]]

      r$plot_fachkraft_detail_item_1_left_title <- get_plot_title(
        plot = r$plot_fachkraft_detail_item_1_left
      )
      r$plot_fachkraft_detail_item_1_right_title <- get_plot_title(
        plot = r$plot_fachkraft_detail_item_1_right
      )

      highcharter::hw_grid(
        plot_list,
        ncol = 2)
    })

    output$download_btn_plot_fachkraft_item_detail_1 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_fachkraft_detail_item_1_right_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_fachkraft_detail_item_1_right,
          filename =  r$plot_fachkraft_detail_item_1_right_title,
          width = 700,
          height = 400)

        file.copy(r$plot_fachkraft_detail_item_1_right_title, file)
        file.remove(r$plot_fachkraft_detail_item_1_right_title)
      }
    )

    ## Bar Vakanz
    output$plot_fachkraft_bar_vakanz_1 <- renderUI({
      plot_list <- plot_fachkraft_bar_vakanz(r)
      r$plot_fachkraft_bar_vakanz_1 <- plot_list

      r$plot_fachkraft_bar_vakanz_1_title <- get_plot_title(
        plot = r$plot_fachkraft_bar_vakanz_1
      )

      plot_list
    })

    output$download_btn_plot_fachkraft_bar_vakanz_1 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_fachkraft_bar_vakanz_1_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_fachkraft_bar_vakanz_1,
          filename =  r$plot_fachkraft_bar_vakanz_1_title,
          width = 700,
          height = 400)

        file.copy(r$plot_fachkraft_bar_vakanz_1_title, file)
        file.remove(r$plot_fachkraft_bar_vakanz_1_title)
      }
    )

    # BOX 5 International Table

    output$international_table_1 <- DT::renderDataTable({
      r$int_table_DT <- DT::datatable(
        data = r$int_table,
        # filter = list(position = "top"),
        rownames = FALSE,
        colnames = stringr::str_to_title(names(r$int_table)),
        escape = FALSE,
        options = list(
          dom = "t"),
        # add logo and source
        caption = htmltools::tags$caption(
          style = 'caption-side: bottom; text-align: right;',
          htmltools::div(
            style = "display: flex; justify-content: space-between;",
            htmltools::p(paste0("Quellen: ", r$int_table_source)),
            htmltools::img(
              src="https://raw.githubusercontent.com/mint-vernetzt/datalab/main/inst/app/www/MINTvernetztLogo_klein.png",
              alt="MINT vernetzt Logo",
              width="30",height="30",
              style = "align-self: center;"
            )
          )
        )
      )

      r$int_table_DT
    })

    output$download_btn_png_international_table_1 <- downloadHandler(
      contentType = "text/csv",
      filename = function() {"International_data_custom_table.png"},
      content = function(file) {
        logger::log_info("Donwload png custom table with international data")
        download_table(table = r$int_table_DT,
                       filename = "International_data_custom_table.png",
                       width = 1000,
                       height = 300)

        file.copy("International_data_custom_table.png", file)
        file.remove("International_data_custom_table.png")
      }
    )

    output$download_btn_csv_international_table_1 <- downloadHandler(
      contentType = "text/csv",
      filename = function() {"International_data_custom_table.csv"},
      content = function(file) {
        logger::log_info("Donwload csv custom table with international data")

        write.csv2(x = prep_download_data(r$int_table_csv),
                   file = file,
                   row.names = FALSE)

      }
    )
  })
}

## To be copied in the UI
# mod_international_start_ui("international_start_1")

## To be copied in the server
# mod_international_start_server("international_start_1")
