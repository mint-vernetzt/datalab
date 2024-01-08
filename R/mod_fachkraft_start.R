#' fachkraft_start UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fachkraft_start_ui <- function(id){

  logger::log_debug("start mod_fachkraft_start_ui")

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

    # Box 1 - Arbeitsmarkt
    fluidRow(id="fachkraft_arbeitsmarkt",
             shinydashboard::box(
               title = "FACHKRÄFTE",
               width = 12,
               p("LOREM IPSUM INFO"),
               tabsetPanel(type = "tabs",
                           # tabPanel("EPA nach MINT", br(),
                           #
                           #          # tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                           #          # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                           #
                           #          shiny::sidebarPanel(
                           #            width = 4,
                           #            mod_fachkraft_item_epa_ui("fachkraft_item_epa_1")
                           #
                           #          ),
                           #          shiny::mainPanel(
                           #            width = 8,
                           #            htmlOutput(ns("plot_fachkraft_epa_item_1")),
                           #            p(style="font-size:12px;color:grey",
                           #              "hier Quellen"),
                           #            shinyBS::bsPopover(id="h_fachkraft_arbeitsmarkt_1", title="",
                           #                               content = paste0("POPUP INFO TEXT HERE"),
                           #                               placement = "top",
                           #                               trigger = "hover"),
                           #            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_fachkraft_arbeitsmarkt_1")
                           #          )
                           # ),

                           # tabPanel("MINT nach EPA", br(),
                           #
                           #          # tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                           #          # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                           #
                           #          shiny::sidebarPanel(
                           #            width = 3,
                           #            mod_fachkraft_item_mint_ui("fachkraft_item_mint_1")
                           #
                           #          ),
                           #          shiny::mainPanel(
                           #            width = 9,
                           #            htmlOutput(ns("plot_fachkraft_mint_item_1")),
                           #            p(style="font-size:12px;color:grey",
                           #              "hier Quellen"),
                           #            shinyBS::bsPopover(id="h_fachkraft_arbeitsmarkt_2", title="",
                           #                               content = paste0("POPUP INFO TEXT HERE"),
                           #                               placement = "top",
                           #                               trigger = "hover"),
                           #            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_fachkraft_arbeitsmarkt_2")
                           #          )
                           # ),
                           #
                           # tabPanel("Detailansicht", br(),
                           #
                           #          # tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                           #          # .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                           #
                           #          shiny::sidebarPanel(
                           #            width = 3,
                           #            mod_fachkraft_item_detail_ui("fachkraft_item_detail_1")
                           #          ),
                           #          shiny::mainPanel(
                           #            width = 9,
                           #            htmlOutput(ns("plot_fachkraft_detail_item_1")),
                           #            p(style="font-size:12px;color:grey",
                           #              "hier Quellen"),
                           #
                           #            shinyBS::bsPopover(id="h_fachkraft_arbeitsmarkt_3", title="",
                           #                               content = paste0("POPUP INFO TEXT HERE"),
                           #                               placement = "top",
                           #                               trigger = "hover"),
                           #            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_fachkraft_arbeitsmarkt_3")
                           #          )
                           # )
               )
             )
    )
  )
}

#' fachkraft_start Server Functions
#'
#' @noRd
mod_fachkraft_start_server <- function(id, r){

  logger::log_debug("start mod_fachkraft_start_server")

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Box 1 - Arbeitsmarkt
    # output$plot_fachkraft_epa_item_1 <- renderUI({
    #   plot_fachkraft_epa_item(r)
    # })

    # output$plot_fachkraft_mint_item_1 <- renderUI({
    #   plot_fachkraft_mint_item(r)
    # })
    #
    # output$plot_fachkraft_detail_item_1 <- renderUI({
    #   plot_fachkraft_detail_item(r)
    # })

  })
}

## To be copied in the UI
# mod_fachkraft_start_ui("fachkraft_start_1")

## To be copied in the server
# mod_fachkraft_start_server("fachkraft_start_1")
