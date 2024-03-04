#' suche UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_suche_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Banner
    fluidRow(
      shinydashboard::box(
        width = 12,
        img(src='www/Banner_Suche.jpg',
            class = "img-responsive",
            #height = "150px", width = "150px",
            alt = "Banner Quellen",
            style="display: block; margin-left: auto; margin-right: auto;"
        ))),

    # Info-Texte ----
    fluidRow(
      shinydashboard::box(
        title = "BETA: Suche",
        width = 7,
        p("Auf dieser Seite werden die Ergebnisse des Suchfeldes angezeigt. Bei der vorläufgen Suchfunktion handelt es sich noch um eine BETA-Version, die nicht abschließend optimiert ist.", br(), "Zum Aufrufen einer Grafik dient der Link in der Ergebnistabelle.
          Dieser Link führt zur entsprechenden Unterseite und der richtgien Box. Von dort aus kann die gewünschte Registerkarte aufgerufen werden."),
        br(),
        p(tags$b(span(
          "Sucheingabe:"))),
        p(mod_suche_eingabe_ui("suche_eingabe_2"))
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
        title = "",
        width = 12,
        p(tags$b(span("Suchergebnisse für:"))),
        p(shiny::textOutput(ns("suche_txt"))),br(),
        # p(style = "text-align: left; font-size = 16px",
        #   "Suchergebnisse"),

        DT::dataTableOutput(outputId = ns("search_table"))
      )
    )
  )
}

#' suche Server Functions
#'
#' @noRd
mod_suche_server <- function(id, react_search, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    shinyInput <- function(FUN, len, id, ns, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
      }
      inputs
    }

    shiny::observeEvent(react_search$suchtabelle, {
      output$suche_txt <- shiny::renderText(react_search$suche_eingabe_txt)
    })


    search_table <- reactive({

      tmp <- react_search$suchtabelle
      tmp$Link <- shinyInput(
        actionButton, nrow(react_search$suchtabelle),
        'rowline_',
        label = "Plot",
        icon = shiny::icon("chart-column"),
        onclick = sprintf("Shiny.onInputChange('%s', this.id)", ns("select_button"))
      )
      tmp
    })




    output$search_table <- DT::renderDataTable({
      target_cols <- which(
        # select columns to be shown in the table
        !names(search_table()) %in% c("Bereich", "Registerkarte", "Plotart", "Link")
        # the - 1 is because js uses 0 index instead of 1 like R
        ) - 1

      DT::datatable(search_table(),
                # filter = list(position = "top"),
                rownames = FALSE,
                escape = FALSE,
                options = list(
                  pageLength = 10,
                  dom = "tp",
                  columnDefs = list(
                    list(
                      visible = FALSE,
                      targets = target_cols)
                    )
                )
      )
    }
    # ,
    # escape = FALSE,
    # rownames = FALSE,
    # selection = "none",
    # options = list(
    #   dom = 'rtSip',
    #   searching = FALSE
    # )
    )


    observeEvent(input$select_button, {

      # naming is "rowline_" & "id", eg rowline_1
      # important is the _1 to be seperateds
      selected_idx <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
      # selected_row <- suchtabelle[1,]
      selected_row <- react_search$suchtabelle[selected_idx,]

      shinydashboard::updateTabItems(
        session = parent_session,
        inputId = "tabs",
        selected = selected_row$menuItem..tabName)



      # set delay of 0.5 second to let the target page load
      session$sendCustomMessage(
        type = 'delayedScroll',
        message = list(
          id = selected_row$Box..ID,
          delay = 1000)
      )



    })



  })
}

## To be copied in the UI
# mod_suche_ui("suche_1")

## To be copied in the server
# mod_suche_server("suche_1")
