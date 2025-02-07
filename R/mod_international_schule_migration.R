#' international_schule_migration UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_schule_migration_ui <- function(id){



  ns <- NS(id)
  tagList(

    tags$head(
      tags$style(HTML("
        .dropdown-menu .bs-actionsbox .btn-group .btn {
          background-color: #e7f1ff !important;  /* Hellblau für die Alle auswählen/abwählen Buttons */
          color: #000000 !important;
        }
        .dropdown-menu .bs-actionsbox .btn-group .btn:hover {
          background-color: #d0e8ff !important;  /* Etwas dunkleres Blau beim Hover */
          color: #000000 !important;
        }
      "))
    ),



    p("Erhebung:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("line_l_int_schule"),
      choices = c("TIMSS", "PISA"),
      justified = TRUE
    ),

    conditionalPanel(condition = "input.line_l_int_schule == 'TIMSS'",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("line_y_timss_int_schule"),
                       label = NULL,
                       choices = international_ui_years(region = "TIMSS"),
                       selected = "2023"
                     ),

                     p("Fachbereich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("line_f_timss_int_schule"),
                       choices = c("Mathematik", "Naturwissenschaften"),
                       selected = c("Mathematik"),
                       multiple = FALSE
                     ),

                     p("Indikator:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("line_li_timss_int_schule"),
                       choices = c("nach Geschlecht", "nach sozialem Status"),
                       selected = c("nach sozialem Status"),
                       multiple = FALSE
                     )),

                     # p("Länder:"),
                     # shinyWidgets::pickerInput(
                     #   inputId = ns("regio_int_schule"),
                     #   label = NULL,
                     #   choices = international_ui_country("TIMSS"),
                     #   multiple = TRUE,
                     #   options =  list(
                     #     "max-options" = 10,
                     #     "max-options-text" = "<span style='color: red;'>Maximal 10 Länder auswählen</span>"),
                     #   selected = c("Interantionaler Durchschnitt", "Deutschland","Schweden", "Italien", "Türkei","Vereinigte Staaten" )
                     # )),

    conditionalPanel(condition = "input.line_l_int_schule == 'PISA'",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("line_y_pisa_int_schule"),
                       label = NULL,
                       choices = international_ui_years(region = "PISA"),
                       selected = "2022"
                     ),

                     p("Fachbereich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("line_f_pisa_int_schule_timss"),
                       choices = c("Mathematik", "Naturwissenschaften"),
                       selected = c("Mathematik"),
                       multiple = FALSE
                     ),

                     p("Indikator"),
                     shinyWidgets::pickerInput(
                       inputId = ns("line_li_pisa_int_schule"),
                       choices = c("nach Geschlecht",
                                   "nach Zuwanderungsgeschichte",
                                   "nach Bildungskapital"),
                       selected = c("nach Bildungskapital"),
                       multiple = FALSE
                     )),

                     # p("Länder:"),
                     # shinyWidgets::pickerInput(
                     #   inputId = ns("regio_int_schule_pisa"),
                     #   label = NULL,
                     #   choices = international_ui_country("PISA"),
                     #   multiple = TRUE,
                     #   options =  list(
                     #     "max-options" = 10,
                     #     "max-options-text" = "<span style='color: red;'>Maximal 10 Länder auswählen</span>"),
                     #   selected = c("OECD Durchschnitt", "Deutschland","Schweden", "Italien", "Türkei","Vereinigte Staaten" )
                     # )),

    ###

    p("Länder:"),
      shinyWidgets::pickerInput(
        inputId = ns("regio_int_schule"),
        label = NULL,
        choices = international_ui_country("TIMSS"),
        # choices = c(
        #   "Interantionaler Durchschnitt",
        #   "Deutschland",
        #   "Korea, Republik von",
        #   "Schweden",
        #   "Dänemark",
        #   "Kanada",
        #   "Nordirland",
        #   "Finnland",
        #   "Irland",
        #   "Norwegen",
        #   "Singapur",
        #   "Zypern",
        #   "Frankreich",
        #   "Ungarn",
        #   "Belgien",
        #   "Hongkong",
        #   "Malta",
        #   "Tschechische Republik",
        #   "Österreich",
        #   "Polen",
        #   "Spanien",
        #   "Taiwan",
        #   "Lettland",
        #   "Portugal",
        #   "Slowakei",
        #   "Litauen",
        #   "Georgien",
        #   "Russische Föderation",
        #   "Bulgarien",
        #   "Serbien",
        #   "Japan",
        #   "Katar",
        #   "Kroatien",
        #   "Italien",
        #   "Montenegro",
        #   "Bahrain",
        #   "Nordmazedonien",
        #   "Kasachstan",
        #   "Türkei",
        #   "Armenien",
        #   "Albanien",
        #   "Bosnien und Herzegowina",
        #   "Iran",
        #   "Oman",
        #   "Chile",
        #   "Kuwait",
        #   "Kosovo",
        #   "Saudi Arabien",
        #   "Aserbaidschan",
        #   "Südafrika",
        #   "Marokko",
        #   "Pakistan",
        #   "Philippinen",
        #   "Australien",
        #   "England",
        #   "Niederlande",
        #   "Vereinigte Staaten",
        #   "Neuseeland",
        #   "Vereinigte Arabische Emirate"
        # ),
        multiple = TRUE,
        options =  list(
          "max-options" = 10,
          "max-options-text" = "<span style='color: red;'>Maximal 10 Länder auswählen</span>"),
        selected = c("Interantionaler Durchschnitt", "Deutschland","Schweden", "Italien", "Türkei","Vereinigte Staaten" )
                       ),
    ##

    # TODO extract into own module, since this is repeated on a lot of modules


    shinyBS::bsPopover(id="ih_international_schule_gruppen", title="",
                       content = paste0("Die erste Darstellung zeigt, dass in allen teilnehmenden Ländern Kinder mit höherem sozialem Status höhere Punktzahlen in dem Kompetenztest von TIMSS erreichen. So auch in Deutschland, wo Kinder mit höherem sozialem Status 572 Punkte und Kinder mit niedrigerem sozialem Status 521 Punkte erzielen."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_international_schule_gruppen")
  )

}

#' international_schule_migration Server Functions
#'
#' @noRd
mod_international_schule_migration_server <- function(id, r){

  # logger::log_debug("start mod_international_schule_migration_server")

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # region change updates respective sub inputs, which will otherwise
    # still be the last values.
    observeEvent(input$line_l_int_schule, {
      r$line_l_int_schule <- input$line_l_int_schule
      if (input$line_l_int_schule == "TIMSS") {
        r$line_y_int_schule <- input$line_y_timss_int_schule
        r$line_f_int_schule <- input$line_f_timss_int_schule
        r$line_li_int_schule <- input$line_li_timss_int_schule
        # r$regio_int_schule  <- input$regio_int_schule
      }
      if (input$line_l_int_schule == "PISA") {
        r$line_y_int_schule <- input$line_y_pisa_int_schule
        r$line_f_int_schule <- input$line_f_pisa_int_schule
        r$line_li_int_schule <- input$line_li_pisa_int_schule
        # r$regio_int_schule_pisa <- input$regio_int_schule_pisa
      }
    })

    observeEvent(input$line_y_pisa_int_schule, {
      r$line_y_int_schule <- input$line_y_pisa_int_schule
    })

    observeEvent(input$line_f_pisa_int_schule, {
      r$line_f_int_schule <- input$line_f_pisa_int_schule
    })

    observeEvent(input$line_li_pisa_int_schule, {
      r$line_li_int_schule <- input$line_li_pisa_int_schule
    })

    # timss check should be after pisa check, since it is the default and will
    # otherwise be overwritten on initial load up
    observeEvent(input$line_y_timss_int_schule, {
      r$line_y_int_schule<- input$line_y_timss_int_schule
    })

    observeEvent(input$line_f_timss_int_schule, {
      r$line_f_int_schule <- input$line_f_timss_int_schule
    })

    observeEvent(input$line_li_timss_int_schule, {
      r$line_li_int_schule <- input$line_li_timss_int_schule
    })

    observeEvent(input$regio_int_schule, {
      r$regio_int_schule <- input$regio_int_schule
    })

    # observeEvent(input$regio_int_schule_pisa, {
    #   r$regio_int_schule_pisa <- input$regio_int_schule_pisa
    # })


  })
}

## To be copied in the UI
# mod_international_schule_migration_ui("international_schule_migration_1")

## To be copied in the server
# mod_international_schule_migration_server("international_schule_migration_1")
