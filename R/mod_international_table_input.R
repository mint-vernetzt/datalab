#' international_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_table_input_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Eine Änderung der Region setzt alle Eingestellten Filter zurück."),
    p("Eine Änderung des Landes kann zur einer Rücksetzung des Jahresfilters führen."),
    p("Sollten für bestimmte Kombinationen keine Daten vorhanden sein, liegt es in den meisten Fällen an der Jahreseinstellung"),
    p("Es können maximal 20 Filterzeilen erstellt werden."),
    fluidRow(
      column(
        width = 2,
        p("Region:"),
        shinyWidgets::pickerInput(
          inputId = ns("map_int_table_region"),
          label = NULL,
          choices = c("Europa", "OECD Länder"),
          selected = "Europa",
          multiple = FALSE
        )
      ),
      column(
        width = 4,
        p("Land:"),
        shinyWidgets::pickerInput(
          inputId = ns("map_int_table_land"),
          choices = international_zentral_get_unique_values(
            var = "land",
            filter = list(region = "Europa")),
          selected = "Deutschland",
          multiple = TRUE,
          options =  list(
            title = "Bis zu 3 Länder wählen",
            "max-options" = 2,
            "max-options-text" = "Bitte nur maximal 2 Länder auswählen"
          )
        )
      )
    ),
    fluidRow(
      column(width = 2, p("Indikator")),
      column(width = 4, p("Gruppe")),
      column(width = 4, p("Fachbereich")),
      column(width = 2, p("Jahr"))
    ),

    # add this as a baseline to start when all filters are removed
    fluidRow(id = "map_int_table_input_row_0"),
    # shiny::uiOutput(ns("all_rows")),
    create_filter_row(i_input = 1, ns = ns),

    ## end looping
    fluidRow(
      column(
        width = 12,
        actionButton(ns("int_table_insertBtn"), "Weitere Betrachtung hinzufügen"),
        actionButton(ns("int_table_runBtn"), "Betrachtungen anzeigen")
      )
    )
  )
}

#' international_table Server Functions
#'
#' @noRd
mod_international_table_input_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # n_obs <- nrow(international_zentral)
    n_obs <- 20
    # add 0 as anchor, which can not be removed
    active_rows <- reactiveVal(c(0,1))

    # initial number of rows is one
    # output$all_rows <- renderUI({
    #   lapply(1, create_filter_row, ns = ns)
    # })

    # create observeEvent for all possible delete buttons
    int_table_changes <- lapply(seq_len(n_obs), FUN = function(i_input) {
      observeEvent({input[[paste0("int_table_removeBtn_", i_input)]]}, {
        # reset i_input values, so they are not used for filtering
        shinyjs::reset(paste0("map_int_table_indikator_", i_input), asis = FALSE)
        shinyjs::reset(paste0("map_int_table_fachbereich_", i_input), asis = FALSE)
        shinyjs::reset(paste0("map_int_table_gruppe_", i_input), asis = FALSE)
        shinyjs::reset(paste0("map_int_table_year_", i_input), asis = FALSE)
        active_rows(setdiff(active_rows(), i_input))
        # remove UI element
        removeUI(selector = paste0("#map_int_table_input_row_", i_input, ""))
      })
    })

    observeEvent(input$int_table_insertBtn, {
      all_active_rows <- active_rows()
      added_row <- setdiff(seq_len(n_obs), all_active_rows)

      if (length(added_row) != 0 ) {
        # update active rows
        active_rows(c(all_active_rows, min(added_row)))
        # add new UI element
        i_input <- min(added_row)
        # for each new row a new UI element is inserted
        # the index is the smallest not used number
        # maximum number of filters is the number of rows of the input data
        insertUI(
          selector = paste0("#map_int_table_input_row_", dplyr::last(all_active_rows)),
          where = "afterEnd",
          ui = create_filter_row(i_input = i_input,
                                 ns = ns,
                                 region = input$map_int_table_region,
                                 land = input$map_int_table_land)
        )
      } else {
        # case when all possible rows are present
        shiny::showNotification(
          ui = "Es sind bereits alle möglichen Filter-Kombinationen erstellt!",
          duration = NULL,
          closeButton = TRUE,
          type = "warning")
      }
    })


    # create table with given filters
    observeEvent(input$int_table_runBtn, {

      all_active_rows <- setdiff(active_rows(), 0)
      shiny::req(all_active_rows)

      filter_list <- lapply(all_active_rows, function(i_input) {

        list(
          bereich = input[[paste0("map_int_table_indikator_", i_input)]],
          fach = input[[paste0("map_int_table_fachbereich_", i_input)]],
          gruppe = input[[paste0("map_int_table_gruppe_", i_input)]],
          jahr = input[[paste0("map_int_table_year_", i_input)]]
        )
      })

      # check for complete filter inputs
      filter_check <- sapply(filter_list, function(x) {
        any(sapply(x, function(y) {is.null(y) || y == ""}))
      })

      if (any(filter_check)) {
        shiny::showNotification(
          "Die Filtereingaben sind unvollständig",
          duration = 10,
          type = "warning"
        )
        shiny::req(FALSE)
      }

      all_filters <- lapply(filter_list, create_filter_rule)


      # update table with filters
      tmp_int_table_pre_filtered <-
        international_zentral %>%
        dplyr::filter(region == input$map_int_table_region) %>%
        dplyr::filter(land %in% input[["map_int_table_land"]])


      tmp_int_table_filtered_list <- lapply(
        X = seq_along(all_filters),
        FUN = function(i) {
          this_filter <- all_filters[[i]]
          out <- tmp_int_table_pre_filtered %>%
            dplyr::filter(eval(parse(text = this_filter))
            )

          if (nrow(out) == 0) {
            # case when data filter returned empty data
            shiny::showNotification(
              ui = paste0("Keine Daten zur ", i, ". Filterzeile gefunden."),
              duration = NULL,
              closeButton = TRUE,
              type = "warning")
          }
          return(out)
        })

      tmp_int_table_filtered <- data.table::rbindlist(tmp_int_table_filtered_list)

      if (nrow(tmp_int_table_filtered) == 0) {
        # case when data filter returned empty data
        shiny::showNotification(
          ui = "Keine Daten zu diesen Filtern gefunden. Evlt. liegt es an der Jahreseinstellung",
          duration = NULL,
          closeButton = TRUE,
          type = "warning")
        shiny::req(FALSE)
      }



      filtered_land <- unique(tmp_int_table_filtered$land)

      tmp_int_table_display <- tmp_int_table_filtered %>%
        dplyr::mutate(
          wert = paste0(round(wert_prozent,1), " %<br>",
                        dplyr::if_else(is.na(wert_absolut),
                                       "",
                                       paste0("(", wert_absolut, ")"))
                        )
          ) %>%
        # transform data into column format for countries
        dplyr::mutate(gruppe_in_fach = paste0(gruppe, " in ", fach)) %>% # hier Text in der ersten Spalte anpassen
        dplyr::select(gruppe_in_fach, land, wert, jahr, hinweis) %>%
        tidyr::pivot_wider(
          names_from = land,
          values_from = wert,
          values_fill = "-")

      tmp_int_table_display <- tmp_int_table_display %>%
        dplyr::relocate(gruppe_in_fach,
                        intersect(filtered_land,
                                  input[["map_int_table_land"]]),
                        jahr, hinweis)

      missing_land <- setdiff(input[["map_int_table_land"]], filtered_land)
      if (length(missing_land) > 0) {
        shiny::showNotification(
          ui = paste0("Es konnten in dieser Kombination keine Daten für '",
                     paste0(missing_land, collapse = "', '")
                     ,"' gefunden werden."),
          duration = NULL,
          closeButton = TRUE,
          type = "warning")
      }

      # table for png and display
      r$int_table <- tmp_int_table_display
      # table for csv download
      r$int_table_csv <- tmp_int_table_filtered

      # update source string
      # TODO hier anpassen, dass Quellen sich kombinieren, wenn mehrere Regionen
      r$int_table_source <- international_zentral %>%
        dplyr::filter(region == input$map_int_table_region) %>%
        dplyr::pull(quelle) %>%
        dplyr::first()



    })


    # observe input for dropdown changes ----
    observeEvent(input$map_int_table_region, {

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "map_int_table_land",
        choices = international_zentral_get_unique_values(
          var = "land",
          filter = list(region = input$map_int_table_region)),
        selected = NULL
      )

      all_active_rows <- setdiff(active_rows(), 0)
      lapply(all_active_rows, function(i_input) {
        # shinyWidgets::updatePickerInput(
        #   session = session,
        #   inputId = paste0("map_int_table_indikator_", i_input),
        #   choices = international_zentral_get_unique_values(
        #     var = "bereich",
        #     filter = list(region = input$map_int_table_region)),
        #   selected = NULL
        # )

        shinyjs::reset(paste0("map_int_table_indikator_", i_input), asis = FALSE)
        shinyjs::reset(paste0("map_int_table_fachbereich_", i_input), asis = FALSE)
        shinyjs::reset(paste0("map_int_table_gruppe_", i_input), asis = FALSE)
        shinyjs::reset(paste0("map_int_table_year_", i_input), asis = FALSE)


        # remove UI element
        removeUI(
          selector = paste0("#map_int_table_input_row_", i_input, "")
        )

      })

      # set rows back to one and recreate it
      active_rows(c(0, 1))
      insertUI(
        selector = "#map_int_table_input_row_0",
        where = "afterEnd",
        ui = create_filter_row(i_input = 1,
                               ns = ns,
                               region = input$map_int_table_region,
                               land = input$map_int_table_land)
      )

    })

    # adjust dropdowns depending on the 'land' inpput
    observeEvent(input$map_int_table_land, {
      all_active_rows <- setdiff(active_rows(), 0)
      lapply(all_active_rows, function(i_input) {
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = paste0("map_int_table_year_", i_input),
          choices = international_zentral_get_unique_values(
            var = "jahr",
            filter = list(land = input$map_int_table_land,
                          region = input$map_int_table_region)),
          selected = input[[paste0("map_int_table_year_", i_input)]]
        )
      })
    })



    # adjust dropdowns depending on the 'indikator' inpput
    int_table_group_btn <- lapply(seq_len(n_obs), FUN = function(i_input) {

      ## adjust dropdown for 'gruppe and 'fach'
      observeEvent({input[[paste0("map_int_table_indikator_", i_input)]]}, {
        shiny::req(input[[paste0("map_int_table_indikator_", i_input)]])

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = paste0("map_int_table_gruppe_", i_input),
          choices = international_zentral_get_unique_values(
            var = "gruppe",
            filter = list(
              region = input$map_int_table_region,
              bereich = input[[paste0("map_int_table_indikator_", i_input)]]
            )
          ),
          selected = NULL
        )

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = paste0("map_int_table_fachbereich_", i_input),
          choices = international_zentral_get_unique_values(
            var = "fach",
            filter = list(
              region = input$map_int_table_region,
              bereich = input[[paste0("map_int_table_indikator_", i_input)]]
            )
          ),
          selected = NULL
        )
      })
    })


  })
}

## To be copied in the UI
# mod_international_table_input_ui("international_table_input_1")

## To be copied in the server
# mod_international_table_input_server("international_table_input_1")


