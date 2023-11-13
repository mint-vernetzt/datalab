#' A function to plot a graph.
#'
#' @description A function to create a plots inside the tab "FACHKRAFT".
#'
#' @return The return value is a plot
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd


plot_fachkraft_epa_item <- function(r) {
  logger::log_debug("plot_fachkraft_epa_item")
  #timerange <- 2020; fach <- c("Nicht MINT", "Gesamt"); bf_label <- "Fachkräfte"
  #timerange <- 2020; fach <- c("MINT gesamt", "Informatik"); bf_label <- "Gesamt"
  #timerange <- 2020; fach <- c("Alle Berufe"); bf_label <- "Gesamt"

  timerange <- r$map_y_fachkraft_arbeit_epa
  fach <- r$map_f_fachkraft_arbeit_epa
  bf_label <- r$map_bl_fachkraft_arbeit_epa

  if (bf_label == "Gesamt") {
    bf <- fachkraft_ui_berufslevel()
  } else {
    bf <- bf_label
  }


  plot_data_raw <- arbeitsmarkt_epa_detail %>%
    dplyr::filter(jahr == timerange &
                    indikator == "Engpassindikator" &
                    anforderung %in% bf)

  if ("Alle Berufe" %in% fach) {
    fach[fach == "Alle Berufe"] <- "Gesamt"
  }
  if ("MINT gesamt" %in% fach) {
    plot_data_raw <- plot_data_raw %>%
      dplyr::filter(!mint_zuordnung %in% c("Nicht MINT", "Gesmat")) %>%
      # TODO wonach soll hier aggregiert werden?
      # TODO ist indikator der richtige Filter?
      # TODO was ist mit berufsgruppe?
      dplyr::mutate(mint_zuordnung = "MINT gesamt") %>%
      rbind(arbeitsmarkt_epa_detail)
  }


  # enthält den Text für den plot
  epa_kat_levels <- c("Engpassberuf",
                      "Anzeichen eines Engpassberufs",
                      "kein Engpassberuf")
  group_col_dt <- data.frame(
    epa_kat = factor(x = epa_kat_levels,
                     levels = epa_kat_levels),
    epa_group_order = c(1:3),
    group_text = c("Text A",
                   "Text B",
                   "Text C"),
    group_col = c("#EE7775", "#FBBF24", "#35BD97")
  )

  plot_data <- plot_data_raw %>%
    dplyr::filter(mint_zuordnung %in% fach &
                    !is.na(epa_kat)) %>%
    dplyr::group_by(epa_kat, mint_zuordnung)  %>%
    dplyr::summarise(beruf_num = dplyr::n()) %>%
    dplyr::group_by(mint_zuordnung)  %>%
    dplyr::mutate(value = round_preserve_sum(beruf_num / sum(beruf_num) * 100)) %>%
    dplyr::left_join(group_col_dt, by = "epa_kat") %>%
    dplyr::arrange(epa_group_order)

  # expand data for heatmap
  expanded_dt <- plot_data[rep(row.names(plot_data), plot_data$value),] %>%
    dplyr::arrange(mint_zuordnung, epa_group_order) %>%
    # the order of XX and YY determines if the plot is shown right to left or bottom to top
    dplyr::mutate(XX = rep(c(1:10), each = 10),
                  YY = rep(c(1:10), times = 10),
                  epa_kat = factor(x = epa_kat,
                                   levels = epa_kat_levels))

  plot_left <- highcharter::hchart(
    object = expanded_dt %>% dplyr::filter(mint_zuordnung == fach[1]),
    type = "heatmap",
    mapping = highcharter::hcaes(x = XX,
                                 y = YY,
                                 value = value,
                                 color = group_col,
                                 group = epa_kat)) %>%
    highcharter::hc_colorAxis(stops = highcharter::color_stops(colors = group_col_dt$group_col),
                              showInLegend = FALSE) %>%
    #hc_legend(enabled = FALSE) %>% # Remove color legend
    highcharter::hc_colors(group_col_dt$group_col) %>%
    highcharter::hc_tooltip(
      # headerFormat = '{point.group}',
      pointFormat = 'Berufe: {point.beruf_num}<br/>Anteil: {point.value}%'
    ) %>%
    highcharter::hc_xAxis(visible = FALSE) %>%
    highcharter::hc_yAxis(visible = FALSE) %>%
    highcharter::hc_plotOptions(
      series = list(
        borderColor = "white",
        borderWidth = 1
      )
    ) %>%
    highcharter::hc_title(
      text = paste0("Verteilung von ", fach[1], "-Berufen nach Engpassrisikos",
                    " mit Berufslevel ", bf_label, " in ", timerange),
      margin = 10,
      align = "center",
      style = list(color = "black",
                   useHTML = TRUE,
                   fontFamily = "SourceSans3-Regular",
                   fontSize = "20px")
    ) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular")
    )

  if (length(fach) == 2) {
    plot_right <- highcharter::hchart(
      object = expanded_dt %>% dplyr::filter(mint_zuordnung == fach[2]),
      type = "heatmap",
      mapping = highcharter::hcaes(x = XX,
                                   y = YY,
                                   value = value,
                                   color = group_col,
                                   group = epa_kat)) %>%
      highcharter::hc_colorAxis(stops = highcharter::color_stops(colors = group_col_dt$group_col),
                                showInLegend = FALSE) %>%
      #hc_legend(enabled = FALSE) %>% # Remove color legend
      highcharter::hc_colors(group_col_dt$group_col) %>%
      highcharter::hc_tooltip(
        # headerFormat = '{point.group}',
        pointFormat = 'Berufe: {point.beruf_num}<br/>Anteil: {point.value}%'
      ) %>%
      highcharter::hc_xAxis(visible = FALSE) %>%
      highcharter::hc_yAxis(visible = FALSE) %>%
      highcharter::hc_plotOptions(
        series = list(
          borderColor = "white",
          borderWidth = 1
        )
      ) %>%
      highcharter::hc_title(
        text = paste0("Verteilung von ", fach[2], "-Berufen nach Engpassrisikos",
                      " mit Berufslevel ", bf_label, " in ", timerange),
        margin = 10,
        align = "center",
        style = list(color = "black",
                     useHTML = TRUE,
                     fontFamily = "SourceSans3-Regular",
                     fontSize = "20px")
      ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular")
      )
  } else {
    plot_right <- NULL
  }


  out <-   highcharter::hw_grid(
    plot_left,
    plot_right,
    ncol = 2)

  return(out)

}

plot_fachkraft_mint_item  <- function(r) {
  logger::log_debug("plot_fachkraft_mint_item")
  hc <- highchart() %>%
    hc_chart(type = "bar") %>%
    hc_add_series(data = c(1, 2, 3, 4, 5))

  hc
}

plot_fachkraft_detail_item  <- function(r) {
  logger::log_debug("plot_fachkraft_detail_item")
  hc <- highchart() %>%
    hc_chart(type = "bar") %>%
    hc_add_series(data = rev(c(1, 2, 3, 4, 5)))

  hc
}
