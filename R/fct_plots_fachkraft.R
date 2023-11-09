#' A function to plot a graph.
#'
#' @description A function to create a plots inside the tab "FACHKRAFT".
#'
#' @return The return value is a plot
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd


plot_fachkraft_epa_item <- function(r) {
  logger::log_debug("plot_fachkraft_epa_item")
browser()
  #timerange <- 2020; fach <- c("Nicht MINT", "Gesamt"); bf <- "Fachkräfte"
  #timerange <- 2020; fach <- c("MINT gesamt", "Informatik"); bf <- "Gesamt"

  timerange <- r$map_y_fachkraft_arbeit_epa
  fach <- r$map_f_fachkraft_arbeit_epa
  bf <- r$map_bl_fachkraft_arbeit_epa


  plot_data_raw <- arbeitsmarkt_epa_detail %>%
    dplyr::filter(jahr == timerange &
                    indikator == "Engpassindikator" &
                    anforderung == bf)

  if ("Alle Berufe" %in% fach) {
    fach[fach_m == "Alle Berufe"] <- "Gesamt"
  }
  if ("Mint gesamt" %in% fach) {
    plot_data_raw <- plot_data_raw %>%
      dplyr::filter(!mint_zuordnung %in% c("Nicht MINT", "Gesmat")) %>%
      # TODO wonach soll hier aggregiert werden?
      # TODO ist indikator der richtige Filter?
      # TODO was ist mit berufsgruppe?
      dplyr::mutate(mint_zuordnung = "MINT gesamt") %>%
      rbind(arbeitsmarkt_epa_detail)
  }


  # enthält den Text für den plot
  group_col_dt <- data.frame(
    epa_kat = c("Engpassberuf",
                "Anzeichen eines Engpassberufs",
                "kein Engpassberuf"),
    group_text = c("Text A",
                   "Text B",
                   "Text C"),
    group_col = c("#EE7775", "#FBBF24", "#35BD97")
  )

  plot_data <- plot_data_raw %>%
    dplyr::filter(mint_zuordnung %in% fach) %>%
    dplyr::group_by(epa_kat, mint_zuordnung)  %>%
    dplyr::summarise(beruf_num = dplyr::n()) %>%
    dplyr::group_by(mint_zuordnung)  %>%
    dplyr::mutate(value = round(beruf_num / sum(beruf_num) * 100)) %>%
    dplyr::left_join(group_col_dt, by = "epa_kat")




  plot_left <- highcharter::hchart(
    plot_data %>% dplyr::filter(mint_zuordnung == fach[1]),
    "item",
    highcharter::hcaes(
      name = epa_kat,
      y = value,
      label = epa_kat,
      color = group_col),
    name = "group",
    showInLegend = TRUE,
    size = "100%",
    rows = 10
  ) %>%
    # highcharter::hc_caption(
    #   text = paste0(plot_legend_data$legend_text, collapse = "<br>"),
    #   useHTML = TRUE
    # ) %>%
    highcharter::hc_tooltip(
      pointFormat = paste0("Berufe: {point.beruf_num}")) %>%
    highcharter::hc_title(
      text = paste0("Verteilung von ", fach[1], "-Berufen nach Engpassrisikos",
                    " mit Berufslevel ", bf, " in ", timerange),
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
      plot_data %>% dplyr::filter(mint_zuordnung == fach[2]),
      "item",
      highcharter::hcaes(
        name = epa_kat,
        y = value,
        label = epa_kat,
        color = group_col),
      name = "group",
      showInLegend = TRUE,
      size = "100%",
      rows = 10
    ) %>%
      # highcharter::hc_caption(
      #   text = paste0(plot_legend_data$legend_text, collapse = "<br>"),
      #   useHTML = TRUE
      # ) %>%
      highcharter::hc_tooltip(
        pointFormat = paste0("Berufe: {point.beruf_num}")) %>%
      highcharter::hc_title(
        text = paste0("Verteilung von ", fach[2], "-Berufen nach Engpassrisikos",
                      " mit Berufslevel ", bf, " in ", timerange),
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
