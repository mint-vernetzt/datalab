#' A function to plot a graph.
#'
#' @description A function to create a plots inside the tab "FACHKRAFT".
#'
#' @return The return value is a plot
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd


plot_fachkraft_epa_item <- function(r) {
  logger::log_debug("plot_fachkraft_epa_item")
  #timerange <- 2022; fach <- c("Landtechnik", "Bau- und Gebäudetechnik"); bf_label <- "Spezialist*innen"
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
      dplyr::mutate(mint_zuordnung = "MINT gesamt") %>%
      rbind(plot_data_raw)
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

  used_colors <- group_col_dt %>%
    dplyr::filter(epa_kat %in% (expanded_dt %>%
                                  dplyr::filter(mint_zuordnung == fach[1]) %>%
                                  dplyr::pull(epa_kat) %>%
                                  unique())) %>%
    dplyr::pull(group_col)

  # titel zusammenbauen
  level <- dplyr::case_when(
    bf_label == "Gesamt" ~ " ",
    bf_label == "Fachkräfte" ~ " von Beschäftigten in Ausbildungsberufen",
    bf_label == "Spezialist*innen" ~ " von Beschäftigten in Meister-/Technikerstellen o.ä.",
    bf_label == "Expert*innen" ~ " von Beschäftigten in Akademikerberufen",
  )
  fach_1 <- dplyr::case_when(
    fach[1] == "MINT gesamt" ~ "MINT",
    fach[1] == "Gesamt" ~ "allen Berufen",
    fach[1] == "Nicht MINT" ~ "allen Berufen außer MINT",
    T ~ fach[1]
  )
  fach_2 <- dplyr::case_when(
    fach[2] == "MINT gesamt" ~ "MINT",
    fach[2] == "Gesamt" ~ "allen Berufen",
    fach[2] == "Nicht MINT" ~ "allen Berufen außer MINT",
    T ~ fach[2]
  )
  titel_1 <- paste0("Engpassrisiko von Berufen in ", fach_1, level, " (", timerange, ")")
  titel_2 <- paste0("Engpassrisiko in ", fach_2, level, " (", timerange, ")")


  plot_left <- highcharter::hchart(
    object = expanded_dt %>% dplyr::filter(mint_zuordnung == fach[1]),
    type = "heatmap",
    mapping = highcharter::hcaes(x = XX,
                                 y = YY,
                                 value = value,
                                 color = group_col,
                                 group = epa_kat)) %>%
    highcharter::hc_colorAxis(
      stops = highcharter::color_stops(colors = group_col_dt$group_col),
      showInLegend = FALSE) %>%
    #hc_legend(enabled = FALSE) %>% # Remove color legend
    highcharter::hc_colors(used_colors) %>%
    highcharter::hc_tooltip(
      # headerFormat = '{point.group}',
      pointFormat = 'Anteil: {point.value} % <br/> Anzahl betroffener Berufe: {point.beruf_num}'
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
      text = titel_1,
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
    used_colors <- group_col_dt %>%
      dplyr::filter(epa_kat %in% (expanded_dt %>%
                                    dplyr::filter(mint_zuordnung == fach[2]) %>%
                                    dplyr::pull(epa_kat) %>%
                                    unique())) %>%
      dplyr::pull(group_col)

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
      highcharter::hc_colors(used_colors) %>%
      highcharter::hc_tooltip(
        # headerFormat = '{point.group}',
        pointFormat = 'Anteil: {point.value} % <br/> Anzahl betroffener Berufe: {point.beruf_num}'
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
        text = titel_2,
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


  # out <- highcharter::hw_grid(
  #   plot_left,
  #   plot_right,
  #   ncol = 2)
  out <- list(plot_left, plot_right)

  return(out)

}

plot_fachkraft_mint_item  <- function(r) {
  logger::log_debug("plot_fachkraft_mint_item")
  #timerange <- 2022; bf_label <- "Spezialist*innen"
  #timerange <- 2020; bf_label <- "Gesamt"
  #timerange <- 2020; bf_label <- "Gesamt"
  timerange <- r$map_y_fachkraft_arbeit_mint
  bf_label <- r$map_bl_fachkraft_arbeit_mint

  if (bf_label == "Gesamt") {
    bf <- fachkraft_ui_berufslevel()
  } else {
    bf <- bf_label
  }



  plot_data_raw <- arbeitsmarkt_epa_detail %>%
    dplyr::filter(jahr == timerange &
                    indikator == "Engpassindikator" &
                    anforderung %in% bf) %>%
    dplyr::mutate(mint_zuordnung = dplyr::if_else(
      !mint_zuordnung %in% c("Nicht MINT", "Gesamt"),
      "MINT gesamt",
      mint_zuordnung)) %>%
    dplyr::filter(mint_zuordnung %in% c("Nicht MINT", "MINT gesamt")) %>%
    dplyr::group_by(mint_zuordnung, epa_kat) %>%
    dplyr::summarise(berufe = dplyr::n()) %>%
    dplyr::mutate(mint_epa_kat = paste0(mint_zuordnung, " - ", epa_kat)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(percent_total = round_preserve_sum(berufe / sum(berufe, na.rm = TRUE) * 100)) %>%
    dplyr::group_by(epa_kat) %>%
    dplyr::mutate(percent_epa = round_preserve_sum(berufe / sum(berufe, na.rm = TRUE) * 100)) %>%
    dplyr::ungroup()



  # enthält den Text für den plot
  epa_kat_levels <- c("MINT gesamt - Engpassberuf",
                      "Nicht MINT - Engpassberuf",
                      "MINT gesamt - Anzeichen eines Engpassberufs",
                      "Nicht MINT - Anzeichen eines Engpassberufs",
                      "MINT gesamt - kein Engpassberuf",
                      "Nicht MINT - kein Engpassberuf")
  group_col_dt <- data.frame(
    mint_epa_kat = factor(x = epa_kat_levels,
                          levels = epa_kat_levels),
    epa_group_order = c(1:6),
    group_col = c("#EE7775","#f5adac",
                  "#Fcc433", "#fdd670",
                  "#00a87a", "#66cbaf")
  )

  plot_data <- plot_data_raw %>%
    dplyr::right_join(group_col_dt, by = "mint_epa_kat") %>%
    dplyr::arrange(epa_group_order) %>%
    dplyr::group_by(mint_epa_kat) %>%
    dplyr::mutate(berufe = dplyr::if_else(is.na(berufe), 0, berufe),
                  percent_total = dplyr::if_else(is.na(percent_total), 0, percent_total),
                  percent_epa = dplyr::if_else(is.na(percent_epa), 0, percent_epa)) %>%
    dplyr::ungroup()

  # used_colors <- group_col_dt %>%
  #   dplyr::filter(mint_epa_kat %in% (expanded_dt %>%
  #                                 dplyr::filter(mint_zuordnung == fach[1]) %>%
  #                                 dplyr::pull(epa_kat) %>%
  #                                 unique())) %>%
  #   dplyr::pull(group_col)

  plot <- highcharter::hchart(
    plot_data,
    "item",
    highcharter::hcaes(
      name = mint_epa_kat,
      y = berufe,
      # label = group,
      color = group_col),
    # name = "group",
    showInLegend = TRUE
  ) %>%
    # highcharter::hc_caption(
    #   text = paste0(plot_legend_data$legend_text, collapse = "<br>"),
    #   useHTML = TRUE
    # ) %>%
    highcharter::hc_tooltip(
      pointFormat = paste0(
        " {point.berufe} Berufe<br>",
        " {point.percent_epa}% von {point.epa_kat}<br>",
        " {point.percent_total}% aller Berufe")) %>%
    highcharter::hc_title(
      text = paste0("Anteil von MINT-Berufen in der Verteilung des Engpassrisikos im Berufslevel ",
                    bf_label, " (", timerange, ")"),
      margin = 10,
      align = "center",
      style = list(color = "black",
                   useHTML = TRUE,
                   fontFamily = "SourceSans3-Regular",
                   fontSize = "20px")
    ) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular")
    ) %>%
    # highcharter::hc_size(600, 450) %>%
    highcharter::hc_credits(enabled = FALSE) %>%
    # highcharter::hc_legend(layout = "horizontal", floating = FALSE,
    #                        verticalAlign = "bottom")
    highcharter::hc_legend(enabled = FALSE)


  # leeren Plot mit individuell angepasster Legende erstellen

  # Erstelle eine Liste von Kategorien für die Legende
  categories <- c("MINT gesamt - Engpassberuf",
                  "MINT gesamt - Anzeichen eines Engpassberufs",
                  "MINT gesamt - kein Engpassberuf",
                  "Nicht MINT - Engpassberuf",
                  "Nicht MINT - Anzeichen eines Engpassberufs",
                  "Nicht MINT - kein Engpassberuf")

  # Farben für jede Kategorie
  colors <- c("#EE7775",  "#Fcc433", "#00a87a", "#f5adac", "#fdd670", "#66cbaf")

  # Erstelle einen 'leeren' Plot mit einer Serie für jede Kategorie
  legend_plot <- highcharter::highchart() %>%
    highcharter::hc_chart(type = 'bar', height = 100) %>% # Reduziere die Höhe
    highcharter::hc_add_series(name = categories[1], data = list(NULL), color = colors[1]) %>%
    highcharter::hc_add_series(name = categories[2], data = list(NULL), color = colors[2]) %>%
    highcharter::hc_add_series(name = categories[3], data = list(NULL), color = colors[3]) %>%
    highcharter::hc_add_series(name = categories[4], data = list(NULL), color = colors[4]) %>%
    highcharter::hc_add_series(name = categories[5], data = list(NULL), color = colors[5]) %>%
    highcharter::hc_add_series(name = categories[6], data = list(NULL), color = colors[6]) %>%
    highcharter::hc_legend(enabled = TRUE) %>%
    highcharter::hc_title(text = NULL) %>%
    highcharter::hc_subtitle(text = NULL) %>%
    highcharter::hc_xAxis(visible = FALSE) %>%
    highcharter::hc_yAxis(visible = FALSE) %>%
    highcharter::hc_credits(enabled = FALSE) %>%
    highcharter::hc_chart(margin = 0, spacing = c(0, 0, 0, 0)) # Reduziere Margen und Abstand

  plot_list <- list(plot, legend_plot)

  out <- highcharter::hw_grid(
    plot_list,
    ncol=1)

  return(out)
}

plot_fachkraft_detail_item  <- function(r) {
  logger::log_debug("plot_fachkraft_detail_item")
  #timerange <- 2022; bf_label <- "Spezialist*innen"; this_beruf <-"Gesamt"
  # timerange <- 2022; bf_label <- "Spezialist*innen"; this_beruf <-"Gesamt"

  timerange <- r$map_y_fachkraft_arbeit_detail
  bf_label <- r$map_bl_fachkraft_arbeit_detail
  this_beruf <- r$map_b_fachkraft_arbeit_detail

  plot_solidgauge_data <- arbeitsmarkt_epa_detail %>%
    dplyr::filter(jahr == timerange &
                    indikator == "Engpassindikator" &
                    anforderung == bf_label &
                    beruf %in% this_beruf)

  used_kategories <- switch(
    EXPR = plot_solidgauge_data$epa_kat[1],
    "Anzeichen eines Engpassberufs" = c("Engpassanalyse", "Risikoanalyse"),
    "Engpassberuf" = c("Engpassanalyse"),
    "kein Engpassberuf" = c("Engpassanalyse"),
  )
  plot_bar_data <- arbeitsmarkt_epa_detail %>%
    dplyr::filter(jahr == timerange &
                    #indikator == "Engpassindikator" &
                    anforderung == bf_label &
                    indikator != "Engpassindikator" &
                    beruf %in% this_beruf &
                    kategorie %in% used_kategories &
                    !is.na(wert)) %>%
    dplyr::select(indikator, kategorie, wert) %>%
    dplyr::mutate(wert = round(wert, 2))

  # color change on 0.01. level, since data labels are also rounded to 2 decimal places
  col_stops <- data.frame(
    q = c(0, 1.49, 1.50, 2, 2.01),
    c = c("#35BD97", "#35BD97", "#FBBF24", "#FBBF24", "#EE7775"),
    stringsAsFactors = FALSE
  )

  color_idx <- sapply(plot_bar_data$wert, function(x) {
    max(which((col_stops$q)<= x))
  })
  plot_bar_data$bar_color <- col_stops$c[color_idx]
  # divide by three (the maximum) to get percentage change values for the gauge plot
  col_stops$q <- col_stops$q / 3

  plot_left <- highcharter::highchart() %>%
    highcharter::hc_chart(type = "solidgauge") %>%
    highcharter::hc_pane(
      startAngle = -90,
      endAngle = 90,
      background = list(
        outerRadius = '100%',
        innerRadius = '60%',
        shape = "arc"
      )
    ) %>%
    highcharter::hc_tooltip(enabled = FALSE) %>%
    highcharter::hc_yAxis(
      stops = highcharter::list_parse2(col_stops),
      lineWidth = 0,
      minorTickWidth = NULL,
      tickWidth = 0,
      tickAmount = 4,
      min = 0,
      max = 3,
      labels = list(
        y = 26,
        style = list(fontSize = "22px"),
        # only show min and max values in label
        formatter = highcharter::JS(
          "function () {
            if (this.value === this.axis.min || this.value === this.axis.max) {
              return this.value;
            } else {
              return null;
            }
          }"))
    ) %>%
    highcharter::hc_add_series(
      data = round(plot_solidgauge_data$wert, 2),
      dataLabels = list(
        y = -50,
        borderWidth = 0,
        useHTML = TRUE,
        style = list(
          fontFamily = "SourceSans3-Regular",
          fontSize = "20px")
      )
    ) %>%
    highcharter::hc_title(
      text = paste0("Engpassindikator für den Beruf ", this_beruf,
                    " auf dem ", bf_label, "-Level (", timerange, ")"),
      margin = 10,
      align = "center",
      style = list(color = "black",
                   useHTML = TRUE,
                   fontFamily = "SourceSans3-Regular",
                   fontSize = "20px")
    )


  # count "Engpassanalyse" to add line afterward
  sep_line_risiko <- sum(plot_bar_data$kategorie == "Engpassanalyse") - 0.4
  sep_line_engpass <-  -0.4

  plot_right <- highcharter::hchart(
    object = plot_bar_data,
    type =  'bar',
    name = "Engpassanalyse",
    mapping = highcharter::hcaes(
      x = indikator,
      y = wert,
      color = bar_color)) %>%
    highcharter::hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE, format = "{point.wert:.2f}")
      )) %>%
    highcharter::hc_tooltip(pointFormat = "Wert: {point.wert:.2f}") %>%
    highcharter::hc_yAxis(title = list(text = ""),
                          min = 0,
                          max = 3,
                          tickInterval = 1) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_title(
      text = paste0("Einzelne Indikatoren der Engpassanalyse (gesamt ",
                    round(plot_solidgauge_data$wert, 2),
                    ") für Beruf ", this_beruf, " (", timerange, ")"),
      margin = 45,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE)

  if ("Risikoanalyse" %in% plot_bar_data$kategorie) {
    plot_right <- plot_right %>%
      highcharter::hc_xAxis(
        plotLines = list(
          list(
            color = 'grey', # Color of the line
            width = 1, # Width of the line
            value = sep_line_engpass, # Position of the line (between Canada and Germany)
            label = list(
              text = 'Engpassanalyse', # Text of the label
              align = 'right', # Position of the label
              x = 5, # Horizontal position offset for the label
              style = list(
                color = 'grey'#,
                #fontWeight = 'bold'
              )
            ),
            zIndex = 5 # Ensure the plot line is above the grid lines
          ),
          list(
            color = 'grey', # Color of the line
            width = 1, # Width of the line
            value = sep_line_risiko, # Position of the line (between Canada and Germany)
            label = list(
              text = 'Riskoindikatoren', # Text of the label
              align = 'right', # Position of the label
              x = 5, # Horizontal position offset for the label
              style = list(
                color = 'grey'#,
                #fontWeight = 'bold'
              )
            ),
            zIndex = 5 # Ensure the plot line is above the grid lines
          )
        ),
        title = list(text = ""))

  }




  # out <- highcharter::hw_grid(
  #   plot_left,
  #   plot_right,
  #   ncol = 2)
  out <- list(plot_left, plot_right)

  return(out)
}

plot_fachkraft_bar_vakanz  <- function(r) {
  logger::log_debug("plot_fachkraft_bar_vakanz")
  #this_indikator <- "Abgeschlossene Vakanzzeit"; timerange <- 2021; bf_label <- "Spezialist*innen"; this_region <-"Deutschland"
  #this_indikator <- "Arbeitslosen-Stellen-Relation"; timerange <- 2022; bf_label <- "Gesamt"; this_region <-"Deutschland"

  this_indikator <- r$map_ind_fachkraft_arbeit_bar
  timerange <- r$map_y_fachkraft_arbeit_bar
  this_region <- r$map_reg_fachkraft_arbeit_bar
  bf_label <- r$map_bl_fachkraft_arbeit_bar

  berufe_order <- c("Insgesamt", "Keine MINT-Berufe", "MINT-Berufe")

  plot_data <- arbeitsmarkt_fachkraefte %>%
    dplyr::filter(jahr == timerange &
                    indikator == this_indikator &
                    anforderung == bf_label &
                    region == this_region) %>%
    dplyr::group_by(fachbereich) %>%
    dplyr::summarise(wert = round(mean(wert, na.rm = TRUE), 2)) %>%
    na.omit() %>%
    dplyr::mutate(
      group_color = dplyr::if_else(
        fachbereich %in% berufe_order, "#B16FAB", "#D0A9CD"),
      fachbereich = factor(
        x = fachbereich,
        levels = c(berufe_order, sort(setdiff(fachbereich, berufe_order)))
      )
    ) %>%
    dplyr::arrange(fachbereich)

  out <- highcharter::hchart(
    object = plot_data,
      type = "bar",
      mapping = highcharter::hcaes(x = fachbereich, y = wert, color = group_color)
      ) %>%
    highcharter::hc_title(
      text = paste0("Anteil von MINT-Berufen in der Verteilung der ", this_indikator,
                    " auf dem ", bf_label, "-Level (", timerange, ")"),
      margin = 10,
      align = "center",
      style = list(color = "black",
                   useHTML = TRUE,
                   fontFamily = "SourceSans3-Regular",
                   fontSize = "20px")
    ) %>%
    highcharter::hc_tooltip(
      pointFormat = 'Anteil: {point.wert}%'
    ) %>%
    highcharter::hc_yAxis(title = list(text = "")) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%

  return(out)
}
