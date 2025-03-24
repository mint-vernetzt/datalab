
argument_verlauf <- function(r){

  # load UI inputs from reactive value
  t <- 2017:2023
  absolut_selector <- "Anzahl"
  regio <- r$region_argumentationshilfe

  query_df <- glue::glue_sql("
  SELECT bereich, indikator, fachbereich, jahr, wert
  FROM zentral
  WHERE jahr IN ({t*})
    AND region = {regio}
    AND geschlecht = 'Gesamt'
    AND fachbereich = 'MINT'
", .con = con)

  df <- DBI::dbGetQuery(con, query_df)

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    df <- df[with(df, order(fachbereich, jahr, decreasing = FALSE)), ]

    #Trennpunkte für lange Zahlen ergänzen
    df$wert_besr <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

    # Ordnen der Legende
    sorted_indicators <- df %>%
      dplyr::group_by(indikator) %>%
      dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
      dplyr::arrange(desc(m_value)) %>%
      dplyr::pull(indikator)

    df$indikator <- factor(df$indikator, levels = sorted_indicators)

    titel = paste0("Personen in MINT nach Bildungsbereichen in ", regio)

    tooltip <- paste("Anteil MINT <br> Indikator: {point.indikator} <br> Anzahl: {point.wert_besr}")

    # plot

    format <- "{value:, f}"
    color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24")
    out <- linebuilder(df, titel, x = "jahr", y="wert", group = "indikator", tooltip = tooltip, format, color)

  return (out)

}

argument_fachkraft <- function(r){

  timerange <- 2023
  fach <- c("MINT gesamt", "Nicht MINT")
  bf <- fachkraft_ui_berufslevel()
  regio <- r$region_argumentationshilfe

  #für Deutschland:

  if(regio == "Deutschland"){

    df_query <- glue::glue_sql("
  SELECT *
  FROM arbeitsmarkt_epa_detail
  WHERE jahr = {timerange}
  AND indikator = 'Engpassindikator'
  AND anforderung IN ({bf*})
                               ", .con = con)

    plot_data_raw <- DBI::dbGetQuery(con, df_query)

    if ("MINT gesamt" %in% fach) {
      plot_data_raw <- plot_data_raw %>%
        dplyr::filter(!mint_zuordnung %in% c("Nicht MINT", "Gesamt")) %>%
        dplyr::mutate(mint_zuordnung = "MINT gesamt") %>%
        rbind(plot_data_raw)
    }

    # enthält den Text für den plot
    epa_kat_levels <- c("Engpassberuf",
                        "Anzeichen eines Engpassberufs",
                        "Kein Engpassberuf")
    group_col_dt <- data.frame(
      epa_kat = factor(x = epa_kat_levels,
                       levels = epa_kat_levels),
      epa_group_order = c(1:3),
      group_text = c("Text A",
                     "Text B",
                     "Text C"),
      group_col = c("#EE7775", "#FBBF24", "#35BD97")
    )

    # Aggregate rausfiltern
    plot_data_raw <- subset(plot_data_raw, !(plot_data_raw$beruf %in%
                                               c("Gesamt",
                                                 "MINT",
                                                 "Informatik",
                                                 "Landtechnik",
                                                 "Produktionstechnik",
                                                 "Bau- und Gebäudetechnik",
                                                 "Mathematik, Naturwissenschaften",
                                                 "Verkehrs-, Sicherheits- und Veranstaltungstechnik",
                                                 "Gesundheitstechnik",
                                                 "Nicht MINT"
                                               ))
    )


    plot_data <- plot_data_raw %>%
      dplyr::filter(mint_zuordnung %in% fach &
                      !is.na(epa_kat)) %>%
      dplyr::group_by(epa_kat, mint_zuordnung)  %>%
      dplyr::summarise(beruf_num = dplyr::n()) %>%
      dplyr::group_by(mint_zuordnung)  %>%
      dplyr::mutate(value = round_preserve_sum(beruf_num / sum(beruf_num) * 100,0)) %>%
      dplyr::left_join(group_col_dt, by = "epa_kat") %>%
      dplyr::arrange(epa_group_order)

    # expand data for heatmap
    expanded_dt <- plot_data[rep(row.names(plot_data), plot_data$value),] %>%
      dplyr::arrange(mint_zuordnung, epa_group_order) %>%
      #
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

    fach_1 <- dplyr::case_when(
      fach[1] == "MINT gesamt" ~ "MINT",
      fach[1] == "Gesamt" ~ "allen Berufen",
      fach[1] == "Nicht MINT" ~ "allen Berufen außer MINT",
      T ~ fach[1]
    )

    titel_1 <- paste0("Engpassrisiko von Berufen in ", fach_1," (", timerange, ")")

    # Entfernen aller Zeilen, bei denen group_col NA ist
    expanded_dt <- expanded_dt[!is.na(expanded_dt$group_col), ]

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
      highcharter::hc_colors(used_colors) %>%
      highcharter::hc_tooltip(
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
                     fontFamily = "Calibri Regular",
                     fontSize = "20px")
      ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular")
      ) %>%
      highcharter::hc_size(380, 480) %>%
      highcharter::hc_exporting(enabled = TRUE,
                                buttons = list(
                                  contextButton = list(
                                    menuItems = list("downloadPNG", "downloadCSV")
                                  )
                                )
      )

    fach_2 <- dplyr::case_when(
      fach[2] == "MINT gesamt" ~ "MINT",
      fach[2] == "Gesamt" ~ "allen Berufen",
      fach[2] == "Nicht MINT" ~ "allen Berufen außer MINT",
      T ~ fach[2]
    )

    titel_2 <- paste0("Engpassrisiko in ", fach_2," (", timerange, ")")

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
      highcharter::hc_colors(used_colors) %>%
      highcharter::hc_tooltip(
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
                     fontFamily = "Calibri Regular",
                     fontSize = "20px")
      ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular")
      )%>%
      highcharter::hc_size(380, 480) %>%
      highcharter::hc_exporting(enabled = TRUE,
                                buttons = list(
                                  contextButton = list(
                                    menuItems = list("downloadPNG", "downloadCSV")
                                  )
                                )
      )


    out <- highcharter::hw_grid(
      plot_left, plot_right,
      ncol = 2)

    return(out)

  }

  #für Bundesländer
  else if(regio != "Deutschland"){

   regio <- dplyr::case_when(
     regio == "Brandenburg" | regio == "Berlin" ~ "Brandenburg / Berlin",
     regio == "Niedersachsen" | regio == "Bremen" ~ "Niedersachsen / Bremen",
     regio == "Rheinland-Pfalz" | regio == "Saarland" ~ "Rheinland-Pfalz / Saarland",
     regio == "Schleswig-Holstein" | regio == "Hamburg" ~ "Schleswig-Holstein / Hamburg",
     T ~ regio
   )

   df_query <- glue::glue_sql("
   SELECT *
   FROM arbeitsmarkt_epa
   WHERE jahr = {timerange}
   AND indikator = 'Engpassindikator'
   AND anforderung IN ({bf*})
   AND region = {regio}
                               ", .con = con)

    plot_data_raw <- DBI::dbGetQuery(con, df_query)

    # enthält den Text für den plot
    epa_kat_levels <- c("Engpassberuf",
                        "Anzeichen eines Engpassberufs",
                        "Kein Engpassberuf")
    group_col_dt <- data.frame(
      epa_kat = factor(x = epa_kat_levels,
                       levels = epa_kat_levels),
      epa_group_order = c(1:3),
      group_text = c("Text A",
                     "Text B",
                     "Text C"),
      group_col = c("#EE7775", "#FBBF24", "#35BD97")
    )

    # Aggregate rausfiltern
    plot_data_raw <- subset(plot_data_raw, !(plot_data_raw$berufsgruppe %in%
                                               c("Gesamt",
                                                 "MINT gesamt",
                                                 "Informatik",
                                                 "Landtechnik",
                                                 "Produktionstechnik",
                                                 "Bau- und Gebäudetechnik",
                                                 "Mathematik, Naturwissenschaften",
                                                 "Verkehrs-, Sicherheits- und Veranstaltungstechnik",
                                                 "Gesundheitstechnik",
                                                 "Nicht MINT"
                                               ))
    )

    if ("MINT gesamt" %in% fach) {
      plot_data_raw <- plot_data_raw %>%
        dplyr::filter(!mint_zuordnung %in% c("Nicht MINT", "Gesamt")) %>%
        dplyr::mutate(mint_zuordnung = "MINT gesamt") %>%
        rbind(plot_data_raw)
    }

    plot_data <- plot_data_raw %>%
      dplyr::filter(mint_zuordnung %in% fach &
                      !is.na(epa_kat)) %>%
      dplyr::group_by(epa_kat, mint_zuordnung)  %>%
      dplyr::summarise(beruf_num = dplyr::n()) %>%
      dplyr::group_by(mint_zuordnung)  %>%
      dplyr::mutate(value = round_preserve_sum(beruf_num / sum(beruf_num) * 100,0)) %>%
      dplyr::left_join(group_col_dt, by = "epa_kat") %>%
      dplyr::arrange(epa_group_order)

    # expand data for heatmap
    expanded_dt <- plot_data[rep(row.names(plot_data), plot_data$value),] %>%
      dplyr::arrange(mint_zuordnung, epa_group_order) %>%
      #
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

    fach_1 <- dplyr::case_when(
      fach[1] == "MINT gesamt" ~ "MINT",
      fach[1] == "Gesamt" ~ "allen Berufen",
      fach[1] == "Nicht MINT" ~ "allen Berufen außer MINT",
      T ~ fach[1]
    )

    titel_1 <- paste0("Engpassrisiko von Berufen in ", fach_1," (", timerange, ")")

    # Entfernen aller Zeilen, bei denen group_col NA ist
    expanded_dt <- expanded_dt[!is.na(expanded_dt$group_col), ]


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
      highcharter::hc_colors(used_colors) %>%
      highcharter::hc_tooltip(
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
                     fontFamily = "Calibri Regular",
                     fontSize = "20px")
      ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular")
      ) %>%
      highcharter::hc_size(380, 480) %>%
      highcharter::hc_exporting(enabled = TRUE,
                                buttons = list(
                                  contextButton = list(
                                    menuItems = list("downloadPNG", "downloadCSV")
                                  )
                                )
      )


      fach_2 <- dplyr::case_when(
        fach[2] == "MINT gesamt" ~ "MINT",
        fach[2] == "Gesamt" ~ "allen Berufen",
        fach[2] == "Nicht MINT" ~ "allen Berufen außer MINT",
        T ~ fach[2]
      )

      titel_2 <- paste0("Engpassrisiko in ", fach_2," (", timerange, ")")

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
        highcharter::hc_colors(used_colors) %>%
        highcharter::hc_tooltip(
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
                       fontFamily = "Calibri Regular",
                       fontSize = "20px")
        ) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "Calibri Regular")
        )%>%
        highcharter::hc_size(380, 480) %>%
        highcharter::hc_exporting(enabled = TRUE,
                                  buttons = list(
                                    contextButton = list(
                                      menuItems = list("downloadPNG", "downloadCSV")
                                    )
                                  )
        )


      out <- highcharter::hw_grid(
        plot_left, plot_right,
        ncol = 2)

      return(out)

  }

}

argument_demografie <- function(r){

}

argument_nachwuchs <- function(r){

}

argument_wirkhebel <- function(r){

}

argument_frauen <- function(r){

}
