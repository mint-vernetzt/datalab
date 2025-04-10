
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

  betrachtung <- "Einzelansicht - Kuchendiagramm"
  testy1 <- 2023 ####################################################################
  regio <- r$region_argumentationshilfe
  testl1 <- c("Studierende", "internationale Studierende", "Studierende (Lehramt)", "Studienanfänger:innen (1. Hochschulsemester)", "internationale Studienanfänger:innen (1. Hochschulsemester)", "Absolvent:innen", "internationale Absolvent:innen")  ################################



  df_query <- glue::glue_sql("
    SELECT region, jahr, indikator, fach, wert
    FROM studierende_detailliert
    WHERE jahr = {testy1}
    AND geschlecht = 'Gesamt'
    AND region = {regio}
    AND indikator in ({testl1*})
    AND fach IN ('Alle Nicht MINT-Fächer','Alle MINT-Fächer')", .con = con)

  df <- DBI::dbGetQuery(con, df_query)


  df_query <- glue::glue_sql("
    SELECT region, jahr, indikator, fach, wert as wert_ges
    FROM studierende_detailliert
    WHERE jahr = {testy1}
    AND geschlecht = 'Gesamt'
    AND region = {regio}
    AND indikator in ({testl1*})
    AND fach = 'Alle Fächer'", .con = con)

  alle <- DBI::dbGetQuery(con, df_query)


  df <- df %>%
    dplyr::left_join(alle, by = c("region", "jahr", "indikator")) %>%
    dplyr::rename(fach = fach.x) %>%
    dplyr::mutate(proportion = round(wert / wert_ges * 100, 1)) %>%
    dplyr::select(-fach.y)

  df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
  df$display_rel <- prettyNum(df$proportion, big.mark = ".", decimal.mark = ",")
  df <- within(df, fach <- factor(fach, levels = c("Alle Nicht MINT-Fächer", "Alle MINT-Fächer")))


  df <- df %>% dplyr::filter(indikator %in% testl1)
  df <- df[with(df, order(proportion, decreasing = TRUE)), ]

  out_1 <- highcharter::hchart(df, 'bar', highcharter::hcaes(y = proportion, x = indikator, group =forcats::fct_rev(fach)))%>%
    highcharter::hc_tooltip(pointFormat = "Anteil: {point.display_rel} % <br> Anzahl: {point.wert}") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  F) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
    highcharter::hc_colors(c( "#b16fab","#efe8e6")) %>%
    highcharter::hc_title(text = ifelse(regio == "Saarland",
                                        paste0("Anteil von Studierenden in MINT an allen Studierenden im ", regio, " (", testy1, ")"),
                                        paste0("Anteil von Studierenden in MINT an allen Studierenden in ", regio, " (", testy1, ")")),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG", "downloadCSV")
                                )
                              )
    )


  #

  #######


  betrachtung <- "Gruppenvergleich - Balkendiagramm"
  timerange <- 2023 #L
  regio <- r$region_argumentationshilfe
  faecher <- "MINT"

  gruppe <- c("Auszubildende",
                "Auszubildende (1. Jahr)",
                "ausländische Auszubildende",
                "Beschäftigte",
                "ausländische Beschäftigte",
                "Beschäftigte u25",
                "Beschäftigte 25-55",
                "Beschäftigte ü55")



  df_query <- glue::glue_sql("
  SELECT *
  FROM arbeitsmarkt_detail
  WHERE jahr = {timerange}
  AND landkreis = 'alle Landkreise'
  AND bundesland = {regio}
  AND anforderung = 'Gesamt'
  AND geschlecht = 'Gesamt'
  AND indikator IN ({gruppe*})
  AND fachbereich = {faecher}
                               ", .con = con)

  df1 <- DBI::dbGetQuery(con, df_query)

  df <- df1 %>%
    dplyr::select( "indikator", "bundesland", "landkreis", "fachbereich",
                   "landkreis_zusatz", "landkreis_nummer", "jahr", "anforderung", "geschlecht", "wert")



  df$wert_disp <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
  df <- df[with(df, order(wert, decreasing = TRUE)), ]


  titel <- ifelse(regio == "Saarland",
                  paste0("Beschäftigte in MINT in unterschiedlichen Beschäftigtengruppen im ", regio, " (", timerange, ")"),
                  paste0("Beschäftigte in MINT in unterschiedlichen Beschäftigtengruppen in ", regio, " (", timerange, ")"))
  out_2 <- balkenbuilder(df, titel, x="indikator", y="wert",group=NULL, tooltip = "Anzahl: {point.wert_disp}", format = "{value:, f}", color = "#b16fab")



  out <- highcharter::hw_grid(
    out_1, out_2,
    ncol = 2)

  return(out)


}

argument_wirkhebel <- function(r){

## es besteht nichts in bezug auf filter zu den verschiedenen Bundesländern.
  year_filter <- 2037

  df_query <- glue::glue_sql("
  SELECT *
  FROM fachkraefte_prognose
  WHERE jahr = {year_filter}
  AND indikator = 'Verbesserung'
  AND geschlecht = 'Gesamt'
  AND nationalitaet = 'Gesamt'
  AND anforderung = 'Gesamt'
", .con = con)

  df_query <- glue::glue_sql("
  SELECT *
  FROM fachkraefte_prognose
  WHERE wirkhebel = 'Basis-Szenario'
  AND geschlecht = 'Gesamt'
  AND nationalitaet = 'Gesamt'
  AND anforderung = 'Gesamt'
  AND jahr = 2022", .con = con)

  basis_wert <- DBI::dbGetQuery(con, df_query)

  basis_wert <- basis_wert %>%
    dplyr::pull(wert)


  df_query <- glue::glue_sql("
  SELECT *
  FROM fachkraefte_prognose
  WHERE jahr = {year_filter}
  AND indikator = 'Verbesserung'
  AND geschlecht = 'Gesamt'
  AND nationalitaet = 'Gesamt'
  AND anforderung = 'Gesamt'", .con = con)

  uebersicht_data <- DBI::dbGetQuery(con, df_query)

  uebersicht_data <- uebersicht_data %>%
    dplyr::mutate(basis_wert = basis_wert) %>%
    dplyr::select(wirkhebel, basis_wert, wert)%>%
    dplyr::mutate(wirkhebel = dplyr::case_when(wirkhebel == "Frauen in MINT" ~ "Mädchen und Frauen in MINT fördern",
                                               wirkhebel == "MINT-Bildung" ~ "MINT-Nachwuchs fördern",
                                               wirkhebel == "Internationale MINT-Fachkräfte" ~ "Zuwanderung MINT-Fachkräfte",
                                               wirkhebel == "Beteiligung älterer MINT-Fachkräfte" ~ "Verbleib älterer MINT-Fachkräfte",
                                               T ~ wirkhebel),diff = wert - basis_wert)


  row_to_move <- which(uebersicht_data$wirkhebel == "Gesamteffekt")

  uebersicht_data <- uebersicht_data %>%
    dplyr::slice(-row_to_move) %>%
    dplyr::bind_rows(uebersicht_data[row_to_move, ]) %>%
    dplyr::mutate(basis_label = paste0("Basis-Szenario"),
                  improvement_label = paste0("Positives Szenario: ", wirkhebel),

                  basis_wert_txt = prettyNum(basis_wert, big.mark = ".", decimal.mark = ","),
                  wert_txt = prettyNum(wert, big.mark = ".", decimal.mark = ","),
                  diff_txt = prettyNum(diff, big.mark = ".", decimal.mark = ",")) %>%
    dplyr::arrange(diff)

  fig <- plotly::plot_ly(uebersicht_data, color = I("gray80")) %>%
    plotly::add_segments(
      x = ~basis_wert,
      xend = ~wert,
      y = ~wirkhebel,
      yend = ~wirkhebel,
      showlegend = FALSE,
      text = ~paste0("MINT-Fachkräfteanzahl im Basis: ", basis_wert, "<br>Wert: ", wert_txt, "<br>: ", diff_txt),
      hoverinfo = "text"
    ) %>%
    plotly::add_markers(
      x = ~basis_wert,
      y = ~wirkhebel,
      name = "Basis-Szenario 2022",
      color = I("#D0A9CD"),
      symbol = I("square"),
      size = I(50),
      text = ~paste0("Basis-Szenario 2022: ", basis_wert_txt),
      hoverinfo = "text"
    ) %>%
    plotly::add_markers(
      x = ~wert,
      y = ~wirkhebel,
      name = paste0("Positives Szenario ",year_filter),
      color = I("#b16fab"),
      symbol = I("square"),
      size = I(50),
      text = ~paste0("Positives Szenario für Wirkhebel ", wirkhebel, ": ", wert_txt, "<br>Zunahme der MINT-Fachkräfte seit 2022: ", diff_txt),
      hoverinfo = "text"
    ) %>%
    plotly::layout(
      title = list(
        text = paste0(
          "Wie wirken sich die unten gelisteten Wirkhebel auf die Anzahl der MINT-Fachkräfte aus?"
        )
      ),
      xaxis = list(
        title = "Anzahl MINT-Fachkräfte",
        tickformat = ",",
        range = c(7500000, 9500000)
      ),
      yaxis = list(
        title = "",
        categoryorder = "array",
        categoryarray = unique(uebersicht_data$wirkhebel)
      ),
      margin = list(l = 100, r = 50, t = 80, b = 50),
      hoverlabel = list(bgcolor = "white"),
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.5,
        xanchor = "center",
        yanchor = "top"
      )
    )

  hc <- fig

  return(hc)

  ###



}

argument_frauen <- function(r){


  # load UI inputs from reactive value
  timerange <- c(2013, 2025)
  t<- timerange[1]:timerange[2]
  regio <- r$region_argumentationshilfe
  indikator_choice <- c("Leistungskurse", "Studierende", "Auszubildende", "Beschäftigte")
  abs_selector <- "Anzahl"




  query <- glue::glue_sql("
  SELECT bereich, indikator, fachbereich, geschlecht, jahr, wert
  FROM zentral
  WHERE jahr IN ({t*})
    AND region IN ({regio*})
    AND geschlecht = 'Frauen'
    AND fachbereich = 'MINT'
    AND indikator IN ({indikator_choice*})
", .con = con)

  df <- DBI::dbGetQuery(con, query)




  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- "."
  options(highcharter.lang = hcoptslang)

  df <- df[with(df, order(indikator, jahr, decreasing = FALSE)), ]

  # Ordnen der Legende
  sorted_indicators <- df %>%
    dplyr::group_by(indikator) %>%
    dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
    dplyr::arrange(desc(m_value)) %>%
    dplyr::pull(indikator)

  df$indikator <- factor(df$indikator, levels = sorted_indicators)

  titel <- "Anzahl von Frauen in MINT nach Bildungsbereichen"
  tooltip <- "Anzahl Frauen <br> Indikator: {point.indikator} <br> Anzahl: {point.y} "
  format <- "{value}"

  color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24" )

  out <- linebuilder(df,titel,x="jahr", y="wert", group="indikator", tooltip, format, color)




}
