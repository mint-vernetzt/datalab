# Box 1 ----
#
#




plot_fachkraft_prognose  <- function(r) {

  filter_wirkhebel <- c("Basis-Szenario", r$fachkraft_item_prog_wirkhebel)
  filter_indikator <- c("Status-quo", r$fachkraft_item_prog_scenario)
  filter_berufslevel <- "Gesamt"

  # plot_data <- dplyr::tbl(con, from ="fachkraefte_prognose") %>%

  df_query <- glue::glue_sql("
  SELECT *
  FROM fachkraefte_prognose
  WHERE wirkhebel IN ({filter_wirkhebel*})
  AND indikator IN ({filter_indikator*})
  AND geschlecht = 'Gesamt'
  AND nationalitaet = 'Gesamt'
  AND jahr <= 2037
  AND anforderung = {filter_berufslevel}
                               ", .con = con)



  plot_data <- DBI::dbGetQuery(con, df_query)



  plot_data <- plot_data %>%
    dplyr::group_by("jahr") %>%
    dplyr::mutate(
      wert = dplyr::case_when(
        wirkhebel == filter_wirkhebel[2] ~ wert - wert[which(wirkhebel == "Basis-Szenario")],
        TRUE ~ wert
      )
    ) %>%
    dplyr::ungroup()%>%
    dplyr::mutate(wirkhebel = dplyr::case_when(wirkhebel == "Frauen in MINT" ~ "Mädchen und Frauen in MINT fördern",
                                               wirkhebel == "MINT-Bildung" ~ "MINT-Nachwuchs fördern",
                                               T ~ wirkhebel))

  plot_data <- plot_data %>%
    dplyr::mutate(display_color = ifelse(indikator == "Status-quo", "#DCBED9", "#154194"))

  data_list <- split(plot_data, plot_data$wirkhebel)

 # browser()

  if (filter_wirkhebel[2] == "Frauen in MINT") filter_wirkhebel[2]<-"Mädchen und Frauen in MINT fördern"
  if (filter_wirkhebel[2] == "MINT-Bildung") filter_wirkhebel[2]<-"MINT-Nachwuchs fördern"
  # Texte vorbereiten
  szenario <- paste0(filter_indikator[2], " in der ", filter_wirkhebel[2])
  szenario <- ifelse(filter_wirkhebel[2] == "Gesamteffekt",
                    paste0(filter_indikator[2], " in der Gesamtsituation von Nachwuchs- und
                     Frauenförderung sowie der Integration internationaler und älterer Fachkräfte in MINT"),
                     szenario)
  szenario <- ifelse(filter_wirkhebel[2] == "Internationale MINT-Fachkräfte",
                     paste0(filter_indikator[2],
                            " bei der Integration internationaler MINT-Fachkräfte"),
                     szenario)
  szenario <- ifelse(filter_wirkhebel[2] == "Mädchen und Frauen in MINT fördern",
                     paste0(filter_indikator[2],
                            " bei der Gewinnug von Frauen für MINT"),
                     szenario)
  szenario <- ifelse(filter_wirkhebel[2] == "MINT-Nachwuchs fördern",
                     paste0(filter_indikator[2],
                            " bei der Förderung des MINT-Nachwuchses"),
                     szenario)



  titel <- paste0("Zukünftige MINT-Fachkräfteentwicklung bei aktuellen Verhältnissen
  im Vergleich zu einer ", szenario, " bis 2037")

  titel <- ifelse(filter_wirkhebel[2] == "Internationale MINT-Fachkräfte" &
                    filter_indikator[2] == "Stillstand",
                  "Zukünftige MINT-Fachkräfteentwicklung bei aktuellen Verhältnissen
                  im Vergleich zu einem Kombination aus einem Stillstand in der Zuwanderung
                  von internationalen MINT-Fachkräften bis 2037",
                  titel)

  titel <- ifelse(filter_wirkhebel[2] == "Mädchen und Frauen in MINT fördern" &
                       filter_indikator[2] == "starke Verbesserung",
                  "Zukünftige MINT-Fachkräfteentwicklung bei aktuellen Verhältnissen
                  im Vergleich zu einer Kombination aus einem Zuwachs im
                  MINT-Nachwuchs und einem außerdem zusätzlich verstärkten Zuwachs von
                  jungen Frauen in MINT bis 2037",
                  titel)

  if(filter_wirkhebel[2] == "MINT-Nachwuchs fördern"){
    if(filter_indikator[2] == "Verbesserung"){
      subtitel <- "Die Berechnung beruht auf der Annahme, dass es durch MINT-Bildungsförderung gelingt,
    den Anteil an jungen Menschen, die einen MINT-Beruf ergreifen, zu erhöhen. <br>
    Dadurch könnten im Vergleich zum Basisszenario knapp 800.000 Personen mehr 2037 in MINT beschäftigt sein."
    }else if(filter_indikator[2] == "Verschlechterung"){
    subtitel <- "Die Berechnung beruht auf der Annahme, dass sich der Anteil an jungen Menschen, die einen MINT-Beruf ergreifen,
    durch einen Rückgang in MINT-Bildungsinitiativen verringert. <br>
    Dadurch könnten im Vergleich zum Basisszenario knapp 800.000 Personen weniger 2037 in MINT beschäftigt sein."
    }
  }else if(filter_wirkhebel[2] == "Mädchen und Frauen in MINT fördern"){
    if(filter_indikator[2] == "Verbesserung"){
      subtitel <- "Die Berechnung beruht auf den Annahmen, dass es durch eine Förderung von Mädchen und jungen Frauen in MINT gelingt,
    den Anteil an jungen Frauen in MINT zu erhöhen. <br>
    Dadurch könnten im Vergleich zum Basisszenario ca. 290.000 Personen mehr 2037 in MINT beschäftigt sein."
    }else if(filter_indikator[2] == "starke Verbesserung"){
    subtitel <- "Die Berechnung beruht auf den Annahmen, dass es durch eine Kombination aus MINT-Bildungsförderung und
    besonderer Förderung von Mädchen und jungen Frauen in MINT gelingt,
    den Anteil an jungen Menschen, die einen MINT-Beruf ergreifen, zu erhöhen. <br>
    Dadurch könnten im Vergleich zum Basisszenario ca. 1 Mio. Personen mehr 2037 in MINT beschäftigt sein."
    }
  }else if(filter_wirkhebel[2] == "Internationale MINT-Fachkräfte"){
    if(filter_indikator[2] == "Verbesserung"){
      subtitel <- "Die Berechnung beruht auf der Annahme, dass die Zahl an zugewanderten internationalen
    MINT-Fachkräften zukünftig noch stärker ansteigt als bisher. <br>
    Dadurch könnten im Vergleich zum Basisszenario ca. 150.000 Personen mehr 2037 in MINT beschäftigt sein."
    }else if(filter_indikator[2] == "Verschlechterung"){
    subtitel <- "Die Berechnung beruht auf der Annahme, dass sich der positive Trend in der Zuwanderung internationaler
    MINT-Fachkräfte wieder verschlechtert. <br>
    Dadurch könnten im Vergleich zum Basisszenario ca. 150.000 Personen weniger 2037 in MINT beschäftigt sein."
    }else if(filter_indikator[2] == "Stillstand"){
      subtitel <- "Die Berechnung beruht auf der Annahme, dass die Zuwanderung internationaler MINT-Fachkräfte vollständig
      zum Erliegen kommt. <br>
    Dadurch könnten im Vergleich zum Basisszenario gut 350.000 Personen weniger 2037 in MINT beschäftigt sein."
    }
  }else if(filter_wirkhebel[2] == "Beteiligung älterer MINT-Fachkräfte"){
    subtitel <- "Die Berechnung beruht auf der Annahme, dass eine längere Beschäftigung von älteren MINT-Fachkräften
    noch weiter ansteigt als bisher.<br>
    Dadurch könnten im Vergleich zum Basisszenario gut 250.000 Personen mehr 2037 in MINT beschäftigt sein."
  }else if(filter_wirkhebel[2] == "Gesamteffekt"){
    if(filter_indikator[2] == "Verbesserung"){
      subtitel <- "Die Berechnung betrachtet den Gesamteffekt von einer Erhöhung des MINT-Nachwuchses durch Bildungsinitiativen,
    der Stärkung von Frauen in MINT und der stärkeren Integration von internationalen und älteren MINT-Fachkräften.
    Dadurch könnten im Vergleich zum Basiszenario ca. 1,5 Mio. Personen mehr 2037 in MINT beschäftigt sein."
    }else if(filter_indikator[2] == "Verschlechterung"){
      subtitel <- "Die Berechnung betrachtet den Gesamteffekt auf die MINT-Nachwuchzahlen bei einem Rückgang von Bildungsinitiativen und Frauenförderung in MINT
      und geringerer Integration von internationaler und älterer MINT-Fachkräften.
    Dadurch würden im Vergleich zum Basisszenario gut 1 Mio. Personen weniger 2037 in MINT beschäftigt sein."
    }
  }

  hc <- highcharter::highchart() %>%
    highcharter::hc_chart(type = "areaspline") %>%
    highcharter::hc_title(text =  titel, align = "left") %>%
    highcharter::hc_subtitle(text = subtitel, align = "left") %>%
    highcharter::hc_legend(
      layout = "horizontal",
      align = "center",
      verticalAlign = "bottom"
    ) %>%
    highcharter::hc_xAxis(plotBands = list(
      list(
        from = 2012,
        to = 2022,
        color = "#F9F6F5"
      )
    )) %>%
    highcharter::hc_yAxis(title = list(text = ""),
                          min = 2500000,
                          labels = list(formatter = highcharter::JS("
                          function() {
                            return Highcharts.numberFormat(this.value, 0, '.', '.');
                          }
                        "))) %>%
    highcharter::hc_tooltip(shared = FALSE, headerFormat = "<b>Fachkräfte-Entwicklung {point.x}</b><br>") %>%
    highcharter::hc_caption(text = "Vorausberechnung durch das IW Köln, 2024, beauftragt durch MINTvernetzt",
                            style = list(fontSize = "11px", color = "gray")) %>%
    highcharter::hc_credits(enabled = FALSE) %>%
    highcharter::hc_plotOptions(
      series = list(pointStart = 2012,
                    stacking = 'normal'),
      areaspline = list(fillOpacity = 0.5)
    )

  # Serie für "Gesamteffekt" hinzufügen
  hc <- hc %>% highcharter::hc_add_series(
    name = filter_wirkhebel[2],
    data = plot_data %>% dplyr::filter(wirkhebel == filter_wirkhebel[2]) %>% dplyr::pull(wert),
    color = "#154194",
    zoneAxis = 'x',
    zones = list(
       list(value = 2022),
      list(dashStyle = 'Dash')
    )
  )

  hc <- hc %>% highcharter::hc_add_series(
    name = "Basis-Szenario",
    data = plot_data %>% dplyr::filter(wirkhebel == "Basis-Szenario") %>% dplyr::pull(wert),
    color = "#D0A9CD",
    zoneAxis = 'x',
    zones = list(
      list(value = 2022),
      list(dashStyle = 'Dash')
    )
  )

  hc <- hc  %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG", "downloadCSV")
                                  )
                                )
                            )

   #                                                 list(
   #                                                   text = "Daten für GPT",
   #                                                   onclick = htmlwidgets::JS(sprintf(
   #                                                     "function () {
   #   var date = new Date().toISOString().slice(0,10);
   #   var chartTitle = '%s'.replace(/\\s+/g, '_');
   #   var filename = chartTitle + '_' + date + '.txt';
   #
   #   var data = this.getCSV();
   #   var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
   #   if (window.navigator.msSaveBlob) {
   #     window.navigator.msSaveBlob(blob, filename);
   #   } else {
   #     var link = document.createElement('a');
   #     link.href = URL.createObjectURL(blob);
   #     link.download = filename;
   #     link.click();
   #   }
   # }", gsub("'", "\\\\'", titel)))))
   #                              )
   #                            )
   #  )

  return(hc)
}

#Tab 3
plot_fachkraft_prognose_alle  <- function(r) {

  filter_wirkhebel <- c("Basis-Szenario", r$fachkraft_item_prog_alle_wirkhebel)

  df_query <- glue::glue_sql("
  SELECT *
  FROM fachkraefte_prognose
  WHERE wirkhebel IN ({filter_wirkhebel*})
  AND geschlecht = 'Gesamt'
  AND nationalitaet = 'Gesamt'
  AND jahr <= 2037
  AND anforderung = 'Gesamt'
                               ", .con = con)


  plot_data <- DBI::dbGetQuery(con, df_query)


  if(filter_wirkhebel[2] == "Frauen in MINT"){

    plot_data$indikator <- factor(plot_data$indikator, levels = c("starke Verbesserung",
                                                                  "Verbesserung",
                                                                  "Status-quo"))
    levels(plot_data$indikator) <- c("kombiniertes Szenario", "Positives Szenario",
                                     "aktuelles Szenario")
    color_vec <- c("#154194", "#D0A9CD", "#8893a7" )

  }else if (filter_wirkhebel[2] == "Internationale MINT-Fachkräfte"){

    plot_data$indikator <- factor(plot_data$indikator, levels = c("Verbesserung",
                                                                  "Verschlechterung",
                                                                  "Stillstand",
                                                                  "Status-quo"))
    levels(plot_data$indikator) <- c("Positives Szenario", "Rückgang im Positivtrend der Zuwanderung",
                                     "vollständiger Stillstand der Zuwanderung", "aktuelles Szenario")

    color_vec <- c("#154194", "#D0A9CD", "#DCBED9", "#8893a7" )

  }else if (filter_wirkhebel[2] == "Beteiligung älterer MINT-Fachkräfte"){

    plot_data$indikator <- factor(plot_data$indikator, levels = c("Verbesserung",
                                                                  "Status-quo"))
    levels(plot_data$indikator) <- c("Positives Szenario",
                                     "aktuelles Szenario")
    color_vec <- c("#154194", "#8893a7" )

  }else{

    plot_data$indikator <- factor(plot_data$indikator, levels = c("Verbesserung",
                                                                  "Verschlechterung",
                                                                  "Status-quo"))
    levels(plot_data$indikator) <- c("Positives Szenario",
                                     "negatives Szenario",
                                     "aktuelles Szenario")
    color_vec <- c("#154194", "#D0A9CD", "#8893a7" )

  }

  if(filter_wirkhebel[2] == "Frauen in MINT") filter_wirkhebel[2]<-"Mädchen und Frauen in MINT fördern"
  if(filter_wirkhebel[2] == "MINT-Bildung") filter_wirkhebel[2] <- "MINT-Nachwuchs fördern"

  # Texte vorbereiten
  titel <- paste0("Mögliche Zukunftsszenarien für die MINT-Fachkräftezahlen
                   bei unterschiedlichen Entwicklungen in der ", filter_wirkhebel[2])#"Titel"
  titel <- ifelse(filter_wirkhebel[2] == "Mädchen und Frauen in MINT fördern",
                  "Mögliche Zukunftsszenarien für die MINT-Fachkräftezahlen
                   bei unterschiedlichen Entwicklungen in der Förderung von Mädchen und jungen
                   Frauen in MINT", titel)
  titel <- ifelse(filter_wirkhebel[2] == "Internationale MINT-Fachkräfte",
                  "Mögliche Zukunftsszenarien für die MINT-Fachkräftezahlen
                   bei unterschiedlichen Entwicklungen in der Integration internationaler MINT-Fachkräfte",
                  titel)
  titel <- ifelse(filter_wirkhebel[2] == "MINT-Nachwuchs fördern",
                  "Mögliche Zukunftsszenarien für die MINT-Fachkräftezahlen
                   bei unterschiedlichen Entwicklungen in der Förderung des MINT-Nachwuchses",
                  titel)
  titel <- ifelse(filter_wirkhebel[2] == "Gesamteffekt",
                  "Mögliche Zukunftsszenarien für die MINT-Fachkräftezahlen
                   bei unterschiedlichen Entwicklungen der Gesamteffekte",
                  titel)




  titel <- titel
  tooltip <- "Anzahl: {point.y}"
  format <- "{value:, f}"
  color <- color_vec
  quelle <- "Vorausberechnung durch IW Köln, 2024, beauftragt durch MINTvernetzt"
  hc <- linebuilder_light(plot_data, titel, x = "jahr", y = "wert", group = "indikator", tooltip, format, color, quelle = quelle)


  return(hc)
}

plot_fachkraft_prognose_detail  <- function(r) {
  filter_wirkhebel <- r$fachkraft_item_prog_detail_wirkhebel
  filter_indikator <- c("Status-quo", ifelse(r$fachkraft_item_prog_detail_wirkhebel == "Basis-Szenario",
                                             "Status-quo",
                                             ifelse(is.null(r$fachkraft_item_prog_detail_scenario), "Verbesserung",
                                                    r$fachkraft_item_prog_detail_scenario)))
  filter_gruppe <- r$fachkraft_item_prog_detail_gruppe


  color_palette <- NULL
  if(filter_gruppe == "Berufslevel"){
    color_palette <- c(colors_mint_vernetzt$general)
  } else if(filter_gruppe == "Geschlecht"){
    color_palette <- c(colors_mint_vernetzt$short)
  } else if(filter_gruppe == "Nationalität"){
    color_palette <- c(colors_mint_vernetzt$short)
  }

  focused_column <- ifelse(filter_gruppe == "Berufslevel", "anforderung",
                           ifelse(filter_gruppe == "Geschlecht", "geschlecht",
                                  ifelse(filter_gruppe == "Nationalität", "nationalitaet", NULL)))

  not_focused_column <- c("anforderung", "geschlecht", "nationalitaet")[c("anforderung", "geschlecht", "nationalitaet") != focused_column]



  df_query <- glue::glue_sql("
  SELECT *
  FROM fachkraefte_prognose
  WHERE wirkhebel IN ({filter_wirkhebel*})
  AND indikator IN ({filter_indikator*})
  AND jahr <= 2037
                               ", .con = con)



  plot_data <- DBI::dbGetQuery(con, df_query)



  plot_data <-plot_data %>%
    dplyr::filter(if_all(all_of(not_focused_column), ~ .x == "Gesamt")) %>%
    dplyr::select(all_of(c("wirkhebel", "indikator", "jahr", focused_column, "wert"))) %>%
    dplyr::filter(!!dplyr::sym(focused_column) != "Gesamt") %>%
    dplyr::mutate(dplyr::across(all_of(focused_column), ~ factor(.x, levels = c(sort(unique(.x)))))) %>%
    dplyr::arrange(dplyr::across(all_of(focused_column)), jahr)%>%
    dplyr::mutate(wirkhebel = dplyr::case_when(wirkhebel == "Frauen in MINT" ~ "Mädchen und Frauen in MINT fördern",
                                               wirkhebel == "MINT-Bildung" ~ "MINT-Nachwuchs fördern",
                                               T ~ wirkhebel))

  if(filter_wirkhebel == "Frauen in MINT") filter_wirkhebel <- "Mädchen und Frauen in MINT fördern"
  if(filter_wirkhebel == "MINT-Bildung") filter_wirkhebel <- "MINT-Nachwuchs fördern"

  if(focused_column == "nationalitaet"){
    plot_data$nationalitaet <- factor(plot_data$nationalitaet,
                                      levels = c("Keine deutsche Staatsangehörigkeit",
                                                 "deutsche Staatsangehörigkeit"))

  }else if(focused_column == "anforderung"){

    plot_data$anforderung <- factor(plot_data$anforderung,
                                    levels = c("Expert:innen",
                                               "Spezialist:innen",
                                               "Fachkräfte"))

    levels(plot_data$anforderung) <- c("Akademiker:innen", "Facharbeiter:innen mit Fortbildung (Techniker, Meister)",
                                       "Facharbeiter:innen mit Ausbildung")

  }


  data_list <- split(plot_data, plot_data[focused_column])

  if(filter_wirkhebel == "MINT-Nachwuchs fördern"){
    subtitel <- "Die Berechnung beruht auf der Annahme, dass es durch MINT-Bildungsförderung gelingt,
    den Anteil an jungen Menschen, die einen MINT-Beruf ergreifen, zu erhöhen."
  }else if(filter_wirkhebel == "Mädchen und Frauen in MINT fördern"){
    subtitel <- "Die Berechnung beruht auf den Annahmen, dass es durch Förderung
    von Mädchen und jungen Frauen in MINT gelingt,
    den Anteil an jungen Frauen, die einen MINT-Beruf ergreifen, zu erhöhen."
  }else if(filter_wirkhebel == "Internationale MINT-Fachkräfte"){
    subtitel <- "Die Berechnung beruht auf der Annahme, dass die Zahl an zugewanderten internationalen
    MINT-Fachkräften zukünftig noch stärker ansteigt als bisher."
  }else if(filter_wirkhebel == "Beteiligung älterer MINT-Fachkräfte"){
    subtitel <- "Die Berechnung beruht auf der Annahme, dass eine längere Beschäftigung von älteren MINT-Fachkräften
    noch weiter ansteigt als bisher."
  }else if(filter_wirkhebel == "Gesamteffekt"){
    subtitel <- "Die Berechnung betrachtet den Gesamteffekt von einer Erhöhung des MINT-Nachwuchses durch Bildungsinitiativen,
    der Stärkung von Frauen in MINT und der stärkeren Integration von internationalen und älteren MINT-Fachkräften."

  }else if(filter_wirkhebel == "Basis-Szenario"){
    subtitel <- "Die Berechnung schreibt die aktuellen Entwicklungen in den MINT-Fachkräftezahlen bis 2037 fort (Basis-Szenario)."
  }



  #browser()



  hc <- highcharter::highchart() %>%
    highcharter::hc_chart(type = "areaspline") %>%
    highcharter::hc_title(text = paste0("Zukünftige MINT-Fachkräfteentwicklung bis 2037 betrachtet nach ", filter_gruppe), align = "left") %>%
    highcharter::hc_subtitle(text = subtitel, align = "left") %>%
    highcharter::hc_legend(
      layout = "horizontal",
      align = "center",
      verticalAlign = "bottom"
    ) %>%
    highcharter::hc_xAxis(plotBands = list(
      list(
        from = 2012,
        to = 2022,
        color = "#F9F6F5"
      )
    )) %>%
    highcharter::hc_yAxis(title = list(text = ""),
                          min = 2500000,
                          labels = list(formatter = highcharter::JS("
                          function() {
                            return Highcharts.numberFormat(this.value, 0, '.', '.');
                          }
                        "))) %>%
    highcharter::hc_tooltip(shared = TRUE, headerFormat = "<b>Fachkräfte-Entwicklung {point.x}</b><br>") %>%
    highcharter::hc_caption(text = "Vorausberechnung durch das IW Köln, 2024, beauftragt durch MINTvernetzt",
                            style = list(fontSize = "11px", color = "gray")) %>%
    highcharter::hc_credits(enabled = FALSE) %>%
    highcharter::hc_plotOptions(
      series = list(pointStart = 2012,
                    stacking = 'normal'),
      areaspline = list(fillOpacity = 0.5)
    )


  for(i in 1:length(data_list)) {
    hc <- hc %>% highcharter::hc_add_series(
      name = paste(data_list[[i]] %>%
                     dplyr::pull(!!dplyr::sym(focused_column)) %>%
                     unique()),

      data = data_list[[i]] %>%
        dplyr::pull(wert),
      color = color_palette[i],
      zoneAxis = 'x',
      zones = list(
        list(value = 2022),
        list(dashStyle = 'dot')
      )
    )
  }


  return(hc)
}
########################################################## bis hierin

plot_fachkraft_wirkhebel_analyse  <- function(r) {
  year_filter <- r$fachkraft_item_wirkhebel_analyse



  if (is.null(year_filter) || !is.numeric(year_filter)) {
    stop("Fehler: `year_filter` ist NULL oder kein numerisches Jahr!")
  }

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
  AND jahr = 2022
                               ", .con = con)
  basis_wert <- DBI::dbGetQuery(con, df_query)

  #browser()

  basis_wert <- basis_wert %>%
    dplyr::pull(wert)


  df_query <- glue::glue_sql("
  SELECT *
  FROM fachkraefte_prognose
  WHERE jahr = {year_filter}
  AND indikator = 'Verbesserung'
  AND geschlecht = 'Gesamt'
  AND nationalitaet = 'Gesamt'
  AND anforderung = 'Gesamt'
                               ", .con = con)
  uebersicht_data <- DBI::dbGetQuery(con, df_query)

  uebersicht_data <- uebersicht_data %>%
    dplyr::mutate(basis_wert = basis_wert) %>%
    dplyr::select(wirkhebel, basis_wert, wert)%>%
    dplyr::mutate(wirkhebel = dplyr::case_when(wirkhebel == "Frauen in MINT" ~ "Mädchen und Frauen in MINT fördern",
                                               wirkhebel == "MINT-Bildung" ~ "MINT-Nachwuchs fördern",
                                               wirkhebel == "Internationale MINT-Fachkräfte" ~ "Zuwanderung MINT-Fachkräfte",
                                               wirkhebel == "Beteiligung älterer MINT-Fachkräfte" ~ "Verbleib älterer MINT-Fachkräfte",
                                               T ~ wirkhebel),
                  diff = wert - basis_wert)


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
      color = I("#154194"),
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
      ),
      annotations = list(
        list(
          text = "Vorausberechnung durch IW Köln, 2024, beauftragt durch MINTvernetzt",
          x = 0,
          y = -0.35,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          xanchor = "left",
          yanchor = "top",
          font = list(size = 11, color = "gray")
        )
      )
    )%>%
    plotly::config(displaylogo = FALSE,  modeBarButtonsToRemove = c(
      'sendDataToCloud', 'autoScale2d', 'resetScale2d', 'toggleSpikelines',
      'hoverClosestCartesian', 'hoverCompareCartesian',
      'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d'
    ),modeBarButtonsToAdd = list(
      list(
        name = "Download CSV",
        icon = list(
          path = "M16,2H8C6.9,2,6,2.9,6,4v16c0,1.1,0.9,2,2,2h8c1.1,0,2-0.9,2-2V4C18,2.9,17.1,2,16,2z M16,20H8V4h8V20z M14.5,14h-2v3h-1v-3h-2l2.5-3.5L14.5,14z",
          width = 24,
          height = 24
        ),
        click = htmlwidgets::JS("
              function(gd) {
                var csv = 'x,y\\n';
                var data = gd.data[0];
                for (var i = 0; i < data.x.length; i++) {
                  csv += data.x[i] + ',' + data.y[i] + '\\n';
                }
                var blob = new Blob([csv], { type: 'text/csv' });
                var a = document.createElement('a');
                a.href = URL.createObjectURL(blob);
                a.download = 'data.csv';
                a.click();
              }
            ")
      )
    )
    )

  hc <- fig

  return(hc)
}



# Box 2 ----
#' A function to plot a graph.
#'
#' @description A function to create a plots inside the tab "FACHKRAFT".
#'
#' @return The return value is a plot
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd


plot_fachkraft_epa_item <- function(r) {

  timerange <- r$map_y_fachkraft_arbeit_epa
  fach <- r$map_f_fachkraft_arbeit_epa
  bf_label <- r$map_bl_fachkraft_arbeit_epa

  if (bf_label == "Gesamt") {
    bf <- fachkraft_ui_berufslevel()
  } else {
    bf <- bf_label
  }




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

  if ("Technik gesamt" %in% fach) {
    plot_data_raw <- plot_data_raw %>%
      dplyr::filter(mint_zuordnung %in% c("Landtechnik",
                                          "Produktionstechnik",
                                          "Bau- und Gebäudetechnik",
                                          "Verkehrs-, Sicherheits- und Veranstaltungstechnik",
                                          "Gesundheitstechnik")) %>%
      dplyr::mutate(mint_zuordnung = "Technik gesamt") %>%
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

  if("Gesamt" %in% fach){
    plot_data_ges <- plot_data_raw %>%
      dplyr::filter(!is.na(epa_kat)) %>%
      dplyr::group_by(epa_kat) %>%
      dplyr::summarise(beruf_num = dplyr::n()) %>%
      dplyr::mutate(value = round_preserve_sum(beruf_num / sum(beruf_num) * 100,0)) %>%
      dplyr::left_join(group_col_dt, by = "epa_kat") %>%
      dplyr::arrange(epa_group_order) %>%
      dplyr::mutate(mint_zuordnung = "Gesamt")

    plot_data <- rbind(plot_data, plot_data_ges)
  }

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
  level <- dplyr::case_when(
    bf_label == "Gesamt" ~ "",
    bf_label == "Fachkräfte" ~ "Nur Beschäftigte in Ausbildungsberufen, ",
    bf_label == "Spezialist*innen" ~ "Nur Beschäftigte in Meister-/Technikerstellen o.ä., ",
    bf_label == "Expert*innen" ~ "Nur Beschäftigten in Akademikerberufen, ",
  )
  fach_1 <- dplyr::case_when(
    fach[1] == "MINT gesamt" ~ "MINT",
    fach[1] == "Gesamt" ~ "allen Berufen",
    fach[1] == "Nicht MINT" ~ "allen Berufen außer MINT",
    T ~ fach[1]
  )

  titel_1 <- paste0("Engpassrisiko von Berufen in ", fach_1," (", level, timerange, ")")

  # Entfernen aller Zeilen, bei denen group_col NA ist
  expanded_dt <- expanded_dt[!is.na(expanded_dt$group_col), ]



  titel <- titel_1

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
    highcharter::hc_caption(text = "Quelle der Daten: Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
                              style = list(fontSize = "11px", color = "gray")) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG", "downloadCSV",
                                                   list(
                                                     text = "Daten für GPT",
                                                     onclick = htmlwidgets::JS(sprintf(
                                                       "function () {
     var date = new Date().toISOString().slice(0,10);
     var chartTitle = '%s'.replace(/\\s+/g, '_');
     var filename = chartTitle + '_' + date + '.txt';

     var data ='Quelle der Daten: Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt\\n\\n'+ this.getCSV();
     var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
     if (window.navigator.msSaveBlob) {
       window.navigator.msSaveBlob(blob, filename);
     } else {
       var link = document.createElement('a');
       link.href = URL.createObjectURL(blob);
       link.download = filename;
       link.click();
     }
   }", gsub("'", "\\\\'", titel)))))
                                )
                              )
    )


  if (length(fach) == 2) {



    fach_2 <- dplyr::case_when(
      fach[2] == "MINT gesamt" ~ "MINT",
      fach[2] == "Gesamt" ~ "allen Berufen",
      fach[2] == "Nicht MINT" ~ "allen Berufen außer MINT",
      T ~ fach[2]
    )

    titel_2 <- paste0("Engpassrisiko in ", fach_2," (", level,timerange, ")")
    titel <- titel_2
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
      highcharter::hc_caption(text = "Quelle der Daten: Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
                              style = list(fontSize = "11px", color = "gray")) %>%
      highcharter::hc_exporting(enabled = TRUE,
                                buttons = list(
                                  contextButton = list(
                                    menuItems = list("downloadPNG", "downloadCSV",
                                                     list(
                                                       text = "Daten für GPT",
                                                       onclick = htmlwidgets::JS(sprintf(
                                                         "function () {
     var date = new Date().toISOString().slice(0,10);
     var chartTitle = '%s'.replace(/\\s+/g, '_');
     var filename = chartTitle + '_' + date + '.txt';

     var data = 'Quelle der Daten: Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt\\n\\n'+this.getCSV();
     var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
     if (window.navigator.msSaveBlob) {
       window.navigator.msSaveBlob(blob, filename);
     } else {
       var link = document.createElement('a');
       link.href = URL.createObjectURL(blob);
       link.download = filename;
       link.click();
     }
   }", gsub("'", "\\\\'", titel)))))
                                  )
                                )
      )


    out <- list(plot_left, plot_right)

    return(out)

  }else{
    return(plot_left)
  }

}

plot_fachkraft_epa_bulas <- function(r) {


   timerange <- r$y_fachkraft_epa_bulas
   fach <- r$f_fachkraft_epa_bulas
   bf_label <- r$bl_fachkraft_epa_bulas

   if (bf_label == "Gesamt") {
     bf <- fachkraft_ui_berufslevel()
   } else {
     bf <- bf_label
   }

   if (timerange %in% 2020:2021){
     regio <- r$regio_fachkraft_epa_bulas20_21
   } else if(timerange == 2022){
     regio <- r$regio_fachkraft_epa_bulas22
   }else if(timerange == 2023){
     regio <- r$regio_fachkraft_epa_bulas23
   }

   df_query <- glue::glue_sql("

   SELECT *
   FROM arbeitsmarkt_epa
   WHERE jahr = {timerange}
   AND indikator = 'Engpassindikator'
   AND anforderung IN ({bf*})
   AND region = {regio}
                               ", .con = con)

   plot_data_raw <- DBI::dbGetQuery(con, df_query)

   # Aggregate rausfiltern
   plot_data_raw <- subset(plot_data_raw, !(plot_data_raw$beruf %in%
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

   if ("Technik gesamt" %in% fach) {
     plot_data_raw <- plot_data_raw %>%
       dplyr::filter(mint_zuordnung %in% c("Landtechnik",
                                           "Produktionstechnik",
                                           "Bau- und Gebäudetechnik",
                                           "Verkehrs-, Sicherheits- und Veranstaltungstechnik",
                                           "Gesundheitstechnik")) %>%
       dplyr::mutate(mint_zuordnung = "Technik gesamt") %>%
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

  plot_data <- plot_data_raw %>%
    dplyr::filter(mint_zuordnung %in% fach &
                    !is.na(epa_kat)) %>%
    dplyr::group_by(epa_kat, mint_zuordnung)  %>%
    dplyr::summarise(beruf_num = dplyr::n()) %>%
    dplyr::group_by(mint_zuordnung)  %>%
    dplyr::mutate(value = round_preserve_sum(beruf_num / sum(beruf_num) * 100,0)) %>%
    dplyr::left_join(group_col_dt, by = "epa_kat") %>%
    dplyr::arrange(epa_group_order)

  if("Gesamt" %in% fach){
    plot_data_ges <- plot_data_raw %>%
      dplyr::filter(!is.na(epa_kat)) %>%
      dplyr::group_by(epa_kat) %>%
      dplyr::summarise(beruf_num = dplyr::n()) %>%
      dplyr::mutate(value = round_preserve_sum(beruf_num / sum(beruf_num) * 100,0)) %>%
      dplyr::left_join(group_col_dt, by = "epa_kat") %>%
      dplyr::arrange(epa_group_order) %>%
      dplyr::mutate(mint_zuordnung = "Gesamt")

    plot_data <- rbind(plot_data, plot_data_ges)
  }

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
  level <- dplyr::case_when(
    bf_label == "Gesamt" ~ "",
    bf_label == "Fachkräfte" ~ "Nur Beschäftigte in Ausbildungsberufen, ",
    bf_label == "Spezialist*innen" ~ "Nur Beschäftigte in Meister-/Technikerstellen o.ä., ",
    bf_label == "Expert*innen" ~ "Nur Beschäftigten in Akademikerberufen, ",
  )
  fach_1 <- dplyr::case_when(
    fach[1] == "MINT gesamt" ~ "MINT",
    fach[1] == "Gesamt" ~ "allen Berufen",
    fach[1] == "Nicht MINT" ~ "allen Berufen außer MINT",
    T ~ fach[1]
  )

  titel_1 <- paste0("Engpassrisiko von Berufen in ", fach_1," (", level, timerange, ")")
  titel <- titel_1

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
    highcharter::hc_caption(text = "Quelle der Daten: Bundesagentur für Arbeit, 2024, freier Download, eigene Berechnungen durch MINTvernetzt.",
                            style = list(fontSize = "11px", color = "gray")) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG", "downloadCSV",
                                  list(
                                                        text = "Daten für GPT",
                                                        onclick = htmlwidgets::JS(sprintf(
                                                          "function () {
     var date = new Date().toISOString().slice(0,10);
     var chartTitle = '%s'.replace(/\\s+/g, '_');
     var filename = chartTitle + '_' + date + '.txt';

     var data ='Quelle der Daten: Bundesagentur für Arbeit, 2024, freier Download, eigene Berechnungen durch MINTvernetzt.\\n\\n'+ this.getCSV();
     var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
     if (window.navigator.msSaveBlob) {
       window.navigator.msSaveBlob(blob, filename);
     } else {
       var link = document.createElement('a');
       link.href = URL.createObjectURL(blob);
       link.download = filename;
       link.click();
     }
   }", gsub("'", "\\\\'", titel)))))
                                   )
                                 )
       )


  if (length(fach) == 2) {

    fach_2 <- dplyr::case_when(
      fach[2] == "MINT gesamt" ~ "MINT",
      fach[2] == "Gesamt" ~ "allen Berufen",
      fach[2] == "Nicht MINT" ~ "allen Berufen außer MINT",
      T ~ fach[2]
    )

    titel_2 <- paste0("Engpassrisiko in ", fach_2," (", level,timerange, ")")
    titel <- titel_2

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
      highcharter::hc_caption(text = "Quelle der Daten: Bundesagentur für Arbeit, 2024, freier Download, eigene Berechnungen durch MINTvernetzt.",
                              style = list(fontSize = "11px", color = "gray")) %>%
      highcharter::hc_exporting(enabled = TRUE,
                                buttons = list(
                                  contextButton = list(
                                    menuItems = list("downloadPNG", "downloadCSV",
                                  list(
                                                        text = "Daten für GPT",
                                                        onclick = htmlwidgets::JS(sprintf(
                                                          "function () {
     var date = new Date().toISOString().slice(0,10);
     var chartTitle = '%s'.replace(/\\s+/g, '_');
     var filename = chartTitle + '_' + date + '.txt';

     var data = 'Quelle der Daten: Bundesagentur für Arbeit, 2024, freier Download, eigene Berechnungen durch MINTvernetzt.\\n\\n'+this.getCSV();
     var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
     if (window.navigator.msSaveBlob) {
       window.navigator.msSaveBlob(blob, filename);
     } else {
       var link = document.createElement('a');
       link.href = URL.createObjectURL(blob);
       link.download = filename;
       link.click();
     }
   }", gsub("'", "\\\\'", titel)))))
                                   )
                                 )
       )

    out <- highcharter::hw_grid(
      plot_left, plot_right,
      ncol = 2)

     return(out)

   }else{
     return(plot_left)
   }

}


# plot_fachkraft_mint_item  <- function(r) {
#
#
#   timerange <- r$map_y_fachkraft_arbeit_mint
#   bf_label <- r$map_bl_fachkraft_arbeit_mint
#
#   if (bf_label == "Gesamt") {
#     bf <- fachkraft_ui_berufslevel()
#   } else {
#     bf <- bf_label
#   }
#
#
#
#    plot_data_raw <- dplyr::tbl(con, from ="arbeitsmarkt_epa_detail") %>%
#     dplyr::filter(jahr == timerange &
#                     indikator == "Engpassindikator" &
#                     anforderung %in% bf) %>%
#      dplyr::collect()
#
#    plot_data_raw <- plot_data_raw %>%
#      dplyr::mutate(mint_zuordnung = dplyr::if_else(
#       !mint_zuordnung %in% c("Nicht MINT", "Gesamt"),
#       "MINT gesamt",
#       mint_zuordnung)) %>%
#     dplyr::filter(mint_zuordnung %in% c("Nicht MINT", "MINT gesamt")) %>%
#     dplyr::group_by(mint_zuordnung, epa_kat) %>%
#     dplyr::summarise(berufe = dplyr::n()) %>%
#     dplyr::mutate(mint_epa_kat = paste0(mint_zuordnung, " - ", epa_kat)) %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(percent_total = round_preserve_sum(berufe / sum(berufe, na.rm = TRUE) * 100,1)) %>%
#     dplyr::group_by(epa_kat) %>%
#     dplyr::mutate(percent_epa = round_preserve_sum(berufe / sum(berufe, na.rm = TRUE) * 100,1)) %>%
#     dplyr::ungroup()
#
#
#   # enthält den Text für den plot
#   epa_kat_levels <- c("MINT gesamt - Engpassberuf",
#                       "Nicht MINT - Engpassberuf",
#                       "MINT gesamt - Anzeichen eines Engpassberufs",
#                       "Nicht MINT - Anzeichen eines Engpassberufs",
#                       "MINT gesamt - Kein Engpassberuf",
#                       "Nicht MINT - Kein Engpassberuf")
#   group_col_dt <- data.frame(
#     mint_epa_kat = factor(x = epa_kat_levels,
#                           levels = epa_kat_levels),
#     epa_group_order = c(1:6),
#     group_col = c("#EE7775","#f5adac",
#                   "#Fcc433", "#fdd670",
#                   "#00a87a", "#66cbaf")
#   )
#
#   plot_data <- plot_data_raw %>%
#     dplyr::right_join(group_col_dt, by = "mint_epa_kat") %>%
#     dplyr::arrange(epa_group_order) %>%
#     dplyr::group_by(mint_epa_kat) %>%
#     dplyr::mutate(berufe = dplyr::if_else(is.na(berufe), 0, berufe),
#                   percent_total = dplyr::if_else(is.na(percent_total), 0, percent_total),
#                   percent_epa = dplyr::if_else(is.na(percent_epa), 0, percent_epa)) %>%
#     dplyr::ungroup()
#
#
#   plot_data$mint_zuordnung <- ifelse(
#     plot_data$mint_zuordnung == "MINT gesamt",
#     "MINT-Berufe", "Nicht-MINT-Berufe"
#   )
#
#   # Titel vorbereiten
#   level <- dplyr::case_when(
#     bf_label == "Gesamt" ~ "",
#     bf_label == "Fachkräfte" ~ "Nur Beschäftigte in Ausbildungsberufen, ",
#     bf_label == "Spezialist*innen" ~ "Nur Beschäftigte in Meister-/Technikerstellen o.ä., ",
#     bf_label == "Expert*innen" ~ "Nur Beschäftigten in Akademikerberufen, ",
#   )
#
#   plot <- highcharter::hchart(
#     plot_data,
#     "item",
#     highcharter::hcaes(
#       name = mint_epa_kat,
#       y = berufe,
#       # label = group,
#       color = group_col),
#     # name = "group",
#     showInLegend = TRUE
#   ) %>%
#     highcharter::hc_tooltip(
#       pointFormat = paste0(
#
#         " {point.percent_epa}% der {point.epa_kat}<br>",
#         " sind Berufe in {point.mint_zuordnung}<br>",
#         " Anzahl: {point.berufe}<br>"
#         )) %>%
#     highcharter::hc_title(
#       text = paste0("Verteilung der Berufe in MINT vs. Nicht-MINT nach ihrem Engpassrisiko",
#                     " <br>(", level, timerange, ")"),
#       margin = 10,
#       align = "center",
#       style = list(color = "black",
#                    useHTML = TRUE,
#                    fontFamily = "SourceSans3-Regular",
#                    fontSize = "20px")
#     ) %>%
#     highcharter::hc_subtitle(
#       text = "Jeder Punkt steht für einen Beruf.
#       Die dunkleren Punkte sind Berufe im Bereich \"MINT\", die helleren Punkte sind Berufe,
#       die nicht in den MINT-Bereich zählen.",
#       align = "left"
#     ) %>%
#     highcharter::hc_chart(
#       style = list(fontFamily = "SourceSans3-Regular")
#     ) %>%
#
#     highcharter::hc_credits(enabled = FALSE) %>%
#
#      highcharter::hc_legend(enabled = FALSE)
#
#
#   out <- plot
#
#   return(out)
# }

plot_fachkraft_bar_vakanz  <- function(r) {


  this_indikator <- r$map_ind_fachkraft_arbeit_bar
  timerange <- r$map_y_fachkraft_arbeit_bar
  this_region <- r$map_reg_fachkraft_arbeit_bar
  bf_label <- r$map_bl_fachkraft_arbeit_bar


  berufe_order <- c("Insgesamt", "Keine MINT-Berufe", "MINT-Berufe")
#
#
#
#
#
  df_query <- glue::glue_sql("
  SELECT *
  FROM arbeitsmarkt_fachkraefte
  WHERE jahr = {timerange}
  AND indikator = {this_indikator}
  AND anforderung = {bf_label}
  AND region = {this_region}
                               ", .con = con)

  plot_data <- DBI::dbGetQuery(con, df_query)


  plot_data <- plot_data %>%
    dplyr::group_by(fachbereich) %>%
    dplyr::summarise(wert = round(mean(wert, na.rm = TRUE), 1)) %>%
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

  # für Überschrift/Subtitle
  level <- dplyr::case_when(
    bf_label == "Gesamt" ~ "",
    bf_label == "Fachkräfte" ~ "Nur Beschäftigte in Ausbildungsberufen, ",
    bf_label == "Spezialist*innen" ~ "Nur Beschäftigte in Meister-/Technikerstellen o.ä., ",
    bf_label == "Expert*innen" ~ "Nur Beschäftigten in Akademikerberufen, ",
  )

  if(this_indikator == "Arbeitslosen-Stellen-Relation"){

    subtitel <- "Arbeitslosen-Stellen-Relation = Arbeitslose & -suchende / sozialversicherungspflichtige Stellen.
      <br>Hier ist der Mittelwert in den Bereichen dargestellt. Je geringer der Wert, desto schwieriger ist es, Stellen passend zu besetzten."

  }else if(this_indikator == "Abgeschlossene Vakanzzeit"){

    subtitel <- "Abgeschlossene Vakanzzeit = mittlere Zeit, bis eine Stelle besetzt werden kann.
      <br>Hier ist der Mittelwert in den Bereichen dargestellt. Je höher der Wert, desto schwieriger ist es, Stellen passend zu besetzten."

  }


  titel <- paste0(this_indikator, " in den MINT-Bereichen in ", this_region,
                  " (", level, timerange, ")")

  ##NICHT IN BALKENBUILDER DA ES eine andere struktur hat, und komplizierter is

  plot<- highcharter::hchart(
    object = plot_data,
    type = "bar",
    mapping = highcharter::hcaes(x = fachbereich, y = wert, color = group_color)
  ) %>%
    highcharter::hc_title(
      text = paste0(this_indikator, " in den MINT-Bereichen in ", this_region,
                    " (", level, timerange, ")"),
      margin = 10,
      align = "center",
      style = list(color = "black",
                   useHTML = TRUE,
                   fontFamily = "Calibri Regular",
                   fontSize = "20px")
    ) %>%
    highcharter::hc_subtitle(
      text = subtitel,
      align = "left"
    ) %>%
    highcharter::hc_tooltip(
      pointFormat = ifelse(this_indikator == "Arbeitslosen-Stellen-Relation",
                           'Auf eine ausgeschriebene Stelle kommen {point.wert} Arbeitslose & -suchende.',
                           'Eine ausgeschriebene Stelle steht {point.wert} Tage leer, bis sie besetzt werden kann.'
      )) %>%
    highcharter::hc_yAxis(title = list(text = "")) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_caption(text = "Quelle der Daten: Bundesagentur für Arbeit, 2024, auf Anfrage; eigene Berechnungen durch MINTvernetzt.",
                            style = list(fontSize = "11px", color = "gray")) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG", "downloadCSV",
                                                   list(
                                                     text = "Daten für GPT",
                                                     onclick = htmlwidgets::JS(sprintf(
                                                       "function () {
     var date = new Date().toISOString().slice(0,10);
     var chartTitle = '%s'.replace(/\\s+/g, '_');
     var filename = chartTitle + '_' + date + '.txt';

     var data = 'Quelle der Daten: Bundesagentur für Arbeit, 2024, auf Anfrage; eigene Berechnungen durch MINTvernet\\n\\n'+this.getCSV();
     var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
     if (window.navigator.msSaveBlob) {
       window.navigator.msSaveBlob(blob, filename);
     } else {
       var link = document.createElement('a');
       link.href = URL.createObjectURL(blob);
       link.download = filename;
       link.click();
     }
   }", gsub("'", "\\\\'", titel)))))
                                )
                              )
    )

  out <- plot
  return(out)
}



# Box 3 ----
plot_fachkraft_detail_item  <- function(r) {

  timerange <- r$map_y_fachkraft_arbeit_detail
  bf_label <- r$map_bl_fachkraft_arbeit_detail
  this_beruf <- r$map_b_fachkraft_arbeit_detail

  if (length(this_beruf) == 0) {
    stop("this_beruf ist leer – SQL-Abfrage kann nicht ausgeführt werden.")
  }

  if (length(this_beruf) == 0) {
    # Wenn keine Berufe, dann leerer Output und frühzeitig raus
    plot_solidgauge_data <- data.frame()
    df <- data.frame()
  } else {
    # Berufsteil bauen je nach Anzahl
    if (length(this_beruf) > 1) {
      beruf_sql <- glue::glue_sql("AND beruf IN ({this_beruf*})", .con = con)
    } else {
      beruf_sql <- glue::glue_sql("AND beruf = {this_beruf}", .con = con)
    }

    # Erste Abfrage
    df_query <- glue::glue_sql("
    SELECT *
    FROM arbeitsmarkt_epa_detail
    WHERE jahr = {timerange}
      AND indikator = 'Engpassindikator'
      AND anforderung = {bf_label}
      {beruf_sql}
  ", .con = con)

    plot_solidgauge_data <- DBI::dbGetQuery(con, df_query)

    used_kategories <- switch(
      EXPR = plot_solidgauge_data$epa_kat[1],
      "Anzeichen eines Engpassberufs" = c("Engpassanalyse", "Risikoanalyse"),
      "Engpassberuf" = c("Engpassanalyse"),
      "kein Engpassberuf" = c("Engpassanalyse")
    )
    if (nrow(plot_solidgauge_data) == 0) {
      df <- data.frame()
      return()
    } else {
    # Zweite Abfrage
    df_query <- glue::glue_sql("
    SELECT indikator, kategorie, wert
    FROM arbeitsmarkt_epa_detail
    WHERE jahr = {timerange}
      AND anforderung = {bf_label}
      AND NOT indikator = 'Engpassindikator'
      AND kategorie IN ({used_kategories*})
      AND wert IS NOT NULL
      {beruf_sql}
  ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)
    }
  }


  plot_bar_data <- df %>%
    dplyr::mutate(wert = round(wert, 1))

  # color change on 0.01. level, since data labels are also rounded to 2 decimal places
  col_stops <- data.frame(
    q = c(0, 1.49, 1.50, 2, 2.01),
    c = c("#35BD97", "#35BD97", "#FBBF24", "#FBBF24", "#EE7775"),
    stringsAsFactors = FALSE
  )

  color_idx <- sapply(plot_bar_data$wert, function(x) {
    max(which((col_stops$q)<= x))
  })
  if(length(color_idx)>0) plot_bar_data$bar_color <- col_stops$c[color_idx]
  # divide by three (the maximum) to get percentage change values for the gauge plot
  col_stops$q <- col_stops$q / 3

  # titel
 beruf <- this_beruf

 if(!is.null(this_beruf)){
   if(this_beruf %in% c("Bau- und Gebäudetechnik", "Gesundheitstechnik",
                        "Informatik", "Landtechnik", "Mathematik, Naturwissenschaften",
                        "Nicht MINT", "Produktionstechnik", "Verkehr-, Sicherheits- und Veranstaltungstechnik",
                        "MINT")){
     beruf <- paste0("Berufe in ", beruf)
   }
   if(this_beruf == "Gesamt"){
     beruf <- paste0("alle Berufe")
   }
 }




 titel <- paste0("Engpassindikator für ", beruf,
                 " auf dem ", bf_label, "-Level ", timerange, ": ", plot_solidgauge_data$epa_kat)

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
      data = round(plot_solidgauge_data$wert, 1),
      dataLabels = list(
        y = -50,
        borderWidth = 0,
        useHTML = TRUE,
        style = list(
          fontFamily = "Calibri Regular",
          fontSize = "20px")
      )
    ) %>%
    highcharter::hc_title(
      text = paste0("Engpassindikator für ", beruf,
                    " auf dem ", bf_label, "-Level ", timerange, ": ", plot_solidgauge_data$epa_kat),
      margin = 10,
      align = "center",
      style = list(color = "black",
                   useHTML = TRUE,
                   fontFamily = "Calibri Regular",
                   fontSize = "20px")
    ) %>%
    highcharter::hc_caption(
      text = paste0("Werte < 1,5                : kein Fachkräfteengpass", br(),
                    "Werte zwischen 1,5 und 1,9 : Anzeichen eines Fachkräfteengpasses", br(),
                    "Werte >= 2,0               : Fachkräfteengpass", br(),
                    "<span style='font-size:11px; color:gray;'>Quelle der Daten: Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt.</span>"),
      align = "left",
      style = list(color = "grey",
                   useHTML = TRUE,
                   fontFamily = "Calibri Regular",
                   fontSize = "13px")
    ) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG", "downloadCSV",
                                                   list(
                                                     text = "Daten für GPT",
                                                     onclick = htmlwidgets::JS(sprintf(
                                                       "function () {
     var date = new Date().toISOString().slice(0,10);
     var chartTitle = '%s'.replace(/\\s+/g, '_');
     var filename = chartTitle + '_' + date + '.txt';

     var data = this.getCSV();
     var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
     if (window.navigator.msSaveBlob) {
       window.navigator.msSaveBlob(blob, filename);
     } else {
       var link = document.createElement('a');
       link.href = URL.createObjectURL(blob);
       link.download = filename;
       link.click();
     }
   }", gsub("'", "\\\\'", titel)))))
                                )
                              )
    )

  # count "Engpassanalyse" to add line afterward
  sep_line_risiko <- sum(plot_bar_data$kategorie == "Engpassanalyse") - 0.4
  sep_line_engpass <-  -0.4



  titel <- paste0("Einzelne Indikatoren der Engpassanalyse (gesamt ",
                  round(plot_solidgauge_data$wert, 1),
                  ") für Beruf ", this_beruf, " (", timerange, ")")


  ##NICHT IN BALKENBUILDER DA ES eine andere struktur hat, und komplizierter is

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
        dataLabels = list(enabled = TRUE, format = "{point.wert:.2f}",
                          style = list(textOutline = "none"))
      )) %>%
    highcharter::hc_tooltip(pointFormat = "Wert: {point.wert:.2f}") %>%
    highcharter::hc_yAxis(title = list(text = ""),
                          min = 0,
                          max = 3,
                          tickInterval = 1) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_title(
      text = paste0("Einzelne Indikatoren der Engpassanalyse (gesamt ",
                    round(plot_solidgauge_data$wert, 1),
                    ") für Beruf ", this_beruf, " (", timerange, ")"),
      margin = 45,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "Calibri Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
    highcharter::hc_caption(text = "Quelle der Daten: Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
                            style = list(fontSize = "11px", color = "gray")) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG", "downloadCSV",
                                                   list(
                                                     text = "Daten für GPT",
                                                     onclick = htmlwidgets::JS(sprintf(
                                                       "function () {
     var date = new Date().toISOString().slice(0,10);
     var chartTitle = '%s'.replace(/\\s+/g, '_');
     var filename = chartTitle + '_' + date + '.txt';

     var data = this.getCSV();
     var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
     if (window.navigator.msSaveBlob) {
       window.navigator.msSaveBlob(blob, filename);
     } else {
       var link = document.createElement('a');
       link.href = URL.createObjectURL(blob);
       link.download = filename;
       link.click();
     }
   }", gsub("'", "\\\\'", titel)))))
                                )
                              )
    )

  if ("Risikoanalyse" %in% plot_bar_data$kategorie) {

    titel <- ""



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
              )
            ),
            zIndex = 5 # Ensure the plot line is above the grid lines
          )
        ),
        title = list(text = "")) %>%
      highcharter::hc_caption(text = "Quelle der Daten: Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
                              style = list(fontSize = "11px", color = "gray")) %>%
      highcharter::hc_exporting(enabled = TRUE,
                                buttons = list(
                                  contextButton = list(
                                    menuItems = list("downloadPNG", "downloadCSV",
                                                     list(
                                                       text = "Daten für GPT",
                                                       onclick = htmlwidgets::JS(sprintf(
                                                         "function () {
     var date = new Date().toISOString().slice(0,10);
     var chartTitle = '%s'.replace(/\\s+/g, '_');
     var filename = chartTitle + '_' + date + '.txt';

     var data ='Quelle der Daten: Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt\\n\\n'+ this.getCSV();
     var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
     if (window.navigator.msSaveBlob) {
       window.navigator.msSaveBlob(blob, filename);
     } else {
       var link = document.createElement('a');
       link.href = URL.createObjectURL(blob);
       link.download = filename;
       link.click();
     }
   }", gsub("'", "\\\\'", titel)))))
                                  )
                                )
      )

  }

  out <- list(plot_left, plot_right)

  return(out)
}





plot_fachkraft_ranking_epa  <- function(r) {

  timerange <- r$fachkraft_ranking_epa_1
  bf_label <- r$fachkraft_ranking_epa_3

  this_beruf <- r$fachkraft_ranking_epa_2 #Alle Berufe, MINT-Berufe

  ###wenn Gesamt ausgewählt ist, kann man anforderung = Spezialist usw weglassen da man nach allem schaut


  if(this_beruf == "Alle Berufe"){


  if(bf_label == "Gesamt"){

    df_query <- glue::glue_sql("
      SELECT *
      FROM arbeitsmarkt_epa_detail
      WHERE jahr = {timerange}
      AND indikator = 'Engpassindikator'
      ORDER BY wert DESC
      LIMIT 30
    ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

    titel <- paste0("Die Berufe mit dem höchsten Engpassrisiko unter allen Berufsleveln in allen Berufsgruppen")

  }
  else{

    df_query <- glue::glue_sql("
      SELECT *
      FROM arbeitsmarkt_epa_detail
      WHERE jahr = {timerange}
        AND indikator = 'Engpassindikator'
        AND anforderung = {bf_label}
      ORDER BY wert DESC
      LIMIT 30
    ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

    titel <- paste0("Die Berufe mit dem höchsten Engpassrisiko unter ", bf_label ," in allen Berufsgruppen")

  }
  } else if (this_beruf == "MINT-Berufe"){


    if(bf_label == "Gesamt"){

      df_query <- glue::glue_sql("
      SELECT *
      FROM arbeitsmarkt_epa_detail
      WHERE jahr = {timerange}
      AND indikator = 'Engpassindikator'
      AND mint_zuordnung != 'Nicht MINT'
      ORDER BY wert DESC
      LIMIT 30
    ", .con = con)
      titel <- paste0("Die Berufe mit dem höchsten Engpassrisiko unter allen Berufsleveln in MINT-Berufen")

      df <- DBI::dbGetQuery(con, df_query)

    }
    else{

      df_query <- glue::glue_sql("
      SELECT *
      FROM arbeitsmarkt_epa_detail
      WHERE jahr = {timerange}
        AND indikator = 'Engpassindikator'
        AND anforderung = {bf_label}
      AND mint_zuordnung != 'Nicht MINT'
      ORDER BY wert DESC
      LIMIT 30
    ", .con = con)

      titel <- paste0("Die Berufe mit dem höchsten Engpassrisiko unter ", bf_label ," in MINT-Berufen")

      df <- DBI::dbGetQuery(con, df_query)

    }

  }
  #weitere arbeit
  wert <- df[10, "wert"]
  if (wert == df[11, "wert"]){

    df <- df[1:9,]
  } else if (wert > df[11, "wert"]){

    df <- df[1:10,]
  }



    quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    out <- balkenbuilder(df, titel, x="beruf", y="wert",group=NULL, tooltip = "Anzahl: {point.wert}", format = "{value:, f}", color = "#EE7775", quelle = quelle)




  return(out)
}




