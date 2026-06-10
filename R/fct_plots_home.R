# Wer wählt MINT ----

#' A function to plot a graph.
#'
#' @description A function to create a pie chart for the first box
#' inside the tab "Home".
#'
#' @return The return value is a plot
#' @param df The dataframe "zentral.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

#TODO nach merge aus Mariettas Funktion nur den Balken-Teil hierfür behalten
home_einstieg <- function(r) {

  # load UI inputs from reactive value
  betrachtung <- r$ansicht_start_einstieg
  zeit <- r$date_start_comparison_mint
  regio <- r$region_start_einstieg_comparsion
  indikator_choice_1 <- r$indikator_start_einstieg_1

  # filter dataset based on UI input

  query_df <- glue::glue_sql("
  SELECT bereich, indikator, fachbereich, wert
  FROM zentral
  WHERE jahr = {zeit}
    AND region = {regio}
    AND geschlecht = 'Gesamt'
    AND fachbereich IN ('MINT', 'Nicht MINT')
", .con = con)

  df <- DBI::dbGetQuery(con, query_df)

  query_df_alle <- glue::glue_sql("
  SELECT bereich, indikator, fachbereich, wert
  FROM zentral
  WHERE jahr = {zeit}
    AND region = {regio}
    AND geschlecht = 'Gesamt'
    AND fachbereich = 'Alle'
", .con = con)

  df_alle <- DBI::dbGetQuery(con, query_df_alle)

  df <- df %>%
    dplyr::left_join(df_alle, by = c("bereich", "indikator")) %>%
    dplyr::rename(wert = wert.x,
                  wert_ges = wert.y,
                  fachbereich = fachbereich.x) %>%
    dplyr::mutate(prop = round(wert / wert_ges * 100, 1)) %>%
    dplyr::select(-fachbereich.y, -wert_ges)



  #Trennpunkte für lange Zahlen ergänzen
  df$wert_besr <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
  df$prop_besr <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")

  if(betrachtung == "Einzelansicht - Kuchendiagramm"){

    df <- df %>%
    dplyr::filter(indikator %in% indikator_choice_1)

  if(length(indikator_choice_1) == 1) {

    if(nrow(df) == 0){

      # TODO leeren Plot mit Plotly umsetzten (Funktion dafür!)
      #bleibt hier da leer

      tooltip <- "Anzahl: {point.display_abs}"
      titel <- "Schüler:innendaten für 2024 sind noch nicht verfügbar."
      wert <- NA
      jahr <- NA
      df <- dplyr::data_frame(wert, jahr)
      out <- highcharter::hchart(df, 'line', highcharter::hcaes(x = reorder(jahr, wert), y = wert)) %>%
        highcharter::hc_tooltip(pointFormat = "Anzahl: {point.display_abs}") %>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular")) %>%
        highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE, style = list(fontFamily = "Calibri Regular")) %>%
        highcharter::hc_title(text = titel,
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px"))


    }else{

    if(indikator_choice_1 == "Leistungskurse") indikator_choice_1 <- "Schüler:innen im Leistungskurs"
    titel <- ifelse(regio == "Saarland",
                    paste0(indikator_choice_1, " im ", regio, " (", zeit, ")"),
                    paste0(indikator_choice_1, " in ", regio, " (", zeit, ")"))
    tooltip <- paste('Anteil: {point.prop_besr} % <br> Anzahl: {point.wert_besr}')

    quelle <- "Quellen: Destatis, 2025; Bundesagentur für Arbeit, 2025; KMK, 2025, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    out <- piebuilder(df,titel,  x = "fachbereich", y = "prop", tooltip, quelle = quelle)

    }
  } else if(length(indikator_choice_1) == 2) {

    # filter for UI input and ensure proportions sum to 1
    df_1 <- df %>% dplyr::filter(indikator == indikator_choice_1[1])
    if(indikator_choice_1[1] == "Leistungskurse") indikator_choice_1[1] <- "Schüler:innen im Leistungskurs"
    titel_1 <- ifelse(regio == "Saarland",
                      paste0(indikator_choice_1[1], " im ", regio, " (", zeit, ")"),
                      paste0(indikator_choice_1[1], " in ", regio, " (", zeit, ")"))
    quelle <- "Quellen: Destatis, 2025; Bundesagentur für Arbeit, 2025; KMK, 2025, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    df_2 <- df %>% dplyr::filter(indikator == indikator_choice_1[2])
    if(indikator_choice_1[2] == "Leistungskurse") indikator_choice_1[2] <- "Schüler:innen im Leistungskurs"
    titel_2 <- ifelse(regio == "Saarland",
                     paste0(indikator_choice_1[2], " im ", regio, " (", zeit, ")"),
                     paste0(indikator_choice_1[2], " in ", regio, " (", zeit, ")"))

    quelle <- "Quellen: Destatis, 2025; Bundesagentur für Arbeit, 2025; KMK, 2025, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    if(nrow(df_1) == 0){

      #es soll kein plot erstellt werden daher hier ein leeres lineplot, pie wurde übernommen unten
      titel1 <- "Schüler:innendaten für 2024 sind noch nicht verfügbar."
      wert <- NA
      jahr <- NA
      df_1 <- dplyr::data_frame(wert, jahr)
      out1 <- highcharter::hchart(df_1, 'line', highcharter::hcaes(x = reorder(jahr, wert), y = wert)) %>%
        highcharter::hc_tooltip(pointFormat = "Anzahl: {point.display_abs}") %>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular")) %>%
        highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE, style = list(fontFamily = "Calibri Regular")) %>%
        highcharter::hc_title(text = titel1,
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px"))


      y <- "prop"
      titel <- titel_2
      x <- "fachbereich"
      tooltip <- paste('Anteil: {point.prop_besr} % <br> Anzahl: {point.wert_besr}')
      color <-c("#b16fab", "#efe8e6")
      format <- '{point.prop_besr}%'
      subtitel <- NULL

      quelle <- "Quellen: Destatis, 2025; Bundesagentur für Arbeit, 2025; KMK, 2025, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt."

      out2 <- piebuilder(df_2, titel, x, y, tooltip, color, format, subtitel, quelle = quelle)


      # ou
    }else if(nrow(df_2) == 0){

    #empty plot

      titel2 <- "Schüler:innendaten für 2024 sind noch nicht verfügbar."
      wert <- NA
      jahr <- NA
      df_2 <- dplyr::data_frame(wert, jahr)
      out2 <- highcharter::hchart(df_2, 'line', highcharter::hcaes(x = reorder(jahr, wert), y = wert)) %>%
        highcharter::hc_tooltip(pointFormat = "Anzahl: {point.display_abs}") %>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular")) %>%
        highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(fontFamily = "Calibri Regular")) %>%
        highcharter::hc_title(text = titel2,
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px"))



      tooltip <- paste('Anteil: {point.prop_besr} % <br> Anzahl: {point.wert_besr}')
      color <- c( "#b16fab", "#efe8e6")
      format <- '{point.prop_besr}%'
      quelle <- "Quellen: Destatis, 2025; Bundesagentur für Arbeit, 2025; KMK, 2025, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt."
      out1 = piebuilder(df_1, titel_1, x="fachbereich", y="prop", tooltip, color, format, quelle = quelle)


    }else{

      df_1 <- df_1 %>%
        dplyr::mutate(
          tooltip = paste0(
            "<b>", indikator, "</b><br>",
            "Anteil: ", prop_besr, " %<br>",
            "Anzahl: ", wert_besr
          )
        )
      out1 <- piebuilder_plotly(df_1, titel = titel_1, x = "fachbereich", y = "prop", quelle = quelle)

      df_2 <- df_2 %>%
        dplyr::mutate(
          tooltip = paste0(
            "<b>", indikator, "</b><br>",
            "Anteil: ", prop_besr, " %<br>",
            "Anzahl: ", wert_besr
          )
        )
      out2 <- piebuilder(df_2, titel = titel_2, x = "fachbereich", y = "prop", quelle = quelle)

    }

    out <- highcharter::hw_grid(out1, out2, ncol = 2, browsable = TRUE)

    # if(exists("out_1") && exists("out_2")){
    #
    #   out <- highcharter::hw_grid(out_1, out_2, ncol = 2, browsable = TRUE)
    #
    # } else if(exists("out_1") && !exists("out_2")){
    #   out <- out_1
    # } else if(!exists("out_1") && exists("out_2")){
    #   out <- out_2
    # } else {
    #   out <- out
    # }




    }
  }
  else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){

   df <- df[with(df, order(prop, decreasing = TRUE)), ] ####
   titel <- ifelse(regio == "Saarland",
                   paste0("Anteil von MINT nach Bildungsbereichen im ", regio, " (", zeit,")"),
                   paste0("Anteil von MINT nach Bildungsbereichen in ", regio, " (", zeit,")"))


   title <- ifelse(regio == "Saarland",
                   paste0("Anteil von MINT nach Bildungsbereichen im ", regio, " (", zeit,")"),
                   paste0("Anteil von MINT nach Bildungsbereichen in ", regio, " (", zeit,")"))

   #dies kann nicht als systemvariable übergeben werden also group

   out <- highcharter::hchart(df, 'bar', highcharter::hcaes(y = prop, x = indikator, group = "fachbereich"))%>%
      highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.prop_besr}% <br> Anzahl: {point.wert_besr}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  FALSE) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
      highcharter::hc_colors(c("#b16fab", "#efe8e6")) %>%
      highcharter::hc_title(text = titel,
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
     highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
     highcharter::hc_caption(text = "Quellen: Destatis, 2025; Bundesagentur für Arbeit, 2025; KMK, 2025, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
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

     var data = 'Titel: %s\\n' + this.getCSV();
     data += '\\nQuelle: Quellen: Destatis, 2025; Bundesagentur für Arbeit, 2025; KMK, 2025, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt.';


     var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
     if (window.navigator.msSaveBlob) {
       window.navigator.msSaveBlob(blob, filename);
     } else {
       var link = document.createElement('a');
       link.href = URL.createObjectURL(blob);
       link.download = filename;
       link.click();
     }
   }", gsub("'", "\\\\'", titel), gsub("'", "\\\\'", titel)))))
                                 )
                               )
     )


  }

  return(out)
}

home_einstieg_pie <- function(r,
                                indi = NULL) {

  # load UI inputs from reactive value
  zeit <- r$date_start_comparison_mint
  regio <- r$region_start_einstieg_comparsion

  if(is.null(indi)){
    indikator_choice_1 <- r$indikator_start_einstieg_1
  }else{
    indikator_choice_1 <- indi
  }


  # filter dataset based on UI input

  query_df <- glue::glue_sql("
  SELECT bereich, indikator, fachbereich, wert
  FROM zentral
  WHERE jahr = {zeit}
    AND region = {regio}
    AND geschlecht = 'Gesamt'
    AND fachbereich IN ('MINT', 'Nicht MINT')
  ", .con = con)

  df <- DBI::dbGetQuery(con, query_df)

  query_df_alle <- glue::glue_sql("
  SELECT bereich, indikator, fachbereich, wert
  FROM zentral
  WHERE jahr = {zeit}
    AND region = {regio}
    AND geschlecht = 'Gesamt'
    AND fachbereich = 'Alle'
  ", .con = con)

  df_alle <- DBI::dbGetQuery(con, query_df_alle)

  df <- df %>%
    dplyr::left_join(df_alle, by = c("bereich", "indikator")) %>%
    dplyr::rename(wert = wert.x,
                  wert_ges = wert.y,
                  fachbereich = fachbereich.x) %>%
    dplyr::mutate(prop = round(wert / wert_ges * 100, 1)) %>%
    dplyr::select(-fachbereich.y, -wert_ges)

    #Trennpunkte für lange Zahlen ergänzen
    df$wert_besr <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$prop_besr <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")


    df <- df %>%
      dplyr::filter(indikator %in% indikator_choice_1)


    if(indikator_choice_1 == "Leistungskurse") indikator_choice_1 <- "Schüler:innen im Leistungskurs"
    titel <- ifelse(regio == "Saarland",
                    paste0(indikator_choice_1, " im ", regio, " (", zeit, ")"),
                    paste0(indikator_choice_1, " in ", regio, " (", zeit, ")"))

    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", indikator, "</b><br>",
          "Anteil: ", prop_besr, " %<br>",
          "Anzahl: ", wert_besr
        )
      )

    daten_quelle <- ifelse(indikator_choice_1 == "Leistungskurse", "KMK, 2025",
                           ifelse(indikator_choice_1 == "Studierende", "Destatis, 2025",
                                  "Bundesagentur für Arbeit, 2025"))
    quelle <- paste0(daten_quelle, " auf Anfrage, eigene Berechnungen durch MINTvernetzt")

    out <- piebuilder_plotly(df,titel,  x = "fachbereich", y = "prop", quelle = quelle)

  return(out)
}


#' A function to plot a graph.
#'
#' @description A function to create a line chart for the third box
#' inside the tab "Home".
#'
#' @return The return value is a plot
#' @param df The dataframe "zentral.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

home_rest_mint_verlauf <- function(r) {

  # load UI inputs from reactive value
  timerange <- r$date_start_multiple
  t <- timerange[1]:timerange[2]
  absolut_selector <- r$abs_zahlen_start_multiple
  indikator_choice_1 <- r$indikator_start_multiple_1
  regio <- r$region_start_multiple

  query_df <- glue::glue_sql("
  SELECT bereich, indikator, fachbereich, jahr, wert
  FROM zentral
  WHERE jahr IN ({t*})
    AND region = {regio}
    AND geschlecht = 'Gesamt'
    AND fachbereich = 'MINT'
    AND indikator IN ({indikator_choice_1*})
", .con = con)

  df <- DBI::dbGetQuery(con, query_df)

  if(absolut_selector=="In Prozent"){

    query_df_alle <- glue::glue_sql("
  SELECT bereich, indikator, fachbereich, jahr, wert
  FROM zentral
  WHERE jahr IN ({t*})
    AND region = {regio}
    AND geschlecht = 'Gesamt'
    AND fachbereich = 'Alle'
    AND indikator IN ({indikator_choice_1*})
", .con = con)

    df_alle <- DBI::dbGetQuery(con, query_df_alle)

    df <- df %>%
      dplyr::left_join(df_alle, by = c("bereich", "indikator", "jahr")) %>%
      dplyr::rename(wert = wert.x,
                    wert_ges = wert.y,
                    fachbereich = fachbereich.x) %>%
      dplyr::mutate(prop = round(wert / wert_ges * 100, 1)) %>%
      dplyr::select(-fachbereich.y, -wert_ges)


    df <- df[with(df, order(fachbereich, jahr, decreasing = FALSE)), ]
    #Trennpunkte für lange Zahlen ergänzen
    df$wert_besr <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$prop_besr <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")

    # Ordnen der Legende
    sorted_indicators <- df %>%
      dplyr::group_by(indikator) %>%
      dplyr::summarize(m_value = mean(round(prop, 1), na.rm = TRUE)) %>%
      dplyr::arrange(m_value) %>%
      dplyr::pull(indikator)

    df$indikator <- factor(df$indikator, levels = sorted_indicators)

    # plot
    titel1 <- ifelse(regio == "Saarland",
                     paste0("MINT-Anteil nach Bildungsbereichen im ", regio),
                     paste0("MINT-Anteil nach Bildungsbereichen in ", regio))

    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", indikator, "</b><br>",
          "Jahr: ", jahr, "<br>",
          "Anteil: ", prettyNum(prop, big.mark = ".", decimal.mark = ","), " %"
        )
      )

    color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24")

    quelle <- "Quellen: Destatis, 2025; Bundesagentur für Arbeit, 2025; KMK, 2025, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    out <- linebuilder_plotly(df, titel1, x="jahr", y="prop", group="indikator",
                              color = color, quelle = quelle)

  } else if (absolut_selector=="Anzahl") {

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

    # plot
    titel = ifelse(regio == "Saarland",
                   paste0("Personen in MINT nach Bildungsbereichen im ", regio),
                   paste0("Personen in MINT nach Bildungsbereichen in ", regio))

    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", indikator, "</b><br>",
          "Jahr: ", jahr, "<br>",
          "Anzahl: ", wert_besr
        )
      )

    format <- ",d"
    color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24")
    quelle <- "Quellen: Destatis, 2025; Bundesagentur für Arbeit, 2025; KMK, 2025, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    out <- linebuilder_plotly(df, titel, x = "jahr", y="wert", group = "indikator",
                              format = format, color = color, quelle = quelle)

  }

  return (out)
}



# Frauen in MINT ----

#' A function to plot a graph.
#'
#' @description A function to create a pie chart for the first box
#' inside the tab "Home".
#'
#' @return The return value is a plot
#' @param df The dataframe "zentral.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
home_einstieg_gender <- function(r) {

  # load UI inputs from reactive value
  betrachtung <- r$ansicht_start_comparison_mint_gender
  zeit <- r$date_start_comparison_mint_gender
  gegenwert <- r$gegenwert_start_comparison_gender
  indi <- r$indikator_start_einstieg_1_gender
  regio <- r$regio_start_comparison_gender

  praep <- ifelse(regio == "Saarland", "im", "in")

  # filter dataset based on UI input

  query_df <- glue::glue_sql("
  SELECT bereich, indikator, fachbereich, geschlecht, wert
  FROM zentral
  WHERE jahr = {zeit}
    AND region = {regio}
    AND geschlecht IN ('Frauen', 'Männer')
    AND fachbereich IN ('MINT', 'Nicht MINT')
", .con = con)

  df <- DBI::dbGetQuery(con, query_df)

  query_df_alle <- glue::glue_sql("
  SELECT bereich, indikator, fachbereich, geschlecht, wert
  FROM zentral
  WHERE jahr = {zeit}
    AND region = {regio}
    AND geschlecht = 'Gesamt'
    AND fachbereich IN ('MINT', 'Nicht MINT')
", .con = con)

  df_alle <- DBI::dbGetQuery(con, query_df_alle)


  if("Leistungskurse" %in% indi & regio == "Deutschland" |
     betrachtung == "Gruppenvergleich - Balkendiagramm" & regio == "Deutschland"){

    #Baden-Würrtemberg rausrechnen, da dort keine Geschlechter erfasst werden
    query_df_alle_bw <- glue::glue_sql("
      SELECT bereich, indikator, fachbereich, geschlecht, wert
      FROM zentral
      WHERE jahr = {zeit}
        AND region = 'Baden-Württemberg'
        AND geschlecht = 'Gesamt'
        AND fachbereich IN ('MINT', 'Nicht MINT')
        AND bereich = 'Schule'
    ", .con = con)

    df_alle_bw <- DBI::dbGetQuery(con, query_df_alle_bw)


    df_alle_schule <- df_alle[df_alle$bereich == "Schule",] %>%
      dplyr::left_join(df_alle_bw, by = c("bereich", "indikator", "fachbereich", "geschlecht")) %>%
      dplyr::mutate(wert.x = wert.x - wert.y) %>%
      dplyr::select(-wert.y) %>%
      dplyr::rename(wert = wert.x)

    df_alle <- df_alle %>%
      dplyr::filter(bereich != "Schule") %>%
      rbind(df_alle_schule)

  }

  df <- df %>%
    dplyr::left_join(df_alle, by = c("bereich", "indikator", "fachbereich")) %>%
    dplyr::rename(wert = wert.x,
                  wert_ges = wert.y,
                  geschlecht = geschlecht.x) %>%
    dplyr::mutate(prop = round(wert / wert_ges * 100, 1)) %>%
    dplyr::select(-geschlecht.y, -wert_ges)

  if("Leistungskurse" %in% indi){
    df$indikator[df$indikator == "Leistungskurse"] <- "Schüler:innen im Leistungskurs"
    indi[indi == "Leistungskurse"] <- "Schüler:innen im Leistungskurs"
      }

  #Trennpunkte für lange Zahlen ergänzen
  df$wert_besr <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
  df$prop_besr <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")

  #sortieren
  df <- df[with(df, order(geschlecht, decreasing = TRUE)), ]

  #Titel erstellen
  df$titel_help <- "Schüler:innen in MINT-Leistungskursen"
  df$titel_help <- ifelse(df$indikator == "Beschäftigte", "MINT-Beschäftigte", df$titel_help)
  df$titel_help <- ifelse(df$indikator == "Auszubildende", "MINT-Auszubildende", df$titel_help)
  df$titel_help <- ifelse(df$indikator == "Studierende", "MINT-Studierende", df$titel_help)

  df$titel_help2 <- "Vergleich: Schüler:innen in anderen Leistungskursen"
  df$titel_help2 <- ifelse(df$indikator == "Beschäftigte", "Vergleich: Beschäftigte in anderen Bereichen", df$titel_help2)
  df$titel_help2 <- ifelse(df$indikator == "Auszubildende", "Vergleich: Auszubildende in anderen Bereichen", df$titel_help2)
  df$titel_help2 <- ifelse(df$indikator == "Studierende", "Vergleich: Studierende in anderen Bereichen", df$titel_help2)


  if(betrachtung == "Einzelansicht - Kuchendiagramm"){

    df <- df %>%
      dplyr::filter(indikator %in% indi)

  if(length(indi) == 1){

    df_mint <- df %>% dplyr::filter(fachbereich == "MINT")
    df_rest <- df %>% dplyr::filter(fachbereich == "Nicht MINT")

    titel <- paste0(df_mint$titel_help[1], " ", praep, " ", regio, " (", zeit, ")")
    color = c("#efe8e6", "#154194")
    daten_quelle <- ifelse(indi == "Leistungskurse", "KMK, 2025",
                           ifelse(indi == "Studierende", "Destatis, 2025",
                                  "Bundesagentur für Arbeit, 2025"))
    quelle <- paste0(daten_quelle, " auf Anfrage, eigene Berechnungen durch MINTvernetzt")

    df_mint <- df_mint %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", geschlecht, "</b><br>",
          "Anteil: ", prop_besr, " %<br>",
          "Anzahl: ", wert_besr
        )
      )

    mint1 <- piebuilder_plotly(df_mint, titel = titel,
                               x = "geschlecht", y = "prop", color = color,
                               quelle = quelle)

    if(gegenwert == "Nein"){

     out <- mint1

    }
    if(gegenwert == "Ja"){

     titel <- ""
     subtitel <- paste0(df_mint$titel_help2[1], " ", praep, " ", regio, " (", zeit, ")")
     daten_quelle <- ifelse(indi == "Leistungskurse", "KMK, 2025",
                            ifelse(indi == "Studierende", "Destatis, 2025",
                                   "Bundesagentur für Arbeit, 2025"))
     quelle <- paste0(daten_quelle, " auf Anfrage, eigene Berechnungen durch MINTvernetzt")
     df_rest <- df_rest %>%
       dplyr::mutate(
         tooltip = paste0(
           "<b>", geschlecht, "</b><br>",
           "Anteil: ", prop_besr, " %<br>",
           "Anzahl: ", wert_besr
         )
       )
     nmint1 <- piebuilder_plotly(df_rest, titel, x = "geschlecht", y = "prop",
                                 color = color, quelle = quelle,
                                 subtitel = subtitel)

     out <- list(mint1, nmint1)

    }
  }else if(length(indi) == 2) {

    # filter for UI input and ensure proportions sum to 1
    df_mint <- df %>% dplyr::filter(fachbereich == "MINT")
    df_1_mint <- df_mint %>% dplyr::filter(indikator == indi[1])
    df_2_mint <- df_mint %>% dplyr::filter(indikator == indi[2])

    df_rest <- df %>% dplyr::filter(fachbereich == "Nicht MINT")
    df_1_rest <- df_rest %>% dplyr::filter(indikator == indi[1])
    df_2_rest<- df_rest %>% dplyr::filter(indikator == indi[2])

    titel <- paste0(df_2_mint$titel_help[1], " ", praep, " ", regio, " (", zeit, ")")

    daten_quelle <- ifelse(indi[1] == "Leistungskurse", "KMK, 2025",
                           ifelse(indi[1] == "Studierende", "Destatis, 2025",
                                  "Bundesagentur für Arbeit, 2025"))
    quelle <- paste0(daten_quelle, " auf Anfrage, eigene Berechnungen durch MINTvernetzt")

    df_1_mint <- df_1_mint %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", geschlecht, "</b><br>",
          "Anteil: ", prop_besr, " %<br>",
          "Anzahl: ", wert_besr
        )
      )

    mint1 <- piebuilder_plotly(df_1_mint, titel = paste0(df_1_mint$titel_help[1], " ", praep, " ", regio, " (", zeit, ")"),
                               x = "geschlecht", y = "prop", c("#efe8e6", "#154194"),
                               quelle = quelle)

    daten_quelle <- ifelse(indi[2] == "Leistungskurse", "KMK, 2025",
                           ifelse(indi[2] == "Studierende", "Destatis, 2025",
                                  "Bundesagentur für Arbeit, 2025"))
    quelle <- paste0(daten_quelle, " auf Anfrage, eigene Berechnungen durch MINTvernetzt")

    df_2_mint <- df_2_mint %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", geschlecht, "</b><br>",
          "Anteil: ", prop_besr, " %<br>",
          "Anzahl: ", wert_besr
        )
      )

    mint2 <- piebuilder_plotly(df_2_mint, titel = paste0(df_2_mint$titel_help[1], " ", praep, " ", regio, " (", zeit, ")"),
                               x = "geschlecht", y = "prop", c("#efe8e6", "#154194"),
                               quelle = quelle)


      if(gegenwert == "Nein"){

        out <- list(mint1, mint2)

      } else if(gegenwert == "Ja"){

        daten_quelle <- ifelse(indi[1] == "Leistungskurse", "KMK, 2025",
                               ifelse(indi[1] == "Studierende", "Destatis, 2025",
                                      "Bundesagentur für Arbeit, 2025"))
        quelle <- paste0(daten_quelle, " auf Anfrage, eigene Berechnungen durch MINTvernetzt")

        df_1_rest <- df_1_rest %>%
          dplyr::mutate(
            tooltip = paste0(
              "<b>", geschlecht, "</b><br>",
              "Anteil: ", prop_besr, " %<br>",
              "Anzahl: ", wert_besr
            )
          )
        titel <- ""
        subtitel <- paste0(df_1_rest$titel_help2[1], " ", praep, " ", regio, " (", zeit, ")")
         nmint1 <- piebuilder_plotly(df_1_rest, titel = titel, subtitel = subtitel,
                             x = "geschlecht", y = "prop",
                             c("#efe8e6", "#154194"), quelle = quelle)

         daten_quelle <- ifelse(indi[2] == "Leistungskurse", "KMK, 2025",
                                ifelse(indi[2] == "Studierende", "Destatis, 2025",
                                       "Bundesagentur für Arbeit, 2025"))
         quelle <- paste0(daten_quelle, " auf Anfrage, eigene Berechnungen durch MINTvernetzt")

         df_2_rest <- df_2_rest %>%
           dplyr::mutate(
             tooltip = paste0(
               "<b>", geschlecht, "</b><br>",
               "Anteil: ", prop_besr, " %<br>",
               "Anzahl: ", wert_besr
             )
           )
         titel <- ""
         subtitel <- paste0(df_2_rest$titel_help2[1], " ", praep, " ", regio, " (", zeit, ")")

         nmint2 <- piebuilder_plotly(df_2_rest, titel = titel, subtitel = subtitel,
                              x = "geschlecht", y = "prop",
                              c("#efe8e6", "#154194"), quelle = quelle)

         out <- list(mint1, mint2, nmint1, nmint2)
      }

    }
  }
  else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){

    df <- df %>%
      dplyr::mutate(indikator = dplyr::case_when(
        fachbereich == "MINT" ~ paste0(indikator, " in MINT"),
        fachbereich == "Nicht MINT" ~ paste0(indikator, " in anderen Bereichen")
      ))

    if(gegenwert == "Nein"){
      df <- df %>% dplyr::filter(fachbereich == "MINT")

      df <- df[with(df, order(prop, decreasing = TRUE)), ]


   #   titel <- paste0("Anteil von Frauen in MINT nach Bildungsbereichen in ", regio, " (", zeit, ")")

 #     out <- highcharter::hchart(df, 'bar', highcharter::hcaes( x = indikator, y=prop, group = geschlecht)) %>%
 #       highcharter::hc_tooltip(pointFormat = "{point.anzeige_geschlecht}Anteil: {point.prop_besr} % <br> Anzahl: {point.wert_besr}") %>%
 #       highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  FALSE) %>%
 #       highcharter::hc_xAxis(title = list(text = "")) %>%
 #       highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
 #       highcharter::hc_colors(c("#154194", "#efe8e6")) %>%
 #       highcharter::hc_title(text = paste0("Anteil von Frauen in MINT nach Bildungsbereichen in ", regio, " (", zeit, ")"),
 #                             margin = 25,
 #                             align = "center",
 #                             style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
 #       highcharter::hc_chart(
 #         style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
 #       ) %>%
 #       highcharter::hc_legend(enabled = TRUE, reversed = FALSE) %>%
 #       highcharter::hc_exporting(enabled = TRUE,
 #                                 buttons = list(
 #                                   contextButton = list(
 #                                     menuItems = list("downloadPNG", "downloadCSV",
 #                                                      list(
 #                                                        text = "Daten für GPT",
 #                                                        onclick = htmlwidgets::JS(sprintf(
 #                                                          "function () {
 #    var date = new Date().toISOString().slice(0,10);
 #    var chartTitle = '%s'.replace(/\\s+/g, '_');
 #    var filename = chartTitle + '_' + date + '.txt';
 #
 #    var data = this.getCSV();
 #    var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
 #    if (window.navigator.msSaveBlob) {
 #      window.navigator.msSaveBlob(blob, filename);
 #    } else {
 #      var link = document.createElement('a');
 #      link.href = URL.createObjectURL(blob);
 #      link.download = filename;
 #      link.click();
 #    }
 #  }", gsub("'", "\\\\'", titel)))))
  #                                  )
  #                                )
  #      )

      x <- "indikator"
      y <- "prop"
      group <- "geschlecht"
      tooltip <- "{point.anzeige_geschlecht}Anteil: {point.prop_besr} % <br> Anzahl: {point.wert_besr}"
      stacking <- "percent"
      titel <- paste0("Anteil von Frauen in MINT nach Bildungsbereichen ", praep, " ", regio, " (", zeit, ")")
      reversed <- FALSE
      format <- "{value}%"

      colors <- c("#154194", "#efe8e6")

      quelle <- "Quellen: Destatis, 2025; Bundesagentur für Arbeit, 2025; KMK, 2025, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt."

      out <- balkenbuilder(df, titel, x, y, group, tooltip, format="{value}%", color = colors, reverse=reversed, stacking = stacking, quelle = quelle)




    }else{

      df <- df[with(df, order(prop, decreasing = TRUE)), ]


      titel <- paste0("Anteil von Frauen in MINT nach Bildungsbereichen ", praep, " ", regio, " (", zeit, ")")

      ##balkenbuilder nicht verwendet wegen dem sonderfall Kategorie

      out <- highcharter::hchart(df, 'bar', highcharter::hcaes(x = indikator, y = prop, group = geschlecht)) %>%
        highcharter::hc_tooltip(pointFormat = "{point.anzeige_geschlecht} Anteil: {point.prop_besr} % <br> Anzahl: {point.wert_besr}") %>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), reversedStacks = FALSE) %>%
        highcharter::hc_xAxis(
          title = list(text = ""),
          categories = c(
            "Leistungskurse in MINT",
            "Leistungskurse in anderen Bereichen",
            "Studierende in MINT",
            "Studierende in anderen Bereichen",
            "Auszubildende in MINT",
            "Auszubildende in anderen Bereichen",
            "Beschäftigte in MINT",
            "Beschäftigte in anderen Bereichen"
          )
        ) %>%
        highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
        highcharter::hc_colors(c("#154194", "#efe8e6")) %>%

        highcharter::hc_title(text = titel,
                              margin = 25,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "Calibri Regular", fontSize = "14px")
        ) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = FALSE) %>%
        highcharter::hc_caption(text = "Quellen: Destatis, 2025; Bundesagentur für Arbeit, 2025; KMK, 2025, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
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


     var data = 'Titel: %s\\n' + this.getCSV();
     data += '\\nQuelle: Quellen: Destatis, 2025; Bundesagentur für Arbeit, 2025; KMK, 2025, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt.';


     var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
     if (window.navigator.msSaveBlob) {
       window.navigator.msSaveBlob(blob, filename);
     } else {
       var link = document.createElement('a');
       link.href = URL.createObjectURL(blob);
       link.download = filename;
       link.click();
     }
   }", gsub("'", "\\\\'", titel),gsub("'", "\\\\'", titel)))))
                                    )
                                  )
        )



    }


  }


  return(out)
}



#' A function to plot a graph.
#'
#' @description A function to create a line chart
#'
#' @return The return value is a plot
#' @param df The dataframe "zentral.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
home_comparison_line <- function(r) {

  # load UI inputs from reactive value
  timerange <- r$date_start_comparison
  t<- timerange[1]:timerange[2]
  regio <- r$regio_start_comparison
  indikator_choice <- r$indikator_start_comparison
  abs_selector <- r$abs_zahlen_start_comparison

  praep <- ifelse(regio == "Saarland", "im", "in")


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


  if(abs_selector=="In Prozent"){

    query_df_alle <- glue::glue_sql("
  SELECT bereich, indikator, fachbereich, geschlecht, jahr, wert
  FROM zentral
  WHERE jahr IN ({t*})
    AND region IN ({regio*})
    AND NOT geschlecht = 'Gesamt'
    AND fachbereich = 'MINT'
    AND indikator IN ({indikator_choice*})
", .con = con)

    df_alle <- DBI::dbGetQuery(con, query_df_alle)


    df_alle <- df_alle %>%
      dplyr::group_by(bereich, indikator, fachbereich, jahr) %>%
      dplyr::reframe(
        wert = sum(wert, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(geschlecht = "Gesamt") %>%
      dplyr::select(bereich, indikator, fachbereich, geschlecht, jahr, wert)


    df <- df %>%
      dplyr::left_join(df_alle, by = c("indikator", "jahr", "fachbereich", "bereich")) %>%
      dplyr::mutate(prop = round(wert.x/wert.y * 100, 1)) %>%
      dplyr::rename(geschlecht = geschlecht.x,
                    wert = wert.x) %>%
      dplyr::select(-c(wert.y, geschlecht.y))


    df <- df[with(df, order(indikator, jahr, decreasing = FALSE)), ]

    sorted_indicators <- df %>%
      dplyr::group_by(indikator) %>%
      dplyr::summarize(m_value = mean(round(prop, 1), na.rm = TRUE)) %>%
      dplyr::arrange(m_value) %>%
      dplyr::pull(indikator)


    df$indikator <- factor(df$indikator, levels = sorted_indicators)

    #Trennpunkte für lange Zahlen ergänzen
    df$wert_besr <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$prop_besr <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")

    # plot

    titel <- paste0("Anteil von Frauen in MINT nach Bildungsbereichen ", praep, " ", regio)

    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", indikator, "</b><br>",
          "Jahr: ", jahr, "<br>",
          "Anteil: ", prop_besr, " %"
        )
      )

    color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24")

    quelle <- "Quellen: Destatis, 2025; Bundesagentur für Arbeit, 2025; KMK, 2025, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    out <- linebuilder_plotly(df, titel, x = "jahr", y = "prop", group="indikator",
                              color = color, quelle = quelle)


  } else if (abs_selector =="Anzahl"){


    df <- df[with(df, order(indikator, jahr, decreasing = FALSE)), ]

    # Ordnen der Legende
    sorted_indicators <- df %>%
      dplyr::group_by(indikator) %>%
      dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
      dplyr::arrange(desc(m_value)) %>%
      dplyr::pull(indikator)

    df$indikator <- factor(df$indikator, levels = sorted_indicators)

    titel <- paste0("Anteil von Frauen in MINT nach Bildungsbereichen ", praep, " ", regio)

    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", indikator, "</b><br>",
          "Jahr: ", jahr, "<br>",
          "Anzahl: ", prettyNum(wert, big.mark = ".", decimal.mark = ",")
        )
      )

    format <- ",d"

    color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24" )
    quelle <- "Quellen: Destatis, 2025; Bundesagentur für Arbeit, 2025; KMK, 2025, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    out <- linebuilder_plotly(df,titel,x="jahr", y="wert", group="indikator",
                       format = format, color = color, quelle = quelle)



  }


return (out)
}




