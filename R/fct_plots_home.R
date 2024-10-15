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
home_einstieg <- function(r) {

  # load UI inputs from reactive value
  betrachtung <- r$ansicht_start_einstieg
  zeit <- r$date_start_comparison_mint
  #regio <- r$region_start_einstieg_comparsion -->fehlt noch in Arbeitsmarkt bei zentral nach Bulas
  indikator_choice_1 <- r$indikator_start_einstieg_1

  # filter dataset based on UI input

  df <- dplyr::tbl(con, from = "zentral") %>%
    dplyr::filter(jahr == zeit,
                  region == "Deutschland",
                  geschlecht=="Gesamt") %>%
   # dplyr::select(bereich, region, indikator, geschlecht, fachbereich, jahr, wert) %>%
    dplyr::select(bereich, indikator, fachbereich, wert) %>%
    dplyr::collect()

  df_hs <- df %>% dplyr::filter(bereich == "Hochschule")%>%
    dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle")

  df_s <- df %>% dplyr::filter(bereich == "Schule")%>%
    dplyr::filter(fachbereich== "MINT" | fachbereich == "Alle Fächer")%>%
    dplyr::mutate(indikator= paste0("Schüler:innen ", .$indikator ))

  df_s$fachbereich <- ifelse(grepl("Alle Fächer", df_s$fachbereich), "Alle", df_s$fachbereich)

  df_ab <- df %>%
    dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle"|
                      fachbereich == "Ingenieurwissenschaften" |
                      fachbereich == "Mathematik_Naturwissenschaften",
                  bereich != "Hochschule" & bereich != "Schule")%>%
    unique()


  dfy <- dplyr::bind_rows(df_s, df_hs, df_ab)%>%
    dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle")%>%
    tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
    dplyr::mutate("Nicht MINT" = Alle - MINT)

  #Tennen für Anzeige absoluter Werte
  ##neuen Df erstellen ohne weitere Berechnungen
  dfk2_wert1 <- dfy
  dfk2_wert1 <- dfk2_wert1 %>%
  #  dplyr::filter(geschlecht=="Gesamt")%>%
    dplyr::select(- Alle)%>%
    tidyr::pivot_longer(c(MINT, `Nicht MINT`), names_to = "fachbereich", values_to = "wert")

  #Anteil berechnen
  dfy <- dfy %>%
    dplyr::mutate(dplyr::across(c(MINT, `Nicht MINT`), ~./Alle*100)) %>%
  #  dplyr::filter(geschlecht=="Gesamt")%>%
    dplyr::select(- Alle)%>%
    tidyr::pivot_longer(c(MINT, `Nicht MINT`), names_to = "fachbereich", values_to = "proportion")

  #absolute Werte an DF mit Proportionen anhängen
  wert <- dfk2_wert1$wert
  dfk2_fn <- cbind(dfy, wert)

  #Trennpunkte für lange Zahlen ergänzen
  dfk2_fn$wert <- prettyNum(dfk2_fn$wert, big.mark = ".", decimal.mark = ",")


  dfk2_fn$proportion <- round(as.numeric(dfk2_fn$proportion),1)

#  dfk2_fn <- dfk2_fn[with(dfk2_fn, order(region, fachbereich, jahr, decreasing = TRUE)), ]
   dfk2_fn <- dfk2_fn[with(dfk2_fn, order( fachbereich, decreasing = TRUE)), ]

  #here only MINT
  dft <- dfk2_fn %>% dplyr::filter(fachbereich == "MINT")

  dfö <- dft %>% dplyr::filter(indikator %in% indikator_choice_1)

  if(betrachtung == "Einzelansicht - Kuchendiagramm"){

  if(length(indikator_choice_1) == 1) {

    # ensure that proportion sum to 1
    #df$proportion <- round_preserve_sum(as.numeric(df$proportion),0)

    title_help <- helper_title_home(indikator_choice_1)

    df <- dfk2_fn %>% dplyr::filter(indikator == indikator_choice_1)

    highcharter::hw_grid(
      df %>%
        highcharter::hchart(
          "pie", highcharter::hcaes(x = fachbereich, y = proportion)
        ) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f} % <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c( "#efe8e6", "#b16fab")) %>%
        highcharter::hc_title(text = paste0("", indikator_choice_1, " (", zeit, ")"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),

      ncol = 1,
      browsable = TRUE
    )


  } else if(length(indikator_choice_1) == 2) {

    # filter for UI input and ensure proportions sum to 1
    df_1 <- dfk2_fn %>% dplyr::filter(indikator == indikator_choice_1[1])

    # df_1$proportion <- round_preserve_sum(as.numeric(df_1$proportion),0)

    title_help_1 <- helper_title_home(indikator_choice_1[1])

    df_2 <- dfk2_fn %>% dplyr::filter(indikator == indikator_choice_1[2])

    # df_2$proportion <- round_preserve_sum(as.numeric(df_2$proportion),0)

    title_help_2 <- helper_title_home(indikator_choice_1[2])


    highcharter::hw_grid(
      highcharter::hchart(df_1, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f} % <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
        highcharter::hc_title(text = paste0("", indikator_choice_1[1], " (", zeit, ")"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),


      highcharter::hchart(df_2, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f} % <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
        highcharter::hc_title(text = paste0("", indikator_choice_1[2], " (", zeit, ")"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE)),

      ncol = 2,
      browsable = TRUE
    )


  } else if(length(indikator_choice_1) == 3) {

    # filter for UI input and ensure proportions sum to 1

    df_1 <- dfk2_fn %>% dplyr::filter(indikator == indikator_choice_1[1])

    # df_1$proportion <- round_preserve_sum(as.numeric(df_1$proportion),0)

    title_help_1 <- helper_title_home(indikator_choice_1[1])

    df_2 <- dfk2_fn %>% dplyr::filter(indikator == indikator_choice_1[2])

    # df_2$proportion <- round_preserve_sum(as.numeric(df_2$proportion),0)

    title_help_2 <- helper_title_home(indikator_choice_1[2])

    df_3 <- dfk2_fn %>% dplyr::filter(indikator == indikator_choice_1[3])

    # df_3$proportion <- round_preserve_sum(as.numeric(df_3$proportion),0)

    title_help_3 <- helper_title_home(indikator_choice_1[3])


    highcharter::hw_grid(
      highcharter::hchart(df_1, size = 170, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f} % <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
        highcharter::hc_title(text = paste0("", indikator_choice_1[1], " (", zeit, ")"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),


      highcharter::hchart(df_2, size = 170, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f} % <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
        highcharter::hc_title(text = paste0("", indikator_choice_1[2], " (", zeit, ")"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE)),

      highcharter::hchart(df_3, size = 170, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f} % <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
        highcharter::hc_title(text = paste0("", indikator_choice_1[3], " (", zeit, ")"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
        #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),



      ncol = 3,
      browsable = TRUE
    )

  }
    }else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){

    highcharter::hchart(dfk2_fn, 'bar', highcharter::hcaes(y = proportion, x = indikator, group = "fachbereich"))%>%
      highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  FALSE) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
      highcharter::hc_colors(c("#b16fab", "#efe8e6")) %>%
      # highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = paste0("Anteil von MINT nach Bildungsbereichen (", zeit,")"),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
      highcharter::hc_exporting(enabled = FALSE,
                                buttons = list(contextButton = list(
                                  symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                  onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                  align = 'right',
                                  verticalAlign = 'bottom',
                                  theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

  }

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
  t <- as.character(timerange[1]:timerange[2])

  absolut_selector <- r$abs_zahlen_start_multiple

  indikator_choice_1 <- r$indikator_start_multiple_1

  # filter dataset based on UI inputs
  df <- dplyr::tbl(con, from = "zentral") %>%
    dplyr::filter(
      #jahr >= timerange[1] & jahr <= timerange[2],
      jahr %in% t,
      region == "Deutschland",
      geschlecht=="Gesamt",
      fachbereich == "MINT" | fachbereich == "Alle"|  fachbereich == "Ingenieurwissenschaften" |fachbereich == "Mathematik_Naturwissenschaften"
      |fachbereich == "Alle Fächer") %>%
    dplyr::select(bereich, indikator, fachbereich, jahr, wert) %>%
    dplyr::collect()


  dfk2a2 <- df %>% dplyr::filter(bereich == "Hochschule")%>%
  dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle")


  dfk2c2 <- df %>% dplyr::filter(bereich == "Schule")%>%
    dplyr::filter(fachbereich== "MINT" | fachbereich == "Alle Fächer")%>%
    dplyr::mutate(indikator= paste0("Schüler:innen ", .$indikator ))


  dfk2c2$fachbereich <- ifelse(grepl("Alle Fächer", dfk2c2$fachbereich), "Alle", dfk2c2$fachbereich)

  dfk2b2 <- df %>% dplyr::filter(bereich != "Hochschule" & bereich != "Schule")%>%
    unique()

  dfk2_fn2 <- dplyr::bind_rows(dfk2b2, dfk2a2, dfk2c2)%>%
    dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle")%>%
    tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
    dplyr::mutate("Nicht MINT" = Alle - MINT)%>%
    dplyr::mutate(MINT_p= MINT/Alle*100)%>%
    dplyr::mutate("Nicht MINT_p" = `Nicht MINT`/Alle*100)%>%
    # dplyr::filter(geschlecht=="Gesamt")%>%
    dplyr::select(- Alle)%>%
    tidyr::pivot_longer(c(MINT, `Nicht MINT`, `Nicht MINT_p`, `MINT_p`), names_to = "fachbereich", values_to = "wert")%>%
    dplyr::mutate(selector=dplyr::case_when(stringr::str_ends(.$fachbereich, "_p") ~ "In Prozent",
                                            T~"Anzahl"))

  dfk2_fn2$fachbereich <- gsub("_p", "", dfk2_fn2$fachbereich)


  dfk2_fn2$wert <- ifelse(stringr::str_detect(dfk2_fn2$selector, "In Prozent"),round(as.numeric(dfk2_fn2$wert),1), dfk2_fn2$wert )


  if(absolut_selector=="In Prozent"){

    df <- dfk2_fn2 %>%
      dplyr::filter(selector=="In Prozent")

    df <- df[with(df, order(fachbereich, jahr, decreasing = FALSE)), ]

    #here only MINT
    df <- df %>% dplyr::filter(fachbereich == "MINT")

    df <- df %>% dplyr::filter(indikator %in% indikator_choice_1)

    # Ordnen der Legende
    sorted_indicators <- df %>%
      dplyr::group_by(indikator) %>%
      dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
      dplyr::arrange(desc(m_value)) %>%
      dplyr::pull(indikator)

    df$indikator <- factor(df$indikator, levels = sorted_indicators)

    # plot
    out <- highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group = indikator)) %>%
      highcharter::hc_tooltip(pointFormat = "Anteil MINT <br> Indikator: {point.indikator} <br> Anteil: {point.y} %") %>%
      highcharter::hc_yAxis(title = list(text = " "), labels = list(format = "{value}%"),
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = "Anteil von MINT nach Bildungsbereichen",
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf", "#fbbf24" )) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_exporting(enabled = FALSE,
                                buttons = list(contextButton = list(
                                  symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                  onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                  align = 'right',
                                  verticalAlign = 'bottom',
                                  theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

  } else if (absolut_selector=="Anzahl") {

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    #here only MINT
    dft <- dfk2_fn2 %>% dplyr::filter(fachbereich == "MINT")

    dfö <- dft %>% dplyr::filter(indikator %in% indikator_choice_1)


    df <- dfö %>%
      dplyr::filter(selector=="Anzahl")

    df <- df[with(df, order(fachbereich, jahr, decreasing = FALSE)), ]

    # Ordnen der Legende
    sorted_indicators <- df %>%
      dplyr::group_by(indikator) %>%
      dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
      dplyr::arrange(desc(m_value)) %>%
      dplyr::pull(indikator)

    df$indikator <- factor(df$indikator, levels = sorted_indicators)


    # plot
    out <- highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group = indikator)) %>%
      highcharter::hc_tooltip(pointFormat = "Anzahl: {point.y}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = "Anzahl von Personen in MINT nach Bildungsbereichen",
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf", "#fbbf24" )) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_exporting(enabled = FALSE,
                                buttons = list(contextButton = list(
                                  symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                  onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                  align = 'right',
                                  verticalAlign = 'bottom',
                                  theme = list(states = list(hover = list(fill = '#FFFFFF'))))))




  }

  return (out)
}



#' A function to plot a graph.
#'
#' @description A function to create a stacked bar chart
#'
#' @return The return value is a plot
#' @param df The dataframe "zentral.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
home_stacked_comparison_mint <- function(r) {

  # load UI inputs from reactive value
  timerange <- r$date_start_comparison_mint


  # filter dataset based on UI inputs
  df <- dplyr::tbl(con, from = "zentral") %>%
    dplyr::filter(jahr == timerange,
                  region == "Deutschland",
                  geschlecht=="Gesamt",
                  fachbereich == "MINT" | fachbereich == "Alle"|  fachbereich == "Ingenieurwissenschaften" |fachbereich == "Mathematik_Naturwissenschaften"
                  |fachbereich == "Alle Fächer") %>%
    dplyr::select(bereich, indikator, fachbereich, wert) %>%
    dplyr::collect()


  # call function to calculate the share of MINT for every "bereich"
  # df <- share_MINT(df)
  #
  # df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")
  #
  #
  # df <- df %>% dplyr::filter(indikator %in% indikator_choice_1)
  #
  # # calculate proportions
  # df <- df %>% dplyr::group_by(indikator, jahr) %>%
  #   dplyr::mutate(props = sum(wert))
  #
  #
  # df <- df %>% dplyr::group_by(indikator, jahr,fachbereich) %>%
  #   dplyr::summarize(proportion = wert/props)
  #
  # df$proportion <- df$proportion * 100

  # dfü <- df %>% dplyr::filter(jahr == timerange)

  # dfk <- dfü %>% dplyr::filter(region == "Deutschland")



  dfk2a3 <- df %>% dplyr::filter(bereich == "Hochschule")%>%
  dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle")


  dfk2c3 <- df %>% dplyr::filter(bereich == "Schule")%>%
    dplyr::filter(fachbereich== "MINT" | fachbereich == "Alle Fächer")%>%
    dplyr::mutate(indikator= paste0("Schüler:innen ", .$indikator ))


  dfk2c3$fachbereich <- ifelse(grepl("Alle Fächer", dfk2c3$fachbereich), "Alle", dfk2c3$fachbereich)

  dfk2b3 <- df %>% dplyr::filter(bereich != "Hochschule" & bereich != "Schule")%>%
    unique()



  dfk2_fn3 <- dplyr::bind_rows(dfk2b3, dfk2a3, dfk2c3)%>%
    dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle")%>%
    tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
    dplyr::mutate("Nicht MINT" = Alle - MINT)%>%
    dplyr::mutate(MINT_p= MINT/Alle*100)%>%
    dplyr::mutate("Nicht MINT_p" = `Nicht MINT`/Alle*100)%>%
    # dplyr::filter(geschlecht=="Gesamt")%>%
    dplyr::select(- Alle)%>%
    tidyr::pivot_longer(c(MINT, `Nicht MINT`, `Nicht MINT_p`, `MINT_p`), names_to = "fachbereich", values_to = "wert")%>%
    dplyr::mutate(selector=dplyr::case_when(stringr::str_ends(.$fachbereich, "_p") ~ "In Prozent",
                                            T~"Anzahl"))%>%
    dplyr::filter(indikator %in% c("Schüler:innen Leistungskurse", "Studierende",
                                   "Auszubildende", "Beschäftigte"))


  # order
  x <- ordered(factor(dfk2_fn3$indikator), levels=c("Schüler:innen Leistungskurse", "Studierende",
                                                    "Auszubildende", "Beschäftigte"))

  dfk2_fn3 <- dfk2_fn3[order(x),]

  #df[df$fachbereich != "MINT", "fachbereich"] <- "andere Fachbereiche"

  #Absoluten Wert speichern
  df_wert <- dfk2_fn3 %>%
    dplyr::filter(!(stringr::str_ends(.$fachbereich, "_p"))) %>%
    dplyr::rename(
      wert_abs = wert
    )

  wert_abs <- df_wert$wert_abs

  # Protenz filtern und Runden
  dfd3 <- dfk2_fn3 %>%
    dplyr::filter(stringr::str_ends(.$fachbereich, "_p"))%>%
    dplyr::mutate(fachbereich=dplyr::case_when(
      fachbereich== "Nicht MINT_p" ~ "Nicht MINT",
      fachbereich== "MINT_p" ~ "MINT"
    ))%>% dplyr::mutate(wert = round(.$wert,1))

  #Ansoluten Wert anhägnge
  dfd3 <- dfd3 %>% dplyr::left_join(df_wert, by = c("bereich","indikator", "fachbereich"))

  #Trennpunkte für lange Zahlen ergänzen
  dfd3$wert_abs <- prettyNum(dfd3$wert_abs, big.mark = ".", decimal.mark = ",")

  out <- highcharter::hchart(dfd3, 'bar', highcharter::hcaes(y = wert, x = indikator, group = "fachbereich"))%>%
    highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.y} % <br> Anzahl: {point.wert_abs}") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  FALSE) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
    highcharter::hc_colors(c("#b16fab", "#efe8e6")) %>%
    # highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil von MINT nach Bildungsbereichen (", timerange,")"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
    highcharter::hc_exporting(enabled = FALSE,
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

  return(out)
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
  indikator_choice_1_gender <- r$indikator_start_einstieg_1_gender
 #TODO Region mit rein, wenn in zentral ergänzt wurde

  # filter dataset based on UI input

  df <- dplyr::tbl(con, from = "zentral") %>%
    dplyr::filter(region == "Deutschland",
                  jahr == zeit) %>%
    dplyr::select(bereich, indikator, geschlecht, fachbereich, wert) %>%
    dplyr::collect()


  dfk24 <- df %>%
    dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle"|  fachbereich == "Ingenieurwissenschaften" |fachbereich == "Mathematik_Naturwissenschaften"
                  |fachbereich == "Alle Fächer")%>%
    dplyr::filter(indikator %in% c("Leistungskurse",
                                   "Studierende",
                                   "Auszubildende", "Beschäftigte"))

  dfk2a4 <- dfk24 %>% dplyr::filter(bereich == "Hochschule")%>%
  dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle") %>%
    tidyr::pivot_wider(names_from=geschlecht, values_from=wert)%>%
    dplyr::mutate(Männer=Gesamt-Frauen)%>%
    tidyr::pivot_longer(c("Männer", "Gesamt", "Frauen"), values_to = "wert", names_to="geschlecht")


  dfk2c4 <- df %>% dplyr::filter(bereich == "Schule")%>%
    dplyr::filter(fachbereich== "MINT" | fachbereich == "Alle Fächer")%>%
    dplyr::mutate(indikator= paste0("Schüler:innen ", .$indikator ))%>%
    tidyr::pivot_wider(names_from=geschlecht, values_from = wert)%>%
    dplyr::mutate(Gesamt = Frauen + Männer)%>%
    tidyr::pivot_longer(c("Gesamt", "Männer", "Frauen"), values_to = "wert", names_to = "geschlecht")

  dfk2c4$fachbereich <- ifelse(grepl("Alle Fächer", dfk2c4$fachbereich), "Alle", dfk2c4$fachbereich)

  dfk2b4 <- dfk24 %>% dplyr::filter(bereich != "Hochschule" & bereich != "Schule")%>%
    unique()%>%
    tidyr::pivot_wider(names_from=geschlecht, values_from=wert)%>%
    dplyr::mutate(Männer=Gesamt-Frauen)%>%
    tidyr::pivot_longer(c("Männer", "Gesamt", "Frauen"), values_to = "wert", names_to="geschlecht")



  dfk2_fn4 <- dplyr::bind_rows(dfk2b4, dfk2a4, dfk2c4)%>%
    dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle")%>%
    tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
    dplyr::mutate("andere Fächer" = Alle - MINT)%>%
    dplyr::select(- Alle)%>%
    tidyr::pivot_longer(c(MINT, `andere Fächer`), names_to = "fachbereich", values_to = "wert")%>%
    tidyr::pivot_wider(names_from = geschlecht, values_from = wert)

  #Datensatz kopieren, um absolute Werte zu behalten
  df_wert4 <- dfk2_fn4 %>%
    dplyr::select(- Gesamt)%>%
    tidyr::pivot_longer(c(Männer, Frauen), names_to = "geschlecht", values_to = "wert")

  #Berechnung des Anteils
  dfk2_fn4 <- dfk2_fn4 %>%
    dplyr::mutate(dplyr::across(c(Männer, Frauen), ~./Gesamt*100))%>%
    dplyr::select(- Gesamt)%>%
    tidyr::pivot_longer(c(Männer, Frauen), names_to = "geschlecht", values_to = "proportion")

  #absolute Werte anhängen
  wert4 <- df_wert4$wert
  dfk2_fn4 <- cbind(dfk2_fn4, wert4)

  # Indikator u25 mit NAs löschen und Runden
  dfk2_fn4 <- stats::na.omit(dfk2_fn4)
  dfk2_fn4$proportion <- round_preserve_sum(as.numeric(dfk2_fn4$proportion),1)

  #Trennpunkte für lange Zahlen ergänzen
  dfk2_fn4$wert <- prettyNum(dfk2_fn4$wert, big.mark = ".", decimal.mark = ",")

  #sortieren
  dfk2_fn4 <- dfk2_fn4[with(dfk2_fn4, order(fachbereich, decreasing = TRUE)), ]

  #Titel erstellen
  dfk2_fn4$titel_help <- "Schüler:innen in MINT-Leistungskursen"
  dfk2_fn4$titel_help <- ifelse(dfk2_fn4$indikator == "Beschäftigte", "MINT-Beschäftigte", dfk2_fn4$titel_help)
  dfk2_fn4$titel_help <- ifelse(dfk2_fn4$indikator == "Auszubildende", "MINT-Auszubildend", dfk2_fn4$titel_help)
  dfk2_fn4$titel_help <- ifelse(dfk2_fn4$indikator == "Studierende", "MINT-Studierende", dfk2_fn4$titel_help)

  dfk2_fn4$titel_help2 <- "Vergleich: Schüler:innen in anderen Leistungskursen"
  dfk2_fn4$titel_help2 <- ifelse(dfk2_fn4$indikator == "Beschäftigte", "Vergleich: Beschäftigte in anderen Bereichen", dfk2_fn4$titel_help2)
  dfk2_fn4$titel_help2 <- ifelse(dfk2_fn4$indikator == "Auszubildende", "Vergleich: Auszubildend in anderen Bereichen", dfk2_fn4$titel_help2)
  dfk2_fn4$titel_help2 <- ifelse(dfk2_fn4$indikator == "Studierende", "Vergleich: Studierende in anderen Bereichen", dfk2_fn4$titel_help2)


  if(betrachtung == "Einzelansicht - Kuchendiagramm"){

    #gewählte Indikatoren ausfiltern
    dfk2_fn4 <- dfk2_fn4 %>% dplyr::filter(indikator %in% indikator_choice_1_gender)

  if(length(indikator_choice_1_gender) == 1) {

    # ensure that proportion sum to 1
    df_mint4 <- dfk2_fn4 %>% dplyr::filter(fachbereich == "MINT")

    #df_mint$wert <- round_preserve_sum(as.numeric(df_mint$wert),0)


    df_rest4 <- dfk2_fn4 %>% dplyr::filter(fachbereich == "andere Fächer")

    #df_rest$wert <- round_preserve_sum(as.numeric(df_rest$wert),0)

    mint1 <-  highcharter::hchart(df_mint4, size = 280,
                          "pie", highcharter::hcaes(x = geschlecht, y = proportion)
      ) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0(df_mint4$titel_help[1], " (2022)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE))

    if(gegenwert == "Nein"){

     out <- mint1

    }
    if(gegenwert == "Ja"){
     nmint1 <- highcharter::hchart(df_rest4, size = 150,
                          "pie", highcharter::hcaes(x = geschlecht, y = proportion)
      ) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0(df_mint4$titel_help2[1], " (2022)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, y = -120) %>%
        # highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE,
                                               opacity = 0.7))

     out <- highcharter::hw_grid(
        mint1, nmint1,
        ncol = 2,
        browsable = TRUE
      )
    }


  } else if(length(indikator_choice_1_gender) == 2) {

    # filter for UI input and ensure proportions sum to 1
    df_mint4 <- dfk2_fn4 %>% dplyr::filter(fachbereich == "MINT")

    df_1_mint4 <- df_mint4 %>% dplyr::filter(indikator == indikator_choice_1_gender[1])

    #df_1_mint$wert <- round_preserve_sum(as.numeric(df_1_mint$wert),0)

    df_2_mint4 <- df_mint4 %>% dplyr::filter(indikator == indikator_choice_1_gender[2])

    #df_2_mint$wert <- round_preserve_sum(as.numeric(df_2_mint$wert),0)

    df_rest4 <- dfk2_fn4 %>% dplyr::filter(fachbereich == "andere Fächer")

    df_1_rest4 <- df_rest4 %>% dplyr::filter(indikator == indikator_choice_1_gender[1])

    #df_1_rest$wert <- round_preserve_sum(as.numeric(df_1_rest$wert),0)

    df_2_rest4<- df_rest4 %>% dplyr::filter(indikator == indikator_choice_1_gender[2])

    #df_2_rest$wert <- round_preserve_sum(as.numeric(df_2_rest$wert),0)


     mint1 <- highcharter::hchart(df_1_mint4, size = 280, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0(df_1_mint4$titel_help[1], " (2022)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE))


      mint2 <-highcharter::hchart(df_2_mint4, size = 280, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0(df_2_mint4$titel_help[1], " (2022)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE))
      if(gegenwert == "Nein"){

        out <- highcharter::hw_grid(
          mint1, mint2,
          ncol = 2,
          browsable = TRUE
        )
      }
      if(gegenwert == "Ja"){

      nmint1 <- highcharter::hchart(df_1_rest4, size = 150, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0(df_1_rest4$titel_help2[1], " (2022)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_legend(enabled = TRUE, y = -120, reversed = T) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE,
                                               opacity = 0.7))


     nmint2 <- highcharter::hchart(df_2_rest4, size = 150, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0(df_2_rest4$titel_help2[1], " (2022)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_legend(enabled = TRUE, y = -120, reversed = T) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE,
                                               opacity = 0.7))

     out <-  highcharter::hw_grid(
        mint1, mint2, nmint1, nmint2,
        ncol = 2,
        browsable = TRUE
      )
    }

    ### 3 Kreise ###

  } else if(length(indikator_choice_1_gender) == 3) {

    # filter for UI input and ensure proportions sum to 1
    # filter for UI input and ensure proportions sum to 1
    df_mint4 <- dfk2_fn4 %>% dplyr::filter(fachbereich == "MINT")

    df_1_mint4 <- df_mint4 %>% dplyr::filter(indikator == indikator_choice_1_gender[1])

    #df_1_mint$wert <- round_preserve_sum(as.numeric(df_1_mint$wert),0)

    df_2_mint4 <- df_mint4 %>% dplyr::filter(indikator == indikator_choice_1_gender[2])

    #df_2_mint$wert <- round_preserve_sum(as.numeric(df_2_mint$wert),0)

    df_3_mint4 <- df_mint4 %>% dplyr::filter(indikator == indikator_choice_1_gender[3])

    #df_3_mint$wert <- round_preserve_sum(as.numeric(df_3_mint$wert),0)

    df_rest4 <- dfk2_fn4 %>% dplyr::filter(fachbereich == "andere Fächer")

    df_1_rest4 <- df_rest4 %>% dplyr::filter(indikator == indikator_choice_1_gender[1])

    #df_1_rest$wert <- round_preserve_sum(as.numeric(df_1_rest$wert),0)

    df_2_rest4<- df_rest4 %>% dplyr::filter(indikator == indikator_choice_1_gender[2])

    #df_2_rest$wert <- round_preserve_sum(as.numeric(df_2_rest$wert),0)

    df_3_rest4<- df_rest4 %>% dplyr::filter(indikator == indikator_choice_1_gender[3])

    #df_3_rest$wert <- round_preserve_sum(as.numeric(df_3_rest$wert),0)


    mint1<-  highcharter::hchart(df_1_mint4, size = 170, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0(df_1_mint4$titel_help[1], " (2022)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE))


    mint2<-  highcharter::hchart(df_2_mint4, size = 170, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0(df_2_mint4$titel_help[1], " (2022)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE))

    mint3 <-  highcharter::hchart(df_3_mint4, size = 170, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0(df_3_mint4$titel_help[1], " (2022)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE))

    if(gegenwert == "Nein"){

      out <- highcharter::hw_grid(
        mint1, mint2, mint3,
        ncol = 3,
        browsable = TRUE
      )
    }
      ## Untere Kreise: Vergleiche

    if(gegenwert == "Ja"){

    nmint1 <-  highcharter::hchart(df_1_rest4, size = 100, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0(df_1_rest4$titel_help2[1], " (2022)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_legend(enabled = TRUE, y = -120, reversed = T) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE,
                                               opacity = 0.7))


    nmint2 <-  highcharter::hchart(df_2_rest4, size = 100, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0(df_2_rest4$titel_help2[1], " (2022)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_legend(enabled = TRUE, y = -120, reversed = T) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE,
                                               opacity = 0.7))

     nmint3 <- highcharter::hchart(df_3_rest4, size = 100, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0(df_3_rest4$titel_help2[1], " (2022)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_legend(enabled = TRUE, y = -120, reversed = T) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE,
                                               opacity = 0.7))


     out <- highcharter::hw_grid(
      mint1, mint2, mint3, nmint1, nmint2, nmint3,
      ncol = 3,
      browsable = TRUE
    )
    }

  }
  } else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){
    df <- dfk2_fn4 %>%
      dplyr::mutate(indikator = dplyr::case_when(
        fachbereich == "MINT" ~ paste0(indikator, " in MINT"),
        fachbereich == "andere Fächer" ~ paste0(indikator, " in anderen Bereichen")
      ))

    if(gegenwert == "Nein"){
      df <- df %>% dplyr::filter(fachbereich == "MINT")
      # ,
      # !fachbereich %in% c("Schüler:innen Grundkurse in MINT",
      #                     "Schüler:innen Leistungskurse in MINT")

      out <- highcharter::hchart(df, 'bar', highcharter::hcaes( x = indikator, y=round(proportion,1), group = geschlecht)) %>%
        highcharter::hc_tooltip(pointFormat = "{point.anzeige_geschlecht}Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  FALSE) %>%
        highcharter::hc_xAxis(title = list(text = ""), categories = c("Schüler:innen Oberstufenbelegungen in MINT",
                                                                      "Schüler:innen Grundkurse in MINT",
                                                                      "Schüler:innen Leistungskurse in MINT",
                                                                      "Studierende in MINT",
                                                                      "Auszubildende in MINT",
                                                                      "Beschäftigte in MINT")) %>%
        highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
        highcharter::hc_colors(c("#154194", "#efe8e6")) %>%
        highcharter::hc_title(text = paste0("Anteil von Frauen in MINT nach Bildungsbereichen (", zeit, ")"),
                              margin = 25,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
        ) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = FALSE) %>%
        highcharter::hc_exporting(enabled = FALSE,
                                  buttons = list(contextButton = list(
                                    symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                    onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                    align = 'right',
                                    verticalAlign = 'bottom',
                                    theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
    }else{

      out <- highcharter::hchart(df, 'bar', highcharter::hcaes( x = indikator, y=round(proportion,1), group = geschlecht)) %>%
        highcharter::hc_tooltip(pointFormat = "{point.anzeige_geschlecht}Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  FALSE) %>%
        highcharter::hc_xAxis(title = list(text = ""), categories = c("Schüler:innen Oberstufenbelegungen in MINT",
                                                                      "Schüler:innen Oberstufenbelegungen in anderen Bereichen",
                                                                      "Schüler:innen Grundkurse in MINT",
                                                                      "Schüler:innen Grundkurse in anderen Bereichen",
                                                                      "Schüler:innen Leistungskurse in MINT",
                                                                      "Schüler:innen Leistungskurse in anderen Bereichen",
                                                                      "Studierende in MINT",
                                                                      "Studierende in anderen Bereichen",
                                                                      "Auszubildende in MINT",
                                                                      "Auszubildende in anderen Bereichen",
                                                                      "Beschäftigte in MINT",
                                                                      "Beschäftigte in anderen Bereichen")) %>%
        highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
        highcharter::hc_colors(c("#154194", "#efe8e6")) %>%
        highcharter::hc_title(text = paste0("Anteil von Frauen in MINT nach Bildungsbereichen (", zeit, ")"),
                              margin = 25,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
        ) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = FALSE) %>%
        highcharter::hc_exporting(enabled = FALSE,
                                  buttons = list(contextButton = list(
                                    symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                    onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                    align = 'right',
                                    verticalAlign = 'bottom',
                                    theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

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
  t<- as.character(timerange[1]:timerange[2])

  indikator_choice <- r$indikator_start_comparison

  abs_selector <- r$abs_zahlen_start_comparison

  # filter dataset based on UI inputs
  df <- dplyr::tbl(con, from = "zentral") %>%
    dplyr::filter(jahr %in% t,
                  region == "Deutschland",
                  fachbereich == "MINT" | fachbereich == "Alle"|  fachbereich == "Ingenieurwissenschaften" |fachbereich == "Mathematik_Naturwissenschaften"
                  |fachbereich == "Alle Fächer",
                  indikator %in% c("Leistungskurse",
                                   "Studierende",
                                   "Auszubildende", "Beschäftigte")) %>%
    dplyr::select(bereich, indikator, fachbereich, geschlecht, jahr, wert) %>%
    dplyr::collect()

  dfa5 <- df %>% dplyr::filter(bereich == "Hochschule")%>%
  dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle") %>%
    tidyr::pivot_wider(names_from=geschlecht, values_from=wert)%>%
    dplyr::mutate(Männer=Gesamt-Frauen)%>%
    tidyr::pivot_longer(c("Männer", "Gesamt", "Frauen"), values_to = "wert", names_to="geschlecht")


  dfc5 <- df %>% dplyr::filter(bereich == "Schule")%>%
    dplyr::filter(fachbereich== "MINT" | fachbereich == "Alle Fächer")%>%
    dplyr::mutate(indikator= paste0("Schülerinnen ", .$indikator ))%>%
    tidyr::pivot_wider(names_from=geschlecht, values_from = wert)%>%
    dplyr::mutate(Gesamt = Frauen + Männer)%>%
    tidyr::pivot_longer(c("Gesamt", "Männer", "Frauen"), values_to = "wert", names_to = "geschlecht")

  dfc5$fachbereich <- ifelse(grepl("Alle Fächer", dfc5$fachbereich), "Alle", dfc5$fachbereich)

  dfb5 <- df %>% dplyr::filter(bereich != "Hochschule" & bereich != "Schule")%>%
    unique()%>%
    tidyr::pivot_wider(names_from=geschlecht, values_from=wert)%>%
    dplyr::mutate(Männer=Gesamt-Frauen)%>%
    tidyr::pivot_longer(c("Männer", "Gesamt", "Frauen"), values_to = "wert", names_to="geschlecht")



  df_fn5 <- dplyr::bind_rows(dfb5, dfc5, dfa5)%>%
    dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle")%>%
    tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
    dplyr::mutate("andere Fächer" = Alle - MINT)%>%
    dplyr::select(- Alle)%>%
    tidyr::pivot_longer(c(MINT, `andere Fächer`), names_to = "fachbereich", values_to = "wert")%>%
    tidyr::pivot_wider(names_from = geschlecht, values_from = wert)%>%
    dplyr::filter(indikator %in% c("Schülerinnen Leistungskurse", "Studierende",
                                   "Auszubildende", "Beschäftigte"))

  df_fn5_wert <- df_fn5 %>%
    dplyr::select(-Gesamt, - Männer)%>%
    tidyr::pivot_longer(Frauen, names_to = "geschlecht", values_to = "wert")%>%
    dplyr::mutate(selector = "Anzahl")

  #Berechnung des Anteils
  df_fn5_prop <- df_fn5 %>%
    dplyr::mutate(dplyr::across(c(Männer, Frauen), ~./Gesamt*100))%>%
    dplyr::select(- Gesamt, -Männer)%>%
    tidyr::pivot_longer(Frauen, names_to = "geschlecht", values_to = "wert")%>%
    dplyr::mutate(selector="In Prozent")

  df_fn51 <- dplyr::bind_rows(df_fn5_wert, df_fn5_prop)

  df_fn51 <- df_fn51[with(df_fn51, order(fachbereich, decreasing = TRUE)), ]


  #Trennpunkte für lange Zahlen ergänzen
  #df_fn5$proportion <- prettyNum(df_fn5$proportion, big.mark = ".", decimal.mark = ",")

  #sortieren


  df_fn51 <- df_fn51 %>%
    dplyr::filter(indikator %in% indikator_choice)%>%
    dplyr::filter(fachbereich == "MINT")
  df_fn51 <- df_fn51 %>%
    dplyr::arrange(desc(jahr))

  #

  if(abs_selector=="In Prozent"){

    df_fn51 <- df_fn51 %>%
      dplyr::filter(selector == "In Prozent")

    # Ordnen der Legende
    sorted_indicators <- df_fn51 %>%
      dplyr::group_by(indikator) %>%
      dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
      dplyr::arrange(desc(m_value)) %>%
      dplyr::pull(indikator)

    df_fn51$indikator <- factor(df_fn51$indikator, levels = sorted_indicators)

    # plot
    out <- highcharter::hchart(df_fn51, 'line', highcharter::hcaes(x = jahr, y = round(wert, 1), group = indikator))%>%
      highcharter::hc_tooltip(pointFormat = "Anteil Frauen <br> Indikator: {point.indikator} <br> Anteil: {point.y} %") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular"),
                            min = 10, max = 45) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular"), reversed = T) %>%
      #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = "Anteil von Frauen in MINT nach Bildungsbereichen",
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf", "#fbbf24" )) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_exporting(enabled = FALSE,
                                buttons = list(contextButton = list(
                                  symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                  onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                  align = 'right',
                                  verticalAlign = 'bottom',
                                  theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

  } else if (abs_selector =="Anzahl"){

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    df_fn51 <- df_fn51 %>%
      dplyr::filter(selector == "Anzahl")

    # Ordnen der Legende
    sorted_indicators <- df_fn51 %>%
      dplyr::group_by(indikator) %>%
      dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
      dplyr::arrange(desc(m_value)) %>%
      dplyr::pull(indikator)

    df_fn51$indikator <- factor(df_fn51$indikator, levels = sorted_indicators)

    out <- highcharter::hchart(df_fn51, 'line', highcharter::hcaes(x = jahr, y = wert, group = indikator))%>%
      highcharter::hc_tooltip(pointFormat = "Anzahl Frauen <br> Indikator: {point.indikator} <br> Anzahl: {point.y} ")%>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular"), reversed = T ) %>%
      #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = "Anzahl von Frauen in MINT nach Bildungsbereichen",
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf", "#fbbf24" )) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_exporting(enabled = FALSE,
                                buttons = list(contextButton = list(
                                  symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                  onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                  align = 'right',
                                  verticalAlign = 'bottom',
                                  theme = list(states = list(hover = list(fill = '#FFFFFF'))))))



  }


return (out)
}


#' A function to plot a graph.
#'
#' @description A function to create a stacked bar chart
#'
#' @return The return value is a plot
#' @param df The dataframe "zentral.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
home_stacked_comparison_gender <- function(r) {

  # load UI inputs from reactive value
  timerange <- r$date_start_comparison_mint_gender

  # filter dataset based on UI inputs
  df <- dplyr::tbl(con, from = "zentral") %>%
    dplyr::filter(region == "Deutschland",
                  jahr == timerange,
                  fachbereich == "MINT" | fachbereich == "Alle"|  fachbereich == "Ingenieurwissenschaften" |fachbereich == "Mathematik_Naturwissenschaften"
                  |fachbereich == "Alle Fächer",
                  indikator %in% c("Leistungskurse",
                                   "Studierende",
                                   "Auszubildende", "Beschäftigte")) %>%
    dplyr::select(bereich, indikator, geschlecht, fachbereich, wert) %>%
    dplyr::collect()


  df6a <- df %>% dplyr::filter(bereich == "Hochschule")%>%
  dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle") %>%
    tidyr::pivot_wider(names_from=geschlecht, values_from=wert)%>%
    dplyr::mutate(Männer=Gesamt-Frauen)%>%
    tidyr::pivot_longer(c("Männer", "Gesamt", "Frauen"), values_to = "wert", names_to="geschlecht")


  df6c <- df %>% dplyr::filter(bereich == "Schule")%>%
    dplyr::filter(fachbereich== "MINT" | fachbereich == "Alle Fächer")%>%
    dplyr::mutate(indikator= paste0("Schülerinnen ", .$indikator ))%>%
    tidyr::pivot_wider(names_from=geschlecht, values_from = wert)%>%
    dplyr::mutate(Gesamt = Frauen + Männer)%>%
    tidyr::pivot_longer(c("Gesamt", "Männer", "Frauen"), values_to = "wert", names_to = "geschlecht")

  df6c$fachbereich <- ifelse(grepl("Alle Fächer", df6c$fachbereich), "Alle", df6c$fachbereich)

  df6b <- df %>% dplyr::filter(bereich != "Hochschule" & bereich != "Schule")%>%
    unique()%>%
    tidyr::pivot_wider(names_from=geschlecht, values_from=wert)%>%
    dplyr::mutate(Männer=Gesamt-Frauen)%>%
    tidyr::pivot_longer(c("Männer", "Gesamt", "Frauen"), values_to = "wert", names_to="geschlecht")



  df6_fn <- dplyr::bind_rows(df6b, df6a, df6c)%>%
    dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle")%>%
    tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
    dplyr::mutate("andere Fächer" = Alle - MINT)%>%
    dplyr::select(- Alle)%>%
    tidyr::pivot_longer(c(MINT, `andere Fächer`), names_to = "fachbereich", values_to = "wert")%>%
    tidyr::pivot_wider(names_from = geschlecht, values_from = wert)

  #Trennen um Wert abzuspeichern
  df_wert <- df6_fn %>%
    dplyr::select(- Gesamt)%>%
    tidyr::pivot_longer(c(Männer, Frauen), names_to = "geschlecht", values_to = "wert")

  #Berechnung des Anteils
  df6_fn <- df6_fn %>%
    dplyr::mutate(dplyr::across(c(Männer, Frauen), ~./Gesamt*100))%>%
    dplyr::select(- Gesamt)%>%
    tidyr::pivot_longer(c(Männer, Frauen), names_to = "geschlecht", values_to = "proportion")

  #Wert anhängen
  df6_fn <- df6_fn %>% dplyr::left_join(df_wert, by = c("bereich","indikator",  "fachbereich", "geschlecht"))

  #Trennpunkte für lange Zahlen ergänzen
  df6_fn$wert <- prettyNum(df6_fn$wert, big.mark = ".", decimal.mark = ",")


  #sortieren
  df6_fn <- df6_fn[with(df6_fn, order(fachbereich, decreasing = TRUE)), ]

  #gewählte Indikatoren ausfiltern
  df6_fn <- df6_fn %>% dplyr::filter(indikator %in% c("Schülerinnen Leistungskurse", "Studierende",
                                                       "Auszubildende", "Beschäftigte"), fachbereich == "MINT")


  # plot
  hc_1 <- highcharter::hchart(df6_fn, 'bar', highcharter::hcaes( x = indikator, y=round(proportion,1), group = geschlecht)) %>%
    highcharter::hc_tooltip(pointFormat = "{point.anzeige_geschlecht}Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  FALSE) %>%
    highcharter::hc_xAxis(title = list(text = ""), categories = c("Leistungskurse", "Studierende",
                                                 "Auszubildende", "Beschäftigte")) %>%
    highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
    highcharter::hc_colors(c("#154194", "#efe8e6")) %>%
    highcharter::hc_title(text = paste0("Anteil von Frauen in MINT nach Bildungsbereichen (", timerange, ")"),
                          margin = 25,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = FALSE) %>%
    highcharter::hc_exporting(enabled = FALSE,
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))


  out <- hc_1

  return(out)

  # ggplot2::ggplot(df, ggplot2::aes(x=indikator, y=wert, fill = anzeige_geschlecht)) +
  #   ggplot2::geom_bar(stat="identity", position = "dodge") +
  #   ggplot2::geom_text(ggplot2::aes(label=paste(round(wert),"%"), vjust = - 0.25),
  #                      position=ggplot2::position_dodge(width=0.9),
  #                      fontface = "bold") +
  #   ggplot2::theme_minimal() +
  #   ggplot2::theme(
  #     text = ggplot2::element_text(size = 12),
  #     plot.title = ggtext::element_markdown(hjust = 0.5)) +
  #   ggplot2::xlab("") + ggplot2::ylab("Anteil") +
  #   ggplot2::scale_fill_manual(values = c("#154194","#efe8e6")) +
  #   ggplot2::labs(title = paste0(paste0("<span style='font-size:20.5pt; color:black'>",
  #                                "Anteil von Frauen in MINT nach Bildungsbereichen (", timerange, ")",
  #                                "<br><br><br>")),
  #                 fill = ""
  #                 ,
  #            # caption = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen."
  #             ) +
  #   ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"))

}

