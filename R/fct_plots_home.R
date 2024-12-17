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
  regio <- r$region_start_einstieg_comparsion
  indikator_choice_1 <- r$indikator_start_einstieg_1

  # filter dataset based on UI input

  df <- dplyr::tbl(con, from = "zentral") %>%
    dplyr::filter(jahr == zeit,
                  region == regio,
                  geschlecht=="Gesamt",
                  fachbereich %in% c("MINT", "Nicht MINT")) %>%
   # dplyr::select(bereich, region, indikator, geschlecht, fachbereich, jahr, wert) %>%
    dplyr::select(bereich, indikator, fachbereich, wert) %>%
    dplyr::collect()

  df_alle <- dplyr::tbl(con, from = "zentral") %>%
    dplyr::filter(jahr == zeit,
                  region == regio,
                  geschlecht=="Gesamt",
                  fachbereich == "Alle") %>%
    # dplyr::select(bereich, region, indikator, geschlecht, fachbereich, jahr, wert) %>%
    dplyr::select(bereich, indikator, fachbereich, wert) %>%
    dplyr::collect()

  df <- df %>%
    dplyr::left_join(df_alle, by = c("bereich", "indikator")) %>%
    dplyr::rename(wert = wert.x,
                  wert_ges = wert.y,
                  fachbereich = fachbereich.x) %>%
    dplyr::mutate(prop = round(wert/wert_ges*100,1)) %>%
    dplyr::select(-fachbereich.y, -wert_ges)


  #Trennpunkte für lange Zahlen ergänzen
  df$wert_besr <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
  df$prop_besr <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")

  if(betrachtung == "Einzelansicht - Kuchendiagramm"){

    df <- df %>%
    dplyr::filter(indikator %in% indikator_choice_1)

  if(length(indikator_choice_1) == 1) {

    if(nrow(df) == 0){
      titel <- "Schüler:innendaten für 2023 sind noch nicht verfügbar."
      df$jahr <- NA
      out <- highcharter::hchart(df, 'line', highcharter::hcaes(x = reorder(jahr, wert), y = wert, group = indikator)) %>%
        highcharter::hc_tooltip(pointFormat = "Anzahl: {point.display_abs}") %>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
        highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
        highcharter::hc_title(text = titel,
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px"))

    }else{

    if(indikator_choice_1 == "Leistungskurse") indikator_choice_1 <- "Schüler:innen im Leistungskurs"
    titel <- paste0(indikator_choice_1, " in ", regio, " (", zeit, ")")

      out <-  highcharter::hchart(
          df, "pie", highcharter::hcaes(x = fachbereich, y = prop)
        ) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.prop_besr} % <br> Anzahl: {point.wert_besr}')) %>%
        highcharter::hc_colors(c("#b16fab", "#efe8e6")) %>%
        highcharter::hc_title(text = titel,
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.prop_besr}%'), showInLegend = TRUE))

    }
  } else if(length(indikator_choice_1) == 2) {

    # filter for UI input and ensure proportions sum to 1
    df_1 <- df %>% dplyr::filter(indikator == indikator_choice_1[1])
    if(indikator_choice_1[1] == "Leistungskurse") indikator_choice_1[1] <- "Schüler:innen im Leistungskurs"
    titel_1 <- paste0(indikator_choice_1[1], " in ", regio, " (", zeit, ")")

    df_2 <- df %>% dplyr::filter(indikator == indikator_choice_1[2])
    if(indikator_choice_1[2] == "Leistungskurse") indikator_choice_1[2] <- "Schüler:innen im Leistungskurs"
    titel_2 <- paste0(indikator_choice_1[2], " in ", regio, " (", zeit, ")")

    if(nrow(df_1) == 0){
      titel1 <- "Schüler:innendaten für 2023 sind noch nicht verfügbar."
      df_1$jahr <- NA
      out1 <- highcharter::hchart(df_1, 'line', highcharter::hcaes(x = reorder(jahr, wert), y = wert, group = indikator)) %>%
        highcharter::hc_tooltip(pointFormat = "Anzahl: {point.display_abs}") %>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
        highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
        highcharter::hc_title(text = titel1,
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px"))
      out2 <- highcharter::hchart(df_2, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = prop)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.prop_besr} % <br> Anzahl: {point.wert_besr}')) %>%
        highcharter::hc_colors(c("#b16fab", "#efe8e6")) %>%
        highcharter::hc_title(text = titel_2,
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.prop_besr}%'), showInLegend = TRUE))


    }else if(nrow(df_2) == 0){
      titel2 <- "Schüler:innendaten für 2023 sind noch nicht verfügbar."
      df_2$jahr <- NA
      out2 <- highcharter::hchart(df_2, 'line', highcharter::hcaes(x = reorder(jahr, wert), y = wert, group = indikator)) %>%
        highcharter::hc_tooltip(pointFormat = "Anzahl: {point.display_abs}") %>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
        highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
        highcharter::hc_title(text = titel2,
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px"))
      out1 <- highcharter::hchart(df_1, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = prop)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.prop_besr} % <br> Anzahl: {point.wert_besr}')) %>%
        highcharter::hc_colors(c( "#b16fab", "#efe8e6")) %>%
        highcharter::hc_title(text = titel_1,
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.prop_besr}%'), showInLegend = TRUE))


    }else{

      out1 <- highcharter::hchart(df_1, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = prop)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.prop_besr} % <br> Anzahl: {point.wert_besr}')) %>%
        highcharter::hc_colors(c( "#b16fab", "#efe8e6")) %>%
        highcharter::hc_title(text = titel_1,
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.prop_besr}%'), showInLegend = TRUE))


      out2 <- highcharter::hchart(df_2, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = prop)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.prop_besr} % <br> Anzahl: {point.wert_besr}')) %>%
        highcharter::hc_colors(c("#b16fab", "#efe8e6")) %>%
        highcharter::hc_title(text = titel_2,
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.prop_besr}%'), showInLegend = TRUE))

    }

    out <-highcharter::hw_grid(out1, out2,
                         ncol = 2, browsable = TRUE)

    }
  }
  else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){
   if(zeit == 2023){subtitel <- paste0("Schüler:innendaten für 2023 liegen noch nicht vor.")}else{
     subtitel <- ""
   }

   df <- df[with(df, order(prop, decreasing = TRUE)), ] ################################
   out <- highcharter::hchart(df, 'bar', highcharter::hcaes(y = prop, x = indikator, group = "fachbereich"))%>%
      highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.prop_besr}% <br> Anzahl: {point.wert_besr}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  FALSE) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
      highcharter::hc_colors(c("#b16fab", "#efe8e6")) %>%
      # highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = paste0("Anteil von MINT nach Bildungsbereichen in ", regio, " (", zeit,")"),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
     highcharter::hc_subtitle(text = subtitel,
                              margin = 20,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
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

  df <- dplyr::tbl(con, from = "zentral") %>%
    dplyr::filter(jahr %in% t,
                  region == regio,
                  geschlecht=="Gesamt",
                  fachbereich %in% c("MINT"),
                  indikator %in% indikator_choice_1) %>%
    dplyr::select(bereich, indikator, fachbereich, jahr, wert) %>%
    dplyr::collect()

  if(absolut_selector=="In Prozent"){

    df_alle <- dplyr::tbl(con, from = "zentral") %>%
      dplyr::filter(jahr %in% t,
                    region == regio,
                    geschlecht=="Gesamt",
                    fachbereich == "Alle",
                    indikator %in% indikator_choice_1) %>%
      dplyr::select(bereich, indikator, fachbereich, jahr, wert) %>%
      dplyr::collect()

    df <- df %>%
      dplyr::left_join(df_alle, by = c("bereich", "indikator", "jahr")) %>%
      dplyr::rename(wert = wert.x,
                    wert_ges = wert.y,
                    fachbereich = fachbereich.x) %>%
      dplyr::mutate(prop = round(wert/wert_ges*100,1)) %>%
      dplyr::select(-fachbereich.y, -wert_ges)

    df <- df[with(df, order(fachbereich, jahr, decreasing = FALSE)), ]
    #Trennpunkte für lange Zahlen ergänzen
    df$wert_besr <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$prop_besr <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")

    # Ordnen der Legende
    sorted_indicators <- df %>%
      dplyr::group_by(indikator) %>%
      dplyr::summarize(m_value = mean(round(prop, 1), na.rm = TRUE)) %>%
      dplyr::arrange(desc(m_value)) %>%
      dplyr::pull(indikator)

    df$indikator <- factor(df$indikator, levels = sorted_indicators)

    # plot
    out <- highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = prop, group = indikator)) %>%
      highcharter::hc_tooltip(pointFormat = "Anteil MINT <br> Indikator: {point.indikator} <br> Anteil: {point.prop_besr} %") %>%
      highcharter::hc_yAxis(title = list(text = " "), labels = list(format = "{value}%"),
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = paste0("MINT-Anteil nach Bildungsbereichen in ", regio),
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
    out <- highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group = indikator)) %>%
      highcharter::hc_tooltip(pointFormat = "Anzahl: {point.wert_besr}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = paste0("Personen in MINT nach Bildungsbereichen in ", regio),
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
# home_stacked_comparison_mint <- function(r) {
#
#   # load UI inputs from reactive value
#   timerange <- r$date_start_comparison_mint
#
#
#   # filter dataset based on UI inputs
#   df <- dplyr::tbl(con, from = "zentral") %>%
#     dplyr::filter(jahr == timerange,
#                   region == "Deutschland",
#                   geschlecht=="Gesamt",
#                   fachbereich == "MINT" | fachbereich == "Alle"|  fachbereich == "Ingenieurwissenschaften" |fachbereich == "Mathematik_Naturwissenschaften"
#                   |fachbereich == "Alle Fächer") %>%
#     dplyr::select(bereich, indikator, fachbereich, wert) %>%
#     dplyr::collect()
#
#
#   # call function to calculate the share of MINT for every "bereich"
#   # df <- share_MINT(df)
#   #
#   # df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")
#   #
#   #
#   # df <- df %>% dplyr::filter(indikator %in% indikator_choice_1)
#   #
#   # # calculate proportions
#   # df <- df %>% dplyr::group_by(indikator, jahr) %>%
#   #   dplyr::mutate(props = sum(wert))
#   #
#   #
#   # df <- df %>% dplyr::group_by(indikator, jahr,fachbereich) %>%
#   #   dplyr::summarize(proportion = wert/props)
#   #
#   # df$proportion <- df$proportion * 100
#
#   # dfü <- df %>% dplyr::filter(jahr == timerange)
#
#   # dfk <- dfü %>% dplyr::filter(region == "Deutschland")
#
#
#
#   dfk2a3 <- df %>% dplyr::filter(bereich == "Hochschule")%>%
#   dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle")
#
#
#   dfk2c3 <- df %>% dplyr::filter(bereich == "Schule")%>%
#     dplyr::filter(fachbereich== "MINT" | fachbereich == "Alle Fächer")%>%
#     dplyr::mutate(indikator= paste0("Schüler:innen ", .$indikator ))
#
#
#   dfk2c3$fachbereich <- ifelse(grepl("Alle Fächer", dfk2c3$fachbereich), "Alle", dfk2c3$fachbereich)
#
#   dfk2b3 <- df %>% dplyr::filter(bereich != "Hochschule" & bereich != "Schule")%>%
#     unique()
#
#
#
#   dfk2_fn3 <- dplyr::bind_rows(dfk2b3, dfk2a3, dfk2c3)%>%
#     dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle")%>%
#     tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
#     dplyr::mutate("Nicht MINT" = Alle - MINT)%>%
#     dplyr::mutate(MINT_p= MINT/Alle*100)%>%
#     dplyr::mutate("Nicht MINT_p" = `Nicht MINT`/Alle*100)%>%
#     # dplyr::filter(geschlecht=="Gesamt")%>%
#     dplyr::select(- Alle)%>%
#     tidyr::pivot_longer(c(MINT, `Nicht MINT`, `Nicht MINT_p`, `MINT_p`), names_to = "fachbereich", values_to = "wert")%>%
#     dplyr::mutate(selector=dplyr::case_when(stringr::str_ends(.$fachbereich, "_p") ~ "In Prozent",
#                                             T~"Anzahl"))%>%
#     dplyr::filter(indikator %in% c("Schüler:innen Leistungskurse", "Studierende",
#                                    "Auszubildende", "Beschäftigte"))
#
#
#   # order
#   x <- ordered(factor(dfk2_fn3$indikator), levels=c("Schüler:innen Leistungskurse", "Studierende",
#                                                     "Auszubildende", "Beschäftigte"))
#
#   dfk2_fn3 <- dfk2_fn3[order(x),]
#
#   #df[df$fachbereich != "MINT", "fachbereich"] <- "andere Fachbereiche"
#
#   #Absoluten Wert speichern
#   df_wert <- dfk2_fn3 %>%
#     dplyr::filter(!(stringr::str_ends(.$fachbereich, "_p"))) %>%
#     dplyr::rename(
#       wert_abs = wert
#     )
#
#   wert_abs <- df_wert$wert_abs
#
#   # Protenz filtern und Runden
#   dfd3 <- dfk2_fn3 %>%
#     dplyr::filter(stringr::str_ends(.$fachbereich, "_p"))%>%
#     dplyr::mutate(fachbereich=dplyr::case_when(
#       fachbereich== "Nicht MINT_p" ~ "Nicht MINT",
#       fachbereich== "MINT_p" ~ "MINT"
#     ))%>% dplyr::mutate(wert = round(.$wert,1))
#
#   #Ansoluten Wert anhägnge
#   dfd3 <- dfd3 %>% dplyr::left_join(df_wert, by = c("bereich","indikator", "fachbereich"))
#
#   #Trennpunkte für lange Zahlen ergänzen
#   dfd3$wert_abs <- prettyNum(dfd3$wert_abs, big.mark = ".", decimal.mark = ",")
#
#   out <- highcharter::hchart(dfd3, 'bar', highcharter::hcaes(y = wert, x = indikator, group = "fachbereich"))%>%
#     highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.y} % <br> Anzahl: {point.wert_abs}") %>%
#     highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  FALSE) %>%
#     highcharter::hc_xAxis(title = list(text = "")) %>%
#     highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
#     highcharter::hc_colors(c("#b16fab", "#efe8e6")) %>%
#     # highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
#     highcharter::hc_title(text = paste0("Anteil von MINT nach Bildungsbereichen (", timerange,")"),
#                           margin = 45,
#                           align = "center",
#                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#     highcharter::hc_chart(
#       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
#     ) %>%
#     highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
#     highcharter::hc_exporting(enabled = FALSE,
#                               buttons = list(contextButton = list(
#                                 symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
#                                 onclick = highcharter::JS("function () {
#                                                               this.exportChart({ type: 'image/png' }); }"),
#                                 align = 'right',
#                                 verticalAlign = 'bottom',
#                                 theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
#
#   return(out)
# }

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

  # filter dataset based on UI input

  df <- dplyr::tbl(con, from = "zentral") %>%
    dplyr::filter(jahr == zeit,
                  region == regio,
                  geschlecht %in% c("Frauen", "Männer"),
                  fachbereich %in% c("MINT", "Nicht MINT")) %>%
    # dplyr::select(bereich, region, indikator, geschlecht, fachbereich, jahr, wert) %>%
    dplyr::select(bereich, indikator, fachbereich, geschlecht, wert) %>%
    dplyr::collect()

  df_alle <- dplyr::tbl(con, from = "zentral") %>%
    dplyr::filter(jahr == zeit,
                  region == regio,
                  geschlecht == "Gesamt",
                  fachbereich %in% c("MINT", "Nicht MINT")) %>%
    # dplyr::select(bereich, region, indikator, geschlecht, fachbereich, jahr, wert) %>%
    dplyr::select(bereich, indikator, fachbereich, geschlecht, wert) %>%
    dplyr::collect()

  if("Leistungskurse" %in% indi & regio == "Deutschland"){

    #Baden-Würrtemberg rausrechnen, da dort keine Geschlechter erfasst werden
    df_alle_bw <- dplyr::tbl(con, from = "zentral") %>%
      dplyr::filter(jahr == zeit,
                    region == "Baden-Württemberg",
                    geschlecht == "Gesamt",
                    fachbereich %in% c("MINT", "Nicht MINT"),
                    bereich == "Schule") %>%
      # dplyr::select(bereich, region, indikator, geschlecht, fachbereich, jahr, wert) %>%
      dplyr::select(bereich, indikator, fachbereich, geschlecht, wert) %>%
      dplyr::collect()

    df_alle_schule <- df_alle[df_alle$bereich == "Schule",] %>%
      dplyr::left_join(df_alle_bw, by = c("bereich", "indikator", "fachbereich",
                                          "geschlecht")) %>%
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
    dplyr::mutate(prop = round(wert/wert_ges*100,1)) %>%
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

  if(length(indi) == 1) {

    if(nrow(df) == 0){
      titel <- "Schüler:innendaten für 2023 sind noch nicht verfügbar."
      df$jahr <- NA
      out <- highcharter::hchart(df, 'line', highcharter::hcaes(x = reorder(jahr, wert), y = wert, group = indikator)) %>%
        highcharter::hc_tooltip(pointFormat = "Anzahl: {point.display_abs}") %>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
        highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
        highcharter::hc_title(text = titel,
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px"))

    }else{


    df_mint <- df %>% dplyr::filter(fachbereich == "MINT")
    #df_mint$wert <- round_preserve_sum(as.numeric(df_mint$wert),0)

    df_rest <- df %>% dplyr::filter(fachbereich == "Nicht MINT")
    #df_rest$wert <- round_preserve_sum(as.numeric(df_rest$wert),0)

    mint1 <-  highcharter::hchart(df_mint, size = 280,
                          "pie", highcharter::hcaes(x = geschlecht, y = prop)
      ) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.prop_besr}% <br> Anzahl: {point.wert_besr}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0(df_mint$titel_help[1], " in ", regio, "(", zeit, ")"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.prop_besr}%'), showInLegend = TRUE))

    if(gegenwert == "Nein"){

     out <- mint1

    }
    if(gegenwert == "Ja"){
     nmint1 <- highcharter::hchart(df_rest, size = 150,
                          "pie", highcharter::hcaes(x = geschlecht, y = prop)
      ) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.prop_besr}% <br> Anzahl: {point.wert_besr}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0(df_mint$titel_help2[1], " in ", regio, " (", zeit, ")"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, y = -120) %>%
        # highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.prop_besr}%'), showInLegend = TRUE,
                                               opacity = 0.7))

     out <- highcharter::hw_grid(
        mint1, nmint1,
        ncol = 2,
        browsable = TRUE
      )
    }
  }

  } else if(length(indi) == 2) {

    # filter for UI input and ensure proportions sum to 1
    df_mint <- df %>% dplyr::filter(fachbereich == "MINT")
    df_1_mint <- df_mint %>% dplyr::filter(indikator == indi[1])
    #df_1_mint$wert <- round_preserve_sum(as.numeric(df_1_mint$wert),0)

    df_2_mint <- df_mint %>% dplyr::filter(indikator == indi[2])
    #df_2_mint$wert <- round_preserve_sum(as.numeric(df_2_mint$wert),0)

    df_rest <- df %>% dplyr::filter(fachbereich == "Nicht MINT")
    df_1_rest <- df_rest %>% dplyr::filter(indikator == indi[1])
   #df_1_rest$wert <- round_preserve_sum(as.numeric(df_1_rest$wert),0)

    df_2_rest<- df_rest %>% dplyr::filter(indikator == indi[2])
    #df_2_rest$wert <- round_preserve_sum(as.numeric(df_2_rest$wert),0)

    if(indi[1] == "Schüler:innen im Leistungskurs" & nrow(df_1_mint) == 0){
      titel1 <- "Schüler:innendaten für 2023 sind noch nicht verfügbar."
      df_1_mint$jahr <- NA
      mint1 <- highcharter::hchart(df_1_mint, 'line', highcharter::hcaes(x = reorder(jahr, wert), y = wert, group = indikator)) %>%
        highcharter::hc_tooltip(pointFormat = "Anzahl: {point.display_abs}") %>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
        highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
        highcharter::hc_title(text = titel1,
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px"))

      mint2 <-highcharter::hchart(df_2_mint, size = 280, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = prop)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.prop_besr}% <br> Anzahl: {point.wert_besr}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0(df_2_mint$titel_help[1], " in ", regio, " (", zeit, ")"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.prop_besr}%'), showInLegend = TRUE))
    }else if(indi[2] == "Schüler:innen im Leistungskurs" & nrow(df_2_mint) == 0){
      titel2 <- "Schüler:innendaten für 2023 sind noch nicht verfügbar."
      df_2_mint$jahr <- NA
      mint2 <- highcharter::hchart(df_2_mint, 'line', highcharter::hcaes(x = reorder(jahr, wert), y = wert, group = indikator)) %>%
        highcharter::hc_tooltip(pointFormat = "Anzahl: {point.display_abs}") %>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
        highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
        highcharter::hc_title(text = titel1,
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px"))

      mint1 <- highcharter::hchart(df_1_mint, size = 280, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = prop)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.prop_besr}% <br> Anzahl: {point.wert_besr}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0(df_1_mint$titel_help[1], " in ", regio, " (", zeit, ")"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.prop_besr}%'), showInLegend = TRUE))

      }else{

     mint1 <- highcharter::hchart(df_1_mint, size = 280, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = prop)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.prop_besr}% <br> Anzahl: {point.wert_besr}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0(df_1_mint$titel_help[1], " in ", regio, " (", zeit, ")"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.prop_besr}%'), showInLegend = TRUE))


      mint2 <-highcharter::hchart(df_2_mint, size = 280, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = prop)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.prop_besr}% <br> Anzahl: {point.wert_besr}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0(df_2_mint$titel_help[1], " in ", regio, " (", zeit, ")"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                        dataLabels = list(enabled = TRUE,  format='{point.prop_besr}%'), showInLegend = TRUE))
      }
      if(gegenwert == "Nein"){

        out <- highcharter::hw_grid(
          mint1, mint2,
          ncol = 2,
          browsable = TRUE
        )
      }
      if(gegenwert == "Ja"){

        if(indi[1] == "Schüler:innen im Leistungskurs" & nrow(df_1_rest) == 0){
          titel1 <- ""
          df_1_rest$jahr <- NA
          nmint1 <- highcharter::hchart(df_1_rest, 'line', highcharter::hcaes(x = reorder(jahr, wert), y = wert, group = indikator)) %>%
            highcharter::hc_tooltip(pointFormat = "Anzahl: {point.display_abs}") %>%
            highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
            highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
            highcharter::hc_title(text = titel1,
                                  margin = 45,
                                  align = "center",
                                  style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px"))

          nmint2 <- highcharter::hchart(df_2_rest, size = 150, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = prop)) %>%
            highcharter::hc_tooltip(
              pointFormat=paste('Anteil: {point.prop_besr}% <br> Anzahl: {point.wert_besr}')) %>%
            highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
            highcharter::hc_title(text = paste0(df_2_rest$titel_help2[1], " in ", regio, " (", zeit, ")"),
                                  margin = 45,
                                  align = "center",
                                  style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
            highcharter::hc_legend(enabled = TRUE, y = -120, reversed = T) %>%
            highcharter::hc_chart(
              style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
            #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
            highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                                   dataLabels = list(enabled = TRUE, format='{point.prop_besr}%'), showInLegend = TRUE,
                                                   opacity = 0.7))

        }else if(indi[2] == "Schüler:innen im Leistungskurs" & nrow(df_2_rest) == 0){

          nmint1 <- highcharter::hchart(df_1_rest, size = 150, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = prop)) %>%
            highcharter::hc_tooltip(
              pointFormat=paste('Anteil: {point.prop_besr}% <br> Anzahl: {point.wert_besr}')) %>%
            highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
            highcharter::hc_title(text = paste0(df_1_rest$titel_help2[1], " in ", regio, " (", zeit, ")"),
                                  margin = 45,
                                  align = "center",
                                  style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
            highcharter::hc_legend(enabled = TRUE, y = -120, reversed = T) %>%
            highcharter::hc_chart(
              style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
            highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                                   dataLabels = list(enabled = TRUE, format='{point.prop_besr}%'), showInLegend = TRUE,
                                                   opacity = 0.7))

          titel2 <- "Schüler:innendaten für 2023 sind noch nicht verfügbar."
          df_2_rest$jahr <- NA
          nmint2 <- highcharter::hchart(df_2_rest, 'line', highcharter::hcaes(x = reorder(jahr, wert), y = wert, group = indikator)) %>%
            highcharter::hc_tooltip(pointFormat = "Anzahl: {point.display_abs}") %>%
            highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
            highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
            highcharter::hc_title(text = titel2,
                                  margin = 45,
                                  align = "center",
                                  style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px"))


        }else{

      nmint1 <- highcharter::hchart(df_1_rest, size = 150, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = prop)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.prop_besr}% <br> Anzahl: {point.wert_besr}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0(df_1_rest$titel_help2[1], " in ", regio, " (", zeit, ")"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_legend(enabled = TRUE, y = -120, reversed = T) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.prop_besr}%'), showInLegend = TRUE,
                                               opacity = 0.7))

     nmint2 <- highcharter::hchart(df_2_rest, size = 150, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = prop)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.prop_besr}% <br> Anzahl: {point.wert_besr}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0(df_2_rest$titel_help2[1], " in ", regio, " (", zeit, ")"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_legend(enabled = TRUE, y = -120, reversed = T) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.prop_besr}%'), showInLegend = TRUE,
                                               opacity = 0.7))

        }

     out <-  highcharter::hw_grid(
        mint1, mint2, nmint1, nmint2,
        ncol = 2,
        browsable = TRUE
      )
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
      # ,
      # !fachbereich %in% c("Schüler:innen Grundkurse in MINT",
      #                     "Schüler:innen Leistungskurse in MINT")

      df <- df[with(df, order(prop, decreasing = TRUE)), ]###############################################

      out <- highcharter::hchart(df, 'bar', highcharter::hcaes( x = indikator, y=prop, group = geschlecht)) %>%
        highcharter::hc_tooltip(pointFormat = "{point.anzeige_geschlecht}Anteil: {point.prop_besr} % <br> Anzahl: {point.wert_besr}") %>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  FALSE) %>%
        highcharter::hc_xAxis(title = list(text = ""), categories = c("Schüler:innen Leistungskurse in MINT",
                                                                      "Studierende in MINT",
                                                                      "Auszubildende in MINT",
                                                                      "Beschäftigte in MINT")) %>%
        highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
        highcharter::hc_colors(c("#154194", "#efe8e6")) %>%
        highcharter::hc_title(text = paste0("Anteil von Frauen in MINT nach Bildungsbereichen in ", regio, " (", zeit, ")"),
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

      out <- highcharter::hchart(df, 'bar', highcharter::hcaes(x = indikator, y = prop, group = geschlecht)) %>%
        highcharter::hc_tooltip(pointFormat = "{point.anzeige_geschlecht} Anteil: {point.prop_besr} % <br> Anzahl: {point.wert_besr}") %>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), reversedStacks = FALSE) %>%
        highcharter::hc_xAxis(
          title = list(text = ""),
          categories = c(
            "Schüler:innen Leistungskurse in MINT",
            "Schüler:innen Leistungskurse in anderen Bereichen",
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

       # highcharter::hc_colors(unique(df$farbe)) %>%
        highcharter::hc_title(text = paste0("Anteil von Frauen in MINT nach Bildungsbereichen in ", regio, " (", zeit, ")"),
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

      # out <- highcharter::hchart(df, 'bar', highcharter::hcaes( x = indikator, y=round(proportion,1), group = geschlecht)) %>%
      #   highcharter::hc_tooltip(pointFormat = "{point.anzeige_geschlecht}Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
      #   highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  FALSE) %>%
      #   highcharter::hc_xAxis(title = list(text = ""), categories = c("Schüler:innen Oberstufenbelegungen in MINT",
      #                                                                 "Schüler:innen Oberstufenbelegungen in anderen Bereichen",
      #                                                                 "Schüler:innen Grundkurse in MINT",
      #                                                                 "Schüler:innen Grundkurse in anderen Bereichen",
      #                                                                 "Schüler:innen Leistungskurse in MINT",
      #                                                                 "Schüler:innen Leistungskurse in anderen Bereichen",
      #                                                                 "Studierende in MINT",
      #                                                                 "Studierende in anderen Bereichen",
      #                                                                 "Auszubildende in MINT",
      #                                                                 "Auszubildende in anderen Bereichen",
      #                                                                 "Beschäftigte in MINT",
      #                                                                 "Beschäftigte in anderen Bereichen")) %>%
      #   highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
      #  # highcharter::hc_colors(c("#154194", "#efe8e6")) %>%
      #   highcharter::hc_colors(
      #     ifelse(grepl("in anderen Bereichen$",
      #                  c("Schüler:innen Oberstufenbelegungen in MINT",
      #                    "Schüler:innen Oberstufenbelegungen in anderen Bereichen",
      #                    "Schüler:innen Grundkurse in MINT",
      #                    "Schüler:innen Grundkurse in anderen Bereichen",
      #                    "Schüler:innen Leistungskurse in MINT",
      #                    "Schüler:innen Leistungskurse in anderen Bereichen",
      #                    "Studierende in MINT",
      #                    "Studierende in anderen Bereichen",
      #                    "Auszubildende in MINT",
      #                    "Auszubildende in anderen Bereichen",
      #                    "Beschäftigte in MINT",
      #                    "Beschäftigte in anderen Bereichen")
      #     ), c("#dc2626", "#efe8e6"), c("#154194", "#efe8e6"))
      #   ) %>%
      #   highcharter::hc_title(text = paste0("Anteil von Frauen in MINT nach Bildungsbereichen (", zeit, ")"),
      #                         margin = 25,
      #                         align = "center",
      #                         style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      #   highcharter::hc_chart(
      #     style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      #   ) %>%
      #   highcharter::hc_legend(enabled = TRUE, reversed = FALSE) %>%
      #   highcharter::hc_exporting(enabled = FALSE,
      #                             buttons = list(contextButton = list(
      #                               symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
      #                               onclick = highcharter::JS("function () {
      #                                                         this.exportChart({ type: 'image/png' }); }"),
      #                               align = 'right',
      #                               verticalAlign = 'bottom',
      #                               theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

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

  # filter dataset based on UI inputs
  df <- dplyr::tbl(con, from = "zentral") %>%
    dplyr::filter(jahr %in% t,
                  region %in% regio,
                  geschlecht == "Frauen",
                  fachbereich == "MINT") %>%
    dplyr::select(bereich, indikator, fachbereich, geschlecht, jahr, wert) %>%
    dplyr::collect()

  if(abs_selector=="In Prozent"){

    df_alle <- dplyr::tbl(con, from = "zentral") %>%
      dplyr::filter(jahr %in% t,
                    region %in% regio,
                    geschlecht == "Gesamt",
                    fachbereich == "MINT") %>%
      dplyr::select(bereich, indikator, fachbereich, geschlecht, jahr, wert) %>%
      dplyr::collect()

    if("Schülerinnen Leistungskurse" %in% indikator_choice & regio == "Deutschland"){

      #Baden-Würrtemberg rausrechnen, da dort keine Geschlechter erfasst werden
      df_alle_bw <- dplyr::tbl(con, from = "zentral") %>%
        dplyr::filter(jahr %in% t,
                      region == "Baden-Württemberg",
                      geschlecht == "Gesamt",
                      fachbereich == "MINT",
                      bereich == "Schule") %>%
        dplyr::select(bereich, indikator, fachbereich, geschlecht, jahr, wert) %>%
        dplyr::collect()

      df_alle_schule <- df_alle[df_alle$bereich == "Schule",] %>%
        dplyr::left_join(df_alle_bw, by = c("bereich", "indikator", "fachbereich",
                                            "jahr", "geschlecht")) %>%
        dplyr::mutate(wert.x = wert.x - wert.y) %>%
        dplyr::select(-wert.y) %>%
        dplyr::rename(wert = wert.x)

      df_alle <- df_alle %>%
        dplyr::filter(bereich != "Schule") %>%
        rbind(df_alle_schule)

    }

    df <- df %>%
      dplyr::left_join(df_alle, by = c("indikator", "jahr", "fachbereich", "bereich")) %>%
      dplyr::mutate(prop = round(wert.x/wert.y * 100, 1)) %>%
      dplyr::rename(geschlecht = geschlecht.x,
                    wert = wert.x) %>%
      dplyr::select(-c(wert.y, geschlecht.y))

    df <- df[with(df, order(indikator, jahr, decreasing = FALSE)), ]

    # Ordnen der Legende
    sorted_indicators <- df %>%
      dplyr::group_by(indikator) %>%
      dplyr::summarize(m_value = mean(round(prop, 1), na.rm = TRUE)) %>%
      dplyr::arrange(desc(m_value)) %>%
      dplyr::pull(indikator)

    df$indikator <- factor(df$indikator, levels = sorted_indicators)

    #Trennpunkte für lange Zahlen ergänzen
    df$wert_besr <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$prop_besr <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")

    # plot
    out <- highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = prop, group = indikator))%>%
      highcharter::hc_tooltip(pointFormat = "Anteil Frauen <br> Indikator: {point.indikator} <br> Anteil: {point.prop_besr} %") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
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

    df <- df[with(df, order(indikator, jahr, decreasing = FALSE)), ]

    # Ordnen der Legende
    sorted_indicators <- df %>%
      dplyr::group_by(indikator) %>%
      dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
      dplyr::arrange(desc(m_value)) %>%
      dplyr::pull(indikator)

    df$indikator <- factor(df$indikator, levels = sorted_indicators)

    out <- highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group = indikator))%>%
      highcharter::hc_tooltip(pointFormat = "Anzahl Frauen <br> Indikator: {point.indikator} <br> Anzahl: {point.wert_besr} ")%>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
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
# home_stacked_comparison_gender <- function(r) {
#
#   # load UI inputs from reactive value
#   timerange <- r$date_start_comparison_mint_gender
#
#   # filter dataset based on UI inputs
#   df <- dplyr::tbl(con, from = "zentral") %>%
#     dplyr::filter(region == "Deutschland",
#                   jahr == timerange,
#                   fachbereich == "MINT" | fachbereich == "Alle"|  fachbereich == "Ingenieurwissenschaften" |fachbereich == "Mathematik_Naturwissenschaften"
#                   |fachbereich == "Alle Fächer",
#                   indikator %in% c("Leistungskurse",
#                                    "Studierende",
#                                    "Auszubildende", "Beschäftigte")) %>%
#     dplyr::select(bereich, indikator, geschlecht, fachbereich, wert) %>%
#     dplyr::collect()
#
#
#   df6a <- df %>% dplyr::filter(bereich == "Hochschule")%>%
#   dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle") %>%
#     tidyr::pivot_wider(names_from=geschlecht, values_from=wert)%>%
#     dplyr::mutate(Männer=Gesamt-Frauen)%>%
#     tidyr::pivot_longer(c("Männer", "Gesamt", "Frauen"), values_to = "wert", names_to="geschlecht")
#
#
#   df6c <- df %>% dplyr::filter(bereich == "Schule")%>%
#     dplyr::filter(fachbereich== "MINT" | fachbereich == "Alle Fächer")%>%
#     dplyr::mutate(indikator= paste0("Schülerinnen ", .$indikator ))%>%
#     tidyr::pivot_wider(names_from=geschlecht, values_from = wert)%>%
#     dplyr::mutate(Gesamt = Frauen + Männer)%>%
#     tidyr::pivot_longer(c("Gesamt", "Männer", "Frauen"), values_to = "wert", names_to = "geschlecht")
#
#   df6c$fachbereich <- ifelse(grepl("Alle Fächer", df6c$fachbereich), "Alle", df6c$fachbereich)
#
#   df6b <- df %>% dplyr::filter(bereich != "Hochschule" & bereich != "Schule")%>%
#     unique()%>%
#     tidyr::pivot_wider(names_from=geschlecht, values_from=wert)%>%
#     dplyr::mutate(Männer=Gesamt-Frauen)%>%
#     tidyr::pivot_longer(c("Männer", "Gesamt", "Frauen"), values_to = "wert", names_to="geschlecht")
#
#
#
#   df6_fn <- dplyr::bind_rows(df6b, df6a, df6c)%>%
#     dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle")%>%
#     tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
#     dplyr::mutate("andere Fächer" = Alle - MINT)%>%
#     dplyr::select(- Alle)%>%
#     tidyr::pivot_longer(c(MINT, `andere Fächer`), names_to = "fachbereich", values_to = "wert")%>%
#     tidyr::pivot_wider(names_from = geschlecht, values_from = wert)
#
#   #Trennen um Wert abzuspeichern
#   df_wert <- df6_fn %>%
#     dplyr::select(- Gesamt)%>%
#     tidyr::pivot_longer(c(Männer, Frauen), names_to = "geschlecht", values_to = "wert")
#
#   #Berechnung des Anteils
#   df6_fn <- df6_fn %>%
#     dplyr::mutate(dplyr::across(c(Männer, Frauen), ~./Gesamt*100))%>%
#     dplyr::select(- Gesamt)%>%
#     tidyr::pivot_longer(c(Männer, Frauen), names_to = "geschlecht", values_to = "proportion")
#
#   #Wert anhängen
#   df6_fn <- df6_fn %>% dplyr::left_join(df_wert, by = c("bereich","indikator",  "fachbereich", "geschlecht"))
#
#   #Trennpunkte für lange Zahlen ergänzen
#   df6_fn$wert <- prettyNum(df6_fn$wert, big.mark = ".", decimal.mark = ",")
#
#
#   #sortieren
#   df6_fn <- df6_fn[with(df6_fn, order(fachbereich, decreasing = TRUE)), ]
#
#   #gewählte Indikatoren ausfiltern
#   df6_fn <- df6_fn %>% dplyr::filter(indikator %in% c("Schülerinnen Leistungskurse", "Studierende",
#                                                        "Auszubildende", "Beschäftigte"), fachbereich == "MINT")
#
#
#   # plot
#   hc_1 <- highcharter::hchart(df6_fn, 'bar', highcharter::hcaes( x = indikator, y=round(proportion,1), group = geschlecht)) %>%
#     highcharter::hc_tooltip(pointFormat = "{point.anzeige_geschlecht}Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
#     highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  FALSE) %>%
#     highcharter::hc_xAxis(title = list(text = ""), categories = c("Leistungskurse", "Studierende",
#                                                  "Auszubildende", "Beschäftigte")) %>%
#     highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
#     highcharter::hc_colors(c("#154194", "#efe8e6")) %>%
#     highcharter::hc_title(text = paste0("Anteil von Frauen in MINT nach Bildungsbereichen (", timerange, ")"),
#                           margin = 25,
#                           align = "center",
#                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#     highcharter::hc_chart(
#       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
#     ) %>%
#     highcharter::hc_legend(enabled = TRUE, reversed = FALSE) %>%
#     highcharter::hc_exporting(enabled = FALSE,
#                               buttons = list(contextButton = list(
#                                 symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
#                                 onclick = highcharter::JS("function () {
#                                                               this.exportChart({ type: 'image/png' }); }"),
#                                 align = 'right',
#                                 verticalAlign = 'bottom',
#                                 theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
#
#
#   out <- hc_1
#
#   return(out)
#
#   # ggplot2::ggplot(df, ggplot2::aes(x=indikator, y=wert, fill = anzeige_geschlecht)) +
#   #   ggplot2::geom_bar(stat="identity", position = "dodge") +
#   #   ggplot2::geom_text(ggplot2::aes(label=paste(round(wert),"%"), vjust = - 0.25),
#   #                      position=ggplot2::position_dodge(width=0.9),
#   #                      fontface = "bold") +
#   #   ggplot2::theme_minimal() +
#   #   ggplot2::theme(
#   #     text = ggplot2::element_text(size = 12),
#   #     plot.title = ggtext::element_markdown(hjust = 0.5)) +
#   #   ggplot2::xlab("") + ggplot2::ylab("Anteil") +
#   #   ggplot2::scale_fill_manual(values = c("#154194","#efe8e6")) +
#   #   ggplot2::labs(title = paste0(paste0("<span style='font-size:20.5pt; color:black'>",
#   #                                "Anteil von Frauen in MINT nach Bildungsbereichen (", timerange, ")",
#   #                                "<br><br><br>")),
#   #                 fill = ""
#   #                 ,
#   #            # caption = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen."
#   #             ) +
#   #   ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"))
#
# }

