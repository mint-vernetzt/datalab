# Berufswahl MINT ----
#' A function to plot bar plot
#'
#' @description A function to plot bar plots
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

beruf_einstieg_vergleich <- function(r) {

  # load UI inputs from reactive value
  betrachtung <- r$ansicht_arbeitsmarkt_einsteig_vergleich
  timerange <- r$date_arbeitsmarkt_einstieg_vergleich
  regio <- r$region_arbeitsmarkt_einstieg_vergleich
  faecher <- r$fachbereich_arbeitsmarkt_einstieg_gender

  if(betrachtung == "Einzelansicht - Kuchendiagramm"){
    gruppe <- r$indikator_arbeitsmarkt_einsteig_vergleich
  }else{
   # gruppe <- DBI::dbGetQuery(con, "SELECT DISTINCT indikator FROM arbeitsmarkt_detail")$indikator
    gruppe <- c("Auszubildende",
                "Auszubildende (1. Jahr)",
                "ausländische Auszubildende",
                "Beschäftigte",
                "ausländische Beschäftigte",
                "Beschäftigte u25",
                "Beschäftigte 25-55",
                "Beschäftigte ü55",
                "in Minijobs")
  }

  df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
    dplyr::filter(jahr == timerange &
                    landkreis == "alle Landkreise" &
                    bundesland == regio &
                    anforderung == "Gesamt" &
                    geschlecht == "Gesamt" &
                    indikator %in% gruppe &
                    fachbereich == faecher)%>%
    dplyr::select( "indikator", "bundesland", "landkreis", "fachbereich",
                   "landkreis_zusatz", "landkreis_nummer", "jahr", "anforderung", "geschlecht", "wert") %>%
    dplyr::collect()

  #Anteil MINT berechnen
  df_new_gesamt <- dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
    dplyr::filter(jahr == timerange &
                    landkreis == "alle Landkreise" &
                    bundesland == regio &
                    anforderung == "Gesamt" &
                    geschlecht == "Gesamt" &
                    indikator %in% gruppe &
                    fachbereich == "Alle")%>%
    dplyr::select( "indikator", "bundesland", "landkreis", "fachbereich",
                   "landkreis_zusatz", "landkreis_nummer", "jahr", "anforderung", "geschlecht", "wert") %>%
    dplyr::rename(wert_gesamt = "wert") %>%
    dplyr::select(-fachbereich) %>%
    dplyr::collect()

  df <- df %>%
    dplyr::left_join(df_new_gesamt, by = c("indikator", "bundesland", "landkreis",
                                           "landkreis_zusatz", "landkreis_nummer", "jahr", "anforderung", "geschlecht")) %>%
    #dplyr::rename(fachbereich = "fachbereich.x") %>%
    #dplyr::select(-fachbereich.y) %>%
    dplyr::group_by(indikator) %>%
    dplyr::mutate(proportion = round((wert/wert_gesamt)*100,1))

  #andere Berufe berechnen:
  df_andere <- df %>%
    dplyr::mutate(fachbereich = "Andere Berufe") %>%
    dplyr::mutate(wert = wert_gesamt-wert) %>%
    dplyr::mutate(proportion = round((wert/wert_gesamt)*100,1))

  df <- rbind(df, df_andere)

  #Umbennen von MINT/Andere Berufe
  df$fachbereich[df$fachbereich=="MINT"]<-"beschäftigt in MINT"
  df$fachbereich[df$fachbereich=="Andere Berufe"]<-"beschäftigt in allen Bereichen außer MINT"
  df$fachbereich <- factor(df$fachbereich, levels = c("beschäftigt in allen Bereichen außer MINT",
                                                      "beschäftigt in MINT"))

  #Trennpunkte für lange Zahlen ergänzen
  df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")


  #Graifken
  if(betrachtung == "Einzelansicht - Kuchendiagramm"){

   out <- highcharter::hchart(df, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
      highcharter::hc_tooltip(
        pointFormat=paste('Anteil: {point.percentage:.0f} % <br> Anzahl: {point.wert}')) %>%
      highcharter::hc_title(text = paste0(gruppe, " in ", regio, " (", timerange, ")"),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
      highcharter::hc_legend(enabled = TRUE) %>%
      highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                             dataLabels = list(enabled = TRUE,  format='{point.percentage:.0f}%'), showInLegend = TRUE)) %>%
      highcharter::hc_colors(c("#b16fab","#efe8e6"))

  }else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){

    # df <- df %>% dplyr::filter(indikator %in% c("Auszubildende",
    #                                             "Auszubildende (1. Jahr)",
    #                                             "ausländische Auszubildende",
    #                                             "Beschäftigte",
    #                                             "ausländische Beschäftigte",
    #                                             "Beschäftigte u25",
    #                                             "Beschäftigte 25-55",
    #                                             "Beschäftigte ü55",
    #                                             "in Minijobs"))
    df$indikator[df$indikator == "Auszubildende (1. Jahr)"] <- "Auszubildende mit neuem Lehrvertrag"
    indi <- c("Auszubildende",
              "Auszubildende mit neuem Lehrvertrag",
              "ausländische Auszubildende",
              "Beschäftigte",
              "ausländische Beschäftigte",
              "Beschäftigte u25",
              "Beschäftigte 25-55",
              "Beschäftigte ü55",
              "in Minijobs")

    # plot
    out <- highcharter::hchart(df, 'bar', highcharter::hcaes(y = round(proportion,1), x = indikator, group = "fachbereich")) %>%
      highcharter::hc_tooltip(pointFormat = "{point.fachbereich} <br> Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
      highcharter::hc_xAxis(title = list(text = ""), categories = indi) %>%
      highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
      highcharter::hc_colors(c("#efe8e6","#b16fab")) %>%
      highcharter::hc_title(text = paste0("Anteil von MINT-Beschäftigten und -Auszubildenden an allen Beschäftigten o. Auszubildenden in ", regio, " (", timerange, ")"),
                            margin = 45, # o. war vorher /
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
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


#' A function to plot time series
#'
#' @description A function to plot the time series
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

beruf_verlauf_single <- function(r) {

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt_einstieg_verlauf
  absolut_selector <- r$abs_zahlen_arbeitsmarkt_einstieg_verlauf
  t <-timerange[1]:timerange[2]
  regio <- r$region_arbeitsmarkt_einstieg_verlauf
  indi <- r$indikator_arbeitsmarkt_einstieg_verlauf

  df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
    dplyr::filter(jahr %in% t &
                    bundesland == regio &
                    landkreis == "alle Landkreise" &
                    geschlecht == "Gesamt" &
                    anforderung == "Gesamt" &
                    fachbereich == "MINT" &
                    indikator %in% indi
    )%>%
    dplyr::select(indikator, bundesland, fachbereich, jahr, wert) %>%
    dplyr::collect()

  if (absolut_selector == "In Prozent"){

    df_alle <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
      dplyr::filter(jahr %in% t &
                      bundesland == regio &
                      landkreis == "alle Landkreise" &
                      geschlecht == "Gesamt" &
                      anforderung == "Gesamt" &
                      fachbereich == "Alle" &
                      indikator %in% indi
      )%>%
      dplyr::select(indikator, bundesland, fachbereich, jahr, wert) %>%
      dplyr::collect()

    df <- df %>%
      dplyr::left_join(df_alle, dplyr::join_by(indikator, bundesland, jahr)) %>%
      dplyr::select(-fachbereich.y)%>%
      dplyr::rename(fachbereich = fachbereich.x,
                    wert = wert.x,
                    wert_ges = wert.y) %>%
      dplyr::mutate(prop = round(wert/wert_ges *100, 1))

    df$prop_disp <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")

    # order years for plot
    df <- df[with(df, order(jahr, decreasing = FALSE)), ]

    # plot

    highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = prop, group = indikator)) %>%
      highcharter::hc_tooltip(pointFormat = "{point.indikator} <br> Anteil: {point.prop_disp} %") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = paste0("Anteil von MINT-Beschäftigten und -Auszubildenden an allen Beschäftigten o. Auszubildenden in ", regio),
                            margin = 45, # o. war vorher /
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf", "#35bd97", "#5d335a",
                               "#5f94f9", "#007655", "#d0a9cd", "#112c5f")) %>%
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

  } else if(absolut_selector == "Anzahl") {

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    df$wert_disp <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

    # order years for plot
    df <- df[with(df, order(jahr, decreasing = FALSE)), ]

    # plot

    highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group = indikator)) %>%
      highcharter::hc_tooltip(pointFormat = "{point.indikator} <br> Anteil: {point.wert_disp}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = paste0("Anteil von MINT-Beschäftigten und -Auszubildenden an allen Beschäftigten o. Auszubildenden in ", regio),
                            margin = 45, # o. war vorher /
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf", "#35bd97", "#5d335a",
                               "#5f94f9", "#007655", "#d0a9cd", "#112c5f")) %>%
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
}

#' A function to plot time series
#'
#' @description A function to plot the time series
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_mint_bulas <- function(r) {

  betrachtung <- r$ansicht_beruf_mint_bula

  if(betrachtung == "Übersicht - Kartendiagramm"){
    timerange <- r$zeit_beruf_mint_bula_karte
    indi <- r$indikator_beruf_mint_bula_karte

    df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
      dplyr::filter(
        jahr == timerange &
          indikator == indi &
          landkreis == "alle Landkreise" &
          anforderung == "Gesamt" &
          geschlecht == "Gesamt"&
          fachbereich %in% c("MINT", "Alle") &
          !(bundesland %in% c("Deutschland", "Westdeutschland (o. Berlin)", "Ostdeutschland (einschl. Berlin)")))%>%
      dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)%>%
      dplyr::collect()

    # Anteil berechnen
    df_gesamt <- df %>% dplyr::filter(fachbereich == "Alle")
    df <- df %>%
      dplyr::left_join(df_gesamt, by = c("bundesland", "jahr", "geschlecht", "indikator")) %>%
      dplyr::rename(fachbereich = fachbereich.x,
                    wert = "wert.x",
                    wert_sum = "wert.y") %>%
      dplyr::filter(fachbereich != "Alle") %>%
      dplyr::select(-fachbereich.y) %>%
      dplyr::mutate(proportion = (wert/wert_sum)*100)


    #Gerundetes Prop für Hover:
    df$display_rel <- prettyNum(round(df$proportion, 1), big.mark = ".", decimal.mark = ",")
    #Trennpunkte für lange Zahlen ergänzen
    df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    #Überschirft vorbereiten
    #TODO

    # plot
    out <- highcharter::hcmap(
      "countries/de/de-all",
      data = df,
      value = "proportion",
      joinBy = c("name", "bundesland"),
      borderColor = "#FAFAFA",
      name = paste0("MINT"),
      borderWidth = 0.1,
      nullColor = "#A9A9A9",
      tooltip = list(
        valueDecimals = 0,
        valueSuffix = "%"
      )
      # ,
      # download_map_data = FALSE
    ) %>%
      highcharter::hc_tooltip(pointFormat = "{point.bundesland} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.wert}") %>%
      highcharter::hc_colorAxis(min=0,minColor= "#f4f5f6", maxColor="#b16fab", labels = list(format = "{text}%")) %>%
      highcharter::hc_title(
        text = paste0("Anteil von ",  indi, "n in MINT an allen ",  indi, "n (", timerange, ")" #, title_help_sub
        ),
        margin = 10,
        align = "center",
        style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
      ) %>%
      # highcharter::hc_caption(
      #   text = "Quelle:",  style = list(fontSize = "12px")
      # ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular")
      ) %>% highcharter::hc_size(600, 550) %>%
      highcharter::hc_credits(enabled = FALSE) %>%
      highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                             verticalAlign = "bottom")


  }
  else if(betrachtung == "Gruppenvergleich - Balkendiagramm" ){
    timerange <- r$zeit_beruf_mint_bula_balken
    indikator_choice <- r$indikator_beruf_mint_bula_balken

    df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
      dplyr::filter(
        jahr == timerange &
          indikator == indikator_choice &
          landkreis == "alle Landkreise" &
          anforderung == "Gesamt" &
          geschlecht == "Gesamt" &
          fachbereich %in% c("MINT", "Alle"))%>%
      dplyr::select(`bundesland`, `jahr`, `indikator`, `fachbereich`, `wert`)%>%
      dplyr::collect()

    # Alle als extra Spalte anhängen und Anteil berechnen
    df_ges <- df %>%
      dplyr::filter(fachbereich == "Alle") %>%
      dplyr::rename(wert_ges = wert) %>%
      dplyr::ungroup()%>%
      dplyr::select(indikator, fachbereich, jahr, bundesland, wert_ges)

    df <- df %>%
      dplyr::left_join(df_ges, by = c("indikator", "jahr", "bundesland")) %>%
      dplyr::rename(fachbereich = "fachbereich.x")%>%
      dplyr::ungroup()%>%
      dplyr::select(-c("fachbereich.y")) %>%
      dplyr::mutate(prop = (wert/wert_ges)*100)%>%
      dplyr::mutate(prop = round(prop,1)) %>%
      dplyr::filter(fachbereich == "MINT")

    #Trennpunkte für lange Zahlen in absolutem Wert ergänzen
    df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")


    df <- df[with(df, order(prop, decreasing = TRUE)),]

    #df <- df[with(df, order(fachbereich, decreasing = TRUE)),]
    # df <- df[with(df, order(fachbereich, jahr, decreasing = TRUE)),]

    # nur nötig für stacked, machen wir hier doch nicht
    # #gegenwert Berechnen für jeweilige Auswahl
    # df_n <- df %>% dplyr::group_by(region, indikator) %>%
    #   dplyr::mutate(proportion = 100 - proportion)
    # df_n$fachbereich <- "andere Bereiche"
    #
    # df <- rbind(df, df_n)

    # titel-helper
    title_help <- paste0(indikator_choice, "n")
    title_help <- ifelse(grepl("ausländische Beschäftigte", indikator_choice), "ausländischen Beschäftigten", title_help)
    title_help <- ifelse(grepl("ausländische Auszubildende", indikator_choice), "ausländischen Auszubildenden", title_help)
    title_help <- ifelse(grepl("Jahr", indikator_choice), "Auszubildenden im ersten Lehrjahr", title_help)
    title_help <- ifelse(grepl("u25", indikator_choice), "Beschäftigten unter 25 Jahren", title_help)
    title_help <- ifelse(grepl("25-55", indikator_choice), "Beschäftigten zwischen 25 und 55 Jahren", title_help)
    title_help <- ifelse(grepl("ü55", indikator_choice), "Beschäftigten über 55 Jahren", title_help)


    title_h2 <- ifelse(grepl("Auszu", title_help), "Auszubildenden", "Beschäftigten")
    title_h2 <- ifelse(grepl("25 Jahr", title_help), "Beschäftigten U25", title_h2)
    title_h2 <- ifelse(grepl("25 und 55", title_help), "Beschäftigten zwischen 25 und 55 Jahren", title_h2)
    title_h2 <- ifelse(grepl("über 55", title_help), "Beschäftigten Ü55", title_h2)
    title_h2 <- ifelse(grepl("jahr", title_help), "Auszubildenden im ersten Lehrjahr", title_h2)
    title_h2 <- ifelse(grepl("ausländischen Auszu", title_help), "ausländischen Auszubildenden", title_h2)
    title_h2 <- ifelse(grepl("ländischen B", title_help), "ausländischen Beschäftigten", title_h2)

    # plot
    out <- highcharter::hchart(df, 'bar', highcharter::hcaes(y = prop, x = bundesland)) %>%
      highcharter::hc_tooltip(pointFormat = "{point.fachbereich} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.wert}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
      highcharter::hc_xAxis(title = list(text = "")
                            # ,categories =c("Deutschland",
                            #                                        "Westdeutschland (o. Berlin)",
                            #                                        "Ostdeutschland (einschl. Berlin)",
                            #                                        "Baden-Württemberg",
                            #                                        "Bayern",
                            #                                        "Berlin",
                            #                                        "Brandenburg",
                            #                                        "Bremen",
                            #                                        "Hamburg",
                            #                                        "Hessen",
                            #                                        "Mecklenburg-Vorpommern",
                            #                                        "Niedersachsen",
                            #                                        "Nordrhein-Westfalen",
                            #                                        "Rheinland-Pfalz",
                            #                                        "Saarland",
                            #                                        "Sachsen",
                            #                                        "Sachsen-Anhalt",
                            #                                        "Schleswig-Holstein",
                            #                                        "Thüringen")
      ) %>%
      # highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
      # highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
      # highcharter::hc_plotOptions(bar = list(
      #   colorByPoint = TRUE,
      #   colors = ifelse(df$bundesland %in% c("Deutschland","Westdeutschland (o. Berlin)",
      #                                        "Ostdeutschland (einschl. Berlin)"), "#b16fab", "#A9A9A9")))

      highcharter::hc_plotOptions(bar = list(
        colorByPoint = TRUE,
        colors = ifelse(df$bundesland == "Deutschland", "#b16fab",
                        ifelse(df$bundesland == "Ostdeutschland (einschl. Berlin)", "#d3a4d7",
                               ifelse(df$bundesland == "Westdeutschland (o. Berlin)", "#d3a4d7", "#A9A9A9")))))%>%
      # highcharter::hc_colors( "#b16fab") %>%
      highcharter::hc_title(text = paste0( "Anteil von ", title_help, " in MINT an allen ", title_h2, " in ", timerange,
                                           "<br><br><br>"),
                            margin = 20,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
      highcharter::hc_exporting(enabled = FALSE,
                                buttons = list(contextButton = list(
                                  symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                  onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                  align = 'right',
                                  verticalAlign = 'bottom',
                                  theme = list(feld = list(hover = list(fill = '#FFFFFF'))))))

  }
  else if(betrachtung == "Zeitverlauf - Liniendiagramm"){
    timerange <-r$zeit_beruf_mint_bula_verlauf
    t <- as.character(timerange[1]:timerange[2])
    aniveau <- r$indikator_beruf_mint_bula_verlauf
    states <- r$region_beruf_mint_bula_verlauf
    absolut_selector <- r$abs_beruf_mint_bula_verlauf

    # filter dataset based on UI inputs
    df <-  dplyr::tbl(con, from = "arbeitsmarkt")%>%
      dplyr::filter(jahr %in% t &
                    geschlecht == "Gesamt" &
                    anforderung == "Gesamt",
                    fachbereich %in% c("Alle", "MINT"),
                    region %in% states,
                    indikator == aniveau
      )%>%
      dplyr::select(
        "indikator",
        "fachbereich",
        #"geschlecht",
        "region",
        "jahr",
        #"anforderung",
        "wert" ) %>%
      dplyr::collect()


    df <- df %>%
      tidyr::pivot_wider(values_from=wert, names_from=fachbereich)%>%
      dplyr::mutate(MINT_p= round(MINT/Alle*100,1))%>%
      dplyr::select(- "Alle")%>%
      dplyr::rename(Absolut = MINT, Relativ = MINT_p)%>%
      tidyr::pivot_longer(c(Absolut , Relativ), names_to="selector", values_to="wert")%>%
      dplyr::mutate(selector = dplyr::case_when(
        selector=="Relativ" ~ "In Prozent",
        selector=="Absolut" ~ "Anzahl"
      ))

    # Hilfe für Titel
    title_help <- paste0(aniveau, "n")
    if(absolut_selector=="In Prozent"){

      df <- df %>%
        dplyr::filter(selector=="In Prozent")
      df$display_rel <- prettyNum(round(df$wert,1), big.mark = ".", decimal.mark = ",")
      df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

      # plot
      out <- highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group = region)) %>%
        highcharter::hc_tooltip(pointFormat = "Anteil <br> Bundesland: {point.region} <br> Wert: {point.display_rel} %") %>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
        highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
        #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_title(text = paste0("Anteil von ", title_help, " in MINT-Berufen an allen ", title_help
        ),
        margin = 45,
        align = "center",
        style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                                 "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")) %>%
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

    } else if(absolut_selector=="Anzahl"){

      hcoptslang <- getOption("highcharter.lang")
      hcoptslang$thousandsSep <- "."
      options(highcharter.lang = hcoptslang)

      df <- df %>%
        dplyr::filter(selector=="Anzahl")
      df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
      df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

      # plot
      out <- highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group = region)) %>%
        highcharter::hc_tooltip(pointFormat = "Anzahl: {point.display_abs}") %>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
        highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
        #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_title(text =paste0("Anzahl von ", title_help, " in MINT-Berufen"
        ),
        margin = 45,
        align = "center",
        style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                                 "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")) %>%
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
  }
 return(out)
}


#' A function to plot a waffle chart ::: b3
#'
#' @description A function to create a waffle chart for the tab "Beruf"
#'
#' @return The return value is a waffle chart
#' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_anforderungen_gender <- function(r) {


  timerange <- r$date_arbeitsmarkt_anforderungen_gender

  if(timerange ==2021) indikator_choice <- r$level_arbeitsmarkt_anforderungen_gender_21
  if(timerange ==2022) indikator_choice <- r$level_arbeitsmarkt_anforderungen_gender_22


  df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
    dplyr::filter(jahr %in% timerange &
                    bundesland == "Deutschland" &
                    geschlecht != "Gesamt"&
                    anforderung == "Gesamt" &
                    indikator %in% c("Auszubildende",
                                     "Auszubildende (1. Jahr)",
                                     "Beschäftigte",
                                     "ausländische Auszubildende",
                                     "ausländische Beschäftigte")&
                    fachbereich %in% c("Alle", "MINT", "Mathematik, Naturwissenschaften",
                                       "Informatik", "Technik (gesamt)"))%>%
    dplyr::select(indikator, fachbereich, wert, geschlecht) %>%
    dplyr::collect()

  # # filter dataset based on UI inputs
  # df <- df %>% dplyr::filter(jahr == timerange)
  #
  # df <- df %>% dplyr::filter(bundesland == "Deutschland")
  #
  # #df <- df %>% dplyr::filter(landkreis == "alle Landkreise")
  #
  # df <- df %>% dplyr::filter(anforderung == "Gesamt")

  #Indikatoren u25 - ü25 ausfiltern, da hier i nicht nach Geschlecht unterschieden werden kann
  # df <- df %>% dplyr::filter(indikator %in% c("Auszubildende",
  #                                             "Auszubildende (1. Jahr)",
  #                                             "Beschäftigte",
  #                                             "ausländische Auszubildende",
  #                                             "ausländische Beschäftigte"))


  # Deutschland gesamt berechnen und Landkreise/Bundesländer ausschließen
  # df <- df %>%
  #   dplyr::group_by(jahr, indikator, fachbereich, geschlecht) %>%
  #   dplyr::summarize(wert = sum(wert))

  # Auswahl der Berufsgruppen für Waffel
  # df <- df %>% dplyr::filter(fachbereich %in% c("Alle", "MINT", "Mathematik, Naturwissenschaften",
  #                                               "Informatik", "Technik (gesamt)"))

  # Berechnung von andere Fächergruppen
  df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"]-
    df[df$fachbereich == "MINT", "wert"]
  df$fachbereich[df$fachbereich == "Alle"]<-"andere Fächergruppen"
  df <- df %>% dplyr::filter(fachbereich != "MINT")

  # Berechnen Männer - sind enthalten
  # df_m <- df %>% dplyr::group_by(jahr, indikator, fachbereich) %>%
  #   dplyr::summarise(wert = wert[geschlecht=="Gesamt"]-wert[geschlecht=="Frauen"])
  # df_m$geschlecht <- "Männer"
  #
  # df <- df %>%dplyr::filter(geschlecht=="Frauen")
  #
  # df <- rbind(df, df_m)

  # df <- df %>% dplyr::filter(geschlecht != "Gesamt")


  # Anteil berechnen
  df <- df %>%
    dplyr::group_by(indikator, geschlecht) %>%
    dplyr::mutate(props = sum(wert))

  df <- df %>% dplyr::group_by(fachbereich, indikator, geschlecht) %>%
    dplyr::mutate(proportion = wert/props)

  df$proportion <- df$proportion * 100


  # Ausgewählte Indikatoren filtern
  df <- df %>% dplyr::filter(indikator == indikator_choice)

  # nach Geschlechtern trennen
  # Frauen
  df_fr <- df %>% dplyr::filter(geschlecht=="Frauen")

  df_fr <- setNames(round_preserve_sum(as.numeric(df_fr$proportion),1),
                    df_fr$fachbereich)
  df_fr <- df_fr[order(factor(names(df_fr), levels = c("Mathematik, Naturwissenschaften",
                                                       "Informatik", "Technik (gesamt)",
                                                       'andere Fächergruppen')))]

  # Männer
  df_me <- df %>% dplyr::filter(geschlecht=="Männer")

  df_me <- setNames(round_preserve_sum(as.numeric(df_me$proportion),1),
                    df_me$fachbereich)
  df_me <- df_me[order(factor(names(df_me), levels = c("Mathematik, Naturwissenschaften",
                                                       "Informatik", "Technik (gesamt)",
                                                       'andere Fächergruppen')))]

  # Titel für Plots
  title_help <- paste0(indikator_choice, "n <br>")
  title_help <- ifelse(grepl("ausländische Beschäftigte", indikator_choice), "ausländischen <br> Beschäftigten", title_help)
  title_help <- ifelse(grepl("ausländische Auszubildende", indikator_choice), "ausländischen <br> Auszubildenden", title_help)
  title_help <- ifelse(grepl("Jahr", indikator_choice), "Auszubildenden <br> mit neuem Lehrvertrag <br>", title_help)
  # title_help <- ifelse(grepl("u25", indikator_choice), "Beschäftigten unter 25 Jahren", title_help)
  # title_help <- ifelse(grepl("25-55", indikator_choice), "Beschäftigten zwischen 25 und 55 Jahren", title_help)
  # title_help <- ifelse(grepl("ü55", indikator_choice), "Beschäftigten über 55 Jahren", title_help)
  #

  title_male <- paste0("Von männlichen ", title_help, " gewählte Berufsfelder <br> (", timerange, ")")
  title_female <- paste0("Von weiblichen ", title_help, " gewählte Berufsfelder <br>(", timerange, ")")

  #waffles
  waffle_fr <- waffle::waffle(df_fr, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", title_female, "<br>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "bottom") +
    # account for the possability that female has 0% share of "Experte
    # if (df_trainee[[3]] == 0) {
    # waffle_fr <- waffle_fr +
    ggplot2::scale_fill_manual(
      values =  c("#ee7775",
                  "#fbbf24",
                  "#35bd97",
                  '#8893a7'),
      limits = c("Mathematik, Naturwissenschaften",
                 "Informatik", "Technik (gesamt)",
                 'Andere Fachbereiche'),
      na.value='#8893a7',
      guide = ggplot2::guide_legend(reverse = TRUE),
      labels = c(
        paste0("Mathematik, Naturwissenschaften",", ",df_fr[1], "%"),
        paste0("Informatik",", ",df_fr[2], "%"),
        paste0("Technik (gesamt)",", ",df_fr[3], "%"),
        paste0("Andere Fachbereiche",", ",df_fr[4], "%")
      )) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))


  waffle_me <- waffle::waffle(df_me, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", title_male ,"<br>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "bottom")+
    ggplot2::scale_fill_manual(
      values =  c("#ee7775",
                  "#fbbf24",
                  "#35bd97",
                  '#8893a7'),
      limits = c("Mathematik, Naturwissenschaften",
                 "Informatik", "Technik (gesamt)",
                 'Andere Fachbereiche'),
      na.value='#8893a7',
      guide = ggplot2::guide_legend(reverse = TRUE),
      labels = c(
        paste0("Mathematik, Naturwissenschaften",", ",df_me[1], "%"),
        paste0("Informatik",", ",df_me[2], "%"),
        paste0("Technik (gesamt)",", ",df_me[3], "%"),
        paste0("Andere Fachbereiche",", ",df_me[4], "%")
      )) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))

  ggpubr::ggarrange(waffle_fr, NULL ,waffle_me, widths = c(1, 0.1, 1), nrow=1)

  # ALT - für alten df, ohne Fächer, nach Anforderungsniveau

  #df <- df %>% dplyr::filter(anforderung != "Helfer")

  # # calculate new "Gesamt
  # df_new_gesamt <- df %>% dplyr::filter(anforderung == "Gesamt")
  #   # dplyr::group_by(region, fachbereich, indikator, jahr, geschlecht, bereich) %>% ## Hier vllt drin lassen, da es die Grafik verfälscht
  #   # dplyr::summarise(wert = sum(wert)) %>%
  #   # dplyr::mutate(anforderung = "Gesamt") %>% dplyr::ungroup()
  #
  # df <- rbind(df %>% dplyr::filter(anforderung != "Gesamt"), df_new_gesamt)

  # df <- calc_arbeitsmarkt_males(df)
  #
  # df_total_gender <- calc_arbeitsmarkt_males(df_new_gesamt) %>%
  #   dplyr::filter(geschlecht != "Gesamt") %>%
  #   dplyr::filter(fachbereich == "Alle") %>%
  #   dplyr::select("region", "indikator", "jahr", "wert", "geschlecht") %>%
  #   dplyr::rename(wert_gesamt = "wert")
  #
  # df <- df %>% dplyr::left_join(df_total_gender, by = c("region", "indikator", "jahr", "geschlecht")) %>%
  #   dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
  #   dplyr::select(-c("wert", "wert_gesamt")) %>%
  #   dplyr::filter(geschlecht != "Gesamt",
  #                 ((fachbereich == "Andere Berufe") & (anforderung == "Gesamt")) | ((fachbereich != "Andere Berufe") & (anforderung != "Gesamt"))) %>%
  #   dplyr::mutate(anforderung = dplyr::case_when(anforderung == "Gesamt" ~ "Andere Berufe",
  #                                                       TRUE ~ anforderung))
  #
  #
  #   # male
  #   df_male <- df %>% dplyr::filter(geschlecht == "Männer")
  #
  #   df_male <- setNames(
  #     round_preserve_sum(as.numeric(df_male$proportion),0),
  #                       df_male$anforderung)
  #
  #   df_male <- df_male[order(factor(names(df_male), levels = c('Fachkraft', 'Spezialist',
  #                                                              'Experte',
  #                                                              'Andere Berufe')))]
  #
  #   df_male1 <- c("MINT"=df_male[1]+df_male[2]+df_male[3], "Andere Fachbereiche"= df_male[4]) # kab
  #
  #   attr(x = df_male1, which = "names") <- c("MINT", "Andere Fachbereiche")
  #
  #  # df_male <- df_male
  #
  #   # female
  #   df_female <- df %>% dplyr::filter(geschlecht == "Frauen")
  #
  #   df_female <- setNames(round_preserve_sum(as.numeric(df_female$proportion),0),
  #                         df_female$anforderung)
  #
  #   df_female <- df_female[order(factor(names(df_female), levels = c('Fachkraft', 'Spezialist',
  #                                                                    'Experte',
  #                                                                    'Andere Berufe')))]
  #
  #   df_female1 <- c("MINT"=df_female[1]+df_female[2]+df_female[3], "Andere Fachbereiche"= df_female[4]) # kab
  #
  #   attr(x = df_female1, which = "names") <- c("MINT", "Andere Fachbereiche")
  #
  #   title_male <- paste0("Berufswahl von Männern <br>(", indikator_choice, ", ", timerange, ")")
  #   title_female <- paste0("Berufswahl von Frauen <br>(", indikator_choice, ", ", timerange, ")")


  # create plot objects for waffle charts
  # waffle_male <- waffle::waffle(df_male1, keep = FALSE) +
  #   ggplot2::labs(
  #     fill = "",
  #     title = paste0("<span style='color:black;'>", title_male, "</span> <br>")) +
  #   ggplot2::theme(plot.title = ggtext::element_markdown(),
  #                  plot.subtitle = ggtext::element_markdown(),
  #                  text = ggplot2::element_text(size = 14),
  #                  plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
  #                  legend.position = "bottom")+
  #   ggplot2::scale_fill_manual(
  #     values =  c("#154194",
  #                 # "#fcc433"
  #                 # ,
  #                 # "#00a87a",
  #                 '#b1b5c3'
  #                 ),
  #     limits = c(
  #       # 'Fachkraft', 'Spezialist',
  #       #          'Experte',
  #       "MINT",
  #                'Andere Fachbereiche'),
  #     # na.value="#b1b5c3",
  #     guide = ggplot2::guide_legend(reverse = TRUE),
  #     labels = c(
  #       paste0("MINT",", ",df_male1[1], "%"),
  #       # paste0("MINT-Spezialist",", ",df_male[2], "%"),
  #       # paste0("MINT-Experte",", ",df_male[3], "%"),
  #       paste0("Andere Bereiche",", ",df_male1[2], "%"))) +
  #   ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))
  #
  #
  # waffle_female <- waffle::waffle(df_female1, keep = FALSE) +
  #   ggplot2::labs(
  #     fill = "",
  #     title = paste0("<span style='color:black;'>", title_female,"</span> <br>")) +
  #   ggplot2::theme(plot.title = ggtext::element_markdown(),
  #                  plot.subtitle = ggtext::element_markdown(),
  #                  text = ggplot2::element_text(size = 14),
  #                  plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
  #                  legend.position = "bottom")
  #
  # # account for the possability that female has 0% share of "Experte
  # #if (df_female[[3]] == 0) {
  #
  #   waffle_female1 <- waffle_female +
  #     ggplot2::scale_fill_manual(
  #       values =  c("#154194",
  #                   # "#fcc433",
  #                   # # "#00a87a",
  #                   '#b1b5c3'),
  #       limits = c("MINT",
  #         # 'Fachkraft',
  #         #          'Spezialist',
  #                  'Andere Fachbereiche'),
  #       guide = ggplot2::guide_legend(reverse = TRUE),
  #       # na.value="#b1b5c3",
  #       labels = c(
  #         paste0("MINT",", ",df_female1[1], "%"),
  #         #paste0("MINT-Spezialistin",", ",df_female[2], "%"),
  #         # paste0("Experte",", ",df_female[3], "%"),
  #         paste0("Andere Fachbereiche",", ",df_female1[2], "%"))) +
  #     ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))

  # } else{
  #
  #   waffle_female <- waffle_female +
  #     ggplot2::scale_fill_manual(
  #       values =  c("#ee7775",
  #                   # "#fcc433",
  #                   # "#00a87a",
  #                   '#b1b5c3'),
  #       limits = c("MINT",
  #         # 'Fachkraft', 'Spezialist',
  #         #          'Experte',
  #                  'Andere Fachbereiche'),
  #       na.value="#b1b5c3",
  #       guide = ggplot2::guide_legend(reverse = TRUE),
  #       labels = c(
  #         paste0("MINT-Beschäftigte",", ",df_female1[1], "%"),
  #         # paste0("Spezialist",", ",df_female[2], "%"),
  #         # paste0("Experte",", ",df_female[3], "%"),
  #         paste0("Andere Fachbereiche",", ",df_female1[2], "%"))) +
  #     ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))
  #
  # }

  # ggpubr::ggarrange(waffle_female1, NULL ,waffle_male, widths = c(1, 0.1, 1), nrow=1)

  # text <- c(
  #   paste0("<span style='font-size:20.5pt; color:black'> Anforderungslevel in MINT-Berufen im Vergleich"))

  # ggpubr::annotate_figure(plot, gridtext::richtext_grob(text = text))

}

#' A function to plot the german map ::::box 6
#'
#' @description A function to plot the german map with all states that contain
#' information about the share of women in STEM
#'
#' @return The return value is the german map with information
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_bl_gender <- function(r) {

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt_bl_gender

  #anforderung <- r$anforderung_arbeitsmarkt_bl_gender

  if(timerange == 2021) indikator_choice <- r$level_arbeitsmarkt_bl_gender_21
  if(timerange == 2022) indikator_choice <- r$level_arbeitsmarkt_bl_gender_22

  fachbereich_choice <- r$fach_arbeitsmarkt_bl_gender

  df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
    dplyr::filter(jahr %in% timerange &
                    !bundesland %in% c("Deutschland",
                                        "Westdeutschland (o. Berlin)",
                                        "Ostdeutschland (einschl. Berlin)") &
                    landkreis == "alle Landkreise" &
                    geschlecht != "Gesamt"&
                    anforderung == "Gesamt" )%>%
    dplyr::select(indikator, fachbereich, wert, geschlecht, bundesland, jahr) %>%
    dplyr::collect()

  # filter dataset based on UI inputs
  # df <- df %>% dplyr::filter(jahr == timerange)
  # # filtern nach Anforderungsniveau
  # df <- df %>% dplyr::filter(anforderung == "Gesamt")

  #direkt angegebene Werte hier vollständiger als selbst berechnete Aggregate, daher Folgendes raus

  # # remove - Deutschland nicht enthalten in DF
  # df <- df %>% dplyr::filter(region != "Deutschland")
  #
  # # im neuen DF doch Aggregate enthalten, ausfiltern das Folgecode weiter stimmt
  # df <- df %>% dplyr::filter(landkreis != "alle Landkreise")

  # # Aggregat auf Bundeslandebene berechnen und LKs ausschließen
  # df <- df %>%
  #   dplyr::group_by(jahr, indikator, fachbereich, geschlecht, bundesland) %>%
  #   dplyr::summarize(wert = sum(wert))

  # Filtern nach Bundesländern
  # df <- df %>%
  #   dplyr::filter(landkreis == "alle Landkreise") %>%
  #   dplyr::filter(!(bundesland %in% c("Deutschland", "Westdeutschland (o. Berlin)", "Ostdeutschland (einschl. Berlin)")))


  # Berechnung von andere Fächergruppen
  df_andere <- df %>% dplyr::filter(fachbereich=="Alle")
  df_mint <- df %>% dplyr::filter(fachbereich=="MINT")
  df_andere$wert <- df_andere$wert - df_mint$wert
  df_andere$fachbereich[df_andere$fachbereich == "Alle"]<-"Andere Berufsgruppen"

  df <- rbind(df, df_andere)

  #nicht nötig, da Männer schon in df berechnet
  #df <- calc_arbeitsmarkt_males(df)

  df <- df %>% dplyr::filter(indikator == indikator_choice)

  df_gesamt <- df %>%
    dplyr::filter(fachbereich == "Alle")
  # ,
  # anforderung == "Gesamt")

  df <- df %>%
    dplyr::left_join(df_gesamt, by = c("bundesland", "jahr", "geschlecht", "indikator")) %>%
    dplyr::rename(fachbereich = fachbereich.x,
                  wert = "wert.x",
                  wert_sum = "wert.y") %>%
    dplyr::select(-fachbereich.y) %>%
    dplyr::mutate(proportion = (wert/wert_sum)*100)%>%
    dplyr::filter(fachbereich == fachbereich_choice)

  #Gerundetes Prop für Hover:
  df$prop <- round(df$proportion, 1)

  #Trennpunkte für lange Zahlen ergänzen
  df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

  values_female <- df %>% dplyr::filter(geschlecht == "Frauen")
  values_male <- df %>% dplyr::filter(geschlecht == "Männer")

  # if (anforderung == "Gesamt"){
  #
  #   title_help_sub <- " insgesamt"
  #
  # } else {
  #
  #   title_help_sub <- paste0(" mit anforderung ", anforderung)
  #
  # }

  #Überschrift erstellen
  title_help <- paste0(indikator_choice, "r")
  title_help <- ifelse(grepl("ausländische Beschäftigte", indikator_choice), "ausländischer Beschäftigter", title_help)
  title_help <- ifelse(grepl("ausländische Auszubildende", indikator_choice), "ausländischer Auszubildender", title_help)
  title_help <- ifelse(grepl("Jahr", indikator_choice), "Auszubildender mit neuem Lehrvertrag", title_help)
  # title_help <- ifelse(grepl("u25", indikator_choice), "Beschäftigten unter 25 Jahren", title_help)
  # title_help <- ifelse(grepl("25-55", indikator_choice), "Beschäftigten zwischen 25 und 55 Jahren", title_help)
  # title_help <- ifelse(grepl("ü55", indikator_choice), "Beschäftigten über 55 Jahren", title_help)

  titel_w <- ifelse(fachbereich_choice == "Andere Berufsgruppen", paste0("Anteil weiblicher ", title_help, ", die kein MINT-Berufsfeld wählen (", timerange, ")"),
                    paste0("Anteil weiblicher ", title_help, ", die das Berufsfeld ", fachbereich_choice, " wählen (", timerange, ")"))
  titel_m <- ifelse(fachbereich_choice == "Andere Berufsgruppen", paste0("Anteil männlicher ", title_help, ", die kein MINT-Berufsfeld wählen (", timerange, ")"),
                    paste0("Anteil männlicher ", title_help, ", die das Berufsfeld ", fachbereich_choice, " wählen (", timerange, ")"))



    # plot
    out_1 <- highcharter::hcmap(
      "countries/de/de-all",
      data = values_female,
      value = "proportion",
      joinBy = c("name", "bundesland"),
      borderColor = "#FAFAFA",
      name = paste0(fachbereich_choice),
      borderWidth = 0.1,
      nullColor = "#A9A9A9",
      tooltip = list(
        valueDecimals = 0,
        valueSuffix = "%"
      )
      #,
      #download_map_data = FALSE
    ) %>%
      highcharter::hc_tooltip(pointFormat = "{point.bundesland} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}") %>%
      highcharter::hc_colorAxis(min=0,labels = list(format = "{text}%")) %>%
      highcharter::hc_title(
        text = titel_w,
        margin = 10,
        align = "center",
        style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
      ) %>%
      # highcharter::hc_caption(
      #   text = "Quelle:",  style = list(fontSize = "12px")
      # ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular")
      ) %>% highcharter::hc_size(600, 550) %>%
      highcharter::hc_credits(enabled = FALSE) %>%
      highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                             verticalAlign = "bottom")

    out_2 <- highcharter::hcmap(
      "countries/de/de-all",
      data = values_male,
      value = "proportion",
      joinBy = c("name", "bundesland"),
      borderColor = "#FAFAFA",
      name = paste0(fachbereich_choice),
      borderWidth = 0.1,
      nullColor = "#A9A9A9",
      tooltip = list(
        valueDecimals = 0,
        valueSuffix = "%"
      )
      #,
      #download_map_data = FALSE
    ) %>%
      highcharter::hc_tooltip(pointFormat = "{point.bundesland} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}") %>%
      highcharter::hc_colorAxis(min=0,labels = list(format = "{text}%")) %>%
      highcharter::hc_title(
        text = titel_m,
        margin = 10,
        align = "center",
        style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
      ) %>%
      # highcharter::hc_caption(
      #   text = "Quelle:",  style = list(fontSize = "12px")
      # ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular")
      ) %>% highcharter::hc_size(600, 550) %>%
      highcharter::hc_credits(enabled = FALSE) %>%
      highcharter::hc_legend(layout = "horizontal", floating = FALSE, verticalAlign = "bottom")


    out <- list(out_1, out_2)

    return (out)


}


#' A function to plot time series
#'
#' @description A function to plot the time series
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_bl_gender_verlauf <- function(r) {

  # load UI inputs from reactive value

  absolut_selector <- r$abs_zahlen_beruf_arbeitsmarkt_bl_gender_verlauf

  timerange <- r$date_beruf_arbeitsmarkt_bl_gender_verlauf

  #anforderung <- r$anforderung_beruf_arbeitsmarkt_bl_gender_verlauf

  indikator_choice <- r$indikator_beruf_arbeitsmarkt_bl_gender_verlauf

  states <- r$states_beruf_arbeitsmarkt_bl_gender_verlauf

  t <- as.character(timerange[1]:timerange[2])


  df <-  dplyr::tbl(con, from = "arbeitsmarkt")%>%
    dplyr::filter(jahr %in% t,
                  indikator == indikator_choice,
                  region %in% states,
                  anforderung %in% "Gesamt",
                  geschlecht == "Frauen",
                  fachbereich %in% c("Alle", "MINT")
    )%>%
  dplyr::select("bereich",
  "indikator",
  "fachbereich",
  "geschlecht",
  "region",
  "jahr",
  "anforderung",
  "wert" ) %>%
    dplyr::collect()

  # filter dataset based on UI inputs
  #df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  #df <- df %>% dplyr::filter(indikator == indikator_choice)

  #df <- prep_arbeitsmarkt_east_west(df)

  # df <- df %>%
  #   dplyr::mutate(region = dplyr::case_when(
  #     region == "Westen" ~ "Westdeutschland (o. Berlin)",
  #     region == "Osten" ~ "Ostdeutschland (inkl. Berlin)",
  #     T ~ .$region
  #   ))

  df <- df %>% dplyr::filter(anforderung != "Keine Zuordnung möglich")

  df_gesamt <- df %>%
    dplyr::filter(fachbereich == "Alle",
                  anforderung == "Gesamt")



  df <- df %>%
    dplyr::left_join(df_gesamt, by = c("region", "jahr", "geschlecht", "indikator", "bereich")) %>%
    dplyr::rename(anforderung = "anforderung.x",
                  fachbereich = "fachbereich.x",
                  wert = "wert.x",
                  wert_sum = "wert.y") %>%
    dplyr::select(-c("fachbereich.y", "anforderung.y")) %>%
    dplyr::mutate(proportion = (wert/wert_sum)*100)%>%
    dplyr::filter(anforderung == "Gesamt",
                  fachbereich == "MINT")%>%
    dplyr::select(-wert_sum)%>%
    dplyr::rename(Relativ = proportion, Absolut=wert)%>%
    tidyr::pivot_longer(c(Absolut, Relativ), names_to = "selector", values_to = "wert")%>%
    dplyr::mutate(selector = dplyr::case_when(
      selector == "Relativ" ~ "In Prozent",
      selector == "Absolut" ~ "Anzahl"
    ))


  df <- df %>% dplyr::filter(fachbereich == "MINT")

  if(absolut_selector=="In Prozent"){

    df <- df %>%
      dplyr::filter(selector =="In Prozent")



    # order years for plot
    df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

    title_help <- paste0(indikator_choice, "r")

    # plot
    highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(wert,1), group = region)) %>%
      highcharter::hc_tooltip(pointFormat = "{point.region} <br> Anteil: {point.y} %") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = paste0("Anteil weiblicher ", title_help, ", die MINT-Berufe wählen"
      ),
      margin = 45,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                               "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")) %>%
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


  }else if(absolut_selector=="Anzahl"){

    title_help <- paste0(indikator_choice, "r")

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    df <- df %>%
      dplyr::filter(selector == "Anzahl")

    df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]


    # plot
    highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(wert,1), group = region)) %>%
      highcharter::hc_tooltip(pointFormat = "Anzahl: {point.y}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = paste0("Anzahl weiblicher ", title_help, ", die MINT-Berufe wählen"
      ),
      margin = 45,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                               "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")) %>%
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




}


#' A function to plot time series
#'
#' @description A function to plot the time series
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_bl_verlauf <- function(r) {


  #fach_choice <- r$pick_i

  absolut_selector <- r$abs_zahlen_4

  aniveau <- r$niveau

  # load UI inputs from reactive value
  timerange <- r$date_beruf_arbeitsmarkt_bl_verlauf

  t <- as.character(timerange[1]:timerange[2])

  # anforderung <- r$anforderung_beruf_arbeitsmarkt_bl_verlauf
  #
  # indikator_choice <- r$indikator_beruf_arbeitsmarkt_bl_verlauf

  states <- r$states_beruf_arbeitsmarkt_bl_verlauf

  # filter dataset based on UI inputs

  df <-  dplyr::tbl(con, from = "arbeitsmarkt")%>%
    dplyr::filter(jahr %in% t &
                    geschlecht == "Gesamt" &
                    anforderung == "Gesamt",
                  fachbereich %in% c("Alle", "MINT"),
                  region %in% states,
                  indikator == aniveau
    )%>%
    dplyr::select(
                  "indikator",
                  "fachbereich",
                  #"geschlecht",
                  "region",
                  "jahr",
                  #"anforderung",
                  "wert" ) %>%
    dplyr::collect()


  df <- df%>%
    # tidyr::pivot_wider(names_from= region, values_from=wert)%>%
    # dplyr::mutate("Ostdeutschland (inkl. Berlin)" = rowSums(dplyr::select(., c(Berlin, Brandenburg, `Mecklenburg-Vorpommern`, Sachsen, `Sachsen-Anhalt`, Thüringen)),na.rm = T),
    #               "Westdeutschland (o. Berlin)"= rowSums(dplyr::select(., c(`Baden-Württemberg`, Bayern, Bremen, Hamburg, Hessen, Niedersachsen, `Schleswig-Holstein`, `Nordrhein-Westfalen`,
    #                                                                         `Rheinland-Pfalz`, Saarland  )),na.rm = T))%>%

    # tidyr::pivot_longer(c("Deutschland","Ostdeutschland (inkl. Berlin)", "Westdeutschland (o. Berlin)",
    #                       "Berlin", "Brandenburg", "Mecklenburg-Vorpommern", "Sachsen",
    #                       "Sachsen-Anhalt", "Thüringen",
    #                       "Baden-Württemberg", "Bayern", "Bremen", "Hamburg", "Hessen",
    #                       "Niedersachsen", "Schleswig-Holstein", "Nordrhein-Westfalen",
    #                       "Rheinland-Pfalz", "Saarland"
    #                       ),names_to= "region", values_to="wert")%>%
     tidyr::pivot_wider(values_from=wert, names_from=fachbereich)%>%
    dplyr::mutate(MINT_p= round(MINT/Alle*100,1))%>%
    dplyr::select(- "Alle")%>%
    dplyr::rename(Absolut = MINT, Relativ = MINT_p)%>%
    tidyr::pivot_longer(c(Absolut , Relativ), names_to="selector", values_to="wert")%>%
    dplyr::mutate(selector = dplyr::case_when(
      selector=="Relativ" ~ "In Prozent",
      selector=="Absolut" ~ "Anzahl"
    ))


 # df <- df %>% dplyr::filter(region %in% states)


 # df <- df %>% dplyr::filter(indikator == aniveau)

  # Hilfe für Titel
  title_help <- paste0(aniveau, "n")

  if(absolut_selector=="In Prozent"){

    df <- df %>%
      dplyr::filter(selector=="In Prozent")



    df$display_rel <- prettyNum(round(df$wert,1), big.mark = ".", decimal.mark = ",")


    df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]







    # plot
    highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group = region)) %>%
      highcharter::hc_tooltip(pointFormat = "Anteil <br> Bundesland: {point.region} <br> Wert: {point.display_rel} %") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = paste0("Anteil von ", title_help, " in MINT-Berufen an allen ", title_help
      ),
      margin = 45,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                               "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")) %>%
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

  } else if(absolut_selector=="Anzahl"){


    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    df <- df %>%
      dplyr::filter(selector=="Anzahl")

    df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

    df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]


    # plot
    highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group = region)) %>%
      highcharter::hc_tooltip(pointFormat = "Anzahl: {point.display_abs}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text =paste0("Anzahl von ", title_help, " in MINT-Berufen"
      ),
      margin = 45,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                               "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")) %>%
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

}

# M-I-N-T ----


#' A function to plot a waffle chart
#'
#' @description A function to create a waffle chart for the tab "Beruf"
#'
#' @return The return value is a waffle chart
#' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_faecher_anteil <- function(r) {

  color_fachbereich <- c(
    "Informatik" = "#2D6BE1",
    "Technik (gesamt)" = "#00a87a",
    "Mathematik, Naturwissenschaften" = "#fcc433",
    "andere Berufsfelder" = "#efe8e6"
  )
  color_fachbereich_balken <- c(
    "Informatik" = "#2D6BE1",
    "Technik (gesamt)" = "#00a87a",
    "Mathematik, Naturwissenschaften" = "#fcc433",
    "Alle Berufsfelder außer MINT" = "#efe8e6"
  )

  betrachtung <- r$ansicht_arbeitsmarkt_fach_vergleich
  timerange <- r$date_arbeitsmarkt_fach_vergleich
  regio <- r$region_arbeitsmarkt_fach_vergleich
  nicht_mint <- r$gegenwert_arbeitsmarkt_fach_vergleich

  if(betrachtung == "Einzelansicht - Kuchendiagramm"){
    indikator_choice <- r$indikator_arbeitsmarkt_fach_vergleich_pies

    if(nicht_mint == "Nein"){
      df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
        dplyr::filter(jahr %in% timerange &
                        landkreis == "alle Landkreise" &
                        bundesland == regio &
                        geschlecht == "Gesamt" &
                        anforderung == "Gesamt" &
                        fachbereich %in% c(
                          "Mathematik, Naturwissenschaften",
                          "Informatik", "Technik (gesamt)") &
                        indikator %in% indikator_choice)%>%
        dplyr::select(indikator, jahr, bundesland, fachbereich, wert) %>%
        dplyr::collect()

      # Anteil berechnen
      df <- df %>%
        dplyr::group_by(indikator) %>%
        dplyr::mutate(prop = sum(wert))

      df <- df %>% dplyr::group_by(fachbereich, indikator) %>%
        dplyr::mutate(prop = round(wert/prop*100,1))

      preposition <- ifelse(grepl("aarland$", regio), "im", "in")

      df$titel_help <- paste0(df$indikator, "n <br>")
      df$titel_help <- ifelse(df$indikator == "Auszubildende (1. Jahr)", "Auszubildenden <br>mit neuem Lehrvertrag ", df$titel_help)
      df$titel_help <- ifelse(df$indikator == "ausländische Auszubildende", "ausländischen <br> Auszubildenden ", df$titel_help)
      df$titel_help <- ifelse(df$indikator == "ausländische Beschäftigte", "ausländischen <br> Beschäftigten ", df$titel_help)
      df$titel_help <- ifelse(df$indikator == "Beschäftigte 25-55", "Beschäftigten <br> 25-55 ", df$titel_help)
      df$titel_help <- ifelse(df$indikator == "Beschäftigte u25", "Beschäftigten <br> u25 ", df$titel_help)
      df$titel_help <- ifelse(df$indikator == "Beschäftigte ü55", "Beschäftigten <br> ü55 ", df$titel_help)


      df$wert_disp <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
      df$prop_disp <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")
    }
    else{
      df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
        dplyr::filter(jahr %in% timerange &
                        landkreis == "alle Landkreise" &
                        bundesland == regio &
                        geschlecht == "Gesamt" &
                        anforderung == "Gesamt" &
                        fachbereich %in% c("Alle", "MINT",
                                           "Mathematik, Naturwissenschaften",
                                           "Informatik", "Technik (gesamt)") &
                        indikator %in% indikator_choice)%>%
        dplyr::select(indikator, jahr, bundesland, fachbereich, wert) %>%
        dplyr::collect()

      # Berechnung von andere Fächergruppen
      df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"]-
        df[df$fachbereich == "MINT", "wert"]
      df$fachbereich[df$fachbereich == "Alle"]<-"andere Berufsfelder"
      df <- df %>% dplyr::filter(fachbereich != "MINT")

      # Anteil berechnen
      df <- df %>%
        dplyr::group_by(indikator) %>%
        dplyr::mutate(prop = sum(wert))

      df <- df %>% dplyr::group_by(fachbereich, indikator) %>%
        dplyr::mutate(prop = round(wert/prop*100,1))


      preposition <- ifelse(grepl("aarland$", regio), "im", "in")

      df$titel_help <- paste0(df$indikator, "n <br>")
      df$titel_help <- ifelse(df$indikator == "Auszubildende (1. Jahr)", "Auszubildenden <br>mit neuem Lehrvertrag ", df$titel_help)
      df$titel_help <- ifelse(df$indikator == "ausländische Auszubildende", "ausländischen <br> Auszubildenden ", df$titel_help)
      df$titel_help <- ifelse(df$indikator == "ausländische Beschäftigte", "ausländischen <br> Beschäftigten ", df$titel_help)
      df$titel_help <- ifelse(df$indikator == "Beschäftigte 25-55", "Beschäftigten <br> 25-55 ", df$titel_help)
      df$titel_help <- ifelse(df$indikator == "Beschäftigte u25", "Beschäftigten <br> u25 ", df$titel_help)
      df$titel_help <- ifelse(df$indikator == "Beschäftigte ü55", "Beschäftigten <br> ü55 ", df$titel_help)

      df$wert_disp <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
      df$prop_disp <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")
    }

    if(length(indikator_choice) == 1) {

      df <- df[with(df, order(prop, decreasing = FALSE)), ]
      df <- df %>%
        dplyr::mutate(color = color_fachbereich[fachbereich])

      out <- df %>%
        highcharter::hchart(
          size = 280, type ="pie", mapping = highcharter::hcaes(x = fachbereich , y = prop)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}')) %>%
        highcharter::hc_colors(as.character(df$color)) %>%
        highcharter::hc_title(text = paste0("MINT-Anteile von ", unique(df$titel_help), preposition, " ", regio, " (", timerange, ")"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.prop_disp}%'), showInLegend = TRUE))


    } else if(length(indikator_choice) == 2) {

      df_1 <- df %>% dplyr::filter(indikator == indikator_choice[1])
      df_1 <- df_1[with(df_1, order(prop, decreasing = FALSE)), ]
      df_1 <- df_1 %>%
        dplyr::mutate(color = color_fachbereich[fachbereich])

      df_2 <- df %>% dplyr::filter(indikator == indikator_choice[2])
      df_2 <- df_2[with(df_2, order(prop, decreasing = FALSE)), ]
      df_2 <- df_2 %>%
        dplyr::mutate(color = color_fachbereich[fachbereich])

      out_1 <- df_1 %>%
        highcharter::hchart(
          size = 280, type ="pie", mapping = highcharter::hcaes(x = fachbereich , y = prop)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}')) %>%
        highcharter::hc_colors(as.character(df_1$color)) %>%
        highcharter::hc_title(text = paste0("MINT-Anteile von ", unique(df_1$titel_help), preposition, " ", regio, " (", timerange, ")"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.prop_disp}%'), showInLegend = TRUE))
      out_2 <- df_2 %>%
        highcharter::hchart(
          size = 280, type ="pie", mapping = highcharter::hcaes(x = fachbereich , y = prop)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}')) %>%
        highcharter::hc_colors(as.character(df_2$color)) %>%
        highcharter::hc_title(text = paste0("MINT-Anteile von ", unique(df_2$titel_help), preposition, " ", regio, " (", timerange, ")"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.prop_disp}%'), showInLegend = TRUE))

      out <- highcharter::hw_grid(
        out_1, out_2,
        ncol = 2,
        browsable = TRUE
      )
    }

  }else
    if (betrachtung == "Gruppenvergleich - Balkendiagramm"){

    indikator_choice <- r$indikator_arbeitsmarkt_fach_vergleich_balken

    df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
      dplyr::filter(
        jahr == timerange &
          indikator == indikator_choice &
          landkreis == "alle Landkreise" &
          anforderung == "Gesamt" &
          geschlecht == "Gesamt"&
          bundesland == regio &
          fachbereich %in% c("Alle", "MINT",
                             "Mathematik, Naturwissenschaften",
                             "Informatik", "Technik (gesamt)"))%>%
      dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)%>%
      dplyr::collect()

    df_andere <- df %>% dplyr::filter(fachbereich=="Alle")
    df_mint <- df %>% dplyr::filter(fachbereich=="MINT")
    df_andere$wert <- df_andere$wert - df_mint$wert
    df_andere$fachbereich[df_andere$fachbereich == "Alle"]<-"Alle Berufsfelder außer MINT"
    df <- rbind(df, df_andere)
    df <- df %>% dplyr::filter(fachbereich != "Alle")

    df_alle <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
      dplyr::filter(
        jahr == timerange &
          indikator == indikator_choice &
          landkreis == "alle Landkreise" &
          anforderung == "Gesamt" &
          geschlecht == "Gesamt"&
          bundesland == regio &
          fachbereich == "Alle")%>%
      dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)%>%
      dplyr::collect()

    df <- df %>%
      dplyr::left_join(df_alle,
                       dplyr::join_by("bundesland", "jahr", "geschlecht", "indikator")) %>%
      dplyr::select(-fachbereich.y) %>%
      dplyr::rename(fachbereich = fachbereich.x,
                    wert = wert.x,
                    wert_ges = wert.y) %>%
      dplyr::mutate(prop = round(wert/wert_ges * 100,1))

    #Trennpunkte für lange Zahlen ergänzen
    df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")

    #für Überblick unterarten von Technik wieder raus
    df <- df %>% dplyr::filter(fachbereich %in% c("Alle Berufsfelder außer MINT",
                                                  "Mathematik, Naturwissenschaften",
                                                  "Informatik",
                                                  "Technik (gesamt)"))

    df <- df[with(df, order(prop, decreasing = TRUE)), ]
    df <- df %>%
      dplyr::mutate(color = color_fachbereich_balken[fachbereich])

    # titel-helper
    title_help <- paste0(indikator_choice, "n")
    title_help <- ifelse(grepl("ausländische Beschäftigte", indikator_choice), "ausländischen Beschäftigten", title_help)
    title_help <- ifelse(grepl("ausländische Auszubildende", indikator_choice), "ausländischen Auszubildenden", title_help)
    title_help <- ifelse(grepl("Jahr", indikator_choice), "Auszubildenden mit neuem Lehrvertrag", title_help)
    title_help <- ifelse(grepl("u25", indikator_choice), "Beschäftigten unter 25 Jahren", title_help)
    title_help <- ifelse(grepl("25-55", indikator_choice), "Beschäftigten zwischen 25 und 55 Jahren", title_help)
    title_help <- ifelse(grepl("ü55", indikator_choice), "Beschäftigten über 55 Jahren", title_help)

    hover <- "Anteil an allen Berufsfeldern: {point.display_rel} % <br> Anzahl {point.indikator}: {point.wert}"
    if(indikator_choice == "Auszubildende (1. Jahr)") hover <- "Anteil an allen Berufsfeldern: {point.display_rel} % <br> Anzahl Auszubildende mit neuem Lehrvertrag: {point.wert}"

    # plot
    out <- highcharter::hchart(df, 'bar', highcharter::hcaes(y = prop, x = fachbereich)) %>%
      highcharter::hc_tooltip(pointFormat = hover) %>%
      highcharter::hc_yAxis(title = list(text=""), labels = list(format = "{value}%")) %>%
      highcharter::hc_xAxis(titel = list(text="")) %>%
      highcharter::hc_plotOptions(bar = list(
        colorByPoint = TRUE,
        colors = as.character(df$color)
      )) %>%
      #highcharter::hc_colors(as.character(df$color)) %>%
      highcharter::hc_title(text = paste0( "Überblick über die Berufsfelder von ", title_help,
                                           br(), "in ",regio, " (", timerange, ")"),
                            margin = 20,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
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

#' A function to plot time series
#'
#' @description A function to plot the time series
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

beruf_verlauf_faecher <- function(r) {

  color_fachbereich <- c(
    "Informatik" = "#2D6BE1",
    "Technik (gesamt)" = "#00a87a",
    "Mathematik, Naturwissenschaften" = "#fcc433"
  )
  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt_faecher_verlauf
  t <- timerange[1]:timerange[2]
  regio <- r$region_arbeitsmarkt_faecher_verlauf
  indi <- r$indikator_arbeitsmarkt_faecher_verlauf
  absolut_selector <- r$abs_zahlen_arbeitsmarkt_faecher_verlauf

  # Daten abrufen
  df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
    dplyr::filter(
      jahr %in% t &
        indikator == indi &
        landkreis == "alle Landkreise" &
        anforderung == "Gesamt" &
        geschlecht == "Gesamt"&
        bundesland == regio &
        fachbereich %in% c(
                           "Mathematik, Naturwissenschaften",
                           "Informatik", "Technik (gesamt)"))%>%
    dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)%>%
    dplyr::collect()


  if (absolut_selector == "In Prozent"){

    df_alle <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
      dplyr::filter(
        jahr %in% t &
          indikator == indi &
          landkreis == "alle Landkreise" &
          anforderung == "Gesamt" &
          geschlecht == "Gesamt"&
          bundesland == regio &
          fachbereich == "Alle")%>%
      dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)%>%
      dplyr::collect()

    df <- df %>%
      dplyr::left_join(df_alle,
                       dplyr::join_by("bundesland", "jahr", "geschlecht", "indikator")) %>%
      dplyr::select(-fachbereich.y) %>%
      dplyr::rename(fachbereich = fachbereich.x,
                    wert = wert.x,
                    wert_ges = wert.y) %>%
      dplyr::mutate(prop = round(wert/wert_ges * 100,1)) %>%
      dplyr::filter(fachbereich != "MINT")

    # order years for plot and create labels
    df <- df[with(df, order(jahr, decreasing = FALSE)), ]
    df$prop_disp <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")
    sorted_indicators <- df %>%
      dplyr::group_by(fachbereich) %>%
      dplyr::summarize(m_value = mean(round(prop, 1), na.rm = TRUE)) %>%
      dplyr::arrange(desc(m_value)) %>%
      dplyr::pull(fachbereich)

    df$fachbereich <- factor(df$fachbereich, levels = sorted_indicators)
    colors <- color_fachbereich[sorted_indicators]



    # plot
    out <- highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(prop,1), group = fachbereich)) %>%
      highcharter::hc_tooltip(pointFormat = "{point.indikator} <br> Anteil: {point.prop_disp} %") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value} %"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = paste0("Entwicklung des MINT-Anteils unter ", indi, "n in ", regio),
                            margin = 45, # o. war vorher /
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(as.character(colors)) %>%
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

  } else if(absolut_selector == "Anzahl") {

    # order years for plot and create labels
    df <- df[with(df, order(jahr, decreasing = FALSE)), ]
    df$wert_disp <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    sorted_indicators <- df %>%
      dplyr::group_by(fachbereich) %>%
      dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
      dplyr::arrange(desc(m_value)) %>%
      dplyr::pull(fachbereich)

    df$fachbereich <- factor(df$fachbereich, levels = sorted_indicators)
    colors <- color_fachbereich[sorted_indicators]

    # plot
    out <- highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(wert,1), group = fachbereich)) %>%
      highcharter::hc_tooltip(pointFormat = "{point.indikator} <br> Anzahl: {point.wert_disp}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = paste0("Entwicklung des MINT-Anteils unter ", indi, " in ", regio),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(as.character(colors)) %>%
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

  return(out)
}

#' A function to plot the german map
#'
#' @description A function to plot the german map with all states that contain
#' information about the share of women in STEM
#'
#' @return The return value is the german map with information
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_bula_faecher <- function(r) {

  betrachtung <- r$ansicht_beruf_faecher_bula

  if(betrachtung == "Übersicht - Kartendiagramm"){
    timerange <- r$zeit_beruf_faecher_bula_karte
    indi <- r$indikator_beruf_faecher_bula_karte
    faecher <- r$fachbereich_beruf_faecher_bula_karte

    df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
      dplyr::filter(
        jahr == timerange &
          indikator == indi &
          landkreis == "alle Landkreise" &
          anforderung == "Gesamt" &
          geschlecht == "Gesamt"&
          fachbereich %in% c(faecher, "Alle") &
          !(bundesland %in% c("Deutschland", "Westdeutschland (o. Berlin)", "Ostdeutschland (einschl. Berlin)")))%>%
      dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)%>%
      dplyr::collect()

    # Anteil berechnen
    df_gesamt <- df %>% dplyr::filter(fachbereich == "Alle")
    df <- df %>%
      dplyr::left_join(df_gesamt, by = c("bundesland", "jahr", "geschlecht", "indikator")) %>%
      dplyr::rename(fachbereich = fachbereich.x,
                    wert = "wert.x",
                    wert_sum = "wert.y") %>%
      dplyr::filter(fachbereich != "Alle") %>%
      dplyr::select(-fachbereich.y) %>%
      dplyr::mutate(proportion = (wert/wert_sum)*100)


    #Gerundetes Prop für Hover:
    df$display_rel <- prettyNum(round(df$proportion, 1), big.mark = ".", decimal.mark = ",")
    #Trennpunkte für lange Zahlen ergänzen
    df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    #Überschirft vorbereiten
    #TODO

    # plot
    out <- highcharter::hcmap(
      "countries/de/de-all",
      data = df,
      value = "proportion",
      joinBy = c("name", "bundesland"),
      borderColor = "#FAFAFA",
      name = paste0("MINT"),
      borderWidth = 0.1,
      nullColor = "#A9A9A9",
      tooltip = list(
        valueDecimals = 0,
        valueSuffix = "%"
      )
      # ,
      # download_map_data = FALSE
    ) %>%
      highcharter::hc_tooltip(pointFormat = "{point.bundesland} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.wert}") %>%
      highcharter::hc_colorAxis(min=0,minColor= "#f4f5f6", maxColor="#b16fab", labels = list(format = "{text}%")) %>%
      highcharter::hc_title(
        text = paste0("Anteil von ",  indi, " in MINT an allen ",  indi, " (", timerange, ")" #, title_help_sub
        ),
        margin = 10,
        align = "center",
        style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
      ) %>%
      # highcharter::hc_caption(
      #   text = "Quelle:",  style = list(fontSize = "12px")
      # ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular")
      ) %>% highcharter::hc_size(600, 550) %>%
      highcharter::hc_credits(enabled = FALSE) %>%
      highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                             verticalAlign = "bottom")


  }
  else if(betrachtung == "Gruppenvergleich - Balkendiagramm" ){
    timerange <- r$zeit_beruf_faecher_bula_balken
    indikator_choice <- r$indikator_beruf_faecher_bula_balken
    faecher <- r$fachbereich_beruf_faecher_bula_verlauf

    df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
      dplyr::filter(
        jahr == timerange &
          indikator == indikator_choice &
          landkreis == "alle Landkreise" &
          anforderung == "Gesamt" &
          geschlecht == "Gesamt" &
          fachbereich == faecher) %>%
      dplyr::select(`bundesland`, `jahr`, `indikator`, `fachbereich`, `wert`)%>%
      dplyr::collect()

    # Alle als extra Spalte anhängen und Anteil berechnen
    df_ges <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
      dplyr::filter(
        jahr == timerange &
          indikator == indikator_choice &
          landkreis == "alle Landkreise" &
          anforderung == "Gesamt" &
          geschlecht == "Gesamt" &
          fachbereich == "Alle") %>%
      dplyr::select(`bundesland`, `jahr`, `indikator`, `fachbereich`, `wert`)%>%
      dplyr::rename(wert_ges = wert) %>%
      dplyr::ungroup()%>%
      dplyr::collect()

    df <- df %>%
      dplyr::left_join(df_ges, by = c("indikator", "jahr", "bundesland")) %>%
      dplyr::rename(fachbereich = "fachbereich.x")%>%
      dplyr::ungroup()%>%
      dplyr::select(-c("fachbereich.y")) %>%
      dplyr::mutate(prop = (wert/wert_ges)*100)%>%
      dplyr::mutate(prop = round(prop,1))

    #Trennpunkte für lange Zahlen in absolutem Wert ergänzen
    df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")

    df <- df[with(df, order(fachbereich, jahr, decreasing = FALSE)), ]

    # nur nötig für stacked, machen wir hier doch nicht
    # #gegenwert Berechnen für jeweilige Auswahl
    # df_n <- df %>% dplyr::group_by(region, indikator) %>%
    #   dplyr::mutate(proportion = 100 - proportion)
    # df_n$fachbereich <- "andere Bereiche"
    #
    # df <- rbind(df, df_n)


    df <- df[with(df, order(prop, decreasing = TRUE)),]

    # titel-helper
    title_help <- paste0(indikator_choice, "n")
    title_help <- ifelse(grepl("ausländische Beschäftigte", indikator_choice), "ausländischen Beschäftigten", title_help)
    title_help <- ifelse(grepl("ausländische Auszubildende", indikator_choice), "ausländischen Auszubildenden", title_help)
    title_help <- ifelse(grepl("Jahr", indikator_choice), "Auszubildenden im ersten Lehrjahr", title_help)
    title_help <- ifelse(grepl("u25", indikator_choice), "Beschäftigten unter 25 Jahren", title_help)
    title_help <- ifelse(grepl("25-55", indikator_choice), "Beschäftigten zwischen 25 und 55 Jahren", title_help)
    title_help <- ifelse(grepl("ü55", indikator_choice), "Beschäftigten über 55 Jahren", title_help)


    title_h2 <- ifelse(grepl("Auszu", title_help), "Auszubildenden", "Beschäftigten")
    title_h2 <- ifelse(grepl("25 Jahr", title_help), "Beschäftigten U25", title_h2)
    title_h2 <- ifelse(grepl("25 und 55", title_help), "Beschäftigten zwischen 25 und 55 Jahren", title_h2)
    title_h2 <- ifelse(grepl("über 55", title_help), "Beschäftigten Ü55", title_h2)
    title_h2 <- ifelse(grepl("jahr", title_help), "Auszubildenden im ersten Lehrjahr", title_h2)
    title_h2 <- ifelse(grepl("ausländischen Auszu", title_help), "ausländischen Auszubildenden", title_h2)
    title_h2 <- ifelse(grepl("ländischen B", title_help), "ausländischen Beschäftigten", title_h2)

    # plot
    out <- highcharter::hchart(df, 'bar', highcharter::hcaes(y = prop, x = bundesland)) %>%
      highcharter::hc_tooltip(pointFormat = "{point.fachbereich} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.wert}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
      highcharter::hc_xAxis(title = list(text = ""), categories =c("Deutschland",
                                                                   "Westdeutschland (o. Berlin)",
                                                                   "Ostdeutschland (einschl. Berlin)",
                                                                   "Baden-Württemberg",
                                                                   "Bayern",
                                                                   "Berlin",
                                                                   "Brandenburg",
                                                                   "Bremen",
                                                                   "Hamburg",
                                                                   "Hessen",
                                                                   "Mecklenburg-Vorpommern",
                                                                   "Niedersachsen",
                                                                   "Nordrhein-Westfalen",
                                                                   "Rheinland-Pfalz",
                                                                   "Saarland",
                                                                   "Sachsen",
                                                                   "Sachsen-Anhalt",
                                                                   "Schleswig-Holstein",
                                                                   "Thüringen")
      ) %>%
      # highcharter::hc_plotOptions(bar = list(
      #   colorByPoint = TRUE,
      #   colors = ifelse(df$bundesland %in% c("Deutschland","Westdeutschland (o. Berlin)",
      #                                        "Ostdeutschland (einschl. Berlin)"), "#d0a9cd", "#b16fab")
      # )) %>%

      highcharter::hc_plotOptions(bar = list(
        colorByPoint = TRUE,
        colors = ifelse(df$bundesland == "Deutschland", "#b16fab",
                        ifelse(df$bundesland == "Ostdeutschland (einschl. Berlin)", "#d3a4d7",
                               ifelse(df$bundesland == "Westdeutschland (o. Berlin)", "#d3a4d7", "#A9A9A9")))))%>%


      highcharter::hc_title(text = paste0( "Anteil von ", title_help, " in ", faecher, " an allen ", title_h2, " in ", timerange,
                                           "<br><br><br>"),
                            margin = 20,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
      highcharter::hc_exporting(enabled = FALSE,
                                buttons = list(contextButton = list(
                                  symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                  onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                  align = 'right',
                                  verticalAlign = 'bottom',
                                  theme = list(feld = list(hover = list(fill = '#FFFFFF'))))))

  }
  else if(betrachtung == "Zeitverlauf - Liniendiagramm"){
    timerange <-r$zeit_beruf_faecher_bula_verlauf
    t <- timerange[1]:timerange[2]
    indi <- r$indikator_beruf_faecher_bula_verlauf
    states <- r$region_beruf_faecher_bula_verlauf
    absolut_selector <- r$abs_beruf_faecher_bula_verlauf
    faecher <- r$fachbereich_beruf_faecher_bula_balken

    # filter dataset based on UI inputs
    df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
      dplyr::filter(
        jahr %in% t &
          indikator == indi &
          landkreis == "alle Landkreise" &
          bundesland %in% states &
          anforderung == "Gesamt" &
          geschlecht == "Gesamt" &
          fachbereich == faecher) %>%
      dplyr::select(`bundesland`, `jahr`, `indikator`, `fachbereich`, `wert`)%>%
      dplyr::collect()


    # Hilfe für Titel
    #TODO Überschrift ordentlich
    title_help <- paste0(indi, "n")

    if(absolut_selector=="In Prozent"){

      # Alle als extra Spalte anhängen und Anteil berechnen
      df_ges <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
        dplyr::filter(
          jahr %in% t &
            indikator == indi &
            landkreis == "alle Landkreise" &
            bundesland %in% states &
            anforderung == "Gesamt" &
            geschlecht == "Gesamt" &
            fachbereich == "Alle") %>%
        dplyr::select(`bundesland`, `jahr`, `indikator`, `fachbereich`, `wert`)%>%
        dplyr::rename(wert_ges = wert) %>%
        dplyr::ungroup()%>%
        dplyr::collect()

      df <- df %>%
        dplyr::left_join(df_ges, by = c("indikator", "jahr", "bundesland")) %>%
        dplyr::rename(fachbereich = "fachbereich.x")%>%
        dplyr::ungroup()%>%
        dplyr::select(-c("fachbereich.y")) %>%
        dplyr::mutate(prop = (wert/wert_ges)*100)%>%
        dplyr::mutate(prop = round(prop,1))

      df$display_rel <- prettyNum(round(df$prop,1), big.mark = ".", decimal.mark = ",")
      df <- df[with(df, order(bundesland, jahr, decreasing = FALSE)), ]

      # plot
      out <- highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = prop, group = bundesland)) %>%
        highcharter::hc_tooltip(pointFormat = "Anteil <br> Bundesland: {point.region} <br> Wert: {point.display_rel} %") %>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
        highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
        #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_title(text = paste0(faecher, "Anteil von ", title_help, " in MINT-Berufen an allen ", title_help
        ),
        margin = 45,
        align = "center",
        style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                                 "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")) %>%
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

    } else if(absolut_selector=="Anzahl"){

      hcoptslang <- getOption("highcharter.lang")
      hcoptslang$thousandsSep <- "."
      options(highcharter.lang = hcoptslang)

      df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
      df <- df[with(df, order(bundesland, jahr, decreasing = FALSE)), ]

      # plot
      out <- highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group = region)) %>%
        highcharter::hc_tooltip(pointFormat = "Anzahl: {point.display_abs}") %>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
        highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
        #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_title(text =paste0("Anzahl von ", title_help, " in MINT-Berufen"
        ),
        margin = 45,
        align = "center",
        style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                                 "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")) %>%
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
  }
  return(out)
}

#' A function to plot a single bundesland with landkreise
#'
#' @description A function to plot a map
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Arbeitsmarkt_detailliert.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_überblick_fächer <- function( r) {
  # load UI inputs from reactive value

  timerange <- r$date_arbeitsmarkt_überblick_fächer
  state <- r$state_arbeitsmarkt_überblick_fächer

  if(timerange == 2021) indikator_choice <- r$indikator_arbeitsmarkt_überblick_fächer_21
  if(timerange == 2022) indikator_choice <- r$indikator_arbeitsmarkt_überblick_fächer_22

  df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
    dplyr::filter(
      jahr == timerange &
        indikator == indikator_choice &
        landkreis == "alle Landkreise" &
        anforderung == "Gesamt" &
        geschlecht == "Gesamt"&
        bundesland == state)%>%
    dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)%>%
    dplyr::collect()


  # # filtern nach Auswahl
  # df <- df %>% dplyr::filter(jahr == timerange)
  # df <- df %>% dplyr::filter(indikator == indikator_choice)
  #
  #
  # # Anforderung und Geschlecht auf gesamt setzten
  # df <- df %>% dplyr::filter(anforderung == "Gesamt")
  # df <- df %>% dplyr::filter(geschlecht == "Gesamt")
  #
  # # in neuen DF doch Aggregate enthalten, für genauere absolute Zahlen damit arbeiten
  # df <- df %>%
  #   dplyr::filter(landkreis == "alle Landkreise")

  # Ausgewählte Region filtern
  # df <- df %>% dplyr::filter(bundesland == state)

  # MINT direkt berechnen und nicht-MINT berechnen
  df[df$fachbereich == "MINT", "wert"] <- df[df$fachbereich == "Mathematik, Naturwissenschaften", "wert"]+
    df[df$fachbereich == "Informatik", "wert"]+df[df$fachbereich == "Technik (gesamt)", "wert"]
  df$fachbereich[df$fachbereich == "MINT"]<-"MINT-Berufsfelder (gesamt)"

  # Berechnung von andere Fächergruppen
  # df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"]-
  #   df[df$fachbereich == "MINT-Berufsfelder (gesamt)"]
  df_andere <- df %>% dplyr::filter(fachbereich=="Alle")
  df_mint <- df %>% dplyr::filter(fachbereich=="MINT-Berufsfelder (gesamt)")
  df_andere$wert <- df_andere$wert - df_mint$wert
  df_andere$fachbereich[df_andere$fachbereich == "Alle"]<-"Alle Berufsfelder außer MINT (gesamt)"

  df <- rbind(df, df_andere)
  df <- df %>% dplyr::filter(fachbereich != "Alle")

  # Anteil Berechnen für aggregierte Werte MINT
  mint_agg <- df %>%
    dplyr::filter(fachbereich %in% c("MINT-Berufsfelder (gesamt)","Alle Berufsfelder außer MINT (gesamt)" )) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(prop = (wert/sum(wert))*100)%>%
    dplyr::mutate(prop= round(prop,1))
  mint_agg <-  mint_agg %>% dplyr::filter(fachbereich == "MINT-Berufsfelder (gesamt)")

  #Anteil Berechnen für Technik (gesamt)
  technik_agg <- df %>%
    dplyr::filter(fachbereich %in% c("Mathematik, Naturwissenschaften",
                                     "Informatik", "Technik (gesamt)", "Alle Berufsfelder außer MINT (gesamt)" )) %>%
    dplyr::ungroup()%>%
    dplyr::mutate(prop = (wert/sum(wert))*100)%>%
    dplyr::mutate(prop= round(prop,1))
  technik_agg <-  technik_agg %>% dplyr::filter(fachbereich == "Technik (gesamt)")

  #Anteil Berechnen für Technik-Gruppen
  df <- df %>%
    dplyr::filter(!(fachbereich %in% c("MINT-Berufsfelder (gesamt)", "Technik (gesamt)"))) %>%
    dplyr::ungroup()%>%
    dplyr::mutate(prop = (wert/sum(wert))*100)%>%
    dplyr::mutate(prop= round(prop,1))

  #Alle Werte zusammenfügen
  df <- rbind(df, mint_agg, technik_agg)

  #Trennpunkte für lange Zahlen ergänzen
  df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
  df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")

  #für Überblick unterarten von Technik wieder raus
  df <- df %>% dplyr::filter(fachbereich %in% c("Alle Berufsfelder außer MINT (gesamt)",
                                                "MINT-Berufsfelder (gesamt)",
                                                "Mathematik, Naturwissenschaften",
                                                "Informatik",
                                                "Technik (gesamt)"))

  df$fachbereich[df$fachbereich == "Technik (gesamt)"]<-"Technik"

  # Reihenfolge sortieren für Plot
  df$fachbereich <- factor(df$fachbereich, levels = c("Alle Berufsfelder außer MINT (gesamt)",
                                                      "MINT-Berufsfelder (gesamt)",
                                                      "Mathematik, Naturwissenschaften",
                                                      "Informatik",
                                                      "Technik"))
  # df$fachbereich <- factor(df$fachbereich, levels = c("Alle Berufsfelder außer MINT (gesamt)",
  #                                                              "MINT-Berufsfelder (gesamt)",
  #                                                              "Mathematik, Naturwissenschaften",
  #                                                              "Informatik",
  #                                                              "Technik (gesamt)",
  #                                                              "Bau- und Gebäudetechnik",
  #                                                              "Gesundheitstechnik",
  #                                                              "Landtechnik",
  #                                                              "Produktionstechnik",
  #                                                              "Verkehrs-, Sicherheits- u. Veranstaltungstechnik"
  #   ))

  # titel-helper
  title_help <- paste0(indikator_choice, "n")
  title_help <- ifelse(grepl("ausländische Beschäftigte", indikator_choice), "ausländischen Beschäftigten", title_help)
  title_help <- ifelse(grepl("ausländische Auszubildende", indikator_choice), "ausländischen Auszubildenden", title_help)
  title_help <- ifelse(grepl("Jahr", indikator_choice), "Auszubildenden mit neuem Lehrvertrag", title_help)
  title_help <- ifelse(grepl("u25", indikator_choice), "Beschäftigten unter 25 Jahren", title_help)
  title_help <- ifelse(grepl("25-55", indikator_choice), "Beschäftigten zwischen 25 und 55 Jahren", title_help)
  title_help <- ifelse(grepl("ü55", indikator_choice), "Beschäftigten über 55 Jahren", title_help)

  hover <- "Anteil an allen Berufsfeldern: {point.display_rel} % <br> Anzahl {point.indikator}: {point.wert}"
  if(indikator_choice == "Auszubildende (1. Jahr)") hover <- "Anteil an allen Berufsfeldern: {point.display_rel} % <br> Anzahl Auszubildende mit neuem Lehrvertrag: {point.wert}"

  # plot
  highcharter::hchart(df, 'bar', highcharter::hcaes(y = prop, x = fachbereich)) %>%
    highcharter::hc_tooltip(pointFormat = hover) %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = ""), categories =c("Alle Berufsfelder außer MINT (gesamt)",
                                                                 "MINT-Berufsfelder (gesamt)",
                                                                 "Mathematik, Naturwissenschaften",
                                                                 "Informatik",
                                                                 "Technik"
    )) %>%
    highcharter::hc_plotOptions(bar = list(
      colorByPoint = TRUE,
      colors = ifelse(df$fachbereich %in% c("Alle Berufsfelder außer MINT (gesamt)","MINT-Berufsfelder (gesamt)"), "#b16fab", "#d0a9cd")
    )) %>%
    highcharter::hc_title(text = paste0( "Überblick über die Berufsfelder von ", title_help,
                                         br(), "in ",state, " (", timerange, ")"),
                          margin = 20,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
    highcharter::hc_exporting(enabled = FALSE,
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
}

#' A function to plot time series
#'
#' @description A function to plot the time series
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_bl_vergleich <- function(r) {

  # load UI inputs from reactive value
  timerange <- r$date_bl_vergl

  if(timerange == 2021) indikator_choice <- r$indikator_bl_vergl_21
  if(timerange == 2022) indikator_choice <- r$indikator_bl_vergl_22

  feld <- r$feld_bl_vergl

  df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
    dplyr::filter(
      jahr == timerange &
        indikator == indikator_choice &
        landkreis == "alle Landkreise" &
        anforderung == "Gesamt" &
        geschlecht == "Gesamt")%>%
    dplyr::select(`bundesland`, `jahr`, `indikator`, `fachbereich`, `wert`)%>%
    dplyr::collect()

  # Zeit filtern
 # df <- df %>% dplyr::filter(jahr == timerange)

  # nur aggregierte Werte nutzen:
 # df <- df %>% dplyr::filter(landkreis == "alle Landkreise") #(inkl. DE, West, Ost)
  # Anforderungsniveau auf Gesamt
  #df <- df %>% dplyr::filter(anforderung == "Gesamt")
  #Geschelcht Gesamt auswählen
 # df <- df %>% dplyr::filter(geschlecht == "Gesamt")

  # Alle als extra Spalte anhängen und Anteil berechnen
  df_ges <- df %>%
    dplyr::filter(fachbereich == "Alle") %>%
    dplyr::rename(wert_ges = wert) %>%
    dplyr::ungroup()%>%
    dplyr::select(indikator, fachbereich, jahr, bundesland, wert_ges)

  df <- df %>%
    dplyr::left_join(df_ges, by = c("indikator", "jahr", "bundesland")) %>%
    dplyr::rename(fachbereich = "fachbereich.x")%>%
    dplyr::ungroup()%>%
    dplyr::select(-c("fachbereich.y")) %>%
    dplyr::mutate(prop = (wert/wert_ges)*100)%>%
    dplyr::mutate(prop = round(prop,1))

  #Trennpunkte für lange Zahlen in absolutem Wert ergänzen
  df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
  df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")


  df <- df%>%
    dplyr::filter(indikator==indikator_choice &
                    fachbereich==feld)

  df <- df[with(df, order(fachbereich, jahr, decreasing = FALSE)), ]


  # nur nötig für stacked, machen wir hier doch nicht
  # #gegenwert Berechnen für jeweilige Auswahl
  # df_n <- df %>% dplyr::group_by(region, indikator) %>%
  #   dplyr::mutate(proportion = 100 - proportion)
  # df_n$fachbereich <- "andere Bereiche"
  #
  # df <- rbind(df, df_n)

  # titel-helper
  title_help <- paste0(indikator_choice, "n")
  title_help <- ifelse(grepl("ausländische Beschäftigte", indikator_choice), "ausländischen Beschäftigten", title_help)
  title_help <- ifelse(grepl("ausländische Auszubildende", indikator_choice), "ausländischen Auszubildenden", title_help)
  title_help <- ifelse(grepl("Jahr", indikator_choice), "Auszubildenden im ersten Lehrjahr", title_help)
  title_help <- ifelse(grepl("u25", indikator_choice), "Beschäftigten unter 25 Jahren", title_help)
  title_help <- ifelse(grepl("25-55", indikator_choice), "Beschäftigten zwischen 25 und 55 Jahren", title_help)
  title_help <- ifelse(grepl("ü55", indikator_choice), "Beschäftigten über 55 Jahren", title_help)


  title_h2 <- ifelse(grepl("Auszu", title_help), "Auszubildenden", "Beschäftigten")
  title_h2 <- ifelse(grepl("25 Jahr", title_help), "Beschäftigten U25", title_h2)
  title_h2 <- ifelse(grepl("25 und 55", title_help), "Beschäftigten zwischen 25 und 55 Jahren", title_h2)
  title_h2 <- ifelse(grepl("über 55", title_help), "Beschäftigten Ü55", title_h2)
  title_h2 <- ifelse(grepl("jahr", title_help), "Auszubildenden im ersten Lehrjahr", title_h2)
  title_h2 <- ifelse(grepl("ausländischen Auszu", title_help), "ausländischen Auszubildenden", title_h2)
  title_h2 <- ifelse(grepl("ländischen B", title_help), "ausländischen Beschäftigten", title_h2)


  # plot
  highcharter::hchart(df, 'bar', highcharter::hcaes(y = prop, x = bundesland)) %>%
    highcharter::hc_tooltip(pointFormat = "{point.fachbereich} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.wert}") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = ""), categories =c("Deutschland",
                                                                 "Westdeutschland (o. Berlin)",
                                                                 "Ostdeutschland (einschl. Berlin)",
                                                                 "Baden-Württemberg",
                                                                 "Bayern",
                                                                 "Berlin",
                                                                 "Brandenburg",
                                                                 "Bremen",
                                                                 "Hamburg",
                                                                 "Hessen",
                                                                 "Mecklenburg-Vorpommern",
                                                                 "Niedersachsen",
                                                                 "Nordrhein-Westfalen",
                                                                 "Rheinland-Pfalz",
                                                                 "Saarland",
                                                                 "Sachsen",
                                                                 "Sachsen-Anhalt",
                                                                 "Schleswig-Holstein",
                                                                 "Thüringen")
    ) %>%
    # highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
    # highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
    highcharter::hc_plotOptions(bar = list(
      colorByPoint = TRUE,
      colors = ifelse(df$bundesland %in% c("Deutschland","Westdeutschland (o. Berlin)",
                                           "Ostdeutschland (einschl. Berlin)"), "#d0a9cd", "#b16fab")
    )) %>%
    # highcharter::hc_colors( "#b16fab") %>%
    highcharter::hc_title(text = paste0( "Anteil von ", title_help, " in ", feld, " an allen ", title_h2, " in ", timerange,
                                         "<br><br><br>"),
                          margin = 20,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
    highcharter::hc_exporting(enabled = FALSE,
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(feld = list(hover = list(fill = '#FFFFFF'))))))

  # ggplot2::ggplot(df, ggplot2::aes(y=region, x=proportion)) +
  #   ggplot2::geom_bar(stat="identity", fill = "#b16fab") +
  #   ggplot2::geom_text(ggplot2::aes(label=paste(round(proportion),"%")), hjust = -0.3,
  #                      fontface = "bold") +
  #   ggplot2::theme_minimal() +
  #   ggplot2::theme(
  #     text = ggplot2::element_text(size = 14),
  #     plot.title = ggtext::element_markdown(hjust = 0.5)) +
  #   ggplot2::ylab("") + ggplot2::xlab("") +
  #   ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
  #                               "Anteil von MINT-Berufen (", indikator_choice, ")",
  #                                br(), timerange,
  #                                "<br><br><br>"),
  #                 fill = "") +
  #   ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))
}

arbeitsmarkt_top10 <- function( r){

  # UI Input zuweisen
  time <- r$date_top_beruf
  bula <- r$states_top_beruf
  abs_rel <- r$betr_abs_rel
  fb <- r$FB_top_beruf


  df <- dplyr::tbl(con, from = "data_naa") %>%
    dplyr::filter(
      jahr == time &
        ebene == "Ebene 3" &
        region == bula )%>%
    dplyr::select(-code)%>%
    dplyr::collect()

  df <- df %>% dplyr::ungroup() %>%
    dplyr::mutate(region = dplyr::case_when(
      region == "Westdeutschland (ohne Berlin)" ~ "Westdeutschland (o. Berlin)",
      region == "Ostdeutschland (mit Berlin)" ~ "Ostdeutschland (inkl. Berlin)",
      T ~ .$region
    ))

  # un-groupen
  # df <- df %>%dplyr::ungroup()

  # West- / Ost umbenennen, dass es zum Imput passt
  # df$region[df$region == "Westdeutschland (ohne Berlin)"] <- "Westdeutschland (o. Berlin)"
  # df$region[df$region == "Ostdeutschland (mit Berlin)"] <- "Ostdeutschland (inkl. Berlin)"
  #
  # # Filtern
  # df <- df %>% dplyr::filter(df$ebene == "Ebene 3") #Ebene der einzelnen Berufe aus Datensatz ausfiltern
  # df <- df %>% dplyr::filter(df$jahr == time) #Jahr auswählen
  # df <- df %>% dplyr::filter(df$region == bula) #gewähltes Bundesland filtern
  # df <- df %>% dplyr::select(-code)
  #
  # Auswahl Fachbereich
  if(fb != "MINT (gesamt)"){
    df <- df %>% dplyr::filter(fachbereich == fb)
  }

  # zu gering besetzte Ausbildungen ausfiltern
  df <- df %>% dplyr::filter(df$wert > 8)

  # Anteile von Frauen/Männern berechnen
  # Gesamt als eigenen Df speichern, mit dem Anteil berechnet wird
  df_gesamt <- df %>% dplyr::filter(geschlecht == "Gesamt")

  # Anteil berechnen und Geschlecht Gesamt ausfiltern
  df <- df %>%
    dplyr::left_join(df_gesamt,
                     by = c("region",
                            "jahr",
                            "fachbereich",
                            "beruf")) %>%
    dplyr::mutate(prop = round((wert.x/wert.y)*100,1)) %>%
    dplyr::rename(wert = wert.x,
                  wert_ges = wert.y,
                  geschlecht = geschlecht.x,
                  ebene = ebene.x) %>%
    dplyr::select(-c("geschlecht.y", "ebene.y")) %>%
    dplyr::filter(geschlecht != "Gesamt")


  # Split dataframe by gender and create plots
  if(abs_rel == "In Prozent"){

    # female
    berufe_frauen <- df %>%
      dplyr::filter(geschlecht == "Frauen") %>%
      dplyr::arrange(desc(prop)) %>%
      dplyr::slice(1:10)

    # male
    berufe_maenner <- df %>%
      dplyr::filter(geschlecht == "Männer") %>%
      dplyr::arrange(desc(prop)) %>%
      dplyr::slice(1:10)


    if(sum(berufe_maenner$prop)==1000){ #falls alle 10 Berufe 100 % sind
      berufe_maenner <- df %>%
        dplyr::filter(geschlecht == "Männer") %>%
        dplyr::filter(prop == 100) %>% #diese 100% Jobs auswählen
        dplyr::arrange(desc(wert)) %>% #und die anzeigen, die am stärksten besetzt sind von diesen
        dplyr::slice(1:10)
    }

    # Create female plot
    plot_frau <- highcharter::hchart(berufe_frauen, 'bar', highcharter::hcaes(y = prop, x = beruf)) %>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, format = "{point.prop} %",
                            style = list(textOutline = "none"))
        )) %>%
      highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil an allen neuen MINT-Ausbildungsverträgen: {point.y} % <br> Anzahl der neu abgeschlossenen Ausbildugnsverträge: {point.wert}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value} %", rotation = -45), min = 0, max = 100, tickInterval = 10) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_colors(c("#154194")) %>%
      highcharter::hc_title(text = paste0("Höchster Frauenanteil unter den neuen Auszubildenden im Fachbereich " ,fb ," in ", bula, " (", time, ")"),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE)
    # %>%
    #   highcharter::hc_exporting(enabled = TRUE,
    #                             buttons = list(contextButton = list(
    #                               symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
    #                               onclick = highcharter::JS("function () {
    #                                                           this.exportChart({ type: 'image/jpeg' }); }"),
    #                               align = 'right',
    #                               verticalAlign = 'bottom',
    #                               theme = list(states = list(hover = list(fill = '#FFFFFF'))))))


    # Create male plot
    plot_mann <- highcharter::hchart(berufe_maenner, 'bar', highcharter::hcaes(y = prop, x = beruf)) %>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE,
                            format = "{point.prop} %",
                            style = list(textOutline = "none"))
        )) %>%
      highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil an allen neuen MINT-Ausbildungsverträgen: {point.y} % <br> Anzahl der neu abgeschlossenen Ausbildugnsverträge: {point.wert}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value} %", rotation = -45), min = 0, max = 100, tickInterval = 10) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_colors(c("#66cbaf")) %>%
      highcharter::hc_title(text = paste0("Höchster Männeranteil unter den neuen Auszubildenden im Fachbereich ", fb ," in ", bula ," (", time, ")"),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE)
    # %>%
    #   highcharter::hc_exporting(enabled = TRUE,
    #                             buttons = list(contextButton = list(
    #                               symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
    #                               onclick = highcharter::JS("function () {
    #                                                           this.exportChart({ type: 'image/jpeg' }); }"),
    #                               align = 'right',
    #                               verticalAlign = 'bottom',
    #                               theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

  } else if(abs_rel == "Anzahl"){

    df <- df %>%
      dplyr::mutate(display_abs = prettyNum(df$wert, big.mark = ".", decimal.mark = ","))
    # female
    berufe_frauen <- df %>%
      dplyr::filter(geschlecht == "Frauen") %>%
      dplyr::arrange(desc(wert)) %>%
      dplyr::slice(1:10)

    # male
    berufe_maenner <- df %>%
      dplyr::filter(geschlecht == "Männer") %>%
      dplyr::arrange(desc(wert)) %>%
      dplyr::slice(1:10)

    # Create female plot
    plot_frau <- highcharter::hchart(berufe_frauen, 'bar', highcharter::hcaes(y = wert, x = beruf)) %>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, format = "{point.display_abs}",
                            style = list(textOutline = "none"))
        )) %>%
      highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.prop} % <br> Anzahl: {point.display_abs}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}", rotation = -45), min = 0, max = plyr::round_any(max(berufe_frauen$wert), 500, f = ceiling), tickInterval = 1000) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_colors(c("#154194")) %>%
      highcharter::hc_title(text = paste0("Am häufigsten gewählte MINT-Ausbildungsberufe von weiblichen Neu-Auszubildenden im Fachbereich " ,fb ,"  in ", bula ," (", time, ")"),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE)
    # %>%
    #   highcharter::hc_exporting(enabled = TRUE,
    #                             buttons = list(contextButton = list(
    #                               symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
    #                               onclick = highcharter::JS("function () {
    #                                                           this.exportChart({ type: 'image/jpeg' }); }"),
    #                               align = 'right',
    #                               verticalAlign = 'bottom',
    #                               theme = list(states = list(hover = list(fill = '#FFFFFF'))))))


    # Create male plot
    plot_mann <- highcharter::hchart(berufe_maenner, 'bar', highcharter::hcaes(y = wert, x = beruf)) %>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, format = "{point.display_abs}",
                            style = list(textOutline = "none"))
        )) %>%
      highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.prop} % <br> Absolut: {point.display_abs}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}", rotation = -45), min = 0, max = plyr::round_any(max(berufe_maenner$wert), 1000, f = ceiling), tickInterval = 1000) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_colors(c("#66cbaf")) %>%
      highcharter::hc_title(text = paste0("Am häufigsten gewählte MINT-Ausbildungsberufe von männlichen Neu-Auszubildenden im Fachbereich ",fb," in ", bula ," (", time, ")"),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE)
    # %>%
    #   highcharter::hc_exporting(enabled = TRUE,
    #                             buttons = list(contextButton = list(
    #                               symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
    #                               onclick = highcharter::JS("function () {
    #                                                           this.exportChart({ type: 'image/jpeg' }); }"),
    #                               align = 'right',
    #                               verticalAlign = 'bottom',
    #                               theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

  }

  out <- list(
    plot_frau,
    plot_mann)

  return(out)


}

# Frauen in MINT ----

#' A function to plot a pic charts
#'
#' @description A function to create pie charts for the tab "Beruf".
#'
#' @return The return value is a plot
#' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_einstieg_pie_gender <- function(r) {

  # load UI inputs from reactive value
  betrachtung <- r$ansicht_arbeitsmarkt_einstieg_gender
  timerange <- r$date_arbeitsmarkt_einstieg_gender
  regio <- r$region_arbeitsmarkt_einstieg_gender
  faecher <- r$fachbereich_arbeitsmarkt_einstieg_gender

  if(betrachtung == "Einzelansicht - Kuchendiagramm"){
    indi <- r$indikator_arbeitsmarkt_einsteig_gender_pie
    gegenwert <- r$arbeitsmarkt_gender_gegenwert_pie
  }else{
    indi <- r$indikator_arbeitsmarkt_einsteig_gender_balken
    gegenwert <- r$arbeitsmarkt_gender_gegenwert_balken
  }

  df <- dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
    dplyr::filter(jahr == timerange &
                    landkreis == "alle Landkreise" &
                    bundesland == regio &
                    anforderung == "Gesamt" &
                    geschlecht != "Gesamt" &
                    indikator %in% indi &
                    fachbereich == faecher)%>%
    dplyr::select( "indikator", "bundesland", "fachbereich", "jahr", "geschlecht", "wert") %>%
    dplyr::collect()

  df_alle <- dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
    dplyr::filter(jahr == timerange &
                    landkreis == "alle Landkreise" &
                    bundesland == regio &
                    anforderung == "Gesamt" &
                    geschlecht == "Gesamt" &
                    indikator %in% indi &
                    fachbereich == faecher)%>%
    dplyr::select( "indikator", "bundesland", "fachbereich", "jahr", "geschlecht", "wert") %>%
    dplyr::rename(wert_gesamt = "wert") %>%
    dplyr::collect()

  df <- df %>%
    dplyr::left_join(df_alle, by = c("indikator", "bundesland", "jahr", "fachbereich")) %>%
    dplyr::rename(geschlecht = geschlecht.x) %>%
    dplyr::select(-geschlecht.y) %>%
    dplyr::group_by(indikator, geschlecht) %>%
    dplyr::mutate(proportion = round((wert/wert_gesamt)*100,1))

  if(gegenwert == "Ja"){

    df_andere <- dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
      dplyr::filter(jahr == timerange &
                      landkreis == "alle Landkreise" &
                      bundesland == regio &
                      anforderung == "Gesamt" &
                   #   geschlecht != "Gesamt" &
                      indikator %in% indi &
                      fachbereich == faecher)%>%
      dplyr::select( "indikator", "bundesland", "fachbereich", "jahr", "geschlecht", "wert") %>%
      dplyr::collect()

    df_alle_faecher <- dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
      dplyr::filter(jahr == timerange &
                      landkreis == "alle Landkreise" &
                      bundesland == regio &
                      anforderung == "Gesamt" &
                     # geschlecht != "Gesamt" &
                      indikator %in% indi &
                      fachbereich == "Alle")%>%
      dplyr::select( "indikator", "bundesland", "fachbereich", "jahr", "geschlecht", "wert") %>%
      dplyr::rename(wert_gesamt = "wert") %>%
      dplyr::collect()

    df_andere <- df_andere %>%
      dplyr::left_join(df_alle_faecher, by = c("indikator", "bundesland", "jahr", "geschlecht")) %>%
      dplyr::rename(fachbereich = fachbereich.x) %>%
      dplyr::select(-fachbereich.y) %>%
      dplyr::group_by(indikator, geschlecht) %>%
      dplyr::mutate(wert = wert_gesamt- wert)
    df_andere$fachbereich <- "Andere Berufe"

    df_andere_ges <- subset(df_andere, geschlecht == "Gesamt")

    df_andere_ges <- df_andere_ges %>%
      dplyr::select(-wert_gesamt) %>%
      dplyr::rename(wert_gesamt = wert)

    df_andere <- df_andere %>%
      dplyr::filter(geschlecht != "Gesamt") %>%
      dplyr::select(-wert_gesamt)

    df_andere <- df_andere %>%
      dplyr::left_join(df_andere_ges, by = c("indikator", "bundesland", "jahr", "fachbereich")) %>%
      dplyr::rename(geschlecht = geschlecht.x) %>%
      dplyr::select(-geschlecht.y) %>%
      dplyr::group_by(indikator, geschlecht) %>%
      dplyr::mutate(proportion = round((wert/wert_gesamt)*100,1))

    df <- rbind(df, df_andere)
  }

  #Trennpunkte für lange Zahlen ergänzen
  df$wert_disp <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
  df$prop_disp <- prettyNum(df$proportion, big.mark = ".", decimal.mark = ",")

  if(betrachtung == "Einzelansicht - Kuchendiagramm"){
   if(length(indi) == 1) {
     df_p <- df[df$fachbereich == faecher,]
     p1 <- highcharter::hchart(df_p, size = 280, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
       highcharter::hc_tooltip(
         pointFormat=paste('Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}')) %>%
       highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
       highcharter::hc_title(text = paste0("Frauenanteil unter ",indi[1], " in ", faecher[1], " (", timerange, ")"),
                             margin = 45,
                             align = "center",
                             style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
       highcharter::hc_chart(
         style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
       highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
       #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
       highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                              dataLabels = list(enabled = TRUE,  format='{point.prop_disp}%'), showInLegend = TRUE)
       )
     out <- p1
     if(gegenwert == "Ja"){
       df_g <- df[df$fachbereich == "Andere Berufe",]

       p1g <- highcharter::hchart(df_g, size = 150, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
         highcharter::hc_tooltip(
           pointFormat=paste('Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}')) %>%
         highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
         highcharter::hc_title(text = paste0("Frauenanteil unter ",indi[1], " in Nicht MINT-Berufen (", timerange, ")"),
                               margin = 45,
                               align = "center",
                               style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
         highcharter::hc_chart(
           style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
         highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
         #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
         highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                                dataLabels = list(enabled = TRUE,  format='{point.prop_disp}%'), showInLegend = TRUE)
         )

       out <- highcharter::hw_grid(p1, p1g,
                                   ncol = 2,
                                   browsable = TRUE)

     }

   } else if(length(indi) == 2) {

     df_1_pie <- df %>% dplyr::filter(indikator == indi[1], fachbereich != "Andere Berufe")
     df_2_pie <- df %>% dplyr::filter(indikator == indi[2], fachbereich != "Andere Berufe")

     p1<- highcharter::hchart(df_1_pie, size = 280, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
       highcharter::hc_tooltip(
         pointFormat=paste('Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}')) %>%
       highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
       highcharter::hc_title(text=paste0("Frauenanteil unter ",indi[1], " in ", faecher[1], " (", timerange, ")"),
                             margin = 45,
                             align = "center",
                             style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
       highcharter::hc_chart(
         style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
       highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
       highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                              dataLabels = list(enabled = TRUE,  format='{point.prop_disp}%'), showInLegend = TRUE))


     p2 <- highcharter:: hchart(df_2_pie, size = 280, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion))%>%
       highcharter::hc_tooltip(
         pointFormat=paste('Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}'))%>%
       highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
       highcharter::hc_title(text=paste0("Frauenanteil unter ",indi[2], " in ", faecher[1], " (", timerange, ")"),
                             margin = 45,
                             align = "center",
                             style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
       highcharter::hc_chart(
         style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
       highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
       #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
       highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                              dataLabels = list(enabled = TRUE, format='{point.prop_disp}%'), showInLegend = TRUE))

     out<- highcharter::hw_grid(
       p1, p2,
       ncol = 2,
       browsable = TRUE
     )
     if(gegenwert == "Ja"){
       df1_g <- df[df$fachbereich == "Andere Berufe" & df$indikator == indi[1],]
       df2_g <- df[df$fachbereich == "Andere Berufe" & df$indikator == indi[2],]


       p1g <- highcharter::hchart(df1_g, size = 150, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
         highcharter::hc_tooltip(
           pointFormat=paste('Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}')) %>%
         highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
         highcharter::hc_title(text = paste0("Frauenanteil unter ",indi[1], " in Nicht MINT-Berufen (", timerange, ")"),
                               margin = 45,
                               align = "center",
                               style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
         highcharter::hc_chart(
           style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
         highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
         #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
         highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                                dataLabels = list(enabled = TRUE,  format='{point.prop_disp}%'), showInLegend = TRUE)
         )

       p2g <- highcharter::hchart(df2_g, size = 150, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
         highcharter::hc_tooltip(
           pointFormat=paste('Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}')) %>%
         highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
         highcharter::hc_title(text = paste0("Frauenanteil unter ",indi[2], " in Nicht MINT-Berufen (", timerange, ")"),
                               margin = 45,
                               align = "center",
                               style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
         highcharter::hc_chart(
           style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
         highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
         #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
         highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                                dataLabels = list(enabled = TRUE,  format='{point.prop_disp}%'), showInLegend = TRUE)
         )

       out <- highcharter::hw_grid(p1, p2,
                                   p1g, p2g,
                                   ncol = 2,
                                   browsable = TRUE)

     }

   } else if(length(indi) == 3) {

     df_1_pie <- df %>% dplyr::filter(indikator == indi[1], fachbereich != "Andere Berufe")
     df_2_pie <- df %>% dplyr::filter(indikator == indi[2], fachbereich != "Andere Berufe")
     df_3_pie <- df %>% dplyr::filter(indikator == indi[3], fachbereich != "Andere Berufe")

     p1 <- highcharter::hchart(df_1_pie, size = 170, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
       highcharter::hc_tooltip(
         pointFormat=paste('Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}')) %>%
       highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
       highcharter::hc_title(text=paste0("Frauenanteil unter ",indi[1], " in ", faecher[1], " (", timerange, ")"),
                             margin = 45,
                             align = "center",
                             style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
       highcharter::hc_chart(
         style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
       highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
       highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                              dataLabels = list(enabled = TRUE,  format='{point.prop_disp}%'), showInLegend = TRUE))


     p2 <- highcharter::hchart(df_2_pie, size = 170, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
       highcharter::hc_tooltip(
         pointFormat=paste('Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}')) %>%
       highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
       highcharter::hc_title(text=paste0("Frauenanteil unter ",indi[2], " in ", faecher[1], " (", timerange, ")"),
                             margin = 45,
                             align = "center",
                             style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
       highcharter::hc_chart(
         style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
       highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
       highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                              dataLabels = list(enabled = TRUE, format='{point.prop_disp}%'), showInLegend = TRUE))


     p3 <-  highcharter::hchart(df_3_pie, size = 170, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
       highcharter::hc_tooltip(
         pointFormat=paste('Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}')) %>%
       highcharter::hc_colors(c( "#efe8e6", "#154194")) %>%
       highcharter::hc_title(text=paste0("Frauenanteil unter ",indi[3], " in ", faecher[1], " (", timerange, ")"),
                             margin = 45,
                             align = "center",
                             style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
       highcharter::hc_chart(
         style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
       highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
       #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
       highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                              dataLabels = list(enabled = TRUE,  format='{point.prop_disp}%'), showInLegend = TRUE))


     out <- highcharter::hw_grid(
       p1, p2, p3,
       ncol = 3,
       browsable = TRUE
     )

     if(gegenwert == "Ja"){
       df1_g <- df[df$fachbereich == "Andere Berufe" & df$indikator == indi,]
       df2_g <- df[df$fachbereich == "Andere Berufe" & df$indikator == indi,]
       df3_g <- df[df$fachbereich == "Andere Berufe" & df$indikator == indi,]

       p1g <- highcharter::hchart(df1_g, size = 150, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
         highcharter::hc_tooltip(
           pointFormat=paste('Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}')) %>%
         highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
         highcharter::hc_title(text = paste0("Frauenanteil unter ",indi[1], " in Nicht MINT-Berufe (", timerange, ")"),
                               margin = 45,
                               align = "center",
                               style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
         highcharter::hc_chart(
           style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
         highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
         #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
         highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                                dataLabels = list(enabled = TRUE,  format='{point.prop_disp}%'), showInLegend = TRUE)
         )

       p2g <- highcharter::hchart(df2_g, size = 150, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
         highcharter::hc_tooltip(
           pointFormat=paste('Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}')) %>%
         highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
         highcharter::hc_title(text = paste0("Frauenanteil unter ",indi[2], " in Nicht MINT-Berufe (", timerange, ")"),
                               margin = 45,
                               align = "center",
                               style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
         highcharter::hc_chart(
           style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
         highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
         #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
         highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                                dataLabels = list(enabled = TRUE,  format='{point.prop_disp}%'), showInLegend = TRUE)
         )

       p3g <- highcharter::hchart(df3_g, size = 150, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
         highcharter::hc_tooltip(
           pointFormat=paste('Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}')) %>%
         highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
         highcharter::hc_title(text = paste0("Frauenanteil unter ",indi[3], " in Nicht MINT-Berufe (", timerange, ")"),
                               margin = 45,
                               align = "center",
                               style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
         highcharter::hc_chart(
           style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
         highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
         #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
         highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                                dataLabels = list(enabled = TRUE,  format='{point.prop_disp}%'), showInLegend = TRUE)
         )
       out <- highcharter::hw_grid(p1, p2, p3,
                                   p1g, p2g, p3g,
                                   ncol = 3,
                                   browsable = TRUE)

     }

   }
  }
  else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){

    df$indi_fach <- paste0(df$indikator, " - ", df$fachbereich)

    # plot
   out <- highcharter::hchart(df, 'bar', highcharter::hcaes( x = indi_fach, y=proportion, group = geschlecht)) %>%
      highcharter::hc_tooltip(pointFormat = "{point.geschlecht}-Anteil: {point.y} % <br> Anzahl: {disp_wert}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  FALSE) %>%
      highcharter::hc_xAxis(title = list(text = "") ) %>%
      highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
      highcharter::hc_colors(c("#154194", "#efe8e6")) %>%
      highcharter::hc_title(text = paste0("Frauenanteil in MINT- und anderen Berufen (", timerange, ")",
                                          "<br><br><br>"),
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

  return(out)

}

#' A function to plot time series
#'
#' @description A function to plot the time series
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_einstieg_verlauf_gender <- function(r) {

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt_verlauf_gender
  t <-timerange[1]:timerange[2]
  indi <- r$indikator_arbeitsmarkt_verlauf_gender
  faecher <- r$fachbereich_arbeitsmarkt_verlauf_gender
  regio <- r$region_arbeitsmarkt_verlauf_gender
  absolut_selector <- r$abs_zahlen_arbeitsmarkt_verlauf_gender

  df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
    dplyr::filter(jahr %in% t,
                  landkreis == "alle Landkreise",
                  bundesland == regio,
                  anforderung == "Gesamt",
                  fachbereich == faecher,
                  indikator %in% indi,
                  geschlecht == "Frauen")%>%
    dplyr::select(jahr, indikator, geschlecht, bundesland, wert, fachbereich)%>%
    dplyr::collect()

  if(absolut_selector=="In Prozent"){

    df_gen_alle <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
      dplyr::filter(jahr %in% t,
                    landkreis == "alle Landkreise",
                    bundesland == regio,
                    anforderung == "Gesamt",
                    fachbereich == faecher,
                    indikator %in% indi,
                    geschlecht == "Gesamt")%>%
      dplyr::select(jahr, indikator, geschlecht, bundesland, wert, fachbereich)%>%
      dplyr::rename(wert_ges = wert) %>%
      dplyr::collect()

    df <- df %>% dplyr::left_join(df_gen_alle,
                                  by = c("jahr", "indikator", "bundesland", "fachbereich")) %>%
      dplyr::rename(geschlecht = geschlecht.x) %>%
      dplyr::select(-geschlecht.y) %>%
      dplyr::mutate(prop = round(wert/wert_ges * 100, 1)) %>%
      dplyr::filter(geschlecht != "Gesamt")


    # order years for plot
    df$prop_disp <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")
    df <- df[with(df, order(jahr, decreasing = FALSE)), ]

    # plot

    ###vorbereitung titel
    combine_with_and <- function(items) {
      if (length(items) == 1) {
        return(items)
      } else if (length(items) == 2) {
        return(paste(items, collapse = " und "))
      } else {
        return(paste(paste(items[-length(items)], collapse = ", "), "und", items[length(items)]))
      }
    }

    # Kombiniere die Indikatoren mit Komma und "und", falls es mehrere gibt
    indi_text <- combine_with_and(indi)

    # Kombiniere auch den Fachbereich auf gleiche Weise
    faecher_text <- combine_with_and(faecher)

    # Generiere den Titel dynamisch
    title_text <- paste0("Frauenanteil in den Gruppen ", indi_text, " im Feld ", faecher_text, " in ", regio)

    highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = prop, group = indikator)) %>%
      highcharter::hc_tooltip(pointFormat = "Anteil Frauen  {point.indikator} <br> Wert: {point.prop_disp} %") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      # highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
      # highcharter::hc_title(text = paste0("Frauenanteil in der Gruppe ", indi," im Feld ", faecher," in ", regio), #error 3 ist leer
      #                       margin = 45,
      #                       align = "center",
      #                       style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_title(text = title_text,   # Verwende den dynamisch generierten Titel
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                               "#bfc6d3", "#5f94f9")) %>%
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



  } else if(absolut_selector=="Anzahl"){

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    df$wert_disp <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df <- df[with(df, order(jahr, decreasing = FALSE)), ]

    # plot



    combine_with_and <- function(items) {
      if (length(items) == 1) {
        return(items)
      } else if (length(items) == 2) {
        return(paste(items, collapse = " und "))
      } else {
        return(paste(paste(items[-length(items)], collapse = ", "), "und", items[length(items)]))
      }
    }

    # Kombiniere die Indikatoren mit Komma und "und", falls es mehrere gibt
    indi_text <- combine_with_and(indi)

    # Kombiniere auch den Fachbereich auf gleiche Weise
    faecher_text <- combine_with_and(faecher)

    # Generiere den Titel dynamisch
    title_text <- paste0("Frauenanteil in den Gruppen ", indi_text, " im Feld ", faecher_text, " in ", regio)

    highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group = indikator)) %>%
      highcharter::hc_tooltip(pointFormat = "Anzahl: {point.wert_disp}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      # highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
      # highcharter::hc_title(text = paste0("Anzahl an Frauen in MINT-Ausbildungen und -Beschäftigungen"),
      #                       margin = 45,
      #                       align = "center",
      #                       style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      # highcharter::hc_colors(c("#b16fab", "#154194")) %>%
      highcharter::hc_title(text = title_text,   # Verwende den dynamisch generierten Titel
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                               "#bfc6d3", "#5f94f9")) %>%
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
}

#' A function to plot time series
#'
#' @description A function to plot a bar chart
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_wahl_gender <- function(r) {

  betrachtung <- r$ansicht_arbeitsmarkt_wahl_gender

   if(betrachtung == "Einzelansicht - Kuchendiagramm"){
     color_fachbereich <- c(
       "Informatik" = "#2D6BE1",
       "Technik (gesamt)" = "#00a87a",
       "Mathematik, Naturwissenschaften" = "#fcc433",
       "andere Berufsfelder" = "#efe8e6"
     )
    timerange <- r$date_arbeitsmarkt_wahl_gender_pie
    indi <- r$level_arbeitsmarkt_wahl_gender_pie
    regio <- r$region_arbeitsmarkt_wahl_gender_pie

    df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
     dplyr::filter(jahr %in% timerange &
                   landkreis == "alle Landkreise",
                   bundesland == regio &
                   geschlecht != "Gesamt"&
                   anforderung == "Gesamt" &
                   indikator == indi,
                   fachbereich %in% c("Alle", "MINT", "Mathematik, Naturwissenschaften",
                                        "Informatik", "Technik (gesamt)"))%>%
     dplyr::select(jahr, bundesland, indikator, fachbereich, wert, geschlecht) %>%
     dplyr::collect()

    # Berechnung von andere Fächergruppen
    df[df$fachbereich == "Alle" & df$geschlecht == "Frauen", "wert"] <- df[df$fachbereich == "Alle" & df$geschlecht == "Frauen", "wert"]-
      df[df$fachbereich == "MINT" & df$geschlecht == "Frauen", "wert"]
    df[df$fachbereich == "Alle" & df$geschlecht == "Männer", "wert"] <- df[df$fachbereich == "Alle" & df$geschlecht == "Männer", "wert"]-
      df[df$fachbereich == "MINT" & df$geschlecht == "Männer", "wert"]
    df$fachbereich[df$fachbereich == "Alle"]<-"andere Berufsfelder"
    df <- df %>% dplyr::filter(fachbereich != "MINT")

     # Anteil berechnen
    df_alle <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
      dplyr::filter(jahr %in% timerange &
                      landkreis == "alle Landkreise",
                    bundesland == regio &
                      geschlecht != "Gesamt"&
                      anforderung == "Gesamt" &
                      indikator == indi,
                    fachbereich == "Alle")%>%
      dplyr::select(jahr, bundesland, indikator, fachbereich, wert, geschlecht) %>%
      dplyr::rename(wert_ges = wert) %>%
      dplyr::collect()

     df <- df %>% dplyr::left_join(df_alle, by = c("jahr", "bundesland", "indikator",
                                                  "geschlecht")) %>%
      dplyr::rename(fachbereich = fachbereich.x) %>%
      dplyr::select(-fachbereich.y) %>%
      dplyr::mutate(prop = round(wert/wert_ges *100, 1))

     # nach Geschlechtern trennen
     df$wert_disp <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
     df$prop_disp <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")
     df_f <- df %>% dplyr::filter(geschlecht=="Frauen")
     df_m <- df %>% dplyr::filter(geschlecht=="Männer")

     # Titel für Plots
     title_help <- paste0(indi, "n <br>")
     title_help <- ifelse(grepl("ausländische Beschäftigte", indi), "ausländischen <br> Beschäftigten", title_help)
     title_help <- ifelse(grepl("ausländische Auszubildende", indi), "ausländischen <br> Auszubildenden", title_help)
     title_help <- ifelse(grepl("Jahr", indi), "Auszubildenden <br> mit neuem Lehrvertrag <br>", title_help)

     df_f <- df_f[with(df_f, order(prop, decreasing = FALSE)), ]
     df_f <- df_f %>%
       dplyr::mutate(color = color_fachbereich[fachbereich])

     df_m <- df_m[with(df_m, order(prop, decreasing = FALSE)), ]
     df_m <- df_m %>%
       dplyr::mutate(color = color_fachbereich[fachbereich])

     out_1 <- df_f %>%
       highcharter::hchart(
         size = 280, type ="pie", mapping = highcharter::hcaes(x = fachbereich , y = prop)) %>%
       highcharter::hc_tooltip(
         pointFormat=paste('Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}')) %>%
       highcharter::hc_colors(as.character(df_f$color)) %>%
       highcharter::hc_title(text = paste0("Berufswahl unter Frauen in ", regio, " (", timerange, ")"),
                             margin = 45,
                             align = "center",
                             style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
       highcharter::hc_subtitle(text = paste0("Anteile von Frauen, die MINT-Berufe wählen, an allen weiblichen ", indi, "n"))%>%
       highcharter::hc_chart(
         style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
       highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
       highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                              dataLabels = list(enabled = TRUE,  format='{point.prop_disp}%'), showInLegend = TRUE))

     #subtitle
     hc_subtitle_text <- paste0("Anteile von Männern, die MINT-Berufe wählen, an allen männlichen ",
                                ifelse(grepl("Auszubildende \\(mit neuem Lehrvertrag\\)", indi),
                                       "Auszubildenden (1. Jahr)",
                                       paste0(indi, "n"))
     )

     out_2 <- df_m %>%
       highcharter::hchart(
         size = 280, type ="pie", mapping = highcharter::hcaes(x = fachbereich , y = prop)) %>%
       highcharter::hc_tooltip(
         pointFormat=paste('Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}')) %>%
       highcharter::hc_colors(as.character(df_m$color)) %>%
       highcharter::hc_title(text = paste0("Berufswahl unter Männern in ", regio, " (", timerange, ")"),
                             margin = 45,
                             align = "center",
                             style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
       # highcharter::hc_subtitle(text = paste0("Anteile von Männern, die MINT-Berufe wählen, an allen männlichen ", indi,"n")) %>%
       highcharter::hc_subtitle(text = hc_subtitle_text) %>%
       highcharter::hc_chart(
         style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
       highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
       highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                              dataLabels = list(enabled = TRUE,  format='{point.prop_disp}%'), showInLegend = TRUE))

     out <- highcharter::hw_grid(
       out_1, out_2,
       ncol = 2,
       browsable = TRUE
     )

   }else
     if(betrachtung == "Übersicht - Kartendiagramm"){
     timerange <- r$date_arbeitsmarkt_wahl_gender_karte
     indi <- r$level_arbeitsmarkt_wahl_gender_karte
     faecher <- r$fach_arbeitsmarkt_wahl_gender_karte

     df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
       dplyr::filter(jahr %in% timerange &
                       !bundesland %in% c("Deutschland",
                                          "Westdeutschland (o. Berlin)",
                                          "Ostdeutschland (einschl. Berlin)") &
                       landkreis == "alle Landkreise" &
                       geschlecht != "Gesamt"&
                       anforderung == "Gesamt",
                     indikator == indi,
                     fachbereich == faecher)%>%
       dplyr::select(indikator, fachbereich, wert, geschlecht, bundesland, jahr) %>%
       dplyr::collect()

     df_alle <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
       dplyr::filter(jahr %in% timerange &
                       !bundesland %in% c("Deutschland",
                                          "Westdeutschland (o. Berlin)",
                                          "Ostdeutschland (einschl. Berlin)") &
                       landkreis == "alle Landkreise" &
                       geschlecht != "Gesamt"&
                       anforderung == "Gesamt",
                     indikator == indi,
                     fachbereich == "Alle")%>%
       dplyr::select(indikator, fachbereich, wert, geschlecht, bundesland, jahr) %>%
       dplyr::rename(wert_ges = wert) %>%
       dplyr::collect()

     df <- df %>%
       dplyr::left_join(df_alle, by = c("bundesland", "jahr", "geschlecht", "indikator")) %>%
       dplyr::rename(fachbereich = fachbereich.x) %>%
       dplyr::select(-fachbereich.y) %>%
       dplyr::mutate(prop = round(wert/wert_ges*100,1))%>%
       dplyr::filter(fachbereich != "Alle")

     #Gerundetes Prop für Hover:
     df$prop_disp <- prettyNum(round(df$prop, 1), big.mark = ".", decimal.mark = ",")
     df$wert_disp <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

     values_female <- df %>% dplyr::filter(geschlecht == "Frauen")
     values_male <- df %>% dplyr::filter(geschlecht == "Männer")

     #Überschrift erstellen
     title_help <- paste0(indi, "r")
     title_help <- ifelse(grepl("ausländische Beschäftigte", indi), "ausländischer Beschäftigter", title_help)
     title_help <- ifelse(grepl("ausländische Auszubildende", indi), "ausländischer Auszubildender", title_help)
     title_help <- ifelse(grepl("Jahr", indi), "Auszubildender mit neuem Lehrvertrag", title_help)
     # title_help <- ifelse(grepl("u25", indi), "Beschäftigten unter 25 Jahren", title_help)
     # title_help <- ifelse(grepl("25-55", indi), "Beschäftigten zwischen 25 und 55 Jahren", title_help)
     # title_help <- ifelse(grepl("ü55", indi), "Beschäftigten über 55 Jahren", title_help)

     titel_w <- ifelse(faecher == "Andere Berufsgruppen", paste0("Anteil weiblicher ", title_help, ", die kein MINT-Berufsfeld wählen (", timerange, ")"),
                       paste0("Anteil weiblicher ", title_help, ", die das Berufsfeld ", faecher, " wählen (", timerange, ")"))
     titel_m <- ifelse(faecher == "Andere Berufsgruppen", paste0("Anteil männlicher ", title_help, ", die kein MINT-Berufsfeld wählen (", timerange, ")"),
                       paste0("Anteil männlicher ", title_help, ", die das Berufsfeld ", faecher, " wählen (", timerange, ")"))

     # plot
     out_1 <- highcharter::hcmap(
       "countries/de/de-all",
       data = values_female,
       value = "prop",
       joinBy = c("name", "bundesland"),
       borderColor = "#FAFAFA",
       name = paste0(faecher),
       borderWidth = 0.1,
       nullColor = "#A9A9A9",
       tooltip = list(
         valueDecimals = 0,
         valueSuffix = "%"
       )
       #,
       #download_map_data = FALSE
     ) %>%
       highcharter::hc_tooltip(pointFormat = "{point.bundesland} <br> Anteil: {point.prop_disp} % <br> Anzahl: {point.wert_disp}") %>%
       highcharter::hc_colorAxis(min=0,labels = list(format = "{text}%")) %>%
       highcharter::hc_title(
         text = titel_w,
         margin = 10,
         align = "center",
         style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
       ) %>%
       # highcharter::hc_caption(
       #   text = "Quelle:",  style = list(fontSize = "12px")
       # ) %>%
       highcharter::hc_chart(
         style = list(fontFamily = "SourceSans3-Regular")
       ) %>% highcharter::hc_size(600, 550) %>%
       highcharter::hc_credits(enabled = FALSE) %>%
       highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                              verticalAlign = "bottom")

     out_2 <- highcharter::hcmap(
       "countries/de/de-all",
       data = values_male,
       value = "prop",
       joinBy = c("name", "bundesland"),
       borderColor = "#FAFAFA",
       name = paste0(faecher),
       borderWidth = 0.1,
       nullColor = "#A9A9A9",
       tooltip = list(
         valueDecimals = 0,
         valueSuffix = "%"
       )
       #,
       #download_map_data = FALSE
     ) %>%
       highcharter::hc_tooltip(pointFormat = "{point.bundesland} <br> Anteil: {point.prop_disp} % <br> Anzahl: {point.wert_disp}") %>%
       highcharter::hc_colorAxis(min=0,labels = list(format = "{text}%")) %>%
       highcharter::hc_title(
         text = titel_m,
         margin = 10,
         align = "center",
         style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
       ) %>%
       # highcharter::hc_caption(
       #   text = "Quelle:",  style = list(fontSize = "12px")
       # ) %>%
       highcharter::hc_chart(
         style = list(fontFamily = "SourceSans3-Regular")
       ) %>% highcharter::hc_size(600, 550) %>%
       highcharter::hc_credits(enabled = FALSE) %>%
       highcharter::hc_legend(layout = "horizontal", floating = FALSE, verticalAlign = "bottom")

     out <- highcharter::hw_grid(
       out_1, out_2,
       ncol = 2,
       browsable = TRUE
     )

     }else
       if(betrachtung == "Zeitverlauf - Liniendiagramm"){
    timerange <- r$date_arbeitsmarkt_wahl_gender_verlauf
    t <- timerange[1]:timerange[2]
    inid <- r$level_arbeitsmarkt_wahl_gender_verlauf
    regio <- r$states_arbeitsmarkt_wahl_gender_verlauf
    faecher <- r$fach_arbeitsmarkt_wahl_gender_verlauf
    absolut_selector <- r$abs_zahlen_arbeitsmarkt_wahl_gender_verlauf

     df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
       dplyr::filter(jahr %in% t,
                     indikator == indi,
                     landkreis == "alle Landkreise",
                     bundesland %in% regio,
                     anforderung == "Gesamt",
                     geschlecht == "Frauen",
                     fachbereich == faecher)%>%
       dplyr::select("indikator", "fachbereich", "geschlecht", "bundesland",
                     "jahr", "wert" ) %>%
       dplyr::collect()

     if(absolut_selector=="In Prozent"){

       df_alle <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
         dplyr::filter(jahr %in% t,
                       indikator == indi,
                       landkreis == "alle Landkreise",
                       bundesland %in% regio,
                       anforderung == "Gesamt",
                       geschlecht == "Frauen",
                       fachbereich == "Alle")%>%
         dplyr::select("indikator", "fachbereich", "geschlecht", "bundesland",
                       "jahr", "wert" ) %>%
         dplyr::rename(wert_ges = wert) %>%
         dplyr::collect()

       df <- df %>%
         dplyr::left_join(df_alle, by = c("bundesland", "jahr", "geschlecht", "indikator")) %>%
         dplyr::rename(fachbereich = fachbereich.x) %>%
         dplyr::select(-fachbereich.y) %>%
         dplyr::mutate(prop = round(wert/wert_ges*100, 1))%>%
         dplyr::filter(fachbereich != "Alle")

       df$prop_disp <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")
       # order years for plot
       df <- df[with(df, order(bundesland, jahr, decreasing = FALSE)), ]

       title_help <- paste0(indi, "r")

       # plot
       out <- highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = prop, group = bundesland)) %>%
         highcharter::hc_tooltip(pointFormat = "{point.region} <br> Anteil: {point.prop_disp} %") %>%
         highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
         highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
         #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
         highcharter::hc_title(text = paste0("Anteil weiblicher ", title_help, ", die MINT-Berufe wählen"
         ),
         margin = 45,
         align = "center",
         style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
         highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                                  "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")) %>%
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


     }else if(absolut_selector=="Anzahl"){

       title_help <- paste0(indikator_choice, "r")

       hcoptslang <- getOption("highcharter.lang")
       hcoptslang$thousandsSep <- "."
       options(highcharter.lang = hcoptslang)

       df$wert_disp <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
       df <- df[with(df, order(bundesland, jahr, decreasing = FALSE)), ]

       # plot
      out<- highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group = bundesland)) %>%
         highcharter::hc_tooltip(pointFormat = "Anzahl: {point.wert_disp}") %>%
         highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
         highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
         #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
         highcharter::hc_title(text = paste0("Anzahl weiblicher ", title_help, ", die MINT-Berufe wählen"
         ),
         margin = 45,
         align = "center",
         style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
         highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                                  "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")) %>%
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



  }
  return(out)
}

#' A function to plot a waffle chart ::: b3
#'
#' @description A function to create a waffle chart for the tab "Beruf"
#'
#' @return The return value is a waffle chart
#' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# arbeitsmarkt_anforderungen_gender <- function(r) {
#
#
#   timerange <- r$date_arbeitsmarkt_anforderungen_gender
#
#   if(timerange ==2021) indikator_choice <- r$level_arbeitsmarkt_anforderungen_gender_21
#   if(timerange ==2022) indikator_choice <- r$level_arbeitsmarkt_anforderungen_gender_22
#
#
#   df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
#     dplyr::filter(jahr %in% timerange &
#                     bundesland == "Deutschland" &
#                     geschlecht != "Gesamt"&
#                     anforderung == "Gesamt" &
#                     indikator %in% c("Auszubildende",
#                                      "Auszubildende (1. Jahr)",
#                                      "Beschäftigte",
#                                      "ausländische Auszubildende",
#                                      "ausländische Beschäftigte")&
#                     fachbereich %in% c("Alle", "MINT", "Mathematik, Naturwissenschaften",
#                                        "Informatik", "Technik (gesamt)"))%>%
#     dplyr::select(indikator, fachbereich, wert, geschlecht) %>%
#     dplyr::collect()
#
#   # Berechnung von andere Fächergruppen
#   df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"]-
#     df[df$fachbereich == "MINT", "wert"]
#   df$fachbereich[df$fachbereich == "Alle"]<-"andere Fächergruppen"
#   df <- df %>% dplyr::filter(fachbereich != "MINT")
#
#   # Anteil berechnen
#   df <- df %>%
#     dplyr::group_by(indikator, geschlecht) %>%
#     dplyr::mutate(props = sum(wert))
#
#   df <- df %>% dplyr::group_by(fachbereich, indikator, geschlecht) %>%
#     dplyr::mutate(proportion = wert/props)
#
#   df$proportion <- df$proportion * 100
#
#
#   # Ausgewählte Indikatoren filtern
#   df <- df %>% dplyr::filter(indikator == indikator_choice)
#
#   # nach Geschlechtern trennen
#   # Frauen
#   df_fr <- df %>% dplyr::filter(geschlecht=="Frauen")
#
#   df_fr <- setNames(round_preserve_sum(as.numeric(df_fr$proportion),0),
#                     df_fr$fachbereich)
#   df_fr <- df_fr[order(factor(names(df_fr), levels = c("Mathematik, Naturwissenschaften",
#                                                        "Informatik", "Technik (gesamt)",
#                                                        'andere Fächergruppen')))]
#
#   # Männer
#   df_me <- df %>% dplyr::filter(geschlecht=="Männer")
#
#   df_me <- setNames(round_preserve_sum(as.numeric(df_me$proportion),0),
#                     df_me$fachbereich)
#   df_me <- df_me[order(factor(names(df_me), levels = c("Mathematik, Naturwissenschaften",
#                                                        "Informatik", "Technik (gesamt)",
#                                                        'andere Fächergruppen')))]
#
#   # Titel für Plots
#   title_help <- paste0(indikator_choice, "n <br>")
#   title_help <- ifelse(grepl("ausländische Beschäftigte", indikator_choice), "ausländischen <br> Beschäftigten", title_help)
#   title_help <- ifelse(grepl("ausländische Auszubildende", indikator_choice), "ausländischen <br> Auszubildenden", title_help)
#   title_help <- ifelse(grepl("Jahr", indikator_choice), "Auszubildenden <br> mit neuem Lehrvertrag <br>", title_help)
#
#
#   title_male <- paste0("Von männlichen ", title_help, " gewählte Berufsfelder <br> (", timerange, ")")
#   title_female <- paste0("Von weiblichen ", title_help, " gewählte Berufsfelder <br>(", timerange, ")")
#
#   #waffles
#   waffle_fr <- waffle::waffle(df_fr, keep = FALSE) +
#     ggplot2::labs(
#       fill = "",
#       title = paste0("<span style='color:black;'>", title_female, "<br>")) +
#     ggplot2::theme(plot.title = ggtext::element_markdown(),
#                    plot.subtitle = ggtext::element_markdown(),
#                    text = ggplot2::element_text(size = 14),
#                    plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
#                    legend.position = "bottom") +
#     # account for the possability that female has 0% share of "Experte
#     # if (df_trainee[[3]] == 0) {
#     # waffle_fr <- waffle_fr +
#     ggplot2::scale_fill_manual(
#       values =  c("#ee7775",
#                   "#fbbf24",
#                   "#35bd97",
#                   '#8893a7'),
#       limits = c("Mathematik, Naturwissenschaften",
#                  "Informatik", "Technik (gesamt)",
#                  'Andere Fachbereiche'),
#       na.value='#8893a7',
#       guide = ggplot2::guide_legend(reverse = TRUE),
#       labels = c(
#         paste0("Mathematik, Naturwissenschaften",", ",df_fr[1], "%"),
#         paste0("Informatik",", ",df_fr[2], "%"),
#         paste0("Technik (gesamt)",", ",df_fr[3], "%"),
#         paste0("Andere Fachbereiche",", ",df_fr[4], "%")
#       )) +
#     ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))
#
#
#   waffle_me <- waffle::waffle(df_me, keep = FALSE) +
#     ggplot2::labs(
#       fill = "",
#       title = paste0("<span style='color:black;'>", title_male ,"<br>")) +
#     ggplot2::theme(plot.title = ggtext::element_markdown(),
#                    plot.subtitle = ggtext::element_markdown(),
#                    text = ggplot2::element_text(size = 14),
#                    plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
#                    legend.position = "bottom")+
#     ggplot2::scale_fill_manual(
#       values =  c("#ee7775",
#                   "#fbbf24",
#                   "#35bd97",
#                   '#8893a7'),
#       limits = c("Mathematik, Naturwissenschaften",
#                  "Informatik", "Technik (gesamt)",
#                  'Andere Fachbereiche'),
#       na.value='#8893a7',
#       guide = ggplot2::guide_legend(reverse = TRUE),
#       labels = c(
#         paste0("Mathematik, Naturwissenschaften",", ",df_me[1], "%"),
#         paste0("Informatik",", ",df_me[2], "%"),
#         paste0("Technik (gesamt)",", ",df_me[3], "%"),
#         paste0("Andere Fachbereiche",", ",df_me[4], "%")
#       )) +
#     ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))
#
#   ggpubr::ggarrange(waffle_fr, NULL ,waffle_me, widths = c(1, 0.1, 1), nrow=1)
#
#
# }



#' A function to plot the german map ::::box 6
#'
#' @description A function to plot the german map with all states that contain
#' information about the share of women in STEM
#'
#' @return The return value is the german map with information
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# arbeitsmarkt_bl_gender <- function(r) {
#
#   # load UI inputs from reactive value
#   timerange <- r$date_arbeitsmarkt_bl_gender
#
#   #anforderung <- r$anforderung_arbeitsmarkt_bl_gender
#
#   if(timerange == 2021) indikator_choice <- r$level_arbeitsmarkt_bl_gender_21
#   if(timerange == 2022) indikator_choice <- r$level_arbeitsmarkt_bl_gender_22
#
#   fachbereich_choice <- r$fach_arbeitsmarkt_bl_gender
#
#   df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
#     dplyr::filter(jahr %in% timerange &
#                     !bundesland %in% c("Deutschland",
#                                        "Westdeutschland (o. Berlin)",
#                                        "Ostdeutschland (einschl. Berlin)") &
#                     landkreis == "alle Landkreise" &
#                     geschlecht != "Gesamt"&
#                     anforderung == "Gesamt" )%>%
#     dplyr::select(indikator, fachbereich, wert, geschlecht, bundesland, jahr) %>%
#     dplyr::collect()
#
#   # filter dataset based on UI inputs
#   # df <- df %>% dplyr::filter(jahr == timerange)
#   # # filtern nach Anforderungsniveau
#   # df <- df %>% dplyr::filter(anforderung == "Gesamt")
#
#   #direkt angegebene Werte hier vollständiger als selbst berechnete Aggregate, daher Folgendes raus
#
#   # # remove - Deutschland nicht enthalten in DF
#   # df <- df %>% dplyr::filter(region != "Deutschland")
#   #
#   # # im neuen DF doch Aggregate enthalten, ausfiltern das Folgecode weiter stimmt
#   # df <- df %>% dplyr::filter(landkreis != "alle Landkreise")
#
#   # # Aggregat auf Bundeslandebene berechnen und LKs ausschließen
#   # df <- df %>%
#   #   dplyr::group_by(jahr, indikator, fachbereich, geschlecht, bundesland) %>%
#   #   dplyr::summarize(wert = sum(wert))
#
#   # Filtern nach Bundesländern
#   # df <- df %>%
#   #   dplyr::filter(landkreis == "alle Landkreise") %>%
#   #   dplyr::filter(!(bundesland %in% c("Deutschland", "Westdeutschland (o. Berlin)", "Ostdeutschland (einschl. Berlin)")))
#
#
#   # Berechnung von andere Fächergruppen
#   df_andere <- df %>% dplyr::filter(fachbereich=="Alle")
#   df_mint <- df %>% dplyr::filter(fachbereich=="MINT")
#   df_andere$wert <- df_andere$wert - df_mint$wert
#   df_andere$fachbereich[df_andere$fachbereich == "Alle"]<-"Andere Berufsgruppen"
#
#   df <- rbind(df, df_andere)
#
#   #nicht nötig, da Männer schon in df berechnet
#   #df <- calc_arbeitsmarkt_males(df)
#
#   df <- df %>% dplyr::filter(indikator == indikator_choice)
#
#   df_gesamt <- df %>%
#     dplyr::filter(fachbereich == "Alle")
#   # ,
#   # anforderung == "Gesamt")
#
#   df <- df %>%
#     dplyr::left_join(df_gesamt, by = c("bundesland", "jahr", "geschlecht", "indikator")) %>%
#     dplyr::rename(fachbereich = fachbereich.x,
#                   wert = "wert.x",
#                   wert_sum = "wert.y") %>%
#     dplyr::select(-fachbereich.y) %>%
#     dplyr::mutate(proportion = (wert/wert_sum)*100)%>%
#     dplyr::filter(fachbereich == fachbereich_choice)
#
#   #Gerundetes Prop für Hover:
#   df$prop <- round(df$proportion, 0)
#
#   #Trennpunkte für lange Zahlen ergänzen
#   df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
#
#   values_female <- df %>% dplyr::filter(geschlecht == "Frauen")
#   values_male <- df %>% dplyr::filter(geschlecht == "Männer")
#
#   # if (anforderung == "Gesamt"){
#   #
#   #   title_help_sub <- " insgesamt"
#   #
#   # } else {
#   #
#   #   title_help_sub <- paste0(" mit anforderung ", anforderung)
#   #
#   # }
#
#   #Überschrift erstellen
#   title_help <- paste0(indikator_choice, "r")
#   title_help <- ifelse(grepl("ausländische Beschäftigte", indikator_choice), "ausländischer Beschäftigter", title_help)
#   title_help <- ifelse(grepl("ausländische Auszubildende", indikator_choice), "ausländischer Auszubildender", title_help)
#   title_help <- ifelse(grepl("Jahr", indikator_choice), "Auszubildender mit neuem Lehrvertrag", title_help)
#   # title_help <- ifelse(grepl("u25", indikator_choice), "Beschäftigten unter 25 Jahren", title_help)
#   # title_help <- ifelse(grepl("25-55", indikator_choice), "Beschäftigten zwischen 25 und 55 Jahren", title_help)
#   # title_help <- ifelse(grepl("ü55", indikator_choice), "Beschäftigten über 55 Jahren", title_help)
#
#   titel_w <- ifelse(fachbereich_choice == "Andere Berufsgruppen", paste0("Anteil weiblicher ", title_help, ", die kein MINT-Berufsfeld wählen (", timerange, ")"),
#                     paste0("Anteil weiblicher ", title_help, ", die das Berufsfeld ", fachbereich_choice, " wählen (", timerange, ")"))
#   titel_m <- ifelse(fachbereich_choice == "Andere Berufsgruppen", paste0("Anteil männlicher ", title_help, ", die kein MINT-Berufsfeld wählen (", timerange, ")"),
#                     paste0("Anteil männlicher ", title_help, ", die das Berufsfeld ", fachbereich_choice, " wählen (", timerange, ")"))
#
#
#
#   # plot
#   out_1 <- highcharter::hcmap(
#     "countries/de/de-all",
#     data = values_female,
#     value = "proportion",
#     joinBy = c("name", "bundesland"),
#     borderColor = "#FAFAFA",
#     name = paste0(fachbereich_choice),
#     borderWidth = 0.1,
#     nullColor = "#A9A9A9",
#     tooltip = list(
#       valueDecimals = 0,
#       valueSuffix = "%"
#     )
#     #,
#     #download_map_data = FALSE
#   ) %>%
#     highcharter::hc_tooltip(pointFormat = "{point.bundesland} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}") %>%
#     highcharter::hc_colorAxis(min=0,labels = list(format = "{text}%")) %>%
#     highcharter::hc_title(
#       text = titel_w,
#       margin = 10,
#       align = "center",
#       style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
#     ) %>%
#     # highcharter::hc_caption(
#     #   text = "Quelle:",  style = list(fontSize = "12px")
#     # ) %>%
#     highcharter::hc_chart(
#       style = list(fontFamily = "SourceSans3-Regular")
#     ) %>% highcharter::hc_size(600, 550) %>%
#     highcharter::hc_credits(enabled = FALSE) %>%
#     highcharter::hc_legend(layout = "horizontal", floating = FALSE,
#                            verticalAlign = "bottom")
#
#   out_2 <- highcharter::hcmap(
#     "countries/de/de-all",
#     data = values_male,
#     value = "proportion",
#     joinBy = c("name", "bundesland"),
#     borderColor = "#FAFAFA",
#     name = paste0(fachbereich_choice),
#     borderWidth = 0.1,
#     nullColor = "#A9A9A9",
#     tooltip = list(
#       valueDecimals = 0,
#       valueSuffix = "%"
#     )
#     #,
#     #download_map_data = FALSE
#   ) %>%
#     highcharter::hc_tooltip(pointFormat = "{point.bundesland} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}") %>%
#     highcharter::hc_colorAxis(min=0,labels = list(format = "{text}%")) %>%
#     highcharter::hc_title(
#       text = titel_m,
#       margin = 10,
#       align = "center",
#       style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
#     ) %>%
#     # highcharter::hc_caption(
#     #   text = "Quelle:",  style = list(fontSize = "12px")
#     # ) %>%
#     highcharter::hc_chart(
#       style = list(fontFamily = "SourceSans3-Regular")
#     ) %>% highcharter::hc_size(600, 550) %>%
#     highcharter::hc_credits(enabled = FALSE) %>%
#     highcharter::hc_legend(layout = "horizontal", floating = FALSE, verticalAlign = "bottom")
#
#
#   out <- list(out_1, out_2)
#
#   return (out)
#
# }

#' A function to plot time series
#'
#' @description A function to plot the time series
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_bl_gender_verlauf <- function(r) {

  # load UI inputs from reactive value

  absolut_selector <- r$abs_zahlen_beruf_arbeitsmarkt_bl_gender_verlauf

  timerange <- r$date_beruf_arbeitsmarkt_bl_gender_verlauf

  #anforderung <- r$anforderung_beruf_arbeitsmarkt_bl_gender_verlauf

  indikator_choice <- r$indikator_beruf_arbeitsmarkt_bl_gender_verlauf

  states <- r$states_beruf_arbeitsmarkt_bl_gender_verlauf

  t <- as.character(timerange[1]:timerange[2])


  df <-  dplyr::tbl(con, from = "arbeitsmarkt")%>%
    dplyr::filter(jahr %in% t,
                  indikator == indikator_choice,
                  region %in% states,
                  anforderung %in% "Gesamt",
                  geschlecht == "Frauen",
                  fachbereich %in% c("Alle", "MINT")
    )%>%
    dplyr::select("bereich",
                  "indikator",
                  "fachbereich",
                  "geschlecht",
                  "region",
                  "jahr",
                  "anforderung",
                  "wert" ) %>%
    dplyr::collect()

  # filter dataset based on UI inputs
  #df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  #df <- df %>% dplyr::filter(indikator == indikator_choice)

  #df <- prep_arbeitsmarkt_east_west(df)

  # df <- df %>%
  #   dplyr::mutate(region = dplyr::case_when(
  #     region == "Westen" ~ "Westdeutschland (o. Berlin)",
  #     region == "Osten" ~ "Ostdeutschland (inkl. Berlin)",
  #     T ~ .$region
  #   ))

  df <- df %>% dplyr::filter(anforderung != "Keine Zuordnung möglich")

  df_gesamt <- df %>%
    dplyr::filter(fachbereich == "Alle",
                  anforderung == "Gesamt")



  df <- df %>%
    dplyr::left_join(df_gesamt, by = c("region", "jahr", "geschlecht", "indikator", "bereich")) %>%
    dplyr::rename(anforderung = "anforderung.x",
                  fachbereich = "fachbereich.x",
                  wert = "wert.x",
                  wert_sum = "wert.y") %>%
    dplyr::select(-c("fachbereich.y", "anforderung.y")) %>%
    dplyr::mutate(proportion = (wert/wert_sum)*100)%>%
    dplyr::filter(anforderung == "Gesamt",
                  fachbereich == "MINT")%>%
    dplyr::select(-wert_sum)%>%
    dplyr::rename(Relativ = proportion, Absolut=wert)%>%
    tidyr::pivot_longer(c(Absolut, Relativ), names_to = "selector", values_to = "wert")%>%
    dplyr::mutate(selector = dplyr::case_when(
      selector == "Relativ" ~ "In Prozent",
      selector == "Absolut" ~ "Anzahl"
    ))


  df <- df %>% dplyr::filter(fachbereich == "MINT")

  if(absolut_selector=="In Prozent"){

    df <- df %>%
      dplyr::filter(selector =="In Prozent")



    # order years for plot
    df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

    title_help <- paste0(indikator_choice, "r")

    # plot
    highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(wert,1), group = region)) %>%
      highcharter::hc_tooltip(pointFormat = "{point.region} <br> Anteil: {point.y} %") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = paste0("Anteil weiblicher ", title_help, ", die MINT-Berufe wählen"
      ),
      margin = 45,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                               "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")) %>%
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


  }else if(absolut_selector=="Anzahl"){

    title_help <- paste0(indikator_choice, "r")

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    df <- df %>%
      dplyr::filter(selector == "Anzahl")

    df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]


    # plot
    highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(wert,1), group = region)) %>%
      highcharter::hc_tooltip(pointFormat = "Anzahl: {point.y}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = paste0("Anzahl weiblicher ", title_help, ", die MINT-Berufe wählen"
      ),
      margin = 45,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                               "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")) %>%
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

}

# Regionaler MINT Steckbrief ----

arbeitsmarkt_lk_detail_map <- function(r) {

  # load UI inputs from reactive value
  timerange <- r$date_beruf_arbeitsmarkt_landkreis_karte
  states <- r$states_beruf_arbeitsmarkt_landkreis_karte

  # input values for first map
  category_1 <- r$kategorie_beruf_arbeitsmarkt_landkreis_karte1
  domain_1 <- r$fachbereich_beruf_arbeitsmarkt_landkreis_karte1
  indikator_azubi_1 <- r$indikator1_beruf_arbeitsmarkt_landkreis_karte1
  indikator_besch_1 <- r$indikator2_beruf_arbeitsmarkt_landkreis_karte1

  # input values for second map
  # category_2 <- r$kategorie_beruf_arbeitsmarkt_landkreis_karte2
  # domain_2 <- r$fachbereich_beruf_arbeitsmarkt_landkreis_karte2
  # indikator_azubi_2 <- r$indikator1_beruf_arbeitsmarkt_landkreis_karte2
  # indikator_besch_2 <- r$indikator2_beruf_arbeitsmarkt_landkreis_karte2


  df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
    dplyr::filter(
      jahr == timerange)%>%
    dplyr::select(-bereich)%>%
    dplyr::collect()

  # filter Jahre
  #df <- df %>% dplyr::filter(jahr == timerange)

  # map states for state codes
  state_codes <- data.frame(
    state = c(
      "Baden-Württemberg",
      "Bayern",
      "Berlin",
      "Brandenburg",
      "Bremen",
      "Hamburg",
      "Hessen",
      "Mecklenburg-Vorpommern",
      "Niedersachsen",
      "Nordrhein-Westfalen",
      "Rheinland-Pfalz",
      "Saarland",
      "Sachsen",
      "Sachsen-Anhalt",
      "Schleswig-Holstein",
      "Thüringen"
    ),
    short = c(
      "bw",
      "by",
      "be",
      "bb",
      "hb",
      "hh",
      "he",
      "mv",
      "ni",
      "nw",
      "rp",
      "sl",
      "sn",
      "st",
      "sh",
      "th"
    )
  )

  state_code <- state_codes %>% dplyr::filter(state == states) %>% dplyr::pull()

  # calculate comparison map 1

  df1_list <- calculate_landkreis(df, states, category_1, domain_1, indikator_azubi_1, indikator_besch_1)

  df1_map <- df1_list[[1]]
  titel_gesamt1 <- df1_list[[2]]
  titel_gesamt1_2 <- df1_list[[3]]
  titel_sub1 <- df1_list[[4]]
  titel_sub1_2 <- df1_list[[5]]

  # df2_list <- calculate_landkreis(df, states, category_2, domain_2, indikator_azubi_2, indikator_besch_2)
  #
  # df2_map <- df2_list[[1]]
  # titel_gesamt2 <- df2_list[[2]]
  # titel_gesamt2_2 <- df2_list[[3]]
  # titel_sub2 <- df2_list[[4]]
  # titel_sub2_2 <- df2_list[[5]]

  # hilfe für Hover-Text
  if(category_1 == "Beschäftigte") {
    adjektiv_1 <- indikator_besch_1
  }else{
    adjketiv_1 <- indikator_azubi_1
  }

  # if(category_2 == "Beschäftigte") {
  #   adjektiv_2 <- indikator_besch_2
  # }else{
  #   adjketiv_2 <- indikator_azubi_2
  # }

  # adjust landkreis_nummer for correct mapping
  df1_map <- df1_map %>% dplyr::mutate(
    landkreis_nummer = paste0("de-", state_code, "-", landkreis_nummer, "000"))

  # df2_map <- df2_map %>% dplyr::mutate(
  #   landkreis_nummer = paste0("de-", state_code, "-", landkreis_nummer, "000"))

  #Trennpunkte für lange Zahlen ergänzen in Absolute Zahlen für Hover + Text für Hover
  df1_map$wert <- prettyNum(df1_map$wert, big.mark = ".", decimal.mark = ",")
  # df2_map$wert <- prettyNum(df2_map$wert, big.mark = ".", decimal.mark = ",")
  domain_1 <- ifelse(domain_1 == "Alle", "alle Berufsbereiche", domain_1)
  # domain_2 <- ifelse(domain_2 == "Alle", "alle Berufsbereiche", domain_2)


  # create plots
  out <- highcharter::hcmap(
    paste0("countries/de/de-", state_code ,"-all"),
    data = df1_map,
    value = "prob",
    joinBy = c("hc-key","landkreis_nummer"),
    borderColor = "#FAFAFA",
    #name = paste0("Anteil von ", titel_sub1_2, titel_gesamt1, titel_gesamt1_2, " in ", states, " (2021)"),
    name = paste0( domain_1, "<br>", titel_sub1_2),
    borderWidth = 0.1,
    nullColor = "#A9A9A9",
    tooltip = list(
      valueDecimals = 0,
      valueSuffix = "%"
    )
    #,
    # download_map_data = FALSE
  ) %>%
    highcharter::hc_colorAxis(min=0,labels = list(format = "{text}%")) %>%
    highcharter::hc_title(
      text = paste0("Anteil von ", titel_sub1_2, titel_gesamt1, titel_gesamt1_2, " in ", states, " (", timerange, ")"),
      margin = 10,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
    ) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular")
    ) %>% highcharter::hc_size(600, 550) %>%
    highcharter::hc_credits(enabled = FALSE) %>%
    highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                           verticalAlign = "bottom") %>%
    highcharter::hc_exporting(enabled = FALSE, #noch kein Download bis jetzt
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/jpeg' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

  # map2 <- highcharter::hcmap(
  #   paste0("countries/de/de-", state_code ,"-all"),
  #   data = df2_map,
  #   value = "prob",
  #   joinBy = c("hc-key", "landkreis_nummer"),
  #   borderColor = "#FAFAFA",
  #   # name = paste0("Anteil von ", titel_sub2_2, titel_gesamt2, titel_gesamt2_2, " in ", states, " (2021)"),
  #   name = paste0(domain_2, "<br>", titel_sub2_2),
  #   borderWidth = 0.1,
  #   nullColor = "#A9A9A9",
  #   tooltip = list(
  #     valueDecimals = 0,
  #     valueSuffix = "%"
  #   )
  #   #,
  #   # download_map_data = FALSE
  # ) %>%
  #   highcharter::hc_colorAxis(min=0,labels = list(format = "{text}%")) %>%
  #   highcharter::hc_title(
  #     text = paste0("Anteil von ", titel_sub2_2, titel_gesamt2, titel_gesamt2_2, " in ", states, " (", timerange, ")"),
  #     margin = 10,
  #     align = "center",
  #     style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
  #   ) %>%
  #   highcharter::hc_chart(
  #     style = list(fontFamily = "SourceSans3-Regular")
  #   ) %>% highcharter::hc_size(600, 550) %>%
  #   highcharter::hc_credits(enabled = FALSE) %>%
  #   highcharter::hc_legend(layout = "horizontal", floating = FALSE,
  #                          verticalAlign = "bottom") %>%
  #   highcharter::hc_exporting(enabled = FALSE, #noch kein Download bis jetzt
  #                             buttons = list(contextButton = list(
  #                               symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
  #                               onclick = highcharter::JS("function () {
  #                                                             this.exportChart({ type: 'image/jpeg' }); }"),
  #                               align = 'right',
  #                               verticalAlign = 'bottom',
  #                               theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
  #
  # out <- list(map1, map2)

  return(out)
}
#' A function to plot a bar chart
#'
#' @description A function to create a bar chart for detailed overview for landkreise
#'
#' @return The return value is a bar chart
#' @param df The dataframe "Arbeitsmarkt_detailliert.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_lk_detail_vergleich <- function(r){

  # load UI inputs from reactive value
  timerange <- r$date_beruf_arbeitsmarkt_landkreis_vergleich
  states <- r$states_beruf_arbeitsmarkt_landkreis_vergleich
  search_val <- r$search_in_bar_chart

  # filtern nach Zeit
  df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
    dplyr::filter(
      jahr == timerange)%>%
    dplyr::select(-bereich)%>%
    dplyr::collect()

  # input values
  category <- r$kategorie_beruf_arbeitsmarkt_landkreis_vergleich
  domain <- r$fachbereich_beruf_arbeitsmarkt_landkreis_vergleich
  indikator_azubi <- r$indikator1_beruf_arbeitsmarkt_landkreis_vergleich
  indikator_besch <- r$indikator2_beruf_arbeitsmarkt_landkreis_vergleich
  display_form <- r$darstellung_beruf_arbeitsmarkt_landkreis_vergleich

  # calculate comparison

  df_compare_list <- calculate_landkreis(df, states, category, domain, indikator_azubi, indikator_besch)

  df_compare <- df_compare_list[[1]]
  titel_gesamt_1 <- df_compare_list[[2]]
  titel_gesamt_2 <- df_compare_list[[3]]
  titel_sub <- df_compare_list[[4]]
  titel_sub2 <- df_compare_list[[5]]

  # titel_gesamt_2 <- df_compare_list[[3]]
  # titel_sub <- df_compare_list[[4]]

  # differentiate between relative and absolute
  if(display_form == "In Prozent") {
    df_compare <- df_compare %>%
      dplyr::mutate(display_value = prob) %>%
      dplyr::arrange(display_value)

    legende <- paste0("{point.landkreis} <br> Anteil: {point.y} %")
    yAxis <- "{value}%"
    titel <- paste0("Anteil von ", titel_sub2, titel_gesamt_1, titel_gesamt_2, " in ", states, " (", timerange, ")")

  } else if(display_form== "Anzahl") {
    df_compare <- df_compare %>%
      dplyr::mutate(display_value = wert) %>%
      dplyr::arrange(display_value) %>%
      dplyr::filter(landkreis != "alle Landkreise")

    legende <- paste0("{point.landkreis} <br> Wert: {point.y}")
    yAxis <- "{value}"
    titel_gesamt_1 <- stringr::str_remove(titel_gesamt_1, "an allen")
    titel <- paste0("Anzahl ", titel_sub, titel_gesamt_1, " in ", states, " (", timerange, ")")
  }

  #Vector für angepasste Größe des Plots
  länder <-   c("Baden-Württemberg",
                "Bayern",
                "Berlin",
                "Brandenburg",
                "Bremen",
                "Hamburg",
                "Hessen",
                "Mecklenburg-Vorpommern",
                "Niedersachsen",
                "Nordrhein-Westfalen",
                "Rheinland-Pfalz",
                "Saarland",
                "Sachsen",
                "Sachsen-Anhalt",
                "Schleswig-Holstein",
                "Thüringen")
  höhe <- c(10, 20, 3, 6, 3, 3, 8, 5, 11, 11, 10, 4, 6, 6, 6, 7)
  plt.add <- data.frame(länder, höhe)


  # plt.add$subtitle <- "Quelle der Daten: Bundesagentur für Arbeit, 2022, auf Anfrage, eigene Berechnungen.

  # create plot
  highcharter::hchart(df_compare, 'bar', highcharter::hcaes(y = display_value, x = landkreis)) %>%
    highcharter::hc_tooltip(pointFormat = legende) %>%
    highcharter::hc_yAxis(title = list(text = paste0(br(), br(),"Quelle der Daten: Bundesagentur für Arbeit, 2022, auf Anfrage, eigene Berechnungen.") , align="left"), labels = list(format = yAxis)) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_plotOptions(bar = list(
      colorByPoint = TRUE,
      colors = ifelse(df_compare$landkreis == "alle Landkreise", "#b16fab",
                      ifelse(df_compare$landkreis == search_val, "#00A87A", "#154194"))
    )) %>%
    highcharter::hc_size(height = 80*plt.add$höhe[plt.add$länder == states]) %>%
    highcharter::hc_title(text = paste0(titel, "<br><br>"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE)


  # %>%
  #   highcharter::hc_exporting(enabled = TRUE, #button erscheint
  #                             buttons = list(contextButton = list(
  #                               symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)', #Bild für Download Symbol
  #                               onclick = highcharter::JS("function () {
  #                                                             this.exportChart({ type: 'image/jpeg' }); }"), #ruft Java-Skript fkt zum Export auf
  #                                                             # mögliche Anpassungen hier drin, z. B. Größe, Name des Downloads, Hintergrund-Bilder
  #                                                             # font, Dateiname, subtitle angeben mit Quelle
  #                               align = 'right',
  #                               verticalAlign = 'bottom',
  #                               theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

}


# Rest ----


#' #' A function to plot a waffle chart
#' #'
#' #' @description A function to create a waffle chart for the second box inside the
#' #' tab "Beruf".
#' #'
#' #' @return The return value is a waffle chart
#' #' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#' arbeitnehmer_waffle <- function(df,r) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_arbeitsmarkt
#'
#'   anforderung <- r$anforderung_arbeitsmarkt
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr == timerange)
#'
#'   df <- df %>% dplyr::filter(region == "Deutschland")
#'
#'   df <- df %>% dplyr::filter(anforderung == anforderung)
#'
#'   df <- df %>% dplyr::filter(fachbereich == "MINT")
#'
#'   # calculate the share of males
#'   values <- df %>%
#'     dplyr::group_by(indikator) %>%
#'     dplyr::mutate(wert = wert - dplyr::lead(wert)) %>% dplyr::select(wert) %>% na.omit()
#'
#'   df[df$geschlecht == "Gesamt", "wert"] <- values$wert
#'
#'   df[df$geschlecht == "Gesamt", "geschlecht"] <- "Männer"
#'
#'
#'   # calculate proportions
#'   df <- df %>% dplyr::group_by(indikator) %>%
#'     dplyr::mutate(props = sum(wert))
#'
#'
#'   df <- df %>% dplyr::group_by(geschlecht, indikator, fachbereich) %>%
#'     dplyr::summarize(proportion = wert/props)
#'
#'   df$proportion <- df$proportion * 100
#'
#'   df_beschaeftigte <- df %>% dplyr::filter(indikator == "Beschäftigte")
#'
#'
#'   df_beschaeftigte$geschlecht <- paste0(df_beschaeftigte$geschlecht, " (", df_beschaeftigte$fachbereich, ")")
#'
#'   # ensure proportions sum to 1
#'   x_beschaeftigte <- setNames(round_preserve_sum(as.numeric(df_beschaeftigte$proportion),0),
#'                               df_beschaeftigte$geschlecht)
#'
#'   x_beschaeftigte <- x_beschaeftigte[order(factor(names(x_beschaeftigte), levels = c('Frauen (MINT)', 'Männer (MINT)')))]
#'
#'   df_auszubildende <- df %>% dplyr::filter(indikator == "Auszubildende")
#'
#'   df_auszubildende$geschlecht <- paste0(df_auszubildende$geschlecht, " (", df_auszubildende$fachbereich, ")")
#'
#'   # ensure proportions sum to 1
#'   x_auszubildende <- setNames(round_preserve_sum(as.numeric(df_auszubildende$proportion),0),
#'                               df_auszubildende$geschlecht)
#'
#'   x_auszubildende <- x_auszubildende[order(factor(names(x_auszubildende), levels = c('Frauen (MINT)', 'Männer (MINT)')))]
#'
#'
#'   # create plot objects for waffle charts
#'   waffle_aus <- waffle::waffle(x_auszubildende, keep = FALSE) +
#'     ggplot2::labs(
#'       fill = "",
#'       title = paste0("<span style='color:black;'>", "Berufswahl (Auszubildende)</span>", br(), timerange)) +
#'     ggplot2::theme(plot.title = ggtext::element_markdown(),
#'                    plot.subtitle = ggtext::element_markdown(),
#'                    text = ggplot2::element_text(size = 14),
#'                    plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
#'                    legend.position = "bottom") +
#'     ggplot2::scale_fill_manual(
#'       values =  c("#b16fab",
#'                   "#b1b5c3"),
#'       na.value="#b1b5c3",
#'       limits = c("Frauen (MINT)", "Männer (MINT)"),
#'       guide = ggplot2::guide_legend(reverse = TRUE),
#'       labels = c(
#'         paste0("Frauen (MINT)",", ",x_auszubildende[1], "%"),
#'         paste0("Männer (MINT)",", ",x_auszubildende[2], "%"))) +
#'     ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))
#'
#'
#'
#'   waffle_be <- waffle::waffle(x_beschaeftigte, keep = FALSE) +
#'     ggplot2::labs(
#'       fill = "",
#'       title = paste0("<span style='color:black;'>", "Berufswahl (Beschäftigte)</span>", br(), timerange)) +
#'     ggplot2::theme(plot.title = ggtext::element_markdown(),
#'                    plot.subtitle = ggtext::element_markdown(),
#'                    text = ggplot2::element_text(size = 14),
#'                    plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
#'                    legend.position = "bottom") +
#'       ggplot2::scale_fill_manual(
#'         values =  c("#b16fab",
#'                     "#b1b5c3"),
#'         na.value="#b1b5c3",
#'         limits = c("Frauen (MINT)", "Männer (MINT)"),
#'         guide = ggplot2::guide_legend(reverse = TRUE),
#'         labels = c(
#'           paste0("Frauen (MINT)",", ",x_beschaeftigte[1], "%"),
#'           paste0("Männer (MINT)",", ",x_beschaeftigte[2], "%"))) +
#'       ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))
#'
#'
#'
#'
#'   if (anforderung == "Gesamt"){
#'
#'     title_help_sub <- " insgesamt"
#'
#'   } else {
#'
#'     title_help_sub <- paste0(" (",anforderung,")")
#'
#'   }
#'
#'
#'
#'   plot <- ggpubr::ggarrange(waffle_aus, NULL ,waffle_be, widths = c(1, 0.1, 1), nrow=1)
#'   text <- c(
#'     paste0("<span style='font-size:20.5pt; color:black'> Anteil von Frauen und Männer ", title_help_sub," an MINT und <br> allen anderen Berufszweigen in ", timerange))
#'
#'   ggpubr::annotate_figure(plot, gridtext::richtext_grob(text = text))
#'
#' }

#' A function to create a paired bar plot
#'
#' @description A function to return a paired bar plot for the second box inside
#' the tab "Beruf"
#'
#' @return The return value is a bar plot
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# arbeitsmarkt_absolut <- function(df,r) {
#
#   # load UI inputs from reactive value
#   timerange <- r$date_arbeitsmarkt
#
#   anforderung <- r$anforderung_arbeitsmarkt
#
#
#   # filter dataset based on UI inputs
#   df <- df %>% dplyr::filter(jahr == timerange)
#
#   df <- df %>% dplyr::filter(region == "Deutschland")
#
#   df <- df %>% dplyr::filter(anforderung == anforderung)
#
#   # call function to calculate the share of MINT and the remaining subjects
#   df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
#     df[df$fachbereich == "MINT", "wert"]
#
#   df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"
#
#   # calculate the share of males
#   values <- df %>%
#     dplyr::group_by(fachbereich, indikator) %>%
#     dplyr::mutate(wert = wert - dplyr::lead(wert)) %>% dplyr::select(wert) %>% na.omit()
#
#   df[df$geschlecht == "Gesamt", "wert"] <- values$wert
#
#   df[df$geschlecht == "Gesamt", "geschlecht"] <- "Männer"
#
#
#   if (anforderung == "Gesamt"){
#
#     title_help_sub <- " insgesamt"
#
#   } else {
#
#     title_help_sub <- paste0(" (",anforderung,")")
#
#   }
#   options(scipen=999)
#
#
#   # plot
#   ggplot2::ggplot(df, ggplot2::aes(x=indikator, y=wert, fill = fachbereich)) +
#     ggplot2::geom_bar(stat="identity", position = "dodge") +
#     ggplot2::geom_text(ggplot2::aes(label=wert, vjust = - 0.25),
#                        position=ggplot2::position_dodge(width=0.9),
#                        fontface = "bold") +
#     ggplot2::theme_minimal() +
#     ggplot2::facet_grid(~ geschlecht) +
#     ggplot2::theme(
#       strip.background = ggplot2::element_blank(),
#       text = ggplot2::element_text(size = 14),
#       plot.title = ggtext::element_markdown(hjust = 0.5)) +
#     ggplot2::xlab("") + ggplot2::ylab("Anzahl") +
#     ggplot2::scale_fill_manual(values = c("#efe8e6","#b16fab")) +
#     ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
#                                  "Arbeitnehmer*innen ", title_help_sub," in MINT und allen anderen Berufszweigen in ", timerange,
#                                  "<br><br><br>"),
#                   fill = "")
#
#
# }





#' A function to return a filtered dataset
#'
#' @description A function to similar to 'arbeitsmarkt_einstieg_bar' but with the
#' difference that it returns a dataframe instead of plot.
#'
#' @return The return value is a dataframe
#' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# data_mix_beruf <- function(df,r) {
#
#   # load UI inputs from reactive value
#   timerange <- r$date_arbeitsmarkt
#
#   anforderung <- r$anforderung_arbeitsmarkt
#
#   # filter dataset based on UI inputs
#   df <- df %>% dplyr::filter(jahr == timerange)
#
#   df <- df %>% dplyr::filter(anforderung == anforderung)
#
#   df <- df %>% dplyr::filter(fachbereich != "Alle")
#
#   colnames(df) <- c("Region", "Fachbereich", "anforderung", "Wert", "Indikator", "Jahr", "Geschlecht", "Bereich")
#
#   return(df)
#
# }




#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# arbeitsmarkt_verlauf <- function(df,r) {
#
#   # load UI inputs from reactive value
#   status_arbeitnehmer <- r$indikator_arbeitsmarkt_verlauf
#
#   timerange <- r$date_arbeitsmarkt_verlauf
#
#   states <- r$states_arbeitsmarkt_verlauf
#
#   topic <- r$topic_arbeitsmarkt_verlauf
#
#   anforderung <- r$anforderung_arbeitsmarkt_verlauf
#
#   # filter dataset based on UI inputs
#   df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])
#
#   df <- df %>% dplyr::filter(indikator == status_arbeitnehmer)
#
#   df <- df %>% dplyr::filter(anforderung == anforderung)
#
#   # remove
#   df <- df %>% dplyr::filter(region != "Deutschland")
#
#   # call function to calculate the share of MINT and the remaining subjects
#   df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
#     df[df$fachbereich == "MINT", "wert"]
#
#   df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"
#
#   # include "Osten" und "Westen" in Dataframe
#   df <- prep_arbeitsmarkt_east_west(df)
#
#   # order
#   df <- df[with(df, order(region, fachbereich, jahr, decreasing = TRUE)), ]
#
#   # calculate proportion
#   df <-  df %>%
#     dplyr::group_by(region, fachbereich, jahr) %>%
#     dplyr::mutate(props = wert[geschlecht == "Frauen"]/
#                     wert[geschlecht == "Gesamt"])
#
#   df <- df %>% dplyr::filter(geschlecht != "Gesamt")
#
#
#   df <- df %>% dplyr::filter(region %in% states)
#
#   # filter MINT or remaining subjects
#   df <- df %>% dplyr::filter(fachbereich == topic)
#
#   # order years for plot
#   df <- df[with(df, order(region, jahr, decreasing = TRUE)), ]
#
#   df$props <- df$props * 100
#
#   if (anforderung == "Gesamt"){
#
#     title_help_sub_sub <- " insgesamt"
#
#   } else {
#
#     title_help_sub_sub <- paste0(" (",anforderung,")")
#
#   }
#
#
#   if (status_arbeitnehmer == "Auszubildende"){
#
#     title_help_sub <- " in Ausbildung"
#
#   }else{
#
#     title_help_sub <- " in Beschäftigung"
#
#   }
#
#   if (topic == "MINT"){
#
#     title_help <- paste0("MINT", title_help_sub, title_help_sub_sub)
#
#   }else {
#
#     title_help <- paste0("allen anderen Berufszweigen", title_help_sub, title_help_sub_sub)
#
#   }
#
#   # plot
#   highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(props), group = region)) %>%
#     highcharter::hc_tooltip(pointFormat = "Anteil Frauen <br> Bundesland: {point.region} <br> Wert: {point.y} %") %>%
#     highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%")) %>%
#     highcharter::hc_xAxis(title = list(text = "Jahr"), style = list(fontFamily = "SourceSans3-Regular")) %>%
#     #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.", style = list(fontSize = "12px") ) %>%
#     highcharter::hc_title(text = paste0("Anteil von Frauen an ", title_help ," im Verlauf"),
#                           margin = 45,
#                           align = "center",
#                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#     highcharter::hc_chart(
#       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
#     ) %>%
#     highcharter::hc_exporting(enabled = FALSE,
#                               buttons = list(contextButton = list(
#                                 symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
#                                 onclick = highcharter::JS("function () {
#                                                               this.exportChart({ type: 'image/png' }); }"),
#                                 align = 'right',
#                                 verticalAlign = 'bottom',
#                                 theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
#
# }


#' A function to return a filtered dataset
#'
#' @description A function to similar to 'arbeitsmarkt_einstieg_bar' but with the
#' difference that it returns a dataframe instead of plot.
#'
#' @return The return value is a dataframe
#' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# data_verlauf_beruf <- function(df,r) {
#
#   # load UI inputs from reactive value
#   status_arbeitnehmer <- r$indikator_arbeitsmarkt_verlauf
#
#   timerange <- r$date_arbeitsmarkt_verlauf
#
#   states <- r$states_arbeitsmarkt_verlauf
#
#   topic <- r$topic_arbeitsmarkt_verlauf
#
#   anforderung <- r$anforderung_arbeitsmarkt_verlauf
#
#   # filter dataset based on UI inputs
#   df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])
#
#   df <- df %>% dplyr::filter(indikator == status_arbeitnehmer)
#
#   df <- df %>% dplyr::filter(anforderung == anforderung)
#
#   # remove
#   df <- df %>% dplyr::filter(region != "Deutschland")
#
#   # call function to calculate the share of MINT and the remaining subjects
#   df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
#     df[df$fachbereich == "MINT", "wert"]
#
#   df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"
#
#   # include "Osten" und "Westen" in Dataframe
#   df <- prep_arbeitsmarkt_east_west(df)
#
#   # order
#   df <- df[with(df, order(region, fachbereich, jahr, decreasing = TRUE)), ]
#
#
#   df <-  df %>%
#     dplyr::group_by(region, fachbereich, jahr, anforderung, indikator, bereich) %>%
#     dplyr::mutate(props = wert[geschlecht == "Frauen"]/
#                     wert[geschlecht == "Gesamt"])
#
#   df <- df %>% dplyr::filter(geschlecht != "Gesamt")
#
#
#   df <- df %>% dplyr::filter(region %in% states)
#
#   # filter MINT or remaining subjects
#   df <- df %>% dplyr::filter(fachbereich == topic)
#
#   # order years for plot
#   df <- df[with(df, order(region, jahr, decreasing = TRUE)), ]
#
#   df$props <- df$props * 100
#
#   colnames(df) <- c("Region", "Fachbereich", "anforderung", "Wert","Indikator", "Jahr","Geschlecht",
#                         "Bereich", "Anteil")
#
#   return(df)
#
# }

#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
#
# arbeitsmarkt_verlauf_bl <- function(df,r) {
#
#   # load UI inputs from reactive value
#   timerange <- r$date_arbeitsmarkt_verlauf_bl
#
#   states <- r$states_arbeitsmarkt_verlauf_bl
#
#   topic <- r$topic_arbeitsmarkt_verlauf_bl
#
#   anforderung <- r$anforderung_arbeitsmarkt_verlauf_bl
#
#   # filter dataset based on UI inputs
#   df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])
#
#   df <- df %>% dplyr::filter(anforderung == anforderung)
#
#   # remove
#   df <- df %>% dplyr::filter(region != "Deutschland")
#
#   # call function to calculate the share of MINT and the remaining subjects
#   df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
#     df[df$fachbereich == "MINT", "wert"]
#
#   df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"
#
#   # include "Osten" und "Westen" in Dataframe
#   df <- prep_arbeitsmarkt_east_west(df)
#
#     # calculate the share of males
#   df <- calc_arbeitsmarkt_males(df)
#
#   # calculate new "Gesamt"
#   df <-  df %>% dplyr::filter(geschlecht != "Gesamt") %>%
#     dplyr::group_by(region, fachbereich, indikator, jahr) %>%
#     dplyr::mutate(props = wert[geschlecht == "Frauen"] +
#                     wert[geschlecht == "Männer"])
#
#   # calcualte proportions
#   df <- df %>% dplyr::group_by(region, indikator, fachbereich, geschlecht, jahr) %>%
#     dplyr::summarize(proportion = wert/props)
#
#   df$proportion <- df$proportion * 100
#
#
#
#   df <- df %>% dplyr::filter(region %in% states)
#
#   df <- df %>% dplyr::filter(fachbereich == topic)
#
#   df$geschlecht <- paste0(df$geschlecht, " (", df$indikator, ")")
#
#
#
#   # order years for plot
#   df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
#
#   if (anforderung == "Gesamt"){
#
#     title_help_sub_sub <- " insgesamt"
#
#   } else {
#
#     title_help_sub_sub <- paste0(" (",anforderung,")")
#
#   }
#
#
#   if (topic == "MINT"){
#
#     title_help <- paste0("MINT", title_help_sub_sub)
#
#   }else {
#
#     title_help <- paste0("allen anderen Berufszweigen", title_help_sub_sub)
#
#   }
#
#
#   # plot
#   highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = geschlecht)) %>%
#     highcharter::hc_tooltip(pointFormat = "Anteil {point.geschlecht} <br> Wert: {point.y} %") %>%
#     highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%")) %>%
#     highcharter::hc_xAxis(title = list(text = "Jahr"), style = list(fontFamily = "SourceSans3-Regular")) %>%
#     #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.", style = list(fontSize = "12px") ) %>%
#     highcharter::hc_title(text = paste0("Anteil von Frauen und Männer ", title_help ," im Verlauf"),
#                           margin = 45,
#                           align = "center",
#                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#     highcharter::hc_chart(
#       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
#     ) %>%
#     highcharter::hc_exporting(enabled = FALSE,
#                               buttons = list(contextButton = list(
#                                 symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
#                                 onclick = highcharter::JS("function () {
#                                                               this.exportChart({ type: 'image/png' }); }"),
#                                 align = 'right',
#                                 verticalAlign = 'bottom',
#                                 theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
#
# }
















#' A function to plot time series
#'
#' @description A function to plot the time series
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# arbeitsmarkt_anforderungen_verlauf <- function(df,r) {
#
#   # load UI inputs from reactive value
#   timerange <- r$date_arbeitsmarkt_anforderungen_verlauf
#
#   states <- r$states_arbeitsmarkt_anforderungen_verlauf
#
#   indikator_choice <- r$indikator_arbeitsmarkt_anforderungen_verlauf
#
#   anforderung <- r$anforderung_arbeitsmarkt_anforderungen_verlauf
#
#   # filter dataset based on UI inputs
#   df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])
#
#   # remove
#   df <- df %>% dplyr::filter(geschlecht == "Gesamt")
#
#   # df <- df %>% dplyr::filter(anforderung != "Helfer")
#
#   df <- prep_arbeitsmarkt_east_west(df)
#
#   # calculate new "Gesamt
#   df_new_gesamt <- df %>% dplyr::filter(anforderung == "Gesamt")
#     # dplyr::group_by(region, fachbereich, indikator, jahr, geschlecht, bereich) %>%
#     # dplyr::summarise(wert = sum(wert)) %>%
#     # dplyr::mutate(anforderung = "Gesamt") %>%
#     # dplyr::ungroup()
#
#   df <- rbind(df %>% dplyr::filter(anforderung != "Gesamt"), df_new_gesamt)
#
#   # df <- calc_arbeitsmarkt_mint(df)
#
#   df_new_gesamt <- df_new_gesamt %>%
#     dplyr::filter(fachbereich == "Alle") %>%
#     dplyr::rename(wert_gesamt = "wert") %>%
#     dplyr::select(-c("fachbereich", "anforderung"))
#
#   df <- df %>% dplyr::left_join(df_new_gesamt, by = c("region", "indikator", "jahr", "geschlecht", "bereich")) %>%
#     dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
#     dplyr::select(-c("wert", "wert_gesamt"))
#
#   df <- df %>% dplyr::filter(region == states)
#
#   df <- df %>% dplyr::filter(indikator == indikator_choice)
#
#   df <- df %>% dplyr::filter(fachbereich == "MINT")
#
#   df <- df %>% dplyr::filter(anforderung %in% anforderung)
#
#   # plot
#   highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = anforderung)) %>%
#     highcharter::hc_tooltip(pointFormat = "Anteil <br> Bundesland: {point.region} <br> Wert: {point.y} %") %>%
#     highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
#     highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
#     #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
#     highcharter::hc_title(text = paste0("Anteil der Arbeitnehmerinnen im Zeitverlauf in ", states),
#                           margin = 45,
#                           align = "center",
#                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#     highcharter::hc_chart(
#       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
#     ) %>%
#     highcharter::hc_exporting(enabled = FALSE,
#                               buttons = list(contextButton = list(
#                                 symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
#                                 onclick = highcharter::JS("function () {
#                                                               this.exportChart({ type: 'image/png' }); }"),
#                                 align = 'right',
#                                 verticalAlign = 'bottom',
#                                 theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
#
# }

#' A function to plot a bar plot
#'
#' @description A function to create a bar plot for the "Beruf" tab
#'
#' @return The return value is a waffle chart
#' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# arbeitsmarkt_anforderungen_vergleich <- function(df,r) {
#
#   timerange <- r$date_arbeitsmarkt_anforderungen_vergleich
#
#   states <- r$states_arbeitsmarkt_anforderungen_vergleich
#
#   indikator_choice <- r$indikator_arbeitsmarkt_anforderungen_vergleich
#
#   # filter dataset based on UI inputs
#   df <- df %>% dplyr::filter(jahr == timerange)
#
#   # remove
#   df <- df %>% dplyr::filter(geschlecht == "Gesamt")
#
#   # df <- df %>% dplyr::filter(anforderung != "Helfer")
#
#   df <- prep_arbeitsmarkt_east_west(df)
#
#   # calculate new "Gesamt
#   # df_new_gesamt <- df %>% dplyr::filter(anforderung != "Gesamt") %>%                       ###kab
#   #   dplyr::group_by(region, fachbereich, indikator, jahr, geschlecht, bereich) %>%
#   #   dplyr::summarise(wert = sum(wert)) %>%
#   #   dplyr::mutate(anforderung = "Gesamt") %>%
#   #   dplyr::ungroup()
#
#   df_new_gesamt <- df %>% dplyr::filter(anforderung == "Gesamt")
#       #dplyr::group_by(region, fachbereich, indikator, jahr, geschlecht, bereich) %>%
#      # dplyr::summarise(wert = sum(wert)) %>%
#       # dplyr::mutate(anforderung = "Gesamt") %>%
#       #dplyr::ungroup()
#
#   df <- rbind(df %>% dplyr::filter(anforderung != "Gesamt"), df_new_gesamt)
#
#   # df <- calc_arbeitsmarkt_mint(df)
#
#   df_new_gesamt <- df_new_gesamt %>%
#     dplyr::filter(fachbereich == "Alle") %>%
#     dplyr::rename(wert_gesamt = "wert") %>%
#     dplyr::select(-c("fachbereich", "anforderung"))
#
#   df <- df %>% dplyr::filter(region == states)
#
#   df <- df %>% dplyr::left_join(df_new_gesamt, by = c("region", "indikator", "jahr", "geschlecht", "bereich")) %>%
#     dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
#     dplyr::select(-c("wert", "wert_gesamt"))
#
#   df <- df %>% dplyr::filter(indikator == indikator_choice)
#
#   df <- df %>% dplyr::filter(fachbereich == "MINT")
#
#   reihenfolge <- c("Experte", "Spezialist", "Fachkraft", "Gesamt")
#
#   df <- df %>%
#     dplyr::mutate(anforderung =  factor(anforderung, levels = reihenfolge)) %>%
#     dplyr::arrange(anforderung)
#
#   # plot
#   a <- ifelse(df$anforderung == "Gesamt", "#b16fab", "grey30")
#
#   ggplot2::ggplot(df, ggplot2::aes(y=anforderung, x=proportion)) +
#     ggplot2::geom_bar(stat="identity", fill = "#b16fab") +
#     ggplot2::geom_text(ggplot2::aes(label=paste(round(proportion),"%")), hjust = -0.3,
#                        fontface = "bold") +
#     ggplot2::theme_minimal() +
#     ggplot2::theme(
#       axis.text.y = ggplot2::element_text(colour = a),
#       text = ggplot2::element_text(size = 14),
#       plot.title = ggtext::element_markdown(hjust = 0.5)) +
#     ggplot2::ylab("") + ggplot2::xlab("Anteil") +
#     ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
#                                  indikator_choice, ": Anteil der anforderungs im Vergleich in ", timerange,
#                                  "<br><br><br>"),
#                   fill = "") +
#     ggplot2::scale_y_discrete(expand = c(0,0)) +
#     ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))
#
# }




#' A function to plot a graph.
#'
#' @description A function to create a pie chart for the first box
#' inside the tab "Beruf".
#'
#' @return The return value is a plot
#' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# arbeitsmarkt_einstieg_pie <- function(df,r) {
#
#   # load UI inputs from reactive value
#   timerange <- r$date_arbeitsmarkt_einstieg
#
#   df <- df %>% dplyr::filter(jahr == timerange)
#
#   df <- df %>% dplyr::filter(region == "Deutschland")
#
#   df <- df %>% dplyr::filter(anforderung == "Gesamt")
#
#   # call function to calculate the share of MINT and the remaining subjects
#   df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
#     df[df$fachbereich == "MINT", "wert"]
#
#   df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"
#
#   # calculate the share of males
#   df <- calc_arbeitsmarkt_males(df)
#
#   df_beschaeftigte <- df %>% dplyr::filter(indikator == "Beschäftigte")
#
#   df_beschaeftigte <- df_beschaeftigte %>% dplyr::filter(geschlecht == "Gesamt")
#
#   # calculate proportions
#   df_beschaeftigte <- share_pie_neu(df_beschaeftigte)
#
#   df_beschaeftigte$geschlecht <- df_beschaeftigte$fachbereich
#
#   df_auszubildende <- df %>% dplyr::filter(indikator == "Auszubildende")
#
#   df_auszubildende <- df_auszubildende %>% dplyr::filter(geschlecht == "Gesamt")
#
#   # calculate proportions
#   df_auszubildende <- share_pie_neu(df_auszubildende)
#
#   df_auszubildende$geschlecht <- df_auszubildende$fachbereich
#
#   plot_auszubildende <- highcharter::hchart(df_auszubildende, size = 280, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
#     highcharter::hc_tooltip(
#       pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#
#     highcharter::hc_colors(c("#efe8e6","#b16fab")) %>%
#     highcharter::hc_title(text = paste0("Berufswahl (Auszubildende)", br(), timerange),
#                           margin = 45,
#                           align = "center",
#                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#     highcharter::hc_chart(
#       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
#     highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
#     highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#                                            dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE))
#
#
#
#   plot_beschaeftigte <- highcharter::hchart(df_beschaeftigte, size = 280, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
#     highcharter::hc_tooltip(
#       pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#     highcharter::hc_colors(c("#efe8e6","#b16fab")) %>%
#     highcharter::hc_title(text = paste0("Berufswahl (Beschäftigte)", br(), timerange),
#                           margin = 45,
#                           align = "center",
#                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#     highcharter::hc_chart(
#       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
#     highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
#     highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#                                            dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE))
#
#
#
#     plot_beschaeftigte <- plot_beschaeftigte %>% highcharter::hc_colors(c("#efe8e6","#b16fab"))
#
#     plot_auszubildende <- plot_auszubildende %>% highcharter::hc_colors(c("#efe8e6","#b16fab"))
#
#
#   highcharter::hw_grid(
#
#     plot_auszubildende,
#
#     plot_beschaeftigte,
#
#     ncol = 2,
#     browsable = TRUE
#   )
#
#
# }



#' A function to return a filtered dataset
#'
#' @description A function to similar to 'arbeitsmarkt_einstieg_bar' but with the
#' difference that it returns a dataframe instead of plot.
#'
#' @return The return value is a dataframe
#' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# data_einstieg_beruf <- function(df,r) {
#
#   # load UI inputs from reactive value
#   timerange <- r$date_arbeitsmarkt_einstieg
#
#   # filter dataset based on UI inputs
#   df <- df %>% dplyr::filter(jahr == timerange)
#
#   df <- df %>% dplyr::filter(region == "Deutschland")
#
#   df <- df %>% dplyr::filter(anforderung == "Gesamt")
#
#   # call function to calculate the share of MINT and the remaining subjects
#   df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
#     df[df$fachbereich == "MINT", "wert"]
#
#   df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"
#
#   # calculate the share of males
#   df <-   df <- calc_arbeitsmarkt_males(df)
#
#   colnames(df) <- c("Region", "Fachbereich", "anforderung", "Wert", "Indikator", "Jahr", "Geschlecht", "Bereich")
#
#   return(df)
#
# }







#' #' A function to plot time series
#' #'
#' #' @description A function to plot the time series
#' #'
#' #' @return The return value, if any, from executing the function.
#' #' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' arbeitsmarkt_anforderungen_verlauf_gender <- function(df,r) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_arbeitsmarkt_anforderungen_gender_verlauf
#'
#'   anforderung <- r$level_arbeitsmarkt_anforderungen_gender_verlauf
#'
#'   states <- r$states_arbeitsmarkt_anforderungen_gender_verlauf
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])
#'
#'   # remove
#'   #df <- df %>% dplyr::filter(anforderung != "Helfer")### kab
#'
#'   df <- prep_arbeitsmarkt_east_west(df)
#'
#'   # calculate new "Gesamt
#'   df_new_gesamt <- df %>% dplyr::filter(anforderung == "Gesamt")
#'   # dplyr::group_by(region, fachbereich, indikator, jahr, geschlecht, bereich) %>%
#'   # dplyr::summarise(wert = sum(wert)) %>%
#'   # dplyr::mutate(anforderung = "Gesamt") %>% dplyr::ungroup()
#'
#'
#'
#'   df <- rbind(df %>% dplyr::filter(anforderung != "Gesamt"), df_new_gesamt)
#'
#'   df <- df %>% dplyr::filter(region %in% states)
#'
#'   df <- calc_arbeitsmarkt_males(df)
#'
#'   df <- calc_arbeitsmarkt_mint(df)
#'
#'   df_total_gender <- calc_arbeitsmarkt_males(df_new_gesamt) %>%
#'     dplyr::filter(geschlecht != "Gesamt") %>%
#'     dplyr::filter(fachbereich == "Alle") %>%
#'     dplyr::select("region", "indikator", "jahr", "wert", "geschlecht") %>%
#'     dplyr::rename(wert_gesamt = "wert")
#'
#'   df <- df %>% dplyr::left_join(df_total_gender, by = c("region", "indikator", "jahr", "geschlecht")) %>%
#'     dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
#'     dplyr::select(-c("wert", "wert_gesamt")) %>%
#'     dplyr::filter(geschlecht == "Frauen")
#'
#'   df <- df %>% dplyr::filter(anforderung %in% anforderung)
#'
#'   df <- df %>% dplyr::filter(fachbereich == "MINT")
#'
#'   df$geschlecht <- paste0(df$geschlecht, " (", df$indikator, ")")
#'
#'   # order years for plot
#'   df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
#'
#'   if (anforderung == "Gesamt"){
#'
#'     title_help <- " insgesamt"
#'
#'   } else {
#'
#'     title_help <- paste0(" mit anforderung ", anforderung)
#'
#'   }
#'
#'   # plot
#'   highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = geschlecht)) %>%
#'     highcharter::hc_tooltip(pointFormat = "Anteil <br> Bundesland: {point.region} <br> Wert: {point.y} %") %>%
#'     highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
#'     highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
#'     # highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
#'     highcharter::hc_title(text = paste0("Arbeitnehmerinnen: ", title_help, " in ", states),
#'                           margin = 45,
#'                           align = "center",
#'                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#'     highcharter::hc_chart(
#'       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
#'     ) %>%
#'     highcharter::hc_exporting(enabled = FALSE,
#'                               buttons = list(contextButton = list(
#'                                 symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
#'                                 onclick = highcharter::JS("function () {
#'                                                               this.exportChart({ type: 'image/png' }); }"),
#'                                 align = 'right',
#'                                 verticalAlign = 'bottom',
#'                                 theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
#'
#' }
#'
#'
#' #' A function to create a dumbbell plot
#' #'
#' #' @description A function to return a ranking of subject for both genders
#' #'
#' #' @return The return value is a dumbbell plot
#' #' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' arbeitsmarkt_anforderungen_vergleich_gender <- function(df, r) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_arbeitsmarkt_anforderungen_gender_vegleich
#'
#'   states <- r$states_arbeitsmarkt_anforderungen_gender_vegleich
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr == timerange)
#'
#'   # remove
#'   # df <- df %>% dplyr::filter(anforderung != "Helfer")
#'
#'   df <- prep_arbeitsmarkt_east_west(df)
#'
#'   # calculate new "Gesamt
#'   df_new_gesamt <- df %>% dplyr::filter(anforderung == "Gesamt")
#'   # dplyr::group_by(region, fachbereich, indikator, jahr, geschlecht, bereich) %>%
#'   # dplyr::summarise(wert = sum(wert)) %>%
#'   # dplyr::mutate(anforderung = "Gesamt") %>% dplyr::ungroup()
#'
#'   df <- rbind(df %>% dplyr::filter(anforderung != "Gesamt"), df_new_gesamt)
#'
#'   df <- df %>% dplyr::filter(region %in% states)
#'
#'   df <- calc_arbeitsmarkt_males(df)
#'
#'   df <- calc_arbeitsmarkt_mint(df)
#'
#'   df_total_gender <- calc_arbeitsmarkt_males(df_new_gesamt) %>%
#'     dplyr::filter(geschlecht != "Gesamt") %>%
#'     dplyr::filter(fachbereich == "Alle") %>%
#'     dplyr::select("region", "indikator", "jahr", "wert", "geschlecht") %>%
#'     dplyr::rename(wert_gesamt = "wert")
#'
#'   df <- df %>% dplyr::left_join(df_total_gender, by = c("region", "indikator", "jahr", "geschlecht")) %>%
#'     dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
#'     dplyr::select(-c("wert", "wert_gesamt")) %>%
#'     dplyr::filter(geschlecht == "Frauen")
#'
#'   df <- df %>% dplyr::filter(fachbereich == "MINT")
#'
#'   # spread column
#'   df <- tidyr::spread(df, indikator, proportion)
#'
#'   df <- df %>% tidyr::drop_na()
#'
#'   df2 <- tidyr::gather(df, group, value, -anforderung) %>%
#'     dplyr::filter(group %in% c("Beschäftigte", "Auszubildende")) %>%
#'     dplyr::mutate(value = as.numeric(value))
#'
#'   df$anforderung <- reorder(df$anforderung, df$Beschäftigte)
#'
#'   df2$anforderung <- factor(df2$anforderung, levels = levels(df$anforderung))
#'
#'   ggplot2::ggplot(df,
#'                   ggplot2::aes(y = anforderung)) +
#'     ggplot2::geom_point(data = df2, ggplot2::aes(x = value, color = group), size = 5) +
#'     ggalt::geom_dumbbell(
#'       ggplot2::aes(x = Auszubildende, xend = Beschäftigte),
#'       size = 0.5,
#'       size_x = 5,
#'       size_xend = 5,
#'       colour = "black",
#'       colour_x = "#b1b5c366",
#'       colour_xend = "#f5adac66",
#'       dot_guide=TRUE) +
#'     ggplot2::theme_minimal() +
#'     ggplot2::scale_color_manual(name = "", values = c("#b1b5c366", "#f5adac66")) +
#'     ggplot2::theme(legend.position="top",
#'                    panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
#'                    plot.title = ggtext::element_markdown(hjust = 0.5),
#'                    axis.text.y = ggplot2::element_text(size = 11)) +
#'     ggplot2::ylab("") + ggplot2::xlab("") +
#'     ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
#'                                  "Frauen: Wahl von MINT-Berufen <br>", states, " in ",timerange,
#'                                  "<br><br><br>"),
#'                   color = "") +
#'     ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))
#'
#' }
#'
#'
#' #' A function to create a bar plot
#' #'
#' #' @description A function to return a ranking of subject for both genders
#' #'
#' #' @return The return value is a bar plot
#' #' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' arbeitsmarkt_bl_gender_vergleich <- function(df, r) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_arbeitsmarkt_bl_gender_vergleich
#'
#'   #anforderung <- r$anforderung_arbeitsmarkt_bl_gender_vergleich
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr == timerange)
#'
#'   # remove
#'   df <- df %>% dplyr::filter(region != "Deutschland")
#'
#'   df <- calc_arbeitsmarkt_males(df)
#'
#'   df_gesamt <- df %>%
#'     dplyr::filter(fachbereich == "Alle",
#'                   anforderung == "Gesamt")
#'
#'   df <- df %>%
#'     dplyr::left_join(df_gesamt, by = c("region", "jahr", "geschlecht", "indikator", "bereich")) %>%
#'     dplyr::rename(anforderung = "anforderung.x",
#'                   fachbereich = "fachbereich.x",
#'                   wert = "wert.x",
#'                   wert_sum = "wert.y") %>%
#'     dplyr::select(-c("fachbereich.y", "anforderung.y")) %>%
#'     dplyr::mutate(proportion = (wert/wert_sum)*100)%>%
#'     dplyr::filter(anforderung == "Gesamt",
#'                   fachbereich == "MINT")
#'
#'   df <- df %>% dplyr::filter(anforderung == "Gesamt")
#'
#'   df <- df %>% dplyr::filter(geschlecht == "Frauen")
#'
#'   df <- df %>% dplyr::filter(fachbereich == "MINT")
#'
#'   df <- df %>% dplyr::select(-c("wert", "wert_sum"))
#'
#'   # spread column
#'   df <- tidyr::spread(df, indikator, proportion)
#'
#'   df <- df %>% tidyr::drop_na()
#'
#'   df2 <- tidyr::gather(df, group, value, -region) %>%
#'     dplyr::filter(group %in% c("Beschäftigte", "Auszubildende")) %>%
#'     dplyr::mutate(value = as.numeric(value))
#'
#'   df$region <- reorder(df$region, df$Beschäftigte)
#'
#'   df2$region <- factor(df2$region, levels = levels(df$region))
#'
#'   # if (anforderung == "Gesamt"){
#'   #
#'   #   title_help <- " insgesamt"
#'   #
#'   # } else {
#'   #
#'   #   title_help <- paste0(" mit anforderung ", anforderung)
#'   #
#'   # }
#'
#'   ggplot2::ggplot(df,
#'                   ggplot2::aes(y = region)) +
#'     ggplot2::geom_point(data = df2, ggplot2::aes(x = value, color = group), size = 5) +
#'     ggalt::geom_dumbbell(
#'       ggplot2::aes(x = Auszubildende, xend = Beschäftigte),
#'       size = 0.5,
#'       size_x = 5,
#'       size_xend = 5,
#'       colour = "black",
#'       colour_x = "#b1b5c366",
#'       colour_xend = "#f5adac66",
#'       dot_guide=TRUE) +
#'     ggplot2::theme_minimal() +
#'     ggplot2::scale_color_manual(name = "", values = c("#b1b5c366", "#f5adac66")) +
#'     ggplot2::theme(legend.position="top",
#'                    panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
#'                    plot.title = ggtext::element_markdown(hjust = 0.5),
#'                    axis.text.y = ggplot2::element_text(size = 11)) +
#'     ggplot2::ylab("") + ggplot2::xlab("") +
#'     ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
#'                                  "Frauen: Wahl von MINT-Berufen<br>"
#'                                  #, title_help
#'                                  ,timerange,
#'                                  "<br><br><br>"),
#'                   color = "") +
#'     ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))
#'
#' }
#'
#'
#' #' A function to plot a table
#' #'
#' #' @description A function to create a table for detailed overview for landkreise
#' #'
#' #' @return The return value is a table
#' #' @param df The dataframe "Arbeitsmarkt_detailliert.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' arbeitsmarkt_lk_detail_table <- function(df, input_values, r) {
#'
#'   # get input variables
#'   input_count <- stringr::str_sub(names(input_values), 1, 4) %>% unique()
#'   variable_counts <- input_count[input_count %>% stringr::str_detect("var")] %>% sort()
#'
#'   state1 <- r[["mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_1-states_beruf_arbeitsmarkt_landkreis_table"]]
#'   state2 <- r[["mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_2-states_beruf_arbeitsmarkt_landkreis_table"]]
#'   state3 <- r[["mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_3-states_beruf_arbeitsmarkt_landkreis_table"]]
#'
#'   region1 <- r[["mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_1-region_beruf_arbeitsmarkt_landkreis_table"]]
#'   region2 <- r[["mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_2-region_beruf_arbeitsmarkt_landkreis_table"]]
#'   region3 <- r[["mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_3-region_beruf_arbeitsmarkt_landkreis_table"]]
#'
#'
#'   # create empty dataframe
#'   df_steckbrief <- data.frame()
#'
#'   # for each 'Betrachtung' = variable_counts, get detailed input, calculate
#'   # values and build display dataframe
#'   for(i in variable_counts){
#'
#'     category <- input_values[[paste0(i, "-kategorie_beruf_arbeitsmarkt_landkreis_vergleich")]]
#'     domain <- input_values[[paste0(i, "-fachbereich_beruf_arbeitsmarkt_landkreis_vergleich")]]
#'     indikator_azubi <- input_values[[paste0(i, "-indikator1_beruf_arbeitsmarkt_landkreis_vergleich")]]
#'     indikator_besch <- input_values[[paste0(i, "-indikator2_beruf_arbeitsmarkt_landkreis_vergleich")]]
#'
#'     df_compare_list_region1 <- calculate_landkreis(df, state1, category, domain, indikator_azubi, indikator_besch, region1)
#'     if(region1 != "Gesamt"){
#'       df_compare_list_region1[[1]] <- df_compare_list_region1[[1]] %>% dplyr::filter(landkreis == region1)
#'     }
#'
#'     df_compare_list_region2 <- calculate_landkreis(df, state2, category, domain, indikator_azubi, indikator_besch, region2)
#'
#'     if(region2 != "Gesamt"){
#'       df_compare_list_region2[[1]] <- df_compare_list_region2[[1]] %>% dplyr::filter(landkreis == region2)
#'     }
#'
#'     df_compare_list_region3 <- calculate_landkreis(df, state3, category, domain, indikator_azubi, indikator_besch, region3)
#'
#'     if(region3 != "Gesamt"){
#'       df_compare_list_region3[[1]] <- df_compare_list_region3[[1]] %>% dplyr::filter(landkreis == region3)
#'     }
#'
#'     line_name <- paste(category, domain, ifelse(category=="Auszubildende", indikator_azubi, indikator_besch), sep = "-")
#'     df_var <- data.frame(line_name = line_name,
#'                          region1 = paste0(df_compare_list_region1[[1]]$wert, "<br/>(", df_compare_list_region1[[1]]$prob, "% ", df_compare_list_region1[[3]], " an ", df_compare_list_region1[[2]], ")"),
#'                          region2 = paste0(df_compare_list_region2[[1]]$wert, "<br/>(", df_compare_list_region2[[1]]$prob, "% ", df_compare_list_region2[[3]], " an ", df_compare_list_region2[[2]], ")"),
#'                          region3 = paste0(df_compare_list_region3[[1]]$wert, "<br/>(", df_compare_list_region3[[1]]$prob, "% ", df_compare_list_region3[[3]], " an ", df_compare_list_region3[[2]], ")"))
#'
#'
#'     df_steckbrief <- dplyr::bind_rows(df_steckbrief, df_var)
#'
#'   }
#'
#'   # adjust names for the dataframe
#'   names(df_steckbrief) <- c("", paste0("<b>", state1, "-" ,region1, "</b>"), paste0("<b>", state2, "-" ,region2, "</b>"), paste0("<b>", state3, "-" ,region3, "</b>"))
#'
#'   return(df_steckbrief)
#'
#' }


