



map_selection_germany <- readRDS("data/map_data/map_selection_german.rds")
map_selection_europe <- readRDS("data/map_data/map_selection_europa.rds")
map_selection_international <- readRDS("data/map_data/map_selection_international.rds")










# Berufswahl MINT ----
###Tab 1 ----
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

  praep <- ifelse(regio == "Saarland", " im ", " in ")

  if(betrachtung == "Einzelansicht - Kuchendiagramm"){
    gruppe <- r$indikator_arbeitsmarkt_einsteig_vergleich_kuchen
    abs_rel <- "In Prozent"
  }else if (betrachtung == "Gruppenvergleich - Balkendiagramm"){

    gruppe <- r$indikator_arbeitsmarkt_einsteig_vergleich_balken
    abs_rel <- r$abs_zahlen_arbeitsmarkt_einstieg_vergleich

  } else {
    gruppe <- c("Auszubildende",
                "Auszubildende (1. Jahr)",
                "ausländische Auszubildende",
                "Beschäftigte",
                "ausländische Beschäftigte",
                "Beschäftigte u25",
                "Beschäftigte 25-55",
                "Beschäftigte ü55")
  }

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

    #Anteil MINT berechnen

    df_query <- glue::glue_sql("
    SELECT *
    FROM arbeitsmarkt_detail
    WHERE landkreis = 'alle Landkreise'
    AND bundesland = {regio}
    AND anforderung = 'Gesamt'
    AND geschlecht = 'Gesamt'
    AND indikator In ({gruppe*})
    AND fachbereich = 'Alle'", .con = con)

    df_new_gesamt <- DBI::dbGetQuery(con, df_query)

    df_new_gesamt <- df_new_gesamt %>%
      dplyr::select( "indikator", "bundesland", "landkreis", "fachbereich",
                     "landkreis_zusatz", "landkreis_nummer", "jahr", "anforderung", "geschlecht", "wert") %>%
      dplyr::rename(wert_gesamt = "wert")

    df3 <- df %>%
      dplyr::left_join(df_new_gesamt, by = c("indikator", "bundesland", "landkreis",
                                             "landkreis_zusatz", "landkreis_nummer", "jahr", "anforderung", "geschlecht")) %>%
      dplyr::rename(fachbereich = "fachbereich.x") %>%
      dplyr::select(-fachbereich.y) %>%
      dplyr::group_by(indikator) %>%
      dplyr::mutate(proportion = round((wert/wert_gesamt)*100, 1))

    #andere Berufe berechnen:
    df_andere <- df %>%
      dplyr::left_join(df_new_gesamt, by = c("indikator", "bundesland", "landkreis",
                                             "landkreis_zusatz", "landkreis_nummer", "jahr", "anforderung", "geschlecht")) %>%
      dplyr::mutate(fachbereich = "Andere Berufe") %>%
      dplyr::mutate(wert = wert_gesamt-wert) %>%
      dplyr::mutate(proportion = round((wert/wert_gesamt)*100, 1))

    df <- rbind(df3, df_andere)


  #Graifken
  if(betrachtung == "Einzelansicht - Kuchendiagramm"){

    df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

    titel <- ifelse(regio != "Saarland",
                    paste0(gruppe, " in ", regio, " (", timerange, ")"),
                    paste0(gruppe, " im ", regio, " (", timerange, ")"))

    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", fachbereich, "</b><br>",
          "Jahr: ", jahr, "<br>",
          "Anteil: ", prettyNum(proportion, big.mark = ".", decimal.mark = ","), " %,
          Anzahl: ", wert
        )
      )

    color <- c("#b16fab","#efe8e6")
    quelle <- "Quelle: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."


   out <- piebuilder_plotly(df, titel, x="fachbereich", y = "proportion",
                            color, quelle=quelle)

  }
  else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){



    if(abs_rel == "In Prozent"){


      df <- df[with(df, order(proportion, decreasing = TRUE)), ]

      order <- rev(unique(df$indikator))

      df <- df %>%
        dplyr::mutate(
          fachbereich = factor(fachbereich, levels = c("MINT", "Andere Berufe")),
          .tooltip = paste0(
            "<b><span style='font-size:15px;'>", indikator, "</span></b><br>",
            "<span style='font-size:15px;'>", fachbereich, "</span><br>",
            "Anzahl: ", (formatC(as.numeric(wert),format = "f",digits = 0,big.mark = ".")), "<br>",
           "Anteil: ", round(proportion, 1), " %")
          )



      x <- "indikator"
      y <- "proportion"
      group <- "fachbereich"
      titel <- paste0("MINT-Anteil unterschiedlicher Beschäftigtengruppen", praep, regio, " (", timerange, ")")

      color <- c("#b16fab", "#efe8e6")
      quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."


      out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h", group=group, color = color,
                                  order = order, stacking = TRUE, percent = TRUE, quelle=quelle)




    }else{

      #Trennpunkte für lange Zahlen ergänzen
      #df$wert_disp <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

      df$wert <- as.numeric(gsub("\\.", "", df$wert))
      #df <- df[with(df, order(wert, decreasing = TRUE)), ]

      order <- unique(df$indikator)

      df <- df %>%
        dplyr::mutate(
          fachbereich = factor(fachbereich, levels = c("MINT", "Andere Berufe")),
          .tooltip = paste0(
            "<b><span style='font-size:15px;'>", indikator, "</span></b><br>",
            "<span style='font-size:15px;'>", fachbereich, "</span><br>",
            "Anzahl: ", (formatC(as.numeric(wert),format = "f",digits = 0,big.mark = ".")), "<br>",
            "Anteil: ", round(proportion, 1), " %")
        )



      titel <- paste0("Beschäftigte in MINT in unterschiedlichen Beschäftigtengruppen",praep, regio, " (Anzahl,", timerange, ")")


      x <- "indikator"
      y <- "wert"
      group <- "fachbereich"

      color <- c("#b16fab", "#efe8e6")
      quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."


      out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h", group=group, color = color,
                                  order = order, percent=FALSE, stacking=FALSE, quelle=quelle)




    }

  }
  return(out)
}


### Tab 2 ----
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
  indi <- r$indikator_arbeitsmarkt_einstieg_verlauf_2

  # df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%

  df_query <- glue::glue_sql("
  SELECT indikator, bundesland, fachbereich, jahr, wert
  FROM arbeitsmarkt_detail
  WHERE jahr IN ({t*})
  AND bundesland = {regio}
  AND landkreis = 'alle Landkreise'
  AND geschlecht = 'Gesamt'
  AND anforderung = 'Gesamt'
  AND fachbereich = 'MINT'
  AND indikator IN ({indi*})

                               ", .con = con)
  df <- DBI::dbGetQuery(con, df_query)

  if (absolut_selector == "In Prozent"){


    df_query <- glue::glue_sql("
    SELECT indikator, bundesland, fachbereich, jahr, wert
    FROM arbeitsmarkt_detail
    WHERE jahr IN ({t*})
    AND bundesland = {regio}
    AND landkreis = 'alle Landkreise'
    AND geschlecht = 'Gesamt'
    AND anforderung = 'Gesamt'
    AND fachbereich = 'Alle'
    AND indikator IN ({indi*})
                               ", .con = con)
    df_alle <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::left_join(df_alle, dplyr::join_by(indikator, bundesland, jahr)) %>%
      dplyr::select(-fachbereich.y)%>%
      dplyr::rename(fachbereich = fachbereich.x,
                    wert = wert.x,
                    wert_ges = wert.y) %>%
      dplyr::mutate(prop = round(wert/wert_ges *100, 1))

    sorted_indicators <- df %>%
      dplyr::group_by(fachbereich) %>%
      dplyr::summarize(m_value = mean(round(prop, 1), na.rm = TRUE)) %>%
      dplyr::arrange(m_value) %>%
      dplyr::pull(fachbereich)

    df$fachbereich <- factor(df$fachbereich, levels = sorted_indicators)

    # order years for plot
    df <- df[with(df, order(jahr, decreasing = FALSE)), ]

    # plot

    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", indikator, "</b><br>",
          "Jahr: ", jahr, "<br>",
          "Anteil: ", prettyNum(prop, big.mark = ".", decimal.mark = ","), " %"
        )
      )

    titel <- ifelse(regio == "Saarland",
                    paste0("MINT-Anteil unterschiedlicher Beschäftigtengruppen im ", regio),
                    paste0("MINT-Anteil unterschiedlicher Beschäftigtengruppen in ", regio))

    color <- c("#b16fab", "#154194","#66cbaf","#112c5f", "#35bd97", "#5d335a",
               "#5f94f9", "#007655", "#d0a9cd")[1:(length(unique(df$indikator)))]

    quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    out <- linebuilder_plotly(df, titel, x = "jahr", y = "prop", group = "indikator", color = color, quelle = quelle)

  } else if(absolut_selector == "Anzahl") {

    sorted_indicators <- df %>%
      dplyr::group_by(fachbereich) %>%
      dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
      dplyr::arrange(m_value) %>%
      dplyr::pull(fachbereich)

    df$fachbereich <- factor(df$fachbereich, levels = sorted_indicators)

    # order years for plot
    df <- df[with(df, order(jahr, decreasing = FALSE)), ]

    titel <- paste0("Anzahl von MINT-Beschäftigten und -Auszubildenden an allen Beschäftigten o. Auszubildenden in ", regio)
    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", indikator, "</b><br>",
          "Jahr: ", jahr, "<br>",
          "Anzahl: ", prettyNum(wert, big.mark = ".", decimal.mark = ",")
        )
      )

    color <- c("#b16fab", "#154194","#66cbaf", "#35bd97", "#5d335a",
               "#5f94f9", "#007655", "#d0a9cd", "#112c5f")[1:(length(unique(df$indikator)))]

    quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    out <- linebuilder_plotly(df, titel, x = "jahr", y = "wert", group = "indikator", format = ",d", color = color, quelle = quelle)



  }
}

### Tab 3 ----
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

    df_query <- glue::glue_sql("
    SELECT *
    FROM arbeitsmarkt_detail
    WHERE jahr = {timerange}
    AND indikator = {indi}
    AND landkreis = 'alle Landkreise'
    AND anforderung = 'Gesamt'
    AND geschlecht = 'Gesamt'
    AND fachbereich In ('MINT', 'Alle')
    AND NOT bundesland IN ('Deutschland', 'Westdeutschland (o. Berlin)', 'Ostdeutschland (einschl. Berlin)')
                               ", .con = con)
    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)


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
    title_help <- paste0(indi, "n")
    title_help <- ifelse(grepl("ausländische Beschäftigte", indi), "ausländischen Beschäftigten", title_help)
    title_help <- ifelse(grepl("ausländische Auszubildende", indi), "ausländischen Auszubildenden", title_help)
    title_help <- ifelse(grepl("Jahr", indi), "Auszubildenden im ersten Lehrjahr", title_help)
    title_help <- ifelse(grepl("u25", indi), "Beschäftigten unter 25 Jahren", title_help)
    title_help <- ifelse(grepl("25-55", indi), "Beschäftigten zwischen 25 und 55 Jahren", title_help)
    title_help <- ifelse(grepl("ü55", indi), "Beschäftigten über 55 Jahren", title_help)

    title_h2 <- ifelse(grepl("Auszu", title_help), "Auszubildenden", "Beschäftigten")
    title_h2 <- ifelse(grepl("25 Jahr", title_help), "Beschäftigten U25", title_h2)
    title_h2 <- ifelse(grepl("25 und 55", title_help), "Beschäftigten zwischen 25 und 55 Jahren", title_h2)
    title_h2 <- ifelse(grepl("über 55", title_help), "Beschäftigten Ü55", title_h2)
    title_h2 <- ifelse(grepl("jahr", title_help), "Auszubildenden im ersten Lehrjahr", title_h2)
    title_h2 <- ifelse(grepl("ausländischen Auszu", title_help), "ausländischen Auszubildenden", title_h2)
    title_h2 <- ifelse(grepl("ländischen B", title_help), "ausländischen Beschäftigten", title_h2)

    # plot

    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", bundesland, "</b><br>",
          "Anteil: ", display_rel, " %<br>",
          "Anzahl: ", wert
        )
      )
    titel <- paste0("Anteil von ",  title_help, " in MINT an allen ",  title_help, " (", timerange, ")")
    quelle <- "uelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    out <- mapbuilder_plotly(df,
                      value_col = "proportion",
                      regio_col = "bundesland",
                      titel = titel,
                      quelle = quelle)

  }
  else if(betrachtung == "Gruppenvergleich - Balkendiagramm" ){
    timerange <- r$zeit_beruf_mint_bula_balken
    indikator_choice <- r$indikator_beruf_mint_bula_balken
    darstellung <- r$abs_zahlen_arbeitsmarkt_einstieg_vergleich_123bula


    df_query <- glue::glue_sql("
    SELECT *
    FROM arbeitsmarkt_detail
    WHERE jahr = {timerange}
    AND indikator = {indikator_choice}
    AND landkreis = 'alle Landkreise'
    AND anforderung = 'Gesamt'
    AND geschlecht = 'Gesamt'
    AND fachbereich IN ('MINT', 'Alle')
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::select(`bundesland`, `jahr`, `indikator`, `fachbereich`, `wert`)

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

    df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")


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


    titel <- paste0( "Anteil von ", title_help, " in MINT an allen ", title_h2, " in ", timerange,"<br><br><br>")


    color <- c(
      "Deutschland" = "#b16fab",
      "Ostdeutschland (inkl. Berlin)" = "#d3a4d7",
      "Westdeutschland (o. Berlin)" = "#d3a4d7",
      "Baden-Württemberg" = "#A9A9A9",
      "Bayern" = "#A9A9A9",
      "Berlin" = "#A9A9A9",
      "Brandenburg" = "#A9A9A9",
      "Bremen" = "#A9A9A9",
      "Hamburg" = "#A9A9A9",
      "Hessen" = "#A9A9A9",
      "Mecklenburg-Vorpommern" = "#A9A9A9",
      "Niedersachsen" = "#A9A9A9",
      "Nordrhein-Westfalen" = "#A9A9A9",
      "Rheinland-Pfalz" = "#A9A9A9",
      "Saarland" = "#A9A9A9",
      "Sachsen" = "#A9A9A9",
      "Sachsen-Anhalt" = "#A9A9A9",
      "Schleswig-Holstein" = "#A9A9A9",
      "Thüringen" = "#A9A9A9"
    )


    quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    if (darstellung == "In Prozent"){

      order <- unique(df$bundesland)

      df <- df %>%
        dplyr::mutate(
          .tooltip = paste0(
            "<b><span style='font-size:15px;'>", bundesland, "</span></b><br>",
            "Anteil: ", round(prop, 1), " %<br>",
            "Anzahl: ", (formatC(as.numeric(wert), format = "f", digits = 0, big.mark = "."))
          ))


      x <- "bundesland"
      y <- "prop"
      quelle_y <- -0.15
      out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h",percent=TRUE, color=color,
                                  order=order, stacking=FALSE, quelle_y=quelle_y, quelle=quelle)



     }
    else
    {

      df$wert <- as.numeric(gsub("\\.", "", df$wert))

      order <- unique(df$bundesland)

      df <- df %>%
        dplyr::mutate(
          .tooltip = paste0(
            "<b><span style='font-size:15px;'>", bundesland, "</span></b><br>",
            "Anteil: ", round(prop, 1), " %<br>",
            "Anzahl: ", (formatC(as.numeric(wert), format = "f", digits = 0, big.mark = "."))
          ))


      x <- "bundesland"
      y <- "wert"
      quelle_y <- -0.15

      out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h",percent=FALSE, color=color,
                                  order=order, stacking=FALSE, quelle_y=quelle_y, quelle=quelle)

       }
  }

  else if(betrachtung == "Zeitverlauf - Liniendiagramm"){
    timerange <-r$zeit_beruf_mint_bula_verlauf
    t <- timerange[1]:timerange[2]
    aniveau <- r$indikator_beruf_mint_bula_verlauf
    states <- r$region_beruf_mint_bula_verlauf
    absolut_selector <- r$abs_beruf_mint_bula_verlauf

    df_query <- glue::glue_sql("
    SELECT indikator, fachbereich, bundesland, jahr, wert
    FROM arbeitsmarkt_detail
    WHERE jahr IN ({t*})
    AND landkreis = 'alle Landkreise'
    AND geschlecht ='Gesamt'
    AND anforderung = 'Gesamt'
    AND fachbereich IN ('Alle', 'MINT')
    AND bundesland IN ({states*})
    AND indikator = {aniveau}
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

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
    #Überschirft vorbereiten
    title_help <- paste0(aniveau, "n")
    title_help <- ifelse(grepl("ausländische Beschäftigte", aniveau), "ausländischen Beschäftigten", title_help)
    title_help <- ifelse(grepl("ausländische Auszubildende", aniveau), "ausländischen Auszubildenden", title_help)
    title_help <- ifelse(grepl("Jahr", aniveau), "Auszubildenden im ersten Lehrjahr", title_help)
    title_help <- ifelse(grepl("u25", aniveau), "Beschäftigten unter 25 Jahren", title_help)
    title_help <- ifelse(grepl("25-55", aniveau), "Beschäftigten zwischen 25 und 55 Jahren", title_help)
    title_help <- ifelse(grepl("ü55", aniveau), "Beschäftigten über 55 Jahren", title_help)

    if(absolut_selector=="In Prozent"){

      df <- df %>%
        dplyr::filter(selector=="In Prozent")

      sorted_indicators <- df %>%
        dplyr::group_by(bundesland) %>%
        dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
        dplyr::arrange(m_value) %>%
        dplyr::pull(bundesland)

      df$bundesland <- factor(df$bundesland, levels = sorted_indicators)

      df <- df[with(df, order(bundesland, jahr, decreasing = FALSE)), ]

      titel <- paste0("Anteil von ", title_help, " in MINT-Berufen an allen ", title_help)

      df <- df %>%
        dplyr::mutate(
          tooltip = paste0(
            "<b>", bundesland, "</b><br>",
            "Jahr: ", jahr, "<br>",
            "Anteil: ", prettyNum(wert, big.mark = ".", decimal.mark = ","), " %"
          )
        )

      color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                 "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")[1:length(unique(df$bundesland))]

      quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

      out <- linebuilder_plotly(df, titel, x = "jahr", y = "wert", group = "bundesland", color = color, quelle = quelle)



    } else if(absolut_selector=="Anzahl"){

      df <- df %>%
        dplyr::filter(selector=="Anzahl")

      df <- df[with(df, order(bundesland, jahr, decreasing = FALSE)), ]

      sorted_indicators <- df %>%
        dplyr::group_by(bundesland) %>%
        dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
        dplyr::arrange(m_value) %>%
        dplyr::pull(bundesland)

      df$bundesland <- factor(df$bundesland, levels = sorted_indicators)

      titel <- paste0("Anzahl von ", title_help, " in MINT-Berufen")

      df <- df %>%
        dplyr::mutate(
          tooltip = paste0(
            "<b>", bundesland, "</b><br>",
            "Jahr: ", jahr, "<br>",
            "Anzahl: ", prettyNum(wert, big.mark = ".", decimal.mark = ",")
          )
        )
      format <- ",d"
      color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                 "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")[1:length(unique(df$bundesland))]

      quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
      out <- linebuilder_plotly(df, titel, x = "jahr", y = "wert", group = "bundesland",
                                format = format, color = color, quelle = quelle)

    }
  }
 return(out)
}

### Nicht Box 1 ----

#'
#' #' A function to plot the german map ::::box 6
#' #'
#' #' @description A function to plot the german map with all states that contain
#' #' information about the share of women in STEM
#' #'
#' #' @return The return value is the german map with information
#' #' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' arbeitsmarkt_bl_gender <- function(r) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_arbeitsmarkt_bl_gender
#'
#'   if(timerange == 2021) indikator_choice <- r$level_arbeitsmarkt_bl_gender_21
#'   if(timerange == 2022) indikator_choice <- r$level_arbeitsmarkt_bl_gender_22
#'
#'   fachbereich_choice <- r$fach_arbeitsmarkt_bl_gender
#'
#'
#' #
#'   df_query <- glue::glue_sql("
#'   SELECT indikator, fachbereich, wert, geschlecht, bundesland, jahr
#'   FROM arbeitsmarkt_detail
#'   WHERE jahr IN ({timerange*})
#'   AND NOT bundesland IN ('Deutschland', 'Westdeutschland (o. Berlin)', 'Ostdeutschland (einschl. Berlin)')
#'   AND landkreis = 'alle Landkreise'
#'   AND NOT geschlecht = 'Gesamt'
#'   AND anforderung = 'Gesamt'
#'                                ", .con = con)
#'
#'   df <- DBI::dbGetQuery(con, df_query)
#'
#'   # df <- df %>%
#'   #   dplyr::select(indikator, fachbereich, wert, geschlecht, bundesland, jahr)
#'
#'
#'   # Berechnung von andere Fächergruppen
#'   df_andere <- df %>% dplyr::filter(fachbereich=="Alle")
#'   df_mint <- df %>% dplyr::filter(fachbereich=="MINT")
#'   df_andere$wert <- df_andere$wert - df_mint$wert
#'   df_andere$fachbereich[df_andere$fachbereich == "Alle"]<-"Andere Berufsgruppen"
#'
#'   df <- rbind(df, df_andere)
#'
#'   #nicht nötig, da Männer schon in df berechnet
#'   #df <- calc_arbeitsmarkt_males(df)
#'
#'   df <- df %>% dplyr::filter(indikator == indikator_choice)
#'
#'   df_gesamt <- df %>%
#'     dplyr::filter(fachbereich == "Alle")
#'   # ,
#'   # anforderung == "Gesamt")
#'
#'   df <- df %>%
#'     dplyr::left_join(df_gesamt, by = c("bundesland", "jahr", "geschlecht", "indikator")) %>%
#'     dplyr::rename(fachbereich = fachbereich.x,
#'                   wert = "wert.x",
#'                   wert_sum = "wert.y") %>%
#'     dplyr::select(-fachbereich.y) %>%
#'     dplyr::mutate(proportion = (wert/wert_sum)*100)%>%
#'     dplyr::filter(fachbereich == fachbereich_choice)
#'
#'   #Gerundetes Prop für Hover:
#'   df$prop <- round(df$proportion, 1)
#'
#'   #Trennpunkte für lange Zahlen ergänzen
#'   df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
#'
#'   values_female <- df %>% dplyr::filter(geschlecht == "Frauen")
#'   values_male <- df %>% dplyr::filter(geschlecht == "Männer")
#'
#'
#'   #Überschrift erstellen
#'   title_help <- paste0(indikator_choice, "r")
#'   title_help <- ifelse(grepl("ausländische Beschäftigte", indikator_choice), "ausländischer Beschäftigter", title_help)
#'   title_help <- ifelse(grepl("ausländische Auszubildende", indikator_choice), "ausländischer Auszubildender", title_help)
#'   title_help <- ifelse(grepl("Jahr", indikator_choice), "Auszubildender mit neuem Lehrvertrag", title_help)
#'
#'   titel_w <- ifelse(fachbereich_choice == "Andere Berufsgruppen", paste0("Anteil weiblicher ", title_help, ", die kein MINT-Berufsfeld wählen (", timerange, ")"),
#'                     paste0("Anteil weiblicher ", title_help, ", die das Berufsfeld ", fachbereich_choice, " wählen (", timerange, ")"))
#'   titel_m <- ifelse(fachbereich_choice == "Andere Berufsgruppen", paste0("Anteil männlicher ", title_help, ", die kein MINT-Berufsfeld wählen (", timerange, ")"),
#'                     paste0("Anteil männlicher ", title_help, ", die das Berufsfeld ", fachbereich_choice, " wählen (", timerange, ")"))
#'
#'
#'
#'     # plot
#'
#'     df <-values_female
#'     joinby <- c("name", "bundesland")
#'     name <- paste0(fachbereich_choice)
#'     tooltip <- "{point.bundesland} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}"
#'     titel <-titel_w
#'     mincolor <- "#f4f5f6"
#'     maxcolor <- "#b16fab"
#'     map_selection <- 1
#'     out1 <- mapbuilder(df, joinby,name, tooltip, titel, mincolor, maxcolor,prop=FALSE, wert=FALSE, map=map_selection)
#'
#'
#'
#'     df <-values_male
#'     joinby <- c("name", "bundesland")
#'     name <- paste0(fachbereich_choice)
#'     tooltip <- "{point.bundesland} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}"
#'     titel <-titel_m
#'     mincolor <- "#f4f5f6"
#'     maxcolor <- "#b16fab"
#'     map_selection <- 1
#'     out2 <- mapbuilder(df, joinby,name, tooltip, titel, mincolor, maxcolor,prop=FALSE, wert=FALSE, map=map_selection)
#'
#'
#'     out <- list(out_1, out_2)
#'
#'     return (out)
#'
#'
#' }
#'
#'
#' #' A function to plot time series
#' #'
#' #' @description A function to plot the time series
#' #'
#' #' @return The return value, if any, from executing the function.
#' #' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' arbeitsmarkt_bl_gender_verlauf <- function(r) {
#'
#'   # load UI inputs from reactive value
#'
#'   absolut_selector <- r$abs_zahlen_beruf_arbeitsmarkt_bl_gender_verlauf
#'   timerange <- r$date_beruf_arbeitsmarkt_bl_gender_verlauf
#'   indikator_choice <- r$indikator_beruf_arbeitsmarkt_bl_gender_verlauf
#'   states <- r$states_beruf_arbeitsmarkt_bl_gender_verlauf
#'   t <- as.character(timerange[1]:timerange[2])
#' #
#' #
#'
#'
#'   df_query <- glue::glue_sql("
#'   SELECT *
#'   FROM arbeitsmarkt
#'   WHERE jahr IN ({t*})
#'   AND indikator = {indikator_choice}
#'   AND region IN {states}
#'   AND anforderung = 'Gesamt'
#'   AND geschlecht = 'Frauen'
#'   AND fachbereich IN c('Alle', 'MINT')
#'                                ", .con = con)
#'
#'   df <- DBI::dbGetQuery(con, df_query)
#'
#'   df <- df %>%
#'     dplyr::select("bereich",
#'                   "indikator",
#'                   "fachbereich",
#'                   "geschlecht",
#'                   "region",
#'                   "jahr",
#'                   "anforderung",
#'                   "wert" )
#'
#'
#'   df <- df %>% dplyr::filter(anforderung != "Keine Zuordnung möglich")
#'
#'   df_gesamt <- df %>%
#'     dplyr::filter(fachbereich == "Alle",
#'                   anforderung == "Gesamt")
#'
#'
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
#'                   fachbereich == "MINT")%>%
#'     dplyr::select(-wert_sum)%>%
#'     dplyr::rename(Relativ = proportion, Absolut=wert)%>%
#'     tidyr::pivot_longer(c(Absolut, Relativ), names_to = "selector", values_to = "wert")%>%
#'     dplyr::mutate(selector = dplyr::case_when(
#'       selector == "Relativ" ~ "In Prozent",
#'       selector == "Absolut" ~ "Anzahl"
#'     ))
#'
#'
#'   df <- df %>% dplyr::filter(fachbereich == "MINT")
#'
#'   if(absolut_selector=="In Prozent"){
#'
#'     df <- df %>%
#'       dplyr::filter(selector =="In Prozent")
#'
#'     # order years for plot
#'     df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
#'
#'     title_help <- paste0(indikator_choice, "r")
#'
#'     titel <- paste0("Anteil weiblicher ", title_help, ", die MINT-Berufe wählen")
#'     tooltip <-"{point.region} <br> Anteil: {point.y} %"
#'     format <- "{value}%"
#'     color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
#'                "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
#'     out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color)
#'
#'
#'   }else if(absolut_selector=="Anzahl"){
#'
#'     title_help <- paste0(indikator_choice, "r")
#'
#'     hcoptslang <- getOption("highcharter.lang")
#'     hcoptslang$thousandsSep <- "."
#'     options(highcharter.lang = hcoptslang)
#'
#'     df <- df %>%
#'       dplyr::filter(selector == "Anzahl")
#'
#'     df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
#'
#'
#'     # plot
#'     titel <- paste0("Anzahl weiblicher ", title_help, ", die MINT-Berufe wählen")
#'     tooltip <- "Anzahl: {point.y}"
#'     format <-  "{value:, f}"
#'     color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
#'                "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
#'     out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color)
#'
#'   }
#'
#'
#'
#'
#' }
#'
#'
#' #' A function to plot time series
#' #'
#' #' @description A function to plot the time series
#' #'
#' #' @return The return value, if any, from executing the function.
#' #' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' arbeitsmarkt_bl_verlauf <- function(r) {
#'
#'
#'   absolut_selector <- r$abs_zahlen_4
#'   aniveau <- r$niveau
#'   # load UI inputs from reactive value
#'   timerange <- r$date_beruf_arbeitsmarkt_bl_verlauf
#'   t <- as.character(timerange[1]:timerange[2])
#'   states <- r$states_beruf_arbeitsmarkt_bl_verlauf
#'
#'   # filter dataset based on UI inputs
#'
#'
#'
#'   df_query <- glue::glue_sql("
#'   SELECT *
#'   FROM arbeitsmarkt
#'   WHERE jahr IN ({t*})
#'   AND geschlecht = 'Gesamt'
#'   AND anforderung = 'Gesamt'
#'   AND fachbereich IN ('Alle', 'MINT')
#'   AND region IN ({states*})
#'   AND indikator = {aniveau}
#'
#'                                ", .con = con)
#'
#'   df <- DBI::dbGetQuery(con, df_query)
#'
#'   df <- df %>%
#'     dplyr::select(
#'       "indikator",
#'       "fachbereich",
#'       #"geschlecht",
#'       "region",
#'       "jahr",
#'       #"anforderung",
#'       "wert" )
#'
#'
#'   df <- df%>%
#'     tidyr::pivot_wider(values_from=wert, names_from=fachbereich)%>%
#'     dplyr::mutate(MINT_p= round(MINT/Alle*100,1))%>%
#'     dplyr::select(- "Alle")%>%
#'     dplyr::rename(Absolut = MINT, Relativ = MINT_p)%>%
#'     tidyr::pivot_longer(c(Absolut , Relativ), names_to="selector", values_to="wert")%>%
#'     dplyr::mutate(selector = dplyr::case_when(
#'       selector=="Relativ" ~ "In Prozent",
#'       selector=="Absolut" ~ "Anzahl"
#'     ))
#'
#'
#'   # Hilfe für Titel
#'   title_help <- paste0(aniveau, "n")
#'
#'   if(absolut_selector=="In Prozent"){
#'
#'     df <- df %>%
#'       dplyr::filter(selector=="In Prozent")
#'
#'
#'
#'     df$display_rel <- prettyNum(round(df$wert,1), big.mark = ".", decimal.mark = ",")
#'
#'
#'     df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
#'
#'     # plot
#'
#'     titel <- paste0("Anteil von ", title_help, " in MINT-Berufen an allen ", title_help)
#'     tooltip <- "Anteil <br> Bundesland: {point.region} <br> Wert: {point.display_rel} %"
#'     format <- "{value}%"
#'     color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
#'                "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
#'     out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color)
#'
#'
#'
#'   } else if(absolut_selector=="Anzahl"){
#'
#'
#'     hcoptslang <- getOption("highcharter.lang")
#'     hcoptslang$thousandsSep <- "."
#'     options(highcharter.lang = hcoptslang)
#'
#'     df <- df %>%
#'       dplyr::filter(selector=="Anzahl")
#'
#'     df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
#'
#'     df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
#'
#'
#'     # plot
#'
#'     titel <- paste0("Anzahl von ", title_help, " in MINT-Berufen")
#'     tooltip <- "Anzahl: {point.display_abs}"
#'     format <-  "{value:, f}"
#'     color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
#'                "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
#'     out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color)
#'
#'
#'   }
#'
#' }

# M-I-N-T ----
### Tab 1 ----

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
  bereich_balken <- c(
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

      df_query <- glue::glue_sql("
      SELECT indikator, jahr, bundesland, fachbereich, wert
      FROM arbeitsmarkt_detail
      WHERE jahr IN ({timerange*})
      AND landkreis = 'alle Landkreise'
      AND bundesland = {regio}
      AND geschlecht = 'Gesamt'
      AND anforderung = 'Gesamt'
      AND fachbereich IN ('Mathematik, Naturwissenschaften', 'Informatik', 'Technik (gesamt)')
      AND indikator IN ({indikator_choice*})
                               ", .con = con)

      df <- DBI::dbGetQuery(con, df_query)

      df <- df %>%
        dplyr::group_by(indikator) %>%
        dplyr::mutate(prop = sum(wert))

      df <- df %>% dplyr::group_by(fachbereich, indikator) %>%
        dplyr::mutate(prop = round(wert/prop*100,1))

      preposition <- ifelse(grepl("aarland$", regio), "im", "in")

      df$wert_disp <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
      df$prop_disp <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")
    }
    else{

      df_query <- glue::glue_sql("
      SELECT indikator, jahr, bundesland, fachbereich, wert
      FROM arbeitsmarkt_detail
      WHERE jahr IN ({timerange*})
      AND landkreis = 'alle Landkreise'
      AND bundesland = {regio}
      AND geschlecht = 'Gesamt'
      AND anforderung = 'Gesamt'
      AND fachbereich IN ('Alle', 'MINT', 'Mathematik, Naturwissenschaften', 'Informatik', 'Technik (gesamt)')
      AND indikator IN ({indikator_choice*})
                               ", .con = con)

      df <- DBI::dbGetQuery(con, df_query)

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


      preposition <- ifelse(grepl("aarland$", regio), " im ", " in ")

      df$wert_disp <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
      df$prop_disp <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")
    }

    color <- color_fachbereich

    if(length(indikator_choice) == 1) {

      df <- df[with(df, order(prop, decreasing = FALSE)), ]

      titel <- paste0("MINT-Anteile von ", indikator_choice, preposition, " ", regio, " (", timerange, ")")
      df <- df %>%
        dplyr::mutate(
          tooltip = paste0(
            "<b>", fachbereich, "</b><br>",
            "Anteil: ", prop_disp, " %<br>",
            "Anzahl: ", wert_disp
          )
        )
     # color <- as.character(df$color)

      quelle <- "Quelle: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

      out <- piebuilder_plotly(df, titel, x="fachbereich", y = "prop",
                        color=color_fachbereich, quelle = quelle)

    } else if(length(indikator_choice) == 2) {

      df <- df %>%
        dplyr::mutate(
          tooltip = paste0(
            "<b>", fachbereich, "</b><br>",
            "Anteil: ", prop_disp, " %<br>",
            "Anzahl: ", wert_disp
          )
        )
      df_1 <- df %>% dplyr::filter(indikator == indikator_choice[1])
      df_1 <- df_1[with(df_1, order(prop, decreasing = FALSE)), ]
      df_1 <- df_1 %>%
        dplyr::mutate(color = color_fachbereich[fachbereich])

      df_2 <- df %>% dplyr::filter(indikator == indikator_choice[2])
      df_2 <- df_2[with(df_2, order(prop, decreasing = FALSE)), ]
      df_2 <- df_2 %>%
        dplyr::mutate(color = color_fachbereich[fachbereich])

      titel1 <- paste0("MINT-Anteile von ",indikator_choice[1], preposition, " ", regio, " (", timerange, ")")
      titel2 <- paste0("MINT-Anteile von ", indikator_choice[2], preposition, " ", regio, " (", timerange, ")")

      color1 <- as.character(df_1$color)
      color2 <- as.character(df_2$color)

      quelle <- "Quelle: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

      out_1 <- piebuilder_plotly(df_1, titel1, x="fachbereich", y = "prop", color = color1, legend_y=0.01,
                          quelle= quelle)|>
        plotly::layout(
          annotations = list(
            list(
              text = quelle,
              x = 1,
              y = -0.5,
              xref = "paper",
              yref = "paper",
              xanchor = "right",
              yanchor = "top",
              showarrow = FALSE,
              font = list(size = 11, color = "gray", family = "Calibri Regular", align = "right")
            )
          ),
          margin = list(t = 90, b = 130, r = 50, l = 50)
        )

      out_2 <- piebuilder_plotly(df_2, titel2, x="fachbereich", y = "prop", legend_y=0.01,
                          color = color2, quelle = quelle)|>
        plotly::layout(
          annotations = list(
            list(
              text = quelle,
              x = 1,
              y = -0.5,
              xref = "paper",
              yref = "paper",
              xanchor = "right",
              yanchor = "top",
              showarrow = FALSE,
              font = list(size = 11, color = "gray", family = "Calibri Regular", align = "right")
            )
          ),
          margin = list(t = 90, b = 130, r = 50, l = 50)
        )

      out <- list( out_1, out_2 )
    }

  }else
    if (betrachtung == "Gruppenvergleich - Balkendiagramm"){

    indikator_choice <- r$indikator_arbeitsmarkt_fach_vergleich_balken
    darstellung <- r$abs_zahlen_arbeitsmarkt_einstieg_vergleich12_1

    praep <- ifelse(regio == "Saarland", " im ", " in ")


    df_query <- glue::glue_sql("
    SELECT *
    FROM arbeitsmarkt_detail
    WHERE jahr = {timerange}
    AND indikator = {indikator_choice}
    AND landkreis = 'alle Landkreise'
    AND anforderung = 'Gesamt'
    AND geschlecht = 'Gesamt'
    AND bundesland = {regio}
    AND fachbereich IN ('Alle', 'MINT', 'Mathematik, Naturwissenschaften', 'Informatik', 'Technik (gesamt)')
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)

    df_andere <- df %>% dplyr::filter(fachbereich=="Alle")
    df_mint <- df %>% dplyr::filter(fachbereich=="MINT")
    df_andere$wert <- df_andere$wert - df_mint$wert
    df_andere$fachbereich[df_andere$fachbereich == "Alle"]<-"Alle Berufsfelder außer MINT"
    df <- rbind(df, df_andere)
    df <- df %>% dplyr::filter(fachbereich != "Alle")


    df_query <- glue::glue_sql("
    SELECT *
    FROM arbeitsmarkt_detail
    WHERE jahr = {timerange}
    AND indikator = {indikator_choice}
    AND landkreis = 'alle Landkreise'
    AND anforderung = 'Gesamt'
    AND geschlecht = 'Gesamt'
    AND bundesland = {regio}
    AND fachbereich = 'Alle'
                               ", .con = con)

    df_alle <- DBI::dbGetQuery(con, df_query)


    df_alle <- df_alle %>%
      dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)


    df <- df %>%
      dplyr::left_join(df_alle,
                       dplyr::join_by("bundesland", "jahr", "geschlecht", "indikator")) %>%
      dplyr::select(-fachbereich.y) %>%
      dplyr::rename(fachbereich = fachbereich.x,
                    wert = wert.x,
                    wert_ges = wert.y) %>%
      dplyr::mutate(prop = round(wert/wert_ges * 100,1))

    #Trennpunkte für lange Zahlen ergänzen
    #df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")

    #für Überblick unterarten von Technik wieder raus
    df <- df %>% dplyr::filter(fachbereich %in% c("Alle Berufsfelder außer MINT",
                                                  "Mathematik, Naturwissenschaften",
                                                  "Informatik",
                                                  "Technik (gesamt)"))

    df <- df[with(df, order(prop, decreasing = TRUE)), ]
    df <- df %>%
      dplyr::mutate(color = bereich_balken[fachbereich])

    # titel-helper
    title_help <- paste0(indikator_choice, "n")
    title_help <- ifelse(grepl("ausländische Beschäftigte", indikator_choice), "ausländischen Beschäftigten", title_help)
    title_help <- ifelse(grepl("ausländische Auszubildende", indikator_choice), "ausländischen Auszubildenden", title_help)
    title_help <- ifelse(grepl("Jahr", indikator_choice), "Auszubildenden mit neuem Lehrvertrag", title_help)
    title_help <- ifelse(grepl("u25", indikator_choice), "Beschäftigten unter 25 Jahren", title_help)
    title_help <- ifelse(grepl("25-55", indikator_choice), "Beschäftigten zwischen 25 und 55 Jahren", title_help)
    title_help <- ifelse(grepl("ü55", indikator_choice), "Beschäftigten über 55 Jahren", title_help)



    titel <- paste0( "Überblick über die Berufsfelder von ", title_help, "<br>", praep, regio, " (", timerange, ")")

    color <- bereich_balken

    quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."


    if(darstellung == "In Prozent"){

      order <- unique(df$fachbereich)

      df <- df %>%
        dplyr::mutate(
          .tooltip = paste0(
            "<b><span style='font-size:15px;'>", fachbereich, "</span></b><br>",
            "<span style='font-size:15px;'> Anteil an allen Berufsfeldern: </span>", prop, "%<br>",
            "Anzahl: ", (formatC(as.numeric(wert),format = "f",digits = 0,big.mark = "."))
        ))



      x <- "fachbereich"
      y <- "prop"
      quelle_y <- -0.17

      out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h", group=NULL, color = color,
                                  order = order, percent=TRUE, stacking=FALSE, quelle=quelle, quelle_y=quelle_y)


 } else {

      df$wert <- as.numeric(gsub("\\.", "", df$wert))

      order <- unique(df$fachbereich)

      df <- df %>%
        dplyr::mutate(
          .tooltip = paste0(
            "<b><span style='font-size:15px;'>", fachbereich, "</span></b><br>",
            "<span style='font-size:15px;'> Anteil an allen Berufsfeldern: </span>", prop, "%<br>",
            "Anzahl: ", (formatC(as.numeric(wert),format = "f",digits = 0,big.mark = "."))
          ))


      x <- "fachbereich"
      y <- "wert"
      quelle_y <- -0.17


      out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h", group=NULL, color = color,
                                  order = order, percent=FALSE, stacking=FALSE, quelle=quelle, quelle_y=quelle_y )


     }
  }

  return(out)
}

### Tab 2 ----

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

  df_query <- glue::glue_sql("
  SELECT *
  FROM arbeitsmarkt_detail
  WHERE jahr IN ({t*})
  AND indikator = {indi}
  AND landkreis = 'alle Landkreise'
  AND anforderung = 'Gesamt'
  AND geschlecht = 'Gesamt'
  AND bundesland = {regio}
  AND fachbereich IN ('Mathematik, Naturwissenschaften', 'Informatik', 'Technik (gesamt)')
                               ", .con = con)


  df <- DBI::dbGetQuery(con, df_query)

  df <- df %>%
    dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)

  if (absolut_selector == "In Prozent"){



    df_query <- glue::glue_sql("
  SELECT *
  FROM arbeitsmarkt_detail
  WHERE jahr IN ({t*})
  AND indikator = {indi}
  AND landkreis = 'alle Landkreise'
  AND anforderung = 'Gesamt'
  AND geschlecht = 'Gesamt'
  AND bundesland = {regio}
  AND fachbereich = 'Alle'
                               ", .con = con)

    df_alle <- DBI::dbGetQuery(con, df_query)

    df_alle <- df_alle %>%
      dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)

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

    sorted_indicators <- df %>%
      dplyr::group_by(fachbereich) %>%
      dplyr::summarize(m_value = mean(round(prop, 1), na.rm = TRUE)) %>%
      dplyr::arrange(m_value) %>%
      dplyr::pull(fachbereich)

    df$fachbereich <- factor(df$fachbereich, levels = sorted_indicators)
    colors <- color_fachbereich[sorted_indicators]

    # titel-helper
    title_help <- paste0(indi, "n")
    title_help <- ifelse(grepl("Jahr", indi), "Auszubildenden mit neuem Lehrvertrag", title_help)
    title_help <- ifelse(grepl("ausländische Auszubildende", indi), "ausländischen Auszubildenden", title_help)
    title_help <- ifelse(grepl("ausländische Beschäftigte", indi), "ausländischen Beschäftigten", title_help)
    title_help <- ifelse(grepl("Beschäftigte 25-55", indi), "Beschäftigten zwischen 25 und 55 Jahren", title_help)
    title_help <- ifelse(grepl("Beschäftigte ü55", indi), "Beschäftigten über 55 Jahren", title_help)
    title_help <- ifelse(grepl("Beschäftigte u25", indi), "Beschäftigten unter 25 Jahren", title_help)

    # plot

    titel <- ist_saarland(gruppe="Entwicklung des MINT-Anteils unter ", optional = title_help ,regio, timerange=0)
    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", fachbereich, "</b><br>",
          "Jahr: ", jahr, "<br>",
          "Anteil: ", prettyNum(prop, big.mark = ".", decimal.mark = ","), " %"
        )
      )
    color <- as.character(colors)
    quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    out <- linebuilder_plotly(df, titel, x = "jahr", y = "prop", group = "fachbereich", color = color, quelle = quelle)

  } else if(absolut_selector == "Anzahl") {

    # order years for plot and create labels
    df <- df[with(df, order(jahr, decreasing = FALSE)), ]

    sorted_indicators <- df %>%
      dplyr::group_by(fachbereich) %>%
      dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
      dplyr::arrange(m_value) %>%
      dplyr::pull(fachbereich)

    df$fachbereich <- factor(df$fachbereich, levels = sorted_indicators)

    colors <- color_fachbereich[sorted_indicators]
    #titlehelper
    title_help <- paste0(indi, "n")
    title_help <- ifelse(grepl("Jahr", indi), "Auszubildenden mit neuem Lehrvertrag", title_help)
    title_help <- ifelse(grepl("ausländische Auszubildende", indi), "ausländischen Auszubildenden", title_help)
    title_help <- ifelse(grepl("ausländische Beschäftigte", indi), "ausländischen Beschäftigten", title_help)
    title_help <- ifelse(grepl("Beschäftigte 25-55", indi), "Beschäftigten zwischen 25 und 55 Jahren", title_help)
    title_help <- ifelse(grepl("Beschäftigte ü55", indi), "Beschäftigten über 55 Jahren", title_help)
    title_help <- ifelse(grepl("Beschäftigte u25", indi), "Beschäftigten unter 25 Jahren", title_help)


    # plot

    titel <- ist_saarland2(optional1="Entwicklung der Anzahl der ", title_help, optional2=" in MINT", regio)
    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", fachbereich, "</b><br>",
          "Jahr: ", jahr, "<br>",
          "Anzahl: ", prettyNum(wert, big.mark = ".", decimal.mark = ",")
        )
      )

    format <- ",d"
    color <- as.character(colors)
    quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    out <- linebuilder_plotly(df, titel, x = "jahr", y = "wert", group = "fachbereich", format = format, color = color, quelle = quelle)


  }

  return(out)
}

### Tab 3 ----

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


    df_query <- glue::glue_sql("
    SELECT *
    FROM arbeitsmarkt_detail
    WHERE jahr = {timerange}
    AND indikator = {indi}
    AND landkreis = 'alle Landkreise'
    AND anforderung = 'Gesamt'
    AND geschlecht = 'Gesamt'
    AND fachbereich IN ({faecher}, 'Alle')
    AND NOT bundesland IN ('Deutschland', 'Westdeutschland (o. Berlin)', 'Ostdeutschland (inkl. Berlin)')
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)



    df <- df %>%
      dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)

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
    title_help <- paste0(indi, "n")
    title_help <- ifelse(grepl("Jahr", indi), "Auszubildenden mit neuem Lehrvertrag", title_help)
    title_help <- ifelse(grepl("ausländische Auszubildende", indi), "ausländischen Auszubildenden", title_help)
    title_help <- ifelse(grepl("ausländische Beschäftigte", indi), "ausländischen Beschäftigten", title_help)
    title_help <- ifelse(grepl("Beschäftigte 25-55", indi), "Beschäftigten zwischen 25 und 55 Jahren", title_help)
    title_help <- ifelse(grepl("Beschäftigte ü55", indi), "Beschäftigten über 55 Jahren", title_help)
    title_help <- ifelse(grepl("Beschäftigte u25", indi), "Beschäftigten unter 25 Jahren", title_help)

    # plot

    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", bundesland, "</b><br>",
          "Anteil: ", display_rel, " %<br>",
          "Anzahl: ", wert
        )
      )
    titel <- paste0("Anteil von ",  title_help, " in ", faecher, " an allen ",  title_help, " (", timerange, ")")
    quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    out <- mapbuilder_plotly(df,
                      value_col = "proportion",
                      regio_col = "bundesland",
                      titel = titel,
                      quelle = quelle)



  }
  else if(betrachtung == "Gruppenvergleich - Balkendiagramm" ){
    timerange <- r$zeit_beruf_faecher_bula_balken
    indikator_choice <- r$indikator_beruf_faecher_bula_balken
    faecher <- r$fachbereich_beruf_faecher_bula_balken
    darstellung12 <- r$abs_zahlen_arbeitsmarkt_einstieg_vergleich_123bula123


    df_query <- glue::glue_sql("
    SELECT *
    FROM arbeitsmarkt_detail
    WHERE jahr = {timerange}
    AND indikator = {indikator_choice}
    AND landkreis = 'alle Landkreise'
    AND anforderung = 'Gesamt'
    and geschlecht = 'Gesamt'
    AND fachbereich = {faecher}
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::select(`bundesland`, `jahr`, `indikator`, `fachbereich`, `wert`)


    # Alle als extra Spalte anhängen und Anteil berechnen
    df_query <- glue::glue_sql("
    SELECT *
    FROM arbeitsmarkt_detail
    WHERE jahr = {timerange}
    AND indikator = {indikator_choice}
    AND landkreis = 'alle Landkreise'
    AND anforderung = 'Gesamt'
    AND geschlecht = 'Gesamt'
    AND fachbereich = 'Alle'
                               ", .con = con)

    df_ges <- DBI::dbGetQuery(con, df_query)

    df_ges <- df_ges %>%
      dplyr::select(`bundesland`, `jahr`, `indikator`, `fachbereich`, `wert`)%>%
      dplyr::rename(wert_ges = wert)


    df <- df %>%
      dplyr::left_join(df_ges, by = c("indikator", "jahr", "bundesland")) %>%
      dplyr::rename(fachbereich = "fachbereich.x")%>%
      dplyr::ungroup()%>%
      dplyr::select(-c("fachbereich.y")) %>%
      dplyr::mutate(prop = (wert/wert_ges)*100)%>%
      dplyr::mutate(prop = round(prop,1))

    #Trennpunkte für lange Zahlen in absolutem Wert ergänzen
    #df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")
    df <- df[with(df, order(prop, decreasing = T)),]



    # titel-helper
    title_help <- paste0(indikator_choice, "n")
    title_help <- ifelse(grepl("ausländische Beschäftigte", indikator_choice), "ausländischen Beschäftigten", title_help)
    title_help <- ifelse(grepl("ausländische Auszubildende", indikator_choice), "ausländischen Auszubildenden", title_help)
    title_help <- ifelse(grepl("Jahr", indikator_choice), "Auszubildenden im ersten Lehrjahr", title_help)
    title_help <- ifelse(grepl("u25", indikator_choice), "Beschäftigten unter 25 Jahren", title_help)
    title_help <- ifelse(grepl("25-55", indikator_choice), "Beschäftigten zwischen 25 und 55 Jahren", title_help)
    title_help <- ifelse(grepl("ü55", indikator_choice), "Beschäftigten über 55 Jahren", title_help)



    titel <- paste0("Anteil von ", title_help, " im Berufsfeld ", faecher, " an allen ", title_help, " in ", timerange)

    quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    color <- c(
      "Deutschland" = "#b16fab",
      "Ostdeutschland (inkl. Berlin)" = "#d3a4d7",
      "Westdeutschland (o. Berlin)" = "#d3a4d7",
      "Baden-Württemberg" = "#A9A9A9",
      "Bayern" = "#A9A9A9",
      "Berlin" = "#A9A9A9",
      "Brandenburg" = "#A9A9A9",
      "Bremen" = "#A9A9A9",
      "Hamburg" = "#A9A9A9",
      "Hessen" = "#A9A9A9",
      "Mecklenburg-Vorpommern" = "#A9A9A9",
      "Niedersachsen" = "#A9A9A9",
      "Nordrhein-Westfalen" = "#A9A9A9",
      "Rheinland-Pfalz" = "#A9A9A9",
      "Saarland" = "#A9A9A9",
      "Sachsen" = "#A9A9A9",
      "Sachsen-Anhalt" = "#A9A9A9",
      "Schleswig-Holstein" = "#A9A9A9",
      "Thüringen" = "#A9A9A9"
    )


    if(darstellung12 == "In Prozent"){



        order <- unique(df$bundesland)

        df <- df %>%
          dplyr::mutate(
            .tooltip = paste0(
              "<b><span style='font-size:15px;'>", bundesland, "</span></b><br>",
              "Anteil: ", round(prop, 1), " %<br>",
              "Anzahl: ", (formatC(as.numeric(wert), format = "f", digits = 0, big.mark = "."))
            ))


        x <- "bundesland"
        y <- "prop"
        quelle_y <- -0.15
        out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h",percent=TRUE, color=color,
                                    order=order, stacking=FALSE, quelle_y=quelle_y,margin_t=80, quelle=quelle)



      }
      else
      {

        df$wert <- as.numeric(gsub("\\.", "", df$wert))

        order <- unique(df$bundesland)

        df <- df %>%
          dplyr::mutate(
            .tooltip = paste0(
              "<b><span style='font-size:15px;'>", bundesland, "</span></b><br>",
              "Anteil: ", round(prop, 1), " %<br>",
              "Anzahl: ", (formatC(as.numeric(wert), format = "f", digits = 0, big.mark = "."))
            ))


        x <- "bundesland"
        y <- "wert"
        quelle_y <- -0.15

        out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h",percent=FALSE, color=color,
                                    order=order, stacking=FALSE, quelle_y=quelle_y, margin_t=80, quelle=quelle)

      }



  }
  else if(betrachtung == "Zeitverlauf - Liniendiagramm"){
    timerange <-r$zeit_beruf_faecher_bula_verlauf
    t <- timerange[1]:timerange[2]
    indi <- r$indikator_beruf_faecher_bula_verlauf
    states <- r$region_beruf_faecher_bula_verlauf
    absolut_selector <- r$abs_beruf_faecher_bula_verlauf
    faecher <- r$fachbereich_beruf_faecher_bula_verlauf

    df_query <- glue::glue_sql("

    SELECT *
    FROM arbeitsmarkt_detail
    WHERE jahr IN ({t*})
    AND indikator = {indi}
    AND landkreis = 'alle Landkreise'
    AND bundesland IN ({states*})
    AND anforderung = 'Gesamt'
    AND geschlecht = 'Gesamt'
    AND fachbereich = {faecher}
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)


    df <- df %>%
      dplyr::select(`bundesland`, `jahr`, `indikator`, `fachbereich`, `wert`)

    # Hilfe für Titel
    title_help <- paste0(indi, "n")
    title_help <- ifelse(grepl("ausländische Beschäftigte", indi), "ausländischen Beschäftigten", title_help)
    title_help <- ifelse(grepl("ausländische Auszubildende", indi), "ausländischen Auszubildenden", title_help)
    title_help <- ifelse(grepl("Jahr", indi), "Auszubildenden im ersten Lehrjahr", title_help)
    title_help <- ifelse(grepl("u25", indi), "Beschäftigten unter 25 Jahren", title_help)
    title_help <- ifelse(grepl("25-55", indi), "Beschäftigten zwischen 25 und 55 Jahren", title_help)
    title_help <- ifelse(grepl("ü55", indi), "Beschäftigten über 55 Jahren", title_help)

    if(absolut_selector=="In Prozent"){
#
#       # Alle
      df_query <- glue::glue_sql("
      SELECT *
      FROM arbeitsmarkt_detail
      WHERE jahr IN ({t*})
      AND indikator = {indi}
      AND landkreis = 'alle Landkreise'
      AND bundesland IN ({states*})
      AND anforderung = 'Gesamt'
      AND geschlecht = 'Gesamt'
      AND fachbereich = 'Alle'
                               ", .con = con)

      df_ges <- DBI::dbGetQuery(con, df_query)

      df_ges <- df_ges %>%
        dplyr::select(`bundesland`, `jahr`, `indikator`, `fachbereich`, `wert`)%>%
        dplyr::rename(wert_ges = wert)

      df <- df %>%
        dplyr::left_join(df_ges, by = c("indikator", "jahr", "bundesland")) %>%
        dplyr::rename(fachbereich = "fachbereich.x")%>%
        dplyr::ungroup()%>%
        dplyr::select(-c("fachbereich.y")) %>%
        dplyr::mutate(prop = (wert/wert_ges)*100)%>%
        dplyr::mutate(prop = round(prop,1))

      df <- df[with(df, order(bundesland, jahr, decreasing = FALSE)), ]


      # plot

      titel <-  paste0("Anteil von ", title_help, " im Berufsfeld ", faecher, " im Berufsfeld ")
      df <- df %>%
        dplyr::mutate(
          tooltip = paste0(
            "<b>", bundesland, "</b><br>",
            "Jahr: ", jahr, "<br>",
            "Anteil: ", prettyNum(prop, big.mark = ".", decimal.mark = ","), " %"
          )
        )

      color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                 "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")[1:length(unique(df$bundesland))]
      quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
      out <- linebuilder_plotly(df, titel, x = "jahr", y = "prop", group = "bundesland", color = color, quelle = quelle)




    } else if(absolut_selector=="Anzahl"){


      df <- df[with(df, order(bundesland, jahr, decreasing = FALSE)), ]

      # plot

      titel <-  paste0("Anzahl von ", title_help, " in MINT-Berufen im Berufsfeld ", faecher)
      df <- df %>%
        dplyr::mutate(
          tooltip = paste0(
            "<b>", bundesland, "</b><br>",
            "Jahr: ", jahr, "<br>",
            "Anzahl: ", prettyNum(wert, big.mark = ".", decimal.mark = ",")
          )
        )
      format <- ",d"
      color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                 "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")[1:length(unique(df$bundesland))]
      quelle2 <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
      out <- linebuilder_plotly(df, titel, x = "jahr", y = "wert", group = "bundesland", format = format, color = color, quelle = quelle2)


    }
  }
  return(out)
}

### Nicht Box 2 ----

#' A function to plot a single bundesland with landkreise
#'
#' @description A function to plot a map
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Arbeitsmarkt_detailliert.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

#
# arbeitsmarkt_überblick_fächer <- function( r) {
#   # load UI inputs from reactive value
#
#   timerange <- r$date_arbeitsmarkt_überblick_fächer
#   state <- r$state_arbeitsmarkt_überblick_fächer
#
#   if(timerange == 2021) indikator_choice <- r$indikator_arbeitsmarkt_überblick_fächer_21
#   if(timerange == 2022) indikator_choice <- r$indikator_arbeitsmarkt_überblick_fächer_22
#
#   df_query <- glue::glue_sql("
#   SELECT *
#   FROM arbeitsmarkt_detail
#   WHERE jahr = {timerange}
#   AND indikator = {indikator_choice}
#   AND landkreis = 'alle Landkreise'
#   AND anforderung = 'Gesamt'
#   AND geschlecht = 'Gesamt'
#   AND bundesland = {state}
#                                ", .con = con)
#
#   df <- DBI::dbGetQuery(con, df_query)
#
#   df <- df %>%
#     dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)
#
#
#   # MINT direkt berechnen und nicht-MINT berechnen
#   df[df$fachbereich == "MINT", "wert"] <- df[df$fachbereich == "Mathematik, Naturwissenschaften", "wert"]+
#     df[df$fachbereich == "Informatik", "wert"]+df[df$fachbereich == "Technik (gesamt)", "wert"]
#   df$fachbereich[df$fachbereich == "MINT"]<-"MINT-Berufsfelder (gesamt)"
#
#   df_andere <- df %>% dplyr::filter(fachbereich=="Alle")
#   df_mint <- df %>% dplyr::filter(fachbereich=="MINT-Berufsfelder (gesamt)")
#   df_andere$wert <- df_andere$wert - df_mint$wert
#   df_andere$fachbereich[df_andere$fachbereich == "Alle"]<-"Alle Berufsfelder außer MINT (gesamt)"
#
#   df <- rbind(df, df_andere)
#   df <- df %>% dplyr::filter(fachbereich != "Alle")
#
#   # Anteil Berechnen für aggregierte Werte MINT
#   mint_agg <- df %>%
#     dplyr::filter(fachbereich %in% c("MINT-Berufsfelder (gesamt)","Alle Berufsfelder außer MINT (gesamt)" )) %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(prop = (wert/sum(wert))*100)%>%
#     dplyr::mutate(prop= round(prop,1))
#   mint_agg <-  mint_agg %>% dplyr::filter(fachbereich == "MINT-Berufsfelder (gesamt)")
#
#   #Anteil Berechnen für Technik (gesamt)
#   technik_agg <- df %>%
#     dplyr::filter(fachbereich %in% c("Mathematik, Naturwissenschaften",
#                                      "Informatik", "Technik (gesamt)", "Alle Berufsfelder außer MINT (gesamt)" )) %>%
#     dplyr::ungroup()%>%
#     dplyr::mutate(prop = (wert/sum(wert))*100)%>%
#     dplyr::mutate(prop= round(prop,1))
#   technik_agg <-  technik_agg %>% dplyr::filter(fachbereich == "Technik (gesamt)")
#
#   #Anteil Berechnen für Technik-Gruppen
#   df <- df %>%
#     dplyr::filter(!(fachbereich %in% c("MINT-Berufsfelder (gesamt)", "Technik (gesamt)"))) %>%
#     dplyr::ungroup()%>%
#     dplyr::mutate(prop = (wert/sum(wert))*100)%>%
#     dplyr::mutate(prop= round(prop,1))
#
#   #Alle Werte zusammenfügen
#   df <- rbind(df, mint_agg, technik_agg)
#
#   #Trennpunkte für lange Zahlen ergänzen
#   df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
#   df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")
#
#   #für Überblick unterarten von Technik wieder raus
#   df <- df %>% dplyr::filter(fachbereich %in% c("Alle Berufsfelder außer MINT (gesamt)",
#                                                 "MINT-Berufsfelder (gesamt)",
#                                                 "Mathematik, Naturwissenschaften",
#                                                 "Informatik",
#                                                 "Technik (gesamt)"))
#
#   df$fachbereich[df$fachbereich == "Technik (gesamt)"]<-"Technik"
#
#   # Reihenfolge sortieren für Plot
#   df$fachbereich <- factor(df$fachbereich, levels = c("Alle Berufsfelder außer MINT (gesamt)",
#                                                       "MINT-Berufsfelder (gesamt)",
#                                                       "Mathematik, Naturwissenschaften",
#                                                       "Informatik",
#                                                       "Technik"))
#
#   # titel-helper
#   title_help <- paste0(indikator_choice, "n")
#   title_help <- ifelse(grepl("ausländische Beschäftigte", indikator_choice), "ausländischen Beschäftigten", title_help)
#   title_help <- ifelse(grepl("ausländische Auszubildende", indikator_choice), "ausländischen Auszubildenden", title_help)
#   title_help <- ifelse(grepl("Jahr", indikator_choice), "Auszubildenden mit neuem Lehrvertrag", title_help)
#   title_help <- ifelse(grepl("u25", indikator_choice), "Beschäftigten unter 25 Jahren", title_help)
#   title_help <- ifelse(grepl("25-55", indikator_choice), "Beschäftigten zwischen 25 und 55 Jahren", title_help)
#   title_help <- ifelse(grepl("ü55", indikator_choice), "Beschäftigten über 55 Jahren", title_help)
#
#   hover <- "Anteil an allen Berufsfeldern: {point.display_rel} % <br> Anzahl {point.indikator}: {point.wert}"
#   if(indikator_choice == "Auszubildende (1. Jahr)") hover <- "Anteil an allen Berufsfeldern: {point.display_rel} % <br> Anzahl Auszubildende mit neuem Lehrvertrag: {point.wert}"
#
#   # plot
#
#   #balkenbuilder wird hier nicht verwendet, weil das wieder so specialized ist
#   highcharter::hchart(df, 'bar', highcharter::hcaes(y = prop, x = fachbereich)) %>%
#     highcharter::hc_tooltip(pointFormat = hover) %>%
#     highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
#     highcharter::hc_xAxis(title = list(text = ""), categories =c("Alle Berufsfelder außer MINT (gesamt)",
#                                                                  "MINT-Berufsfelder (gesamt)",
#                                                                  "Mathematik, Naturwissenschaften",
#                                                                  "Informatik",
#                                                                  "Technik"
#     )) %>%
#     highcharter::hc_plotOptions(bar = list(
#       colorByPoint = TRUE,
#       colors = ifelse(df$fachbereich %in% c("Alle Berufsfelder außer MINT (gesamt)","MINT-Berufsfelder (gesamt)"), "#b16fab", "#d0a9cd")
#     )) %>%
#     highcharter::hc_title(text = paste0( "Überblick über die Berufsfelder von ", title_help,
#                                          br(), "in ",state, " (", timerange, ")"),
#                           margin = 20,
#                           align = "center",
#                           style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
#     highcharter::hc_chart(
#       style = list(fontFamily = "Calibri Regular", fontSize = "14px")
#     ) %>%
#     highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
#     highcharter::hc_exporting(enabled = TRUE,
#                               buttons = list(
#                                 contextButton = list(
#                                   menuItems = list("downloadPNG", "downloadCSV")
#                                 )
#                               )
#     )
# }



# Frauen in MINT ----
### Tab 1 ----
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


  df_query <- glue::glue_sql("
  SELECT *
  FROM arbeitsmarkt_detail
  WHERE jahr = {timerange}
  AND landkreis = 'alle Landkreise'
  AND bundesland = {regio}
  AND anforderung = 'Gesamt'
  AND NOT geschlecht = 'Gesamt'
  AND indikator IN ({indi*})
  AND fachbereich = {faecher}
                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)

  df <- df %>%
    dplyr::select( "indikator", "bundesland", "fachbereich", "jahr", "geschlecht", "wert")



  df_query <- glue::glue_sql("
  SELECT *
  FROM arbeitsmarkt_detail
  WHERE jahr = {timerange}
  AND landkreis = 'alle Landkreise'
  AND bundesland = {regio}
  AND anforderung = 'Gesamt'
  AND geschlecht = 'Gesamt'
  AND indikator IN ({indi*})
  AND fachbereich = {faecher}

                               ", .con = con)

  df_alle <- DBI::dbGetQuery(con, df_query)

  df_alle <- df_alle %>%
    dplyr::select( "indikator", "bundesland", "fachbereich", "jahr", "geschlecht", "wert") %>%
    dplyr::rename(wert_gesamt = "wert")

  df <- df %>%
    dplyr::left_join(df_alle, by = c("indikator", "bundesland", "jahr", "fachbereich")) %>%
    dplyr::rename(geschlecht = geschlecht.x) %>%
    dplyr::select(-geschlecht.y) %>%
    dplyr::group_by(indikator, geschlecht) %>%
    dplyr::mutate(proportion = round((wert/wert_gesamt)*100,1))

  if(gegenwert == "Ja"){


    df_query <- glue::glue_sql("
    SELECT *
    FROM arbeitsmarkt_detail
    WHERE jahr = {timerange}
    AND landkreis = 'alle Landkreise'
    AND bundesland = {regio}
    AND anforderung = 'Gesamt'
    AND indikator IN ({indi*})
    AND fachbereich = {faecher}

                               ", .con = con)

    df_andere <- DBI::dbGetQuery(con, df_query)

    df_andere <- df_andere %>%
      dplyr::select( "indikator", "bundesland", "fachbereich", "jahr", "geschlecht", "wert")


    df_query <- glue::glue_sql("
    SELECT *
    FROM arbeitsmarkt_detail
    WHERE jahr = {timerange}
    AND landkreis = 'alle Landkreise'
    AND bundesland = {regio}
    AND anforderung = 'Gesamt'
    AND indikator IN ({indi*})
    AND fachbereich = 'Alle'
                               ", .con = con)

    df_alle_faecher <- DBI::dbGetQuery(con, df_query)

    df_alle_faecher <- df_alle_faecher %>%
      dplyr::select( "indikator", "bundesland", "fachbereich", "jahr", "geschlecht", "wert") %>%
      dplyr::rename(wert_gesamt = "wert")


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

     title_help <- paste0(indi, "n")
     title_help <- ifelse(grepl("Jahr", indi), "Auszubildenden mit neuem Lehrvertrag", title_help)
     title_help <- ifelse(grepl("ausländische Auszubildende", indi), "ausländischen Auszubildenden", title_help)
     title_help <- ifelse(grepl("ausländische Beschäftigte", indi), "ausländischen Beschäftigten", title_help)

     df_p <- df[df$fachbereich == faecher,]
     titel <- ifelse(regio == "Saarland",
                     paste0("Frauenanteil unter ", title_help, " in ", faecher, " im ", regio, " (", timerange, ")"),
                     paste0("Frauenanteil unter ", title_help, " in ", faecher, " in ", regio, " (", timerange, ")"))
     color <- c("Männer" = "#efe8e6","Frauen" = "#154194")
     df_p <- df_p %>%
       dplyr::mutate(
         tooltip = paste0(
           "<b>", geschlecht, "</b><br>",
           "Anteil: ", prop_disp, " %<br>",
           "Anzahl: ", wert_disp
         )
       )

     quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

     p1 <- piebuilder_plotly(df_p, titel, x="geschlecht", y = "proportion",
                      color=color, quelle = quelle)
     out <- p1

     if(gegenwert == "Ja"){
       df_g <- df[df$fachbereich == "Andere Berufe",]

       titel1 <- ifelse(regio == "Saarland",
                        paste0("Frauenanteil unter ", title_help, " in Nicht MINT-Berufen im ", regio, " (", timerange, ")"),
                        paste0("Frauenanteil unter ", title_help, " in Nicht MINT-Berufen in ", regio, " (", timerange, ")"))
       df_g <- df_g %>%
         dplyr::mutate(
           tooltip = paste0(
             "<b>", geschlecht, "</b><br>",
             "Anteil: ", prop_disp, " %<br>",
             "Anzahl: ", wert_disp
           )
         )

       color <- c("Männer" = "#efe8e6","Frauen" = "#154194")

       quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

       p1g <- piebuilder_plotly(df_g, titel1, x="geschlecht", y = "proportion",
                                color=color, quelle = quelle) |>
         plotly::layout(height = 400)

       out <- list(p1, p1g)

     }

   } else if(length(indi) == 2) {

     title_help1 <- paste0(indi[1], "n")
     title_help1 <- ifelse(grepl("Jahr", indi[1]), "Auszubildenden mit neuem Lehrvertrag", title_help1)
     title_help1 <- ifelse(grepl("ausländische Auszubildende", indi[1]), "ausländischen Auszubildenden", title_help1)
     title_help1 <- ifelse(grepl("ausländische Beschäftigte", indi[1]), "ausländischen Beschäftigten", title_help1)
     title_help2 <- paste0(indi[2], "n")
     title_help2 <- ifelse(grepl("Jahr", indi[2]), "Auszubildenden mit neuem Lehrvertrag", title_help2)
     title_help2 <- ifelse(grepl("ausländische Auszubildende", indi[2]), "ausländischen Auszubildenden", title_help2)
     title_help2 <- ifelse(grepl("ausländische Beschäftigte", indi[2]), "ausländischen Beschäftigten", title_help2)


     df_1_pie <- df %>% dplyr::filter(indikator == indi[1], fachbereich != "Andere Berufe")
     df_2_pie <- df %>% dplyr::filter(indikator == indi[2], fachbereich != "Andere Berufe")

     titel1 <- ifelse(regio == "Saarland",
                      paste0("Frauenanteil unter ", title_help1, " in ", faecher[1], " im ", regio, " (", timerange, ")"),
                      paste0("Frauenanteil unter ", title_help1, " in ", faecher[1], " in ", regio, " (", timerange, ")"))
     titel2 <- ifelse(regio == "Saarland",
                      paste0("Frauenanteil unter ", title_help2, " in ", faecher[1], " im ", regio, " (", timerange, ")"),
                      paste0("Frauenanteil unter ", title_help2, " in ", faecher[1], " in ", regio, " (", timerange, ")"))
     df_1_pie <- df_1_pie %>%
       dplyr::mutate(
         tooltip = paste0(
           "<b>", geschlecht, "</b><br>",
           "Anteil: ", prop_disp, " %<br>",
           "Anzahl: ", wert_disp
         )
       )
     df_2_pie <- df_2_pie %>%
       dplyr::mutate(
         tooltip = paste0(
           "<b>", geschlecht, "</b><br>",
           "Anteil: ", prop_disp, " %<br>",
           "Anzahl: ", wert_disp
         )
       )
     color <- c("Männer" = "#efe8e6","Frauen" = "#154194")

     quelle <- "Quelle: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

     p1 <- piebuilder_plotly(df_1_pie, titel1, x="geschlecht", y = "proportion",
                      color=color, quelle=quelle)
     p2 <- piebuilder_plotly(df_2_pie, titel2, x="geschlecht", y = "proportion", color=color,
                      quelle = quelle)

     out<- list( p1, p2)

     if(gegenwert == "Ja"){
       df <- df %>%
         dplyr::mutate(
           tooltip = paste0(
             "<b>", geschlecht, "</b><br>",
             "Anteil: ", prop_disp, " %<br>",
             "Anzahl: ", wert_disp
           )
         )
       df1_g <- df[df$fachbereich == "Andere Berufe" & df$indikator == indi[1],]
       df2_g <- df[df$fachbereich == "Andere Berufe" & df$indikator == indi[2],]

       titel1 <- ifelse(regio == "Saarland",
                       paste0("Frauenanteil unter ", title_help1 , " in Nicht MINT-Berufen im ", regio , " (", timerange, ")"),
                       paste0("Frauenanteil unter ", title_help1 , " in Nicht MINT-Berufen in ", regio , " (", timerange, ")"))

       titel2 <- ifelse(regio == "Saarland",
                        paste0("Frauenanteil unter ", title_help2, " in Nicht MINT-Berufen im ", regio , " (", timerange, ")"),
                        paste0("Frauenanteil unter ", title_help2, " in Nicht MINT-Berufen in ", regio , " (", timerange, ")"))

       color <- c("Männer" = "#efe8e6","Frauen" = "#154194")

       quelle <- "Quelle: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

       p1g <- piebuilder_plotly(df1_g, titel1, x="geschlecht", y = "proportion",
                         color=color, quelle=quelle)|>
         plotly::layout(height = 400)
       p2g <- piebuilder_plotly(df2_g, titel2, x="geschlecht", y = "proportion",
                         color=color, quelle=quelle)|>
         plotly::layout(height = 400)


       out <- list(p1, p2, p1g, p2g)

     }

   }
  }
  else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){


    df$indikator_fachbereich <- paste(df$indikator, df$fachbereich, sep = " - ")


    df <- df %>%
      dplyr::mutate(
        indikator_fachbereich_name = dplyr::case_when(
          stringr::str_length(indikator_fachbereich) > 60 ~
            paste0(
              stringr::str_trunc(indikator_fachbereich, 55)
            ),
          TRUE ~ indikator_fachbereich
        ),
        indikator_fachbereich_name = as.character(indikator_fachbereich_name)
      )



    df <- df[with(df, order(proportion, decreasing = TRUE)), ]


    if(gegenwert == "Ja"){
      titel <- ifelse(regio == "Saarland",
                      paste0("Frauenanteil in ", faecher," <br> und restlichen Berufen im ", regio, " (", timerange, ")"),
                      paste0("Frauenanteil in ", faecher," <br> und restlichen Berufen in ", regio, " (", timerange, ")"))
    }else{
      titel <- ifelse(regio == "Saarland",
                      paste0("Frauenanteil in ", faecher," im ", regio, " (", timerange, ")"),
                      paste0("Frauenanteil in ", faecher," in ", regio, " (", timerange, ")"))
    }




    df_order <- df %>%
      dplyr::filter(geschlecht == "Frauen") %>%
      dplyr::arrange(
        factor(fachbereich,
               levels = c("Andere Berufe", "MINT")),
        dplyr::desc(proportion)
      )

    order <- unique(df_order$indikator_fachbereich)


   df <- df %>%
     dplyr::mutate(
       .tooltip = paste0(
         "<b><span style='font-size:15px;'>", indikator_fachbereich, "</span></b><br>",
         "<span style='font-size:15px;'>", geschlecht, "</span><br>",
         "Anzahl: ", formatC(as.numeric(wert), format = "f", digits = 0, big.mark = "."), "<br>",
         "Anteil: ", proportion, " %"
       )
     )


   x <- "indikator_fachbereich"
   y <- "proportion"
   group <- "geschlecht"

   color <- c("#154194", "#efe8e6")
   quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
   quelle_y <- -0.20
   legend_y <- -0.07

   out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h", group=group, color = color,
                               tickvals = df$indikator_fachbereich, ticktext = df$indikator_fachbereich_name,
                               order = order, stacking = TRUE, percent = TRUE, quelle=quelle, quelle_y=quelle_y, legend_y=legend_y)



 }

  return(out)

}

### Tab 2 ----

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


  df_query <- glue::glue_sql("
  SELECT jahr, indikator, geschlecht, bundesland, wert, fachbereich
  FROM arbeitsmarkt_detail
  WHERE jahr IN ({t*})
  AND landkreis = 'alle Landkreise'
  AND bundesland = {regio}
  AND anforderung = 'Gesamt'
  AND fachbereich = {faecher}
  AND indikator IN ({indi*})
  AND geschlecht = 'Frauen'
                             ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)

  # df <- df %>%
  #   dplyr::select(jahr, indikator, geschlecht, bundesland, wert, fachbereich)


  if(absolut_selector=="In Prozent"){



    df_query <- glue::glue_sql("
    SELECT jahr, indikator, geschlecht, bundesland, wert AS wert_ges, fachbereich
    FROM arbeitsmarkt_detail
    WHERE jahr IN ({t*})
    AND landkreis = 'alle Landkreise'
    AND bundesland = {regio}
    AND anforderung = 'Gesamt'
    AND fachbereich = {faecher}
    AND indikator IN ({indi*})
    AND geschlecht = 'Gesamt'
                               ", .con = con)

    df_gen_alle <- DBI::dbGetQuery(con, df_query)


    df <- df %>% dplyr::left_join(df_gen_alle,
                                  by = c("jahr", "indikator", "bundesland", "fachbereich")) %>%
      dplyr::rename(geschlecht = geschlecht.x) %>%
      dplyr::select(-geschlecht.y) %>%
      dplyr::mutate(prop = round(wert/wert_ges * 100, 1)) %>%
      dplyr::filter(geschlecht != "Gesamt")


    # order years for plot

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

    if(nrow(df) == 0){
      titel_text <- paste0("Für die ausgewählte Kombination aus Gruppe, Berufsfeld und Region liegen keine Daten vor.
                           Gründe dafür sind, das entweder keine oder fast keine Frauen in dieser Gruppe arbeiten.")
    }else{
      titel_text <- ifelse(regio == "Saarland",
                           paste0("Entwicklung des Frauenanteils im Berufsfeld ", faecher, " im ", regio),
                           paste0("Entwicklung des Frauenanteils im Berufsfeld ", faecher, " in ", regio))
    }


    titel <-  titel_text
    tooltip <-  "{point.indikator} <br> Frauenanteil: {point.prop_disp} %"
    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", fachbereich, "</b><br>",
          "Jahr: ", jahr, "<br>",
          "Frauenanteil: ", prettyNum(prop, big.mark = ".", decimal.mark = ","), " %"
        )
      )

    color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
               "#bfc6d3", "#5f94f9")[1:length(unique(df$indikator))]
    quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    out <- linebuilder_plotly(df, titel, x = "jahr", y = "prop", group = "indikator", color = color, quelle = quelle)



  } else if(absolut_selector=="Anzahl"){

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


    if(nrow(df) == 0){
      titel_text <- paste0("Für die ausgewählte Kombination aus Gruppe, Berufsfeld und Region liegen keine Daten vor.
                           Gründe dafür sind, das entweder keine oder fast keine Frauen in dieser Gruppe arbeiten.")
    }else{
    titel_text <- ifelse(regio == "Saarland",
                         paste0("Entwicklung der Anzahl an Frauen im Berufsfeld ", faecher, " im ", regio),
                         paste0("Entwicklung der Anzahl an Frauen im Berufsfeld ", faecher, " in ", regio))
    }


    titel <-  titel_text

    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", indikator, "</b><br>",
          "Jahr: ", jahr, "<br>",
          "Anzahl Frauen: ", prettyNum(wert, big.mark = ".", decimal.mark = ",")
        )
      )
    format <- ",d"
    color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
               "#bfc6d3", "#5f94f9")[1:length(unique(df$indikator))]
    quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    out <- linebuilder_plotly(df, titel, x = "jahr", y = "wert", group = "indikator", format = format, color = color, quelle = quelle)

  }
}

### Tab 3 ----

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

    df_query <- glue::glue_sql("
    SELECT jahr, bundesland, indikator, fachbereich, wert, geschlecht
    FROM arbeitsmarkt_detail
    WHERE jahr IN ({timerange*})
    AND landkreis = 'alle Landkreise'
    AND bundesland = {regio}
    AND NOT geschlecht = 'Gesamt'
    AND anforderung = 'Gesamt'
    AND indikator = {indi}
    AND fachbereich IN ('Alle', 'MINT', 'Mathematik, Naturwissenschaften', 'Informatik', 'Technik (gesamt)')
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)


    # Berechnung von andere Fächergruppen
    df[df$fachbereich == "Alle" & df$geschlecht == "Frauen", "wert"] <- df[df$fachbereich == "Alle" & df$geschlecht == "Frauen", "wert"]-
      df[df$fachbereich == "MINT" & df$geschlecht == "Frauen", "wert"]
    df[df$fachbereich == "Alle" & df$geschlecht == "Männer", "wert"] <- df[df$fachbereich == "Alle" & df$geschlecht == "Männer", "wert"]-
      df[df$fachbereich == "MINT" & df$geschlecht == "Männer", "wert"]
    df$fachbereich[df$fachbereich == "Alle"]<-"andere Berufsfelder"
    df <- df %>% dplyr::filter(fachbereich != "MINT")

     # Anteil berechnen

    df_query <- glue::glue_sql("
    SELECT jahr, bundesland, indikator, fachbereich, geschlecht, wert as wert_ges
    FROM arbeitsmarkt_detail
    WHERE jahr IN ({timerange*})
    AND landkreis = 'alle Landkreise'
    AND bundesland = {regio}
    AND NOT geschlecht = 'Gesamt'
    AND anforderung = 'Gesamt'
    AND indikator = {indi}
    AND fachbereich = 'Alle'
                               ", .con = con)

    df_alle <- DBI::dbGetQuery(con, df_query)


     df <- df %>% dplyr::left_join(df_alle, by = c("jahr", "bundesland", "indikator",
                                                  "geschlecht")) %>%
      dplyr::rename(fachbereich = fachbereich.x) %>%
      dplyr::select(-fachbereich.y) %>%
      dplyr::mutate(prop = round(wert/wert_ges *100, 1))

     # nach Geschlechtern trennen
     df$wert_disp <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
     df$prop_disp <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")
     df <- df %>%
       dplyr::mutate(
         tooltip = paste0(
           "<b>", fachbereich, "</b><br>",
           "Anteil: ", prop_disp, " %<br>",
           "Anzahl: ", wert_disp
         )
       )
     df_f <- df %>% dplyr::filter(geschlecht=="Frauen")
     df_m <- df %>% dplyr::filter(geschlecht=="Männer")

     # Titel für Plots
     title_help <- paste0(indi)
     title_help <- ifelse(grepl("Beschäftigte", indi), "Beschäftigten", title_help)
     title_help <- ifelse(grepl("Auszubildende", indi), "Auszubildenden", title_help)
     title_help <- ifelse(grepl("ausländische Beschäftigte", indi), "ausländischen Beschäftigten", title_help)
     title_help <- ifelse(grepl("ausländische Auszubildende", indi), "ausländischen Auszubildenden", title_help)
     title_help <- ifelse(grepl("Jahr", indi), "Auszubildenden mit neuem Lehrvertrag", title_help)

     df_f <- df_f[with(df_f, order(prop, decreasing = FALSE)), ]

     df_m <- df_m[with(df_m, order(prop, decreasing = FALSE)), ]


     titel1 <- paste0("Berufswahl unter Frauen in ", regio, " (", timerange, ")")
     titel2 <- paste0("Berufswahl unter Männern in ", regio, " (", timerange, ")")
     subtitel1 <- paste0("Von allen weiblichen ", title_help, " arbeiten ", round(100-df_f$prop[df_f$fachbereich == "andere Berufsfelder"],1), "% in MINT")
     subtitel2 <-  paste0("Von allen männlichen ", title_help, " arbeiten ", round(100-df_m$prop[df_m$fachbereich == "andere Berufsfelder"],1), "% in MINT")

     quelle <- "Quelle: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

     out_1 <- piebuilder_plotly(df_f, titel1, x="fachbereich", y = "prop", legend_y=0.01,
                                color=color_fachbereich, subtitel = subtitel1, quelle=quelle) |>
       plotly::layout(
         annotations = list(
           list(
             text = quelle,
             x = 1,
             y = -0.5,
             xref = "paper",
             yref = "paper",
             xanchor = "right",
             yanchor = "top",
             showarrow = FALSE,
             font = list(size = 11, color = "gray", family = "Calibri Regular", align = "right")
           )
         )
       )
     out_2 <- piebuilder_plotly(df_m, titel2, x="fachbereich", y = "prop", legend_y=0.01,
                                color=color_fachbereich, subtitel = subtitel2, quelle=quelle)|>
       plotly::layout(
         annotations = list(
           list(
             text = quelle,
             x = 1,
             y = -0.5,
             xref = "paper",
             yref = "paper",
             xanchor = "right",
             yanchor = "top",
             showarrow = FALSE,
             font = list(size = 11, color = "gray", family = "Calibri Regular", align = "right")
           )
         )
       )


     out <- list(out_1, out_2)

   }else
     if(betrachtung == "Übersicht - Kartendiagramm"){
     timerange <- r$date_arbeitsmarkt_wahl_gender_karte
     indi <- r$level_arbeitsmarkt_wahl_gender_karte
     faecher <- r$fach_arbeitsmarkt_wahl_gender_karte


     df_query <- glue::glue_sql("
     SELECT indikator, fachbereich, wert, geschlecht, bundesland, jahr
     FROM arbeitsmarkt_detail
     WHERE jahr IN ({timerange*})
     AND NOT bundesland IN ('Deutschland', 'Westdeutschland (o. Berlin)', 'Ostdeutschland (inkl. Berlin)')
     AND landkreis = 'alle Landkreise'
     AND NOT geschlecht = 'Gesamt'
     AND anforderung = 'Gesamt'
     AND indikator = {indi}
     AND fachbereich = {faecher}", .con = con)

     df <- DBI::dbGetQuery(con, df_query)


     df_query <- glue::glue_sql("
     SELECT indikator, fachbereich, geschlecht, bundesland, wert as wert_ges, jahr
     FROM arbeitsmarkt_detail
     WHERE NOT bundesland IN ('Deutschland', 'Westdeutschland (o. Berlin)', 'Ostdeutschland (inkl. Berlin)')
     AND landkreis = 'alle Landkreise'
     AND NOT geschlecht = 'Gesamt'
     AND anforderung = 'Gesamt'
     AND indikator = {indi}
     AND fachbereich = 'Alle'
                               ", .con = con)

     df_alle <- DBI::dbGetQuery(con, df_query)

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

     titel_w <- ifelse(faecher == "Andere Berufsgruppen", paste0("Anteil weiblicher ", title_help, ", die kein <br> MINT-Berufsfeld wählen (", timerange, ")"),
                       paste0("Anteil weiblicher ", title_help, ", die das Berufsfeld ", faecher, " wählen (", timerange, ")"))
     titel_m <- ifelse(faecher == "Andere Berufsgruppen", paste0("Anteil männlicher ", title_help, ", die kein <br> MINT-Berufsfeld wählen (", timerange, ")"),
                       paste0("Anteil männlicher ", title_help, ", die das Berufsfeld ", faecher, " wählen (", timerange, ")"))

     # plot


     df <- values_female
     df <- df %>%
       dplyr::mutate(
         tooltip = paste0(
           "<b>", bundesland, "</b><br>",
           "Anteil: ", prop_disp, " %<br>",
           "Anzahl: ", wert_disp
         )
       )
     titel <- titel_w
     quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
     out_1 <- mapbuilder_plotly(df,
                         titel = titel,
                         value_col = "prop",
                         regio_col = "bundesland",
                         quelle = quelle)


     df <- values_male
     df <- df %>%
       dplyr::mutate(
         tooltip = paste0(
           "<b>", bundesland, "</b><br>",
           "Anteil: ", prop_disp, " %<br>",
           "Anzahl: ", wert_disp
         )
       )
     titel <- titel_m
     quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
     out_2 <- mapbuilder_plotly(df,
                         titel = titel,
                         value_col = "prop",
                         regio_col = "bundesland",
                         quelle = quelle)



     out <- list(
       out_1, out_2)


     }else if(betrachtung == "Zeitverlauf - Liniendiagramm"){
    timerange <- r$date_arbeitsmarkt_wahl_gender_verlauf
    t <- timerange[1]:timerange[2]
    indi <- r$level_arbeitsmarkt_wahl_gender_verlauf
    regio <- r$states_arbeitsmarkt_wahl_gender_verlauf
    faecher <- r$fach_arbeitsmarkt_wahl_gender_verlauf
    absolut_selector <- r$abs_zahlen_arbeitsmarkt_wahl_gender_verlauf

     df_query <- glue::glue_sql("
     SELECT *
     FROM arbeitsmarkt_detail
     WHERE jahr IN ({t*})
     AND indikator = {indi}
     AND landkreis = 'alle Landkreise'
     AND bundesland IN ({regio*})
     AND anforderung = 'Gesamt'
     AND geschlecht = 'Frauen'
     AND fachbereich = {faecher}
                               ", .con = con)

     df <- DBI::dbGetQuery(con, df_query)

     df <- df %>%
       dplyr::select("indikator", "fachbereich", "geschlecht", "bundesland",
                     "jahr", "wert" )

     if(absolut_selector=="In Prozent"){

       df_query <- glue::glue_sql("

       SELECT *
       FROM arbeitsmarkt_detail
       WHERE jahr IN ({t*})
       AND indikator = {indi}
       AND landkreis = 'alle Landkreise'
       AND bundesland IN ({regio*})
       AND anforderung = 'Gesamt'
       AND geschlecht = 'Frauen'
       AND fachbereich = 'Alle'
                               ", .con = con)

       df_alle <- DBI::dbGetQuery(con, df_query)

       df_alle <- df_alle %>%
         dplyr::select("indikator", "fachbereich", "geschlecht", "bundesland",
                       "jahr", "wert" ) %>%
         dplyr::rename(wert_ges = wert)

       df <- df %>%
         dplyr::left_join(df_alle, by = c("bundesland", "jahr", "geschlecht", "indikator")) %>%
         dplyr::rename(fachbereich = fachbereich.x) %>%
         dplyr::select(-fachbereich.y) %>%
         dplyr::mutate(prop = round(wert/wert_ges*100, 1))%>%
         dplyr::filter(fachbereich != "Alle")


       sorted_indicators <- df %>%
         dplyr::group_by(bundesland) %>%
         dplyr::summarize(m_value = mean(round(prop, 1), na.rm = TRUE)) %>%
         dplyr::arrange(m_value) %>%
         dplyr::pull(bundesland)

       df$bundesland <- factor(df$bundesland, levels = sorted_indicators)

       # order years for plot
       df <- df[with(df, order(bundesland, jahr, decreasing = FALSE)), ]

       title_help <- paste0(indi, "r")
       title_help <- ifelse(grepl("ausländische Beschäftigte", indi), "ausländischer Beschäftigter", title_help)
       title_help <- ifelse(grepl("ausländische Auszubildende", indi), "ausländischer Auszubildender", title_help)
       title_help <- ifelse(grepl("Jahr", indi), "Auszubildender mit neuem Lehrvertrag", title_help)
       titel_w <- ifelse(faecher == "Andere Berufsgruppen", paste0("Anteil weiblicher ", title_help, ", die kein MINT-Berufsfeld wählen (", timerange, ")"),
                         paste0("Anteil weiblicher ", title_help, ", die das Berufsfeld ", faecher, " wählen"))

       # plot

       titel <-  titel_w
       df <- df %>%
         dplyr::mutate(
           tooltip = paste0(
             "<b>", bundesland, "</b><br>",
             "Jahr: ", jahr, "<br>",
             "Anteil: ", prettyNum(prop, big.mark = ".", decimal.mark = ","), " %"
           )
         )
       color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                  "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")


       quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
       out <- linebuilder_plotly(df, titel, x = "jahr", y = "prop", group = "bundesland", color=color, quelle = quelle)


     }else if(absolut_selector=="Anzahl"){

       title_help <- paste0(indi, "r")
       title_help <- ifelse(grepl("ausländische Beschäftigte", indi), "ausländischer Beschäftigter", title_help)
       title_help <- ifelse(grepl("ausländische Auszubildende", indi), "ausländischer Auszubildender", title_help)
       title_help <- ifelse(grepl("Jahr", indi), "Auszubildender mit neuem Lehrvertrag", title_help)

       titel_w <- ifelse(faecher == "Andere Berufsgruppen", paste0("Anzahl weiblicher ", title_help, ", die kein MINT-Berufsfeld wählen (", timerange, ")"),
                         paste0("Anzahl weiblicher ", title_help, ", die das Berufsfeld ", faecher, " wählen (", timerange, ")"))

       sorted_indicators <- df %>%
         dplyr::group_by(bundesland) %>%
         dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
         dplyr::arrange(m_value) %>%
         dplyr::pull(bundesland)

       df$bundesland <- factor(df$bundesland, levels = sorted_indicators)

       df <- df[with(df, order(bundesland, jahr, decreasing = FALSE)), ]

       # plot

      titel <- paste0("Anzahl weiblicher ", title_help, ", die MINT-Berufe wählen")

      df <- df %>%
        dplyr::mutate(
          tooltip = paste0(
            "<b>", bundesland, "</b><br>",
            "Jahr: ", jahr, "<br>",
            "Anzahl: ", prettyNum(wert, big.mark = ".", decimal.mark = ",")
          )
        )
      format <- ",d"
      color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                 "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
      quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
      out <- linebuilder_plotly(df, titel, x = "jahr", y = "wert", group = "bundesland", format= format, color = color, quelle = quelle)

     }



  }
  return(out)
}

### Tab 4 ----

arbeitsmarkt_top10 <- function( r){

  # UI Input zuweisen
  time <- r$date_top_beruf
  bula <- r$states_top_beruf
  abs_rel <- r$betr_abs_rel
  fb <- r$FB_top_beruf


  df_query <- glue::glue_sql("
  SELECT *
  FROM data_naa
  WHERE jahr = {time}
  AND ebene = 'Ebene 3'
  AND region = {bula}
                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)

  df <- df %>%
    dplyr::select(-code)

  df <- df %>% dplyr::ungroup() %>%
    dplyr::mutate(region = dplyr::case_when(
      region == "Westdeutschland (ohne Berlin)" ~ "Westdeutschland (o. Berlin)",
      region == "Ostdeutschland (mit Berlin)" ~ "Ostdeutschland (inkl. Berlin)",
      T ~ .$region
    ))

  # Auswahl Fachbereich
  if(fb != "MINT (gesamt)"){
    df <- df %>% dplyr::filter(fachbereich == fb)
  }


  # zu gering besetzte Ausbildungen ausfiltern
  df <- df %>% dplyr::filter(df$wert > 50)

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



  df <- df %>%
    dplyr::mutate(
      beruf_short = stringr::str_trunc(
        as.character(beruf),
        width = 30
      )
    )

  praep <- ifelse(bula == "Saarland", " im ", " in ")


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


    if(sum(berufe_maenner$prop)==1000){
      berufe_maenner <- df %>%
        dplyr::filter(geschlecht == "Männer") %>%
        dplyr::filter(prop == 100) %>%
        dplyr::arrange(desc(wert)) %>%
        dplyr::slice(1:10)
    }


# Create female plot

    titel <- paste0("Höchster Frauenanteil unter den neuen Auszubildenden im Fachbereich " ,fb ,praep, bula, " (", time, ")")



    order <- unique(berufe_frauen$beruf)


    berufe_frauen <- berufe_frauen %>%
      dplyr::mutate(
        .tooltip = paste0(
          "<b><span style='font-size:15px;'>", beruf, "</span></b><br>",
          "Anzahl: ", formatC(as.numeric(wert), format = "f", digits = 0, big.mark = "."), "<br>",
          "Anteil: ", prop, " %"
        )
      )


    x <- "beruf"
    y <- "prop"

    color <- c("#154194")
    quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    quelle_y <- -0.16

    plot_female <- balkenbuilder_plotly(df=berufe_frauen, x=x, y=y, titel=titel, orientation = "h", group=NULL, color = color,
                                        tickvals = df$beruf, ticktext = df$beruf_short, wrap_width=40,
                                        order = order, stacking = FALSE, percent = TRUE, quelle=quelle, quelle_y=quelle_y) %>%
      plotly::layout(margin = list(t=100))




    # Create male plot

    titel <- paste0("Höchster Männeranteil unter den neuen Auszubildenden im Fachbereich " ,fb ,praep, bula, " (", time, ")")


    order <- unique(berufe_maenner$beruf)


    berufe_maenner <- berufe_maenner %>%
      dplyr::mutate(
        .tooltip = paste0(
          "<b><span style='font-size:15px;'>", beruf, "</span></b><br>",
          "Anzahl: ", formatC(as.numeric(wert), format = "f", digits = 0, big.mark = "."), "<br>",
          "Anteil: ", prop, " %"
        )
      )


    x <- "beruf"
    y <- "prop"

    color <- c("#66cbaf")
    quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    quelle_y <- -0.16

    plot_male <- balkenbuilder_plotly(df=berufe_maenner, x=x, y=y, titel=titel, orientation = "h", group=NULL, color = color,
                                      tickvals = df$beruf, ticktext = df$beruf_short, wrap_width=40,
                                      order = order, stacking = FALSE, percent = TRUE, quelle=quelle, quelle_y=quelle_y) %>%
      plotly::layout(margin = list(t=100))








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

    titel <- paste0("Am häufigsten gewählte MINT-Ausbildungsberufe von weiblichen Neu-Auszubildenden im Fachbereich " ,fb , praep, bula ," (", time, ")")


    order <- unique(berufe_frauen$beruf)


    berufe_frauen <- berufe_frauen %>%
      dplyr::mutate(
        .tooltip = paste0(
          "<b><span style='font-size:15px;'>", beruf, "</span></b><br>",
          "Anzahl: ", formatC(as.numeric(wert), format = "f", digits = 0, big.mark = "."), "<br>",
          "Anteil: ", prop, " %"
        )
      )


    x <- "beruf"
    y <- "wert"

    color <- c("#154194")
    quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    quelle_y <- -0.16

    plot_female <- balkenbuilder_plotly(df=berufe_frauen, x=x, y=y, titel=titel, orientation = "h", group=NULL, color = color,
                                        tickvals = df$beruf, ticktext = df$beruf_short,wrap_width=40,
                                        order = order, stacking = FALSE, percent = FALSE, quelle=quelle, quelle_y=quelle_y)








# Create male plot

    titel <- paste0("Am häufigsten gewählte MINT-Ausbildungsberufe von männlichen Neu-Auszubildenden im Fachbereich  " ,fb , praep, bula ," (", time, ")")


    order <- unique(berufe_maenner$beruf)


    berufe_maenner <- berufe_maenner %>%
      dplyr::mutate(
        .tooltip = paste0(
          "<b><span style='font-size:15px;'>", beruf, "</span></b><br>",
          "Anzahl: ", formatC(as.numeric(wert), format = "f", digits = 0, big.mark = "."), "<br>",
          "Anteil: ", prop, " %"
        )
      )


    x <- "beruf"
    y <- "wert"

    color <- c("#66cbaf")
    quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    quelle_y <- -0.16

    plot_male <- balkenbuilder_plotly(df=berufe_maenner, x=x, y=y, titel=titel, orientation = "h", group=NULL, color = color,
                                      tickvals = df$beruf, ticktext = df$beruf_short, wrap_width=40,
                                      order = order, stacking = FALSE, percent = FALSE, quelle=quelle, quelle_y=quelle_y)


  }


  out <- list(plot_female, plot_male)

  return(out)

}









### Tab 5 ----

#' A function to plot a waffle chart
#'
#' @description A function to create a waffle chart for the tab "Beruf"
#'
#' @return The return value is a waffle chart
#' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_faecher_anteil_frauen <- function(r) {

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


  timerange <- r$date_arbeitsmarkt_fach_vergleich_frauen
  regio <- r$region_arbeitsmarkt_fach_vergleich_frauen
  nicht_mint <- r$gegenwert_arbeitsmarkt_fach_vergleich_frauen
  indikator_choice <- r$indikator_arbeitsmarkt_fach_vergleich_balken_frauen

  indikator_choice <- gsub("^weibliche ", "", indikator_choice)



      df_query <- glue::glue_sql("
    SELECT *
    FROM arbeitsmarkt_detail
    WHERE jahr = {timerange}
    AND indikator = {indikator_choice}
    AND landkreis = 'alle Landkreise'
    AND anforderung = 'Gesamt'
    AND geschlecht = 'Frauen'
    AND bundesland = {regio}
    AND fachbereich IN ('Alle', 'MINT', 'Mathematik, Naturwissenschaften', 'Informatik', 'Technik (gesamt)')
                               ", .con = con)

      df <- DBI::dbGetQuery(con, df_query)

      df <- df %>%
        dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)

      df_andere <- df %>% dplyr::filter(fachbereich=="Alle")
      df_mint <- df %>% dplyr::filter(fachbereich=="MINT")
      df_andere$wert <- df_andere$wert - df_mint$wert
      df_andere$fachbereich[df_andere$fachbereich == "Alle"]<-"Alle Berufsfelder außer MINT"
      df <- rbind(df, df_andere)
      df <- df %>% dplyr::filter(fachbereich != "Alle")


      df_query <- glue::glue_sql("
    SELECT *
    FROM arbeitsmarkt_detail
    WHERE jahr = {timerange}
    AND indikator = {indikator_choice}
    AND landkreis = 'alle Landkreise'
    AND anforderung = 'Gesamt'
    AND geschlecht = 'Gesamt'
    AND bundesland = {regio}
    AND fachbereich IN ('Alle', 'MINT', 'Mathematik, Naturwissenschaften', 'Informatik', 'Technik (gesamt)')
                               ", .con = con)

      df_alle <- DBI::dbGetQuery(con, df_query)



      df_alle <- df_alle %>%
        dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)

      df_andere <- df_alle %>% dplyr::filter(fachbereich=="Alle")
      df_mint <- df_alle %>% dplyr::filter(fachbereich=="MINT")
      df_andere$wert <- df_andere$wert - df_mint$wert
      df_andere$fachbereich[df_andere$fachbereich == "Alle"]<-"Alle Berufsfelder außer MINT"
      df_alle <- rbind(df_alle, df_andere)
      df_alle <- df_alle %>% dplyr::filter(fachbereich != "Alle")

      df <- df %>%
        dplyr::left_join(df_alle,
                         dplyr::join_by("bundesland", "jahr", "indikator", "fachbereich")) %>%
      #  dplyr::select(-fachbereich.y) %>%
        dplyr::rename(
                      wert = wert.x,
                      wert_ges = wert.y) %>%
        dplyr::mutate(prop = round(wert/wert_ges * 100,1))

      #Trennpunkte für lange Zahlen ergänzen
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


      praep <- ifelse(regio == "Saarland", " im ", " in ")

      titel <- paste0( "Überblick über die Berufsfelder von weiblichen ", title_help, praep, regio, " (", timerange, ")")
      quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."


      order <- unique(df$fachbereich)

      df <- df %>%
        dplyr::mutate(
          .tooltip = paste0(
            "<b><span style='font-size:15px;'>", fachbereich, "</span></b><br>",
            "<span style='font-size:15px;'> Anteil an allen Berufsfeldern: </span>", prop, "%<br>",
            "Anzahl: ", (formatC(as.numeric(wert),format = "f",digits = 0,big.mark = "."))
          ))



      x <- "fachbereich"
      y <- "prop"
      color <- color_fachbereich_balken
      quelle_y <- -0.15


      out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h", group=NULL, color = color,
                                  order = order, percent=TRUE, stacking=FALSE,quelle_y=quelle_y, quelle=quelle)



  return(out)
}











### Nicht Box 3 ----###################################################

#' A function to plot time series
#'
#' @description A function to plot the time series
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
# arbeitsmarkt_bl_gender_verlauf <- function(r) {
#
#   # load UI inputs from reactive value
#
#   absolut_selector <- r$abs_zahlen_beruf_arbeitsmarkt_bl_gender_verlauf
#   timerange <- r$date_beruf_arbeitsmarkt_bl_gender_verlauf
#   indikator_choice <- r$indikator_beruf_arbeitsmarkt_bl_gender_verlauf
#   states <- r$states_beruf_arbeitsmarkt_bl_gender_verlauf
#   t <- as.character(timerange[1]:timerange[2])
#
#
#
#
#   df_query <- glue::glue_sql("
#   SELECT *
#   FROM arbeitsmarkt
#   WHERE jahr IN ({t*})
#   AND region IN ({states*})
#   AND anforderung = 'Gesamt'
#   AND geschlecht = 'Frauen'
#   AND fachbereich IN ('Alle', 'MINT')
#                                ", .con = con)
#
#   df <- DBI::dbGetQuery(con, df_query)
#
#   df <- df %>%
#     dplyr::select("bereich",
#                   "indikator",
#                   "fachbereich",
#                   "geschlecht",
#                   "region",
#                   "jahr",
#                   "anforderung",
#                   "wert" )
#
#   df <- df %>% dplyr::filter(anforderung != "Keine Zuordnung möglich")
#
#   df_gesamt <- df %>%
#     dplyr::filter(fachbereich == "Alle",
#                   anforderung == "Gesamt")
#
#
#   df <- df %>%
#     dplyr::left_join(df_gesamt, by = c("region", "jahr", "geschlecht", "indikator", "bereich")) %>%
#     dplyr::rename(anforderung = "anforderung.x",
#                   fachbereich = "fachbereich.x",
#                   wert = "wert.x",
#                   wert_sum = "wert.y") %>%
#     dplyr::select(-c("fachbereich.y", "anforderung.y")) %>%
#     dplyr::mutate(proportion = (wert/wert_sum)*100)%>%
#     dplyr::filter(anforderung == "Gesamt",
#                   fachbereich == "MINT")%>%
#     dplyr::select(-wert_sum)%>%
#     dplyr::rename(Relativ = proportion, Absolut=wert)%>%
#     tidyr::pivot_longer(c(Absolut, Relativ), names_to = "selector", values_to = "wert")%>%
#     dplyr::mutate(selector = dplyr::case_when(
#       selector == "Relativ" ~ "In Prozent",
#       selector == "Absolut" ~ "Anzahl"
#     ))
#
#
#   df <- df %>% dplyr::filter(fachbereich == "MINT")
#
#   if(absolut_selector=="In Prozent"){
#
#     df <- df %>%
#       dplyr::filter(selector =="In Prozent")
#
#
#     # order years for plot
#     df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
#
#     title_help <- paste0(indikator_choice, "r")
#
#     # plot
#
#     titel <- paste0("Anteil weiblicher ", title_help, ", die MINT-Berufe wählen")
#     tooltip <- "{point.region} <br> Anteil: {point.y} %"
#     format <- "{value}%"
#     color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
#                "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
#     out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color)
#
#
#   }else if(absolut_selector=="Anzahl"){
#
#     title_help <- paste0(indikator_choice, "r")
#
#     hcoptslang <- getOption("highcharter.lang")
#     hcoptslang$thousandsSep <- "."
#     options(highcharter.lang = hcoptslang)
#
#     df <- df %>%
#       dplyr::filter(selector == "Anzahl")
#
#     df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
#
#
#     # plot
#
#     titel <- paste0("Anzahl weiblicher ", title_help, ", die MINT-Berufe wählen")
#     tooltip <- "Anzahl: {point.y}"
#     format <- "{value:, f}"
#     color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
#                "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
#     out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color)
#
#   }
#
# }
# Regionaler MINT Steckbrief ----
# Tab 1 ----

arbeitsmarkt_lk_detail_map <- function(r) {

  # load UI inputs from reactive value
  timerange <- r$date_beruf_arbeitsmarkt_landkreis_karte
  states <- r$states_beruf_arbeitsmarkt_landkreis_karte

  # input values for first map
  category_1 <- r$kategorie_beruf_arbeitsmarkt_landkreis_karte1
  domain_1 <- r$fachbereich_beruf_arbeitsmarkt_landkreis_karte1
  indikator_azubi_1 <- r$indikator1_beruf_arbeitsmarkt_landkreis_karte1
  indikator_besch_1 <- r$indikator2_beruf_arbeitsmarkt_landkreis_karte1


  # df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%


  df_query <- glue::glue_sql("
  SELECT *
  FROM arbeitsmarkt_detail
  where jahr = {timerange}
                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)

  df <- df %>%
    dplyr::select(-bereich)


  # map states for state codes
  # state_codes <- data.frame(
  #   state = c(
  #     "Baden-Württemberg",
  #     "Bayern",
  #     "Berlin",
  #     "Brandenburg",
  #     "Bremen",
  #     "Hamburg",
  #     "Hessen",
  #     "Mecklenburg-Vorpommern",
  #     "Niedersachsen",
  #     "Nordrhein-Westfalen",
  #     "Rheinland-Pfalz",
  #     "Saarland",
  #     "Sachsen",
  #     "Sachsen-Anhalt",
  #     "Schleswig-Holstein",
  #     "Thüringen"
  #   ),
  #   short = c(
  #     "bw",
  #     "by",
  #     "be",
  #     "bb",
  #     "hb",
  #     "hh",
  #     "he",
  #     "mv",
  #     "ni",
  #     "nw",
  #     "rp",
  #     "sl",
  #     "sn",
  #     "st",
  #     "sh",
  #     "th"
  #   )
  # )
  #
  # state_code <- state_codes %>% dplyr::filter(state == states) %>% dplyr::pull()

  # calculate comparison map 1

  df1_list <- calculate_landkreis(df, states, category_1, domain_1, indikator_azubi_1, indikator_besch_1)

  df1_map <- df1_list[[1]]
  titel_gesamt1 <- df1_list[[2]]
  titel_gesamt1_2 <- df1_list[[3]]
  titel_sub1 <- df1_list[[4]]
  titel_sub1_2 <- df1_list[[5]]


  # hilfe für Hover-Text
  if(category_1 == "Beschäftigte") {
    adjektiv_1 <- indikator_besch_1
  }else{
    adjketiv_1 <- indikator_azubi_1
  }


  # adjust landkreis_nummer for correct mapping
  # df1_map <- df1_map %>% dplyr::mutate(
  #   landkreis_nummer = paste0("de-", state_code, "-", landkreis_nummer, "000"))


  #Trennpunkte für lange Zahlen ergänzen in Absolute Zahlen für Hover + Text für Hover
  df1_map$wert <- prettyNum(df1_map$wert, big.mark = ".", decimal.mark = ",")
  domain_1 <- ifelse(domain_1 == "Alle", "alle Berufsbereiche", domain_1)

  quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

  # create plots





  # state_codes <- data.frame(
  #   state = c(
  #     "Baden-Württemberg","Bayern","Berlin","Brandenburg","Bremen","Hamburg",
  #     "Hessen","Mecklenburg-Vorpommern","Niedersachsen","Nordrhein-Westfalen",
  #     "Rheinland-Pfalz","Saarland","Sachsen","Sachsen-Anhalt",
  #     "Schleswig-Holstein","Thüringen"
  #   ),
  #   short = c("bw","by","be","bb","hb","hh","he","mv","ni",
  #             "nw","rp","sl","sn","st","sh","th")
  # )
  # state_code <- state_codes %>%
  #   dplyr::filter(state == states) %>%
  #   dplyr::pull(short)
  #
  # # RDS-Datei für das Bundesland laden
  # map_state <- readRDS(paste0("data/map_data/map_de_", state_code, ".rds"))
  #


  titel <- paste0("Anteil von ", titel_sub1_2, titel_gesamt1, titel_gesamt1_2, " in ", states, " (", timerange, ")")

  df1_map <- df1_map %>%
    dplyr::mutate(
      tooltip = paste0(
        "<b>", landkreis, "</b><br>",
        "Anteil: ", prettyNum(round(prob,1), big.mark = ".", decimal.mark = ","), " %<br>",
        "Anzahl: ", wert
      )
    )

  map1 <- mapbuilder_plotly(df = df1_map,
                            value_col = "prob",
                            regio_col = "landkreis",
                            mincolor = "#f4f5f6",
                            maxcolor = "#154194",
                            titel = titel,
                            quelle = quelle,
                            map = "germany_choropleth_landkreise.rds")

  return(map1)


}


# Tab 2 -------

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

  df_query <- glue::glue_sql("
  SELECT *
  FROM arbeitsmarkt_detail
  WHERE jahr = {timerange}
                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)

  df <- df %>%
    dplyr::select(-bereich)



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

  praep <- ifelse(states == "Saarland", " im ", " in ")


    df_compare <- df_compare %>%
    dplyr::mutate(landkreis = as.character(landkreis))

  if(display_form == "In Prozent") {

    order <- df_compare %>%
      dplyr::arrange(dplyr::desc(prob)) %>%
      dplyr::pull(landkreis) %>%
      unique()


    y <- "prob"

    titel <- paste0("Anteil von ", titel_sub2, titel_gesamt_1, titel_gesamt_2, praep, states, " (", timerange, ")")

  } else if(display_form== "Anzahl") {

    order <- df_compare %>%
      dplyr::arrange(dplyr::desc(wert)) %>%
      dplyr::pull(landkreis) %>%
      unique()

    y <- "wert"

    titel_gesamt_1 <- stringr::str_remove(titel_gesamt_1, "an allen")
    titel <- paste0("Anzahl ", titel_sub, titel_gesamt_1, praep, states, " (", timerange, ")")
  }




  x <- "landkreis"
  percent <- if (display_form == "In Prozent") TRUE else FALSE
  quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."


  landkreise <- unique(df_compare$landkreis)
  color <- setNames(rep("lightgray", length(landkreise)),landkreise)
  color["alle Landkreise"] <- "#b16fab"


  df_compare <- df_compare %>%
    dplyr::mutate(
      .tooltip = paste0(
        "<b><span style='font-size:15px;'>", landkreis, "</span></b><br>",
        "Anteil: ", prob, "%<br>",
        "Anzahl: ", (formatC(as.numeric(wert),format = "f",digits = 0,big.mark = "."))
      ))

  if(states %in% c("Bayern", "Baden-Württemberg", "Nordrhein-Westfalen", "Niedersachsen")) {
    height <- 1600
    titel_y <- 0.99
    quelle_y <- -0.02
  } else if(states %in% c("Berlin", "Bremen", "Hamburg", "Saarland")) {
    height <- 500
    titel_y <- 0.96
    quelle_y <- -0.12
  } else {
    height <- 900
    titel_y <- 0.97
    quelle_y <- -0.04
  }


  out <- balkenbuilder_plotly(df=df_compare, x=x, y=y, titel=titel, orientation = "h", group=NULL, color = color, height=height,
                              order = order, percent=percent, stacking=FALSE, titel_y=titel_y, quelle_y=quelle_y, quelle=quelle)




}

# Tab 3 -----
arbeitsmarkt_lk_verlauf <- function(r){

  zeit <- r$date_beruf_arbeitsmarkt_landkreis_verlauf
  t <- zeit[1]:zeit[2]
  regio <- r$states_beruf_arbeitsmarkt_landkreis_verlauf
  lk <- r$kreise_beruf_arbeitsmarkt_landkreis_verlauf
  lk_filter <- ifelse(lk == "Landesdurchschnitt", "alle Landkreise", lk)
  gruppe <- r$kategorie_beruf_arbeitsmarkt_landkreis_verlauf
  fach <- r$fachbereich_beruf_arbeitsmarkt_landkreis_verlauf
  absolut_selector <- r$abs_zahlen_beruf_arbeitsmarkt_landkreis_verlauf


  if(grepl("weiblich", gruppe)){
    gruppe <- gsub("weibliche ", "", gruppe)
    geschlecht_s <- "Frauen"
  }else{
    geschlecht_s <- "Gesamt"
  }

  df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
    dplyr::filter(jahr %in% t &
                    bundesland == regio &
                    landkreis %in% lk_filter &
                    geschlecht == geschlecht_s &
                    anforderung == "Gesamt" &
                    fachbereich == fach &
                    indikator == gruppe
    )%>%
    dplyr::select(indikator, bundesland, landkreis, fachbereich, jahr, wert) %>%
    dplyr::collect()



  if (absolut_selector == "In Prozent"){

    df_alle <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
      dplyr::filter(jahr %in% t &
                      bundesland == regio &
                      landkreis %in% lk_filter &
                      geschlecht == geschlecht_s &
                      anforderung == "Gesamt" &
                      fachbereich == "Alle" &
                      indikator == gruppe
      )%>%
      dplyr::select(indikator, bundesland, landkreis, fachbereich, jahr, wert) %>%
      dplyr::collect()

    df <- df %>%
      dplyr::left_join(df_alle, dplyr::join_by(indikator, landkreis, bundesland, jahr)) %>%
      dplyr::select(-fachbereich.y)%>%
      dplyr::rename(fachbereich = fachbereich.x,
                    wert = wert.x,
                    wert_ges = wert.y) %>%
      dplyr::mutate(prop = round(wert/wert_ges *100, 1))

    df$landkreis[df$landkreis == "alle Landkreise"] <- "Insgesamt"

    # order years for plot
    df <- df[with(df, order(landkreis, jahr, decreasing = FALSE)), ]

    # plot


    titel <- ifelse(regio == "Saarland",
                    paste0("Anteil der ", gruppe ," in ", fach, " im ", regio),
                    paste0("Anteil der ", gruppe ," in ", fach, " in ", regio))

    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", landkreis, "</b><br>",
          "Jahr: ", jahr, "<br>",
          "Anteil: ", prettyNum(prop, big.mark = ".", decimal.mark = ","), " %"
        )
      )

    color <- c("#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
               "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a", "#007655", "#dc6262",
               "#9d7265", "#5d335a", "#bfc6d3",  "#B45309","#d4c1bb", "#112c5f", "#8893a7")
    quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    out <- linebuilder_plotly(df, titel, x = "jahr", y = "prop", group = "landkreis", color=color, quelle = quelle)


  } else if(absolut_selector == "Anzahl") {

    df$landkreis[df$landkreis == "alle Landkreise"] <- "Landesdurchschnitt"


    # order years for plot
    df <- df[with(df, order(landkreis, jahr, decreasing = FALSE)), ]

    # plot

    titel <- paste0("Anzahl der ", gruppe ," in ", fach, " in ", regio)

    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", landkreis, "</b><br>",
          "Jahr: ", jahr, "<br>",
          "Anzahl: ", prettyNum(wert, big.mark = ".", decimal.mark = ",")
        )
      )
    format <- ",d"
    color <- c("#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
               "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a", "#007655", "#dc6262",
               "#9d7265", "#5d335a", "#bfc6d3",  "#B45309","#d4c1bb", "#112c5f", "#8893a7")
    queleeee <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    out <- linebuilder_plotly(df, titel, x = "jahr", y = "wert", group = "landkreis", format = format, color = color, quelle=queleeee)

  }
}


## Entgelte Box 5 ---------------

### Tab 1 --------

entgelte_vergleich_1 <- function(r) {

  geschlecht <- r$beruf_arbeitsmarkt_entgel_geschlecht
  datum <- r$date_arbeitsmarkt_entgelt_vergleich
  land <- r$region_arbeitsmarkt_entgelt_vergleich
  # berufsleb <- r$beruf_arbeitsmarkt_entgelt_berufslev
  berufsleb <- "Gesamt"



  df_query <- glue::glue_sql("
  SELECT *
  FROM arbeitsmarkt_entgelte
  WHERE geschlecht = {geschlecht}
  AND jahr = {datum}
  AND bundesland = {land}
  AND berufslevel = 'Gesamt'
                               ", .con = con)


  df <- DBI::dbGetQuery(con, df_query)

  df1 <- df %>%
    dplyr::filter(stringr::str_detect(wert, "^[0-9.,]+$")) %>%
     dplyr::filter(berufsgruppe == beruf) %>%
     dplyr::mutate(wert = round(as.numeric(wert)),0)

 df1 <- df1[with(df1, order(wert, decreasing = TRUE)),]

  if(berufsleb == "Gesamt"){
    berufsleb <- "alle Berufslevel"
  } else if (berufsleb == "Fachkraft"){
    berufsleb <- "Fachkräften"
  } else if (berufsleb == "Spezialist"){
    berufsleb <- "Spezialisten"
  } else if (berufsleb == "Experte"){
    berufsleb <- "Experten"
  }


  if(geschlecht == "Insgesamt"){
    geschlecht <- "alle Geschlechter"
  }






  df1$wert <- as.numeric(df1$wert)
  df1$berufsgruppe <- as.factor(df1$berufsgruppe)

#clear
  df1 <-  df1 %>%
    dplyr::filter(!(grepl("technik$", berufsgruppe, ignore.case = TRUE) & berufsgruppe != "Technik"))

  praep <- ifelse(land == "Saarland", " im ", " in ")

  titel <- paste0("Mittleres Entgelt nach Berufsfeldern", praep, land, " ", datum,
                  " (", berufsleb, ", ", geschlecht, ")" )

  quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."



  order <- unique(df1$berufsgruppe)

  df1 <- df1 %>%
    dplyr::mutate(
      .tooltip = paste0(
        "<b><span style='font-size:15px;'>", berufsgruppe, "</span></b><br>",
        "Wert in Euro: ", (formatC(as.numeric(wert), format = "f", digits = 0, big.mark = "."))
      ))


  x <- "berufsgruppe"
  y <- "wert"
  color <- c( "Insgesamt" = "#b16fab", "MINT-Berufe" = "#b16fab", "Keine MINT-Berufe" = "#b16fab",
              "Informatik" = "#efe8e6", "Mathematik, Naturwissenschaften" = "#efe8e6", "Technik" = "#efe8e6" )


  out <- balkenbuilder_plotly(df=df1, x=x, y=y, titel=titel, orientation = "h",percent=FALSE,
                              group=NULL, color=color,
                              order=order, stacking = FALSE, quelle=quelle)




  return(out)

}



### Tab 2 -------

entgelte_verlauf_1 <- function(r) {


  datum <- r$date_arbeitsmarkt_entgelt_verlauf
  land <- r$region_arbeitsmarkt_entgelt_verlauf
  #berufsleb <- r$indikator_arbeitsmarkt_entgelt_verlauf_2
  berufsleb <- "Gesamt"
  geschlecht <- r$abs_zahlen_arbeitsmarkt_entgelt_verlauf

  datum1 <- datum[1]:datum[2]


  if(geschlecht == "Ingesamt") {
    geschlecht <- "Insgesamt"
  }

  df_query <- glue::glue_sql("
  SELECT *
  FROM arbeitsmarkt_entgelte
  WHERE geschlecht = {geschlecht}
  AND jahr IN  ({datum1*})
  AND bundesland = {land}
  AND berufslevel = {berufsleb}
                               ", .con = con)


  df <- DBI::dbGetQuery(con, df_query)

  df <- df %>%
    dplyr::mutate(wertq = readr::parse_number(wert))


#  df <- na.omit(df)

  df <- df %>%
    dplyr::filter(
      berufsgruppe %in% c("MINT-Berufe", "Insgesamt", "Keine MINT-Berufe",
                          "Informatik","Technik", "Mathematik, Naturwissenschaften")
    ) %>%
    dplyr::filter(
      berufsgruppe == beruf
    )

  if(geschlecht == "Insgesamt"){
    geschlecht = "alle Geschlechter"
  }
  if(berufsleb == "Gesamt") {
    berufsleb = "alle Berufslevel"
  }

  sorted_indicators <- df %>%
    dplyr::group_by(berufsgruppe) %>%
    dplyr::summarize(m_value = mean(round(wertq, 1), na.rm = TRUE)) %>%
    dplyr::arrange(m_value) %>%
    dplyr::pull(berufsgruppe)

  df$berufsgruppe <- factor(df$berufsgruppe, levels = sorted_indicators)


  titel <- paste0("Entwicklung der Entgelte in den verschiedenen Kategorien in ", land, " (", geschlecht, ",", " ", berufsleb, ")")

  df <- df %>%
    dplyr::mutate(
      tooltip = paste0(
        "<b>", berufsgruppe, "</b><br>",
        "Jahr: ", jahr, "<br>",
        "Wert in Euro: ", prettyNum(round(wertq,digits = 0), big.mark = ".", decimal.mark = ",")
      )
    )
  format <- ",d"
  color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
             "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")[1:length(unique(df$berufsgruppe))]

  quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."


  df <- df[order(df$jahr, decreasing = FALSE), ]

  out <- linebuilder_plotly(df, titel, x = "jahr", y = "wertq", group = "berufsgruppe", format = format, color = color, quelle = quelle)


}



### Tab 3 -------

entgelte_balken_1 <- function(r) {

  # inf1 <- r$ansicht_balken_entgelt
  jahr <- r$date_balken_entgelt
  bulasa <- r$states_balken_entgelt
  status <- r$status_balken_entgelt

  identi <- r$abs_zahlen_balken_entgelt

  jahr <- as.numeric(jahr)



  df_query <- glue::glue_sql("
  SELECT *
  FROM arbeitsmarkt_entgelte
  WHERE geschlecht = 'Insgesamt'
  AND jahr = {jahr}
  AND bundesland = {bulasa}
  AND berufslevel = {status}
  AND berufsgruppe IN ({identi*})
                               ", .con = con)



  df <- DBI::dbGetQuery(con, df_query)

  df <- df %>%
    dplyr::filter(
     beruf == berufsgruppe
    )




  titel <- paste0("MINT-Anteil in")
  tooltip <- paste('Wert {point.x}')
  format <- "{wert}"

  quelle <- ""

  out <- balkenbuilder(df, titel, x = "wert", y = "berufsgruppe", tooltip = tooltip,  color =  c("#b16fab","#b16fab"), format = format , quelle = quelle)

}

### Tab 4 -------


plot_ranking_top_entgeltee <- function(r) {

  info1 <- r$date_top_entgelt
  inf22 <- r$states_top_entgelt
  inf23 <- r$subject_top_entgelt
  inf_4 <- r$subject_abs_rel_engelt

}


