library(DBI)
library(dplyr)
library(dbplyr)


# Wer wählt MINT ----



#' A function to plot time series
#'
#' @description A function to plot a bar chart
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

kurse_einstieg_comparison <- function(r) {

  # load UI inputs from reactive value
  timerange <- r$date_kurse_einstieg_comparison
  regio <- r$region_kurse_einstieg_comparison
  betrachtung <- r$ansicht_kurse_einstieg_comparison

    df_query <- glue::glue_sql("
    SELECT *
    FROM kurse
    WHERE jahr IN ({timerange*})
    AND region = {regio}
    AND fachbereich IN ('MINT', 'andere Fächer')
    AND anzeige_geschlecht = 'Gesamt'
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::select(-region, -jahr, -bereich) %>%
      dplyr::group_by(indikator) %>%
      dplyr::mutate(sum_wert = sum(wert))


  # calculate proportions
  df1 <- df %>% dplyr::group_by(indikator, fachbereich) %>%
    dplyr::mutate(proportion = wert/sum_wert)

  df1$proportion <- df1$proportion * 100

  #Trennpunkte für lange Zahlen ergänzen
  df1$wert <- prettyNum(df1$wert, big.mark = ".", decimal.mark = ",")

  #als Faktor für Darstellung
  df1$fachbereich <- as.factor(df1$fachbereich)
  df1$fachbereich <- factor(df1$fachbereich, levels = c("MINT", "andere Fächer"))

  # plot
  if(betrachtung == "Gruppenvergleich - Balkendiagramm"){

   df1 <- df1[with(df1, order(round(proportion,1), decreasing = FALSE)),]


   #forcats, daher nicht in balkenbuilder
 out <-  highcharter::hchart(df1, 'bar', highcharter::hcaes(y = round(proportion,1), x = indikator, group = forcats::fct_rev(fachbereich))) %>%
    highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
    highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
    highcharter::hc_title(text = paste0("Anteil von MINT-Belegungen an allen Belegungen in ", regio, " (", timerange,")"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "Calibri Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
   highcharter::hc_exporting(enabled = TRUE,
                             buttons = list(
                               contextButton = list(
                                 menuItems = list("downloadPNG", "downloadCSV")
                               )
                             )
   )

 # titel <- paste0("Anteil von MINT-Belegungen an allen Belegungen in ", regio, " (", timerange,")")
 # tooltip <- "Fachbereich: {point.fachbereich} <br> Anteil: {point.y} % <br> Anzahl: {point.wert}"
 # format <- "{value}%"
 # color <- c("#efe8e6", "#b16fab")
 # optional <- list(bar = list(stacking = "percent"))
 #
 # out <- balkenbuilder(df1, titel, x = "indikator", y="proportion", group="fachbereich", tooltip, color, format, optional)




  }else if(betrachtung == "Einzelansicht - Kuchendiagramm"){

    indika <- r$indikator_kurse_einstieg_comparison

    df1 <- df1 %>%
      dplyr::filter(indikator == indika)
    df1$proportion <- round(df1$proportion, 1)

    df1 <- df1[with(df1, order(fachbereich, decreasing = FALSE)), ]

    titel_help <- sprintf("den %sn", indika)
    titel_help <- ifelse(indika == "Oberstufenbelegungen", "der Oberstufe",
                         titel_help)

    df1$wert <- as.numeric(as.character(df1$wert)) #wert ist charakter

    titel <- paste0("MINT-Anteil in ", titel_help,  " in ", regio, " (", timerange, ")")
    tooltip <- paste('Anteil: {point.proportion} % <br> Anzahl: {point.wert}')
    format <- "{point.proportion} %"

    quelle <- "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    out <- piebuilder(df1, titel, x = "fachbereich", y = "proportion", tooltip,  color =  c("#b16fab", "#efe8e6"), format, quelle = quelle)

  }

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

kurse_verlauf_single <- function(r) {

  # load UI inputs from reactive value
  timerange <- r$date_kurse_einstieg_verlauf
  t <- as.numeric(timerange[1]:timerange[2])
  regio <- r$region_kurse_einstieg_verlauf
  absolut_selector <- r$abs_zahlen_kurse_einstieg_verlauf

    df_query <- glue::glue_sql("
                               SELECT *
                               FROM kurse
                               WHERE jahr IN ({t*})
                               AND region = {regio}
                               AND anzeige_geschlecht = 'Gesamt'
                               AND fachbereich IN ('MINT', 'Alle Fächer')
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::select(-bereich, -region, -anzeige_geschlecht)

  df_alle <- subset(df, df$fachbereich == "Alle Fächer")
  df_alle <- df_alle %>%
    dplyr::rename(wert_all = wert) %>%
    dplyr::select(-fachbereich)

  df <- df %>%
    dplyr::filter(fachbereich == "MINT")%>%
    dplyr::select(-fachbereich)

  df <- df %>%
    dplyr::left_join(df_alle, by = c("indikator", "jahr")) %>%
    dplyr::mutate(prop = wert/wert_all*100)

  if(absolut_selector == "In Prozent"){

    # order years for plot
   df <- df[with(df, order(jahr, decreasing = FALSE)), ]

    # plot
    titel <- paste0("Anteil von MINT-Belegungen an allen Belegungen in ", regio)
    tooltip <- "Anteil: {point.indikator} <br> Wert: {point.y} %"
    format <- "{value}%"
    color <- c("#b16fab", "#154194","#66cbaf")

    quelle <- "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    out <- linebuilder(df, titel, x = "jahr", y = "prop", group = "indikator", tooltip, format, color, quelle = quelle)


  } else if (absolut_selector=="Anzahl") {

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    # order years for plot
    df <- df[with(df, order(jahr, decreasing = FALSE)), ]

    titel <- paste0("Anzahl an MINT-Belegungen in der Schule in ", regio)
    tooltip <- "Anzahl: {point.y}"
    format <-  "{value:, f}"
    color <- c("#b16fab", "#154194","#66cbaf")
    quelle <- "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "indikator", tooltip, format, color, quelle = quelle)


  }
 return(out)

}

  #' A function to plot the german map
#'
#' @description A function to plot the german map with all states that contain
#' information about the share of women in STEM
#'
#' @return The return value is the german map with information
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

kurse_mint_map <- function(r) {
  betrachtung <- r$ansicht_mint_map

  if(betrachtung == "Zeitverlauf - Liniendiagramm"){

    # load UI inputs from reactive value
    absolut_selector <- r$abs_zahlen_kurse_verlauf_mint

    timerange <- r$date_kurse_verlauf_mint
    t <- as.numeric(timerange[1]:timerange[2])

    states <- r$states_kurse_verlauf_mint

    indikator_select <- r$topic_selected_mint

    # filter dataset based on UI inputs
    # df <- dplyr::tbl(con, from = "kurse") %>%
    #   dplyr::filter(jahr %in% t,
    #                 indikator == indikator_select,
    #                 anzeige_geschlecht == "Gesamt",
    #                 fachbereich %in% c("MINT", "Alle Fächer")) %>%
    #   dplyr::select(indikator, fachbereich, anzeige_geschlecht, region, jahr, wert)%>%
    #   dplyr::collect()


    df_query <- glue::glue_sql("
                               SELECT indikator, fachbereich, anzeige_geschlecht, region, jahr, wert
                               FROM kurse
                               WHERE jahr IN ({t*})
                               AND indikator = {indikator_select}
                               AND anzeige_geschlecht = 'Gesamt'
                               AND fachbereich IN ('MINT', 'Alle Fächer')
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

    # include "Osten" und "Westen" in Dataframe
    #df <- prep_kurse_east_west(df)

    df <- df %>%
      dplyr::mutate(fachbereich = dplyr::case_when(fachbereich == "MINT"~ "MINT-Fächer (gesamt)",
                                                   fachbereich == "andere Fächer" ~ "andere Fächer (gesamt)",
                                                   T~ fachbereich))%>%
      dplyr::group_by(indikator, anzeige_geschlecht, region, jahr)%>%
      dplyr::summarise(fachbereich, indikator, anzeige_geschlecht, region, jahr, wert,
                       sum_props = wert[fachbereich == "Alle Fächer"])%>%
      dplyr::ungroup() %>%
      dplyr::filter(fachbereich == "MINT-Fächer (gesamt)")

    # calculate proportions
    df <- df %>% dplyr::group_by(jahr, region, indikator) %>%
      dplyr::summarize(wert, proportion = wert/sum_props)%>%
      dplyr::rename(Relativ = proportion, Absolut=wert)%>%
      tidyr::pivot_longer(c(Absolut, Relativ), names_to = "selector", values_to = "wert")%>%
      dplyr::mutate(selector = dplyr::case_when(
        selector == "Relativ" ~ "In Prozent",
        selector == "Absolut" ~ "Anzahl"
      ))%>%
      dplyr::ungroup()%>%
      dplyr::mutate(region=dplyr::case_when(
        region == "Westen" ~ "Westdeutschland (o. Berlin)",
        region == "Osten" ~ "Ostdeutschland (inkl. Berlin)",
        T ~ .$region
      ))

    # fitler states
    df <- df %>% dplyr::filter(region %in% states)

    if(absolut_selector=="In Prozent"){

      df <- df %>%
        dplyr::filter(selector=="In Prozent")


      df$wert <- df$wert * 100


      if(indikator_select == "Grundkurse") {

        title_help <- "Grundkursbelegungen"

      }else {

        title_help <- "Leistungskursbelegungen"

      }

      # order years for plot
      df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]


      help_title <-  "MINT-Fächern"

      # plot

      titel <- paste0("Anteil von ", help_title, " an allen ", title_help)
      tooltip <- "MINT-Anteil in {point.region} <br> Wert: {point.y} %"
      format <-  "{value}%"
      color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                 "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5")

      quelle <- "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
      out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color, quelle = quelle)

    } else if(absolut_selector =="Anzahl"){

      hcoptslang <- getOption("highcharter.lang")
      hcoptslang$thousandsSep <- "."
      options(highcharter.lang = hcoptslang)

      df <- df %>%
        dplyr::filter(selector=="Anzahl")



      if(indikator_select == "Grundkurse") {

        title_help <- "Grundkursbelegungen "

      }else {

        title_help <- "Leistungskursbelegungen "

      }

      # order years for plot
      df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

      help_title <- "in MINT"

      # plot

      titel <- paste0("Anzahl an ", title_help ,help_title)
      tooltip <- "{point.region} <br> Anzahl: {point.y}"
      format <-  "{value:, f}"
      color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                 "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5")
      quelle <- "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
      out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color, quelle = quelle)
    }


    return(out)

  }
  else if(betrachtung == "Übersicht - Kartendiagramm"){

    # load UI inputs from reactive value
    timerange <- r$date_mint_map

    # SQL: DONE
    # filter dataset based on UI inputs
    # df <- dplyr::tbl(con, from = "kurse") %>%
    #   dplyr::filter(jahr == timerange,
    #                 region != "Deutschland",
    #                 anzeige_geschlecht == "Gesamt",
    #                 fachbereich %in% c("MINT", "Alle Fächer")) %>%
    #   dplyr::select(fachbereich, indikator, anzeige_geschlecht, region, jahr,wert)%>%
    #   dplyr::collect()


    df_query <- glue::glue_sql("
                               SELECT fachbereich, indikator, anzeige_geschlecht, region, jahr,wert
                               FROM kurse
                               WHERE jahr IN ({timerange*})
                               AND NOT region = 'Deutschland'
                               AND anzeige_geschlecht = 'Gesamt'
                               AND fachbereich IN ('MINT', 'Alle Fächer')
                               ", .con =con)

    df <- DBI::dbGetQuery(con, df_query)


    df <- df %>%
      dplyr::mutate(fachbereich = dplyr::case_when(fachbereich == "MINT"~ "MINT-Fächer (gesamt)",
                                                   fachbereich == "andere Fächer" ~ "andere Fächer (gesamt)",
                                                   T~ fachbereich))%>%
      dplyr::group_by(indikator, anzeige_geschlecht, region, jahr)%>%
      dplyr::summarise(fachbereich, indikator, anzeige_geschlecht, region, jahr, wert,wert_sum = wert[fachbereich == "Alle Fächer"])%>%
      dplyr::ungroup()%>%
      dplyr::filter(fachbereich != "Alle Fächer")

    #calculate proportions
    df <- df %>% # dplyr::group_by(region, indikator) %>%
      dplyr::mutate(proportion = wert/wert_sum*100)

    help_title <- "MINT-Fächern"

    #Extra gerundeten Proportions-Wert erstellen, für Anzeige in Hover
    df$prop <- df$proportion
    df$prop <- round(df$prop, 1)

    #Trennpunkte für lange Zahlen ergänzen
    df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

    # plots


    df1 <- df[df$indikator == "Grundkurse",]
    joinby <- c("name", "region")
    name <- paste0("MINT-Anteil")
    tooltip <- "{point.region} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}"
    titel <- paste0("Anteil von ", help_title, "<br> an allen Grundkursbelegungen ", "(",timerange, ")")
    mincolor <- "#f4f5f6"
    map_selection <- 1
    maxcolor <- "#b16fab"
    quelle <- "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    map1 <- mapbuilder(df1, joinby,name, tooltip, titel, mincolor, maxcolor,prop=FALSE, wert=FALSE, map=map_selection, quelle = quelle)

    df2 <- df[df$indikator == "Leistungskurse",]
    joinby <- c("name", "region")
    name <- paste0("MINT-Anteil")
    tooltip <- "{point.region} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}"
    titel <- paste0("Anteil von ", help_title, "<br> an allen Leistungskursbelegungen ", "(",timerange, ")")
    mincolor <- "#f4f5f6"
    map_selection <- 1
    maxcolor <- "#b16fab"
    quelle <- "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    map2 <- mapbuilder(df2, joinby,name, tooltip, titel, mincolor, maxcolor,prop=FALSE, wert=FALSE, map=map_selection, quelle = quelle)


    list <- list(map1,map2)
    out <- highcharter::hw_grid(
      list,
      ncol = 2)
  }

  return(out)

}


# M-I-N-T ----

#' A function to plot a waffle chart
#'
#' @description A function to create a waffle chart for the second box inside the
#' tab "Schule".
#'
#' @return The return value is a waffle chart
#' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

kurse_waffle_mint <- function(r) {

  betrachtung <- r$ansicht_kurse_mint
  timerange <- r$date_kurse_mint
  regio <- r$region_kurse_mint
  ebene <- r$ebene_kurse_mint
  indika <- r$indikator_kurse_mint

  color_fach <- c(
    "Informatik" = "#00a87a",
    "Naturwissenschaften" = "#fcc433",
    "Biologie" = "#fbbf24",
    "Chemie" = "#D97706",
    "Physik" = "#F59E0B",
    "andere naturwiss.-technische Fächer" =  "#fde68a",
    "Mathematik" = "#ee7775",
    "Andere Fächer" = "#efe8e6"
  )

  if(betrachtung == "Einzelansicht - Kuchendiagramm"){

    # df <- dplyr::tbl(con, from = "kurse") %>%
    #   dplyr::filter(jahr == timerange,
    #                 region == regio,
    #                 anzeige_geschlecht == "Gesamt",
    #                 indikator == indika) %>%
    #   dplyr::select(indikator, fachbereich, anzeige_geschlecht, wert) %>%
    #   dplyr::collect()

    query_df <- glue::glue_sql("
    SELECT indikator, fachbereich, anzeige_geschlecht, wert
    FROM kurse
    WHERE jahr IN ({timerange*})
    AND region = {regio}
    AND anzeige_geschlecht = 'Gesamt'
    AND indikator = {indika}
    ", .con = con)

    df <- DBI::dbGetQuery(con, query_df)

    if(ebene == "MINT-Fachbereiche"){
      
      # combine subjects to get numbers on share of MINT
      # make a function out of it
      subjects_not <- c("Mathematik", "Informatik",  "Biologie", "Chemie",
                        "Physik", "andere naturwiss.-technische Fächer")

      values_natwi <- df %>%
        dplyr::filter(fachbereich %in% c("Biologie", "Chemie", "Physik", "andere naturwiss.-technische Fächer" )) %>%
        dplyr::group_by( indikator, anzeige_geschlecht) %>%
        dplyr::summarise(wert = sum(wert)) %>%
        dplyr::mutate(fachbereich = "Naturwissenschaften") %>%
        dplyr::ungroup()

      df <- rbind(df, values_natwi)

      df <- df %>% dplyr::filter(fachbereich %in% c("Mathematik", "Informatik",
                                                    "Naturwissenschaften", "Andere Fächer"))


    }else if(ebene == "MINT-Fächer"){

      df <- df %>% dplyr::filter(fachbereich %in% c("Mathematik", "Informatik", "Physik", "Chemie",
                                                    "Biologie", "andere naturwiss.-technische Fächer",
                                                    "Andere Fächer"))
    }


    # extract new "Gesamt"
    df <- df %>% dplyr::group_by(indikator) %>%
      dplyr::mutate(props = sum(wert, na.rm = TRUE))

    # calculate proportion
    df <-  df %>% dplyr::group_by(fachbereich, indikator) %>%
      dplyr::mutate(proportion = round(wert/props *100, 1),
                    wert = prettyNum(wert, big.mark = ".", decimal.mark = ","))

    df <- na.omit(df)

    df <- df[with(df, order(proportion, decreasing = FALSE)), ]
    df <- df %>%
      dplyr::mutate(color = color_fach[fachbereich])

    titel_help <- sprintf("den %sn", indika)
    titel_help <- ifelse(indika == "Oberstufenbelegungen", "der Oberstufe",
                         titel_help)


    titel <- paste0("MINT-Fächeranteile in ", titel_help , " in ", regio, " (", timerange, ")")
    tooltip <- paste('Anteil: {point.proportion}% <br> Anzahl: {point.wert}')
    color <- as.character(df$color)
    format <-"{point.proportion}  %"

    quelle <- "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    out <- piebuilder(df, titel, x = "fachbereich", y = "proportion", tooltip, color, format, quelle = quelle)

  }
  else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){

    # filter dataset based on UI inputs
    # df <- dplyr::tbl(con, from = "kurse") %>%
    #   dplyr::filter(anzeige_geschlecht == "Gesamt",
    #                 jahr == timerange) %>%
    #   dplyr::select(-bereich)%>%
    #   dplyr::collect()

    df_query <- glue::glue_sql("
                               SELECT *
                               FROM kurse
                               WHERE anzeige_geschlecht = 'Gesamt'
                               AND jahr IN ({timerange*})
                               ", .con = con)
    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::select(-bereich) %>%
      dplyr::mutate(fachbereich = dplyr::case_when(fachbereich == "MINT"~ "MINT-Fächer (gesamt)",
                                                   fachbereich == "andere Fächer" ~ "andere Fächer (gesamt)",
                                                   T~ fachbereich))%>%
      dplyr::group_by(indikator, anzeige_geschlecht, region, jahr)%>%
      dplyr::summarise(fachbereich, indikator, anzeige_geschlecht, region, jahr, wert,wert_gesamt = wert[fachbereich == "Alle Fächer"])%>%
      dplyr::ungroup()%>%
      dplyr::filter(fachbereich != "Alle Fächer")%>%
      dplyr::mutate(proportion = (wert/wert_gesamt)*100)

    # Bei Leistungskurse: Religion/Ethik raus, da minimal
    if(indika == "Leistungskurse"){

      df <- df %>%
        dplyr::filter(fachbereich!="Religion/Ethik")

    }

    df <- df %>% dplyr::filter(indikator == indika)

    df <- na.omit(df)

    df <- df %>%
      dplyr::ungroup()%>%
      dplyr::mutate(region= dplyr::case_when(
        region == "Westen" ~ "Westdeutschland (o. Berlin)",
        region == "Osten" ~ "Ostdeutschland (inkl. Berlin)",
        T ~ .$region
      ))

    df <- df %>% dplyr::filter(region == regio)

    x <- c("MINT-Fächer (gesamt)", "Mathematik", "Informatik", "Physik", "Chemie",
           "Biologie", "andere naturwiss.-technische Fächer",
           "andere Fächer (gesamt)", "Deutsch", "Fremdsprachen", "Gesellschaftswissenschaften",
           "Musik/Kunst", "Religion/Ethik", "Sport")


    df <- df %>%
      dplyr::mutate(fachbereich =  factor(fachbereich, levels = x)) %>%
      dplyr::arrange(fachbereich)

    #Trennpunkte für lange Zahlen ergänzen
    df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

    df <- df %>% dplyr::filter(fachbereich != "andere naturwiss.-technische Fächer")

    #Anordnen mit / ohne Religion
    c <- c("MINT-Fächer (gesamt)", "Mathematik", "Informatik", "Physik", "Chemie",
           "Biologie",
           "andere Fächer (gesamt)", "Deutsch", "Fremdsprachen", "Gesellschaftswissenschaften",
           "Musik/Kunst", "Religion/Ethik", "Sport")

    color_fach <- c(
      "MINT-Fächer (gesamt)" = "#D4C1BB",
      "Mathematik" = "#ee7775",
      "Informatik" = "#00a87a",
      "Physik" = "#F59E0B",
      "Chemie" = "#D97706",
      "Biologie" = "#fcc433",
      "andere Fächer (gesamt)"  = "#D4C1BB",
      "Deutsch"= "#efe8e6",
      "Fremdsprachen"= "#efe8e6",
      "Gesellschaftswissenschaften" = "#efe8e6",
      "Musik/Kunst" = "#efe8e6",
      "Religion/Ethik"= "#efe8e6",
      "Sport"= "#efe8e6"
    )

    if(indika=="Leistungskurse"){
      c <- c("MINT-Fächer (gesamt)", "Mathematik", "Informatik", "Physik", "Chemie",
             "Biologie",
             "andere Fächer (gesamt)", "Deutsch", "Fremdsprachen", "Gesellschaftswissenschaften",
             "Musik/Kunst", "Sport")
      color_fach <- c(
        "MINT-Fächer (gesamt)" = "#D4C1BB",
        "Mathematik" = "#ee7775",
        "Informatik" = "#00a87a",
        "Physik" = "#F59E0B",
        "Chemie" = "#D97706",
        "Biologie" = "#fcc433",
        "andere Fächer (gesamt)"  = "#D4C1BB",
        "Deutsch"= "#efe8e6",
        "Fremdsprachen"= "#efe8e6",
        "Gesellschaftswissenschaften" = "#efe8e6",
        "Musik/Kunst" = "#efe8e6",
        "Religion/Ethik"= "#efe8e6",
        "Sport"= "#efe8e6"
      )
    }


    #Vorbereitung für Überschrift
    if(indika=="Leistungskurse"){
      indika <- "Leistungskurs"
    } else if(indika == "Grundkurse") {
      indika <- "Grundkurs"
    } else {
      indika <- "Oberstufen"
    }

    # # # plot

    #nicht in bar wegen categories
    out <- highcharter::hchart(df, 'bar', highcharter::hcaes(y = round(proportion,1), x = fachbereich))%>%
      highcharter::hc_tooltip(pointFormat = "Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
      highcharter::hc_xAxis(title = list(text = ""), categories = c
      ) %>%
      highcharter::hc_plotOptions(bar = list(
        colorByPoint = TRUE,
       colors = as.character(color_fach)
      )) %>%
      highcharter::hc_title(text = paste0( "Anteil von ", indika, "-Belegungen nach Fächern in ", regio, " (", timerange, ")"
      ),
      margin = 45,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
      highcharter::hc_caption(text = "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
                              style = list(fontSize = "11px", color = "gray")) %>%
      highcharter::hc_exporting(enabled = TRUE,
                                buttons = list(
                                  contextButton = list(
                                    menuItems = list("downloadPNG", "downloadCSV")
                                  )
                                )
      )
  }


  return(out)


}

#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

kurse_verlauf_subjects_bl <- function(r) {

  # load UI inputs from reactive value

  absolut_selector <- r$abs_zahlen_kurse_verlauf_subject_bl
  timerange <- r$date_kurse_verlauf_subject_bl
  t <- as.numeric(timerange[1]:timerange[2])
  states <- r$states_kurse_verlauf_subject_bl
  indikator_kurse <- r$topic_selected_subject_bl
  subjects_select <- r$subject_selected_bl_sub

  color_fach <- c(
    "MINT-Fächer (gesamt)" = "#b16fab",
    "Informatik" = "#00a87a",
    "Naturwissenschaften" = "#fcc433",
    "Biologie" = "#fbbf24",
    "Chemie" = "#D97706",
    "Physik" = "#F59E0B",
    "andere naturwiss.-technische Fächer" =  "#fde68a",
    "Mathematik" = "#ee7775",
    "andere Fächer (gesamt)" = "#D4C1BB",
    "Deutsch"= "#9D7265",
    "Fremdsprachen"= "#112C5F",
    "Gesellschaftswissenschaften" = "#D4C1BB",
    "Musik/Kunst" = "#5f94f9",
    "Religion/Ethik"= "#725249",
    "Sport"= "#154194"
  )

  df_query <- glue::glue_sql("
  SELECT fachbereich, indikator, anzeige_geschlecht, region, jahr, wert
  FROM kurse
  WHERe jahr IN ({t*})
  AND indikator = {indikator_kurse}
  AND region IN ({states*})
                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)




  df <- df %>%
    dplyr::mutate(fachbereich = dplyr::case_when(fachbereich == "MINT"~ "MINT-Fächer (gesamt)",
                                                 fachbereich == "andere Fächer" ~ "andere Fächer (gesamt)",
                                                 T~ fachbereich))%>%
    tidyr::pivot_wider(names_from = anzeige_geschlecht, values_from = wert)%>%
    dplyr::mutate(Gesamt = dplyr::case_when(region == "Baden-Württemberg" ~ Gesamt,
                                            T ~ Männer + Frauen))%>%
    dplyr::select(- Männer, - Frauen)%>%
    tidyr::pivot_longer(Gesamt, names_to = "anzeige_geschlecht", values_to = "wert")%>%
    dplyr::select(-anzeige_geschlecht)%>%
    dplyr::group_by(indikator, region, jahr)%>%
    dplyr::summarise(fachbereich, indikator,  region, jahr, wert, sum_props = wert[fachbereich == "Alle Fächer"])%>%
    dplyr::ungroup()%>%
    dplyr::filter(fachbereich != "Alle Fächer")


  df <- df %>% dplyr::filter(fachbereich %in% subjects_select)

  # calculate proportions

  df <- df %>%
    dplyr::mutate(Relativ = round((wert/ sum_props*100),1))%>%
    dplyr::rename(Absolut = wert)%>%
    tidyr::pivot_longer(c(Absolut, Relativ), names_to = "selector", values_to = "wert")%>%
    dplyr::mutate(selector= dplyr::case_when(
      selector == "Absolut" ~ "Anzahl",
      selector == "Relativ" ~ "In Prozent"
    ))%>%
    dplyr::ungroup()%>%
    dplyr::mutate(region= dplyr::case_when(
      region == "Osten" ~ "Ostdeutschland (inkl. Berlin)",
      region == "Westen" ~ "Westdeutschland (o. Berlin)",
      T ~ .$region
    ))


  # fitler states

  #titel hilfe für Plot
  kurs_help <- ifelse(indikator_kurse == "Grundkurse", "Grundkursbelegungen", "Leistungskursbelegungen")


  if(absolut_selector=="In Prozent"){

    df <- df %>%
      dplyr::filter(selector == "In Prozent")

    df <- df %>% dplyr::mutate(color = color_fach[(fachbereich)])

    sorted_subjects <- df %>%
      dplyr::group_by(fachbereich) %>%
      dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
      dplyr::arrange(desc(m_value)) %>%
      dplyr::pull(fachbereich)

    df$fachbereich <- factor(df$fachbereich, levels = sorted_subjects)
    df <- df %>% dplyr::arrange(fachbereich)

    # order years for plot
    df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

    # plot
    quelle <- "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    titel <- paste0("Anteil einzelner Fächer an den ", kurs_help ," in ", states)
    tooltip <-  "{point.fachbereich} <br> Anteil: {point.y} %"
    format <-  "{value}%"
    color <- as.character(df$color)
    
    out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "fachbereich", tooltip, format, color, quelle = quelle)

  } else if (absolut_selector=="Anzahl"){

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    df <- df %>%
      dplyr::filter(selector == "Anzahl")

    df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

    quelle2 <- "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    titel <- paste0("Anzahl der ", kurs_help, " in einzelnen Fächern in ", states)
    tooltip <-  "{point.fachbereich} <br> Anzahl: {point.y}"
    format <-  "{value:, f}"
    color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#d0a9cd",
               "#bfc6d3", "#5f94f9", "#B45309")
    out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "fachbereich", tooltip, format, color, quelle = quelle2)

  }

  return(out)
}
   

#' A function to plot the german map
#'
#' @description A function to plot the german map with all states that contain
#' information about the share of women in STEM
#'
#' @return The return value is the german map with information
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

kurse_map <- function(r) {

  # load UI inputs from reactive value
  betrachtung <- r$ansicht_map
  timerange <- r$date_map
  subjects <- r$subject_map

  color_fach <- c(
    "MINT-Fächer (gesamt)" = "#b16fab",
    "Informatik" = "#00a87a",
    "Naturwissenschaften" = "#fcc433",
    "Biologie" = "#fbbf24",
    "Chemie" = "#D97706",
    "Physik" = "#F59E0B",
    "andere naturwiss.-technische Fächer" =  "#fde68a",
    "Mathematik" = "#ee7775",
    "andere Fächer (gesamt)" = "#D4C1BB",
    "Deutsch"= "#9D7265",
    "Fremdsprachen"= "#112C5F",
    "Gesellschaftswissenschaften" = "#D4C1BB",
    "Musik/Kunst" = "#5f94f9",
    "Religion/Ethik"= "#725249",
    "Sport"= "#154194"
  )

  if(betrachtung == "Übersicht - Kartendiagramm"){
    # filter dataset based on UI inputs
    # df <- dplyr::tbl(con, from = "kurse") %>%
    #   dplyr::filter(jahr == timerange,
    #                 region != "Deutschland",
    #                 anzeige_geschlecht == "Gesamt") %>%
    #   dplyr::select(fachbereich, indikator, anzeige_geschlecht, region, jahr,wert)%>%
    #   dplyr::collect()

    df_query <- glue::glue_sql("
                               SELECT fachbereich, indikator, anzeige_geschlecht, region, jahr,wert
                               FROM kurse
                               WHERE jahr IN ({timerange*})
                               AND NOT region = 'Deutschland'
                               AND anzeige_geschlecht = 'Gesamt'
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)


    df <- df %>%
      dplyr::mutate(fachbereich = dplyr::case_when(fachbereich == "MINT"~ "MINT-Fächer (gesamt)",
                                                   fachbereich == "andere Fächer" ~ "andere Fächer (gesamt)",
                                                   T~ fachbereich))%>%
      dplyr::group_by(indikator, anzeige_geschlecht, region, jahr)%>%
      dplyr::summarise(fachbereich, indikator, anzeige_geschlecht, region, jahr, wert, wert_sum = wert[fachbereich == "Alle Fächer"])%>%
      dplyr::ungroup()%>%
      dplyr::filter(fachbereich != "Alle Fächer")

    df <- df %>% dplyr::filter(fachbereich == subjects)

    #calculate proportions
    df <- df %>% # dplyr::group_by(region, indikator) %>%
      dplyr::mutate(proportion = wert/wert_sum*100)

    help_title <- ifelse(subjects == "MINT-Fächer (gesamt)", "MINT-Fächern", subjects)
    help_title <- ifelse(help_title == "andere Fächer (gesamt)", "allen Fächern außer MINT", help_title)

    #Extra gerundeten Proportions-Wert erstellen, für Anzeige in Hover
    df$prop <- df$proportion
    df$prop <- round(df$prop, 1)

    #Trennpunkte für lange Zahlen ergänzen
    df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    col <- as.character(color_fach[unique(df$fachbereich)])


    # plots

      df1 <- df[df$indikator == "Grundkurse",]
    joinby <- c("name", "region")
    name <- paste0(subjects)
    tooltip <-"{point.region} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}"
    titel <- paste0("Anteil von ", help_title, "<br> an allen Grundkursbelegungen ", "(",timerange, ")")
    mincolor <- "#fcfcfd"
    maxcolor <- col
    map_selection <- 1
    quelle <- "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    map1 <- mapbuilder(df1, joinby,name, tooltip, titel, mincolor, maxcolor,prop=FALSE, wert=FALSE, map=map_selection, quelle = quelle)


    df$proportion <- as.numeric(as.character(df$proportion))
    df$wert <- as.numeric(as.character(df$wert))

    # Leistungskurs läuft
    df2 <- df[df$indikator == "Leistungskurse",]
    joinby <- c("name", "region")
    name <- paste0(subjects)
    tooltip <-"{point.region} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}"
    titel <- paste0("Anteil von ", help_title, "<br> an allen Leistungskursbelegungen ", "(",timerange, ")")
    mincolor <- "#fcfcfd"
    maxcolor <- col
    map_selection <- 1
    quelle <- "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    map2 <- mapbuilder(df2, joinby,name, tooltip, titel, mincolor, maxcolor,prop=FALSE, wert=FALSE, map=map_selection, quelle = quelle)


    plot_list <- list(map1,map2)

    out <- highcharter::hw_grid(
      plot_list,
      ncol = 2)

    return(out)

  }
  else if(betrachtung == "Zeitverlauf - Liniendiagramm"){
    absolut_selector <- r$abs_zahlen_kurse_verlauf_multiple

    timerange <- r$date_kurse_verlauf_multiple
    t <- as.numeric(timerange[1]:timerange[2])

    states <- r$states_kurse_verlauf_multiple

    subjects_select <- r$subject_selected_multiple

    indikator_select <- r$topic_selected_multiple

    # filter dataset based on UI inputs
    # df <- dplyr::tbl(con, from = "kurse") %>%
    #   dplyr::filter(jahr %in% t,
    #                 indikator == indikator_select,
    #                 anzeige_geschlecht == "Gesamt") %>%
    #   dplyr::select(indikator, fachbereich, anzeige_geschlecht, region, jahr, wert)%>%
    #   dplyr::collect()


    df_query <- glue::glue_sql("
    SELECT indikator, fachbereich, anzeige_geschlecht, region, jahr, wert
    FROM kurse
    WHERE jahr IN ({t*})
    AND indikator = {indikator_select}
    AND anzeige_geschlecht = 'Gesamt'
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

    # include "Osten" und "Westen" in Dataframe
    #df <- prep_kurse_east_west(df)

    df <- df %>%
      dplyr::mutate(fachbereich = dplyr::case_when(fachbereich == "MINT"~ "MINT-Fächer (gesamt)",
                                                   fachbereich == "andere Fächer" ~ "andere Fächer (gesamt)",
                                                   T~ fachbereich))%>%
      dplyr::group_by(indikator, anzeige_geschlecht, region, jahr)%>%
      dplyr::summarise(fachbereich, indikator, anzeige_geschlecht, region, jahr, wert,sum_props = wert[fachbereich == "Alle Fächer"])%>%
      dplyr::ungroup()%>%
      dplyr::filter(fachbereich != "Alle Fächer")

    df <- df %>% dplyr::filter(fachbereich %in% subjects_select)

    # calculate proportions
    df <- df %>% dplyr::group_by(jahr, region, indikator) %>%
      dplyr::summarize(wert, proportion = wert/sum_props)%>%
      dplyr::rename(Relativ = proportion, Absolut=wert)%>%
      tidyr::pivot_longer(c(Absolut, Relativ), names_to = "selector", values_to = "wert")%>%
      dplyr::mutate(selector = dplyr::case_when(
        selector == "Relativ" ~ "In Prozent",
        selector == "Absolut" ~ "Anzahl"
      ))%>%
      dplyr::ungroup()%>%
      dplyr::mutate(region=dplyr::case_when(
        region == "Westen" ~ "Westdeutschland (o. Berlin)",
        region == "Osten" ~ "Ostdeutschland (inkl. Berlin)",
        T ~ .$region
      ))

    # fitler states
    df <- df %>% dplyr::filter(region %in% states)

    if(absolut_selector=="In Prozent"){

      df <- df %>%
        dplyr::filter(selector=="In Prozent")


      df$wert <- df$wert * 100


      if(indikator_select == "Grundkurse") {

        title_help <- "Grundkursbelegungen"

      }else {

        title_help <- "Leistungskursbelegungen"

      }

      # order years for plot
      df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

      help_title <- ifelse(subjects_select == "MINT-Fächer (gesamt)", "MINT-Fächern", subjects_select)
      help_title <- ifelse(help_title == "andere Fächer (gesamt)", "allen Fächern außer MINT", help_title)
      
    titel <- paste0("Anteil von ", help_title, " an allen ", title_help)
    tooltip <-  "MINT-Anteil in {point.region} <br> Wert: {point.y} %"
    format <-  "{value}%"
    color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
               "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5")
    quelle <- "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color, quelle = quelle)

    } else if(absolut_selector =="Anzahl"){
      hcoptslang <- getOption("highcharter.lang")
      hcoptslang$thousandsSep <- "."
      options(highcharter.lang = hcoptslang)

      df <- df %>%
        dplyr::filter(selector=="Anzahl")

      if(indikator_select == "Grundkurse") {
        title_help <- "Grundkursbelegungen "
      }else {
        title_help <- "Leistungskursbelegungen "
      }

      # order years for plot
      df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]


      help_title <- stringr::str_c("in ", subjects_select)
      help_title <- ifelse(grepl("MINT-Fächer", help_title), "in MINT", help_title)
      help_title <- ifelse(grepl("andere Fächer", help_title), "in allen Fächern außer MINT", help_title)

      titel <- paste0("Anzahl an ", title_help, help_title)
      tooltip <- "{point.region} <br> Anzahl: {point.y}"
      format <-  "{value:, f}"
      color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                 "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5")
      quelle <- "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
      out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color, quelle = quelle)
    }

    return(out)

  }
  else if(betrachtung == "Vergleich - Balkendiagramm"){

    timerange <- r$date_comparison_bl

    indikator_comparison <- r$indikator_comparison_bl

    if(indikator_comparison=="Grundkurse") {

      subject <- r$subject_comparison_bl1

    } else {

      subject <- r$subject_comparison_bl2

    }

    df_query <- glue::glue_sql("
    SELECT *
    FROM kurse
    WHERE jahr = {timerange}
    AND anzeige_geschlecht = 'Gesamt'
    AND indikator = {indikator_comparison}
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

    # aggregate all subjects to calculate proportion later
    df <- df %>%
      dplyr::select(-jahr, -bereich) %>%
      dplyr::mutate(fachbereich = dplyr::case_when(fachbereich == "MINT"~ "MINT-Fächer (gesamt)",
                                                   fachbereich == "andere Fächer" ~ "andere Fächer (gesamt)",
                                                   T~ fachbereich))%>%
      dplyr::group_by(indikator, anzeige_geschlecht, region)%>%
      dplyr::summarise(fachbereich, indikator, anzeige_geschlecht, region,  wert, props = wert[fachbereich == "Alle Fächer"])%>%
      dplyr::ungroup()%>%
      dplyr::filter(fachbereich != "Alle Fächer")

    df <- df %>% dplyr::filter(fachbereich == subject)

    # nötig für stacked

    # calculate proportion
    df <- df %>% dplyr::group_by(region, fachbereich, indikator) %>%
      dplyr::mutate(proportion = wert/props)

    df$proportion <- round(df$proportion * 100,1)

    df <- subset(df, proportion >= 0.5)

    #Trennpunkte für lange Zahlen ergänzen
    df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

    #Vorbereitung Überschrift
    help_title <- ifelse(subject == "MINT-Fächer (gesamt)", "MINT", subject)
    help_title <- ifelse(help_title == "andere Fächer (gesamt)", "allen Fächern außer MINT", help_title)

    kurs_help <- ifelse(indikator_comparison == "Grundkurse", "Grundkurs", "Leistungskurs")


    df <- df %>% dplyr::arrange(desc(proportion))

    #Plot
    #net verwendet da kein color
    out <- highcharter::hchart(df, 'bar', highcharter::hcaes(y = round(proportion,1), x = region)) %>%
      highcharter::hc_tooltip(pointFormat = "{point.fachbereich} <br> Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_plotOptions(bar = list(
        colorByPoint = TRUE,
        colors = ifelse(df$region == "Deutschland", "#b16fab",
                        ifelse(df$region == "Ostdeutschland (inkl. Berlin)", "#d3a4d7",
                               ifelse(df$region == "Westdeutschland (o. Berlin)", "#d3a4d7", "#A9A9A9"))))) %>%
      highcharter::hc_title(text = paste0( "Anteil von ", kurs_help, "-Belegungen in ", help_title, " nach Bundesländern (",  timerange, ")"
      ),
      margin = 20,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
     highcharter::hc_caption(text = "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
                              style = list(fontSize = "11px", color = "gray")) %>%
      highcharter::hc_exporting(enabled = TRUE,
                                buttons = list(
                                  contextButton = list(
                                    menuItems = list("downloadPNG", "downloadCSV")
                                  )
                                )
      )

    return(out)

  }

}

## Nicht Box 2 ----

#'
#' #' A function to plot time series
#' #'
#' #' @description A function to plot the time series of the german states
#' #'
#' #' @return The return value, if any, from executing the function.
#' #' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' kurse_verlauf_multiple_bl <- function(r) {
#'
#'   # load UI inputs from reactive value
#'
#'   absolut_selector <- r$abs_zahlen_kurse_verlauf_multiple
#'
#'   timerange <- r$date_kurse_verlauf_multiple
#'   t <- as.numeric(timerange[1]:timerange[2])
#'
#'   states <- r$states_kurse_verlauf_multiple
#'
#'   subjects_select <- r$subject_selected_multiple
#'
#'   indikator_select <- r$topic_selected_multiple
#'
#'   # SQL: DONE
#'   # filter dataset based on UI inputs
#'   # df <- dplyr::tbl(con, from = "kurse") %>%
#'   #   dplyr::filter(jahr %in% t,
#'   #                 indikator == indikator_select,
#'   #                 anzeige_geschlecht == "Gesamt") %>%
#'   #   dplyr::select(indikator, fachbereich, anzeige_geschlecht, region, jahr, wert)%>%
#'   #   dplyr::collect()
#'
#'   df_query <- glue::glue_sql("
#'   SELECT indikator, fachbereich, anzeige_geschlecht, region, jahr, wert
#'   FROM kurse
#'   WHERE jahr IN ({t*})
#'   AND indikator = {indikator_select}
#'   AND anzeige_geschlecht = 'Gesamt'
#'                                ", .con = con)
#'
#'   df <- DBI::dbGetQuery(con, df_query)
#'
#'   # include "Osten" und "Westen" in Dataframe
#'   #df <- prep_kurse_east_west(df)
#'
#'   df <- df %>%
#'     dplyr::mutate(fachbereich = dplyr::case_when(fachbereich == "MINT"~ "MINT-Fächer (gesamt)",
#'                                                  fachbereich == "andere Fächer" ~ "andere Fächer (gesamt)",
#'                                                  T~ fachbereich))%>%
#'     dplyr::group_by(indikator, anzeige_geschlecht, region, jahr)%>%
#'     dplyr::summarise(fachbereich, indikator, anzeige_geschlecht, region, jahr, wert,sum_props = wert[fachbereich == "Alle Fächer"])%>%
#'     dplyr::ungroup()%>%
#'     dplyr::filter(fachbereich != "Alle Fächer")
#'
#'
#'   df <- df %>% dplyr::filter(fachbereich %in% subjects_select)
#'
#'   # calculate proportions
#'   df <- df %>% dplyr::group_by(jahr, region, indikator) %>%
#'     dplyr::summarize(wert, proportion = wert/sum_props)%>%
#'     dplyr::rename(Relativ = proportion, Absolut=wert)%>%
#'     tidyr::pivot_longer(c(Absolut, Relativ), names_to = "selector", values_to = "wert")%>%
#'     dplyr::mutate(selector = dplyr::case_when(
#'       selector == "Relativ" ~ "In Prozent",
#'       selector == "Absolut" ~ "Anzahl"
#'     ))%>%
#'     dplyr::ungroup()%>%
#'     dplyr::mutate(region=dplyr::case_when(
#'       region == "Westen" ~ "Westdeutschland (o. Berlin)",
#'       region == "Osten" ~ "Ostdeutschland (inkl. Berlin)",
#'       T ~ .$region
#'     ))
#'
#'   # fitler states
#'   df <- df %>% dplyr::filter(region %in% states)
#'
#'   if(absolut_selector=="In Prozent"){
#'
#'     df <- df %>%
#'       dplyr::filter(selector=="In Prozent")
#'
#'     df$wert <- df$wert * 100
#'
#'     if(indikator_select == "Grundkurse") {
#'       title_help <- "Grundkursbelegungen"
#'     }else {
#'       title_help <- "Leistungskursbelegungen"
#'     }
#'
#'     # order years for plot
#'     df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
#'
#'
#'     help_title <- ifelse(subjects_select == "MINT-Fächer (gesamt)", "MINT-Fächern", subjects_select)
#'     help_title <- ifelse(help_title == "andere Fächer (gesamt)", "allen Fächern außer MINT", help_title)
#'
#'     # plot
#'
#'
#'     titel <- paste0("Anteil von ", help_title, " an allen ", title_help)
#'     tooltip <-  "Anteil {point.region} <br> Wert: {point.y} %"
#'     format <-  "{value}%"
#'     color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
#'                "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5")
#'     out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color)
#'
#'   } else if(absolut_selector =="Anzahl"){
#'
#'     hcoptslang <- getOption("highcharter.lang")
#'     hcoptslang$thousandsSep <- "."
#'     options(highcharter.lang = hcoptslang)
#'
#'     df <- df %>%
#'       dplyr::filter(selector=="Anzahl")
#'
#'
#'
#'     if(indikator_select == "Grundkurse") {
#'       title_help <- "Grundkursbelegungen "
#'     }else {
#'       title_help <- "Leistungskursbelegungen "
#'     }
#'
#'     # order years for plot
#'     df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
#'
#'
#'     help_title <- stringr::str_c("in ", subjects_select)
#'     help_title <- ifelse(grepl("MINT-Fächer", help_title), "in MINT", help_title)
#'     help_title <- ifelse(grepl("andere Fächer", help_title), "in allen Fächern außer MINT", help_title)
#'
#'     # plot
#'
#'     titel <- paste0("Anzahl an ", title_help, help_title)
#'     tooltip <-  "Anzahl: {point.y}"
#'     format <-  "{value:, f}"
#'     color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
#'                "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5")
#'     out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color)
#'   }
#'
#'
#'   return(out)
#' }
#'
#'
#'
#' #' A function to create a bar plot
#' #'
#' #' @description A function to return a ranking of subject for both genders
#' #'
#' #' @return The return value is a bar plot
#' #' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' kurse_ranking_gender <- function(r) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_kurse_ranking_gender
#'
#'   subject <- r$subject_kurse_ranking_gender
#'
#'   #SQL: DONE
#'   # filter dataset based on UI inputs
#'   # df <- dplyr::tbl(con, from = "kurse") %>%
#'   #   dplyr::filter(jahr == timerange,
#'   #                 region != "Deutschland",
#'   #                 region != "Baden-Württemberg") %>%
#'   #   dplyr::select(fachbereich, indikator, anzeige_geschlecht, region, jahr, wert)%>%
#'   #   dplyr::collect()
#'
#'
#'   df_query <- glue::glue_sql("
#'   SELECT fachbereich, indikator, anzeige_geschlecht, region, jahr, wert
#'   FROM kurse
#'   WHERE jahr = {timerange}
#'   AND NOT region = {Deutschland}
#'   AND NOT region = {Deutschland}
#'                                ", .con = con)
#'
#'   df <- DBI::dbGetQuery(con, df_query)
#'
#'
#'   df <- df %>%
#'     dplyr::mutate(fachbereich = dplyr::case_when(fachbereich == "MINT"~ "MINT-Fächer (gesamt)",
#'                                                  fachbereich == "andere Fächer" ~ "andere Fächer (gesamt)",
#'                                                  T~ fachbereich))%>%
#'     dplyr::group_by(indikator, anzeige_geschlecht, region, jahr)%>%
#'     dplyr::summarise(fachbereich, indikator, anzeige_geschlecht, region, jahr, wert, wert_sum = wert[fachbereich == "Alle Fächer"])%>%
#'     dplyr::ungroup()%>%
#'     dplyr::filter(fachbereich != "Alle Fächer")%>%
#'     dplyr::filter(fachbereich == subject)
#'
#'   df <- df %>%
#'
#'     dplyr::mutate(proportion = (wert/wert_sum)*100) %>%
#'     dplyr::select(-c("wert", "wert_sum")) %>%
#'     dplyr::filter(anzeige_geschlecht=="Frauen")
#'
#'   # spread column
#'   df <- tidyr::spread(df, indikator, proportion)
#'
#'   df2 <- tidyr::gather(df, group, value, -region) %>%
#'     dplyr::filter(group %in% c("Grundkurse", "Leistungskurse")) %>%
#'     dplyr::mutate(value = as.numeric(value))
#'
#'   df$region <- reorder(df$region, df$Leistungskurse)
#'
#'   df2$region <- factor(df2$region, levels = levels(df$region))
#'
#'   help_title <- ifelse(subject == "MINT-Fächer (gesamt)", "MINT-Fächern", subject)
#'   help_title <- ifelse(help_title == "andere Fächer (gesamt)", "allen Fächern außer MINT", help_title)
#'
#'   ggplot2::ggplot(df,
#'                   ggplot2::aes(y = region)) +
#'     ggplot2::geom_point(data = df2, ggplot2::aes(x = value, color = group), size = 5) +
#'     ggalt::geom_dumbbell(
#'       ggplot2::aes(x = Grundkurse, xend = Leistungskurse),
#'       size = 0.5,
#'       size_x = 5,
#'       size_xend = 5,
#'       colour = "black",
#'       colour_x = "#bfc6d3",
#'       colour_xend = "#66cbaf",
#'       dot_guide=TRUE) +
#'     ggplot2::theme_minimal() +
#'     ggplot2::scale_color_manual(name = "", values = c("#bfc6d3", "#66cbaf")) +
#'     ggplot2::theme(legend.position="top",
#'                    panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
#'                    plot.title = ggtext::element_markdown(hjust = 0.5),
#'                    axis.text.y = ggplot2::element_text(size = 11)) +
#'     ggplot2::ylab("") + ggplot2::xlab("") +
#'     ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
#'                                  "Belegungen von Mädchen in ", help_title ,br(),timerange,
#'                                  "<br><br><br>"),
#'                   color = "") +
#'     ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))
#'
#' }
#'
#'
#'
#' #' A function to plot a waffle chart
#' #'
#' #' @description A function to create a waffle chart for the second box inside the
#' #' tab "Schule".
#' #'
#' #' @return The return value is a waffle chart
#' #' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' kurse_mint_comparison <- function(r) {
#'
#'
#'   timerange <- r$date_comparison_subject
#'
#'   state <- r$state_comparison_subject
#'
#'   indikator_comparison <- r$indikator_comparison_subject
#'
#'   # filter dataset based on UI inputs
#'   # df <- dplyr::tbl(con, from = "kurse") %>%
#'   #   dplyr::filter(anzeige_geschlecht == "Gesamt",
#'   #                 jahr == timerange) %>%
#'   #   dplyr::select(-bereich)%>%
#'   #   dplyr::collect()
#'
#'   df_query <- glue::glue_sql("
#'   SELECT *
#'   FROM kurse
#'   WHERE anzeige_geschlecht = 'Gesamt'
#'   AND jahr = {timerange}
#'                                ", .con = con)
#'   df <- DBI::dbGetQuery(con, df_query)
#'
#'
#'   df <- df %>%
#'     dplyr::select(-bereich)%>%
#'     dplyr::mutate(fachbereich = dplyr::case_when(fachbereich == "MINT"~ "MINT-Fächer (gesamt)",
#'                                                  fachbereich == "andere Fächer" ~ "andere Fächer (gesamt)",
#'                                                  T~ fachbereich))%>%
#'     dplyr::group_by(indikator, anzeige_geschlecht, region, jahr)%>%
#'     dplyr::summarise(fachbereich, indikator, anzeige_geschlecht, region, jahr, wert,wert_gesamt = wert[fachbereich == "Alle Fächer"])%>%
#'     dplyr::ungroup()%>%
#'     dplyr::filter(fachbereich != "Alle Fächer")%>%
#'     dplyr::mutate(proportion = (wert/wert_gesamt)*100)
#'
#'   # Bei Leistungskurse: Religion/Ethik raus, da minimal
#'   if(indikator_comparison=="Leistungskurse"){
#'
#'     df <- df %>%
#'       dplyr::filter(fachbereich!="Religion/Ethik")
#'
#'   }
#'
#'   df <- df %>% dplyr::filter(indikator == indikator_comparison)
#'
#'   df <- na.omit(df)
#'
#'   df <- df %>%
#'     dplyr::ungroup()%>%
#'     dplyr::mutate(region= dplyr::case_when(
#'       region == "Westen" ~ "Westdeutschland (o. Berlin)",
#'       region == "Osten" ~ "Ostdeutschland (inkl. Berlin)",
#'       T ~ .$region
#'     ))
#'
#'   df <- df %>% dplyr::filter(region == state)
#'
#'   x <- c("MINT-Fächer (gesamt)", "Mathematik", "Informatik", "Physik", "Chemie",
#'          "Biologie", "andere naturwiss.-technische Fächer",
#'          "andere Fächer (gesamt)", "Deutsch", "Fremdsprachen", "Gesellschaftswissenschaften",
#'          "Musik/Kunst", "Religion/Ethik", "Sport")
#'
#'   df <- df %>%
#'     dplyr::mutate(fachbereich =  factor(fachbereich, levels = x)) %>%
#'     dplyr::arrange(fachbereich)
#'
#'   #Trennpunkte für lange Zahlen ergänzen
#'   df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
#'
#'   df <- df %>% dplyr::filter(fachbereich != "andere naturwiss.-technische Fächer")
#'
#'   #Anordnen mit / ohne Religion
#'   c <- c("MINT-Fächer (gesamt)", "Mathematik", "Informatik", "Physik", "Chemie",
#'          "Biologie",
#'          "andere Fächer (gesamt)", "Deutsch", "Fremdsprachen", "Gesellschaftswissenschaften",
#'          "Musik/Kunst", "Religion/Ethik", "Sport")
#'   if(indikator_comparison=="Leistungskurse") c <- c("MINT-Fächer (gesamt)", "Mathematik", "Informatik", "Physik", "Chemie",
#'                                                     "Biologie",
#'                                                     "andere Fächer (gesamt)", "Deutsch", "Fremdsprachen", "Gesellschaftswissenschaften",
#'                                                     "Musik/Kunst", "Sport")
#'
#'   #Vorbereitung für Überschrift
#'   if(indikator_comparison=="Leistungskurse"){
#'     indikator_comparison <- "Leistungskurs"
#'   } else {
#'     indikator_comparison <- "Grundkurs"
#'   }
#'
#'   # plot
#'   #net verwendet da kein color explizit und komplexer
#'   out <- highcharter::hchart(df, 'bar', highcharter::hcaes(y = round(proportion,1), x = fachbereich))%>%
#'     highcharter::hc_tooltip(pointFormat = "{point.region} <br> Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
#'     highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
#'     highcharter::hc_xAxis(title = list(text = ""), categories = c
#'     ) %>%
#'     highcharter::hc_plotOptions(bar = list(
#'       colorByPoint = TRUE,
#'       colors = ifelse(df$fachbereich %in% c("MINT-Fächer (gesamt)", "andere Fächer (gesamt)"), "#b16fab", "#d0a9cd")
#'     )) %>%
#'     highcharter::hc_title(text = paste0( "Anteil von ", indikator_comparison, "-Belegungen nach Fächern in ", state, " (", timerange, ")"
#'                                          ),
#'                           margin = 45,
#'                           align = "center",
#'                           style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
#'     highcharter::hc_chart(
#'       style = list(fontFamily = "Calibri Regular", fontSize = "14px")
#'     ) %>%
#'     highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
#'     highcharter::hc_exporting(enabled = TRUE,
#'                               buttons = list(
#'                                 contextButton = list(
#'                                   menuItems = list("downloadPNG", "downloadCSV")
#'                                 )
#'                               )
#'     )
#'
#'
#'
#' return (out)
#'
#'
#' }
#'
#'
#' #' A function to plot a waffle chart
#' #'
#' #' @description A function to create a waffle chart for the second box inside the
#' #' tab "Schule".
#' #'
#' #' @return The return value is a waffle chart
#' #' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' kurse_mint_comparison_bl <- function(r) {
#'
#'
#'   timerange <- r$date_comparison_bl
#'
#'
#'   indikator_comparison <- r$indikator_comparison_bl
#'
#'   if(indikator_comparison=="Grundkurse") {
#'
#'     subject <- r$subject_comparison_bl1
#'
#'   } else {
#'
#'     subject <- r$subject_comparison_bl2
#'
#'   }
#'
#'   # filter dataset based on UI inputs
#'   # df <- dplyr::tbl(con, from = "kurse") %>%
#'   #   dplyr::filter(jahr == timerange,
#'   #                 region != "Deutschland",
#'   #                 anzeige_geschlecht == "Gesamt",
#'   #                 indikator == indikator_comparison) %>%
#'   #   dplyr::select(-jahr, -bereich) %>%
#'   #   dplyr::collect()
#'
#'   df_query <- glue::glue_sql("
#'   SELECT *
#'   FROM kurse
#'   WHERE jahr = {timerange}
#'   AND anzeige_geschlecht = 'Gesamt'
#'   AND indikator = {indikator_comparison}
#'                                ", .con = con)
#'
#'   df <- DBI::dbGetQuery(con, df_query)
#'
#'   # aggregate all subjects to calculate proportion later
#'   df <- df %>%
#'     dplyr::select(-jahr, -bereich) %>%
#'     dplyr::mutate(fachbereich = dplyr::case_when(fachbereich == "MINT"~ "MINT-Fächer (gesamt)",
#'                                                  fachbereich == "andere Fächer" ~ "andere Fächer (gesamt)",
#'                                                  T~ fachbereich))%>%
#'     dplyr::group_by(indikator, anzeige_geschlecht, region)%>%
#'     dplyr::summarise(fachbereich, indikator, anzeige_geschlecht, region,  wert, props = wert[fachbereich == "Alle Fächer"])%>%
#'     dplyr::ungroup()%>%
#'     dplyr::filter(fachbereich != "Alle Fächer")
#'
#'
#'   # df <- rbind(df, df_sub)
#'
#'   df <- df %>% dplyr::filter(fachbereich == subject)%>%
#'     dplyr::filter(!region %in% c("Westen", "Osten"))
#'
#'
#'
#'   # calculate proportion
#'   df <- df %>% dplyr::group_by(region, fachbereich, indikator) %>%
#'     dplyr::mutate(proportion = wert/props)
#'
#'   df$proportion <- round(df$proportion * 100,1)
#'
#'   df <- subset(df, proportion >= 0.5)
#'
#'   #Trennpunkte für lange Zahlen ergänzen
#'   df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
#'
#'   #Vorbereitung Überschrift
#'   help_title <- ifelse(subject == "MINT-Fächer (gesamt)", "MINT", subject)
#'   help_title <- ifelse(help_title == "andere Fächer (gesamt)", "allen Fächern außer MINT", help_title)
#'
#'   kurs_help <- ifelse(indikator_comparison == "Grundkurse", "Grundkurs", "Leistungskurs")
#'
#'   #Plot
#'   #net verwendet da kein color explizit und komplexer
#'   out <- highcharter::hchart(df, 'bar', highcharter::hcaes(y = round(proportion,1), x = region)) %>%
#'     highcharter::hc_tooltip(pointFormat = "{point.fachbereich} <br> Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
#'     highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
#'     highcharter::hc_xAxis(title = list(text = "")) %>%
#'     # highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
#'     # highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
#'     highcharter::hc_colors("#b16fab") %>%
#'     highcharter::hc_title(text = paste0( "Anteil von ", kurs_help, "-Belegungen in ", help_title, " nach Bundesländern (",  timerange, ")"
#'                                         ),
#'                           margin = 20,
#'                           align = "center",
#'                           style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
#'     highcharter::hc_chart(
#'       style = list(fontFamily = "Calibri Regular", fontSize = "14px")
#'     ) %>%
#'     highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
#'     highcharter::hc_exporting(enabled = TRUE,
#'                               buttons = list(
#'                                 contextButton = list(
#'                                   menuItems = list("downloadPNG", "downloadCSV")
#'                                 )
#'                               )
#'     )
#'
#'
#'
#'   return(out)
#' }

# Frauen in MINT ----

  #' A function to plot time series
#'
#' @description A function to plot a bar chart
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

kurse_comparison_gender <- function(r) {

  # load UI inputs from reactive value
  betrachtung <- r$ansicht_kurse_comparison_gender
  timerange <- r$date_kurse_comparison_gender
  indika <- r$indikator_kurse_comparison_gender
  regio <- r$region_kurse_comparison_gender

  if(betrachtung == "Einzelansicht - Kuchendiagramm"){
    gegenwert <- r$gegenwert_kurse_comparison_gender
  }else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){
    gegenwert <- r$gegenwert_kurse_comparison_gender_balken
   # indika <- DBI:dbGetQuery(con, "SELECT DISTINCT indikator FROM kurse")
  }

  if (betrachtung == "Kursvergleich - Hanteldiagramm"){

    df_query <- glue::glue_sql("
    SELECT *
    FROM kurse
    WHERE jahr = {timerange}
    AND NOT fachbereich = 'Alle Fächer'
    AND NOT indikator = 'Oberstufenbelegungen'
                               ", .con = con)
    df <- DBI::dbGetQuery(con, df_query)


    # calcualte the new "Gesamt"
    df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
      dplyr::group_by(region, fachbereich, indikator, jahr) %>%
      dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

    df <- df %>%
      dplyr::ungroup()%>%
      dplyr::mutate(region = dplyr::case_when(
        region == "Westen" ~ "Westdeutschland (o. Berlin)",
        region == "Osten" ~ "Ostdeutschland (inkl. Berlin)",
        T ~ .$region
      ))

    df <- df %>% dplyr::filter(region == regio)


    df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")

    # calcualte propotion
    df <- df %>% dplyr::group_by(fachbereich, anzeige_geschlecht, indikator) %>%
      dplyr::summarize(proportion = wert/props)%>%
      dplyr::filter(!fachbereich %in% c("MINT", "andere Fächer" ))

    df$proportion <- df$proportion * 100

    df$anzeige_geschlecht <- NULL

    # spread column
    df <- tidyr::spread(df, indikator, proportion)

    df <- df %>%
      tidyr::drop_na() %>%
      dplyr::mutate(
        Grundkurse = round(Grundkurse, 1),
        Leistungskurse = round(Leistungskurse, 1)
      )

    out <- plotly::plot_ly(data = df, color = I("gray80")) %>%
      plotly::add_segments(
        x = ~Grundkurse,
        xend = ~Leistungskurse,
        y = ~fachbereich,
        yend = ~fachbereich,
        showlegend = FALSE,
        hoverinfo = "text"
      ) %>%
      plotly::add_markers(
        x = ~Grundkurse,
        y = ~fachbereich,
        name = "Grundkurse",
        marker = list(
          size = 12,
          color = "#bfc6d3"
        ),
        text = ~ifelse(is.na(Grundkurse), NA, paste0("Grundkurse: ", Grundkurse)),
        hoverinfo = "text"
      ) %>%
      plotly::add_markers(
        x = ~Leistungskurse,
        y = ~fachbereich,
        name = "Leistungskurse",
        marker = list(
          size = 12,
          color = "#66CBAF"
        ),
        text = ~ifelse(is.na(Leistungskurse), NA, paste0("Leistungskurse: ", Leistungskurse)),
        hoverinfo = "text"
      ) %>%
      # Layout anpassen
      plotly::layout(
        title = paste0("Mädchen-Anteil nach Fächern in ",regio, " (", timerange, ")"),
        xaxis = list(title = ""),
        yaxis = list(title = ""),
        margin = list(l = 100, r = 50, t = 50, b = 50),
        hoverlabel = list(bgcolor = "white"),
        legend = list(
          orientation = "h",
          x = 0.5,
          y = -0.2,
          xanchor = "center",
          yanchor = "top"
        ),
        annotations = list(
          list(
            text = "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
            xref = "paper", yref = "paper",
            x = 0, y = -0.3,
            xanchor = "left", yanchor = "top",
            showarrow = FALSE,
            font = list(size = 10, color = "gray")
          )
        )
      ) %>%
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

  }else {


  df_query <- glue::glue_sql("
  SELECT region, fachbereich, anzeige_geschlecht, indikator, jahr, wert
  FROM kurse
  WHERE jahr = {timerange}
  AND region = {regio}
  AND fachbereich IN ('MINT', 'andere Fächer')
                               ", .con = con)


  df <- DBI::dbGetQuery(con, df_query)


  # calcualte the new "Gesamt"
  df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")

  #gegenwert Berechnen für jeweilige Auswahl
  df_n <- df %>% dplyr::group_by(region, fachbereich, indikator, jahr) %>%
    dplyr::mutate(wert = props - wert)
  df_n$anzeige_geschlecht <- "Männer"

  df <- rbind(df, df_n)

  # calcualte proportions
  df1 <- df %>% dplyr::group_by(indikator, fachbereich, anzeige_geschlecht, jahr) %>%
    dplyr::mutate(proportion = wert/props)

  df1$proportion <- df1$proportion * 100


  df1$fachbereich <- factor(df1$fachbereich, levels = c("MINT","andere Fächer"))

  # order years for plot
  df1 <- df1[with(df, order(jahr, decreasing = FALSE)), ]

  df1$anzeige_geschlecht[df1$anzeige_geschlecht == "Frauen"] <- "Mädchen"
  df1$anzeige_geschlecht[df1$anzeige_geschlecht == "Männer"] <- "Jungen"

  #Trennpunkte für lange Zahlen ergänzen
  df1$wert <- prettyNum(df1$wert, big.mark = ".", decimal.mark = ",")

  # plot
  if(betrachtung == "Einzelansicht - Kuchendiagramm"){

    df1 <- df1 %>%
      dplyr::filter(indikator == indika)

    df1$proportion <- round(df1$proportion)

    titel_help <- sprintf("%sn", indika)
    titel_help <- ifelse(indika == "Oberstufenbelegungen", "Oberstufenkursen",
                         titel_help)

    if(gegenwert == "Ja"){

      df_mint <- df1 %>%
        dplyr::filter(fachbereich == "MINT")
      df_rest <- df1 %>%
        dplyr::filter(fachbereich != "MINT")

      titel <- paste0("Mädchen-Anteil in MINT-", titel_help,  " in ", regio, " (", timerange, ")")
      titelg <- paste0("Mädchen-Anteil in anderen ", titel_help,  " in ", regio, " (", timerange, ")")
      tooltip <- paste('Anteil: {point.proportion}% <br> Anzahl: {point.wert}')

      quelle <- "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

      mint <- piebuilder(df_mint, titel, x = "anzeige_geschlecht", y = "proportion", tooltip, color = c("#154194" ,"#efe8e6"), format='{point.proportion}%', quelle = quelle)

      rest <- piebuilder(df_rest, titelg, x = "anzeige_geschlecht", y = "proportion", tooltip, color = c("#154194" ,"#efe8e6"), format='{point.proportion}%', quelle = quelle)


      out <- highcharter::hw_grid(
        mint, rest,
        ncol = 2,
        browsable = TRUE
      )


    }else if(gegenwert == "Nein"){

      df1 <- df1 %>%
        dplyr::filter(fachbereich == "MINT")


      titel = paste0("Mädchen-Anteil in MINT-", titel_help,  " in ", regio, " (", timerange, ")")
      tooltip = paste('Anteil: {point.proportion}% <br> Anzahl: {point.wert}')


      quelle <- "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

      out <- piebuilder(df1, titel, x = "anzeige_geschlecht", y = "proportion", tooltip, color = c("#154194" ,"#efe8e6"), format = '{point.proportion}%', quelle = quelle)

    }

  }else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){

    df1$indikator <- ifelse(df1$indikator == "Grundkurse" & df1$fachbereich == "MINT", "Grundkurse MINT-Fächer", df1$indikator)
    df1$indikator <- ifelse(df1$indikator == "Grundkurse" & df1$fachbereich == "andere Fächer", "Grundkurse andere Fächer", df1$indikator)
    df1$indikator <- ifelse(df1$indikator == "Leistungskurse" & df1$fachbereich == "MINT", "Leistungskurse MINT-Fächer", df1$indikator)
    df1$indikator <- ifelse(df1$indikator == "Leistungskurse" & df1$fachbereich == "andere Fächer", "Leistungskurse andere Fächer", df1$indikator)
    df1$indikator <- ifelse(df1$indikator == "Oberstufenbelegungen" & df1$fachbereich == "MINT", "Oberstufenbelegungen MINT-Fächer", df1$indikator)
    df1$indikator <- ifelse(df1$indikator == "Oberstufenbelegungen" & df1$fachbereich == "andere Fächer", "Oberstufenbelegungen andere Fächer", df1$indikator)


    if(gegenwert == "Ja"){


      #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
      out <- highcharter::hchart(df1, 'bar', highcharter::hcaes( x = indikator, y=round(proportion,1), group = anzeige_geschlecht)) %>%
        highcharter::hc_tooltip(pointFormat = "{point.anzeige_geschlecht}-Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  TRUE) %>%
        highcharter::hc_xAxis(title = list(text = ""), categories=c("Grundkurse MINT-Fächer",
                                                                    "Grundkurse andere Fächer",
                                                                    "Leistungskurse MINT-Fächer",
                                                                    "Leistungskurse andere Fächer",
                                                                    "Oberstufenbelegungen MINT-Fächer",
                                                                    "Oberstufenbelegungen andere Fächer")) %>%
        highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0("Anteil von Mädchen in MINT- und anderen Fächern in ",regio, " (", timerange, ")"),
                              margin = 25,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "Calibri Regular", fontSize = "14px")
        ) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
        highcharter::hc_caption(text = "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
                                style = list(fontSize = "11px", color = "gray")) %>%
        highcharter::hc_exporting(enabled = TRUE,
                                  buttons = list(
                                    contextButton = list(
                                      menuItems = list("downloadPNG", "downloadCSV")
                                    )
                                  ))


    }else if(gegenwert == "Nein"){

      df1 <- df1 %>% dplyr::filter(indikator %in%
                                     c("Grundkurse MINT-Fächer",
                                       "Leistungskurse MINT-Fächer",
                                       "Oberstufenbelegungen MINT-Fächer"))

      out <- highcharter::hchart(df1, 'bar', highcharter::hcaes( x = indikator, y=round(proportion,1), group = anzeige_geschlecht)) %>%
        highcharter::hc_tooltip(pointFormat = "{point.anzeige_geschlecht}-Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  TRUE) %>%
        highcharter::hc_xAxis(title = list(text = ""), categories=c("Grundkurse MINT-Fächer",
                                                                    "Leistungskurse MINT-Fächer",
                                                                    "Oberstufenbelegungen MINT-Fächer")) %>%
        highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0("Anteil von Mädchen in MINT-Fächern in ",regio, " (", timerange, ")"),
                              margin = 25,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "Calibri Regular", fontSize = "14px")
        ) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
        highcharter::hc_caption(text = "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
                                style = list(fontSize = "11px", color = "gray")) %>%
        highcharter::hc_exporting(enabled = TRUE,
                                  buttons = list(
                                    contextButton = list(
                                      menuItems = list("downloadPNG", "downloadCSV")
                                    )
                                  )
        )
    }

  }
  }


  return(out)
}


# Output Zeitverlauf
kurse_verlauf_gender <- function(r){

  timerange <- r$datum_kurse_verlauf_gender
  t <- timerange[1]:timerange[2]
  regio <- r$region_kurse_verlauf_gender
  abs_rel <- r$abs_rel_kurse_verlauf_gender


  df_query <- glue::glue_sql("
        SELECT *
        FROM kurse
        WHERE region = {regio}
        AND fachbereich = 'MINT'
        AND jahr IN ({t*})
        AND NOT anzeige_geschlecht = 'Gesamt'
                               ", .con = con)#
  df <- DBI::dbGetQuery(con, df_query)

  df <-  df %>%
    dplyr::group_by(fachbereich, indikator, jahr) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                  wert[anzeige_geschlecht == "Männer"])

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")

  # calcualte proportions
  df <- df %>% dplyr::group_by(indikator, fachbereich, anzeige_geschlecht, jahr) %>%
    dplyr::mutate(proportion = wert/props)

  df$proportion <- round(df$proportion*100,1)

  df$anzeige_geschlecht[df$anzeige_geschlecht == "Frauen"] <- "Mädchen"

  #Trennpunkte für lange Zahlen ergänzen
  df$wert_anzeige <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

  if(abs_rel == "Anzahl"){

    # order years for plot
    df <- df[with(df, order(jahr, decreasing = FALSE)), ]

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    # plot

    titel <- paste0("Anzahl von Mädchen in MINT-Oberstufenkursen in ", regio)
    tooltip <-  "{point.indikator} <br> Wert: {point.wert_anzeige}"
    format <-  "{value:, f}"
    color <- colors_mint_vernetzt$general
    quelle <- "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "indikator", tooltip, format, color, quelle = quelle)



  } else if (abs_rel =="In Prozent") {

    # order years for plot
    df <- df[with(df, order(jahr, decreasing = FALSE)), ]


    titel <- paste0("Mädchenanteil in MINT-Oberstufenkursen", regio)
    tooltip <-  "{point.indikator} <br> Wert: {point.y}%"
    format <-  "{value}%"
    color <- colors_mint_vernetzt$general
    quelle <- "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    out <- linebuilder(df, titel, x = "jahr", y = "proportion", group = "indikator", tooltip, format, color, quelle = quelle)

  }
}




#' A function to plot a waffle chart
#'
#' @description A function to create a waffle chart for the second box inside the
#' tab "Schule".
#'
#' @return The return value is a waffle chart
#' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

kurse_wahl <- function(r) {

  betrachtung <- r$ansicht_kurse_gender
  timerange <- r$date_kurse
  indikator_gender <- r$indikator_kurse_gender
  vergleich <- r$gegenwert_kurse_gender
  regio <- r$region_kurse_gender

  if(betrachtung == "Einzelansicht - Kuchendiagramm"){
    color_fach <- c(
      "Informatik" = "#00a87a",
      "Naturwissenschaften" = "#fcc433",
      "Biologie" = "#fbbf24",
      "Chemie" = "#D97706",
      "Physik" = "#F59E0B",
      "andere naturwiss.-technische Fächer" =  "#fde68a",
      "Mathematik" = "#ee7775",
      "andere Fächer" = "#efe8e6"
    )


    df_query <- glue::glue_sql("
    SELECT indikator, fachbereich, anzeige_geschlecht, wert
    FROM kurse
    WHERE jahr = {timerange}
    AND region = {regio}
    AND indikator = {indikator_gender}
    AND NOT anzeige_geschlecht = 'Gesamt'
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)


    # combine subjects to get numbers on share of MINT
    # make a function out of it
    subjects_not <- c("Mathematik", "Informatik",  "Biologie", "Chemie",
                      "Physik", "andere naturwiss.-technische Fächer")

    values_natwi <- df %>%
      dplyr::filter(fachbereich %in% c("Biologie", "Chemie", "Physik", "andere naturwiss.-technische Fächer")) %>%
      dplyr::group_by(indikator, anzeige_geschlecht) %>%
      dplyr::summarise(wert = sum(wert, na.rm = TRUE)) %>%
      dplyr::mutate(fachbereich = "Naturwissenschaften") %>%
      dplyr::ungroup()

    df <- rbind(df, values_natwi)

    df <- df %>% dplyr::filter(fachbereich %in% c("Mathematik", "Informatik", "Naturwissenschaften", "andere Fächer"))

    # extract new "Gesamt"
    df <- df %>% dplyr::group_by(indikator, anzeige_geschlecht) %>%
      dplyr::mutate(props = sum(wert, na.rm = TRUE))

    # calculate proportion
    df <- df %>% dplyr::group_by(fachbereich, indikator, anzeige_geschlecht) %>%
      dplyr::mutate(proportion = round(wert/props *100,1))

    df <- df[with(df, order(proportion, decreasing = FALSE)), ]
    df <- df %>% dplyr::mutate(col = color_fach[fachbereich])
    df$wert <- prettyNum(df$wert, big.mark=".", decimal.mark = ",")

    df <- na.omit(df)

    # Titel erstellen
    titel_help <- indikator_gender
    titel_help <-ifelse(grepl("Leistung", titel_help), "Leistungskursbelegungen", "Grundkursbelegungen")
    titel_help <-ifelse(grepl("Ober", titel_help), "Oberstufenbelegungen", titel_help)

    if(vergleich == "Ja"){

      df_f <- df %>%
        dplyr::filter(anzeige_geschlecht == "Frauen")
      df_m <- df %>%
        dplyr::filter(anzeige_geschlecht == "Männer")

      titelf <- paste0(titel_help, " von Mädchen in ", regio, " (", timerange, ")")
      titelm <- paste0(titel_help, " von Jungen in ", regio, " (", timerange, ")")
      tooltip <- paste('Anteil: {point.proportion}% <br> Anzahl: {point.wert}')
      color = as.character(df_f$col)

      quelle <- "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

      frauen <- piebuilder(df_f, titelf, x = "fachbereich", y = "proportion", tooltip, color, format = '{point.proportion}%', quelle = quelle)
      männer <- piebuilder(df_m, titelm, x = "fachbereich", y = "proportion", tooltip, color, format = '{point.proportion}%', quelle = quelle)
      out <- highcharter::hw_grid(
        frauen, männer,
        ncol = 2,
        browsable = TRUE
      )


    }else if(vergleich == "Nein"){

      df_f <- df %>%
        dplyr::filter(anzeige_geschlecht == "Frauen")

      titel <- paste0(titel_help, " von Mädchen in ", regio, " (", timerange, ")")
      tooltip <- paste('Anteil: {point.proportion}% <br> Anzahl: {point.wert}')
      format = '{point.proportion}%'
      color = as.character(df_f$col)

      quelle <- "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

      out <- piebuilder(df_f, titel, x = "fachbereich", y = "proportion", tooltip, color, format, quelle=quelle)

    }
  }else if(betrachtung == "Bundeslandvergleich - Kartendiagramm"){

    subjects <- r$subject_kurse_gender
    kurs_select <- indikator_gender

    color_fach <- c(
      "MINT-Fächer (gesamt)" = "#b16fab",
      "Informatik" = "#00a87a",
      "Naturwissenschaften" = "#fcc433",
      "Biologie" = "#fbbf24",
      "Chemie" = "#D97706",
      "Physik" = "#F59E0B",
      "andere naturwiss.-technische Fächer" = "#fde68a",
      "Mathematik" = "#ee7775",
      "andere Fächer (gesamt)" = "#D4C1BB",
      "Deutsch"= "#D4C1BB",
      "Fremdsprachen"= "#D4C1BB",
      "Gesellschaftswissenschaften" ="#D4C1BB",
      "Musik/Kunst" = "#D4C1BB",
      "Religion/Ethik"= "#D4C1BB",
      "Sport"= "#D4C1BB"
    )

    df_query <- glue::glue_sql("SELECT bereich, fachbereich, indikator, anzeige_geschlecht, region, jahr, wert
    FROM kurse
    WHERE jahr = {timerange}
    AND NOT region IN ('Deutschland', 'Westen', 'Osten')
    AND indikator = {kurs_select}
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::mutate(fachbereich = dplyr::case_when(fachbereich == "MINT"~ "MINT-Fächer (gesamt)",
                                                   fachbereich == "andere Fächer" ~ "andere Fächer (gesamt)",
                                                   T~ fachbereich))%>%
      dplyr::group_by(indikator, anzeige_geschlecht, region, jahr)%>%
      dplyr::summarise(fachbereich, indikator, anzeige_geschlecht, region, jahr, wert,wert_gesamt = wert[fachbereich == "Alle Fächer"])%>%
      dplyr::ungroup()%>%
      dplyr::filter(fachbereich != "Alle Fächer")%>%
      dplyr::mutate(proportion = (wert/wert_gesamt)*100)%>%
      dplyr::filter(fachbereich == subjects)


    df_f <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")

    df_m <- df %>% dplyr::filter(anzeige_geschlecht == "Männer")

    help_title <- ifelse(subjects == "MINT-Fächer (gesamt)", "MINT-Fächern (gesamt)", subjects)
    help_title <- ifelse(help_title == "andere Fächer (gesamt)", "anderen Fächern (gesamt)", help_title)
    help_kurs <- ifelse(kurs_select == "Grundkurse", "Grundkurs-B", "Leistungskurs-B")
    help_kurs <- ifelse(kurs_select == "Oberstufenbelegungen", "Oberstufen", help_kurs)


    #Extra gerundeten Proportions-Wert erstellen, für Anzeige in Hover

    df_f$prop <- df_f$proportion
    df_f$prop <- round(df_f$prop, 1)

    df_m$prop <- df_m$proportion
    df_m$prop <- round(df_m$prop, 1)

    #Trennpunkte für lange Zahlen ergänzen
    df_f$wert <- prettyNum(df_f$wert, big.mark = ".", decimal.mark = ",")
    df_m$wert <- prettyNum(df_m$wert, big.mark = ".", decimal.mark = ",")


    # Plots


    df1 <- df_f[df_f$indikator == kurs_select,]
    joinby <- c("name", "region")
    name <- paste0(subjects)
    tooltip <-"{point.region} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}"
    titel <- paste0(help_kurs, "belegungen von Mädchen in ", help_title, " (", timerange, ")")
    mincolor <- "#fcfcfd"
    maxcolor <- as.character(color_fach[subjects])
    map_selection <- 1
    quelle <- "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    out1 <- mapbuilder(df1, joinby,name, tooltip, titel, mincolor, maxcolor,prop=FALSE, wert=FALSE, map=map_selection, quelle = quelle)

    out <- out1

    if(vergleich =="Ja"){


      df2 <- df_m[df_m$indikator == kurs_select,]
      joinby <- c("name", "region")
      name <- paste0(subjects)
      tooltip <-"{point.region} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}"
      titel <- paste0(help_kurs, "belegungen von Jungen in ", help_title," (", timerange, ")")
      mincolor <- "#fcfcfd"
      maxcolor <- as.character(color_fach[subjects])
      map_selection <- 1
      quelle <- "Quelle der Daten: KMK, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
      out2 <- mapbuilder(df2, joinby,name, tooltip, titel, mincolor, maxcolor,prop=FALSE, wert=FALSE, map=map_selection, quelle = quelle)

      out <- highcharter::hw_grid(
        out1, out2,
        ncol = 2,
        browsable = TRUE
      )
    }

  }

return(out)

}


# IQB ----

#' A function to create a bar plot
#'
#' @description A function to return a ranking of iqb test scores by year
#'
#' @return The return value is a bar plot
#' @param data The dataframe "iqb" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

iqb_standard_zeitverlauf <- function(r){


  # reactive values übergeben
  kl_select <- r$klasse_iqb_standard_zeitverlauf
  if(kl_select == "4. Klasse"){
    bl_select <- r$land_iqb_standard_zeitverlauf_4
  }else{
    bl_select <- r$land_iqb_standard_zeitverlauf_9
  }


  df_query <- glue::glue_sql("
  SELECT jahr, indikator, fach, region, wert
  FROM iqb
  WHERE klasse = {kl_select}
  AND region IN ({bl_select*})
  AND geschlecht = 'gesamt'
  AND indikator = 'Mindeststandard nicht erreicht'
                               ", .con = con)
  df <- DBI::dbGetQuery(con, df_query)
  df <- df %>%
    dplyr::mutate(display_rel = prettyNum(df$wert, big.mark = ".", decimal.mark = ","))

  # title helper

  if (length(bl_select)==1){
    title_help <- paste0(bl_select)
  }else if(length(bl_select)==2){
    title_help <- paste0(bl_select[1], " & ", bl_select[2] )
  }else if (length(bl_select)==3){
    title_help <- paste0(bl_select[1], ", ", bl_select[2], " & ", bl_select[3])
  }

  color <- dplyr::case_when(
    kl_select == "4. Klasse" ~ c("#efe8e6","#D0A9CD", "#b16fab"),
    kl_select == "9. Klasse" ~ "#b16fab"
  )

  out <- highcharter::hchart(df, 'column', highcharter::hcaes(y = wert, x = region, group=jahr))%>%
    highcharter::hc_plotOptions(column = list(pointWidth = 90))%>%
    highcharter::hc_tooltip(pointFormat = "{point.jahr} <br> {point.display_rel} % leistungsschwach")%>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value} %")) %>% #, pointsWidth=100
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_colors(color) %>%
    highcharter::hc_title(text = paste0("Anteil der Schüler:innen aus ", title_help, ", die den Mindeststandard in Mathematik nicht erreichen (", kl_select, ")"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "Calibri Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
    highcharter::hc_caption(text = "Quelle der Daten: Institut zur Qualitätsentwicklung im Bildungswesen, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
                            style = list(fontSize = "11px", color = "gray")) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG", "downloadCSV")
                                )
                              )
    )

  return(out)

}


#' A function to create a bar plot
#'
#' @description A function to return iqb test scores by year and group
#'
#' @return The return value is a bar plot
#' @param data The dataframe "iqb" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd


iqb_mathe_mittel_zeitverlauf <- function(r){

  # mindeststandard

  # reactive values übergeben
  klasse_select <- r$klasse_iqb_mathe_mittel_zeitverlauf
  fach_select <- r$fach_iqb_mathe_mittel_zeitverlauf
  score_select <- r$score_iqb_mathe_mittel_zeitverlauf

  if(klasse_select == "4. Klasse"){
    indikator_select <- r$indi_iqb_mathe_mittel_zeitverlauf_4
  }else{
    indikator_select <- r$indi_iqb_mathe_mittel_zeitverlauf_9
  }

  if(klasse_select == "4. Klasse"){
    bl_select <- r$land_iqb_mathe_mittel_zeitverlauf_4
  }else{
    if(indikator_select == "nach sozialem Status") bl_select <- r$land_iqb_mathe_mittel_zeitverlauf_9_sozS
    if(indikator_select == "nach Geschlecht") bl_select <- r$land_iqb_mathe_mittel_zeitverlauf_9_gen
    if(indikator_select == "nach Zuwanderungsgeschichte") bl_select <- r$land_iqb_mathe_mittel_zeitverlauf_9_zwg
  }


  df_query <- glue::glue_sql("
  SELECT jahr, indikator, fach, region, geschlecht, wert
  FROM iqb
  WHERE region = {bl_select}
  AND klasse = {klasse_select}
                               ", .con = con)
  df <- DBI::dbGetQuery(con, df_query)


  # für 9 Klassen Fach filtern
  if(klasse_select == "9. Klasse"){
    df <- df %>% dplyr::filter(fach == fach_select)
  }

  # Jahr als Faktor speichern, für schönere x-Achse
  df$jahr <- as.factor(df$jahr)

  # nach gewählter Vergleichsgruppe filtern

  # Datensatzaufbereitung bei Auswahl Geschlecht
  if (indikator_select == "nach Geschlecht") {
    if(score_select == "Mindeststandard") {
      df <- df %>% dplyr::filter(indikator == "Mindeststandard nicht erreicht")
    }else{
      df <- df %>% dplyr::filter(indikator == "Alle")
    }

    df <- df %>% dplyr::mutate(geschlecht=dplyr::case_when(
      geschlecht=="gesamt" ~ "Gesamt",
      T~df$geschlecht
    ))%>%
      dplyr::filter(geschlecht != "Gesamt")

    df$geschlecht[df$geschlecht == "Frauen"] <- "Mädchen"
    df$geschlecht[df$geschlecht == "Männer"] <- "Jungen"
    df$geschlecht <- as.factor(df$geschlecht)
    df$geschlecht <- factor(df$geschlecht, levels = c("Mädchen", "Jungen"))



  }

  # Datensatzaufbereitung bei Auswahl Zuwanderungsgeschichte
  if (indikator_select == "nach Zuwanderungsgeschichte" & klasse_select == "4. Klasse"){
    df <- df %>% dplyr::filter(indikator %in% c("Alle", "mit Zuwanderungsgeschichte", "ohne Zuwanderungsgeschichte"))
    df <- df %>% dplyr::filter(geschlecht == "gesamt")

    # Alle als Gesamtgruppe ausfiltern
    df <- df %>%
      dplyr::filter(indikator!="Alle")

    # Für Grafik als Faktor speichern
    df$indikator<- as.factor(df$indikator)
    df$indikator <- factor(df$indikator, levels = c("ohne Zuwanderungsgeschichte",
                                                    "mit Zuwanderungsgeschichte"))


  }

  if(indikator_select == "nach Zuwanderungsgeschichte" & klasse_select == "9. Klasse"){
    df <- df %>% dplyr::filter(indikator %in% c("mit Zuwanderungsgeschichte (beide Elternteile)",
                                                "mit Zuwanderungsgeschichte (ein Elternteil)",
                                                "ohne Zuwanderungsgeschichte"))


    # Für Grafik als Faktor speichern
    df$indikator<- as.factor(df$indikator)
    df$indikator <- factor(df$indikator, levels = c("ohne Zuwanderungsgeschichte",
                                                    "mit Zuwanderungsgeschichte (ein Elternteil)",
                                                    "mit Zuwanderungsgeschichte (beide Elternteile)"))

  }

  # Datensatzaufbereitung bei Auswahl Bildungskapital/sozialem Status
  if(indikator_select == "nach Bildungskapital") {
    df <- df %>% dplyr::filter(indikator %in% c("Alle", "kapital_hoch", "kapital_niedrig"))
    df <- df %>% dplyr::filter(geschlecht == "gesamt")%>%
      dplyr::mutate(indikator=dplyr::case_when(indikator == "Alle" ~"Gesamt",
                                               indikator == "kapital_hoch" ~ "hohes Bildungskapital",
                                               indikator == "kapital_niedrig" ~ "niedriges Bildungskapital"))

    df <- df %>% dplyr::mutate(geschlecht=dplyr::case_when(
      geschlecht=="gesamt" ~ "Gesamt",
      T~df$geschlecht
    ))
    # Für Grafik als Faktor speichern
    df$indikator<- as.factor(df$indikator)
    df$indikator <- factor(df$indikator, levels = c("hohes Bildungskapital", "niedriges Bildungskapital" ))

    # Alle als Gesamtgruppe ausfiltern
    df <- df %>%
      dplyr::filter(indikator!="Alle")
  }


  if(indikator_select == "nach sozialem Status") {
    df <- df %>% dplyr::filter(indikator %in% c("hoher Status", "niedriger Status"))
    df <- df %>% dplyr::filter(geschlecht == "gesamt")

    # Für Grafik als Faktor speichern
    df$indikator<- as.factor(df$indikator)
    df$indikator <- factor(df$indikator, levels = c("hoher Status", "niedriger Status" ))

  }

  if(klasse_select == "9. Klasse" & indikator_select == "nach Zuwanderungsgeschichte") {
    color <- c("#efe8e6", "#AFF3E0", "#66cbaf")
  }else{
    color <- c("#efe8e6", "#66cbaf")
  }


  # Plot

  if(indikator_select == "nach Geschlecht" & klasse_select == "4. Klasse"){
    if(score_select == "Mindeststandard"){

      df <- df %>%
        dplyr::mutate(display_rel = prettyNum(df$wert, big.mark = ".", decimal.mark = ","))

     out <- highcharter::hchart(df, 'column', highcharter::hcaes(y = wert, x = jahr, group = geschlecht))%>%
        highcharter::hc_plotOptions(column = list(pointWidth = 90))%>%
        highcharter::hc_tooltip(pointFormat = "{point.geschlecht} <br> Anteil Mindeststandard nicht erreicht: {point.display_rel} %")%>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value} %")) %>%
        highcharter::hc_xAxis(title = list(text = ""), categories = c("2011",
                                                                      "2016",
                                                                      "2021")
        ) %>%
        highcharter::hc_colors(c("#154194",
                                 "#efe8e6"
        )) %>%
        highcharter::hc_title(text = paste0("Anteil der Schüler:innen, die den Mindeststandard in Mathematik nicht erreichen, nach Geschlecht in " , bl_select, " (", klasse_select, ")"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "Calibri Regular", fontSize = "14px")
        ) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
       highcharter::hc_caption(text = "Quelle der Daten: Institut zur Qualitätsentwicklung im Bildungswesen, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
                               style = list(fontSize = "11px", color = "gray")) %>%
       highcharter::hc_exporting(enabled = TRUE,
                                 buttons = list(
                                   contextButton = list(
                                     menuItems = list("downloadPNG", "downloadCSV")
                                   )
                                 )
       )
    } else{
      df$wert <- round(df$wert,1)

      out <- highcharter::hchart(df, 'column', highcharter::hcaes(y = wert, x = jahr, group = geschlecht))%>%
        highcharter::hc_plotOptions(column = list(pointWidth = 90))%>%
        highcharter::hc_tooltip(pointFormat = "{point.geschlecht} <br> Durchschnittliche Punktzahl: {point.y}")%>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}"),
                              min=300) %>%
        highcharter::hc_xAxis(title = list(text = ""), categories = c("2011",
                                                                      "2016",
                                                                      "2021")
        ) %>%
        highcharter::hc_colors(c("#154194",
                                 "#efe8e6"
        )) %>%
        highcharter::hc_title(text = paste0("Durchschnittliche Leistung der Schüler:innen im Mathematik-Kompetenztest nach Geschlecht in " , bl_select, " (", klasse_select, ")"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "Calibri Regular", fontSize = "14px")
        ) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
        highcharter::hc_caption(text = "Quelle der Daten: Institut zur Qualitätsentwicklung im Bildungswesen, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
                                style = list(fontSize = "11px", color = "gray")) %>%
        highcharter::hc_exporting(enabled = TRUE,
                                  buttons = list(
                                    contextButton = list(
                                      menuItems = list("downloadPNG", "downloadCSV")
                                    )
                                  )
        )
    }
  }

  else{
    if(indikator_select == "nach Geschlecht" & klasse_select == "9. Klasse") {
      df$wert <- round(df$wert,1)

      out <- highcharter::hchart(df, 'column', highcharter::hcaes(y = wert, x = jahr, group = geschlecht))%>%
        highcharter::hc_plotOptions(column = list(pointWidth = 90))%>%
        highcharter::hc_tooltip(pointFormat = "{point.geschlecht} <br> Durchschnittliche Punktzahl: {point.y}")%>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}"),
                              min=300) %>%
        highcharter::hc_xAxis(title = list(text = ""), categories = c("2012",
                                                                      "2018")
        ) %>%
        highcharter::hc_colors(c("#154194",
                                 "#efe8e6"
        )) %>%
        highcharter::hc_title(text = paste0("Durchschnittliche Leistung der Schüler:innen im ", fach_select, "-Kompetenztest nach Geschlecht in " , bl_select, " (", klasse_select, ")"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "Calibri Regular", fontSize = "14px")
        ) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
        highcharter::hc_caption(text = "Quelle der Daten: Institut zur Qualitätsentwicklung im Bildungswesen, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
                                style = list(fontSize = "11px", color = "gray")) %>%
        highcharter::hc_exporting(enabled = TRUE,
                                  buttons = list(
                                    contextButton = list(
                                      menuItems = list("downloadPNG", "downloadCSV")
                                    )
                                  )
        )
    }else {
      if(klasse_select == "4. Klasse") {

        df$wert <- round(df$wert,1)

        out <- highcharter::hchart(df, 'column', highcharter::hcaes(y = wert, x = jahr, group = indikator))%>%
          highcharter::hc_plotOptions(column = list(pointWidth = 90))%>%
          highcharter::hc_tooltip(pointFormat = "{point.indikator} <br> Durchschnittliche Punktzahl: {point.y}")%>%
          highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}"),min=300) %>%
          highcharter::hc_xAxis(title = list(text = ""), categories = c("2011",
                                                                        "2016",
                                                                        "2021")) %>%
          highcharter::hc_colors(c("#efe8e6", "#66cbaf"
          )) %>%
          highcharter::hc_title(text = paste0("Durchschnittliche Leistung der Schüler:innen im Mathematik-Kompetenztest ", indikator_select, " in " , bl_select, " (", klasse_select, ")"),
                                margin = 45,
                                align = "center",
                                style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
          highcharter::hc_chart(
            style = list(fontFamily = "Calibri Regular", fontSize = "14px")
          ) %>%
          highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
          highcharter::hc_caption(text = "Quelle der Daten: Institut zur Qualitätsentwicklung im Bildungswesen, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
                                  style = list(fontSize = "11px", color = "gray")) %>%
          highcharter::hc_exporting(enabled = TRUE,
                                    buttons = list(
                                      contextButton = list(
                                        menuItems = list("downloadPNG", "downloadCSV")
                                      )
                                    )
          )
      }else{
        if(bl_select %in% c("Berlin", "Bremen", "Saarland")){
          df <- df %>% dplyr::filter(jahr == "2018")
          df$wert <- round(df$wert,1)

          out <- highcharter::hchart(df, 'column', highcharter::hcaes(y = wert, x = jahr, group = indikator))%>%
            highcharter::hc_plotOptions(column = list(pointWidth = 90))%>%
            highcharter::hc_tooltip(pointFormat = "{point.indikator} Durchschnittliche Punktzahl: {point.y}")%>%
            highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}"),min=300) %>%
            highcharter::hc_xAxis(title = list(text = "")) %>%
            highcharter::hc_colors(color) %>%
            highcharter::hc_title(text = paste0("Durchschnittliche Leistung der Schüler:innen im ", fach_select, "-Kompetenztest ", indikator_select, " in " , bl_select, " (", klasse_select, ")"),
                                  margin = 45,
                                  align = "center",
                                  style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
            highcharter::hc_chart(
              style = list(fontFamily = "Calibri Regular", fontSize = "14px")
            ) %>%
            highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
            highcharter::hc_caption(text = "Quelle der Daten: Institut zur Qualitätsentwicklung im Bildungswesen, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
                                    style = list(fontSize = "11px", color = "gray")) %>%
            highcharter::hc_exporting(enabled = TRUE,
                                      buttons = list(
                                        contextButton = list(
                                          menuItems = list("downloadPNG", "downloadCSV")
                                        )
                                      )
            )
        }else{
          df$wert <- round(df$wert,1)

          out <- highcharter::hchart(df, 'column', highcharter::hcaes(y = wert, x = jahr, group = indikator))%>%
            highcharter::hc_plotOptions(column = list(pointWidth = 90))%>%
            highcharter::hc_tooltip(pointFormat = "{point.indikator} Durchschnittliche Punktzahl: {point.y}")%>%
            highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}"),min=300) %>%
            highcharter::hc_xAxis(title = list(text = ""), categories = c("2012",
                                                                          "2018")) %>%
            highcharter::hc_colors(color) %>%
            highcharter::hc_title(text = paste0("Durchschnittliche Leistung der Schüler:innen im ", fach_select, "-Kompetenztest ", indikator_select, " in " , bl_select, " (", klasse_select, ")"),
                                  margin = 45,
                                  align = "center",
                                  style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
            highcharter::hc_chart(
              style = list(fontFamily = "Calibri Regular", fontSize = "14px")
            ) %>%
            highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
            highcharter::hc_caption(text = "Quelle der Daten: Institut zur Qualitätsentwicklung im Bildungswesen, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
                                    style = list(fontSize = "11px", color = "gray")) %>%
            highcharter::hc_exporting(enabled = TRUE,
                                      buttons = list(
                                        contextButton = list(
                                          menuItems = list("downloadPNG", "downloadCSV")
                                        )
                                      )
            )
        }

      }}

    return(out)
  }

}

#' A function to create a bar plot
#'
#' @description A function to return iqb survey scores by gender
#'
#' @return The return value is a bar plot
#' @param data The dataframe "iqb" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd


iqb_fragebogen <- function(r){

  # reactive values einlesen
  jahr_select <- r$jahr_iqb_fragebogen
  fach_select <- r$fach_iqb_fragebogen



    df_query <- glue::glue_sql("
    SELECT fach, indikator, geschlecht, jahr, wert
    FROM iqb
    WHERE typ = 'fragen'
    AND jahr = {jahr_select}
    AND fach = {fach_select}
    AND NOT geschlecht = 'Gesamt'
                               ", .con = con)
  df <- DBI::dbGetQuery(con, df_query)


    # als Faktor speichern für Reihenfolge und Selbstkonzept umbennenen
    df <- df %>%
      dplyr::mutate(
          indikator = dplyr::case_when(
            indikator == "Selbstkonzept" ~ "Selbsteinschätzung der eigenen Fähigkeiten",
            indikator == "Interesse" ~ "Interesse für das Fach"
          ))

  df <- df %>%
    dplyr::mutate(display_rel = prettyNum(round(df$wert,1), big.mark = ".", decimal.mark = ","))


  df$geschlecht <- as.factor(df$geschlecht)
  df$geschlecht <- factor(df$geschlecht, levels = c("Mädchen", "Jungen"))

  # plot
  out <- highcharter::hchart(df, 'column', highcharter::hcaes(y = round(wert, 1), x = indikator, group = geschlecht))%>%
    highcharter::hc_plotOptions(column = list(pointWidth = 90))%>%
    highcharter::hc_tooltip(pointFormat = "{point.geschlecht} <br> {point.display_rel}")%>%
    highcharter::hc_yAxis(title = list(text = "Skalenwert  1 - 4"),
                          labels = list(format = "{value}"),
                          pointsWidth = 4,
                          min = 1) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_colors(c("#154194",
                             "#efe8e6")) %>%
    highcharter::hc_title(text = paste0("Selbsteinschätzung des Interesses und der eigenen Fähigkeiten in ", fach_select,
                                        " von Schüler:innen der 4. Klasse (", jahr_select, ")"
    ),
    margin = 45,
    align = "center",
    style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "Calibri Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
    highcharter::hc_caption(text = "Quelle der Daten: Institut zur Qualitätsentwicklung im Bildungswesen, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
                            style = list(fontSize = "11px", color = "gray")) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG", "downloadCSV")
                                )
                              )
    )

  return(out)

}




