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

  # df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
  #   dplyr::filter(jahr == timerange &
  #                   landkreis == "alle Landkreise" &
  #                   bundesland == regio &
  #                   anforderung == "Gesamt" &
  #                   geschlecht == "Gesamt" &
  #                   indikator %in% gruppe &
  #                   fachbereich == faecher)%>%
  #   dplyr::select( "indikator", "bundesland", "landkreis", "fachbereich",
  #                  "landkreis_zusatz", "landkreis_nummer", "jahr", "anforderung", "geschlecht", "wert") %>%
  #   dplyr::collect()

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
  #

  if(is.null(abs_rel) | abs_rel == "In Prozent"){
    #Anteil MINT berechnen
    # df_new_gesamt <- dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
    #   dplyr::filter(jahr == timerange &
    #                   landkreis == "alle Landkreise" &
    #                   bundesland == regio &
    #                   anforderung == "Gesamt" &
    #                   geschlecht == "Gesamt" &
    #                   indikator %in% gruppe &
    #                   fachbereich == "Alle")%>%
    #   dplyr::select( "indikator", "bundesland", "landkreis", "fachbereich",
    #                  "landkreis_zusatz", "landkreis_nummer", "jahr", "anforderung", "geschlecht", "wert") %>%
    #   dplyr::rename(wert_gesamt = "wert") %>%
    #   dplyr::select(-fachbereich) %>%
    #   dplyr::collect()

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
  }

  #Graifken
  if(betrachtung == "Einzelansicht - Kuchendiagramm"){

    df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")


    #titel <- ist_saarland(gruppe, regio, timerange)
    titel <- ifelse(regio != "Saarland",
                    paste0(gruppe, " in ", regio, " (", timerange, ")"),
                    paste0(gruppe, " im ", regio, " (", timerange, ")"))

    tooltip <- paste('Anteil: {point.percentage:.0f} % <br> Anzahl: {point.wert}')
    format <- '{point.percentage:.0f}%'
    color <- c("#b16fab","#efe8e6")

   out <- piebuilder(df, titel, x="fachbereich", y = "proportion", tooltip, color, format)

  }
  else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){


    if(abs_rel == "In Prozent"){

      #Trennpunkte für lange Zahlen ergänzen
      df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

      df <- df[with(df, order(proportion, decreasing = TRUE)), ]
      titel <- ifelse(regio == "Saarland",
                      paste0("MINT-Anteil unterschiedlicher Beschäftigtengruppen im ", regio, " (", timerange, ")"),
                      paste0("MINT-Anteil unterschiedlicher Beschäftigtengruppen in ", regio, " (", timerange, ")"))
      format <- "{value}%"
      color <- c("#efe8e6","#b16fab")
      tooltip <- "{point.fachbereich} <br> Anteil: {point.y} % <br> Anzahl: {point.wert}"
      optional = list(bar = list(stacking = "percent"))

      out <- balkenbuilder(df, titel, x="indikator", y = "proportion", group = "fachbereich", tooltip, format, color, optional)


    }else{

      #Trennpunkte für lange Zahlen ergänzen
      df$wert_disp <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
      df <- df[with(df, order(wert, decreasing = TRUE)), ]


      titel <- ifelse(regio == "Saarland",
                      paste0("Beschäftigte in MINT in unterschiedlichen Beschäftigtengruppen im ", regio, " (", timerange, ")"),
                      paste0("Beschäftigte in MINT in unterschiedlichen Beschäftigtengruppen in ", regio, " (", timerange, ")"))
      out <- balkenbuilder(df, titel, x="indikator", y="wert",group=NULL, tooltip = "Anzahl: {point.wert_disp}", format = "{value:, f}", color = "#b16fab")
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
  #   dplyr::filter(jahr %in% t &
  #                   bundesland == regio &
  #                   landkreis == "alle Landkreise" &
  #                   geschlecht == "Gesamt" &
  #                   anforderung == "Gesamt" &
  #                   fachbereich == "MINT" &
  #                   indikator %in% indi
  #   )%>%
  #   dplyr::select(indikator, bundesland, fachbereich, jahr, wert) %>%
  #   dplyr::collect()

  df_query <- glue::glue_sql("
  SELECT *
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

  df <- df %>%
    dplyr::select(indikator, bundesland, fachbereich, jahr, wert)

  if (absolut_selector == "In Prozent"){

    # df_alle <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
    #   dplyr::filter(jahr %in% t &
    #                   bundesland == regio &
    #                   landkreis == "alle Landkreise" &
    #                   geschlecht == "Gesamt" &
    #                   anforderung == "Gesamt" &
    #                   fachbereich == "Alle" &
    #                   indikator %in% indi
    #   )%>%
    #   dplyr::select(indikator, bundesland, fachbereich, jahr, wert) %>%
    #   dplyr::collect()

    df_query <- glue::glue_sql("
    SELECT *
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

    df_alle <- df_alle %>%
      dplyr::select(indikator, bundesland, fachbereich, jahr, wert)


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

    titel <- ifelse(regio == "Saarland",
                    paste0("MINT-Anteil unterschiedlicher Beschäftigtengruppen im ", regio),
                    paste0("MINT-Anteil unterschiedlicher Beschäftigtengruppen in ", regio))
    tooltip <- "{point.indikator} <br> Anteil: {point.prop_disp} %"
    format <- "{value}%"
    color <- c("#b16fab", "#154194","#66cbaf","#112c5f", "#35bd97", "#5d335a",
               "#5f94f9", "#007655", "#d0a9cd")

    # plot


    out <- linebuilder(df, titel, x = "jahr", y = "prop", group = "indikator", tooltip, format, color)

  } else if(absolut_selector == "Anzahl") {

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    df$wert_disp <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

    # order years for plot
    df <- df[with(df, order(jahr, decreasing = FALSE)), ]

    titel <- paste0("Anteil von MINT-Beschäftigten und -Auszubildenden an allen Beschäftigten o. Auszubildenden in ", regio)
    tooltip <- "{point.indikator} <br> Anteil: {point.wert_disp}"
    format <- "{value:, f}"
    color <- c("#b16fab", "#154194","#66cbaf", "#35bd97", "#5d335a",
               "#5f94f9", "#007655", "#d0a9cd", "#112c5f")
    out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "indikator", tooltip, format, color)


    # plot




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

    # df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
    #   dplyr::filter(
    #     jahr == timerange &
    #       indikator == indi &
    #       landkreis == "alle Landkreise" &
    #       anforderung == "Gesamt" &
    #       geschlecht == "Gesamt"&
    #       fachbereich %in% c("MINT", "Alle") &
    #       !(bundesland %in% c("Deutschland", "Westdeutschland (o. Berlin)", "Ostdeutschland (einschl. Berlin)")))%>%
    #   dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)%>%
    #   dplyr::collect()

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

    df <-df
    joinby <- c("name", "bundesland")
    name <- paste0("MINT")
    tooltip <- "{point.bundesland} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.wert}"
    titel <- paste0("Anteil von ",  title_help, " in MINT an allen ",  title_help, " (", timerange, ")")
    mincolor <- "#f4f5f6"
    map_selection <- 1
    maxcolor <- "#b16fab"
    out <- mapbuilder(df, joinby,name, tooltip, titel, mincolor, maxcolor,prop=FALSE, wert=FALSE, map=map_selection)


  }
  else if(betrachtung == "Gruppenvergleich - Balkendiagramm" ){
    timerange <- r$zeit_beruf_mint_bula_balken
    indikator_choice <- r$indikator_beruf_mint_bula_balken

    # df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
    #   dplyr::filter(
    #     jahr == timerange &
    #       indikator == indikator_choice &
    #       landkreis == "alle Landkreise" &
    #       anforderung == "Gesamt" &
    #       geschlecht == "Gesamt" &
    #       fachbereich %in% c("MINT", "Alle"))%>%
    #   dplyr::select(`bundesland`, `jahr`, `indikator`, `fachbereich`, `wert`)%>%
    #   dplyr::collect()

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
    df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
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
    optional <- list(bar = list(
      colorByPoint = TRUE,
      colors = ifelse(df$bundesland == "Deutschland", "#b16fab",
                      ifelse(df$bundesland == "Ostdeutschland (inkl. Berlin)", "#d3a4d7",
                             ifelse(df$bundesland == "Westdeutschland (o. Berlin)", "#d3a4d7", "#A9A9A9")))))

    out <- balkenbuilder(df, titel, x="bundesland", y="prop", group = NULL, tooltip = "{point.fachbereich} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.wert}", format = "{value}%", color = "#b16fab", optional = optional)


  }
  else if(betrachtung == "Zeitverlauf - Liniendiagramm"){
    timerange <-r$zeit_beruf_mint_bula_verlauf
    t <- timerange[1]:timerange[2]
    aniveau <- r$indikator_beruf_mint_bula_verlauf
    states <- r$region_beruf_mint_bula_verlauf
    absolut_selector <- r$abs_beruf_mint_bula_verlauf
#
#     # filter dataset based on UI inputs
#     df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
#       dplyr::filter(jahr %in% t,
#                     landkreis == "alle Landkreise",
#                     geschlecht == "Gesamt",
#                     anforderung == "Gesamt",
#                     fachbereich %in% c("Alle", "MINT"),
#                     bundesland %in% states,
#                     indikator == aniveau
#       )%>%
#       dplyr::select(
#         "indikator",
#         "fachbereich",
#         #"geschlecht",
#         "bundesland",
#         "jahr",
#         #"anforderung",
#         "wert" ) %>%
#       dplyr::collect()


    df_query <- glue::glue_sql("
    SELECT *
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
      dplyr::select(
        "indikator",
        "fachbereich",
        #"geschlecht",
        "bundesland",
        "jahr",
        #"anforderung",
        "wert" )



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
      df$display_rel <- prettyNum(round(df$wert,1), big.mark = ".", decimal.mark = ",")
      df <- df[with(df, order(bundesland, jahr, decreasing = FALSE)), ]

      titel <- paste0("Anteil von ", title_help, " in MINT-Berufen an allen ", title_help)
      tooltip <-"Anteil <br> Bundesland: {point.region} <br> Wert: {point.display_rel} %"
      format <- "{value:, f}"
      color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                 "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
      out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "bundesland", tooltip, format, color)



    } else if(absolut_selector=="Anzahl"){

      hcoptslang <- getOption("highcharter.lang")
      hcoptslang$thousandsSep <- "."
      options(highcharter.lang = hcoptslang)

      df <- df %>%
        dplyr::filter(selector=="Anzahl")
      df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
      df <- df[with(df, order(bundesland, jahr, decreasing = FALSE)), ]





      titel <- paste0("Anzahl von ", title_help, " in MINT-Berufen")
      tooltip <- "Anzahl: {point.display_abs}"
      format <- "{value:, f}"
      color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                 "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
      out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "bundesland", tooltip, format, color)

    }
  }
 return(out)
}

### Nicht Box 1 ----
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


  # df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
  #   dplyr::filter(jahr %in% timerange &
  #                   bundesland == "Deutschland" &
  #                   geschlecht != "Gesamt"&
  #                   anforderung == "Gesamt" &
  #                   indikator %in% c("Auszubildende",
  #                                    "Auszubildende (1. Jahr)",
  #                                    "Beschäftigte",
  #                                    "ausländische Auszubildende",
  #                                    "ausländische Beschäftigte")&
  #                   fachbereich %in% c("Alle", "MINT", "Mathematik, Naturwissenschaften",
  #                                      "Informatik", "Technik (gesamt)"))%>%
  #   dplyr::select(indikator, fachbereich, wert, geschlecht) %>%
  #   dplyr::collect()


  df_query <- glue::glue_sql("
  SELECT *
  FROM arbeitsmarkt_detail
  WHERE jahr IN ({timerange*})
  AND bundesland = 'Deutschland'
  AND NOT geschlecht = 'Gesamt'
  AND anforderung = 'Gesamt'
  AND indikator IN ('Auszubildende', 'Auszubildende (1. Jahr)', 'Beschäftigte', 'ausländische Auszubildende', 'ausländische Beschäftigte')
  AND fachbereich IN ('Alle', 'MINT', 'Mathematik, Naturwissenschaften', 'Informatik', 'Technik (gesamt)')
                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)

  df <- df %>%
    dplyr::select(indikator, fachbereich, wert, geschlecht)



  # Berechnung von andere Fächergruppen
  df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"]-
    df[df$fachbereich == "MINT", "wert"]
  df$fachbereich[df$fachbereich == "Alle"]<-"andere Fächergruppen"
  df <- df %>% dplyr::filter(fachbereich != "MINT")



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

  if(timerange == 2021) indikator_choice <- r$level_arbeitsmarkt_bl_gender_21
  if(timerange == 2022) indikator_choice <- r$level_arbeitsmarkt_bl_gender_22

  fachbereich_choice <- r$fach_arbeitsmarkt_bl_gender
#
#   df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
#     dplyr::filter(jahr %in% timerange &
#                     !bundesland %in% c("Deutschland",
#                                         "Westdeutschland (o. Berlin)",
#                                         "Ostdeutschland (einschl. Berlin)") &
#                     landkreis == "alle Landkreise" &
#                     geschlecht != "Gesamt"&
#                     anforderung == "Gesamt" )%>%
#     dplyr::select(indikator, fachbereich, wert, geschlecht, bundesland, jahr) %>%
#     dplyr::collect()
#
#
  df_query <- glue::glue_sql("
  SELECT *
  FROM arbeitsmarkt_detail
  WHERE jahr IN ({timerange*})
  AND NOT bundesland IN ('Deutschland', 'Westdeutschland (o. Berlin)', 'Ostdeutschland (einschl. Berlin)')
  AND landkreis = 'alle Landkreise'
  AND NOT geschlecht = 'Gesamt'
  AND anforderung = 'Gesamt'
                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)

  df <- df %>%
    dplyr::select(indikator, fachbereich, wert, geschlecht, bundesland, jahr)


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


  #Überschrift erstellen
  title_help <- paste0(indikator_choice, "r")
  title_help <- ifelse(grepl("ausländische Beschäftigte", indikator_choice), "ausländischer Beschäftigter", title_help)
  title_help <- ifelse(grepl("ausländische Auszubildende", indikator_choice), "ausländischer Auszubildender", title_help)
  title_help <- ifelse(grepl("Jahr", indikator_choice), "Auszubildender mit neuem Lehrvertrag", title_help)

  titel_w <- ifelse(fachbereich_choice == "Andere Berufsgruppen", paste0("Anteil weiblicher ", title_help, ", die kein MINT-Berufsfeld wählen (", timerange, ")"),
                    paste0("Anteil weiblicher ", title_help, ", die das Berufsfeld ", fachbereich_choice, " wählen (", timerange, ")"))
  titel_m <- ifelse(fachbereich_choice == "Andere Berufsgruppen", paste0("Anteil männlicher ", title_help, ", die kein MINT-Berufsfeld wählen (", timerange, ")"),
                    paste0("Anteil männlicher ", title_help, ", die das Berufsfeld ", fachbereich_choice, " wählen (", timerange, ")"))



    # plot

    df <-values_female
    joinby <- c("name", "bundesland")
    name <- paste0(fachbereich_choice)
    tooltip <- "{point.bundesland} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}"
    titel <-titel_w
    mincolor <- "#f4f5f6"
    maxcolor <- "#b16fab"
    map_selection <- 1
    out1 <- mapbuilder(df, joinby,name, tooltip, titel, mincolor, maxcolor,prop=FALSE, wert=FALSE, map=map_selection)



    df <-values_male
    joinby <- c("name", "bundesland")
    name <- paste0(fachbereich_choice)
    tooltip <- "{point.bundesland} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}"
    titel <-titel_m
    mincolor <- "#f4f5f6"
    maxcolor <- "#b16fab"
    map_selection <- 1
    out2 <- mapbuilder(df, joinby,name, tooltip, titel, mincolor, maxcolor,prop=FALSE, wert=FALSE, map=map_selection)


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
  indikator_choice <- r$indikator_beruf_arbeitsmarkt_bl_gender_verlauf
  states <- r$states_beruf_arbeitsmarkt_bl_gender_verlauf
  t <- as.character(timerange[1]:timerange[2])
#
#
#   df <-  dplyr::tbl(con, from = "arbeitsmarkt")%>%
#     dplyr::filter(jahr %in% t,
#                   indikator == indikator_choice,
#                   region %in% states,
#                   anforderung %in% "Gesamt",
#                   geschlecht == "Frauen",
#                   fachbereich %in% c("Alle", "MINT")
#     )%>%
#   dplyr::select("bereich",
#   "indikator",
#   "fachbereich",
#   "geschlecht",
#   "region",
#   "jahr",
#   "anforderung",
#   "wert" ) %>%
#     dplyr::collect()

  df_query <- glue::glue_sql("
  SELECT *
  FROM arbeitsmarkt
  WHERE jahr IN ({t*})
  AND indikator = {indikator_choice}
  AND region IN {states}
  AND anforderung = 'Gesamt'
  AND geschlecht = 'Frauen'
  AND fachbereich IN c('Alle', 'MINT')
                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)

  df <- df %>%
    dplyr::select("bereich",
                  "indikator",
                  "fachbereich",
                  "geschlecht",
                  "region",
                  "jahr",
                  "anforderung",
                  "wert" )


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

    titel <- paste0("Anteil weiblicher ", title_help, ", die MINT-Berufe wählen")
    tooltip <-"{point.region} <br> Anteil: {point.y} %"
    format <- "{value}%"
    color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
               "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
    out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color)


  }else if(absolut_selector=="Anzahl"){

    title_help <- paste0(indikator_choice, "r")

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    df <- df %>%
      dplyr::filter(selector == "Anzahl")

    df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]


    # plot
    titel <- paste0("Anzahl weiblicher ", title_help, ", die MINT-Berufe wählen")
    tooltip <- "Anzahl: {point.y}"
    format <-  "{value:, f}"
    color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
               "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
    out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color)

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


  absolut_selector <- r$abs_zahlen_4
  aniveau <- r$niveau
  # load UI inputs from reactive value
  timerange <- r$date_beruf_arbeitsmarkt_bl_verlauf
  t <- as.character(timerange[1]:timerange[2])
  states <- r$states_beruf_arbeitsmarkt_bl_verlauf

  # filter dataset based on UI inputs

  # df <-  dplyr::tbl(con, from = "arbeitsmarkt")%>%
  #   dplyr::filter(jahr %in% t &
  #                   geschlecht == "Gesamt" &
  #                   anforderung == "Gesamt",
  #                 fachbereich %in% c("Alle", "MINT"),
  #                 region %in% states,
  #                 indikator == aniveau
  #   )%>%
  #   dplyr::select(
  #                 "indikator",
  #                 "fachbereich",
  #                 #"geschlecht",
  #                 "region",
  #                 "jahr",
  #                 #"anforderung",
  #                 "wert" ) %>%
  #   dplyr::collect()

  df_query <- glue::glue_sql("
  SELECT *
  FROM arbeitsmarkt
  WHERE jahr IN ({t*})
  AND geschlecht = 'Gesamt'
  AND anforderung = 'Gesamt'
  AND fachbereich IN ('Alle', 'MINT')
  AND region IN ({states*})
  AND indikator = {aniveau}

                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)

  df <- df %>%
    dplyr::select(
      "indikator",
      "fachbereich",
      #"geschlecht",
      "region",
      "jahr",
      #"anforderung",
      "wert" )


  df <- df%>%
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

    titel <- paste0("Anteil von ", title_help, " in MINT-Berufen an allen ", title_help)
    tooltip <- "Anteil <br> Bundesland: {point.region} <br> Wert: {point.display_rel} %"
    format <- "{value}%"
    color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
               "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
    out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color)



  } else if(absolut_selector=="Anzahl"){


    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    df <- df %>%
      dplyr::filter(selector=="Anzahl")

    df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

    df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]


    # plot

    titel <- paste0("Anzahl von ", title_help, " in MINT-Berufen")
    tooltip <- "Anzahl: {point.display_abs}"
    format <-  "{value:, f}"
    color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
               "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
    out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color)


  }

}

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
      # df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
      #   dplyr::filter(jahr %in% timerange &
      #                   landkreis == "alle Landkreise" &
      #                   bundesland == regio &
      #                   geschlecht == "Gesamt" &
      #                   anforderung == "Gesamt" &
      #                   fachbereich %in% c(
      #                     "Mathematik, Naturwissenschaften",
      #                     "Informatik", "Technik (gesamt)") &
      #                   indikator %in% indikator_choice)%>%
      #   dplyr::select(indikator, jahr, bundesland, fachbereich, wert) %>%
      #   dplyr::collect()
      #

      df_query <- glue::glue_sql("
      SELECT *
      FROM arbeitsmarkt_detail
      WHERE jahr IN ({timerange*})
      AND landkreis = 'alle Landkreise'
      AND bundesland = {regio}
      AND geschlecht = 'Gesamt'
      AND anforderung = 'Gesamt'
      AND fachbereich IN ('Mathematik', 'Naturwissenschaften', 'Informatik', 'Technik (gesamt)')
      AND indikator IN ({indikator_choice*})
                               ", .con = con)

      df <- DBI::dbGetQuery(con, df_query)

      df <- df %>%
        dplyr::select(indikator, jahr, bundesland, fachbereich, wert)

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
      df$titel_help <- ifelse(df$indikator == "Beschäftigte 25-55", "Beschäftigten <br> zwischen 25 und 55 Jahren ", df$titel_help)
      df$titel_help <- ifelse(df$indikator == "Beschäftigte u25", "Beschäftigten <br> unter 25 Jahren ", df$titel_help)
      df$titel_help <- ifelse(df$indikator == "Beschäftigte ü55", "Beschäftigten <br> über 55 Jahren ", df$titel_help)


      df$wert_disp <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
      df$prop_disp <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")
    }
    else{
      # df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
      #   dplyr::filter(jahr %in% timerange &
      #                   landkreis == "alle Landkreise" &
      #                   bundesland == regio &
      #                   geschlecht == "Gesamt" &
      #                   anforderung == "Gesamt" &
      #                   fachbereich %in% c("Alle", "MINT",
      #                                      "Mathematik, Naturwissenschaften",
      #                                      "Informatik", "Technik (gesamt)") &
      #                   indikator %in% indikator_choice)%>%
      #   dplyr::select(indikator, jahr, bundesland, fachbereich, wert) %>%
      #   dplyr::collect()

      df_query <- glue::glue_sql("
      SELECT *
      FROM arbeitsmarkt_detail
      WHERE jahr IN ({timerange*})
      AND landkreis = 'alle Landkreise'
      AND bundesland = {regio}
      AND geschlecht = 'Gesamt'
      AND anforderung = 'Gesamt'
      AND fachbereich IN ('Alle', 'MINT', 'Mathematik, Naturwissenschaften', 'Informatik", "Technik (gesamt)')
      AND indikator IN ({indikator_choice*})
                               ", .con = con)

      df <- DBI::dbGetQuery(con, df_query)

      df <- df %>%
        dplyr::select(indikator, jahr, bundesland, fachbereich, wert)

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
      df$titel_help <- ifelse(df$indikator == "Auszubildende (1. Jahr)", "Auszubildenden mit neuem Lehrvertrag ", df$titel_help)
      df$titel_help <- ifelse(df$indikator == "ausländische Auszubildende", "ausländischen Auszubildenden ", df$titel_help)
      df$titel_help <- ifelse(df$indikator == "ausländische Beschäftigte", "ausländischen Beschäftigten ", df$titel_help)
      df$titel_help <- ifelse(df$indikator == "Beschäftigte 25-55", "Beschäftigten 25-55 ", df$titel_help)
      df$titel_help <- ifelse(df$indikator == "Beschäftigte u25", "Beschäftigten u25 ", df$titel_help)
      df$titel_help <- ifelse(df$indikator == "Beschäftigte ü55", "Beschäftigten ü55 ", df$titel_help)

      df$wert_disp <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
      df$prop_disp <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")
    }

    if(length(indikator_choice) == 1) {

      df <- df[with(df, order(prop, decreasing = FALSE)), ]
      df <- df %>%
        dplyr::mutate(color = color_fachbereich[fachbereich])

      titel <- paste0("MINT-Anteile von ", unique(df$titel_help), preposition, " ", regio, " (", timerange, ")")
      tooltip <- paste('Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}')
      format <- '{point.prop_disp}%'
      color <- as.character(df$color)

      out <- piebuilder(df, titel, x="fachbereich", y = "prop", tooltip, color, format)

    } else if(length(indikator_choice) == 2) {

      df_1 <- df %>% dplyr::filter(indikator == indikator_choice[1])
      df_1 <- df_1[with(df_1, order(prop, decreasing = FALSE)), ]
      df_1 <- df_1 %>%
        dplyr::mutate(color = color_fachbereich[fachbereich])

      df_2 <- df %>% dplyr::filter(indikator == indikator_choice[2])
      df_2 <- df_2[with(df_2, order(prop, decreasing = FALSE)), ]
      df_2 <- df_2 %>%
        dplyr::mutate(color = color_fachbereich[fachbereich])

      titel1 <- paste0("MINT-Anteile von ", unique(df_1$titel_help), preposition, " ", regio, " (", timerange, ")")
      titel2 <- paste0("MINT-Anteile von ", unique(df_2$titel_help), preposition, " ", regio, " (", timerange, ")")
      tooltip <- paste('Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}')
      format <- '{point.prop_disp}%'
      color1 <- as.character(df_1$color)
      color2 <- as.character(df_2$color)


      out_1 <- piebuilder(df_1, titel1, x="fachbereich", y = "prop", tooltip, color1, format)
      out_2 <- piebuilder(df_2, titel2, x="fachbereich", y = "prop", tooltip, color2, format)


      out <- highcharter::hw_grid(
        out_1, out_2,
        ncol = 2,
        browsable = TRUE
      )
    }

  }else
    if (betrachtung == "Gruppenvergleich - Balkendiagramm"){

    indikator_choice <- r$indikator_arbeitsmarkt_fach_vergleich_balken

    # df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
    #   dplyr::filter(
    #     jahr == timerange &
    #       indikator == indikator_choice &
    #       landkreis == "alle Landkreise" &
    #       anforderung == "Gesamt" &
    #       geschlecht == "Gesamt"&
    #       bundesland == regio &
    #       fachbereich %in% c("Alle", "MINT",
    #                          "Mathematik, Naturwissenschaften",
    #                          "Informatik", "Technik (gesamt)"))%>%
    #   dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)%>%
    #   dplyr::collect()

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

    # df_alle <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
    #   dplyr::filter(
    #     jahr == timerange &
    #       indikator == indikator_choice &
    #       landkreis == "alle Landkreise" &
    #       anforderung == "Gesamt" &
    #       geschlecht == "Gesamt"&
    #       bundesland == regio &
    #       fachbereich == "Alle")%>%
    #   dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)%>%
    #   dplyr::collect()

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

    titel <- paste0( "Überblick über die Berufsfelder von ", title_help, br(), "in ",regio, " (", timerange, ")")
    format <- "{value}%"
    color <- c("#efe8e6","#b16fab")
    tooltip <- hover
    optional = list(bar = list(
      colorByPoint = TRUE,
      colors = as.character(df$color)
    ))

    out <- balkenbuilder(df, titel, x="fachbereich", y = "prop", group=NULL, tooltip, format, color, optional)

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
  # df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
  #   dplyr::filter(
  #     jahr %in% t &
  #       indikator == indi &
  #       landkreis == "alle Landkreise" &
  #       anforderung == "Gesamt" &
  #       geschlecht == "Gesamt"&
  #       bundesland == regio &
  #       fachbereich %in% c(
  #                          "Mathematik, Naturwissenschaften",
  #                          "Informatik", "Technik (gesamt)"))%>%
  #   dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)%>%
  #   dplyr::collect()

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

    # df_alle <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
    #   dplyr::filter(
    #     jahr %in% t &
    #       indikator == indi &
    #       landkreis == "alle Landkreise" &
    #       anforderung == "Gesamt" &
    #       geschlecht == "Gesamt"&
    #       bundesland == regio &
    #       fachbereich == "Alle")%>%
    #   dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)%>%
    #   dplyr::collect()


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
    df$prop_disp <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")
    sorted_indicators <- df %>%
      dplyr::group_by(fachbereich) %>%
      dplyr::summarize(m_value = mean(round(prop, 1), na.rm = TRUE)) %>%
      dplyr::arrange(desc(m_value)) %>%
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
    tooltip <- "{point.indikator} <br> Anteil: {point.prop_disp} %"
    format <- "{value} %"
    color <- as.character(colors)
    out <- linebuilder(df, titel, x = "jahr", y = "prop", group = "fachbereich", tooltip, format, color)

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

    #titlehelper
    title_help <- paste0(indi, "n")
    title_help <- ifelse(grepl("Jahr", indi), "Auszubildenden mit neuem Lehrvertrag", title_help)
    title_help <- ifelse(grepl("ausländische Auszubildende", indi), "ausländischen Auszubildenden", title_help)
    title_help <- ifelse(grepl("ausländische Beschäftigte", indi), "ausländischen Beschäftigten", title_help)
    title_help <- ifelse(grepl("Beschäftigte 25-55", indi), "Beschäftigten zwischen 25 und 55 Jahren", title_help)
    title_help <- ifelse(grepl("Beschäftigte ü55", indi), "Beschäftigten über 55 Jahren", title_help)
    title_help <- ifelse(grepl("Beschäftigte u25", indi), "Beschäftigten unter 25 Jahren", title_help)


    # plot
    titel <- ist_saarland2(optional1="Entwicklung der ", title_help, optional2=" in MINT", regio)
    tooltip <- "{point.indikator} <br> Anzahl: {point.wert_disp}"
    format <- "{value:, f}"
    color <- as.character(colors)
    out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "fachbereich", tooltip, format, color)


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

    # df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
    #   dplyr::filter(
    #     jahr == timerange &
    #       indikator == indi &
    #       landkreis == "alle Landkreise" &
    #       anforderung == "Gesamt" &
    #       geschlecht == "Gesamt"&
    #       fachbereich %in% c(faecher, "Alle") &
    #       !(bundesland %in% c("Deutschland", "Westdeutschland (o. Berlin)", "Ostdeutschland (inkl. Berlin)")))%>%
    #   dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)%>%
    #   dplyr::collect()

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

    df <- df
    joinby <- c("name", "bundesland")
    name <- paste0("MINT")
    tooltip <- "{point.bundesland} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.wert}"
    titel <- paste0("Anteil von ",  title_help, " in ", faecher, " an allen ",  title_help, " (", timerange, ")")
    mincolor <- "#f4f5f6"
    maxcolor <- "#b16fab"
    map_selection <- 1
    out <- mapbuilder(df, joinby,name, tooltip, titel, mincolor, maxcolor,prop=FALSE, wert=FALSE, map=map_selection)



  }
  else if(betrachtung == "Gruppenvergleich - Balkendiagramm" ){
    timerange <- r$zeit_beruf_faecher_bula_balken
    indikator_choice <- r$indikator_beruf_faecher_bula_balken
    faecher <- r$fachbereich_beruf_faecher_bula_balken
#
#     df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
#       dplyr::filter(
#         jahr == timerange &
#           indikator == indikator_choice &
#           landkreis == "alle Landkreise" &
#           anforderung == "Gesamt" &
#           geschlecht == "Gesamt" &
#           fachbereich == faecher) %>%
#       dplyr::select(`bundesland`, `jahr`, `indikator`, `fachbereich`, `wert`)%>%
#       dplyr::collect()
#
#
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
    # df_ges <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
    #   dplyr::filter(
    #     jahr == timerange &
    #       indikator == indikator_choice &
    #       landkreis == "alle Landkreise" &
    #       anforderung == "Gesamt" &
    #       geschlecht == "Gesamt" &
    #       fachbereich == "Alle") %>%
    #   dplyr::select(`bundesland`, `jahr`, `indikator`, `fachbereich`, `wert`)%>%
    #   dplyr::rename(wert_ges = wert) %>%
    #   dplyr::ungroup()%>%
    #   dplyr::collect()

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
    df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
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


    tooltip <- "{point.fachbereich} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.wert}"
    format <- "{value}%"
    color <- "#b16fab"
    titel <- paste0("Anteil von ", title_help, " im Berufsfeld ", faecher, " an allen ", title_help, " in ", timerange)
    optional <- list(bar = list(
      colorByPoint = TRUE,
      colors = ifelse(df$bundesland == "Deutschland", "#b16fab",
                      ifelse(df$bundesland == "Ostdeutschland (inkl. Berlin)", "#d3a4d7",
                             ifelse(df$bundesland == "Westdeutschland (o. Berlin)", "#d3a4d7", "#A9A9A9")))))

    out <- balkenbuilder(df, titel, x= "bundesland", y="prop", group=NULL, tooltip, format, color, optional=optional)


  }
  else if(betrachtung == "Zeitverlauf - Liniendiagramm"){
    timerange <-r$zeit_beruf_faecher_bula_verlauf
    t <- timerange[1]:timerange[2]
    indi <- r$indikator_beruf_faecher_bula_verlauf
    states <- r$region_beruf_faecher_bula_verlauf
    absolut_selector <- r$abs_beruf_faecher_bula_verlauf
    faecher <- r$fachbereich_beruf_faecher_bula_verlauf

    # # filter dataset based on UI inputs
    # df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
    #   dplyr::filter(
    #     jahr %in% t &
    #       indikator == indi &
    #       landkreis == "alle Landkreise" &
    #       bundesland %in% states &
    #       anforderung == "Gesamt" &
    #       geschlecht == "Gesamt" &
    #       fachbereich == faecher) %>%
    #   dplyr::select(`bundesland`, `jahr`, `indikator`, `fachbereich`, `wert`)%>%
    #   dplyr::collect()

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
#       # Alle als extra Spalte anhängen und Anteil berechnen
#       df_ges <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
#         dplyr::filter(
#           jahr %in% t &
#             indikator == indi &
#             landkreis == "alle Landkreise" &
#             bundesland %in% states &
#             anforderung == "Gesamt" &
#             geschlecht == "Gesamt" &
#             fachbereich == "Alle") %>%
#         dplyr::select(`bundesland`, `jahr`, `indikator`, `fachbereich`, `wert`)%>%
#         dplyr::rename(wert_ges = wert) %>%
#         dplyr::ungroup()%>%
#         dplyr::collect()
#
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

      df$display_rel <- prettyNum(round(df$prop,1), big.mark = ".", decimal.mark = ",")
      df <- df[with(df, order(bundesland, jahr, decreasing = FALSE)), ]


      # plot

      titel <-  paste0("Anteil von ", title_help, " im Berufsfeld ", faecher, " im Berufsfeld ")
      tooltip <-"Anteil <br> Bundesland: {point.region} <br> Wert: {point.display_rel} %"
      format <- "{value} %"
      color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                 "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
      out <- linebuilder(df, titel, x = "jahr", y = "prop", group = "bundesland", tooltip, format, color)




    } else if(absolut_selector=="Anzahl"){

      hcoptslang <- getOption("highcharter.lang")
      hcoptslang$thousandsSep <- "."
      options(highcharter.lang = hcoptslang)

      df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
      df <- df[with(df, order(bundesland, jahr, decreasing = FALSE)), ]



      # plot
      titel <-  paste0("Anzahl von ", title_help, " in MINT-Berufen im Berufsfeld ", faecher)
      tooltip <-  "Anzahl: {point.display_abs}"
      format <- "{value:, f}"
      color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                 "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
      out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "bundesland", tooltip, format, color)


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

arbeitsmarkt_überblick_fächer <- function( r) {
  # load UI inputs from reactive value

  timerange <- r$date_arbeitsmarkt_überblick_fächer
  state <- r$state_arbeitsmarkt_überblick_fächer

  if(timerange == 2021) indikator_choice <- r$indikator_arbeitsmarkt_überblick_fächer_21
  if(timerange == 2022) indikator_choice <- r$indikator_arbeitsmarkt_überblick_fächer_22

  # df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
  #   dplyr::filter(
  #     jahr == timerange &
  #       indikator == indikator_choice &
  #       landkreis == "alle Landkreise" &
  #       anforderung == "Gesamt" &
  #       geschlecht == "Gesamt"&
  #       bundesland == state)%>%
  #   dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)%>%
  #   dplyr::collect()

  df_query <- glue::glue_sql("
  SELECT *
  FROM arbeitsmarkt_detail
  WHERE jahr = {timerange}
  AND indikator = {indikator_choice}
  AND landkreis = 'alle Landkreise'
  AND anforderung = 'Gesamt'
  AND geschlecht = 'Gesamt'
  AND bundesland = {state}
                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)

  df <- df %>%
    dplyr::select(`bundesland`, `jahr`, `geschlecht`, `indikator`, `fachbereich`, `wert`)


  # MINT direkt berechnen und nicht-MINT berechnen
  df[df$fachbereich == "MINT", "wert"] <- df[df$fachbereich == "Mathematik, Naturwissenschaften", "wert"]+
    df[df$fachbereich == "Informatik", "wert"]+df[df$fachbereich == "Technik (gesamt)", "wert"]
  df$fachbereich[df$fachbereich == "MINT"]<-"MINT-Berufsfelder (gesamt)"

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

  # df <- dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
  #   dplyr::filter(jahr == timerange &
  #                   landkreis == "alle Landkreise" &
  #                   bundesland == regio &
  #                   anforderung == "Gesamt" &
  #                   geschlecht != "Gesamt" &
  #                   indikator %in% indi &
  #                   fachbereich == faecher)%>%
  #   dplyr::select( "indikator", "bundesland", "fachbereich", "jahr", "geschlecht", "wert") %>%
  #   dplyr::collect()

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

  #
  # df_alle <- dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
  #   dplyr::filter(jahr == timerange &
  #                   landkreis == "alle Landkreise" &
  #                   bundesland == regio &
  #                   anforderung == "Gesamt" &
  #                   geschlecht == "Gesamt" &
  #                   indikator %in% indi &
  #                   fachbereich == faecher)%>%
  #   dplyr::select( "indikator", "bundesland", "fachbereich", "jahr", "geschlecht", "wert") %>%
  #   dplyr::rename(wert_gesamt = "wert") %>%
  #   dplyr::collect()

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

    # df_andere <- dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
    #   dplyr::filter(jahr == timerange &
    #                   landkreis == "alle Landkreise" &
    #                   bundesland == regio &
    #                   anforderung == "Gesamt" &
    #                #   geschlecht != "Gesamt" &
    #                   indikator %in% indi &
    #                   fachbereich == faecher)%>%
    #   dplyr::select( "indikator", "bundesland", "fachbereich", "jahr", "geschlecht", "wert") %>%
    #   dplyr::collect()

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

    # df_alle_faecher <- dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
    #   dplyr::filter(jahr == timerange &
    #                   landkreis == "alle Landkreise" &
    #                   bundesland == regio &
    #                   anforderung == "Gesamt" &
    #                   indikator %in% indi &
    #                   fachbereich == "Alle")%>%
    #   dplyr::select( "indikator", "bundesland", "fachbereich", "jahr", "geschlecht", "wert") %>%
    #   dplyr::rename(wert_gesamt = "wert") %>%
    #   dplyr::collect()

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
     color <- c("#efe8e6", "#154194")
     tooltip <- 'Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}'
     format <- '{point.prop_disp}%'

     p1 <- piebuilder(df_p, titel, x="geschlecht", y = "proportion", tooltip, color, format)
     out <- p1

     if(gegenwert == "Ja"){
       df_g <- df[df$fachbereich == "Andere Berufe",]

       titel1 <- ifelse(regio == "Saarland",
                        paste0("Frauenanteil unter ", title_help, " in Nicht MINT-Berufen im ", regio, " (", timerange, ")"),
                        paste0("Frauenanteil unter ", title_help, " in Nicht MINT-Berufen in ", regio, " (", timerange, ")"))
       tooltip <- paste('Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}')
       format <- '{point.prop_disp}%'
       color <- c("#efe8e6", "#154194")

       p1g <- piebuilder(df_g, titel1, x="geschlecht", y = "proportion", tooltip, color, format)

       out <- highcharter::hw_grid(p1, p1g,
                                   ncol = 2,
                                   browsable = TRUE)

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
     tooltip <- 'Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}'
     color <- c("#efe8e6", "#154194")
     format <- '{point.prop_disp}%'

     p1 <- piebuilder(df_1_pie, titel1, x="geschlecht", y = "proportion", tooltip, color, format)
     p2 <- piebuilder(df_2_pie, titel2, x="geschlecht", y = "proportion", tooltip, color, format)

     out<- highcharter::hw_grid(
       p1, p2,
       ncol = 2,
       browsable = TRUE
     )
     if(gegenwert == "Ja"){
       df1_g <- df[df$fachbereich == "Andere Berufe" & df$indikator == indi[1],]
       df2_g <- df[df$fachbereich == "Andere Berufe" & df$indikator == indi[2],]

       titel1 <- ifelse(regio == "Saarland",
                       paste0("Frauenanteil unter ", title_help1 , " in Nicht MINT-Berufen im ", regio , " (", timerange, ")"),
                       paste0("Frauenanteil unter ", title_help1 , " in Nicht MINT-Berufen in ", regio , " (", timerange, ")"))
       titel2 <- ifelse(regio == "Saarland",
                        paste0("Frauenanteil unter ", title_help2, " in Nicht MINT-Berufen im ", regio , " (", timerange, ")"),
                        paste0("Frauenanteil unter ", title_help2, " in Nicht MINT-Berufen in ", regio , " (", timerange, ")"))
       tooltip <- paste('Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}')
       color <- c("#efe8e6", "#154194")
       format <- '{point.prop_disp}%'

       p1g <- piebuilder(df1_g, titel1, x="geschlecht", y = "proportion", tooltip, color, format)
       p2g <- piebuilder(df2_g, titel2, x="geschlecht", y = "proportion", tooltip, color, format)


       out <- highcharter::hw_grid(p1, p2,
                                   p1g, p2g,
                                   ncol = 2,
                                   browsable = TRUE)

     }

   }
  }
  else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){

    df$indi_fach <- paste0(df$indikator, " - ", df$fachbereich)
    df <- df[with(df, order(proportion, decreasing = TRUE)), ] ######################################################
    if(gegenwert == "Ja"){
      titel <- ifelse(regio == "Saarland",
                      paste0("Frauenanteil in MINT- und anderen Berufen im ", regio, " (", timerange, ")"),
                      paste0("Frauenanteil in MINT- und anderen Berufen in ", regio, " (", timerange, ")"))
    }else{
      titel <- ifelse(regio == "Saarland",
                      paste0("Frauenanteil in MINT-Berufen im ", regio, " (", timerange, ")"),
                      paste0("Frauenanteil in MINT-Berufen in ", regio, " (", timerange, ")"))
    }


    #
   tooltip <- "{point.geschlecht}-Anteil: {point.y} % <br> Anzahl: {disp_wert}"
   format <- "{value}%"
   optional = list(bar = list(stacking = "percent"))

   out <- balkenbuilder2(TF = FALSE, df, titel, x="indi_fach", y="proportion", group="geschlecht", tooltip, format, color = c("#154194", "#efe8e6"))

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

  # df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
  #   dplyr::filter(jahr %in% t,
  #                 landkreis == "alle Landkreise",
  #                 bundesland == regio,
  #                 anforderung == "Gesamt",
  #                 fachbereich == faecher,
  #                 indikator %in% indi,
  #                 geschlecht == "Frauen")%>%
  #   dplyr::select(jahr, indikator, geschlecht, bundesland, wert, fachbereich)%>%
  #   dplyr::collect()
  #
  df_query <- glue::glue_sql("
  SELECT *
  FROM arbeitsmarkt_detail
  WHERE jahr IN ({t*})
  AND landkreis = 'alle Landkreise'
  AND bundesland = {regio}
  AND anforderung = 'Gesamt'
  AND fachbereich = {faecher}
  AND indikator IN ({faecher*})
  AND geschlecht = 'Frauen'
                             ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)

  df <- df %>%
    dplyr::select(jahr, indikator, geschlecht, bundesland, wert, fachbereich)


  if(absolut_selector=="In Prozent"){

    # df_gen_alle <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
    #   dplyr::filter(jahr %in% t,
    #                 landkreis == "alle Landkreise",
    #                 bundesland == regio,
    #                 anforderung == "Gesamt",
    #                 fachbereich == faecher,
    #                 indikator %in% indi,
    #                 geschlecht == "Gesamt")%>%
    #   dplyr::select(jahr, indikator, geschlecht, bundesland, wert, fachbereich)%>%
    #   dplyr::rename(wert_ges = wert) %>%
    #   dplyr::collect()

    df_query <- glue::glue_sql("
    SELECT *
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

    df_gen_alle <- df_gen_alle %>%
      dplyr::select(jahr, indikator, geschlecht, bundesland, wert, fachbereich)%>%
      dplyr::rename(wert_ges = wert)

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

    if(nrow(df) == 0){
      titel_text <- paste0("Für die ausgewählte Kombination aus Gruppe, Berufsfeld und Region liegen keine Daten vor.
                           Gründe dafür sind, das entweder keine oder fast keine Frauen in dieser Gruppe arbeiten.")
    }else{
      titel_text <- ifelse(regio == "Saarland",
                           paste0("Entwicklung des Frauenanteils im Berufsfeld ", faecher, " im ", regio),
                           paste0("Entwicklung des Frauenanteils im Berufsfeld ", faecher, " in ", regio))
    }



    titel <-  titel_text
    tooltip <-  "Anteil Frauen  {point.indikator} <br> Wert: {point.prop_disp} %"
    format <- "{value}%"
    color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
               "#bfc6d3", "#5f94f9")
    out <- linebuilder(df, titel, x = "jahr", y = "prop", group = "indikator", tooltip, format, color)



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


    if(nrow(df) == 0){
      titel_text <- paste0("Für die ausgewählte Kombination aus Gruppe, Berufsfeld und Region liegen keine Daten vor.
                           Gründe dafür sind, das entweder keine oder fast keine Frauen in dieser Gruppe arbeiten.")
    }else{
    titel_text <- ifelse(regio == "Saarland",
                         paste0("Entwicklung der Anzahl an Frauen im Berufsfeld ", faecher, " im ", regio),
                         paste0("Entwicklung der Anzahl an Frauen im Berufsfeld ", faecher, "in ", regio))
    }


    titel <-  titel_text
    tooltip <-  "Anzahl: {point.wert_disp}"
    format <- "{value:, f}"
    color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
               "#bfc6d3", "#5f94f9")
    out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "indikator", tooltip, format, color)

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

    # df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
    #  dplyr::filter(jahr %in% timerange &
    #                landkreis == "alle Landkreise",
    #                bundesland == regio &
    #                geschlecht != "Gesamt"&
    #                anforderung == "Gesamt" &
    #                indikator == indi,
    #                fachbereich %in% c("Alle", "MINT", "Mathematik, Naturwissenschaften",
    #                                     "Informatik", "Technik (gesamt)"))%>%
    #  dplyr::select(jahr, bundesland, indikator, fachbereich, wert, geschlecht) %>%
    #  dplyr::collect()
    #


    df_query <- glue::glue_sql("
    SELECT *
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

    df <- df %>%
      dplyr::select(jahr, bundesland, indikator, fachbereich, wert, geschlecht)

    # Berechnung von andere Fächergruppen
    df[df$fachbereich == "Alle" & df$geschlecht == "Frauen", "wert"] <- df[df$fachbereich == "Alle" & df$geschlecht == "Frauen", "wert"]-
      df[df$fachbereich == "MINT" & df$geschlecht == "Frauen", "wert"]
    df[df$fachbereich == "Alle" & df$geschlecht == "Männer", "wert"] <- df[df$fachbereich == "Alle" & df$geschlecht == "Männer", "wert"]-
      df[df$fachbereich == "MINT" & df$geschlecht == "Männer", "wert"]
    df$fachbereich[df$fachbereich == "Alle"]<-"andere Berufsfelder"
    df <- df %>% dplyr::filter(fachbereich != "MINT")

     # Anteil berechnen
    # df_alle <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
    #   dplyr::filter(jahr %in% timerange &
    #                   landkreis == "alle Landkreise",
    #                 bundesland == regio &
    #                   geschlecht != "Gesamt"&
    #                   anforderung == "Gesamt" &
    #                   indikator == indi,
    #                 fachbereich == "Alle")%>%
    #   dplyr::select(jahr, bundesland, indikator, fachbereich, wert, geschlecht) %>%
    #   dplyr::rename(wert_ges = wert) %>%
    #   dplyr::collect()

    df_query <- glue::glue_sql("
    SELECT *
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

    df_alle <- df_alle %>%
      dplyr::select(jahr, bundesland, indikator, fachbereich, wert, geschlecht) %>%
      dplyr::rename(wert_ges = wert)


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
     title_help <- paste0(indi)
     title_help <- ifelse(grepl("Beschäftigte", indi), "Beschäftigten", title_help)
     title_help <- ifelse(grepl("Auszubildende", indi), "Auszubildenden", title_help)
     title_help <- ifelse(grepl("ausländische Beschäftigte", indi), "ausländischen Beschäftigten", title_help)
     title_help <- ifelse(grepl("ausländische Auszubildende", indi), "ausländischen Auszubildenden", title_help)
     title_help <- ifelse(grepl("Jahr", indi), "Auszubildenden mit neuem Lehrvertrag", title_help)

     df_f <- df_f[with(df_f, order(prop, decreasing = FALSE)), ]
     df_f <- df_f %>%
       dplyr::mutate(color = color_fachbereich[fachbereich])

     df_m <- df_m[with(df_m, order(prop, decreasing = FALSE)), ]
     df_m <- df_m %>%
       dplyr::mutate(color = color_fachbereich[fachbereich])


     titel1 <- paste0("Berufswahl unter Frauen in ", regio, " (", timerange, ")")
     titel2 <- paste0("Berufswahl unter Männern in ", regio, " (", timerange, ")")
     subtitel1 <- paste0("Von allen weiblichen ", title_help, " arbeiten ", round(100-df_f$prop[df_f$fachbereich == "andere Berufsfelder"],1), "% in MINT")
     subtitel2 <-  paste0("Von allen männlichen ", title_help, " arbeiten ", round(100-df_m$prop[df_m$fachbereich == "andere Berufsfelder"],1), "% in MINT")
     color1 <- as.character(df_f$color)
     color2 <- as.character(df_m$color)
     tooltip <- paste('Anteil: {point.prop_disp}% <br> Anzahl: {point.wert_disp}')
     format <- '{point.prop_disp}%'

     out_1 <- piebuilder(df_f, titel1, x="fachbereich", y = "prop", tooltip, color1, format, subtitel = subtitel1)
     out_2 <- piebuilder(df_m, titel2, x="fachbereich", y = "prop", tooltip, color2, format, subtitel = subtitel2)


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

     # df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
     #   dplyr::filter(jahr %in% timerange &
     #                   !bundesland %in% c("Deutschland",
     #                                      "Westdeutschland (o. Berlin)",
     #                                      "Ostdeutschland (inkl. Berlin)") &
     #                   landkreis == "alle Landkreise" &
     #                   geschlecht != "Gesamt"&
     #                   anforderung == "Gesamt",
     #                 indikator == indi,
     #                 fachbereich == faecher)%>%
     #   dplyr::select(indikator, fachbereich, wert, geschlecht, bundesland, jahr) %>%
     #   dplyr::collect()

     df_query <- glue::glue_sql("
     SELECT *
     FROM arbeitsmarkt_detail
     WHERE jahr IN ({timerange*})
     AND NOT bundesland IN ('Deutschland', 'Westdeutschland (o. Berlin)', 'Ostdeutschland (inkl. Berlin)')
     AND landkreis = 'alle Landkreise'
     AND NOT geschlecht = 'Gesamt'
     AND anforderung = 'Gesamt'
     AND indikator = {indi}
     AND fachbereich = {faecher}", .con = con)

     df <- DBI::dbGetQuery(con, df_query)

     df <- df %>%
       dplyr::select(indikator, fachbereich, wert, geschlecht, bundesland, jahr)

#
#      df_alle <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
#        dplyr::filter(jahr %in% timerange &
#                        !bundesland %in% c("Deutschland",
#                                           "Westdeutschland (o. Berlin)",
#                                           "Ostdeutschland (inkl. Berlin)") &
#                        landkreis == "alle Landkreise" &
#                        geschlecht != "Gesamt"&
#                        anforderung == "Gesamt",
#                      indikator == indi,
#                      fachbereich == "Alle")%>%
#        dplyr::select(indikator, fachbereich, wert, geschlecht, bundesland, jahr) %>%
#        dplyr::rename(wert_ges = wert) %>%
#        dplyr::collect()

     df_query <- glue::glue_sql("
     SELECT *
     FROM arbeitsmarkt_detail
     WHERE NOT bundesland IN ('Deutschland', 'Westdeutschland (o. Berlin)', 'Ostdeutschland (inkl. Berlin)')
     AND landkreis = 'alle Landkreise'
     AND NOT geschlecht = 'Gesamt'
     AND anforderung = 'Gesamt'
     AND indikator = {indi}
     AND fachbereich = 'Alle'
                               ", .con = con)

     df_alle <- DBI::dbGetQuery(con, df_query)

     df_alle <- df_alle %>%
       dplyr::select(indikator, fachbereich, wert, geschlecht, bundesland, jahr) %>%
       dplyr::rename(wert_ges = wert)

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
                       paste0("Anteil weiblicher ", title_help, ", die das Berufsfeld <br>", faecher, " wählen (", timerange, ")"))
     titel_m <- ifelse(faecher == "Andere Berufsgruppen", paste0("Anteil männlicher ", title_help, ", die kein <br> MINT-Berufsfeld wählen (", timerange, ")"),
                       paste0("Anteil männlicher ", title_help, ", die das Berufsfeld <br>", faecher, " wählen (", timerange, ")"))

     # plot

     df <- values_female
     joinby <- c("name", "bundesland")
     name <- paste0(faecher)
     tooltip <-"{point.bundesland} <br> Anteil: {point.prop_disp} % <br> Anzahl: {point.wert_disp}"
     titel <- titel_w
     mincolor <- "#fcfcfd"
     maxcolor <- "#b16fab"
     map_selection <- 1
     out_1 <- mapbuilder(df, joinby,name, tooltip, titel, mincolor, maxcolor,prop=TRUE, wert=FALSE, map=map_selection)


     df <- values_male
     joinby <- c("name", "bundesland")
     name <- paste0(faecher)
     tooltip <-"{point.bundesland} <br> Anteil: {point.prop_disp} % <br> Anzahl: {point.wert_disp}"
     titel <- titel_m
     mincolor <- "#fcfcfd"
     maxcolor <- "#b16fab"
     map_selection <- 1
     out_2 <- mapbuilder(df, joinby,name, tooltip, titel, mincolor, maxcolor,prop=TRUE, wert=FALSE, map=map_selection)



     out <- highcharter::hw_grid(
       out_1, out_2,
       ncol = 2,
       browsable = TRUE
     )



     }else
       if(betrachtung == "Zeitverlauf - Liniendiagramm"){
    timerange <- r$date_arbeitsmarkt_wahl_gender_verlauf
    t <- timerange[1]:timerange[2]
    indi <- r$level_arbeitsmarkt_wahl_gender_verlauf
    regio <- r$states_arbeitsmarkt_wahl_gender_verlauf
    faecher <- r$fach_arbeitsmarkt_wahl_gender_verlauf
    absolut_selector <- r$abs_zahlen_arbeitsmarkt_wahl_gender_verlauf

     # df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
     #   dplyr::filter(jahr %in% t,
     #                 indikator == indi,
     #                 landkreis == "alle Landkreise",
     #                 bundesland %in% regio,
     #                 anforderung == "Gesamt",
     #                 geschlecht == "Frauen",
     #                 fachbereich == faecher)%>%
     #   dplyr::select("indikator", "fachbereich", "geschlecht", "bundesland",
     #                 "jahr", "wert" ) %>%
     #   dplyr::collect()

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

       # df_alle <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
       #   dplyr::filter(jahr %in% t,
       #                 indikator == indi,
       #                 landkreis == "alle Landkreise",
       #                 bundesland %in% regio,
       #                 anforderung == "Gesamt",
       #                 geschlecht == "Frauen",
       #                 fachbereich == "Alle")%>%
       #   dplyr::select("indikator", "fachbereich", "geschlecht", "bundesland",
       #                 "jahr", "wert" ) %>%
       #   dplyr::rename(wert_ges = wert) %>%
       #   dplyr::collect()

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

       df$prop_disp <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")
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
       tooltip <-  "{Anteil: {point.prop_disp} %"
       format <- "{value}%"
       color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                  "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
       out <- linebuilder(df, titel, x = "jahr", y = "prop", group = "bundesland", tooltip, format, color)


     }else if(absolut_selector=="Anzahl"){



       title_help <- paste0(indi, "r")
       title_help <- ifelse(grepl("ausländische Beschäftigte", indi), "ausländischer Beschäftigter", title_help)
       title_help <- ifelse(grepl("ausländische Auszubildende", indi), "ausländischer Auszubildender", title_help)
       title_help <- ifelse(grepl("Jahr", indi), "Auszubildender mit neuem Lehrvertrag", title_help)

       titel_w <- ifelse(faecher == "Andere Berufsgruppen", paste0("Anzahl weiblicher ", title_help, ", die kein MINT-Berufsfeld wählen (", timerange, ")"),
                         paste0("Anzahl weiblicher ", title_help, ", die das Berufsfeld ", faecher, " wählen (", timerange, ")"))


       hcoptslang <- getOption("highcharter.lang")
       hcoptslang$thousandsSep <- "."
       options(highcharter.lang = hcoptslang)

       df$wert_disp <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
       df <- df[with(df, order(bundesland, jahr, decreasing = FALSE)), ]

       # plot

      titel <- paste0("Anzahl weiblicher ", title_help, ", die MINT-Berufe wählen")
      tooltip <-"Anzahl: {point.wert_disp}"
      format <- "{value:, f}"
      color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                 "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
      out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "bundesland", tooltip, format, color)

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


  # df <- dplyr::tbl(con, from = "data_naa") %>%
  #   dplyr::filter(
  #     jahr == time &
  #       ebene == "Ebene 3" &
  #       region == bula )%>%
  #   dplyr::select(-code)%>%
  #   dplyr::collect()

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


    if(sum(berufe_maenner$prop)==1000){
      berufe_maenner <- df %>%
        dplyr::filter(geschlecht == "Männer") %>%
        dplyr::filter(prop == 100) %>%
        dplyr::arrange(desc(wert)) %>%
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

    # das bleibt ohne verallgemeinerung da es spezialisierter ist.

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


  }

  out <- list(
    plot_frau,
    plot_mann)

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

arbeitsmarkt_bl_gender_verlauf <- function(r) {

  # load UI inputs from reactive value

  absolut_selector <- r$abs_zahlen_beruf_arbeitsmarkt_bl_gender_verlauf
  timerange <- r$date_beruf_arbeitsmarkt_bl_gender_verlauf
  indikator_choice <- r$indikator_beruf_arbeitsmarkt_bl_gender_verlauf
  states <- r$states_beruf_arbeitsmarkt_bl_gender_verlauf
  t <- as.character(timerange[1]:timerange[2])


  # df <-  dplyr::tbl(con, from = "arbeitsmarkt")%>%
  #   dplyr::filter(jahr %in% t,
  #                 indikator == indikator_choice,
  #                 region %in% states,
  #                 anforderung %in% "Gesamt",
  #                 geschlecht == "Frauen",
  #                 fachbereich %in% c("Alle", "MINT")
  #   )%>%
  #   dplyr::select("bereich",
  #                 "indikator",
  #                 "fachbereich",
  #                 "geschlecht",
  #                 "region",
  #                 "jahr",
  #                 "anforderung",
  #                 "wert" ) %>%
  #   dplyr::collect()


  df_query <- glue::glue_sql("
  SELECT *
  FROM arbeitsmarkt
  WHERE jahr IN ({t*})
  AND region IN ({states*})
  AND anforderung = 'Gesamt'
  AND geschlecht = 'Frauen'
  AND fachbereich IN ('Alle', 'MINT')
                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)

  df <- df %>%
    dplyr::select("bereich",
                  "indikator",
                  "fachbereich",
                  "geschlecht",
                  "region",
                  "jahr",
                  "anforderung",
                  "wert" )

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

    titel <- paste0("Anteil weiblicher ", title_help, ", die MINT-Berufe wählen")
    tooltip <- "{point.region} <br> Anteil: {point.y} %"
    format <- "{value}%"
    color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
               "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
    out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color)


  }else if(absolut_selector=="Anzahl"){

    title_help <- paste0(indikator_choice, "r")

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    df <- df %>%
      dplyr::filter(selector == "Anzahl")

    df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]


    # plot

    titel <- paste0("Anzahl weiblicher ", title_help, ", die MINT-Berufe wählen")
    tooltip <- "Anzahl: {point.y}"
    format <- "{value:, f}"
    color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
               "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
    out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color)

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


  # df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
  #   dplyr::filter(
  #     jahr == timerange)%>%
  #   dplyr::select(-bereich)%>%
  #   dplyr::collect()

  df_query <- glue::glue_sql("
  SELECT *
  FROM arbeitsmarkt_detail
  where jahr = {timerange}
                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)

  df <- df %>%
    dplyr::select(-bereich)


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


  # hilfe für Hover-Text
  if(category_1 == "Beschäftigte") {
    adjektiv_1 <- indikator_besch_1
  }else{
    adjketiv_1 <- indikator_azubi_1
  }


  # adjust landkreis_nummer for correct mapping
  df1_map <- df1_map %>% dplyr::mutate(
    landkreis_nummer = paste0("de-", state_code, "-", landkreis_nummer, "000"))


  #Trennpunkte für lange Zahlen ergänzen in Absolute Zahlen für Hover + Text für Hover
  df1_map$wert <- prettyNum(df1_map$wert, big.mark = ".", decimal.mark = ",")
  domain_1 <- ifelse(domain_1 == "Alle", "alle Berufsbereiche", domain_1)


  # create plots
  highcharter::hcmap(
    paste0("countries/de/de-", state_code ,"-all"),
    data = df1_map,
    value = "prob",
    joinBy = c("hc-key","landkreis_nummer"),
    borderColor = "#FAFAFA",
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
    ) %>% #highcharter::hc_size(600, 550) %>%
    highcharter::hc_credits(enabled = FALSE) %>%
    highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                           verticalAlign = "bottom"
    ) %>%

    highcharter::hc_exporting(enabled = FALSE, #noch kein Download bis jetzt
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/jpeg' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))





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
  # df <- dplyr::tbl(con, from = "arbeitsmarkt_detail") %>%
  #   dplyr::filter(
  #     jahr == timerange)%>%
  #   dplyr::select(-bereich)%>%
  #   dplyr::collect()
  #

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



  # create plot
  titel <- paste0(titel, "<br><br>")
  tooltip <- legende
  format <- yAxis
  color <- "#00A87A"
  optional <- list(bar = list(
    colorByPoint = TRUE,
    colors = ifelse(df_compare$landkreis == "alle Landkreise", "#b16fab",
                    ifelse(df_compare$landkreis == search_val, "#00A87A", "#154194"))
  ))

  my_hc_size <- function(hc) {
    hc %>% highcharter::hc_size(height = 80 * plt.add$höhe[plt.add$länder == states])
  }


  out <- balkenbuilder3(df_compare, titel, x="landkreis", y="display_value", tooltip, format, color, optional = optional, optional2 = my_hc_size)


}

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

    df$prop_disp <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")

    # order years for plot
    df <- df[with(df, order(landkreis, jahr, decreasing = FALSE)), ]

    # plot


    titel <- ifelse(regio == "Saarland",
                    paste0("Anteil der ", gruppe ," in ", fach, " im ", regio),
                    paste0("Anteil der ", gruppe ," in ", fach, " in ", regio))
    tooltip <- "{point.landkreis} <br> Anteil: {point.prop_disp} %"
    format <- "{value}%"
    color <- c("#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
               "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a", "#007655", "#dc6262",
               "#9d7265", "#5d335a", "#bfc6d3",  "#B45309","#d4c1bb", "#112c5f", "#8893a7")
    out <- linebuilder(df, titel, x = "jahr", y = "prop", group = "landkreis", tooltip, format, color)

  } else if(absolut_selector == "Anzahl") {

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    df$landkreis[df$landkreis == "alle Landkreise"] <- "Landesdurchschnitt"

    df$wert_disp <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

    # order years for plot
    df <- df[with(df, order(landkreis, jahr, decreasing = FALSE)), ]

    # plot



    titel <- paste0("Anzahl der ", gruppe ," in ", fach, " in ", regio)
    tooltip <- "{point.landkreis} <br> Anteil: {point.wert_disp}"
    format <- "{value:, f}"
    color <- c("#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
               "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a", "#007655", "#dc6262",
               "#9d7265", "#5d335a", "#bfc6d3",  "#B45309","#d4c1bb", "#112c5f", "#8893a7")
    out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "landkreis", tooltip, format, color)

  }
}



