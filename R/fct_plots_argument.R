
daten_download <- function(r){

  regio <- r$region_argumentationshilfe

  region_reserve <- r$region_argumentationshilfe

  ### Daten Verlauf MINT ----
  t <- 2017:2023
  absolut_selector <- "Anzahl"

  query_df <- glue::glue_sql("
  SELECT bereich, indikator, fachbereich, jahr, wert
  FROM zentral
  WHERE jahr IN ({t*})
    AND region = {regio}
    AND geschlecht = 'Gesamt'
    AND fachbereich = 'MINT'
    AND indikator = 'Beschäftigte'
", .con = con)

  df_beschäftigte <- DBI::dbGetQuery(con, query_df)

  # df_beschäftigte <- df_beschäftigte %>%
  #   mutate(Bereich = "Beschäftigte MINT",
  #          Quelle = "Statistisches Bundesamt, 2024; Bundesagentur für Arbeit, 2024; KMK, 2024, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt")
  #

  ### Daten Fachkräfte ----
  timerange <- 2023
  fach <- c("MINT gesamt", "Nicht MINT")
  bf <- fachkraft_ui_berufslevel()

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

    # plot_data <- plot_data %>%
    #   mutate(Bereich = "Engpassindikator",
    #          Quelle = "Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt")
    #
    #

    }else if(regio != "Deutschland"){############################################################################

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

      # plot_data <- plot_data %>%
      #   mutate(Bereich = "Engpassindikator",
      #          Quelle = "Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt")
      #


    }

    ### Daten Demografie ----
    betrachtung <- "Gruppenvergleich - Balkendiagramm"
    timerange <- 2023 #L
    faecher <- "MINT"


    regio_logisch <- c()
    if (regio == "Brandenburg / Berlin") regio_logisch <- c("Brandenburg", "Berlin")
    else if (regio == "Niedersachsen / Bremen") regio_logisch <- c("Niedersachsen", "Bremen")
    else if (regio == "Rheinland-Pfalz / Saarland") regio_logisch <- c("Rheinland-Pfalz", "Saarland")
    else if (regio == "Schleswig-Holstein / Hamburg") regio_logisch <- c("Schleswig-Holstein", "Hamburg")
    else regio_logisch <- c(regio)


    gruppe <- c(
      "Beschäftigte",
      "Beschäftigte u25",
      "Beschäftigte 25-55",
      "Beschäftigte ü55")

    df_query <- glue::glue_sql("
    SELECT *
    FROM arbeitsmarkt_detail
    WHERE jahr = {timerange}
    AND landkreis = 'alle Landkreise'
    AND bundesland IN ({regio_logisch*})
    AND anforderung = 'Gesamt'
   AND geschlecht = 'Gesamt'
   AND indikator IN ({gruppe*})
   AND fachbereich = {faecher}
", .con = con)


  #   df_query <- glue::glue_sql("
  # SELECT *
  # FROM arbeitsmarkt_detail
  # WHERE jahr = {timerange}
  # AND landkreis = 'alle Landkreise'
  # AND bundesland = {regio}
  # AND anforderung = 'Gesamt'
  # AND geschlecht = 'Gesamt'
  # AND indikator IN ({gruppe*})
  # AND fachbereich = {faecher}
  #                              ", .con = con)

    df1 <- DBI::dbGetQuery(con, df_query)

    df <- df1 %>%
      dplyr::select( "indikator", "bundesland", "landkreis", "fachbereich",
                     "landkreis_zusatz", "landkreis_nummer", "jahr", "anforderung", "geschlecht", "wert")

    # df <- df %>%
    #   mutate(Bereich = "Demografie MINT",
    #          Quelle = "Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt")



    ### Nachwuchs ----
    query_df <- glue::glue_sql("
  SELECT region, fach, jahr, indikator, wert
  FROM studierende_detailliert
  WHERE jahr IN (2017, 2018, 2019, 2020, 2021, 2022, 2023)
    AND region IN ({regio_logisch*})
    AND geschlecht = 'Gesamt'
    AND fach IN ('Mathematik, Naturwissenschaften', 'Informatik', 'Ingenieurwissenschaften (ohne Informatik)')
    AND indikator = 'Studierende'
", .con = con)

    df_studierende <- DBI::dbGetQuery(con, query_df)

    query_df <- glue::glue_sql("
  SELECT bundesland, fachbereich, jahr, kategorie, wert
  FROM arbeitsmarkt_detail
  WHERE jahr IN (2017, 2018, 2019, 2020, 2021, 2022, 2023)
    AND bundesland IN ({regio_logisch*})
    AND geschlecht = 'Gesamt'
    AND fachbereich IN ('Mathematik, Naturwissenschaften', 'Informatik', 'Technik (gesamt)')
    AND kategorie = 'Auszubildende'
", .con = con)

    df_auszubildende <- DBI::dbGetQuery(con, query_df)


    df_azubi_clean <- df_auszubildende %>%
      rename(region = bundesland, fach = fachbereich, indikator = kategorie) %>%
      mutate(
        fach = dplyr::case_when(
          fach == "Technik (gesamt)" ~ "Technik (inkl. Ingenieurwesen)",
          TRUE ~ fach
        ),
        indikator = "Nachwuchs"
      )

    df_studi_clean <- df_studierende %>%
      rename(fach = fach) %>%
      mutate(
        fach = dplyr::case_when(
          fach == "Ingenieurwissenschaften (ohne Informatik)" ~ "Technik (inkl. Ingenieurwesen)",
          TRUE ~ fach
        ),
        indikator = "Nachwuchs"
      )

    df_nachwuchs <- bind_rows(df_azubi_clean, df_studi_clean)


    df_nachwuchs_agg <- df_nachwuchs %>%
      dplyr::group_by(region, fach, jahr, indikator) %>%
      dplyr::summarise(wert = sum(wert), .groups = "drop") %>%
      dplyr::ungroup()

    # df_nachwuchs_agg <- df_nachwuchs_agg %>%
    #   mutate(Bereich = "Nachwuchs MINT",
    #          Quelle = "Destatis, 2024 und Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt")
    #
    #

    # Entwicklung für Hover berechnen
    df_start <- df_nachwuchs_agg %>%
      dplyr::filter(jahr == min(df_nachwuchs_agg$jahr)) %>%
      dplyr::select(fach, wert) %>%
      dplyr::rename(wert_alt =wert)
    df_ende <- df_nachwuchs_agg %>%
      dplyr::filter(jahr == max(df_nachwuchs_agg$jahr)) %>%
      dplyr::select(fach, wert) %>%
      dplyr::rename(wert_neu =wert)
    df_nachwuchs_agg <- df_nachwuchs_agg %>%
      dplyr::left_join(df_start, by = c("fach")) %>%
      dplyr::left_join(df_ende, by = c("fach")) %>%
      dplyr::mutate(diff = round(((wert_neu - wert_alt)/wert_alt)*100,1))

    df_nachwuchs_agg$display_diff <- ifelse(df_nachwuchs_agg$diff < 0,
                                            paste0("-", df_nachwuchs_agg$diff),
                                            paste0("+", df_nachwuchs_agg$diff))

    df_nachwuchs_agg <- df_nachwuchs_agg %>%
      filter(!(is.na(wert_alt) |  is.na(wert_neu) | is.na(diff) | is.na(display_diff)))

    # df_nachwuchs_agg <- df_nachwuchs_agg %>%
    #   na.omit()

    ### Wirkhebel ----
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

    whatever_this_is <- DBI::dbGetQuery(con, df_query)

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

    # uebersicht_data <- uebersicht_data %>%
    #   mutate(Bereich = "Wirkhebel MINT",
    #          Quelle = "Berechnungen durch das IW Köln, 2024, beauftragt durch MINTvernetzt")

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

    ### Zusammenfügen ----
    # Alle Datensätze anpassen: Einheitliche Struktur und Bereichsangabe

    # 1. Beschäftigte MINT
    df_beschäftigte_clean <- df_beschäftigte %>%
      mutate(Bereich = "Beschäftigte MINT",
             Quelle = "Statistisches Bundesamt, 2024; Bundesagentur für Arbeit, 2024; KMK, 2024, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt",
             Region = region_reserve)



    # 2. Engpassindikator
    # plot_data_clean <- plot_data %>%
    #   mutate(Bereich = "Engpassindikator")
    plot_data_clean <- plot_data %>%
      mutate(Bereich = "Engpassindikator",
             Quelle = "Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt",
             Region = region_reserve)


    # 3. Demografie MINT
    # df_demografie_clean <- df %>%
    #   mutate(Bereich = "Demografie MINT")
    df_demografie_clean <- df %>%
      mutate(Bereich = "Demografie MINT",
             Quelle = "Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt",
             Region = region_reserve)


    # 4. Nachwuchs (Studierende + Azubis)
    # df_nachwuchs_clean <- df_nachwuchs_agg %>%
    #   mutate(Bereich = "Nachwuchs MINT")
    df_nachwuchs_clean <- df_nachwuchs_agg %>%
      mutate(Bereich = "Nachwuchs MINT",
             Quelle = "Destatis, 2024 und Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt")


    # 5. Wirkhebel (Prognosen)
    # uebersicht_data_clean <- uebersicht_data %>%
    #   mutate(Bereich = "Wirkhebel MINT")
    uebersicht_data_clean <- uebersicht_data %>%
      mutate(Bereich = "Wirkhebel MINT",
             Quelle = "Berechnungen durch das IW Köln, 2024, beauftragt durch MINTvernetzt",
             Region = "Deutschland (immer deutschland)")

    # Falls nötig: Nur gleiche Spaltennamen verwenden
    # Dazu alle Datensätze auf einen gemeinsamen Satz von Spalten bringen
    # (z.B. alle Spalten als Character, fehlende Spalten füllen)

    # Einfügen einer Funktion, die alle Datensätze vereinheitlicht:
    vereinheitlichen <- function(df) {
      df %>%
        mutate(across(everything(), as.character)) %>%
        select(Bereich, everything())
    }

    # Alle Datensätze vereinheitlichen
    df_list <- list(
      vereinheitlichen(df_beschäftigte_clean),
      vereinheitlichen(plot_data_clean),
      vereinheitlichen(df_demografie_clean),
      vereinheitlichen(df_nachwuchs_clean),
      vereinheitlichen(uebersicht_data_clean)
    )

    # Alle Datensätze zusammenfügen
    final_df <- bind_rows(df_list)

    # Download-Format: TXT
    # Hinweis: Schreibe Tabulator als Trenner ("\t"), weil TXT normalerweise tab-getrennt besser lesbar ist

    # Beispiel: Direkt als String für Download vorbereiten
    txt_output <- final_df %>%
      readr::format_delim(delim = "\t")

    # Oder falls du es direkt speichern willst (z. B. als Temp-Datei in Shiny)
    # readr::write_delim(final_df, path = "dein_pfad/deindateiname.txt", delim = "\t")

    return(txt_output)

}

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
    AND indikator = 'Beschäftigte'
", .con = con)

  df_beschäftigte <- DBI::dbGetQuery(con, query_df)

  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- "."
  options(highcharter.lang = hcoptslang)

  df_beschäftigte <- df_beschäftigte[with(df_beschäftigte, order(fachbereich, jahr, decreasing = FALSE)), ]

  #Trennpunkte für lange Zahlen ergänzen
  df_beschäftigte$wert_besr <- prettyNum(df_beschäftigte$wert, big.mark = ".", decimal.mark = ",")

  # Ordnen der Legende
  sorted_indicators <- df_beschäftigte %>%
    dplyr::group_by(indikator) %>%
    dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
    dplyr::arrange(desc(m_value)) %>%
    dplyr::pull(indikator)

  df_beschäftigte$indikator <- factor(df_beschäftigte$indikator, levels = sorted_indicators)


  query_df <- glue::glue_sql("
  SELECT bereich, indikator, fachbereich, jahr, wert
  FROM zentral
  WHERE jahr IN ({t*})
    AND region = {regio}
    AND geschlecht = 'Gesamt'
    AND fachbereich = 'MINT'
    AND indikator IN ('Studierende', 'Auszubildende')
", .con = con)

  df_andere <- DBI::dbGetQuery(con, query_df)


    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    df_andere <- df_andere[with(df_andere, order(fachbereich, jahr, decreasing = FALSE)), ]

    #Trennpunkte für lange Zahlen ergänzen
    df_andere$wert_besr <- prettyNum(df_andere$wert, big.mark = ".", decimal.mark = ",")

    # Ordnen der Legende
    sorted_indicators <- df_andere %>%
      dplyr::group_by(indikator) %>%
      dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
      dplyr::arrange(desc(m_value)) %>%
      dplyr::pull(indikator)

    df_andere$indikator <- factor(df_andere$indikator, levels = sorted_indicators)

    titel_beschäftigte <- paste0("Entwicklung der Beschäftigtenzahlen in MINT in ", regio)
    titel_andere <- paste0("Entwicklung der Studierenden- und Auszubildendenzahlen in MINT in ", regio)
    tooltip <- paste("{point.indikator} <br> Anzahl: {point.wert_besr}")

    # plot
    format <- "{value:, f}"
    color1 <- c("#b16fab")
    color2 <-  c("#154194","#66cbaf")


    out_beschäftigte <- highcharter::hchart(df_beschäftigte, 'line', highcharter::hcaes(x = "jahr", y = "wert", group = "indikator")) %>%
      highcharter::hc_tooltip(pointFormat = tooltip) %>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, format = "{point.wert_besr}",
                            style = list(textOutline = "none"))
        )) %>%
      highcharter::hc_yAxis(title = list(text = " "), labels = list(format = format),
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_title(text = titel_beschäftigte,
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(color1) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular", fontSize = "14px")
      )  %>%
      highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2024; Bundesagentur für Arbeit, 2024; KMK, 2024, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
                              style = list(fontSize = "11px", color = "gray")) %>%
      highcharter::hc_exporting(enabled = TRUE,
                                buttons = list(
                                  contextButton = list(
                                    menuItems = list("downloadPNG", "downloadCSV")
                                  )
                                )
      )

    out_andere <- highcharter::hchart(df_andere, 'line', highcharter::hcaes(x = "jahr", y = "wert", group = "indikator")) %>%
      highcharter::hc_tooltip(pointFormat = tooltip) %>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, format = "{point.wert_besr}",
                            style = list(textOutline = "none"))
        )) %>%
      highcharter::hc_yAxis(title = list(text = " "), labels = list(format = format),
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_title(text = titel_andere,
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(color2) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular", fontSize = "14px")
      )  %>%
      highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2024; Bundesagentur für Arbeit, 2024; KMK, 2024, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
                              style = list(fontSize = "11px", color = "gray")) %>%
      highcharter::hc_exporting(enabled = TRUE,
                                buttons = list(
                                  contextButton = list(
                                    menuItems = list("downloadPNG", "downloadCSV")
                                  )
                                )
      )

    out <- highcharter::hw_grid(
      out_beschäftigte, out_andere,
      ncol = 2)


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
      highcharter::hc_caption(text = "Quelle der Daten: Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
                              style = list(fontSize = "11px", color = "gray")) %>%
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
      highcharter::hc_caption(text = "Quelle der Daten: Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
                              style = list(fontSize = "11px", color = "gray")) %>%
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

    if(regio %in% c("Hamburg", "Schleswig-Holstein")){
      text = "Es liegen nur Daten zusammengefasst für Hamburg und Schleswig-Holstein vor."
    } else if(regio %in% c("Niedersachsen", "Bremen")){
      text = "Es liegen nur Daten zusammengefasst für Niedersachsen und Bremen vor." }
    else if(regio %in% c("Berlin", "Brandenburg")){
      text = "Es liegen nur Daten zusammengefasst für Berlin und Brandenburg vor."
    } else if(regio %in% c("Rheinland-Pfalz", "Saarland")){
      text = "Es liegen nur Daten zusammengefasst für Rheinland-Pfalz und das Saarland vor."
    } else {
      text = " "
    }

   regio <- dplyr::case_when(
     regio == "Brandenburg" | regio == "Berlin" ~ "Brandenburg / Berlin",
     regio == "Niedersachsen" | regio == "Bremen" ~ "Niedersachsen / Bremen",
     regio == "Rheinland-Pfalz" | regio == "Saarland" ~ "Rheinland-Pfalz / Saarland",
     regio == "Schleswig-Holstein" | regio == "Hamburg" ~ "Schleswig-Holstein / Hamburg",
     T ~ regio
   )

   # df_query <- glue::glue_sql("
   # SELECT *
   # FROM arbeitsmarkt_epa
   # WHERE jahr = {timerange}
   # AND indikator = 'Engpassindikator'
   # AND anforderung IN ({bf*})
   # AND region = {regio}
   #                             ", .con = con)
   #
   #  plot_data_raw <- DBI::dbGetQuery(con, df_query)
   #
   #
   #  # enthält den Text für den plot
   #  epa_kat_levels <- c("Engpassberuf",
   #                      "Anzeichen eines Engpassberufs",
   #                      "Kein Engpassberuf")
   #  group_col_dt <- data.frame(
   #    epa_kat = factor(x = epa_kat_levels,
   #                     levels = epa_kat_levels),
   #    epa_group_order = c(1:3),
   #    group_text = c("Text A",
   #                   "Text B",
   #                   "Text C"),
   #    group_col = c("#EE7775", "#FBBF24", "#35BD97")
   #  )
   #
   #  # Aggregate rausfiltern
   #  plot_data_raw <- subset(plot_data_raw, !(plot_data_raw$berufsgruppe %in%
   #                                             c("Gesamt",
   #                                               "MINT gesamt",
   #                                               "Informatik",
   #                                               "Landtechnik",
   #                                               "Produktionstechnik",
   #                                               "Bau- und Gebäudetechnik",
   #                                               "Mathematik, Naturwissenschaften",
   #                                               "Verkehrs-, Sicherheits- und Veranstaltungstechnik",
   #                                               "Gesundheitstechnik",
   #                                               "Nicht MINT"
   #                                             ))
   #  )
   #
   #  if ("MINT gesamt" %in% fach) {
   #    plot_data_raw <- plot_data_raw %>%
   #      dplyr::filter(!mint_zuordnung %in% c("Nicht MINT", "Gesamt")) %>%
   #      dplyr::mutate(mint_zuordnung = "MINT gesamt") %>%
   #      rbind(plot_data_raw)
   #  }
   #
   #  if ("Technik gesamt" %in% fach) {
   #    plot_data_raw <- plot_data_raw %>%
   #      dplyr::filter(mint_zuordnung %in% c("Landtechnik",
   #                                          "Produktionstechnik",
   #                                          "Bau- und Gebäudetechnik",
   #                                          "Verkehrs-, Sicherheits- und Veranstaltungstechnik",
   #                                          "Gesundheitstechnik")) %>%
   #      dplyr::mutate(mint_zuordnung = "Technik gesamt") %>%
   #      rbind(plot_data_raw)
   #  }

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

    titel_1 <- ifelse(regio == "Saarland",
                      paste0("Engpassrisiko in MINT-Berufen im ", regio," (", timerange, ")"),
                      paste0("Engpassrisiko in MINT-Berufen in ", regio," (", timerange, ")"))

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
      highcharter::hc_subtitle(text = text,
                               margin = 5,
                               align = "center",
                               style = list(color = "grey",
                                            useHTML = TRUE,
                                            fontFamily = "Calibri Regular",
                                            fontSize = "14px")

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

      titel_2 <- ifelse(regio == "Saarland",
            paste0("Engpassrisiko in Nicht-MINT-Berufen im ", regio," (", timerange, ")"),
            paste0("Engpassrisiko in Nicht-MINT-Berufen in ", regio," (", timerange, ")"))

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
        highcharter::hc_subtitle(text = text,
                                 margin = 5,
                                 align = "center",
                                 style = list(color = "grey",
                                              useHTML = TRUE,
                                              fontFamily = "Calibri Regular",
                                              fontSize = "14px")

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

  betrachtung <- "Gruppenvergleich - Balkendiagramm"
  timerange <- 2023 #L
  regio <- r$region_argumentationshilfe
  faecher <- "MINT"

  gruppe <- c(
    "Beschäftigte",
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
  tooltip = "Anzahl: {point.wert_disp}"
  format = "{value:, f}"

  # Farbzuweisung: "Beschäftigte" blau, alle anderen rosa
  df$color <- ifelse(df$indikator == "Beschäftigte", "#154194", "#b16fab")


  titel <- ifelse(regio == "Saarland",
                  paste0("Demografischer Wandel: Beschäftigte in MINT nach Altersgruppen im ", regio, " (", timerange, ")"),
                  paste0("Demografischer Wandel: Beschäftigte in MINT nach Altersgruppen in ", regio, " (", timerange, ")"))


  out <- highcharter::hchart(df, 'bar', highcharter::hcaes(
    y = !!sym("wert"),
    x = !!sym("indikator"),
    color = !!sym("color")
  )) %>%
    highcharter::hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE, format = "{point.wert_disp}",
                          style = list(textOutline = "none"))
      )) %>%
    highcharter::hc_tooltip(pointFormat = tooltip) %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = format)) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    # hc_colors weglassen
    highcharter::hc_title(text = titel,
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "Calibri Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
    highcharter::hc_caption(text = "    Quelle der Daten: Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt. ",
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

argument_nachwuchs <- function(r){

  regio <- r$region_argumentationshilfe

  query_df <- glue::glue_sql("
  SELECT region, fach, jahr, indikator, wert
  FROM studierende_detailliert
  WHERE jahr IN (2017, 2018, 2019, 2020, 2021, 2022, 2023)
    AND region = {regio}
    AND geschlecht = 'Gesamt'
    AND fach IN ('Mathematik, Naturwissenschaften', 'Informatik', 'Ingenieurwissenschaften (ohne Informatik)')
    AND indikator = 'Studierende'
", .con = con)

  df_studierende <- DBI::dbGetQuery(con, query_df)

  query_df <- glue::glue_sql("
  SELECT bundesland, fachbereich, jahr, kategorie, wert
  FROM arbeitsmarkt_detail
  WHERE jahr IN (2017, 2018, 2019, 2020, 2021, 2022, 2023)
    AND bundesland = {regio}
    AND geschlecht = 'Gesamt'
    AND fachbereich IN ('Mathematik, Naturwissenschaften', 'Informatik', 'Technik (gesamt)')
    AND kategorie = 'Auszubildende'
", .con = con)

  df_auszubildende <- DBI::dbGetQuery(con, query_df)

  df_azubi_clean <- df_auszubildende %>%
    rename(region = bundesland, fach = fachbereich, indikator = kategorie) %>%
    mutate(
      fach = dplyr::case_when(
        fach == "Technik (gesamt)" ~ "Technik (inkl. Ingenieurwesen)",
        TRUE ~ fach
      ),
      indikator = "Nachwuchs",
      wert = as.numeric(wert)
    ) %>%
    mutate(across(c(region, fach, indikator), as.character)) %>%
    filter(!is.na(wert))

  df_azubi_clean %>%
    group_by(region, fach, jahr, indikator) %>%
    summarise(wert = sum(wert, na.rm = TRUE), .groups = "drop")



  df_studi_clean <- df_studierende %>%
    rename(fach = fach) %>%
    mutate(
      fach = dplyr::case_when(
        fach == "Ingenieurwissenschaften (ohne Informatik)" ~ "Technik (inkl. Ingenieurwesen)",
        TRUE ~ fach
      ),
      indikator = "Nachwuchs"
    )

  df_studi_clean <- df_studi_clean %>%
    dplyr::group_by(region, fach, jahr, indikator) %>%
    dplyr::summarise(wert = sum(wert), .groups = "drop") %>%
    dplyr::ungroup()


  df_nachwuchs <- bind_rows(df_azubi_clean, df_studi_clean)

  df_nachwuchs_agg <- df_nachwuchs %>%
    dplyr::group_by(region, fach, jahr, indikator) %>%
    dplyr::summarise(wert = sum(wert), .groups = "drop") %>%
    dplyr::ungroup()

  # Entwicklung für Hover berechnen
  df_start <- df_nachwuchs_agg %>%
    dplyr::filter(jahr == min(df_nachwuchs_agg$jahr)) %>%
    dplyr::select(fach, wert) %>%
    dplyr::rename(wert_alt =wert)
  df_ende <- df_nachwuchs_agg %>%
    dplyr::filter(jahr == max(df_nachwuchs_agg$jahr)) %>%
    dplyr::select(fach, wert) %>%
    dplyr::rename(wert_neu =wert)
  df_nachwuchs_agg <- df_nachwuchs_agg %>%
    dplyr::left_join(df_start, by = c("fach")) %>%
    dplyr::left_join(df_ende, by = c("fach")) %>%
    dplyr::mutate(diff = round(((wert_neu - wert_alt)/wert_alt)*100,1))

  df_nachwuchs_agg$display_diff <- ifelse(df_nachwuchs_agg$diff < 0,
                                          paste0("-", df_nachwuchs_agg$diff),
                                          paste0("+", df_nachwuchs_agg$diff))

  # Wert für Anzeige formatieren
  df_nachwuchs_agg$wert_disp <- prettyNum(df_nachwuchs_agg$wert, big.mark = ".", decimal.mark = ",")

  tooltip <- "{point.fach}, Anzahl: {point.wert_disp}, <br>Veränderung seit 2017: {point.display_diff}"
  format <- "{value:,f}"

  #Farben zuweisen
  sorted_indicators <- df_nachwuchs_agg %>%
    dplyr::group_by(fach) %>%
    dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
    dplyr::arrange(desc(m_value)) %>%
    dplyr::pull(fach)

  df_nachwuchs_agg$fach <- factor(df_nachwuchs_agg$fach, levels = sorted_indicators)
  color_fachbereich <- c(
    "Informatik" = "#2D6BE1",
    "Technik (inkl. Ingenieurwesen)" = "#00a87a",
    "Mathematik, Naturwissenschaften" = "#fcc433"
  )
  colors <- as.character(color_fachbereich)
  colors <- color_fachbereich[sorted_indicators]

  df_nachwuchs_agg <- df_nachwuchs_agg[with(df_nachwuchs_agg, order(jahr)),]

  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- "."
  options(highcharter.lang = hcoptslang)


  out <- highcharter::hchart(df_nachwuchs_agg, 'line', highcharter::hcaes(x = jahr, y = wert, group = fach)) %>%
    highcharter::hc_tooltip(pointFormat = tooltip) %>%
    highcharter::hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE, format = "{point.wert_disp}",
                          style = list(textOutline = "none"))
      )) %>%
    highcharter::hc_yAxis(title = list(text = " "), labels = list(format = format),
                          style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_title(text = ifelse(regio == "Saarland",
                                        paste0("Entwicklung der Nachwuchszahlen in den MINT-Disziplinen im ", regio),
                                        paste0("Entwicklung der Nachwuchszahlen in den MINT-Disziplinen in ", regio)),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
    highcharter::hc_colors(as.character(colors)) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "Calibri Regular", fontSize = "14px")
    )  %>%
    highcharter::hc_caption(text = "Quelle der Daten: Quelle der Daten: Destatis, 2024 und Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
                            style = list(fontSize = "11px", color = "gray")) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG", "downloadCSV")
                                )
                              )
    )


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

  whatever_this_is <- DBI::dbGetQuery(con, df_query)

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

  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- "."
  options(highcharter.lang = hcoptslang)

  titel <- "Einfluss der folgenden vier Wirkhebel auf die Entwicklung der MINT-Fachkräfte bis 2037 deutschlandweit <br>"
  tooltip <- paste0("Anzahl an MINT-Fachkräften, die bis 2037
                    gewonnen werden können: {point.diff_disp}")
  format <- "{value:, f}"

  final_data <- uebersicht_data %>%
    dplyr::select(wirkhebel, diff)

  final_data$color <- ifelse(final_data$wirkhebel == "Gesamteffekt", "#154194", "#b16fab")

  final_data <- final_data[with(final_data, order(diff, decreasing = TRUE)),]
  final_data$diff_disp <- prettyNum(final_data$diff, decimal.mark = ",", big.mark = ".")

  out <- highcharter::hchart(final_data, 'bar', highcharter::hcaes(
    y = !!sym("diff"),
    x = !!sym("wirkhebel"),
    color = !!sym("color")
  )) %>%
    highcharter::hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE, format = "{point.diff_disp}",
                          style = list(textOutline = "none"))
      )) %>%
    highcharter::hc_tooltip(pointFormat = tooltip) %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = format)) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    #highcharter::hc_colors(final_data$color) %>%
    highcharter::hc_title(text = titel,
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "Calibri Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
    highcharter::hc_caption(text = "Berechnungen durch das IW Köln, 2024, beauftragt durch MINTvernetzt.",
                            style = list(fontSize = "11px", color = "gray")) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG", "downloadCSV")
                                )
                              )
    )


  # fig <- plotly::plot_ly(uebersicht_data, color = I("gray80")) %>%
  #   plotly::add_segments(
  #     x = ~basis_wert,
  #     xend = ~wert,
  #     y = ~wirkhebel,
  #     yend = ~wirkhebel,
  #     showlegend = FALSE,
  #     text = ~paste0("MINT-Fachkräfteanzahl im Basis: ", basis_wert, "<br>Wert: ", wert_txt, "<br>: ", diff_txt),
  #     hoverinfo = "text"
  #   ) %>%
  #   plotly::add_markers(
  #     x = ~basis_wert,
  #     y = ~wirkhebel,
  #     name = "Basis-Szenario 2022",
  #     color = I("#D0A9CD"),
  #     symbol = I("square"),
  #     size = I(50),
  #     text = ~paste0("Basis-Szenario 2022: ", basis_wert_txt),
  #     hoverinfo = "text"
  #   ) %>%
  #   plotly::add_markers(
  #     x = ~wert,
  #     y = ~wirkhebel,
  #     name = paste0("Positives Szenario ",year_filter),
  #     color = I("#b16fab"),
  #     symbol = I("square"),
  #     size = I(50),
  #     text = ~paste0("Positives Szenario für Wirkhebel ", wirkhebel, ": ", wert_txt, "<br>Zunahme der MINT-Fachkräfte seit 2022: ", diff_txt),
  #     hoverinfo = "text"
  #   ) %>%
  #   plotly::layout(
  #     title = list(
  #       text = paste0(
  #         "Wie wirken sich die unten gelisteten Wirkhebel auf die Anzahl der MINT-Fachkräfte aus?"
  #       )
  #     ),
  #     xaxis = list(
  #       title = "Anzahl MINT-Fachkräfte",
  #       tickformat = ",",
  #       range = c(7500000, 9500000)
  #     ),
  #     yaxis = list(
  #       title = "",
  #       categoryorder = "array",
  #       categoryarray = unique(uebersicht_data$wirkhebel)
  #     ),
  #     margin = list(l = 100, r = 50, t = 80, b = 50),
  #     hoverlabel = list(bgcolor = "white"),
  #     legend = list(
  #       orientation = "h",
  #       x = 0.5,
  #       y = -0.5,
  #       xanchor = "center",
  #       yanchor = "top"
  #     )
  #   )
  #
  # hc <- fig

  return(out)

  ###



}

# argument_frauen <- function(r){
#
#
#   # load UI inputs from reactive value
#   timerange <- c(2013, 2025)
#   t<- timerange[1]:timerange[2]
#   regio <- r$region_argumentationshilfe
#   indikator_choice <- c("Leistungskurse", "Studierende", "Auszubildende", "Beschäftigte")
#   abs_selector <- "Anzahl"
#
#
#
#
#   query <- glue::glue_sql("
#   SELECT bereich, indikator, fachbereich, geschlecht, jahr, wert
#   FROM zentral
#   WHERE jahr IN ({t*})
#     AND region IN ({regio*})
#     AND geschlecht = 'Frauen'
#     AND fachbereich = 'MINT'
#     AND indikator IN ({indikator_choice*})
# ", .con = con)
#
#   df <- DBI::dbGetQuery(con, query)
#
#
#
#
#   hcoptslang <- getOption("highcharter.lang")
#   hcoptslang$thousandsSep <- "."
#   options(highcharter.lang = hcoptslang)
#
#   df <- df[with(df, order(indikator, jahr, decreasing = FALSE)), ]
#
#   # Ordnen der Legende
#   sorted_indicators <- df %>%
#     dplyr::group_by(indikator) %>%
#     dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
#     dplyr::arrange(desc(m_value)) %>%
#     dplyr::pull(indikator)
#
#   df$indikator <- factor(df$indikator, levels = sorted_indicators)
#
#   titel <- "Anzahl von Frauen in MINT nach Bildungsbereichen"
#   tooltip <- "Anzahl Frauen <br> Indikator: {point.indikator} <br> Anzahl: {point.y} "
#   format <- "{value}"
#
#   color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24" )
#
#   out <- linebuilder(df,titel,x="jahr", y="wert", group="indikator", tooltip, format, color)
#
#
#
#
# }
