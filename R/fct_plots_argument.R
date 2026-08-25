
# Funktion Datendownload --------------------------------------------------

daten_download <- function(r){

  fokus <- r$frauen_fokus
  regio <- r$region_argumentationshilfe
  region_reserve <- regio

## MINT allgemein ----

  if(fokus == FALSE){

  ### Daten Verlauf MINT ----
  t <- 2017:2024
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

  df_alle <- rbind(df_beschäftigte, df_andere)

  ### Daten Fachkräfte ----
  timerange <- 2025
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

    save_regio <- regio

    }else if(regio != "Deutschland"){

      regio <- dplyr::case_when(
        regio == "Brandenburg" | regio == "Berlin" ~ "Brandenburg / Berlin",
        regio == "Niedersachsen" | regio == "Bremen" ~ "Niedersachsen / Bremen",
        regio == "Rheinland-Pfalz" | regio == "Saarland" ~ "Rheinland-Pfalz / Saarland",
        regio == "Schleswig-Holstein" | regio == "Hamburg" ~ "Schleswig-Holstein / Hamburg",
        T ~ regio
      )

      save_regio <- regio

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

    }


    ### Daten Demografie ----
    betrachtung <- "Gruppenvergleich - Balkendiagramm"
    timerange <- 2025 #L
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


    ### Nachwuchs ----
    query_df <- glue::glue_sql("
      SELECT region, fach, jahr, indikator, wert
      FROM studierende_detailliert
      WHERE jahr IN (2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024)
      AND region = {regio}
      AND geschlecht = 'Gesamt'
      AND fach IN ('Mathematik, Naturwissenschaften', 'Informatik', 'Ingenieurwissenschaften (ohne Informatik)')
      AND indikator = 'Studierende'
      ", .con = con)

    df_studierende <- DBI::dbGetQuery(con, query_df)

    query_df <- glue::glue_sql("
      SELECT bundesland, fachbereich, jahr, indikator, wert
      FROM arbeitsmarkt_detail
      WHERE jahr IN (2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024)
      AND bundesland = {regio}
      AND geschlecht = 'Gesamt'
      AND fachbereich IN ('Mathematik, Naturwissenschaften', 'Informatik', 'Technik (gesamt)')
      AND indikator = 'Auszubildende'
      AND landkreis = 'alle Landkreise'
      ", .con = con)

    df_auszubildende <- DBI::dbGetQuery(con, query_df)


    df_azubi_clean <- df_auszubildende %>%
      dplyr::rename(region = bundesland, fach = fachbereich) %>%
      dplyr::mutate(
        fach = dplyr::case_when(
          fach == "Technik (gesamt)" ~ "Technik (inkl. Ingenieurwesen)",
          TRUE ~ fach
        ),
        indikator = "Nachwuchs",
        jahr = as.numeric(jahr)
      )

    df_studi_clean <- df_studierende %>%
      dplyr::rename(fach = fach) %>%
      dplyr::mutate(
        fach = dplyr::case_when(
          fach == "Ingenieurwissenschaften (ohne Informatik)" ~ "Technik (inkl. Ingenieurwesen)",
          TRUE ~ fach
        ),
        indikator = "Nachwuchs"
      )

    df_nachwuchs <-  dplyr::bind_rows(df_azubi_clean, df_studi_clean)

    df_nachwuchs_agg <- df_nachwuchs %>%
      dplyr::group_by(region, fach, jahr, indikator) %>%
      dplyr::summarise(wert = sum(wert), .groups = "drop") %>%
      dplyr::ungroup()

    # Entwicklung für Hover berechnen
    df_start <- df_nachwuchs_agg %>%
      dplyr::filter(jahr == min(df_nachwuchs_agg$jahr)) %>%
      dplyr::select(fach, wert, region) %>%
      dplyr::rename(wert_alt =wert)
    df_ende <- df_nachwuchs_agg %>%
      dplyr::filter(jahr == max(df_nachwuchs_agg$jahr)) %>%
      dplyr::select(fach, wert, region) %>%
      dplyr::rename(wert_neu =wert)
    df_nachwuchs_agg <- df_nachwuchs_agg %>%
      dplyr::left_join(df_start, by = c("fach", "region")) %>%
      dplyr::left_join(df_ende, by = c("fach", "region")) %>%
      dplyr::mutate(diff = round(((wert_neu - wert_alt)/wert_alt)*100,1))

    df_nachwuchs_agg$display_diff <- ifelse(df_nachwuchs_agg$diff < 0,
                                            paste0("-", df_nachwuchs_agg$diff),
                                            paste0("+", df_nachwuchs_agg$diff))

    df_nachwuchs_agg <- df_nachwuchs_agg %>%
      dplyr::filter(!(is.na(df_nachwuchs_agg$wert_alt) |
                        is.na(df_nachwuchs_agg$wert_neu) |
                        is.na(df_nachwuchs_agg$diff) |
                        is.na(df_nachwuchs_agg$display_diff)))


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
    df_beschäftigte_clean <- df_alle %>%
      dplyr::mutate(Bereich = "Beschäftigte MINT",
             Quelle = "Statistisches Bundesamt, 2025; Bundesagentur für Arbeit, 2025, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt",
             Region = region_reserve)

    # 2. Engpassindikator
    plot_data_clean <- plot_data %>%
      dplyr::mutate(Bereich = "Engpassindikator",
             Quelle = "Berichtsjahr 2025, Bundesagentur für Arbeit, 2026, auf Anfrage, eigene Berechnungen durch MINTvernetzt",
             Region = save_regio)

    # 3. Demografie MINT
    df_demografie_clean <- df %>%
      dplyr::mutate(Bereich = "Demografie MINT",
             Quelle = "Bundesagentur für Arbeit, 2026, auf Anfrage, eigene Berechnungen durch MINTvernetzt",
             Region = region_reserve)


    # 4. Nachwuchs (Studierende + Azubis)
    df_nachwuchs_clean <- df_nachwuchs_agg %>%
      dplyr::mutate(Bereich = "Nachwuchs MINT",
             Quelle = "Destatis, 2025 und Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt")


    # 5. Wirkhebel (Prognosen)
    uebersicht_data_clean <- uebersicht_data %>%
      dplyr::mutate(Bereich = "Wirkhebel MINT",
             Quelle = "Berechnungen durch das IW Köln, 2024, beauftragt durch MINTvernetzt",
             Region = "Deutschland (immer Deutschland)")

    # Einfügen einer Funktion, die alle Datensätze vereinheitlicht:
    vereinheitlichen <- function(df) {
      df %>%
        dplyr::mutate( dplyr::across(everything(), as.character)) %>%
        dplyr::select(Bereich, everything())
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
    final_df <-  dplyr::bind_rows(df_list)

    # Download-Format: TXT
    # Hinweis: Schreibe Tabulator als Trenner ("\t"), weil TXT normalerweise tab-getrennt besser lesbar ist

    # Beispiel: Direkt als String für Download vorbereiten
    txt_output <- final_df %>%
      readr::format_delim(delim = "\t")

  }else{

    ### Frauen Vergleich Bildungskette ----

    zeit <- 2024
    regio <- r$region_argumentationshilfe
    indikator_choice <- c("Leistungskurse", "Studierende",
                          "Auszubildende", "Beschäftigte")

    # filter dataset based on UI input
    query_df <- glue::glue_sql("
  SELECT bereich, indikator, fachbereich, geschlecht, wert
  FROM zentral
  WHERE jahr = {zeit}
    AND region = {regio}
    AND geschlecht IN ('Frauen', 'Männer')
    AND fachbereich = 'MINT'
", .con = con)

    df <- DBI::dbGetQuery(con, query_df)

    query_df_alle <- glue::glue_sql("
  SELECT bereich, indikator, fachbereich, geschlecht, wert
  FROM zentral
  WHERE jahr = {zeit}
    AND region = {regio}
    AND geschlecht = 'Gesamt'
    AND fachbereich = 'MINT'
", .con = con)

    df_alle <- DBI::dbGetQuery(con, query_df_alle)

    if (regio == "Deutschland"){

      #Baden-Würrtemberg rausrechnen, da dort keine Geschlechter erfasst werden
      query_df_alle_bw <- glue::glue_sql("
  SELECT bereich, indikator, fachbereich, geschlecht, wert
  FROM zentral
  WHERE jahr = {zeit}
    AND region = 'Baden-Württemberg'
    AND geschlecht = 'Gesamt'
    AND fachbereich = 'MINT'
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

    df_fr_vgl <- df %>%
      dplyr::left_join(df_alle, by = c("bereich", "indikator", "fachbereich")) %>%
      dplyr::rename(wert = wert.x,
                    wert_ges = wert.y,
                    geschlecht = geschlecht.x) %>%
      dplyr::mutate(prop = round(wert / wert_ges * 100, 1)) %>%
      dplyr::select(-geschlecht.y, -wert_ges)


    ### Frauen in MINT-Berufen ----
    timerange <- 2025
    indi <- "Beschäftigte"

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


    df_fr_beruf <- df %>% dplyr::left_join(df_alle, by = c("jahr", "bundesland", "indikator",
                                                  "geschlecht")) %>%
      dplyr::rename(fachbereich = fachbereich.x) %>%
      dplyr::select(-fachbereich.y) %>%
      dplyr::mutate(prop = round(wert/wert_ges *100, 1))


    ### Mädchen Selbstkonzept ----

    jahr_select <- 2024
    region_select <- regio
    gruppe_select <- c("Mädchen", "Jungen")

    df_query <- glue::glue_sql("
    SELECT fach, indikator, geschlecht, typ, jahr, wert
    FROM iqb
    WHERE typ IN ('Mittelwert', 'Standardabweichung')
    AND indikator = 'Selbstkonzept'
    AND jahr = {jahr_select}
    AND region = {region_select}
    AND geschlecht IN ({gruppe_select[1]}, {gruppe_select[2]})
                               ", .con = con)
    df <- DBI::dbGetQuery(con, df_query)

    # als Faktor speichern für Reihenfolge und Selbstkonzept umbennenen
    df <- df %>%
      dplyr::mutate(
        indikator = dplyr::case_when(
          indikator == "Selbstkonzept" ~ "Selbsteinschätzung der eigenen Fähigkeiten"
        ))

    df_sd <- df %>%
      dplyr::filter(typ == "Standardabweichung") %>%
      dplyr::rename("sd" = "wert") %>%
      dplyr::select(-typ)

    df_selbst <- df %>%
      dplyr::filter(typ == "Mittelwert") %>%
      dplyr::select(-typ) %>%
      dplyr::left_join(df_sd, by = c("geschlecht", "fach", "indikator", "jahr"))

    ### Faecherverteilung in MINT ----

    timerange <- 2024

    # filter dataset based on UI inputs

    df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr == {timerange}
        AND typ = 'Einzelauswahl'
        AND geschlecht = 'Frauen'
        AND indikator = 'Studierende'
        AND region = {regio}
        AND mint_select = 'MINT'
                               ", .con = con)
    df <- DBI::dbGetQuery(con, df_query)

    df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr == {timerange}
        AND geschlecht = 'Gesamt'
        AND indikator = 'Studierende'
        AND typ = 'Einzelauswahl'
        AND region = {regio}
        AND mint_select = 'MINT'
                               ", .con = con)
    alle <- DBI::dbGetQuery(con, df_query)

    df_faecher <- df %>%
      dplyr::left_join(alle,
                       by = c("region", "jahr", "bereich", "indikator", "mint_select", "typ", "fachbereich", "fach")) %>%
      dplyr::rename(
        wert = wert.x,
        wert_ges = wert.y
      ) %>%
      dplyr::mutate(prop = round(wert / wert_ges * 100, 1))

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

    # 1. Frauen entlang der Bildungskette - Verlauf
    df_frauenvergleich_clean <- df_fr_vgl %>%
      dplyr::mutate(Bereich = "Verlauf von Frauenanteilen in MINT entlang der Bildungskette",
                    Quelle = "KMK, 2025; Statistisches Bundesamt, 2025; Bundesagentur für Arbeit, 2025, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt",
                    Region = regio)

    # 2. Frauen im Beruf
    df_beruf_clean <- df_fr_beruf %>%
      dplyr::mutate(Bereich = "Anteil aller berufstätigen Frauen, die MINT-Beruf ergreifen",
                    Quelle = "Bundesagentur für Arbeit, 2026, auf Anfrage, eigene Berechnungen durch MINTvernetzt",
                    Region = regio)

    # 3. Selbstkonzept in MINT-Fächern
    df_selbstkonzept_clean <- df_selbst %>%
      dplyr::mutate(Bereich = "Selbsteinschätzung fachlicher Kompetenzen von 9.-Klässler:innen in MINT-Fächern",
                    Quelle = "Institut für Qualitätssicherung, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt",
                    Region = regio)


    # 4. Fächerunterschiede in MINT nach Geschlecht
    df_faecher_clean <- df_faecher %>%
      dplyr::mutate(Bereich = "Vergleich der Frauenanteile in verschiedenen MINT-Fächergruppen im Studium",
                    Quelle = "Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt",
                    Region = regio)


    # 5. Wirkhebel (Prognosen)
    df_wirkhebel_clean <- uebersicht_data %>%
      dplyr::mutate(Bereich = "Wirkhebel von Zukunftsszenarien für die MINT-Fachkräfteenwicklung",
                    Quelle = "Berechnungen durch das IW Köln, 2024, beauftragt durch MINTvernetzt",
                    Region = "Deutschland (liegt nicht auf regionaler Ebene vor)")

    # Einfügen einer Funktion, die alle Datensätze vereinheitlicht:
    vereinheitlichen <- function(df) {
      df %>%
        dplyr::mutate( dplyr::across(everything(), as.character)) %>%
        dplyr::select(Bereich, everything())
    }

    # Alle Datensätze vereinheitlichen
    df_list <- list(
      vereinheitlichen(df_frauenvergleich_clean),
      vereinheitlichen(df_beruf_clean),
      vereinheitlichen(df_selbstkonzept_clean),
      vereinheitlichen(df_faecher_clean),
      vereinheitlichen(df_wirkhebel_clean)
    )

    # Alle Datensätze zusammenfügen
    final_df <-  dplyr::bind_rows(df_list)

    # Download-Format: TXT
    # Hinweis: Schreibe Tabulator als Trenner ("\t"), weil TXT normalerweise tab-getrennt besser lesbar ist

    # Beispiel: Direkt als String für Download vorbereiten
    txt_output <- final_df %>%
      readr::format_delim(delim = "\t")
}

    return(final_df)

}


# Funktionen für Grafiken -------------------------------------------------


argument_verlauf_1 <- function(r){

  # load UI inputs from reactive value
  t <- 2017:2025
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

  df_beschäftigte <- df_beschäftigte[with(df_beschäftigte, order(fachbereich, jahr, decreasing = FALSE)), ]

  #Trennpunkte für lange Zahlen ergänzen
  df_beschäftigte$wert_besr <- prettyNum(df_beschäftigte$wert, big.mark = ".", decimal.mark = ",")


    titel_beschäftigte <- ifelse(regio == "Saarland",
                                 paste0("Entwicklung der Beschäftigtenzahlen ",
                                        "in MINT im ", regio),
                                 paste0("Entwicklung der Beschäftigtenzahlen ",
                                        "in MINT in ", regio))

    df_beschäftigte <- df_beschäftigte %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", indikator, "</b><br>",
          "Jahr: ", jahr, "<br>",
          "Anzahl: ", wert_besr
        )
      )

    df_beschäftigte$label <- df_beschäftigte$wert_besr

    # plot
    format <- ",d"
    color1 <- c("#b16fab")

    titel <- titel_beschäftigte
    quelle <- "Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    out <- linebuilder_plotly(df_beschäftigte, titel = titel, x = "jahr", y = "wert", group = "indikator",
                       format = format, color = color1, quelle = quelle, quelle_y = -0.17,
                       label = TRUE) |>
      plotly::layout(
        margin = list(t = 40, b = 100, r = 50)
      )

  return(out)

}

argument_verlauf_2 <- function(r){

  # load UI inputs from reactive value
  t <- 2017:2025
  regio <- r$region_argumentationshilfe

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

  df_andere <- df_andere[with(df_andere, order(fachbereich, jahr, decreasing = FALSE)), ]

  #Trennpunkte für lange Zahlen ergänzen
  df_andere$wert_besr <- prettyNum(df_andere$wert, big.mark = ".", decimal.mark = ",")

  # Ordnen der Legende
  sorted_indicators <- df_andere %>%
    dplyr::group_by(indikator) %>%
    dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
    dplyr::arrange(m_value) %>%
    dplyr::pull(indikator)

  df_andere$indikator <- factor(df_andere$indikator, levels = sorted_indicators)

  titel_andere <- ifelse(regio == "Saarland",
                         paste0("Entwicklung der Nachwuchszahlen in MINT im ", regio),
                         paste0("Entwicklung der Nachwuchszahlen in MINT in ", regio))

  df_andere <- df_andere %>%
    dplyr::mutate(
      tooltip = paste0(
        "<b>", indikator, "</b><br>",
        "Jahr: ", jahr, "<br>",
        "Anzahl: ", wert_besr
      )
    )

  df_andere$label <- df_andere$wert_besr

  # plot
  format <- ",d"
  color2 <-  c("#154194","#66cbaf")

  titel <- titel_andere
  quelle <- "Destatis, 2025 und Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt"

  out <- linebuilder_plotly(df_andere, titel = titel, x = "jahr", y = "wert", group = "indikator",
                     format = format, color = color2, quelle = quelle, label =TRUE)


  return(out)

}

argument_fachkraft <- function(r){

  timerange <- 2025
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





    titel_1 <- stringr::str_wrap(
      paste0("Engpassrisiko in MINT-Berufen in ", regio," (", timerange, ")"),
      width = 40
    )

    df_download <- expanded_dt %>%
      dplyr::filter(mint_zuordnung == fach[1])

    # Entfernen aller Zeilen, bei denen group_col NA ist
    df_download <- df_download %>%
      dplyr::filter(!is.na(group_col))

    #df_json <- subset(plot_data, select = c(x, y, group))
    df_json <- jsonlite::toJSON(plot_data,dataframe = "rows",auto_unbox = TRUE, na = "null")
    titel_js <- jsonlite::toJSON(titel_1, auto_unbox = TRUE)
    quelle_js <- jsonlite::toJSON("Quelle der Daten: Bundesagentur für Arbeit, 2026, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",auto_unbox = TRUE)
    x_js      <- "XX"
    y_js      <- "YY"
    group_js  <- "epa_kat"



    plot_left <- plotly::plot_ly(
      data = df_download,
      x = ~XX,
      y = ~YY,
      type = "scatter",
      mode = "markers",
      color = ~epa_kat,
      colors = group_col_dt$group_col,
      text = ~paste0(
        "<b>", epa_kat, "</b><br>",
        "Anteil: ", value, " %<br>",
        "Anzahl betroffener Berufe: ", beruf_num
      ),
      hoverinfo = "text",
      marker = list(
        symbol = "square",
        size = 30,
        line = list(
          width = 0))
    ) %>%
      plotly::style(
        hoverlabel = list(bgcolor = "white",
                          font = list(size = 12))
      ) %>%
      plotly::layout(
        title = list(
          text = titel_1,
          x = 0.5, y=0.95,
          xanchor = "center",
          font = list(
            family = "Calibri, sans-serif",
            size = 20,
            color = "black"
          )
        ),
        xaxis = list(
          visible = FALSE,showgrid = FALSE,
          zeroline = FALSE,fixedrange = TRUE
        ),
        yaxis = list(
          visible = FALSE,showgrid = FALSE,
          zeroline = FALSE,fixedrange = TRUE,
          scaleanchor = "x"
        ),
        legend = list(
          orientation = "v",
          x = 0.2,y = -0.15,
          font = list(
            family = "Calibri, sans-serif",size = 12)
        ),
        margin = list(
          t = 80,b = 120,
          l = 20,r = 20
        ),
        annotations = list(
          list(
            text = "Quelle der Daten: Bundesagentur für Arbeit, 2026, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
            x = 0,
            y = -0.22,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            xanchor = "left",
            font = list(
              family = "Calibri, sans-serif",
              size = 11,
              color = "gray"
            )
          )
        )
      ) %>%
      plotly::config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "sendDataToCloud", "autoScale2d", "resetScale2d", "toggleSpikelines",
          "hoverClosestCartesian", "hoverCompareCartesian",
          "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d"
        ),
        modeBarButtonsToAdd = list(

          # CSV-Download
          list(
            name = "Download CSV",
            icon = list(
              path = "M16,2H8C6.9,2,6,2.9,6,4v16c0,1.1,0.9,2,2,2h8c1.1,0,2-0.9,2-2V4C18,2.9,17.1,2,16,2z M16,20H8V4h8V20z M14.5,14h-2v3h-1v-3h-2l2.5-3.5L14.5,14z",
              width = 24,
              height = 24
            ),

            click = htmlwidgets::JS(
              paste0("
              function(gd) {
                var rows = ", df_json, ";

                var date = new Date().toISOString().slice(0,10);
                var filename = 'export_' + date + '.csv';

                if (!rows.length) return;

                var cols = Object.keys(rows[0]);
                var csv = cols.join(';') + '\\n';

                rows.forEach(function(row) {
                  var values = cols.map(function(col) {
                    var value = row[col];
                    if (value == null) return '';
                    value = String(value).replace(/\"/g, '\"\"');
                    if (value.search(/[\";\\n]/) >= 0) {
                      value = '\"' + value + '\"';
                    }
                    return value;
                  });
                  csv += values.join(';') + '\\n';
                });

                var blob = new Blob([csv], { type: 'text/csv;charset=utf-8;' });

                var link = document.createElement('a');
                link.href = URL.createObjectURL(blob);
                link.download = filename;
                link.click();
              }
            ")
            )
          ),

          # TXT-Download für KI
          list(
            name = "Download Daten für KI-Chats als txt",
            icon = list(
              path = "M14,2H6C4.9,2,4,2.9,4,4v16c0,1.1,0.9,2,2,2h12c1.1,0,2-0.9,2-2V8L14,2z M14,4.5L17.5,8H14V4.5z M18,20H6V4h6v6h6V20z",
              width = 24,
              height = 24
            ),
            click = htmlwidgets::JS(
              paste0("
              function(gd) {
                var rows = ", df_json, ";
                var titel = ", titel_js, ";
                var quelle = ", quelle_js, ";

                var date = new Date().toISOString().slice(0,10);
                var chartTitle = titel.replace(/\\s+/g, '_');
                var filename = chartTitle + '_' + date + '.txt';

                if (!rows.length) return;

                var cols = Object.keys(rows[0]);

                var text = '';
                text += 'Titel: ' + titel + '\\n';
                text += 'Quelle: ' + quelle + '\\n\\n';
                text += 'Daten:\\n';

                text += cols.join('\\t') + '\\n';

                rows.forEach(function(row) {
                  var values = cols.map(function(col) {
                    var value = row[col];
                    if (value === null || value === undefined) return '';
                    return String(value);
                  });
                  text += values.join('\\t') + '\\n';
                });

                var blob = new Blob([text], { type: 'text/plain;charset=utf-8;' });

                var link = document.createElement('a');
                link.href = URL.createObjectURL(blob);
                link.download = filename;
                link.click();
              }
            ")
            )
          )
        )
      )




    fach_2 <- dplyr::case_when(
      fach[2] == "MINT gesamt" ~ "MINT",
      fach[2] == "Gesamt" ~ "allen Berufen",
      fach[2] == "Nicht MINT" ~ "allen Berufen außer MINT",
      T ~ fach[2]
    )

    titel_2 <- stringr::str_wrap(
      paste0("Engpassrisiko in Nicht-MINT-Berufen in ", regio," (", timerange, ")"),
      width = 40
    )
    used_colors <- group_col_dt %>%
      dplyr::filter(epa_kat %in% (expanded_dt %>%
                                    dplyr::filter(mint_zuordnung == fach[2]) %>%
                                    dplyr::pull(epa_kat) %>%
                                    unique())) %>%
      dplyr::pull(group_col)


    df_download2 <- expanded_dt %>%
      dplyr::filter(mint_zuordnung == fach[2]) %>%
      dplyr::filter(!is.na(group_col))

    df_json <- jsonlite::toJSON(plot_data,dataframe = "rows",auto_unbox = TRUE, na = "null")

    #df_json <- jsonlite::toJSON(df_download2,dataframe = "rows",auto_unbox = TRUE, na = "null")
    titel_js <- jsonlite::toJSON(titel_2, auto_unbox = TRUE)
    quelle_js <- jsonlite::toJSON("Quelle der Daten: Bundesagentur für Arbeit, 2026, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",auto_unbox = TRUE)
    x_js      <- "XX"
    y_js      <- "YY"
    group_js  <- "epa_kat"



    plot_right <- plotly::plot_ly(
      data = df_download2,
      x = ~XX,
      y = ~YY,
      type = "scatter",
      mode = "markers",
      color = ~epa_kat,
      colors = group_col_dt$group_col,
      text = ~paste0(
        "<b>", epa_kat, "</b><br>",
        "Anteil: ", value, " %<br>",
        "Anzahl betroffener Berufe: ", beruf_num
      ),
      hoverinfo = "text",
      marker = list(
        symbol = "square",
        size = 30,
        line = list(
          width = 0))
    ) %>%
      plotly::style(
      hoverlabel = list(bgcolor = "white",
                        font = list(size = 12))
    ) %>%
      plotly::layout(
        title = list(
          text = titel_2,
          x = 0.5,y=0.95, xanchor = "center",
          font = list(
            family = "Calibri, sans-serif",size = 20,color = "black")
        ),
        xaxis = list(
          visible = FALSE,showgrid = FALSE,
          zeroline = FALSE,fixedrange = TRUE
        ),
        yaxis = list(
          visible = FALSE,showgrid = FALSE,
          zeroline = FALSE,fixedrange = TRUE,
          scaleanchor = "x"
        ),
        legend = list(
          orientation = "v",
          x = 0.2, y = -0.15,
          font = list(
            family = "Calibri, sans-serif",size = 12)
        ),
        margin = list(
          t = 80,b = 120,
          l = 20,r = 20
        ),
        annotations = list(
          list(
            text = "Quelle der Daten: Bundesagentur für Arbeit, 2026, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
            x = 0,y = -0.22,
            xref = "paper",yref = "paper",
            showarrow = FALSE,xanchor = "left",
            font = list(
              family = "Calibri, sans-serif",size = 11,color = "gray"
            )))) %>%
      plotly::config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "sendDataToCloud", "autoScale2d", "resetScale2d", "toggleSpikelines",
          "hoverClosestCartesian", "hoverCompareCartesian",
          "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d"
        ),
        modeBarButtonsToAdd = list(

          # CSV-Download
          list(
            name = "Download CSV",
            icon = list(
              path = "M16,2H8C6.9,2,6,2.9,6,4v16c0,1.1,0.9,2,2,2h8c1.1,0,2-0.9,2-2V4C18,2.9,17.1,2,16,2z M16,20H8V4h8V20z M14.5,14h-2v3h-1v-3h-2l2.5-3.5L14.5,14z",
              width = 24,
              height = 24
            ),

            click = htmlwidgets::JS(
              paste0("
              function(gd) {
                var rows = ", df_json, ";

                var date = new Date().toISOString().slice(0,10);
                var filename = 'export_' + date + '.csv';

                if (!rows.length) return;

                var cols = Object.keys(rows[0]);
                var csv = cols.join(';') + '\\n';

                rows.forEach(function(row) {
                  var values = cols.map(function(col) {
                    var value = row[col];
                    if (value == null) return '';
                    value = String(value).replace(/\"/g, '\"\"');
                    if (value.search(/[\";\\n]/) >= 0) {
                      value = '\"' + value + '\"';
                    }
                    return value;
                  });
                  csv += values.join(';') + '\\n';
                });

                var blob = new Blob([csv], { type: 'text/csv;charset=utf-8;' });

                var link = document.createElement('a');
                link.href = URL.createObjectURL(blob);
                link.download = filename;
                link.click();
              }
            ")
            )
          ),

          # TXT-Download für KI
          list(
            name = "Download Daten für KI-Chats als txt",
            icon = list(
              path = "M14,2H6C4.9,2,4,2.9,4,4v16c0,1.1,0.9,2,2,2h12c1.1,0,2-0.9,2-2V8L14,2z M14,4.5L17.5,8H14V4.5z M18,20H6V4h6v6h6V20z",
              width = 24,
              height = 24
            ),
            click = htmlwidgets::JS(
              paste0("
              function(gd) {
                var rows = ", df_json, ";
                var titel = ", titel_js, ";
                var quelle = ", quelle_js, ";

                var date = new Date().toISOString().slice(0,10);
                var chartTitle = titel.replace(/\\s+/g, '_');
                var filename = chartTitle + '_' + date + '.txt';

                if (!rows.length) return;

                var cols = Object.keys(rows[0]);

                var text = '';
                text += 'Titel: ' + titel + '\\n';
                text += 'Quelle: ' + quelle + '\\n\\n';
                text += 'Daten:\\n';

                text += cols.join('\\t') + '\\n';

                rows.forEach(function(row) {
                  var values = cols.map(function(col) {
                    var value = row[col];
                    if (value === null || value === undefined) return '';
                    return String(value);
                  });
                  text += values.join('\\t') + '\\n';
                });

                var blob = new Blob([text], { type: 'text/plain;charset=utf-8;' });

                var link = document.createElement('a');
                link.href = URL.createObjectURL(blob);
                link.download = filename;
                link.click();
              }
            ")
            )
          )
        )
      )


    return(list(plot_left, plot_right))

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


    # Entfernen aller Zeilen, bei denen group_col NA ist
    expanded_dt <- expanded_dt[!is.na(expanded_dt$group_col), ]




    hinweis <- dplyr::case_when(
      regio == "Schleswig-Holstein / Hamburg" ~
        "Es liegen nur zusammengefasste Daten für Hamburg <br> und Schleswig-Holstein vor.",

      regio == "Niedersachsen / Bremen" ~
        "Es liegen nur zusammengefasste Daten für <br> Niedersachsen und Bremen vor.",

      regio == "Brandenburg / Berlin" ~
        "Es liegen nur zusammengefasste Daten für <br> Berlin und Brandenburg vor.",

      regio == "Rheinland-Pfalz / Saarland" ~
        "Es liegen nur zusammengefasste Daten für <br> Rheinland-Pfalz und das Saarland vor.",

      TRUE ~ ""
    )


    titel_1 <- stringr::str_wrap(
      paste0( "Engpassrisiko in MINT-Berufen in ", regio," (", timerange, ")."),
      width = 40
    )

    titel_2<- stringr::str_wrap(
      paste0( "Engpassrisiko in Nicht-MINT-Berufen in ",regio," (", timerange, ")."),
              width = 40
    )




    df_download <- expanded_dt %>%
      dplyr::filter(mint_zuordnung == fach[1])

    # Entfernen aller Zeilen, bei denen group_col NA ist
    df_download <- df_download %>%
      dplyr::filter(!is.na(group_col))

    #df_json <- subset(plot_data, select = c(x, y, group))
    df_json <- jsonlite::toJSON(plot_data,dataframe = "rows",auto_unbox = TRUE, na = "null")
    titel_js <- jsonlite::toJSON(titel_1, auto_unbox = TRUE)
    quelle_js <- jsonlite::toJSON("Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",auto_unbox = TRUE)
    x_js      <- "XX"
    y_js      <- "YY"
    group_js  <- "epa_kat"



    plot_left <- plotly::plot_ly(
      data = df_download,
      x = ~XX,
      y = ~YY,
      type = "scatter",
      mode = "markers",
      color = ~epa_kat,
      colors = group_col_dt$group_col,
      text = ~paste0(
        "<b>", epa_kat, "</b><br>",
        "Anteil: ", value, " %<br>",
        "Anzahl betroffener Berufe: ", beruf_num
      ),
      hoverinfo = "text",
      marker = list(
        symbol = "square",
        size = 30,
        line = list(
          width = 0))
    ) %>%
      plotly::layout(
        title = list(
          text = titel_1,
          x = 0.5, y=0.95,
          xanchor = "center",
          font = list(
            family = "Calibri, sans-serif",
            size = 20,
            color = "black"
          )
        ),
        xaxis = list(
          visible = FALSE,showgrid = FALSE,
          zeroline = FALSE,fixedrange = TRUE
        ),
        yaxis = list(
          visible = FALSE,showgrid = FALSE,
          zeroline = FALSE,fixedrange = TRUE,
          scaleanchor = "x"
        ),
        legend = list(
          orientation = "v",
          x = 0.2,y = -0.15,
          font = list(
            family = "Calibri, sans-serif",size = 12)
        ),
        margin = list(
          t = 80,b = 120,
          l = 20,r = 20
        ),
        annotations = list(
          list(
            text = hinweis,
            x = 0.5,
            y = 0.98,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            xanchor = "center",
            font = list(
              family = "Calibri, sans-serif",
              size = 8,
              color = "gray"
            )
          ),
          list(
            text = "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
            x = 0,
            y = -0.22,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            xanchor = "left",
            font = list(
              family = "Calibri, sans-serif",
              size = 11,
              color = "gray"
            )
          )
        )
      ) %>%
      plotly::config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "sendDataToCloud", "autoScale2d", "resetScale2d", "toggleSpikelines",
          "hoverClosestCartesian", "hoverCompareCartesian",
          "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d"
        ),
        modeBarButtonsToAdd = list(

          # CSV-Download
          list(
            name = "Download CSV",
            icon = list(
              path = "M16,2H8C6.9,2,6,2.9,6,4v16c0,1.1,0.9,2,2,2h8c1.1,0,2-0.9,2-2V4C18,2.9,17.1,2,16,2z M16,20H8V4h8V20z M14.5,14h-2v3h-1v-3h-2l2.5-3.5L14.5,14z",
              width = 24,
              height = 24
            ),

            click = htmlwidgets::JS(
              paste0("
              function(gd) {
                var rows = ", df_json, ";

                var date = new Date().toISOString().slice(0,10);
                var filename = 'export_' + date + '.csv';

                if (!rows.length) return;

                var cols = Object.keys(rows[0]);
                var csv = cols.join(';') + '\\n';

                rows.forEach(function(row) {
                  var values = cols.map(function(col) {
                    var value = row[col];
                    if (value == null) return '';
                    value = String(value).replace(/\"/g, '\"\"');
                    if (value.search(/[\";\\n]/) >= 0) {
                      value = '\"' + value + '\"';
                    }
                    return value;
                  });
                  csv += values.join(';') + '\\n';
                });

                var blob = new Blob([csv], { type: 'text/csv;charset=utf-8;' });

                var link = document.createElement('a');
                link.href = URL.createObjectURL(blob);
                link.download = filename;
                link.click();
              }
            ")
            )
          ),

          # TXT-Download für KI
          list(
            name = "Download Daten für KI-Chats als txt",
            icon = list(
              path = "M14,2H6C4.9,2,4,2.9,4,4v16c0,1.1,0.9,2,2,2h12c1.1,0,2-0.9,2-2V8L14,2z M14,4.5L17.5,8H14V4.5z M18,20H6V4h6v6h6V20z",
              width = 24,
              height = 24
            ),
            click = htmlwidgets::JS(
              paste0("
              function(gd) {
                var rows = ", df_json, ";
                var titel = ", titel_js, ";
                var quelle = ", quelle_js, ";

                var date = new Date().toISOString().slice(0,10);
                var chartTitle = titel.replace(/\\s+/g, '_');
                var filename = chartTitle + '_' + date + '.txt';

                if (!rows.length) return;

                var cols = Object.keys(rows[0]);

                var text = '';
                text += 'Titel: ' + titel + '\\n';
                text += 'Quelle: ' + quelle + '\\n\\n';
                text += 'Daten:\\n';

                text += cols.join('\\t') + '\\n';

                rows.forEach(function(row) {
                  var values = cols.map(function(col) {
                    var value = row[col];
                    if (value === null || value === undefined) return '';
                    return String(value);
                  });
                  text += values.join('\\t') + '\\n';
                });

                var blob = new Blob([text], { type: 'text/plain;charset=utf-8;' });

                var link = document.createElement('a');
                link.href = URL.createObjectURL(blob);
                link.download = filename;
                link.click();
              }
            ")
            )
          )
        )
      )




    fach_2 <- dplyr::case_when(
      fach[2] == "MINT gesamt" ~ "MINT",
      fach[2] == "Gesamt" ~ "allen Berufen",
      fach[2] == "Nicht MINT" ~ "allen Berufen außer MINT",
      T ~ fach[2]
    )


    used_colors <- group_col_dt %>%
      dplyr::filter(epa_kat %in% (expanded_dt %>%
                                    dplyr::filter(mint_zuordnung == fach[2]) %>%
                                    dplyr::pull(epa_kat) %>%
                                    unique())) %>%
      dplyr::pull(group_col)


    df_download2 <- expanded_dt %>%
      dplyr::filter(mint_zuordnung == fach[2]) %>%
      dplyr::filter(!is.na(group_col))

    df_json <- jsonlite::toJSON(plot_data,dataframe = "rows",auto_unbox = TRUE, na = "null")

    #df_json <- jsonlite::toJSON(df_download2,dataframe = "rows",auto_unbox = TRUE, na = "null")
    titel_js <- jsonlite::toJSON(titel_2, auto_unbox = TRUE)
    quelle_js <- jsonlite::toJSON("Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",auto_unbox = TRUE)
    x_js      <- "XX"
    y_js      <- "YY"
    group_js  <- "epa_kat"



    plot_right <- plotly::plot_ly(
      data = df_download2,
      x = ~XX,
      y = ~YY,
      type = "scatter",
      mode = "markers",
      color = ~epa_kat,
      colors = group_col_dt$group_col,
      text = ~paste0(
        "<b>", epa_kat, "</b><br>",
        "Anteil: ", value, " %<br>",
        "Anzahl betroffener Berufe: ", beruf_num
      ),
      hoverinfo = "text",
      marker = list(
        symbol = "square",
        size = 30,
        line = list(
          width = 0))
    ) %>%
      plotly::layout(
        title = list(
          text = titel_2,
          x = 0.5,y=0.95, xanchor = "center",
          font = list(
            family = "Calibri, sans-serif",size = 20,color = "black")
        ),
        xaxis = list(
          visible = FALSE,showgrid = FALSE,
          zeroline = FALSE,fixedrange = TRUE
        ),
        yaxis = list(
          visible = FALSE,showgrid = FALSE,
          zeroline = FALSE,fixedrange = TRUE,
          scaleanchor = "x"
        ),
        legend = list(
          orientation = "v",
          x = 0.2, y = -0.15,
          font = list(
            family = "Calibri, sans-serif",size = 12)
        ),
        margin = list(
          t = 80,b = 120,
          l = 20,r = 20
        ),
        annotations = list(
          list(
            text = hinweis,
            x = 0.5,
            y = 0.98,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            xanchor = "center",
            font = list(
              family = "Calibri, sans-serif",
              size = 8,
              color = "gray"
              )
            ),
          list(
            text = "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
            x = 0,y = -0.22,
            xref = "paper",yref = "paper",
            showarrow = FALSE,xanchor = "left",
            font = list(
              family = "Calibri, sans-serif",size = 11,color = "gray"
            )))) %>%
      plotly::config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "sendDataToCloud", "autoScale2d", "resetScale2d", "toggleSpikelines",
          "hoverClosestCartesian", "hoverCompareCartesian",
          "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d"
        ),
        modeBarButtonsToAdd = list(

          # CSV-Download
          list(
            name = "Download CSV",
            icon = list(
              path = "M16,2H8C6.9,2,6,2.9,6,4v16c0,1.1,0.9,2,2,2h8c1.1,0,2-0.9,2-2V4C18,2.9,17.1,2,16,2z M16,20H8V4h8V20z M14.5,14h-2v3h-1v-3h-2l2.5-3.5L14.5,14z",
              width = 24,
              height = 24
            ),

            click = htmlwidgets::JS(
              paste0("
              function(gd) {
                var rows = ", df_json, ";

                var date = new Date().toISOString().slice(0,10);
                var filename = 'export_' + date + '.csv';

                if (!rows.length) return;

                var cols = Object.keys(rows[0]);
                var csv = cols.join(';') + '\\n';

                rows.forEach(function(row) {
                  var values = cols.map(function(col) {
                    var value = row[col];
                    if (value == null) return '';
                    value = String(value).replace(/\"/g, '\"\"');
                    if (value.search(/[\";\\n]/) >= 0) {
                      value = '\"' + value + '\"';
                    }
                    return value;
                  });
                  csv += values.join(';') + '\\n';
                });

                var blob = new Blob([csv], { type: 'text/csv;charset=utf-8;' });

                var link = document.createElement('a');
                link.href = URL.createObjectURL(blob);
                link.download = filename;
                link.click();
              }
            ")
            )
          ),

          # TXT-Download für KI
          list(
            name = "Download Daten für KI-Chats als txt",
            icon = list(
              path = "M14,2H6C4.9,2,4,2.9,4,4v16c0,1.1,0.9,2,2,2h12c1.1,0,2-0.9,2-2V8L14,2z M14,4.5L17.5,8H14V4.5z M18,20H6V4h6v6h6V20z",
              width = 24,
              height = 24
            ),
            click = htmlwidgets::JS(
              paste0("
              function(gd) {
                var rows = ", df_json, ";
                var titel = ", titel_js, ";
                var quelle = ", quelle_js, ";

                var date = new Date().toISOString().slice(0,10);
                var chartTitle = titel.replace(/\\s+/g, '_');
                var filename = chartTitle + '_' + date + '.txt';

                if (!rows.length) return;

                var cols = Object.keys(rows[0]);

                var text = '';
                text += 'Titel: ' + titel + '\\n';
                text += 'Quelle: ' + quelle + '\\n\\n';
                text += 'Daten:\\n';

                text += cols.join('\\t') + '\\n';

                rows.forEach(function(row) {
                  var values = cols.map(function(col) {
                    var value = row[col];
                    if (value === null || value === undefined) return '';
                    return String(value);
                  });
                  text += values.join('\\t') + '\\n';
                });

                var blob = new Blob([text], { type: 'text/plain;charset=utf-8;' });

                var link = document.createElement('a');
                link.href = URL.createObjectURL(blob);
                link.download = filename;
                link.click();
              }
            ")
            )
          )
        )
      )


    return(list(plot_left, plot_right))

  }

}



argument_demografie <- function(r){

  betrachtung <- "Gruppenvergleich - Balkendiagramm"
  timerange <- 2025
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


  titel <- ifelse(regio == "Saarland",
                  paste0("Demografischer Wandel: Beschäftigte in MINT nach Altersgruppen im ", regio, " (", timerange, ")"),
                  paste0("Demografischer Wandel: Beschäftigte in MINT nach Altersgruppen in ", regio, " (", timerange, ")"))

  quelle <- "Quelle: Bundesagentur für Arbeit, 2026, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

  quelle_y <- -0.13


  order <- unique(df$indikator)

  df1 <- df1 %>%
    dplyr::mutate(
      .tooltip = paste0(
        "<b><span style='font-size:15px;'>", indikator, "</span></b><br>",
        "Anzahl: ",(formatC(as.numeric(wert), format = "f", digits = 0, big.mark = "."))
      ))


  x <- "indikator"
  y <- "wert"

  color <- c("Beschäftigte" = "#154194", "Beschäftigte 25-55" = "#b16fab", "Beschäftigte ü55" = "#b16fab", "Beschäftigte u25" = "#b16fab" )

  out <- balkenbuilder_plotly(df=df1, x=x, y=y, titel=titel, orientation = "h",percent=FALSE, group=NULL, color=color, quelle_y=quelle_y,
                              order=order, stacking = FALSE, quelle=quelle)




  return(out)

}



argument_nachwuchs <- function(r){

  regio <- r$region_argumentationshilfe

  query_df <- glue::glue_sql("
  SELECT region, fach, jahr, indikator, wert
  FROM studierende_detailliert
  WHERE jahr IN (2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024)
    AND region = {regio}
    AND geschlecht = 'Gesamt'
    AND fach IN ('Mathematik, Naturwissenschaften', 'Informatik', 'Ingenieurwissenschaften (ohne Informatik)')
    AND indikator = 'Studierende'
", .con = con)

  df_studierende <- DBI::dbGetQuery(con, query_df)

  query_df <- glue::glue_sql("
  SELECT bundesland, fachbereich, jahr, indikator, wert
  FROM arbeitsmarkt_detail
  WHERE jahr IN (2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024)
    AND bundesland = {regio}
    AND geschlecht = 'Gesamt'
    AND fachbereich IN ('Mathematik, Naturwissenschaften', 'Informatik', 'Technik (gesamt)')
    AND indikator = 'Auszubildende'
", .con = con)

  df_auszubildende <- DBI::dbGetQuery(con, query_df)

  df_azubi_clean <- df_auszubildende %>%
    dplyr::rename(region = bundesland, fach = fachbereich) %>%
    dplyr::mutate(
      fach = dplyr::case_when(
        fach == "Technik (gesamt)" ~ "Technik (inkl. Ingenieurwesen)",
        TRUE ~ fach
      ),
      indikator = "Nachwuchs",
      wert = as.numeric(wert),
      jahr = as.numeric(jahr)
    ) %>%
    dplyr::mutate(across(c(region, fach, indikator), as.character)) %>%
    dplyr::filter(!is.na(wert))

  df_studi_clean <- df_studierende %>%
    dplyr::rename(fach = fach) %>%
    dplyr::mutate(
      fach = dplyr::case_when(
        fach == "Ingenieurwissenschaften (ohne Informatik)" ~ "Technik (inkl. Ingenieurwesen)",
        TRUE ~ fach
      ),
      indikator = "Nachwuchs"
    )

  df_nachwuchs <- dplyr::bind_rows(df_azubi_clean, df_studi_clean)

  df_nachwuchs_agg <- df_nachwuchs %>%
    dplyr::group_by(region, fach, jahr, indikator) %>%
    dplyr::summarise(wert = sum(wert), .groups = "drop") %>%
    dplyr::ungroup()

  # Entwicklung für Hover berechnen
  df_start <- df_nachwuchs_agg %>%
    dplyr::filter(jahr == min(df_nachwuchs_agg$jahr)) %>%
    dplyr::select(fach, wert) %>%
    dplyr::rename(wert_alt = wert)
  df_ende <- df_nachwuchs_agg %>%
    dplyr::filter(jahr == max(df_nachwuchs_agg$jahr)) %>%
    dplyr::select(fach, wert) %>%
    dplyr::rename(wert_neu = wert)
  df_nachwuchs_agg <- df_nachwuchs_agg %>%
    dplyr::left_join(df_start, by = c("fach")) %>%
    dplyr::left_join(df_ende, by = c("fach")) %>%
    dplyr::mutate(diff = round(((wert_neu - wert_alt)/wert_alt)*100,1))

  df_nachwuchs_agg$display_diff <- ifelse(df_nachwuchs_agg$diff < 0,
                                          paste0("-", df_nachwuchs_agg$diff),
                                          paste0("+", df_nachwuchs_agg$diff))

  # Wert für Anzeige formatieren
  df_nachwuchs_agg$wert_disp <- prettyNum(df_nachwuchs_agg$wert, big.mark = ".", decimal.mark = ",")

  df_nachwuchs_agg <- df_nachwuchs_agg %>%
    dplyr::mutate(
      tooltip = paste0(
        "<b>", fach, "</b><br>",
        "Jahr: ", jahr, "<br>",
        "Anzahl: ", wert_disp, "<br>",
        "Veränderung seit 2017: ", display_diff
      )
    )
  format <- ",d"

  #Farben zuweisen
  sorted_indicators <- df_nachwuchs_agg %>%
    dplyr::group_by(fach) %>%
    dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
    dplyr::arrange(m_value) %>%
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


  df_nachwuchs_agg <- df_nachwuchs_agg %>%
    dplyr::mutate(label = dplyr::case_when(
      fach == "Informatik" ~ "",
      fach %in% c("Mathematik, Naturwissenschaften",
                  "Technik (inkl. Ingenieurwesen)") ~ wert_disp
    ))


  titel <- ifelse(regio == "Saarland",
                  paste0("Entwicklung der Nachwuchszahlen in den MINT-Disziplinen im ", regio),
                  paste0("Entwicklung der Nachwuchszahlen in den MINT-Disziplinen in ", regio))
  quelle <- "Destatis, 2025 und Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."


  out <- linebuilder_plotly(df_nachwuchs_agg, titel = titel, x = "jahr",
                            y = "wert", group = "fach", format = format, color = colors,
                            quelle = quelle, label = TRUE)

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



  final_data <- uebersicht_data %>%
    dplyr::select(wirkhebel, diff)


  final_data <- final_data[with(final_data, order(diff, decreasing = TRUE)),]


  titel <- "Einfluss der folgenden vier Wirkhebel auf die Entwicklung der MINT-Fachkräfte bis 2037 deutschlandweit <br>"
  quelle <- "Berechnungen durch das IW Köln, 2024, beauftragt durch MINTvernetzt."

  quelle_y <- -0.13


  order <- unique(final_data$wirkhebel)

  final_data <- final_data %>%
    dplyr::mutate(
      .tooltip = paste0(
        "<b><span style='font-size:15px;'>", "Anzahl an MINT-Fachkräften, die bis 2037 gewonnen werden können: </span></b>",
        "<b><span style='font-size:15px;'>", (formatC(as.numeric(diff), format = "f", digits = 0, big.mark = ".")), "</span></b>"
      ))


  x <- "wirkhebel"
  y <- "diff"

  color <- c("Gesamteffekt" = "#154194",
             "MINT-Nachwuchs fördern" = "#b16fab",
             "Mädchen und Frauen in MINT fördern" = "#b16fab",
             "Verbleib älterer MINT-Fachkräfte" = "#b16fab",
             "Zuwanderung MINT-Fachkräfte" = "#b16fab")

  out <- balkenbuilder_plotly(df=final_data, x=x, y=y, titel=titel, orientation = "h",percent=FALSE, group=NULL, color=color,
                              quelle_y=quelle_y,order=order, stacking = FALSE, quelle=quelle)



  return(out)


}


# Funktionen Frauen-Grafiken ----------------------------------------------

argument_frauen_bildungskette <- function(r){

  # load UI inputs from reactive value
  zeit <- 2024
  regio <- r$region_argumentationshilfe
  indikator_choice <- c("Leistungskurse", "Studierende",
                        "Auszubildende", "Beschäftigte")

  # filter dataset based on UI input
  query_df <- glue::glue_sql("
  SELECT bereich, indikator, fachbereich, geschlecht, wert
  FROM zentral
  WHERE jahr = {zeit}
    AND region = {regio}
    AND geschlecht IN ('Frauen', 'Männer')
    AND fachbereich = 'MINT'
", .con = con)

  df <- DBI::dbGetQuery(con, query_df)

  query_df_alle <- glue::glue_sql("
  SELECT bereich, indikator, fachbereich, geschlecht, wert
  FROM zentral
  WHERE jahr = {zeit}
    AND region = {regio}
    AND geschlecht = 'Gesamt'
    AND fachbereich = 'MINT'
", .con = con)

  df_alle <- DBI::dbGetQuery(con, query_df_alle)

if (regio == "Deutschland"){

  #Baden-Würrtemberg rausrechnen, da dort keine Geschlechter erfasst werden
  query_df_alle_bw <- glue::glue_sql("
  SELECT bereich, indikator, fachbereich, geschlecht, wert
  FROM zentral
  WHERE jahr = {zeit}
    AND region = 'Baden-Württemberg'
    AND geschlecht = 'Gesamt'
    AND fachbereich = 'MINT'
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


    df$indikator[df$indikator == "Leistungskurse"] <- "Schüler:innen im Leistungskurs"


  #sortieren
  df <- df[with(df, order(geschlecht, decreasing = TRUE)), ]

  #Titel erstellen
  df$titel_help <- "Schüler:innen in MINT-Leistungskursen"
  df$titel_help <- ifelse(df$indikator == "Beschäftigte", "MINT-Beschäftigte", df$titel_help)
  df$titel_help <- ifelse(df$indikator == "Auszubildende", "MINT-Auszubildende", df$titel_help)
  df$titel_help <- ifelse(df$indikator == "Studierende", "MINT-Studierende", df$titel_help)


  df <- df[with(df, order(prop, decreasing = TRUE)), ]


  order <- rev(unique(df$indikator))

  df <- df %>%
    dplyr::mutate(
      .tooltip = paste0(
        "<b><span style='font-size:15px;'>", indikator, "</span></b><br>",
        "Anteil: ", round(prop, 1), " %<br>",
        "Anzahl: ", (formatC(as.numeric(wert), format = "f", digits = 0, big.mark = "."))
      ))



  titel <- ifelse(regio == "Saarland",
                  paste0("Anteil von Frauen in MINT nach Bildungsbereichen im ", regio, " (", zeit, ")"),
                  paste0("Anteil von Frauen in MINT nach Bildungsbereichen in ", regio, " (", zeit, ")"))


  color <- c("Frauen" = "#154194","Männer" = "#efe8e6")

  quelle <- "Quellen: Destatis, 2025; Bundesagentur für Arbeit, 2025; KMK, 2025, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt."
  quelle_y <- -0.16
  legend_y <- -0.07

  x <- "indikator"
  y <- "prop"
  group <- "geschlecht"

  out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h",percent=TRUE, group=group, color=color,
                              order=order, stacking = TRUE, quelle=quelle, quelle_y=quelle_y, legend_y=legend_y)




  return(out)

}


argument_großer_unterschied <- function(r) {

    color_fachbereich <- c(
      "Informatik" = "#2D6BE1",
      "Technik (gesamt)" = "#00a87a",
      "Mathematik, Naturwissenschaften" = "#fcc433",
      "andere Berufsfelder" = "#efe8e6"
    )

    timerange <- 2025
    regio <- r$region_argumentationshilfe
    indi <- "Beschäftigte"

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

    quelle <- "Quelle: Bundesagentur für Arbeit, 2026, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    out_1 <- piebuilder_plotly(df_f, titel1, x="fachbereich", y = "prop", legend_y= 0.06,
                               color=color_fachbereich, subtitel = subtitel1, quelle="") |>
      plotly::layout(
        annotations = list(
          list(
            text = quelle,
            x = 1,
            y = -0.30,
            xref = "paper",
            yref = "paper",
            xanchor = "right",
            yanchor = "top",
            showarrow = FALSE,
            font = list(size = 11, color = "gray", family = "Calibri Regular", align = "right")
          )
        )
      )
    out_2 <- piebuilder_plotly(df_m, titel2, x="fachbereich", y = "prop", legend_y= 0.06,
                               color=color_fachbereich, subtitel = subtitel2, quelle="")|>
      plotly::layout(
        annotations = list(
          list(
            text = quelle,
            x = 1,
            y = -0.30,
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





  return(out)
}



argument_selbstkonzept <- function(r){

  # reactive values einlesen

    jahr_select <- 2024
    region_select <- r$region_argumentationshilfe
    gruppe_select <- c("Mädchen", "Jungen")

    df_query <- glue::glue_sql("
    SELECT fach, indikator, geschlecht, typ, jahr, wert
    FROM iqb
    WHERE typ IN ('Mittelwert', 'Standardabweichung')
    AND indikator = 'Selbstkonzept'
    AND jahr = {jahr_select}
    AND region = {region_select}
    AND geschlecht IN ({gruppe_select[1]}, {gruppe_select[2]})
                               ", .con = con)
    df <- DBI::dbGetQuery(con, df_query)

    # als Faktor speichern für Reihenfolge und Selbstkonzept umbennenen
    df <- df %>%
      dplyr::mutate(
        indikator = dplyr::case_when(
          indikator == "Selbstkonzept" ~ "Selbsteinschätzung der eigenen Fähigkeiten"
        ))

    df_sd <- df %>%
      dplyr::filter(typ == "Standardabweichung") %>%
      dplyr::rename("sd" = "wert") %>%
      dplyr::select(-typ)

    df <- df %>%
      dplyr::filter(typ == "Mittelwert") %>%
      dplyr::select(-typ) %>%
      dplyr::left_join(df_sd, by = c("geschlecht", "fach", "indikator", "jahr"))

    df <- df %>%
      dplyr::mutate(display_rel = prettyNum(round(df$wert,1), big.mark = ".", decimal.mark = ","),
                    display_sd = prettyNum(sd, big.mark = ".", decimal.mark = ","))

     df$geschlecht <- as.factor(df$geschlecht)
     df$geschlecht <- factor(df$geschlecht, levels = c("Mädchen", "Jungen"))

     df <- df %>%
       dplyr::mutate(wert = round(wert, 1))


  # plot


    order <- rev(unique(df$fach))

    df <- df %>%
      dplyr::mutate(
        .tooltip = paste0(
          "<b><span style='font-size:15px;'>", fach, "</span></b><br>",
          geschlecht, ": ", round(wert, 1), " (SD = ", sd, ")" )
        )


    titel <- ifelse(region_select == "Saarland",
                    paste0("Selbsteinschätzung der eigenen Fähigkeiten in MINT-Fächern von Schüler:innen der 9. Klasse im ", region_select, " (", jahr_select, ")"),
                    paste0("Selbsteinschätzung der eigenen Fähigkeiten in MINT-Fächern von Schüler:innen der 9. Klasse in ", region_select, " (", jahr_select, ")"))


    color <- c("Mädchen" = "#154194","Jungen" = "#efe8e6")
    quelle <- "Quelle der Daten: Institut zur Qualitätsentwicklung im Bildungswesen, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    quelle_y <- -0.15
    legend_y <- -0.06

    x <- "fach"
    y <- "wert"
    group <- "geschlecht"

    out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "v",percent=TRUE, group=group, color=color,
                                order=order, stacking = FALSE, quelle=quelle, quelle_y=quelle_y, legend_y=legend_y) %>%
  plotly::layout(
    bargroupgap = 0.4
  )



  return(out)

}


argument_faecherverteilung <- function(r){

  # load UI inputs from reactive value
  timerange <- 2024
  regio <- r$region_argumentationshilfe

  color_fach_balken <- c(
    "Informatik" = "#00a87a",
    "Elektrotechnik und Informationstechnik" = "#00a87a",
    "Maschinenbau/Verfahrenstechnik" = "#00a87a",
    "Biologie" = "#fcc433",
    "Mathematik" = "#fcc433",
    "Wirtschaftsingenieurwesen mit ingenieurwissenschaftlichem Schwerpunkt" =
      "#00a87a",
    "Bauingenieurwesen" = "#00a87a",
    "Ingenieurwesen allgemein" = "#00a87a",
    "Chemie" = "#fcc433",
    "Physik, Astronomie" = "#fcc433",
    "Architektur, Innenarchitektur" ="#00a87a",
    "Verkehrstechnik, Nautik" = "#00a87a",
    "Geographie" = "#fcc433",
    "allgemeine naturwissenschaftliche und mathematische Fächer" = "#fcc433",
    "Pharmazie" = "#fcc433",
    "Geowissenschaften (ohne Geographie)" = "#fcc433",
    "Materialwissenschaft und Werkstofftechnik" = "#00a87a",
    "Vermessungswesen" = "#00a87a",
    "Bergbau, Hüttenwesen" = "#00a87a",
    "Raumplanung" = "#00a87a",
    "Alle Nicht MINT-Fächer" = "#efe8e6"
  )

  # filter dataset based on UI inputs

    df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr == {timerange}
        AND typ = 'Einzelauswahl'
        AND geschlecht = 'Frauen'
        AND indikator = 'Studierende'
        AND region = {regio}
        AND mint_select = 'MINT'
                               ", .con = con)
    df <- DBI::dbGetQuery(con, df_query)

    df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr == {timerange}
        AND geschlecht = 'Gesamt'
        AND indikator = 'Studierende'
        AND typ = 'Einzelauswahl'
        AND region = {regio}
        AND mint_select = 'MINT'
                               ", .con = con)
    alle <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::left_join(alle,
                       by = c("region", "jahr", "bereich", "indikator", "mint_select", "typ", "fachbereich", "fach")) %>%
      dplyr::rename(
        wert = wert.x,
        wert_ges = wert.y
      ) %>%
      dplyr::mutate(prop = round(wert / wert_ges * 100, 1))


    df <- df[with(df, order(prop, decreasing = TRUE)), ]


    titel <- ifelse(regio == "Saarland",
                paste0( "Anteil der weiblichen Studierenden nach Fachbereich im ",regio," (", timerange, ")"),
                paste0( "Anteil der weiblichen Studierenden nach Fachbereich in ",regio," (", timerange, ")"))




  order <- unique(df$fach)

  df <- df %>%
    dplyr::mutate(
      .tooltip = paste0(
        "<b><span style='font-size:15px;'>", fach, "</span></b><br>",
        "<span style='font-size:15px;'>Deutschland </span><br>",
        "Anteil: ", prop, "% <br>",
        "Anzahl: ", (formatC(as.numeric(wert),format = "f",digits = 0,big.mark = ".")), "<br>"
    ))




  color <- color_fach_balken
  quelle <- "Destatis, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
  quelle_y <- -0.12

  x <- "fach"
  y <- "prop"

  out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h",percent=TRUE, color=color, group=NULL,
                              order=order, stacking = FALSE, quelle=quelle, quelle_y=quelle_y)




  return(out)

}

### argument_wirkhebel wie oben ----




