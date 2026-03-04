
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
  timerange <- 2024
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
    timerange <- 2024 #L
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
      SELECT bundesland, fachbereich, jahr, kategorie, wert
      FROM arbeitsmarkt_detail
      WHERE jahr IN (2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024)
      AND bundesland = {regio}
      AND geschlecht = 'Gesamt'
      AND fachbereich IN ('Mathematik, Naturwissenschaften', 'Informatik', 'Technik (gesamt)')
      AND kategorie = 'Auszubildende'
      AND indikator = 'Auszubildende'
      AND landkreis = 'alle Landkreise'
      ", .con = con)

    df_auszubildende <- DBI::dbGetQuery(con, query_df)


    df_azubi_clean <- df_auszubildende %>%
      dplyr::rename(region = bundesland, fach = fachbereich, indikator = kategorie) %>%
      dplyr::mutate(
        fach = dplyr::case_when(
          fach == "Technik (gesamt)" ~ "Technik (inkl. Ingenieurwesen)",
          TRUE ~ fach
        ),
        indikator = "Nachwuchs"
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
             Quelle = "Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt",
             Region = save_regio)

    # 3. Demografie MINT
    df_demografie_clean <- df %>%
      dplyr::mutate(Bereich = "Demografie MINT",
             Quelle = "Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt",
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

    ### Frauen Vergleich Zeitverlauf ----

    t<- 2015:2024
    indikator_choice <- c("Leistungskurse", "Studierende",
                          "Auszubildende", "Beschäftigte")

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
    df_fr_vgl <- df[with(df, order(indikator, jahr, decreasing = FALSE)), ]

    ### Frauen in MINT-Berufen ----
    timerange <- 2024
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
                    Quelle = "Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt",
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


argument_verlauf <- function(r){

  # load UI inputs from reactive value
  t <- 2017:2024
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

    titel <- titel_beschäftigte
    quelle <- "Destatis, 2025 und Bundesagentur für Arbeit, 2025, beides auf Anfrage, eigene Berechnungen durch MINTvernetzt."


    out_beschäftigte <- highcharter::hchart(df_beschäftigte, 'line', highcharter::hcaes(x = "jahr", y = "wert")) %>%
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
      highcharter::hc_caption(text = "Quellen: Destatis, 2025 und Bundesagentur für Arbeit, 2025, beides auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
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
     data += '\\nQuelle: %s';
     var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
     if (window.navigator.msSaveBlob) {
       window.navigator.msSaveBlob(blob, filename);
     } else {
       var link = document.createElement('a');
       link.href = URL.createObjectURL(blob);
       link.download = filename;
       link.click();
     }
   }", gsub("'", "\\\\'", titel),
       gsub("'", "\\\\'", titel),
       gsub("'", "\\\\'", quelle)
                                                         )  #
                                                       )))
                                  ))
      )

    titel <- titel_andere
    quelle <- "Destatis, 2025 und Bundesagentur für Arbeit, 2025, beides auf Anfrage, eigene Berechnungen durch MINTvernetzt"

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
      highcharter::hc_caption(text = "Quellen: Destatis, 2025 und Bundesagentur für Arbeit, 2025, beides auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
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
     data += '\\nQuelle: %s';
     var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
     if (window.navigator.msSaveBlob) {
       window.navigator.msSaveBlob(blob, filename);
     } else {
       var link = document.createElement('a');
       link.href = URL.createObjectURL(blob);
       link.download = filename;
       link.click();
     }
   }", gsub("'", "\\\\'", titel),
       gsub("'", "\\\\'", titel),
       gsub("'", "\\\\'", quelle)
                                                         )  #
                                                       )))
                                  ))
      )

    out <- highcharter::hw_grid(
      out_beschäftigte, out_andere,
      ncol = 2)


  return (out)

}

argument_fachkraft <- function(r){

  timerange <- 2024
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


    titel <- titel_1
    quelle <- "Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

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
      highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
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
     data += '\\nQuelle: %s';
     var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
     if (window.navigator.msSaveBlob) {
       window.navigator.msSaveBlob(blob, filename);
     } else {
       var link = document.createElement('a');
       link.href = URL.createObjectURL(blob);
       link.download = filename;
       link.click();
     }
   }", gsub("'", "\\\\'", titel),
      gsub("'", "\\\\'", titel),
      gsub("'", "\\\\'", quelle)
                                                         )  #
                                                       )))
                                  ))
      )

    fach_2 <- dplyr::case_when(
      fach[2] == "MINT gesamt" ~ "MINT",
      fach[2] == "Gesamt" ~ "allen Berufen",
      fach[2] == "Nicht MINT" ~ "allen Berufen außer MINT",
      T ~ fach[2]
    )

    titel_2 <- paste0("Engpassrisiko in ", fach_2," (", timerange, ")")
    quelle <- "Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."


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
      highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt.",
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
     data += '\\nQuelle: %s';
     var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
     if (window.navigator.msSaveBlob) {
       window.navigator.msSaveBlob(blob, filename);
     } else {
       var link = document.createElement('a');
       link.href = URL.createObjectURL(blob);
       link.download = filename;
       link.click();
     }
   }", gsub("'", "\\\\'", titel_2),
       gsub("'", "\\\\'", titel_2),
       gsub("'", "\\\\'", quelle)
                                                         )  #
                                                       )))
                                  ))
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

    titel <- titel_1
    quelle <- "Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."


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
                                    menuItems = list("downloadPNG", "downloadCSV",
                                                     list(
                                                       text = "Daten für GPT",
                                                       onclick = htmlwidgets::JS(sprintf(
                                                         "function () {
     var date = new Date().toISOString().slice(0,10);
     var chartTitle = '%s'.replace(/\\s+/g, '_');
     var filename = chartTitle + '_' + date + '.txt';

     var data = 'Titel: %s\\n' + this.getCSV();
     data += '\\nQuelle: %s';
     var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
     if (window.navigator.msSaveBlob) {
       window.navigator.msSaveBlob(blob, filename);
     } else {
       var link = document.createElement('a');
       link.href = URL.createObjectURL(blob);
       link.download = filename;
       link.click();
     }
   }", gsub("'", "\\\\'", titel),
       gsub("'", "\\\\'", titel),
       gsub("'", "\\\\'", quelle)
                                                         )  #
                                                       )))
                                  ))
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

      titel <- titel_2
      quelle <- "Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."


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
                                      menuItems = list("downloadPNG", "downloadCSV",
                                                       list(
                                                         text = "Daten für GPT",
                                                         onclick = htmlwidgets::JS(sprintf(
                                                           "function () {
     var date = new Date().toISOString().slice(0,10);
     var chartTitle = '%s'.replace(/\\s+/g, '_');
     var filename = chartTitle + '_' + date + '.txt';

     var data = 'Titel: %s\\n' + this.getCSV();
     data += '\\nQuelle: %s';
     var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
     if (window.navigator.msSaveBlob) {
       window.navigator.msSaveBlob(blob, filename);
     } else {
       var link = document.createElement('a');
       link.href = URL.createObjectURL(blob);
       link.download = filename;
       link.click();
     }
   }", gsub("'", "\\\\'", titel),
       gsub("'", "\\\\'", titel),
       gsub("'", "\\\\'", quelle)
                                                           )  #
                                                         )))
                                    ))
        )


      out <- highcharter::hw_grid(
        plot_left, plot_right,
        ncol = 2)

      return(out)

  }

}

argument_demografie <- function(r){

  betrachtung <- "Gruppenvergleich - Balkendiagramm"
  timerange <- 2024
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
  quelle <- "Quelle: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

  out <- highcharter::hchart(df, 'bar', highcharter::hcaes(
    y = !!rlang::sym("wert"),
    x = !!rlang::sym("indikator"),
    color = !!rlang::sym("color")
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
    highcharter::hc_caption(text = quelle,
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
     data += '\\nQuelle: %s';
     var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
     if (window.navigator.msSaveBlob) {
       window.navigator.msSaveBlob(blob, filename);
     } else {
       var link = document.createElement('a');
       link.href = URL.createObjectURL(blob);
       link.download = filename;
       link.click();
     }
   }", gsub("'", "\\\\'", titel),
       gsub("'", "\\\\'", titel),
       gsub("'", "\\\\'", quelle)
                                                       )  #
                                                     )))
                                ))
    )

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
  SELECT bundesland, fachbereich, jahr, kategorie, wert
  FROM arbeitsmarkt_detail
  WHERE jahr IN (2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024)
    AND bundesland = {regio}
    AND geschlecht = 'Gesamt'
    AND fachbereich IN ('Mathematik, Naturwissenschaften', 'Informatik', 'Technik (gesamt)')
    AND kategorie = 'Auszubildende'
", .con = con)

  df_auszubildende <- DBI::dbGetQuery(con, query_df)



  df_azubi_clean <- df_auszubildende %>%
    dplyr::rename(region = bundesland, fach = fachbereich, indikator = kategorie) %>%
    dplyr::mutate(
      fach = dplyr::case_when(
        fach == "Technik (gesamt)" ~ "Technik (inkl. Ingenieurwesen)",
        TRUE ~ fach
      ),
      indikator = "Nachwuchs",
      wert = as.numeric(wert)
    ) %>%
    dplyr::mutate(across(c(region, fach, indikator), as.character)) %>%
    dplyr::filter(!is.na(wert))

  df_azubi_clean %>%
    dplyr::group_by(region, fach, jahr, indikator) %>%
    dplyr::summarise(wert = sum(wert, na.rm = TRUE), .groups = "drop")



  df_studi_clean <- df_studierende %>%
    dplyr::rename(fach = fach) %>%
    dplyr::mutate(
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

  titel <- ifelse(regio == "Saarland",
                  paste0("Entwicklung der Nachwuchszahlen in den MINT-Disziplinen im ", regio),
                  paste0("Entwicklung der Nachwuchszahlen in den MINT-Disziplinen in ", regio))
  quelle <- "Destatis, 2025 und Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

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
    highcharter::hc_caption(text = quelle,
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
     data += '\\nQuelle: %s';
     var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
     if (window.navigator.msSaveBlob) {
       window.navigator.msSaveBlob(blob, filename);
     } else {
       var link = document.createElement('a');
       link.href = URL.createObjectURL(blob);
       link.download = filename;
       link.click();
     }
   }", gsub("'", "\\\\'", titel),
       gsub("'", "\\\\'", titel),
       gsub("'", "\\\\'", quelle)
                                                       )  #
                                                     )))
                                ))
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
  quelle <- "Berechnungen durch das IW Köln, 2024, beauftragt durch MINTvernetzt."
  tooltip <- paste0("Anzahl an MINT-Fachkräften, die bis 2037
                    gewonnen werden können: {point.diff_disp}")
  format <- "{value:, f}"

  final_data <- uebersicht_data %>%
    dplyr::select(wirkhebel, diff)

  final_data$color <- ifelse(final_data$wirkhebel == "Gesamteffekt", "#154194", "#b16fab")

  final_data <- final_data[with(final_data, order(diff, decreasing = TRUE)),]
  final_data$diff_disp <- prettyNum(final_data$diff, decimal.mark = ",", big.mark = ".")


  out <- highcharter::hchart(final_data, 'bar', highcharter::hcaes(
    y = !!rlang::sym("diff"),
    x = !!rlang::sym("wirkhebel"),
    color = !!rlang::sym("color")
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
    highcharter::hc_caption(text = quelle,
                            style = list(fontSize = "11px", color = "gray")) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG", "downloadCSV")
   #                                ,
   #                                                 list(
   #                                                   text = "Daten für GPT",
   #                                                   onclick = htmlwidgets::JS(sprintf(
   #                                                     "function () {
   #   var date = new Date().toISOString().slice(0,10);
   #   var chartTitle = '%s'.replace(/\\s+/g, '_');
   #   var filename = chartTitle + '_' + date + '.txt';
   #
   #   var data = 'Titel: %s\\n' + this.getCSV();
   #   data += '\\nQuelle: %s';
   #   var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
   #   if (window.navigator.msSaveBlob) {
   #     window.navigator.msSaveBlob(blob, filename);
   #   } else {
   #     var link = document.createElement('a');
   #     link.href = URL.createObjectURL(blob);
   #     link.download = filename;
   #     link.click();
   #   }
   # }", gsub("'", "\\\\'", titel),
   #     gsub("'", "\\\\'", titel),
   #     gsub("'", "\\\\'", quelle)
   #                                                     )  #
   #                                                   )
                                            # )
                                      # )
                                ))
    )

  return(out)


}


# Funktionen Frauen-Grafiken ----------------------------------------------

argument_frauen_bildungskette <- function(r){

  # load UI inputs from reactive value
  zeit <- 2024
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


  df <- df %>%
    dplyr::left_join(df_alle, by = c("bereich", "indikator", "fachbereich")) %>%
    dplyr::rename(wert = wert.x,
                  wert_ges = wert.y,
                  geschlecht = geschlecht.x) %>%
    dplyr::mutate(prop = round(wert / wert_ges * 100, 1)) %>%
    dplyr::select(-geschlecht.y, -wert_ges)


    df$indikator[df$indikator == "Leistungskurse"] <- "Schüler:innen im Leistungskurs"

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


  df <- df[with(df, order(prop, decreasing = TRUE)), ]


  x <- "indikator"
  y <- "prop"
  group <- "geschlecht"
  tooltip <- "{point.anzeige_geschlecht}Anteil: {point.prop_besr} % <br> Anzahl: {point.wert_besr}"
  stacking <- "percent"
  titel <- paste0("Anteil von Frauen in MINT nach Bildungsbereichen in ", regio, " (", zeit, ")")
  reversed <- FALSE
  format <- "{value}%"

  colors <- c("#154194", "#efe8e6")

  quelle <- "Quellen: Destatis, 2025; Bundesagentur für Arbeit, 2025; KMK, 2025, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt."

  out <- balkenbuilder(df, titel, x, y, group, tooltip, format="{value}%", color = colors, reverse=reversed, stacking = stacking, quelle = quelle)

  return(out)

}

# argument_frauen_gehen <- function(r) {
#
#   # load UI inputs from reactive value
#   t<- 2024
#   indikator_choice <- c("Leistungskurse", "Studierende",
#                         "Auszubildende", "Beschäftigte")
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
#     hcoptslang <- getOption("highcharter.lang")
#     hcoptslang$thousandsSep <- "."
#     options(highcharter.lang = hcoptslang)
#
#     df <- df[with(df, order(indikator, jahr, decreasing = FALSE)), ]
#
#     # Ordnen der Legende
#     sorted_indicators <- df %>%
#       dplyr::group_by(indikator) %>%
#       dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
#       dplyr::arrange(desc(m_value)) %>%
#       dplyr::pull(indikator)
#
#     df$indikator <- factor(df$indikator, levels = sorted_indicators)
#
#     titel <- paste0("Anzahl von Frauen in MINT nach Bildungsbereichen in ", regio)
#     tooltip <- "Anzahl Frauen <br> Indikator: {point.indikator} <br> Anzahl: {point.y} "
#     format <- "{value}"
#
#     color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24" )
#     quelle <- "Quellen: Destatis, 2025; Bundesagentur für Arbeit, 2025; KMK, 2025, alle auf Anfrage, eigene Berechnungen durch MINTvernetzt."
#
#     out <- linebuilder(df,titel,x="jahr", y="wert", group="indikator", tooltip, format, color, quelle = quelle)
#
#   return (out)
# }

argument_großer_unterschied <- function(r) {

    color_fachbereich <- c(
      "Informatik" = "#2D6BE1",
      "Technik (gesamt)" = "#00a87a",
      "Mathematik, Naturwissenschaften" = "#fcc433",
      "andere Berufsfelder" = "#efe8e6"
    )

    timerange <- 2024
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

    quelle <- "Quelle der Daten: Bundesagentur für Arbeit, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    out_1 <- piebuilder(df_f, titel1, x="fachbereich", y = "prop", tooltip, color1, format, subtitel = subtitel1, quelle=quelle)
    out_2 <- piebuilder(df_m, titel2, x="fachbereich", y = "prop", tooltip, color2, format, subtitel = subtitel2, quelle=quelle)


    out <- highcharter::hw_grid(
      out_1, out_2,
      ncol = 2,
      browsable = TRUE
    )


  return(out)
}

argument_selbstkonzept <- function(r){

  # reactive values einlesen

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

    df <- df %>%
      dplyr::filter(typ == "Mittelwert") %>%
      dplyr::select(-typ) %>%
      dplyr::left_join(df_sd, by = c("geschlecht", "fach", "indikator", "jahr"))

    df <- df %>%
      dplyr::mutate(display_rel = prettyNum(round(df$wert,1), big.mark = ".", decimal.mark = ","),
                    display_sd = prettyNum(sd, big.mark = ".", decimal.mark = ","))

     df$geschlecht <- as.factor(df$geschlecht)
     df$geschlecht <- factor(df$geschlecht, levels = c("Mädchen", "Jungen"))

    titel <- paste0("Selbsteinschätzung der eigenen Fähigkeiten in MINT-Fächern
                    von Schüler:innen der 9. Klasse (", jahr_select, ")")
    tooltip_text <- "{point.geschlecht}: {point.display_rel} (SD = {point.display_sd})"
    quelle_text <- "Quelle der Daten: Institut zur Qualitätsentwicklung im Bildungswesen, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."


  # plot

  out <- highcharter::hchart(df, 'column', highcharter::hcaes(y = round(wert, 1), x = fach, group = geschlecht))%>%
    highcharter::hc_tooltip(pointFormat = tooltip_text)%>%
    highcharter::hc_yAxis(title = list(text = "Skalenwert  1 - 4"),
                          labels = list(format = "{value}"),
                          pointsWidth = 4,
                          min = 0) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_colors(c("#154194",
                             "#efe8e6")) %>%
    highcharter::hc_title(text = titel,
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "Calibri Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
    highcharter::hc_caption(text = quelle_text,
                            style = list(fontSize = "11px", color = "gray")) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG", "downloadCSV")
                                  #,
   #                                                 list(
   #                                                   text = "Daten für GPT",
   #                                                   onclick = htmlwidgets::JS(sprintf(
   #                                                     "function () {
   #   var date = new Date().toISOString().slice(0,10);
   #   var chartTitle = '%s'.replace(/\\s+/g, '_');
   #   var filename = chartTitle + '_' + date + '.txt';
   #
   #
   #
   #   var data = 'Titel: %s\\n' + this.getCSV();
   #   data += '\\nQuelle: Institut zur Qualitätsentwicklung im Bildungswesen, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt';
   #
   #
   #   var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
   #   if (window.navigator.msSaveBlob) {
   #     window.navigator.msSaveBlob(blob, filename);
   #   } else {
   #     var link = document.createElement('a');
   #     link.href = URL.createObjectURL(blob);
   #     link.download = filename;
   #     link.click();
   #   }
   # }", gsub("'", "\\\\'", titel),  gsub("'", "\\\\'", titel)    ))))
                                )
                              )
    )

  return(out)

}

argument_faecherverteilung <- function(r){

  # load UI inputs from reactive value
  timerange <- 2024

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


    #df vorbeiten für Plot-Darstellung
      df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
      df <- df[with(df, order(prop, decreasing = FALSE)), ]

      df <- df %>%
        dplyr::mutate(color = color_fach_balken[fach])


  df <- df[with(df, order(prop, decreasing = TRUE)), ]

  df <- df %>%
    dplyr::mutate(color = color_fach_balken[fach])

  col <- df$color
  titel <- ifelse(regio == "Saarland",
                paste0( "Anteil der weiblichen Studierender nach Fachbereich ",regio," (", timerange, ")"),
                  paste0( "Anteil der weiblichen Studierenden nach Fachbereich im ",regio," (", timerange, ")"))


    out <- highcharter::hchart(df, 'bar', highcharter::hcaes(y=prop, x= fach))%>%
      highcharter::hc_tooltip(pointFormat = "{point.region} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}") %>% #Inhalt für Hover-Box
      highcharter::hc_yAxis(title = list(text=""), labels = list(format = "{value}%")) %>% #x-Achse -->Werte in %
      highcharter::hc_xAxis(title= list(text="")
      ) %>%
      highcharter::hc_plotOptions(bar = list(
        colorByPoint = TRUE,
        colors = as.character(df$color)
      )) %>%
      highcharter::hc_title(text = titel,
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_exporting(enabled = TRUE,
                                buttons = list(
                                  contextButton = list(
                                    menuItems = list("downloadPNG", "downloadCSV")
   #                                                     ,
   #                                                   list(
   #                                                     text = "Daten für GPT",
   #                                                     onclick = htmlwidgets::JS(sprintf(
   #                                                       "function () {
   #   var date = new Date().toISOString().slice(0,10);
   #   var chartTitle = '%s'.replace(/\\s+/g, '_');
   #   var filename = chartTitle + '_' + date + '.txt';
   #
   #   var data = 'Titel: %s\\n' + this.getCSV();
   #
   #   var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
   #   if (window.navigator.msSaveBlob) {
   #     window.navigator.msSaveBlob(blob, filename);
   #   } else {
   #     var link = document.createElement('a');
   #     link.href = URL.createObjectURL(blob);
   #     link.download = filename;
   #     link.click();
   #   }
   # }", gsub("'", "\\\\'", titel),  gsub("'", "\\\\'", titel) ))))
                                  )
                                )
      )


  return(out)

}

### argument_wirkhebel wie oben ----




