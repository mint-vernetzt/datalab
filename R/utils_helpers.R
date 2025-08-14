#library(tidyr)
#library(rlang)
#library(tidyr)

#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#'
#'
#'
# gruppe, " im ", regio, " (", timerange, ")

ist_saarland <- function(gruppe="Gruppe", optional=NULL,regio, timerange=0){

  if (timerange != 0){
    if(regio == "Saarland"){
      title = paste0(gruppe, " im ", optional, regio, " (", timerange, ")")
    } else {
      title = paste0(gruppe, " in ", optional, regio, " (", timerange, ")")
    }
  } else {
    if(regio == "Saarland"){
      title = paste0(gruppe, optional, " im ", regio)
    } else {
      title = paste0(gruppe, optional, " in ", regio)
    }
  }
  return(title)
}


ist_saarland2 <- function(indi, regio, optional1= NULL, optional2=NULL){
  if (is.null(optional1) && is.null(optional2)){}
  else{
    if(regio=="Saarland"){
      title = paste0(optional1, indi, optional2, " im ",regio)
    } else {
      title = paste0(optional1, indi, optional2, " in ",regio)
    }

  }
  return(title)
}



#' helpers
#'
#' @description A utils function to make sure that rounding does not cause
#' a value below or above 100
#'
#' @return The return value is a vector of values
#'
#' @noRd
round_preserve_sum <- function(x, digits = 0) {

  up <- 10 ^ digits

  x <- x * up

  y <- floor(x)

  indices <- tail(order(x-y), round(sum(x)) - sum(y))

  y[indices] <- y[indices] + 1

  y <- y / up

  return(y)
}


#' helpers
#'
#' @description A utils list which contains the hex codes of colors from the
#' design guide
#'
#' @noRd

colors_mint_vernetzt <- list(general = c("#154194", "#b16fab", "#00a87a"),
                             attention = c("#00a87a", "#fcc433", "#ee7775"),
                             short = c("#154194", "#b16fab")
                             #neutral = c("#141416", "#e6e8ec"),
                             #gender = c("#f5adac", "#b1b5c3"))
)


#' helpers
#'
#' @description A utils list which contains the all states of the former west and
#' east of germany
#'
#' @noRd
states_east_west <- list(west = c("Baden-Württemberg", "Bayern", "Bremen", "Hamburg",
                                  "Hessen", "Niedersachsen", "Nordrhein-Westfalen",
                                  "Rheinland-Pfalz", "Saarland", "Schleswig-Holstein"),
                         east = c("Brandenburg", "Mecklenburg-Vorpommern", "Sachsen",
                                  "Sachsen-Anhalt", "Thüringen", "Berlin"))








# funktion zur ordnung der fachauswahl für studierende_detailliert
studi_det_ui_faecher <-function(spezif_i, spezif_r){

  require(magrittr)

  if(missing(spezif_i)&missing(spezif_r)){

    # df1 <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    #   dplyr::filter(mint_select == "MINT"  | fach %in% c("Alle MINT-Fächer", "Alle Nicht MINT-Fächer")) %>%
    #   dplyr::select(fach)%>%
    #   dplyr::collect()
    #
    # df1 <- df1 %>%
    #   unique()%>%
    #   as.vector()%>%
    #   unlist()%>%
    #   unname()
    #
    # df1 <- sort(df1)

    df1_query <- glue::glue_sql("
  SELECT DISTINCT fach
  FROM studierende_detailliert
  WHERE mint_select = 'MINT'
     OR fach IN ('Alle MINT-Fächer', 'Alle Nicht MINT-Fächer')
  ORDER BY fach
", .con = con)

    df1 <- DBI::dbGetQuery(con, df1_query)

  } else if (missing(spezif_i)){

    # df1 <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    #   dplyr::filter(mint_select == "MINT"  | fach %in% c("Alle MINT-Fächer", "Alle Nicht MINT-Fächer"))%>%
    #   dplyr::filter(region %in%  spezif_r) %>%
    #   dplyr::collect()
    #
    # df1 <- df1 %>%dplyr::select(fach)%>%
    #   unique()%>%
    #   as.vector()%>%
    #   unlist()%>%
    #   unname()
    #
    # df1 <- sort(df1)

    df1_query <- glue::glue_sql("
  SELECT DISTINCT fach
  FROM studierende_detailliert
  WHERE (mint_select = 'MINT' OR fach IN ('Alle MINT-Fächer', 'Alle Nicht MINT-Fächer'))
    AND region IN ({spezif_r*})
  ORDER BY fach
", .con = con)

    df1 <- DBI::dbGetQuery(con, df1_query) %>%
      dplyr::pull(fach)

  } else if(missing(spezif_r)){
#
#     df1 <- dplyr::tbl(con, from = "studierende_detailliert") %>%
#       dplyr::filter(mint_select == "MINT"  | fach %in% c("Alle MINT-Fächer", "Alle Nicht MINT-Fächer"))%>%
#       dplyr::filter(indikator %in%  spezif_i) %>%
#       dplyr::collect()
#
#     df1 <- df1 %>%dplyr::select(fach)%>%
#       unique()%>%
#       as.vector()%>%
#       unlist()%>%
#       unname()
#
#     df1 <- sort(df1)

    df1_query <- glue::glue_sql("
  SELECT DISTINCT fach
  FROM studierende_detailliert
  WHERE (mint_select = 'MINT' OR fach IN ('Alle MINT-Fächer', 'Alle Nicht MINT-Fächer'))
    AND indikator IN ({spezif_i*})
  ORDER BY fach
", .con = con)

    df1 <- DBI::dbGetQuery(con, df1_query) %>%
      dplyr::pull(fach)


  }
}


# Funktion zur Fachauswahl bei international Daten
international_ui_faecher <- function(region = "EU") {

  selection <- NULL
  if (region == "OECD") {

    selection <- c("MINT",
                   "--- Naturwissenschaften, Mathematik und Statistik" = "Naturwissenschaften, Mathematik und Statistik",
                   "--- Informatik & Kommunikationstechnologie" = "Informatik & Kommunikationstechnologie",
                   "--- Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe" = "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                   "Pädagogik",
                   "Geisteswissenschaften und Künste",
                   "Sozialwissenschaften, Journalismus und Informationswesen",
                   "Wirtschaft, Verwaltung und Recht",
                   "Dienstleistungen",
                   "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin")
  }

  if (region == "EU") {

    selection <- c("Alle MINT-Fächer",
                   "--- Naturwissenschaften, Mathematik und Statistik" = "Naturwissenschaften, Mathematik und Statistik",
                   "--- Informatik & Kommunikationstechnologie" = "Informatik & Kommunikationstechnologie",
                   "--- Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe" = "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe"
    )
  }

  # for arbeitsmarkt international
  if (region == "arbeit") {
    #load(file = system.file(package="datalab","data/schule_timss.rda"))

    # selection <- dplyr::tbl(con, from = "arbeitsmarkt_anfaenger_absolv_oecd") %>%
    #   dplyr::filter(geschlecht == "Gesamt" &
    #                   variable %in% c("Anteil Absolvent*innen nach Fach an allen Fächern",
    #                                   "Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern")
    #   ) %>%
    #   dplyr::pull(fachbereich) %>%
    #   unique() %>%
    #   sort()
    df_query <- glue::glue_sql("
  SELECT DISTINCT land, fachbereich,
  FROM arbeitsmarkt_anfaenger_absolv_oecd
  WHERE geschlecht = 'Gesamt'
  AND variable IN ('Anteil Absolvent*innen nach Fach an allen Fächern', 'Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern')
", .con = con)


    selection <- DBI::dbGetQuery(con, df_query)

    # selection <- selection %>%
    #   dplyr::pull(fachbereich) %>%
    #   unique() %>%
    #   sort()

      }



  return(selection)

}

# Funktion zur Jahresauswahl bei internationalen Daten
international_ui_years <- function(region = "EU") {

  selection <- NULL

  # for studium international
  if (region == "OECD") {

    df_query <- glue::glue_sql("
  SELECT jahr
  FROM studierende_anzahl_oecd
  WHERE geschlecht = 'Gesamt'
", .con = con)

    selection <- DBI::dbGetQuery(con, df_query)

    selection <- selection %>%
      dplyr::pull(jahr) %>%
      unique() %>%
      sort()


  }

  if (region == "EU") {

    # selection <- dplyr::tbl(con, from = "studierende_europa") %>%
    #   dplyr::filter(geschlecht == "Gesamt"  &
    #                   mint_select == "mint" &
    #                   indikator == "Fächerwahl") %>%
    #   dplyr::pull(jahr) %>%
    #   unique() %>%
    #   sort()
    # selection<- selection[-1]


    jahr_query <- glue::glue_sql("
  SELECT DISTINCT jahr
  FROM studierende_europa
  WHERE geschlecht = 'Gesamt'
    AND mint_select = 'mint'
    AND indikator = 'Fächerwahl'
  ORDER BY jahr
", .con = con)

    selection <- DBI::dbGetQuery(con, jahr_query) %>%
      dplyr::pull(jahr)

    # Entferne das erste Element
    selection <- selection[-1]

    #
  }

  if (region == "Weltweit"){

    df_query <- glue::glue_sql("
  SELECT *
  FROM studierende_absolventen_weltweit
  WHERE geschlecht = 'Insgesamt'
  AND jahr != '2022'
", .con = con)

    selection <- DBI::dbGetQuery(con, df_query)

    selection <- selection %>%
      dplyr::pull(jahr) %>%
      unique() %>%
      sort()

  }

  # for schule international
  if (region == "TIMSS") {

    df_query <- glue::glue_sql("
  SELECT *
  FROM schule_timss
  WHERE ordnung IN ('Achievement', 'Benchmarks')
  AND indikator IN ('Mittlerer int''l. Maßstab (475)', 'Insgesamt')
", .con = con)

    selection <- DBI::dbGetQuery(con, df_query)

    selection <- selection %>%
      dplyr::pull(jahr) %>%
      unique() %>%
      sort()


  }

  if (region == "PISA") {

df_query <- glue::glue_sql("
  SELECT *
  FROM schule_pisa
  WHERE bereich = 'Ländermittel'
  AND indikator = 'Insgesamt'
", .con = con)

    selection <- DBI::dbGetQuery(con, df_query)

    selection <- selection %>%
      dplyr::filter(!is.na(wert))%>%
      dplyr::pull(jahr) %>%
      unique() %>%
      sort()


  }

  # for arbeitsmarkt international
  if (region == "arbeit") {


    # selection <- dplyr::tbl(con, from = "arbeitsmarkt_anfaenger_absolv_oecd") %>%
    #   dplyr::filter(
    #     geschlecht == "Gesamt" &
    #       jahr >= 2013 &
    #       variable %in% c("Anteil Absolvent*innen nach Fach an allen Fächern",
    #                       "Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern")
    #
    #   ) %>%
    #   dplyr::pull(jahr) %>%
    #   unique() %>%
    #   sort()


    jahr_query <- glue::glue_sql("
  SELECT DISTINCT jahr
  FROM arbeitsmarkt_anfaenger_absolv_oecd
  WHERE geschlecht = 'Gesamt'
    AND jahr >= 2013
    AND variable IN (
      'Anteil Absolvent*innen nach Fach an allen Fächern',
      'Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern'
    )
  ORDER BY jahr
", .con = con)

    selection <- DBI::dbGetQuery(con, jahr_query) %>%
      dplyr::pull(jahr)



  }

  return(selection)
}

# Funktion zur Länderauswahl bei internationalen Daten
international_ui_country <- function(type = "arbeit", n = NA) {

  selection <- NULL

  df_query <- glue::glue_sql("
  SELECT DISTINCT jahr
  FROM arbeitsmarkt_anfaenger_absolv_oecd
  WHERE geschlecht = 'Gesamt'
", .con = con)

  for_year <- DBI::dbGetQuery(con, df_query)

  year <- max(for_year$jahr)

  # for studium international
  if (type == "arbeit") {


    df_query <- glue::glue_sql("
  SELECT *
  FROM arbeitsmarkt_anfaenger_absolv_oecd
  WHERE geschlecht = 'Gesamt'
  AND jahr = {year}
  AND variable IN ('Anteil Absolvent*innen nach Fach an allen Fächern','Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern')
", .con = con)

    tmp_df <- DBI::dbGetQuery(con, df_query)



    if (!is.na(n)) {
      tmp_df <- tmp_df %>%
        dplyr::filter(
          fachbereich == "MINT" &
            variable == "Anteil Absolvent*innen nach Fach an allen Fächern") %>%
        dplyr::group_by(land) %>%
        dplyr::summarise(wert = sum(wert)) %>%
        dplyr::arrange(desc(wert)) %>%
        head(n = 10)
    }

    selection <- tmp_df %>%
      dplyr::pull(land) %>%
      unique() %>%
      sort()
  }

}

int_schule_ui_country <- function(type = "TIMSS", n = NULL) {


  selection <- NULL

  df_query <- glue::glue_sql("
  SELECT land, jahr
  FROM arbeitsmarkt_anfaenger_absolv_oecd
  WHERE geschlecht = 'Gesamt'
", .con = con)

  for_year <- DBI::dbGetQuery(con, df_query)


  year <- max(for_year$jahr)

  # for studium international
  if (type == "arbeit") {



    df_query <- glue::glue_sql("
  SELECT *
  FROM arbeitsmarkt_anfaenger_absolv_oecd
  WHERE geschlecht = 'Gesamt'
  AND jahr = {year}
  AND variable IN ('Anteil Absolvent*innen nach Fach an allen Fächern','Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern')
", .con = con)

    tmp_df <- DBI::dbGetQuery(con, df_query)

    if (!is.na(n)) {
      tmp_df <- tmp_df %>%
        dplyr::filter(
          fachbereich == "MINT" &
            variable == "Anteil Absolvent*innen nach Fach an allen Fächern") %>%
        dplyr::group_by(land) %>%
        dplyr::summarise(wert = sum(wert)) %>%
        dplyr::arrange(desc(wert)) %>%
        head(n = 10)
    }

    selection <- tmp_df %>%
      dplyr::pull(land) %>%
      unique() %>%
      sort()
  }

  if(type=="TIMSS" && is.null(n)){

    df_query <- glue::glue_sql("
    SELECT DISTINCT land FROM schule_timss
WHERE ordnung = 'Ressourcen' AND wert IS NOT NULL
", .con = con)

    selection <- DBI::dbGetQuery(con, df_query)


#     df_query <- glue::glue_sql("
#   SELECT *
#   FROM schule_timss
# ", .con = con)
#
#     selection <- DBI::dbGetQuery(con, df_query)
#
#
#
#     selection <- selection %>%
#       group_by(land) %>%
#       filter(
#         # Behalte nur Länder,
#         # die mindestens eine Zeile mit ordnung == "Ressourcen" und NICHT NA in wert haben
#         any(ordnung == "Ressourcen" & !is.na(wert))
#       ) %>%
#       ungroup() %>%
#     # browser()
#     # # %>%
#       dplyr::filter(!is.na(wert)) %>%
#       dplyr::distinct(land) %>%  #
#       dplyr::arrange(land) %>%   #
#       dplyr::pull(land)

  }

}

int_pisa_ui_country <- function(type = "TIMSS", n = NULL) {

  selection <- NULL



  df_query <- glue::glue_sql("
  SELECT land, jahr
  FROM arbeitsmarkt_anfaenger_absolv_oecd
  WHERE geschlecht = 'Gesamt'
", .con = con)

  for_year <- DBI::dbGetQuery(con, df_query)


  year <- max(for_year$jahr)

  # for studium international
  if (type == "arbeit") {


    df_query <- glue::glue_sql("
  SELECT *
  FROM arbeitsmarkt_anfaenger_absolv_oecd
  WHERE geschlecht = 'Gesamt'
  AND variable IN ('Anteil Absolvent*innen nach Fach an allen Fächern','Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern')
", .con = con)

    tmp_df <- DBI::dbGetQuery(con, df_query)

    tmp_df <- tmp_df %>%
      dplyr::collect()



    if (!is.na(n)) {
      tmp_df <- tmp_df %>%
        dplyr::filter(
          fachbereich == "MINT" &
            variable == "Anteil Absolvent*innen nach Fach an allen Fächern") %>%
        dplyr::group_by(land) %>%
        dplyr::summarise(wert = sum(wert)) %>%
        dplyr::arrange(desc(wert)) %>%
        head(n = 10)
    }

    selection <- tmp_df %>%
      dplyr::pull(land) %>%
      unique() %>%
      sort()
  }

  if(type == "PISA"  && is.null(n)){


    selection <- DBI::dbGetQuery(con,
                                 "SELECT DISTINCT land
                                 FROM schule_pisa")$land



  }

  if(type == "PISA" && !is.null(n)){



    selection <- DBI::dbGetQuery(con,
                                 "SELECT DISTINCT land
                                 FROM schule_pisa")

    laender_ausgeschlossen <- c("Luxemburg", "Japan", "Großbritannien")

    selection <- selection %>%
      dplyr::filter(!land %in% laender_ausgeschlossen) %>%
      dplyr::pull(land)

  }

}

# Funktion zur Jahresauswahl bei Fachkraft Daten
fachkraft_ui_years <- function(reg = "DE") {


  selection <- NULL

if(reg == "DE"){

  df_query <- glue::glue_sql("
  SELECT DISTINCT jahr
  FROM arbeitsmarkt_epa_detail
  WHERE indikator = 'Engpassindikator'
", .con = con)

  selection <- DBI::dbGetQuery(con, df_query)

  selection <- selection %>%
    dplyr::pull(jahr) %>%
    unique() %>%
    sort()

  # selection <- dplyr::tbl(con, from = "arbeitsmarkt_epa_detail") %>%
  #   dplyr::filter(indikator == "Engpassindikator") %>%
  #   dplyr::pull(jahr) %>%
  #   unique() %>%
  #   sort()

}else if(reg== "BULA"){
##############################################################
  df_query <- glue::glue_sql("
  SELECT DISTINCT jahr
  FROM arbeitsmarkt_epa
  WHERE indikator = 'Engpassindikator'
", .con = con)

  selection <- DBI::dbGetQuery(con, df_query)

  selection <- selection %>%
    dplyr::pull(jahr) %>%
    unique() %>%
    sort()

  # selection <- dplyr::tbl(con, from = "arbeitsmarkt_epa") %>%
  #   dplyr::filter(indikator == "Engpassindikator") %>%
  #   dplyr::pull(jahr) %>%
  #   unique() %>%
  #   sort()

}



  return(selection)
}


# Funktion zur Fachauswahl bei Fachkraft Daten
fachkraft_ui_faecher <- function(exclude = c()) {


  selection <- NULL
  selection <- c(
    "MINT gesamt", #"MINT",
    "Mathematik, Naturwissenschaften",
    "Informatik",
    "Technik gesamt",
    "Landtechnik",
    "Produktionstechnik",
    "Bau- und Gebäudetechnik",
    "Verkehrs-, Sicherheits- und Veranstaltungstechnik",
    "Gesundheitstechnik",
    "Alle Berufe" ="Gesamt",
    "Nicht MINT"
  )

  selection <- setdiff(selection, exclude)


  return(selection)

}

# Funktion zur Berufslevelauswahl bei Fachkraft Daten
fachkraft_ui_berufslevel <- function() {


  selection <- NULL
  selection <- c(
    "Gesamt",
    "Fachkräfte",
    "Spezialist*innen",
    "Expert*innen"
  )


  return(selection)
}

fachkraft_ui_berufe <- function(level = "Fachkräfte", zeitpunkt = 2023) {


  selection <- NULL

  df_query <- glue::glue_sql("
  SELECT beruf, wert
  FROM arbeitsmarkt_epa_detail
  WHERE indikator = 'Engpassindikator'
  AND anforderung = {level}
  AND jahr = {zeitpunkt}
", .con = con)

  selection <- DBI::dbGetQuery(con, df_query)

  selection <- selection %>%
    dplyr::filter(!is.na(wert)) %>%
    dplyr::pull(beruf) %>%
    unique()

  # selection <- dplyr::tbl(con, from = "arbeitsmarkt_epa_detail") %>%
  #   dplyr::filter(indikator == "Engpassindikator" &
  #                   anforderung == level &
  #                   jahr == zeitpunkt &
  #                   !is.na(wert)) %>%
  #   dplyr::pull(beruf) %>%
  #   unique()


  return(selection)
}

fachkraft_ui_wirkhebel <- function(version = "lang") {

  selection <- NULL

  if(version == "lang"){

    selection <- c(
      "Gesamteffekt",
      "MINT-Nachwuchs fördern" = "MINT-Bildung",
      "Mädchen und Frauen in MINT fördern" = "Frauen in MINT",
      "Zuwanderung MINT-Fachkräfte" = "Internationale MINT-Fachkräfte",
      "Verbleib älterer MINT-Fachkräfte" = "Beteiligung älterer MINT-Fachkräfte"
    )
  }else if (version == "kurz"){
    selection <- c(
      "MINT-Nachwuchs fördern" = "MINT-Bildung",
      "Mädchen und Frauen in MINT fördern" = "Frauen in MINT",
      "Zuwanderung MINT-Fachkräfte" = "Internationale MINT-Fachkräfte",
      "Verbleib älterer MINT-Fachkräfte" = "Beteiligung älterer MINT-Fachkräfte"
    )
  }
  return(selection)
}

fachkraft_ui_scenario <- function(wirkhebel) {

  selection <- NULL

  if(wirkhebel %in% c("MINT-Bildung", "Gesamteffekt")){
    selection <- c(
      "positives Szenario" = "Verbesserung",
      "negatives Szenario" = "Verschlechterung"
    )
  } else if(wirkhebel == "Internationale MINT-Fachkräfte"){
    selection <- c(
      "positives Szenario" = "Verbesserung",
      "Rückgang im Positivtrend der Zuwanderung" = "Verschlechterung",
      "vollständiger Stillstand der Zuwanderung" = "Stillstand"


    )
  } else if(wirkhebel == "Beteiligung älterer MINT-Fachkräfte"){
    selection <- c("positives Szenario" = "Verbesserung")

  } else if(wirkhebel == "Frauen in MINT"){
    selection <- c(
      "positives Szenario" = "Verbesserung",
      "kombiniertes Szenario" = "starke Verbesserung"
    )
  } else if(wirkhebel == "Basis-Szenario"){
    selection <- c(
      "Fortschreibung aktueller Lage" = "Status-quo"
    )
  }

  return(selection)
}




# function to extract a plot title from a highcharter object
get_plot_title <- function(plot, path = ".") {

  shiny::req(highcharter::is.highchart(plot))

  out <- file.path(
    path,
    paste0(
      ifelse(
        is.null(plot$x$hc_opts$title$text),
        "MINTvernetzt_PLOT",
        plot$x$hc_opts$title$text),
      "_", format(Sys.time(), "%Y%M%d_%H%M"),
      ".png")
  )
  return(out)
}

# Funktion zur Jahreswahl bei Arbeit-Fachkraft Daten
arbeit_fachkraft_ui_years <- function() {

  selection <- NULL

  df_query <- glue::glue_sql("
  SELECT DISTINCT jahr
  FROM arbeitsmarkt_fachkraefte
  WHERE indikator IN ('Abgeschlossene Vakanzzeit', 'Arbeitslosen-Stellen-Relation')
", .con = con)
#####################################################

  selection <- DBI::dbGetQuery(con, df_query)

  selection <- selection %>%
    dplyr::pull(jahr) %>%
    unique() %>%
    sort()



  # selection <- dplyr::tbl(con, from = "arbeitsmarkt_fachkraefte") %>%
  #   dplyr::filter(indikator %in% c("Abgeschlossene Vakanzzeit",
  #                                  "Arbeitslosen-Stellen-Relation")) %>%
  #   dplyr::pull(jahr) %>%
  #   unique() %>%
  #   sort()


  return(selection)
}

# Funktion zur Region bei Arbeit-Fachkraft Daten
arbeit_fachkraft_ui_region <- function() {

  selection <- NULL
##########################################################
  df_query <- glue::glue_sql("
  SELECT DISTINCT region
  FROM arbeitsmarkt_fachkraefte
  WHERE indikator IN ('Abgeschlossene Vakanzzeit', 'Arbeitslosen-Stellen-Relation')
", .con = con)


  selection <- DBI::dbGetQuery(con, df_query)

  selection <- selection %>%
    dplyr::pull(region) %>%
    unique()

  # selection <- dplyr::tbl(con, from = "arbeitsmarkt_fachkraefte") %>%
  #   dplyr::filter(indikator %in% c("Abgeschlossene Vakanzzeit",
  #                                  "Arbeitslosen-Stellen-Relation")) %>%
  #   dplyr::pull(region) %>%
  #   unique()


  return(selection)
}

# function to download plots with added cption and logo
add_caption_and_download <- function(
    hc,
    filename = "plot.png",
    labelformat = '{point.y}',
    with_labels = TRUE
    ,
    width = 450,
    height = 300
    ) {

  require(highcharter)
  require(webshot2)
  require(htmlwidgets)


  ## roll back webshot2

  # remove.packages("webshot2")
  # packageurl <- "https://cran.r-project.org/src/contrib/Archive/webshot2/webshot2_0.1.0.tar.gz"
  # install.packages(packageurl, repos=NULL, type="source")
  #
  # packageVersion("webshot2")
  #

  # set chromote, determine chromium variant
  # Sys.setenv(
  #   CHROMOTE_CHROME = "C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe"
  # )

  # force the use of pagedown to install chrome on shinyapps.io (this is a workaround)
  require(pagedown)
  # force the use of curl because chromote needs it (see https://github.com/rstudio/chromote/issues/37)
  require(curl)

  # hc <- plot
  shiny::req(highcharter::is.highchart(hc))

  shiny::showNotification(ui = "Plot wird gespeichert...",
                          type = "message",
                          duration = NULL,
                          id = "download_notification")
#browser()
  hc_out <- hc %>%
    # Add the caption to the plot
    highcharter::hc_size(width = width, height = height) %>%
    highcharter::hc_caption(text = paste0(
      '<div style="width: ',width - 10, 'px;',
      ' display: flex; justify-content: space-between;">',
      '<span>',
      # '<span style="font-size: 10px;">', # max-width: ', width - 50, 'px;
      'Quellen: Statistisches Bundesamt, 2022; Bundesagentur für Arbeit, 2022;',
      ' KMK, 2022, alle auf Anfrage,<br>',
      ' Eigene Berechnungen durch MINTvernetzt</span>',
      '<span>',
      '<span style="padding-right: 10px;">',
      #'<img src="https://mint-vernetzt.de/static/e99e5a7a75c99c8651863585408242bb/mintvernetzt_og-img.png"',
      '<img src="https://raw.githubusercontent.com/mint-vernetzt/datalab/main/inst/app/www/MINTvernetztLogo_klein.png"',
      #'<img src="www/MINTvernetztLogo_klein.png"',
      'alt="MINT vernetzt Logo" width="30" height="30">',
      '</span>',
      '</div>'
    ),
    useHTML = TRUE,
    align = "right") %>%
    # TODO add correct font
    highcharter::hc_title(
      # only overwrite needed values
      style = list(fontFamily = "Calibri")
    ) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "Calibri")
    )

  if (with_labels) {
    hc_out <- hc_out %>%
      highcharter::hc_plotOptions(
        series = list(
          # Add value labels
          dataLabels = list(
            enabled = TRUE,
            format = paste(labelformat)
          )
        )
      )
  }

  #browser()
  #print(hc_out)
  # Save the plot as a standalone HTML file
  html_file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(hc_out, file = html_file, selfcontained = TRUE)
  print(html_file)
  # # Capture the HTML as a PNG image
  webshot2::webshot(url = html_file,
                    file = filename
                    ,
                    delay = 2,
                    zoom = 2,
                    vwidth = width,
                    vheight = height
                    )

  shiny::showNotification(
    ui = paste0("Gespeichert als '", filename, "'"),
    type = "message",
    id = "download_notification")

  return(NULL)
}




#' preprocess_beruf on landkreis level
#'
#' @description Function calculates the shares on landkreis level
#'
#' @return a dataframe.
#'
#' @noRd
calculate_landkreis <- function(df, states, category, domain, indikator_azubi, indikator_besch, region = "") {

  # filter dataset based on UI inputs
  df_filtered <- df %>% dplyr::filter(bundesland == states,
                                      anforderung == "Gesamt",
                                      kategorie == category) # dropdown 1 - Azubis oder Beschäftigte

  # dropdown 2 auf Gesamt --> kein Fachbereich ausgewählt, nur Indikator
  if (domain == "Alle") {
    df_gesamt <- df_filtered %>% dplyr::filter(fachbereich == "Alle",
                                               indikator == category,
                                               geschlecht == "Gesamt")
    #titel_gesamt_1 <- paste0(" an allen ")
    titel_gesamt_1 <- ifelse(domain == "Alle", " an allen ", paste0(" in ", domain, " an allen "))


  } else {
    # dropdown 2 nicht auf Gesamt

    # dropdown 3 auf Gesamt --> nach folgendem filter selbes wie drüber
    if ((category == indikator_besch) |
        (category == indikator_azubi)) {
      df_gesamt <- df_filtered %>% dplyr::filter(fachbereich == "Alle",
                                                 indikator == category,
                                                 geschlecht == "Gesamt")

      # titel_gesamt_1 <- paste0(" an allen ")
      titel_gesamt_1 <- ifelse(domain == "Alle", " an allen ", paste0(" in ", domain, " an allen "))


    } else {
      # dropdown 3 nicht auf Gesamt --> Fachbereich und Indikator ausgewählt

      df_gesamt <- df_filtered %>% dplyr::filter(fachbereich == domain,
                                                 indikator == category,
                                                 geschlecht == "Gesamt")

      # titel_gesamt_1 <- paste0(" in ", domain, " an allen ")
      titel_gesamt_1 <- ifelse(domain == "Alle", " an allen ", paste0(" in ", domain, " an allen "))

    }

  }

  df_sub <- df_filtered %>% dplyr::filter(fachbereich == domain)

  # dropdown 3
  if(category == "Beschäftigte"){

    titel_sub <- indikator_besch

    if(indikator_besch != "Frauen"){

      df_sub <- df_sub %>% dplyr::filter(indikator == indikator_besch,
                                         geschlecht == "Gesamt")

      titel_sub <- paste0(indikator_besch)
      titel_sub <- ifelse(grepl("ausl", indikator_besch), "ausländischer Beschäftigter", titel_sub)
      titel_sub <- ifelse(grepl("u25", indikator_besch), "Beschäftigter unter 25 Jahren", titel_sub)
      titel_sub <- ifelse(grepl("25-55", indikator_besch), "Beschäftigter zwischen 25 und 55 Jahren", titel_sub)
      titel_sub <- ifelse(grepl("ü55", indikator_besch), "Beschäftigter über 55 Jahren", titel_sub)
      titel_sub2 <- paste0(indikator_besch, "n")
      titel_sub2 <- ifelse(grepl("ausl", indikator_besch), "ausländischen Beschäftigten", titel_sub2)
      titel_sub2 <- ifelse(grepl("u25", indikator_besch), "Beschäftigten unter 25 Jahren", titel_sub2)
      titel_sub2 <- ifelse(grepl("25-55", indikator_besch), "Beschäftigten zwischen 25 und 55 Jahren", titel_sub2)
      titel_sub2 <- ifelse(grepl("ü55", indikator_besch), "Beschäftigten über 55 Jahren", titel_sub2)
      # titel_gesamt_1 <- paste0(" in ", domain, " an allen ")
      titel_gesamt_1 <- ifelse(domain == "Alle", " an allen ", paste0(" in ", domain, " an allen "))
      titel_gesamt_2 <- ifelse(titel_sub %in% c("ausländischer Beschäftigter",
                                                "Beschäftigter unter 25 Jahren",
                                                "Beschäftigter zwischen 25 und 55 Jahren",
                                                "Beschäftigter über 55 Jahren"), paste0("Beschäftigten in ", domain), "Beschäftigten")
      titel_gesamt_2 <- ifelse(domain == "Alle", "Beschäftigten in allen Berufsbereichen", titel_gesamt_2)


    } else if(indikator_besch == "Frauen"){

      df_sub <- df_sub %>% dplyr::filter(indikator == category,
                                         geschlecht == indikator_besch)

      titel_sub <- paste0(" weiblicher ", category, "r")
      titel_sub2 <- paste0(" weiblichen ", category, "n")
      # titel_gesamt_1 <- paste0(" in ", domain, " an allen ")
      titel_gesamt_1 <- ifelse(domain == "Alle", " an allen ", paste0(" in ", domain, " an allen "))
      titel_gesamt_2 <- paste0("Beschäftigten in ", domain)
      titel_gesamt_2 <- ifelse(domain == "Alle", "Beschäftigten in allen Berufsbereichen", titel_gesamt_2)


    }

  } else if(category == "Auszubildende"){

    titel_sub <- indikator_azubi

    if(indikator_azubi != "Frauen"){

      df_sub <- df_sub %>% dplyr::filter(indikator == indikator_azubi,
                                         geschlecht == "Gesamt")

      titel_sub <- paste0(indikator_azubi)
      titel_sub <- ifelse(grepl("ausl", indikator_azubi), "ausländischer Auszubildender", titel_sub)
      titel_sub <- ifelse(grepl("(1. Jahr)", indikator_azubi), "Auszubildender mit neuem Lehrvertrag", titel_sub)
      titel_sub2 <- paste0(indikator_azubi, "n")
      titel_sub2 <- ifelse(grepl("ausl", indikator_azubi), "ausländischen Auszubildenden", titel_sub2)
      titel_sub2 <- ifelse(grepl("(1. Jahr)", indikator_azubi), "Auszubildenden mit neuem Lehrvertrag", titel_sub2)
      # titel_gesamt_1 <- paste0(" in ", domain, " an allen ")
      titel_gesamt_1 <- ifelse(domain == "Alle", " an allen ", paste0(" in ", domain, " an allen "))
      titel_gesamt_2 <- ifelse(titel_sub %in% c("ausländischer Auszubildender",
                                                "Auszubildender mit neuem Lehrvertrag"), paste0("Auszubildenden in ", domain), "Auszubildenden")
      titel_gesamt_2 <- ifelse(domain == "Alle", "Auszubildende in allen Berufsbereichen", titel_gesamt_2)



    } else if(indikator_azubi == "Frauen"){

      df_sub <- df_sub %>% dplyr::filter(indikator == category,
                                         geschlecht == indikator_azubi)
      titel_sub <- paste0(" weiblicher ", category, "r")
      titel_sub2 <- paste0(" weiblichen ", category, "n")
      # titel_gesamt_1 <- paste0(" in ", domain, " an allen ")
      titel_gesamt_1 <- ifelse(domain == "Alle", " an allen ", paste0(" in ", domain, " an allen "))
      titel_gesamt_2 <- paste0("Auszubildenden in ", domain)
      titel_gesamt_2 <- ifelse(domain == "Alle", "Auszubildende in allen Berufsbereichen", titel_gesamt_2)
    }
  }

  # merge dataframes and compute prob
  df_compare <- df_sub %>%
    dplyr::left_join(df_gesamt,
                     by = c(
                       "kategorie",
                       "bundesland",
                       "landkreis",
                       "landkreis_nummer",
                       "jahr",
                       "anforderung"))

  if(region == "Gesamt"){
    df_compare <- df_compare %>%
      dplyr::group_by(bereich,
                      kategorie,
                      bundesland,
                      jahr,
                      anforderung) %>%
      dplyr::summarise(wert.x = sum(wert.x),
                       wert.y = sum(wert.y)) %>%
      dplyr::mutate(prob = round((wert.x/wert.y)*100,1)) %>%
      dplyr::rename(wert = wert.x) %>%
      dplyr::select(-wert.y) %>%
      dplyr::ungroup()

  } else {
    df_compare <- df_compare %>%
      dplyr::mutate(prob = round((wert.x/wert.y)*100,1)) %>%
      dplyr::rename(wert = wert.x,
                    geschlecht = geschlecht.x) %>%
      dplyr::select(-c(wert.y, geschlecht.y))

  }

  # return relevant values as a list

  return_list <- list()
  return_list[[1]] <- df_compare
  return_list[[2]] <- titel_gesamt_1
  return_list[[3]] <- titel_gesamt_2
  return_list[[4]] <- titel_sub
  return_list[[5]] <- titel_sub2

  return(return_list)
}



get_lks <- function(bula = "Sachsen"){

  lks <- DBI::dbGetQuery(con,
                         paste0("SELECT DISTINCT landkreis
                       FROM arbeitsmarkt_detail
                       WHERE bundesland = '", bula, "'"))$landkreis
  lks <- setdiff(lks, "alle Landkreise")
  lks <- c("Landesdurchschnitt", lks)

  return(lks)
}


#zusammenfasser
darstellung <- function(id, title = NULL) {
  tagList(
    shinyBS::bsPopover(
      id = id,
      title = title,
      content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
      trigger = "hover"
    ),
    tags$a(
      "Probleme bei der Darstellung",
      icon("question-circle"),
      id = id
    ),
  )
}



# test des funktionszusammenfasser


piebuilder <- function(df, titel, x, y, tooltip, color = c("#b16fab", "#efe8e6"), format = '{point.prop_besr}%', subtitel = NULL, quelle="Quelle"){

  if (is.null(subtitel)){
    out <- highcharter::hchart(df, size = 280, type = "pie", mapping = highcharter::hcaes(x = !!rlang::sym(x), y = !!rlang::sym(y)), name = "value") %>%
      highcharter::hc_tooltip(
        pointFormat=tooltip) %>%
      highcharter::hc_colors(color) %>%
      highcharter::hc_title(text = titel,
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>% #Calibri Regular
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular", fontSize = "14px")) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
      highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                             dataLabels = list(enabled = TRUE, format = format ), showInLegend = TRUE)) %>%
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

   }",
                                                         gsub("'", "\\\\'", titel),
                                                         gsub("'", "\\\\'", titel),
                                                         gsub("'", "\\\\'", quelle)
                                                       ))

                                                     )
                                                     )
                                  )
                                )
      )
  } else {
    out <- highcharter::hchart(df, size = 280, type = "pie", mapping = highcharter::hcaes(x = !!rlang::sym(x), y = !!rlang::sym(y)), name = "value") %>%
      highcharter::hc_tooltip(
        pointFormat=tooltip) %>%
      highcharter::hc_colors(color) %>%
      highcharter::hc_title(text = titel,
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
      highcharter::hc_subtitle(text = subtitel,
                               style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "16px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular", fontSize = "14px")) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
      highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                             dataLabels = list(enabled = TRUE, format = format ), showInLegend = TRUE)) %>%
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

   }",
                                                         gsub("'", "\\\\'", titel),
                                                         gsub("'", "\\\\'", titel),
                                                         gsub("'", "\\\\'", quelle)
                                                       ))

                                                     )
                                                     )
                                  )
                                )
      )
  }

  return(out)
}




#df, titel, x, y, tooltip

linebuilder <- function(df, titel, x , y, group = NULL, tooltip, format, color = c("#b16fab", "#154194","#66cbaf", "#fbbf24"), quelle="Quelle"){

  df <- df %>%
    dplyr::mutate(!!y := round(!!rlang::sym(y), 1))

  out <- highcharter::hchart(df, 'line', highcharter::hcaes(x = !!rlang::sym(x), y = !!rlang::sym(y), group = !!rlang::sym(group))) %>%
    highcharter::hc_tooltip(pointFormat = tooltip) %>%
    highcharter::hc_yAxis(title = list(text = " "), labels = list(format = format),
                          style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular")) %>%
    highcharter::hc_title(text = titel,
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
    highcharter::hc_colors(color) %>%
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
   }",
                                                       gsub("'", "\\\\'", titel),
                                                       gsub("'", "\\\\'", titel),
                                                       gsub("'", "\\\\'", quelle)
                                                     ))

   #                                                   onclick = htmlwidgets::JS(sprintf(
   #                                                     "function () {
   #   var date = new Date().toISOString().slice(0,10);
   #   var chartTitle = '%s'.replace(/\\s+/g, '_');
   #   var filename = chartTitle + '_' + date + '.txt';
   #
   #   var data = this.getCSV();
   #
   #   data += '\\n%s';  // <- Quelle anhängen
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
   #
   # }",
   #                                                     gsub("'", "\\\\'", titel),  # Titel escapen
   #                                                     gsub("'", "\\\\'", quelle)  # Quelle escapen
   #                                                   ))

                                                   ))
                                )
                              )
    )


}

#}

linebuilder_light <- function(df, titel, x , y, group = NULL, tooltip, format, color = c("#b16fab", "#154194","#66cbaf", "#fbbf24"), quelle="Quelle"){

  df <- df %>%
    dplyr::mutate(!!y := round(!!rlang::sym(y), 1))

  out <- highcharter::hchart(df, 'line', highcharter::hcaes(x = !!rlang::sym(x), y = !!rlang::sym(y), group = !!rlang::sym(group))) %>%
    highcharter::hc_tooltip(pointFormat = tooltip) %>%
    highcharter::hc_yAxis(title = list(text = " "), labels = list(format = format),
                          style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular")) %>%
    highcharter::hc_title(text = titel,
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
    highcharter::hc_colors(color) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "Calibri Regular", fontSize = "14px")
    )  %>%
    highcharter::hc_caption(text = quelle,
                            style = list(fontSize = "11px", color = "gray")) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG", "downloadCSV"))
                              )
    )


}

balkenbuilder <- function(df, titel , x, y, group=NULL, tooltip, format, color,
                          optional=NULL, reverse = TRUE, TF=NULL, stacking=NULL, subtitel = NULL, quelle="Quelle"){

  if (is.numeric(df[[y]])) {
  df <- df %>%
    dplyr::mutate(!!y := round(!!rlang::sym(y), 1))
  }


  if(is.null(group) && is.null(optional)){


    out <- highcharter::hchart(df, 'bar', highcharter::hcaes(y =!!rlang::sym(y), x = !!rlang::sym(x))) %>%
      highcharter::hc_tooltip(pointFormat = tooltip) %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = format)) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_colors(color) %>%
      highcharter::hc_title(text = titel,
                            margin = 45, # o. war vorher /
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = reverse)  %>%
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

   }",
                                                         gsub("'", "\\\\'", titel),
                                                         gsub("'", "\\\\'", titel),
                                                         gsub("'", "\\\\'", quelle)
                                                       ))

                                                     ))
                                  )
                                )
      )

  } else if (!is.null(group) && is.null(optional) && is.null(stacking)){

    out <- highcharter::hchart(df, 'bar', highcharter::hcaes(y =!!rlang::sym(y), x = !!rlang::sym(x), group = !!rlang::sym(group))) %>%
      highcharter::hc_tooltip(pointFormat = tooltip) %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = format)) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_colors(color) %>%
      highcharter::hc_title(text = titel,
                            margin = 45, # o. war vorher /
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = reverse)  %>%
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

   }",
                                                         gsub("'", "\\\\'", titel),  # Titel escapen
                                                         gsub("'", "\\\\'", titel),  # Quelle escapen
                                                         gsub("'", "\\\\'", quelle)
                                                       ))

                                                     ))
                                  )
                                )
      )

  } else if (is.null(group) && !is.null(optional) && is.null(stacking)) {


    out <- highcharter::hchart(df, 'bar', highcharter::hcaes(y =!!rlang::sym(y), x = !!rlang::sym(x))) %>%
      highcharter::hc_tooltip(pointFormat = tooltip) %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = format)) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      {do.call(highcharter::hc_plotOptions,  c(list(.), optional))} %>% #keine ahnung wieso chatgpt help
      highcharter::hc_colors(color) %>%
      highcharter::hc_title(text = titel,
                            margin = 45, # o. war vorher /
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = reverse)  %>%
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

   }",
                                                         gsub("'", "\\\\'", titel),  # Titel escapen
                                                         gsub("'", "\\\\'", titel),###
                                                         gsub("'", "\\\\'", quelle)

                                                       ))

                                                     ))
                                  )
                                )
      )


  } else if (!is.null(group) && !is.null(optional) && is.null(stacking)){



    out <- highcharter::hchart(df, 'bar', highcharter::hcaes(y =!!rlang::sym(y), x = !!rlang::sym(x), group = !!rlang::sym(group))) %>%
      highcharter::hc_tooltip(pointFormat = tooltip) %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = format)) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      {do.call(highcharter::hc_plotOptions,  c(list(.), optional))} %>% #keine ahnung wieso chatgpt help
      highcharter::hc_colors(color) %>%
      highcharter::hc_title(text = titel,
                            margin = 45, # o. war vorher /
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = reverse)  %>%
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

   }",
                                                         gsub("'", "\\\\'", titel),
                                                         gsub("'", "\\\\'", titel),# Titel escapen
                                                         gsub("'", "\\\\'", quelle)  # Quelle escapen
                                                       ))

                                                     ))
                                  )
                                )
      )


  } else if (!is.null(TF) && !is.null(group) && is.null(stacking)){



    out <- highcharter::hchart(df, 'bar', highcharter::hcaes(y =!!rlang::sym(y), x = !!rlang::sym(x), group = !!rlang::sym(group))) %>%
      highcharter::hc_tooltip(pointFormat = tooltip) %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = format), reversedStacks = TF) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_colors(color) %>%
      highcharter::hc_title(text = titel,
                            margin = 45, # o. war vorher /
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = reverse) %>%
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
   }",
                                                         gsub("'", "\\\\'", titel),
                                                         gsub("'", "\\\\'", titel),
                                                         gsub("'", "\\\\'", quelle)
                                                       ))

                                                     ))
                                  )
                                )
      )


  } else if (!is.null(TF) && !is.null(group) && !is.null(stacking)){



    out <- highcharter::hchart(df, 'bar', highcharter::hcaes(y =!!rlang::sym(y), x = !!rlang::sym(x), group = !!rlang::sym(group))) %>%
      highcharter::hc_plotOptions(bar = list(stacking = stacking )) %>%
      highcharter::hc_tooltip(pointFormat = tooltip) %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = format), reversedStacks = TF) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_colors(color) %>%
      highcharter::hc_title(text = titel,
                            margin = 45, # o. war vorher /
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = reverse) %>%
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
   }",
                                                         gsub("'", "\\\\'", titel),
                                                         gsub("'", "\\\\'", titel),
                                                         gsub("'", "\\\\'", quelle)  # Quelle escapen
                                                       ))

                                                     ))
                                  )
                                )
      )


  }  else if(!is.null(stacking) && format == "1" ){

    unused_format <- format



    out <- df %>% highcharter::hchart("bar", highcharter::hcaes(y =!!rlang::sym(y), x = !!rlang::sym(x), group = !!rlang::sym(group)))%>%
      highcharter::hc_plotOptions(bar = list(stacking = stacking )) %>%
      highcharter::hc_tooltip(pointFormat=tooltip) %>%
      highcharter::hc_colors(color) %>%
      highcharter::hc_title(text = titel,
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
      highcharter::hc_subtitle(text = subtitel,
                               align = "center",
                               style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "18px")) %>%
      highcharter::hc_yAxis(title = list(text = "")) %>% #######NO FORMAT
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular", fontSize = "18px")) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = reverse) %>%
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
   }",
                                                         gsub("'", "\\\\'", titel),
                                                         gsub("'", "\\\\'", titel),
                                                         gsub("'", "\\\\'", quelle)  # Quelle escapen
                                                       ))

                                                     ))
                                  )
                                )
      )


  } else if(!is.null(stacking) && !is.null(group) && format != "1"){


    out <- df %>% highcharter::hchart("bar", highcharter::hcaes(y =!!rlang::sym(y), x = !!rlang::sym(x), group = !!rlang::sym(group)))%>%
      highcharter::hc_plotOptions(bar = list(stacking = stacking )) %>%
      highcharter::hc_tooltip(pointFormat=tooltip) %>%
      highcharter::hc_colors(color) %>%
      highcharter::hc_title(text = titel,
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
      highcharter::hc_subtitle(text = subtitel,
                               align = "center",
                               style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "18px")) %>%
      highcharter::hc_yAxis(title = list(text = ""),  labels = list(format = "{value}%"), reversedStacks =  FALSE) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular", fontSize = "18px")) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = reverse) %>%
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
   }",
                                                         gsub("'", "\\\\'", titel),
                                                         gsub("'", "\\\\'", titel),
                                                         gsub("'", "\\\\'", quelle)  # Quelle escapen
                                                       ))

                                                     ))
                                  )
                                )
      )


  }

  else {
    return(1)
  }


  return(out)

}





get_top10_hc_plot_options <- function(hc,
                                      hc_title = "",
                                      hc_tooltip = "",
                                      max_percent_used = 100,
                                      col = "#B16FAB",
                                      marker = "IEA") {
  if(marker=="IEA"){
    its <- "Quelle der Daten: IEA, 2023; OECD, 2023, freier Download, eigene Berechnungen durch MINTvernetzt."
  } else if (marker=="OECD"){
    its <- "Quelle der Daten: Eurostat, 2023; OECD, 2023; UNESCO, 2023; freier Download, eigene Berechnungen durch MINTvernetzt."

  }


  out <- hc %>%
    highcharter::hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE, format = "{point.wert} %",
                          style = list(textOutline = "none"))
      )) %>%
    highcharter::hc_tooltip(pointFormat = hc_tooltip) %>%
    highcharter::hc_yAxis(title = list(text = ""),
                          labels = list(format = "{value} %"),
                          min = 0,
                          max = max_percent_used,
                          tickInterval = 10) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_colors(c(col)) %>%
    highcharter::hc_title(text = hc_title,
                          margin = 45,
                          align = "center",
                          style = list(color = "black",
                                       useHTML = TRUE,
                                       fontFamily = "Calibri Regular",
                                       fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "Calibri Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
    highcharter::hc_caption(text = its,
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
                                                       gsub("'", "\\\\'", quelle))  #
                                                     )

                                                   ))
                                )
                              )
    )

  return(out)
}






balkenbuilder3 <- function(df, titel , x, y, tooltip, format, color, optional, optional2,  quelle="Quelle"){

  out <- highcharter::hchart(df, 'bar', highcharter::hcaes(y =!!rlang::sym(y), x = !!rlang::sym(x))) %>%
    highcharter::hc_tooltip(pointFormat = tooltip) %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = format)) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    {do.call(highcharter::hc_plotOptions,  c(list(.), optional))} %>% #keine ahnung wieso chatgpt help
    {
      if (!is.null(optional2)) {
        optional2(.)
      } else {
        .
      }
    } %>%
    highcharter::hc_colors(color) %>%
    highcharter::hc_title(text = titel,
                          margin = 45, # o. war vorher /
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "Calibri Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE)  %>%
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
   }", gsub("'", "\\\\'", titel),gsub("'", "\\\\'", titel),  gsub("'", "\\\\'", quelle))  #
                                                     )

                                                   ))
                                )
                              )
    )

  return(out)


}


#mapbuilder

mapbuilder <- function(df, joinby, name, tooltip,titel, mincolor, maxcolor, prop = FALSE, wert = FALSE, map = map_selection, landkarten = FALSE, states=NULL,  quelle="Quelle"){

if(prop==FALSE && wert == FALSE){
  out<- highcharter::hcmap(
    "countries/de/de-all",
    data = df,
    value = "proportion",
    joinBy = joinby,
    borderColor = "#FAFAFA",
    name = name,
    borderWidth = 0.1,
    nullColor = "#A9A9A9",
    tooltip = list(
      valueDecimals = 0,
      valueSuffix = "%"
    )) %>%
    highcharter::hc_tooltip(pointFormat = tooltip) %>%
    highcharter::hc_colorAxis(min=0,minColor= mincolor, maxColor=maxcolor, labels = list(format = "{text}%")) %>%
    highcharter::hc_title(
      text = titel,
      margin = 10,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")
    ) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "Calibri Regular")
    ) %>% highcharter::hc_size(600, 550) %>%
    highcharter::hc_credits(enabled = FALSE) %>%
    highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                           verticalAlign = "bottom")  %>%
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
   }", gsub("'", "\\\\'", titel),gsub("'", "\\\\'", titel),gsub("'", "\\\\'", quelle))  #
                                                     )

                                                   ))
                                )
                              )
    )

} else if(prop == TRUE && landkarten == FALSE){
  out<- highcharter::hcmap(
    "countries/de/de-all",
    data = df,
    value = "prop",
    joinBy = joinby,
    borderColor = "#FAFAFA",
    name = name,
    borderWidth = 0.1,
    nullColor = "#A9A9A9",
    tooltip = list(
      valueDecimals = 0,
      valueSuffix = "%"
    )) %>%
    highcharter::hc_tooltip(pointFormat = tooltip) %>%
    highcharter::hc_colorAxis(min=0,minColor= mincolor, maxColor=maxcolor, labels = list(format = "{text}%")) %>%
    highcharter::hc_title(
      text = titel,
      margin = 10,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")
    ) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "Calibri Regular")
    ) %>% highcharter::hc_size(600, 550) %>%
    highcharter::hc_credits(enabled = FALSE) %>%
    highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                           verticalAlign = "bottom")  %>%
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
   }", gsub("'", "\\\\'", titel),gsub("'", "\\\\'", titel), gsub("'", "\\\\'", quelle))  #
                                                     )

                                                   ))
                                )
                              )
    )

} else if(wert==TRUE){

  out <- highcharter::hcmap(
    map = map,
    data = df,
    value = "wert",
    joinBy = joinby,
    borderColor = "#FAFAFA",
    name = name,
    borderWidth = 0.1,
    nullColor = "#A9A9A9",
    tooltip = list(
      valueDecimals = 0,
      valueSuffix = "%"
    )) %>%
    highcharter::hc_tooltip(pointFormat = tooltip) %>%
    highcharter::hc_colorAxis(min=0, minColor= mincolor, maxColor=maxcolor,labels = list(format = "{text}%")) %>%
    highcharter::hc_title(
      text = titel,
      margin = 10,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")
    ) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "Calibri Regular")
    ) %>% highcharter::hc_size(1000, 600) %>%
    highcharter::hc_credits(enabled = FALSE) %>%
    highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                           verticalAlign = "bottom")  %>%
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
   }", gsub("'", "\\\\'", titel),gsub("'", "\\\\'", titel), gsub("'", "\\\\'", quelle))  #
                                                     )

                                                   ))
                                )
                              )
    )

} else if (landkarten == TRUE && prop == TRUE && !is.null(states))
{

  useless <- map
  mincolor1 <- mincolor
  maxcolor1 <- maxcolor



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

  out <- highcharter::hcmap(
    paste0("countries/de/de-", state_code ,"-all"),
    data = df,
    value = "prob",
    joinBy = joinby,
    borderColor = "#FAFAFA",
    name = name,
    borderWidth = 0.1,
    nullColor = "#A9A9A9",
    tooltip = list(
      valueDecimals = 0,
      valueSuffix = "%"
    )
    #,
    # download_map_data = FALSE
  ) %>%
    highcharter::hc_colorAxis(min=0, labels = list(format = "{text}%")) %>%
    highcharter::hc_title(
      text = titel,
      margin = 10,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")
    ) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "Calibri Regular")
    ) %>% #highcharter::hc_size(600, 550) %>%
    highcharter::hc_credits(enabled = FALSE) %>%
    highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                           verticalAlign = "bottom"
    ) %>%
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
   }", gsub("'", "\\\\'", titel),gsub("'", "\\\\'", titel), gsub("'", "\\\\'", quelle))  #
                                                     )

                                                   ))
                                )
                              )
    )




}

  return(out)
}






#' A function to search relevants subpages
#'
#' @description A function that searches the given keywords and filters the lookup table to present suitable subpages and plots
#'
#' @return The return value is a plot
#' @param term a string with the search term
#' @noRd

get_search_data <- function(term, session) {
  # lookup table
  # term <- "test"
  # term <- ""

  # term zu lower case
  this_search <- tolower(term)
  # stopwords raus, satzzeichen raus, stemming
  # this_search <- tm::removeWords(this_search, tm::stopwords("german"))
  # this_search <- quanteda::tokens(this_search, remove_punct = TRUE)
  # this_search <- quanteda::tokens_wordstem(this_search, language = "de")
  # this_search <- paste(this_search)


  # this_search <- SnowballC::wordStem(this_search, language = "de")
  # this_search <- paste0(unlist(strsplit(x = this_search, split = " ")))
  # this_search <- "speed car distance"
  search_text <- suchtabelle$term
  # search for each term and then return any findings
  search_idx <- lapply(
    X = this_search,
    FUN = function(term) {
      grepl(pattern = term, search_text
            #, max.distance = 2
      )
    }
  )
  # combine searches and only thos with all search terms present are used
  search_idx <- Reduce("&", search_idx)

  if (is.null(search_idx)) {
    out <- suchtabelle
  } else {
    out <- suchtabelle[search_idx,]
  }

  out <- out %>%
    dplyr::select(Bereich, Registerkarte, Plotart, menuItem..tabName, Box..ID)%>%
    dplyr::mutate(Plotart = stringr::str_to_title(Plotart))

  return(out)
}





#bilder downloads
# save_widget_to_png <- function(widget, png_path,
#                                vwidth = 1200, vheight = 800, zoom = 2) {
#   if (!inherits(widget, "htmlwidget")) {
#     stop("Das übergebene Objekt ist kein htmlwidget.")
#   }
#
#   html_path <- tempfile(fileext = ".html")
#   on.exit(unlink(html_path, force = TRUE), add = TRUE)
#
#   htmlwidgets::saveWidget(widget, file = html_path, selfcontained = TRUE)
#
#   chrome_path <- tryCatch(webshot2::find_chrome(), error = function(e) NULL)
#
#   if (!is.null(chrome_path) && file.exists(chrome_path)) {
#     webshot2::webshot(url = html_path, file = png_path,
#                       vwidth = vwidth, vheight = vheight, zoom = zoom)
#   } else {
#     if (!requireNamespace("webshot", quietly = TRUE)) {
#       stop("Kein Chrome/Chromium gefunden und Paket 'webshot' ist nicht installiert.")
#     }
#     if (isFALSE(webshot::is_phantomjs_installed())) {
#       webshot::install_phantomjs()
#     }
#     webshot::webshot(url = html_path, file = png_path,
#                      vwidth = vwidth, vheight = vheight)
#   }
#
#   if (!file.exists(png_path)) stop("PNG wurde nicht erzeugt (webshot fehlgeschlagen).")
#   invisible(png_path)
# }



