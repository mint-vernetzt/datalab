library(dplyr)
library(tidyr)

#' A function to plot a graph.
#'
#' @description A function to create a map for the first box
#' inside the tab "International".
#'
#' @return The return value is a plot
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd



add_avg_to_hc <- function(hc, hc_mean, type) {

  if(type == "MINT"){
    col <- "#154194"
  }else if(type == "Frauen"){
    col <- "#B16FAB"
  }

  out <- hc %>%
    highcharter::hc_yAxis(
      plotLines = list(
        list(
          value = hc_mean,
          color = col,
          width = 3,
          zIndex = 4
        )
      )
    )

  return(out)
}


## studium ----
plot_international_map <- function(r) {

  timerange <- r$map_y_int_studium
  label_m <- r$map_l_int_studium
  fach_m <- r$map_f_int_studium

  if (is.null(fach_m)) { fach_m <- ""}


  if (label_m == "Weltweit") {
    map_selection <- highcharter::download_map_data(url = "custom/world", showinfo = FALSE)

    fach_m <- "Alle MINT-Fächer"

    # df <- dplyr::tbl(con, from = "studierende_absolventen_weltweit") %>%
    #   dplyr::filter(fach == "Alle MINT-Fächer") %>%
    #   dplyr::filter(land != "San Marino") %>%
    #   dplyr::filter(jahr != "2022") %>%
    #   dplyr::mutate(wert = round(wert, 1)) %>%
    #   dplyr::collect()

    df_query <- glue::glue_sql("
    SELECT *
    FROM studierende_absolventen_weltweit
    WHERE fach = 'Alle MINT-Fächer'
    AND NOT land = 'San Marino'
    AND NOT jahr = '2022'
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::mutate(wert = round(wert, 1))


  } else if (label_m == "OECD") {
    map_selection <- highcharter::download_map_data(url = "custom/world", showinfo = FALSE)

    # filter for selection

    # df_filtered <- dplyr::tbl(con, from = "studierende_anzahl_oecd") %>%
    #   dplyr::filter(geschlecht == "Gesamt" &
    #                   jahr == timerange &
    #                   ebene == 1 &
    #                   anforderung %in% c("Bachelor oder vergleichbar (akademisch)",
    #
    #                                      "Master oder vergleichbar (akademisch)",
    #                                      "Promotion (ISCED 8)")) %>%
    #   dplyr::collect()
    #
    df_query <- glue::glue_sql("
    SELECT *
    FROM studierende_anzahl_oecd
    WHERE geschlecht = 'Gesamt'
    AND jahr = {timerange}
    AND ebene = 1
    AND anforderung IN ('Bachelor oder vergleichbar (akademisch)', 'Master oder vergleichbar (akademisch)', 'Promotion (ISCED 8)')
                               ", .con = con)

    df_filtered <- DBI::dbGetQuery(con, df_query)



    df_filtered <- df_filtered %>%
      dplyr::group_by(fach, geschlecht, jahr, land) %>%
      dplyr::filter(dplyr::n_distinct(anforderung) == 3) %>%
      dplyr::ungroup() %>%
      dplyr::collect()



    # calculate total amount by land
    this_df_alle <- df_filtered %>%
      dplyr::filter(fachbereich == "Alle") %>%
      dplyr::group_by(land, jahr, fach)%>%
      dplyr::summarise(total = sum(wert, na.rm = TRUE)) %>%
      dplyr::ungroup()

    # calculate percentage values by land
    df <- df_filtered %>%
      dplyr::filter(fachbereich == fach_m) %>%
      dplyr::group_by(land, jahr, fach) %>%
      dplyr::summarise(wert = sum(wert, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::select(land, wert) %>%
      dplyr::left_join(this_df_alle, by = "land") %>%
      dplyr::mutate(wert_absolut = wert) %>%
      dplyr::mutate(wert = round(wert / total * 100, 1))



  } else if (label_m == "EU") {
    map_selection <- highcharter::download_map_data(url = "custom/europe", showinfo = FALSE)

    # df <- dplyr::tbl(con, from = "studierende_europa") %>%
    #   dplyr::filter(geschlecht == "Gesamt"  &
    #                   (
    #                   mint_select == "mint" |
    #                   (
    #                   mint_select == "nicht mint" &
    #                   fach_m == "Alle MINT-Fächer"
    #                   )
    #                   )
    #                   &
    #                   fach == fach_m &
    #                   indikator == "Fächerwahl") %>%
    #   dplyr::collect()


    df_query <- glue::glue_sql("
      SELECT *
      FROM studierende_europa
      WHERE geschlecht = 'Gesamt'
      AND (
          mint_select = 'mint'
          OR (mint_select = 'nicht mint' AND fach = 'Alle MINT-Fächer')
      )
      AND fach = {fach_m}
      AND indikator = 'Fächerwahl'
      ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)


  } else {
    # input is not what it should be
    shiny::req(FALSE)
  }


  # filter dataset based on UI inputs
  df_insp1 <- df %>% dplyr::filter(jahr == timerange &
                                     !is.na(wert))


  # Hover vorbereiten
  df_insp1$display_wert <- prettyNum(df_insp1$wert, big.mark = ".", decimal.mark = ",")
  hover <- "{point.land} <br> Anteil: {point.display_wert} %"

  if(label_m == "OECD"){
    df_insp1$wert_absolut_display <- prettyNum(df_insp1$wert_absolut, big.mark = ".", decimal.mark = ",")
    hover <- "{point.land} <br> Anteil: {point.display_wert} % <br> Anzahl: {point.wert_absolut_display}"
  }

  if(label_m == "OECD"){
    df7 <- df_insp1 %>%
      dplyr::select(land, jahr, fach, wert, display_wert, wert_absolut_display) %>%
      dplyr::inner_join(countries_names, by = "land") %>%
      dplyr::mutate(alpha2 = toupper(alpha2))
  }else{
    df7 <- df_insp1 %>%
      dplyr::select(land, jahr, fach, wert, display_wert) %>%
      dplyr::inner_join(countries_names, by = "land") %>%
      dplyr::mutate(alpha2 = toupper(alpha2))
  }


  # Plot

  # Vorbereitung Überschrift
  fach_help <- fach_m
  fach_help <- ifelse(fach_m == "Alle MINT-Fächer", "MINT", fach_help)
  fach_help <- ifelse(fach_help == "Dienstleistungen", "Fächern aus dem Bereich 'Dienstleistungen'", fach_help)

  if(label_m == "Weltweit"){
    title_m <- paste0("Anteil von Studienabsolvent:innen in ", fach_help, " an allen Studienabsolvent:innen ",
                      timerange, " weltweit (UNESCO)")
  }else{if(label_m == "OECD"){
    title_m <- paste0("Anteil von Studierenden in ", fach_help, " an allen Studierenden ",
                      timerange, " in den OECD-Staaten")
  }else{
    title_m <- paste0("Anteil von Studierenden in ", fach_help, " an allen Studierenden ",
                      timerange, " in Europa")
  }
  }

  #Europakarte
  if(label_m == "EU"){
    size_width <- 800
    size_hight <- 600
    #Weltkarte
  }else if(label_m == "OECD" | label_m == "Weltweit"){
    size_width <- 1000
    size_hight <- 600
  }



  #
  data_map_1 <- df7


  #zu komplex / different
  highcharter::highchart(type = "map") %>%
    highcharter::hc_add_series_map(
      map = map_selection,
      df = data_map_1,
      value = "wert",
      joinBy = c("hc-a2", "alpha2"),
      borderColor = "#FAFAFA",
      name = paste0(fach_m),
      borderWidth = 0.1,
      nullColor = "#A9A9A9",
      tooltip = list(valueDecimals = 1, valueSuffix = "%")
    )%>%
    highcharter::hc_tooltip(pointFormat = hover) %>%
    highcharter::hc_colorAxis(min=0, minColor= "#f4f5f6", maxColor="#b16fab",labels = list(format = "{text} %")) %>%
    highcharter::hc_title(
      text = title_m,
      margin = 10,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")
    ) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "Calibri Regular")
    ) %>% highcharter::hc_size(size_width, size_hight) %>%
    highcharter::hc_credits(enabled = FALSE) %>%
    highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                           verticalAlign = "bottom") %>%
    highcharter::hc_caption(text = "    Quelle der Daten: Eurostat, 2023; OECD, 2023, freier Download, eigene Berechnungen durch MINTvernetzt.",
                            style = list(fontSize = "11px", color = "gray")) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG", "downloadCSV")
                                )
                              )
    )


}


plot_international_map_fem <- function(r){

  # region ui input laden
  label_m <- r$map_l_f


  # falls region EU
  if(label_m == "EU"){
    # kartenabschnitt für hc definieren
    map_selection <- "custom/europe"

    # Spezifische inputs laden
    timerange <- r$map_y_f
    fach_m <- r$map_f_f
    betr <- r$map_le_betr



    # falls betrachtung = fva
    if(betr == "Anteil von Frauen an Allen"){

      # daten in richtige form bringen und runden
      # df1 <- dplyr::tbl(con, from = "studierende_europa") %>%
      #   dplyr::filter(ebene == "1" &
      #                   indikator == "Frauen-/Männeranteil"&
      #                   mint_select == "mint")%>%
      #   dplyr::collect()

      df_query <- glue::glue_sql("
      SELECT *
      FROM studierende_europa
      WHERE ebene = '1'
      AND indikator = 'Frauen-/Männeranteil'
      AND mint_select = 'mint'
                               ", .con = con)

      df1 <- DBI::dbGetQuery(con, df_query)



      df1 <- df1 %>%
        tidyr::pivot_wider(values_from = wert, names_from = geschlecht)%>%
        dplyr::select(-Männer, - Gesamt)%>%
        dplyr::rename(wert = Frauen)%>%
        dplyr::mutate(across(wert, ~ round(.,1)))%>%
        dplyr::filter( fach == fach_m&
                         jahr == timerange)


      # wert für hover vorbereiten
      df1$display_rel <- prettyNum( df1$wert, big.mark = ".", decimal.mark = ",")

      # mit geo mapping data joinen
      map_data_1 <- df1 %>%
        dplyr::left_join(countries_names, by = "land") %>%
        dplyr::mutate(alpha2 = toupper(alpha2))



      # spezifischen hover vorbereiten
      hoverplot <- "{point.land} <br> Anteil: {point.display_rel}%"

      # spezifischen titel vorbereiten
      title_dyn <- if(fach_m=="Alle MINT-Fächer"){
        paste("Anteil von weiblichen Studierenden an allen Studierenden in MINT (" , timerange, ")")

      } else if (fach_m=="Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe"){

        paste("Anteil von weiblichen Studierenden an allen Studierenden in Ingenieurwesen, verarbeitendem Gewerbe und Baugewerbe (" , timerange, ")")

      } else {

        paste("Anteil von weiblichen Studierenden an allen Studierenden in ", fach_m, " (" , timerange, ")")
      }


    } else if(betr=="Anteil an Frauen von Frauen"){
      # falls betrachtung == fvf

      # daten in richtige form bringen und runden

      # df1 <- dplyr::tbl(con, from = "studierende_europa") %>%
      #   dplyr::filter(ebene == "1" &
      #                   indikator == "Fächerwahl"&
      #                   mint_select == "mint" &
      #                   geschlecht == "Frauen")%>%
      #   dplyr::filter(fach == fach_m &
      #                   jahr == timerange)%>%
      #   dplyr::collect()




      df_query <- glue::glue_sql("
      SELECT *
      FROM studierende_europa
      WHERE ebene = '1'
      AND indikator = 'Fächerwahl'
      AND mint_select = 'mint'
      AND geschlecht = 'Frauen'
                               ", .con = con)

      df <- DBI::dbGetQuery(con, df_query)

      df1 <- df %>%
        dplyr::filter(fach == fach_m &
                        jahr == timerange)


      df1 <- df1 %>%
        dplyr::mutate(display_rel = prettyNum(round(wert,1), big.mark = ".", decimal.mark = ","))  # hover und titel vorbereiten


      # mit geo mapping data joinen
      map_data_1 <- df1 %>%
        dplyr::left_join(countries_names, by = "land") %>%
        dplyr::mutate(alpha2 = toupper(alpha2))

      # spezifischen hover vorbereiten
      hoverplot <- "{point.land} <br> Anteil: {point.display_rel}%"


      # spezifischen titel vorbereiten
      title_dyn <- if(fach_m=="Alle MINT-Fächer"){
        paste("Anteil von weiblichen Studierenden in MINT an allen weiblichnen Studierenden (" , timerange, ")")

      } else if (fach_m=="Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe"){

        paste("Anteil von weiblichen Studierenden in Ingenieurwesen, verarbeitendem Gewerbe und Baugewerbe an allen Studierenden (" , timerange, ")")

      } else {

        paste("Anteil von weiblichen Studierenden in ", fach_m, " an allen weiblichen Studierenden (", timerange, ")")
      }

    }

  }

  # falls Region == OECD
  if (label_m == "OECD"){

    # ui inputs laden
    level <- r$map_le_f
    betr <- r$map_le_betr
    timerange <- r$map_y_f
    fach_m <- r$map_f_f

    # Kartenabschnitt für hc definieren
    map_selection <- "custom/world"

    df_query <- glue::glue_sql("
    SELECT *
    FROM studierende_anzahl_oecd
    WHERE geschlecht IN ('Frauen', 'Gesamt')
    AND jahr = {timerange}
    AND ebene = '1'
    AND anforderung IN ('Bachelor oder vergleichbar (akademisch)', 'Master oder vergleichbar (akademisch)', 'Promotion (ISCED 8)')
                               ", .con = con)

    df_filteredd <- DBI::dbGetQuery(con, df_query)
    df_filtered <- df_filteredd

    df_filtered <- df_filtered %>%
      tidyr::pivot_wider(names_from = anforderung, values_from = wert)%>%
      # Zahl d. Studierenden ist hier Summe aus master + bachelor + Promovierende
      dplyr::mutate(wert = rowSums(dplyr::select(., "Bachelor oder vergleichbar (akademisch)",
                                                 "Master oder vergleichbar (akademisch)",
                                                 "Promotion (ISCED 8)"), na.rm= T ))%>%
      dplyr::select(- c("Bachelor oder vergleichbar (akademisch)",
                        "Master oder vergleichbar (akademisch)",
                        "Promotion (ISCED 8)"))



    # Frauenanzahl für beide Betrachtungsweien errechnen

    # Frauen von Allen
    df_share_fem <- df_filtered %>%
      dplyr::select(-fachbereich)%>%
      dplyr::filter(mint_select== "mint" |
                      fach=="Alle")%>%
      tidyr::pivot_wider(values_from = wert, names_from = geschlecht)%>%
      dplyr::mutate(fva = Frauen/Gesamt*100)%>%
      dplyr::select(- Gesamt)%>%
      dplyr::mutate(display_rel = prettyNum(round(.$fva,1), big.mark = ".", decimal.mark = ","))%>%
      dplyr::mutate(display_total = prettyNum(.$Frauen, big.mark = ".", decimal.mark = ","))%>%
      dplyr::rename(wert = fva)


    # Frauen von Allen

    # Filtern für alle Einzelfächer in MINT
    df_share_fem2 <- df_filtered %>%
      dplyr::select(-fachbereich)%>%
      dplyr::filter(mint_select== "mint" & geschlecht == "Frauen" |
                      fach=="Alle" & geschlecht == "Frauen")%>%
      dplyr::select(-mint_select)

    # Filtern für Alle Fächer (insgesamt)
    df_share_fem3 <- df_filtered %>%
      dplyr::select(-fachbereich)%>%
      dplyr::filter(mint_select== "mint" & geschlecht == "Frauen" |
                      fach=="Alle" & geschlecht == "Frauen") %>%
      dplyr::filter(fach== "Alle")%>%
      dplyr::rename(Alle = wert)%>%
      dplyr::select(-fach,-mint_select)

    # Beide werte zusammenbringen, um rechenoperation zu ermöglichen
    df_share_fem4 <- df_share_fem2 %>%
      dplyr::full_join(df_share_fem3, by=c("bereich",
                                           "quelle",
                                           "typ",
                                           "indikator",
                                           "ebene",
                                           "geschlecht",
                                           "population",
                                           "land_code",
                                           "land",
                                           "jahr"
      ))%>%
      # Anteil berechnen: *Einzelfach* / Alle Fächer
      dplyr::mutate(fvf = wert/Alle*100)%>%
      dplyr::select(-Alle)%>%
      dplyr::mutate(display_rel = prettyNum(round(.$fvf,1), big.mark = ".", decimal.mark = ","))%>%
      dplyr::mutate(display_total = prettyNum(.$wert, big.mark = ".", decimal.mark = ","))%>%
      dplyr::select(-wert)%>%
      dplyr::rename(wert = fvf)


    # Für fva
    if(betr =="Anteil von Frauen an Allen"){

      # Mit geo mapping joinen
      map_data_1 <- df_share_fem %>%
        dplyr::select(land, jahr, fach, wert, display_rel, display_total) %>%
        dplyr::inner_join(countries_names, by = "land") %>%
        dplyr::mutate(alpha2 = toupper(alpha2))%>%
        dplyr::filter(fach == fach_m)

      # hc titel vorbereiten
      title_dyn <- if(fach_m=="MINT"){
        paste("Anteil von weiblichen Studierenden an allen Studierenden in MINT (" , timerange, ")")

      } else if (fach_m=="Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe"){

        paste("Anteil von weiblichen Studierenden an allen Studierenden in Ingenieurwesen, verarbeitendem Gewerbe und Baugewerbe (" , timerange, ")")

      } else {

        paste("Anteil von weiblichen Studierenden an allen Studierenden in ", fach_m, " " , timerange)
      }

    # fvf
    } else if(betr=="Anteil an Frauen von Frauen") {

      # Mit geo mapping joinen
      map_data_1 <- df_share_fem4 %>%
        dplyr::select(land, jahr, fach, wert, display_rel, display_total) %>%
        dplyr::inner_join(countries_names, by = "land") %>%
        dplyr::mutate(alpha2 = toupper(alpha2))%>%
        dplyr::filter(fach == fach_m)

      # hc titel vorbereiten
      title_dyn <- if(fach_m=="MINT"){
        paste("Anteil von weiblichen MINT-Studierenden an allen weiblichen Studierenden (" , timerange, " )")

      } else if (fach_m=="Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe"){

        paste("Anteil von weiblichen Studierenden in Ingenieurwesen, verarbeitendem Gewerbe und Baugewerbe an allen weiblichen Studierenden (" , timerange, ")")

      } else {

        paste("Anteil von weiblichen Studierenden in ", fach_m,  " an allen weiblichen Studierenden (" , timerange, ")")
      }

    }

    # plot hover vorbereiten
    hoverplot <- "{point.land} <br> Anteil: {point.display_rel}% <br> Anzahl: {point.display_total}"






  }
  #Europakarte
  if(label_m == "EU"){
    size_width <- 800
    size_hight <- 600
  #Weltkarte
  }else if(label_m == "OECD" | label_m == "Weltweit"){
    size_width <- 1000
    size_hight <- 600
  }

    # plot
##################


      df <- map_data_1
      joinby <- c("hc-a2", "alpha2")
      name <- paste0(fach_m)
      tooltip <-hoverplot
      titel <-title_dyn
      mincolor <- "#f4f5f6"
      maxcolor <- "#154194"
      quelle <- "Quelle der Daten: Eurostat, 2023; OECD, 2023, freier Download, eigene Berechnungen durch MINTvernetzt."
      out1 <- mapbuilder(df, joinby,name, tooltip, titel, mincolor, maxcolor, prop=FALSE, wert=TRUE, map=map_selection, quelle = quelle)


}


plot_international_top10 <- function(r) {

  timerange <- r$map_y_m
  label_m <- r$map_l_m
  fach_m <- r$map_f_m
  show_avg <- r$show_avg_top10_mint_line_m



  if (is.null(fach_m)) { fach_m <- ""}

  if (label_m == "Weltweit") {

    df <- dplyr::tbl(con, from = "studierende_absolventen_weltweit")  %>%
      dplyr::filter(fach == "Alle MINT-Fächer" &
                      jahr == timerange &
                      land != "San Marino") %>%
      dplyr::mutate(wert = round(wert, 1)) %>%
      dplyr::select(land, wert) %>%
      dplyr::collect()




  }
  else if (label_m == "OECD") {


    df_query <- glue::glue_sql("
    SELECT *
    FROM studierende_anzahl_oecd
    WHERE geschlecht = 'Gesamt'
    AND jahr = {timerange}
    AND ebene = '1'
    AND anforderung IN ('Bachelor oder vergleichbar (akademisch)', 'Master oder vergleichbar (akademisch)', 'Promotion (ISCED 8)')
                               ", .con = con)

    df_filtered <- DBI::dbGetQuery(con, df_query)

    df_filtered <- df_filtered %>%
      dplyr::group_by(fach, geschlecht, jahr, land) %>%
      dplyr::filter(dplyr::n_distinct(anforderung) == 3) %>%
      dplyr::ungroup()


    # calculate total amount by land
    this_df_alle <- df_filtered %>%
      dplyr::filter(fachbereich == "Alle") %>%
      dplyr::group_by(land) %>%
      dplyr::summarise(total = sum(wert, na.rm = TRUE)) %>%
      dplyr::ungroup()

    # calculate percentage values by land
    df <- df_filtered %>%
      dplyr::filter(fachbereich == fach_m) %>%
      dplyr::group_by(land) %>%
      dplyr::summarise(wert = sum(wert, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::select(land, wert) %>%
      dplyr::left_join(this_df_alle, by = "land") %>%
      dplyr::mutate(wert_absolut = wert) %>%
      dplyr::mutate(wert = round(wert / total * 100, 1))

  }
  if (label_m == "EU") {


    # Prüfe, ob mint_select existiert, falls nicht, setze Standardwert
    if (!exists("mint_select") || is.null(mint_select)) mint_select <- "mint"

    df_query <- glue::glue_sql("
SELECT land, ROUND(wert, 1) AS wert
FROM studierende_europa
WHERE geschlecht = 'Gesamt'
AND jahr = {timerange}
AND (
    {mint_select} = 'mint'
    OR ({mint_select} = 'nicht mint' AND {fach_m} = 'Alle MINT-Fächer')
)
AND fach = {fach_m}
AND indikator = 'Fächerwahl'
AND land != 'Lichtenstein'
", .con = con)

df <- DBI::dbGetQuery(con, df_query)

df <- df %>%
  dplyr::mutate(wert = round(wert, 1)) %>%
  dplyr::select(land, wert)


  }

  # filter missing values
  df <- df %>%
    dplyr::filter(!is.na(wert))

  # Grenze für die X-Achse ist immer etwas größer als der maximale wert
  # aber nie größer als 100%
  # Grenze soll immer in 10er Schritten gehen
  max_percent_used <- ceiling(min(c(100, max(df$wert) * 1.2)) / 10) * 10


  # Hover vorbereiten
  df$wert_display <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
  hover <- "Anteil: {point.wert_display} %"
  if(label_m == "OECD"){
    df$wert_absolut_display <- prettyNum(df$wert_absolut, big.mark = ".", decimal.mark = ",")
    hover <- "Anteil: {point.wert_display} % <br> Anzahl: {point.wert_absolut_display}"
  }

  # Überschrift vorbereiten
  t_gruppe <- "Studierenden in "
  t_gruppe <- ifelse(label_m == "Weltweit", "Studienabsolvent*innen in ", t_gruppe)
  t_fach <- fach_m
  t_fach <- ifelse(t_fach %in% c("MINT", "Alle MINT-Fächer"), "MINT-Fächern", t_fach)
  t_fach <- ifelse(t_fach == "Dienstleistungen", "Fächern aus dem Bereich 'Dienstleistungen'", t_fach)
  t_quelle <- ""
  t_quelle <- ifelse(label_m == "Weltweit", " (UNESCO)", t_quelle)
  t_quelle1 <- ""
  t_quelle1 <- ifelse(label_m == "OECD", "OECD-", t_quelle1)
  t_quelle1 <- ifelse(label_m == "EU", "Europäische ", t_quelle1)



  # Create top 10 plot

  #dies ist schon als funktion automatisiert, too complex
  plot_top <- highcharter::hchart(
    df %>% dplyr::arrange(desc(wert)) %>% dplyr::slice(1:10),
    'bar',
    highcharter::hcaes(y = wert, x = land)) %>%
    get_top10_hc_plot_options(
      hc_title = paste0(t_quelle1, "Länder mit dem größten Anteil an ", t_gruppe, t_fach, " in ", timerange, t_quelle),
      hc_tooltip = hover,
      max_percent_used = max_percent_used,
      marker="OECD")

  # Create bottom 10 plot
  #dies ist schon als funktion automatisiert, too complex
  plot_bottom <- highcharter::hchart(
    df %>% dplyr::arrange(desc(wert)) %>% dplyr::slice_tail(n = 10),
    'bar',
    highcharter::hcaes(y = wert, x = land)) %>%
    get_top10_hc_plot_options(
      hc_title = paste0(t_quelle1, "Länder mit dem niedrigsten Anteil an ", t_gruppe, t_fach, " in ", timerange, t_quelle),
      hc_tooltip = hover,
      max_percent_used = max_percent_used,
      marker="OECD")


  if (show_avg == "Ja") {
    plot_top <- plot_top %>%
      add_avg_to_hc(hc_mean = mean(df$wert, na.rm = TRUE), type = "MINT")
    plot_bottom <- plot_bottom %>%
      add_avg_to_hc(hc_mean = mean(df$wert, na.rm = TRUE), type = "MINT")
  }


out <- list(plot_top, plot_bottom)

return(out)


}


plot_international_top10_gender <- function(r) {


  timerange <- r$map_y_g
  label_m <- r$map_l_g
  fach_m <- r$map_f_g
  show_avg <- r$show_avg_g
  # höchster Frauenanteil in MINT vs meiste Frauen wählen MINT
  # AA vs BB
  art <- r$art_g


  if (is.null(fach_m)) { fach_m <- ""}
  if (is.null(art)) { art <- ""}


  if (label_m == "OECD" & art == "meisten Frauen wählen MINT") {
    # meiste Frauen wählen MINT

    df_query <- glue::glue_sql("
    SELECT *
    FROM studierende_anzahl_oecd
    WHERE geschlecht = 'Frauen'
    AND ebene = 1
    AND jahr = {timerange}
    AND anforderung IN ('Bachelor oder vergleichbar (akademisch)','Master oder vergleichbar (akademisch)','Promotion (ISCED 8)')

                               ", .con = con)

    df_filtered <- DBI::dbGetQuery(con, df_query)


    df_filtered <- df_filtered %>%
      dplyr::group_by(fach, geschlecht, jahr, land) %>%
      dplyr::filter(dplyr::n_distinct(anforderung) == 3) %>%
      dplyr::ungroup()



    # calculate total amount by land
    this_df_alle <- df_filtered %>%
      dplyr::filter(fachbereich == "Alle") %>%
      dplyr::group_by(land) %>%
      dplyr::summarise(total = sum(wert, na.rm = TRUE)) %>%
      dplyr::ungroup()

    # calculate percentage values by land
    df <- df_filtered %>%
      dplyr::filter(fachbereich == fach_m) %>%
      dplyr::group_by(land) %>%
      dplyr::summarise(wert = sum(wert, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::select(land, wert) %>%
      dplyr::left_join(this_df_alle, by = "land") %>%
      dplyr::mutate(wert_absolut = wert) %>%
      dplyr::mutate(wert = round(wert / total * 100, 1))

  }
  if (label_m == "OECD" & art == "höchster Frauenanteil in MINT") {
    # höchster Frauenanteil

    df_query <- glue::glue_sql("
    SELECT *
    FROM studierende_anzahl_oecd
    WHERE fachbereich = {fach_m}
    AND jahr = {timerange}
    AND ebene = 1
    AND anforderung IN ('Bachelor oder vergleichbar (akademisch)', 'Master oder vergleichbar (akademisch)','Promotion (ISCED 8)')
                               ", .con = con)

    df_filtered <- DBI::dbGetQuery(con, df_query)


    df_filtered <- df_filtered %>%
      dplyr::group_by(fach, geschlecht, jahr, land) %>%
      dplyr::filter(dplyr::n_distinct(anforderung) == 3) %>%
      dplyr::ungroup()


    # calculate total amount by land
    this_df_alle <- df_filtered %>%
      dplyr::filter(geschlecht == "Gesamt") %>%
      dplyr::group_by(land) %>%
      dplyr::summarise(total = sum(wert, na.rm = TRUE)) %>%
      dplyr::ungroup()

    # calculate percentage values by land
    df <- df_filtered %>%
      dplyr::filter(geschlecht == "Frauen") %>%
      dplyr::group_by(land) %>%
      dplyr::summarise(wert = sum(wert, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::select(land, wert) %>%
      dplyr::left_join(this_df_alle, by = "land") %>%
      dplyr::mutate(wert_absolut = wert) %>%
      dplyr::mutate(wert = round(wert / total * 100, 1))

  }
  if (label_m == "EU" & art == "höchster Frauenanteil in MINT") {
    # höchster Frauenanteil


    if (!exists("mint_select") || is.null(mint_select)) mint_select <- "mint"


    df_query <- glue::glue_sql("
      SELECT land, wert
      FROM studierende_europa
      WHERE geschlecht = 'Frauen'
      AND jahr = {timerange}
      AND (
          mint_select = 'mint'
          OR ({mint_select} = 'nicht mint' AND {fach_m} = 'Alle MINT-Fächer')
          )

      AND fach = {fach_m}
      AND indikator = 'Frauen-/Männeranteil'
      AND land NOT IN ('EU (27), seit 2020', 'Liechtenstein')
      ", .con = con)

      df <- DBI::dbGetQuery(con, df_query)

      df <- df %>%
        dplyr::select(land, wert)

  }
  if (label_m == "EU" & art == "meisten Frauen wählen MINT") {
    # meiste Frauen wählen MINT

    df_query <- glue::glue_sql("
            SELECT land, wert
            FROM studierende_europa
            WHERE geschlecht = 'Frauen'
            AND jahr = {timerange}
            AND (
                mint_select = 'mint'
                OR (mint_select = 'nicht mint' AND fach_m = 'Alle MINT-Fächer')
            )
            AND fach = {fach_m}
            AND indikator = 'Fächerwahl'
            AND land NOT IN ('EU (27), seit 2020', 'Lichtenstein')
      ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::select(land, wert)



  }


  # Grenze für die X-Achse ist immer etwas größer als der maximale wert
  # aber nie größer als 100%
  # Grenze soll immer in 10er Schritten gehen
  max_percent_used <- ceiling(min(c(100, max(df$wert) * 1.2)) / 10) * 10


  # filter missing values
  df <- df %>%
    dplyr::filter(!is.na(wert)) %>%
    dplyr::mutate(wert = round(wert, 1))


  # Hover vorbereiten
  df$wert_display <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
  hover <- "Anteil: {point.wert_display} %"
  if(label_m == "OECD"){
    df$wert_absolut_display <- prettyNum(df$wert_absolut, big.mark = ".", decimal.mark = ",")
    hover <- "Anteil: {point.wert_display} % <br> Anzahl: {point.wert_absolut_display}"
  }

  # Überschrift vorbereiten
  t_quelle1 <- "OECD-"
  t_quelle1 <- ifelse(label_m == "EU", "Europäische ", t_quelle1)
  t_fach <- fach_m
  t_fach <- ifelse(t_fach == "Dienstleistungen", "Fächern aus dem Bereich 'Dienstleistungen'", t_fach)

  if(art == "meisten Frauen wählen MINT"){
    titel1 <- paste0(t_quelle1, "Länder, in denen sich der höchste Anteil an Frauen für ein Studium in ",
                     t_fach, " entscheidet (", timerange, ")")
    titel2 <- paste0(t_quelle1, "Länder, in denen sich der geringste Anteil an Frauen für ein Studium in ",
                     t_fach, " entscheidet (", timerange, ")")
  }else{
    titel1 <- paste0(t_quelle1, "Länder mit dem größten Frauenanteil unter Studierenden in ",
                     t_fach, " (", timerange, ")")
    titel2 <- paste0(t_quelle1, "Länder mit dem niedrigsten Frauenanteil unter Studierenden in ",
                     t_fach, " (", timerange, ")")
  }


  # Create top 10 plot
  #dies ist schon als funktion automatisiert, too complex
  plot_top <- highcharter::hchart(
    df %>% dplyr::arrange(desc(wert)) %>% dplyr::slice(1:10),
    'bar',
    highcharter::hcaes(y = wert, x = land)) %>%
    get_top10_hc_plot_options(
      hc_title = titel1,
      hc_tooltip = hover,
      max_percent_used = max_percent_used,
      col = "#154194",
      marker="OECD"
      )


  # Create bottom 10 plot
  #dies ist schon als funktion automatisiert, too complex
  plot_bottom <- highcharter::hchart(
    df %>% dplyr::arrange(desc(wert)) %>% dplyr::slice_tail(n = 10),
    'bar',
    highcharter::hcaes(y = wert, x = land)) %>%
    get_top10_hc_plot_options(
      hc_title = titel2,
      hc_tooltip = "Anteil: {point.wert} % <br> Anzahl: {point.wert_absolut}" ,
      max_percent_used = max_percent_used,
      col = "#154194",
      marker="OECD"
      )


  if (show_avg == "Ja") {
    plot_top <- plot_top %>%
      add_avg_to_hc(hc_mean = mean(df$wert, na.rm = TRUE), type = "Frauen")
    plot_bottom <- plot_bottom %>%
      add_avg_to_hc(hc_mean = mean(df$wert, na.rm = TRUE), type = "Frauen")
  }

  out <- list(plot_top, plot_bottom)

  return(out)



}



plot_international_mint_top_10 <- function(r){

# Überschriften anpassen, einheitlich mit anderen plots

# ui inputs laden
avg_line <- r$show_avg_ti
inpy <- r$map_y_ti


df_query <- glue::glue_sql("
  SELECT *
  FROM studierende_mobil_eu_absolut
  WHERE geschlecht = 'Gesamt'
  AND anforderung IN ('Bachelor oder vergleichbar (ISCED 6)',
                      'Master oder vergleichbar (ISCED 7)',
                      'Promotion (ISCED 8)')
  AND fach = 'MINT'
  AND kommentar IS NULL
          ", .con = con)

  data1 <- DBI::dbGetQuery(con, df_query)


  data1 <- data1 %>%
    dplyr::group_by(fach,geschlecht,
                    land,jahr,
                    kommentar,ebene,mint_select,
                    bereich,indikator,typ )%>%
    # relevanter wert = summe der anforderungsniveaus in mint
    dplyr::summarise(fach,geschlecht,
                     land,jahr,
                     kommentar,ebene,mint_select,
                     bereich,indikator,typ, wert= sum(wert,na.rm =T))%>%
    unique()%>%
    dplyr::ungroup()%>%
    # filtern für input
    dplyr::filter(jahr == inpy) %>%
    # wert für hover vorbereiten
    dplyr::mutate(display_total = prettyNum(.$wert, big.mark = ".", decimal.mark = ","))




  # titel vorbereiten
  title_dyn_top <- paste("Länder Europas mit der höchsten Zahl an \ninternationalen Studierenden in MINT im Jahr", inpy)
  title_dyn_bot <- paste("Länder Europas mit der niedrigsten Zahl an \ninternationalen Studierenden in MINT im Jahr", inpy)


if (avg_line == "Ja"){

  data_avg <- round(mean(data1$wert, na.rm = T),1)

  #dies ist schon als funktion automatisiert, too complex
    plot_top <- highcharter::hchart(
      data1 %>% dplyr::arrange(desc(wert)) %>% dplyr::slice(1:10),
      'bar',
      highcharter::hcaes(y = wert, x = land))%>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, format = "{point.display_total}")
        )) %>%
      highcharter::hc_tooltip(pointFormat = "Anzahl: {point.display_total}") %>%
      highcharter::hc_yAxis(plotLines = list(
        list(
          value = data_avg,
          color = "#B16FAB",
          width = 3,
          zIndex = 4
        )
      ),title = list(text = ""),
      labels = list(format = "{value}"),
      min = 0,
      max = max(data1$wert)*1.2)%>%
      highcharter::hc_xAxis(title = list(text = " ")) %>%
      highcharter::hc_colors(c("#66CBAF")) %>%
      highcharter::hc_title(text = title_dyn_top,
                            margin = 10,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")
                            ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
      highcharter::hc_caption(text = "Quelle der Daten: Eurostat, 2023; OECD, 2023; UNESCO, 2023; freier Download, eigene Berechnungen durch MINTvernetzt.",
                              style = list(fontSize = "11px", color = "gray")) %>%
      highcharter::hc_exporting(enabled = TRUE,
                                buttons = list(
                                  contextButton = list(
                                    menuItems = list("downloadPNG", "downloadCSV")
                                  )
                                )
      )




    #dies ist schon als funktion automatisiert, too complex
    plot_bottom <- highcharter::hchart(
      data1 %>% dplyr::arrange(desc(wert)) %>% dplyr::slice_tail(n = 10),
      'bar',
      highcharter::hcaes(y = wert, x = land))%>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, format = "{point.display_total}")
        )) %>%
      highcharter::hc_tooltip(pointFormat = "Anzahl: {point.display_total}") %>%
      highcharter::hc_yAxis(
                            plotLines = list(
                              list(
                                value = data_avg,
                                color = "#B16FAB",
                                width = 3,
                                zIndex = 4
                              )
                            ),title = list(text = ""),
                            labels = list(format = "{value}"),
                            min = 0,
                            max = max(data1$wert)*1.2)%>%
      highcharter::hc_xAxis(title = list(text = " ")) %>%
      highcharter::hc_colors(c("#154194")) %>%
      highcharter::hc_title(text =  title_dyn_bot,
                            margin = 10,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")
      ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
      highcharter::hc_caption(text = "Quelle der Daten: Eurostat, 2023; OECD, 2023; UNESCO, 2023; freier Download, eigene Berechnungen durch MINTvernetzt.",
                              style = list(fontSize = "11px", color = "gray")) %>%
      highcharter::hc_exporting(enabled = TRUE,
                                buttons = list(
                                  contextButton = list(
                                    menuItems = list("downloadPNG", "downloadCSV")
                                  )
                                )
      )




} else if (avg_line == "Nein"){


  #dies ist schon als funktion automatisiert, too complex
  plot_top <- highcharter::hchart(
    data1 %>% dplyr::arrange(desc(wert)) %>% dplyr::slice(1:10),
    'bar',
    highcharter::hcaes(y = wert, x = land))%>%
    highcharter::hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE, format = "{point.display_total}")
      )) %>%
    highcharter::hc_tooltip(pointFormat = "Anzahl: {point.display_total}") %>%
    highcharter::hc_yAxis(title = list(text = ""),
                          labels = list(format = "{value}"),
                          min = 0,
                          max = max(data1$wert)*1.2) %>%
    highcharter::hc_xAxis(title = list(text = " ")) %>%
    highcharter::hc_colors(c("#154194")) %>%
    highcharter::hc_title(text = title_dyn_top,
                          margin = 10,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")
    ) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "Calibri Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
    highcharter::hc_caption(text = "Quelle der Daten: Eurostat, 2023; OECD, 2023; UNESCO, 2023; freier Download, eigene Berechnungen durch MINTvernetzt.",
                            style = list(fontSize = "11px", color = "gray")) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG", "downloadCSV")
                                )
                              )
    )



  #dies ist schon als funktion automatisiert, too complex
  plot_bottom <- highcharter::hchart(
    data1 %>% dplyr::arrange(desc(wert)) %>% dplyr::slice_tail(n = 10),
    'bar',
    highcharter::hcaes(y = wert, x = land))%>%
    highcharter::hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE, format = "{point.display_total}")
      )) %>%
    highcharter::hc_tooltip(pointFormat = "Anzahl: {point.display_total}") %>%
    highcharter::hc_yAxis(title = list(text = ""),
                          labels = list(format = "{value}"),
                          min = 0,
                          max = max(data1$wert)*1.2) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_colors(c("#154194")) %>%
    highcharter::hc_title(text =  title_dyn_bot,
                          margin = 10,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")
    ) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "Calibri Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
    highcharter::hc_caption(text = "Quelle der Daten: Eurostat, 2023; OECD, 2023; UNESCO, 2023; freier Download, eigene Berechnungen durch MINTvernetzt.",
                            style = list(fontSize = "11px", color = "gray")) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG", "downloadCSV")
                                )
                              )
    )

}

 out <- list(plot_top, plot_bottom)

 return(out)

}

## schule ----
plot_international_schule_map <- function(r) {

  timerange <- r$map_y_int_schule
  label_m <- r$map_l_int_schule
  fach_m <- r$map_f_int_schule
  leistungsindikator_m <- r$map_li_int_schule

  if (is.null(fach_m)) { fach_m <- ""}


  map_selection <- "custom/world"

  if (label_m == "TIMSS") {
    this_ordnung <- ifelse(
      leistungsindikator_m == "Test-Punktzahl",
      "Achievement",
      "Benchmarks")

    this_indikator <- ifelse(
      leistungsindikator_m == "Mittlerer Standard erreicht",
      "Mittlerer int'l. Maßstab (475)",
      "Insgesamt")


    df_query <- glue::glue_sql("
    SELECT *
    FROM schule_timss
    WHERE ordnung = {this_ordnung}
    AND indikator = {this_indikator}
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

    help_l <- "4. Klasse"
  }
  if (label_m == "PISA") {

    df_query <- glue::glue_sql("
    SELECT *
    FROM schule_pisa
    WHERE bereich = 'Ländermittel'
    AND indikator = 'Insgesamt'
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)


    help_l <- "9. & 10. Klasse"
  }


  # filter dataset based on UI inputs
  dfs <- df %>%
    dplyr::filter(jahr == timerange &
                    fach == fach_m &
                    !is.na(wert))


  # Hover & Titel vorbereiten
  titel <- paste0("Durchnittliche Leistung von Schüler:innen der ", help_l,
                  " im ", fach_m, "-Kompetenztest von ",
                  label_m, " ", timerange)

  dfs$display_wert <- prettyNum(round(dfs$wert, 1),
                                big.mark = ".",
                                decimal.mark = ",")

  if (leistungsindikator_m == "Test-Punktzahl") {
    tooltip_prefix <- "Punktzahl"
    tooltip_scale <- ""
  }
  if (leistungsindikator_m == "Mittlerer Standard erreicht") {
    tooltip_prefix <- "Anteil"
    tooltip_scale <- "%"
    titel <- paste0("Anteil von Schüler:innen der ", help_l,
                    ", die im ", fach_m, "-Kompetenztest von ",
                    label_m, " den mittleren internationalen Standard erreichen (", timerange, ")")
  }

  # Skalen-Minimum/-Maximum anpassen
  s_min <- ifelse(label_m == "PISA", 300, 200)
  s_min <- ifelse(label_m == "TIMSS" & leistungsindikator_m == "Mittlerer Standard erreicht", 0, s_min)
  s_max <- ifelse(label_m == "PISA", 600, 650)
  s_max <- ifelse(label_m == "TIMSS" & leistungsindikator_m == "Mittlerer Standard erreicht", 100, s_max)

  data_map_1 <- dfs %>%
    dplyr::select(land, jahr, fach, wert, display_wert) %>%
    dplyr::inner_join(countries_names, by = "land") %>%
    dplyr::mutate(alpha2 = toupper(alpha2))


 map_selection <- highcharter::download_map_data(url = "custom/world", showinfo = FALSE)

 #zu komplex / different
  # plot
  highcharter::highchart(type = "map") %>%
    highcharter::hc_add_series_map(
      map = map_selection,
      df = data_map_1,
      value = "wert",
      joinBy = c("hc-a2", "alpha2"),
      borderColor = "#FAFAFA",
      name = paste0(fach_m),
      borderWidth = 0.1,
      nullColor = "#A9A9A9",
      tooltip = list(valueDecimals = 0, valueSuffix = "%")
    ) %>%

    highcharter::hc_tooltip(
      pointFormat = paste0("{point.land} <br> ", tooltip_prefix,
                           ": {point.display_wert} ", tooltip_scale)) %>%
    highcharter::hc_colorAxis(min=s_min, max=s_max,
                              minColor= "#f4f5f6",
                              maxColor="#b16fab",
                              labels = list(format = paste0("{text}", tooltip_scale))) %>%
    highcharter::hc_title(
      text = titel,
      margin = 10,
      align = "center",
      style = list(color = "black",
                   useHTML = TRUE,
                   fontFamily = "Calibri Regular",
                   fontSize = "20px")
    ) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "Calibri Regular")
    ) %>% highcharter::hc_size(1000, 600) %>%
    highcharter::hc_credits(enabled = FALSE) %>%
    highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                           verticalAlign = "bottom") %>%
    highcharter::hc_caption(text = "Quelle der Daten: IEA, 2023; OECD, 2023; freier Download, eigene Berechnungen durch MINTvernetzt.",
                            style = list(fontSize = "11px", color = "gray")) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG", "downloadCSV")
                                )
                              )


    )

}


plot_international_schule_item <- function(r) {


  timerange <- r$item_y_int_schule
  label_m <- "TIMSS"
  fach_m <- r$item_f_int_schule

  if (is.null(fach_m)) { fach_m <- ""}


  df_query <- glue::glue_sql("
  SELECT *
  FROM schule_timss
  WHERE jahr = {timerange}
  AND fach = {fach_m}
  AND ordnung = 'Gender'", .con = con)

  df <- DBI::dbGetQuery(con, df_query)



  set_group <- function(gender, diff) {
    # Prüfen, ob alle Werte "
    if (all(is.na(diff))) {
      out <- "kein signifikanter Unterschied"

    } else if (any(diff[gender == "Jungen"] == "Ja", na.rm = TRUE)) {
      out <- "Jungen signifikant besser"

    } else if (any(diff[gender == "Mädchen"] == "Ja", na.rm = TRUE)) {
      out <- "Mädchen signifikant besser"

    } else {
      out <- "kein signifikanter Unterschied"
    }

    return(out)
  }

  # enthält den Text für den plot
  group_col_dt <- data.frame(
    group = c("kein signifikanter Unterschied", "Jungen signifikant besser", "Mädchen signifikant besser"),
    group_text = c(" mit keinem signifikaten Unterschied zwischen Jungen und Mädchen",
                   ", in denen Jungen signifikant besser abschneiden als Mädchen",
                   ", in denen Mädchen signifikant besser abscheniden als Jungen"),
    group_col = c("#66cbaf", "#D0A9CD", "#EFE8E6")
  )

  # reshape long to wide for later merge
  wert_dt <- df %>%
    dplyr::select(land, indikator, wert) %>%
    tidyr::pivot_wider(names_from = indikator,
                       values_from = wert,
                       names_prefix = "wert_")

  plot_data <- df %>%
    dplyr::group_by(land) %>%
    dplyr::summarise(group = set_group(gender = indikator,
                                       diff = `signifikant höher gegenüber anderem geschlecht`),
                     count = 1) %>%
    dplyr::left_join(y = group_col_dt, by = "group") %>%
    dplyr::left_join(y = wert_dt, by = "land") %>%
    dplyr::arrange(group, land) #%>%
  #dplyr::group_split(group)


  plot_legend_data <- plot_data %>%
    dplyr::group_by(group_text, group_col) %>%
    dplyr::summarise(count = sum(count)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(legend_text =  paste0(
      "<span style='color:", group_col, ";'>&#x25FC;</span>",
      count, " Länder", group_text,"<br>"
    )) %>%
    dplyr::filter(!is.na(count))

  # Farbe für Deutschland
  plot_data$group_col <- ifelse(plot_data$land == "Deutschland" & plot_data$group == "Jungen signifikant besser", "#703D6B", plot_data$group_col )
  plot_data$group_col <- ifelse(plot_data$land == "Deutschland" & plot_data$group == "Mädchen signifikant besser", "#9D7265", plot_data$group_col )
  plot_data$group_col <- ifelse(plot_data$land == "Deutschland" & plot_data$group == "kein signifikanter Unterschied", "#008F68", plot_data$group_col )


  out <- highcharter::hchart(
    plot_data,
    "item",
    highcharter::hcaes(
      name = group,
      y = count,
      label = group,
      color = group_col),
    name = "group",
    showInLegend = FALSE
  ) %>%
    highcharter::hc_caption(
      text = paste0(plot_legend_data$legend_text, collapse = "<br>"),
      useHTML = TRUE
    ) %>%
    highcharter::hc_tooltip(
      pointFormat = paste0("{point.land} <br>",
                           "Test-Punktzahl<br>",
                           " Jungen: {point.wert_Jungen}<br>",
                           " Mädchen: {point.wert_Mädchen}")) %>%
    highcharter::hc_title(
      text = paste0("Geschlechtsunterschiede der 4.-Klässler:innen im ",
                    fach_m, "-Kompetenztest von ",
                    label_m, " (", timerange, ")"),
      margin = 10,
      align = "center",
      style = list(color = "black",
                   useHTML = TRUE,
                   fontFamily = "Calibri Regular",
                   fontSize = "20px")
    ) %>%
    highcharter::hc_subtitle(
      text= paste0("Jeder Punkt repräsentiert ein Land.", br(),
      "Deutschland ist als dunkler hervorgehoben."),
      align = "left"
    )%>%
    highcharter::hc_chart(
      style = list(fontFamily = "Calibri Regular")
    ) %>%
    highcharter::hc_size(580, 450) %>%
    highcharter::hc_caption(text = "    Quelle der Daten: IEA, 2023; OECD, 2023, freier Download, eigene Berechnungen durch MINTvernetzt.",
                            style = list(fontSize = "11px", color = "gray")) %>%
    highcharter::hc_credits(enabled = FALSE) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG", "downloadCSV")
                                )
                              )
    )



  return(out)
}







plot_international_schule_migration <- function(r) {

  timerange <- r$line_y_int_schule
  label_m <- r$line_l_int_schule
  fach_m <- r$line_f_int_schule
  leistungsindikator_m <- r$line_li_int_schule
   # lander <- r$regio_int_schule

  if(label_m == "TIMSS") {lander <- r$regio_int_schule_timss} else{
    lander <- r$regio_int_schule_pisa
  }


  if (is.null(fach_m)) { fach_m <- ""}

  if (label_m == "TIMSS") {
    this_ordnung <- switch(
      leistungsindikator_m,
      "nach Geschlecht" = "Gender",
      "nach sozialem Status" = "Ressourcen"
    )
    this_indikator <- switch(
      leistungsindikator_m,
      "nach Geschlecht" = c("Jungen", "Mädchen"),
      "nach sozialem Status" = c("Viele Ressourcen",
                                 "Einige Ressourcen",
                                 "Wenige Ressourcen")
    )
    this_typ <- switch(
      leistungsindikator_m,
      "nach Geschlecht" = "Test-Punktzahl",
      "nach sozialem Status" = "Gemittelte Test-Punktzahl"
    )


    df_query <- glue::glue_sql("
    SELECT *
    FROM schule_timss
    WHERE ordnung = {this_ordnung}
    AND indikator IN ({this_indikator*})
    AND typ = {this_typ}
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

    help_l <- "4. Klasse"

    # Labels anpassen

    df$indikator[df$indikator == "Viele Ressourcen"] <- "hoher sozialer Status"
    df$indikator[df$indikator == "Einige Ressourcen"] <- "mittlerer sozialer Status"
    df$indikator[df$indikator == "Wenige Ressourcen"] <- "niedriger sozialer Status"

  }
  if (label_m == "PISA") {

    this_bereich <- switch(
      leistungsindikator_m,
      "nach Geschlecht" = "Ländermittel",
      "nach Zuwanderungsgeschichte" = "Migrationshintergrund",
      "nach Bildungskapital" = "Bücher im Haushalt"
    )

    this_indikator <- switch(
      leistungsindikator_m,
      "nach Geschlecht" = c("Jungen", "Mädchen"),
      "nach Zuwanderungsgeschichte" = c("Keiner",
                                        "Zweite Generation",
                                        "Erste Generation"),
      "nach Bildungskapital" = c("0-10", "1-10", "26-100", "Mehr als 500")
    )


    df_query <- glue::glue_sql("
    SELECT *
    FROM schule_pisa
    WHERE bereich = {this_bereich}
    AND indikator IN ({this_indikator*})
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)


    # Labels anpassen
    df$indikator[df$indikator == "Keiner"] <- "ohne Zuwanderungsgeschichte"
    df$indikator[df$indikator == "Zweite Generation"] <- "nur Eltern zugewandert"
    df$indikator[df$indikator == "Erste Generation"] <- "Kind selbst zugewandert"
    df$indikator[df$indikator == "0-10"] <- "sehr niedriges Bildungskapital (bis zu 10 Bücher zuhause)"
    df$indikator[df$indikator == "26-100"] <- "niedriges Bildungskapital (bis zu 100 Bücher zuhause)"
    df$indikator[df$indikator == "Mehr als 500"] <- "hohes Bildungskapital (über 500 Bücher zuhause)"
    df$indikator[df$indikator == "1-10"] <- "sehr niedriges Bildungskapital (bis zu 10 Bücher zuhause)"

    help_l <- "9. & 10. Klasse"
  }


  # filter dataset based on UI inputs


  dfs <- df %>%
    dplyr::filter(jahr == timerange &
                    fach == fach_m)

  used_lands <- dfs %>%
    dplyr::filter(!is.na(wert)) %>%
    dplyr::pull(land) %>%
    unique()

  dfs <- dfs %>%
    dplyr::filter(land %in% used_lands)

  #Trennpunkte für lange Zahlen ergänzen
  dfs$display_wert <- prettyNum(round(dfs$wert, 1),
                                big.mark = ".",
                                decimal.mark = ",")


  data_line <- dfs %>%
    dplyr::select(land, wert, display_wert, indikator)

  line_colors <- c("#B16FAB", "#154194", "#35BD97",
                   "#8893A7", "#FBBF24", "#9D7265")
  color <- line_colors[seq_along(this_indikator)]

  plot_data <- data_line %>%
    select(land, indikator, wert) %>%
    pivot_wider(
      names_from = indikator,
      values_from = wert
    )


    if (label_m == "TIMSS" && leistungsindikator_m == "nach sozialem Status") {

      plot_data <- plot_data %>%
        rename(
          basis_wert = `niedriger sozialer Status`,
          mittel_wert = `mittlerer sozialer Status`,
          wert = `hoher sozialer Status`
        )

      plot_data <- plot_data %>%
        filter(!is.na(mittel_wert) | !is.na(wert) | !is.na(basis_wert))

      plot_data <- plot_data %>%
        filter(land %in% lander)

      plot_data <- plot_data %>%
        arrange(desc(basis_wert))  # Sortieren absteigend nach `basis_wert`


      ####
      fig <- plotly::plot_ly(data = plot_data, color = I("gray80")) %>%
        plotly::add_segments(
          x = ~basis_wert,
          xend = ~mittel_wert,
          y = ~land,
          yend = ~land,
          showlegend = FALSE,
          text = ~ifelse(is.na(basis_wert) | is.na(mittel_wert), NA,
                         paste0("Basis: ", basis_wert, "<br>Mittel: ", mittel_wert)),
          hoverinfo = "text"
        ) %>%
        plotly::add_segments(
          x = ~mittel_wert,
          xend = ~wert,
          y = ~land,
          yend = ~land,
          showlegend = FALSE,
          text = ~ifelse(is.na(mittel_wert) | is.na(wert), NA,
                         paste0("Mittel: ", mittel_wert, "<br>Positiv: ", wert)),
          hoverinfo = "text"
        ) %>%
        plotly::add_markers(
          x = ~basis_wert,
          y = ~land,
          name = "Niedriger Status",
          marker = list(
            size = 12,
            color = "#D0A9CD"
          ),
          text = ~ifelse(is.na(basis_wert), NA, paste0("Niedriger Status: ", basis_wert)),
          hoverinfo = "text"
        ) %>%
        plotly::add_markers(
          x = ~mittel_wert,
          y = ~land,
          name = "Mittlerer Status",
          marker = list(
            size = 12,
            color = "#66cbaf"
          ),
          text = ~ifelse(is.na(mittel_wert), NA, paste0("Mittlerer Status: ", mittel_wert)),
          hoverinfo = "text"
        ) %>%
        plotly::add_markers(
          x = ~wert,
          y = ~land,
          name = "Hoher Status",
          marker = list(
            size = 12,
            color = "#b16fab"
          ),
          text = ~ifelse(is.na(wert), NA, paste0("Hoher Status: ", wert)),
          hoverinfo = "text"
        ) %>%
        # Layout anpassen
        plotly::layout(
          title = "Vergleich der sozialen Statuswerte",
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
              text = "Quelle der Daten: IEA, 2023; OECD, 2023, freier Download, eigene Berechnungen durch MINTvernetzt",
              x = 0,
              y = -0.7,  # passt die vertikale Position (ggf. justieren!)
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              xanchor = "left",
              yanchor = "top",
              font = list(size = 11, color = "gray")
            )
          )
        )

    }
  else if (label_m == "TIMSS" && leistungsindikator_m == "nach Geschlecht") {

      plot_data <- plot_data %>%
        rename(
          basis_wert = Jungen,
          wert = Mädchen
        )

      plot_data <- plot_data %>%
        filter(!is.na(wert) | !is.na(basis_wert))

      plot_data <- plot_data %>%
        filter(land %in% lander)

      fig <- plotly::plot_ly(data = plot_data, color = I("gray80")) %>%
        plotly::add_segments(
          x = ~basis_wert,
          xend = ~wert,
          y = ~land,
          yend = ~land,
          showlegend = FALSE,
          text = ~ifelse(is.na(basis_wert) | is.na(wert), NA,
                         paste0("Jungen: ", basis_wert, "<br>Mädchen: ", wert)),
          hoverinfo = "text"
        ) %>%
        plotly::add_markers(
          x = ~basis_wert,
          y = ~land,
          name = "Jungen",
          marker = list(
            size = 12,
            color = "#D0A9CD"
          ),
          text = ~ifelse(is.na(basis_wert), NA, paste0("Jungen: ", basis_wert)),
          hoverinfo = "text"
        ) %>%
        plotly::add_markers(
          x = ~wert,
          y = ~land,
          name = "Mädchen",
          marker = list(
            size = 12,
            color = "#b16fab"
          ),
          text = ~ifelse(is.na(wert), NA, paste0("Mädchen: ", wert)),
          hoverinfo = "text"
        ) %>%
        # Layout anpassen
        plotly::layout(
          title = "Vergleich der Geschlechter",
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
          ,
          annotations = list(
            list(
              text = "Quelle der Daten: IEA, 2023; OECD, 2023, freier Download, eigene Berechnungen durch MINTvernetzt",
              x = 0,
              y = -0.7,  # passt die vertikale Position (ggf. justieren!)
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              xanchor = "left",
              yanchor = "top",
              font = list(size = 11, color = "gray")
            )
          )
        )


    }
  else if (label_m == "PISA" && leistungsindikator_m == "nach Geschlecht") {
      plot_data <- plot_data %>%
        rename(
          basis_wert = Jungen,
          wert = Mädchen
        )

      plot_data <- plot_data %>%
        filter(!is.na(wert) | !is.na(basis_wert))

      plot_data <- plot_data %>%
        filter(land %in% lander)

      fig <- plotly::plot_ly(data = plot_data, color = I("gray80")) %>%
        plotly::add_segments(
          x = ~basis_wert,
          xend = ~wert,
          y = ~land,
          yend = ~land,
          showlegend = FALSE,
          text = ~ifelse(is.na(basis_wert) | is.na(wert), NA,
                         paste0("Jungen: ", basis_wert, "<br>Mädchen: ", wert)),
          hoverinfo = "text"
        ) %>%
        plotly::add_markers(
          x = ~basis_wert,
          y = ~land,
          name = "Jungen",
          marker = list(
            size = 12,
            color = "#D0A9CD"
          ),
          text = ~ifelse(is.na(basis_wert), NA, paste0("Jungen: ", basis_wert)),
          hoverinfo = "text"
        ) %>%
        plotly::add_markers(
          x = ~wert,
          y = ~land,
          name = "Mädchen",
          marker = list(
            size = 12,
            color = "#b16fab"
          ),
          text = ~ifelse(is.na(wert), NA, paste0("Mädchen: ", wert)),
          hoverinfo = "text"
        ) %>%
        # Layout anpassen
        plotly::layout(
          title = "Vergleich der Geschlechter",
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
              text = "Quelle der Daten: IEA , 2023; OECD, 2023, freier Download, eigene Berechnungen durch MINTvernetzt",
              x = 0,
              y = -0.7,  # passt die vertikale Position (ggf. justieren!)
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              xanchor = "left",
              yanchor = "top",
              font = list(size = 11, color = "gray")
            )
          )
        )


    }
  else if (label_m == "PISA" && leistungsindikator_m == "nach Zuwanderungsgeschichte") {
      plot_data <- plot_data %>%
        rename(
          basis_wert = `ohne Zuwanderungsgeschichte`,
          mittel_wert = `nur Eltern zugewandert`,
          wert = `Kind selbst zugewandert`
        )


      plot_data <- plot_data %>%
        filter(!is.na(mittel_wert) | !is.na(wert) | !is.na(basis_wert))

      plot_data <- plot_data %>%
        filter(land %in% lander)

      fig <- plotly::plot_ly(data = plot_data, color = I("gray80")) %>%
        plotly::add_segments(
          x = ~basis_wert,
          xend = ~mittel_wert,
          y = ~land,
          yend = ~land,
          showlegend = FALSE,
          text = ~ifelse(is.na(basis_wert) | is.na(mittel_wert), NA,
                         paste0("Ohne Zuwanderungsgeschichte: ", basis_wert, "<br>Erste Generation: ", mittel_wert)),
          hoverinfo = "text"
        ) %>%
        plotly::add_segments(
          x = ~mittel_wert,
          xend = ~wert,
          y = ~land,
          yend = ~land,
          showlegend = FALSE,
          text = ~ifelse(is.na(mittel_wert) | is.na(wert), NA,
                         paste0("Erste Generation: ", mittel_wert, "<br>Zweite Generation: ", wert)),
          hoverinfo = "text"
        ) %>%
        plotly::add_markers(
          x = ~basis_wert,
          y = ~land,
          name = "Ohne Zuwanderungsgeschichte",
          marker = list(
            size = 12,
            color = "#D0A9CD"
          ),
          text = ~ifelse(is.na(basis_wert), NA, paste0("Ohne Zuwanderungsgeschichte: ", basis_wert)),
          hoverinfo = "text"
        ) %>%
        plotly::add_markers(
          x = ~mittel_wert,
          y = ~land,
          name = "Erste Generation",
          marker = list(
            size = 12,
            color = "#66cbaf"
          ),
          text = ~ifelse(is.na(mittel_wert), NA, paste0("Erste Generation: ", mittel_wert)),
          hoverinfo = "text"
        ) %>%
        plotly::add_markers(
          x = ~wert,
          y = ~land,
          name = "Zweite Generation",
          marker = list(
            size = 12,
            color = "#b16fab"
          ),
          text = ~ifelse(is.na(wert), NA, paste0("Zweite Generation: ", wert)),
          hoverinfo = "text"
        ) %>%
        # Layout anpassen
        plotly::layout(
          title = "Vergleich der Zuwanderungsgeschichte",
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
              text = "Quelle der Daten: IEA, 2023; OECD, 2023, freier Download, eigene Berechnungen durch MINTvernetzt",
              x = 0,
              y = -0.7,  # passt die vertikale Position (ggf. justieren!)
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              xanchor = "left",
              yanchor = "top",
              font = list(size = 11, color = "gray")
            )
          )
        )


    }
  else if (label_m == "PISA" && leistungsindikator_m == "nach Bildungskapital") {

     plot_data <- plot_data %>%
        rename(
          sehr_niedrig = `sehr niedriges Bildungskapital (bis zu 10 Bücher zuhause)`,
          niedrig = `niedriges Bildungskapital (bis zu 100 Bücher zuhause)`,
          hoch = `hohes Bildungskapital (über 500 Bücher zuhause)`
        )

      plot_data <- plot_data %>%
        filter(!is.na(niedrig) | !is.na(hoch) | !is.na(sehr_niedrig))

      plot_data <- plot_data %>%
        filter(land %in% lander)

      fig <- plotly::plot_ly(data = plot_data, color = I("gray80")) %>%
        plotly::add_segments(
          x = ~sehr_niedrig,
          xend = ~niedrig,
          y = ~land,
          yend = ~land,
          showlegend = FALSE,
          text = ~ifelse(is.na(sehr_niedrig) | is.na(niedrig), NA,
                         paste0("Sehr niedriges Bildungskapital: ", sehr_niedrig, "<br>Sehr niedriges Bildungskapital: ", sehr_niedrig)),
          hoverinfo = "text"
        ) %>%
        plotly::add_segments(
          x = ~niedrig,
          xend = ~hoch,
          y = ~land,
          yend = ~land,
          showlegend = FALSE,
          text = ~ifelse(is.na(niedrig) | is.na(hoch), NA,
                         paste0("Niedriges Bildungskapital: ", niedrig, "<br>Hohes Bildungskapital: ", hoch)),
          hoverinfo = "text"
        ) %>%
        plotly::add_markers(
          x = ~sehr_niedrig,
          y = ~land,
          name = "Sehr niedriges Bildungskapital (bis zu 10 Bücher zuhause)",
          marker = list(
            size = 12,
            color = "#D0A9CD"
          ),
          text = ~ifelse(is.na(sehr_niedrig), NA, paste0("Sehr niedriges Bildungskapital: ", sehr_niedrig)),
          hoverinfo = "text"
        ) %>%
        plotly::add_markers(
          x = ~niedrig,
          y = ~land,
          name = "Niedriges Bildungskapital (bis zu 100 Bücher zuhause)",
          marker = list(
            size = 12,
            color = "#66cbaf"
          ),
          text = ~ifelse(is.na(niedrig), NA, paste0("Niedriges Bildungskapital: ", niedrig)),
          hoverinfo = "text"
        ) %>%
        plotly::add_markers(
          x = ~hoch,
          y = ~land,
          name = "Hohes Bildungskapital (über 500 Bücher zuhause)",
          marker = list(
            size = 12,
            color = "#b16fab"
          ),
          text = ~ifelse(is.na(hoch), NA, paste0("Hohes Bildungskapital: ", hoch)),
          hoverinfo = "text"
        ) %>%
        # Layout anpassen
        plotly::layout(
          title = "Vergleich des Bildungskapitals",
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
              text = "Quelle der Daten: IEA, 2023; OECD, 2023, freier Download, eigene Berechnungen durch MINTvernetzt",
              x = 0,
              y = -0.7,  # passt die vertikale Position (ggf. justieren!)
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              xanchor = "left",
              yanchor = "top",
              font = list(size = 11, color = "gray")
            )
          )
        )

    } else {

    }


  fig <- fig %>%
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


  return(fig)

}




## arbeitsmarkt ----




plot_international_map_arb <- function(r) {

   # ui input für Region laden
  map_l <- r$map_l_arb

  # Falls Region EU ist:
  if(map_l== "EU"){

    # Spezifische UI inputs laden
    inpy <- r$map_y_arb
    inpp <- r$map_pers_arb

    # Kartenausschnitt für hc definieren
    map_selection <- "custom/europe"

    df_query <- glue::glue_sql("
    SELECT *
    FROM arbeitsmarkt_beschaeftigte_eu
    WHERE geschlecht = 'Gesamt'
    AND jahr = {inpy}
    AND indikator = {inpp}
    AND variable = 'Anteil an arbeitender Bevölkerung'
                               ", .con = con)

    data1 <- DBI::dbGetQuery(con, df_query)


    data1 <- data1 %>%
      tidyr::pivot_wider(names_from = variable, values_from = wert)%>%
      dplyr::rename(wert="Anteil an arbeitender Bevölkerung")

    # für hover vorbereiten
    data1$display_rel <- prettyNum(round(data1$wert,1), big.mark = ".", decimal.mark = ",")



    df_query <- glue::glue_sql("
    SELECT *
    FROM arbeitsmarkt_beschaeftigte_eu
    WHERE geschlecht = 'Gesamt'
    AND jahr = {inpy}
    AND indikator = {inpp}
    AND variable = 'Anzahl in Tsd.'
                               ", .con = con)

    data2 <- DBI::dbGetQuery(con, df_query)

    data2 <- data2 %>%
      tidyr::pivot_wider(names_from = variable, values_from = wert)%>%
      dplyr::mutate(across(`Anzahl in Tsd.`, ~ as.numeric(.)*1000))%>%
      dplyr::rename(display_total = "Anzahl in Tsd." )%>%
      dplyr::select(display_total, land)%>%
      dplyr::mutate(across(display_total, ~ prettyNum(., big.mark = ".", decimal.mark = ",")))

    # Realtiv und absolut zusammenführen
    data_map <- data1 %>%
      dplyr::left_join(data2,by=c("land"))%>%
      dplyr::left_join(countries_names %>%
                         dplyr::mutate(land=dplyr::case_when(land == "Tschechien" ~ "Tschechische Republik",
                                                      T ~ .$land)), by= "land")%>%
      dplyr::mutate(alpha2= toupper(alpha2))



    # Titel vorbereiten
    title_eu <- paste0(inpp, "n", " in MINT-Fächern an allen ", inpp, "n ",inpy )



      # plot
      df <- data_map
      joinby <- c("hc-a2", "alpha2")
      name <- paste0(inpp)
      tooltip <-"{point.land} <br> Anteil: {point.display_rel}% <br> Anzahl: {point.display_total}"
      titel <- paste0("Anteil von ", title_eu, " in Europa")
      mincolor <- "#f4f5f6"
      maxcolor <- "#b16fab"
      map <- map_selection
      quell <- "Quelle der Daten: Eurostat, 2023; OECD, 2023; freier Download, eigene Berechnungen durch MINTvernetzt."
      out1 <- mapbuilder(df, joinby,name, tooltip, titel, mincolor, maxcolor, prop=FALSE, wert=TRUE, map=map, quelle=quell)

  }


  # Falls Region OECD ist
  else if (map_l== "OECD"){

    # Kartenausschnitt
    map_selection <- "custom/world"

    # ui inputs laden
    inpp <- r$map_pers_arb
    inpy <- r$map_y_arb
    inpf <- r$map_f_arb


    # falls indiktoren aus datensatz arbeitsmarkt_anfänger_absolv_oecd gewählt werden
    if(inpp %in%  c("Anfänger*innen Ausbildung (ISCED 45)",
       "Anfänger*innen Erstausbildung (ISCED 35)",
       "Absolvent*innen Ausbildung (ISCED 45)",
       "Absolvent*innen Erstausbildung (ISCED 35)")){

      df_query <- glue::glue_sql("
      SELECT *
      FROM jahr = {inpy}
      AND fachbereich IN ('MINT','Informatik & Kommunikationstechnologie','Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe','Naturwissenschaften, Mathematik und Statistik','Alle')
      AND geschlecht = 'Gesamt'
                               ", .con = con)

      data1 <- DBI::dbGetQuery(con, df_query)

      data1 <- data1 %>%
        dplyr::mutate(display_rel= prettyNum(round(.$wert, 1), big.mark = ".", decimal.mark = ","))


      if(inpp == "Anfänger*innen Ausbildung (ISCED 45)"){

        # fitlern für spezifischen fachbereich und mit geo mapping joinen
        data_map <- data1 %>%
          dplyr::filter(anforderung == "Ausbildung (ISCED 45)" &
                          variable == "Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern" &
                          fachbereich == inpf)%>%
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))


      } else if (inpp == "Anfänger*innen Erstausbildung (ISCED 35)"){

        # fitlern für spezifischen fachbereich und mit geo mapping joinen
        data_map <- data1 %>%
          dplyr::filter(anforderung == "Erstausbildung (ISCED 35)" &
                          variable == "Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern"&
                          fachbereich == inpf)%>%
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))

      } else if (inpp == "Absolvent*innen Ausbildung (ISCED 45)"){

        # fitlern für spezifischen fachbereich und mit geo mapping joinen
        data_map <- data1 %>%
          dplyr::filter(anforderung == "Ausbildung (ISCED 45)" &
                          variable == "Anteil Absolvent*innen nach Fach an allen Fächern"&
                          fachbereich == inpf)%>%
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))

      } else if (inpp == "Absolvent*innen Erstausbildung (ISCED 35)"){

        # fitlern für spezifischen fachbereich und mit geo mapping joinen
        data_map <- data1 %>%
          dplyr::filter(anforderung == "Erstausbildung (ISCED 35)" &
                          variable == "Anteil Absolvent*innen nach Fach an allen Fächern"&
                          fachbereich == inpf)%>%
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))

      }

# titel vorbereiten
  title_oecd_1_1 <- if(inpp == "Anfänger*innen Ausbildung (ISCED 45)"){
    paste0("Ausbildungsanfänger*innen (ISCED 45)")
  }else if(inpp =="Anfänger*innen Erstausbildung (ISCED 35)"){
    paste0("Anfänger*innen in Erstausbildung (ISCED 35)")
  }else if(inpp =="Absolvent*innen Ausbildung (ISCED 45)"){
    paste0("Ausbildungsabsolvent*innen (ISCED 45)")
  }else if(inpp =="Absolvent*innen Erstausbildung (ISCED 35)"){
    paste0("Absolvent*innen der Erstausbildung (ISCED 35)")
  }

        # plot


        df <- data_map
        joinby <- c("hc-a2", "alpha2")
        name <- paste0(inpp)
        tooltip <-"{point.land} <br> Anteil: {point.display_rel}%"
        titel <- paste0("Anteil von ", title_oecd_1_1, " in ", inpf, " ", inpy, " weltweit (OECD)" )
        mincolor <- "#f4f5f6"
        maxcolor <- "#b16fab"
        que <- "Quelle der Daten: Eurostat, 2023; OECD, 2023; freier Download, eigene Berechnungen durch MINTvernetzt."
        out1 <- mapbuilder(df, joinby,name, tooltip, titel, mincolor, maxcolor, prop=FALSE, wert=TRUE, map=map_selection, quelle=que)



    }

    # Falls Indikatoren aus datensatz arbeitsmarkt_anzahl_azubis_oecd stammen
    else {


      df_query <- glue::glue_sql("
      SELECT *
      FROM arbeitsmarkt_anzahl_azubis_oecd
      WHERE geschlecht = 'Gesamt'
      AND indikator = 'berufsorientiert'
      AND jahr = {inpy}
      AND fach IN ('MINT','Informatik & Kommunikationstechnologie', 'Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe','Naturwissenschaften, Mathematik und Statistik','Alle')
                               ", .con = con)

      data1 <- DBI::dbGetQuery(con, df_query)

      data1 <- data1 %>%
        tidyr::pivot_wider(values_from = wert, names_from = fach)%>%
        # relative häufigkeit des faches errechnen und runden
        dplyr::mutate(across(c("MINT",
                             "Informatik & Kommunikationstechnologie",
                             "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                             "Naturwissenschaften, Mathematik und Statistik"), ~ round(./Alle*100,1)))%>%
        dplyr::select(-Alle)%>%
        # Zurückpivoten
        tidyr::pivot_longer(c("MINT",
                              "Informatik & Kommunikationstechnologie",
                              "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                              "Naturwissenschaften, Mathematik und Statistik"), values_to = "wert",
                            names_to = "fach") %>%
        # daten für hover vorbereiten
        dplyr::mutate(display_rel= prettyNum(.$wert, big.mark = ".", decimal.mark = ","))

      df_query <- glue::glue_sql("
      SELECT *
      FROM arbeitsmarkt_anzahl_azubis_oecd
      WHERE geschlecht = 'Gesamt'
      AND indikator = 'berufsorientiert'
      AND jahr = {inpy}
      AND fach IN ('MINT','Informatik & Kommunikationstechnologie', 'Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe','Naturwissenschaften, Mathematik und Statistik','Alle')
                               ", .con = con)

      data2 <- DBI::dbGetQuery(con, df_query)




      data2 <- data2 %>%
        dplyr::mutate(display_total= prettyNum(.$display_total, big.mark = ".", decimal.mark = ","))%>%
        dplyr::select(land, jahr, display_total, fach, anforderung)


      # Absolute und relative Häufigkeit zusammenführen und mit geo mapping erweitern
      data3 <- data1 %>%
        dplyr::left_join(data2, by=c("land", "jahr", "fach", "anforderung"))%>%
        dplyr::inner_join(countries_names, by = "land") %>%
        dplyr::mutate(alpha2 = toupper(alpha2))


      # Filtern für spezifisches Fach und Anforderung
      if (inpp == "Auszubildende (ISCED 45)"){

        data_map <- data3 %>%
          dplyr::filter(anforderung=="Ausbildung (ISCED 45)"&
                          fach == inpf)

      } else if(inpp == "Auszubildende in Erstausbildung (ISCED 35)"){

        data_map <- data3 %>%
          dplyr::filter(anforderung=="Erstausbildung (ISCED 35)"&
                          fach == inpf)

      } else if(inpp == "In Meisterlehre (< 880 Std. Vorbereitung, ISCED 55)") {

        data_map <- data3 %>%
          dplyr::filter(anforderung=="kurzes tertiäres Bildungsprogramm (berufsorientiert)"&
                          fach == inpf)

      } else if(inpp == "In Meister-/Technikerlehre (> 880 Std. Vorbereitung, ISCED 65)"){

        data_map <- data3 %>%
          dplyr::filter(anforderung== "Bachelor oder vergleichbar (berufsorientiert)"&
                          fach == inpf)

      }



      # titel vorbereiten
      title_oecd_2_1 <- if(inpp == "Auszubildende (ISCED 45)"){
          paste0("Auszubildenden (ISCED 45)")
      } else if(inpp == "In Meisterlehre (< 880 Std. Vorbereitung, ISCED 55)"){
          paste0("Meisterlehrlingen (< 880 Std. Vorbereitung, ISCED 55)")
      } else if(inpp == "Auszubildende in Erstausbildung (ISCED 35)"){
          paste0("Auszubildenden in Erstausbildung (ISCED 35)")
      }else if (inpp == "In Meister-/Technikerlehre (> 880 Std. Vorbereitung, ISCED 65)"){
          paste0("Meister-/Technikerlehrlingen (> 880 Std. Vorbereitung, ISCED 65)")
      }



        # plot


        df <- data_map
        joinby <- c("hc-a2", "alpha2")
        name <- paste0(inpp)
        tooltip <- "{point.land} <br> Anteil: {point.display_rel}% <br> Anzahl: {point.display_total}"
        titel <- paste0("Anteil von ", title_oecd_2_1, " in ", inpf, " ",  inpy," weltweit (OECD)" )
        mincolor <- "#f4f5f6"
        maxcolor <- "#b16fab"
        que <- "Quelle der Daten: Eurostat, 2023; OECD, 2023; freier Download, eigene Berechnungen durch MINTvernetzt."
        out1 <- mapbuilder(df, joinby,name, tooltip, titel, mincolor, maxcolor, prop=FALSE, wert=TRUE, map=map_selection, quelle=que)



      }



  }




}

plot_international_map_arb_gender <- function(r) {


  # ui input für Region laden
  inpl <- r$map_l_arb_gender

  # Falls Region EU ist:
  if(inpl== "EU"){

    #Spezifische ui inputs laden
    inpy <- r$map_y_arb_gender
    inpp <- r$map_pers_arb_gender

    # Kartenabschnitt für hc
    map_selection <- "custom/europe"


    df_query <- glue::glue_sql("
    SELECT *
    FROM arbeitsmarkt_beschaeftigte_eu
    WHERE geschlecht IN ('Gesamt','Frauen')
    AND jahr = {inpy}
    AND indikator = {inpp}
    AND variable = 'Anteil an arbeitender Bevölkerung'
                               ", .con = con)

    data1 <- DBI::dbGetQuery(con, df_query)


    data1 <- data1 %>%
      tidyr::pivot_wider(names_from = geschlecht, values_from = wert)%>%
      dplyr::rename(wert="Frauen")%>%
      dplyr::select(-Gesamt)

    # Wert für hover vorbereiten
    data1$display_rel <- prettyNum(data1$wert, big.mark = ".", decimal.mark = ",")



    df_query <- glue::glue_sql("
    SELECT *
    FROM arbeitsmarkt_beschaeftigte_eu
    WHERE geschlecht = 'Frauen'
    AND jahr = {inpy}
    AND indikator = {inpp}
    AND variable = 'Anzahl in Tsd.'
                               ", .con = con)

    data2 <- DBI::dbGetQuery(con, df_query)

    data2 <- data2 %>%
      tidyr::pivot_wider(names_from = variable, values_from = wert)%>%
      dplyr::mutate(across(`Anzahl in Tsd.`, ~ as.numeric(.)*1000))%>%
      dplyr::rename(display_total = "Anzahl in Tsd." )%>%
      dplyr::select(display_total, land)%>%
      dplyr::mutate(across(display_total, ~ prettyNum(., big.mark = ".", decimal.mark = ",")))

    # Absolut und relativ zusammenführen und mit geo mapping vereinen
    data_map <- data1 %>%
      dplyr::left_join(data2,by=c("land"))%>%
      dplyr::left_join(countries_names %>%
                         dplyr::mutate(land=dplyr::case_when(land == "Tschechien" ~ "Tschechische Republik",
                                                             T ~ .$land)), by= "land")%>%
      dplyr::mutate(alpha2= toupper(alpha2))

    # Tielt vorbereiten
    title_eu <- paste0(inpp, "n", " in allen MINT-Fächern ", inpy )


      # plot


      df <- data_map
      joinby <- c("hc-a2", "alpha2")
      name <- paste0(inpp)
      tooltip <- "{point.land} <br> Anteil: {point.display_rel}% <br> Anzahl: {point.display_total}"
      titel <- paste0("Anteil von Frauen an allen ", title_eu, " in Europa")
      mincolor <- "#f4f5f6"
      maxcolor <- "#b16fab"
      map <- map_selection
      quelle <- "Quelle der Daten: Eurostat, 2023; OECD, 2023; freier Download, eigene Berechnungen durch MINTvernetzt."
      out1 <- mapbuilder(df, joinby,name, tooltip, titel, mincolor, maxcolor, prop=FALSE, wert=TRUE, map=map, quelle = quelle)



  }

  # Falls Region OECD ist:

  else if (inpl== "OECD"){

    # Kartenausschnitt für hc
    map_selection <- "custom/world"

    # ui inputs für oecd laden
    inpp <- r$map_pers_arb_gender
    inpy <- r$map_y_arb_gender
    inpf <- r$map_f_arb_gender


    # Falls indiktoren aus datensatz arbeitsmarkt_anfänger_absolv_oecd gewählt werden
    if(inpp %in%  c("Anfänger*innen Ausbildung (ISCED 45)",
                    "Anfänger*innen Erstausbildung (ISCED 35)",
                    "Absolvent*innen Ausbildung (ISCED 45)",
                    "Absolvent*innen Erstausbildung (ISCED 35)")){

      #
      df_query <- glue::glue_sql("
      SELECT *
      FROM arbeitsmarkt_anfaenger_absolv_oecd
      WHERE jahr = {inpy}
            AND fach IN ('MINT', 'Informatik & Kommunikationstechnologie','Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe','Naturwissenschaften, Mathematik und Statistik','Alle')
            AND geschecht = 'Frauen'
                               ", .con = con)

      data1 <- DBI::dbGetQuery(con, df_query)



      data1 <- data1 %>%
        tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
        # MINT Anteil errechen: Summe der relativen Häufigekeiten d. Einzelfächer geteilt durc hdie Anzahl der Einzelfcher
        # d.h. Druchscnitt der relativen Häufigkeit der Einzelfächer
        dplyr::mutate(MINT = (rowSums(dplyr::select(., "Informatik & Kommunikationstechnologie",
                                                    "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                                    "Naturwissenschaften, Mathematik und Statistik"), na.rm = T))/3)%>%
        tidyr::pivot_longer(c("MINT",
                              "Informatik & Kommunikationstechnologie",
                              "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                              "Naturwissenschaften, Mathematik und Statistik",
                              "Alle"), values_to = "wert", names_to = "fachbereich")%>%
        # Wert für hover vorbereiten
        dplyr::mutate(display_rel= prettyNum(.$wert, big.mark = ".", decimal.mark = ","))

      # Für spezifsiche Indikatoren filtern und mit geo mapping erweitern und wert für hover vorbereiten
      if(inpp == "Anfänger*innen Ausbildung (ISCED 45)"){

        data_map <- data1 %>%
          dplyr::filter(anforderung == "Ausbildung (ISCED 45)" &
                          variable == "Frauen-/Männeranteil Ausbildungs-/Studiumsanfänger*innen nach Fachbereichen" &
                          fachbereich == inpf)%>%
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))

        data_map$display_rel <- prettyNum(round(data_map$wert,1), big.mark = ".", decimal.mark = ",")


      } else if (inpp == "Anfänger*innen Erstausbildung (ISCED 35)"){

        data_map <- data1 %>%
          dplyr::filter(anforderung == "Erstausbildung (ISCED 35)" &
                          variable == "Frauen-/Männeranteil Ausbildungs-/Studiumsanfänger*innen nach Fachbereichen"&
                          fachbereich == inpf)%>%
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))

        data_map$display_rel <- prettyNum(round(data_map$wert,1), big.mark = ".", decimal.mark = ",")

      } else if (inpp == "Absolvent*innen Ausbildung (ISCED 45)"){

        data_map <- data1 %>%
          dplyr::filter(anforderung == "Ausbildung (ISCED 45)" &
                          variable == "Frauen-/Männeranteil Absolvent*innen nach Fachbereichen"&
                          fachbereich == inpf)%>%
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))

        data_map$display_rel <- prettyNum(round(data_map$wert,1), big.mark = ".", decimal.mark = ",")

      } else if (inpp == "Absolvent*innen Erstausbildung (ISCED 35)"){

        data_map <- data1 %>%
          dplyr::filter(anforderung == "Erstausbildung (ISCED 35)" &
                          variable == "Frauen-/Männeranteil Absolvent*innen nach Fachbereichen"&
                          fachbereich == inpf)%>%
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))

        data_map$display_rel <- prettyNum(round(data_map$wert,1), big.mark = ".", decimal.mark = ",")

      }


      # Titel vorbereiten
      title_oecd_1_1 <- if(inpp == "Anfänger*innen Ausbildung (ISCED 45)"){
        paste0("Ausbildungsanfänger*innen (ISCED 45)")
      }else if(inpp =="Anfänger*innen Erstausbildung (ISCED 35)"){
        paste0("Anfänger*innen in Erstausbildung (ISCED 35)")
      }else if(inpp =="Absolvent*innen Ausbildung (ISCED 45)"){
        paste0("Ausbildungsabsolvent*innen (ISCED 45)")
      }else if(inpp =="Absolvent*innen Erstausbildung (ISCED 35)"){
        paste0("Absolvent*innen der Erstausbildung (ISCED 35)")
      }


        # plot


        df <- data_map
        joinby <- c("hc-a2", "alpha2")
        name <- paste0(inpp)
        tooltip <- "{point.land} <br> Anteil: {point.display_rel}%"
        titel <- paste0("Anteil von Frauen an allen ", title_oecd_1_1, " in ", inpf, " ", inpy, " weltweit (OECD)")
        mincolor <- "#f4f5f6"
        maxcolor <- "#b16fab"
        qk <- "Quelle der Daten: Eurostat, 2023; OECD, 2023; freier Download, eigene Berechnungen durch MINTvernetzt."
        out1 <- mapbuilder(df, joinby,name, tooltip, titel, mincolor, maxcolor, prop=FALSE, wert=TRUE, map=map_selection, quelle=qk)





    } # Falls indikator aus arbeitsmarkt_anzahl_azubis_oecd gewählt wird
    else {

      # ui input für Betrachtungsweise filtern
      inpbe <- r$map_betr_oecd_arb_gender



      #
      df_query <- glue::glue_sql("
      SELECT *
      FROM arbeitsmarkt_anzahl_azubis_oecd
      WHERE geschlecht IN ('Gesamt', 'Frauen')
      AND indikator = 'berufsorientiert'
      AND jahr = {inpy}
      AND fach IN ('MINT', 'Informatik & Kommunikationstechnologie','Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe','Naturwissenschaften, Mathematik und Statistik','Alle')
                               ", .con = con)

      data_fva <- DBI::dbGetQuery(con, df_query)


      data_fva <- data_fva %>%
        tidyr::pivot_wider(values_from = wert, names_from = geschlecht)%>%
        # Errechnen der relativen Häufigkeit
        dplyr::mutate(wert= round(Frauen/Gesamt *100,1))%>%
        # Werte für hover vorbereiten
        dplyr::mutate(display_rel= prettyNum(.$wert, big.mark = ".", decimal.mark = ","),
                      display_total= prettyNum(.$Frauen, big.mark = ".", decimal.mark = ","))%>%
        dplyr::select(-Gesamt, - Frauen)%>%
        # Mit geo mapping erweitern
        dplyr::inner_join(countries_names, by = "land") %>%
        dplyr::mutate(alpha2 = toupper(alpha2))



      df_query <- glue::glue_sql("
      SELECT *
      FROM arbeitsmarkt_anzahl_azubis_oecd
      WHERE geschlecht = 'Frauen'
      AND indikator = 'berufsorientiert'
      AND jahr = {inpy}
            AND fach IN ('MINT', 'Informatik & Kommunikationstechnologie','Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe','Naturwissenschaften, Mathematik und Statistik','Alle')
                               ", .con = con)

      data_fvf1 <- DBI::dbGetQuery(con, df_query)

      data_fvf1 <- data_fvf1 %>%
        tidyr::pivot_wider(values_from = wert, names_from = fach)%>%
        # Relative Häufigkeit errechenn
        dplyr::mutate(across(c("MINT",
                               "Informatik & Kommunikationstechnologie",
                               "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                               "Naturwissenschaften, Mathematik und Statistik"), ~ round(./Alle*100,1)))%>%
        dplyr::select(-Alle)%>%
        tidyr::pivot_longer(c("MINT",
                              "Informatik & Kommunikationstechnologie",
                              "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                              "Naturwissenschaften, Mathematik und Statistik"), values_to = "wert",
                            names_to = "fach") %>%
        # Wert für hover vorbereiten
        dplyr::mutate(display_rel= prettyNum(.$wert, big.mark = ".", decimal.mark = ","))


      df_query <- glue::glue_sql("
      SELECT *
      FROM arbeitsmarkt_anzahl_azubis_oecd
      WHERE geschlecht = 'Frauen'
      AND indikator = 'berufsorientiert'
      AND jahr = {inpy}
            AND fach IN ('MINT', 'Informatik & Kommunikationstechnologie','Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe','Naturwissenschaften, Mathematik und Statistik','Alle')
                               ", .con = con)

      data_fvf2 <- DBI::dbGetQuery(con, df_query)

      data_fvf2 <- data_fvf2 %>%
        dplyr::rename(display_total = wert)







        # Wert für hover vorbereiten
      data_fvf2 <- data_fvf2 %>%
        dplyr::mutate(display_total= prettyNum(.$display_total, big.mark = ".", decimal.mark = ","))%>%
        dplyr::select(land, jahr, display_total, fach, anforderung)


      # Relative, abslute Häufigkeit und geo mapping zusammenführen
      data_fvf3 <- data_fvf1 %>%
        dplyr::left_join(data_fvf2, by=c("land", "jahr", "fach", "anforderung"))%>%
        dplyr::inner_join(countries_names, by = "land") %>%
        dplyr::mutate(alpha2 = toupper(alpha2))


      # Für die erste Betrachtungsweise
      if (inpbe == "Anteil von Frauen an Allen"){

        # Daten zuweisen
        data1 <- data_fva

        # Titel vorbereiten
        title_oecd_2_1 <- if(inpp == "Auszubildende (ISCED 45)"){
          paste0("weiblichen Auszubildenden (ISCED 45) an allen Auszubildenden in ", inpf)
        } else if(inpp == "In Meisterlehre (< 880 Std. Vorbereitung, ISCED 55)"){
          paste0("weiblichen Meisterlehrlingen (< 880 Std. Vorbereitung, ISCED 55) an allen Meisterlehrlingen in ", inpf)
        } else if(inpp == "Auszubildende in Erstausbildung (ISCED 35)"){
          paste0("weiblichen Auszubildenden in Erstausbildung (ISCED 35) an allen Auszubildenden in Erstausbildung in ", inpf)
        }else if (inpp == "In Meister-/Technikerlehre (> 880 Std. Vorbereitung, ISCED 65)"){
          paste0("weiblichen Meister-/Technikerlehrlingen (> 880 Std. Vorbereitung, ISCED 65) an allen Meister-/Technikerlehrlingen in ", inpf)
        }

      } # Falls zwite Betrachtungsweise gewählt wird
      else if(inpbe == "Anteil an Frauen von Frauen"){


        # Daten zuweisen
        data1 <- data_fvf3

        # Titel vorbereiten
        title_oecd_2_1 <- if(inpp == "Auszubildende (ISCED 45)"){
          paste0("Auszubildenden (ISCED 45) in ", inpf, " an allen weiblichen Auszubildenden")
        } else if(inpp == "In Meisterlehre (< 880 Std. Vorbereitung, ISCED 55)"){
          paste0("Meisterlehrlingen (< 880 Std. Vorbereitung, ISCED 55) in ", inpf, " an allen weiblichen Meisterlehrlingen")
        } else if(inpp == "Auszubildende in Erstausbildung (ISCED 35)"){
          paste0("Auszubildenden in Erstausbildung (ISCED 35) in ", inpf, " an allen weiblichen Auszubildenden in Erstausbildung")
        }else if (inpp == "In Meister-/Technikerlehre (> 880 Std. Vorbereitung, ISCED 65)"){
          paste0("Meister-/Technikerlehrlingen (> 880 Std. Vorbereitung, ISCED 65) in ",inpf, " allen weiblichen Meister-/Technikerlehrlingen" )
        }
      }


      # Für spezifischere Indikator filtern
      if (inpp == "Auszubildende (ISCED 45)"){

        data_map <- data1 %>%
          dplyr::filter(anforderung=="Ausbildung (ISCED 45)"&
                          fach == inpf)


      } else if(inpp == "Auszubildende in Erstausbildung (ISCED 35)"){

        data_map <- data1 %>%
          dplyr::filter(anforderung=="Erstausbildung (ISCED 35)"&
                          fach == inpf)

      } else if(inpp == "In Meisterlehre (< 880 Std. Vorbereitung, ISCED 55)") {

        data_map <- data1 %>%
          dplyr::filter(anforderung=="kurzes tertiäres Bildungsprogramm (berufsorientiert)"&
                          fach == inpf)

      } else if(inpp == "In Meister-/Technikerlehre (> 880 Std. Vorbereitung, ISCED 65)"){

        data_map <- data1 %>%
          dplyr::filter(anforderung== "Bachelor oder vergleichbar (berufsorientiert)"&
                          fach == inpf)

      }


        # plot

        df <- data_map
        joinby <- c("hc-a2", "alpha2")
        name <- paste0(inpp)
        tooltip <- "{point.land} <br> Anteil: {point.display_rel}% <br> Anzahl: {point.display_total}"
        titel <- paste0("Anteil von ", title_oecd_2_1, " ", inpy, " weltweit (OECD)" )
        mincolor <- "#f4f5f6"
        maxcolor <- "#b16fab"
        quelle <- "Quelle der Daten: Eurostat, 2023; OECD, 2023; freier Download, eigene Berechnungen durch MINTvernetzt."
        out1 <- mapbuilder(df, joinby,name, tooltip, titel, mincolor, maxcolor, prop=FALSE, wert=TRUE, map=map_selection, quelle = quelle)


    }



  }




}

plot_international_top10_mint_arb <- function(r) {

  # ui input für Region laden
  map_l <- r$map_l_top10_mint_arb

  # falls die Region EU ist:
  if(map_l== "EU"){

    # EU spezifische ui inputs laden
    inpy <- r$map_y_eu_top10_mint_arb
    inpp <- r$map_pers_eu_top10_mint_arb

    # Kartenauschnitt für hc
    map_selection <- "custom/europe"

    df_query <- glue::glue_sql("
    SELECT *
    FROM arbeitsmarkt_beschaeftigte_eu
    WHERE geschlecht = 'Gesamt'
    AND jahr = {inpy}
    AND indikator = {inpp}
    AND variable = 'Anteil an arbeitender Bevölkerung'
                               ", .con = con)

    data1 <- DBI::dbGetQuery(con, df_query)

    data1 <- data1 %>%
      tidyr::pivot_wider(names_from = variable, values_from = wert)%>%
      dplyr::rename(wert="Anteil an arbeitender Bevölkerung")%>%
      dplyr::mutate(display_rel=prettyNum(.$wert, big.mark = ".", decimal.mark = ","))


    df_query <- glue::glue_sql("
    SELECT *
    FROM arbeitsmarkt_beschaeftigte_eu
    WHERE geschlecht = 'Gesamt'
    AND jahr = {inpy}
    AND indikator = {inpp}
    AND variable = 'Anzahl in Tsd.'
                               ", .con = con)

    data2 <- DBI::dbGetQuery(con, df_query)





    data2 <- data2 %>%
      tidyr::pivot_wider(names_from = variable, values_from = wert)%>%
      dplyr::mutate(across(`Anzahl in Tsd.`, ~ as.numeric(.)*1000))%>%
      dplyr::rename(display_total = "Anzahl in Tsd." )%>%
      dplyr::select(display_total, land)%>%
      dplyr::mutate(across(display_total, ~ prettyNum(., big.mark = ".", decimal.mark = ",")))

    # Relative, absolut und geo mapping zusammenführen
    data_fn <- data1 %>%
      dplyr::left_join(data2,by=c("land"))%>%
      dplyr::left_join(countries_names %>%
                         dplyr::mutate(land=dplyr::case_when(land == "Tschechien" ~ "Tschechische Republik",
                                                             T ~ .$land)), by= "land")%>%
      dplyr::mutate(alpha2= toupper(alpha2))%>%
      dplyr::filter(!is.na(.$wert) & wert!=0)

    # Hover vorbereiten
    plotopshov <- "Anteil: {point.display_rel}% <br> Anzahl: {point.display_total}"

# Titel vorbereiten
title_top <- paste0("Länder Europas mit dem höchsten Anteil von ", inpp, "n in MINT an allen ", inpp, "n ", inpy )
title_bot <- paste0("Länder Europas mit dem niedrigsten Anteil von ", inpp, "n in MINT an allen ", inpp, "n ",  inpy )

  }


  # Falls die Region OECD ist
  else if (map_l== "OECD"){

    # Kartenausschnitt
    map_selection <- "custom/world"

    # Für OECD spezifischen Indikator laden
    inpp <- r$map_pers_oecd_top10_mint_arb

    # Falls indiktoren aus datensatz arbeitsmarkt_anfänger_absolv_oecd gewählt werden
    if(inpp %in%  c("Anfänger*innen Ausbildung (ISCED 45)",
                    "Anfänger*innen Erstausbildung (ISCED 35)",
                    "Absolvent*innen Ausbildung (ISCED 45)",
                    "Absolvent*innen Erstausbildung (ISCED 35)")){

      # ui inputs laden
      inpy <- r$map_y_oecd_top10_mint_arb
      inpf <- r$map_f_oecd_top10_mint_arb

      # hover vorbereiten
      plotopshov <- "Anteil: {point.display_rel}%"


      df_query <- glue::glue_sql("
      SELECT *
      FROM arbeitsmarkt_anfaenger_absolv_oecd
      WHERE jahr = {inpy}
      AND fachbereich IN ('MINT', 'Informatik & Kommunikationstechnologie', 'Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe', 'Naturwissenschaften, Mathematik und Statistik', 'Alle')
      AND geschlecht = 'Gesamt'
                               ", .con = con)

      data1 <- DBI::dbGetQuery(con, df_query)


      data1 <- data1 %>%
        dplyr::mutate(display_rel=prettyNum(round(.$wert,1), big.mark = ".", decimal.mark = ","))%>%
        dplyr::filter(!is.na(.$wert) & wert!=0)  # Nullen und NAs raus




      # Für spezifsiche Indikatoren filtern und wert für hover vorbetreiten
      if(inpp == "Anfänger*innen Ausbildung (ISCED 45)"){

        data_fn <- data1 %>%
          dplyr::filter(anforderung == "Ausbildung (ISCED 45)" &
                          variable == "Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern" &
                          fachbereich == inpf)%>%
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))


      } else if (inpp == "Anfänger*innen Erstausbildung (ISCED 35)"){

        data_fn <- data1 %>%
          dplyr::filter(anforderung == "Erstausbildung (ISCED 35)" &
                          variable == "Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern"&
                          fachbereich == inpf)%>%
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))

      } else if (inpp == "Absolvent*innen Ausbildung (ISCED 45)"){

        data_fn <- data1 %>%
          dplyr::filter(anforderung == "Ausbildung (ISCED 45)" &
                          variable == "Anteil Absolvent*innen nach Fach an allen Fächern"&
                          fachbereich == inpf)%>%
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))

      } else if (inpp == "Absolvent*innen Erstausbildung (ISCED 35)"){

        data_fn <- data1 %>%
          dplyr::filter(anforderung == "Erstausbildung (ISCED 35)" &
                          variable == "Anteil Absolvent*innen nach Fach an allen Fächern"&
                          fachbereich == inpf)%>%
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))

      }

    # Titel vorbereiten

    # Top
    title_top <- if (inpp == "Anfänger*innen Ausbildung (ISCED 45)"){
      paste0("Länder (weltweit, OECD) mit dem höchsten Anteil von Ausbildungsanfänger*innen (ISCED 45) in ",
             inpf, " an allen Ausbildungsanfänger*innen ", inpy)
    } else if(inpp =="Anfänger*innen Erstausbildung (ISCED 35)"){
      paste0("Länder (weltweit, OECD) mit dem höchsten Anteil von Anfänger*innen in Erstausbildung (ISCED 35) in ",
             inpf, " an allen Anfänger*innen in Erstausbildung ", inpy)
    }else if(inpp =="Absolvent*innen Ausbildung (ISCED 45)"){
      paste0("Länder (weltweit, OECD) mit dem höchsten Anteil von Ausbildungsabsolvent*innen (ISCED 45) in ",
             inpf, " an allen Ausbildungsabsolvent*innen ", inpy)
    }else if (inpp =="Absolvent*innen Erstausbildung (ISCED 35)"){
      paste0("Länder (weltweit, OECD) mit dem höchsten Anteil von Absolvent*innen der Erstausbildung (ISCED 35) in ",
             inpf, " an allen Absolvent*innen der Erstausbildung", inpy)
    }

    # Bottom
    title_bot <- if (inpp == "Anfänger*innen Ausbildung (ISCED 45)"){
      paste0("Länder (weltweit, OECD) mit dem niedrigsten Anteil von Ausbildungsanfänger*innen (ISCED 45) in ",
             inpf, " an allen Ausbildungsanfänger*innen ", inpy)
    } else if(inpp =="Anfänger*innen Erstausbildung (ISCED 35)"){
      paste0("Länder (weltweit, OECD) mit dem niedrigsten Anteil von Anfänger*innen in Erstausbildung (ISCED 35) in ",
             inpf, " an allen Anfänger*innen in Erstausbildung ", inpy)
    }else if(inpp =="Absolvent*innen Ausbildung (ISCED 45)"){
      paste0("Länder (weltweit, OECD) mit dem niedrigsten Anteil von Ausbildungsabsolvent*innen (ISCED 45) in ",
             inpf, " an allen Ausbildungsabsolvent*innen ", inpy)
    }else if (inpp =="Absolvent*innen Erstausbildung (ISCED 35)"){
      paste0("Länder (weltweit, OECD) mit dem niedrigsten Anteil von Absolvent*innen der Erstausbildung (ISCED 35) in ",
             inpf, " an allen Absolvent*innen der Erstausbildung", inpy)
    }





    } # Falls indikator aus arbeitsmarkt_anzahl_azubis_oecd gewählt wird
    else {

      # Spezifische ui inputs laden
      inpy <- r$map_y_oecd2_top10_mint_arb
      inpf <- r$map_f_oecd2_top10_mint_arb

      # Hover vorbereiten
      plotopshov <- "Anteil: {point.display_rel}% <br> Anzahl: {point.display_total}"



      df_query <- glue::glue_sql("
      SELECT *
      FROM arbeitsmarkt_anzahl_azubis_oecd
      WHERE jahr = {inpy}
      AND indikator = 'berufsorientiert'
      AND fachbereich IN ('MINT', 'Informatik & Kommunikationstechnologie', 'Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe', 'Naturwissenschaften, Mathematik und Statistik', 'Alle')
      AND geschlecht = 'Gesamt'
                               ", .con = con)

      data1 <- DBI::dbGetQuery(con, df_query)


      data1 <- data1 %>%
        tidyr::pivot_wider(values_from = wert, names_from = fach)%>%
        dplyr::mutate(across(c("MINT",
                               "Informatik & Kommunikationstechnologie",
                               "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                               "Naturwissenschaften, Mathematik und Statistik"), ~ round(./Alle*100,1)))%>%
        dplyr::select(-Alle)%>%
        tidyr::pivot_longer(c("MINT",
                              "Informatik & Kommunikationstechnologie",
                              "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                              "Naturwissenschaften, Mathematik und Statistik"), values_to = "wert",
                            names_to = "fach") %>%
        dplyr::mutate(display_rel= prettyNum(.$wert, big.mark = ".", decimal.mark = ","))


      df_query <- glue::glue_sql("
      SELECT *
      FROM arbeitsmarkt_anzahl_azubis_oecd
      WHERE jahr = {inpy}
      AND indikator = 'berufsorientiert'
      AND fachbereich IN ('MINT', 'Informatik & Kommunikationstechnologie', 'Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe', 'Naturwissenschaften, Mathematik und Statistik', 'Alle')
      AND geschlecht = 'Gesamt'
                               ", .con = con)

      data2 <- DBI::dbGetQuery(con, df_query)

      data2 <- data2 %>%
        dplyr::rename(display_total = wert)





      data2 <- data2 %>%
        dplyr::mutate(display_total= prettyNum(.$display_total, big.mark = ".", decimal.mark = ","))%>%
        dplyr::select(land, jahr, display_total, fach, anforderung)


      # Zusammenführen und geo mapping
      data3 <- data1 %>%
        dplyr::left_join(data2, by=c("land", "jahr", "fach", "anforderung"))%>%
        dplyr::inner_join(countries_names, by = "land") %>%
        dplyr::mutate(alpha2 = toupper(alpha2))%>%
        dplyr::filter(!is.na(.$wert) & wert!=0)



      # nach spezifischen Indikatoren filtern
      if (inpp == "Auszubildende (ISCED 45)"){

        data_fn <- data3 %>%
          dplyr::filter(anforderung=="Ausbildung (ISCED 45)"&
                          fach == inpf)



      } else if(inpp == "Auszubildende in Erstausbildung (ISCED 35)"){

        data_fn <- data3 %>%
          dplyr::filter(anforderung=="Erstausbildung (ISCED 35)"&
                          fach == inpf)

      } else if(inpp == "In Meisterlehre (< 880 Std. Vorbereitung, ISCED 55)") {

        data_fn <- data3 %>%
          dplyr::filter(anforderung=="kurzes tertiäres Bildungsprogramm (berufsorientiert)"&
                          fach == inpf)

      } else if(inpp == "In Meister-/Technikerlehre (> 880 Std. Vorbereitung, ISCED 65)"){

        data_fn <- data3 %>%
          dplyr::filter(anforderung== "Bachelor oder vergleichbar (berufsorientiert)"&
                          fach == inpf)

      }


    # Titel vorbereiten

    # Top

    title_top <- if (inpp == "Auszubildende (ISCED 45)"){
      paste0("Länder (weltweit, OECD) mit dem höchsten Anteil von Auszubildenden (ISCED 45) in ",
             inpf, " an allen Auszubildenden (ISCED 45) ", inpy)
    } else if(inpp =="In Meisterlehre (< 880 Std. Vorbereitung, ISCED 55)"){
      paste0("Länder (weltweit, OECD) mit dem höchsten Anteil von Meisterlehrlingen (< 880 Std. Vorbereitung, ISCED 55) in ",
             inpf, " an allen Meisterlehrlingen in Erstausbildung ", inpy)
    }else if(inpp =="Auszubildende in Erstausbildung (ISCED 35)"){
      paste0("Länder (weltweit, OECD) mit dem höchsten Anteil von Auszubildenden in Erstausbildung (ISCED 35) in ",
             inpf, " an allen Auszubildenden in Erstausbildung ", inpy)
    }else if (inpp =="In Meister-/Technikerlehre (> 880 Std. Vorbereitung, ISCED 65)"){
      paste0("Länder (weltweit, OECD) mit dem höchsten Anteil von Meister-/Technikerlehrlingen (> 880 Std. Vorbereitung, ISCED 65) in ",
             inpf, " an allen Meister-/Technikerlehrlingen ", inpy)
    }


    # Bottom
    title_bot <- if (inpp == "Auszubildende (ISCED 45)"){
      paste0("Länder (weltweit, OECD) mit dem niedrigsten Anteil von Auszubildenden (ISCED 45) in ",
             inpf, " an allen Auszubildenden (ISCED 45) ", inpy)
    } else if(inpp =="In Meisterlehre (< 880 Std. Vorbereitung, ISCED 55)"){
      paste0("Länder (weltweit, OECD) mit dem niedrigsten Anteil von Meisterlehrlingen (< 880 Std. Vorbereitung, ISCED 55) in ",
             inpf, " an allen Meisterlehrlingen in Erstausbildung ", inpy)
    }else if(inpp =="Auszubildende in Erstausbildung (ISCED 35)"){
      paste0("Länder (weltweit, OECD) mit dem niedrigsten Anteil von Auszubildenden in Erstausbildung (ISCED 35) in ",
             inpf, " an allen Auszubildenden in Erstausbildung ", inpy)
    }else if (inpp =="In Meister-/Technikerlehre (> 880 Std. Vorbereitung, ISCED 65)"){
      paste0("Länder (weltweit, OECD) mit dem niedrigsten Anteil von Meister-/Technikerlehrlingen (> 880 Std. Vorbereitung, ISCED 65) in ",
             inpf, " an allen Meister-/Technikerlehrlingen ", inpy)
    }

}
    }

  # Kodition für Durschnittslinie laden
  avg_line <- r$show_ave

  # Create top 10 plot
    if (avg_line == "Ja"){

      data_avg <- round(mean(data_fn$wert, na.rm = T),1)
      #dies ist schon als funktion automatisiert, too complex
      plot_top <- highcharter::hchart(
        data_fn %>% dplyr::arrange(desc(wert)) %>% dplyr::slice(1:10),
        'bar',
        highcharter::hcaes(y = wert, x = land))%>%
        highcharter::hc_plotOptions(
          series = list(
            boderWidth = 0,
            dataLabels = list(enabled = TRUE, format = "{point.display_rel} %",
                              style = list(textOutline = "none"))
          )) %>%
        highcharter::hc_tooltip(pointFormat = plotopshov )%>%
        highcharter::hc_yAxis(plotLines = list(
          list(
            value = data_avg,
            color = "#154194",
            width = 3,
            zIndex = 4
          )
        ),title = list(text = ""),
        labels = list(format = "{value}%"),
        min = 0,
        max = max(data_fn$wert, na.rm = T)*1.2)%>%
        highcharter::hc_xAxis(title = list(text = " ")) %>%
        highcharter::hc_colors(c("#B16FAB")) %>%
        highcharter::hc_title(text = title_top,
                              margin = 10,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")
        ) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "Calibri Regular", fontSize = "14px")
        ) %>%
        highcharter::hc_caption(text = "Quelle der Daten: Eurostat, 2023; OECD, 2023; freier Download, eigene Berechnungen durch MINTvernetzt.",
                                style = list(fontSize = "11px", color = "gray")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE)

      #dies ist schon als funktion automatisiert, too complex
      plot_bottom <- highcharter::hchart(
        data_fn %>% dplyr::arrange(desc(wert)) %>% dplyr::slice_tail(n = 10),
        'bar',
        highcharter::hcaes(y = wert, x = land))%>%
        highcharter::hc_plotOptions(
          series = list(
            boderWidth = 0,
            dataLabels = list(enabled = TRUE, format = "{point.display_rel} %",
                              style = list(textOutline = "none"))
          )) %>%
        highcharter::hc_tooltip(pointFormat = plotopshov)%>%
        highcharter::hc_yAxis(
          plotLines = list(
            list(
              value = data_avg,
              color = "#154194",
              width = 3,
              zIndex = 4
            )
          ),title = list(text = ""),
          labels = list(format = "{value}%"),
          min = 0,
          max = max(data_fn$wert, na.rm = T)*1.2)%>%
        highcharter::hc_xAxis(title = list(text = " ")) %>%
        highcharter::hc_colors(c("#B16FAB")) %>%
        highcharter::hc_title(text =  title_bot,
                              margin = 10,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")
        ) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "Calibri Regular", fontSize = "14px")
        ) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
        highcharter::hc_caption(text = "Quelle der Daten: Eurostat, 2023; OECD, 2023; freier Download, eigene Berechnungen durch MINTvernetzt.",
                                style = list(fontSize = "11px", color = "gray")) %>%
        highcharter::hc_exporting(enabled = TRUE,
                                  buttons = list(
                                    contextButton = list(
                                      menuItems = list("downloadPNG", "downloadCSV")
                                    )
                                  )
        )



     out <- list(plot_top, plot_bottom)

    } else if (avg_line == "Nein"){


      #dies ist schon als funktion automatisiert, too complex
      plot_top <- highcharter::hchart(
        data_fn %>% dplyr::arrange(desc(wert)) %>% dplyr::slice(1:10),
        'bar',
        highcharter::hcaes(y = wert, x = land))%>%
        highcharter::hc_plotOptions(
          series = list(
            boderWidth = 0,
            dataLabels = list(enabled = TRUE, format = "{point.display_rel} %",
                              style = list(textOutline = "none"))
          )) %>%
        highcharter::hc_tooltip(pointFormat = plotopshov)%>%
        highcharter::hc_yAxis(title = list(text = ""),
                              labels = list(format = "{value}%"),
                              min = 0,
                              max = max(data_fn$wert, na.rm = T)*1.2)%>%
        highcharter::hc_xAxis(title = list(text = " ")) %>%
        highcharter::hc_colors(c("#B16FAB")) %>%
        highcharter::hc_title(text = title_top,
                              margin = 10,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")
        ) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "Calibri Regular", fontSize = "14px")
        ) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
        highcharter::hc_caption(text = "Quelle der Daten: Eurostat, 2023; OECD, 2023; freier Download, eigene Berechnungen durch MINTvernetzt.",
                                style = list(fontSize = "11px", color = "gray")) %>%
        highcharter::hc_exporting(enabled = TRUE,
                                  buttons = list(
                                    contextButton = list(
                                      menuItems = list("downloadPNG", "downloadCSV")
                                    )
                                  )
        )


      #dies ist schon als funktion automatisiert, too complex
      plot_bottom <- highcharter::hchart(
        data_fn %>% dplyr::arrange(desc(wert)) %>% dplyr::slice_tail(n = 10),
        'bar',
        highcharter::hcaes(y = wert, x = land))%>%
        highcharter::hc_plotOptions(
          series = list(
            boderWidth = 0,
            dataLabels = list(enabled = TRUE, format = "{point.display_rel} %",
                              style = list(textOutline = "none"))
          )) %>%
        highcharter::hc_tooltip(pointFormat = plotopshov)%>%
        highcharter::hc_yAxis(title = list(text = ""),
                              labels = list(format = "{value}%"),
                              min = 0,
                              max = max(data_fn$wert, na.rm = T)*1.2)%>%
        highcharter::hc_xAxis(title = list(text = "")) %>%
        highcharter::hc_colors(c("#B16FAB")) %>%
        highcharter::hc_title(text =  title_bot,
                              margin = 10,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")
        ) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "Calibri Regular", fontSize = "14px")
        ) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
        highcharter::hc_caption(text = "Quelle der Daten: Eurostat, 2023; OECD, 2023; freier Download, eigene Berechnungen durch MINTvernetzt.",
                                style = list(fontSize = "11px", color = "gray")) %>%
        highcharter::hc_exporting(enabled = TRUE,
                                  buttons = list(
                                    contextButton = list(
                                      menuItems = list("downloadPNG", "downloadCSV")
                                    )
                                  )
        )


      out <- list(plot_top, plot_bottom)



    }


}


plot_international_top10_mint_arb_gender <- function(r) {


  # Input region laden
  inpl <- r$map_l_top10_mint_arb_gender


  # EU
  if(inpl== "EU"){


    # Spez. EU inputs
    inpy <- r$map_y_eu_top10_mint_arb_gender
    inpp <- r$map_pers_top10_mint_arb_gender



    # Kartenausschnitt
    map_selection <- "custom/europe"


    df_query <- glue::glue_sql("
    SELECT *
    FROM arbeitsmarkt_beschaeftigte_eu
    WHERE geschlecht IN ('Gesamt', 'Frauen')
    AND jahr = {inpy}
    AND indikator = {inpp}
    AND variable = 'Anteil an arbeitender Bevölkerung'
                               ", .con = con)

    data1 <- DBI::dbGetQuery(con, df_query)



    data1 <- data1 %>%
      tidyr::pivot_wider(names_from = geschlecht, values_from = wert)%>%
      dplyr::rename(wert="Frauen")%>%
      dplyr::select(-Gesamt)


    data1$display_rel <- prettyNum(round(data1$wert,1), big.mark = ".", decimal.mark = ",")
#

#
    df_query <- glue::glue_sql("
    SELECT *
    FROM arbeitsmarkt_beschaeftigte_eu
    WHERE geschlecht = 'Frauen'
    AND jahr = {inpy}
    AND indikator = {inpp}
    AND variable = 'Anzahl in Tsd.'
                               ", .con = con)

    data2 <- DBI::dbGetQuery(con, df_query)


    data2 <- data2 %>%
      tidyr::pivot_wider(names_from = variable, values_from = wert)%>%
      dplyr::mutate(across(`Anzahl in Tsd.`, ~ as.numeric(.)*1000))%>%
      dplyr::rename(display_total = "Anzahl in Tsd." )%>%
      dplyr::select(display_total, land)%>%
      dplyr::mutate(across(display_total, ~ prettyNum(., big.mark = ".", decimal.mark = ",")))

    # Zusammenführen und Geomappen
    data1 <- data1 %>%
      dplyr::left_join(data2,by=c("land"))%>%
      dplyr::left_join(countries_names %>%
                         dplyr::mutate(land=dplyr::case_when(land == "Tschechien" ~ "Tschechische Republik",
                                                             T ~ .$land)), by= "land")%>%
      dplyr::mutate(alpha2= toupper(alpha2))%>%
      dplyr::filter(!is.na(.$wert) & wert!=0)

    # Hover vornereiten
    plotopshov <- "Anteil: {point.display_rel}% <br> Anzahl: {point.display_total}"

    # Tielt vorbereiten
    title_top <- paste0("Länder Europas mit dem höchsten Anteil von weiblichen ", inpp, "n an allen ", inpp, "n in MINT ", inpy )
    title_bot <- paste0("Länder Europas mit dem niedrigsten Anteil von weiblichen ", inpp, "n an allen ", inpp, "n in MINT  ",  inpy )



    }


  # OECD
  else if (inpl== "OECD"){


    # Kartenausschnitt
    map_selection <- "custom/world"


    # ui inputs
    inpp <- r$map_pers_top10_mint_arb_gender
    inpy <- r$map_y_top10_mint_arb_gender
    inpf <- r$map_f_top10_mint_arb_gender

    # Indikatore aus: arbeitsmarkt_anfänger_absolv_oecd
    if(inpp %in%  c("Anfänger*innen Ausbildung (ISCED 45)",
                    "Anfänger*innen Erstausbildung (ISCED 35)",
                    "Absolvent*innen Ausbildung (ISCED 45)",
                    "Absolvent*innen Erstausbildung (ISCED 35)")){

      # Hover vorbereiten
      plotopshov <- "Anteil: {point.display_rel}%"


      df_query <- glue::glue_sql("
      SELECT *
      FROM arbeitsmarkt_anfaenger_absolv_oecd
      WHERE jahr = {inpy}
      AND fachbereich IN ('MINT', 'Informatik & Kommunikationstechnologie', 'Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe', 'Naturwissenschaften, Mathematik und Statistik', 'Alle')
      AND geschlecht = 'Frauen'
                               ", .con = con)

      data1 <- DBI::dbGetQuery(con, df_query)



      data1 <- data1 %>%
          tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
        # Durschnittliche relative Häufigkeit aller MINT-Einzelfächer= realtive Häufigkeit MINT
        dplyr::mutate(MINT = (rowSums(dplyr::select(., "Informatik & Kommunikationstechnologie",
                                             "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                             "Naturwissenschaften, Mathematik und Statistik"), na.rm = T))/3)%>%
        tidyr::pivot_longer(c("MINT",
                               "Informatik & Kommunikationstechnologie",
                               "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                               "Naturwissenschaften, Mathematik und Statistik",
                               "Alle"), values_to = "wert", names_to = "fachbereich")%>%
        dplyr::filter(!is.na(.$wert) & wert!=0)

      # Filtern nach spez. Indikatoren, geo mapping und wert für hover vorbereiten
      if(inpp == "Anfänger*innen Ausbildung (ISCED 45)"){

        data1 <- data1 %>%
          dplyr::filter(anforderung == "Ausbildung (ISCED 45)" &
                        variable == "Frauen-/Männeranteil Ausbildungs-/Studiumsanfänger*innen nach Fachbereichen" &
                        fachbereich == inpf
            )%>%
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))


        data1$display_rel <- prettyNum(round(data1$wert,1), big.mark = ".", decimal.mark = ",")


      } else if (inpp == "Anfänger*innen Erstausbildung (ISCED 35)"){

        data1 <- data1 %>%
          dplyr::filter(anforderung == "Erstausbildung (ISCED 35)" &
                          variable == "Frauen-/Männeranteil Ausbildungs-/Studiumsanfänger*innen nach Fachbereichen"&
                          fachbereich == inpf)%>%
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))

        data1$display_rel <- prettyNum(round(data1$wert,1), big.mark = ".", decimal.mark = ",")

      } else if (inpp == "Absolvent*innen Ausbildung (ISCED 45)"){

        data1 <- data1 %>%
          dplyr::filter(anforderung == "Ausbildung (ISCED 45)" &
                          variable == "Frauen-/Männeranteil Absolvent*innen nach Fachbereichen"&
                          fachbereich == inpf)%>%
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))

        data1$display_rel <- prettyNum(round(data1$wert,1), big.mark = ".", decimal.mark = ",")

      } else if (inpp == "Absolvent*innen Erstausbildung (ISCED 35)"){

        data1 <- data1 %>%
          dplyr::filter(anforderung == "Erstausbildung (ISCED 35)" &
                          variable == "Frauen-/Männeranteil Absolvent*innen nach Fachbereichen"&
                          fachbereich == inpf)
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))

        data1$display_rel <- prettyNum(round(data1$wert,1), big.mark = ".", decimal.mark = ",")

      }

      #Titel vorbereiten


      # Top
      title_top <- if (inpp == "Anfänger*innen Ausbildung (ISCED 45)"){
        paste0("Länder (weltweit, OECD) mit dem höchsten Anteil von weiblichen Ausbildungsanfänger*innen (ISCED 45) an allen Ausbildungsanfänger*innen in ",
                inpf, " ", inpy)
      } else if(inpp =="Anfänger*innen Erstausbildung (ISCED 35)"){
        paste0("Länder (weltweit, OECD) mit dem höchsten Anteil von weiblichen Anfänger*innen in Erstausbildung (ISCED 35) an allen Anfänger*innen in Erstausbildung ",
                inpf, " ", inpy)
      }else if(inpp =="Absolvent*innen Ausbildung (ISCED 45)"){
        paste0("Länder (weltweit, OECD) mit dem höchsten Anteil von weiblichen Ausbildungsabsolvent*innen (ISCED 45) an allen Ausbildungsabsolvent*innen ",
               inpf, " ", inpy)
      }else if (inpp =="Absolvent*innen Erstausbildung (ISCED 35)"){
        paste0("Länder (weltweit, OECD) mit dem höchsten Anteil von Absolvent*innen der Erstausbildung (ISCED 35) an allen Absolvent*innen der Erstausbildung",
               inpf, " ", inpy)
      }

      # Bottom
      title_bot <- if (inpp == "Anfänger*innen Ausbildung (ISCED 45)"){
        paste0("Länder (weltweit, OECD) mit dem niedrigsten Anteil von weiblichen Ausbildungsanfänger*innen (ISCED 45) an allen Ausbildungsanfänger*innen in ",
               inpf, " ", inpy)
      } else if(inpp =="Anfänger*innen Erstausbildung (ISCED 35)"){
        paste0("Länder (weltweit, OECD) mit dem niedrigsten Anteil von weiblichen Anfänger*innen in Erstausbildung (ISCED 35) an allen Anfänger*innen in Erstausbildung ",
               inpf, " ", inpy)
      }else if(inpp =="Absolvent*innen Ausbildung (ISCED 45)"){
        paste0("Länder (weltweit, OECD) mit dem niedrigsten Anteil von weiblichen Ausbildungsabsolvent*innen (ISCED 45) an allen Ausbildungsabsolvent*innen ",
               inpf, " ", inpy)
      }else if (inpp =="Absolvent*innen Erstausbildung (ISCED 35)"){
        paste0("Länder (weltweit, OECD) mit dem niedrigsten Anteil von Absolvent*innen der Erstausbildung (ISCED 35) an allen Absolvent*innen der Erstausbildung",
               inpf, " ", inpy)
      }


    } # indikatoren aus arbeitsmarkt_anzahl_azubis_oecd
    else {

      # ui input für Betrachtung
      inpbe <- r$map_betr_oecd_top10_mint_arb_gender

      # Hover vorbereiten
      plotopshov <- "Anteil: {point.display_rel}% <br> Anzahl: {point.display_total}"


      df_query <- glue::glue_sql("
      SELECT *
      FROM arbeitsmarkt_anzahl_azubis_oecd
      WHERE geschlecht IN ('Gesamt', 'Frauen')
      AND indikator = 'berufsorientiert'
      AND jahr = {inpy}
      AND fachbereich IN ('MINT', 'Informatik & Kommunikationstechnologie', 'Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe', 'Naturwissenschaften, Mathematik und Statistik', 'Alle')
                               ", .con = con)

      data_fva <- DBI::dbGetQuery(con, df_query)







      data_fva <- data_fva %>%
        tidyr::pivot_wider(values_from = wert, names_from = geschlecht)%>%
        dplyr::mutate(wert= round(Frauen/Gesamt *100,1))%>%
        dplyr::mutate(display_rel= prettyNum(round(.$wert,1), big.mark = ".", decimal.mark = ","),
                      display_total= prettyNum(.$Frauen, big.mark = ".", decimal.mark = ","))%>%
        dplyr::select(-Gesamt, - Frauen)%>%
        dplyr::inner_join(countries_names, by = "land") %>%
        dplyr::mutate(alpha2 = toupper(alpha2))


      df_query <- glue::glue_sql("
      SELECT *
      FROM arbeitsmarkt_anzahl_azubis_oecd
      WHERE geschlecht = 'Frauen'
      AND indikator = 'berufsorientiert'
      AND jahr = {inpy}
      AND fachbereich IN ('MINT', 'Informatik & Kommunikationstechnologie', 'Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe', 'Naturwissenschaften, Mathematik und Statistik', 'Alle')
                               ", .con = con)

      data_fvf1 <- DBI::dbGetQuery(con, df_query)









      data_fvf1 <- data_fvf1 %>%
        tidyr::pivot_wider(values_from = wert, names_from = fach)%>%
        dplyr::mutate(across(c("MINT",
                               "Informatik & Kommunikationstechnologie",
                               "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                               "Naturwissenschaften, Mathematik und Statistik"), ~ round(./Alle*100,1)))%>%
        dplyr::select(-Alle)%>%
        tidyr::pivot_longer(c("MINT",
                              "Informatik & Kommunikationstechnologie",
                              "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                              "Naturwissenschaften, Mathematik und Statistik"), values_to = "wert",
                            names_to = "fach") %>%
        dplyr::mutate(display_rel= prettyNum(round(.$wert,1), big.mark = ".", decimal.mark = ","))


      df_query <- glue::glue_sql("
      SELECT *
      FROM arbeitsmarkt_anzahl_azubis_oecd
      WHERE geschlecht = 'Frauen'
      AND indikator = 'berufsorientiert'
      AND jahr = {inpy}
      AND fachbereich IN ('MINT', 'Informatik & Kommunikationstechnologie', 'Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe', 'Naturwissenschaften, Mathematik und Statistik', 'Alle')
                               ", .con = con)

      data_fvf2 <- DBI::dbGetQuery(con, df_query)



      data_fvf2 <- data_fvf2 %>%
        dplyr::rename(display_total = wert)%>%
        dplyr::mutate(display_total= prettyNum(.$display_total, big.mark = ".", decimal.mark = ","))%>%
        dplyr::select(land, jahr, display_total, fach, anforderung)


      # Zusammenführen
      data_fvf3 <- data_fvf1 %>%
        dplyr::left_join(data_fvf2, by=c("land", "jahr", "fach", "anforderung"))%>%
        dplyr::inner_join(countries_names, by = "land") %>%
        dplyr::mutate(alpha2 = toupper(alpha2))


      # Ertre Betrachtungweise
      if (inpbe == "Anteil von Frauen an Allen"){


        data1 <- data_fva%>%
          dplyr::filter(!is.na(.$wert) & wert!=0)


        # Titel vorbereiten

        # Top
        title_top <- if (inpp == "Auszubildende (ISCED 45)"){
          paste0("Länder (weltweit, OECD) mit dem höchsten Anteil von weiblichen Auszubildenden (ISCED 45) an allen Auszubildenden in ",
                 inpf, " ", inpy)
        } else if(inpp =="In Meisterlehre (< 880 Std. Vorbereitung, ISCED 55)"){
          paste0("Länder (weltweit, OECD) mit dem höchsten Anteil von weiblichen Meisterlehrlingen (< 880 Std. Vorbereitung, ISCED 55) an allen Meisterlehrlingen in ",
                 inpf, " ", inpy)
        }else if(inpp =="Auszubildende in Erstausbildung (ISCED 35)"){
          paste0("Länder (weltweit, OECD) mit dem höchsten Anteil von weiblichen Auszubildenden in Erstausbildung (ISCED 35) an allen Auszubildenden in Erstausbildung in ",
                 inpf, " ", inpy)
        }else if (inpp =="In Meister-/Technikerlehre (> 880 Std. Vorbereitung, ISCED 65)"){
          paste0("Länder (weltweit, OECD) mit dem höchsten Anteil von weiblichen Meister-/Technikerlehrlingen (> 880 Std. Vorbereitung, ISCED 65) an allen Meister-/Technikerlehrlingen in ",
                 inpf, " ", inpy)
        }

        # Bottom
        title_bot <- if (inpp == "Auszubildende (ISCED 45)"){
          paste0("Länder (weltweit, OECD) mit dem niedrigsten Anteil von weiblichen Auszubildenden (ISCED 45) an allen Auszubildenden in ",
                 inpf, " ", inpy)
        } else if(inpp =="In Meisterlehre (< 880 Std. Vorbereitung, ISCED 55)"){
          paste0("Länder (weltweit, OECD) mit dem niedrigsten Anteil von weiblichen Meisterlehrlingen (< 880 Std. Vorbereitung, ISCED 55) an allen Meisterlehrlingen in ",
                 inpf, " ", inpy)
        }else if(inpp =="Auszubildende in Erstausbildung (ISCED 35)"){
          paste0("Länder (weltweit, OECD) mit dem niedrigsten Anteil von weiblichen Auszubildenden in Erstausbildung (ISCED 35) an allen Auszubildenden in Erstausbildung in ",
                 inpf, " ", inpy)
        }else if (inpp =="In Meister-/Technikerlehre (> 880 Std. Vorbereitung, ISCED 65)"){
          paste0("Länder (weltweit, OECD) mit dem niedrigsten Anteil von Meister-/Technikerlehrlingen (> 880 Std. Vorbereitung, ISCED 65) an allen Meister-/Technikerlehrlingen in ",
                 inpf, " ", inpy)
        }





      }# Zweite Betrachtungsweise
      else if(inpbe == "Anteil an Frauen von Frauen"){

        data1 <- data_fvf3%>%
          dplyr::filter(!is.na(.$wert) & wert!=0)


        # Titel vorbereiten

        # Top
        title_top <- if (inpp == "Auszubildende (ISCED 45)"){
          paste0("Länder (weltweit, OECD) mit dem höchsten Anteil von Auszubildenden (ISCED 45) in ", inpf, " an weiblichen Auszubildenden ",
                 " ", inpy)
        }else if(inpp == "In Meisterlehre (< 880 Std. Vorbereitung, ISCED 55)"){
          paste0("Länder (weltweit, OECD) mit dem höchsten Anteil von Meisterlehrlingen (< 880 Std. Vorbereitung, ISCED 55) in ", inpf, " an weiblichen Meisterlehrlingen ",
                 " ", inpy)
        }else if(inpp =="Auszubildende in Erstausbildung (ISCED 35)"){
          paste0("Länder (weltweit, OECD) mit dem höchsten Anteil von Auszubildenden in Erstausbildung (ISCED 35) in ", inpf, " an weiblichen Auszubildenden in Erstausbildung ",
                 " ", inpy)
        }else if (inpp =="In Meister-/Technikerlehre (> 880 Std. Vorbereitung, ISCED 65)"){
          paste0("Länder (weltweit, OECD) mit dem höchsten Anteil von Meister-/Technikerlehrlingen (> 880 Std. Vorbereitung, ISCED 65) in ", inpf, " an allen weiblichen Meister-/Technikerlehrlingen ",
                 " ", inpy)
        }

        # Bottom
        title_bot <- if (inpp == "Auszubildende (ISCED 45)"){
          paste0("Länder (weltweit, OECD) mit dem niedrigsten Anteil von Auszubildenden (ISCED 45) in ", inpf, " an weiblichen Auszubildenden in ",
                 " ", inpy)
        }else if(inpp =="In Meisterlehre (< 880 Std. Vorbereitung, ISCED 55)"){
          paste0("Länder (weltweit, OECD) mit dem niedrigsten Anteil von Meisterlehrlingen (< 880 Std. Vorbereitung, ISCED 55) in ", inpf, " an weiblichen Meisterlehrlingen ",
                 " ", inpy)
        }else if(inpp =="Auszubildende in Erstausbildung (ISCED 35)"){
          paste0("Länder (weltweit, OECD) mit dem niedrigsten Anteil von Auszubildenden in Erstausbildung (ISCED 35) in ", inpf, " an weiblichen Auszubildenden in Erstausbildung ",
                 " ", inpy)
        }else if (inpp =="In Meister-/Technikerlehre (> 880 Std. Vorbereitung, ISCED 65)"){
          paste0("Länder (weltweit, OECD) mit dem niedrigsten Anteil von Meister-/Technikerlehrlingen (> 880 Std. Vorbereitung, ISCED 65) in ", inpf, " an allen weiblichen Meister-/Technikerlehrlingen ",
                 " ", inpy)
        }


      }



      # Nach spez. Inidkatoren filtern
      if (inpp == "Auszubildende (ISCED 45)"){

        data1 <- data1 %>%
          dplyr::filter(anforderung=="Ausbildung (ISCED 45)"&
                          fach == inpf)


      } else if(inpp == "Auszubildende in Erstausbildung (ISCED 35)"){

        data1 <- data1 %>%
          dplyr::filter(anforderung=="Erstausbildung (ISCED 35)"&
                          fach == inpf)

      } else if(inpp == "In Meisterlehre (< 880 Std. Vorbereitung, ISCED 55)") {

        data1 <- data1 %>%
          dplyr::filter(anforderung=="kurzes tertiäres Bildungsprogramm (berufsorientiert)"&
                          fach == inpf)

      } else if(inpp == "In Meister-/Technikerlehre (> 880 Std. Vorbereitung, ISCED 65)"){

        data1 <- data1 %>%
          dplyr::filter(anforderung== "Bachelor oder vergleichbar (berufsorientiert)"&
                          fach == inpf)

      }
    }

  }

  # Kodition Durschnittslinie
  avg_line <- r$show_avg_top10_mint_arb_gender

  # Create top 10 plot
  if (avg_line == "Ja"){

    data_avg <- round(mean(data1$wert, na.rm = T),1)



    #dies ist schon als funktion automatisiert, too complex
    plot_top <- highcharter::hchart(
      data1 %>% dplyr::arrange(desc(wert)) %>% dplyr::slice(1:10),
      'bar',
      highcharter::hcaes(y = wert, x = land))%>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, format = "{point.display_rel} %",
                            style = list(textOutline = "none"))
        )) %>%
      highcharter::hc_tooltip(pointFormat = plotopshov) %>%
      highcharter::hc_yAxis(plotLines = list(
        list(
          value = data_avg,
          color = "#B16FAB",
          width = 3,
          zIndex = 4
        )
      ),title = list(text = ""),
      labels = list(format = "{value}%"),
      min = 0,
      max = max(data1$wert, na.rm = T)*1.2)%>%
      highcharter::hc_xAxis(title = list(text = " ")) %>%
      highcharter::hc_colors(c("#154194")) %>%
      highcharter::hc_title(text = title_top,
                            margin = 10,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")
      ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
      highcharter::hc_caption(text = "Quelle der Daten: Eurostat, 2023; OECD, 2023; freier Download, eigene Berechnungen durch MINTvernetzt.",
                              style = list(fontSize = "11px", color = "gray")) %>%
      highcharter::hc_exporting(enabled = TRUE,
                                buttons = list(#
                                  contextButton = list(
                                    menuItems = list("downloadPNG", "downloadCSV")
                                  )
                                )
      )

    #dies ist schon als funktion automatisiert, too complex
    plot_bottom <- highcharter::hchart(
      data1 %>% dplyr::arrange(desc(wert)) %>% dplyr::slice_tail(n = 10),
      'bar',
      highcharter::hcaes(y = wert, x = land))%>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, format = "{point.display_rel} %",
                            style = list(textOutline = "none"))
        )) %>%
      highcharter::hc_tooltip(pointFormat = plotopshov) %>%
      highcharter::hc_yAxis(
        plotLines = list(
          list(
            value = data_avg,
            color = "#B16FAB",
            width = 3,
            zIndex = 4
          )
        ),title = list(text = ""),
        labels = list(format = "{value}%"),
        min = 0,
        max = max(data1$wert, na.rm = T)*1.2)%>%
      highcharter::hc_xAxis(title = list(text = " ")) %>%
      highcharter::hc_colors(c("#154194")) %>%
      highcharter::hc_title(text =  title_bot,
                            margin = 10,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")
      ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
      highcharter::hc_caption(text = "Quelle der Daten: Eurostat, 2023; OECD, 2023; freier Download, eigene Berechnungen durch MINTvernetzt.",
                              style = list(fontSize = "11px", color = "gray")) %>%
      highcharter::hc_exporting(enabled = TRUE,
                                buttons = list(
                                  contextButton = list(
                                    menuItems = list("downloadPNG", "downloadCSV")
                                  )
                                )
      )

    out <- list(plot_top, plot_bottom)
    #dies ist schon als funktion automatisiert, too complex

  } else if (avg_line == "Nein"){



    plot_top <- highcharter::hchart(
      data1 %>% dplyr::arrange(desc(wert)) %>% dplyr::slice(1:10),
      'bar',
      highcharter::hcaes(y = wert, x = land))%>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, format = "{point.display_rel} %",
                            style = list(textOutline = "none"))
        )) %>%
      highcharter::hc_tooltip(pointFormat = plotopshov) %>%
      highcharter::hc_yAxis(title = list(text = ""),
                            labels = list(format = "{value}%"),
                            min = 0,
                            max = max(data1$wert, na.rm = T)*1.2) %>%
      highcharter::hc_xAxis(title = list(text = " ")) %>%
      highcharter::hc_colors(c("#154194")) %>%
      highcharter::hc_title(text = title_top,
                            margin = 10,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")
      ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
      highcharter::hc_caption(text = "Quelle der Daten: Eurostat, 2023; OECD, 2023; freier Download, eigene Berechnungen durch MINTvernetzt.",
                              style = list(fontSize = "11px", color = "gray")) %>%
      highcharter::hc_exporting(enabled = TRUE,
                                buttons = list(
                                  contextButton = list(
                                    menuItems = list("downloadPNG", "downloadCSV")
                                  )
                                )
      )



    #dies ist schon als funktion automatisiert, too complex
    plot_bottom <- highcharter::hchart(
      data1 %>% dplyr::arrange(desc(wert)) %>% dplyr::slice_tail(n = 10),
      'bar',
      highcharter::hcaes(y = wert, x = land))%>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, format = "{point.display_rel} %",
                            style = list(textOutline = "none"))
        )) %>%
      highcharter::hc_tooltip(pointFormat = plotopshov) %>%
      highcharter::hc_yAxis(title = list(text = ""),
                            labels = list(format = "{value}%"),
                            min = 0,
                            max = max(data1$wert, na.rm = T)*1.2) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_colors(c("#154194")) %>%
      highcharter::hc_title(text =  title_bot,
                            margin = 10,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")
      ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
      highcharter::hc_caption(text = "Quelle der Daten: Eurostat, 2023; OECD, 2023; freier Download, eigene Berechnungen durch MINTvernetzt.",
                              style = list(fontSize = "11px", color = "gray")) %>%
      highcharter::hc_exporting(enabled = TRUE,
                                buttons = list(
                                  contextButton = list(
                                    menuItems = list("downloadPNG", "downloadCSV")
                                  )
                                )
      )


    out <- list(plot_top, plot_bottom)

  }

  }





plot_international_arbeitsmarkt_vergleiche <- function(r) {


  timerange <- r$vergleich_y_int_arbeitsmarkt
  land_m <- r$vergleich_l_int_arbeitsmarkt
  fach_m <- r$vergleich_f_int_arbeitsmarkt

  variable_set <- c("Anteil Absolvent*innen nach Fach an allen Fächern",
                    "Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern")


  df_query <- glue::glue_sql("

  SELECT *
  FROM arbeitsmarkt_anfaenger_absolv_oecd
  WHERE geschlecht = 'Gesamt'
  AND anforderung = 'tertiäre Bildung (gesamt)'
  AND jahr = {timerange}
  AND land IN ({land_m*})
  AND variable IN ({variable_set*})
  AND fachbereich = {fach_m}

                               ", .con = con)

  tmp_df <- DBI::dbGetQuery(con, df_query)



  # check if variables are present
  if (!all(variable_set %in% unique(tmp_df$variable))) {

    return("Für diese Kombination an Filtereinstellungen sind leider keine Daten vorhanden.")
  }


  tooltip_data <- tmp_df %>%
    tidyr::pivot_wider(names_from = variable,
                       id_cols = land,
                       values_from = wert) %>%
    dplyr::mutate(
      Difference = round(
        x = `Anteil Absolvent*innen nach Fach an allen Fächern` -
          `Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern`,
        digits = 0),
      max = pmax(
        `Anteil Absolvent*innen nach Fach an allen Fächern`,
        `Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern`,
        na.rm = TRUE)
    ) %>%
    dplyr::select(land, Difference, max) %>%
    dplyr::distinct()


  annotation_data <- lapply(seq_len(nrow(tooltip_data)), function(x){
    # create annotation only if there is a difference
    if (is.na(tooltip_data$Difference[x])) {
      out <- NULL
    } else {
      out <- list(point = list(x = x -1,
                               y = tooltip_data$max[x],
                               xAxis = 0,
                               yAxis = 0),
                  text = as.character(tooltip_data$Difference[x]))
    }
    return(out)
  })

  tmp_df$wert <- round(tmp_df$wert, 1)

  tmp_df$variable <- factor(tmp_df$variable, levels = c("Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern",
                                                        "Anteil Absolvent*innen nach Fach an allen Fächern"))
  # Create the plot
  plot <- highcharter::hchart(object = tmp_df,
                              type = "column",
                              mapping = highcharter::hcaes(x = land, y = wert, group = variable))  %>%
    highcharter::hc_xAxis(title="Land") %>%
    highcharter::hc_yAxis(title = list(text = "")) %>%
    highcharter::hc_plotOptions(
      series = list(
        dataLabels = list(enabled = FALSE))) %>%
    highcharter::hc_colors(c("#B16FAB", "#66CBAF")) %>%
    highcharter::hc_title(
      text = paste0(
        "Anteil der Ausbildungs-/Studiums-Anfänger*innen und Absolvent*innen in ",
        fach_m, " in ", timerange)
    ) %>%
    highcharter::hc_legend(enabled = TRUE) %>%
    highcharter::hc_exporting(enabled = FALSE) %>%
    highcharter::hc_tooltip(
      pointFormat = paste0(
        '<span style="color:{point.color}">\u25CF</span>',
        '{series.name}: ','<b>{point.y} %</b><br/>'),
      shared = TRUE,
      useHTML = TRUE) %>%
    highcharter::hc_annotations(
      list(
        labels = annotation_data,
        labelOptions = list(
          style = list(color = 'black'),
          backgroundColor = 'none', # Remove background color
          borderWidth = 0#, # Remove box
          #shadow = FALSE,
        )
      )
    ) %>%
    highcharter::hc_caption(text = "Quelle der Daten: Eurostat, 2023; OECD, 2023; freier Download, eigene Berechnungen durch MINTvernetzt.",
                            style = list(fontSize = "11px", color = "gray")) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG", "downloadCSV")
                                )
                              )
    )


  return(plot)
}


