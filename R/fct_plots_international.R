#' A function to plot a graph.
#'
#' @description A function to create a map for the first box
#' inside the tab "International".
#'
#' @return The return value is a plot
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd


get_top10_hc_plot_options <- function(hc,
                                      hc_title = "",
                                      hc_tooltip = "",
                                      max_percent_used = 100) {
  out <- hc %>%
    highcharter::hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE, format = "{point.wert} %")
      )) %>%
    highcharter::hc_tooltip(pointFormat = hc_tooltip) %>%
    highcharter::hc_yAxis(title = list(text = ""),
                          labels = list(format = "{value} %"),
                          min = 0,
                          max = max_percent_used,
                          tickInterval = 10) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_colors(c("#154194")) %>%
    highcharter::hc_title(text = hc_title,
                          margin = 45,
                          align = "center",
                          style = list(color = "black",
                                       useHTML = TRUE,
                                       fontFamily = "SourceSans3-Regular",
                                       fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE)

  return(out)
}

add_avg_to_hc <- function(hc, hc_mean) {
  out <- hc %>%
    highcharter::hc_yAxis(
      plotLines = list(
        list(
          value = hc_mean,
          color = "#FF0000",
          width = 3,
          zIndex = 4
        )
      )
    )


  return(out)
}


## studium ----
plot_international_map <- function(r) {

  #r <- list(map_y_int_studium = "2019", map_l_int_studium = "OECD", map_f_int_studium = "Umwelt")
  # load UI inputs from reactive value
  timerange <- r$map_y_int_studium
  label_m <- r$map_l_int_studium
  fach_m <- r$map_f_int_studium

  if (is.null(fach_m)) { fach_m <- ""}
  logger::log_debug("Plotting international map for:")
  logger::log_debug("Time: ", timerange,
                    " | Label: ", label_m,
                    " | Fach: ", fach_m)


  if (label_m == "Weltweit") {
    map_selection <- golem::get_golem_options("world_map")
    fach_m <- "Alle MINT-Fächer"
    df <- studierende_absolventen_weltweit  %>%
      dplyr::filter(fach == "Alle MINT-Fächer") %>%
      dplyr::filter(land != "San Marino") %>%
      dplyr::filter(jahr != "2022") %>%
      dplyr::mutate(wert = round(wert, 1))




  } else if (label_m == "OECD") {
    #map_selection <- golem::get_golem_options("world_map")
  map_selection <- "custom/world"


    # filter for selection
    df_filtered <- studierende_anzahl_oecd %>%
      dplyr::filter(geschlecht == "Gesamt" &
                      jahr == timerange &
                      ebene == 1 &
                      anforderung %in% c("Bachelor oder vergleichbar (akademisch)",

                                         "Master oder vergleichbar (akademisch)",
                                         "Promotion (ISCED 8)"))

    # filtern - nur Länder, für die alle Studi-Anforderungsniveaus vorliegend sind
    df_filtered <- df_filtered %>%
      dplyr::group_by(fach, geschlecht, jahr, land) %>%
      dplyr::filter(dplyr::n_distinct(anforderung) == 3) %>%
      dplyr::ungroup()


    # calculate total amount by land
    this_df_alle <<- df_filtered %>%
      dplyr::filter(fachbereich == "Alle") %>%
      dplyr::group_by(land, jahr, fach)
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
    map_selection <- golem::get_golem_options("europa_map")
    df <- studierende_europa %>%
      dplyr::filter(geschlecht == "Gesamt"  &
                      (mint_select == "mint" |
                         (mint_select == "nicht mint" &
                            fach_m == "Alle MINT-Fächer")) &
                      fach == fach_m &
                      indikator == "Fächerwahl")
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
    title_m <- paste0("Anteil von Studienabsolvent*innen in ", fach_help, " an allen Studienabsolvent*innen ",
                      timerange, " weltweit (UNESCO)")
  }else{if(label_m == "OECD"){
    title_m <- paste0("Anteil von Studierenden in ", fach_help, " an allen Studierenden ",
                      timerange, " der OECD-Staaten")
  }else{
    title_m <- paste0("Anteil von Studierenden in ", fach_help, " an allen Studierenden ",
                      timerange, " in Europa")
  }
  }

  data_map_1 <- df7

  highcharter::highchart(type = "map") %>%
    highcharter::hc_add_series_map(
      #"countries/de/de-all",
      map = map_selection,
      df = data_map_1,
      value = "wert",
      joinBy = c("hc-a2", "alpha2"),
      borderColor = "#FAFAFA",
      name = paste0(fach_m),
      borderWidth = 0.1,
      nullColor = "#A9A9A9",
      tooltip = list(valueDecimals = 1, valueSuffix = "%")
    ) %>%
    highcharter::hc_tooltip(pointFormat = hover) %>%
    highcharter::hc_colorAxis(min=0, minColor= "#f4f5f6", maxColor="#b16fab",labels = list(format = "{text} %")) %>%
    highcharter::hc_title(
      text = title_m,
      margin = 10,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
    ) %>%
    # highcharter::hc_caption(
    #   text = "...",  style = list(color= "white", fontSize = "12px")
    # ) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular")
    ) %>% #highcharter::hc_size(600, 550) %>%
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

}


plot_international_map_fem <- function(r){


  label_m <<- r$map_l_f

  #level_m <<- r$map_le_f

  if(label_m == "EU"){
    map_selection <- "custom/europe"

    timerange <<- r$map_y_f
    fach_m <<- r$map_f_f
    betr <<- r$map_le_betr

    # test <- studierende_europa %>%
    #   dplyr::filter(land == "Deutschland" & jahr == 2021)

    if(betr == "Anteil von Frauen an Allen"){

      df1 <<- studierende_europa%>%
        dplyr::filter(!is.na(.$wert))%>%
        dplyr::filter(ebene == 1 &
                        indikator == "Frauen-/Männeranteil"&
                        mint_select == "mint")%>%
        tidyr::pivot_wider(values_from = wert, names_from = geschlecht)%>%
        dplyr::select(-Männer, - Gesamt)%>%
        dplyr::rename(wert = Frauen)%>%
        dplyr::mutate(across(wert, ~ round(.,1)))%>%
        dplyr::filter( fach == fach_m&
                         jahr == timerange)




      df1$display_rel <- prettyNum( df1$wert, big.mark = ".", decimal.mark = ",")

      map_data_1 <<- df1 %>%
        dplyr::left_join(countries_names, by = "land") %>%
        dplyr::mutate(alpha2 = toupper(alpha2))

      # studierende_europa1 <<- map_data_1 %>%
      #   janitor::get_dupes(-wert)

      hoverplot <- "{point.land} <br> Anteil: {point.display_rel}%"

      title_dyn <- if(fach_m=="Alle MINT-Fächer"){
        paste("Anteil von weiblichen Studierenden an allen Studierenden in allen MINT-Fächern " , timerange)

      } else if (fach_m=="Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe"){

        paste("Anteil von weiblichen Studierenden an allen Studierenden in Ingenieurwesen, verarbeitendem Gewerbe und Baugewerbe " , timerange)

      } else {

        paste("Anteil von weiblichen Studierenden an allen Studierenden in ", fach_m, " " , timerange)
      }




    } else if(betr=="Anteil an Frauen von Frauen"){

      df1 <<- studierende_europa%>%
        dplyr::filter(!is.na(.$wert))%>%
        dplyr::filter(ebene == 1 &
                        indikator == "Fächerwahl"&
                        mint_select == "mint" &
                        geschlecht == "Frauen")%>%
        dplyr::filter(fach == fach_m &
                        jahr == timerange)%>%
        dplyr::mutate(display_rel = prettyNum(round(.$wert,1), big.mark = ".", decimal.mark = ","))

      map_data_1 <<- df1 %>%
        dplyr::left_join(countries_names, by = "land") %>%
        dplyr::mutate(alpha2 = toupper(alpha2))

      hoverplot <- "{point.land} <br> Anteil: {point.display_rel}%"



      title_dyn <- if(fach_m=="Alle MINT-Fächer"){
        paste("Anteil von Studierenden in allen MINT-Fächern an allen weiblichnen Studierenden " , timerange)

      } else if (fach_m=="Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe"){

        paste("Anteil von Studierenden in Ingenieurwesen, verarbeitendem Gewerbe und Baugewerbe an allen weiblichen Studierenden in Ingenieurwesen, verarbeitendem Gewerbe und Baugewerbe " , timerange)

      } else {

        paste("Anteil von Studierenden in ", fach_m, " an allen weiblichen Studierenden ", timerange)
      }


    }


    #capt_dyn  <- paste("Quelle der Daten: Eurostat, 2022, eigene Berechnungen durch MINTvernetzt")



  }

  if (label_m == "OECD"){


    level <<- r$map_le_f
    betr <<- r$map_le_betr


    map_selection <- "custom/world"


    timerange <<- r$map_y_f
    fach_m <<- r$map_f_f

    # test <- studierende_anzahl_oecd %>%
    #   dplyr::filter(land== "Kanada" &
    #                   jahr == 2017)%>%
    #   tidyr::pivot_wider(values_from=wert, names_from = anforderung)%>%
    #   dplyr::mutate(testo = rowSums(dplyr::select(., "Master oder vergleichbar (akademisch)",
    #                                "Promotion (ISCED 8)",
    #                                "Bachelor oder vergleichbar (akademisch)"), na.rm=T))%>%
    #   dplyr::select(-c("Master oder vergleichbar (akademisch)",
    #                                "Promotion (ISCED 8)",
    #                                "Bachelor oder vergleichbar (akademisch)"))

    df_filtered <<- studierende_anzahl_oecd %>%
      dplyr::filter(!is.na(.$wert))%>%
      dplyr::filter(geschlecht %in% c("Frauen", "Gesamt") &
                      jahr == timerange &
                      ebene == 1 &
                      anforderung %in% c("Bachelor oder vergleichbar (akademisch)",
                                         "Master oder vergleichbar (akademisch)",
                                         "Promotion (ISCED 8)")
      )%>%
      tidyr::pivot_wider(names_from = anforderung, values_from = wert)%>%
      dplyr::mutate(wert = rowSums(dplyr::select(., "Bachelor oder vergleichbar (akademisch)",
                                                 "Master oder vergleichbar (akademisch)",
                                                 "Promotion (ISCED 8)"), na.rm= T ))%>%
      dplyr::select(- c("Bachelor oder vergleichbar (akademisch)",
                        "Master oder vergleichbar (akademisch)",
                        "Promotion (ISCED 8)"))



    df_share_fem <<- df_filtered %>%
      dplyr::select(-fachbereich)%>%
      dplyr::filter(mint_select== "mint" |
                      fach=="Alle")%>%
      tidyr::pivot_wider(values_from = wert, names_from = geschlecht)%>%
      dplyr::mutate(fva = Frauen/Gesamt*100)%>%
      dplyr::select(- Gesamt)%>%
      dplyr::mutate(display_rel = prettyNum(round(.$fva,1), big.mark = ".", decimal.mark = ","))%>%
      dplyr::mutate(display_total = prettyNum(.$Frauen, big.mark = ".", decimal.mark = ","))%>%
      dplyr::rename(wert = fva)


    df_share_fem2 <<- df_filtered %>%
      dplyr::select(-fachbereich)%>%
      dplyr::filter(mint_select== "mint" & geschlecht == "Frauen" |
                      fach=="Alle" & geschlecht == "Frauen")%>%
      dplyr::select(-mint_select)

    df_share_fem3 <<- df_filtered %>%
      dplyr::select(-fachbereich)%>%
      dplyr::filter(mint_select== "mint" & geschlecht == "Frauen" |
                      fach=="Alle" & geschlecht == "Frauen") %>%
      dplyr::filter(fach== "Alle")%>%
      dplyr::rename(Alle = wert)%>%
      dplyr::select(-fach,-mint_select)

    df_share_fem4 <<- df_share_fem2 %>%
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
      dplyr::mutate(fvf = wert/Alle*100)%>%
      dplyr::select(-Alle)%>%
      dplyr::mutate(display_rel = prettyNum(round(.$fvf,1), big.mark = ".", decimal.mark = ","))%>%
      dplyr::mutate(display_total = prettyNum(.$wert, big.mark = ".", decimal.mark = ","))%>%
      dplyr::select(-wert)%>%
      dplyr::rename(wert = fvf)


    if(betr =="Anteil von Frauen an Allen"){

      map_data_1 <- df_share_fem %>%
        dplyr::select(land, jahr, fach, wert, display_rel, display_total) %>%
        dplyr::inner_join(countries_names, by = "land") %>%
        dplyr::mutate(alpha2 = toupper(alpha2))%>%
        dplyr::filter(fach == fach_m)

      title_dyn <- if(fach_m=="MINT"){
        paste("Anteil von weiblichen Studierenden an allen Studierenden in allen MINT-Fächern " , timerange)

      } else if (fach_m=="Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe"){

        paste("Anteil von weiblichen Studierenden an allen Studierenden in Ingenieurwesen, verarbeitendem Gewerbe und Baugewerbe " , timerange)

      } else {

        paste("Anteil von weiblichen Studierenden an allen Studierenden in ", fach_m, " " , timerange)
      }


    } else if(betr=="Anteil an Frauen von Frauen") {
      map_data_1 <- df_share_fem4 %>%
        dplyr::select(land, jahr, fach, wert, display_rel, display_total) %>%
        dplyr::inner_join(countries_names, by = "land") %>%
        dplyr::mutate(alpha2 = toupper(alpha2))%>%
        dplyr::filter(fach == fach_m)


      title_dyn <- if(fach_m=="MINT"){
        paste("Anteil von Studierenden in allen MINT-Fächern an weiblichen Studierenden " , timerange, " (weltweit, OECD)")

      } else if (fach_m=="Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe"){

        paste("Anteil von Studierenden in Ingenieurwesen, verarbeitendem Gewerbe und Baugewerbe an weiblichen Studierenden " , timerange, " (weltweit, OECD)")

      } else {

        paste("Anteil von Studierenden in ", fach_m,  " an weiblichen Studierenden " , timerange, " (weltweit, OECD)")
      }

    }


    hoverplot <- "{point.land} <br> Anteil: {point.display_rel}% <br> Anzahl: {point.display_total}"






  }

  highcharter::hw_grid(
    # plot
    highcharter::hcmap(
      #"countries/de/de-all",
      map = map_selection,
      data = map_data_1,
      value = "wert",
      joinBy = c("hc-a2", "alpha2"),
      borderColor = "#FAFAFA",
      name = paste0(fach_m),
      borderWidth = 0.1,
      nullColor = "#A9A9A9",
      tooltip = list(
        valueDecimals = 0,
        valueSuffix = "%"
      )
      #,
      #download_map_data = FALSE
    )
    %>%
      highcharter::hc_tooltip(pointFormat = hoverplot) %>%
      highcharter::hc_colorAxis(min=0, minColor= "#f4f5f6", maxColor="#b16fab",labels = list(format = "{text}%")) %>%
      highcharter::hc_title(
        text = title_dyn,
        margin = 10,
        align = "center",
        style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
      ) %>%
      # highcharter::hc_caption(
      #   text = capt_dyn,  style = list(color= "grey", fontSize = "12px")
      # ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular")
      ) %>% highcharter::hc_size(600, 550) %>%
      highcharter::hc_credits(enabled = FALSE) %>%
      highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                             verticalAlign = "bottom")
  )




}


plot_international_top10 <- function(r) {
  #r <- list(map_y = "2019", map_l = "OECD", map_f = "MINT")
  # load UI inputs from reactive value


  timerange <- r$map_y_m
  label_m <- r$map_l_m
  fach_m <- r$map_f_m
  show_avg <- r$show_avg_top10_mint_line_m



  if (is.null(fach_m)) { fach_m <- ""}
  logger::log_debug("Plotting international map for:")
  logger::log_debug("Time: ", timerange,
                    " | Label: ", label_m,
                    " | Fach: ", fach_m,
                    " | Avg.: ", show_avg)


  if (label_m == "Weltweit") {
    #fach_m <- "Alle MINT-Fächer"

    df1 <- studierende_absolventen_weltweit  %>%
      dplyr::filter(fach == "Alle MINT-Fächer" &
                      jahr == timerange &
                      land != "San Marino") %>%
      dplyr::mutate(wert = round(wert, 1)) %>%
      dplyr::select(land, wert)
  }
  else if (label_m == "OECD") {

    # filter for selection
    df_filtered <- studierende_anzahl_oecd %>%
      dplyr::filter(geschlecht == "Gesamt" &
                      jahr == timerange &
                      ebene == 1 &
                      anforderung %in% c("Bachelor oder vergleichbar (akademisch)",
                                         "Master oder vergleichbar (akademisch)",
                                         "Promotion (ISCED 8)"))

    # filtern - nur Länder, für die alle Studi-Anforderungsniveaus vorliegend sind
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
    df <- studierende_europa %>%
      dplyr::filter(geschlecht == "Gesamt" &
                      jahr == timerange &
                      (mint_select == "mint" |
                         (mint_select == "nicht mint" &
                            fach_m == "Alle MINT-Fächer")) &
                      fach == fach_m &
                      indikator == "Fächerwahl") %>%
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
  t_quelle1 <- ifelse(label_m == "EU", "EU-", t_quelle1)



  # Create top 10 plot
  plot_top <- highcharter::hchart(
    df %>% dplyr::arrange(desc(wert)) %>% dplyr::slice(1:10),
    'bar',
    highcharter::hcaes(y = wert, x = land)) %>%
    get_top10_hc_plot_options(
      hc_title = paste0(t_quelle1, "Länder mit dem größten Anteil an ", t_gruppe, t_fach, " in ", timerange, t_quelle),
      hc_tooltip = hover,
      max_percent_used = max_percent_used)

  # Create bottom 10 plot
  plot_bottom <- highcharter::hchart(
    df %>% dplyr::arrange(desc(wert)) %>% dplyr::slice_tail(n = 10),
    'bar',
    highcharter::hcaes(y = wert, x = land)) %>%
    get_top10_hc_plot_options(
      hc_title = paste0(t_quelle1, "Länder mit dem niedrigsten Anteil an ", t_gruppe, t_fach, " in ", timerange, t_quelle),
      hc_tooltip = hover,
      max_percent_used = max_percent_used)


  if (show_avg == "Ja") {
    plot_top <- plot_top %>%
      add_avg_to_hc(hc_mean = mean(df$wert, na.rm = TRUE))
    plot_bottom <- plot_bottom %>%
      add_avg_to_hc(hc_mean = mean(df$wert, na.rm = TRUE))
  }

  highcharter::hw_grid(
    plot_top,
    plot_bottom,
    ncol = 2)



}


plot_international_top10_gender <- function(r) {
  #r <- list(map_y_int_studium_gender = "2021", map_l_int_studium_gender = "EU", map_f_int_studium_gender = "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Ingenieurwesen,\n verarbeitendes Gewerbe und Baugewerbe",show_avg_top10_mint_line = "Ja", show_avg_int_studium_gender = "meisten Frauen wählen MINT")
  # load UI inputs from reactive value



  timerange <- r$map_y_g
  label_m <- r$map_l_g
  fach_m <- r$map_f_g
  show_avg <- r$show_avg_g
  # höchster Frauenanteil in MINT vs meiste Frauen wählen MINT
  # AA vs BB
  art <- r$art_g




  if (is.null(fach_m)) { fach_m <- ""}
  if (is.null(art)) { art <- ""}

  logger::log_debug("Plotting international map for:")
  logger::log_debug("Time: ", timerange,
                    " | Label: ", label_m,
                    " | Fach: ", fach_m,
                    " | Avg.: ", show_avg,
                    " | Type: ", art)


  if (label_m == "OECD" & art == "meisten Frauen wählen MINT") {
    # meiste Frauen wählen MINT

    # filter for selection
    df_filtered <- studierende_anzahl_oecd %>%
      dplyr::filter(geschlecht == "Frauen" &
                      jahr == timerange &
                      ebene == 1 &
                      anforderung %in% c("Bachelor oder vergleichbar (akademisch)",

                                         "Master oder vergleichbar (akademisch)",
                                         "Promotion (ISCED 8)"))

    # filtern - nur Länder, für die alle Studi-Anforderungsniveaus vorliegend sind
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

    # filter for selection
    df_filtered <- studierende_anzahl_oecd %>%
      dplyr::filter(fachbereich == fach_m &
                      jahr == timerange &
                      ebene == 1 &
                      anforderung %in% c("Bachelor oder vergleichbar (akademisch)",

                                         "Master oder vergleichbar (akademisch)",
                                         "Promotion (ISCED 8)"))

    # filtern - nur Länder, für die alle Studi-Anforderungsniveaus vorliegend sind
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

    df <- studierende_europa %>%
      dplyr::filter(geschlecht == "Frauen" &
                      jahr == timerange &
                      (mint_select == "mint" |
                         (mint_select == "nicht mint" &
                            fach_m == "Alle MINT-Fächer")) &
                      fach == fach_m &
                      indikator == "Frauen-/Männeranteil" &
                      land != "EU (27), seit 2020") %>%
      dplyr::select(land, wert)

  }
  if (label_m == "EU" & art == "meisten Frauen wählen MINT") {
    # meiste Frauen wählen MINT

    df <- studierende_europa %>%
      dplyr::filter(geschlecht == "Frauen" &
                      jahr == timerange &
                      (mint_select == "mint" |
                         (mint_select == "nicht mint" &
                            fach_m == "Alle MINT-Fächer")) &
                      fach == fach_m &
                      indikator == "Fächerwahl"&
                      land != "EU (27), seit 2020") %>%
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
  t_quelle1 <- ifelse(label_m == "EU", "EU-", t_quelle1)
  t_fach <- fach_m
  t_fach <- ifelse(t_fach %in% c("MINT", "Alle MINT-Fächer"), "MINT-Fächern", t_fach)
  t_fach <- ifelse(t_fach == "Dienstleistungen", "Fächern aus dem Bereich 'Dienstleistungen'", t_fach)


  # Create top 10 plot
  plot_top <- highcharter::hchart(
    df %>% dplyr::arrange(desc(wert)) %>% dplyr::slice(1:10),
    'bar',
    highcharter::hcaes(y = wert, x = land)) %>%
    get_top10_hc_plot_options(
      hc_title = paste0(t_quelle1, "Länder mit dem größten Frauenanteil an Studierenden in ", t_fach, " in ", timerange),
      hc_tooltip = hover
      # ,
      # max_percent_used = max_percent_used
      )


  # Create bottom 10 plot
  plot_bottom <- highcharter::hchart(
    df %>% dplyr::arrange(desc(wert)) %>% dplyr::slice_tail(n = 10),
    'bar',
    highcharter::hcaes(y = wert, x = land)) %>%
    get_top10_hc_plot_options(
      hc_title = paste0("Länder mit dem niedrigsten Anteil an '", fach_m, "' in ", timerange),
      hc_tooltip = "Anteil: {point.wert} % <br> Anzahl: {point.wert_absolut}"
      # ,
      # max_percent_used = max_percent_used
      )


  if (show_avg == "Ja") {
    plot_top <- plot_top %>%
      add_avg_to_hc(hc_mean = mean(df$wert, na.rm = TRUE))
    plot_bottom <- plot_bottom %>%
      add_avg_to_hc(hc_mean = mean(df$wert, na.rm = TRUE))
  }

  highcharter::hw_grid(
    plot_top,
    plot_bottom,
    ncol = 2)



}



plot_international_mint_top_10 <- function(r){


# Überschriften anpassen, einheitlich mit anderen plots

data_eu_abs <<- studierende_mobil_eu_absolut


avg_line <<- r$show_avg_ti


  inpy <<- r$map_y_ti
  # inpf <<- r$map_f_ti
#
#   test <- data_eu_abs%>%
#     dplyr::filter(geschlecht=="Gesamt" &
#                     anforderung %in% c("Bachelor oder vergleichbar (ISCED 6)",
#                                        "Master oder vergleichbar (ISCED 7)",
#                                        "Promotion (ISCED 8)"))%>%
#     tidyr::pivot_wider(values_from=wert, names_from = anforderung)%>%
#       dplyr::mutate(testo = rowSums(dplyr::select(., "Bachelor oder vergleichbar (ISCED 6)",
#                                                   "Master oder vergleichbar (ISCED 7)",
#                                                   "Promotion (ISCED 8)"), na.rm=T))%>%
#       dplyr::select(-c("Bachelor oder vergleichbar (ISCED 6)",
#                        "Master oder vergleichbar (ISCED 7)",
#                        "Promotion (ISCED 8)"))


  data1 <<- data_eu_abs%>%
    dplyr::filter(geschlecht=="Gesamt" &
                    anforderung %in% c("Bachelor oder vergleichbar (ISCED 6)",
                                       "Master oder vergleichbar (ISCED 7)",
                                       "Promotion (ISCED 8)") &
                    fach== "MINT"&
                    !is.na(.$wert))%>%
    dplyr::group_by(fach,geschlecht,
                    land,jahr,
                    kommentar,ebene,mint_select,
                    bereich,indikator,typ )%>%
    dplyr::summarise(fach,geschlecht,
                     land,jahr,
                     kommentar,ebene,mint_select,
                     bereich,indikator,typ, wert= sum(wert,na.rm =T))%>%
    unique()%>%
    dplyr::ungroup()%>%
    dplyr::filter(jahr == inpy #&
                    # fach == inpf
 )%>% dplyr::mutate(display_total = prettyNum(.$wert, big.mark = ".", decimal.mark = ","))



    # dplyr::mutate(wert=dplyr::case_when(wert == 0 ~ NA,
    #                                     T ~ wert))%>%
    # dplyr::filter(!is.na(.$wert)) ### Doppelung Deutschland!
### Hnweis ergänzen keine 0s


  title_dyn_top <- paste("Länder Europas mit der höchsten Zahl an \ninternationalen Studierenden in MINT im Jahr", inpy)
  title_dyn_bot <- paste("Länder Europas mit der niedrigsten Zahl an \ninternationalen Studierenden in MINT im Jahr", inpy)
  #capt_dyn  <- paste("Quelle der Daten: Eurostat, 2022, eigene Berechnungen durch MINTvernetzt")

if (avg_line == "Ja"){

  data_avg <<- round(mean(data1$wert, na.rm = T),0)

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
          color = "#FF0000",
          width = 3,
          zIndex = 4
        )
      ),title = list(text = ""),
      labels = list(format = "{value}"),
      min = 0,
      max = max(data1$wert)*1.2)%>%
      highcharter::hc_xAxis(title = list(text = " ")) %>%
      highcharter::hc_colors(c("#154194")) %>%
      highcharter::hc_title(text = title_dyn_top,
                            margin = 10,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
                            ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE)
      # highcharter::hc_caption(
      #   text = capt_dyn,  style = list(color= "grey", fontSize = "12px"))




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
                                color = "#FF0000",
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
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
      ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE)



    highcharter::hw_grid(
      plot_top,
      plot_bottom,
      ncol = 2)

} else if (avg_line == "Nein"){



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
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
    ) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE)
    # highcharter::hc_caption(
    #   text = capt_dyn,  style = list(color= "grey", fontSize = "12px"))



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
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
    ) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE)


  highcharter::hw_grid(
    plot_top,
    plot_bottom,
    ncol = 2)


}



}

## schule ----
plot_international_schule_map <- function(r) {

  #r <- list(map_y_int_schule = "2019", map_l_int_schule = "TIMSS", map_f_int_schule = "Mathematik", map_li_int_schule = "Test-Punktzahl")
  # load UI inputs from reactive value

  timerange <- r$map_y_int_schule
  label_m <- r$map_l_int_schule
  fach_m <- r$map_f_int_schule
  leistungsindikator_m <- r$map_li_int_schule

  if (is.null(fach_m)) { fach_m <- ""}
  logger::log_debug("Plotting international schule map for:")
  logger::log_debug("Time: ", timerange,
                    " | Label: ", label_m,
                    " | Indikator: ", leistungsindikator_m,
                    " | Fach: ", fach_m)

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

    df <- schule_timss %>%
      dplyr::filter(ordnung == this_ordnung &
                      indikator == this_indikator)

    help_l <- "4. Klasse"
  }
  if (label_m == "PISA") {
    df <- schule_pisa %>%
      dplyr::filter(bereich == "Ländermittel" &
                      indikator == "Insgesamt")

    help_l <- "9. & 10. Klasse"
  }


  # filter dataset based on UI inputs
  dfs <- df %>%
    dplyr::filter(jahr == timerange &
                    fach == fach_m &
                    !is.na(wert))


  # Hover & Titel vorbereiten
  titel <- paste0("Durschnittliche Leistung von Schüler:innen der ", help_l,
                  " im ", fach_m, "-Kompetenztest von ",
                  label_m, " ", timerange)

  dfs$display_wert <- prettyNum(round(dfs$wert, 0),
                                big.mark = ".",
                                decimal.mark = ",")

  if (leistungsindikator_m == "Test-Punktzahl") {
    tooltip_prefix <- "Punktzahl"
    tooltip_scale <- ""
  }
  if (leistungsindikator_m == "Mittlerer Standard erreicht") {
    tooltip_prefix <- "Anteil"
    tooltip_scale <- "%"
    titel <- paste0("Anteil an Schüler:innen der ", help_l,
                    " die im ", fach_m, "-Kompetenztest von ",
                    label_m, " den mittleren internationalen Standard erreichen ", timerange)
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


  map_selection <- golem::get_golem_options("world_map")

  # plot
  highcharter::highchart(type = "map") %>%
    highcharter::hc_add_series_map(
      #"countries/de/de-all",
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
                   fontFamily = "SourceSans3-Regular",
                   fontSize = "20px")
    ) %>%
    # highcharter::hc_caption(
    #   text = "...",  style = list(color= "white", fontSize = "12px")
    # ) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular")
    ) %>% #highcharter::hc_size(600, 550) %>%
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

}

plot_international_schule_item <- function(r) {

  #r <- list(map_y = "2019", map_l = "TIMSS", map_f = "Mathematik", map_li = "Test-Punktzahl")
  #r <- list(map_y = "2018", map_l = "PISA", map_f = "Mathematik",  map_li = "Test-Punktzahl")
  # load UI inputs from reactive value

  timerange <- r$item_y_int_schule
  label_m <- "TIMSS"
  fach_m <- r$item_f_int_schule

  if (is.null(fach_m)) { fach_m <- ""}
  logger::log_debug("Plotting international schule item for:")
  logger::log_debug("Time: ", timerange,
                    " | Label: ", label_m,
                    " | Fach: ", fach_m)

  # filter dataset based on UI inputs
  df <- schule_timss %>%
    dplyr::filter(jahr == timerange &
                    fach == fach_m &
                    ordnung == "Gender")

  set_group <- function(gender, diff) {
    # Anhanme, dass nie zweimal "Ja" kommt, sonst kein Unterschied
    # gender <- c("Jungen", "Mädchen")
    # gender <- c("Mädchen", "Jungen")
    # diff <- c("Nein", "Ja")
    # diff <- c("Nein", "Nein")
    # diff <- c("Ja", "Nein")
    # diff <- c("Ja", "Ja")

    if (all(diff == "Nein") | all(diff == "Ja")) {
      out <- "kein signifikanter Unterschied"
    } else if (diff[gender == "Jungen"] == "Ja") {
      out <- "Jungen signifikant besser"
    } else if (diff[gender == "Mädchen"] == "Ja") {
      out <- "Mädchen signifikant besser"
    }

    return(out)
  }

  # enthält den Text für den plot
  group_col_dt <- data.frame(
    group = c("kein signifikanter Unterschied", "Jungen signifikant besser", "Mädchen signifikant besser"),
    group_text = c(" mit keinem signifikaten Unterschied zwischen Jungen und Mädchen",
                   ", in denen Jungen signifikant besser abschneiden als Mädchen",
                   ", in denen Mädchen signifikant besser abscheniden als Jungen"),
    group_col = c("#35bd97", "#FBBF24", "#154194")
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
                                       diff = geschlecht_diff),
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
  plot_data$group_col <- ifelse(plot_data$land == "Deutschland" & plot_data$group == "Jungen signifikant besser", "#FEF3C7", plot_data$group_col )
  plot_data$group_col <- ifelse(plot_data$land == "Deutschland" & plot_data$group == "Mädchen signifikant besser", "#5f94f9", plot_data$group_col )
  plot_data$group_col <- ifelse(plot_data$land == "Deutschland" & plot_data$group == "kein signifikanter Unterschied", "#AFF3E0", plot_data$group_col )


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
                    label_m, " im Ländervergleich (", timerange, ")"),
      margin = 10,
      align = "center",
      style = list(color = "black",
                   useHTML = TRUE,
                   fontFamily = "SourceSans3-Regular",
                   fontSize = "20px")
    ) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular")
    ) %>%
    highcharter::hc_size(600, 450) %>%
    highcharter::hc_credits(enabled = FALSE)
  #%>%
  # highcharter::hc_legend(layout = "horizontal", floating = FALSE,
  #                        verticalAlign = "bottom")


  return(out)
}


plot_international_schule_migration <- function(r) {

  #r <- list(line_y_int_schule = "2019", line_l_int_schule = "TIMSS", line_f_int_schule = "Mathematik", line_li_int_schule = "nach Geschlecht")
  # r <- list(line_y_int_schule = "2018", line_l_int_schule = "PISA", line_f_int_schule = "Mathematik", line_li_int_schule = "nach Zuwanderungsgeschichte")
  # load UI inputs from reactive value

  timerange <- r$line_y_int_schule
  label_m <- r$line_l_int_schule
  fach_m <- r$line_f_int_schule
  leistungsindikator_m <- r$line_li_int_schule


  if (is.null(fach_m)) { fach_m <- ""}
  logger::log_debug("Plotting international schule migration for:")
  logger::log_debug("Time: ", timerange,
                    " | Label: ", label_m,
                    " | Indikator: ", leistungsindikator_m,
                    " | Fach: ", fach_m)


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

    df <- schule_timss %>%
      dplyr::filter(ordnung == this_ordnung &
                      indikator %in% this_indikator &
                      typ == this_typ)

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
      "nach Zuwanderungsgeschichte" = "Migrationshintergund",
      "nach Bildungskapital" = "Bücher im Haushalt"
    )

    this_indikator <- switch(
      leistungsindikator_m,
      "nach Geschlecht" = c("Jungen", "Mädchen"),
      "nach Zuwanderungsgeschichte" = c("Keiner",
                                        "Zweite Generation",
                                        "Erste Generation"),
      "nach Bildungskapital" = c("0-10", "26-100", "Mehr als 500")
    )

    df <- schule_pisa %>%
      dplyr::filter(bereich == this_bereich &
                      indikator %in% this_indikator)

    # Labels anpassen
    df$indikator[df$indikator == "Keiner"] <- "ohne Zuwanderungsgeschichte"
    df$indikator[df$indikator == "Zweite Generation"] <- "Eltern zugewandert"
    df$indikator[df$indikator == "Erste Generation"] <- "Kind selbst zugewandert"
    df$indikator[df$indikator == "0-10"] <- "sehr niedriges Bildungskapital (bis zu 10 Bücher zuhause)"
    df$indikator[df$indikator == "26-100"] <- "niedriges Bildungskapital (bis zu 100 Bücher zuhause)"
    df$indikator[df$indikator == "Mehr als 500"] <- "hohes Bildungskapital (über 500 Bücher zuhause)"

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
  dfs$display_wert <- prettyNum(round(dfs$wert, 0),
                                big.mark = ".",
                                decimal.mark = ",")


  data_line <- dfs %>%
    dplyr::select(land, wert, display_wert, indikator)

  line_colors <- c("#B16FAB", "#154194", "#35BD97",
                   "#8893A7", "#FBBF24", "#9D7265")
  color <- line_colors[seq_along(this_indikator)]

  # plot
  highcharter::hchart(
    object = data_line,
    type = 'line',
    highcharter::hcaes(y = wert, x = land, group = indikator)
  ) %>%
    highcharter::hc_plotOptions(column = list(pointWidth = 90))%>%
    highcharter::hc_tooltip(pointFormat = "{point.indikator} <br> {point.display_wert} Pkt")%>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}"), pointsWidth=100) %>%
    highcharter::hc_xAxis(title = list(text = ""), labels = list(rotation = 270)) %>%
    #  highcharter::hc_plotOptions(column = list(stacking = "percent")) %>%
    #highcharter::hc_colors(c("#efe8e6","#D0A9CD", "#b16fab")) %>%
    highcharter::hc_colors(color) %>%
    highcharter::hc_title(
      text = paste0("Durchschnittliche Leistung von Schüler:innen der ", help_l,
                    " im ", fach_m, "-Kompetenztest von ",label_m, " ",
                    leistungsindikator_m, " (", timerange, ")"),
      margin = 45,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = FALSE) %>%
    highcharter::hc_exporting(
      enabled = FALSE,
      buttons = list(contextButton = list(
        symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
        onclick = highcharter::JS("function () {
                                                            this.exportChart({ type: 'image/png' }); }"),
        align = 'right',
        verticalAlign = 'bottom',
        theme = list(states = list(hover = list(fill = '#FFFFFF')))
      )
      )
    )


}


## arbeitsmarkt ----




plot_international_map_arb <- function(r) {

  oecd_abs_anfänger <- arbeitsmarkt_anfänger_absolv_oecd
  oecd_abs_anfänger <<-oecd_abs_anfänger%>% dplyr::filter(!is.na(.$wert))

  oecd_azub <- arbeitsmarkt_anzahl_azubis_oecd
  oecd_azub <<- oecd_azub%>% dplyr::filter(!is.na(.$wert))

  eu_besch <- arbeitsmarkt_beschäftigte_eu
  eu_besch <<- eu_besch%>% dplyr::filter(!is.na(.$wert))

  map_l <<- r$map_l_arb

  if(map_l== "EU"){

    inpy <<- r$map_y_arb
    inpp <<- r$map_pers_arb

    map_selection <- "custom/europe"

    # test <- eu_besch %>%
    #   dplyr::filter(geschlecht == "Gesamt"&
    #                   variable == "Anteil an arbeitender Bevölkerung")

    data1 <<- eu_besch %>%
      dplyr::filter(geschlecht == "Gesamt"&
                      jahr == inpy &
                      indikator == inpp&
                      variable == "Anteil an arbeitender Bevölkerung")%>%
      tidyr::pivot_wider(names_from = variable, values_from = wert)%>%
      dplyr::rename(wert="Anteil an arbeitender Bevölkerung")

    data1$display_rel <- prettyNum(round(data1$wert,1), big.mark = ".", decimal.mark = ",")


    data2 <<- eu_besch %>%
      dplyr::filter(geschlecht == "Gesamt"&
                      jahr == inpy &
                      indikator == inpp&
                      variable == "Anzahl in Tsd.")%>%
      tidyr::pivot_wider(names_from = variable, values_from = wert)%>%
      dplyr::mutate(across(`Anzahl in Tsd.`, ~ as.numeric(.)*1000))%>%
      dplyr::rename(display_total = "Anzahl in Tsd." )%>%
      dplyr::select(display_total, land)%>%
      dplyr::mutate(across(display_total, ~ prettyNum(., big.mark = ".", decimal.mark = ",")))

    data_map <- data1 %>%
      dplyr::left_join(data2,by=c("land"))%>%
      dplyr::left_join(countries_names %>%
                         dplyr::mutate(land=dplyr::case_when(land == "Tschechien" ~ "Tschechische Republik",
                                                      T ~ .$land)), by= "land")%>%
      dplyr::mutate(alpha2= toupper(alpha2))



    title_eu <- paste0(inpp, "n", " in MINT-Fächern an allen ", inpp, "n ",inpy )




    highcharter::hw_grid(
      # plot
      highcharter::hcmap(
        #"countries/de/de-all",
        map = map_selection,
        data = data_map,
        value = "wert",
        joinBy = c("hc-a2", "alpha2"),
        borderColor = "#FAFAFA",
        name = paste0(inpp),
        borderWidth = 0.1,
        nullColor = "#A9A9A9",
        tooltip = list(
          valueDecimals = 0,
          valueSuffix = "%"
        )
        ,
        download_map_data = T
      )
      %>%
        highcharter::hc_tooltip(pointFormat = "{point.land} <br> Anteil: {point.display_rel}% <br> Anzahl: {point.display_total}") %>%
        highcharter::hc_colorAxis(min=0, minColor= "#f4f5f6", maxColor="#b16fab",labels = list(format = "{text}%")) %>%
        highcharter::hc_title(
          text = paste0("Anteil von ", title_eu, " in Europa"),
          margin = 10,
          align = "center",
          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
        ) %>%
        # highcharter::hc_caption(
        #   text = "...",  style = list(color= "white", fontSize = "12px")
        # ) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular")
        ) %>% highcharter::hc_size(600, 550) %>%
        highcharter::hc_credits(enabled = FALSE) %>%
        highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                               verticalAlign = "bottom")
    )
  }


  else if (map_l== "OECD"){

    map_selection <- "custom/world"

    inpp <<- r$map_pers_arb
    inpy <<- r$map_y_arb
    inpf <<- r$map_f_arb


    if(inpp %in%  c("Anfänger*innen Ausbildung (ISCED 45)",
       "Anfänger*innen Erstausbildung (ISCED 35)",
       "Absolvent*innen Ausbildung (ISCED 45)",
       "Absolvent*innen Erstausbildung (ISCED 35)")){

      # test<- oecd_abs_anfänger%>%
      #   dplyr::filter(
      #                   fachbereich %in% c("MINT",
      #                                      "Informatik & Kommunikationstechnologie",
      #                                      "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
      #                                      "Naturwissenschaften, Mathematik und Statistik",
      #                                      "Alle")&
      #                   geschlecht == "Gesamt")%>%
      #   dplyr::mutate(display_rel= prettyNum(round(.$wert, 1), big.mark = ".", decimal.mark = ","))

      data1 <- oecd_abs_anfänger%>%
        dplyr::filter(jahr == inpy &
                      fachbereich %in% c("MINT",
                                         "Informatik & Kommunikationstechnologie",
                                         "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                         "Naturwissenschaften, Mathematik und Statistik",
                                         "Alle")&
                        geschlecht == "Gesamt")%>%
        dplyr::mutate(display_rel= prettyNum(round(.$wert, 1), big.mark = ".", decimal.mark = ","))

      if(inpp == "Anfänger*innen Ausbildung (ISCED 45)"){

        data_map <<- data1 %>%
          dplyr::filter(anforderung == "Ausbildung (ISCED 45)" &
                          variable == "Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern" &
                          fachbereich == inpf)%>%
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))


      } else if (inpp == "Anfänger*innen Erstausbildung (ISCED 35)"){

        data_map <- data1 %>%
          dplyr::filter(anforderung == "Erstausbildung (ISCED 35)" &
                          variable == "Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern"&
                          fachbereich == inpf)%>%
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))

      } else if (inpp == "Absolvent*innen Ausbildung (ISCED 45)"){

        data_map <- data1 %>%
          dplyr::filter(anforderung == "Ausbildung (ISCED 45)" &
                          variable == "Anteil Absolvent*innen nach Fach an allen Fächern"&
                          fachbereich == inpf)%>%
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))

      } else if (inpp == "Absolvent*innen Erstausbildung (ISCED 35)"){

        data_map <- data1 %>%
          dplyr::filter(anforderung == "Erstausbildung (ISCED 35)" &
                          variable == "Anteil Absolvent*innen nach Fach an allen Fächern"&
                          fachbereich == inpf)%>%
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))

      }


  title_oecd_1_1 <- if(inpp == "Anfänger*innen Ausbildung (ISCED 45)"){
    paste0("Ausbildungsanfänger*innen (ISCED 45)")
  }else if(inpp =="Anfänger*innen Erstausbildung (ISCED 35)"){
    paste0("Anfänger*innen in Erstausbildung (ISCED 35)")
  }else if(inpp =="Absolvent*innen Ausbildung (ISCED 45)"){
    paste0("Ausbildungsabsolvent*innen (ISCED 45)")
  }else if(inpp =="Absolvent*innen Erstausbildung (ISCED 35)"){
    paste0("Absolvent*innen der Erstausbildung (ISCED 35)")
  }



      highcharter::hw_grid(
        # plot
        highcharter::hcmap(
          #"countries/de/de-all",
          map = map_selection,
          data = data_map,
          value = "wert",
          joinBy = c("hc-a2", "alpha2"),
          borderColor = "#FAFAFA",
          name = paste0(inpp),
          borderWidth = 0.1,
          nullColor = "#A9A9A9",
          tooltip = list(
            valueDecimals = 0,
            valueSuffix = "%"
          )
          ,
          download_map_data = T
        )
        %>%
          highcharter::hc_tooltip(pointFormat = "{point.land} <br> Anteil: {point.display_rel}%") %>%
          highcharter::hc_colorAxis(min=0, minColor= "#f4f5f6", maxColor="#b16fab",labels = list(format = "{text}%")) %>%
          highcharter::hc_title(
            text = paste0("Anteil von ", title_oecd_1_1, " in ", inpf, " ", inpy, " weltweit (OECD)" ),
            margin = 10,
            align = "center",
            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
          ) %>%
          # highcharter::hc_caption(
          #   text = "...",  style = list(color= "white", fontSize = "12px")
          # ) %>%
          highcharter::hc_chart(
            style = list(fontFamily = "SourceSans3-Regular")
          ) %>% highcharter::hc_size(600, 550) %>%
          highcharter::hc_credits(enabled = FALSE) %>%
          highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                                 verticalAlign = "bottom")
      )


    } else {

      # test1 <- oecd_azub %>%
      #   dplyr::filter(geschlecht == "Gesamt" &
      #                   indikator == "berufsorientiert" &
      #                   #jahr == inpy &
      #                   fach %in% c("MINT",
      #                               "Informatik & Kommunikationstechnologie",
      #                               "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
      #                               "Naturwissenschaften, Mathematik und Statistik",
      #                               "Alle"))%>%
      #   tidyr::pivot_wider(values_from = wert, names_from = fach)

      data1 <- oecd_azub %>%
        dplyr::filter(geschlecht == "Gesamt" &
                 indikator == "berufsorientiert" &
                 jahr == inpy &
                 fach %in% c("MINT",
                             "Informatik & Kommunikationstechnologie",
                             "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                             "Naturwissenschaften, Mathematik und Statistik",
                             "Alle"))%>%
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

      data2 <- oecd_azub %>%
        dplyr::filter(geschlecht == "Gesamt" &
                        indikator == "berufsorientiert" &
                        jahr == inpy &
                        fach %in% c("MINT",
                                    "Informatik & Kommunikationstechnologie",
                                    "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                    "Naturwissenschaften, Mathematik und Statistik",
                                    "Alle"))%>%
        dplyr::rename(display_total = wert)%>%
        dplyr::mutate(display_total= prettyNum(.$display_total, big.mark = ".", decimal.mark = ","))%>%
        dplyr::select(land, jahr, display_total, fach, anforderung)

      data3 <- data1 %>%
        dplyr::left_join(data2, by=c("land", "jahr", "fach", "anforderung"))%>%
        dplyr::inner_join(countries_names, by = "land") %>%
        dplyr::mutate(alpha2 = toupper(alpha2))


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


      title_oecd_2_1 <- if(inpp == "Auszubildende (ISCED 45)"){
          paste0("Auszubildenden (ISCED 45)")
      } else if(inpp == "In Meisterlehre (< 880 Std. Vorbereitung, ISCED 55)"){
          paste0("Meisterlehrlingen (< 880 Std. Vorbereitung, ISCED 55)")
      } else if(inpp == "Auszubildende in Erstausbildung (ISCED 35)"){
          paste0("Auszubildenden in Erstausbildung (ISCED 35)")
      }else if (inpp == "In Meister-/Technikerlehre (> 880 Std. Vorbereitung, ISCED 65)"){
          paste0("Meister-/Technikerlehrlingen (> 880 Std. Vorbereitung, ISCED 65)")
      }



      highcharter::hw_grid(
        # plot
        highcharter::hcmap(
          #"countries/de/de-all",
          map = map_selection,
          data = data_map,
          value = "wert",
          joinBy = c("hc-a2", "alpha2"),
          borderColor = "#FAFAFA",
          name = paste0(inpp),
          borderWidth = 0.1,
          nullColor = "#A9A9A9",
          tooltip = list(
            valueDecimals = 0,
            valueSuffix = "%"
          )
          ,
          download_map_data = T
        )
        %>%
          highcharter::hc_tooltip(pointFormat = "{point.land} <br> Anteil: {point.display_rel}% <br> Anzahl: {point.display_total}") %>%
          highcharter::hc_colorAxis(min=0, minColor= "#f4f5f6", maxColor="#b16fab",labels = list(format = "{text}%")) %>%
          highcharter::hc_title(
            text = paste0("Anteil von ", title_oecd_2_1, " in ", inpf, " ", " inpy, weltweit (OECD)" ),
            margin = 10,
            align = "center",
            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
          ) %>%
          # highcharter::hc_caption(
          #   text = "...",  style = list(color= "white", fontSize = "12px")
          # ) %>%
          highcharter::hc_chart(
            style = list(fontFamily = "SourceSans3-Regular")
          ) %>% highcharter::hc_size(600, 550) %>%
          highcharter::hc_credits(enabled = FALSE) %>%
          highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                                 verticalAlign = "bottom")
      )


      }



  }




}

plot_international_map_arb_gender <- function(r) {

  oecd_abs_anfänger <- arbeitsmarkt_anfänger_absolv_oecd
  oecd_abs_anfänger <<-oecd_abs_anfänger%>% dplyr::filter(!is.na(.$wert))

  oecd_azub <- arbeitsmarkt_anzahl_azubis_oecd
  oecd_azub <<- oecd_azub%>% dplyr::filter(!is.na(.$wert))

  eu_besch <- arbeitsmarkt_beschäftigte_eu
  eu_besch <<- eu_besch%>% dplyr::filter(!is.na(.$wert))

  inpl <<- r$map_l_arb_gender

  if(inpl== "EU"){

    inpy <<- r$map_y_arb_gender
    inpp <<- r$map_pers_arb_gender

    map_selection <- "custom/europe"

    # test <<- eu_besch %>%
    #   dplyr::filter(geschlecht %in% c("Gesamt", "Frauen")&
    #                   # jahr == inpy &
    #                   # indikator == inpp &
    #                   variable == "Anteil an arbeitender Bevölkerung")

    data1 <<- eu_besch %>%
      dplyr::filter(geschlecht %in% c("Gesamt", "Frauen")&
                      jahr == inpy &
                      indikator == inpp &
                      variable == "Anteil an arbeitender Bevölkerung")%>%
      tidyr::pivot_wider(names_from = geschlecht, values_from = wert)%>%
      dplyr::rename(wert="Frauen")%>%
      dplyr::select(-Gesamt)

    data1$display_rel <- prettyNum(data1$wert, big.mark = ".", decimal.mark = ",")


    data2 <<- eu_besch %>%
      dplyr::filter(geschlecht == "Frauen"&
                      jahr == inpy &
                      indikator == inpp &
                      variable == "Anzahl in Tsd.")%>%
      tidyr::pivot_wider(names_from = variable, values_from = wert)%>%
      dplyr::mutate(across(`Anzahl in Tsd.`, ~ as.numeric(.)*1000))%>%
      dplyr::rename(display_total = "Anzahl in Tsd." )%>%
      dplyr::select(display_total, land)%>%
      dplyr::mutate(across(display_total, ~ prettyNum(., big.mark = ".", decimal.mark = ",")))

    data_map <- data1 %>%
      dplyr::left_join(data2,by=c("land"))%>%
      dplyr::left_join(countries_names %>%
                         dplyr::mutate(land=dplyr::case_when(land == "Tschechien" ~ "Tschechische Republik",
                                                             T ~ .$land)), by= "land")%>%
      dplyr::mutate(alpha2= toupper(alpha2))

    title_eu <- paste0(inpp, "n", " in allen MINT-Fächern ", inpy )

    highcharter::hw_grid(
      # plot
      highcharter::hcmap(
        #"countries/de/de-all",
        map = map_selection,
        data = data_map,
        value = "wert",
        joinBy = c("hc-a2", "alpha2"),
        borderColor = "#FAFAFA",
        name = paste0(inpp),
        borderWidth = 0.1,
        nullColor = "#A9A9A9",
        tooltip = list(
          valueDecimals = 0,
          valueSuffix = "%"
        )
        ,
        download_map_data = T
      )
      %>%
        highcharter::hc_tooltip(pointFormat = "{point.land} <br> Anteil: {point.display_rel}% <br> Anzahl: {point.display_total}") %>%
        highcharter::hc_colorAxis(min=0, minColor= "#f4f5f6", maxColor="#b16fab",labels = list(format = "{text}%")) %>%
        highcharter::hc_title(
          text = paste0("Anteil von Frauen an allen ", title_eu, " in Europa"),
          margin = 10,
          align = "center",
          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
        ) %>%
        # highcharter::hc_caption(
        #   text = "...",  style = list(color= "white", fontSize = "12px")
        # ) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular")
        ) %>% highcharter::hc_size(600, 550) %>%
        highcharter::hc_credits(enabled = FALSE) %>%
        highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                               verticalAlign = "bottom")
    )


  }


  else if (inpl== "OECD"){

    map_selection <- "custom/world"

    inpp <<- r$map_pers_arb_gender
    inpy <<- r$map_y_arb_gender
    inpf <<- r$map_f_arb_gender



    if(inpp %in%  c("Anfänger*innen Ausbildung (ISCED 45)",
                    "Anfänger*innen Erstausbildung (ISCED 35)",
                    "Absolvent*innen Ausbildung (ISCED 45)",
                    "Absolvent*innen Erstausbildung (ISCED 35)")){



      # test <<- oecd_abs_anfänger %>%
      #   dplyr::filter(
      #                   fachbereich %in% c("MINT",
      #                                      "Informatik & Kommunikationstechnologie",
      #                                      "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
      #                                      "Naturwissenschaften, Mathematik und Statistik",
      #                                      "Alle")&
      #                   geschlecht =="Frauen")



      data1 <<- oecd_abs_anfänger %>%
        dplyr::filter(jahr == inpy &
                        fachbereich %in% c("MINT",
                                           "Informatik & Kommunikationstechnologie",
                                           "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                           "Naturwissenschaften, Mathematik und Statistik",
                                           "Alle")&
                        geschlecht =="Frauen")%>%
        tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
        dplyr::mutate(MINT = (rowSums(dplyr::select(., "Informatik & Kommunikationstechnologie",
                                                    "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                                    "Naturwissenschaften, Mathematik und Statistik"), na.rm = T))/3)%>%
        tidyr::pivot_longer(c("MINT",
                              "Informatik & Kommunikationstechnologie",
                              "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                              "Naturwissenschaften, Mathematik und Statistik",
                              "Alle"), values_to = "wert", names_to = "fachbereich")%>%
        dplyr::mutate(display_rel= prettyNum(.$wert, big.mark = ".", decimal.mark = ","))

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


      title_oecd_1_1 <- if(inpp == "Anfänger*innen Ausbildung (ISCED 45)"){
        paste0("Ausbildungsanfänger*innen (ISCED 45)")
      }else if(inpp =="Anfänger*innen Erstausbildung (ISCED 35)"){
        paste0("Anfänger*innen in Erstausbildung (ISCED 35)")
      }else if(inpp =="Absolvent*innen Ausbildung (ISCED 45)"){
        paste0("Ausbildungsabsolvent*innen (ISCED 45)")
      }else if(inpp =="Absolvent*innen Erstausbildung (ISCED 35)"){
        paste0("Absolvent*innen der Erstausbildung (ISCED 35)")
      }


      highcharter::hw_grid(
        # plot
        highcharter::hcmap(
          #"countries/de/de-all",
          map = map_selection,
          data = data_map,
          value = "wert",
          joinBy = c("hc-a2", "alpha2"),
          borderColor = "#FAFAFA",
          name = paste0(inpp),
          borderWidth = 0.1,
          nullColor = "#A9A9A9",
          tooltip = list(
            valueDecimals = 0,
            valueSuffix = "%"
          )
          ,
          download_map_data = T
        )
        %>%
          highcharter::hc_tooltip(pointFormat = "{point.land} <br> Anteil: {point.display_rel}%") %>%
          highcharter::hc_colorAxis(min=0, minColor= "#f4f5f6", maxColor="#b16fab",labels = list(format = "{text}%")) %>%
          highcharter::hc_title(
            text = paste0("Anteil von Frauen an allen ", title_oecd_1_1, " in ", inpf, " ", inpy, " weltweit (OECD)"),
            margin = 10,
            align = "center",
            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
          ) %>%
          # highcharter::hc_caption(
          #   text = "...",  style = list(color= "white", fontSize = "12px")
          # ) %>%
          highcharter::hc_chart(
            style = list(fontFamily = "SourceSans3-Regular")
          ) %>% highcharter::hc_size(600, 550) %>%
          highcharter::hc_credits(enabled = FALSE) %>%
          highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                                 verticalAlign = "bottom")
      )


    } else {

      inpbe <<- r$map_betr_oecd_arb_gender


      # test <- oecd_azub %>%
      #   dplyr::filter(geschlecht %in% c("Gesamt", "Frauen") &
      #                   indikator == "berufsorientiert" &
      #                   #jahr == inpy &
      #                   fach %in% c("MINT",
      #                               "Informatik & Kommunikationstechnologie",
      #                               "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
      #                               "Naturwissenschaften, Mathematik und Statistik",
      #                               "Alle"))%>%
      #   tidyr::pivot_wider(values_from=wert, names_from = fach)



      data_fva <- oecd_azub %>%
        dplyr::filter(geschlecht %in% c("Gesamt", "Frauen") &
                        indikator == "berufsorientiert" &
                        jahr == inpy &
                        fach %in% c("MINT",
                                    "Informatik & Kommunikationstechnologie",
                                    "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                    "Naturwissenschaften, Mathematik und Statistik",
                                    "Alle"))%>%
        tidyr::pivot_wider(values_from = wert, names_from = geschlecht)%>%
        dplyr::mutate(wert= round(Frauen/Gesamt *100,1))%>%
        dplyr::mutate(display_rel= prettyNum(.$wert, big.mark = ".", decimal.mark = ","),
                      display_total= prettyNum(.$Frauen, big.mark = ".", decimal.mark = ","))%>%
        dplyr::select(-Gesamt, - Frauen)%>%
        dplyr::inner_join(countries_names, by = "land") %>%
        dplyr::mutate(alpha2 = toupper(alpha2))



      data_fvf1 <- oecd_azub %>%
        dplyr::filter(geschlecht == "Frauen" &
                        indikator == "berufsorientiert" &
                        jahr == inpy &
                        fach %in% c("MINT",
                                    "Informatik & Kommunikationstechnologie",
                                    "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                    "Naturwissenschaften, Mathematik und Statistik",
                                    "Alle"))%>%
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


      data_fvf2 <- oecd_azub %>%
        dplyr::filter(geschlecht == "Frauen" &
                        indikator == "berufsorientiert" &
                        jahr == inpy &
                        fach %in% c("MINT",
                                    "Informatik & Kommunikationstechnologie",
                                    "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                    "Naturwissenschaften, Mathematik und Statistik",
                                    "Alle"))%>%
        dplyr::rename(display_total = wert)%>%
        dplyr::mutate(display_total= prettyNum(.$display_total, big.mark = ".", decimal.mark = ","))%>%
        dplyr::select(land, jahr, display_total, fach, anforderung)

      data_fvf3 <- data_fvf1 %>%
        dplyr::left_join(data_fvf2, by=c("land", "jahr", "fach", "anforderung"))%>%
        dplyr::inner_join(countries_names, by = "land") %>%
        dplyr::mutate(alpha2 = toupper(alpha2))


      if (inpbe == "Anteil von Frauen an Allen"){


        data1 <- data_fva#

        title_oecd_2_1 <- if(inpp == "Auszubildende (ISCED 45)"){
          paste0("weiblichen Auszubildenden (ISCED 45) an allen Auszubildenden in ", inpf)
        } else if(inpp == "In Meisterlehre (< 880 Std. Vorbereitung, ISCED 55)"){
          paste0("weiblichen Meisterlehrlingen (< 880 Std. Vorbereitung, ISCED 55) an allen Meisterlehrlingen in ", inpf)
        } else if(inpp == "Auszubildende in Erstausbildung (ISCED 35)"){
          paste0("weiblichen Auszubildenden in Erstausbildung (ISCED 35) an allen Auszubildenden in Erstausbildung in ", inpf)
        }else if (inpp == "In Meister-/Technikerlehre (> 880 Std. Vorbereitung, ISCED 65)"){
          paste0("weiblichen Meister-/Technikerlehrlingen (> 880 Std. Vorbereitung, ISCED 65) an allen Meister-/Technikerlehrlingen in ", inpf)
        }

      }else if(inpbe == "Anteil an Frauen von Frauen"){


        data1 <- data_fvf3

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

      highcharter::hw_grid(
        # plot
        highcharter::hcmap(
          #"countries/de/de-all",
          map = map_selection,
          data = data_map,
          value = "wert",
          joinBy = c("hc-a2", "alpha2"),
          borderColor = "#FAFAFA",
          name = paste0(inpp),
          borderWidth = 0.1,
          nullColor = "#A9A9A9",
          tooltip = list(
            valueDecimals = 0,
            valueSuffix = "%"
          )
          ,
          download_map_data = T
        )
        %>%
          highcharter::hc_tooltip(pointFormat = "{point.land} <br> Anteil: {point.display_rel}% <br> Anzahl: {point.display_total}") %>%
          highcharter::hc_colorAxis(min=0, minColor= "#f4f5f6", maxColor="#b16fab",labels = list(format = "{text}%")) %>%
          highcharter::hc_title(
            text = paste0("Anteil von ", title_oecd_2_1, " ", inpy, " weltweit (OECD)" ),
            margin = 10,
            align = "center",
            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
          ) %>%
          # highcharter::hc_caption(
          #   text = "...",  style = list(color= "white", fontSize = "12px")
          # ) %>%
          highcharter::hc_chart(
            style = list(fontFamily = "SourceSans3-Regular")
          ) %>% highcharter::hc_size(600, 550) %>%
          highcharter::hc_credits(enabled = FALSE) %>%
          highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                                 verticalAlign = "bottom")
      )

    }



  }




}

plot_international_top10_mint_arb <- function(r) {

  oecd_abs_anfänger <- arbeitsmarkt_anfänger_absolv_oecd
  oecd_abs_anfänger <<-oecd_abs_anfänger%>% dplyr::filter(!is.na(.$wert))

  oecd_azub <- arbeitsmarkt_anzahl_azubis_oecd
  oecd_azub <<- oecd_azub%>% dplyr::filter(!is.na(.$wert))

  eu_besch <- arbeitsmarkt_beschäftigte_eu
  eu_besch <<- eu_besch%>% dplyr::filter(!is.na(.$wert))


  map_l <<- r$map_l_top10_mint_arb

  if(map_l== "EU"){

    inpy <<- r$map_y_eu_top10_mint_arb
    inpp <<- r$map_pers_eu_top10_mint_arb

    map_selection <- "custom/europe"



    # test <<- eu_besch %>%
    #   dplyr::filter(geschlecht == "Gesamt"&
    #                   # jahr == inpy &
    #                   # indikator == inpp &
    #                   variable == "Anteil an arbeitender Bevölkerung")



    data1 <<- eu_besch %>%
      dplyr::filter(geschlecht == "Gesamt"&
                      jahr == inpy &
                      indikator == inpp &
                      variable == "Anteil an arbeitender Bevölkerung")%>%
      tidyr::pivot_wider(names_from = variable, values_from = wert)%>%
      dplyr::rename(wert="Anteil an arbeitender Bevölkerung")%>%
      dplyr::mutate(display_rel=prettyNum(.$wert, big.mark = ".", decimal.mark = ","))



    data2 <<- eu_besch %>%
      dplyr::filter(geschlecht == "Gesamt"&
                      jahr == inpy &
                      indikator == inpp&
                      variable == "Anzahl in Tsd.")%>%
      tidyr::pivot_wider(names_from = variable, values_from = wert)%>%
      dplyr::mutate(across(`Anzahl in Tsd.`, ~ as.numeric(.)*1000))%>%
      dplyr::rename(display_total = "Anzahl in Tsd." )%>%
      dplyr::select(display_total, land)%>%
      dplyr::mutate(across(display_total, ~ prettyNum(., big.mark = ".", decimal.mark = ",")))

    data_fn <- data1 %>%
      dplyr::left_join(data2,by=c("land"))%>%
      dplyr::left_join(countries_names %>%
                         dplyr::mutate(land=dplyr::case_when(land == "Tschechien" ~ "Tschechische Republik",
                                                             T ~ .$land)), by= "land")%>%
      dplyr::mutate(alpha2= toupper(alpha2))%>%
      dplyr::filter(!is.na(.$wert) & wert!=0)

    plotopshov <- "Anteil: {point.display_rel}% <br> Anzahl: {point.display_total}"

title_top <- paste0("Länder Europas mit dem höchsten Anteil von ", inpp, "n in MINT an allen ", inpp, "n ", inpy )
title_bot <- paste0("Länder Europas mit dem niedrigsten Anteil von ", inpp, "n in MINT an allen ", inpp, "n ",  inpy )

  }


  else if (map_l== "OECD"){

    map_selection <- "custom/world"

    inpp <<- r$map_pers_oecd_top10_mint_arb



    if(inpp %in%  c("Anfänger*innen Ausbildung (ISCED 45)",
                    "Anfänger*innen Erstausbildung (ISCED 35)",
                    "Absolvent*innen Ausbildung (ISCED 45)",
                    "Absolvent*innen Erstausbildung (ISCED 35)")){

      inpy <<- r$map_y_oecd_top10_mint_arb
      inpf <<- r$map_f_oecd_top10_mint_arb

      plotopshov <- "Anteil: {point.display_rel}%"


      # test <<- oecd_abs_anfänger%>%
      #   dplyr::filter(
      #                   fachbereich %in% c("MINT",
      #                                      "Informatik & Kommunikationstechnologie",
      #                                      "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
      #                                      "Naturwissenschaften, Mathematik und Statistik",
      #                                      "Alle")&
      #                   geschlecht == "Gesamt")



      data1 <<- oecd_abs_anfänger%>%
        dplyr::filter(jahr == inpy &
                        fachbereich %in% c("MINT",
                                           "Informatik & Kommunikationstechnologie",
                                           "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                           "Naturwissenschaften, Mathematik und Statistik",
                                           "Alle")&
                        geschlecht == "Gesamt")%>%
        dplyr::mutate(display_rel=prettyNum(round(.$wert,1), big.mark = ".", decimal.mark = ","))%>%
        dplyr::filter(!is.na(.$wert) & wert!=0)



      if(inpp == "Anfänger*innen Ausbildung (ISCED 45)"){

        data_fn <<- data1 %>%
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





    } else {

      inpy <<- r$map_y_oecd2_top10_mint_arb
      inpf <<- r$map_f_oecd2_top10_mint_arb



      plotopshov <- "Anteil: {point.display_rel}% <br> Anzahl: {point.display_total}"


      # test <- oecd_azub %>%
      #   dplyr::filter(geschlecht == "Gesamt" &
      #                   indikator == "berufsorientiert" &
      #                   #jahr == inpy &
      #                   fach %in% c("MINT",
      #                               "Informatik & Kommunikationstechnologie",
      #                               "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
      #                               "Naturwissenschaften, Mathematik und Statistik",
      #                               "Alle"))


      data1 <- oecd_azub %>%
        dplyr::filter(geschlecht == "Gesamt" &
                        indikator == "berufsorientiert" &
                        jahr == inpy &
                        fach %in% c("MINT",
                                    "Informatik & Kommunikationstechnologie",
                                    "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                    "Naturwissenschaften, Mathematik und Statistik",
                                    "Alle"))%>%
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

      data2 <- oecd_azub %>%
        dplyr::filter(geschlecht == "Gesamt" &
                        indikator == "berufsorientiert" &
                        jahr == inpy &
                        fach %in% c("MINT",
                                    "Informatik & Kommunikationstechnologie",
                                    "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                    "Naturwissenschaften, Mathematik und Statistik"))%>%
        dplyr::rename(display_total = wert)%>%
        dplyr::mutate(display_total= prettyNum(.$display_total, big.mark = ".", decimal.mark = ","))%>%
        dplyr::select(land, jahr, display_total, fach, anforderung)

      data3 <- data1 %>%
        dplyr::left_join(data2, by=c("land", "jahr", "fach", "anforderung"))%>%
        dplyr::inner_join(countries_names, by = "land") %>%
        dplyr::mutate(alpha2 = toupper(alpha2))%>%
        dplyr::filter(!is.na(.$wert) & wert!=0)


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

  avg_line <<- r$show_ave

  # Create top 10 plot
    if (avg_line == "Ja"){

      data_avg <<- round(mean(data_fn$wert, na.rm = T),0)

      plot_top <- highcharter::hchart(
        data_fn %>% dplyr::arrange(desc(wert)) %>% dplyr::slice(1:10),
        'bar',
        highcharter::hcaes(y = wert, x = land))%>%
        highcharter::hc_plotOptions(
          series = list(
            boderWidth = 0,
            dataLabels = list(enabled = TRUE, format = "{point.display_rel}%")
          )) %>%
        highcharter::hc_tooltip(pointFormat = plotopshov )%>%
        highcharter::hc_yAxis(plotLines = list(
          list(
            value = data_avg,
            color = "#FF0000",
            width = 3,
            zIndex = 4
          )
        ),title = list(text = ""),
        labels = list(format = "{value}%"),
        min = 0,
        max = max(data_fn$wert, na.rm = T)*1.2)%>%
        highcharter::hc_xAxis(title = list(text = " ")) %>%
        highcharter::hc_colors(c("#154194")) %>%
        highcharter::hc_title(text = title_top,
                              margin = 10,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
        ) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
        ) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE)
        # highcharter::hc_caption(
        #   text = "capt_dyn",  style = list(color= "grey", fontSize = "12px"))

      plot_bottom <- highcharter::hchart(
        data_fn %>% dplyr::arrange(desc(wert)) %>% dplyr::slice_tail(n = 10),
        'bar',
        highcharter::hcaes(y = wert, x = land))%>%
        highcharter::hc_plotOptions(
          series = list(
            boderWidth = 0,
            dataLabels = list(enabled = TRUE, format = "{point.display_rel}%")
          )) %>%
        highcharter::hc_tooltip(pointFormat = plotopshov)%>%
        highcharter::hc_yAxis(
          plotLines = list(
            list(
              value = data_avg,
              color = "#FF0000",
              width = 3,
              zIndex = 4
            )
          ),title = list(text = ""),
          labels = list(format = "{value}%"),
          min = 0,
          max = max(data_fn$wert, na.rm = T)*1.2)%>%
        highcharter::hc_xAxis(title = list(text = " ")) %>%
        highcharter::hc_colors(c("#154194")) %>%
        highcharter::hc_title(text =  title_bot,
                              margin = 10,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
        ) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
        ) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE)
        # highcharter::hc_caption(
        #   text = "capt_dyn",  style = list(color= "grey", fontSize = "12px"))



      highcharter::hw_grid(
        plot_top,
        plot_bottom,
        ncol = 2)

    } else if (avg_line == "Nein"){



      plot_top <- highcharter::hchart(
        data_fn %>% dplyr::arrange(desc(wert)) %>% dplyr::slice(1:10),
        'bar',
        highcharter::hcaes(y = wert, x = land))%>%
        highcharter::hc_plotOptions(
          series = list(
            boderWidth = 0,
            dataLabels = list(enabled = TRUE, format = "{point.display_rel}%")
          )) %>%
        highcharter::hc_tooltip(pointFormat = plotopshov)%>%
        highcharter::hc_yAxis(title = list(text = ""),
                              labels = list(format = "{value}%"),
                              min = 0,
                              max = max(data_fn$wert, na.rm = T)*1.2)%>%
        highcharter::hc_xAxis(title = list(text = " ")) %>%
        highcharter::hc_colors(c("#154194")) %>%
        highcharter::hc_title(text = title_top,
                              margin = 10,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
        ) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
        ) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE)
        # highcharter::hc_caption(
        #   text = "capt_dyn",  style = list(color= "grey", fontSize = "12px"))



      plot_bottom <- highcharter::hchart(
        data_fn %>% dplyr::arrange(desc(wert)) %>% dplyr::slice_tail(n = 10),
        'bar',
        highcharter::hcaes(y = wert, x = land))%>%
        highcharter::hc_plotOptions(
          series = list(
            boderWidth = 0,
            dataLabels = list(enabled = TRUE, format = "{point.display_rel}%")
          )) %>%
        highcharter::hc_tooltip(pointFormat = plotopshov)%>%
        highcharter::hc_yAxis(title = list(text = ""),
                              labels = list(format = "{value}%"),
                              min = 0,
                              max = max(data_fn$wert, na.rm = T)*1.2)%>%
        highcharter::hc_xAxis(title = list(text = "")) %>%
        highcharter::hc_colors(c("#154194")) %>%
        highcharter::hc_title(text =  title_bot,
                              margin = 10,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
        ) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
        ) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE)
        # highcharter::hc_caption(
        #   text = "capt_dyn",  style = list(color= "grey", fontSize = "12px"))


      highcharter::hw_grid(
        plot_top,
        plot_bottom,
        ncol = 2)


    }


}


plot_international_top10_mint_arb_gender <- function(r) {



  oecd_abs_anfänger <- arbeitsmarkt_anfänger_absolv_oecd
  oecd_abs_anfänger <<-oecd_abs_anfänger%>% dplyr::filter(!is.na(.$wert))


  oecd_azub <- arbeitsmarkt_anzahl_azubis_oecd
  oecd_azub <<- oecd_azub%>% dplyr::filter(!is.na(.$wert))

  eu_besch <- arbeitsmarkt_beschäftigte_eu
  eu_besch1 <<- eu_besch %>% dplyr::filter(!is.na(.$wert))

  inpl <<- r$map_l_top10_mint_arb_gender


  if(inpl== "EU"){



    inpy <<- r$map_y_eu_top10_mint_arb_gender
    inpp <<- r$map_pers_top10_mint_arb_gender



    map_selection <- "custom/europe"

    # test <<- eu_besch %>%
    #   dplyr::filter(geschlecht %in% c("Gesamt", "Frauen")&
    #                   # jahr == inpy &
    #                   # indikator == inpp &
    #                   variable == "Anteil an arbeitender Bevölkerung")



    data1 <<- eu_besch %>%
      dplyr::filter(geschlecht %in% c("Gesamt", "Frauen")&
                      jahr == inpy &
                      indikator == inpp &
                      variable == "Anteil an arbeitender Bevölkerung")%>%
      tidyr::pivot_wider(names_from = geschlecht, values_from = wert)%>%
      dplyr::rename(wert="Frauen")%>%
      dplyr::select(-Gesamt)

    data1$display_rel <- prettyNum(round(data1$wert,1), big.mark = ".", decimal.mark = ",")


    data2 <<- eu_besch %>%
      dplyr::filter(geschlecht == "Frauen"&
                      jahr == inpy &
                      indikator == inpp&
                      variable == "Anzahl in Tsd.")%>%
      tidyr::pivot_wider(names_from = variable, values_from = wert)%>%
      dplyr::mutate(across(`Anzahl in Tsd.`, ~ as.numeric(.)*1000))%>%
      dplyr::rename(display_total = "Anzahl in Tsd." )%>%
      dplyr::select(display_total, land)%>%
      dplyr::mutate(across(display_total, ~ prettyNum(., big.mark = ".", decimal.mark = ",")))

    data1 <- data1 %>%
      dplyr::left_join(data2,by=c("land"))%>%
      dplyr::left_join(countries_names %>%
                         dplyr::mutate(land=dplyr::case_when(land == "Tschechien" ~ "Tschechische Republik",
                                                             T ~ .$land)), by= "land")%>%
      dplyr::mutate(alpha2= toupper(alpha2))%>%
      dplyr::filter(!is.na(.$wert) & wert!=0)

    plotopshov <- "Anteil: {point.display_rel}% <br> Anzahl: {point.display_total}"

    title_top <- paste0("Länder Europas mit dem höchsten Anteil von weiblichen ", inpp, "n an allen ", inpp, "n in MINT ", inpy )
    title_bot <- paste0("Länder Europas mit dem niedrigsten Anteil von weiblichen ", inpp, "n an allen ", inpp, "n in MINT  ",  inpy )



    }


  else if (inpl== "OECD"){



    map_selection <- "custom/world"

    inpp <<- r$map_pers_top10_mint_arb_gender
    inpy <<- r$map_y_top10_mint_arb_gender
    inpf <<- r$map_f_top10_mint_arb_gender



    if(inpp %in%  c("Anfänger*innen Ausbildung (ISCED 45)",
                    "Anfänger*innen Erstausbildung (ISCED 35)",
                    "Absolvent*innen Ausbildung (ISCED 45)",
                    "Absolvent*innen Erstausbildung (ISCED 35)")){

      plotopshov <- "Anteil: {point.display_rel}%"

      # test <<- oecd_abs_anfänger %>%
      #   dplyr::filter(
      #                   fachbereich %in% c("MINT",
      #                                      "Informatik & Kommunikationstechnologie",
      #                                      "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
      #                                      "Naturwissenschaften, Mathematik und Statistik",
      #                                      "Alle"))


      data1 <<- oecd_abs_anfänger %>%
        dplyr::filter(jahr == inpy &
                        fachbereich %in% c("MINT",
                                           "Informatik & Kommunikationstechnologie",
                                           "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                           "Naturwissenschaften, Mathematik und Statistik",
                                           "Alle")&
                        geschlecht =="Frauen")%>%
          tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
        dplyr::mutate(MINT = (rowSums(dplyr::select(., "Informatik & Kommunikationstechnologie",
                                             "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                             "Naturwissenschaften, Mathematik und Statistik"), na.rm = T))/3)%>%
        tidyr::pivot_longer(c("MINT",
                               "Informatik & Kommunikationstechnologie",
                               "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                               "Naturwissenschaften, Mathematik und Statistik",
                               "Alle"), values_to = "wert", names_to = "fachbereich")%>%
        dplyr::filter(!is.na(.$wert) & wert!=0)

      if(inpp == "Anfänger*innen Ausbildung (ISCED 45)"){

        data1 <<- data1 %>%
          dplyr::filter(anforderung == "Ausbildung (ISCED 45)" &
                        variable == "Frauen-/Männeranteil Ausbildungs-/Studiumsanfänger*innen nach Fachbereichen" &
                        fachbereich == inpf
            )%>%
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))


        data1$display_rel <- prettyNum(round(data1$wert,1), big.mark = ".", decimal.mark = ",")


      } else if (inpp == "Anfänger*innen Erstausbildung (ISCED 35)"){

        data1 <<- data1 %>%
          dplyr::filter(anforderung == "Erstausbildung (ISCED 35)" &
                          variable == "Frauen-/Männeranteil Ausbildungs-/Studiumsanfänger*innen nach Fachbereichen"&
                          fachbereich == inpf)%>%
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))

        data1$display_rel <- prettyNum(round(data1$wert,1), big.mark = ".", decimal.mark = ",")

      } else if (inpp == "Absolvent*innen Ausbildung (ISCED 45)"){

        data1 <<- data1 %>%
          dplyr::filter(anforderung == "Ausbildung (ISCED 45)" &
                          variable == "Frauen-/Männeranteil Absolvent*innen nach Fachbereichen"&
                          fachbereich == inpf)%>%
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))

        data1$display_rel <- prettyNum(round(data1$wert,1), big.mark = ".", decimal.mark = ",")

      } else if (inpp == "Absolvent*innen Erstausbildung (ISCED 35)"){

        data1 <<- data1 %>%
          dplyr::filter(anforderung == "Erstausbildung (ISCED 35)" &
                          variable == "Frauen-/Männeranteil Absolvent*innen nach Fachbereichen"&
                          fachbereich == inpf)
          dplyr::inner_join(countries_names, by = "land") %>%
          dplyr::mutate(alpha2 = toupper(alpha2))

        data1$display_rel <- prettyNum(round(data1$wert,1), big.mark = ".", decimal.mark = ",")

      }

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


    } else {

      inpbe <<- r$map_betr_oecd_top10_mint_arb_gender

      plotopshov <- "Anteil: {point.display_rel}% <br> Anzahl: {point.display_total}"



      # test <<- oecd_azub %>%
      #   dplyr::filter(geschlecht %in% c("Gesamt", "Frauen") &
      #                   indikator == "berufsorientiert" &
      #                   #jahr == inpy &
      #                   fach %in% c("MINT",
      #                               "Informatik & Kommunikationstechnologie",
      #                               "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
      #                               "Naturwissenschaften, Mathematik und Statistik",
      #                               "Alle"))

      data_fva <<- oecd_azub %>%
        dplyr::filter(geschlecht %in% c("Gesamt", "Frauen") &
                        indikator == "berufsorientiert" &
                        jahr == inpy &
                        fach %in% c("MINT",
                                    "Informatik & Kommunikationstechnologie",
                                    "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                    "Naturwissenschaften, Mathematik und Statistik",
                                    "Alle"))%>%
        tidyr::pivot_wider(values_from = wert, names_from = geschlecht)%>%
        dplyr::mutate(wert= round(Frauen/Gesamt *100,1))%>%
        dplyr::mutate(display_rel= prettyNum(round(.$wert,1), big.mark = ".", decimal.mark = ","),
                      display_total= prettyNum(.$Frauen, big.mark = ".", decimal.mark = ","))%>%
        dplyr::select(-Gesamt, - Frauen)%>%
        dplyr::inner_join(countries_names, by = "land") %>%
        dplyr::mutate(alpha2 = toupper(alpha2))



      data_fvf1 <<- oecd_azub %>%
        dplyr::filter(geschlecht == "Frauen" &
                        indikator == "berufsorientiert" &
                        jahr == inpy &
                        fach %in% c("MINT",
                                    "Informatik & Kommunikationstechnologie",
                                    "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                    "Naturwissenschaften, Mathematik und Statistik",
                                    "Alle"))%>%
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


      data_fvf2 <<- oecd_azub %>%
        dplyr::filter(geschlecht == "Frauen" &
                        indikator == "berufsorientiert" &
                        jahr == inpy &
                        fach %in% c("MINT",
                                    "Informatik & Kommunikationstechnologie",
                                    "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                    "Naturwissenschaften, Mathematik und Statistik",
                                    "Alle"))%>%
        dplyr::rename(display_total = wert)%>%
        dplyr::mutate(display_total= prettyNum(.$display_total, big.mark = ".", decimal.mark = ","))%>%
        dplyr::select(land, jahr, display_total, fach, anforderung)

      data_fvf3 <<- data_fvf1 %>%
        dplyr::left_join(data_fvf2, by=c("land", "jahr", "fach", "anforderung"))%>%
        dplyr::inner_join(countries_names, by = "land") %>%
        dplyr::mutate(alpha2 = toupper(alpha2))


      if (inpbe == "Anteil von Frauen an Allen"){


        data1 <<- data_fva%>%
          dplyr::filter(!is.na(.$wert) & wert!=0)


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





      }else if(inpbe == "Anteil an Frauen von Frauen"){

        data1 <<- data_fvf3%>%
          dplyr::filter(!is.na(.$wert) & wert!=0)




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



      if (inpp == "Auszubildende (ISCED 45)"){

        data1 <<- data1 %>%
          dplyr::filter(anforderung=="Ausbildung (ISCED 45)"&
                          fach == inpf)


      } else if(inpp == "Auszubildende in Erstausbildung (ISCED 35)"){

        data1 <<- data1 %>%
          dplyr::filter(anforderung=="Erstausbildung (ISCED 35)"&
                          fach == inpf)

      } else if(inpp == "In Meisterlehre (< 880 Std. Vorbereitung, ISCED 55)") {

        data1 <<- data1 %>%
          dplyr::filter(anforderung=="kurzes tertiäres Bildungsprogramm (berufsorientiert)"&
                          fach == inpf)

      } else if(inpp == "In Meister-/Technikerlehre (> 880 Std. Vorbereitung, ISCED 65)"){

        data1 <<- data1 %>%
          dplyr::filter(anforderung== "Bachelor oder vergleichbar (berufsorientiert)"&
                          fach == inpf)

      }
    }

  }

  avg_line <<- r$show_avg_top10_mint_arb_gender

  # Create top 10 plot
  if (avg_line == "Ja"){

    data_avg <<- round(mean(data1$wert, na.rm = T),0)

    plot_top <- highcharter::hchart(
      data1 %>% dplyr::arrange(desc(wert)) %>% dplyr::slice(1:10),
      'bar',
      highcharter::hcaes(y = wert, x = land))%>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, format = "{point.display_rel}%")
        )) %>%
      highcharter::hc_tooltip(pointFormat = plotopshov) %>%
      highcharter::hc_yAxis(plotLines = list(
        list(
          value = data_avg,
          color = "#FF0000",
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
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
      ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE)

    plot_bottom <- highcharter::hchart(
      data1 %>% dplyr::arrange(desc(wert)) %>% dplyr::slice_tail(n = 10),
      'bar',
      highcharter::hcaes(y = wert, x = land))%>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, format = "{point.display_rel}%")
        )) %>%
      highcharter::hc_tooltip(pointFormat = plotopshov) %>%
      highcharter::hc_yAxis(
        plotLines = list(
          list(
            value = data_avg,
            color = "#FF0000",
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
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
      ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE)
      # highcharter::hc_caption(
      #   text = "capt_dyn",  style = list(color= "grey", fontSize = "12px"))



    highcharter::hw_grid(
      plot_top,
      plot_bottom,
      ncol = 2)

  } else if (avg_line == "Nein"){



    plot_top <- highcharter::hchart(
      data1 %>% dplyr::arrange(desc(wert)) %>% dplyr::slice(1:10),
      'bar',
      highcharter::hcaes(y = wert, x = land))%>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, format = "{point.display_rel}%")
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
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
      ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE)
      # highcharter::hc_caption(
      #   text = "capt_dyn",  style = list(color= "grey", fontSize = "12px"))



    plot_bottom <- highcharter::hchart(
      data1 %>% dplyr::arrange(desc(wert)) %>% dplyr::slice_tail(n = 10),
      'bar',
      highcharter::hcaes(y = wert, x = land))%>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, format = "{point.display_rel}%")
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
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
      ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE)
      # highcharter::hc_caption(
      #   text = "capt_dyn",  style = list(color= "grey", fontSize = "12px"))


    highcharter::hw_grid(
      plot_top,
      plot_bottom,
      ncol = 2)


  }

  }






## arbeitsmarkt (Jakob) ----
plot_international_arbeitsmarkt_map <- function(r) {}
plot_international_arbeitsmakrt_top10 <- function(r) {}
plot_international_arbeitsmarkt_vergleiche <- function(r) {

  #r <- list(vergleich_y_int_arbeitsmarkt = 2012,vergleich_l_int_arbeitsmarkt = c("Australien", "Ungarn", "Deutschland"),vergleich_f_int_arbeitsmarkt = "MINT")
  # load UI inputs from reactive value

  timerange <- r$vergleich_y_int_arbeitsmarkt
  land_m <- r$vergleich_l_int_arbeitsmarkt
  fach_m <- r$vergleich_f_int_arbeitsmarkt

  variable_set <- c("Anteil Absolvent*innen nach Fach an allen Fächern",
                    "Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern")

  tmp_df <-  arbeitsmarkt_anfänger_absolv_oecd %>%
    dplyr::filter(geschlecht == "Gesamt" &
                    jahr == timerange &
                    land %in% land_m &
                    #fachbereich == fach_m &
                    anforderung == "tertiäre Bildung (gesamt)" &
                    variable %in% variable_set
    )

  # check if variables are present
  if (!all(variable_set %in% unique(tmp_df$variable))) {
    logger::log_debug("not all needed variable in this combination")
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

  # Create the plot
  plot <- highcharter::hchart(object = tmp_df,
                      type = "column",
                      mapping = highcharter::hcaes(x = land, y = wert, group = variable))  %>%
    #hc_xAxis(categories = tmp_df$land) %>%
    #hc_yAxis(title = list(text = "THIS TITLE")) %>%
    highcharter::hc_plotOptions(series = list(dataLabels = list(enabled = FALSE))) %>%
    highcharter::hc_colors(c("#B16FAB", "#66CBAF")) %>%
    highcharter::hc_title(
      text = paste0(
        "Anteil der Ausbildungs-/Studiums-Anfänger*innen und Absolvent*innen in ",
        fach_m, " in ", timerange)
    ) %>%
    #hc_subtitle(text = "Each bar represents a type of fruit") %>%
    highcharter::hc_legend(enabled = TRUE) %>%
    highcharter::hc_exporting(enabled = FALSE) %>%
    highcharter::hc_tooltip(
      pointFormat = paste0(
        '<span style="color:{point.color}">\u25CF</span>',
        '{series.name}: ','<b>{point.y}</b><br/>'),
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
    )


  return(plot)
}



