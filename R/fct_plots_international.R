#' A function to plot a graph.
#'
#' @description A function to create a map for the first box
#' inside the tab "International".
#'
#' @return The return value is a plot
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd


plot_international_map <- function(r) {

  #r <- list(map_y = "2019", map_l = "OECD", map_f = "Umwelt")
  # load UI inputs from reactive value
  timerange <- r$map_y
  label_m <- r$map_l
  fach_m <- r$map_f


  if (is.null(fach_m)) { fach_m <- ""}
  logger::log_debug("Plotting international map for:")
  logger::log_debug("Time: ", timerange,
                    " | Label: ", label_m,
                    " | Fach: ", fach_m)


  if (label_m == "Weltweit") {
    map_selection <- "custom/world"
    fach_m <- "Alle MINT-Fächer"
    df <- studierende_absolventen_weltweit  %>%
      dplyr::filter(fach == "Alle MINT-Fächer")
  }
if (label_m == "OECD") {
    map_selection <- "custom/world"
    
    # filter for selection
    df_filtered <- studierende_anzahl_oecd %>%
      dplyr::filter(geschlecht == "Gesamt" &
                      jahr == timerange &
                      ebene == 1 &
                      anforderung %in% c("Bachelor oder vergleichbar (akademisch)",
                                         "Master oder vergleichbar (akademisch)"))
    
    # calculate total amount by land
    this_df_alle <- df_filtered %>%
      dplyr::filter(fachbereich == "Alle") %>%
      dplyr::group_by(land, jahr, fach) %>%
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
      dplyr::mutate(wert = round(wert / total * 100, 1))
    

  }
  if (label_m == "EU") {
    map_selection <- "custom/europe"
    df <- studierende_europa %>%
      dplyr::filter(geschlecht == "Gesamt"  &
                      (mint_select == "mint" |
                         (mint_select == "nicht mint" &
                            fach_m == "Alle MINT-Fächer")) &
                      fach == fach_m &
                      indikator == "Fächerwahl")
  }

  # lehramt <- r$nurLehramt_studium_studienzahl_bl_map
  #
  # hochschulform_select_1 <- r$hochschulform_studium_studienzahl_bl_map1
  #
  # hochschulform_select_2 <- r$hochschulform_studium_studienzahl_bl_map2

  # filter dataset based on UI inputs
  dfs <- df %>% dplyr::filter(jahr == timerange)

  #dfss <- dfs %>% dplyr::filter(region != "Deutschland")

  # df <- df %>% dplyr::filter(region != "Bayern")

  # df <- df %>% dplyr::filter(region != "Baden-Württemberg")



  df_insp1<- dfs

  # df_insp1 <- df_insp %>%
  #   dplyr::select(-fachbereich,- mint_select, -typ )%>%
  #   tidyr::pivot_wider(names_from = fach, values_from = wert)%>%
  #   dplyr::mutate(dplyr::across(c(6:ncol(.)), ~round(./`Alle Fächer`*100,1)))%>%
  #   tidyr::pivot_longer(c(6:ncol(.)), values_to = "proportion", names_to ="fach")%>%
  #   dplyr::right_join(df_insp)

  # englische Namen der Länder
  # https://github.com/stefangabos/world_countries/blob/master/data/countries/de/countries.csv
  #countries_names <-  read.csv(file = "data/countries_de.csv")
  #save(countries_names, file = "data/countries_names.rda")

  #Trennpunkte für lange Zahlen ergänzen
  df_insp1$display_wert <- prettyNum(df_insp1$wert, big.mark = ".", decimal.mark = ",")

  df7 <- df_insp1 %>%
    dplyr::select(land, jahr, fach, wert, display_wert) %>%
    dplyr::inner_join(countries_names, by = "land") %>%
    dplyr::mutate(alpha2 = toupper(alpha2))



  #Anteil mit weniger Nachkommerstellen für Hover
  # df7$prop <- df7$proportion
  # df7$prop <- round(df7$prop, 0)

  # Plot

  # Vorbereitung Überschrift
  help_fach <- fach_m
  help_fach <- ifelse(help_fach == "Alle Nicht MINT-Fächer", "allen Fächern außer MINT", help_fach)
  help_fach <- ifelse(help_fach == "Alle MINT-Fächer", "MINT", help_fach)

  label_m <- ifelse(label_m == "Studierende", paste0(label_m, "n"), label_m)
  label_m <- ifelse(label_m == "Internationale Studierende", "internationalen Studierenden", label_m)
  label_m <- ifelse(grepl("Lehram", label_m), "Studierenden (Lehramt)", label_m)
  label_m <- ifelse(grepl("1. Hoch", label_m), "internationalen Studienanfänger:innen (1. Hochschulsemester)", label_m)

  help_l <- "Studierenden"
  help_l <- ifelse(grepl("1. Hoch", label_m), "internationalen Studienanfänger:innen", help_l)
  help_l <- ifelse(grepl("1. Fach", label_m), "Studienanfänger:innen", help_l)

  data_map_1 <- df7 #%>%
  #dplyr::filter(fach == fach_m)%>%
  #dplyr::mutate(display = as.character(proportion))
  #title_map_1 <-

  highcharter::hw_grid(
    # plot
    highcharter::hcmap(
      #"countries/de/de-all",
      map = map_selection,
      data = data_map_1,
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
      highcharter::hc_tooltip(pointFormat = "{point.land} <br> Anteil: {point.display_wert} %") %>%
      highcharter::hc_colorAxis(min=0, minColor= "#f4f5f6", maxColor="#b16fab",labels = list(format = "{text}%")) %>%
      highcharter::hc_title(
        text = paste0("Anteil von ", label_m, " in ", help_fach, " an allen ", help_l, " (", timerange, ")"),
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


plot_international_top10 <- function(r) {
  #r <- list(map_y = "2019", map_l = "OECD", map_f = "Umwelt")
  # load UI inputs from reactive value

  timerange <- r$map_y
  label_m <- r$map_l
  fach_m <- r$map_f
  show_avg <- r$show_avg_top10_mint_line


  if (is.null(fach_m)) { fach_m <- ""}
  logger::log_debug("Plotting international map for:")
  logger::log_debug("Time: ", timerange,
                    " | Label: ", label_m,
                    " | Fach: ", fach_m,
                    " | Avg.: ", show_avg)


  if (label_m == "Weltweit") {
    fach_m <- "Alle MINT-Fächer"
    df <- studierende_absolventen_weltweit  %>%
      dplyr::filter(fach == "Alle MINT-Fächer" &
                      jahr == timerange) %>%
      dplyr::mutate(wert = round(wert, 2)) %>%
      dplyr::select(land, wert)
  }
  if (label_m == "OECD") {

    # filter for selection
    df_filtered <- studierende_anzahl_oecd %>%
      dplyr::filter(geschlecht == "Gesamt" &
                      jahr == timerange &
                      ebene == 1 &
                      anforderung %in% c("Bachelor oder vergleichbar (akademisch)",
                                         "Master oder vergleichbar (akademisch)"))

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
      dplyr::select(land, wert)
  }

  # Grenze für die X-Achse ist immer etwas größer als der maximale wert
  # aber nie größer als 100%
  # Grenze soll immer in 10er Schritten gehen
  max_percent_used <- ceiling(min(c(100, max(df$wert) * 1.2)) / 10) * 10

  # Create female plot
  plot_top <- highcharter::hchart(
    df %>% dplyr::arrange(desc(wert)) %>% dplyr::slice(1:10),
    'bar',
    highcharter::hcaes(y = wert, x = land)) %>%
    highcharter::hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE, format = "{point.wert} %")
      )) %>%
    #highcharter::hc_tooltip(pointFormat = "Land: {point.land} <br> Anteil an allen neuen MINT-Ausbildungsverträgen: {point.y} % <br> Anzahl der neu abgeschlossenen Ausbildugnsverträge: {point.wert}") %>%
    highcharter::hc_yAxis(title = list(text = ""),
                          labels = list(format = "{value} %"),
                          min = 0,
                          max = max_percent_used,
                          tickInterval = 10) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_colors(c("#154194")) %>%
    highcharter::hc_title(text = paste0("Länder mit dem größten Anteil an '", fach_m, "' in ", timerange),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE)


  # Create male plot
  plot_bottom <- highcharter::hchart(
    df %>% dplyr::arrange(desc(wert)) %>% dplyr::slice_tail(n = 10),
    'bar',
    highcharter::hcaes(y = wert, x = land)) %>%
    highcharter::hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE, format = "{point.wert} %")
      )) %>%
    #highcharter::hc_tooltip(pointFormat = "Land: {point.land} <br> Anteil an allen neuen MINT-Ausbildungsverträgen: {point.y} % <br> Anzahl der neu abgeschlossenen Ausbildugnsverträge: {point.wert}") %>%
    highcharter::hc_yAxis(title = list(text = ""),
                          labels = list(format = "{value} %"),
                          min = 0,
                          max = max_percent_used,
                          tickInterval = 10) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_colors(c("#154194")) %>%
    highcharter::hc_title(text = paste0("Länder mit dem niedrigsten Anteil an '", fach_m, "' in ", timerange),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE)

  if (show_avg == "Ja") {
    plot_top <- plot_top %>%
      highcharter::hc_yAxis(
        # title = list(text = "Cdays"),
        plotLines = list(
          list(
            value = mean(df$wert, na.rm = TRUE),
            color = "#FF0000",
            width = 3,
            zIndex = 4#,
            # label = list(
            #   text = "mean",
            #   style = list(color = "#FF0000")
            # )
          )
        )
      )
    plot_bottom <- plot_bottom %>%
      highcharter::hc_yAxis(
        # title = list(text = "Cdays"),
        plotLines = list(
          list(
            value = mean(df$wert, na.rm = TRUE),
            color = "#FF0000",
            width = 3,
            zIndex = 4#,
            # label = list(
            #   text = "mean",
            #   style = list(color = "#FF0000")
            # )
          )
        )
      )

  }

  highcharter::hw_grid(
    plot_top,
    plot_bottom,
    ncol = 2)



}
