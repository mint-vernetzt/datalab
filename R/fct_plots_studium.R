# Wer wählt MINT ----
### Tab 1 ----
#' A function to plot a graph.
#'
#' @description A function to create a pie chart for the first box
#' inside the tab "Schule".
#'
#' @return The return value is a plot
#' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_mint <- function(r){

    betrachtung <- r$ansicht_studium_anteil
    testy1 <- r$studium_anteil_y
    regio <- r$region_studium_anteil
    testl1 <- if (betrachtung == "Einzelansicht - Kuchendiagramm") r$studium_anteil_i else r$studium_anteil_i_balken


    # df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    #   dplyr::filter(jahr == testy1,
    #                 geschlecht == "Gesamt",
    #                 region == regio,
    #                 indikator %in% testl1,
    #                 fach %in% c("Alle Nicht MINT-Fächer", "Alle MINT-Fächer")) %>%
    #   dplyr::select(region, jahr, indikator, fach, wert) %>%
    #   dplyr::collect()


    df_query <- glue::glue_sql("
    SELECT region, jahr, indikator, fach, wert
    FROM studierende_detailliert
    WHERE jahr = {testy1}
    AND geschlecht = 'Gesamt'
    AND region = {regio}
    AND indikator in ({testl1*})
    AND fach IN ('Alle Nicht MINT-Fächer','Alle MINT-Fächer')
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)
#
#     df <- df %>%
#       dplyr::select(region, jahr, indikator, fach, wert)

    # alle <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    #   dplyr::filter(jahr == testy1,
    #                 geschlecht == "Gesamt",
    #                 region == regio,
    #                 indikator %in% testl1,
    #                 fach == "Alle Fächer") %>%
    #   dplyr::select(region, jahr, indikator, fach, wert) %>%
    #   dplyr::rename(wert_ges = wert) %>%
    #   dplyr::collect()


    df_query <- glue::glue_sql("
    SELECT region, jahr, indikator, fach, wert as wert_ges
    FROM studierende_detailliert
    WHERE jahr = {testy1}
    AND geschlecht = 'Gesamt'
    AND region = {regio}
    AND indikator in ({testl1*})
    AND fach = 'Alle Fächer'
                               ", .con = con)

    alle <- DBI::dbGetQuery(con, df_query)

    # alle <- alle %>%
    #   dplyr::select(region, jahr, indikator, fach, wert) %>%
    #   dplyr::rename(wert_ges = wert)



    df <- df %>%
      dplyr::left_join(alle, by = c("region", "jahr", "indikator")) %>%
      dplyr::rename(fach = fach.x) %>%
      dplyr::mutate(proportion = round(wert / wert_ges * 100, 1)) %>%
      dplyr::select(-fach.y)


    df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$display_rel <- prettyNum(df$proportion, big.mark = ".", decimal.mark = ",")
    df <- within(df, fach <- factor(fach, levels = c("Alle Nicht MINT-Fächer", "Alle MINT-Fächer")))


    if(betrachtung == "Einzelansicht - Kuchendiagramm"){

        if(length(testl1) == 1) {

          df_pie <- df %>% dplyr::filter(indikator == testl1)


          titel <- paste0(testl1[1], " in ", regio, " (", testy1, ")")

          tooltip <-paste('Anteil: {point.display_rel}% <br> Anzahl: {point.wert}')
          format <- '{point.display_rel}%'

          quelle <- "Quelle der Daten: Destatis, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."



          highcharter::hw_grid(


            out <- piebuilder(df_pie, titel, x = "fach", y = "proportion", tooltip, color =  c("#efe8e6", "#b16fab"), format = format, quelle=quelle),


            ncol = 1,
            browsable = TRUE)


        } else if(length(testl1) == 2) {

          # Filterung für den ausgewählten Indikator
          df_1_pie <- df %>% dplyr::filter(indikator == testl1[1])
          df_2_pie <- df %>% dplyr::filter(indikator == testl1[2])

          titel = ifelse(regio == "Saarland",
                         paste0(testl1[1], " im ", regio, " (", testy1, ")"),
                         paste0(testl1[1], " in ", regio, " (", testy1, ")"))
          titel2 = ifelse(regio == "Saarland",
                         paste0(testl1[2], " im ", regio, " (", testy1, ")"),
                         paste0(testl1[2], " in ", regio, " (", testy1, ")"))
          tooltip <- paste('Anteil: {point.display_rel}% <br> Anzahl: {point.wert}')

           highcharter::hw_grid(

            out <- piebuilder(df_1_pie, titel,x = "fach", y = "proportion", tooltip, color = c("#efe8e6", "#b16fab"), format = '{point.display_rel}%'),
            out <- piebuilder(df_2_pie, titel2,x = "fach", y = "proportion", tooltip, color = c("#efe8e6", "#b16fab"), format = '{point.display_rel}%'),


            ncol = 2,
            browsable = TRUE
          )

        }


    }

       else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){

         df <- df %>% dplyr::filter(indikator %in% testl1)
         df <- df[with(df, order(proportion, decreasing = TRUE)), ]


         #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
         highcharter::hchart(df, 'bar', highcharter::hcaes(y = proportion, x = indikator, group =forcats::fct_rev(fach)))%>%
           highcharter::hc_tooltip(pointFormat = "Anteil: {point.display_rel} % <br> Anzahl: {point.wert}") %>%
           highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  F) %>%
           highcharter::hc_xAxis(title = list(text = "")) %>%
           highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
           highcharter::hc_colors(c( "#b16fab","#efe8e6")) %>%
           highcharter::hc_title(text = ifelse(regio == "Saarland",
                                               paste0("Anteil von Studierenden in MINT an allen Studierenden im ", regio, " (", testy1, ")"),
                                               paste0("Anteil von Studierenden in MINT an allen Studierenden in ", regio, " (", testy1, ")")),
                                 margin = 45,
                                 align = "center",
                                 style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
           highcharter::hc_chart(
             style = list(fontFamily = "Calibri Regular", fontSize = "14px")
           ) %>%
           highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
           highcharter::hc_exporting(enabled = TRUE,
                                     buttons = list(
                                       contextButton = list(
                                         menuItems = list("downloadPNG", "downloadCSV")
                                       )
                                     )
           )
       }
}






### Tab 2 ----

#' A function to plot time series
#'
#' @description A function to plot the time series
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_verlauf_single <- function(r) {

  # load UI inputs from reactive value
  indi_selct <- r$studienzahl_einstieg_indi_verlauf
  timerange <- r$date_studienzahl_einstieg_verlauf
  t  <- (timerange[1]:timerange[2])
  regio <- r$region_studienzahl_einstieg_verlauf

  abs_zahlen_selector <- r$abs_zahlen_einstieg_verlauf_indi

  ###

  # # filter dataset based on UI inputs
  # df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
  #   dplyr::filter(jahr %in% t,
  #                 geschlecht == "Gesamt",
  #                 region == regio,
  #                 indikator %in% indi_selct,
  #                 fach == "Alle MINT-Fächer") %>%
  #   dplyr::select(jahr, fach, indikator, wert)%>%
  #   dplyr::collect()
  #

  df_query <- glue::glue_sql("
    SELECT jahr, fach, indikator, wert
    FROM studierende_detailliert
    WHERE jahr IN ({t*})
    AND geschlecht = 'Gesamt'
    AND region = {regio}
    AND indikator in ({indi_selct*})
    AND fach = 'Alle MINT-Fächer'
                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)



  if(abs_zahlen_selector == "In Prozent"){
#
#
#     alle <- dplyr::tbl(con, from = "studierende_detailliert") %>%
#       dplyr::filter(jahr %in% t,
#                     geschlecht == "Gesamt",
#                     region == regio,
#                     indikator %in% indi_selct,
#                     fach == "Alle Fächer")%>%
#       dplyr::select(jahr, fach, indikator, wert)%>%
#       dplyr::rename(wert_ges = wert) %>%
#       dplyr::collect()
#

    df_query <- glue::glue_sql("
    SELECT jahr, fach, indikator, wert as wert_ges
    FROM studierende_detailliert
    WHERE jahr IN ({t*})
    AND geschlecht = 'Gesamt'
    AND region = {regio}
    AND indikator in ({indi_selct*})
    AND fach = 'Alle Fächer'
                               ", .con = con)

    alle <- DBI::dbGetQuery(con, df_query)

    # alle <- alle %>%
    #   dplyr::select(jahr, fach, indikator, wert)%>%
    #   dplyr::rename(wert_ges = wert)



    df <- df %>% dplyr::left_join(alle, by = c( "jahr", "indikator")) %>%
      dplyr::rename(fach = fach.x) %>%
      dplyr::mutate(proportion = round(wert/wert_ges*100,1)) %>%
      dplyr::select(-fach.y)

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)


    df <- df[with(df, order( jahr, decreasing = FALSE)), ]

    #Trennpunkte für lange Zahlen ergänzen
    df$display_rel <- prettyNum(df$proportion, big.mark = ".", decimal.mark = ",")



    titel <- ifelse(regio == "Saarland",paste0("Anteil von Studierenden in MINT an allen Studierenden im ", regio), paste0("Anteil von Studierenden in MINT an allen Studierenden in ", regio))
    tooltip <- "Anteil: {point.display_rel}%"
    format <- "{value}%"
    color <- c("#b16fab", "#154194", "#66cbaf", "#fbbf24", "#AFF3E0", "#2D6BE1", "#008F68", "#8893a7", "#ee7775", "#9d7265", "#35bd97",
               "#bfc6d3", "#5f94f9", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
    out <- linebuilder(df, titel, x = "jahr", y = "proportion", group = "indikator", tooltip, format, color)



  }else if (abs_zahlen_selector == "Anzahl"){

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)


    df <- df[with(df, order( jahr, decreasing = FALSE)), ]

    #Trennpunkte für lange Zahlen ergänzen

    df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

    titel <- ifelse(regio == "Saarland",
                    paste0("Anzahl an Studierenden in MINT im ", regio),
                    paste0("Anzahl an Studierenden in MINT in ", regio))
    tooltip <- "Anzahl: {point.display_abs}"
    format <- "{value:, f}"
    color <- c("#b16fab", "#154194", "#66cbaf", "#fbbf24", "#AFF3E0", "#2D6BE1", "#008F68", "#8893a7", "#ee7775", "#9d7265", "#35bd97",
               "#bfc6d3", "#5f94f9", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
    out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "indikator", tooltip, format, color)


  }


  return (out)

}

### Tab 3 ----
#' A function to plot the german map
#'
#' @description A function to plot the german map with all states that contain
#' information about the share of women in STEM
#'
#' @return The return value is the german map with information
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studierende_bula_mint <- function(r) {

  # load UI inputs from reactive value
  betrachtung <- r$ansicht_studium_bulas

  if(betrachtung == "Übersicht - Kartendiagramm"){

    #UI nach Betrachtung
    timerange <- r$bulas_map_y
    label_m <- r$bulas_map_l

    # filter dataset based on UI inputs
    # df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    #   dplyr::filter(jahr == timerange,
    #                 region != "Deutschland",
    #                 fach %in% c("Alle MINT-Fächer", "Alle Fächer"),
    #                 indikator == label_m,
    #                 geschlecht == "Gesamt") %>%
    #   dplyr::collect()


    df_query <- glue::glue_sql("
    SELECT *
    FROM studierende_detailliert
    WHERE jahr = {timerange}
    AND geschlecht = 'Gesamt'
    AND NOT region = 'Deutschland'
    AND indikator  = {label_m}
    AND fach In ('Alle MINT-Fächer','Alle Fächer')
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)




    df <- df %>%
      dplyr::select(-fachbereich,- mint_select, -typ )%>%
      tidyr::pivot_wider(names_from = fach, values_from = wert)%>%
      dplyr::mutate(dplyr::across(c(6:ncol(.)), ~round(./`Alle Fächer`*100,1)))%>%
      tidyr::pivot_longer(c(6:ncol(.)), values_to = "proportion", names_to ="fach")%>%
      dplyr::right_join(df) %>%
      dplyr::filter(fach != "Alle Fächer")


    df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$display_rel <- prettyNum(df$proportion, big.mark = ".", decimal.mark = ",")


    # Plot
    # Vorbereitung Überschrift

    label_m <- ifelse(label_m == "Studierende", paste0(label_m, "n"), label_m)
    label_m <- ifelse(label_m == "internationale Studierende", "internationalen Studierenden", label_m)
    label_m <- ifelse(grepl("Lehram", label_m), "Studierenden (Lehramt)", label_m)
    label_m <- ifelse(label_m == "internationale Studienanfänger:innen (1. Hochschulsemester)",
                      "internationalen Studienanfänger:innen (1. Hochschulsemester)", label_m)

    help_l <- label_m
    help_l <- ifelse(label_m == "internationalen Studienanfänger:innen (1. Hochschulsemester)",
                     "internationalen Studienanfänger:innen", help_l)
    help_l <- ifelse(label_m == "Studienanfänger:innen (1. Hochschulsemester)", "Studienanfänger:innen", help_l)


    # plot


    df <- df[df$fachbereich == "MINT",]
    joinby <- c("name", "region")
    name <- paste0(label_m, " in MINT")
    tooltip <- "{point.region} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.display_abs}"
    titel <- paste0("Anteil von ", label_m, " in MINT-Fächern an allen ", help_l, " (", timerange, ")")
    mincolor <- "#f4f5f6"
    map_selection <- 1
    maxcolor <- "#b16fab"

    out <- mapbuilder(df, joinby,name, tooltip, titel, mincolor, maxcolor,prop=FALSE, wert=FALSE, map=map_selection)


  }

  else if(betrachtung == "Zeitverlauf - Liniendiagramm"){###hier weiter

    # load UI inputs from reactive value
    timerange <- r$bulas_verlauf_y
    t <- (timerange[1]:timerange[2])
    absolut_selector <- r$bulas_verlauf_abs_rel
    bl_label <- r$bulas_verlauf_l
    states <- r$bulas_verlauf_regio



    df_query <- glue::glue_sql("
    SELECT jahr, fach, indikator, region, wert
    FROM studierende_detailliert
    WHERE jahr IN ({t*})
    AND geschlecht = 'Gesamt'
    AND region IN ({states*})
    AND indikator  = {bl_label}
    AND fach = 'Alle MINT-Fächer'
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)





    # Vorbereitung Überschrift

    label <- ifelse(bl_label == "Studierende", paste0(bl_label, "n"), bl_label)
    label <- ifelse(label == "internationale Studierende", "internationalen Studierenden", label)
    label <- ifelse(grepl("Lehram", label), "Studierenden (Lehramt)", label)
    label <- ifelse(label == "internationale Studienanfänger:innen (1. Hochschulsemester)",
                      "internationalen Studienanfänger:innen (1. Hochschulsemester)", label)

    help_l <- label
    help_l <- ifelse(label == "internationalen Studienanfänger:innen (1. Hochschulsemester)",
                     "internationalen Studienanfänger:innen", help_l)
    help_l <- ifelse(label == "Studienanfänger:innen (1. Hochschulsemester)", "Studienanfänger:innen", help_l)

    # Plot

    if (absolut_selector=="In Prozent"){

      # alle <- dplyr::tbl(con, from = "studierende_detailliert") %>%
      #   dplyr::filter(jahr %in% t,
      #                 geschlecht == "Gesamt",
      #                 region %in% states,
      #                 indikator == bl_label,
      #                 fach == "Alle Fächer")%>%
      #   dplyr::select(jahr, fach, indikator, wert, region)%>%
      #   dplyr::rename(wert_ges = wert) %>%
      #   dplyr::collect()


      df_query <- glue::glue_sql("
        SELECT jahr, fach, indikator, wert AS wert_ges, region
        FROM studierende_detailliert
        WHERE jahr IN ({t*})
        AND geschlecht = 'Gesamt'
        AND region IN ({states*})
        AND indikator  = {bl_label}
        AND fach = 'Alle Fächer'
                               ", .con = con)

      alle <- DBI::dbGetQuery(con, df_query)



      df <- df %>% dplyr::left_join(alle, by = c( "jahr", "indikator", "region")) %>%
        dplyr::rename(fach = fach.x) %>%
        dplyr::mutate(prop = round(wert/wert_ges*100,1)) %>%
        dplyr::select(-fach.y)



      df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
      df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")

      titel <- paste0("Anteil von ", label, " in MINT-Fächern an allen ", help_l)
      tooltip <- "Anteil {point.region} <br> Wert: {point.display_rel} %"
      format <- "{value}%"
      color <- c("#b16fab", "#154194", "#66cbaf", "#fbbf24", "#AFF3E0", "#2D6BE1", "#008F68", "#8893a7", "#ee7775", "#9d7265", "#35bd97",
                 "#bfc6d3", "#5f94f9", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
      out <- linebuilder(df, titel, x = "jahr", y = "prop", group = "region", tooltip, format, color)

    } else if(absolut_selector=="Anzahl"){


      hcoptslang <- getOption("highcharter.lang")
      hcoptslang$thousandsSep <- "."
      options(highcharter.lang = hcoptslang)


      df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
      df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")


      titel <- paste0("Anzahl an ", label)
      tooltip <- "Anzahl: {point.display_abs}"
      format <- "{value:, f}"
      color <- c("#b16fab", "#154194", "#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                 "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
      out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color)
    }

  }

  else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){##

    timerange <- r$bulas_balken_date
    r_lab1 <- r$bulas_balken_l


    # df_ges <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    #   dplyr::filter(geschlecht=="Gesamt",
    #                 jahr %in% timerange,
    #                 fach %in% c("Alle MINT-Fächer", "Alle Fächer"),
    #                 indikator == r_lab1) %>%
    #   dplyr::collect()



    df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr IN ({timerange*})
        AND geschlecht = 'Gesamt'
        AND indikator  = {r_lab1}
        AND fach IN ('Alle MINT-Fächer','Alle Fächer')
                               ", .con = con)

    df_ges <- DBI::dbGetQuery(con, df_query)



    # df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    #   dplyr::filter(geschlecht=="Gesamt",
    #                 jahr %in% timerange,
    #                 fach %in% c("Alle MINT-Fächer", "Alle Fächer"),
    #                 indikator == r_lab1) %>%
    #   dplyr::select(-fachbereich,- mint_select, -typ )%>%
    #   dplyr::collect()



    df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr IN ({timerange*})
        AND geschlecht = 'Gesamt'
        AND indikator  = {r_lab1}
        AND fach IN ('Alle MINT-Fächer','Alle Fächer')
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::select(-fachbereich,- mint_select, -typ )%>%
      tidyr::pivot_wider(names_from = fach, values_from = wert)%>%
      dplyr::mutate(dplyr::across(c(6:ncol(.)), ~round(./`Alle Fächer`*100,1)))%>%
      tidyr::pivot_longer(c(6:ncol(.)), values_to = "proportion", names_to ="fach")%>%
      dplyr::right_join(df_ges)%>%
      dplyr::filter(fach == "Alle MINT-Fächer")

    #Trennpunkte für lange Zahlen ergänzen
    df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

    df <- df %>%
      dplyr::select(indikator, region, jahr, fach, proportion, wert)

    # NA aus fach entfernen für BULAs mit weniger Studienfachgruppen
    df <- stats::na.omit(df)


    # Vorbereitung Überschrift
    r_lab1 <- ifelse(r_lab1 == "Studierende", paste0(r_lab1, "n"), r_lab1)
    r_lab1 <- ifelse(r_lab1 == "internationale Studierende", "internationalen Studierenden", r_lab1)
    r_lab1 <- ifelse(grepl("Lehram", r_lab1), "Studierenden (Lehramt)", r_lab1)
    r_lab1 <- ifelse(r_lab1 == "internationale Studienanfänger:innen (1. Hochschulsemester)",
                    "internationalen Studienanfänger:innen (1. Hochschulsemester)", r_lab1)

    help_l <- r_lab1
    help_l <- ifelse(r_lab1 == "internationalen Studienanfänger:innen (1. Hochschulsemester)",
                     "internationalen Studienanfänger:innen", help_l)
    help_l <- ifelse(r_lab1 == "Studienanfänger:innen (1. Hochschulsemester)", "Studienanfänger:innen", help_l)


    df <- df[with(df, order(proportion, decreasing = TRUE)),]



    # Plot
    #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
    out <- highcharter::hchart(df, 'bar', highcharter::hcaes(x= region, y = proportion))%>%
      highcharter::hc_tooltip(pointFormat = "{point.fach} <br> Anteil: {point.proportion} % <br> Anzahl: {point.wert}") %>% #Inhalt für Hover-Box
      highcharter::hc_yAxis(title = list(text=""), labels = list(format = "{value}%")) %>% #x-Achse -->Werte in %
      highcharter::hc_xAxis(title= list(text="")) %>% #Y-Achse - keine Beschriftung

      #Anpassung der Farben
      highcharter::hc_plotOptions(bar = list(
        colorByPoint = TRUE,
        colors = ifelse(df$region == "Deutschland", "#b16fab",
                        ifelse(df$region == "Ostdeutschland (inkl. Berlin)", "#d3a4d7",
                               ifelse(df$region == "Westdeutschland (o. Berlin)", "#d3a4d7", "#A9A9A9"))))) %>%
      highcharter::hc_title(text = paste0( "Anteil von ", r_lab1 ," in MINT-Fächern an allen ", help_l,  " (", timerange, ")"),
                            margin = 25,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%  #Schrift-Formatierung Überschrift
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

### Nicht Box 1 ----

#' A function to plot a waffle chart
#'
#' @description A function to create a waffle chart for the second box inside the
#' tab "Studium".
#'
#' @return The return value is a waffle chart
#' @param df The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_waffle_mint <- function(r) {

  # load UI inputs from reactive value
  timerange <- r$waffle_y

  label_w <- r$waffle_l


  # filter dataset based on UI inputs
  # df <- dplyr::tbl(con, from = "studierende") %>%
  #   dplyr::filter(jahr == timerange,
  #                 region== "Deutschland",
  #                 geschlecht== "Gesamt")%>%
  #   dplyr::select(-region, -geschlecht, - jahr)%>%
  #   dplyr::collect()

  df_query <- glue::glue_sql("
        SELECT *
        FROM studierende
        WHERE jahr = {timerange}
        AND region = 'Deutschland'
        AND geschlecht = 'Gesamt'
                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)

  df <- df %>%
    dplyr::select(-region, -geschlecht, - jahr)



  df <- df %>%
    tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
    dplyr::mutate(pro_mathe = `Mathematik, Naturwissenschaften`/Alle,
                  pro_nicht = `Nicht MINT`/Alle,
                  pro_ing = Ingenieurwissenschaften/Alle)%>%
    dplyr::select(indikator, pro_ing, pro_mathe, pro_nicht)%>%
    tidyr::pivot_longer(names_to = fachbereich, values_to = wert)

  # Überschriften vorbereiten
  überschrift_label <- function(label) {
    label <- ifelse(label == "Studienanfänger:innen (1.Fachsemester)", "Studienanfänger:innen <br> (1. Fachsemester)", label)
    label <- ifelse(label == "Studienanfänger:innen (1.Hochschulsemester)", "Studienanfänger:innen <br> (1. Hochschulsemester)" , label)
    label <- ifelse(label == "Studienanfänger:innen (Fachhochschulen, 1.Fachsemester)", "Studienanfänger:innen <br> (Fachhochschule, 1. Fachsemester)" , label)
    label <- ifelse(label == "Studienanfänger:innen (Fachhochschulen, 1.Hochschulsemester)", "Studienanfänger:innen <br> (Fachhochschule, 1. Hochschulsemester)" , label)
    label <- ifelse(label == "Studienanfänger:innen (Lehramt, Universität, 1.Fachsemester)", "Studienanfänger:innen <br> (Lehramt, Universität, 1. Fachsemester)" , label)
    label <- ifelse(label == "Studienanfänger:innen (Lehramt, Universität, 1.Hochschulsemester)", "Studienanfänger:innen <br> (Lehramt, Universität, 1. Hochschulsemester)" , label)
    label <- ifelse(label == "Studienanfänger:innen (Universität, 1.Fachsemester)", "Studienanfänger:innen <br> (Universität, 1. Fachsemester)" , label)
    label <- ifelse(label == "Studienanfänger:innen (Universität, 1.Hochschulsemester)", "Studienanfänger:innen <br> (Universität, 1. Hochschulsemester)" , label)
    label <- ifelse(label == "Studierende (Fachhochschulen)", "Studierende <br> (Fachhochschulen)" , label)
    label <- ifelse(label == "Studierende (Lehramt, Universität)", "Studierende <br> (Lehramt, Universität)" , label)
    label <- ifelse(label == "Studierende (Universität)", "Studierende <br> (Universität)" , label)
    return(label)
  }

  if(length(label_w)==1){

    waf_1 <- df %>%
      dplyr::filter(indikator==label_w)

    label_w <- überschrift_label(label_w)
    title_1 <- as.character(label_w)
    data_1 <- as.numeric(as.vector(waf_1[1,2:ncol(waf_1)]))
    data_1 <- round(data_1 * 100,1)
    names(data_1) <- colnames(waf_1[2:ncol(waf_1)])

    data_k <- data_1

    waffle_a <- waffle::waffle(data_1, keep = FALSE)+
      ggplot2::labs(
        fill = "",
        title = paste0("<span style='color:black;'>", title_1, "<br>", timerange, "<br>", sep='\n')) + ###Von mir. Gemeint: Tatsächlich alle Smester? kab
      ggplot2::theme(plot.title = ggtext::element_markdown(),
                     plot.subtitle = ggtext::element_markdown(),
                     text = ggplot2::element_text(size = 14),
                     plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                     legend.position = "bottom") +
      ggplot2::scale_fill_manual(
        values =  c("#35bd97",
                    "#fbbf24",
                    '#8893a7'),
        na.value='#8893a7',
        limits = c("pro_ing" ,  "pro_mathe" ,"pro_nicht"),
        guide = ggplot2::guide_legend(reverse = TRUE),
        labels = c(
          paste0("Ingenieurwissenschaften",", ",data_1[1], "%"),
          paste0("Mathematik/Naturwissenschaften",", ",data_1[2], "%"),
          paste0("andere Studiengänge",", ",data_1[3], "%"))) +
      ggplot2::guides(fill=ggplot2::guide_legend(nrow=3,byrow=TRUE))

    ggpubr::ggarrange(NULL, waffle_a ,NULL,
                      widths = c(1, 1, 1), nrow=1)

  } else if(length(label_w)==2){

    waf_1 <- df %>%
      dplyr::filter(indikator==label_w[1])

    waf_2 <- df %>%
      dplyr::filter(indikator==label_w[2])

    waf_1[1,1] <- überschrift_label(waf_1[1,1])
    title_1 <- as.character(as.vector(waf_1[1,1]))
    data_1 <- as.numeric(as.vector(waf_1[1,2:ncol(waf_1)]))
    data_1 <- round(data_1 * 100,1)

    names(data_1) <- colnames(waf_1[2:ncol(waf_1)])

    waf_2[1,1] <- überschrift_label(waf_2[1,1])
    title_2 <- as.character(as.vector(waf_2[1,1]))
    data_2 <- as.numeric(as.vector(waf_2[1,2:ncol(waf_2)]))
    data_2 <- round(data_2 * 100,1)

    names(data_2) <- colnames(waf_2[2:ncol(waf_2)])



    waffle_a <- waffle::waffle(data_1, keep = FALSE)+
      ggplot2::labs(
        fill = "",
        title = paste0("<span style='color:black;'>", title_1, "<br>", timerange, "<br>", sep='\n')) +
      ggplot2::theme(plot.title = ggtext::element_markdown(),
                     plot.subtitle = ggtext::element_markdown(),
                     text = ggplot2::element_text(size = 14),
                     plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                     legend.position = "bottom") +
      ggplot2::scale_fill_manual(
        values =  c("#35bd97",
                    "#fbbf24",
                    '#8893a7'),
        na.value='#8893a7',
        limits = c("pro_ing" ,  "pro_mathe" ,"pro_nicht"),
        guide = ggplot2::guide_legend(reverse = TRUE),
        labels = c(
          paste0("Ingenieurwissenschaften",", ",data_1[1], "%"),
          paste0("Mathematik/Naturwissenschaften",", ",data_1[2], "%"),
          paste0("andere Studiengänge",", ",data_1[3], "%"))) +
      ggplot2::guides(fill=ggplot2::guide_legend(nrow=3,byrow=TRUE))

    waffle_b <- waffle::waffle(data_2, keep = FALSE)+
      ggplot2::labs(
        fill = "",
        title = paste0("<span style='color:black;'>", title_2, "<br>", timerange, "<br>", sep='\n')) +
      ggplot2::theme(plot.title = ggtext::element_markdown(),
                     plot.subtitle = ggtext::element_markdown(),
                     text = ggplot2::element_text(size = 14),
                     plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                     legend.position = "bottom") +
      ggplot2::scale_fill_manual(
        values =  c("#35bd97",
                    "#fbbf24",
                    "#8893a7"),
        na.value="#8893a7",
        limits = c("pro_ing" ,  "pro_mathe" ,"pro_nicht"),
        guide = ggplot2::guide_legend(reverse = TRUE),
        labels = c(
          paste0("Ingenieurwissenschaften",", ",data_2[1], "%"),
          paste0("Mathematik/Naturwissenschaften",", ",data_2[2], "%"),
          paste0("andere Studiengänge",", ",data_2[3], "%"))) +
      ggplot2::guides(fill=ggplot2::guide_legend(nrow=3,byrow=TRUE))



    ggpubr::ggarrange(waffle_a, NULL ,waffle_b,
                      widths = c(1, 0.1, 1), nrow=1)

  } else if (length(label_w)==3){

    waf_1 <- df %>%
      dplyr::filter(indikator==label_w[1])

    waf_2 <- df %>%
      dplyr::filter(indikator==label_w[2])

    waf_3 <- df %>%
      dplyr::filter(indikator==label_w[3])

    waf_1[1,1] <- überschrift_label(waf_1[1,1])
    title_1 <- as.character(as.vector(waf_1[1,1]))
    data_1 <- as.numeric(as.vector(waf_1[1,2:ncol(waf_1)]))
    data_1 <- round(data_1 * 100,1)
    names(data_1) <- colnames(waf_1[2:ncol(waf_1)])

    waf_2[1,1] <- überschrift_label(waf_2[1,1])
    title_2 <- as.character(as.vector(waf_2[1,1]))
    data_2 <- as.numeric(as.vector(waf_2[1,2:ncol(waf_2)]))
    data_2 <- round(data_2 * 100,1)
    names(data_2) <- colnames(waf_2[2:ncol(waf_2)])

    waf_3[1,1] <- überschrift_label(waf_3[1,1])
    title_3 <- as.character(as.vector(waf_3[1,1]))
    data_3 <- as.numeric(as.vector(waf_3[1,2:ncol(waf_3)]))
    data_3 <- round(data_3 * 100,1)
    names(data_3) <- colnames(waf_3[2:ncol(waf_3)])



    waffle_a <- waffle::waffle(data_1, keep = FALSE)+
      ggplot2::labs(
        fill = "",
        title = paste0("<span style='color:black;'>", title_1, "<br>", timerange, "<br>", sep='\n')) + ###Von mir. Gemeint: Tatsächlich alle Smester? kab
      ggplot2::theme(plot.title = ggtext::element_markdown(),
                     plot.subtitle = ggtext::element_markdown(),
                     text = ggplot2::element_text(size = 14),
                     plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                     legend.position = "bottom") +
      ggplot2::scale_fill_manual(
        values =  c("#35bd97",
                    "#fbbf24",
                    "#8893a7"),
        na.value="#8893a7",
        limits = c("pro_ing" ,  "pro_mathe" ,"pro_nicht"),
        guide = ggplot2::guide_legend(reverse = TRUE),
        labels = c(
          paste0("Ingenieurwissenschaften",", ",data_1[1], "%"),
          paste0("Mathematik/Naturwissenschaften",", ",data_1[2], "%"),
          paste0("andere Studiengänge",", ",data_1[3], "%"))) +
      ggplot2::guides(fill=ggplot2::guide_legend(nrow=3,byrow=TRUE))

    waffle_b <- waffle::waffle(data_2, keep = FALSE)+
      ggplot2::labs(
        fill = "",
        title = paste0("<span style='color:black;'>", title_2, "<br>", timerange, "<br>", sep='\n')) +
      ggplot2::theme(plot.title = ggtext::element_markdown(),
                     plot.subtitle = ggtext::element_markdown(),
                     text = ggplot2::element_text(size = 14),
                     plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                     legend.position = "bottom") +
      ggplot2::scale_fill_manual(
        values =  c("#35bd97",
                    "#fbbf24",
                    "#8893a7"),
        na.value="#8893a7",
        limits = c("pro_ing" ,  "pro_mathe" ,"pro_nicht"),
        guide = ggplot2::guide_legend(reverse = TRUE),
        labels = c(
          paste0("Ingenieurwissenschaften",", ",data_2[1], "%"),
          paste0("Mathematik/Naturwissenschaften",", ",data_2[2], "%"),
          paste0("andere Studiengänge",", ",data_2[3], "%"))) +
      ggplot2::guides(fill=ggplot2::guide_legend(nrow=3,byrow=TRUE))

    waffle_c <- waffle::waffle(data_3, keep = FALSE)+
      ggplot2::labs(
        fill = "",
        title = paste0("<span style='color:black;'>", title_3, "<br>", timerange, "<br>")) +
      ggplot2::theme(plot.title = ggtext::element_markdown(),
                     plot.subtitle = ggtext::element_markdown(),
                     text = ggplot2::element_text(size = 14),
                     plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                     legend.position = "bottom") +
      ggplot2::scale_fill_manual(
        values =  c("#35bd97",
                    "#fbbf24",
                    "#8893a7"),
        na.value="#8893a7",
        limits = c("pro_ing" ,  "pro_mathe" ,"pro_nicht"),
        guide = ggplot2::guide_legend(reverse = TRUE),
        labels = c(
          paste0("Ingenieurwissenschaften",", ",data_3[1], "%"),
          paste0("Mathematik/Naturwissenschaften",", ",data_3[2], "%"),
          paste0("andere Studiengänge",", ",data_3[3], "%"))) +
      ggplot2::guides(fill=ggplot2::guide_legend(nrow=3,byrow=TRUE))



    ggpubr::ggarrange(waffle_a ,waffle_b, waffle_c,
                      widths = c(1, 1, 1), nrow=1)

  }


}








#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_verlauf_bl_subject <- function(r) {


  absolut_selector <- r$abs_zahlen_verlauf_subject_bl

  # load UI inputs from reactive value
  timerange <- r$date_verlauf_subject_bl
  t <- as.character(timerange[1]:timerange[2])

  states <- r$states_verlauf_subject_bl

  label_select <- r$verl_l

  # df <- dplyr::tbl(con, from = "studierende") %>%
  #   dplyr::filter(jahr %in% t,
  #                 geschlecht=="Gesamt",
  #                 region == states,
  #                 indikator==label_select) %>%
  #   dplyr::collect()
  #

  df_query <- glue::glue_sql("
        SELECT *
        FROM studierende
        WHERE jahr in ({t*})
        AND region = {states}
        AND indikator = {label_select}
        AND geschlecht = 'Gesamt'
                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)



  df <- df %>%
    tidyr::pivot_wider(names_from=fachbereich, values_from = wert)%>%
    #dplyr::rename("MINT (gesamt)" = MINT)%>%
    dplyr::mutate("MINT (Gesamt)_p"= `MINT (Gesamt)`/Alle)%>%
    dplyr::mutate(Ingenieurwissenschaften_p=Ingenieurwissenschaften/Alle)%>%
    dplyr::mutate("Mathematik, Naturwissenschaften_p"=`Mathematik, Naturwissenschaften`/Alle)%>%
    dplyr::select(-Alle,- `Nicht MINT`,- geschlecht)%>%
    tidyr::pivot_longer(c(4:9), names_to ="var", values_to = "wert")%>%
    dplyr::mutate(selector=dplyr::case_when(stringr::str_ends(.$var, "p")~"In Prozent",
                                            T~ "Anzahl"))

  df$var <- gsub("_p", "", df$var)


  if(absolut_selector=="In Prozent"){

    df <- df %>%
      dplyr::filter(selector=="In Prozent")

    df$wert <- df$wert *100
    df$wert <- round(df$wert, 1)


    df$display_rel <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")


    # Überschrift vorbereiten
    label_select <- ifelse(label_select == "Studierende", paste0(label_select, "n"), label_select)
    label_select <- ifelse(label_select == "Studierende (Fachhochschulen)", "Studierenden (Fachhochschulen)" , label_select)
    label_select <- ifelse(label_select == "Studierende (Lehramt, Universität)", "Studierenden (Lehramt, Universität)" , label_select)
    label_select <- ifelse(label_select == "Studierende (Universität)", "Studierenden (Universität)" , label_select)

    titel_help <- "Studierenden"
    titel_help <- ifelse(grepl("Studienanfänger:innen",label_select), "Studienanfänger:innen", titel_help)


    df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
    # plot


    titel <- paste0("Anteil von ", label_select, " in MINT an allen ", titel_help, " in ",states )
    tooltip <- "Anteil {point.display_rel}%"
    format <- "{value}%"
    color <- c("#b16fab", "#154194","#66cbaf")
    out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "var", tooltip, format, color)

  } else if (absolut_selector == "Anzahl") {


    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    df <- df %>%
      dplyr::filter(selector=="Anzahl")

    df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")


    df <- df %>% dplyr::filter(indikator==label_select)

    # Überschrift vorbereiten
    label_select <- ifelse(label_select == "Studierende", paste0(label_select, "n"), label_select)
    label_select <- ifelse(label_select == "Studierende (Fachhochschulen)", "Studierenden (Fachhochschulen)" , label_select)
    label_select <- ifelse(label_select == "Studierende (Lehramt, Universität)", "Studierenden (Lehramt, Universität)" , label_select)
    label_select <- ifelse(label_select == "Studierende (Universität)", "Studierenden (Universität)" , label_select)

    df <- df %>% dplyr::filter(region == states)

    df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
    # plot

    titel <- paste0("Anzahl an ", label_select, " in MINT in ",states )
    tooltip <- "Anzahl: {point.display_abs}"
    format <- "Anzahl: {point.display_abs}"
    color <- c("#b16fab", "#154194","#66cbaf")
    out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "var", tooltip, format, color)



  }

return(out)
}



#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studierende_verlauf_multiple_bl <- function(r) {


  # load UI inputs from reactive value
  timerange <- r$date_studium_studienzahl_bl_verlauf
  t <- as.character(timerange[1]:timerange[2])

  absolut_selector <- r$abs_zahlen_studium_studienzahl_bl_verlauf

  subjects_select <- r$subject_studium_studienzahl_bl_verlauf

  bl_label <- r$verl_bl_l

  states <- r$states_studium_studienzahl_bl_verlauf

  # filter dataset based on UI inputs
  # df <- dplyr::tbl(con, from = "studierende") %>%
  #   dplyr::filter(jahr %in% t,
  #                 geschlecht == "Gesamt",
  #                 indikator==bl_label) %>%
  #   dplyr::collect()


  df_query <- glue::glue_sql("
        SELECT *
        FROM studierende
        WHERE jahr in ({t*})
        AND indikator = {bl_label}
        AND geschlecht = 'Gesamt'
                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)


  df <- df %>%
    dplyr::select(-geschlecht)%>%
    dplyr::filter(!is.na(wert))%>%

    tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
    dplyr::mutate(Ingenieurwissenschaften_p=Ingenieurwissenschaften/Alle,
                  "Mathematik, Naturwissenschaften_p"= `Mathematik, Naturwissenschaften`/Alle,
                  "MINT (Gesamt)_p"=`MINT (Gesamt)`/Alle)%>%
    dplyr::select(-Alle)%>%
    dplyr::mutate(
      dplyr::across(
        c(Ingenieurwissenschaften_p, `Mathematik, Naturwissenschaften_p`, `MINT (Gesamt)_p`),  ~round(.*100,1)))%>%
    dplyr::select(-`Nicht MINT`)%>%
    tidyr::pivot_longer(c(4:9), values_to ="wert", names_to="fach")%>%
    dplyr::mutate(selector = dplyr::case_when(
      stringr::str_ends(.$fach, "_p") ~ "In Prozent",
      T~"Anzahl"
    ))

  df$fach <- gsub("_p", "", df$fach)

  df <- df %>% dplyr::filter(
    fach %in% subjects_select)


  df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

  df <- df %>%dplyr::filter(region%in%states)

  # Vorbereitung Überschrift

  bl_label <- ifelse(bl_label == "Studierende", paste0(bl_label, "n"), bl_label)
  bl_label <- ifelse(bl_label == "Studierende (Fachhochschulen)", "Studierenden (Fachhochschulen)" , bl_label)
  bl_label <- ifelse(bl_label == "Studierende (Lehramt, Universität)", "Studierenden (Lehramt, Universität)" , bl_label)
  bl_label <- ifelse(bl_label == "Studierende (Universität)", "Studierenden (Universität)" , bl_label)

  help <- "Studierenden"
  help <- ifelse(grepl("Studienanfänger:innen",bl_label), "Studienanfänger:innen", help)

  fach_label <- subjects_select
  fach_label<- ifelse(fach_label == "MINT (Gesamt)", "MINT", fach_label)

  # Plot

  if (absolut_selector=="In Prozent"){

    df <- df %>%
      dplyr::filter(selector=="In Prozent")

   df$display_rel <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

    titel <- paste0("Anteil von ", bl_label, " in ", fach_label, " an allen ", help)
    tooltip <-  "Anteil {point.region} <br> Wert: {point.display_rel} %"
    format <- "{value}%"
    color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
               "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
    out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color)

  } else if(absolut_selector=="Anzahl"){

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    df <- df %>%
      dplyr::filter(selector == "Anzahl")

    df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")


    titel <- paste0("Anzahl an ", bl_label, " in ", help)
    tooltip <-  "Anzahl: {point.display_abs}"
    format <- "{value:, f}"
    color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
               "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
    out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color)




  }


  return(out)
}



#' A function to plot time series
#'
#' @description A function to plot a bar chart
#'
#' @return The return value, if any, from executing the function.
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_einstieg_comparison <- function(r) {

  # load UI inputs from reactive value
  timerange <- r$date_kurse_einstieg_comparison

  # filter dataset based on UI inputs
  # df <- dplyr::tbl(con, from = "studierende") %>%
  #   dplyr::filter(jahr == timerange,
  #                 geschlecht == "Gesamt",
  #                 region== "Deutschland")%>%
  #   dplyr::collect()
  #

  df_query <- glue::glue_sql("
        SELECT *
        FROM studierende
        WHERE jahr = {timerange}
        AND indikator = {bl_label}
        AND region='Deutschland'
                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)







  df <- df %>%
    tidyr::pivot_wider(names_from=fachbereich, values_from = wert)%>%
    dplyr::select( -region, -Ingenieurwissenschaften,- `Mathematik, Naturwissenschaften`)

  # Calculating props

  df_props <- df %>%
    dplyr::mutate(dplyr::across(c("MINT (Gesamt)", "Nicht MINT"), ~round(./Alle * 100,1)))%>%
    dplyr::select(-Alle)%>%
    tidyr::pivot_longer(c("MINT (Gesamt)", "Nicht MINT"), values_to="prop", names_to = "proportion")

  # joining props and wert
  df <- df%>%
    dplyr::select(-Alle )%>%
    tidyr::pivot_longer(c("MINT (Gesamt)", "Nicht MINT"), values_to="wert", names_to = "proportion")%>%
    dplyr::left_join(df_props)


  #Trennpunkte für lange Zahlen ergänzen

  df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
  df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")



  df$indikator <-factor(df$indikator,levels= c("Studierende",
                                               "Studierende (Fachhochschulen)",
                                               "Studierende (Lehramt, Universität)",
                                               "Studierende (Universität)",
                                               "Studienanfänger:innen (1.Fachsemester)",
                                               "Studienanfänger:innen (1.Hochschulsemester)",
                                               "Studienanfänger:innen (Fachhochschulen, 1.Fachsemester)",
                                               "Studienanfänger:innen (Fachhochschulen, 1.Hochschulsemester)",
                                               "Studienanfänger:innen (Lehramt, Universität, 1.Fachsemester)",
                                               "Studienanfänger:innen (Lehramt, Universität, 1.Hochschulsemester)",
                                               "Studienanfänger:innen (Universität, 1.Fachsemester)",
                                               "Studienanfänger:innen (Universität, 1.Hochschulsemester)"
  )
  )


  # plot

  df <- within(df, proportion <- factor(proportion, levels=c("Nicht MINT", "MINT (Gesamt)")))

  #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
  highcharter::hchart(df, 'bar', highcharter::hcaes(y = prop, x = indikator, group = forcats::fct_rev(proportion)))%>%
    highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.proportion} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.display_abs}") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  F) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
    highcharter::hc_colors(c( "#b16fab","#efe8e6")) %>%
    highcharter::hc_title(text = paste0("Anteil von Studierenden in MINT an allen Studierenden", "(", timerange, ")"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "Calibri Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG", "downloadCSV")
                                )
                              )
    )


}


#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studierende_verlauf_single_bl_gender <- function(r) {

  # load UI inputs from reactive value
  timerange <- r$choice_V_y
  t <- as.character(timerange[1]:timerange[2])

  v_lab <- r$choice_l_v

  absolut_selector <- r$abs_zahlen_l_v

  subjects_select <- r$choice_v_f

  states <- r$choice_states

  # df <- dplyr::tbl(con, from = "studierende") %>%
  #   dplyr::filter(jahr %in% t,
  #                 geschlecht=="Frauen",
  #                 region == states,
  #                 indikator %in%v_lab) %>%
  #   dplyr::collect()


  df_query <- glue::glue_sql("
        SELECT *
        FROM studierende
        WHERE jahr in ({t*})
        AND geschlecht = 'Frauen'
        AND indikator in ({v_lab*})
        AND region = {states}
                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)




  df <- df %>%
    tidyr::pivot_wider(names_from=fachbereich, values_from = wert)%>%

    dplyr::mutate("Mathematik, Naturwissenschaften_p" =round(`Mathematik, Naturwissenschaften`/Alle*100,1),
                  "MINT (Gesamt)_p"= round(`MINT (Gesamt)`/Alle*100,1),
                  "Ingenieurwissenschaften_p"= round(Ingenieurwissenschaften/Alle*100,1))%>%
    dplyr::select(-Alle, -`Nicht MINT`)%>%
    tidyr::pivot_longer(c(5:10),names_to="fach",values_to="wert")%>%
    dplyr::mutate(selector= dplyr::case_when(stringr::str_ends(.$fach, "_p")~"In Prozent",
                                             T~"Anzahl"))

  df$fach <- gsub("_p", "", df$fach)



  df <- df %>%
    dplyr::filter(fach==subjects_select)

  fach_label <- subjects_select
  fach_label <- ifelse(fach_label == "MINT (Gesamt)", "MINT", fach_label)

  if (absolut_selector=="In Prozent"){

    df <- df %>%
      dplyr::filter(selector=="In Prozent")

    df <- df[with(df, order( jahr, decreasing = FALSE)), ]


    df <- df %>%
      dplyr::mutate(jahr= as.numeric(.$jahr))

    df <- df[with(df, order( jahr, decreasing = F)), ]

    df <- df %>%
      dplyr::mutate(jahr= as.character(.$jahr))

    #Trennpunkte für lange Zahlen ergänzen


    df$display_rel <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")




    titel <- paste0("Anteil von weiblichen Studierenden, die ein Studium in ", fach_label, " gewählt haben, an allen weiblichen Studierenden in ", states)
    tooltip <-  "Anteil {point.label} <br> Wert: {point.display_rel} %"
    format <- "{value}%"
    color <- c("#b16fab", "#154194", "#66cbaf", "#fbbf24",
               "#AFF3E0","#2D6BE1","#008F68","#8893a7", "#ee7775", "#9d7265", "#35bd97",
               "#bfc6d3", "#5f94f9",  "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
    out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "indikator", tooltip, format, color)


  }else if(absolut_selector=="Anzahl"){


    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)


    df <- df %>%
      dplyr::filter(selector=="Anzahl")



    df <- df[with(df, order( jahr, decreasing = FALSE)), ]




    df <- df %>%
      dplyr::mutate(jahr= as.numeric(.$jahr))

    df <- df[with(df, order( jahr, decreasing = F)), ]

    df <- df %>%
      dplyr::mutate(jahr= as.character(.$jahr))

    df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")


    titel <- paste0("Anzahl an weiblichen Studierenden, die ein Studium in ", fach_label, " gewählt haben, in ", states)
    tooltip <-  "Anzahl: {point.display_abs}"
    format <- "{value:, f}"
    color <- c("#b16fab", "#154194", "#66cbaf", "#fbbf24","#AFF3E0","#2D6BE1","#008F68","#8893a7", "#ee7775", "#9d7265", "#35bd97",
               "#bfc6d3", "#5f94f9",  "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
    out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "indikator", tooltip, format, color)





  }

  return(out)
}

# M-I-N-T ----
### Tab 1 ----
#' A function to create barplots, showing ranked study subjects
#'
#' @description A function to compare different subjects
#'
#' @return The return value is a barplot
#' @param data The dataframe "studierende_faecher.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

plot_mint_faecher <- function(r){

  # load UI inputs from reactive value
    betrachtung <- r$ansicht_mint_fach
    timerange <- r$jahr_mint_fach
    regio <- r$region_mint_fach
    if(betrachtung == "Einzelansicht - Kuchendiagramm"){
      label_w <- r$gruppe_mint_fach_pies
    }else{
      label_w <- r$gruppe_mint_fach_balken
    }
    ebene <- r$ebene_mint_fach

    color_fachbereich <- c(
      "Ingenieurwissenschaften (inkl. Informatik)" = "#00a87a",
      "Mathematik, Naturwissenschaften" = "#fcc433",
      "Alle Nicht MINT-Fächer" = "#efe8e6"
    )

    color_fach_pie <- c(
      "Informatik" = "#2D6BE1",
      "Elektrotechnik und Informationstechnik" = "#00a87a",
      "Maschinenbau/Verfahrenstechnik" = "#DDFFF6",
      "Biologie" = "#fbbf24",
      "Mathematik" = "#ee7775",
      "Wirtschaftsingenieurwesen mit ingenieurwissenschaftlichem Schwerpunkt" =
        "#35BD97",
      "Bauingenieurwesen" = "#66CBAF",
      "Ingenieurwesen allgemein" = "#007655",
      "Chemie" = "#D97706",
      "Physik, Astronomie" = "#F59E0B",
      "Architektur, Innenarchitektur" = "#AFF3E0",
      "Verkehrstechnik, Nautik" = "#005C43",
      "Geographie" = "#fde68a",
      "Pharmazie" = "#FCD34D",
      "Raumplanung" = "#008F68",
      "Geowissenschaften (ohne Geographie)" = "#fcc433",
      "Materialwissenschaft und Werkstofftechnik" = "#004331",
      "Vermessungswesen" = "#EFFFF7",
      "Bergbau, Hüttenwesen" = "#EDF3FF",
      "allgemeine naturwissenschaftliche und mathematische Fächer" = "#FEF3C7",

      "Alle Nicht MINT-Fächer" = "#efe8e6"
    )

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
  if(ebene == "MINT-Fachbereiche"){
    # df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    #   dplyr::filter(jahr == timerange,
    #                 region== regio,
    #                 geschlecht== "Gesamt",
    #                 indikator %in% label_w,
    #                 (mint_select == "MINT" & typ == "Aggregat") |
    #                   fachbereich == "Nicht MINT")%>%
    #   dplyr::select(-region, -geschlecht, - jahr, -bereich, -mint_select, -typ)%>%
    #   dplyr::collect()

    if (length(label_w) == 0) {
      stop("Fehler: label_w ist leer und verursacht eine ungültige SQL-Abfrage.")
    }

    df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr == {timerange}
        AND geschlecht = 'Gesamt'
        AND indikator IN ({label_w*})
        AND region = {regio}
        AND ((mint_select = 'MINT' AND typ = 'Aggregat') OR fachbereich = 'Nicht MINT')
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::select(-region, -geschlecht, - jahr, -bereich, -mint_select, -typ)



    # alle <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    #   dplyr::filter(jahr == timerange,
    #                 region== regio,
    #                 geschlecht== "Gesamt",
    #                 indikator %in% label_w,
    #                 fachbereich == "Gesamt")%>%
    #   dplyr::select(-region, -geschlecht, - jahr, -fach, -bereich, -mint_select, -typ)%>%
    #   dplyr::collect()



    df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr == {timerange}
        AND geschlecht = 'Gesamt'
        AND indikator IN ({label_w*})
        AND region = {regio}
        AND fachbereich = 'Gesamt'
                               ", .con = con)

    alle <- DBI::dbGetQuery(con, df_query)

    alle <- alle %>%
      dplyr::select(-region, -geschlecht, - jahr, -bereich, -mint_select, -typ, -fach)


  }
    else{
    # df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    #   dplyr::filter(jahr == timerange,
    #                 region== regio,
    #                 geschlecht== "Gesamt",
    #                 indikator %in% label_w,
    #                 (mint_select == "MINT" & typ != "Aggregat"))%>%
    #   dplyr::select(-region, -geschlecht, - jahr, -bereich, -mint_select, -typ)%>%
    #   dplyr::collect()

      if (length(label_w) == 0) {
        stop("Fehler: label_w ist leer und verursacht eine ungültige SQL-Abfrage.")
      }

    df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr = {timerange}
        AND geschlecht = 'Gesamt'
        AND indikator IN ({label_w*})
        AND region = {regio}
        AND mint_select = 'MINT'
        AND NOT typ = 'Aggregat'
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::select(-region, -geschlecht, - jahr, -bereich, -mint_select, -typ)



    # alle <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    #   dplyr::filter(jahr == timerange,
    #                 region== regio,
    #                 geschlecht== "Gesamt",
    #                 indikator %in% label_w,
    #                 fachbereich == "MINT")%>%
    #   dplyr::select(-region, -geschlecht, - jahr, -fach, -bereich, -mint_select, -typ)%>%
    #   dplyr::collect()
    #

    df_query2 <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr = {timerange}
        AND geschlecht = 'Gesamt'
        AND indikator IN ({label_w*})
        AND region = {regio}
        AND fachbereich = 'MINT'
                               ", .con = con)

    alle <- DBI::dbGetQuery(con, df_query2)

    alle <- alle %>%
      dplyr::select(-region, -geschlecht, - jahr, -bereich, -mint_select, -typ, -fach) #wieso -fah


  }

    #Anteil Berechnen
  df <- df %>%
    dplyr::left_join(alle, dplyr::join_by("indikator")) %>%
    dplyr::select(-fachbereich.y) %>%
    dplyr::rename(wert = wert.x,
                  wert_ges = wert.y,
                  fachbereich = fachbereich.x) %>%
    dplyr::mutate(prop = round(wert/wert_ges * 100, 1))



  #df vorbeiten für Plot-Darstellung
  df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")



  df <- df[with(df, order(prop, decreasing = FALSE)), ]


  if(ebene == "MINT-Fächergruppen" & betrachtung == "Einzelansicht - Kuchendiagramm"){
    df <- df %>% dplyr::filter(prop > 2)
  }

  if(ebene == "MINT-Fachbereiche"){
    df <- df %>%
      dplyr::mutate(color = color_fachbereich[fach])
  }else{
    df <- df %>%
      dplyr::mutate(color = color_fach_pie[fach])
  }


  if(betrachtung == "Einzelansicht - Kuchendiagramm"){

    # Überschriften vorbereiten
    ueberschrift_fct <- function(label){
      titel_help <- ifelse(label == "Studierende", paste0(label, "n"), label)
      titel_help <- ifelse(titel_help == "internationale Studierende", "internationalen Studierenden", titel_help)
      titel_help <- ifelse(grepl("Lehr", titel_help), "Studierenden (Lehramt)", titel_help)
      titel_help <- ifelse(titel_help == "internationale Studienanfänger:innen (1. Hochschulsemester)",
                           "internationalenen Studienanfänger:innen (1. Hochschulsemester)", titel_help)
      return(titel_help)
    }

    if(length(label_w)==1){
    titel_help <- ueberschrift_fct(label_w)

    titel = ifelse(regio == "Saarland",
                   paste0("MINT-Fächeranteile von ", titel_help , " im ", regio, " (", timerange, ")"),
                   paste0("MINT-Fächeranteile von ", titel_help , " in ", regio, " (", timerange, ")"))
    tooltip <- paste('Anteil: {point.prop}% <br> Anzahl: {point.wert}')
    color = as.character(df$color)

    out <- piebuilder(df, titel, x = "fach", y ="prop", tooltip, color, format='{point.prop}%')


    } else if(length(label_w)==2){
      titel_help1 <- ueberschrift_fct(label_w[1])
      titel_help2 <- ueberschrift_fct(label_w[2])

      titel <- text = paste0("MINT-Fächeranteile von ", titel_help1 , " in ", regio, " (", timerange, ")")
      tooltip <- paste('Anteil: {point.prop}% <br> Anzahl: {point.wert}')
      color = as.character(df[df$indikator == label_w[1],]$color)

      p1 <- piebuilder(df[df$indikator == label_w[1],], titel, x = "fach", y ="prop", tooltip, color, format='{point.prop}%')

      #noch nutzen?

      out <- p1

}
  }
  else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){

    df <- df[with(df, order(prop, decreasing = TRUE)), ]

    if(ebene == "MINT-Fachbereiche"){
      df <- df %>%
        dplyr::mutate(color = color_fachbereich[fach])
      col <- df$color
      titel <- ifelse(regio == "Saarland",
                      paste0( "Anteil der MINT-Fachbereiche an allen Fächern im ",regio," (", timerange, ")", br(), "Studierendengruppe: ",label_w),
                      paste0( "Anteil der MINT-Fachbereiche an allen Fächern in ",regio," (", timerange, ")", br(), "Studierendengruppe: ",label_w))
    }else{
      df <- df %>%
        dplyr::mutate(color = color_fach_balken[fach])
      col <- df$color
      titel <- ifelse(regio == "Saarland",
                      paste0( "Anteil der MINT-Fächergruppen an allen MINT-Fächern im ",regio," (", timerange, ")", br(), "Studierendengruppe: ",label_w),
                      paste0( "Anteil der MINT-Fächergruppen an allen MINT-Fächern in ",regio," (", timerange, ")", br(), "Studierendengruppe: ",label_w))

    }

    #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
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
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
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

### Tab 2 ----

#' A function to create a bar plot
#'
#' @description A function to return a ranking of MINT
#'
#' @return The return value is a bar plot
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

mint_anteile <- function(r) {

  zeit <- as.numeric(r$anteile_jahr)
  t <- zeit[1]:zeit[2]
  states <- r$anteile_states
  indi <- r$anteile_indi
  ordering <- r$anteile_order
  betrachtung <- r$anteile_betrachtung

  color_fachbereich <- c(
    "Informatik" = "#2D6BE1",
    "Ingenieurwissenschaften (ohne Informatik)" = "#00a87a",
    "Mathematik, Naturwissenschaften" = "#fcc433"
  )

  color_fach <- c(
    "Informatik" = "#2D6BE1",
    "Elektrotechnik und Informationstechnik" = "#00a87a",
    "Maschinenbau/Verfahrenstechnik" = "#DDFFF6",
    "Biologie" = "#fbbf24",
    "Mathematik" = "#ee7775",
    "Wirtschaftsingenieurwesen mit ingenieurwissenschaftlichem Schwerpunkt" =
      "#35BD97",
    "Bauingenieurwesen" = "#66CBAF",
    "Ingenieurwesen allgemein" = "#007655",
    "Chemie" = "#D97706",
    "Physik, Astronomie" = "#F59E0B",
    "Architektur, Innenarchitektur" = "#AFF3E0",
    "Verkehrstechnik, Nautik" = "#005C43",
    "Geographie" = "#fde68a",
    "Pharmazie" = "#FCD34D",
    "Raumplanung" = "#008F68",
    "Geowissenschaften (ohne Geographie)" = "#fcc433",
    "Materialwissenschaft und Werkstofftechnik" = "#004331",
    "Vermessungswesen" = "#EFFFF7",
    "Bergbau, Hüttenwesen" = "#EDF3FF",
    "allgemeine naturwissenschaftliche und mathematische Fächer" = "#FEF3C7",

    "Alle Nicht MINT-Fächer" = "#efe8e6"
  )

  gruppe <- indi
  gruppe <- dplyr::case_when(
    gruppe == "Studierende" ~ "Studierenden",
    gruppe == "internationale Studierende" ~ "internationalen Studierenden",
    gruppe == "Studierende (Lehramt)" ~ "Lehramtstudierenden",
    gruppe == "internationale Studienanfänger:innen (1. Hochschulsemester)" ~
      "internationalen Studienanfänger:innen (1. Hochschulsemester)",
    T ~ gruppe
  )




  faecher_select <- r$anteile_faecher_mint

  # Filter-Optionen für MINT-Fachbereiche
  fachbereiche_filter <- c("Ingenieurwissenschaften (ohne Informatik)", "Mathematik, Naturwissenschaften", "Informatik")

  # Grunddatensatz aus der Datenbank laden
  # df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
  #   dplyr::filter(geschlecht == "Gesamt",
  #                 jahr %in% t,
  #                 indikator == indi,
  #                 region == states) %>%
  #   dplyr::collect()



  df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr IN ({t*})
        AND geschlecht = 'Gesamt'
        AND indikator == {indi}
        AND region = {states}
                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)



  # Gesamtwerte (Alle MINT-Fächer) vorbereiten
  df_ges <- df %>%
    dplyr::filter(fach == "Alle MINT-Fächer") %>%
    tidyr::pivot_wider(names_from = fach, values_from = wert) %>%
    dplyr::select(indikator, region, jahr, `Alle MINT-Fächer`)

  # Daten anreichern und berechnen
  df <- df %>%
    dplyr::left_join(df_ges, by = c("indikator", "region", "jahr")) %>%
    dplyr::filter(if (ordering == "MINT-Fächergruppen") {
      fach %in% faecher_select & fach != "Alle MINT-Fächer"
    } else if (ordering == "MINT-Fachbereiche") {
      fach %in% fachbereiche_filter
    }) %>%
    dplyr::mutate(prop = round(wert / `Alle MINT-Fächer` * 100, 1))

  # Farbzuordnung basierend auf der Auswahl
  colors <- if (ordering == "MINT-Fächergruppen") {
    as.character(color_fach)
  } else if (ordering == "MINT-Fachbereiche") {
    as.character(color_fachbereich)
  }

  # Zeitvektor erstellen
  year_vec <- df %>%
    dplyr::select(jahr) %>%
    unique() %>%
    dplyr::pull()




  #plotting
  if(betrachtung == "In Prozent"){

    sorted_indicators <- df %>%
      dplyr::group_by(fach) %>%
      dplyr::summarize(m_value = mean(round(prop, 1), na.rm = TRUE)) %>%
      dplyr::arrange(desc(m_value)) %>%
      dplyr::pull(fach)

    df$fach <- factor(df$fach, levels = sorted_indicators)
    if(ordering == "MINT-Fachbereiche"){
      colors <- color_fachbereich[sorted_indicators]
    }else{
      colors <- color_fach[sorted_indicators]
    }

    df <- df[with(df, order(jahr)),]

    df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")


    titel <- ifelse(ordering == "MINT-Fachbereiche",
                    ifelse(states == "Saarland",
                           paste0("Zeitverlauf der MINT-Fachbereiche von ", gruppe, " im ", states),
                           paste0("Zeitverlauf der MINT-Fachbereiche von ", gruppe, " in ", states)),
                    ifelse(states == "Saarland",
                           paste0("Zeitverlauf der MINT-Fächer von ", gruppe, " im ", states),
                           paste0("Zeitverlauf der MINT-Fächer von ", gruppe, " in ", states)))

    tooltip <- "{point.fach} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.display_abs}"
    format <- "{value} %"
    color <- as.character(colors)
    out <- linebuilder(df, titel, x = "jahr", y = "prop", group = "fach", tooltip, format, color)




  } else if (betrachtung == "Anzahl"){

    sorted_indicators <- df %>%
      dplyr::group_by(fach) %>%
      dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
      dplyr::arrange(desc(m_value)) %>%
      dplyr::pull(fach)

    df$fach <- factor(df$fach, levels = sorted_indicators)
    if(ordering == "MINT-Fachbereiche"){
      colors <- color_fachbereich[sorted_indicators]
    }else{
      colors <- color_fach[sorted_indicators]
    }

    df <- df[with(df, order(jahr)),]

    df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")


    titel <- ifelse(ordering == "MINT-Fachbereiche",
                    ifelse(states== "Saarland",
                           paste0("Zeitverlauf der MINT-Fachbereiche von ", gruppe, " im ", states),
                           paste0("Zeitverlauf der MINT-Fachbereiche von ", gruppe, " in ", states)),
                    ifelse(states == "Saarland",
                           paste0("Zeitverlauf der MINT-Fächer von ", gruppe, " im ", states),
                           paste0("Zeitverlauf der MINT-Fächer von ", gruppe, " in ", states)))
    tooltip <- "{point.fach} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.display_abs}"
    format <- "{value}"
    color <- as.character(colors)
    out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "fach", tooltip, format, color)

  }


  return(out)

}


### Tab 3 ----
#funktion für Box 2 Tab 3 Zeitlicher Lauf wo der Fehler auftritt mit doppelter Überschrift
plot_studierende_bula_faecher <- function(r){

  #load UI inputs from reactive value
  betrachtung <- r$ansicht_studium_bulas_faecher

  if(betrachtung == "Übersicht - Kartendiagramm"){

    #UI nach Betrachtung
    timerange <- r$bulas_map_y_faecher
    label_m <- r$bulas_map_l_faecher
    if(label_m == "Studierende (Lehramt)"){
      faecher <- r$bl_f_lehr_faecher
    }else{
      faecher <- r$bl_f_alle_faecher
    }

    # filter dataset based on UI inputs
    # df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    #   dplyr::filter(jahr == timerange,
    #                 !(region %in% c("Deutschland", "Ostdeutschland (inkl. Berlin)",
    #                                 "Westdeutschland (o. Berlin)")),
    #                 fach == faecher,
    #                 indikator == label_m,
    #                 geschlecht == "Gesamt") %>%
    #   dplyr::select(-c(bereich, jahr, geschlecht, fachbereich, mint_select, typ)) %>%
    #   dplyr::collect()
    #



    df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr == {timerange}
        AND geschlecht = 'Gesamt'
        AND indikator == {label_m}
        AND NOT region IN ('Deutschland','Ostdeutschland (inkl. Berlin)','Westdeutschland (o. Berlin)')
        AND fach = {faecher}
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::select(-c(bereich, jahr, geschlecht, fachbereich, mint_select, typ))





    # alle <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    #   dplyr::filter(jahr == timerange,
    #                 !(region %in% c("Deutschland", "Ostdeutschland (inkl. Berlin)",
    #                                 "Westdeutschland (o. Berlin)")),
    #                 fach == "Alle Fächer",
    #                 indikator == label_m,
    #                 geschlecht == "Gesamt") %>%
    #   dplyr::select(-c(bereich, jahr, geschlecht, fachbereich, mint_select, typ)) %>%
    #   dplyr::collect()


    df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr = {timerange}
        AND geschlecht = 'Gesamt'
        AND indikator = {label_m}
        AND NOT region IN ('Deutschland','Ostdeutschland (inkl. Berlin)','Westdeutschland (o. Berlin)')
        AND fach = 'Alle Fächer'
                               ", .con = con)

    alle <- DBI::dbGetQuery(con, df_query)

    alle <- alle %>%
      dplyr::select(-c(bereich, jahr, geschlecht, fachbereich, mint_select, typ))



    df <- df %>%
      dplyr::left_join(alle, dplyr::join_by("region")) %>%
      dplyr::select(-fach.y, -indikator.y) %>%
      dplyr::rename(wert = wert.x,
                    wert_ges = wert.y,
                    fach = fach.x,
                    indikator = indikator.x) %>%
      dplyr::mutate(prop = round(wert/wert_ges * 100, 1))



    df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")



    # Vorbereitung Überschrift

    label_m <- ifelse(label_m == "Studierende", paste0(label_m, "n"), label_m)
    label_m <- ifelse(label_m == "internationale Studierende", "internationalen Studierenden", label_m)
    label_m <- ifelse(grepl("Lehramt", label_m), "Studierenden (Lehramt)", label_m)
    label_m <- ifelse(label_m == "internationale Studienanfänger:innen (1. Hochschulsemester)",
                      "internationalen Studienanfänger:innen (1. Hochschulsemester)", label_m) #studienanfänger:innen wird net benötigt, da es grammatikalisch schon passt

    help_l <- label_m
    help_l <- ifelse(label_m == "internationalen Studienanfänger:innen (1. Hochschulsemester)",
                     "internationalen Studienanfänger:innen", help_l)
    help_l <- ifelse(label_m == "Studienanfänger:innen (1. Hochschulsemester)", "Studienanfänger:innen", help_l)



    if(label_m == "Studierende (Lehramt)"){
      faecher <- r$bl_f_lehr_faecher
    }else{
      faecher <- r$bl_f_alle_faecher
    }

  #  titel_help <- label_m
    titel_help <- faecher
    titel_help <- ifelse(titel_help == "Alle MINT-Fächer", "MINT-Fächern", titel_help)
    titel_help <- ifelse(titel_help == "Alle Nicht MINT-Fächer", "allen Nicht-MINT-Fächern", titel_help)
    titel_help <- ifelse(titel_help == "allgemeine naturwissenschaftliche und mathematische Fächer",
                         "allgemeinen naturwissenschaftlichen und mathematischen Fächern", titel_help)
    titel_help <- ifelse(titel_help == "Bauingenieurwesen", "dem Bauingenieurwesen", titel_help)

    if(nrow(df) == 0){
      titel <- "Für diese Kombination aus Fächergruppe und Bundesland bzw. Bundesländer liegen keine Daten vor.
        Bitte wählen Sie eine andere Komination oder Fächergruppe aus."
      }else{
        titel <- paste0("Anteil von ", label_m, " in ", titel_help, " an allen ", help_l, " (", timerange, ")")
      }

    # plot
       #                          verticalAlign = "bottom")

    joinby <- c("name", "region")
    name <- paste0(label_m, " in MINT")
    tooltip <- "{point.region} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.display_abs}"
    titel <- titel
    mincolor <- "#f4f5f6"
    map_selection <- 1
    maxcolor <- "#b16fab"

    out <- mapbuilder(df, joinby,name, tooltip, titel, mincolor, maxcolor,prop=TRUE,
                      wert=FALSE, map=map_selection)



  }
  else if(betrachtung == "Zeitverlauf - Liniendiagramm"){

    # load UI inputs from reactive value
    absolut_selector <-  r$bulas_verlauf_abs_rel_faecher
    timerange <- r$bulas_verlauf_y_faecher
    t <- timerange[1]:timerange[2]
    states <- r$bulas_verlauf_regio_faecher
    label_select <- r$bulas_verlauf_l_faecher

    if("Studierende (Lehramt)" %in% label_select){
      fach_select <- r$bl_verlauf_lehr_faecher
    }else{
      fach_select <-  r$bl_verlauf_alle_faecher
    }



    df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr IN ({t*})
        AND geschlecht = 'Gesamt'
        AND indikator == {label_select}
        AND region IN ({states*})
        AND fach ={fach_select}
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::select(-c(bereich, geschlecht, fachbereich, mint_select, typ))





    if (absolut_selector == "In Prozent") {

#


      df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr IN ({t*})
        AND geschlecht = 'Gesamt'
        AND indikator == {label_select}
        AND region IN ({states*})
        AND fach = 'Alle Fächer'
                               ", .con = con)

      alle <- DBI::dbGetQuery(con, df_query)

      alle <- alle %>%
        dplyr::select(-c(bereich, geschlecht, fachbereich, mint_select, typ))


      df <- df %>%
        dplyr::left_join(alle, dplyr::join_by(region, jahr)) %>%
        dplyr::select(-fach.y, -indikator.y) %>%
        dplyr::rename(wert = wert.x, wert_ges = wert.y, fach = fach.x, indikator = indikator.x) %>%
        dplyr::mutate(prop = round(wert / wert_ges * 100, 1))  # Berechne Prozentsatz


      df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]



      label_m <- ifelse(label_select == "Studierende", paste0(label_select, "n"), label_select)
      label_m <- ifelse(label_m == "internationale Studierende", "internationalen Studierenden", label_m)
      label_m <- ifelse(grepl("Lehram", label_m), "Studierenden (Lehramt)", label_m)
      label_m <- ifelse(label_m == "internationale Studienanfänger:innen (1. Hochschulsemester)",
                        "internationalen Studienanfänger:innen (1. Hochschulsemester)", label_m)

      help_l <- label_m
      help_l <- ifelse(label_m == "internationalen Studienanfänger:innen (1. Hochschulsemester)",
                       "internationalen Studienanfänger:innen", help_l)
      help_l <- ifelse(label_m == "Studienanfänger:innen (1. Hochschulsemester)", "Studienanfänger:innen", help_l)

      titel_help <- fach_select
      titel_help <- ifelse(titel_help == "Alle MINT-Fächer", "MINT-Fächern", titel_help)
      titel_help <- ifelse(titel_help == "Alle Nicht MINT-Fächer", "allen Nicht-MINT-Fächern", titel_help)
      titel_help <- ifelse(titel_help == "allgemeine naturwissenschaftliche und mathematische Fächer",
                           "allgemeinen naturwissenschaftlichen und mathematischen Fächern", titel_help)

      if (nrow(df) == 0) {
        titel <- "Für diese Kombination aus Fächergruppe und Bundesland bzw. Bundesländer liegen keine Daten vor.
    Bitte wählen Sie eine andere Kombination oder Fächergruppe aus."
      } else {
        titel <- paste0("Anteil von ", label_m, " in ", titel_help, " an allen ", help_l)
      }


      titel <-  titel
      tooltip <- "Anteil: {point.prop}%"
      format <- "{value}%"
      color <- c("#b16fab", "#154194", "#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#d0a9cd",
                 "#bfc6d3", "#5f94f9", "#B45309")
      out <- linebuilder(df, titel, x = "jahr", y = "prop", group = "region", tooltip, format, color)


    }




    else if (absolut_selector == "Anzahl") {


      hcoptslang <- getOption("highcharter.lang")
      hcoptslang$thousandsSep <- "."
      options(highcharter.lang = hcoptslang)

      df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

      # Überschrift vorbereiten
      label_m <- ifelse(label_select == "Studierende", paste0(label_select, "n"), label_select)
      label_m <- ifelse(label_m == "internationale Studierende", "internationalen Studierenden", label_m)
      label_m <- ifelse(grepl("Lehram", label_m), "Studierenden (Lehramt)", label_m)
      label_m <- ifelse(label_m == "internationale Studienanfänger:innen (1. Hochschulsemester)",
                        "internationalen Studienanfänger:innen (1. Hochschulsemester)", label_m)

      titel_help <- fach_select
      titel_help <- ifelse(titel_help == "Alle MINT-Fächer", "MINT-Fächern", titel_help)
      titel_help <- ifelse(titel_help == "Alle Nicht MINT-Fächer", "allen Nicht-MINT-Fächern", titel_help)
      titel_help <- ifelse(titel_help == "allgemeine naturwissenschaftliche und mathematische Fächer",
                           "allgemeinen naturwissenschaftlichen und mathematischen Fächern", titel_help)

      if(nrow(df) == 0){
        titel <- "Für diese Kombination aus Fächergruppe und Bundesland bzw. Bundesländer liegen keine Daten vor.
        Bitte wählen Sie eine andere Komination oder Fächergruppe aus."
      }else{
        titel <- paste0("Anzahl an ", label_m, " in ", titel_help)
      }

      df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
      # plot

      titel <-  titel
      tooltip <- "Anzahl: {point.display_abs}"
      format <- "{value:, f}"
      color <- c("#b16fab", "#154194", "#66cbaf")
      out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color)


    }
  }
  else if(betrachtung == "Gruppenvergleich - Balkendiagramm") {

    #UI Input laden
    timerange <- r$bulas_balken_date_faecher
    r_lab1 <- r$bulas_balken_l_faecher
    regio <- r$bulas_balken_regio_faecher
    if(r_lab1 == "Studierende (Lehramt)"){
      fach_bl <- r$bl_balken_lehr_faecher
    }else{
      fach_bl <- r$bl_balken_alle_faecher
    }

    # df_ges <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    #   dplyr::filter(geschlecht=="Gesamt",
    #                 jahr %in% timerange,
    #                 region %in% regio,
    #                 indikator == r_lab1) %>%
    #   dplyr::collect()



    df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr IN ({timerange*})
        AND geschlecht = 'Gesamt'
        AND indikator == {r_lab1}
        AND region IN ({regio*})
                               ", .con = con)

    df_ges <- DBI::dbGetQuery(con, df_query)



    # df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    #   dplyr::filter(geschlecht=="Gesamt",
    #                 jahr %in% timerange,
    #                 region %in% regio,
    #                 indikator == r_lab1) %>%
    #   dplyr::select(-fachbereich,- mint_select, -typ )%>%
    #   dplyr::collect() %>%
    #   tidyr::pivot_wider(names_from = fach, values_from = wert)%>%
    #   dplyr::mutate(dplyr::across(c(6:ncol(.)), ~round(./`Alle Fächer`*100,1)))%>%
    #   tidyr::pivot_longer(c(6:ncol(.)), values_to = "proportion", names_to ="fach")%>%
    #   dplyr::right_join(df_ges)%>%
    #   dplyr::collect()
    #



    df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr IN ({timerange*})
        AND geschlecht = 'Gesamt'
        AND indikator == {r_lab1}
        AND region IN ({regio*})
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::select(-fachbereich,- mint_select, -typ )%>%
      dplyr::collect() %>%
      tidyr::pivot_wider(names_from = fach, values_from = wert)%>%
      dplyr::mutate(dplyr::across(c(6:ncol(.)), ~round(./`Alle Fächer`*100,1)))%>%
      tidyr::pivot_longer(c(6:ncol(.)), values_to = "proportion", names_to ="fach")%>%
      dplyr::right_join(df_ges)









    #Trennpunkte für lange Zahlen ergänzen
    df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

    df <- df %>%
      dplyr::select(indikator, region, jahr, fach, proportion, wert)


    df <- df %>%
      dplyr::filter(fach==fach_bl)

    # NA aus fach entfernen für BULAs mit weniger Studienfachgruppen
    df <- stats::na.omit(df)

    # Vorbereitung Überschrift
    help_s <- fach_bl
    help_s <- ifelse(help_s == "Alle Nicht MINT-Fächer", "allen Fächern außer MINT", help_s)
    help_s <- ifelse(help_s == "Alle MINT-Fächer", "MINT-Fächern", help_s)
    help_s <- ifelse(help_s == "allgemeine naturwissenschaftliche und mathematische Fächer",
                         "allgemeinen naturwissenschaftlichen und mathematischen Fächern", help_s)


    r_lab1 <- ifelse(r_lab1 == "Studierende", paste0(r_lab1, "n"), r_lab1)
    r_lab1 <- ifelse(r_lab1 == "internationale Studierende", "internationalen Studierenden", r_lab1)
    r_lab1 <- ifelse(grepl("Lehr", r_lab1), "Studierenden (Lehramt)", r_lab1)
    r_lab1 <- ifelse(r_lab1 == "internationale Studienanfänger:innen (1. Hochschulsemester)",
                      "internationalenen Studienanfänger:innen (1. Hochschulsemester)", r_lab1)
    help <- r_lab1
    help <- ifelse(help == "internationalenen Studienanfänger:innen (1. Hochschulsemester)", "internationalen Studienanfänger:innen", help)
    help <- ifelse(help == "Studienanfänger:innen (1. Fachsemester)" |
                     help == "Studienanfänger:innen (1. Hochschulsemester)", "Studienanfänger:innen", help)



    df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$display_rel <- prettyNum(df$proportion, big.mark = ".", decimal.mark = ",")

    df <- df[with(df, order(proportion, decreasing = TRUE)),]

    if(nrow(df) == 0){
      titel <- "Für diese Kombination aus Fächergruppe und Bundesland bzw. Bundesländer liegen keine Daten vor.
        Bitte wählen Sie eine andere Komination oder Fächergruppe aus."


      out <- linebuilder(df, titel = titel, x = "jahr", y = "wert", group = "region", tooltip = "Anzahl: {point.display_abs}", format = "{value:, f}")



    }else{
      #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
    out <- highcharter::hchart(df, 'bar', highcharter::hcaes(x= region, y = proportion))%>%
      highcharter::hc_tooltip(pointFormat = "{point.fach} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.display_abs}") %>% #Inhalt für Hover-Box
      highcharter::hc_yAxis(title = list(text=""), labels = list(format = "{value}%")) %>% #x-Achse -->Werte in %
      highcharter::hc_xAxis(title= list(text="")) %>% #Y-Achse - keine Beschriftung
      highcharter::hc_plotOptions(bar = list(
        colorByPoint = TRUE,
        colors = ifelse(df$region == "Deutschland", "#b16fab",
                        ifelse(df$region == "Ostdeutschland (inkl. Berlin)", "#d3a4d7",
                               ifelse(df$region == "Westdeutschland (o. Berlin)", "#d3a4d7", "#A9A9A9"))))) %>%

      highcharter::hc_title(text = paste0( "Anteil von ", r_lab1 ," in ", help_s," an allen ", help,  " (", timerange, ")"),
                            margin = 25,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
      highcharter::hc_exporting(enabled = TRUE,
                                buttons = list(
                                  contextButton = list(
                                    menuItems = list("downloadPNG", "downloadCSV")
                                  )
                                )
      )

    }

  }

    return(out)

}

### Nicht Box 2 ----

#' A function to create a bar plot
#'
#' @description A function to return a ranking o
#'
#' @return The return value is a bar plot
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

ranking_bl_subject <- function(r) {

#### fehler annotations ---- siehe downloaded image
  # load UI inputs from reactive value

  timerange <- r$rank_y

  states <- r$rank_states

  r_lab <- r$rank_l

  # df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
  #   dplyr::filter(jahr == timerange,
  #                 mint_select == "MINT" | fach %in% c("Alle MINT-Fächer",
  #                                                     "Alle Fächer",
  #                                                     "Alle Nicht MINT-Fächer"),
  #                 geschlecht == "Gesamt",
  #                 region == states)%>%
  #   dplyr::select(-bereich,- fachbereich) %>%
  #   dplyr::collect()
  #



  df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr = {timerange}
        AND (mint_select = 'MINT' OR fach IN ('Alle MINT-Fächer','Alle Fächer','Alle Nicht MINT-Fächer'))
        AND geschlecht = 'Gesamt'
        AND region = {states}
                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)

  df <- df %>%
    dplyr::select(-bereich,- fachbereich)







  df_ges <- df %>% dplyr::filter(fach == "Alle Fächer")%>%
    tidyr::pivot_wider(names_from=fach, values_from = wert)%>%
    dplyr::select(indikator, jahr, region, `Alle Fächer`)

  df <- df %>%
    dplyr::select(indikator, jahr, region, fach, wert)%>%
    dplyr::filter(fach != "Alle Fächer")%>%
    dplyr::left_join(df_ges , by=c("indikator", "jahr", "region"))

  df <- df %>%
    dplyr::mutate(prop = round(wert /`Alle Fächer` *100, 1 ))


  df <- df %>%
    dplyr::filter(indikator == r_lab)%>%
    dplyr::filter(!is.na(wert))%>%
    dplyr::filter(prop != 0,
                  fach != "Naturwissenschaften")


  ticks <- c("Alle MINT-Fächer",
             "Mathematik, Naturwissenschaften",
             "Mathematik",
             "Biologie",
             "Chemie",
             "Physik, Astronomie",
             "Pharmazie",
             "Geowissenschaften und Geographie",
             "Ingenieurwissenschaften (inkl. Informatik)",
             "Informatik",
             "Maschinenbau/Verfahrenstechnik" ,
             "Elektrotechnik und Informationstechnik",
             "Verkehrstechnik, Nautik",
             "Architektur, Innenarchitektur",
             "Raumplanung",
             "Bauingenieurwesen",
             "Vermessungswesen",
             "Wirtschaftsingenieurwesen mit ingenieurwissenschaftlichem Schwerpunkt",
             "Materialwissenschaft und Werkstofftechnik",
             "Bergbau, Hüttenwesen",

             "Alle Nicht MINT-Fächer"
  )



  df_t <- df %>%
    dplyr::select(fach)%>%
    unique()%>%
    as.vector()%>%
    unlist()%>%
    unname()

  ticks1 <- ticks[ticks %in% df_t]

  df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
  df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")



  #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
  out <- highcharter::hchart(df, 'bar', highcharter::hcaes(y=prop, x= fach))%>%
    highcharter::hc_tooltip(pointFormat = "{point.region} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.display_abs}") %>% #Inhalt für Hover-Box
    highcharter::hc_yAxis(title = list(text=""), labels = list(format = "{value}%")) %>% #x-Achse -->Werte in %
    highcharter::hc_xAxis(title= list(text=""),
                          categories = ticks1
    ) %>%
    highcharter::hc_plotOptions(bar = list(
      colorByPoint = TRUE,
      colors = ifelse(df$fach %in% c("Alle MINT-Fächer", "Alle Nicht MINT-Fächer",
                                      "Ingenieurwissenschaften (inkl. Informatik)",
                                      "Mathematik, Naturwissenschaften"
      ), "#b16fab", "#d0a9cd")))%>%#balken lila für MINT
    highcharter::hc_title(text = paste0( "Anteil einzelner Fächer an allen Fächern ", "(", r_lab, ")" , " in ",states," ", timerange),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG", "downloadCSV")
                                )
                              )
    )


  return(out)
}




#' A function to plot a bar chart
#'
#' @description A function to create a bar chart to compare different subjects
#' for different Bundesländer.
#'
#' @return The return value is a bar chart
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studierende_mint_vergleich_bl <- function(r) {

  # load UI inputs from reactive value

  timerange <- as.numeric(r$bl_date)

  r_lab1 <- r$rank_bl_l

  # Fach abhängig von Lehramt ja/nein zuweisen
  if(r_lab1 == "Studierende (Lehramt)")  fach_bl <- r$bl_f_lehr
  if(r_lab1 != "Studierende (Lehramt)")  fach_bl <- r$bl_f_alle


  # df_ges <- dplyr::tbl(con, from = "studierende_detailliert") %>%
  #   dplyr::filter(geschlecht=="Gesamt",
  #                 jahr %in% timerange) %>%
  #   dplyr::collect()
  #
  df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr IN ({timerange*})
        AND geschlecht = 'Gesamt'
                               ", .con = con)

  df_ges <- DBI::dbGetQuery(con, df_query)



  # df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
  #   dplyr::filter(geschlecht=="Gesamt",
  #                 jahr %in% timerange) %>%
  #   dplyr::select(-fachbereich,- mint_select, -typ )%>%
  #   dplyr::collect() %>%
  #   tidyr::pivot_wider(names_from = fach, values_from = wert)%>%
  #   dplyr::mutate(dplyr::across(c(6:ncol(.)), ~round(./`Alle Fächer`*100,1)))%>%
  #   tidyr::pivot_longer(c(6:ncol(.)), values_to = "proportion", names_to ="fach")%>%
  #   dplyr::right_join(df_ges)%>%
  #   dplyr::collect()



  df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr IN ({timerange*})
        AND geschlecht = 'Gesamt'
                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)

  df <- df %>%
    dplyr::select(-fachbereich,- mint_select, -typ) %>%
    tidyr::pivot_wider(names_from = fach, values_from = wert)%>%
    dplyr::mutate(dplyr::across(c(6:ncol(.)), ~round(./`Alle Fächer`*100,1)))%>%
    tidyr::pivot_longer(c(6:ncol(.)), values_to = "proportion", names_to ="fach")%>%
    dplyr::right_join(df_ges)




  #Trennpunkte für lange Zahlen ergänzen
  df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

  df <- df %>%
    dplyr::select(indikator, region, jahr, fach, proportion, wert)


  df <- df %>%dplyr::filter(indikator == r_lab1 )%>%
    dplyr::filter(fach==fach_bl)

  # NA aus fach entfernen für BULAs mit weniger Studienfachgruppen
  df <- stats::na.omit(df)

  # Vorbereitung Überschrift
  help_s <- fach_bl
  help_s <- ifelse(help_s == "Alle Nicht MINT-Fächer", "allen Fächern außer MINT", help_s)
  help_s <- ifelse(help_s == "Alle MINT-Fächer", "MINT", help_s)


  r_lab1 <- ifelse(r_lab1 == "Studierende", paste0(r_lab1, "n"), r_lab1)
  r_lab1 <- ifelse(grepl("Lehr", r_lab1), "Studierenden (Lehramt)", r_lab1)
  help <- r_lab1
  help <- ifelse(help == "internationalenen Studienanfänger:innen (1. Hochschulsemester)", "internationalen Studienanfänger:innen", help)
  help <- ifelse(help == "Studienanfänger:innen (1. Fachsemester)" |
                   help == "Studienanfänger:innen (1. Hochschulsemester)", "Studienanfänger:innen", help)



  df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
  df$display_rel <- prettyNum(df$proportion, big.mark = ".", decimal.mark = ",")

  # Plot
  #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
  out <- highcharter::hchart(df, 'bar', highcharter::hcaes(x= region, y = proportion))%>%
    highcharter::hc_tooltip(pointFormat = "{point.fach} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.display_abs}") %>% #Inhalt für Hover-Box
    highcharter::hc_yAxis(title = list(text=""), labels = list(format = "{value}%")) %>% #x-Achse -->Werte in %
    highcharter::hc_xAxis(title= list(text="")) %>% #Y-Achse - keine Beschriftung
    highcharter::hc_colors("#b16fab") %>% #balken lila für MINT
    highcharter::hc_title(text = paste0( "Anteil von ", r_lab1 ," in ", help_s," an allen ", help,  " (", timerange, ")"),
                          margin = 25,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px"))   %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(
                                contextButton = list(
                                  menuItems = list("downloadPNG", "downloadCSV")
                                )
                              )
    )



  return(out)
}


# Frauen in MINT ----
### Tab 1 ----

#' A function to plot a graph.
#'
#' @description A function to create a pie chart for the first box
#' inside the tab "Schule".
#'
#' @return The return value is a plot
#' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_einstieg_gender <- function(r) {
  betrachtung <- r$ansicht_gen_mint #Kuchendia
  timerange <- r$gen_y #Jahr
  genl <- r$gen_l #Studierende, Absolventinnen

  if(betrachtung == "Einzelansicht - Kuchendiagramm"){
    gegenwert <- r$gen_gegenwert_pie
    regio <- r$gen_region_mint

    gen_f <- r$gen_f

        if(gegenwert == "Ja") gen_f <- c(gen_f, "Alle Nicht MINT-Fächer")

        # df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
        #   dplyr::filter(jahr == timerange,
        #                 region== regio,
        #                 fach %in% gen_f,
        #                 geschlecht != "Gesamt",
        #                 indikator %in% genl) %>%
        #   dplyr::select(-fachbereich, -region, - jahr, -bereich, -mint_select, -typ)%>%
        #   dplyr::collect()



        df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr = {timerange}
        AND geschlecht != 'Gesamt'
        AND region = {regio}
        AND fach IN ({gen_f*})
        AND indikator IN ({genl*})
                               ", .con = con)

        df <- DBI::dbGetQuery(con, df_query)

        df <- df %>%
          dplyr::select(-fachbereich, -region, - jahr, -bereich, -mint_select, -typ)



#
#
#         alle <- dplyr::tbl(con, from = "studierende_detailliert") %>%
#           dplyr::filter(jahr == timerange,
#                         region== regio,
#                         fach %in% gen_f,
#                         geschlecht == "Gesamt",
#                         indikator %in% genl) %>%
#                         dplyr::select(-fachbereich, -region, - jahr, -bereich, -mint_select, -typ)%>%
#           dplyr::collect()



        df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr = {timerange}
        AND geschlecht = 'Gesamt'
        AND region = {regio}
        AND fach IN ({gen_f*})
        AND indikator IN ({genl*})
                               ", .con = con)

        alle <- DBI::dbGetQuery(con, df_query)

        alle <- alle %>%
          dplyr::select(-fachbereich, -region, - jahr, -bereich, -mint_select, -typ)



        df <- df %>%
          dplyr::left_join(alle, dplyr::join_by(fach, indikator)) %>%
          dplyr::select(-geschlecht.y) %>%
          dplyr::rename(wert = wert.x,
                        wert_ges = wert.y,
                        geschlecht = geschlecht.x) %>%
          dplyr::mutate(prop = round(wert/wert_ges * 100,1))

        df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

        titel_help <- gen_f[1]
        titel_help <- ifelse(titel_help == "Alle MINT-Fächer", "MINT-Fächern", titel_help)
        titel_help <- ifelse(titel_help == "Alle Nicht MINT-Fächer", "allen Nicht-MINT-Fächern", titel_help)
        titel_help <- ifelse(titel_help == "allgemeine naturwissenschaftliche und mathematische Fächer",
                             "allgemeinen naturwissenschaftlichen und mathematischen Fächern", titel_help)


        if(nrow(df) == 0){
          titel <- "Für diese Kombination aus Fächergruppe und Bundesland bzw. Bundesländer liegen keine Daten vor.
        Bitte wählen Sie eine andere Komination oder Fächergruppe aus."
          df$jahr <- NA

          out <- linebuilder(df, titel = titel, x = "jahr", y = "wert", group = "geschlecht", tooltip = "Anzahl: {point.display_abs}", format = "{value:, f}")

        }else{

        if(length(genl) == 1) {

          #neu

          title_n <- genl[1]
          title_n <- ifelse(title_n == "Studierende", "Studierenden", title_n)
          title_n <- ifelse(title_n == "Studierende (Lehramt)", "Studierenden (Lehramt)", title_n)

          df_p <- df[df$fach == gen_f[1],]

          titel <- ifelse(regio == "Saarland",
                          paste0("Frauenanteil unter ", title_n, " in ", titel_help, " im ", regio, " (", timerange, ")"),
                          paste0("Frauenanteil unter ", title_n, " in ", titel_help, " in ", regio, " (", timerange, ")"))

          tooltip <- paste('Anteil: {point.prop}% <br> Anzahl: {point.display_abs}')
          color <- c("#efe8e6", "#154194")

          p1 <- piebuilder(df_p, titel, x = "geschlecht", y ="prop", tooltip, color = color, format = '{point.prop}%')

          out <- p1


           if(gegenwert == "Ja"){


             title_n <- genl[1]
             title_n <- ifelse(title_n == "Studierende", "Studierenden", title_n)
             title_n <- ifelse(title_n == "Studierende (Lehramt)", "Studierenden (Lehramt)", title_n)

             df_g <- df[df$fach == "Alle Nicht MINT-Fächer",]

             titel <- ifelse(regio == "Saarland",
                             paste0("Frauenanteil unter ", title_n, " in Nicht MINT-Fächern im ", regio, " (", timerange, ")"),
                             paste0("Frauenanteil unter ", title_n, " in Nicht MINT-Fächern in ", regio, " (", timerange, ")"))

             tooltip <- paste('Anteil: {point.prop}% <br> Anzahl: {point.display_abs}')
             color <- c("#efe8e6", "#154194")
             format <- '{point.prop}%'


             p1g <- piebuilder(df_g, titel, x = "geschlecht", y = "prop", tooltip, color, format)

             out <- highcharter::hw_grid(p1, p1g, ncol = 2,    browsable = TRUE)


           }

        } else if(length(genl) == 2) {

          title_n1 <- genl[1]
          title_n1 <- ifelse(title_n1 == "Studierende", "Studierenden", title_n1)
          title_n1 <- ifelse(title_n1 == "Studierende (Lehramt)", "Studierenden (Lehramt)", title_n1)

          title_n2 <- genl[2]
          title_n2 <- ifelse(title_n2 == "Studierende", "Studierenden", title_n2)
          title_n2 <- ifelse(title_n2 == "Studierende (Lehramt)", "Studierenden (Lehramt)", title_n2)

          #neu
          df_1_pie <- df %>% dplyr::filter(indikator == genl[1], fach != "Alle Nicht MINT-Fächer")
          df_2_pie <- df %>% dplyr::filter(indikator == genl[2], fach != "Alle Nicht MINT-Fächer")

          titel1 = ifelse(regio == "Saarland",
                          paste0("Frauenanteil unter ", title_n1, " in ", titel_help, " im ", regio, " (", timerange, ")"),
                          paste0("Frauenanteil unter ", title_n1, " in ", titel_help, " in ", regio, " (", timerange, ")"))

          titel2 = ifelse(regio == "Saarland",
                          paste0("Frauenanteil unter ", title_n2, " in ", titel_help, " im ", regio, " (", timerange, ")"),
                          paste0("Frauenanteil unter ", title_n2, " in ", titel_help, " in ", regio, " (", timerange, ")"))

          tooltip <- paste('Anteil: {point.prop}% <br> Anzahl: {point.display_abs}')
          format = '{point.prop}%'
          color = c("#efe8e6", "#154194")

          p1 <- piebuilder(df_1_pie, titel1, x = "geschlecht", y = "prop", tooltip, color, format)
          p2 <- piebuilder(df_2_pie, titel2, x = "geschlecht", y = "prop", tooltip, color, format)



          out <- highcharter::hw_grid(p1, p2, ncol = 2, browsable = TRUE)


          if(gegenwert == "Ja"){

            title_n1 <- genl[1]
            title_n1 <- ifelse(title_n1 == "Studierende", "Studierenden", titel_n1)
            title_n1 <- ifelse(title_n1 == "Studierende (Lehramt)", "Studierenden (Lehramt)", title_n1)

            title_n2 <- genl[2]
            title_n2 <- ifelse(title_n2 == "Studierende", "Studierenden", title_n2)
            title_n2 <- ifelse(title_n2 == "Studierende (Lehramt)", "Studierenden (Lehramt)", title_n2)

            df1_g <- df[df$fach == "Alle Nicht MINT-Fächer" & df$indikator == genl[1],]
            df2_g <- df[df$fach == "Alle Nicht MINT-Fächer" & df$indikator == genl[2],]

            titel <- ifelse(regio == "Saarland",
                            paste0("Frauenanteil unter ", title_n1, " in Nicht-MINT-Fächern im ", regio, " (", timerange, ")"),
                            paste0("Frauenanteil unter ", title_n1, " in Nicht-MINT-Fächern in ", regio, " (", timerange, ")"))

            tooltip <- paste('Anteil: {point.prop}% <br> Anzahl: {point.display_abs}')
            format <- '{point.prop}%'
            color = c("#efe8e6", "#154194")

            p1g <- piebuilder(df1_g, titel, x = "geschlecht", y = "prop", tooltip, color, format)

            titel2 <- ifelse(regio == "Saarland",
                             paste0("Frauenanteil unter ", title_n2, " in Nicht-MINT-Fächern im ", regio, " (", timerange, ")"),
                             paste0("Frauenanteil unter ", title_n2, " in Nicht-MINT-Fächern im ", regio, " (", timerange, ")"))

            p2g <- piebuilder(df2_g, titel2, x = "geschlecht", y = "prop", tooltip, color, format)

            out <- highcharter::hw_grid(p1, p2, p1g, p2g, ncol = 2, browsable = TRUE)

          }

        #dieser teil ist völlig obsolet
        } else if(length(genl) == 3) {

          df_1_pie <- df %>% dplyr::filter(indikator == genl[1], fach != "Alle Nicht MINT-Fächer")
          df_2_pie <- df %>% dplyr::filter(indikator == genl[2], fach != "Alle Nicht MINT-Fächer")
          df_3_pie <- df %>% dplyr::filter(indikator == genl[3], fach != "Alle Nicht MINT-Fächer")


          titel1 <- paste0("Frauenanteil unter ",genl[1], " in ",titel_help, " (", timerange, ")")
          titel2 <- paste0("Frauenanteil unter ",genl[2], " in ",titel_help, " (", timerange, ")")
          titel3 <- paste0("Frauenanteil unter ",genl[3], " in ",titel_help, " (", timerange, ")")

          tooltip <- paste('Anteil: {point.prop}% <br> Anzahl: {point.display_abs}')
          color <- c("#efe8e6", "#154194")
          format <- '{point.prop}%'

          p1 <- piebuilder(df_1_pie, titel1, x ="geschlecht", y = "prop", tooltip, color, format)
          p2 <- piebuilder(df_2_pie, titel2, x ="geschlecht", y = "prop", tooltip, color, format)
          p3 <- piebuilder(df_3_pie, titel3, x ="geschlecht", y = "prop", tooltip, color, format)


      out <- highcharter::hw_grid(p1, p2, p3, ncol = 3,browsable = TRUE)

      if(gegenwert == "Ja"){
        df1_g <- df[df$fach == "Alle Nicht MINT-Fächer" & df$indikator == genl,]
        df2_g <- df[df$fach == "Alle Nicht MINT-Fächer" & df$indikator == genl,]
        df3_g <- df[df$fach == "Alle Nicht MINT-Fächer" & df$indikator == genl,]

        titel1 <- paste0("Frauenanteil unter ",genl[1], " in Nicht MINT-Fächern (", timerange, ")")
        titel2 <- paste0("Frauenanteil unter ",genl[2], " in Nicht MINT-Fächern (", timerange, ")")
        titel3 <- paste0("Frauenanteil unter ",genl[3], " in Nicht MINT-Fächern (", timerange, ")")

        tooltip <- paste('Anteil: {point.prop}% <br> Anzahl: {point.display_abs}')
        color <- c("#efe8e6", "#154194")

        p1g <- piebuilder(df1_g, titel1, x = "geschlecht", y ="prop", tooltip, color, format= '{point.prop}%')
        p2g <- piebuilder(df2_g, titel2, x = "geschlecht", y ="prop", tooltip, color, format= '{point.prop}%')
        p3g <- piebuilder(df3_g, titel3, x = "geschlecht", y ="prop", tooltip, color, format= '{point.prop}%')

        out <- highcharter::hw_grid(p1, p2, p3,p1g, p2g, p3g,ncol = 3,browsable = TRUE)

      }

        }
        }
  }
  else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){
    sel_bl1 <- r$gen_states_faecher

    # Zuweisung von r zu sel_f in abhängigkeit der Bundesländer
    if(sel_bl1 %in% c("Deutschland",
                      "Baden-Württemberg",
                      "Bayern",
                      "Berlin",
                      "Hamburg",
                      "Hessen",
                      "Nordrhein-Westfalen",
                      "Rheinland-Pfalz",
                      "Sachsen",
                      "Westdeutschland (o. Berlin)",
                      "Ostdeutschland (inkl. Berlin)")) {
      sel_f1 <- r$gen1_f
    }
    else {
      if(sel_bl1 == "Brandenburg") sel_f1 <- r$gen2_f
      if(sel_bl1 == "Bremen") sel_f1 <- r$gen3_f
      if(sel_bl1 == "Mecklenburg-Vorpommern") sel_f1 <- r$gen4_f
      if(sel_bl1 == "Niedersachsen") sel_f1 <- r$gen5_f
      if(sel_bl1 == "Saarland") sel_f1 <- r$gen6_f
      if(sel_bl1 == "Sachsen-Anhalt") sel_f1 <- r$gen7_f
      if(sel_bl1 == "Schleswig-Holstein") sel_f1 <- r$gen8_f
      if(sel_bl1 == "Thüringen") sel_f1 <- r$gen9_f
    }

    gegenwert <- r$gen_gegenwert_balken
    if(gegenwert == "Ja") sel_f1 <- c(sel_f1, "Alle Nicht MINT-Fächer")

    # # filter dataset based on UI inputs
    # df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    #   dplyr::filter(jahr %in% timerange,
    #                 region==sel_bl1,
    #                 fach %in% sel_f1)%>%
    #   dplyr::collect()


    df_query <- glue::glue_sql("
      SELECT region, fach, jahr, indikator, geschlecht, wert
      FROM studierende_detailliert
      WHERE jahr = ({timerange*})
      AND region = {sel_bl1}
      AND fach IN ({sel_f1*})
      ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)




    if(nrow(df) == 0){
      titel <- "Für diese Kombination aus Fächergruppe und Bundesland bzw. Bundesländer liegen keine Daten vor.
        Bitte wählen Sie eine andere Komination oder Fächergruppe aus."
      df$jahr <- NA

      out <- linebuilder(df, titel = titel, x = "jahr", y = "wert", group = "geschlecht", tooltip = "Anzahl: {point.display_abs}", format = "{value:, f}")

    }else{

    df_gen <- df %>%
      tidyr::pivot_wider(names_from = geschlecht, values_from = wert) %>%
      dplyr::mutate(across(c("Männer", "Frauen"), ~ round(./Gesamt*100,1)))%>%
      dplyr::select(-Gesamt)%>%
      tidyr::pivot_longer(c("Männer", "Frauen"), names_to = "geschlecht", values_to  = "proportion")%>%
      dplyr::filter(indikator !="internationale Studienanfänger:innen (1. Hochschulsemester)"&indikator!= "internationale Studierende",
                    !(indikator %in% c("ausländische Studienanfänger:innen (1. Hochschulsemester)", "ausländische Studierende")))

    df <- df %>%
      tidyr::pivot_wider(names_from = geschlecht, values_from = wert) %>%
      dplyr::select(-Gesamt)%>%
      tidyr::pivot_longer(c("Männer", "Frauen"), values_to = "wert", names_to = "geschlecht")%>%
      dplyr::right_join(df_gen)%>%
      dplyr::filter(!is.nan(proportion))


    #Trennpunkte für lange Zahlen ergänzen
    df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$display_rel <- prettyNum(df$proportion, big.mark = ".", decimal.mark = ",")

    #überschrift vorbereiten
    fach_label <- sel_f1
    fach_label <- ifelse(fach_label == "Alle MINT-Fächer", "MINT", fach_label)
    if(gegenwert == "Ja"){
      titel <- ifelse(sel_bl1 == "Saarland",
                      paste0("Frauenanteil in der Studienfachgruppe ", fach_label, " und allen Nicht-MINT-Fächern im ", sel_bl1, " (", timerange, ")"),
                      paste0("Frauenanteil in der Studienfachgruppe ", fach_label, " und allen Nicht-MINT-Fächern in ", sel_bl1, " (", timerange, ")"))
    }else{
      titel <- ifelse(sel_bl1 == "Saarland",
                      paste0("Frauenanteil in der Studienfachgruppe ", fach_label, " im ", sel_bl1, " (", timerange, ")"),
                      paste0("Frauenanteil in der Studienfachgruppe ", fach_label, " in ", sel_bl1, " (", timerange, ")"))

    }

    df$fach_indikator <- paste(df$fach, df$indikator, sep = " - ")

   # df <- df[with(df, order(indikator)), ]
    df <- df[with(df, order(proportion, decreasing = TRUE)), ]

    #da es viele NAs gibt, also Daten, die nicht berechnet wurden und können denke ich, gibt es ein filtering
    df <- df %>%
      dplyr::filter(!is.na(proportion), !is.na(wert))




    #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
    out <- highcharter::hchart(df, 'bar', highcharter::hcaes(x = fach_indikator, y=proportion, group = geschlecht))%>%

      highcharter::hc_tooltip(pointFormat = "{point.geschlecht}-Anteil: {point.display_rel} % <br> Anzahl: {point.display_abs}")%>%

      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  FALSE) %>%
      highcharter::hc_xAxis(title = list(text = "")
      ) %>%
      highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
      highcharter::hc_colors(c("#154194", "#efe8e6")) %>%
      highcharter::hc_title(text = titel,
                            margin = 25,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = FALSE) %>%
      highcharter::hc_exporting(enabled = TRUE,
                                buttons = list(
                                  contextButton = list(
                                    menuItems = list("downloadPNG", "downloadCSV")
                                  )
                                )
      )
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
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_verlauf_single_gender <- function(r) {

  # load UI inputs from reactive value
  absolut_selector <- r$abs_zahlen
  timerange <- r$genz_date
  t <- timerange[1]:timerange[2]
  label_sel <- r$genzl
  regio <- r$gen_z_region
  faecher <- r$gen_z_faecher

  # filter dataset based on UI inputs
  # df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
  #   dplyr::filter(jahr %in% t,
  #                 region== regio,
  #                 fach == faecher,
  #                 indikator %in% label_sel) %>%
  #   dplyr::collect()
  #


  df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr IN ({t*})
        AND region = {regio}
        AND fach = {faecher}
        AND indikator IN ({label_sel*})
                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)





  if(nrow(df) == 0){
    titel <- "Für diese Kombination aus Fächergruppe und Bundesland bzw. Bundesländer liegen keine Daten vor.
        Bitte wählen Sie eine andere Komination oder Fächergruppe aus."
    df$jahr <- NA

    out <- linebuilder(df, titel = titel, x = "jahr", y = "wert", group = "geschlecht", tooltip = "Anzahl: {point.display_abs}", format = "{value:, f}")

  }else{
  # calculation props
  df_frauen <- df %>%
    tidyr::pivot_wider(values_from=wert, names_from=geschlecht)%>%
    dplyr::mutate(across(c("Männer", "Frauen"), ~round(./Gesamt*100,1)))%>%
    dplyr::select(-Gesamt,- Männer)%>%
    tidyr::pivot_longer( "Frauen", names_to = "geschlecht", values_to = "proportion")

  # joining
  df <- df %>%
    dplyr::right_join(df_frauen)%>%
    dplyr::filter(fachbereich != "Nicht MINT")%>%
    tidyr::pivot_longer(c("wert", "proportion"), values_to = "wert", names_to = "selector")%>%
    dplyr::mutate(selector=dplyr::case_when(
      selector=="wert" ~ "Anzahl",
      T~"In Prozent"
    ))}

  #Trennpunkte für lange Zahlen ergänzen

  if(absolut_selector=="In Prozent"){

    df <- df %>% dplyr::filter(selector=="In Prozent")

    df$display_rel <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

    df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

    fach_help <- faecher
    fach_help <- ifelse(fach_help == "Alle MINT-Fächer", "MINT", fach_help)
    fach_help <- ifelse(fach_help == "Alle Nicht MINT-Fächer", "Nicht-MINT", fach_help)


    if(length(label_sel) == 1) {

      df <- df %>% dplyr::filter(indikator== label_sel)


      titel <-  ifelse(regio =="Saarland",
                       paste0("Frauenanteil in der Studienfachgruppe ", fach_help, " im ", regio),
                       paste0("Frauenanteil in der Studienfachgruppe ", fach_help, " in ", regio))
      tooltip <- "Anteil {point.indikator} <br> Wert: {point.display_rel} %"
      format <- "{value}%"
      color <- c("#b16fab", "#154194", "#66cbaf", "#fcc433")
      out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "indikator", tooltip, format, color)

    } else if(length(label_sel)==2){



      df <- df %>% dplyr::filter(indikator == label_sel[1] | indikator == label_sel [2])

      titel <- ifelse(regio == "Saarland",
                      paste0("Frauenanteil in der Studienfachgruppe ", fach_help, " im ", regio),
                      paste0("Frauenanteil in der Studienfachgruppe ", fach_help, " in ", regio))
      tooltip <- "Anteil {point.indikator} <br> Wert: {point.display_rel} %"
      format <- "{value}%"
      color <- c("#b16fab", "#154194", "#66cbaf", "#fcc433")
      out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "indikator", tooltip, format, color)




    } else if(length(label_sel) == 3){


      df<- df %>% dplyr::filter(indikator == label_sel[1] | indikator == label_sel [2] | indikator == label_sel [3])

      titel <- ifelse(regio == "Saarland",
                      paste0("Frauenanteil in der Studienfachgruppe ", fach_help, " im ", regio),
                      paste0("Frauenanteil in der Studienfachgruppe ", fach_help, " in ", regio))
      tooltip <- "Anteil {point.indikator} <br> Wert: {point.display_rel} %"
      format <- "{value}%"
      color <- c("#b16fab", "#154194", "#66cbaf", "#fcc433")
      out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "indikator", tooltip, format, color)


    } else if(length(label_sel) == 4){

      df<- df %>% dplyr::filter(indikator == label_sel[1] | indikator == label_sel [2] | indikator == label_sel [3] | indikator == label_sel [4])

      titel <- ifelse(regio == "Saarland",
                      paste0("Frauenanteil in der Studienfachgruppe ", fach_help, " im ", regio),
                      paste0("Frauenanteil in der Studienfachgruppe ", fach_help, " in ", regio))
      tooltip <- "Anteil {point.indikator} <br> Wert: {point.display_rel} %"
      format <- "{value}%"
      color <- c("#b16fab", "#154194", "#66cbaf", "#fcc433")
      out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "indikator", tooltip, format, color)


    } }else if(absolut_selector=="Anzahl"){


      fach_help <- faecher
      fach_help <- ifelse(fach_help == "Alle MINT-Fächer", "MINT", fach_help)
      fach_help <- ifelse(fach_help == "Alle Nicht MINT-Fächer", "Nicht-MINT", fach_help)

      hcoptslang <- getOption("highcharter.lang")
      hcoptslang$thousandsSep <- "."
      options(highcharter.lang = hcoptslang)

      df <- df%>%
        dplyr::filter(selector=="Anzahl")
      df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

      df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

      if(length(label_sel) == 1) {

        df <- df %>% dplyr::filter(indikator== label_sel)

        titel <- ifelse(regio == "Saarland",
                        paste0("Frauenanteil in der Studienfachgruppe ", fach_help, " im ", regio),
                        paste0("Frauenanteil in der Studienfachgruppe ", fach_help, " in ", regio))
        tooltip <- "Anzahl: {point.display_abs}"
        format <- "{value:, f}"
        color <- c("#b16fab", "#154194", "#66cbaf", "#fcc433")
        out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "indikator", tooltip, format, color)

      } else if(length(label_sel)==2){


        df <- df %>% dplyr::filter(indikator == label_sel[1] | indikator == label_sel [2])

        titel <- ifelse(regio == "Saarland",
                        paste0("Frauenanteil in der Studienfachgruppe ", fach_help, " im ", regio),
                        paste0("Frauenanteil in der Studienfachgruppe ", fach_help, " in ", regio))
        tooltip <- "Anzahl: {point.display_abs}"
        format <- "{value:, f}"
        color <- c("#b16fab", "#154194", "#66cbaf", "#fcc433")
        out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "indikator", tooltip, format, color)

      } else if(length(label_sel) == 3){


        df<- df %>% dplyr::filter(indikator == label_sel[1] | indikator == label_sel [2] | indikator == label_sel [3])

        titel <- ifelse(regio == "Saarland",
                        paste0("Frauenanteil in der Studienfachgruppe ", fach_help, " im ", regio),
                        paste0("Frauenanteil in der Studienfachgruppe ", fach_help, " in ", regio))
        tooltip <- "Anzahl: {point.display_abs}"
        format <- "{value:, f}"
        color <- c("#b16fab", "#154194", "#66cbaf", "#fcc433")
        out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "indikator", tooltip, format, color)




      } else if (length(label_sel) == 4){

        df<- df %>% dplyr::filter(indikator == label_sel[1] | indikator == label_sel [2] | indikator == label_sel [3] | indikator == label_sel [4])

        titel <- ifelse(regio == "Saarland",
                        paste0("Frauenanteil in der Studienfachgruppe ", fach_help, " im ", regio),
                        paste0("Frauenanteil in der Studienfachgruppe ", fach_help, " in ", regio))
        tooltip <- "Anzahl: {point.display_abs}"
        format <- "{value:, f}"
        color <- c("#b16fab", "#154194", "#66cbaf", "#fcc433")
        out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "indikator", tooltip, format, color)

      }
    }




  return(out)
}

### Tab 3 -----

#' A function to plot a waffle chart
#'
#' @description A function to create a waffle chart inside the
#' tab "Studium".
#'
#' @return The return value is a waffle chart
#' @param df The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_choice_gender <- function(r) {

  betrachtung <- r$ansicht_studi_gen_wahl

  if(betrachtung == "Einzelansicht - Kuchendiagramm"){
    # load UI inputs from reactive value
    timerange <- r$choice_y
    lab_cho <- r$choice_l
    vergl <- r$gegenwert_studi_gen
    regio <- r$region_studi_gen

    color_fachbereich <- c(
      "Ingenieurwissenschaften (inkl. Informatik)" = "#00a87a",
      "Mathematik, Naturwissenschaften" = "#fcc433",
      "andere Fachbereiche" = "#efe8e6"
    )

    if(vergl == "Ja"){
      gen <- c("Frauen", "Männer")
    } else{
      gen <- "Frauen"
    }

    # filter dataset based on UI inputs
    # df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    #   dplyr::filter(jahr == timerange,
    #                 region==regio,
    #                 geschlecht %in% gen,
    #                 indikator == lab_cho,
    #                 (fach %in% c("Mathematik, Naturwissenschaften", "Alle Nicht MINT-Fächer",
    #                              "Ingenieurwissenschaften (inkl. Informatik)"))) %>%
    #   dplyr::collect()



    df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr = {timerange}
        AND geschlecht IN ({gen*})
        AND region = {regio}
        AND indikator = {lab_cho}
        AND fach IN ('Mathematik, Naturwissenschaften','Alle Nicht MINT-Fächer','Ingenieurwissenschaften (inkl. Informatik)')
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)





    # df_alle <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    #   dplyr::filter(jahr == timerange,
    #                 region==regio,
    #                 geschlecht %in% gen,
    #                 indikator == lab_cho,
    #                 fach == "Alle Fächer") %>%
    #   dplyr::rename(wert_ges = wert) %>%
    #   dplyr::collect()
    #

    df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr = {timerange}
        AND geschlecht IN ({gen*})
        AND region = {regio}
        AND indikator = {lab_cho}
        AND fach = 'Alle Fächer'
                               ", .con = con)

    df_alle <- DBI::dbGetQuery(con, df_query)

    df_alle <- df_alle %>%
      dplyr::rename(wert_ges = wert)



    df <- df %>%
      dplyr::left_join(df_alle, dplyr::join_by(jahr, indikator, geschlecht, region)) %>%
      dplyr::select(-fach.y) %>%
      dplyr::rename(fach = fach.x) %>%
      dplyr::mutate(prop = round(wert/wert_ges * 100,1))

    df$fach[df$fach == "Alle Nicht MINT-Fächer"] <- "andere Fachbereiche"

    df <- df[with(df, order(prop, decreasing = FALSE)), ]
    df <- df %>% dplyr::mutate(col = color_fachbereich[fach])
    df$wert <- prettyNum(df$wert, big.mark=".", decimal.mark = ",")

    lab_cho <- ifelse(lab_cho == "Studierende", "Studierenden", lab_cho)
    lab_cho <- ifelse(lab_cho == "Studierende (Lehramt)", "Lehramtstudierenden", lab_cho)
    lab_cho <- ifelse(lab_cho == "Studienanfänger:innen (1. Hochschulsemester)", "Studienanfänger:innen", lab_cho)

    if(lab_cho == "Absolvent:innen"){
      titel <- ifelse(regio == "Saarland",
                      paste0("Weibliche Absolvent:innen im ", regio, " nach Fach (", timerange, ")"),
                      paste0("Weibliche Absolvent:innen in ", regio, " nach Fach (", timerange, ")"))
    }else{
      titel <- ifelse(regio == "Saarland",
                      paste0("Studienfachwahl von Frauen im ", regio, " (", timerange, ")"),
                      paste0("Studienfachwahl von Frauen in ", regio, " (", timerange, ")"))
    }

    if(vergl == "Ja"){
      df_f <- df %>% dplyr::filter(geschlecht == "Frauen")
      df_m <- df %>% dplyr::filter(geschlecht == "Männer")

      titelm <- ifelse(regio == "Saarland",
                       paste0("Studienfachwahl von Männern im ", regio, " (", timerange, ")"),
                       paste0("Studienfachwahl von Männern in ", regio, " (", timerange, ")"))
      subtitel <- paste0("Von allen weiblichen ", lab_cho, " wählen ", 100-df_f$prop[df_f$fach=="andere Fachbereiche"],
                         " % ein MINT-Fach.")
      subtitelm <- paste0("Von allen männlichen ", lab_cho, " wählen ", 100-df_m$prop[df_m$fach=="andere Fachbereiche"],
                          " % ein MINT-Fach.")

      tooltip <- paste('Anteil: {point.prop}% <br> Anzahl: {point.wert}')

      color = as.character(df_f$col)

      format <- '{point.prop}%'

      p1 <- piebuilder(df_f, titel, x="fach", y="prop", tooltip, color, format, subtitel)
      p2 <- piebuilder(df_m, titelm, x="fach", y="prop", tooltip, color, format, subtitelm)


      out <- highcharter::hw_grid(p1, p2, ncol = 2, browsable = TRUE)
    }else{

      color <- as.character(df$col)
      format <- '{point.prop}%'
      tooltip <- paste('Anteil: {point.prop}% <br> Anzahl: {point.wert}')

      out <- piebuilder(df, titel, x = "fach", y ="prop", tooltip, color, format)

    }

  } else if(betrachtung == "Zeitverlauf - Liniendiagramm"){

    # load UI inputs from reactive value
    timerange <- r$choice_V_y
    t <- timerange[1]:timerange[2]
    v_lab <- r$choice_l_v
    absolut_selector <- r$abs_zahlen_l_v
    subjects_select <- r$choice_v_f
    states <- r$choice_states

    # df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    #   dplyr::filter(jahr %in% t,
    #                 region==states,
    #                 geschlecht == "Frauen",
    #                 indikator %in% v_lab,
    #                 fach == subjects_select) %>%
    #   dplyr::select(region, fach, jahr, indikator, geschlecht, wert) %>%
    #   dplyr::collect()



    df_query <- glue::glue_sql("
        SELECT region, jahr, indikator, fach, wert, geschlecht
        FROM studierende_detailliert
        WHERE jahr In ({t*})
        AND geschlecht = 'Frauen'
        AND region = {states}
        AND indikator IN ({v_lab*})
        AND fach = {subjects_select}
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)



    titel <- ifelse(states == "Saarland",
                    paste0("Weibliche ",  paste(v_lab, collapse = " & "), " im ", states, " nach Fach"),
                    paste0("Weibliche ",  paste(v_lab, collapse = " & "), " in ", states, " nach Fach"))

    if (absolut_selector=="In Prozent"){

      # df_alle <- dplyr::tbl(con, from = "studierende_detailliert") %>%
      #   dplyr::filter(jahr %in% t,
      #                 region == states,
      #                 geschlecht == "Frauen",
      #                 indikator %in% v_lab,
      #                 fach == "Alle Fächer") %>%
      #   dplyr::select(region, fach, jahr, indikator, geschlecht, wert) %>%
      #   dplyr::rename(wert_ges = wert) %>%
      #   dplyr::collect()

      df_query <- glue::glue_sql("
        SELECT region, fach, jahr, indikator, geschlecht, wert AS wert_ges
        FROM studierende_detailliert
        WHERE jahr in ({t*})
        AND region = {states}
        AND geschlecht = 'Frauen'
        AND indikator IN ({v_lab*})
        AND fach = 'Alle Fächer'
                               ", .con = con)

      df_alle <- DBI::dbGetQuery(con, df_query)




      df <- df %>%
        dplyr::left_join(df_alle, by = c("jahr", "indikator", "geschlecht", "region")) %>%
        dplyr::select(-fach.y) %>%
        dplyr::rename(fach = fach.x) %>%
        dplyr::mutate(prop = round(wert/wert_ges * 100,1))

      df <- df[with(df, order( jahr, decreasing = FALSE)), ]

      #Trennpunkte für lange Zahlen ergänzen
      #browser()
      df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")
      df$display_wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
      max_c <- max(df$prop) + 3
      min_c <- min(df$prop) - 3

      subtitel <- paste0("Von allen weiblichen Studierenden bzw. Absolvent:innen hat ein so großer Anteil ein MINT-Fach belegt.")

      titel <- titel
      tooltip <- "{point.indikator}<br> Anteil: {point.display_rel} %"
      format <- "{value} %"
      color <- c("#b16fab", "#154194", "#66cbaf", "#fbbf24",
                 "#AFF3E0","#2D6BE1","#008F68","#8893a7", "#ee7775", "#9d7265", "#35bd97",
                 "#bfc6d3", "#5f94f9",  "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
      out <- linebuilder(df, titel, x = "jahr", y = "prop", group = "indikator", tooltip, format, color)




    }else if(absolut_selector=="Anzahl"){

      hcoptslang <- getOption("highcharter.lang")
      hcoptslang$thousandsSep <- "."
      options(highcharter.lang = hcoptslang)

      df <- df[with(df, order( jahr, decreasing = FALSE)), ]

      df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

      subtitel <- paste0("Von allen weiblichen Studierenden bzw. Absolvent:innen hat eine so große Anzahl ein MINT-Fach belegt.")



      titel <- titel
      tooltip <- "{point.indikator}<br> Anzahl: {point.display_abs}"
      format <- "{value:, f}"
      color <- c("#b16fab", "#154194", "#66cbaf", "#fbbf24","#AFF3E0","#2D6BE1","#008F68","#8893a7", "#ee7775", "#9d7265", "#35bd97",
                 "#bfc6d3", "#5f94f9",  "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
      out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "indikator", tooltip, format, color)


    }

  }

  return(out)
}

### Tab 4 ----

#' A function to create barplots, showing ranked study subjects
#'
#' @description A function to compare different subjects
#'
#' @return The return value is a barplot
#' @param data The dataframe "studierende_faecher.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

plot_ranking_top_faecher <- function(r) {


  # load UI inputs from reactive value
  timerange <- r$date_top_faecher

  states <- r$states_top_faecher

  subject <- r$subject_top_faecher

  abs_rel <- r$subject_abs_rel

  # filter dataset based on UI inputs
  # df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
  #   dplyr::filter(jahr == timerange,
  #                 indikator == "Studierende",
  #                 region == states,
  #                 !fach %in% c(
  #                   "Außerhalb der Studienbereichsgliederung/Sonstige Fächer",
  #                   "Weitere ingenieurwissenschaftliche Fächer",
  #                   "Weitere naturwissenschaftliche und mathematische Fächer"
  #                 )) %>%
  #   dplyr::collect()


  df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr = {timerange}
        AND region = {states}
        AND indikator = 'Studierende'
        AND fach NOT IN ('Außerhalb der Studienbereichsgliederung/Sonstige Fächer','Weitere ingenieurwissenschaftliche Fächer','Weitere naturwissenschaftliche und mathematische Fächer')
                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)


  df_props <- df %>%
    tidyr::pivot_wider(values_from = wert, names_from=geschlecht)%>%
    dplyr::mutate(dplyr::across(c("Männer", "Frauen"), ~round(./Gesamt*100,1)))%>%
    dplyr::select(-Gesamt)%>%
    tidyr::pivot_longer(c("Männer", "Frauen"), names_to="geschlecht", values_to = "prop")

  df <- df %>%
    dplyr::filter(geschlecht!="Gesamt")%>%
    dplyr::left_join(df_props)



  #Trennpunkte für lange Zahlen ergänzen


  if(subject == "MINT-Fächer"){

    df <- df %>% dplyr::filter(fachbereich %in% c("MINT",
                                                  "Mathematik, Naturwissenschaften",
                                                  "Ingenieurwissenschaften") & typ != "Aggregat")%>%
      dplyr::filter(region == states)




  }else {

    df <- df %>% dplyr::filter(typ == "Aggregat"& fach != "Alle Fächer")%>%
      dplyr::filter(region == states)

  }

  # Split dataframe by gender and create plots
  if(abs_rel == "In Prozent"){

    df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")


    #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
    # female
    studierende_faecher_frauen <- df %>%
      dplyr::filter(geschlecht == "Frauen")%>%
      dplyr::arrange(desc(prop))%>%
      dplyr::slice(1:10)

    #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
    # male
    studierende_faecher_maenner <- df %>%
      dplyr::filter(geschlecht == "Männer") %>%
      dplyr::arrange(desc(prop)) %>%
      dplyr::slice(1:10)

    # Create female plot
    hc_frau <- highcharter::hchart(studierende_faecher_frauen, 'bar', highcharter::hcaes(y = prop, x = fach)) %>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, format = "{point.display_rel} %",
                            style = list(textOutline = "none"))
        )) %>%
      highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.display_abs}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0, max = 100, tickInterval = 5) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_colors(c("#154194")) %>%
      highcharter::hc_title(text = paste0("Fächer mit dem höchsten Frauenanteil in ", states , " (", timerange, ")"),
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



    # Create male plot
    hc_mann <- highcharter::hchart(studierende_faecher_maenner, 'bar', highcharter::hcaes(y = prop, x = fach)) %>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, format = "{point.display_rel} %",
                            style = list(textOutline = "none"))
        )) %>%
      highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0, max = 100, tickInterval = 5) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_colors(c("#66cbaf")) %>%
      highcharter::hc_title(text = paste0("Fächer mit dem höchsten Männeranteil in ",states, " (", timerange, ")"),
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


  } else if(abs_rel == "Anzahl"){

    df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")

    #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
    # female
    studierende_faecher_frauen <- df %>%
      dplyr::filter(geschlecht == "Frauen") %>%
      dplyr::arrange(desc(wert)) %>%
      dplyr::slice(1:10)

    # male
    #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
    studierende_faecher_maenner <- df %>%
      dplyr::filter(geschlecht == "Männer") %>%
      dplyr::arrange(desc(wert)) %>%
      dplyr::slice(1:10)


    # Create female plot
    hc_frau <- highcharter::hchart(studierende_faecher_frauen, 'bar', highcharter::hcaes(y = wert, x = fach)) %>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, format = "{point.display_abs}",
                            style = list(textOutline = "none"))
        )) %>%

      highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.display_abs}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}"), min = 0, max = plyr::round_any(max(studierende_faecher_frauen$wert), 1000, f = ceiling), tickInterval = 1000) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_colors(c("#154194")) %>%
      highcharter::hc_title(text = paste0("Am häufigsten gewählte Fächer von Frauen ", "(", timerange, ")"),
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



    # Create male plot
    hc_mann <- highcharter::hchart(studierende_faecher_maenner, 'bar', highcharter::hcaes(y = wert, x = fach)) %>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, format = "{point.display_abs}",
                            style = list(textOutline = "none"))
        )) %>%
      highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.prop} % <br> Absolut: {point.wert}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}"), min = 0, max = plyr::round_any(max(studierende_faecher_maenner$wert), 1000, f = ceiling), tickInterval = 1000) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_colors(c("#66cbaf")) %>%
      highcharter::hc_title(text = paste0("Am häufigsten gewählte Fächer von Männern ", "(", timerange, ")"),
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


  }



  out <- list(hc_frau, hc_mann)

  return(out)

}





# Internationale Studis ----


#' A function to plot a bar chart
#'
#' @description A function to plot a bar chart of proportion of international students
#'
#' @return The return value, if any, from executing the function.
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd


plot_auslaender_mint <- function(r){

  bl_select <- r$states_studium_studienzahl_ausl

  year_select <- r$date_studium_studienzahl_ausl

  absolut_selector <- r$abs_zahlen_studium_studienzahl_ausl

  status_select <- r$status_ausl

  betr_ebene <- r$ebene_ausl


  # df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
  #   dplyr::filter(indikator %in% c("internationale Studienanfänger:innen (1. Hochschulsemester)",
  #                                  "internationale Studierende",
  #                                  "Studienanfänger:innen (1. Hochschulsemester)",
  #                                  "Studierende","Absolvent:innen",
  #                                     "internationale Absolvent:innen"),
  #                 geschlecht == "Gesamt",
  #                 region==bl_select,
  #                 jahr ==year_select )%>%
  #   dplyr::select(-mint_select,- fachbereich)%>%
  #   dplyr::collect() %>%
  #   tidyr::pivot_wider(names_from=indikator, values_from = wert)%>%
  #   dplyr::mutate("deutsche Studierende" =`Studierende` - `internationale Studierende`,
  #                 "deutsche Studienanfänger:innen (1. Hochschulsemester)"=`Studienanfänger:innen (1. Hochschulsemester)`-
  #                   `internationale Studienanfänger:innen (1. Hochschulsemester)`)%>%
  #   dplyr::mutate("deutsche Studierende_p" =`deutsche Studierende`/`Studierende`,
  #                 "internationale Studierende_p"= `internationale Studierende`/`Studierende`,
  #                 "deutsche Studienanfänger:innen (1. Hochschulsemester)_p" =`deutsche Studienanfänger:innen (1. Hochschulsemester)`/`Studienanfänger:innen (1. Hochschulsemester)`,
  #                 "internationale Studienanfänger:innen (1. Hochschulsemester)_p"=`internationale Studienanfänger:innen (1. Hochschulsemester)`/`Studienanfänger:innen (1. Hochschulsemester)`)%>%
  #   dplyr::mutate("deutsche Absolvent:innen" =`Absolvent:innen`-`internationale Absolvent:innen`,
  #                 "internationale Absolvent:innen"=`Absolvent:innen`-`deutsche Absolvent:innen`)%>%
  #   dplyr::mutate("deutsche Absolvent:innen_p" =`deutsche Absolvent:innen`/`Absolvent:innen`,
  #                 "internationale Absolvent:innen_p"= `internationale Absolvent:innen`/`Absolvent:innen`) %>%
  #   dplyr::select(-c(Studierende, `Studienanfänger:innen (1. Hochschulsemester)` ))%>%
  #   tidyr::pivot_longer(c(7:ncol(.)), names_to="indikator", values_to="wert")%>%
  #   dplyr::mutate(selector=dplyr::case_when(stringr::str_ends(.$indikator, "_p")~"Relativ",
  #                                           T~"Asolut"))%>%
  #   dplyr::mutate(selector=dplyr::case_when(stringr::str_ends(.$indikator, "_p") ~ "In Prozent",
  #                                           T ~ "Anzahl"))%>%
  #   dplyr::mutate(ausl_detect=dplyr::case_when(stringr::str_detect(.$indikator, "international")~"International",
  #                                              T~ "Deutsch"))%>%
  #   dplyr::filter(!fach %in% c("Weitere ingenieurwissenschaftliche Fächer",
  #                              "Weitere naturwissenschaftliche und mathematische Fächer",
  #                              "Außerhalb der Studienbereichsgliederung/Sonstige Fächer"))
  #



  df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr = {year_select}
        AND geschlecht = 'Gesamt'
        AND region = {bl_select}
        AND indikator IN ('internationale Studienanfänger:innen (1. Hochschulsemester)','internationale Studierende','Studienanfänger:innen (1. Hochschulsemester)','Studierende','Absolvent:innen','internationale Absolvent:innen')

                                                    ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)

  df <- df %>%
    dplyr::select(-mint_select,- fachbereich)%>%
    dplyr::collect() %>%
    tidyr::pivot_wider(names_from=indikator, values_from = wert)%>%
    dplyr::mutate("deutsche Studierende" =`Studierende` - `internationale Studierende`,
                  "deutsche Studienanfänger:innen (1. Hochschulsemester)"=`Studienanfänger:innen (1. Hochschulsemester)`-
                    `internationale Studienanfänger:innen (1. Hochschulsemester)`)%>%
    dplyr::mutate("deutsche Studierende_p" =`deutsche Studierende`/`Studierende`,
                  "internationale Studierende_p"= `internationale Studierende`/`Studierende`,
                  "deutsche Studienanfänger:innen (1. Hochschulsemester)_p" =`deutsche Studienanfänger:innen (1. Hochschulsemester)`/`Studienanfänger:innen (1. Hochschulsemester)`,
                  "internationale Studienanfänger:innen (1. Hochschulsemester)_p"=`internationale Studienanfänger:innen (1. Hochschulsemester)`/`Studienanfänger:innen (1. Hochschulsemester)`)%>%
    dplyr::mutate("deutsche Absolvent:innen" =`Absolvent:innen`-`internationale Absolvent:innen`,
                  "internationale Absolvent:innen"=`Absolvent:innen`-`deutsche Absolvent:innen`)%>%
    dplyr::mutate("deutsche Absolvent:innen_p" =`deutsche Absolvent:innen`/`Absolvent:innen`,
                  "internationale Absolvent:innen_p"= `internationale Absolvent:innen`/`Absolvent:innen`) %>%
    dplyr::select(-c(Studierende, `Studienanfänger:innen (1. Hochschulsemester)` ))%>%
    tidyr::pivot_longer(c(7:ncol(.)), names_to="indikator", values_to="wert")%>%
    dplyr::mutate(selector=dplyr::case_when(stringr::str_ends(.$indikator, "_p")~"Relativ",
                                            T~"Asolut"))%>%
    dplyr::mutate(selector=dplyr::case_when(stringr::str_ends(.$indikator, "_p") ~ "In Prozent",
                                            T ~ "Anzahl"))%>%
    dplyr::mutate(ausl_detect=dplyr::case_when(stringr::str_detect(.$indikator, "international")~"International",
                                               T~ "Deutsch"))%>%
    dplyr::filter(!fach %in% c("Weitere ingenieurwissenschaftliche Fächer",
                               "Weitere naturwissenschaftliche und mathematische Fächer",
                               "Außerhalb der Studienbereichsgliederung/Sonstige Fächer"))




  df$indikator <- gsub("_p", "", df$indikator)
  df$indikator <- gsub("deutsche ", "", df$indikator)
  df$indikator <- gsub("internationale ", "", df$indikator)
  df$ausl_detect  <- factor(df$ausl_detect, levels=c("Deutsch", "International"))

    df <- df %>%
      dplyr::filter(indikator==status_select)
  # }

  df_fachbereich <- df %>%
    dplyr::filter(fach %in% c("Geisteswissenschaften",
                              "Mathematik, Naturwissenschaften",
                              "Rechts-, Wirtschafts- und Sozialwissenschaften",
                              "Humanmedizin/Gesundheitswissenschaften",
                              "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin",
                              "Sport",
                              "Kunst, Kunstwissenschaft",
                              "Alle Fächer",
                              "Alle MINT-Fächer",
                              "Alle Nicht MINT-Fächer",
                              "Ingenieurwissenschaften (inkl. Informatik)"))


  df_faecher <- df %>%
    dplyr::filter(!fach %in% c("Geisteswissenschaften",
                               "Rechts-, Wirtschafts- und Sozialwissenschaften",
                               "Humanmedizin/Gesundheitswissenschaften",
                               "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin",
                               "Sport",
                               "Kunst, Kunstwissenschaft",
                               "Ingenieurwissenschaften ohne Informatik",
                               "Alle Nicht MINT-Fächer",
                               "Alle MINT-Fächer",
                               "Alle Fächer",
                               "Mathematik, Naturwissenschaften",
                               "Ingenieurwissenschaften (inkl. Informatik)"))
  df_faecher$fach <- as.factor(df_faecher$fach)

  #Faktor für Höhe des Grafens berechnen
  ebene <- c("Fachbereiche", "MINT-Fächer")
  höhe <- c(8, 11)
  plt.add <- data.frame(ebene, höhe)

  # NA aus fach entfernen für BULAs mit weniger Studienfachgruppen
  df_fachbereich <- stats::na.omit(df_fachbereich)
  df_fachbereich <- df_fachbereich %>%
    dplyr::arrange(wert)
  df_faecher <- stats::na.omit(df_faecher)
  df_faecher <- df_faecher %>%
    dplyr::arrange(wert)


  # Vorbereitung Überschrift
  help <- "Studierender"
  help <- ifelse(grepl("anfänger", status_select), "Studienanfänger:innen", help)

  help2 <- "Studierenden"
  help2 <- ifelse(grepl("anfänger", status_select), "Studienanfänger:innen", help2)


  if(betr_ebene=="Fachbereiche"){



    if(absolut_selector=="In Prozent"){


      df_fachbereich <- df_fachbereich %>%
        dplyr::filter(selector == "In Prozent")%>%
        dplyr::mutate(wert = round(wert*100, 1))

      df_fachbereich$display_rel <- prettyNum(df_fachbereich$wert, big.mark = ".", decimal.mark = ",")



      if (status_select == "Absolvent:innen"){


        #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
        out <- highcharter::hchart(df_fachbereich, 'bar', highcharter::hcaes(y = wert, x = fach, group = ausl_detect))%>%
          highcharter::hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anteil: {point.display_rel} %")%>%
          highcharter::hc_size(height = 60*plt.add$höhe[plt.add$ebene == betr_ebene])%>%
          highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
          highcharter::hc_xAxis(title = list(text = ""), categories = c("Alle Fächer",
                                                                        "Alle MINT-Fächer",
                                                                        "Alle Nicht MINT-Fächer",
                                                                        "",
                                                                        "Ingenieurwissenschaften (inkl. Informatik)",
                                                                        "Kunst, Kunstwissenschaft",
                                                                        "Mathematik, Naturwissenschaften",
                                                                        "Geisteswissenschaften",
                                                                        "Rechts-, Wirtschafts- und Sozialwissenschaften",
                                                                        "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin",
                                                                        "Humanmedizin/Gesundheitswissenschaften",
                                                                        "Sport")) %>%
          highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
          highcharter::hc_colors(c("#efe8e6", "#66cbaf")) %>%
          highcharter::hc_title(text = paste0("Anteil internationaler Absolvent:innen an allen Absolvent:innen in ", bl_select,  " (",year_select, ")" ),
                                margin = 45,
                                align = "center",
                                style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
          highcharter::hc_chart(
            style = list(fontFamily = "Calibri Regular", fontSize = "14px")
          ) %>%
          highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
          highcharter::hc_exporting(enabled = TRUE,
                                    buttons = list(
                                      contextButton = list(
                                        menuItems = list("downloadPNG", "downloadCSV")
                                      )
                                    )
          )

      } else {

        #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
        out <- highcharter::hchart(df_fachbereich, 'bar', highcharter::hcaes(y = wert, x = fach, group = ausl_detect))%>%
          highcharter::hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anteil: {point.display_rel} %")%>%
          highcharter::hc_size(height = 60*plt.add$höhe[plt.add$ebene == betr_ebene])%>%
          highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
          highcharter::hc_xAxis(title = list(text = ""), categories = c("Alle Fächer",
                                                                        "Alle MINT-Fächer",
                                                                        "Alle Nicht MINT-Fächer",
                                                                        "",
                                                                        "Ingenieurwissenschaften (inkl. Informatik)",
                                                                        "Kunst, Kunstwissenschaft",
                                                                        "Mathematik, Naturwissenschaften",
                                                                        "Geisteswissenschaften",
                                                                        "Rechts-, Wirtschafts- und Sozialwissenschaften",
                                                                        "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin",
                                                                        "Humanmedizin/Gesundheitswissenschaften",
                                                                        "Sport")) %>%
          highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
          highcharter::hc_colors(c("#efe8e6", "#66cbaf")) %>%
          highcharter::hc_title(text = paste0("Anteil internationaler ", help, " an allen ", help2, " in ", bl_select,  " (",year_select, ")" ),
                                margin = 45,
                                align = "center",
                                style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
          highcharter::hc_chart(
            style = list(fontFamily = "Calibri Regular", fontSize = "14px")
          ) %>%
          highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
          highcharter::hc_exporting(enabled = TRUE,
                                    buttons = list(
                                      contextButton = list(
                                        menuItems = list("downloadPNG", "downloadCSV")
                                      )
                                    )
          )

      }


    } else if(absolut_selector =="Anzahl"){

      hcoptslang <- getOption("highcharter.lang")
      hcoptslang$thousandsSep <- "."
      options(highcharter.lang = hcoptslang)

      df_fachbereich$display_abs <- prettyNum(df_fachbereich$wert, big.mark = ".", decimal.mark = ",")

      df_fachbereich <- df_fachbereich %>%
        dplyr::filter(selector == "Anzahl")


      if (status_select == "Absolvent:innen"){

        #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
        out <- highcharter::hchart(df_fachbereich, 'bar', highcharter::hcaes(y = wert, x = fach, group = ausl_detect))%>%
          highcharter::hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anzahl: {point.display_abs}")%>%
          highcharter::hc_size(height = 60*plt.add$höhe[plt.add$ebene == betr_ebene])%>%
          highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}")) %>%
          highcharter::hc_xAxis(title = list(text = ""), categories = c("Alle Fächer",
                                                                        "Alle MINT-Fächer",
                                                                        "Alle Nicht MINT-Fächer",
                                                                        "",
                                                                        "Ingenieurwissenschaften (inkl. Informatik)",
                                                                        "Rechts-, Wirtschafts- und Sozialwissenschaften",
                                                                        "Mathematik, Naturwissenschaften",
                                                                        "Geisteswissenschaften",
                                                                        "Kunst, Kunstwissenschaft",
                                                                        "Humanmedizin/Gesundheitswissenschaften",
                                                                        "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin",
                                                                        "Sport")) %>%
          highcharter::hc_colors(c("#efe8e6", "#66cbaf")) %>%
          highcharter::hc_title(text = paste0("Anzahl internationaler Absolvent:innen in ", bl_select,  " (",year_select, ")" ),
                                margin = 45,
                                align = "center",
                                style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
          highcharter::hc_chart(
            style = list(fontFamily = "Calibri Regular", fontSize = "14px")
          ) %>%
          highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
          highcharter::hc_exporting(enabled = TRUE,
                                    buttons = list(
                                      contextButton = list(
                                        menuItems = list("downloadPNG", "downloadCSV")
                                      )
                                    )
          )

      }else{

        #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
        out <- highcharter::hchart(df_fachbereich, 'bar', highcharter::hcaes(y = wert, x = fach, group = ausl_detect))%>%
          highcharter::hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anzahl: {point.display_abs}")%>%
          highcharter::hc_size(height = 60*plt.add$höhe[plt.add$ebene == betr_ebene])%>%
          highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}")) %>%
          highcharter::hc_xAxis(title = list(text = ""), categories = c("Alle Fächer",
                                                                        "Alle MINT-Fächer",
                                                                        "Alle Nicht MINT-Fächer",
                                                                        "",
                                                                        "Ingenieurwissenschaften (inkl. Informatik)",
                                                                        "Rechts-, Wirtschafts- und Sozialwissenschaften",
                                                                        "Mathematik, Naturwissenschaften",
                                                                        "Geisteswissenschaften",
                                                                        "Kunst, Kunstwissenschaft",
                                                                        "Humanmedizin/Gesundheitswissenschaften",
                                                                        "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin",
                                                                        "Sport")) %>%
          highcharter::hc_colors(c("#efe8e6", "#66cbaf")) %>%
          highcharter::hc_title(text = paste0("Anzahl internationaler ", help, " in ", bl_select,  " (",year_select, ")" ),
                                margin = 45,
                                align = "center",
                                style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
          highcharter::hc_chart(
            style = list(fontFamily = "Calibri Regular", fontSize = "14px")
          ) %>%
          highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
          highcharter::hc_exporting(enabled = TRUE,
                                    buttons = list(
                                      contextButton = list(
                                        menuItems = list("downloadPNG", "downloadCSV")
                                      )
                                    )
          )

      }



    }} else if(betr_ebene == "MINT-Fächer"){

      if(absolut_selector=="In Prozent"){


        df_faecher <- df_faecher %>%
          dplyr::filter(selector == "In Prozent")%>%
          dplyr::mutate(wert = round(wert*100, 1)) %>%
          dplyr::arrange(wert)

        df_faecher$display_rel <- prettyNum(df_faecher$wert, big.mark = ".", decimal.mark = ",")

       if (status_select == "Absolvent:innen"){


         #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
         out <- highcharter::hchart(df_faecher, 'bar', highcharter::hcaes(y = wert, x = fach, group = ausl_detect))%>%
           highcharter::hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anteil: {point.display_rel} %")%>%
           highcharter::hc_size(height = 60*plt.add$höhe[plt.add$ebene == betr_ebene])%>%
           highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
           highcharter::hc_xAxis(title = list(text = "")
           ) %>%
           highcharter::hc_plotOptions(bar = list(stacking = "percent"))%>%
           highcharter::hc_colors(c("#efe8e6", "#66cbaf")) %>%

           highcharter::hc_title(text = paste0("Anteil internationaler Absolvent:innen an allen Absolvent:innen in ", bl_select,  " (",year_select, ")" ),
                                 margin = 45,
                                 align = "center",
                                 style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
           highcharter::hc_chart(
             style = list(fontFamily = "Calibri Regular", fontSize = "14px")
           ) %>%
           highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
           highcharter::hc_exporting(enabled = TRUE,
                                     buttons = list(
                                       contextButton = list(
                                         menuItems = list("downloadPNG", "downloadCSV")
                                       )
                                     )
           )

       } else {



         #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
         out <- highcharter::hchart(df_faecher, 'bar', highcharter::hcaes(y = wert, x = fach, group = ausl_detect))%>%
           highcharter::hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anteil: {point.display_rel} %")%>%
           highcharter::hc_size(height = 60*plt.add$höhe[plt.add$ebene == betr_ebene])%>%
           highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
           highcharter::hc_xAxis(title = list(text = "")

           ) %>%
           highcharter::hc_plotOptions(bar = list(stacking = "percent"))%>%
           highcharter::hc_colors(c("#efe8e6", "#66cbaf")) %>%

           highcharter::hc_title(text = paste0("Anteil internationaler ", help, " an allen ", help2, " in ", bl_select,  " (",year_select, ")" ),
                                 margin = 45,
                                 align = "center",
                                 style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
           highcharter::hc_chart(
             style = list(fontFamily = "Calibri Regular", fontSize = "14px")
           ) %>%
           highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
           highcharter::hc_exporting(enabled = TRUE,
                                     buttons = list(
                                       contextButton = list(
                                         menuItems = list("downloadPNG", "downloadCSV")
                                       )
                                     )
           )
       }





      } else if(absolut_selector =="Anzahl"){

        hcoptslang <- getOption("highcharter.lang")
        hcoptslang$thousandsSep <- "."
        options(highcharter.lang = hcoptslang)

        df_faecher <- df_faecher %>%
          dplyr::filter(selector == "Anzahl") %>%
          dplyr::arrange(wert)

        df_faecher$display_abs <- prettyNum(df_faecher$wert, big.mark = ".", decimal.mark = ",")

        df_faecher <- df_faecher[order(-df_faecher$wert),]

        if(status_select == "Absolvent:innen"){
          #
          #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
          #

          out <- highcharter::hchart(df_faecher, 'bar', highcharter::hcaes(y = wert, x = fach, group = ausl_detect))%>%
            highcharter::hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anzahl: {point.display_abs}")%>%
            highcharter::hc_size(height = 60*plt.add$höhe[plt.add$ebene == betr_ebene])%>%
            highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}")) %>%
            highcharter::hc_xAxis(title = list(text = "")) %>%
            highcharter::hc_colors(c("#efe8e6", "#66cbaf")) %>%
            highcharter::hc_title(text = paste0("Anzahl internationaler Absolvent:innen in ", bl_select,  " (",year_select, ")" ),
                                  margin = 45,
                                  align = "center",
                                  style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
            highcharter::hc_chart(
              style = list(fontFamily = "Calibri Regular", fontSize = "14px")
            ) %>%
            highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
            highcharter::hc_exporting(enabled = TRUE,
                                      buttons = list(
                                        contextButton = list(
                                          menuItems = list("downloadPNG", "downloadCSV")
                                        )
                                      )
            )

        } else {

          out <- highcharter::hchart(df_faecher, 'bar', highcharter::hcaes(y = wert, x = fach, group = ausl_detect))%>%
            highcharter::hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anzahl: {point.display_abs}")%>%
            highcharter::hc_size(height = 60*plt.add$höhe[plt.add$ebene == betr_ebene])%>%
            highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}")) %>%
            highcharter::hc_xAxis(title = list(text = "")) %>%
            highcharter::hc_colors(c("#efe8e6", "#66cbaf")) %>%
            highcharter::hc_title(text = paste0("Anzahl internationaler ", help, " in ", bl_select,  " (",year_select, ")" ),
                                  margin = 45,
                                  align = "center",
                                  style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
            highcharter::hc_chart(
              style = list(fontFamily = "Calibri Regular", fontSize = "14px")
            ) %>%
            highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
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

### Tab 2 ----

#' A function to plot a bar chart
#'
#' @description A function to plot a bar chart of proportion of international students over time
#'
#' @return The return value, if any, from executing the function.
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# Internationale Studierende im Zeitverlauf

plot_auslaender_mint_zeit <- function(r){
  time <- r$date_ausl_zeit
  t <- time[1]:time[2]
  bl_select <- r$states_studium_studienzahl_ausl_zeit
  absolut_selector <- r$abs_zahlen_studium_studienzahl_ausl_zeit
  status_select <- r$status_ausl_zeit
  betrachtung <- r$ansicht_ausl_zeit

  if(bl_select %in% c("Deutschland",
                      "Baden-Württemberg",
                      "Bayern",
                      "Berlin",
                      "Hamburg",
                      "Hessen",
                      "Nordrhein-Westfalen",
                      "Rheinland-Pfalz",
                      "Sachsen",
                      "Westdeutschland (o. Berlin)",
                      "Ostdeutschland (inkl. Berlin)")) {
    fach_select <- r$fach1_studium_studienzahl_ausl_zeit
  }
  else {
    if(bl_select == "Brandenburg")fach_select <- r$fach2_studium_studienzahl_ausl_zeit
    if(bl_select == "Bremen")fach_select <- r$fach3_studium_studienzahl_ausl_zeit
    if(bl_select == "Mecklenburg-Vorpommern")fach_select <- r$fach4_studium_studienzahl_ausl_zeit
    if(bl_select == "Niedersachsen")fach_select <- r$fach5_studium_studienzahl_ausl_zeit
    if(bl_select == "Saarland")fach_select <- r$fach6_studium_studienzahl_ausl_zeit
    if(bl_select == "Sachsen-Anhalt")fach_select <- r$fach7_studium_studienzahl_ausl_zeit
    if(bl_select == "Schleswig-Holstein")fach_select <- r$fach8_studium_studienzahl_ausl_zeit
    if(bl_select == "Thüringen")fach_select <- r$fach9_studium_studienzahl_ausl_zeit
  }

  # df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
  #   dplyr::filter(  indikator %in% c("internationale Studienanfänger:innen (1. Hochschulsemester)",
  #                                    "internationale Studierende",
  #                                    "Studienanfänger:innen (1. Hochschulsemester)",
  #                                    "Studierende",
  #                                    "Absolvent:innen", "internationale Absolvent:innen"),
  #                 geschlecht == "Gesamt",
  #                 region==bl_select,
  #                 fach ==fach_select,
  #                 jahr %in% t)%>%
  #   dplyr::select(-mint_select,- fachbereich)%>%
  #   dplyr::collect() %>%
  #   tidyr::pivot_wider(names_from=indikator, values_from = wert)%>%
  #   dplyr::mutate("deutsche Studierende" =`Studierende`-`internationale Studierende`,
  #                 "deutsche Studienanfänger:innen (1. Hochschulsemester)"=`Studienanfänger:innen (1. Hochschulsemester)`-
  #                   `internationale Studienanfänger:innen (1. Hochschulsemester)`)%>%
  #   dplyr::mutate("deutsche Studierende_p" =`deutsche Studierende`/Studierende,
  #                 "internationale Studierende_p"= `internationale Studierende`/`Studierende`,
  #                 "deutsche Studienanfänger:innen (1. Hochschulsemester)_p" =`deutsche Studienanfänger:innen (1. Hochschulsemester)`/`Studienanfänger:innen (1. Hochschulsemester)`,
  #                 "internationale Studienanfänger:innen (1. Hochschulsemester)_p"=`internationale Studienanfänger:innen (1. Hochschulsemester)`/`Studienanfänger:innen (1. Hochschulsemester)`)%>%
  #   dplyr::mutate("deutsche Absolvent:innen" =`Absolvent:innen`-`internationale Absolvent:innen`,
  #                 "internationale Absolvent:innen"=`Absolvent:innen`-`deutsche Absolvent:innen`)%>%
  #   dplyr::mutate("deutsche Absolvent:innen_p" =`deutsche Absolvent:innen`/`Absolvent:innen`,
  #                 "internationale Absolvent:innen_p"= `internationale Absolvent:innen`/`Absolvent:innen`) %>%
  #   dplyr::select(-c(`Studierende`, `Studienanfänger:innen (1. Hochschulsemester)` ))%>%
  #   #  dplyr::filter(geschlecht=="Gesamt")%>%
  #   tidyr::pivot_longer(c(7:ncol(.)), names_to="indikator", values_to="wert")%>%
  #   dplyr::mutate(selector=dplyr::case_when(stringr::str_ends(.$indikator, "_p")~"Relativ",
  #                                           T~"Absolut"))%>%
  #   dplyr::mutate(selector=dplyr::case_when(stringr::str_ends(.$indikator, "_p") ~ "In Prozent",
  #                                           T ~ "Anzahl"))%>%
  #   dplyr::mutate(ausl_detect=dplyr::case_when(stringr::str_detect(.$indikator, "international")~"international",
  #                                              T~ "deutsch"))
  #
  #####
  df_query <- glue::glue_sql("
  SELECT *
  FROM studierende_detailliert
  WHERE jahr IN ({t*})
  AND fach = {fach_select}
  AND geschlecht = 'Gesamt'
  AND region = {bl_select}
  AND indikator IN ('internationale Studienanfänger:innen (1. Hochschulsemester)','internationale Studierende','Studienanfänger:innen (1. Hochschulsemester)','Studierende','Absolvent:innen','internationale Absolvent:innen')
      ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)

  df <- df %>%
    dplyr::select(-mint_select,- fachbereich) %>%
    tidyr::pivot_wider(names_from=indikator, values_from = wert)%>%
    dplyr::mutate("deutsche Studierende" =`Studierende`-`internationale Studierende`,
                  "deutsche Studienanfänger:innen (1. Hochschulsemester)"=`Studienanfänger:innen (1. Hochschulsemester)`-
                    `internationale Studienanfänger:innen (1. Hochschulsemester)`)%>%
    dplyr::mutate("deutsche Studierende_p" =`deutsche Studierende`/Studierende,
                  "internationale Studierende_p"= `internationale Studierende`/`Studierende`,
                  "deutsche Studienanfänger:innen (1. Hochschulsemester)_p" =`deutsche Studienanfänger:innen (1. Hochschulsemester)`/`Studienanfänger:innen (1. Hochschulsemester)`,
                  "internationale Studienanfänger:innen (1. Hochschulsemester)_p"=`internationale Studienanfänger:innen (1. Hochschulsemester)`/`Studienanfänger:innen (1. Hochschulsemester)`)%>%
    dplyr::mutate("deutsche Absolvent:innen" =`Absolvent:innen`-`internationale Absolvent:innen`,
                  "internationale Absolvent:innen"=`Absolvent:innen`-`deutsche Absolvent:innen`)%>%
    dplyr::mutate("deutsche Absolvent:innen_p" =`deutsche Absolvent:innen`/`Absolvent:innen`,
                  "internationale Absolvent:innen_p"= `internationale Absolvent:innen`/`Absolvent:innen`) %>%
    dplyr::select(-c(`Studierende`, `Studienanfänger:innen (1. Hochschulsemester)` ))%>%
    #  dplyr::filter(geschlecht=="Gesamt")%>%
    tidyr::pivot_longer(c(7:ncol(.)), names_to="indikator", values_to="wert")%>%
    dplyr::mutate(selector=dplyr::case_when(stringr::str_ends(.$indikator, "_p")~"Relativ",
                                            T~"Absolut"))%>%
    dplyr::mutate(selector=dplyr::case_when(stringr::str_ends(.$indikator, "_p") ~ "In Prozent",
                                            T ~ "Anzahl"))%>%
    dplyr::mutate(ausl_detect=dplyr::case_when(stringr::str_detect(.$indikator, "international")~"international",
                                               T~ "deutsch"))




  df$indikator <- gsub("_p", "", df$indikator)
  df$indikator <- gsub("deutsche ", "", df$indikator)
  df$indikator <- gsub("internationale ", "", df$indikator)
  df$ausl_detect  <- factor(df$ausl_detect, levels=c("deutsch", "international"))



    df <- df %>%
      dplyr::filter(indikator==status_select)
  # }

  # Vorbereitung Überschrift
  help <- "Studierender"
  help <- ifelse(grepl("anfänger", status_select), "Studienanfänger:innen", help)

  help2 <- "Studierenden"
  help2 <- ifelse(grepl("anfänger", status_select), "Studienanfänger:innen", help2)

  fach_help <- fach_select
  fach_help <- ifelse(fach_help == "Alle MINT-Fächer", "MINT", fach_help)

  # Plot
  if(absolut_selector=="In Prozent"){

    df <- df %>%
      dplyr::filter(selector == absolut_selector)%>%
      dplyr::mutate(dplyr::across(wert, ~round(.*100, 1)))

    df$display_rel <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

    if(betrachtung == "Zeitverlauf - Liniendiagramm"){
      if(status_select == "Absolvent:innen"){

        titel <-  paste0("Anteil internationaler Absolvent:innen an allen Absolvent:innen in ", fach_help , " in ", bl_select )
        tooltip <- "{point.ausl_detect} <br> Anteil: {point.display_rel} %"
        format <- "{value} %"
        color <- c("#154194", "#66cbaf")
        out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "ausl_detect", tooltip, format, color)


      } else {

        titel <-  paste0("Anteil internationaler ", help, " an allen ", help2, " in ", fach_help , " in ", bl_select )
        tooltip <- "{point.ausl_detect} <br> Anteil: {point.display_rel} %"
        format <- "{value} %"
        color <- c("#154194", "#66cbaf")
        out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "ausl_detect", tooltip, format, color)
      }

    }else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){

      if (status_select == "Absolvent:innen"){

        df <- df[with(df, order(wert, decreasing = TRUE)), ]############################################


        highcharter::hchart(df, 'column', highcharter::hcaes(y = wert, x = jahr, group = ausl_detect))%>%
          highcharter::hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anteil: {point.display_rel} %")%>%
          highcharter::hc_yAxis(title = list(text = "")
                                , labels = list(format = "{value} %")
          ) %>%
          highcharter::hc_xAxis(title = list(text = "")) %>%
          highcharter::hc_plotOptions(column = list(stacking = "percent")) %>%
          highcharter::hc_plotOptions(column = list(pointWidth = 70))%>%
          highcharter::hc_colors(c("#efe8e6", "#66cbaf")) %>%
          highcharter::hc_yAxis(max = 40)%>%
          highcharter::hc_title(text = paste0("Anteil internationaler Absolvent:innen an allen Absolvent:innen in ", fach_help , " in ", bl_select ),
                                align = "center",
                                style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
          highcharter::hc_chart(
            style = list(fontFamily = "Calibri Regular", fontSize = "14px")
          ) %>%
          highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
          highcharter::hc_exporting(enabled = TRUE,
                                    buttons = list(
                                      contextButton = list(
                                        menuItems = list("downloadPNG", "downloadCSV")
                                      )
                                    ))


      } else {

        df <- df[with(df, order(wert, decreasing = TRUE)), ]##########################################

        highcharter::hchart(df, 'column', highcharter::hcaes(y = wert, x = jahr, group = ausl_detect))%>%
          highcharter::hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anteil: {point.display_rel} %")%>%
          highcharter::hc_yAxis(title = list(text = "")
                                , labels = list(format = "{value} %")
          ) %>%
          highcharter::hc_xAxis(title = list(text = "")) %>%
          highcharter::hc_plotOptions(column = list(stacking = "percent")) %>%
          highcharter::hc_plotOptions(column = list(pointWidth = 70))%>%
          highcharter::hc_colors(c("#efe8e6", "#66cbaf")) %>%
          highcharter::hc_yAxis(max = 40)%>%
          highcharter::hc_title(text = paste0("Anteil internationaler ", help, " an allen ", help2, " in ", fach_help , " in ", bl_select ),
                                align = "center",
                                style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
          highcharter::hc_chart(
            style = list(fontFamily = "Calibri Regular", fontSize = "14px")
          ) %>%
          highcharter::hc_legend(enabled = TRUE, reversed = T)%>%
          highcharter::hc_exporting(enabled = TRUE,
                                    buttons = list(
                                      contextButton = list(
                                        menuItems = list("downloadPNG", "downloadCSV")
                                      )
                                    ))
      }

    }

  } else if(absolut_selector=="Anzahl"){

    df <- df %>%
      dplyr::filter(selector == absolut_selector)

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

    if(betrachtung == "Zeitverlauf - Liniendiagramm"){

      if (status_select == "Absolvent:innen"){

        titel <-    paste0("Anzahl internationaler Absolvent:innen in ", fach_help, " in ", bl_select)
        tooltip <- "{point.ausl_detect} <br> Anzahl: {point.display_abs}"
        format <- "{value:, f}"
        color <- c("#154194", "#66cbaf")
        out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "ausl_detect", tooltip, format, color)

      } else {

        titel <-  paste0("Anzahl internationaler ", help, " in ", fach_help, " in ", bl_select)
        tooltip <- "{point.ausl_detect} <br> Anzahl: {point.display_abs}"
        format <- "{value:, f}"
        color <- c("#154194", "#66cbaf")
        out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "ausl_detect", tooltip, format, color)
      }

    }else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){

      if (status_select == "Absolvent:innen"){

        df <- df[with(df, order(wert, decreasing = TRUE)), ]##########################################

        highcharter::hchart(df, 'column', highcharter::hcaes(y = wert, x = jahr, group = ausl_detect))%>%
          highcharter::hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anzahl: {point.display_abs}")%>%
          # highcharter::hc_size(height = 1000)%>%
          highcharter::hc_yAxis(title = list(text = "")
                                , labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular")
          ) %>%
          highcharter::hc_xAxis(title = list(text = "")) %>%
          #highcharter::hc_plotOptions(column = list(stacking = "percent")) %>%
          highcharter::hc_colors(c("#efe8e6", "#66cbaf")) %>%
          highcharter::hc_title(text =  paste0("Anzahl internationaler Absolvent:innen in ", fach_help, " in ", bl_select),
                                margin = 45,
                                align = "center",
                                style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
          highcharter::hc_chart(
            style = list(fontFamily = "Calibri Regular", fontSize = "14px")
          ) %>%
          highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
          highcharter::hc_exporting(enabled = TRUE,
                                    buttons = list(
                                      contextButton = list(
                                        menuItems = list("downloadPNG", "downloadCSV")
                                      )
                                    ))

      } else {

        df <- df[with(df, order(wert, decreasing = TRUE)), ]##########################################

        highcharter::hchart(df, 'column', highcharter::hcaes(y = wert, x = jahr, group = ausl_detect))%>%
          highcharter::hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anzahl: {point.display_abs}")%>%
          highcharter::hc_yAxis(title = list(text = "")
                                , labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular")
          ) %>%
          highcharter::hc_xAxis(title = list(text = "")) %>%
          highcharter::hc_colors(c("#efe8e6", "#66cbaf")) %>%
          highcharter::hc_title(text =  paste0("Anzahl internationaler ", help, " in ", fach_help, " in ", bl_select),
                                margin = 45,
                                align = "center",
                                style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
          highcharter::hc_chart(
            style = list(fontFamily = "Calibri Regular", fontSize = "14px")
          ) %>%
          highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
          highcharter::hc_exporting(enabled = TRUE,
                                    buttons = list(
                                      contextButton = list(
                                        menuItems = list("downloadPNG", "downloadCSV")
                                      )
                                    ))

      }
    }

  }



}












### Tab 3 ----
#' A function to plot the german map for internationals
#'
#' @description A function to plot the german map with all states that contain
#' information about the share of internationals
#'
#' @return The return value is the german map with information
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studierende_international_bula_mint <- function(r) {

  # load UI inputs from reactive value
  betrachtung <- r$ansicht_studium_international_bulas

  if(betrachtung == "Übersicht - Kartendiagramm"){

    #UI nach Betrachtung
    timerange <- r$international_bulas_map_y
    label_m <- r$international_bulas_map_l

    # df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    #   dplyr::filter(jahr == timerange,
    #                 region != "Deutschland",
    #                 fach %in% c("Alle MINT-Fächer", "Alle Fächer"),
    #                 indikator == label_m,
    #                 geschlecht == "Gesamt") %>%
    #   dplyr::collect()



    df_query <- glue::glue_sql("
      SELECT *
      FROM studierende_detailliert
      WHERE jahr = {timerange}
      AND fach IN ('Alle MINT-Fächer','Alle Fächer')
      AND region != 'Deutschland'
      AND geschlecht = 'Gesamt'
      AND indikator = {label_m}
      ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)




    df <- df %>%
      dplyr::select(-fachbereich,- mint_select, -typ )%>%
      tidyr::pivot_wider(names_from = fach, values_from = wert)%>%
      dplyr::mutate(dplyr::across(c(6:ncol(.)), ~round(./`Alle Fächer`*100,1)))%>%
      tidyr::pivot_longer(c(6:ncol(.)), values_to = "proportion", names_to ="fach")%>%
      dplyr::right_join(df) %>%
      dplyr::filter(fach != "Alle Fächer")

    df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$display_rel <- prettyNum(df$proportion, big.mark = ".", decimal.mark = ",")



    label_m <- ifelse(label_m == "Studierende", paste0(label_m, "n"), label_m)
    label_m <- ifelse(label_m == "internationale Studierende", "internationalen Studierenden", label_m)
    label_m <- ifelse(grepl("Lehram", label_m), "Studierenden (Lehramt)", label_m)
    label_m <- ifelse(label_m == "internationale Studienanfänger:innen (1. Hochschulsemester)",
                      "internationalen Studienanfänger:innen (1. Hochschulsemester)", label_m)

    help_l <- label_m
    help_l <- ifelse(label_m == "internationalen Studienanfänger:innen (1. Hochschulsemester)",
                     "internationalen Studienanfänger:innen", help_l)
    help_l <- ifelse(label_m == "Studienanfänger:innen (1. Hochschulsemester)", "Studienanfänger:innen", help_l)


    # plot


    df <- df[df$fachbereich == "MINT",]
    joinby <- c("name", "region")
    name <- paste0(label_m, " in MINT")
    tooltip <- "{point.region} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.display_abs}"
    titel <- paste0("MINT-Anteil von ", label_m, " (", timerange, ")")
    mincolor <- "#f4f5f6"
    map_selection <- 1
    maxcolor <- "#b16fab"

    out <- mapbuilder(df, joinby,name, tooltip, titel, mincolor, maxcolor,prop=FALSE, wert=FALSE, map=map_selection)



  }

  else if(betrachtung == "Zeitverlauf - Liniendiagramm"){###hier weiter

    # load UI inputs from reactive value
    timerange <- r$international_bulas_verlauf_y
    t <- (timerange[1]:timerange[2])
    absolut_selector <- r$international_bulas_verlauf_abs_rel
    bl_label <- r$international_bulas_verlauf_l
    states <- r$international_bulas_verlauf_regio


    df_query <- glue::glue_sql("
      SELECT fach, jahr, indikator, region, wert
      FROM studierende_detailliert
      WHERE jahr in ({t*})
      AND fach = 'Alle MINT-Fächer'
      AND region IN ({states*})
      AND indikator = {bl_label}
      AND geschlecht = 'Gesamt'
      ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)


    # Vorbereitung Überschrift

    label <- ifelse(bl_label == "Studierende", paste0(bl_label, "n"), bl_label)
    label <- ifelse(label == "internationale Studierende", "internationalen Studierenden", label)
    label <- ifelse(grepl("Lehram", label), "Studierenden (Lehramt)", label)
    label <- ifelse(label == "internationale Studienanfänger:innen (1. Hochschulsemester)",
                    "internationalen Studienanfänger:innen (1. Hochschulsemester)", label)

    help_l <- label
    help_l <- ifelse(label == "internationalen Studienanfänger:innen (1. Hochschulsemester)",
                     "internationalen Studienanfänger:innen", help_l)
    help_l <- ifelse(label == "Studienanfänger:innen (1. Hochschulsemester)", "Studienanfänger:innen", help_l)

    # Plot

    if (absolut_selector=="In Prozent"){


      df_query <- glue::glue_sql("
      SELECT fach, jahr, indikator, region, wert AS wert_ges
      FROM studierende_detailliert
      WHERE jahr in ({t*})
      AND fach = 'Alle Fächer'
      AND region IN ({states*})
      AND indikator = {bl_label}
      AND geschlecht = 'Gesamt'
      ", .con = con)

      alle <- DBI::dbGetQuery(con, df_query)




      df <- df %>% dplyr::left_join(alle, by = c( "jahr", "indikator", "region")) %>%
        dplyr::rename(fach = fach.x) %>%
        dplyr::mutate(prop = round(wert/wert_ges*100,1)) %>%
        dplyr::select(-fach.y)
      #


      df_start <- df %>%
        dplyr::filter(jahr == timerange[1]) %>%
        dplyr::select(region, prop) %>%
        dplyr::rename(prop_alt =prop)
      df_ende <- df %>%
        dplyr::filter(jahr == timerange[2]) %>%
        dplyr::select(region, prop) %>%
        dplyr::rename(prop_neu =prop)
      df <- df %>%
        dplyr::left_join(df_start, by = c("region")) %>%
        dplyr::left_join(df_ende, by = c("region")) %>%
        dplyr::mutate(diff = round(((prop_neu - prop_alt)/prop_alt)*100,1))


      df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
      df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")
      df$display_diff <- prettyNum(df$diff, big.mark = ".", decimal.mark = ",")
      df$display_diff <- ifelse(df$diff < 0, paste0("-", df$display_diff), paste0("+", df$display_diff))


      titel <-  paste0("MINT-Anteil von ", label, " im Zeitverlauf")
      tooltip <- paste0("{point.region} <br> Wert: {point.display_rel} % <br>Veränderung zwischen ", timerange[1],
                        " und ", timerange[2], ": {point.display_diff} %")
      format <- "{value}%"
      color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                 "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
      out <- linebuilder(df, titel, x = "jahr", y = "prop", group = "region", tooltip, format, color)



    } else if(absolut_selector=="Anzahl"){


      hcoptslang <- getOption("highcharter.lang")
      hcoptslang$thousandsSep <- "."
      options(highcharter.lang = hcoptslang)

      df_start <- df %>%
        dplyr::filter(jahr == timerange[1]) %>%
        dplyr::select(region, wert) %>%
        dplyr::rename(wert_alt =wert)
      df_ende <- df %>%
        dplyr::filter(jahr == timerange[2]) %>%
        dplyr::select(region, wert) %>%
        dplyr::rename(wert_neu =wert)
      df <- df %>%
        dplyr::left_join(df_start, by = c("region")) %>%
        dplyr::left_join(df_ende, by = c("region")) %>%
        dplyr::mutate(diff = round(((wert_neu - wert_alt)/wert_alt)*100,1))

      df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
      df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
      df$display_diff <- prettyNum(df$diff, big.mark = ".", decimal.mark = ",")
      df$display_diff <- ifelse(df$diff < 0, paste0("-", df$display_diff), paste0("+", df$display_diff))



      titel <-paste0("Anzahl der ", label, " in MINT im Zeitverlauf")
      tooltip <- paste0("{point.region} <br> Wert: {point.display_abs} <br>Veränderung zwischen ", timerange[1],
                        " und ", timerange[2], ": {point.display_diff} %")
      format <-  "{value:, f}"
      color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                 "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
      out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color)



    }

  }

  else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){##

    timerange <- r$international_bulas_balken_date
    r_lab1 <- r$international_bulas_balken_l

    # df_ges <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    #   dplyr::filter(geschlecht=="Gesamt",
    #                 jahr %in% timerange,
    #                 fach %in% c("Alle MINT-Fächer", "Alle Fächer"),
    #                 indikator == r_lab1) %>%
    #   dplyr::collect()



    df_query <- glue::glue_sql("
      SELECT *
      FROM studierende_detailliert
      WHERE jahr in ({timerange*})
      AND fach IN ('Alle MINT-Fächer','Alle Fächer')
      AND indikator = {r_lab1}
      AND geschlecht = 'Gesamt'
      ", .con = con)

    df_ges <- DBI::dbGetQuery(con, df_query)




    # df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    #   dplyr::filter(geschlecht=="Gesamt",
    #                 jahr %in% timerange,
    #                 fach %in% c("Alle MINT-Fächer", "Alle Fächer"),
    #                 indikator == r_lab1) %>%
    #   dplyr::select(-fachbereich,- mint_select, -typ )%>%
    #   dplyr::collect() %>%
    #   tidyr::pivot_wider(names_from = fach, values_from = wert)%>%
    #   dplyr::mutate(dplyr::across(c(6:ncol(.)), ~round(./`Alle Fächer`*100,1)))%>%
    #   tidyr::pivot_longer(c(6:ncol(.)), values_to = "proportion", names_to ="fach")%>%
    #   dplyr::right_join(df_ges)%>%
    #   dplyr::filter(fach == "Alle MINT-Fächer")



    df_query <- glue::glue_sql("
      SELECT *
      FROM studierende_detailliert
      WHERE jahr in ({timerange*})
      AND fach IN ('Alle MINT-Fächer','Alle Fächer')
      AND indikator = {r_lab1}
      AND geschlecht = 'Gesamt'
      ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::select(-fachbereich,- mint_select, -typ ) %>%
      tidyr::pivot_wider(names_from = fach, values_from = wert)%>%
      dplyr::mutate(dplyr::across(c(6:ncol(.)), ~round(./`Alle Fächer`*100,1)))%>%
      tidyr::pivot_longer(c(6:ncol(.)), values_to = "proportion", names_to ="fach")%>%
      dplyr::right_join(df_ges)%>%
      dplyr::filter(fach == "Alle MINT-Fächer")



    df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

    df <- df %>%
      dplyr::select(indikator, region, jahr, fach, proportion, wert)

    df <- stats::na.omit(df)

    df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$display_rel <- prettyNum(df$proportion, big.mark = ".", decimal.mark = ",")

    df <- df[with(df, order(proportion, decreasing = TRUE)),]



    # Vorbereitung Überschrift
    r_lab1 <- ifelse(r_lab1 == "Studierende", paste0(r_lab1, "n"), r_lab1)
    r_lab1 <- ifelse(r_lab1 == "internationale Studierende", "internationalen Studierenden", r_lab1)
    r_lab1 <- ifelse(grepl("Lehram", r_lab1), "Studierenden (Lehramt)", r_lab1)
    r_lab1 <- ifelse(r_lab1 == "internationale Studienanfänger:innen (1. Hochschulsemester)",
                     "internationalen Studienanfänger:innen (1. Hochschulsemester)", r_lab1)

    help_l <- r_lab1
    help_l <- ifelse(r_lab1 == "internationalen Studienanfänger:innen (1. Hochschulsemester)",
                     "internationalen Studienanfänger:innen", help_l)
    help_l <- ifelse(r_lab1 == "Studienanfänger:innen (1. Hochschulsemester)", "Studienanfänger:innen", help_l)

    #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
    # Plot
    out <- highcharter::hchart(df, 'bar', highcharter::hcaes(x= region, y = proportion))%>%
      highcharter::hc_tooltip(pointFormat = "{point.fach} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.display_abs}") %>% #Inhalt für Hover-Box
      highcharter::hc_yAxis(title = list(text=""), labels = list(format = "{value}%")) %>% #x-Achse -->Werte in %
      highcharter::hc_xAxis(title= list(text="")) %>% #Y-Achse - keine Beschriftung
      #Anpassung der Farben
      highcharter::hc_plotOptions(bar = list(
        colorByPoint = TRUE,
        colors = ifelse(df$region == "Deutschland", "#b16fab",
                        ifelse(df$region == "Ostdeutschland (inkl. Berlin)", "#d3a4d7",
                               ifelse(df$region == "Westdeutschland (o. Berlin)", "#d3a4d7", "#A9A9A9"))))) %>%
      highcharter::hc_title(text = paste0( "MINT-Anteil unter den ", r_lab1 , " (", timerange, ")"),
                            margin = 25,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px"))   %>%
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











