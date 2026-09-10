# Wer wählt MINT ----
### Tab 1 ----


map_selection_germany <- readRDS("data/map_data/map_selection_german.rds")
map_selection_europe <- readRDS("data/map_data/map_selection_europa.rds")
map_selection_international <- readRDS("data/map_data/map_selection_international.rds")


#
#

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
    darstellung <-  r$abs_zahlen_arbeitsmarkt_einstieg_vergleich123

    praep <- ifelse(regio == "Saarland", " im ", " in ")

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


    df <- df %>%
      dplyr::left_join(alle, by = c("region", "jahr", "indikator")) %>%
      dplyr::rename(fach = fach.x) %>%
      dplyr::mutate(proportion = round(wert / wert_ges * 100, 1)) %>%
      dplyr::select(-fach.y)

    df <- df %>%
      dplyr::mutate(
        wert_label = prettyNum(wert, big.mark = ".", decimal.mark = ","),
        display_rel = prettyNum(proportion, big.mark = ".", decimal.mark = ",")
      )
    # df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    # df$display_rel <- prettyNum(df$proportion, big.mark = ".", decimal.mark = ",")
    df <- within(df, fach <- factor(fach, levels = c("Alle Nicht MINT-Fächer", "Alle MINT-Fächer")))

    if(betrachtung == "Einzelansicht - Kuchendiagramm"){

        if(length(testl1) == 1) {

          df_pie <- df %>% dplyr::filter(indikator == testl1)
          df_pie <- df_pie[with(df_pie, order(proportion, decreasing = FALSE)),]


          titel <- paste0(testl1[1], " ", praep," ", regio, " (", testy1, ")")

          quelle <- "Quelle: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

          df_pie <- df_pie %>%
            dplyr::mutate(
              tooltip = paste0(
                "<b>", fach, "</b><br>",
                "Anteil: ", display_rel, " %<br>",
                "Anzahl: ", wert
              )
            )

          out <- piebuilder_plotly(df_pie, titel, x = "fach", y = "proportion",
                            color =  c("#b16fab", "#efe8e6"), quelle=quelle)


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

          df_1_pie <- df_1_pie %>%
            dplyr::mutate(
              tooltip = paste0(
                "<b>", fach, "</b><br>",
                "Anteil: ", display_rel, " %<br>",
                "Anzahl: ", wert
              )
            )
          df_2_pie <- df_2_pie %>%
            dplyr::mutate(
              tooltip = paste0(
                "<b>", fach, "</b><br>",
                "Anteil: ", display_rel, " %<br>",
                "Anzahl: ", wert
              )
            )

          quelle <- "Quelle: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

          p1 <- piebuilder_plotly(df_1_pie, titel,x = "fach", y = "proportion",
                                  color = c("#efe8e6", "#b16fab"), quelle = quelle)

          p2 <- piebuilder_plotly(df_2_pie, titel2,x = "fach", y = "proportion",
                                  color = c("#efe8e6", "#b16fab"), quelle = quelle)

          out <- list(p1, p2)
        }

    }
       else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){

         df <- df %>% dplyr::filter(indikator %in% testl1)
         df <- df[with(df, order(proportion, decreasing = TRUE)), ]


         if(darstellung == "In Prozent"){


         titel <-  paste0("MINT-Anteil in verschiedenen Studierenden-Gruppen", praep, regio, " (", testy1, ")")

         order <- df %>%
           dplyr::filter(fach == "Alle MINT-Fächer") %>%
           dplyr::arrange(dplyr::desc(proportion)) %>%
           dplyr::pull(indikator) %>%
           rev()

         df <- df %>%
           dplyr::mutate(
             .tooltip = paste0(
               "<b><span style='font-size:15px;'>", indikator, "</span></b><br>",
               "<span style='font-size:15px;'>",fach, "</span><br>",
               "Anteil: ", round(proportion, 1), " %<br>",
               "Anzahl: ", wert_label
             ))


         x <- "indikator"
         y <- "proportion"
         group <- "fach"
         color <- c("#b16fab", "#efe8e6")
         quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
         quelle_y <- -0.20


         out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h",percent=TRUE, group=group, color=color,
                                     order=order, stacking = TRUE,quelle_y=quelle_y, quelle=quelle)



         } else {


           titel <-  paste0("MINT-Anteil in verschiedenen Studierenden-Gruppen", praep, regio, " (", testy1, ")")

           order <- rev(unique(df$indikator))

           df <- df %>%
             dplyr::mutate(
               .tooltip = paste0(
                 "<b><span style='font-size:15px;'>", indikator, "</span></b><br>",
                 "<span style='font-size:15px;'>",fach, "</span><br>",
                 "Anteil: ", round(proportion, 1), " %<br>",
                 "Anzahl: ", wert_label
               ))


           x <- "indikator"
           y <- "wert"
           group <- "fach"
           color <- c("#b16fab", "#efe8e6")
           quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
           quelle_y <- -0.25


           out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h",percent=FALSE, group=group, color=color,
                                       order=order, stacking=FALSE,quelle_y=quelle_y, quelle=quelle)


         }
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


    df <- df %>% dplyr::left_join(alle, by = c( "jahr", "indikator")) %>%
      dplyr::rename(fach = fach.x) %>%
      dplyr::mutate(proportion = round(wert/wert_ges*100,1)) %>%
      dplyr::select(-fach.y)


    df <- df[with(df, order( jahr, decreasing = FALSE)), ]

    titel <- ifelse(regio == "Saarland",
                    paste0("MINT-Anteil in verschiedenen Studierenden-Gruppen im ", regio),
                    paste0("MINT-Anteil in verschiedenen Studierenden-Gruppen in ", regio))
    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", indikator, "</b><br>",
          "Jahr: ", jahr, "<br>",
          "Anteil: ", prettyNum(proportion, big.mark = ".", decimal.mark = ","), " %"
        )
      )
    color <- c("#b16fab", "#154194", "#66cbaf", "#fbbf24", "#AFF3E0", "#2D6BE1", "#008F68", "#8893a7", "#ee7775", "#9d7265", "#35bd97",
               "#bfc6d3", "#5f94f9", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")[1:length(unique(df$indikator))]

    quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    out <- linebuilder_plotly(df, titel, x = "jahr", y = "proportion", group = "indikator", color = color, quelle = quelle)



  }else if (abs_zahlen_selector == "Anzahl"){

    df <- df[with(df, order( jahr, decreasing = FALSE)), ]

    titel <- ifelse(regio == "Saarland",
                    paste0("Anzahl an Studierenden in MINT im ", regio),
                    paste0("Anzahl an Studierenden in MINT in ", regio))
    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", indikator, "</b><br>",
          "Jahr: ", jahr, "<br>",
          "Anzahl: ", prettyNum(wert, big.mark = ".", decimal.mark = ",")
        )
      )
    format <- ",d"
    quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt"
    color <- c("#b16fab", "#154194", "#66cbaf", "#fbbf24", "#AFF3E0", "#2D6BE1", "#008F68", "#8893a7", "#ee7775", "#9d7265", "#35bd97",
               "#bfc6d3", "#5f94f9", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")[1:length(unique(df$indikator))]

    out <- linebuilder_plotly(df, titel, x = "jahr", y = "wert", group = "indikator", format = format, color = color, quelle = quelle)


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


    #praep <- ifelse(region == "Saarland", " im ", " in ")

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
    label_m <- ifelse(label_m == "Studierende (Lehramt)", "Studierenden im Lehramt", label_m)
    label_m <- ifelse(label_m == "Absolvent:innen (Lehramt)", "Lehramts-Absolvent:innen", label_m)
    label_m <- ifelse(label_m == "internationale Studienanfänger:innen (1. Hochschulsemester)",
                      "internationalen Studienanfänger:innen (1. Hochschulsemester)", label_m)
    help_l <- label_m
    help_l <- ifelse(label_m == "internationalen Studienanfänger:innen (1. Hochschulsemester)",
                     "internationalen Studienanfänger:innen", help_l)
    help_l <- ifelse(label_m == "Studienanfänger:innen (1. Hochschulsemester)", "Studienanfänger:innen", help_l)


    # plot
    df <- df[df$fachbereich == "MINT",]
    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", region, "</b><br>",
          "Anteil: ", display_rel, " %<br>",
          "Anzahl: ", display_abs
        )
      )
    titel <- paste0("MINT-Anteil von ", label_m, " (", timerange, ")")
    quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    out <- mapbuilder_plotly(df, titel = titel, value_col = "proportion", quelle = quelle)


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
    label <- ifelse(label == "Studierende (Lehramt)", "Studierenden im Lehramt", label)
    label <- ifelse(label == "Absolvent:innen (Lehramt)", "Lehramts-Absolvent:innen", label)
    label <- ifelse(label == "internationale Studienanfänger:innen (1. Hochschulsemester)",
                      "internationalen Studienanfänger:innen (1. Hochschulsemester)", label)

    help_l <- label
    help_l <- ifelse(label == "internationalen Studienanfänger:innen (1. Hochschulsemester)",
                     "internationalen Studienanfänger:innen", help_l)
    help_l <- ifelse(label == "Studienanfänger:innen (1. Hochschulsemester)", "Studienanfänger:innen", help_l)

    # Plot

    if (absolut_selector=="In Prozent"){


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

      sorted_indicators <- df %>%
        dplyr::group_by(region) %>%
        dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
        dplyr::arrange(m_value) %>%
        dplyr::pull(region)

      df$region <- factor(df$region, levels = sorted_indicators)

      df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

      titel <- paste0("Anteil von ", label, " in MINT-Fächern an allen ", help_l)
      df <- df %>%
        dplyr::mutate(
          tooltip = paste0(
            "<b>", region, "</b><br>",
            "Jahr: ", jahr, "<br>",
            "Anteil: ", prettyNum(prop, big.mark = ".", decimal.mark = ","), " %"
          )
        )
      color <- c("#b16fab", "#154194", "#66cbaf", "#fbbf24", "#AFF3E0", "#2D6BE1", "#008F68", "#8893a7", "#ee7775", "#9d7265", "#35bd97",
                 "#bfc6d3", "#5f94f9", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")[1:length(unique(df$region))]
      quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt"
      out <- linebuilder_plotly(df, titel, x = "jahr", y = "prop", group = "region", color = color, quelle = quelle)

    } else if(absolut_selector=="Anzahl"){

      df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

      titel <- paste0("Anzahl an ", label, " in MINT-Fächern ")
      df <- df %>%
        dplyr::mutate(
          tooltip = paste0(
            "<b>", region, "</b><br>",
            "Jahr: ", jahr, "<br>",
            "Anzahl: ", prettyNum(wert, big.mark = ".", decimal.mark = ",")
          )
        )
      format <- ",d"
      color <- c("#b16fab", "#154194", "#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                 "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")[1:length(unique(df$region))]
      quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt"
      out <- linebuilder_plotly(df, titel, x = "jahr", y = "wert", group = "region", format = format, color = color, quelle = quelle)
    }

  }

  else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){

    timerange <- r$bulas_balken_date
    r_lab1 <- r$bulas_balken_l
    darstellung <- r$abs_zahlen_arbeitsmarkt_einstieg_vergleich_der


    df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr IN ({timerange*})
        AND geschlecht = 'Gesamt'
        AND indikator  = {r_lab1}
        AND fach IN ('Alle MINT-Fächer','Alle Fächer')
                               ", .con = con)

    df_ges <- DBI::dbGetQuery(con, df_query)


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
    r_lab1 <- ifelse(r_lab1 == "Studierende (Lehramt)", "Studierenden im Lehramt", r_lab1)
    r_lab1 <- ifelse(r_lab1 == "Absolvent:innen (Lehramt)", "Lehramts-Absolvent:innen", r_lab1)
    r_lab1 <- ifelse(r_lab1 == "internationale Absolvent:innen", "internationalen Absolvent:innen", r_lab1)
    r_lab1 <- ifelse(r_lab1 == "internationale Studienanfänger:innen (1. Hochschulsemester)",
                    "internationalen Studienanfänger:innen (1. Hochschulsemester)", r_lab1)

    help_l <- r_lab1
    help_l <- ifelse(r_lab1 == "internationalen Studienanfänger:innen (1. Hochschulsemester)",
                    "internationalen Studienanfänger:innen", help_l)
    help_l <- ifelse(r_lab1 == "Studienanfänger:innen (1. Hochschulsemester)", "Studienanfänger:innen", help_l)



     titel <- if (r_lab1 %in% c(
      "internationalen Studienanfänger:innen (1. Hochschulsemester)",
      "Studienanfänger:innen (1. Hochschulsemester)",
      "internationalen Studierenden",
      "internationalen Absolvent:innen")) {
      paste0( "Anteil von ", r_lab1," in MINT-Fächern an allen ", help_l," (", timerange, ")")
    } else {
      paste0("Anteil von ", r_lab1," in MINT-Fächern an allen ", help_l," (", timerange, ")")}


    df <- df[with(df, order(proportion, decreasing = TRUE)),]


  if(darstellung == "In Prozent"){


    order <- unique(df$region)

    df <- df %>%
      dplyr::mutate(
        .tooltip = paste0(
          "<b><span style='font-size:15px;'>", region, "</span></b><br>",
          "<span style='font-size:15px;'>","Alle MINT-Fächer", "</span><br>",
          "Anteil: ", round(proportion, 1), " %<br>",
          "Anzahl: ", wert
        ))


    x <- "region"
    y <- "proportion"
    color <- c(
      "Deutschland" = "#b16fab",
      "Ostdeutschland (inkl. Berlin)" = "#d3a4d7",
      "Westdeutschland (o. Berlin)" = "#d3a4d7",
      "Baden-Württemberg" = "#A9A9A9",
      "Bayern" = "#A9A9A9",
      "Berlin" = "#A9A9A9",
      "Brandenburg" = "#A9A9A9",
      "Bremen" = "#A9A9A9",
      "Hamburg" = "#A9A9A9",
      "Hessen" = "#A9A9A9",
      "Mecklenburg-Vorpommern" = "#A9A9A9",
      "Niedersachsen" = "#A9A9A9",
      "Nordrhein-Westfalen" = "#A9A9A9",
      "Rheinland-Pfalz" = "#A9A9A9",
      "Saarland" = "#A9A9A9",
      "Sachsen" = "#A9A9A9",
      "Sachsen-Anhalt" = "#A9A9A9",
      "Schleswig-Holstein" = "#A9A9A9",
      "Thüringen" = "#A9A9A9"
    )

    quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    quelle_y <- -0.10


    out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h",percent=TRUE, color=color,
                                order=order, stacking=FALSE, quelle_y=quelle_y, quelle=quelle)%>%
      plotly::layout(
        yaxis = list(dtick = 1),
        margin = list(t = 80, b = 100))


  } else {


    df$wert <- as.numeric(gsub("\\.", "", df$wert))
    format <- "{value}"


    df <- df %>%
      dplyr::mutate(
        wert_label = prettyNum(wert, big.mark = ".", decimal.mark = ","),
        .tooltip = paste0(
          "<b><span style='font-size:15px;'>", region, "</span></b><br>",
          "<span style='font-size:15px;'>","Alle MINT-Fächer", "</span><br>",
          "Anteil: ", round(proportion, 1), " %<br>",
          "Anzahl: ", wert_label
        ))


    x <- "region"
    y <- "wert"

    color <- c(
      "Deutschland" = "#b16fab",
      "Ostdeutschland (inkl. Berlin)" = "#d3a4d7",
      "Westdeutschland (o. Berlin)" = "#d3a4d7",
      "Baden-Württemberg" = "#A9A9A9",
      "Bayern" = "#A9A9A9",
      "Berlin" = "#A9A9A9",
      "Brandenburg" = "#A9A9A9",
      "Bremen" = "#A9A9A9",
      "Hamburg" = "#A9A9A9",
      "Hessen" = "#A9A9A9",
      "Mecklenburg-Vorpommern" = "#A9A9A9",
      "Niedersachsen" = "#A9A9A9",
      "Nordrhein-Westfalen" = "#A9A9A9",
      "Rheinland-Pfalz" = "#A9A9A9",
      "Saarland" = "#A9A9A9",
      "Sachsen" = "#A9A9A9",
      "Sachsen-Anhalt" = "#A9A9A9",
      "Schleswig-Holstein" = "#A9A9A9",
      "Thüringen" = "#A9A9A9"
    )

    quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    quelle_y <- -0.10


    out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h",percent=FALSE, color=color,
                                stacking=FALSE,quelle_y=quelle_y, quelle=quelle)%>%
      plotly::layout(
        yaxis = list(dtick = 1),
        margin = list(t = 80, b = 100))


  }
  }


  return(out)

}

### Nicht Box 1 ----

#'
#'
#'
#' #' A function to plot time series
#' #'
#' #' @description A function to plot the time series of the german states
#' #'
#' #' @return The return value, if any, from executing the function.
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' studienzahl_verlauf_bl_subject <- function(r) {
#'
#'
#'   absolut_selector <- r$abs_zahlen_verlauf_subject_bl
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_verlauf_subject_bl
#'   t <- as.character(timerange[1]:timerange[2])
#'
#'   states <- r$states_verlauf_subject_bl
#'
#'   label_select <- r$verl_l
#'
#'   #
#'
#'   df_query <- glue::glue_sql("
#'         SELECT *
#'         FROM studierende
#'         WHERE jahr in ({t*})
#'         AND region = {states}
#'         AND indikator = {label_select}
#'         AND geschlecht = 'Gesamt'
#'                                ", .con = con)
#'
#'   df <- DBI::dbGetQuery(con, df_query)
#'
#'
#'
#'   df <- df %>%
#'     tidyr::pivot_wider(names_from=fachbereich, values_from = wert)%>%
#'     #dplyr::rename("MINT (gesamt)" = MINT)%>%
#'     dplyr::mutate("MINT (Gesamt)_p"= `MINT (Gesamt)`/Alle)%>%
#'     dplyr::mutate(Ingenieurwissenschaften_p=Ingenieurwissenschaften/Alle)%>%
#'     dplyr::mutate("Mathematik, Naturwissenschaften_p"=`Mathematik, Naturwissenschaften`/Alle)%>%
#'     dplyr::select(-Alle,- `Nicht MINT`,- geschlecht)%>%
#'     tidyr::pivot_longer(c(4:9), names_to ="var", values_to = "wert")%>%
#'     dplyr::mutate(selector=dplyr::case_when(stringr::str_ends(.$var, "p")~"In Prozent",
#'                                             T~ "Anzahl"))
#'
#'   df$var <- gsub("_p", "", df$var)
#'
#'
#'   if(absolut_selector=="In Prozent"){
#'
#'     df <- df %>%
#'       dplyr::filter(selector=="In Prozent")
#'
#'     df$wert <- df$wert *100
#'     df$wert <- round(df$wert, 1)
#'
#'
#'     df$display_rel <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
#'
#'
#'     # Überschrift vorbereiten
#'     label_select <- ifelse(label_select == "Studierende", paste0(label_select, "n"), label_select)
#'     label_select <- ifelse(label_select == "Studierende (Fachhochschulen)", "Studierenden (Fachhochschulen)" , label_select)
#'     label_select <- ifelse(label_select == "Studierende (Lehramt, Universität)", "Studierenden (Lehramt, Universität)" , label_select)
#'     label_select <- ifelse(label_select == "Studierende (Universität)", "Studierenden (Universität)" , label_select)
#'
#'     titel_help <- "Studierenden"
#'     titel_help <- ifelse(grepl("Studienanfänger:innen",label_select), "Studienanfänger:innen", titel_help)
#'
#'
#'     df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
#'     # plot
#'
#'
#'     titel <- paste0("Anteil von ", label_select, " in MINT an allen ", titel_help, " in ",states )
#'     tooltip <- "Anteil {point.display_rel}%"
#'     format <- "{value}%"
#'     color <- c("#b16fab", "#154194","#66cbaf")
#'     out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "var", tooltip, format, color)
#'
#'   } else if (absolut_selector == "Anzahl") {
#'
#'
#'     hcoptslang <- getOption("highcharter.lang")
#'     hcoptslang$thousandsSep <- "."
#'     options(highcharter.lang = hcoptslang)
#'
#'     df <- df %>%
#'       dplyr::filter(selector=="Anzahl")
#'
#'     df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
#'
#'
#'     df <- df %>% dplyr::filter(indikator==label_select)
#'
#'     # Überschrift vorbereiten
#'     label_select <- ifelse(label_select == "Studierende", paste0(label_select, "n"), label_select)
#'     label_select <- ifelse(label_select == "Studierende (Fachhochschulen)", "Studierenden (Fachhochschulen)" , label_select)
#'     label_select <- ifelse(label_select == "Studierende (Lehramt, Universität)", "Studierenden (Lehramt, Universität)" , label_select)
#'     label_select <- ifelse(label_select == "Studierende (Universität)", "Studierenden (Universität)" , label_select)
#'
#'     df <- df %>% dplyr::filter(region == states)
#'
#'     df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
#'     # plot
#'
#'     titel <- paste0("Anzahl an ", label_select, " in MINT in ",states )
#'     tooltip <- "Anzahl: {point.display_abs}"
#'     format <- "Anzahl: {point.display_abs}"
#'     color <- c("#b16fab", "#154194","#66cbaf")
#'     out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "var", tooltip, format, color)
#'
#'
#'
#'   }
#'
#' return(out)
#' }
#'
#'
#'
#' #' A function to plot time series
#' #'
#' #' @description A function to plot the time series of the german states
#' #'
#' #' @return The return value, if any, from executing the function.
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' studierende_verlauf_multiple_bl <- function(r) {
#'
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_studium_studienzahl_bl_verlauf
#'   t <- as.character(timerange[1]:timerange[2])
#'
#'   absolut_selector <- r$abs_zahlen_studium_studienzahl_bl_verlauf
#'
#'   subjects_select <- r$subject_studium_studienzahl_bl_verlauf
#'
#'   bl_label <- r$verl_bl_l
#'
#'   states <- r$states_studium_studienzahl_bl_verlauf
#'
#'
#'
#'
#'   df_query <- glue::glue_sql("
#'         SELECT *
#'         FROM studierende
#'         WHERE jahr in ({t*})
#'         AND indikator = {bl_label}
#'         AND geschlecht = 'Gesamt'
#'                                ", .con = con)
#'
#'   df <- DBI::dbGetQuery(con, df_query)
#'
#'
#'   df <- df %>%
#'     dplyr::select(-geschlecht)%>%
#'     dplyr::filter(!is.na(wert))%>%
#'
#'     tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
#'     dplyr::mutate(Ingenieurwissenschaften_p=Ingenieurwissenschaften/Alle,
#'                   "Mathematik, Naturwissenschaften_p"= `Mathematik, Naturwissenschaften`/Alle,
#'                   "MINT (Gesamt)_p"=`MINT (Gesamt)`/Alle)%>%
#'     dplyr::select(-Alle)%>%
#'     dplyr::mutate(
#'       dplyr::across(
#'         c(Ingenieurwissenschaften_p, `Mathematik, Naturwissenschaften_p`, `MINT (Gesamt)_p`),  ~round(.*100,1)))%>%
#'     dplyr::select(-`Nicht MINT`)%>%
#'     tidyr::pivot_longer(c(4:9), values_to ="wert", names_to="fach")%>%
#'     dplyr::mutate(selector = dplyr::case_when(
#'       stringr::str_ends(.$fach, "_p") ~ "In Prozent",
#'       T~"Anzahl"
#'     ))
#'
#'   df$fach <- gsub("_p", "", df$fach)
#'
#'   df <- df %>% dplyr::filter(
#'     fach %in% subjects_select)
#'
#'
#'   df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
#'
#'   df <- df %>%dplyr::filter(region%in%states)
#'
#'   # Vorbereitung Überschrift
#'
#'   bl_label <- ifelse(bl_label == "Studierende", paste0(bl_label, "n"), bl_label)
#'   bl_label <- ifelse(bl_label == "Studierende (Fachhochschulen)", "Studierenden (Fachhochschulen)" , bl_label)
#'   bl_label <- ifelse(bl_label == "Studierende (Lehramt, Universität)", "Studierenden (Lehramt, Universität)" , bl_label)
#'   bl_label <- ifelse(bl_label == "Studierende (Universität)", "Studierenden (Universität)" , bl_label)
#'
#'   help <- "Studierenden"
#'   help <- ifelse(grepl("Studienanfänger:innen",bl_label), "Studienanfänger:innen", help)
#'
#'   fach_label <- subjects_select
#'   fach_label<- ifelse(fach_label == "MINT (Gesamt)", "MINT", fach_label)
#'
#'   # Plot
#'
#'   if (absolut_selector=="In Prozent"){
#'
#'     df <- df %>%
#'       dplyr::filter(selector=="In Prozent")
#'
#'    df$display_rel <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
#'
#'     titel <- paste0("Anteil von ", bl_label, " in ", fach_label, " an allen ", help)
#'     tooltip <-  "Anteil {point.region} <br> Wert: {point.display_rel} %"
#'     format <- "{value}%"
#'     color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
#'                "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
#'     out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color)
#'
#'   } else if(absolut_selector=="Anzahl"){
#'
#'     hcoptslang <- getOption("highcharter.lang")
#'     hcoptslang$thousandsSep <- "."
#'     options(highcharter.lang = hcoptslang)
#'
#'     df <- df %>%
#'       dplyr::filter(selector == "Anzahl")
#'
#'     df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
#'
#'
#'     titel <- paste0("Anzahl an ", bl_label, " in ", help)
#'     tooltip <-  "Anzahl: {point.display_abs}"
#'     format <- "{value:, f}"
#'     color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
#'                "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
#'     out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "region", tooltip, format, color)
#'
#'
#'
#'
#'   }
#'
#'
#'   return(out)
#' }
#'
#'
#'
#' #' A function to plot time series
#' #'
#' #' @description A function to plot a bar chart
#' #'
#' #' @return The return value, if any, from executing the function.
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' studienzahl_einstieg_comparison <- function(r) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_kurse_einstieg_comparison
#'
#'
#'   df_query <- glue::glue_sql("
#'         SELECT *
#'         FROM studierende
#'         WHERE jahr = {timerange}
#'         AND indikator = {bl_label}
#'         AND region='Deutschland'
#'                                ", .con = con)
#'
#'   df <- DBI::dbGetQuery(con, df_query)
#'
#'
#'
#'
#'
#'
#'
#'   df <- df %>%
#'     tidyr::pivot_wider(names_from=fachbereich, values_from = wert)%>%
#'     dplyr::select( -region, -Ingenieurwissenschaften,- `Mathematik, Naturwissenschaften`)
#'
#'   # Calculating props
#'
#'   df_props <- df %>%
#'     dplyr::mutate(dplyr::across(c("MINT (Gesamt)", "Nicht MINT"), ~round(./Alle * 100,1)))%>%
#'     dplyr::select(-Alle)%>%
#'     tidyr::pivot_longer(c("MINT (Gesamt)", "Nicht MINT"), values_to="prop", names_to = "proportion")
#'
#'   # joining props and wert
#'   df <- df%>%
#'     dplyr::select(-Alle )%>%
#'     tidyr::pivot_longer(c("MINT (Gesamt)", "Nicht MINT"), values_to="wert", names_to = "proportion")%>%
#'     dplyr::left_join(df_props)
#'
#'
#'   #Trennpunkte für lange Zahlen ergänzen
#'
#'   df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
#'   df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")
#'
#'
#'
#'   df$indikator <-factor(df$indikator,levels= c("Studierende",
#'                                                "Studierende (Fachhochschulen)",
#'                                                "Studierende (Lehramt, Universität)",
#'                                                "Studierende (Universität)",
#'                                                "Studienanfänger:innen (1.Fachsemester)",
#'                                                "Studienanfänger:innen (1.Hochschulsemester)",
#'                                                "Studienanfänger:innen (Fachhochschulen, 1.Fachsemester)",
#'                                                "Studienanfänger:innen (Fachhochschulen, 1.Hochschulsemester)",
#'                                                "Studienanfänger:innen (Lehramt, Universität, 1.Fachsemester)",
#'                                                "Studienanfänger:innen (Lehramt, Universität, 1.Hochschulsemester)",
#'                                                "Studienanfänger:innen (Universität, 1.Fachsemester)",
#'                                                "Studienanfänger:innen (Universität, 1.Hochschulsemester)"
#'   )
#'   )
#'
#'
#'   # plot
#'
#'   df <- within(df, proportion <- factor(proportion, levels=c("Nicht MINT", "MINT (Gesamt)")))
#'
#'   #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
#'   highcharter::hchart(df, 'bar', highcharter::hcaes(y = prop, x = indikator, group = forcats::fct_rev(proportion)))%>%
#'     highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.proportion} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.display_abs}") %>%
#'     highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  F) %>%
#'     highcharter::hc_xAxis(title = list(text = "")) %>%
#'     highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
#'     highcharter::hc_colors(c( "#b16fab","#efe8e6")) %>%
#'     highcharter::hc_title(text = paste0("Anteil von Studierenden in MINT an allen Studierenden", "(", timerange, ")"),
#'                           margin = 45,
#'                           align = "center",
#'                           style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
#'     highcharter::hc_chart(
#'       style = list(fontFamily = "Calibri Regular", fontSize = "14px")
#'     ) %>%
#'     highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
#'     highcharter::hc_exporting(enabled = TRUE,
#'                               buttons = list(
#'                                 contextButton = list(
#'                                   menuItems = list("downloadPNG", "downloadCSV")
#'                                 )
#'                               )
#'     )
#'
#'
#' }
#'
#'
#' #' A function to plot time series
#' #'
#' #' @description A function to plot the time series of the german states
#' #'
#' #' @return The return value, if any, from executing the function.
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' studierende_verlauf_single_bl_gender <- function(r) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$choice_V_y
#'   t <- as.character(timerange[1]:timerange[2])
#'
#'   v_lab <- r$choice_l_v
#'
#'   absolut_selector <- r$abs_zahlen_l_v
#'
#'   subjects_select <- r$choice_v_f
#'
#'   states <- r$choice_states
#'
#'
#'   df_query <- glue::glue_sql("
#'         SELECT *
#'         FROM studierende
#'         WHERE jahr in ({t*})
#'         AND geschlecht = 'Frauen'
#'         AND indikator in ({v_lab*})
#'         AND region = {states}
#'                                ", .con = con)
#'
#'   df <- DBI::dbGetQuery(con, df_query)
#'
#'
#'
#'
#'   df <- df %>%
#'     tidyr::pivot_wider(names_from=fachbereich, values_from = wert)%>%
#'
#'     dplyr::mutate("Mathematik, Naturwissenschaften_p" =round(`Mathematik, Naturwissenschaften`/Alle*100,1),
#'                   "MINT (Gesamt)_p"= round(`MINT (Gesamt)`/Alle*100,1),
#'                   "Ingenieurwissenschaften_p"= round(Ingenieurwissenschaften/Alle*100,1))%>%
#'     dplyr::select(-Alle, -`Nicht MINT`)%>%
#'     tidyr::pivot_longer(c(5:10),names_to="fach",values_to="wert")%>%
#'     dplyr::mutate(selector= dplyr::case_when(stringr::str_ends(.$fach, "_p")~"In Prozent",
#'                                              T~"Anzahl"))
#'
#'   df$fach <- gsub("_p", "", df$fach)
#'
#'
#'
#'   df <- df %>%
#'     dplyr::filter(fach==subjects_select)
#'
#'   fach_label <- subjects_select
#'   fach_label <- ifelse(fach_label == "MINT (Gesamt)", "MINT", fach_label)
#'
#'   if (absolut_selector=="In Prozent"){
#'
#'     df <- df %>%
#'       dplyr::filter(selector=="In Prozent")
#'
#'     df <- df[with(df, order( jahr, decreasing = FALSE)), ]
#'
#'
#'     df <- df %>%
#'       dplyr::mutate(jahr= as.numeric(.$jahr))
#'
#'     df <- df[with(df, order( jahr, decreasing = F)), ]
#'
#'     df <- df %>%
#'       dplyr::mutate(jahr= as.character(.$jahr))
#'
#'     #Trennpunkte für lange Zahlen ergänzen
#'
#'
#'     df$display_rel <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
#'
#'
#'
#'
#'     titel <- paste0("Anteil von weiblichen Studierenden, die ein Studium in ", fach_label, " gewählt haben, an allen weiblichen Studierenden in ", states)
#'     tooltip <-  "Anteil {point.label} <br> Wert: {point.display_rel} %"
#'     format <- "{value}%"
#'     color <- c("#b16fab", "#154194", "#66cbaf", "#fbbf24",
#'                "#AFF3E0","#2D6BE1","#008F68","#8893a7", "#ee7775", "#9d7265", "#35bd97",
#'                "#bfc6d3", "#5f94f9",  "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
#'     out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "indikator", tooltip, format, color)
#'
#'
#'   }else if(absolut_selector=="Anzahl"){
#'
#'
#'     hcoptslang <- getOption("highcharter.lang")
#'     hcoptslang$thousandsSep <- "."
#'     options(highcharter.lang = hcoptslang)
#'
#'
#'     df <- df %>%
#'       dplyr::filter(selector=="Anzahl")
#'
#'
#'
#'     df <- df[with(df, order( jahr, decreasing = FALSE)), ]
#'
#'
#'
#'
#'     df <- df %>%
#'       dplyr::mutate(jahr= as.numeric(.$jahr))
#'
#'     df <- df[with(df, order( jahr, decreasing = F)), ]
#'
#'     df <- df %>%
#'       dplyr::mutate(jahr= as.character(.$jahr))
#'
#'     df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
#'
#'
#'     titel <- paste0("Anzahl an weiblichen Studierenden, die ein Studium in ", fach_label, " gewählt haben, in ", states)
#'     tooltip <-  "Anzahl: {point.display_abs}"
#'     format <- "{value:, f}"
#'     color <- c("#b16fab", "#154194", "#66cbaf", "#fbbf24","#AFF3E0","#2D6BE1","#008F68","#8893a7", "#ee7775", "#9d7265", "#35bd97",
#'                "#bfc6d3", "#5f94f9",  "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
#'     out <- linebuilder(df, titel, x = "jahr", y = "wert", group = "indikator", tooltip, format, color)
#'
#'
#'
#'
#'
#'   }
#'
#'   return(out)
#' }

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
    praep <- ifelse(regio == "Saarland", "im", "in")
    if(betrachtung == "Einzelansicht - Kuchendiagramm"){
      label_w <- r$gruppe_mint_fach_pies
    }else{
      label_w <- r$gruppe_mint_fach_balken
    }
    ebene <- r$ebene_mint_fach

    darstellung <- r$abs_zahlen_arbeitsmarkt_einstieg_vergleich_derq

    praep <- ifelse(regio == "Saarland", " im ", " in ")

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

  df <- df %>%
    dplyr::mutate(color1 = color_fach_pie[fach])

  df <- df %>%
    dplyr::mutate(color2 = color_fachbereich[fach])



  if(betrachtung == "Einzelansicht - Kuchendiagramm"){

    # Überschriften vorbereiten
    ueberschrift_fct <- function(label){
      titel_help <- ifelse(label == "Studierende", paste0(label, "n"), label)
      titel_help <- ifelse(titel_help == "internationale Studierende", "internationalen Studierenden", titel_help)
      titel_help <- ifelse(titel_help == "internationale Absolvent:innen", "internationalen Absolvent:innen", titel_help)
      titel_help <- ifelse(titel_help == "Studierende (Lehramt)", "Studierenden im Lehramt", titel_help)
      titel_help <- ifelse(titel_help == "Absolvent:innen (Lehramt)", "Lehramts-Absolvent:innen", titel_help)
      titel_help <- ifelse(titel_help == "internationale Studienanfänger:innen (1. Hochschulsemester)",
                           "internationalenen Studienanfänger:innen (1. Hochschulsemester)", titel_help)
      return(titel_help)
    }

    if(length(label_w)==1){
    titel_help <- ueberschrift_fct(label_w)

    titel = ifelse(regio == "Saarland",
                   paste0("MINT-Fächeranteile von ", titel_help , " im ", regio, " (", timerange, ")"),
                   paste0("MINT-Fächeranteile von ", titel_help , " in ", regio, " (", timerange, ")"))
    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", fach, "</b><br>",
          "Anteil: ", prop, " %<br>",
          "Anzahl: ", wert
        )
      )

    quelle <- "Quelle: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    if(ebene == "MINT-Fächergruppen"){
      out <- piebuilder_plotly(df, titel, x = "fach", y ="prop",
                               color=as.character(df$color1), quelle = "")
    }else{
      out <- piebuilder_plotly(df, titel, x = "fach", y ="prop",
                               color=as.character(df$color2), quelle = quelle)
    }

    } else if(length(label_w)==2){


      df1 <- df %>%
        dplyr::filter(indikator == label_w[1])

      df2 <- df %>%
        dplyr::filter(indikator == label_w[2])


      reihenfolge_faecher <- names(color_fach_pie)

      df1 <- df1 %>%
        dplyr::mutate(
          fach = factor(fach, levels = reihenfolge_faecher)
        ) %>%
        dplyr::arrange(fach)

      df2 <- df2 %>%
        dplyr::mutate(
          fach = factor(fach, levels = reihenfolge_faecher)
        ) %>%
        dplyr::arrange(fach)

      df1 <- df1 %>%
        dplyr::mutate(color1 = color_fach_pie[as.character(fach)])

      df2 <- df2 %>%
        dplyr::mutate(color1 = color_fach_pie[as.character(fach)])


      titel_help1 <- ueberschrift_fct(label_w[1])
      titel_help2 <- ueberschrift_fct(label_w[2])

      titel1 <- paste0("MINT-Fächeranteile von ", titel_help1 , " ", praep, " ", regio, " (", timerange, ")")
      titel2 <- paste0("MINT-Fächeranteile von ", titel_help2 , " ", praep, " ", regio, " (", timerange, ")")

      df1 <- df1 %>%
        dplyr::mutate(
          tooltip = paste0(
            "<span style='font-size:15px'><b>", fach, "</b></span><br>",
            "Anteil: ", prop, " %<br>",
            "Anzahl: ", wert
          )
        )

      df2 <- df2 %>%
        dplyr::mutate(
          tooltip = paste0(
            "<span style='font-size:15px'><b>", fach, "</b></span><br>",
            "Anteil: ", prop, " %<br>",
            "Anzahl: ", wert
          )
        )


      quelle <- "Quelle: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

      p1 <- piebuilder_plotly(df=df1,
                       titel1, x = "fach", y ="prop", legend_y=-0.02, quelle_y= 1,
                       color=as.character(df1$color1), quelle = quelle)

      p2 <- piebuilder_plotly(df=df2, titel2, legend_y=-0.02, quelle_y= 1,
                       x = "fach", y ="prop", color=as.character(df2$color1), quelle = quelle)

      if(ebene == "MINT-Fachbereiche"){

        df1 <- df %>%
          dplyr::filter(indikator == label_w[1])

        df2 <- df %>%
          dplyr::filter(indikator == label_w[2])


        reihenfolge <- c(
          "Alle Nicht MINT-Fächer",
          "Ingenieurwissenschaften (inkl. Informatik)",
          "Mathematik, Naturwissenschaften"
        )


        df1 <- df1 %>%
          dplyr::filter(fach %in% reihenfolge) %>%
          dplyr::mutate(fach = factor(fach, levels = reihenfolge)) %>%
          dplyr::arrange(fach)

        df2 <- df2 %>%
          dplyr::filter(fach %in% reihenfolge) %>%
          dplyr::mutate(fach = factor(fach, levels = reihenfolge)) %>%
          dplyr::arrange(fach)

        df1 <- df1 %>%
          dplyr::mutate(
            tooltip = paste0(
              "<span style='font-size:15px'><b>", fach, "</b></span><br>",
              "Anteil: ", prop, " %<br>",
              "Anzahl: ", wert
            )
          )

        df2 <- df2 %>%
          dplyr::mutate(
            tooltip = paste0(
              "<span style='font-size:15px'><b>", fach, "</b></span><br>",
              "Anteil: ", prop, " %<br>",
              "Anzahl: ", wert
            )
          )



        p1 <- piebuilder_plotly(df=df1,
                                titel1, x = "fach", y ="prop", legend_y = 0.03, quelle_y=-0.08,
                                color=as.character(df1$color2), quelle = "")|>
          plotly::layout(
            annotations = list(
              list(
                text = quelle,
                x = 1,
                y = -0.08,
                xref = "paper",
                yref = "paper",
                xanchor = "right",
                yanchor = "top",
                showarrow = FALSE,
                font = list(size = 11, color = "gray", family = "Calibri Regular", align = "right")
              )
            )
          )
        p2 <- piebuilder_plotly(df=df2, titel2, legend_y = 0.03, quelle_y=-0.08,
                                x = "fach", y ="prop", color=as.character(df2$color2), quelle = "")|>
          plotly::layout(
            annotations = list(
              list(
                text = quelle,
                x = 1,
                y = -0.08,
                xref = "paper",
                yref = "paper",
                xanchor = "right",
                yanchor = "top",
                showarrow = FALSE,
                font = list(size = 11, color = "gray", family = "Calibri Regular", align = "right")
              )
            )
          )
      }else{
        p1 <- piebuilder_plotly(df=df1, titel1, legend_y = -0.05, quelle_y=-0.12,
                                x = "fach", y ="prop", color=as.character(df2$color2), quelle = "")

        p2 <- piebuilder_plotly(df=df2, titel2, legend_y = -0.05, quelle_y=-0.12,
                                x = "fach", y ="prop", color=as.character(df2$color2), quelle = "")
      }

    out <- list(p1, p2)
}
  }
  else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){

    df <- df[with(df, order(prop, decreasing = TRUE)), ]

    if(ebene == "MINT-Fachbereiche"){

      color <- color_fachbereich
      titel <- paste0( "Anteil der MINT-Fachbereiche an allen Fächern", praep, regio," (", timerange, ")<br>", "Studierendengruppe: ",label_w)

        }else{

      color <- color_fach_balken
      titel <- paste0( "Anteil der MINT-Fächergruppen an allen MINT-Fächern", praep, regio," (", timerange, ")<br>", "Studierendengruppe: ",label_w)
           }


    if(darstellung == "In Prozent"){


      order <- unique(df$fach)

      df <- df %>%
        dplyr::mutate(
          .tooltip = paste0(
            "<b><span style='font-size:15px;'>",fach, "</span></b><br>",
            "Anteil: ", round(prop, 1), " %<br>",
            "Anzahl: ", wert
          ))


      x <- "fach"
      y <- "prop"

      quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
      quelle_y <- -0.15


      out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h",percent=TRUE, color=color,
                                  order=order, stacking = FALSE,quelle_y=quelle_y, quelle=quelle)%>%
        plotly::layout(
          margin = list(t = 80),
          title = list(text = titel, x = 0.5, y = 0.95,
                       font = list(family = "Calibri Regular", size = 20, color = "black")))



    } else {


      df$wert <- as.numeric(gsub("\\.","", df$wert))


      order <- unique(df$fach)



      df <- df %>%
        dplyr::mutate(
          wert_label = formatC(wert, format = "f", digits = 0,big.mark = ".", decimal.mark = ","),
          .tooltip = paste0(
            "<b><span style='font-size:15px;'>",fach, "</span></b><br>",
            "Anteil: ", round(prop, 1), " %<br>",
            "Anzahl: ", wert_label
          ))


      x <- "fach"
      y <- "wert"

      quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
      quelle_y <- -0.15


      out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h",percent=FALSE, color=color,
                                  order=order, stacking = FALSE,quelle_y=quelle_y, quelle=quelle)%>%
        plotly::layout(
          margin = list(t = 80),
          title = list(text = titel, x = 0.5, y = 0.95,
                       font = list(family = "Calibri Regular", size = 20, color = "black")))


    }
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
    "Mathematik, Naturwissenschaften" = "#fcc433",
    "Mathematik" = "#ee7775"
  )

  color_fach <- c(
    "Informatik" = "#2D6BE1",
    "Ingenieurwissenschaften (ohne Informatik)" = "#00a87a",
    "Ingenieurwissenschaften (inkl. Informatik)" = "#66CBAF",
    "Elektrotechnik und Informationstechnik" = "#005C43",
    "Maschinenbau/Verfahrenstechnik" = "#004331",
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
    "Mathematik, Naturwissenschaften" = "#fcc433",
    "Materialwissenschaft und Werkstofftechnik" = "#004331",
    "Vermessungswesen" = "#AFF3E0",
    "Bergbau, Hüttenwesen" = "#005C43",
    "allgemeine naturwissenschaftliche und mathematische Fächer" = "#FEF3C7",
    "Alle MINT-Fächer" = "#b16fab",
    "Alle Nicht MINT-Fächer" = "#D4C1BB",
    "Alle Fächer" = "#164194"
  )

  gruppe <- indi
  gruppe <- dplyr::case_when(
    gruppe == "Studierende" ~ "Studierenden",
    gruppe == "internationale Studierende" ~ "internationalen Studierenden",
    gruppe == "Studierende (Lehramt)" ~ "Lehramtstudierenden",
    gruppe == "Absolvent:innen (Lehramt)" ~ "Lehramts-Absolvent:innen",
    gruppe == "internationale Absolvent:innen" ~ "internationalen Absolvent:innen",
    gruppe == "internationale Studienanfänger:innen (1. Hochschulsemester)" ~
      "internationalen Studienanfänger:innen (1. Hochschulsemester)",
    T ~ gruppe
  )

  if(ordering == "MINT-Fächergruppen"){
    faecher_select <- c("Ingenieurwissenschaften (ohne Informatik)",
                        "Mathematik, Naturwissenschaften", "Informatik")
  }else{
    faecher_select <- r$anteile_faecher_mint
  }

  df_query <- glue::glue_sql("
        SELECT region, fach, jahr, wert
        FROM studierende_detailliert
        WHERE jahr IN ({t*})
        AND geschlecht = 'Gesamt'
        AND indikator == {indi}
        AND region = {states}
        AND fach IN ({faecher_select*}, 'Alle Fächer')
                               ", .con = con)

  df <- DBI::dbGetQuery(con, df_query)

  # Farbzuordnung basierend auf der Auswahl
  colors <- if (ordering == "Studienbereich") {
    as.character(color_fach)
  } else if (ordering == "Fächergruppen") {
    as.character(color_fachbereich)
  }


  #plotting
  if(betrachtung == "In Prozent"){

    # Gesamtwerte (Alle MINT-Fächer) vorbereiten
    df_ges <- df %>%
      dplyr::filter(fach == "Alle Fächer") %>%
      dplyr::rename(wert_ges = wert) %>%
      dplyr::select(jahr, wert_ges)

    df <- df %>%
      dplyr::left_join(df_ges, by = c("jahr")) %>%
      dplyr::mutate(prop = round(wert / wert_ges * 100),1)

    if(!("Alle Fächer" %in% faecher_select)) df <- df %>% dplyr::filter(fach != "Alle Fächer")

    sorted_indicators <- df %>%
      dplyr::group_by(fach) %>%
      dplyr::summarize(m_value = mean(round(prop, 1), na.rm = TRUE)) %>%
      dplyr::arrange(m_value) %>%
      dplyr::pull(fach)

    df$fach <- factor(df$fach, levels = sorted_indicators)

    if(ordering == "MINT-Fächergruppen"){
      colors <- color_fachbereich[sorted_indicators]
    }else{
      colors <- color_fach[sorted_indicators]
    }

    df <- df[with(df, order(jahr)),]

    titel <- ifelse(ordering == "Fächergruppen",
                    ifelse(states == "Saarland",
                           paste0("Zeitverlauf von ", gruppe, " im ", states, " nach Fächergruppen"),
                           paste0("Zeitverlauf von ", gruppe, " in ", states, " nach Fächergruppen")),
                    ifelse(states == "Saarland",
                           paste0("Zeitverlauf von ", gruppe, " im ", states, " nach Studienbereichen"),
                           paste0("Zeitverlauf von ", gruppe, " in ", states, " nach Studienbereichen")))

    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", fach, "</b><br>",
          "Jahr: ", jahr, "<br>",
          "Anteil: ", prettyNum(prop, big.mark = ".", decimal.mark = ","), " % <br>",
          "Anzahl: ", prettyNum(wert, big.mark = ".", decimal.mark = ",")
        )
      )
    color <- as.character(colors)
    que <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    out <- linebuilder_plotly(df, titel, x = "jahr", y = "prop", group = "fach", color = color, quelle=que)

  } else if (betrachtung == "Anzahl"){

    if(!("Alle Fächer" %in% faecher_select)) df <- df %>% dplyr::filter(fach != "Alle Fächer")

    sorted_indicators <- df %>%
      dplyr::group_by(fach) %>%
      dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
      dplyr::arrange(m_value) %>%
      dplyr::pull(fach)

    df$fach <- factor(df$fach, levels = sorted_indicators)

    if(ordering == "MINT-Fächergruppen"){
      colors <- color_fachbereich[sorted_indicators]
    }else{
      colors <- color_fach[sorted_indicators]
    }

    df <- df[with(df, order(jahr)),]

    titel <- ifelse(ordering == "MINT-Fächergruppen",
                    ifelse(states== "Saarland",
                           paste0("Zeitverlauf von ", gruppe, " im ", states, " nach Fächergruppen"),
                           paste0("Zeitverlauf von ", gruppe, " in ", states, " nach Fächergruppen")),
                    ifelse(states == "Saarland",
                           paste0("Zeitverlauf von ", gruppe, " im ", states, " nach Studienbereichen"),
                           paste0("Zeitverlauf von ", gruppe, " in ", states, " nach Studienbereichen")))

    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", fach, "</b><br>",
          "Jahr: ", jahr, "<br>",
          "Anzahl: ", prettyNum(wert, big.mark = ".", decimal.mark = ","), "<br>"
          # ,
          # "Anteil: ", prettyNum(prop, big.mark = ".", decimal.mark = ","), " %"

        )
      )
    format <- ",d"
    color <- as.character(colors)
    quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    out <- linebuilder_plotly(df, titel, x = "jahr", y = "wert", group = "fach", format = format, color = color, quelle = quelle)

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
    if(label_m %in% c("Studierende (Lehramt)", "Absolvent:innen (Lehramt)")){
      faecher <- r$bl_f_lehr_faecher
    }else{
      faecher <- r$bl_f_alle_faecher
    }


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
    label_m <- ifelse(label_m == "internationale Absolvent:innen", "internationalen Absolvent:innen", label_m)
    label_m <- ifelse(label_m == "Studierende (Lehramt)", "Studierenden im Lehramt", label_m)
    label_m <- ifelse(label_m == "Absolvent:innen (Lehramt)", "Lehramts-Absolvent:innen", label_m)

    help_l <- label_m
    help_l <- ifelse(label_m == "internationalen Studienanfänger:innen (1. Hochschulsemester)",
                     "internationalen Studienanfänger:innen", help_l)
    help_l <- ifelse(label_m == "Studienanfänger:innen (1. Hochschulsemester)", "Studienanfänger:innen", help_l)



    if(grepl("(Lehramt)", label_m)){
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

    #quelle null da kein input

    joinby <- c("name", "region")
    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", region, "</b><br>",
          "Anteil: ", display_rel, " %<br>",
          "Anzahl: ", display_abs
        )
      )
    quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    out <- mapbuilder_plotly(df,
                             titel = titel,
                             value_col = "prop",
                             quelle = quelle)



  }
  else if(betrachtung == "Zeitverlauf - Liniendiagramm"){

    # load UI inputs from reactive value
    absolut_selector <-  r$bulas_verlauf_abs_rel_faecher
    timerange <- r$bulas_verlauf_y_faecher
    t <- timerange[1]:timerange[2]
    states <- r$bulas_verlauf_regio_faecher
    label_select <- r$bulas_verlauf_l_faecher

    if(any(c("Studierende (Lehramt)", "Absolvent:innen (Lehramt)") %in% label_select)){
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

    df_valid <- df %>%
      dplyr::filter(!is.na(wert) & wert != 0)

    valid_regions <- unique(df_valid$region)

    df <- df %>%
      dplyr::select(-c(bereich, geschlecht, fachbereich, mint_select, typ))


    if (absolut_selector == "In Prozent") {

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

      sorted_indicators <- df %>%
        dplyr::group_by(region) %>%
        dplyr::summarize(m_value = mean(round(prop, 1), na.rm = TRUE)) %>%
        dplyr::arrange(m_value) %>%
        dplyr::pull(region)

      df$region <- factor(df$region, levels = sorted_indicators)

      label_m <- ifelse(label_select == "Studierende", paste0(label_select, "n"), label_select)
      label_m <- ifelse(label_m == "internationale Studierende", "internationalen Studierenden", label_m)
      label_m <- ifelse(label_m == "internationale Absolvent:innen", "internationalen Absolvent:innen", label_m)
      label_m <- ifelse(label_m == "Studierende (Lehramt)", "Studierenden im Lehramt", label_m)
      label_m <- ifelse(label_m == "Absolvent:innen (Lehramt)", "Lehramts-Absolvent:innen", label_m)
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
      df <- df %>%
        dplyr::mutate(
          tooltip = paste0(
            "<b>", region, "</b><br>",
            "Jahr: ", jahr, "<br>",
            "Anteil: ", prettyNum(prop, big.mark = ".", decimal.mark = ","), " %"
          )
        )
      color <- c("#b16fab", "#154194", "#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#d0a9cd",
                 "#bfc6d3", "#5f94f9", "#B45309")[1:length(unique(df$region))]
      quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

      out <- linebuilder_plotly(df, titel, x = "jahr", y = "prop", group = "region", color = color, quelle = quelle)


    }


    else if (absolut_selector == "Anzahl") {

      # Überschrift vorbereiten
      label_m <- ifelse(label_select == "Studierende", paste0(label_select, "n"), label_select)
      label_m <- ifelse(label_m == "internationale Studierende", "internationalen Studierenden", label_m)
      label_m <- ifelse(label_m == "internationale Absolvent:innen", "internationalen Absolvent:innen", label_m)
      label_m <- ifelse(label_m == "Studierende (Lehramt)", "Studierenden im Lehramt", label_m)
      label_m <- ifelse(label_m == "Absolvent:innen (Lehramt)", "Lehramts-Absolvent:innen", label_m)
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
      df <- df %>%
        dplyr::mutate(
          tooltip = paste0(
            "<b>", region, "</b><br>",
            "Jahr: ", jahr, "<br>",
            "Anzahl: ", prettyNum(wert, big.mark = ".", decimal.mark = ",")
          )
        )
      format <- ",d"
      color <- c("#b16fab", "#154194", "#66cbaf")
      quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

      out <- linebuilder_plotly(df, titel, x = "jahr", y = "wert", group = "region",
                                format = format, color = color, quelle = quelle)


    }
  }
  else if(betrachtung == "Gruppenvergleich - Balkendiagramm") {

    #UI Input laden
    timerange <- r$bulas_balken_date_faecher
    r_lab1 <- r$bulas_balken_l_faecher
    regio <- r$bulas_balken_regio_faecher
    if(r_lab1 %in% c("Studierende (Lehramt)", "Absolvent:innen (Lehramt)")){
      fach_bl <- r$bl_balken_lehr_faecher
    }else{
      fach_bl <- r$bl_balken_alle_faecher
    }
    darstellungx <- r$abs_zahlen_arbeitsmarkt_einstieg_vergleich_der444


    df_query <- glue::glue_sql("
        SELECT region, indikator, fach, jahr, wert
        FROM studierende_detailliert
        WHERE jahr IN ({timerange*})
        AND geschlecht = 'Gesamt'
        AND indikator == {r_lab1}
        AND region IN ({regio*})
        AND fach = 'Alle Fächer'
                               ", .con = con)

    df_ges <- DBI::dbGetQuery(con, df_query) %>%
      dplyr::rename(wert_ges = wert)


    df_query <- glue::glue_sql("
        SELECT region, indikator, fach, jahr, wert
        FROM studierende_detailliert
        WHERE jahr IN ({timerange*})
        AND geschlecht = 'Gesamt'
        AND indikator == {r_lab1}
        AND region IN ({regio*})
        AND fach = {fach_bl}
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::left_join(df_ges, by = c("region", "jahr", "indikator")) %>%
      dplyr::mutate(prop = wert/wert_ges * 100) %>%
      dplyr::select(-fach.y) %>%
      dplyr::rename(fach = fach.x)


    #Trennpunkte für lange Zahlen ergänzen
    df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")


    # Vorbereitung Überschrift
    help_s <- fach_bl
    help_s <- ifelse(help_s == "Alle Nicht MINT-Fächer", "allen Fächern außer MINT", help_s)
    help_s <- ifelse(help_s == "Alle MINT-Fächer", "MINT-Fächern", help_s)
    help_s <- ifelse(help_s == "allgemeine naturwissenschaftliche und mathematische Fächer",
                         "allgemeinen naturwissenschaftlichen und mathematischen Fächern", help_s)


    r_lab1 <- ifelse(r_lab1 == "Studierende", paste0(r_lab1, "n"), r_lab1)
    r_lab1 <- ifelse(r_lab1 == "internationale Studierende", "internationalen Studierenden", r_lab1)
    r_lab1 <- ifelse(r_lab1 == "internationale Absolvent:innen", "internationalen Absolvent:innen", r_lab1)
    r_lab1 <- ifelse(r_lab1 == "Studierende (Lehramt)", "Studierenden im Lehramt", r_lab1)
    r_lab1 <- ifelse(r_lab1 == "Absolvent:innen (Lehramt)", "Lehramts-Absolvent:innen", r_lab1)
    r_lab1 <- ifelse(r_lab1 == "internationale Studienanfänger:innen (1. Hochschulsemester)",
                      "internationale Studienanfänger:innen (1. Hochschulsemester)", r_lab1)
    help <- r_lab1
    help <- ifelse(help == "internationale Studienanfänger:innen (1. Hochschulsemester)", "internationalen Studienanfänger:innen", help)
    help <- ifelse(help == "Studienanfänger:innen (1. Fachsemester)" |
                     help == "Studienanfänger:innen (1. Hochschulsemester)", "Studienanfänger:innen", help)



    df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")

    df <- df[with(df, order(prop, decreasing = TRUE)),]

    if(nrow(df) == 0){
      titel <- "Für diese Kombination aus Fächergruppe und Bundesland bzw. Bundesländer liegen keine Daten vor.
        Bitte wählen Sie eine andere Komination oder Fächergruppe aus."

      quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

      out <- linebuilder(df, titel = titel, x = "jahr", y = "wert", group = "region", tooltip = "Anzahl: {point.display_abs}", format = "{value:, f}", quelle = quelle)



    }else{


    titel <- if (any(fach_bl %in% c(
      "Wirtschaftsingenieurwesen mit ingenieurwissenschaftlichem Schwerpunkt",
      "allgemeine naturwissenschaftliche und mathematische Fächer"
      ))) {
      paste0( "Anteil von ", r_lab1 ," in ", help_s," an allen ", help,  " (", timerange, ")")
    } else {
      paste0( "Anteil von ", r_lab1 ," in ", help_s," an allen ", help,  " (", timerange, ")")}




    if(darstellungx == "In Prozent"){


      order <- unique(df$region)

      df <- df %>%
        dplyr::mutate(
          .tooltip = paste0(
            "<b><span style='font-size:15px;'>", region, "</span></b><br>",
            "Anteil: ", round(prop, 1), " %<br>",
            "Anzahl: ", wert
          ))


      x <- "region"
      y <- "prop"
      color <- c(
        "Deutschland" = "#b16fab",
        "Ostdeutschland (inkl. Berlin)" = "#d3a4d7",
        "Westdeutschland (o. Berlin)" = "#d3a4d7",
        "Baden-Württemberg" = "#A9A9A9",
        "Bayern" = "#A9A9A9",
        "Berlin" = "#A9A9A9",
        "Brandenburg" = "#A9A9A9",
        "Bremen" = "#A9A9A9",
        "Hamburg" = "#A9A9A9",
        "Hessen" = "#A9A9A9",
        "Mecklenburg-Vorpommern" = "#A9A9A9",
        "Niedersachsen" = "#A9A9A9",
        "Nordrhein-Westfalen" = "#A9A9A9",
        "Rheinland-Pfalz" = "#A9A9A9",
        "Saarland" = "#A9A9A9",
        "Sachsen" = "#A9A9A9",
        "Sachsen-Anhalt" = "#A9A9A9",
        "Schleswig-Holstein" = "#A9A9A9",
        "Thüringen" = "#A9A9A9"
      )

      quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
      quelle_y <- -0.10


      out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h",percent=TRUE, color=color,
                                  order=order, stacking=FALSE, quelle_y=quelle_y, quelle=quelle)%>%
        plotly::layout(
          margin = list(t = 80, b = 100))


    } else {


      order <- unique(df$region)

      df <- df %>%
        dplyr::mutate(
          wert = as.numeric(gsub("\\.", "", wert)),
          wert_label = formatC(wert, format = "f", digits = 0, big.mark = ".", decimal.mark = ","),
          .tooltip = paste0(
            "<b><span style='font-size:15px;'>", region, "</span></b><br>",
            "Anteil: ", round(prop, 1), " %<br>",
            "Anzahl: ", wert_label
          ))


      x <- "region"
      y <- "wert"
      color <- c(
        "Deutschland" = "#b16fab",
        "Ostdeutschland (inkl. Berlin)" = "#d3a4d7",
        "Westdeutschland (o. Berlin)" = "#d3a4d7",
        "Baden-Württemberg" = "#A9A9A9",
        "Bayern" = "#A9A9A9",
        "Berlin" = "#A9A9A9",
        "Brandenburg" = "#A9A9A9",
        "Bremen" = "#A9A9A9",
        "Hamburg" = "#A9A9A9",
        "Hessen" = "#A9A9A9",
        "Mecklenburg-Vorpommern" = "#A9A9A9",
        "Niedersachsen" = "#A9A9A9",
        "Nordrhein-Westfalen" = "#A9A9A9",
        "Rheinland-Pfalz" = "#A9A9A9",
        "Saarland" = "#A9A9A9",
        "Sachsen" = "#A9A9A9",
        "Sachsen-Anhalt" = "#A9A9A9",
        "Schleswig-Holstein" = "#A9A9A9",
        "Thüringen" = "#A9A9A9"
      )

      quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
      quelle_y <- -0.10


      out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h",percent=FALSE, color=color,
                                  order=order, stacking=FALSE, quelle_y=quelle_y, quelle=quelle)%>%
        plotly::layout(
          margin = list(t = 80, b = 100))


    }
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
#'
#' ranking_bl_subject <- function(r) {
#'
#' #### fehler annotations ---- siehe downloaded image
#'   # load UI inputs from reactive value
#'
#'   timerange <- r$rank_y
#'
#'   states <- r$rank_states
#'
#'   r_lab <- r$rank_l
#'
#'
#'   df_query <- glue::glue_sql("
#'         SELECT *
#'         FROM studierende_detailliert
#'         WHERE jahr = {timerange}
#'         AND (mint_select = 'MINT' OR fach IN ('Alle MINT-Fächer','Alle Fächer','Alle Nicht MINT-Fächer'))
#'         AND geschlecht = 'Gesamt'
#'         AND region = {states}
#'                                ", .con = con)
#'
#'   df <- DBI::dbGetQuery(con, df_query)
#'
#'   df <- df %>%
#'     dplyr::select(-bereich,- fachbereich)
#'
#'
#'
#'
#'
#'
#'
#'   df_ges <- df %>% dplyr::filter(fach == "Alle Fächer")%>%
#'     tidyr::pivot_wider(names_from=fach, values_from = wert)%>%
#'     dplyr::select(indikator, jahr, region, `Alle Fächer`)
#'
#'   df <- df %>%
#'     dplyr::select(indikator, jahr, region, fach, wert)%>%
#'     dplyr::filter(fach != "Alle Fächer")%>%
#'     dplyr::left_join(df_ges , by=c("indikator", "jahr", "region"))
#'
#'   df <- df %>%
#'     dplyr::mutate(prop = round(wert /`Alle Fächer` *100, 1 ))
#'
#'
#'   df <- df %>%
#'     dplyr::filter(indikator == r_lab)%>%
#'     dplyr::filter(!is.na(wert))%>%
#'     dplyr::filter(prop != 0,
#'                   fach != "Naturwissenschaften")
#'
#'
#'   ticks <- c("Alle MINT-Fächer",
#'              "Mathematik, Naturwissenschaften",
#'              "Mathematik",
#'              "Biologie",
#'              "Chemie",
#'              "Physik, Astronomie",
#'              "Pharmazie",
#'              "Geowissenschaften und Geographie",
#'              "Ingenieurwissenschaften (inkl. Informatik)",
#'              "Informatik",
#'              "Maschinenbau/Verfahrenstechnik" ,
#'              "Elektrotechnik und Informationstechnik",
#'              "Verkehrstechnik, Nautik",
#'              "Architektur, Innenarchitektur",
#'              "Raumplanung",
#'              "Bauingenieurwesen",
#'              "Vermessungswesen",
#'              "Wirtschaftsingenieurwesen mit ingenieurwissenschaftlichem Schwerpunkt",
#'              "Materialwissenschaft und Werkstofftechnik",
#'              "Bergbau, Hüttenwesen",
#'
#'              "Alle Nicht MINT-Fächer"
#'   )
#'
#'
#'
#'   df_t <- df %>%
#'     dplyr::select(fach)%>%
#'     unique()%>%
#'     as.vector()%>%
#'     unlist()%>%
#'     unname()
#'
#'   ticks1 <- ticks[ticks %in% df_t]
#'
#'   df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
#'   df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")
#'
#'
#'
#'   #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
#'   out <- highcharter::hchart(df, 'bar', highcharter::hcaes(y=prop, x= fach))%>%
#'     highcharter::hc_tooltip(pointFormat = "{point.region} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.display_abs}") %>% #Inhalt für Hover-Box
#'     highcharter::hc_yAxis(title = list(text=""), labels = list(format = "{value}%")) %>% #x-Achse -->Werte in %
#'     highcharter::hc_xAxis(title= list(text=""),
#'                           categories = ticks1
#'     ) %>%
#'     highcharter::hc_plotOptions(bar = list(
#'       colorByPoint = TRUE,
#'       colors = ifelse(df$fach %in% c("Alle MINT-Fächer", "Alle Nicht MINT-Fächer",
#'                                       "Ingenieurwissenschaften (inkl. Informatik)",
#'                                       "Mathematik, Naturwissenschaften"
#'       ), "#b16fab", "#d0a9cd")))%>%#balken lila für MINT
#'     highcharter::hc_title(text = paste0( "Anteil einzelner Fächer an allen Fächern ", "(", r_lab, ")" , " in ",states," ", timerange),
#'                           margin = 45,
#'                           align = "center",
#'                           style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
#'     highcharter::hc_exporting(enabled = TRUE,
#'                               buttons = list(
#'                                 contextButton = list(
#'                                   menuItems = list("downloadPNG", "downloadCSV")
#'                                 )
#'                               )
#'     )
#'
#'
#'   return(out)
#' }
#'
#'
#'
#'
#' #' A function to plot a bar chart
#' #'
#' #' @description A function to create a bar chart to compare different subjects
#' #' for different Bundesländer.
#' #'
#' #' @return The return value is a bar chart
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' studierende_mint_vergleich_bl <- function(r) {
#'
#'   # load UI inputs from reactive value
#'
#'   timerange <- as.numeric(r$bl_date)
#'
#'   r_lab1 <- r$rank_bl_l
#'
#'   # Fach abhängig von Lehramt ja/nein zuweisen
#'   if(r_lab1 == "Studierende (Lehramt)")  fach_bl <- r$bl_f_lehr
#'   if(r_lab1 != "Studierende (Lehramt)")  fach_bl <- r$bl_f_alle
#'
#'
#'   # df_ges <- dplyr::tbl(con, from = "studierende_detailliert") %>%
#'   #   dplyr::filter(geschlecht=="Gesamt",
#'   #                 jahr %in% timerange) %>%
#'   #   dplyr::collect()
#'   #
#'   df_query <- glue::glue_sql("
#'         SELECT *
#'         FROM studierende_detailliert
#'         WHERE jahr IN ({timerange*})
#'         AND geschlecht = 'Gesamt'
#'                                ", .con = con)
#'
#'   df_ges <- DBI::dbGetQuery(con, df_query)
#'
#'
#'
#'
#'   df_query <- glue::glue_sql("
#'         SELECT *
#'         FROM studierende_detailliert
#'         WHERE jahr IN ({timerange*})
#'         AND geschlecht = 'Gesamt'
#'                                ", .con = con)
#'
#'   df <- DBI::dbGetQuery(con, df_query)
#'
#'   df <- df %>%
#'     dplyr::select(-fachbereich,- mint_select, -typ) %>%
#'     tidyr::pivot_wider(names_from = fach, values_from = wert)%>%
#'     dplyr::mutate(dplyr::across(c(6:ncol(.)), ~round(./`Alle Fächer`*100,1)))%>%
#'     tidyr::pivot_longer(c(6:ncol(.)), values_to = "proportion", names_to ="fach")%>%
#'     dplyr::right_join(df_ges)
#'
#'
#'
#'
#'   #Trennpunkte für lange Zahlen ergänzen
#'   df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
#'
#'   df <- df %>%
#'     dplyr::select(indikator, region, jahr, fach, proportion, wert)
#'
#'
#'   df <- df %>%dplyr::filter(indikator == r_lab1 )%>%
#'     dplyr::filter(fach==fach_bl)
#'
#'   # NA aus fach entfernen für BULAs mit weniger Studienfachgruppen
#'   df <- stats::na.omit(df)
#'
#'   # Vorbereitung Überschrift
#'   help_s <- fach_bl
#'   help_s <- ifelse(help_s == "Alle Nicht MINT-Fächer", "allen Fächern außer MINT", help_s)
#'   help_s <- ifelse(help_s == "Alle MINT-Fächer", "MINT", help_s)
#'
#'
#'   r_lab1 <- ifelse(r_lab1 == "Studierende", paste0(r_lab1, "n"), r_lab1)
#'   r_lab1 <- ifelse(grepl("Lehr", r_lab1), "Studierenden (Lehramt)", r_lab1)
#'   help <- r_lab1
#'   help <- ifelse(help == "internationalenen Studienanfänger:innen (1. Hochschulsemester)", "internationalen Studienanfänger:innen", help)
#'   help <- ifelse(help == "Studienanfänger:innen (1. Fachsemester)" |
#'                    help == "Studienanfänger:innen (1. Hochschulsemester)", "Studienanfänger:innen", help)
#'
#'
#'
#'   df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
#'   df$display_rel <- prettyNum(df$proportion, big.mark = ".", decimal.mark = ",")
#'
#'   # Plot
#'   #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
#'   out <- highcharter::hchart(df, 'bar', highcharter::hcaes(x= region, y = proportion))%>%
#'     highcharter::hc_tooltip(pointFormat = "{point.fach} <br> Anteil: {point.display_rel} % <br> Anzahl: {point.display_abs}") %>% #Inhalt für Hover-Box
#'     highcharter::hc_yAxis(title = list(text=""), labels = list(format = "{value}%")) %>% #x-Achse -->Werte in %
#'     highcharter::hc_xAxis(title= list(text="")) %>% #Y-Achse - keine Beschriftung
#'     highcharter::hc_colors("#b16fab") %>% #balken lila für MINT
#'     highcharter::hc_title(text = paste0( "Anteil von ", r_lab1 ," in ", help_s," an allen ", help,  " (", timerange, ")"),
#'                           margin = 25,
#'                           align = "center",
#'                           style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px"))   %>%
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


        if(length(genl) == 1) {

          title_n <- genl[1]
          title_n <- ifelse(title_n == "Studierende", "Studierenden", title_n)
          title_n <- ifelse(title_n == "Studierende (Lehramt)", "Studierenden im Lehramt", title_n)

          df_p <- df[df$fach == gen_f[1],]

          titel <- ifelse(regio == "Saarland",
                          paste0("Frauenanteil unter ", title_n, " in ", titel_help, " im ", regio, " (", timerange, ")"),
                          paste0("Frauenanteil unter ", title_n, " in ", titel_help, " in ", regio, " (", timerange, ")"))

          df_p <- df_p %>%
            dplyr::mutate(
              tooltip = paste0(
                "<b>", geschlecht, "</b><br>",
                "Anteil: ", prop, " %<br>",
                "Anzahl: ", display_abs
              )
            )

          color <- c("Männer" ="#efe8e6", "Frauen" = "#154194")

          quelle <- "Quelle: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

          out <- piebuilder_plotly(df_p, titel, x = "geschlecht", y ="prop",
                                   color=color, quelle = quelle)

           if(gegenwert == "Ja"){

             title_n <- genl[1]
             title_n <- ifelse(title_n == "Studierende", "Studierenden", title_n)
             title_n <- ifelse(title_n == "Studierende (Lehramt)", "Studierenden im Lehramt", title_n)

             df_g <- df[df$fach == "Alle Nicht MINT-Fächer",]

             titel <- ifelse(regio == "Saarland",
                             paste0("Frauenanteil unter ", title_n, " in Nicht MINT-Fächern im ", regio, " (", timerange, ")"),
                             paste0("Frauenanteil unter ", title_n, " in Nicht MINT-Fächern in ", regio, " (", timerange, ")"))

             df_g <- df_g %>%
               dplyr::mutate(
                 tooltip = paste0(
                   "<b>", geschlecht, "</b><br>",
                   "Anteil: ", prop, " %<br>",
                   "Anzahl: ", display_abs
                 )
               )
             color <- c("Männer" ="#efe8e6", "Frauen" = "#154194")

             quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

             p1g <- piebuilder_plotly(df_g, titel, x = "geschlecht", y = "prop",
                                      color=color, quelle = quelle) |>
               plotly::layout(height = 400)

             out <- list(out, p1g)


           }

        } else if(length(genl) == 2) {

          title_n1 <- genl[1]
          title_n1 <- ifelse(title_n1 == "Studierende", "Studierenden", title_n1)
          title_n1 <- ifelse(title_n1 == "Studierende (Lehramt)", "Studierenden im Lehramt", title_n1)

          title_n2 <- genl[2]
          title_n2 <- ifelse(title_n2 == "Studierende", "Studierenden", title_n2)
          title_n2 <- ifelse(title_n2 == "Studierende (Lehramt)", "Studierenden im Lehramt", title_n2)

          df_1_pie <- df %>%
            dplyr::filter(indikator == genl[1], fach != "Alle Nicht MINT-Fächer")
          df_2_pie <- df %>%
            dplyr::filter(indikator == genl[2], fach != "Alle Nicht MINT-Fächer")

          titel1 = ifelse(regio == "Saarland",
                          paste0("Frauenanteil unter ", title_n1, " in ", titel_help, " im ", regio, " (", timerange, ")"),
                          paste0("Frauenanteil unter ", title_n1, " in ", titel_help, " in ", regio, " (", timerange, ")"))

          titel2 = ifelse(regio == "Saarland",
                          paste0("Frauenanteil unter ", title_n2, " in ", titel_help, " im ", regio, " (", timerange, ")"),
                          paste0("Frauenanteil unter ", title_n2, " in ", titel_help, " in ", regio, " (", timerange, ")"))

          df_1_pie <- df_1_pie %>%
            dplyr::mutate(
              tooltip = paste0(
                "<b>", geschlecht, "</b><br>",
                "Anteil: ", prop, " %<br>",
                "Anzahl: ", display_abs
              )
            )
          df_2_pie <- df_2_pie %>%
            dplyr::mutate(
              tooltip = paste0(
                "<b>", geschlecht, "</b><br>",
                "Anteil: ", prop, " %<br>",
                "Anzahl: ", display_abs
              )
            )
          color <- c("Männer" ="#efe8e6", "Frauen" = "#154194")
          quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

          p1 <- piebuilder_plotly(df_1_pie, titel1, x = "geschlecht", y = "prop",
                                  color=color, quelle = quelle)
          p2 <- piebuilder_plotly(df_2_pie, titel2, x = "geschlecht", y = "prop",
                           color=color, quelle = quelle)

          out <- list(p1, p2)


          if(gegenwert == "Ja"){

            title_n1 <- genl[1]
            title_n1 <- ifelse(title_n1 == "Studierende", "Studierenden", title_n1)
            title_n1 <- ifelse(title_n1 == "Studierende (Lehramt)", "Studierenden im Lehramt", title_n1)


            title_n2 <- genl[2]
            title_n2 <- ifelse(title_n2 == "Studierende", "Studierenden", title_n2)
            title_n2 <- ifelse(title_n2 == "Studierende (Lehramt)", "Studierenden im Lehramt", title_n2)


            df1_g <- df[df$fach == "Alle Nicht MINT-Fächer" & df$indikator == genl[1],]
            df2_g <- df[df$fach == "Alle Nicht MINT-Fächer" & df$indikator == genl[2],]

            titel <- ifelse(regio == "Saarland",
                            paste0("Frauenanteil unter ", title_n1, " in Nicht-MINT-Fächern im ", regio, " (", timerange, ")"),
                            paste0("Frauenanteil unter ", title_n1, " in Nicht-MINT-Fächern in ", regio, " (", timerange, ")"))

            df1_g <- df1_g %>%
              dplyr::mutate(
                tooltip = paste0(
                  "<b>", geschlecht, "</b><br>",
                  "Anteil: ", prop, " %<br>",
                  "Anzahl: ", display_abs
                )
              )

            quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

            color <- c("Männer" ="#efe8e6", "Frauen" = "#154194")
            p1g <- piebuilder_plotly(df1_g, titel, x = "geschlecht", y = "prop",
                                     color=color, quelle = quelle) |>
              plotly::layout(height = 400)


            titel2 <- ifelse(regio == "Saarland",
                             paste0("Frauenanteil unter ", title_n2, " in Nicht-MINT-Fächern im ", regio, " (", timerange, ")"),
                             paste0("Frauenanteil unter ", title_n2, " in Nicht-MINT-Fächern in ", regio, " (", timerange, ")"))
            df2_g <- df2_g %>%
              dplyr::mutate(
                tooltip = paste0(
                  "<b>", geschlecht, "</b><br>",
                  "Anteil: ", prop, " %<br>",
                  "Anzahl: ", display_abs
                )
              )
            p2g <- piebuilder_plotly(df2_g, titel2, x = "geschlecht", y = "prop",
                                     color=color, quelle = quelle) |>
              plotly::layout(height = 400)

            out <- list(p1, p2, p1g, p2g)

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
    #if(gegenwert == "Ja") sel_f1 <- c(sel_f1, "Alle Nicht MINT-Fächer")

    # if (gegenwert == "Ja") {
    #   sel_f1 <- c(sel_f1, "Alle Nicht MINT-Fächer")
    # }

    gegenwert <- trimws(as.character(r$gen_gegenwert_balken))

    sel_f_query <- sel_f1

    if (isTRUE(gegenwert == "Ja")) {
      sel_f_query <- unique(c(sel_f_query, "Alle Nicht MINT-Fächer"))
    }


    df_query <- glue::glue_sql("
      SELECT region, fach, jahr, indikator, geschlecht, wert
      FROM studierende_detailliert
      WHERE jahr = ({timerange*})
      AND region = {sel_bl1}
      AND fach IN ({sel_f_query*})
      ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)


    praep <- ifelse(sel_bl1 == "Saarland", " im ", " in ")


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
      dplyr::right_join(df_gen, by = c("region", "fach", "jahr", "indikator", "geschlecht"))%>%
      dplyr::filter(!is.nan(proportion))


    #Trennpunkte für lange Zahlen ergänzen
    df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$display_rel <- prettyNum(df$proportion, big.mark = ".", decimal.mark = ",")

    #überschrift vorbereiten
    fach_label <- sel_f1
    fach_label <- ifelse(fach_label == "Alle MINT-Fächer", "MINT", fach_label)
    if(gegenwert == "Ja"){
      titel <- paste0("Frauenanteil in der Fachgruppe ", fach_label, " und allen Nicht-MINT-Fächern", praep, sel_bl1, " (", timerange, ")")
      }else{
      titel <- paste0("Frauenanteil in der Fachgruppe ",
                      ifelse(
                        fach_label %in% c("Wirtschaftsingenieurwesen mit ingenieurwissenschaftlichem Schwerpunkt",
                                          "allgemeine naturwissenschaftliche und mathematische Fächer"),
                        paste0(fach_label), fach_label),
                      praep, sel_bl1, " (", timerange, ")")
    }


    df$fach_indikator <- paste(df$indikator, df$fach, sep = "- ")


    df <- df %>%
      dplyr::mutate(
        fach_indikator_name = dplyr::case_when(
          stringr::str_length(fach_indikator) > 60 ~
            paste0(
              stringr::str_trunc(fach_indikator, 55)
            ),
          TRUE ~ fach_indikator
        ),
        fach_indikator_name = as.character(fach_indikator_name)
      )



    df <- df[with(df, order(proportion, decreasing = TRUE)), ]

    #da es viele NAs gibt, also Daten, die nicht berechnet wurden und können denke ich, gibt es ein filtering
    df <- df %>%
      dplyr::filter(!is.na(proportion), !is.na(wert))

    order <- df %>%
      dplyr::filter(geschlecht == "Frauen") %>%
      dplyr::arrange(dplyr::desc(proportion)) %>%
      dplyr::pull(fach_indikator) %>%
      unique()

    if (gegenwert == "Ja") {
      nicht_mint <- order[grepl("Alle Nicht MINT-Fächer", order)]
      order <- c(nicht_mint, setdiff(order, nicht_mint))
    }

    df <- df %>%
      dplyr::mutate(
        geschlecht = factor(geschlecht, levels = c("Frauen", "Männer"))
      ) %>%
      dplyr::arrange(geschlecht) %>%
      dplyr::mutate(
        .tooltip = paste0(
          "<b><span style='font-size:15px;'>", indikator, "</span></b><br>",
          "<span style='font-size:15px;'>", fach, "</span><br>",
          "<span style='font-size:15px;'>", geschlecht, "</span><br>",
          "Anteil: ", proportion,"%<br>",
          "Anzahl: ", formatC(as.numeric(wert), format = "f", digits = 0, big.mark = "."))
      )


    x <- "fach_indikator"
    y <- "proportion"
    group <- "geschlecht"
    quelle <- "Quelle der Daten: Destatis, 2025,, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    quelle_y <- -0.15
    legend_y <- -0.06

    color <- c("Frauen" = "#154194", "Männer" = "#efe8e6")

    out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h", group=group, color=color, order=order,
                                tickvals = df$fach_indikator, ticktext = df$fach_indikator_name,
                                percent = TRUE,stacking=TRUE, legend_y=legend_y, quelle_y=quelle_y, quelle=quelle)%>%
      plotly::layout(
        margin = list(t = 80))



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

    out <- linebuilder_plotly(df, titel = titel, x = "jahr", y = "wert", group = "geschlecht")

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

    df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

    fach_help <- faecher
    fach_help <- ifelse(fach_help == "Alle MINT-Fächer", "MINT", fach_help)
    fach_help <- ifelse(fach_help == "Alle Nicht MINT-Fächer", "Nicht-MINT", fach_help)

    df <- df %>% dplyr::filter(indikator %in% label_sel)

    sorted_indicators <- df %>%
      dplyr::group_by(indikator) %>%
      dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
      dplyr::arrange(m_value) %>%
      dplyr::pull(indikator)

    df$indikator <- factor(df$indikator, levels = sorted_indicators)


    titel <-  ifelse(regio =="Saarland",
                     paste0("Frauenanteil in der Studienfachgruppe ", fach_help, " im ", regio),
                     paste0("Frauenanteil in der Studienfachgruppe ", fach_help, " in ", regio))
    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", indikator, "</b><br>",
          "Jahr: ", jahr, "<br>",
          "Anteil: ", prettyNum(wert, big.mark = ".", decimal.mark = ","), " %"
        )
      )
    color <- c("#b16fab", "#154194", "#66cbaf", "#fcc433")[1:length(unique(df$indikator))]

    quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    out <- linebuilder_plotly(df, titel, x = "jahr", y = "wert", group = "indikator", color = color, quelle = quelle)

     }else if(absolut_selector=="Anzahl"){


      fach_help <- faecher
      fach_help <- ifelse(fach_help == "Alle MINT-Fächer", "MINT", fach_help)
      fach_help <- ifelse(fach_help == "Alle Nicht MINT-Fächer", "Nicht-MINT", fach_help)

      df <- df%>%
        dplyr::filter(selector=="Anzahl")

      df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

      df <- df %>% dplyr::filter(indikator %in% label_sel)

      sorted_indicators <- df %>%
        dplyr::group_by(indikator) %>%
        dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
        dplyr::arrange(m_value) %>%
        dplyr::pull(indikator)

      df$indikator <- factor(df$indikator, levels = sorted_indicators)

      titel <- ifelse(regio == "Saarland",
                      paste0("Frauenanteil in der Studienfachgruppe ", fach_help, " im ", regio),
                      paste0("Frauenanteil in der Studienfachgruppe ", fach_help, " in ", regio))

      df <- df %>%
        dplyr::mutate(
          tooltip = paste0(
            "<b>", indikator, "</b><br>",
            "Jahr: ", jahr, "<br>",
            "Anzahl: ", prettyNum(wert, big.mark = ".", decimal.mark = ",")
          )
        )
      format <- ",d"
      color <- c("#b16fab", "#154194", "#66cbaf", "#fcc433")[1:length(unique(df$indikator))]
      quell <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
      out <- linebuilder_plotly(df, titel, x = "jahr", y = "wert", group = "indikator", format = format, color = color, quelle = quell)

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


    color_fachbereich <- c("#efe8e6", "#00a87a", "#fcc433" )

    if(vergl == "Ja"){
      gen <- c("Frauen", "Männer")
    } else{
      gen <- "Frauen"
    }

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
    df$wert <- prettyNum(df$wert, big.mark=".", decimal.mark = ",")

    titel_gruppe <- dplyr::case_when(
      lab_cho == "Studierende" ~ "weiblichen Studierenden",
      lab_cho == "Studienanfänger:innen (1. Hochschulsemester)" ~ "weiblichen Studienanfängerinnen",
      lab_cho == "Studierende (Lehramt)" ~ "weiblichen Lehramtsstudierenden",
      lab_cho == "Absolvent:innen" ~ "weiblichen Absolventinnen",
      TRUE ~ "Frauen"
    )

    titel_gruppe_m <- dplyr::case_when(
      lab_cho == "Studierende" ~ "männlichen Studierenden",
      lab_cho == "Studienanfänger:innen (1. Hochschulsemester)" ~ "männlichen Studienanfängern",
      lab_cho == "Studierende (Lehramt)" ~ "männlichen Lehramtsstudierenden",
      lab_cho == "Absolvent:innen" ~ "männlichen Absolventen",
      TRUE ~ "Männern"
    )
    titel <- ifelse(
      regio == "Saarland",
      paste0("Studienfachwahl von ", titel_gruppe, " im ", regio, " (", timerange, ")"),
      paste0("Studienfachwahl von ", titel_gruppe, " in ", regio, " (", timerange, ")")
    )

    titelm <- ifelse(
      regio == "Saarland",
      paste0("Studienfachwahl von ", titel_gruppe_m, " im ", regio, " (", timerange, ")"),
      paste0("Studienfachwahl von ", titel_gruppe_m, " in ", regio, " (", timerange, ")")
    )

    color <- color_fachbereich


    df_f <- df %>% dplyr::filter(geschlecht == "Frauen")
    df_m <- df %>% dplyr::filter(geschlecht == "Männer")


    reihenfolge <- c(
      "andere Fachbereiche",
      "Ingenieurwissenschaften (inkl. Informatik)",
      "Mathematik, Naturwissenschaften"
    )

    df_f <- df_f %>%
      dplyr::mutate(fach = factor(fach, levels = reihenfolge)) %>%
      dplyr::arrange(fach)

    df_m <- df_m %>%
      dplyr::mutate(fach = factor(fach, levels = reihenfolge)) %>%
      dplyr::arrange(fach)


    if(vergl == "Ja"){

      df_m <- df_m %>%
        dplyr::mutate(
          tooltip = paste0(
            "<b>", geschlecht, "</b><br>",
            "Anteil: ", prop, " %<br>",
            "Anzahl: ", wert
          )
        )

      df_f <- df_f %>%
        dplyr::mutate(
          tooltip = paste0(
            "<b>", geschlecht, "</b><br>",
            "Anteil: ", prop, " %<br>",
            "Anzahl: ", wert
          )
        )


      subtitel <- paste0("Von allen ", titel_gruppe, " wählen ", 100-df_f$prop[df_f$fach=="andere Fachbereiche"],
                         " % ein MINT-Fach.")
      subtitelm <- paste0("Von allen ", titel_gruppe_m, " wählen ", 100-df_m$prop[df_m$fach=="andere Fachbereiche"],
                          " % ein MINT-Fach.")


      quelle <- "Quelle: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

      p1 <- piebuilder_plotly(df_f, titel, x="fach", y="prop", quelle_y= -0.04, legend_y = -0.04,
                       color=color_fachbereich, subtitel = subtitel, quelle = "") |>
        plotly::layout(
          annotations = list(
            list(
              text = quelle,
              x = 1,
              y = -0.25,
              xref = "paper",
              yref = "paper",
              xanchor = "right",
              yanchor = "top",
              showarrow = FALSE,
              font = list(size = 11, color = "gray", family = "Calibri Regular", align = "right")
            )
          ),
          margin = list(t = 140, b = 120, r = 50, l = 50)
        )

      p2 <- piebuilder_plotly(df_m, titelm, x="fach", y="prop",
                              color=color_fachbereich, subtitel = subtitelm, quelle = "") |>
        plotly::layout(
          annotations = list(
            list(
              text = quelle,
              x = 1,
              y = -0.6,
              xref = "paper",
              yref = "paper",
              xanchor = "right",
              yanchor = "top",
              showarrow = FALSE,
              font = list(size = 11, color = "gray", family = "Calibri Regular", align = "right")
            )
          ),
          margin = list(t = 135, b = 120, r = 50, l = 50),
          height = 400
        )

      out <- list(p1, p2)

    }else{

      reihenfolge <- c(
        "andere Fachbereiche",
        "Ingenieurwissenschaften (inkl. Informatik)",
        "Mathematik, Naturwissenschaften"
      )

      df <- df %>%
        dplyr::mutate(fach = factor(fach, levels = reihenfolge)) %>%
        dplyr::arrange(fach)





      subtitel <- paste0("Von allen ", titel_gruppe, " wählen ", 100-df$prop[df$fach=="andere Fachbereiche"],
                         " % ein MINT-Fach.")

      df <- df %>%
        dplyr::mutate(
          tooltip = paste0(
            "<b>", geschlecht, "</b><br>",
            "Anteil: ", prop, " %<br>",
            "Anzahl: ", wert
          )
        )
      quelle <- "Quelle: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

      out <- piebuilder_plotly(df, titel, x = "fach", y ="prop",
                               color=color_fachbereich, subtitel = subtitel, quelle=quelle) |>
        plotly::layout(
          margin = list(t = 120, b = 120, r = 50, l = 50)
        )

    }

  } else if(betrachtung == "Zeitverlauf - Liniendiagramm"){

    # load UI inputs from reactive value
    timerange <- r$choice_V_y
    t <- timerange[1]:timerange[2]
    v_lab <- r$choice_l_v
    absolut_selector <- r$abs_zahlen_l_v
    subjects_select <- r$choice_v_f
    states <- r$choice_states


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

      sorted_indicators <- df %>%
        dplyr::group_by(indikator) %>%
        dplyr::summarize(m_value = mean(round(prop, 1), na.rm = TRUE)) %>%
        dplyr::arrange(m_value) %>%
        dplyr::pull(indikator)

      df$indikator <- factor(df$indikator, levels = sorted_indicators)

      #Trennpunkte für lange Zahlen ergänzen

      df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")
      df$display_wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
      max_c <- max(df$prop) + 3
      min_c <- min(df$prop) - 3

      subtitel <- paste0("Von allen weiblichen Studierenden bzw. Absolvent:innen hat ein so großer Anteil ein MINT-Fach belegt.")
      titel <- titel
      df <- df %>%
        dplyr::mutate(
          tooltip = paste0(
            "<b>", indikator, "</b><br>",
            "Jahr: ", jahr, "<br>",
            "Anteil: ", display_rel, " %"
          )
        )

      color <- c("#b16fab", "#154194", "#66cbaf", "#fbbf24",
                 "#AFF3E0","#2D6BE1","#008F68","#8893a7", "#ee7775", "#9d7265", "#35bd97",
                 "#bfc6d3", "#5f94f9",  "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
      quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

      out <- linebuilder_plotly(df, titel, x = "jahr", y = "prop", group = "indikator",
                                color = color, quelle = quelle, subtitel = subtitel)


    }else if(absolut_selector=="Anzahl"){

      df <- df[with(df, order( jahr, decreasing = FALSE)), ]

      sorted_indicators <- df %>%
        dplyr::group_by(indikator) %>%
        dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
        dplyr::arrange(m_value) %>%
        dplyr::pull(indikator)

      df$indikator <- factor(df$indikator, levels = sorted_indicators)


      df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

      subtitel <- paste0("Von allen weiblichen Studierenden bzw. Absolvent:innen hat eine so große Anzahl ein MINT-Fach belegt.")

      titel <- titel

      df <- df %>%
        dplyr::mutate(
          tooltip = paste0(
            "<b>", indikator, "</b><br>",
            "Jahr: ", jahr, "<br>",
            "Anzahl: ", display_abs
          )
        )

      format <- ",d"
      color <- c("#b16fab", "#154194", "#66cbaf", "#fbbf24","#AFF3E0","#2D6BE1","#008F68","#8893a7", "#ee7775", "#9d7265", "#35bd97",
                 "#bfc6d3", "#5f94f9",  "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
      quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

      out <- linebuilder_plotly(df, titel, x = "jahr", y = "wert", group = "indikator",
                                format = format, color = color, quelle = quelle,
                                subtitel = subtitel)


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

  praep <- ifelse(states == "Saarland", "im", "in")


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



  df <- df %>%
    dplyr::mutate(
      fach_short = stringr::str_trunc(
        as.character(fach),
        width = 30
      )
    )



# Split dataframe by gender and create plots

  if(abs_rel == "In Prozent"){

    df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")


    # female
    studierende_faecher_frauen <- df %>%
      dplyr::filter(geschlecht == "Frauen")%>%
      dplyr::arrange(desc(prop))%>%
      dplyr::slice(1:10)

    # male
    studierende_faecher_maenner <- df %>%
      dplyr::filter(geschlecht == "Männer") %>%
      dplyr::arrange(desc(prop)) %>%
      dplyr::slice(1:10)



# Create female plot

    titel <- paste0("Fächer mit dem höchsten Frauenanteil ", praep," ", states , " (", timerange, ")")



    order <- unique(studierende_faecher_frauen$fach)


    studierende_faecher_frauen <- studierende_faecher_frauen %>%
      dplyr::mutate(
        .tooltip = paste0(
          "<b><span style='font-size:15px;'>", fach, "</span></b><br>",
          "Anzahl: ", formatC(as.numeric(wert), format = "f", digits = 0, big.mark = "."), "<br>",
          "Anteil: ", prop, " %"
        )
      )


    x <- "fach"
    y <- "prop"

    color <- c("#154194")
    quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    quelle_y <- -0.20

    plot_female <- balkenbuilder_plotly(df=studierende_faecher_frauen, x=x, y=y, titel=titel, orientation = "h", group=NULL, color = color,
                                tickvals = df$fach, ticktext = df$fach_short, wrap_width = 40,
                                order = order, stacking = FALSE, percent = TRUE, quelle=quelle, quelle_y=quelle_y)




# Create male plot

    titel <- paste0("Fächer mit dem höchsten Männeranteil ", praep, " ", states, " (", timerange, ")")


    order <- unique(studierende_faecher_maenner$fach)


    studierende_faecher_maenner <- studierende_faecher_maenner %>%
      dplyr::mutate(
        .tooltip = paste0(
          "<b><span style='font-size:15px;'>", fach, "</span></b><br>",
          "Anzahl: ", formatC(as.numeric(wert), format = "f", digits = 0, big.mark = "."), "<br>",
          "Anteil: ", prop, " %"
        )
      )


    x <- "fach"
    y <- "prop"

    color <- c("#66cbaf")
    quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    quelle_y <- -0.20

    plot_male <- balkenbuilder_plotly(df=studierende_faecher_maenner, x=x, y=y, titel=titel, orientation = "h", group=NULL, color = color,
                                      tickvals = df$fach, ticktext = df$fach_short, wrap_width = 40,
                                      order = order, stacking = FALSE, percent = TRUE, quelle=quelle, quelle_y=quelle_y)







  } else if(abs_rel == "Anzahl"){

    df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
    df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")

    studierende_faecher_frauen <- df %>%
      dplyr::filter(geschlecht == "Frauen") %>%
      dplyr::arrange(desc(wert)) %>%
      dplyr::slice(1:10)

   studierende_faecher_maenner <- df %>%
      dplyr::filter(geschlecht == "Männer") %>%
      dplyr::arrange(desc(wert)) %>%
      dplyr::slice(1:10)


    # Create female plot
    titel <- paste0("Am häufigsten gewählte Fächer von Frauen ", praep, " ", states, " (", timerange, ")")




    order <- unique(studierende_faecher_frauen$fach)


    studierende_faecher_frauen <- studierende_faecher_frauen %>%
      dplyr::mutate(
        .tooltip = paste0(
          "<b><span style='font-size:15px;'>", fach, "</span></b><br>",
          "Anzahl: ", formatC(as.numeric(wert), format = "f", digits = 0, big.mark = "."), "<br>",
          "Anteil: ", prop, " %"
        )
      )


    x <- "fach"
    y <- "wert"

    color <- c("#154194")
    quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    quelle_y <- -0.20

    plot_female <- balkenbuilder_plotly(df=studierende_faecher_frauen, x=x, y=y, titel=titel, orientation = "h", group=NULL, color = color,
                                        tickvals = df$fach, ticktext = df$fach_short, wrap_width = 40,
                                        order = order, stacking = FALSE, percent = FALSE, quelle=quelle, quelle_y=quelle_y)








       # Create male plot
    titel <- paste0("Am häufigsten gewählte Fächer von Männern ",praep, " ", states, " (", timerange, ")")


    order <- unique(studierende_faecher_maenner$fach)


    studierende_faecher_maenner <- studierende_faecher_maenner %>%
      dplyr::mutate(
        .tooltip = paste0(
          "<b><span style='font-size:15px;'>", fach, "</span></b><br>",
          "Anzahl: ", formatC(as.numeric(wert), format = "f", digits = 0, big.mark = "."), "<br>",
          "Anteil: ", prop, " %"
        )
      )


    x <- "fach"
    y <- "wert"

    color <- c("#66cbaf")
    quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    quelle_y <- -0.20

    plot_male <- balkenbuilder_plotly(df=studierende_faecher_maenner, x=x, y=y, titel=titel, orientation = "h", group=NULL, color = color,
                                      tickvals = df$fach, ticktext = df$fach_short, wrap_width = 40,
                                      order = order, stacking = FALSE, percent = FALSE, quelle=quelle, quelle_y=quelle_y)



  }



  out <- list(plot_female, plot_male)

  return(out)

}





### Tab 5 ----
plot_mint_faecher_frauen <- function(r){

  # load UI inputs from reactive value
  timerange <- r$jahr_mint_fach_frauen
  regio <- r$region_mint_fach_frauen

  label_w <- r$gruppe_mint_fach_balken_frauen

  ebene <- r$ebene_mint_fach_frauen
  praep <- ifelse(regio == "Saarland","im","in")
  labelll <- label_w

  label_w <- gsub("weibliche ", "", label_w)

  label_titel <- dplyr::case_when(
    label_w[1] == "Studierende" ~ "Studierenden",
    label_w[1] == "Studierende (Lehramt)" ~ "Lehramtsstudierenden",
    label_w[1] == "Studienanfänger:innen (1. Hochschulsemester)" ~ "Studienanfängerinnen",
    label_w[1] == "Absolvent:innen" ~ "Absolventinnen",
    TRUE ~ label_w[1]
  )

  color_fachbereich <- c(
    "Ingenieurwissenschaften (inkl. Informatik)" = "#00a87a",
    "Mathematik, Naturwissenschaften" = "#fcc433",
    "Alle Nicht MINT-Fächer" = "#efe8e6",
    "Humanmedizin/Gesundheitswissenschaften" = "#AFF3E0",
    "Geisteswissenschaften" = "#2D6BE1",
    "Kunst, Kunstenwissenschaft" = "#008F68",
    "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin" = "#EFFFF7",
    "Außerhalb der Studienbereichsgliederung/Sonstige Fächer" = "#35BD97",
    "Rechts- Wirtschafts- und Sozialwissenschaften" = "#F59E0B",
    "Alle Fächer" = "#FEF3C7",
    "Sport" = "#004331",
    "Alle MINT-Fächer" = "#ee7775"
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

  praep <- ifelse(regio == "Saarland", " im ", " in ")

  # filter dataset based on UI inputs
  if(ebene == "MINT-Fächergruppen"){

    if (length(label_w) == 0) {
      stop("Fehler: label_w ist leer und verursacht eine ungültige SQL-Abfrage.")
    }

    df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr == {timerange}
        AND typ = 'Einzelauswahl'
        AND geschlecht = 'Frauen'
        AND indikator IN ({label_w*})
        AND region = {regio}
        AND mint_select = 'MINT'
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)


    df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr == {timerange}
        AND geschlecht = 'Gesamt'
        AND indikator IN ({label_w*})
        AND typ = 'Einzelauswahl'
        AND region = {regio}
        AND mint_select = 'MINT'
                               ", .con = con)

    alle <- DBI::dbGetQuery(con, df_query)

  }
  else{

    if (length(label_w) == 0) {
      stop("Fehler: label_w ist leer und verursacht eine ungültige SQL-Abfrage.")
    }

    df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr == {timerange}
        AND geschlecht = 'Frauen'
        AND indikator IN ({label_w*})
        AND region = {regio}
        AND ((mint_select = 'MINT' AND typ = 'Aggregat') OR fachbereich = 'Nicht MINT')
                               ", .con = con)

    df <- DBI::dbGetQuery(con, df_query)


    df_query <- glue::glue_sql("
        SELECT *
        FROM studierende_detailliert
        WHERE jahr == {timerange}
        AND geschlecht = 'Gesamt'
        AND indikator IN ({label_w*})
        AND region = {regio}
        AND ((mint_select = 'MINT' AND typ = 'Aggregat') OR fachbereich = 'Nicht MINT')
                               ", .con = con)

    alle <- DBI::dbGetQuery(con, df_query)

  }


  if(ebene == "MINT-Fächergruppen"){

  df <- df %>%
    # dplyr::anti_join(df, alle, by = c("region", "jahr", "bereich", "indikator", "mint_select", "typ", "fachbereich", "fach")) %>%
    dplyr::left_join(alle,
                     by = c("region", "jahr", "bereich", "indikator", "mint_select", "typ", "fachbereich", "fach")) %>%
    dplyr::rename(
      wert = wert.x,
      wert_ges = wert.y
    ) %>%
    dplyr::mutate(prop = round(wert / wert_ges * 100, 1))

  }

  else {

    df <- df %>%
      dplyr::left_join(alle,
                       by = c("region", "jahr", "bereich", "indikator", "fach", "fachbereich",
                              "typ", "mint_select")) %>%
      dplyr::rename(
        wert = wert.x,
        wert_ges = wert.y
      ) %>%
      dplyr::select(-geschlecht.x, -geschlecht.y) %>%
      dplyr::mutate(prop = round(wert / wert_ges * 100, 1))
  }


  #df vorbeiten für Plot-Darstellung
  df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")


  df <- df[with(df, order(prop, decreasing = FALSE)), ]

  if(ebene == "MINT-Fachbereiche"){
    df <- df %>%
      dplyr::mutate(color = color_fachbereich[fach])
  }else{
    df <- df %>%
      dplyr::mutate(color = color_fach_pie[fach])
  }

    df <- df[with(df, order(prop, decreasing = TRUE)), ]

    label_titel <- gsub("Studierende", "Studierenden", label_w)

    if(ebene == "MINT-Fachbereiche"){

      color <- color_fachbereich
      titel <- paste0("Anteil der weiblichen ", label_titel,
                      if (label_w == "Studienanfänger:innen (1. Hochschulsemester)") " nach Fachbereich" else " nach Fachbereich",
                      praep, regio, " (", timerange, ")")
      }else{

      color = color_fach_balken
      titel <- paste0("Anteil der weiblichen ", label_titel,
               if (label_w == "Studienanfänger:innen (1. Hochschulsemester)") " in allen MINT-Fächergruppen" else " in allen MINT-Fächergruppen",
              praep, regio, " (", timerange, ")")
      }

# plot


     order <- unique(df$fach)

     df <- df %>%
       dplyr::mutate(
         .tooltip = paste0(
           "<span style='font-size:15px;'><b>",fach, "</span></b><br>",
           "Anteil: ", round(prop, 1), " %<br>",
           "Anzahl: ", wert
         ))


     x <- "fach"
     y <- "prop"

     quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
     quelle_y <- -0.15


     out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h",percent=TRUE, color=color,
                                 order=order, stacking = FALSE,quelle_y=quelle_y, quelle=quelle)%>%
       plotly::layout(
         margin = list(t = 80))



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


  indikator_in <- c(
    "internationale Studienanfänger:innen (1. Hochschulsemester)",
    "internationale Studierende",
    "Studienanfänger:innen (1. Hochschulsemester)",
    "Studierende",
    "Absolvent:innen",
    "internationale Absolvent:innen"
  )

  df_query <- glue::glue_sql("
    SELECT region, fachbereich, fach, jahr, indikator, wert
    FROM studierende_detailliert
    WHERE jahr = {year_select}
      AND region = {bl_select}
      AND geschlecht = 'Gesamt'
      AND indikator IN ({indikator_in*})
  ", .con = con)


  df <- DBI::dbGetQuery(con, df_query)


  df_plot <- df %>%
    tidyr::pivot_wider(
      names_from = indikator,
      values_from = wert
    ) %>%
    dplyr::mutate(
      `deutsche Studierende` =
        `Studierende` - `internationale Studierende`,

      `deutsche Studienanfänger:innen (1. Hochschulsemester)` =
        `Studienanfänger:innen (1. Hochschulsemester)` -
        `internationale Studienanfänger:innen (1. Hochschulsemester)`,

      `deutsche Absolvent:innen` =
        `Absolvent:innen` - `internationale Absolvent:innen`
    ) %>%
    dplyr::select(
      region, fachbereich, fach, jahr,
      dplyr::matches("^(deutsche|internationale)")
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::matches("^(deutsche|internationale)"),
      names_to = "indikator_raw",
      values_to = "wert_abs"
    ) %>%
    dplyr::mutate(
      ausl_detect = dplyr::case_when(
        stringr::str_detect(indikator_raw, "^internationale") ~ "International",
        TRUE ~ "Deutsch"
      ),
      indikator = indikator_raw %>%
        stringr::str_remove("^deutsche ") %>%
        stringr::str_remove("^internationale ")
    ) %>%
    dplyr::group_by(region, fach, jahr, indikator) %>%
    dplyr::mutate(
      wert_prozent = wert_abs / sum(wert_abs, na.rm = TRUE) * 100,
      wert_plot = if (absolut_selector == "In Prozent") wert_prozent else wert_abs
      ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(indikator == status_select)




  df_fachbereich <- df_plot %>%
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


  df_faecher <- df_plot %>%
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


  df_plot <- if (betr_ebene == "Fachbereiche") {
    df_fachbereich
  } else {
    df_faecher
  }




  praep <- ifelse(bl_select == "Saarland", " im ", " in ")

  # Vorbereitung Überschrift
  help <- "Studierender"
  help <- ifelse(grepl("anfänger", status_select), "Studienanfänger:innen", help)

  help2 <- "Studierenden"
  help2 <- ifelse(grepl("anfänger", status_select), "Studienanfänger:innen", help2)




       titel <- paste0("Anteil internationaler ", help, " an allen ", help2, praep, bl_select,  " (",year_select, ")" )




       df_plot <- df_plot %>%
         dplyr::filter(!is.na(fach), !is.na(wert_plot)) %>%
         dplyr::mutate(
           ausl_detect = factor(ausl_detect, levels = c("International", "Deutsch")),
           .tooltip = paste0(
             "<b><span style='font-size:15px;'>", fach, "</span></b><br>",
             "<span style='font-size:15px;'>", ausl_detect,"e ",indikator, "</span><br>",
             "Anteil: ", round(wert_prozent, 1), " %<br>",
             "Anzahl: ", formatC(as.numeric(wert_abs), format = "f", digits = 0, big.mark = "."))
           )

       order <- df_plot %>%
         dplyr::filter(ausl_detect == "International") %>%
         dplyr::arrange(
           dplyr::case_when(
             betr_ebene == "Fachbereiche" & fach == "Alle Fächer" ~ 1,
             betr_ebene == "Fachbereiche" & fach == "Alle MINT-Fächer" ~ 2,
             betr_ebene == "Fachbereiche" & fach == "Alle Nicht MINT-Fächer" ~ 3,
             TRUE ~ 4
           ),
           dplyr::desc(wert_plot)) %>%
         dplyr::pull(fach)



       x <- "fach"


       y <- "wert_plot"
       group <- "ausl_detect"
       color <- c("Deutsch" = "#efe8e6", "International" = "#66cbaf")

       quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
       quelle_y <- -0.15
       legend_y <- -0.07

       percent  <- if (absolut_selector == "In Prozent") TRUE else FALSE
       stacking <- if (absolut_selector == "In Prozent") TRUE else FALSE


       out <- balkenbuilder_plotly(df=df_plot, x=x, y=y, titel=titel, orientation = "h",percent=percent, group=group, color=color,
                                   order=order, stacking = stacking,quelle_y=quelle_y,legend_y=legend_y, quelle=quelle)




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


  df  <- df %>%
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
                                               T~ "deutsch")) %>%
    dplyr::filter(indikator !="Absolvent:innen")

  df$indikator <- gsub("_p", "", df$indikator)
  df$indikator <- gsub("deutsche ", "", df$indikator)
  df$indikator <- gsub("internationale ", "", df$indikator)
  df$ausl_detect  <- factor(df$ausl_detect, levels=c("international", "deutsch"))

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


  praep <- ifelse(bl_select == "Saarland", " im ", " in ")

  # Plot
  if(absolut_selector=="In Prozent"){

    df <- df %>%
      dplyr::filter(selector == absolut_selector)%>%
      dplyr::mutate(dplyr::across(wert, ~round(.*100, 1)))

    df$display_rel <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

    if(betrachtung == "Zeitverlauf - Liniendiagramm"){
      if(status_select == "Absolvent:innen"){

        sorted_indicators <- df %>%
          dplyr::group_by(ausl_detect) %>%
          dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
          dplyr::arrange(m_value) %>%
          dplyr::pull(ausl_detect)

        df$ausl_detect <- factor(df$ausl_detect, levels = sorted_indicators)

        # order years for plot
        df <- df[with(df, order(jahr, decreasing = FALSE)), ]


        titel <-  paste0("Anteil internationaler Absolvent:innen an allen Absolvent:innen in ", fach_help , " in ", bl_select )

        df <- df %>%
          dplyr::mutate(
            tooltip = paste0(
              "<b>", ausl_detect, "</b><br>",
              "Jahr: ", jahr, "<br>",
              "Anteil: ", display_rel, " %"
            )
          )
        color <- c("#154194", "#66cbaf")
        quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt"
        out <- linebuilder_plotly(df, titel, x = "jahr", y = "wert", group = "ausl_detect", color = color, quelle = quelle)


      } else {

        sorted_indicators <- df %>%
          dplyr::group_by(ausl_detect) %>%
          dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
          dplyr::arrange(m_value) %>%
          dplyr::pull(ausl_detect)

        df$ausl_detect <- factor(df$ausl_detect, levels = sorted_indicators)

        titel <-  paste0("Anteil internationaler ", help, " an allen ", help2, " in ", fach_help ,
                         " in ", bl_select )

        df <- df %>%
          dplyr::mutate(
            tooltip = paste0(
              "<b>", ausl_detect, "</b><br>",
              "Jahr: ", jahr, "<br>",
              "Anteil: ", display_rel, " %"
            )
          )

        quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt"
        color <- c("#154194", "#66cbaf")
        out <- linebuilder_plotly(df, titel, x = "jahr", y = "wert", group = "ausl_detect",
                                  color = color, quelle = quelle)
      }

    }else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){

      if (status_select == "Absolvent:innen"){

        df <- df[with(df, order(wert, decreasing = TRUE)), ]

        titel <- paste0("Anteil internationaler Absolvent:innen an allen Absolvent:innen in ", fach_help , praep, bl_select )



        quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

        order <- unique(df$jahr)

        df <- df %>%
          dplyr::mutate(
            .tooltip = paste0(
              "<b><span style='font-size:15px;'>", jahr, "</span></b><br>",
              ausl_detect, "<br>",
              "Anteil: ", wert, " %"
            ))


        x <- "jahr"
        y <- "wert"
        group <- "ausl_detect"
        color <- c("deutsch" = "#efe8e6","international" = "#66cbaf")

        out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "v",percent=TRUE,
                                    group=group, color=color,
                                    order=order, stacking = TRUE, quelle=quelle)



      } else {

        df <- df[with(df, order(wert, decreasing = TRUE)), ]

        titel <- paste0("Anteil internationaler ", help, " an allen ", help2, " in ", fach_help , " in ", bl_select )

        quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

        order <- unique(df$jahr)

        df <- df %>%
          dplyr::mutate(
            .tooltip = paste0(
              "<b><span style='font-size:15px;'>", jahr, "</span></b><br>",
              ausl_detect, "<br>",
              "Anteil: ", wert, " %"
            ))


        x <- "jahr"
        y <- "wert"
        group <- "ausl_detect"
        color <- c("deutsch" = "#efe8e6","international" = "#66cbaf")

        out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "v",percent=TRUE,
                                    group=group, color=color,
                                    order=order, stacking = TRUE, quelle=quelle)


     }

    }

  } else if(absolut_selector=="Anzahl"){

    df <- df %>%
      dplyr::filter(selector == absolut_selector)

    df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

    if(betrachtung == "Zeitverlauf - Liniendiagramm"){

      sorted_indicators <- df %>%
        dplyr::group_by(ausl_detect) %>%
        dplyr::summarize(m_value = mean(round(wert, 1), na.rm = TRUE)) %>%
        dplyr::arrange(m_value) %>%
        dplyr::pull(ausl_detect)

      df$ausl_detect <- factor(df$ausl_detect, levels = sorted_indicators)

      if (status_select == "Absolvent:innen"){

        titel <-    paste0("Anzahl internationaler Absolvent:innen in ", fach_help, praep, bl_select)
        df <- df %>%
          dplyr::mutate(
            tooltip = paste0(
              "<b>", ausl_detect, "</b><br>",
              "Jahr: ", jahr, "<br>",
              "Anteil: ", display_abs
            )
          )
        format <- ",d"
        color <- c("#154194", "#66cbaf")
        quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt"
        out <- linebuilder_plotly(df, titel, x = "jahr", y = "wert", group = "ausl_detect",
                                  format = format, color = color, quelle = quelle)

      } else {

        titel <-  paste0("Anzahl internationaler ", help, " in ", fach_help, praep, bl_select)
        df <- df %>%
          dplyr::mutate(
            tooltip = paste0(
              "<b>", ausl_detect, "</b><br>",
              "Jahr: ", jahr, "<br>",
              "Anteil: ", display_abs
            )
          )
        tooltip <- "{point.ausl_detect} <br> Anzahl: {point.display_abs}"
        format <- ",d"
        color <- c("#154194", "#66cbaf")
        quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt"
        out <- linebuilder_plotly(df, titel, x = "jahr", y = "wert", group = "ausl_detect",
                                  format = format, color = color, quelle = quelle)      }

    }else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){

      if (status_select == "Absolvent:innen"){

        df <- df[with(df, order(wert, decreasing = TRUE)), ]


        titel <- paste0("Anzahl internationaler Absolvent:innen in ", fach_help, praep, bl_select)


        quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

        order <- unique(df$jahr)

        df <- df %>%
          dplyr::mutate(
            .tooltip = paste0(
              "<b><span style='font-size:15px;'>", jahr, "</span></b><br>",
              ausl_detect, "<br>",
              "Anzahl: ",(formatC(as.numeric(wert), format = "f", digits = 0, big.mark = "."))
            ))


        x <- "jahr"
        y <- "wert"
        group <- "ausl_detect"
        color <- c("deutsch" = "#efe8e6","international" = "#66cbaf")

        out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "v",percent=FALSE,
                                    group=group, color=color,
                                    order=order, stacking = FALSE, quelle=quelle)




      } else {

        df <- df[with(df, order(wert, decreasing = TRUE)), ]

        titel <- paste0("Anzahl internationaler ", help, " in ", fach_help, praep, bl_select)



        quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

        order <- unique(df$jahr)

        df <- df %>%
          dplyr::mutate(
            .tooltip = paste0(
              "<b><span style='font-size:15px;'>", jahr, "</span></b><br>",
              ausl_detect, "<br>",
              "Anzahl: ",(formatC(as.numeric(wert), format = "f", digits = 0, big.mark = "."))
            ))


        x <- "jahr"
        y <- "wert"
        group <- "ausl_detect"
        color <- c("deutsch" = "#efe8e6","international" = "#66cbaf")

        out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "v",percent=FALSE,
                                    group=group, color=color,
                                    order=order, stacking = FALSE, quelle=quelle)



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
    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", region, "</b><br>",
          "Anteil: ", display_rel, " %<br>",
          "Anzahl: ", display_abs
        )
      )
    titel <- paste0("MINT-Anteil von ", label_m, " (", timerange, ")")

    quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."

    out <- mapbuilder_plotly(df,
                             titel = titel,
                             value_col = "proportion",
                             quelle=quelle)


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

      sorted_indicators <- df %>%
        dplyr::group_by(region) %>%
        dplyr::summarize(m_value = mean(round(prop, 1), na.rm = TRUE)) %>%
        dplyr::arrange(m_value) %>%
        dplyr::pull(region)

      df$region <- factor(df$region, levels = sorted_indicators)

      df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

      df$display_rel <- prettyNum(df$prop, big.mark = ".", decimal.mark = ",")
      df$display_diff <- prettyNum(df$diff, big.mark = ".", decimal.mark = ",")
      df$display_diff <- ifelse(df$diff < 0, paste0("-", df$display_diff), paste0("+", df$display_diff))


      titel <-  paste0("MINT-Anteil von ", label, " im Zeitverlauf")
      df <- df %>%
        dplyr::mutate(
          tooltip = paste0(
            "<b>", region, "</b><br>",
            "Jahr: ", jahr, "<br>",
            "Wert: ", display_rel, " % <br>",
            "Veränderung zwischen ", timerange[1], " und ", timerange[2],
            ": ", display_diff, " %"
          )
        )
      quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt"
      color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                 "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")

      out <- linebuilder_plotly(df, titel, x = "jahr", y = "prop", group = "region",
                               color = color, quelle = quelle)



    } else if(absolut_selector=="Anzahl"){

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

      df <- df %>%
        dplyr::mutate(
          tooltip = paste0(
            "<b>", region, "</b><br>",
            "Jahr: ", jahr, "<br>",
            "Wert: ", display_abs, " <br>",
            "Veränderung zwischen ", timerange[1], " und ", timerange[2],
            ": ", display_diff, " %"
          )
        )
      format <-  ",d"
      color <- c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                 "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")
      quel123 <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt"

      out <- linebuilder_plotly(df, titel, x = "jahr", y = "wert", group = "region", format = format,
                         color = color, quelle = quel123)


    }

  }

  else if(betrachtung == "Gruppenvergleich - Balkendiagramm"){

    timerange <- r$international_bulas_balken_date
    r_lab1 <- r$international_bulas_balken_l


    df_query <- glue::glue_sql("
      SELECT *
      FROM studierende_detailliert
      WHERE jahr in ({timerange*})
      AND fach IN ('Alle MINT-Fächer','Alle Fächer')
      AND indikator = {r_lab1}
      AND geschlecht = 'Gesamt'
      ", .con = con)

    df_ges <- DBI::dbGetQuery(con, df_query)



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
    r_lab1 <- ifelse(r_lab1 == "internationale Absolvent:innen",
                     "internationalen Absolvent:innen", r_lab1)

    help_l <- r_lab1
    help_l <- ifelse(r_lab1 == "internationalen Studienanfänger:innen (1. Hochschulsemester)",
                     "internationalen Studienanfänger:innen", help_l)
    help_l <- ifelse(r_lab1 == "Studienanfänger:innen (1. Hochschulsemester)", "Studienanfänger:innen", help_l)

    #nicht als funktion, da es 1) zu komplex und 2) besondere feinheiten enthält, die die funktion balkenbuilder überlasten würde
    # Plot


    titel <- paste0( "MINT-Anteil unter den ", r_lab1 , " (", timerange, ")")


    order <- unique(df$region)

    df <- df %>%
      dplyr::mutate(
        .tooltip = paste0(
          "<span style='font-size:15px;'><b>",region, "</span></b><br>",
          "<span style='font-size:15px;'> Alle MINT-Fächer </span><br>",
          "Anteil: ", round(proportion, 1), " %<br>",
          "Anzahl: ", wert
        ))


    x <- "region"
    y <- "proportion"
    color <- c(
      "Deutschland" = "#b16fab",
      "Ostdeutschland (inkl. Berlin)" = "#d3a4d7",
      "Westdeutschland (o. Berlin)" = "#d3a4d7",
      "Baden-Württemberg" = "#A9A9A9",
      "Bayern" = "#A9A9A9",
      "Berlin" = "#A9A9A9",
      "Brandenburg" = "#A9A9A9",
      "Bremen" = "#A9A9A9",
      "Hamburg" = "#A9A9A9",
      "Hessen" = "#A9A9A9",
      "Mecklenburg-Vorpommern" = "#A9A9A9",
      "Niedersachsen" = "#A9A9A9",
      "Nordrhein-Westfalen" = "#A9A9A9",
      "Rheinland-Pfalz" = "#A9A9A9",
      "Saarland" = "#A9A9A9",
      "Sachsen" = "#A9A9A9",
      "Sachsen-Anhalt" = "#A9A9A9",
      "Schleswig-Holstein" = "#A9A9A9",
      "Thüringen" = "#A9A9A9"
    )

    quelle <- "Quelle der Daten: Destatis, 2025, auf Anfrage, eigene Berechnungen durch MINTvernetzt."
    quelle_y <- -0.10


    out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h",percent=TRUE, color=color,
                                order=order, stacking=FALSE, quelle_y=quelle_y, quelle=quelle)%>%
      plotly::layout(
        yaxis = list(dtick = 1),
        margin = list(t = 80, b = 100))



  }


  return(out)

}











