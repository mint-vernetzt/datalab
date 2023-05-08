#' A function to plot a graph.
#'
#' @description A function to create a pie chart for the first box
#' inside the tab "Home".
#'
#' @return The return value is a plot
#' @param df The dataframe "zentral.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
home_einstieg_pie <- function(df,r) {

  dfj <- df
  # load UI inputs from reactive value
  timerange <- "2021"

  indikator_choice_1 <- r$indikator_start_einstieg_1



  # filter dataset based on UI input

  dfk <- dfj %>% dplyr::filter(region == "Deutschland")

  dfk <- dfk %>% dplyr::filter(jahr== timerange)


  # call function to calculate the share of MINT for every "bereich"
  # df <- share_MINT(df)
  #
  # df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")
  #
  #
  # df <- df %>% dplyr::filter(indikator %in% indikator_choice_1)
  #
  # # calculate proportions
  # df <- df %>% dplyr::group_by(indikator, jahr) %>%
  #   dplyr::mutate(props = sum(wert))
  #
  #
  # df <- df %>% dplyr::group_by(indikator, jahr,fachbereich) %>%
  #   dplyr::summarize(proportion = wert/props)
  #
  # df$proportion <- df$proportion * 100

  # dfü <<- df %>% dplyr::filter(jahr == timerange)

  # dfk <- dfü %>% dplyr::filter(region == "Deutschland")

  dfk2 <<- dfk %>% dplyr::filter(geschlecht=="Gesamt")%>%
    dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle"|  fachbereich == "Ingenieurwissenschaften" |fachbereich == "Mathematik_Naturwissenschaften")

  dfk2a <<- dfk2 %>% dplyr::filter(bereich == "Hochschule")%>%
    tidyr::pivot_wider(names_from=fachbereich, values_from=wert)%>%
    dplyr::mutate(MINT=Ingenieurwissenschaften+Mathematik_Naturwissenschaften )%>%
    dplyr::select(-Ingenieurwissenschaften, -Mathematik_Naturwissenschaften)%>%
    tidyr::pivot_longer(c( "Alle", "MINT"), names_to = "fachbereich", values_to="wert")


  dfk2c <<- dfk %>% dplyr::filter(bereich == "Schule")%>%
    dplyr::filter(fachbereich== "MINT" | fachbereich == "Alle Fächer")%>%
    dplyr::mutate(indikator= paste0("Schüler:innen ", .$indikator ))

  dfk2c$fachbereich <- ifelse(grepl("Alle Fächer", dfk2c$fachbereich), "Alle", dfk2c$fachbereich)

  dfk2b <<- dfk2 %>% dplyr::filter(bereich != "Hochschule" & bereich != "Schule")%>%
    unique()


  dfk2_fn <<- dplyr::bind_rows(dfk2b, dfk2a, dfk2c)%>%
    dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle")%>%
    tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
    dplyr::mutate("Nicht MINT" = Alle - MINT)

  #Tennen für Anzeige absoluter Werte
  ##neuen Df erstellen ohne weitere Berechnungen
  dfk2_wert <- dfk2_fn
  dfk2_wert <- dfk2_wert %>%
    dplyr::filter(geschlecht=="Gesamt")%>%
    dplyr::select(- Alle)%>%
    tidyr::pivot_longer(c(MINT, `Nicht MINT`), names_to = "fachbereich", values_to = "wert")

  #Anteil berechnen
  dfk2_fn <- dfk2_fn %>%
    dplyr::mutate(dplyr::across(c(MINT, `Nicht MINT`), ~./Alle*100)) %>%
    dplyr::filter(geschlecht=="Gesamt")%>%
    dplyr::select(- Alle)%>%
    tidyr::pivot_longer(c(MINT, `Nicht MINT`), names_to = "fachbereich", values_to = "proportion")

  #absolute Werte an DF mit Proportionen anhängen
  wert <- dfk2_wert$wert
  dfk2_fn <- cbind(dfk2_fn, wert)

  #Trennpunkte für lange Zahlen ergänzen
  dfk2_fn$wert <- prettyNum(dfk2_fn$wert, big.mark = ".")


  dfk2_fn$proportion <- round_preserve_sum(as.numeric(dfk2_fn$proportion),0)

  dfk2_fn <- dfk2_fn[with(dfk2_fn, order(region, fachbereich, jahr, decreasing = TRUE)), ]

  #here only MINT
  dft <- dfk2_fn %>% dplyr::filter(fachbereich == "MINT")

  dfö <- dft %>% dplyr::filter(indikator %in% indikator_choice_1)


  if(length(indikator_choice_1) == 1) {

    # ensure that proportion sum to 1
    #df$proportion <- round_preserve_sum(as.numeric(df$proportion),0)

    title_help <- helper_title_home(indikator_choice_1)

    df <- dfk2_fn %>% dplyr::filter(indikator == indikator_choice_1)

    highcharter::hw_grid(
      df %>%
        highcharter::hchart(
          "pie", highcharter::hcaes(x = fachbereich, y = proportion)
        ) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f} % <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c( "#efe8e6", "#b16fab")) %>%
        highcharter::hc_title(text = paste0("", indikator_choice_1, " (2021)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),

      ncol = 1,
      browsable = TRUE
    )


  } else if(length(indikator_choice_1) == 2) {

    # filter for UI input and ensure proportions sum to 1
    df_1 <- dfk2_fn %>% dplyr::filter(indikator == indikator_choice_1[1])

    # df_1$proportion <- round_preserve_sum(as.numeric(df_1$proportion),0)

    title_help_1 <- helper_title_home(indikator_choice_1[1])

    df_2 <- dfk2_fn %>% dplyr::filter(indikator == indikator_choice_1[2])

    # df_2$proportion <- round_preserve_sum(as.numeric(df_2$proportion),0)

    title_help_2 <- helper_title_home(indikator_choice_1[2])


    highcharter::hw_grid(
      highcharter::hchart(df_1, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f} % <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
        highcharter::hc_title(text = paste0("", indikator_choice_1[1], " (2021)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),


      highcharter::hchart(df_2, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f} % <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
        highcharter::hc_title(text = paste0("", indikator_choice_1[2], " (2021)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE)),

      ncol = 2,
      browsable = TRUE
    )


  } else if(length(indikator_choice_1) == 3) {

    # filter for UI input and ensure proportions sum to 1

    df_1 <- dfk2_fn %>% dplyr::filter(indikator == indikator_choice_1[1])

    # df_1$proportion <- round_preserve_sum(as.numeric(df_1$proportion),0)

    title_help_1 <- helper_title_home(indikator_choice_1[1])

    df_2 <- dfk2_fn %>% dplyr::filter(indikator == indikator_choice_1[2])

    # df_2$proportion <- round_preserve_sum(as.numeric(df_2$proportion),0)

    title_help_2 <- helper_title_home(indikator_choice_1[2])

    df_3 <- dfk2_fn %>% dplyr::filter(indikator == indikator_choice_1[3])

    # df_3$proportion <- round_preserve_sum(as.numeric(df_3$proportion),0)

    title_help_3 <- helper_title_home(indikator_choice_1[3])


    highcharter::hw_grid(
      highcharter::hchart(df_1, size = 170, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f} % <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
        highcharter::hc_title(text = paste0("", indikator_choice_1[1], " (2021)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),


      highcharter::hchart(df_2, size = 170, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f} % <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
        highcharter::hc_title(text = paste0("", indikator_choice_1[2], " (2021)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE)),

      highcharter::hchart(df_3, size = 170, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f} % <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
        highcharter::hc_title(text = paste0("", indikator_choice_1[3], " (2021)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
        #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),



      ncol = 3,
      browsable = TRUE
    )

  }



  # df11 <<- df %>%
  #   dplyr::rename(anzeige_geschlecht = geschlecht)


  # df2 <- df2 %>%
  #   tidyr::pivot_wider(names_from=anzeige_geschlecht, values_from=wert)%>%
  #   dplyr::mutate(Gesamt=Männer+Frauen)%>%
  #   tidyr::pivot_longer(c("Gesamt", "Frauen", "Männer"), names_to = "anzeige_geschlecht", values_to = "wert")


  # df8 <<- df2 %>%
  #   tidyr::pivot_wider(values_from = wert, names_from = fachbereich)%>%
  #   dplyr::mutate(MINT=Mathematik+Informatik+Physik+Biologie+Chemie,
  #                 "andere Fächer" =`Alle Fächer`- MINT)%>%
  #  tidyr::pivot_longer(c(6:20), values_to = "wert", names_to= "fachbereich")%>%
  #  dplyr::filter(fachbereich=="MINT" | fachbereich == "andere Fächer")
  #
  #
  # df5 <<- df %>% dplyr::filter(bereich != "Schule")
  #
  # #df2<- df2[, colnames(df)]
  #
  # df3<<-  dplyr::bind_rows(df5, df8)

  # df<- df %>%
  #   tidyr::pivot_wider(values_from = wert, names_from = anzeige_geschlecht)%>%
  #   tidyr::pivot_longer(c("Männer","Frauen", "Gesamt"),names_to = "anzeige_geschlecht", values_to= "wert")%>%
  #   dplyr::filter(fachbereich=="MINT" | fachbereich == "andere Fächer")

  # call function to calculate the share of MINT for every "bereich"

  # df <- share_MINT(df11)
  #
  # df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")
  #
  # df <- df %>% dplyr::filter(indikator %in% indikator_choice_1)
  #
  #
  # # calculate proportions for MINT vs. Rest
  # df <- df %>% dplyr::group_by(indikator) %>%
  #   dplyr::mutate(props = sum(wert))
  #
  #
  # df <- df %>% dplyr::group_by(indikator, fachbereich) %>%
  #   dplyr::summarize(proportion = wert/props)
  #
  # df$proportion <- df$proportion * 100

  # create an if statement for the options of plotting 1, 2 or 3 graphs
  # if(length(indikator_choice_1) == 1) {
  #
  #   # ensure that proportion sum to 1
  #   df$proportion <- round_preserve_sum(as.numeric(df$proportion),0)
  #
  #   title_help <- helper_title_home(indikator_choice_1)
  #
  #   highcharter::hw_grid(
  #     df %>%
  #       highcharter::hchart(
  #         "pie", highcharter::hcaes(x = fachbereich, y = proportion)
  #       ) %>%
  #       highcharter::hc_tooltip(
  #         pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
  #       highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
  #       highcharter::hc_title(text = paste0("", indikator_choice_1, " (2020"),
  #                             margin = 45,
  #                             align = "center",
  #                             style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
  #       highcharter::hc_chart(
  #         style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
  #       highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
  #       #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
  #       highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
  #                                              dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),
  #
  #     ncol = 1,
  #     browsable = TRUE
  #   )
  #
  #
  # } else if(length(indikator_choice_1) == 2) {
  #
  #   # filter for UI input and ensure proportions sum to 1
  #   df_1 <- df %>% dplyr::filter(indikator == indikator_choice_1[1])
  #
  #   df_1$proportion <- round_preserve_sum(as.numeric(df_1$proportion),0)
  #
  #   title_help_1 <- helper_title_home(indikator_choice_1[1])
  #
  #   df_2 <- df %>% dplyr::filter(indikator == indikator_choice_1[2])
  #
  #   df_2$proportion <- round_preserve_sum(as.numeric(df_2$proportion),0)
  #
  #   title_help_2 <- helper_title_home(indikator_choice_1[2])
  #
  #
  #   highcharter::hw_grid(
  #     highcharter::hchart(df_1, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
  #       highcharter::hc_tooltip(
  #         pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
  #       highcharter::hc_colors(c("#efe8e6","#b16fab")) %>%
  #       highcharter::hc_title(text = paste0("", indikator_choice_1[1], " (2020)"),
  #                             margin = 45,
  #                             align = "center",
  #                             style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
  #       highcharter::hc_chart(
  #         style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
  #       highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
  #       highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
  #                                              dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),
  #
  #
  #     highcharter::hchart(df_2, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
  #       highcharter::hc_tooltip(
  #         pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
  #       highcharter::hc_colors(c("#efe8e6","#b16fab")) %>%
  #       highcharter::hc_title(text = paste0("", indikator_choice_1[2], " (2020)"),
  #                             margin = 45,
  #                             align = "center",
  #                             style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
  #       highcharter::hc_chart(
  #         style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
  #       highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
  #       #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
  #       highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
  #                                              dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE)),
  #
  #     ncol = 2,
  #     browsable = TRUE
  #   )
  #
  #
  # } else if(length(indikator_choice_1) == 3) {
  #
  #   # filter for UI input and ensure proportions sum to 1
  #
  #   df_1 <- df %>% dplyr::filter(indikator == indikator_choice_1[1])
  #
  #   df_1$proportion <- round_preserve_sum(as.numeric(df_1$proportion),0)
  #
  #   title_help_1 <- helper_title_home(indikator_choice_1[1])
  #
  #   df_2 <- df %>% dplyr::filter(indikator == indikator_choice_1[2])
  #
  #   df_2$proportion <- round_preserve_sum(as.numeric(df_2$proportion),0)
  #
  #   title_help_2 <- helper_title_home(indikator_choice_1[2])
  #
  #   df_3 <- df %>% dplyr::filter(indikator == indikator_choice_1[3])
  #
  #   df_3$proportion <- round_preserve_sum(as.numeric(df_3$proportion),0)
  #
  #   title_help_3 <- helper_title_home(indikator_choice_1[3])
  #
  #
  #   highcharter::hw_grid(
  #     highcharter::hchart(df_1, size = 170, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
  #       highcharter::hc_tooltip(
  #         pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
  #       highcharter::hc_colors(c("#efe8e6","#b16fab")) %>%
  #       highcharter::hc_title(text = paste0("", indikator_choice_1[1], " (2020)"),
  #                             margin = 45,
  #                             align = "center",
  #                             style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
  #       highcharter::hc_chart(
  #         style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
  #       highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
  #       highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
  #                                              dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),
  #
  #
  #     highcharter::hchart(df_2, size = 170, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
  #       highcharter::hc_tooltip(
  #         pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
  #       highcharter::hc_colors(c("#efe8e6","#b16fab")) %>%
  #       highcharter::hc_title(text = paste0("", indikator_choice_1[2], " (2020)"),
  #                             margin = 45,
  #                             align = "center",
  #                             style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
  #       highcharter::hc_chart(
  #         style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
  #       highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
  #       highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
  #                                              dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE)),
  #
  #     highcharter::hchart(df_3, size = 170, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
  #       highcharter::hc_tooltip(
  #         pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
  #       highcharter::hc_colors(c("#efe8e6","#b16fab")) %>%
  #       highcharter::hc_title(text = paste0("", indikator_choice_1[3], " (2020)"),
  #                             margin = 45,
  #                             align = "center",
  #                             style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
  #       highcharter::hc_chart(
  #         style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
  #       highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
  #       #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
  #       highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
  #                                              dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),
  #
  #
  #
  #     ncol = 3,
  #     browsable = TRUE
  #   )
  #
  # }

}


#' A function to plot a graph.
#'
#' @description A function to create a pie chart for the first box
#' inside the tab "Home".
#'
#' @return The return value is a plot
#' @param df The dataframe "zentral.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
home_einstieg_pie_gender <- function(df, df_naa, r) {



  # load UI inputs from reactive value
  timerange <- "2021"

  indikator_choice_1_gender <<- r$indikator_start_einstieg_1_gender


  dfj <<- df
  # load UI inputs from reactive value




  # filter dataset based on UI input

  dfk <<- dfj %>% dplyr::filter(region == "Deutschland")

  dfk <- dfk %>% dplyr::filter(jahr== timerange)


  # call function to calculate the share of MINT for every "bereich"
  # df <- share_MINT(df)
  #
  # df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")
  #
  #
  # df <- df %>% dplyr::filter(indikator %in% indikator_choice_1)
  #
  # # calculate proportions
  # df <- df %>% dplyr::group_by(indikator, jahr) %>%
  #   dplyr::mutate(props = sum(wert))
  #
  #
  # df <- df %>% dplyr::group_by(indikator, jahr,fachbereich) %>%
  #   dplyr::summarize(proportion = wert/props)
  #
  # df$proportion <- df$proportion * 100

  # dfü <<- df %>% dplyr::filter(jahr == timerange)

  # dfk <- dfü %>% dplyr::filter(region == "Deutschland")

  dfk2 <<- dfk %>%
    dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle"|  fachbereich == "Ingenieurwissenschaften" |fachbereich == "Mathematik_Naturwissenschaften")

  dfk2a <<- dfk2 %>% dplyr::filter(bereich == "Hochschule")%>%
    tidyr::pivot_wider(names_from=fachbereich, values_from=wert)%>%
    dplyr::mutate(MINT=Ingenieurwissenschaften+Mathematik_Naturwissenschaften )%>%
    dplyr::select(-Ingenieurwissenschaften, -Mathematik_Naturwissenschaften)%>%
    tidyr::pivot_longer(c( "Alle", "MINT"), names_to = "fachbereich", values_to="wert")%>%
    tidyr::pivot_wider(names_from=geschlecht, values_from=wert)%>%
    dplyr::mutate(Männer=Gesamt-Frauen)%>%
    tidyr::pivot_longer(c("Männer", "Gesamt", "Frauen"), values_to = "wert", names_to="geschlecht")


  dfk2c <<- dfk %>% dplyr::filter(bereich == "Schule")%>%
    dplyr::filter(fachbereich== "MINT" | fachbereich == "Alle Fächer")%>%
    dplyr::mutate(indikator= paste0("Schüler:innen ", .$indikator ))%>%
    tidyr::pivot_wider(names_from=geschlecht, values_from = wert)%>%
    dplyr::mutate(Gesamt = Frauen + Männer)%>%
    tidyr::pivot_longer(c("Gesamt", "Männer", "Frauen"), values_to = "wert", names_to = "geschlecht")

  dfk2c$fachbereich <- ifelse(grepl("Alle Fächer", dfk2c$fachbereich), "Alle", dfk2c$fachbereich)

  dfk2b <<- dfk2 %>% dplyr::filter(bereich != "Hochschule" & bereich != "Schule")%>%
    unique()%>%
    tidyr::pivot_wider(names_from=geschlecht, values_from=wert)%>%
    dplyr::mutate(Männer=Gesamt-Frauen)%>%
    tidyr::pivot_longer(c("Männer", "Gesamt", "Frauen"), values_to = "wert", names_to="geschlecht")



  dfk2_fn <<- dplyr::bind_rows(dfk2b, dfk2a, dfk2c)%>%
    dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle")%>%
    tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
    dplyr::mutate("andere Fächer" = Alle - MINT)%>%
    dplyr::select(- Alle)%>%
    tidyr::pivot_longer(c(MINT, `andere Fächer`), names_to = "fachbereich", values_to = "wert")%>%
    tidyr::pivot_wider(names_from = geschlecht, values_from = wert)

  #Datensatz kopieren, um absolute Werte zu behalten
  df_wert <- dfk2_fn %>%
    dplyr::select(- Gesamt)%>%
    tidyr::pivot_longer(c(Männer, Frauen), names_to = "geschlecht", values_to = "wert")

  #Berechnung des Anteils
  dfk2_fn <- dfk2_fn %>%
    dplyr::mutate(dplyr::across(c(Männer, Frauen), ~./Gesamt*100))%>%
    dplyr::select(- Gesamt)%>%
    tidyr::pivot_longer(c(Männer, Frauen), names_to = "geschlecht", values_to = "proportion")

  #absolute Werte anhängen
  wert <- df_wert$wert
  dfk2_fn <- cbind(dfk2_fn, wert)

  # Indikator u25 mit NAs löschen und Runden
  dfk2_fn <- stats::na.omit(dfk2_fn)
  dfk2_fn$proportion <- round_preserve_sum(as.numeric(dfk2_fn$proportion),0)

  #Trennpunkte für lange Zahlen ergänzen
  dfk2_fn$wert <- prettyNum(dfk2_fn$wert, big.mark = ".")

  #sortieren
  dfk2_fn <- dfk2_fn[with(dfk2_fn, order(region, fachbereich, jahr, decreasing = TRUE)), ]

  #gewählte Indikatoren ausfiltern
  dfk2_fn <- dfk2_fn %>% dplyr::filter(indikator %in% indikator_choice_1_gender)


  if(length(indikator_choice_1_gender) == 1) {

    # ensure that proportion sum to 1
    df_mint <- dfk2_fn %>% dplyr::filter(fachbereich == "MINT")

    #df_mint$wert <- round_preserve_sum(as.numeric(df_mint$wert),0)

    df_rest <- dfk2_fn %>% dplyr::filter(fachbereich == "andere Fächer")

    #df_rest$wert <- round_preserve_sum(as.numeric(df_rest$wert),0)

    #title_help <- helper_title_home(indikator_choice_1_gender)
browser()
    highcharter::hw_grid(

      highcharter::hchart(df_mint, size = 280,
                          "pie", highcharter::hcaes(x = geschlecht, y = proportion)
      ) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0("MINT-", indikator_choice_1_gender, " (2021)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),

      highcharter::hchart(df_rest, size = 150,
                          "pie", highcharter::hcaes(x = geschlecht, y = proportion)
      ) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0("Vergleich: Andere ", indikator_choice_1_gender, " (2021)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, y = -120) %>%
        # highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE,
                                               opacity = 0.7)),

      ncol = 1,
      browsable = TRUE
    )


  } else if(length(indikator_choice_1_gender) == 2) {

    # filter for UI input and ensure proportions sum to 1
    df_mint <- dfk2_fn %>% dplyr::filter(fachbereich == "MINT")

    df_1_mint <- df_mint %>% dplyr::filter(indikator == indikator_choice_1_gender[1])

    #df_1_mint$wert <- round_preserve_sum(as.numeric(df_1_mint$wert),0)

    df_2_mint <- df_mint %>% dplyr::filter(indikator == indikator_choice_1_gender[2])

    #df_2_mint$wert <- round_preserve_sum(as.numeric(df_2_mint$wert),0)

    df_rest <- dfk2_fn %>% dplyr::filter(fachbereich == "andere Fächer")

    df_1_rest <- df_rest %>% dplyr::filter(indikator == indikator_choice_1_gender[1])

    #df_1_rest$wert <- round_preserve_sum(as.numeric(df_1_rest$wert),0)

    df_2_rest<- df_rest %>% dplyr::filter(indikator == indikator_choice_1_gender[2])

    #df_2_rest$wert <- round_preserve_sum(as.numeric(df_2_rest$wert),0)


    highcharter::hw_grid(



      highcharter::hchart(df_1_mint, size = 280, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0("MINT-", indikator_choice_1_gender[1], " (2021)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),


      highcharter::hchart(df_2_mint, size = 280, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0("MINT-", indikator_choice_1_gender[2], " (2021)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),


      highcharter::hchart(df_1_rest, size = 150, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0("Vergleich: Andere ", indikator_choice_1_gender[1], " (2021)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_legend(enabled = TRUE, y = -120, reversed = T) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE,
                                               opacity = 0.7)),


      highcharter::hchart(df_2_rest, size = 150, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0("Vergleich: Andere ", indikator_choice_1_gender[2], " (2021)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_legend(enabled = TRUE, y = -120, reversed = T) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE,
                                               opacity = 0.7)),

      ncol = 2,
      browsable = TRUE
    )

    ### 3 Kreise ###

  } else if(length(indikator_choice_1_gender) == 3) {

    # filter for UI input and ensure proportions sum to 1
    # filter for UI input and ensure proportions sum to 1
    df_mint <- dfk2_fn %>% dplyr::filter(fachbereich == "MINT")

    df_1_mint <- df_mint %>% dplyr::filter(indikator == indikator_choice_1_gender[1])

    #df_1_mint$wert <- round_preserve_sum(as.numeric(df_1_mint$wert),0)

    df_2_mint <- df_mint %>% dplyr::filter(indikator == indikator_choice_1_gender[2])

    #df_2_mint$wert <- round_preserve_sum(as.numeric(df_2_mint$wert),0)

    df_3_mint <- df_mint %>% dplyr::filter(indikator == indikator_choice_1_gender[3])

    #df_3_mint$wert <- round_preserve_sum(as.numeric(df_3_mint$wert),0)

    df_rest <- dfk2_fn %>% dplyr::filter(fachbereich == "andere Fächer")

    df_1_rest <- df_rest %>% dplyr::filter(indikator == indikator_choice_1_gender[1])

    #df_1_rest$wert <- round_preserve_sum(as.numeric(df_1_rest$wert),0)

    df_2_rest<- df_rest %>% dplyr::filter(indikator == indikator_choice_1_gender[2])

    #df_2_rest$wert <- round_preserve_sum(as.numeric(df_2_rest$wert),0)

    df_3_rest<- df_rest %>% dplyr::filter(indikator == indikator_choice_1_gender[3])

    #df_3_rest$wert <- round_preserve_sum(as.numeric(df_3_rest$wert),0)

    highcharter::hw_grid(


      highcharter::hchart(df_1_mint, size = 170, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0("MINT-", indikator_choice_1_gender[1], " (2021)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),


      highcharter::hchart(df_2_mint, size = 170, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0("MINT-", indikator_choice_1_gender[2], " (2021)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),

      highcharter::hchart(df_3_mint, size = 170, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0("MINT-", indikator_choice_1_gender[3], " (2021)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),

      ## Untere Kreise: Vergleiche


      highcharter::hchart(df_1_rest, size = 100, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0("Vergleich: Andere ", indikator_choice_1_gender[1], " (2021)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_legend(enabled = TRUE, y = -120, reversed = T) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE,
                                               opacity = 0.7)),


      highcharter::hchart(df_2_rest, size = 100, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0("Vergleich: Andere ", indikator_choice_1_gender[2], " (2021)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_legend(enabled = TRUE, y = -120, reversed = T) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE,
                                               opacity = 0.7)),

      highcharter::hchart(df_3_rest, size = 100, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0("Vergleich: Andere ", indikator_choice_1_gender[3], " (2021)"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_legend(enabled = TRUE, y = -120, reversed = T) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE,
                                               opacity = 0.7)),




      ncol = 3,
      browsable = TRUE
    )

  }



  # filter dataset based on UI inputs
  # df <- df %>% dplyr::filter(jahr == timerange)
  # df_naa  <- df_naa %>% dplyr::filter(jahr == timerange)
  #
  # df <- df %>% dplyr::filter(region == "Deutschland")
  # df_naa <- df_naa %>% dplyr::filter(region == "Deutschland")
  #
  #
  # # call function to calculate the share of MINT for every "bereich"
  # df<- share_MINT(df)
  #
  #
  # #rename
  # df[df$fachbereich != "MINT", "fachbereich"] <- "Andere Fachbereiche"
  #
  #
  # # order
  # df <- df[with(df, order(indikator, anzeige_geschlecht, decreasing = TRUE)), ]
  #
  # # filter parts which need more invovled calculation of proportion
  # df_sub_2 <- df %>% dplyr::filter(indikator == "Promotionen (angestrebt)" |
  #                                    indikator == "Habilitationen" | indikator == "Leistungskurse")
  #
  # # calculate the new "Gesamt"
  #
  # df_sub_2[(df_sub_2$anzeige_geschlecht == "Gesamt" & df_sub_2$indikator == "Leistungskurse"), "wert"] <-  df_sub_2 %>%
  #   dplyr::filter(indikator == "Leistungskurse") %>%
  #   dplyr::group_by(indikator, jahr) %>%
  #   dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"] +
  #                      wert[anzeige_geschlecht == "Männer"]) %>% dplyr::pull(wert)
  #
  # # calculate proprotion female
  # df_sub_2[df_sub_2$anzeige_geschlecht == "Frauen", "wert"] <-  df_sub_2 %>% dplyr::group_by(indikator, fachbereich) %>%
  #   dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"]/
  #                      wert[anzeige_geschlecht == "Gesamt"]) %>%
  #   dplyr::arrange(-dplyr::row_number()) %>% dplyr::pull(wert)
  #
  #
  # # calculate proprotion male
  # df_sub_2[df_sub_2$anzeige_geschlecht == "Männer", "wert"] <- df_sub_2 %>% dplyr::group_by(indikator, fachbereich) %>%
  #   dplyr::summarise(wert = wert[anzeige_geschlecht == "Männer"]/
  #                      wert[anzeige_geschlecht == "Gesamt"]) %>%
  #   dplyr::arrange(-dplyr::row_number()) %>% dplyr::pull(wert)
  #
  #
  # df_sub_2 <- df_sub_2 %>% dplyr::filter(anzeige_geschlecht != "Gesamt")
  #
  # df_sub_2$wert <- df_sub_2$wert * 100
  #
  # # calcuate porportion of remaining topics
  # df <- df %>% dplyr::filter(indikator != "Promotionen (angestrebt)",
  #                            indikator != "Habilitationen", indikator != "Leistungskurse") %>%
  #   dplyr::group_by(indikator, fachbereich) %>%
  #   dplyr::summarize(wert = dplyr::lead(wert)/wert) %>% na.omit()
  #
  #
  # df$wert <- df$wert * 100
  #
  # df$anzeige_geschlecht <- "Frauen"
  #
  # df_sub <- df
  #
  # df_sub$anzeige_geschlecht <- "Männer"
  #
  # df_sub$wert <- 100 - df$wert
  #
  # df <- rbind(df, df_sub, df_sub_2)
  #
  # # calcualte proportion for "neue ausbildungsverträge"
  # #vorläufige Anpassung, dass in beiden df gleicher Name ist
  # df_n <- df_naa %>%
  #   dplyr::rename(anzeige_geschlecht = geschlecht)
  #
  # df_sub <- df_n %>% dplyr::group_by(anzeige_geschlecht) %>%
  #   dplyr::summarize(wert = sum(wert))
  #
  # df_sub <- df_sub %>% dplyr::group_by(anzeige_geschlecht) %>%
  #   dplyr::summarize(wert = wert/df_sub[df_sub$anzeige_geschlecht == "Gesamt", "wert"][[1]])
  #
  # df_sub$wert <- df_sub$wert * 100
  #
  # df_sub <- df_sub %>% dplyr::filter(anzeige_geschlecht != "Gesamt")
  #
  # df_sub$indikator <- "Neue Ausbildungsverträge"
  #
  # df_sub$fachbereich <- "MINT"
  #
  #
  # df_sub <- df_sub[, c("indikator", "fachbereich", "wert", "anzeige_geschlecht")]
  #
  # df <- subset(df, select = c(indikator, fachbereich, wert, anzeige_geschlecht))
  #
  # df <- rbind(df, df_sub)
  #
  # df <- df %>% dplyr::filter(indikator %in% indikator_choice_1_gender) %>%
  #   dplyr::arrange(anzeige_geschlecht)
  #
  # # create an if statement for the options of plotting 1, 2 or 3 graphs
#   if(length(indikator_choice_1_gender) == 1) {
#
#     # ensure that proportion sum to 1
#     df_mint <- df %>% dplyr::filter(fachbereich == "MINT")
#
#     df_mint$wert <- round_preserve_sum(as.numeric(df_mint$wert),0)
#
#     df_rest <- df %>% dplyr::filter(fachbereich == "Andere Fachbereiche")
#
#     df_rest$wert <- round_preserve_sum(as.numeric(df_rest$wert),0)
#
#     #title_help <- helper_title_home(indikator_choice_1_gender)
#
#     highcharter::hw_grid(
#
#         highcharter::hchart(df_mint, size = 280,
#           "pie", highcharter::hcaes(x = geschlecht, y = wert)
#         ) %>%
#         highcharter::hc_tooltip(
#           pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#         highcharter::hc_colors(c("#154194","#efe8e6")) %>%
#         highcharter::hc_title(text = paste0("MINT-", indikator_choice_1_gender, " (2020)"),
#                               margin = 45,
#                               align = "center",
#                               style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#         highcharter::hc_chart(
#           style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
#         highcharter::hc_legend(enabled = TRUE) %>%
#         highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#                                                dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),
#
#         highcharter::hchart(df_rest, size = 150,
#                             "pie", highcharter::hcaes(x = anzeige_geschlecht, y = wert)
#         ) %>%
#           highcharter::hc_tooltip(
#             pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#           highcharter::hc_colors(c("#154194","#efe8e6")) %>%
#           highcharter::hc_title(text = paste0("Vergleich: Andere ", indikator_choice_1_gender, " (2020)"),
#                                 margin = 45,
#                                 align = "center",
#                                 style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#           highcharter::hc_chart(
#             style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
#           highcharter::hc_legend(enabled = TRUE, y = -120) %>%
#          # highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
#           highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#                                                  dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE,
#                                                  opacity = 0.7)),
#
#       ncol = 1,
#       browsable = TRUE
#     )
#
#
#   } else if(length(indikator_choice_1_gender) == 2) {
#
#     # filter for UI input and ensure proportions sum to 1
#     df_mint <- df %>% dplyr::filter(fachbereich == "MINT")
#
#     df_1_mint <- df_mint %>% dplyr::filter(indikator == indikator_choice_1_gender[1])
#
#     df_1_mint$wert <- round_preserve_sum(as.numeric(df_1_mint$wert),0)
#
#     df_2_mint <- df_mint %>% dplyr::filter(indikator == indikator_choice_1_gender[2])
#
#     df_2_mint$wert <- round_preserve_sum(as.numeric(df_2_mint$wert),0)
#
#     df_rest <- df %>% dplyr::filter(fachbereich == "Andere Fachbereiche")
#
#     df_1_rest <- df_rest %>% dplyr::filter(indikator == indikator_choice_1_gender[1])
#
#     df_1_rest$wert <- round_preserve_sum(as.numeric(df_1_rest$wert),0)
#
#     df_2_rest<- df_rest %>% dplyr::filter(indikator == indikator_choice_1_gender[2])
#
#     df_2_rest$wert <- round_preserve_sum(as.numeric(df_2_rest$wert),0)
#
#
#     highcharter::hw_grid(
#
#
#
#       highcharter::hchart(df_1_mint, size = 280, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
#         highcharter::hc_tooltip(
#           pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#         highcharter::hc_colors(c("#154194","#efe8e6")) %>%
#         highcharter::hc_title(text = paste0("MINT-", indikator_choice_1_gender[1], " (2020)"),
#                               margin = 45,
#                               align = "center",
#                               style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#         highcharter::hc_chart(
#           style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
#         highcharter::hc_legend(enabled = TRUE) %>%
#         highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#                                                dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),
#
#
#       highcharter::hchart(df_2_mint, size = 280, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
#         highcharter::hc_tooltip(
#           pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#         highcharter::hc_colors(c("#154194","#efe8e6")) %>%
#         highcharter::hc_title(text = paste0("MINT-", indikator_choice_1_gender[2], " (2020)"),
#                               margin = 45,
#                               align = "center",
#                               style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#         highcharter::hc_chart(
#           style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
#         highcharter::hc_legend(enabled = TRUE) %>%
#         highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#                                                dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),
#
#
#       highcharter::hchart(df_1_rest, size = 150, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
#         highcharter::hc_tooltip(
#           pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#         highcharter::hc_colors(c("#154194","#efe8e6")) %>%
#         highcharter::hc_title(text = paste0("Vergleich: Andere ", indikator_choice_1_gender[1], " (2020)"),
#                               margin = 45,
#                               align = "center",
#                               style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#         highcharter::hc_legend(enabled = TRUE, y = -120) %>%
#         highcharter::hc_chart(
#           style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
#         highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#                                                dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE,
#                                                opacity = 0.7)),
#
#
#       highcharter::hchart(df_2_rest, size = 150, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
#         highcharter::hc_tooltip(
#           pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#         highcharter::hc_colors(c("#154194","#efe8e6")) %>%
#         highcharter::hc_title(text = paste0("Vergleich: Andere ", indikator_choice_1_gender[2], " (2020)"),
#                               margin = 45,
#                               align = "center",
#                               style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#         highcharter::hc_legend(enabled = TRUE, y = -120) %>%
#         highcharter::hc_chart(
#           style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
#         #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
#         highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#                                                dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE,
#                                                opacity = 0.7)),
#
#       ncol = 2,
#       browsable = TRUE
#     )
#
#     ### 3 Kreise ###
#
#   } else if(length(indikator_choice_1_gender) == 3) {
#
#     # filter for UI input and ensure proportions sum to 1
#     # filter for UI input and ensure proportions sum to 1
#     df_mint <- df %>% dplyr::filter(fachbereich == "MINT")
#
#     df_1_mint <- df_mint %>% dplyr::filter(indikator == indikator_choice_1_gender[1])
#
#     df_1_mint$wert <- round_preserve_sum(as.numeric(df_1_mint$wert),0)
#
#     df_2_mint <- df_mint %>% dplyr::filter(indikator == indikator_choice_1_gender[2])
#
#     df_2_mint$wert <- round_preserve_sum(as.numeric(df_2_mint$wert),0)
#
#     df_3_mint <- df_mint %>% dplyr::filter(indikator == indikator_choice_1_gender[3])
#
#     df_3_mint$wert <- round_preserve_sum(as.numeric(df_3_mint$wert),0)
#
#     df_rest <- df %>% dplyr::filter(fachbereich == "Andere Fachbereiche")
#
#     df_1_rest <- df_rest %>% dplyr::filter(indikator == indikator_choice_1_gender[1])
#
#     df_1_rest$wert <- round_preserve_sum(as.numeric(df_1_rest$wert),0)
#
#     df_2_rest<- df_rest %>% dplyr::filter(indikator == indikator_choice_1_gender[2])
#
#     df_2_rest$wert <- round_preserve_sum(as.numeric(df_2_rest$wert),0)
#
#     df_3_rest<- df_rest %>% dplyr::filter(indikator == indikator_choice_1_gender[3])
#
#     df_3_rest$wert <- round_preserve_sum(as.numeric(df_3_rest$wert),0)
#
#     highcharter::hw_grid(
#
#
#       highcharter::hchart(df_1_mint, size = 170, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
#         highcharter::hc_tooltip(
#           pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#         highcharter::hc_colors(c("#154194","#efe8e6")) %>%
#         highcharter::hc_title(text = paste0("MINT-", indikator_choice_1_gender[1], " (2020)"),
#                               margin = 45,
#                               align = "center",
#                               style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#         highcharter::hc_chart(
#           style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
#         highcharter::hc_legend(enabled = TRUE) %>%
#         highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#                                                dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),
#
#
#       highcharter::hchart(df_2_mint, size = 170, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
#         highcharter::hc_tooltip(
#           pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#         highcharter::hc_colors(c("#154194","#efe8e6")) %>%
#         highcharter::hc_title(text = paste0("MINT-", indikator_choice_1_gender[2], " (2020)"),
#                               margin = 45,
#                               align = "center",
#                               style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#         highcharter::hc_chart(
#           style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
#         highcharter::hc_legend(enabled = TRUE) %>%
#         highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#                                                dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),
#
#       highcharter::hchart(df_3_mint, size = 170, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
#         highcharter::hc_tooltip(
#           pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#         highcharter::hc_colors(c("#154194","#efe8e6")) %>%
#         highcharter::hc_title(text = paste0("MINT-", indikator_choice_1_gender[3], " (2020)"),
#                               margin = 45,
#                               align = "center",
#                               style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#         highcharter::hc_chart(
#           style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
#         highcharter::hc_legend(enabled = TRUE) %>%
#         highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#                                                dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),
#
# ## Untere Kreise: Vergleiche
#
#
#       highcharter::hchart(df_1_rest, size = 100, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
#         highcharter::hc_tooltip(
#           pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#         highcharter::hc_colors(c("#154194","#efe8e6")) %>%
#         highcharter::hc_title(text = paste0("Vergleich: Andere ", indikator_choice_1_gender[1], " (2020)"),
#                               margin = 45,
#                               align = "center",
#                               style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#         highcharter::hc_legend(enabled = TRUE, y = -180) %>%
#         highcharter::hc_chart(
#           style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
#         highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#                                                dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE,
#                                                opacity = 0.7)),
#
#
#       highcharter::hchart(df_2_rest, size = 100, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
#         highcharter::hc_tooltip(
#           pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#         highcharter::hc_colors(c("#154194","#efe8e6")) %>%
#         highcharter::hc_title(text = paste0("Vergleich: Andere ", indikator_choice_1_gender[2], " (2020)"),
#                               margin = 45,
#                               align = "center",
#                               style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#         highcharter::hc_legend(enabled = TRUE, y = -180) %>%
#         highcharter::hc_chart(
#           style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
#         highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#                                                dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE,
#                                                opacity = 0.7)),
#
#       highcharter::hchart(df_3_rest, size = 100, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
#         highcharter::hc_tooltip(
#           pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#         highcharter::hc_colors(c("#154194","#efe8e6")) %>%
#         highcharter::hc_title(text = paste0("Vergleich: Andere ", indikator_choice_1_gender[3], " (2020)"),
#                               margin = 45,
#                               align = "center",
#                               style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#         highcharter::hc_legend(enabled = TRUE, y = -180) %>%
#          #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
#         highcharter::hc_chart(
#           style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
#         highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#                                                dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE,
#                                                opacity = 0.7)),
#
#
#
#
#       ncol = 3,
#       browsable = TRUE
#     )
#
#   }

}


#' A function to plot a graph.
#'
#' @description A function to create a stacked bar chart
#'
#' @return The return value is a plot
#' @param df The dataframe "zentral.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
home_stacked_comparison_gender <- function(df, df_naa, r) {

  # load UI inputs from reactive value
  timerange <- r$date_start_comparison_mint_gender

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)
  df_naa  <- df_naa %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")
  df_naa <- df_naa %>% dplyr::filter(region == "Deutschland")

  # call function to calculate the share of MINT for every "bereich"
  df <- share_MINT(df)

  #rename
  df[df$fachbereich != "MINT", "fachbereich"] <- "Andere Fachbereiche"

  df <- df %>% dplyr::filter(fachbereich != "Andere Fachbereiche")


  # calculate the new "Gesamt"
  df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Leistungskurse"), "wert"] <-  df %>%
    dplyr::filter(indikator == "Leistungskurse") %>%
    dplyr::group_by(indikator, jahr) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"] +
                       wert[anzeige_geschlecht == "Männer"]) %>% dplyr::pull(wert)

  # order
  df <- df[with(df, order(indikator, anzeige_geschlecht, decreasing = TRUE)), ]

  # filter parts which need more invovled calculation of proportion
  df_sub_2 <- df %>% dplyr::filter(indikator == "Promotionen (angestrebt)" |
                                   indikator == "Habilitationen" | indikator == "Leistungskurse")

  # calculate proprotion female
  df_sub_2[df_sub_2$anzeige_geschlecht == "Frauen", "wert"] <-  df_sub_2 %>% dplyr::group_by(indikator) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"]/
                       wert[anzeige_geschlecht == "Gesamt"]) %>%
    dplyr::arrange(-dplyr::row_number()) %>% dplyr::pull(wert)

  # calculate proprotion male
  df_sub_2[df_sub_2$anzeige_geschlecht == "Männer", "wert"] <- df_sub_2 %>% dplyr::group_by(indikator) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Männer"]/
                       wert[anzeige_geschlecht == "Gesamt"]) %>%
    dplyr::arrange(-dplyr::row_number()) %>% dplyr::pull(wert)


  df_sub_2 <- df_sub_2 %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

  df_sub_2$wert <- df_sub_2$wert * 100

  # calcuate porportion of remaining topics
  df <- df %>% dplyr::filter(indikator != "Promotionen (angestrebt)",
                             indikator != "Habilitationen", indikator != "Leistungskurse") %>%
    dplyr::group_by(indikator) %>%
    dplyr::summarize(wert = dplyr::lead(wert)/wert) %>% na.omit()

  df$wert <- df$wert * 100

  df$anzeige_geschlecht <- "Frauen"

  df_sub <- df

  df_sub$anzeige_geschlecht <- "Männer"

  df_sub$wert <- 100 - df$wert

  df <- rbind(df, df_sub, df_sub_2)

  # calcualte proportion for "neue ausbildungsverträge"
  #vorläufige Anpassung, dass in beiden df gleicher Name ist
  df_n <- df_naa %>%
    dplyr::rename(anzeige_geschlecht = geschlecht)

  df_sub <- df_n %>% dplyr::group_by(anzeige_geschlecht) %>%
    dplyr::summarize(wert = sum(wert))

  df_sub <- df_sub %>% dplyr::group_by(anzeige_geschlecht) %>%
    dplyr::summarize(wert = wert/df_sub[df_sub$anzeige_geschlecht == "Gesamt", "wert"][[1]])

  df_sub$wert <- df_sub$wert * 100

  df_sub <- df_sub %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

  df_sub$indikator <- "Neue Ausbildungsverträge"

  df <- rbind(df, df_sub)

  df <- df %>% dplyr::filter(indikator %in% c("Leistungskurse", "Studierende",
                                              "Auszubildende", "Beschäftigte"))

  # order
  df$indikator <- factor(df$indikator , levels=c("Leistungskurse", "Studierende",
                                                 "Auszubildende", "Beschäftigte"))
  # plot
  highcharter::hchart(df, 'bar', highcharter::hcaes( x = indikator, y=round(wert), group = anzeige_geschlecht)) %>%
    highcharter::hc_tooltip(pointFormat = "{point.anzeige_geschlecht}-Anteil: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  FALSE) %>%
    highcharter::hc_xAxis(title = list(text = ""), categories = c("Leistungskurse", "Studierende",
                                                 "Auszubildende", "Beschäftigte")) %>%
    highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
    highcharter::hc_colors(c("#154194", "#efe8e6")) %>%
    highcharter::hc_title(text = paste0("Anteil von Frauen in MINT nach Bildungsbereichen (", timerange, ")",
                                          "<br><br><br>"),
                          margin = 25,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = FALSE) %>%
    highcharter::hc_exporting(enabled = FALSE,
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))


  # ggplot2::ggplot(df, ggplot2::aes(x=indikator, y=wert, fill = anzeige_geschlecht)) +
  #   ggplot2::geom_bar(stat="identity", position = "dodge") +
  #   ggplot2::geom_text(ggplot2::aes(label=paste(round(wert),"%"), vjust = - 0.25),
  #                      position=ggplot2::position_dodge(width=0.9),
  #                      fontface = "bold") +
  #   ggplot2::theme_minimal() +
  #   ggplot2::theme(
  #     text = ggplot2::element_text(size = 12),
  #     plot.title = ggtext::element_markdown(hjust = 0.5)) +
  #   ggplot2::xlab("") + ggplot2::ylab("Anteil") +
  #   ggplot2::scale_fill_manual(values = c("#154194","#efe8e6")) +
  #   ggplot2::labs(title = paste0(paste0("<span style='font-size:20.5pt; color:black'>",
  #                                "Anteil von Frauen in MINT nach Bildungsbereichen (", timerange, ")",
  #                                "<br><br><br>")),
  #                 fill = ""
  #                 ,
  #            # caption = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen."
  #             ) +
  #   ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"))

}


#' A function to plot a graph.
#'
#' @description A function to create a stacked bar chart
#'
#' @return The return value is a plot
#' @param df The dataframe "zentral.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
home_stacked_comparison_mint <- function(df, r) {

  # load UI inputs from reactive value
  timerange <- r$date_start_comparison_mint

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")

  # call function to calculate the share of MINT for every "bereich"
  df <- share_MINT(df)

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

  df <- df %>% dplyr::filter(indikator %in% c("Leistungskurse", "Studierende",
                                              "Auszubildende", "Beschäftigte"))

  # calculate proportions for MINT vs. Rest
  df <- df %>% dplyr::group_by(indikator) %>%
    dplyr::mutate(props = sum(wert))


  df <- df %>% dplyr::group_by(indikator, fachbereich) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100

  # order
  x <- ordered(factor(df$indikator), levels=c("Leistungskurse", "Studierende",
                                              "Auszubildende", "Beschäftigte"))

  df <- df[order(x),]

  df[df$fachbereich != "MINT", "fachbereich"] <- "andere Fachbereiche"



  highcharter::hchart(df, 'bar', highcharter::hcaes(y = round(proportion), x = indikator, group = "fachbereich")) %>%
    highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
    highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
   # highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil von MINT nach Bildungsbereichen (", timerange,")"),
                          margin = 45,
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


#' A function to plot a graph.
#'
#' @description A function to create a line chart
#'
#' @return The return value is a plot
#' @param df The dataframe "zentral.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
home_comparison_line <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_start_comparison

  indikator_choice <- r$indikator_start_comparison

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(region == "Deutschland")


  # call function to calculate the share of MINT for every "bereich"
  df <- share_MINT(df)


  df <- df %>% dplyr::filter(indikator %in% indikator_choice)

  # calculate female share of MINT and Rest
  df <- share_female(df)

  #here only MINT
  df <- df %>% dplyr::filter(fachbereich == "MINT")

  # order years for plot
  df <- df[with(df, order(indikator, jahr, decreasing = FALSE)), ]

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = indikator)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil Frauen <br> Indikator: {point.indikator} <br> Anteil: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular"),
                          min = 10, max = 45) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = "Anteil von Frauen in MINT nach Bildungsbereichen",
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_exporting(enabled = FALSE,
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

}


#' A function to plot a graph.
#'
#' @description A function to create a line chart for the third box
#' inside the tab "Home".
#'
#' @return The return value is a plot
#' @param df The dataframe "zentral.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
home_rest_mint_verlauf <- function(df,r) {




  dfh <-df
  # load UI inputs from reactive value
  timerange <- r$date_start_multiple

  absolut_selector <- r$abs_zahlen_start_multiple

  indikator_choice_1 <- r$indikator_start_multiple_1

  # filter dataset based on UI inputs
  dfj <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  dfk <- dfj %>% dplyr::filter(region == "Deutschland")


  # call function to calculate the share of MINT for every "bereich"
  # df <- share_MINT(df)
  #
  # df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")
  #
  #
  # df <- df %>% dplyr::filter(indikator %in% indikator_choice_1)
  #
  # # calculate proportions
  # df <- df %>% dplyr::group_by(indikator, jahr) %>%
  #   dplyr::mutate(props = sum(wert))
  #
  #
  # df <- df %>% dplyr::group_by(indikator, jahr,fachbereich) %>%
  #   dplyr::summarize(proportion = wert/props)
  #
  # df$proportion <- df$proportion * 100

  # dfü <<- df %>% dplyr::filter(jahr == timerange)

  # dfk <- dfü %>% dplyr::filter(region == "Deutschland")

  dfk2 <- dfk %>% dplyr::filter(geschlecht=="Gesamt")%>%
    dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle"|  fachbereich == "Ingenieurwissenschaften" |fachbereich == "Mathematik_Naturwissenschaften")

  dfk2a <- dfk2 %>% dplyr::filter(bereich == "Hochschule")%>%
    tidyr::pivot_wider(names_from=fachbereich, values_from=wert)%>%
    dplyr::mutate(MINT=Ingenieurwissenschaften+Mathematik_Naturwissenschaften )%>%
    dplyr::select(-Ingenieurwissenschaften, -Mathematik_Naturwissenschaften)%>%
    tidyr::pivot_longer(c( "Alle", "MINT"), names_to = "fachbereich", values_to="wert")


  dfk2c <- dfk %>% dplyr::filter(bereich == "Schule")%>%
    dplyr::filter(fachbereich== "MINT" | fachbereich == "Alle Fächer")%>%
    dplyr::mutate(indikator= paste0("Schüler:innen ", .$indikator ))


  dfk2c$fachbereich <- ifelse(grepl("Alle Fächer", dfk2c$fachbereich), "Alle", dfk2c$fachbereich)

  dfk2b <- dfk2 %>% dplyr::filter(bereich != "Hochschule" & bereich != "Schule")%>%
    unique()



  dfk2_fn <<- dplyr::bind_rows(dfk2b, dfk2a, dfk2c)%>%
    dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle")%>%
    tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
    dplyr::mutate("Nicht MINT" = Alle - MINT)%>%
    dplyr::mutate(MINT_p= MINT/Alle*100)%>%
    dplyr::mutate("Nich MINT_p" = `Nicht MINT`/Alle*100)%>%
    dplyr::filter(geschlecht=="Gesamt")%>%
    dplyr::select(- Alle)%>%
    tidyr::pivot_longer(c(MINT, `Nicht MINT`, `Nich MINT_p`, `MINT_p`), names_to = "fachbereich", values_to = "wert")%>%
    dplyr::mutate(selector=dplyr::case_when(stringr::str_ends(.$fachbereich, "_p") ~ "In Prozent",
                                      T~"Anzahl"))
  # %>%
  #   dplyr::mutate(wert=dplyr::case_when(stringr::str_detect(.$selector, "Relativ") ~ round_preserve_sum(.),
                                        # T~))

  dfk2_fn$fachbereich <- gsub("_p", "", dfk2_fn$fachbereich)


  dfk2_fn$wert <- ifelse(stringr::str_detect(dfk2_fn$selector, "In Prozent"),round_preserve_sum(as.numeric(dfk2_fn$wert),0), dfk2_fn$wert )

  # dfk2_fn$proportion[selector=="Relativ",wert] <- round_preserve_sum(as.numeric(dfk2_fn[selector=="Relativ",wert]),0)



   if(absolut_selector=="In Prozent"){

    df <- dfk2_fn %>%
      dplyr::filter(selector=="In Prozent")

  df <- df[with(df, order(region, fachbereich, jahr, decreasing = FALSE)), ]

  #here only MINT
  df <- df %>% dplyr::filter(fachbereich == "MINT")

  df <- df %>% dplyr::filter(indikator %in% indikator_choice_1)

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group = indikator)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil MINT <br> Indikator: {point.indikator} <br> Anteil: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = " "), labels = list(format = "{value}%"),
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = "Anteil von MINT nach Bildungsbereichen",
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_exporting(enabled = FALSE,
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

  } else if (absolut_selector=="Anzahl") {

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    #here only MINT
    dft <- dfk2_fn %>% dplyr::filter(fachbereich == "MINT")

    dfö <- dft %>% dplyr::filter(indikator %in% indikator_choice_1)


    df <- dfö %>%
      dplyr::filter(selector=="Anzahl")

    df <- df[with(df, order(region, fachbereich, jahr, decreasing = FALSE)), ]



    # plot
    highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group = indikator)) %>%
      highcharter::hc_tooltip(pointFormat = "Anzahl: {point.y}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = "Anteil von MINT nach Bildungsbereichen",
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_exporting(enabled = FALSE,
                                buttons = list(contextButton = list(
                                  symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                  onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                  align = 'right',
                                  verticalAlign = 'bottom',
                                  theme = list(states = list(hover = list(fill = '#FFFFFF'))))))




  }

}
