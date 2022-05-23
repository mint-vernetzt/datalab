#' A function to plot a graph.
#'
#' @description A function to create a stacked bar chart for the first box
#' inside the tab "Home".
#'
#' @return The return value is a plot
#' @param df The dataframe "zentral.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
home_einstieg_pie <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- 2020

  indikator_choice_1 <- r$indikator_start_einstieg_1

  #indikator_choice_2 <- r$indikator_start_einstieg_2

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")


  # aggeregate every "bereich to Rest vs MINT
  df <- share_MINT(df)

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

  df <- df %>% dplyr::filter(indikator %in% indikator_choice_1)


  # calculate proportions
  df <- df %>% dplyr::group_by(indikator) %>%
    dplyr::mutate(props = sum(wert))


  df <- df %>% dplyr::group_by(indikator, fachbereich) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100


  if(length(indikator_choice_1) == 1) {

  df$proportion <- round_preserve_sum(as.numeric(df$proportion),0)

  title_help <- helper_title_home(indikator_choice_1)

  highcharter::hw_grid(
  df %>%
    highcharter::hchart(
      "pie", highcharter::hcaes(x = fachbereich, y = proportion)
      ) %>%
    highcharter::hc_tooltip(
               pointFormat=paste('Anteil: {point.percentage:.1f}%')) %>%
    highcharter::hc_colors(c("#154194", "#b16fab")) %>%
    highcharter::hc_title(text = paste0("Anteil von MINT und allen ", title_help, " in 2020"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
    highcharter::hc_legend(enabled = TRUE) %>%
    highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                           dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),

  ncol = 1,
  browsable = TRUE
  )


  } else if(length(indikator_choice_1) == 2) {

    df_1 <- df %>% dplyr::filter(indikator == indikator_choice_1[1])

    df_1$proportion <- round_preserve_sum(as.numeric(df_1$proportion),0)

    title_help_1 <- helper_title_home(indikator_choice_1[1])

    df_2 <- df %>% dplyr::filter(indikator == indikator_choice_1[2])

    df_2$proportion <- round_preserve_sum(as.numeric(df_2$proportion),0)

    title_help_2 <- helper_title_home(indikator_choice_1[2])


    highcharter::hw_grid(
      highcharter::hchart(df_1, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.1f}%')) %>%
        highcharter::hc_colors(c("#154194","#b16fab")) %>%
        highcharter::hc_title(text = paste0("Anteil von MINT und allen ",title_help_1, " in 2020"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),


      highcharter::hchart(df_2, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.1f}%')) %>%
        highcharter::hc_colors(c("#154194","#b16fab")) %>%
        highcharter::hc_title(text = paste0("Anteil von MINT und allen ",title_help_2, " in 2020"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE)),

      ncol = 2,
      browsable = TRUE
    )


  } else if(length(indikator_choice_1) == 3) {


    df_1 <- df %>% dplyr::filter(indikator == indikator_choice_1[1])

    df_1$proportion <- round_preserve_sum(as.numeric(df_1$proportion),0)

    title_help_1 <- helper_title_home(indikator_choice_1[1])

    df_2 <- df %>% dplyr::filter(indikator == indikator_choice_1[2])

    df_2$proportion <- round_preserve_sum(as.numeric(df_2$proportion),0)

    title_help_2 <- helper_title_home(indikator_choice_1[2])

    df_3 <- df %>% dplyr::filter(indikator == indikator_choice_1[3])

    df_3$proportion <- round_preserve_sum(as.numeric(df_3$proportion),0)

    title_help_3 <- helper_title_home(indikator_choice_1[3])


    highcharter::hw_grid(
      highcharter::hchart(df_1, size = 170, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.1f}%')) %>%
        highcharter::hc_colors(c("#154194","#b16fab")) %>%
        highcharter::hc_title(text = paste0("Anteil von MINT und allen ",title_help_1, " in 2020"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),


      highcharter::hchart(df_2, size = 170, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.1f}%')) %>%
        highcharter::hc_colors(c("#154194","#b16fab")) %>%
        highcharter::hc_title(text = paste0("Anteil von MINT und allen ",title_help_2, " in 2020"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE)),

      highcharter::hchart(df_3, size = 170, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.1f}%')) %>%
        highcharter::hc_colors(c("#154194","#b16fab")) %>%
        highcharter::hc_title(text = paste0("Anteil von MINT und allen ",title_help_3, " in 2020"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),



      ncol = 3,
      browsable = TRUE
    )

    }

}




#' A function to plot a graph.
#'
#' @description A function to create a stacked bar chart
#'
#' @return The return value is a plot
#' @param df The dataframe "zentral.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
home_stacked_comparison <- function(df, df_naa, r) {

  # load UI inputs from reactive value
  timerange <- r$date_start_comparison_mint

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)
  df_naa  <- df_naa %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")
  df_naa <- df_naa %>% dplyr::filter(region == "Deutschland")
  # get other dataset

  # aggeregate every "bereich to Rest vs MINT
  df <- share_MINT(df)


  df[df$fachbereich != "MINT", "fachbereich"] <- "Andere Fachbereiche"

  df <- df %>% dplyr::filter(fachbereich != "Andere Fachbereiche")


  # calculate tnew "Gesamt"
  df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Leistungskurse"), "wert"] <-  df %>%
    dplyr::filter(indikator == "Leistungskurse") %>%
    dplyr::group_by(indikator, jahr) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"] +
                       wert[anzeige_geschlecht == "Männer"]) %>% dplyr::pull(wert)




  df <- df[with(df, order(indikator, anzeige_geschlecht, decreasing = TRUE)), ]

  df_sub_2 <- df %>% dplyr::filter(indikator == "Promotionen (angestrebt)" |
                                   indikator == "Habilitationen" | indikator == "Leistungskurse")


  df_sub_2[df_sub_2$anzeige_geschlecht == "Frauen", "wert"] <-  df_sub_2 %>% dplyr::group_by(indikator) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"]/
                       wert[anzeige_geschlecht == "Gesamt"]) %>%
    dplyr::arrange(-dplyr::row_number()) %>% dplyr::pull(wert)


  df_sub_2[df_sub_2$anzeige_geschlecht == "Männer", "wert"] <- df_sub_2 %>% dplyr::group_by(indikator) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Männer"]/
                       wert[anzeige_geschlecht == "Gesamt"]) %>%
    dplyr::arrange(-dplyr::row_number()) %>% dplyr::pull(wert)


  df_sub_2 <- df_sub_2 %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

  df_sub_2$wert <- df_sub_2$wert * 100


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

  df_sub <- df_naa %>% dplyr::group_by(anzeige_geschlecht) %>%
    dplyr::summarize(wert = sum(wert))

  df_sub <- df_sub %>% dplyr::group_by(anzeige_geschlecht) %>%
    dplyr::summarize(wert = wert/df_sub[df_sub$anzeige_geschlecht == "Gesamt", "wert"][[1]])

  df_sub$wert <- df_sub$wert * 100

  df_sub <- df_sub %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

  df_sub$indikator <- "Neue Ausbildungsverträge"

  df <- rbind(df, df_sub)

  x <- ordered(factor(df$indikator), levels=c('Leistungskurse','Auszubildende','Studienanfänger',
                                   'Studierende', 'Promotionen (angestrebt)', 'Habilitationen',
                                   'Beschäftigte'))

  df <- df[order(x),]

  highcharter::hchart(df, 'bar', highcharter::hcaes(y = round(wert), x = indikator, group = "anzeige_geschlecht")) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil an MINT <br> Geschlecht: {point.anzeige_geschlecht} <br> Anteil: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Anteil"), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
    highcharter::hc_colors(c("#f5adac", "#b1b5c3")) %>%
    highcharter::hc_title(text = "Anteil von Frauen und Männer an MINT",
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    )


}

#' A function to plot a graph.
#'
#' @description A function to create a line chart for the first box
#' inside the tab "Home".
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


  # aggeregate every "bereich to Rest vs MINT
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
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"),
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular"),
                          min = 10, max = 45) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = "Anteil von Frauen an MINT im Verlauf",
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    )


}

#' A function to plot a graph.
#'
#' @description A function to create a bar chart for the second box
#' inside the tab "Home".
#'
#' @return The return value is a plot
#' @param df The dataframe "zentral.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
#'
home_leaky_pipeline <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_start_leaky

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")


  # aggeregate every "bereich to Rest vs MINT
  df <- share_MINT(df)

  # remove
  df <- df %>% dplyr::filter(indikator != "Auszubildende")
  df <- df %>% dplyr::filter(indikator != "Habilitationen")

  # only MINT perspective
  df <- df %>% dplyr::filter(fachbereich == "MINT")

  help_val_kurse <- df[((df$indikator == "Leistungskurse") & (df$anzeige_geschlecht == "Männer")), "wert"] +
    df[((df$indikator == "Leistungskurse") & (df$anzeige_geschlecht == "Frauen")), "wert"][[1]]

  df[((df$indikator == "Leistungskurse") & (df$anzeige_geschlecht == "Gesamt")), "wert"] <- help_val_kurse$wert

  # calculate the share of males for "Studierende/Studienanfänger"
  df_sub <- df %>% dplyr::filter(indikator == "Studienanfänger" | indikator == "Studierende")
  df <- df[!(df$indikator == "Studienanfänger" | df$indikator == "Studierende"),]

  df_sub <- calc_share_male(df_sub, "box_1")

  df <- rbind(df, df_sub)

  df_sub_val <- df %>% dplyr::filter(indikator == "Beschäftigte" & anzeige_geschlecht == "Frauen")

  df_sub_val$anzeige_geschlecht <- "Männer"

  help_val <- df[((df$indikator == "Beschäftigte") & (df$anzeige_geschlecht == "Gesamt")), "wert"] -
    df[((df$indikator == "Beschäftigte") & (df$anzeige_geschlecht == "Frauen")), "wert"][[1]]

  df_sub_val$wert <- help_val$wert

  df <- rbind(df, df_sub_val)

  # calculate the share of male and female
  values_female <- df %>% dplyr::group_by(indikator, fachbereich) %>%
    dplyr::summarise(proportion = wert[anzeige_geschlecht == "Frauen"]/
                       wert[anzeige_geschlecht == "Gesamt"])

  values_female$anzeige_geschlecht <- "Frauen"

  values_male <- df %>% dplyr::group_by(indikator, fachbereich) %>%
    dplyr::summarise(proportion = wert[anzeige_geschlecht == "Männer"]/
                       wert[anzeige_geschlecht == "Gesamt"])

  values_male$anzeige_geschlecht <- "Männer"

  values <- rbind(values_female, values_male)

  values$indikator <- factor(values$indikator,levels = c("Leistungskurse", "Studienanfänger",
                                                         "Studierende", "Promotionen (angestrebt)",
                                                         "Beschäftigte"))
  values$proportion <- values$proportion * 100

  values %>%
    ggplot2::ggplot(ggplot2::aes(y = proportion, x = indikator, color = anzeige_geschlecht, group = anzeige_geschlecht)) +
    ggplot2::geom_line(size = 1.5) +
    ggplot2::theme_classic() +
    ggplot2::theme(plot.title = ggtext::element_markdown(hjust = 0.5),
                   text = ggplot2::element_text(size = 14),
                   panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
                   panel.grid.major.x = ggplot2::element_line(colour = "#D3D3D3")) +
    ggplot2::labs(x = "", y = "Anteil", color = "",
                                         title = paste0("<span style='font-size:20.5pt; color:black'>",
                                                        "Anteil von Frauen an MINT für verschiedene Bereiche in ", timerange,
                                                        "<br><br><br>")) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
    ggplot2::scale_color_manual(values = colors_mint_vernetzt$gender)



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

  # load UI inputs from reactive value
  timerange <- r$date_start_multiple

  indikator_choice_1 <- r$indikator_start_multiple_1

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(region == "Deutschland")


  # aggeregate every "bereich to Rest vs MINT
  df <- share_MINT(df)

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")


  df <- df %>% dplyr::filter(indikator %in% indikator_choice_1)

  # calculate proportions
  df <- df %>% dplyr::group_by(indikator, jahr) %>%
    dplyr::mutate(props = sum(wert))


  df <- df %>% dplyr::group_by(indikator, jahr,fachbereich) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100


  #here only MINT
  df <- df %>% dplyr::filter(fachbereich == "MINT")

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = indikator)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil MINT <br> Indikator: {point.indikator} <br> Anteil: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"),
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = "Anteil von MINT im Verlauf",
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    )

}
