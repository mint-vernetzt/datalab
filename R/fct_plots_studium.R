#' A function to plot a graph.
#'
#' @description A function to create a pie chart for the first box
#' inside the tab "Schule".
#'
#' @return The return value is a plot
#' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_einstieg_pie <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_studierende_einstieg

  lehramt <- r$nurLehramt_studierende_einstieg

  hochschulform_select_1 <- r$hochschulform_studierende_einstieg_1

  hochschulform_select_2 <- r$hochschulform_studierende_einstieg_2

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  # remove
  df <- df %>% dplyr::filter(region == "Deutschland")

  # aggregate MINT
  df2 <- calc_share_MINT(df[(df$indikator == "Studierende"), ])

  df3 <- calc_share_MINT(df[(df$indikator == "Studienanfänger"), ])


  df <- rbind(df2, df3)

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

    df$indikator <- paste0(df$indikator, " (", df$hochschulform, ")")

    df_studierende <- df %>% dplyr::filter(indikator == paste0("Studierende", " (", df$hochschulform, ")"))

    # calculate proportions
    df_studierende <- share_pie(df_studierende)

    df_anfaenger <- df %>% dplyr::filter(indikator == paste0("Studienanfänger", " (", df$hochschulform, ")"))

    # calculate proportions
    df_anfaenger <- share_pie(df_anfaenger)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)

    df$indikator <- paste0(df$indikator, " (", "Lehramt, " ,df$hochschulform, ")")

    df_studierende <- df %>% dplyr::filter(indikator == paste0("Studierende", " (", "Lehramt, " ,df$hochschulform, ")"))

    # calculate proportions
    df_studierende <- share_pie(df_studierende)

    df_anfaenger <- df %>% dplyr::filter(indikator == paste0("Studienanfänger", " (", "Lehramt, " ,df$hochschulform, ")"))

    # calculate proportions
    df_anfaenger <- share_pie(df_anfaenger)


  }


    plot_studierende <- highcharter::hchart(df_studierende, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
      highcharter::hc_tooltip(
        pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
      highcharter::hc_title(text = paste0("Anteil von MINT an allen anderen Studienfächern für Studierende in ", timerange),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
      highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                             dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE))


  plot_anfeanger <- highcharter::hchart(df_anfaenger, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
      highcharter::hc_tooltip(
        pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
      highcharter::hc_title(text = paste0("Anteil von MINT an allen anderen Studienfächern für Studienanfänger in ", timerange),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
    highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                             dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE))


  plot_anfeanger <- plot_anfeanger %>% highcharter::hc_colors(c("#154194","#b16fab"))

  plot_studierende <- plot_studierende %>% highcharter::hc_colors(c("#154194","#b16fab"))

    highcharter::hw_grid(

      plot_anfeanger,
      plot_studierende,

      ncol = 2,
      browsable = TRUE
    )




}

#' A function to plot a graph.
#'
#' @description A function to create a pie chart for the first box
#' inside the tab "Schule".
#'
#' @return The return value is a plot
#' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_einstieg_pie_gender <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_studierende_einstieg_gender

  lehramt <- r$nurLehramt_studierende_einstieg_gender

  hochschulform_select_1 <- r$hochschulform_studierende_einstieg_1_gender

  hochschulform_select_2 <- r$hochschulform_studierende_einstieg_2_gender

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  # remove
  df <- df %>% dplyr::filter(region == "Deutschland")

  df_gesamt <- df %>% dplyr::filter(fachbereich == "Alle",
                                    anzeige_geschlecht == "Gesamt",
                                    nur_lehramt == "Nein",
                                    hochschulform == "insgesamt") %>%
    dplyr::rename(wert_gesamt = "wert")

  df <- calc_share_MINT(df)

  df <- calc_share_male(df, "box_1")

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

  df <- df %>% dplyr::left_join(df_gesamt, by = c("region", "indikator",
                                            "jahr", "bereich")) %>%
    dplyr::rename(anzeige_geschlecht = "anzeige_geschlecht.x",
                  fachbereich = "fachbereich.x",
                  nur_lehramt = "nur_lehramt.x",
                  hochschulform = "hochschulform.x") %>%
    dplyr::select(-c("anzeige_geschlecht.y", "nur_lehramt.y",
                     "hochschulform.y", "fachbereich.y")) %>%
    dplyr::mutate(proportion = (wert/wert_gesamt)*100)


  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

    df$indikator <- paste0(df$indikator, " (", df$hochschulform, ")")

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)

  }

  df_studierende_mint <- df %>% dplyr::filter(fachbereich == "MINT",
                                              grepl("Studierende", indikator))

  df_anfaenger_mint <- df %>% dplyr::filter(fachbereich == "MINT",
                                              grepl("Studienanfänger", indikator))

  df_studierende_rest <- df %>% dplyr::filter(fachbereich == "andere Studiengänge",
                                              grepl("Studierende", indikator))

  df_anfaenger_rest <- df %>% dplyr::filter(fachbereich == "andere Studiengänge",
                                              grepl("Studienanfänger", indikator))

  plot_studierende_mint <- highcharter::hchart(df_studierende_mint, size = 280, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = proportion)) %>%
    highcharter::hc_tooltip(
      pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
    highcharter::hc_title(text = paste0("Anteil von Frauen an MINT-Fächern für Studierende in ", timerange),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
    highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                           dataLabels = list(enabled = TRUE,  format='{point.percentage:.0f}%'), showInLegend = TRUE))


  plot_anfeanger_mint <- highcharter::hchart(df_anfaenger_mint, size = 280, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = proportion)) %>%
    highcharter::hc_tooltip(
      pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
    highcharter::hc_title(text = paste0("Anteil von Frauen an MINT-Fächern für Studienanfänger in ", timerange),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
    highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                           dataLabels = list(enabled = TRUE, format='{point.percentage:.0f}%'), showInLegend = TRUE))

  plot_studierende_rest <- highcharter::hchart(df_studierende_rest, size = 150, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = proportion)) %>%
    highcharter::hc_tooltip(
      pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
    highcharter::hc_title(text = paste0("Anteil von Frauen an allen anderen Studienfächern für Studierende in ", timerange),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
    highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                           dataLabels = list(enabled = TRUE,  format='{point.percentage:.0f}%'), showInLegend = TRUE,
                                           opacity = 0.7))


  plot_anfeanger_rest <- highcharter::hchart(df_anfaenger_rest, size = 150, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = proportion)) %>%
    highcharter::hc_tooltip(
      pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
    highcharter::hc_title(text = paste0("Anteil von Frauen an allen anderen Studienfächern für Studienanfänger in ", timerange),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
    highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                           dataLabels = list(enabled = TRUE, format='{point.percentage:.0f}%'), showInLegend = TRUE,
                                           opacity = 0.7))


  plot_anfeanger_mint <- plot_anfeanger_mint %>% highcharter::hc_colors(c("#154194","#b16fab"))

  plot_studierende_mint <- plot_studierende_mint %>% highcharter::hc_colors(c("#154194","#b16fab"))

  plot_anfeanger_rest <- plot_anfeanger_rest %>% highcharter::hc_colors(c("#154194", "#b16fab"))

  plot_studierende_rest <- plot_studierende_rest %>% highcharter::hc_colors(c("#154194", "#b16fab"))

  highcharter::hw_grid(

    plot_anfeanger_mint,
    plot_studierende_mint,

    plot_anfeanger_rest,
    plot_studierende_rest,

    ncol = 2,
    browsable = TRUE
  )




}


#' A function to plot time series
#'
#' @description A function to plot the time series
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_verlauf_single <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_studienzahl_einstieg_verlauf

  lehramt <- r$nurLehramt_studierende_einstieg_verlauf

  hochschulform_select_1 <- r$hochschulform_studierende_einstieg_verlauf_1

  hochschulform_select_2 <- r$hochschulform_studierende_einstieg_verlauf_2

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  # remove
  df <- df %>% dplyr::filter(region == "Deutschland")

  # aggregate MINT
  df2 <- calc_share_MINT(df[(df$indikator == "Studierende"), ])

  df3 <- calc_share_MINT(df[(df$indikator == "Studienanfänger"), ])


  df <- rbind(df2, df3)

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")


  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

    df$indikator <- paste0(df$indikator, " (", df$hochschulform, ")")

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)

    df$indikator <- paste0(df$indikator, " (", "Lehramt, " ,df$hochschulform, ")")

  }

  # aggregate
  df <- df %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht, bereich) %>%
    dplyr::mutate(props = sum(wert))



  # calculate proportions
  df <- df %>% dplyr::group_by(indikator, fachbereich, jahr) %>%
    dplyr::summarize(proportion = wert/props)

  df <- df %>% dplyr::filter(fachbereich == "MINT")

  df$proportion <- df$proportion * 100

  df <- df[with(df, order(jahr, decreasing = FALSE)), ]

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = indikator)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil {point.indikator} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Anteil"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil von MINT im Zeitverlauf"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))


}

#' A function to plot time series
#'
#' @description A function to plot the time series
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_verlauf_single_gender <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_studienzahl_einstieg_verlauf_gender

  lehramt <- r$nurLehramt_studierende_einstieg_verlauf_gender

  hochschulform_select_1 <- r$hochschulform_studierende_einstieg_verlauf_gender_1

  hochschulform_select_2 <- r$hochschulform_studierende_einstieg_verlauf_gender_2

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  # remove
  df <- df %>% dplyr::filter(region == "Deutschland")

  # aggregate MINT
  df2 <- calc_share_MINT(df[(df$indikator == "Studierende"), ])

  df3 <- calc_share_MINT(df[(df$indikator == "Studienanfänger"), ])

  df2 <- calc_share_male(df2, "box_1")

  df3 <- calc_share_male(df3, "box_1")

  df <- rbind(df2, df3)

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

  df <- df %>% dplyr::filter(fachbereich == "MINT")


  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

    df$indikator <- paste0(df$indikator, " (", df$hochschulform, ")")

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)

    df$indikator <- paste0(df$indikator, " (", "Lehramt, " ,df$hochschulform, ")")

  }

  # aggregate
  df <- df %>%
    dplyr::group_by(jahr, region, indikator, bereich) %>%
    dplyr::mutate(props = sum(wert))



  # calculate proportions
  df <- df %>% dplyr::group_by(indikator, fachbereich, anzeige_geschlecht, jahr) %>%
    dplyr::summarize(proportion = wert/props)

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")

  df$proportion <- df$proportion * 100

  df <- df[with(df, order(jahr, decreasing = FALSE)), ]

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = indikator)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil {point.indikator} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Anteil"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil von MINT im Zeitverlauf"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))


}



#' A function to plot time series
#'
#' @description A function to plot a bar chart
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_einstieg_comparison <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_kurse_einstieg_comparison

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  # remove
  df <- df %>% dplyr::filter(region == "Deutschland")


  # aggregate MINT
  df2 <- calc_share_MINT(df[(df$indikator == "Studierende"), ])

  df3 <- calc_share_MINT(df[(df$indikator == "Studienanfänger"), ])


  df <- rbind(df2, df3)

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")



  df_sub <- df %>% dplyr::filter(nur_lehramt == "Nein")

  df_sub$indikator <- paste0(df_sub$indikator, " (", df_sub$hochschulform, ")")



  df_sub2 <- df %>% dplyr::filter(nur_lehramt == "Ja")

  df_sub2$indikator <- paste0(df_sub2$indikator, " (", "Lehramt, " ,df_sub2$hochschulform, ")")


  df <- rbind(df_sub, df_sub2)

  # aggregate
  df <- df %>%
    dplyr::group_by(indikator) %>%
    dplyr::mutate(props = sum(wert))



  # calculate proportions
  df <- df %>% dplyr::group_by(indikator, fachbereich) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100



  # plot

  highcharter::hchart(df, 'bar', highcharter::hcaes(y = round(proportion), x = indikator, group = "fachbereich")) %>%
    highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Anteil"), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
    highcharter::hc_colors(c("#154194", "#b16fab")) %>%
    highcharter::hc_title(text = paste0("MINT-Anteile im Vergleich in ", timerange),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))


}

#' A function to plot time series
#'
#' @description A function to plot a bar chart
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_einstieg_comparison_gender <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_kurse_einstieg_comparison_gender

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  # remove
  df <- df %>% dplyr::filter(region == "Deutschland")


  # aggregate MINT
  df2 <- calc_share_MINT(df[(df$indikator == "Studierende"), ])

  df3 <- calc_share_MINT(df[(df$indikator == "Studienanfänger"), ])


  df <- rbind(df2, df3)

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Frauen")



  df_sub <- df %>% dplyr::filter(nur_lehramt == "Nein")

  df_sub$indikator <- paste0(df_sub$indikator, " (", df_sub$hochschulform, ")")



  df_sub2 <- df %>% dplyr::filter(nur_lehramt == "Ja")

  df_sub2$indikator <- paste0(df_sub2$indikator, " (", "Lehramt, " ,df_sub2$hochschulform, ")")


  df <- rbind(df_sub, df_sub2)

  # aggregate
  df <- df %>%
    dplyr::group_by(indikator) %>%
    dplyr::mutate(props = sum(wert))



  # calculate proportions
  df <- df %>% dplyr::group_by(indikator, fachbereich) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100

  # plot
  ggplot2::ggplot(df, ggplot2::aes(y=indikator, x=proportion, fill = fachbereich)) +
    ggplot2::geom_bar(stat="identity", position = "dodge") +
    ggplot2::geom_text(ggplot2::aes(label=paste(round(proportion),"%"), vjust= 0.5, hjust = -0.5),
                       position=ggplot2::position_dodge(width=1),
                       fontface = "bold") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 14),
      plot.title = ggtext::element_markdown(hjust = 0.5)) +
    ggplot2::xlab("") + ggplot2::ylab("Anteil") +
    ggplot2::scale_fill_manual(values = c("#154194", "#b16fab")) +
    ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
                                 "Frauenanteil in MINT im Vergleich in ", timerange,
                                 "<br><br><br>"),
                  fill = "") +
    ggplot2::scale_x_continuous(limits = c(0,100), labels = function(x) paste0(x, "%"))


}


#' A function to return a filtered dataset
#'
#' @description A function to similar to 'studienzahl_einstieg_bar' but with the
#' difference that it returns a dataframe instead of plot.
#'
#' @return The return value is a dataframe
#' @param df The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

data_einstieg <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_studierende_einstieg

  lehramt_enthalten <- r$nurLehramt_studierende_einstieg

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")

  # call function to calculate the share of MINT and the remaining subjects
  df <- calc_share_MINT(df)

  # calculate the share of males
  df <- calc_share_male(df, "box_1")

  # filter gender

  if(isTRUE(lehramt_enthalten)){

   # calculate the share of teacher
   df <- calc_share_teacher(df)

   df$hochschulform <- NULL

   colnames(df) <- c("Region", "Geschlecht", "Wert", "Indikator", "Lehramt", "Fachbereich", "Jahr", "Bereich")

    return(df)


  }else{

    df <- df %>% dplyr::filter(nur_lehramt != "Ja")

    df <- df %>% dplyr::filter(hochschulform == "insgesamt")

    df$hochschulform <- NULL

    colnames(df) <- c("Region", "Geschlecht", "Wert", "Indikator", "Lehramt", "Fachbereich", "Jahr", "Bereich")

    return(df)

  }

}


#' A function to plot a waffle chart
#'
#' @description A function to create a waffle chart for the second box inside the
#' tab "Studium".
#'
#' @return The return value is a waffle chart
#' @param df The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_waffle_mint <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_studierende

  lehramt <- r$nurLehramt_studierende

  hochschulform_select_1 <- r$hochschulform_studierende_1

  hochschulform_select_2 <- r$hochschulform_studierende_2

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)


  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  }

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")


  df_studierende <- df %>% dplyr::filter(indikator == "Studierende")

  x_studierende <- calc_share_waffle(df_studierende)



  df_anfaenger <- df %>% dplyr::filter(indikator == "Studienanfänger")

  x_studienanfaenger <- calc_share_waffle(df_anfaenger)


  # set order
  x_studierende <- x_studierende[order(factor(names(x_studierende), levels = c('Ingenieur', 'Mathe',
                                                                               'andere Studiengänge')))]

  x_studienanfaenger <- x_studienanfaenger[order(factor(names(x_studienanfaenger),
                                                        levels = c('Ingenieur', 'Mathe',
                                                                   'andere Studiengänge')))]


  # create plot objects for waffle charts
  waffle_studierende <- waffle::waffle(x_studierende, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", "Studierende: MINT und  <br> andere Fächer im Vergleich</span>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "right") +
    ggplot2::scale_fill_manual(
      values =  c("#00a87a",
                  "#fcc433",
                  '#b1b5c3'),
      na.value="#b1b5c3",
      limits = c("Ingenieur", "Mathe", "andere Studiengänge"),
      guide = ggplot2::guide_legend(reverse = TRUE),
      labels = c(
        paste0("Ingenieur",", ",x_studierende[1], "%"),
        paste0("Mathe",", ",x_studierende[2], "%"),
        paste0("andere Studiengänge",", ",x_studierende[3], "%"))) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=4,byrow=TRUE))



  waffle_studienanfaenger <- waffle::waffle(x_studienanfaenger, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", "Studienanfänger: MINT und  <br> andere Fächer im Vergleich</span>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "left") +
    ggplot2::scale_fill_manual(
      values =  c("#00a87a",
                  "#fcc433",
                  '#b1b5c3'),
      na.value="#b1b5c3",
      limits = c("Ingenieur", "Mathe", "andere Studiengänge"),
      guide = ggplot2::guide_legend(reverse = TRUE),
      labels = c(
        paste0("Ingenieur",", ",x_studienanfaenger[1], "%"),
        paste0("Mathe",", ",x_studienanfaenger[2], "%"),
        paste0("andere Studiengänge",", ",x_studienanfaenger[3], "%"))) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=4,byrow=TRUE))



  ggpubr::ggarrange(waffle_studienanfaenger, NULL ,waffle_studierende,
                            widths = c(1, 0.1, 1), nrow=1)



}




#' A function to plot a waffle chart
#'
#' @description A function to create a waffle chart for the second box inside the
#' tab "Studium".
#'
#' @return The return value is a waffle chart
#' @param df The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_waffle_alternative <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_studierende

  lehramt <- r$nurLehramt_studierende

  hochschulform_select_1 <- r$hochschulform_studierende_1

  hochschulform_select_2 <- r$hochschulform_studierende_2

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)


  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  }

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- df %>% dplyr::filter(fachbereich != "Alle")

  # calculate the share of males
  df <- calc_share_male(df, "box_2")

  # calculate proportions
  x_studierende <- prep_studium_proportion(df[df$indikator == "Studierende",])

  x_studienanfaenger <- prep_studium_proportion(df[df$indikator == "Studienanfänger",])

  # set order
  x_studierende <- x_studierende[order(factor(names(x_studierende), levels = c('Frauen (Ingenieur)', 'Frauen (Mathe)',
                                                                               'Männer (Ingenieur)', 'Männer (Mathe)')))]

  x_studienanfaenger <- x_studienanfaenger[order(factor(names(x_studienanfaenger),
                                                        levels = c('Frauen (Ingenieur)', 'Frauen (Mathe)',
                                                                   'Männer (Ingenieur)', 'Männer (Mathe)')))]


  # create plot objects for waffle charts
  waffle_studierende_female <- waffle::waffle(x_studierende, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", "**Studierende**</span>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "right") +
    ggplot2::scale_fill_manual(
      values =  c("#00a87a",
                  "#fcc433",
                  "#b1b5c3",
                  '#b1b5c3'),
      na.value="#b1b5c3",
      limits = c("Frauen (Ingenieur)", "Frauen (Mathe)", "Männer (Ingenieur)"),
      guide = ggplot2::guide_legend(reverse = TRUE),
      labels = c(
        paste0("Frauen (Ingenieur)",", ",x_studierende[1], "%"),
        paste0("Frauen (Mathe)",", ",x_studierende[2], "%"),
        paste0("Männer (MINT)",", ",x_studierende[3] + x_studierende[4], "%"))) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=4,byrow=TRUE))



  waffle_studienanfaenger_female <- waffle::waffle(x_studienanfaenger, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", "**Studienanfänger**</span>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "left") +
    ggplot2::scale_fill_manual(
      values =  c("#00a87a",
                  "#fcc433",
                  "#b1b5c3",
                  '#b1b5c3'),
      na.value="#b1b5c3",
      limits = c("Frauen (Ingenieur)", "Frauen (Mathe)", "Männer (Ingenieur)"),
      guide = ggplot2::guide_legend(reverse = TRUE),
      labels = c(
        paste0("Frauen (Ingenieur)",", ",x_studienanfaenger[1], "%"),
        paste0("Frauen (Mathe)",", ",x_studienanfaenger[2], "%"),
        paste0("Männer (MINT)",", ",x_studienanfaenger[3] + x_studienanfaenger[4], "%"))) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=4,byrow=TRUE))



  if(lehramt == FALSE){

    title_help_sub_sub <- ""

    hochschulform <- hochschulform_select_1

  } else {

    title_help_sub_sub <- " (nur Lehramt)"

    hochschulform <- hochschulform_select_2

  }

  if (hochschulform == "Uni"){

    title_help_sub <- "an einer Uni"

  }else if (hochschulform == "FH"){

    title_help_sub <- "an einer FH"

  } else {

    title_help_sub <- "insgesamt"

  }



  title_help <- paste0(title_help_sub, title_help_sub_sub)

  plot <- ggpubr::ggarrange(waffle_studienanfaenger_female, NULL ,waffle_studierende_female,
                            widths = c(1, 0.1, 1), nrow=1)

  text <- c(
    paste0("<span style='font-size:20.5pt; color:black'> Anteil von Frauen und Männern an MINT ", title_help,
           " in ",timerange))
  ggpubr::annotate_figure(plot, gridtext::richtext_grob(text = text))


}



#' A function to create a paired bar plot
#'
#' @description A function to return a paired bar plot for the second box inside
#' the tab "Studium
#'
#' @return The return value is a bar plot
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_absolut <- function(df,r) {

  timerange <- r$date_studierende

  lehramt <- r$nurLehramt_studierende

  hochschulform_select_1 <- r$hochschulform_studierende_1

  hochschulform_select_2 <- r$hochschulform_studierende_2


  df <- df %>% dplyr::filter(jahr == timerange)


  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  }


  df <- df %>% dplyr::filter(region == "Deutschland")


  # calculate the share of males
  df <- calc_share_male(df, type = "box_2")

  if(lehramt == FALSE){


    title_help_sub_sub <- ""

    hochschulform <- hochschulform_select_1

  } else {

    title_help_sub_sub <- " (nur Lehramt)"

    hochschulform <- hochschulform_select_2

  }

  if (hochschulform == "Uni"){

    title_help_sub <- "an einer Uni"

  }else if (hochschulform == "FH"){

    title_help_sub <- "an einer FH"

  } else {

    title_help_sub <- "insgesamt"

  }


  title_help <- paste0(title_help_sub, title_help_sub_sub)


  df <- df %>% dplyr::filter(fachbereich != "Alle")

  options(scipen=999)

  # plot
  ggplot2::ggplot(df, ggplot2::aes(x=indikator, y=wert, fill = fachbereich)) +
    ggplot2::geom_bar(stat="identity", position = "dodge") +
    ggplot2::geom_text(ggplot2::aes(label=wert, vjust = - 0.25),
                       position=ggplot2::position_dodge(width=0.9),
                       fontface = "bold") +
    ggplot2::theme_bw() +
    ggplot2::facet_grid(~ anzeige_geschlecht) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      text = ggplot2::element_text(size = 14),
      plot.title = ggtext::element_markdown(hjust = 0.5)) +
    ggplot2::xlab("") + ggplot2::ylab("Anzahl") +
    ggplot2::scale_fill_manual(values = c("#00a87a", "#fcc433")) +
    ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
                                 "Frauen und Männer in MINT und allen anderen Studienfächern ",title_help,
                                 " in ", timerange,
                                 "<br><br><br>"),
                  fill = "")



}



#' A function to plot the german map
#'
#' @description A function to plot the german map with all states that contain
#' information about the share of women in STEM
#'
#' @return The return value is the german map with information
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_map <- function(df,r) {

  timerange <- r$date_studierende

  lehramt <- r$nurLehramt_studierende

  hochschulform_select_1 <- r$hochschulform_studierende_1

  hochschulform_select_2 <- r$hochschulform_studierende_2

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  }

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(region != "Baden-Württemberg")

  # call function to calculate the share of MINT and the remaining subjects
  df <- calc_share_MINT(df)

  df <- df %>% dplyr::filter(fachbereich != "andere Studiengänge")

  # calculate the proportions
  values_studierende <- (df[(df$anzeige_geschlecht == "Frauen" & df$indikator == "Studierende"),
                "wert"]/df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Studierende"), "wert"])*100

  values_studierende$region <- df[(df$anzeige_geschlecht == "Frauen" & df$indikator == "Studierende"), "region"][[1]]

  values_studienanfaenger <- (df[(df$anzeige_geschlecht == "Frauen" & df$indikator == "Studienanfänger"),
                            "wert"]/df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Studienanfänger"), "wert"])*100

  values_studienanfaenger$region <- df[(df$anzeige_geschlecht == "Frauen" & df$indikator == "Studienanfänger"), "region"][[1]]


  if(lehramt == FALSE){


    title_help_sub_sub <- ""

    hochschulform <- hochschulform_select_1

  } else {

    title_help_sub_sub <- " (nur Lehramt)"

    hochschulform <- hochschulform_select_2

  }

  if (hochschulform == "Uni"){

    title_help_sub <- "an einer Uni"

  }else if (hochschulform == "FH"){

    title_help_sub <- "an einer FH"

  } else {

    title_help_sub <- "insgesamt"

  }


  title_help <- paste0(title_help_sub, title_help_sub_sub)


  highcharter::hw_grid(
    # plot
    highcharter::hcmap(
      "countries/de/de-all",
      data = values_studierende,
      value = "wert",
      joinBy = c("name", "region"),
      borderColor = "#FAFAFA",
      name = "Anteil Frauen an MINT",
      borderWidth = 0.1,
      nullColor = "#A9A9A9",
      tooltip = list(
        valueDecimals = 0,
        valueSuffix = "%"
      )
    ) %>%
      highcharter::hc_colorAxis(min=0, max=60) %>%
      highcharter::hc_title(
        text = paste0("Anteil der Studentinnen <br>",title_help ," an MINT in ", timerange),
        margin = 10,
        align = "center",
        style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
      ) %>%
      # highcharter::hc_caption(
      #   text = "Quelle:",  style = list(fontSize = "12px")
      # ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular")
      ) %>% highcharter::hc_size(600, 550) %>%
      highcharter::hc_credits(enabled = FALSE) %>%
      highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                             verticalAlign = "bottom"),

    highcharter::hcmap(
      "countries/de/de-all",
      data = values_studienanfaenger,
      value = "wert",
      joinBy = c("name", "region"),
      borderColor = "#FAFAFA",
      name = "Anteil Frauen an MINT",
      borderWidth = 0.1,
      nullColor = "#A9A9A9",
      tooltip = list(
        valueDecimals = 0,
        valueSuffix = "%"
      )
    ) %>%
      highcharter::hc_title(
        text = paste0("Anteil der Studienanfängerinnen <br> ",title_help ," an MINT in ", timerange),
        margin = 10,
        align = "center",
        style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
      ) %>%
      # highcharter::hc_caption(
      #   text = "Quelle:",  style = list(fontSize = "12px")
      # ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular")
      ) %>% highcharter::hc_size(600, 550) %>%
      highcharter::hc_credits(enabled = FALSE) %>%
      highcharter::hc_legend(layout = "horizontal", floating = FALSE, verticalAlign = "bottom"),


    ncol = 2,
    browsable = TRUE
  )



}


#' A function to return a filtered dataset
#'
#' @description A function to create a dataframe for given filter
#'
#' @return The return value is a dataframe
#' @param df The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

data_mix_studium <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_studierende

  lehramt <- r$nurLehramt_studierende

  hochschulform_select_1 <- r$hochschulform_studierende_1

  hochschulform_select_2 <- r$hochschulform_studierende_2

  df <- df %>% dplyr::filter(jahr == timerange)


  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  }



  # calculate the share of males
  values <- df %>%
    dplyr::group_by(region, fachbereich, indikator, nur_lehramt, hochschulform, jahr, bereich) %>%
    dplyr::mutate(wert = wert - dplyr::lead(wert)) %>% dplyr::select(wert) %>% na.omit()

  values$anzeige_geschlecht <- "Männer"

  values <- values[, colnames(df)]

  df <- rbind(df, values)

  colnames(df) <- c("Region", "Geschlecht", "Wert", "Indikator", "Lehramt", "Hochschulform", "Fachbereich", "Jahr", "Bereich")

  return(df)
}


#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_verlauf <- function(df,r) {

  # load UI inputs from reactive value
  status_studierende <- r$indikator_studierende_verlauf

  timerange <- r$date_studierende_verlauf

  lehramt <- r$nurLehramt_studierende_verlauf

  states <- r$states_studierende_verlauf

  topic <- r$topic_studierende_verlauf

  subject_aggregated <- r$subjects_aggregated

  subjects_select <- r$subject_selected

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == status_studierende)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(region != "Baden-Württemberg")

  # include "Osten" und "Westen" in Dataframe
  df <- prep_studierende_east_west(df)


  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

    title_help_sub_sub <- " insgesamt"

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)

    title_help_sub_sub <- " an einer Uni (nur Lehramt)"
  }


  if (subject_aggregated == "aggregiert"){

    # call function to calculate the share of MINT and the remaining subjects
    df <- calc_share_MINT(df)

    if (topic == "MINT"){

      title_help_sub <- "an MINT"

    } else {

      title_help_sub <- "an anderen Studienfächern"

    }

  }else {


    df <- df %>% dplyr::filter(fachbereich %in% subjects_select)

    title_help_sub <- dictionary_title_studium[[subjects_select]]
  }


  if (status_studierende == "Studierende"){

    title_help <- paste0("Studentinnen ", title_help_sub, title_help_sub_sub)

  }else{

    title_help <- paste0("Studienanfängerinnen ", title_help_sub, title_help_sub_sub)

  }


  values <-  df %>%
    dplyr::group_by(jahr, fachbereich, region) %>%
    dplyr::summarise(props = wert[anzeige_geschlecht == "Frauen"] /
                    wert[anzeige_geschlecht == "Gesamt"])


  if (subject_aggregated == "aggregiert"){
    # filter MINT or remaining subjects
    values <- values %>% dplyr::filter(fachbereich == topic)
  }

  # order years for plot
  values <- values[with(values, order(region, jahr, decreasing = FALSE)), ]

  values$props <- values$props * 100

  values <- values %>% dplyr::filter(region %in% states)

  # plot
  highcharter::hchart(values, 'line', highcharter::hcaes(x = jahr, y = round(props), group = region)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil Frauen <br> Bundesland: {point.region} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil an ", title_help ," im Verlauf"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))


}


#' A function to return a filtered dataset
#'
#' @description A function to create a dataframe for given filter
#'
#' @return The return value is a dataframe
#' @param df The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

data_verlauf_studium <- function(df,r) {

  # load UI inputs from reactive value
  status_studierende <- r$indikator_studierende_verlauf

  timerange <- r$date_studierende_verlauf

  lehramt <- r$nurLehramt_studierende_verlauf

  states <- r$states_studierende_verlauf

  topic <- r$topic_studierende_verlauf

  ost_west <- r$ost_west

  subject_aggregated <- r$subjects_aggregated

  subjects_select <- r$subject_selected

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == status_studierende)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(region != "Baden-Württemberg")

  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  }


  if (subject_aggregated == "aggregiert"){

    # call function to calculate the share of MINT and the remaining subjects
    df <- calc_share_MINT(df)

    if (topic == "MINT"){

      title_help <- "MINT"

    } else {

      title_help <- "anderen Studienfächern"

    }

  }else {


    df <- df %>% dplyr::filter(fachbereich %in% subjects_select)

    title_help <- dictionary_title_studium[[subjects_select]]
  }

  # order
  df <- df[with(df, order(region, fachbereich, jahr, decreasing = TRUE)), ]

  # calculate proportion
  values <- df %>%
    dplyr::group_by(jahr, fachbereich, region, indikator, nur_lehramt, hochschulform, bereich) %>%
    dplyr::mutate(wert = dplyr::lead(wert)/wert) %>% dplyr::select(wert) %>% na.omit()


  if (ost_west == FALSE) {

    values <- values %>% dplyr::filter(region %in% states)

  } else{

    values$dummy_west <- ifelse(values$region %in% states_east_west$west, "Westen", "Osten")

    values <- values %>% dplyr::group_by(jahr, fachbereich, dummy_west,
                                         indikator, nur_lehramt, hochschulform, bereich) %>%
      dplyr::summarise(wert = mean(wert))

    names(values)[3] <- "region"
  }

  if (subject_aggregated == "aggregiert"){
    # filter MINT or remaining subjects
    values <- values %>% dplyr::filter(fachbereich == topic)
  }

  # order years for plot
  values <- values[with(values, order(region, jahr, decreasing = FALSE)), ]

  values$wert <- round(values$wert * 100)

  values$wert <- paste0(values$wert,"%")

  colnames(values) <- c("Jahr", "Fachbereich", "Region", "Indikator", "Lehramt", "Hochschulform", "Bereich", "Wert")


  return(values)
}



#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_verlauf_bl <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_studierende_verlauf_bl

  lehramt <- r$nurLehramt_studierende_verlauf_bl

  states <- r$states_studierende_verlauf_bl

  topic <- r$topic_studierende_verlauf_bl

  subject_aggregated <- r$subjects_aggregated_bl

  subjects_select <- r$subject_selected_bl

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(region != "Baden-Württemberg")

  # include "Osten" und "Westen" in Dataframe
  df <- prep_studierende_east_west(df)


  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

    title_help_sub_sub <- " insgesamt"

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)

    title_help_sub_sub <- " an einer Uni (nur Lehramt)"
  }


  if (subject_aggregated == "aggregiert"){

    # call function to calculate the share of MINT and the remaining subjects
    df <- calc_share_MINT_bl(df)

    if (topic == "MINT"){

      title_help_sub <- "an MINT"

    } else {

      title_help_sub <- "an anderen Studienfächern"

    }

  }else {


    df <- df %>% dplyr::filter(fachbereich %in% subjects_select)

  }

  # calculate share of males for states
  df <- calc_share_male_bl(df)

  # calculate new "Gesamt"
  df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  # calculate proportions
  values <- df %>% dplyr::group_by(region, indikator, fachbereich, anzeige_geschlecht, jahr) %>%
    dplyr::summarize(proportion = wert/props)


  if (subject_aggregated == "aggregiert"){
    # filter MINT or remaining subjects
    values <- values %>% dplyr::filter(fachbereich == topic)
  }

  # order years for plot
  values <- values[with(values, order(region, jahr, decreasing = FALSE)), ]

  values$proportion <- values$proportion * 100

  values <- values %>% dplyr::filter(region %in% states)

  values$anzeige_geschlecht <- paste0(values$anzeige_geschlecht, " (", values$indikator, ")")

  # plot
  highcharter::hchart(values, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = anzeige_geschlecht)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil {point.anzeige_geschlecht} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil von Student*innen im Verlauf"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

}




#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_verlauf_bl_subject <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_verlauf_subject_bl

  lehramt <- r$nurLehramt_studierende_verlauf_bl_subject

  states <- r$states_verlauf_subject_bl

  status_studierende <- r$topic_selected_subject_bl

  subjects_select <- r$subject_selected_bl

  hochschulform_select_1 <- r$hochschulform_studierende_verlauf_1

  hochschulform_select_2 <- r$hochschulform_studierende_verlauf_2

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  # remove
  df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(region != "Baden-Württemberg")

  df <- df %>% dplyr::filter(indikator == status_studierende)

  # include "Osten" und "Westen" in Dataframe
  df <- prep_studierende_east_west(df)


  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

    df$indikator <- paste0(df$indikator, " (", df$hochschulform, ")")

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)

    df$indikator <- paste0(df$indikator, " (", "Lehramt, " ,df$hochschulform, ")")

  }


  # call function to calculate the share of MINT and the remaining subjects
  df_sub <- calc_share_MINT_bl(df)

  df_sub <-  df_sub %>% dplyr::filter(anzeige_geschlecht != "Frauen")

  # aggregate
  df_sub <- df_sub %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht, bereich) %>%
    dplyr::mutate(props = sum(wert))


  df <-  df %>% dplyr::filter(anzeige_geschlecht != "Frauen")

  df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

  df_sub_all <- df %>% dplyr::filter(fachbereich == "Alle")

  df$props = NA

  # Assign new "Gesamt" to each subject
  df[df$fachbereich == "Mathe", "props"] <- df_sub_all$wert
  df[df$fachbereich == "Ingenieur", "props"] <- df_sub_all$wert

  df <- df %>% dplyr::filter(fachbereich != "Alle")

  df <-rbind(df, df_sub)

  #filter
  df <- df %>% dplyr::filter(fachbereich %in% subjects_select)

  # calcualte proportions
  df <- df %>% dplyr::group_by(region, indikator, fachbereich, anzeige_geschlecht, jahr) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100

  # filter
  df <- df %>% dplyr::filter(region %in% states)


  # order years for plot
  df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]


  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = fachbereich)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil {point.fachbereich} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil der Studienfächer im Verlauf"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))


}

#' A function to create a bar plot
#'
#' @description A function to return a ranking o
#'
#' @return The return value is a bar plot
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

ranking_bl_subject <- function(df,r, type) {

  # load UI inputs from reactive value

  timerange <- r$date_ranking_subject_bl

  states <- r$states_ranking_subject_bl

  indikator_comparison <- r$topic_selected_subject_bl

  lehramt <- r$nurLehramt_studierende_ranking_bl_subject

  hochschulform_select_1 <- r$hochschulform_studierende_ranking_bl_1

  hochschulform_select_2 <- r$hochschulform_studierende_ranking_bl_2

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

  df <- df %>% dplyr::filter(indikator == indikator_comparison)

  # include "Osten" und "Westen" in Dataframe
  df <- prep_studierende_east_west(df)

  df <- df %>% dplyr::filter(region %in% states)

  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

    df$indikator <- paste0(df$indikator, " (", df$hochschulform, ")")

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)

    df$indikator <- paste0(df$indikator, " (", "Lehramt, " ,df$hochschulform, ")")

  }

  df <- df %>%
    dplyr::mutate(props = df %>%
                    dplyr::filter(fachbereich == "Alle") %>%
                    dplyr::pull(wert))

  # aggregate to MINT
  values_Mint <- df %>%
    dplyr::filter(fachbereich != "Alle") %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht, hochschulform,
                    nur_lehramt, props) %>%
    dplyr::summarise(wert = sum(wert)) %>%
    dplyr::mutate(bereich = "Hochschule",
                  fachbereich = "MINT (aggregiert)")

  df_andere <- calc_share_MINT(df) %>%
    dplyr::filter(fachbereich == "andere Studiengänge")

  df <- rbind(df %>% dplyr::filter(fachbereich != "Alle"),
              values_Mint,
              df_andere) %>%
    dplyr::mutate(proportion = wert/props) %>%
    dplyr::arrange(fachbereich)

  df$proportion <- df$proportion * 100

  # plot
  a <- ifelse(df$fachbereich == "MINT (aggregiert)" | df$fachbereich == "andere Studiengänge" , "#b16fab", "grey30")


  ggplot2::ggplot(df, ggplot2::aes(y=fachbereich, x=proportion)) +
    ggplot2::geom_bar(stat="identity", fill = "#154194") +
    ggplot2::geom_text(ggplot2::aes(label=paste(round(proportion),"%")), hjust = -0.3,
                       fontface = "bold") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(colour = a),
      text = ggplot2::element_text(size = 14),
      plot.title = ggtext::element_markdown(hjust = 0.5)) +
    ggplot2::ylab("") + ggplot2::xlab("Anteil") +
    ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
                                 indikator_comparison, ": Anteil der Fächer im Vergleich in ", timerange,
                                 "<br><br><br>"),
                  fill = "") +
    ggplot2::scale_y_discrete(expand = c(0,0)) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))

}

#' A function to plot a waffle chart
#'
#' @description A function to create a waffle chart inside the
#' tab "Studium".
#'
#' @return The return value is a waffle chart
#' @param df The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_waffle_choice_gender <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_studium_choice_gender

  lehramt <- r$nurLehramt_studium_choice_gender

  hochschulform_select_1 <- r$hochschulform_studium_choice_gender1

  hochschulform_select_2 <- r$hochschulform_studium_choice_gender2

  studium_level <- r$level_studium_choice_gender

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)


  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  }

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- df %>% dplyr::filter(indikator == studium_level)

  # calculate new "Männer"
  df <- calc_share_male(df, "box_2")

  df_maenner <- df %>% dplyr::filter(anzeige_geschlecht == "Männer")

  x_maenner <- calc_share_waffle(df_maenner)



  df_frauen <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")

  x_frauen <- calc_share_waffle(df_frauen)


  # set order
  x_maenner <- x_maenner[order(factor(names(x_maenner), levels = c('Ingenieur', 'Mathe',
                                                                   'andere Studiengänge')))]

  x_frauen <- x_frauen[order(factor(names(x_frauen),
                                    levels = c('Ingenieur', 'Mathe',
                                               'andere Studiengänge')))]


  # create plot objects for waffle charts
  waffle_maenner <- waffle::waffle(x_maenner, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", "Männliche Studierende</span>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "right") +
    ggplot2::scale_fill_manual(
      values =  c("#00a87a",
                  "#fcc433",
                  '#b1b5c3'),
      na.value="#b1b5c3",
      limits = c("Ingenieur", "Mathe", "andere Studiengänge"),
      guide = ggplot2::guide_legend(reverse = TRUE),
      labels = c(
        paste0("Ingenieur",", ",x_maenner[1], "%"),
        paste0("Mathe",", ",x_maenner[2], "%"),
        paste0("andere Studiengänge",", ",x_maenner[3], "%"))) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=4,byrow=TRUE))



  waffle_frauen <- waffle::waffle(x_frauen, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", "Weibliche Studierende</span>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "left") +
    ggplot2::scale_fill_manual(
      values =  c("#00a87a",
                  "#fcc433",
                  '#b1b5c3'),
      na.value="#b1b5c3",
      limits = c("Ingenieur", "Mathe", "andere Studiengänge"),
      guide = ggplot2::guide_legend(reverse = TRUE),
      labels = c(
        paste0("Ingenieur",", ",x_frauen[1], "%"),
        paste0("Mathe",", ",x_frauen[2], "%"),
        paste0("andere Studiengänge",", ",x_frauen[3], "%"))) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=4,byrow=TRUE))



  ggpubr::ggarrange(waffle_frauen, NULL ,waffle_maenner,
                    widths = c(1, 0.1, 1), nrow=1)

}

#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studierende_verlauf_single_bl_gender <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_verlauf_bl_subject_gender

  states <- r$states_verlauf_bl_subject_gender

  subjects_select <- r$subject_verlauf_bl_subject_gender

  lehramt <- r$nurLehramt_studierende_verlauf_bl_subject_gender

  hochschulform_select_1 <- r$hochschulform_studierende_verlauf_bl_subject_gender_1

  hochschulform_select_2 <- r$hochschulform_studierende_verlauf_bl_subject_gender_2

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  # df <- df %>% dplyr::filter(fachbereich != "Alle Fächer")

  df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(region != "Baden-Württemberg")

  # include "Osten" und "Westen" in Dataframe
  df <- prep_studierende_east_west(df)

  df <- df %>% dplyr::filter(region %in% states)

  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)

  }

  # aggregate all subjects to calculate proportion later
  df_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen",
                                    fachbereich == "Alle") %>%
    dplyr::group_by(region, anzeige_geschlecht, indikator, nur_lehramt, hochschulform, jahr) %>%
    dplyr::mutate(wert_gesamt = sum(wert)) %>%
    dplyr::select(c("region", "indikator", "nur_lehramt",
                    "hochschulform", "jahr", "wert_gesamt"))

  # aggregate to MINT
  values_Mint <- df %>%
    dplyr::filter(fachbereich != "Alle") %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht, hochschulform,
                    nur_lehramt) %>%
    dplyr::summarise(wert = sum(wert)) %>%
    dplyr::mutate(bereich = "Hochschule",
                  fachbereich = "MINT (aggregiert)") %>%
    dplyr::filter(anzeige_geschlecht == "Frauen")

  einzelne_faecher <- df %>%
    dplyr::filter(anzeige_geschlecht == "Frauen")

  df_andere <- calc_share_MINT(df) %>%
    dplyr::filter(fachbereich == "andere Studiengänge",
                  anzeige_geschlecht == "Frauen")

  df <- rbind(values_Mint, einzelne_faecher, df_andere)

  # # calculate proportion
  values <- df %>%
    dplyr::left_join(df_gesamt, by = c("region", "indikator", "nur_lehramt",
                                       "hochschulform", "jahr")) %>%
    dplyr::select(-"anzeige_geschlecht.y") %>%
    dplyr::rename(anzeige_geschlecht = "anzeige_geschlecht.x") %>%
    dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
    dplyr::select(-c("wert","wert_gesamt")) %>%
    dplyr::filter(fachbereich != "Alle",
                  fachbereich != "andere Studiengänge")

  # order years for plot
  values <- values[with(values, order(region, jahr, decreasing = FALSE)), ]

  values <- values %>%  dplyr::filter(fachbereich == subjects_select)

  values$anzeige_geschlecht <- paste0(values$anzeige_geschlecht, " (", values$indikator, ")")

  # plot
  highcharter::hchart(values, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = anzeige_geschlecht)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil {point.anzeige_geschlecht} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil von Studentinnen im Verlauf"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

}

#' A function to create a dumbbell plot
#'
#' @description A function to return a ranking of subject for both genders
#'
#' @return The return value is a dumbbell plot
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienfaecher_ranking <- function(df,r, type) {

  # load UI inputs from reactive value
  timerange <- r$date_studium_ranking_bl_subject_gender

  states <- r$states_studium_ranking_bl_subject_gender

  lehramt <- r$nurLehramt_studium_ranking_bl_subject_gender

  hochschulform_select_1 <- r$hochschulform_studium_ranking_bl_subject_gender_1

  hochschulform_select_2 <- r$hochschulform_studium_ranking_bl_subject_gender_2

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  # df <- df %>% dplyr::filter(fachbereich != "Alle Fächer")

  df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(region != "Baden-Württemberg")

  # include "Osten" und "Westen" in Dataframe
  df <- prep_studierende_east_west(df)

  df <- df %>% dplyr::filter(region %in% states)

  df <- df %>% dplyr::mutate(indikator = replace(indikator,
                                                 indikator == "Studienanfänger",
                                                   "Studienanfängerinnen"))

  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)

  }

  # aggregate all subjects to calculate proportion later
  df_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen",
                                    fachbereich == "Alle") %>%
    dplyr::group_by(region, anzeige_geschlecht, indikator, nur_lehramt, hochschulform, jahr) %>%
    dplyr::mutate(wert_gesamt = sum(wert)) %>%
    dplyr::select(c("region", "indikator", "nur_lehramt",
                    "hochschulform", "jahr", "wert_gesamt"))

  # aggregate to MINT
  values_Mint <- df %>%
    dplyr::filter(fachbereich != "Alle") %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht, hochschulform,
                    nur_lehramt) %>%
    dplyr::summarise(wert = sum(wert)) %>%
    dplyr::mutate(bereich = "Hochschule",
                  fachbereich = "MINT (aggregiert)") %>%
    dplyr::filter(anzeige_geschlecht == "Frauen")

  einzelne_faecher <- df %>%
    dplyr::filter(anzeige_geschlecht == "Frauen")

  df_andere <- calc_share_MINT(df) %>%
    dplyr::filter(fachbereich == "andere Studiengänge",
                  anzeige_geschlecht == "Frauen")

  df <- rbind(values_Mint, einzelne_faecher, df_andere)

  # # calculate proportion
  df <- df %>%
    dplyr::left_join(df_gesamt, by = c("region", "indikator", "nur_lehramt",
                                       "hochschulform", "jahr")) %>%
    dplyr::select(-"anzeige_geschlecht.y") %>%
    dplyr::rename(anzeige_geschlecht = "anzeige_geschlecht.x") %>%
    dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
    dplyr::select(-c("wert","wert_gesamt")) %>%
    dplyr::filter(fachbereich != "Alle")

  # spread column
  df <- tidyr::spread(df, indikator, proportion)

  df <- df %>% tidyr::drop_na()

  df <- df %>% dplyr::select(-hochschulform, -region, -anzeige_geschlecht)

  df2 <- tidyr::gather(df, group, value, -fachbereich) %>%
    dplyr::filter(group %in% c("Studienanfängerinnen", "Studierende")) %>%
    dplyr::mutate(value = as.numeric(value))

  df2$fachbereich <- factor(df2$fachbereich, levels = levels(df$fachbereich))

  ggplot2::ggplot(df,
                  ggplot2::aes(y = fachbereich)) +
    ggplot2::geom_point(data = df2, ggplot2::aes(x = value, color = group), size = 5) +
    ggalt::geom_dumbbell(
      ggplot2::aes(x = Studienanfängerinnen, xend = Studierende),
      size = 0.5,
      size_x = 5,
      size_xend = 5,
      colour = "black",
      colour_x = "#b1b5c366",
      colour_xend = "#f5adac66",
      dot_guide=TRUE) +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(name = "", values = c("#b1b5c366", "#f5adac66")) +
    ggplot2::theme(legend.position="top",
                   panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
                   plot.title = ggtext::element_markdown(hjust = 0.5),
                   axis.text.y = ggplot2::element_text(size = 11)) +
    ggplot2::ylab("") + ggplot2::xlab("") +
    ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
                                 "Relativer Anteil von Studentinnen als Studienanfängerinnen oder Studierende in ",timerange,
                                 "<br><br><br>"),
                  color = "") +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))

}

#' A function to plot the german map
#'
#' @description A function to plot the german map with all states that contain
#' information about the share of women in STEM
#'
#' @return The return value is the german map with information
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studierende_map <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_studium_studienzahl_bl_map

  subjects <- r$subject_studium_studienzahl_bl_map

  lehramt <- r$nurLehramt_studium_studienzahl_bl_map

  hochschulform_select_1 <- r$hochschulform_studium_studienzahl_bl_map1

  hochschulform_select_2 <- r$hochschulform_studium_studienzahl_bl_map2

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(region != "Baden-Württemberg")

  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)

  }

  # aggregate to MINT
  df_sub <- calc_share_MINT_bl(df)

  df_sub <- df_sub[,colnames(df)]

  df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT (aggregiert)"

  df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (aggregiert)"

  df_sub <-  calc_share_male_bl(df_sub)

  df_sub <-  df_sub %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  df_sub <- df_sub %>% dplyr::filter(anzeige_geschlecht != "Männer")

  df_sub <- df_sub %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
    dplyr::mutate(wert_sum = sum(props))

  df <-  calc_share_male_bl(df)

  # calculate the new "Gesamt"
  df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")

  df <- df %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
    dplyr::mutate(wert_sum = sum(props))

  df <- rbind(df, df_sub)

  df <- df %>% dplyr::filter(fachbereich == subjects)

  # calculate proportions
  df <- df %>% dplyr::group_by(region, indikator) %>%
    dplyr::summarize(proportion = props/wert_sum)

  df$proportion <- df$proportion * 100

  highcharter::hw_grid(
    # plot
    highcharter::hcmap(
      "countries/de/de-all",
      data = df[df$indikator == "Studienanfänger",],
      value = "proportion",
      joinBy = c("name", "region"),
      borderColor = "#FAFAFA",
      name = "Anteil",
      borderWidth = 0.1,
      nullColor = "#A9A9A9",
      tooltip = list(
        valueDecimals = 0,
        valueSuffix = "%"
      )
    ) %>%
      highcharter::hc_colorAxis(min=0, max=60) %>%
      highcharter::hc_title(
        text = paste0("Studienanfänger:innen: Anteil an Belegungen <br> in ", subjects),
        margin = 10,
        align = "center",
        style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
      ) %>%
      # highcharter::hc_caption(
      #   text = "Quelle:",  style = list(fontSize = "12px")
      # ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular")
      ) %>% highcharter::hc_size(600, 550) %>%
      highcharter::hc_credits(enabled = FALSE) %>%
      highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                             verticalAlign = "bottom"),

    highcharter::hcmap(
      "countries/de/de-all",
      data = df[df$indikator == "Studierende",],
      value = "proportion",
      joinBy = c("name", "region"),
      borderColor = "#FAFAFA",
      name = "Anteil",
      borderWidth = 0.1,
      nullColor = "#A9A9A9",
      tooltip = list(
        valueDecimals = 0,
        valueSuffix = "%"
      )
    ) %>%
      highcharter::hc_colorAxis(min=0, max=60) %>%
      highcharter::hc_title(
        text = paste0("Studierende: Anteil an Belegungen <br> in ", subjects),
        margin = 10,
        align = "center",
        style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
      ) %>%
      # highcharter::hc_caption(
      #   text = "Quelle:",  style = list(fontSize = "12px")
      # ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular")
      ) %>% highcharter::hc_size(600, 550) %>%
      highcharter::hc_credits(enabled = FALSE) %>%
      highcharter::hc_legend(layout = "horizontal", floating = FALSE, verticalAlign = "bottom"),


    ncol = 2,
    browsable = TRUE
  )
}

#' A function to plot the german map
#'
#' @description A function to plot the german map with all states that contain
#' information about the share of women in STEM
#'
#' @return The return value is the german map with information
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studierende_map_gender <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_studium_studienzahl_bl_gender_map

  subjects <- r$subject_studium_studienzahl_bl_gender_map

  lehramt <- r$nurLehramt_studium_studienzahl_bl_gender_map

  hochschulform_select_1 <- r$hochschulform_studium_studienzahl_bl_gender_map1

  hochschulform_select_2 <- r$hochschulform_studium_studienzahl_bl_gender_map2

  indikator_choice <- r$level_studium_choice_gender

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(region != "Baden-Württemberg")

  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)

  }

  # aggregate to MINT
  df_sub <- calc_share_MINT_bl(df)

  df_sub <- df_sub[,colnames(df)]

  df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT (aggregiert)"

  df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (aggregiert)"

  df_sub <-  calc_share_male_bl(df_sub)

  df_sub <-  df_sub %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  df_sub <- df_sub %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
    dplyr::mutate(wert_sum = sum(props))

  df <-  calc_share_male_bl(df)

  # calculate the new "Gesamt"
  df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  df <- df %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
    dplyr::mutate(wert_sum = sum(props))

  df <- rbind(df, df_sub)

  df <- df %>% dplyr::filter(fachbereich == subjects)

  df <- df %>% dplyr::filter(indikator == indikator_choice)

  # calculate proportions
  df <- df %>% dplyr::group_by(region, anzeige_geschlecht) %>%
    dplyr::summarize(proportion = wert/wert_sum)

  df$proportion <- df$proportion * 100

  highcharter::hw_grid(
    # plot
    highcharter::hcmap(
      "countries/de/de-all",
      data = df[df$anzeige_geschlecht == "Frauen",],
      value = "proportion",
      joinBy = c("name", "region"),
      borderColor = "#FAFAFA",
      name = "Anteil",
      borderWidth = 0.1,
      nullColor = "#A9A9A9",
      tooltip = list(
        valueDecimals = 0,
        valueSuffix = "%"
      )
    ) %>%
      highcharter::hc_colorAxis(min=0, max=60) %>%
      highcharter::hc_title(
        text = paste0("Weibliche ", indikator_choice, ": Anteil an Belegungen <br> in ", subjects),
        margin = 10,
        align = "center",
        style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
      ) %>%
      # highcharter::hc_caption(
      #   text = "Quelle:",  style = list(fontSize = "12px")
      # ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular")
      ) %>% highcharter::hc_size(600, 550) %>%
      highcharter::hc_credits(enabled = FALSE) %>%
      highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                             verticalAlign = "bottom"),

    highcharter::hcmap(
      "countries/de/de-all",
      data = df[df$anzeige_geschlecht == "Männer",],
      value = "proportion",
      joinBy = c("name", "region"),
      borderColor = "#FAFAFA",
      name = "Anteil",
      borderWidth = 0.1,
      nullColor = "#A9A9A9",
      tooltip = list(
        valueDecimals = 0,
        valueSuffix = "%"
      )
    ) %>%
      highcharter::hc_colorAxis(min=0, max=60) %>%
      highcharter::hc_title(
        text = paste0("Männliche ", indikator_choice, ": Anteil an Belegungen <br> in ", subjects),
        margin = 10,
        align = "center",
        style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
      ) %>%
      # highcharter::hc_caption(
      #   text = "Quelle:",  style = list(fontSize = "12px")
      # ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular")
      ) %>% highcharter::hc_size(600, 550) %>%
      highcharter::hc_credits(enabled = FALSE) %>%
      highcharter::hc_legend(layout = "horizontal", floating = FALSE, verticalAlign = "bottom"),


    ncol = 2,
    browsable = TRUE
  )
}

#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studierende_verlauf_multiple_bl <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_studium_studienzahl_bl_verlauf

  subjects_select <- r$subject_studium_studienzahl_bl_verlauf

  lehramt <- r$nurLehramt_studium_studienzahl_bl_verlauf

  hochschulform_select_1 <- r$hochschulform_studium_studienzahl_bl_verlauf1

  hochschulform_select_2 <- r$hochschulform_studium_studienzahl_bl_verlauf2

  studium_level <- r$level_studium_studienzahl_bl_verlauf

  states <- r$states_studium_studienzahl_bl_verlauf

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  # remove
  df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(region != "Baden-Württemberg")

  # include "Osten" und "Westen" in Dataframe
  df <- prep_studierende_east_west(df)

  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

    title_help_sub_sub <- " insgesamt"

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)

    title_help_sub_sub <- " an einer Uni (nur Lehramt)"
  }

  # aggregate to MINT
  df_sub <- calc_share_MINT_bl(df)

  df_sub <- df_sub[,colnames(df)]

  df_sub <- calc_share_male_bl(df_sub)

  # calculate the new "Gesamt"
  df_sub <-  df_sub %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  df_sub <- df_sub %>% dplyr::filter(anzeige_geschlecht != "Männer")

  # aggregate all subjects to calculate proportion later
  df_sub <- df_sub %>% dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
    dplyr::mutate(sum_props = sum(props))


  df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT (aggregiert)"

  df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (aggregiert)"

  df <- calc_share_male_bl(df)

  # calculate new "Gesamt"
  df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")

  df <- df %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
    dplyr::mutate(sum_props = sum(props))

  df <- rbind(df, df_sub)

  # filter states
  df <- df %>% dplyr::filter(region %in% states)

  df <- df %>% dplyr::filter(fachbereich %in% subjects_select)

  df <- df %>% dplyr::filter(indikator %in% studium_level)

  # calculate proportions
  df <- df %>% dplyr::group_by(jahr, region, indikator) %>%
    dplyr::summarize(proportion = props/sum_props)

  df$proportion <- df$proportion * 100

  if(studium_level == "Studierende") {

    title_help <- "Studierende:"

  }else {

    title_help <- "Studienanfänger:"

  }

  # order years for plot
  df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = region)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil {point.region} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0(title_help, " Anteil an Belegungen in ", subjects_select),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

}

#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studierende_verlauf_multiple_bl_gender <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_studium_studienzahl_bl_gender_verlauf

  subjects_select <- r$subject_studium_studienzahl_bl_gender_verlauf

  lehramt <- r$nurLehramt_studium_studienzahl_bl_gender_verlauf

  hochschulform_select_1 <- r$hochschulform_studium_studienzahl_bl_gender_verlauf1

  hochschulform_select_2 <- r$hochschulform_studium_studienzahl_bl_gender_verlauf2

  studium_level <- r$level_studium_studienzahl_bl_gender_verlauf

  states <- r$states_studium_studienzahl_bl_gender_verlauf

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  # remove
  df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(region != "Baden-Württemberg")

  # include "Osten" und "Westen" in Dataframe
  df <- prep_studierende_east_west(df)

  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

    title_help_sub_sub <- " insgesamt"

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)

    title_help_sub_sub <- " an einer Uni (nur Lehramt)"
  }

  # aggregate to MINT
  df_sub <- calc_share_MINT_bl(df)

  df_sub <- df_sub[,colnames(df)]

  df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT (aggregiert)"

  df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (aggregiert)"

  df_sub <-  calc_share_male_bl(df_sub)

  df_sub <-  df_sub %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  df_sub <- df_sub %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
    dplyr::mutate(wert_sum = sum(props))

  df <-  calc_share_male_bl(df)

  # calculate the new "Gesamt"
  df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  df <- df %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
    dplyr::mutate(wert_sum = sum(props))

  df <- rbind(df, df_sub)

  df <- df %>% dplyr::filter(fachbereich == subjects_select)

  df <- df %>% dplyr::filter(indikator == studium_level)

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")

  df <- df %>% dplyr::filter(region %in% states)

  # calculate proportions
  df <- df %>% dplyr::group_by(region, fachbereich, jahr, anzeige_geschlecht) %>%
    dplyr::summarize(proportion = wert/wert_sum) %>% dplyr::ungroup()

  df$proportion <- df$proportion * 100

  if(studium_level == "Studierende") {

    title_help <- "Weibliche Studierende:"

  }else {

    title_help <- "Weibliche Studienanfänger:"

  }

  # order years for plot
  df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = region)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil {point.region} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0(title_help, " Anteil an Belegungen in ", subjects_select),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

}

#' A function to plot a bar chart
#'
#' @description A function to create a bar chart to compare different subjects
#' for different Bundesländer.
#'
#' @return The return value is a bar chart
#' @param df The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studierende_mint_vergleich_bl <- function(df,r) {

  timerange <- r$date_studium_studienzahl_bl_vergleich

  lehramt <- r$nurLehramt_studium_studienzahl_bl_vergleich

  hochschulform_select_1 <- r$hochschulform__studium_studienzahl_bl_vergleich1

  hochschulform_select_2 <- r$hochschulform__studium_studienzahl_bl_vergleich2

  studium_level <- r$level_studium_studienzahl_bl_vergleich

  subject <- r$subject_studium_studienzahl_bl_vergleich

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(region != "Baden-Württemberg")

  df <- df %>% dplyr::filter(indikator == studium_level)

  df <- calc_share_male_bl(df)

  # calculate new "Gesamt"
  df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
    dplyr::mutate(wert = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")

  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

    title_help_sub_sub <- " insgesamt"

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)

    title_help_sub_sub <- " an einer Uni (nur Lehramt)"
  }

  # aggregate to MINT
  df_sub <- calc_share_MINT_bl(df)

  df_sub <- df_sub[,colnames(df)]

  # # aggregate all subjects to calculate proportion later
  df_sub <- df_sub %>% dplyr::group_by(region, indikator) %>%
    dplyr::mutate(props = sum(wert))

  df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT (aggregiert)"

  df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (aggregiert)"

  # aggregate all subjects to calculate proportion later
  df <- df %>% dplyr::group_by(region, indikator) %>%
    dplyr::mutate(props = sum(wert))

  df <- rbind(df, df_sub)

  df <- df %>% dplyr::filter(fachbereich == subject)

  # calculate proportion
  df <- df %>% dplyr::group_by(region, fachbereich, indikator) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- round(df$proportion * 100)

  ggplot2::ggplot(df, ggplot2::aes(y=region, x=proportion)) +
    ggplot2::geom_bar(stat="identity", fill = "#154194") +
    ggplot2::geom_text(ggplot2::aes(label=paste(round(proportion),"%")), hjust = -0.3,
                       fontface = "bold") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 14),
      plot.title = ggtext::element_markdown(hjust = 0.5)) +
    ggplot2::ylab("") + ggplot2::xlab("Anteil") +
    ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
                                 studium_level, ": Anteil der Belegungen in ", subject ," im Vergleich in ", timerange,
                                 "<br><br><br>"),
                  fill = "") +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))

}

#' A function to create a dumbbell plot
#'
#' @description A function to compare a subject for different Bundesländer
#'
#' @return The return value is a dumbbell plot
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

bundeslaender_ranking <- function(df,r, type) {

  # load UI inputs from reactive value
  timerange <- r$date_studium_studienzahl_bl_gender_vergleich

  lehramt <- r$nurLehramt_studium_studienzahl_bl_gender_vergleich

  hochschulform_select_1 <- r$hochschulform_studium_studienzahl_bl_gender_vergleich1

  hochschulform_select_2 <- r$hochschulform_studium_studienzahl_bl_gender_vergleich2

  subject <- r$subject_studium_studienzahl_bl_gender_vergleich

  # filter dataset based on UI inputs
  df <- df %>% dplyr::mutate(fachbereich = replace(fachbereich,
                                                   fachbereich == "Alle",
                                                   "Alle restlichen Fächer"))

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(region != "Baden-Württemberg")

  df <- df %>% dplyr::mutate(indikator = replace(indikator,
                                                 indikator == "Studienanfänger",
                                                 "Studienanfängerinnen"))

  df <- calc_share_male_bl(df)

  # calculate new "Gesamt"
  df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
    dplyr::mutate(wert = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")

  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

    title_help_sub_sub <- " insgesamt"

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)

    title_help_sub_sub <- " an einer Uni (nur Lehramt)"
  }

  # aggregate all subjects to calculate proportion later
  df <- df %>% dplyr::group_by(indikator, region, nur_lehramt, hochschulform) %>%
    dplyr::mutate(props = sum(wert))

  # aggregate to MINT
  values_Mint <- df %>%
    dplyr::filter(fachbereich != "Alle restlichen Fächer") %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht, hochschulform,
                    nur_lehramt, props) %>%
    dplyr::summarise(wert = sum(wert)) %>%
    dplyr::mutate(bereich = "Hochschule",
                  fachbereich = "MINT (aggregiert)")

  df <- rbind(df, values_Mint)

  # # calculate proportion
  df <- df %>% dplyr::group_by(fachbereich, indikator, hochschulform, region, anzeige_geschlecht) %>%
    dplyr::summarize(proportion = wert/props) %>% dplyr::ungroup()

  df$proportion <- df$proportion * 100

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")

  df <- df %>% dplyr::filter(fachbereich == subject)

  # spread column
  df <- tidyr::spread(df, indikator, proportion)

  df <- df %>% tidyr::drop_na()

  df <- df %>% dplyr::select(-hochschulform, -anzeige_geschlecht)

  df2 <- tidyr::gather(df, group, value, -region) %>%
    dplyr::filter(group != "fachbereich") %>%
    dplyr::mutate(value = as.numeric(value))

  df$region <- reorder(df$region, df$Studierende)

  df2$region <- factor(df2$region, levels = levels(df$region))

  ggplot2::ggplot(df,
                  ggplot2::aes(y = region)) +
    ggplot2::geom_point(data = df2, ggplot2::aes(x = value, color = group), size = 5) +
    ggalt::geom_dumbbell(
      ggplot2::aes(x = Studienanfängerinnen, xend = Studierende),
      size = 0.5,
      size_x = 5,
      size_xend = 5,
      colour = "black",
      colour_x = "#b1b5c366",
      colour_xend = "#f5adac66",
      dot_guide=TRUE) +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(name = "", values = c("#b1b5c366", "#f5adac66")) +
    ggplot2::theme(legend.position="top",
                   panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
                   plot.title = ggtext::element_markdown(hjust = 0.5),
                   axis.text.y = ggplot2::element_text(size = 11)) +
    ggplot2::ylab("") + ggplot2::xlab("") +
    ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
                                 "Relativer Anteil von Studentinnen als Studienanfängerinnen oder Studierende in ",timerange,
                                 "<br><br><br>"),
                  color = "") +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))

}
