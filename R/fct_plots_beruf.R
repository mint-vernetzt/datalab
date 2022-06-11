#' A function to plot a graph.
#'
#' @description A function to create a pie chart for the first box
#' inside the tab "Beruf".
#'
#' @return The return value is a plot
#' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_einstieg_pie <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt_einstieg

  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- df %>% dplyr::filter(anforderungsniveau == "Gesamt")

  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
    df[df$fachbereich == "MINT", "wert"]

  df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"

  # calculate the share of males
  df <- calc_arbeitsmarkt_males(df)

  df_beschaeftigte <- df %>% dplyr::filter(indikator == "Beschäftigte")

  df_beschaeftigte <- df_beschaeftigte %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

  # calculate proportions
  df_beschaeftigte <- share_pie(df_beschaeftigte)

  df_beschaeftigte$anzeige_geschlecht <- df_beschaeftigte$fachbereich

  df_auszubildende <- df %>% dplyr::filter(indikator == "Auszubildende")

  df_auszubildende <- df_auszubildende %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

  # calculate proportions
  df_auszubildende <- share_pie(df_auszubildende)

  df_auszubildende$anzeige_geschlecht <- df_auszubildende$fachbereich

  plot_auszubildende <- highcharter::hchart(df_auszubildende, size = 280, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = proportion)) %>%
    highcharter::hc_tooltip(
      pointFormat=paste('Anteil: {point.percentage:.1f}%')) %>%
    highcharter::hc_colors(c("#154194", 'rgba(21, 65, 148, 0.50)',"#b16fab", 'rgba(177, 111, 171, 0.50)')) %>%
    highcharter::hc_title(text = paste0("Anteil von MINT an allen anderen Berufszweigen für Auszubildende in ", timerange),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
    highcharter::hc_legend(enabled = TRUE) %>%
    highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                           dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE))



  plot_beschaeftigte <- highcharter::hchart(df_beschaeftigte, size = 280, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = proportion)) %>%
    highcharter::hc_tooltip(
      pointFormat=paste('Anteil: {point.percentage:.1f}%')) %>%
    highcharter::hc_colors(c("#154194", 'rgba(21, 65, 148, 0.50)',"#b16fab", 'rgba(177, 111, 171, 0.50)')) %>%
    highcharter::hc_title(text = paste0("Anteil von MINT an allen anderen Berufszweigen für Beschäftigte in ", timerange),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
    highcharter::hc_legend(enabled = TRUE) %>%
    highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                           dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE))



    plot_beschaeftigte <- plot_beschaeftigte %>% highcharter::hc_colors(c("#154194","#b16fab"))

    plot_auszubildende <- plot_auszubildende %>% highcharter::hc_colors(c("#154194","#b16fab"))


  highcharter::hw_grid(

    plot_auszubildende,

    plot_beschaeftigte,

    ncol = 2,
    browsable = TRUE
  )


}


#' A function to return a filtered dataset
#'
#' @description A function to similar to 'arbeitsmarkt_einstieg_bar' but with the
#' difference that it returns a dataframe instead of plot.
#'
#' @return The return value is a dataframe
#' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

data_einstieg_beruf <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt_einstieg

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- df %>% dplyr::filter(anforderungsniveau == "Gesamt")

  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
    df[df$fachbereich == "MINT", "wert"]

  df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"

  # calculate the share of males
  df <-   df <- calc_arbeitsmarkt_males(df)

  colnames(df) <- c("Region", "Fachbereich", "Anforderungsniveau", "Wert", "Indikator", "Jahr", "Geschlecht", "Bereich")

  return(df)

}

#' A function to plot a waffle chart
#'
#' @description A function to create a waffle chart for the second box inside the
#' tab "Beruf".
#'
#' @return The return value is a waffle chart
#' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
arbeitnehmer_waffle <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt

  anforderung <- r$anforderungsniveau_arbeitsmarkt

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- df %>% dplyr::filter(anforderungsniveau == anforderung)

  df <- df %>% dplyr::filter(fachbereich == "MINT")

  # calculate the share of males
  values <- df %>%
    dplyr::group_by(indikator) %>%
    dplyr::mutate(wert = wert - dplyr::lead(wert)) %>% dplyr::select(wert) %>% na.omit()

  df[df$anzeige_geschlecht == "Gesamt", "wert"] <- values$wert

  df[df$anzeige_geschlecht == "Gesamt", "anzeige_geschlecht"] <- "Männer"


  # calculate proportions
  df <- df %>% dplyr::group_by(indikator) %>%
    dplyr::mutate(props = sum(wert))


  df <- df %>% dplyr::group_by(anzeige_geschlecht, indikator, fachbereich) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100

  df_beschaeftigte <- df %>% dplyr::filter(indikator == "Beschäftigte")


  df_beschaeftigte$anzeige_geschlecht <- paste0(df_beschaeftigte$anzeige_geschlecht, " (", df_beschaeftigte$fachbereich, ")")

  # ensure proportions sum to 1
  x_beschaeftigte <- setNames(round_preserve_sum(as.numeric(df_beschaeftigte$proportion),0),
                              df_beschaeftigte$anzeige_geschlecht)

  x_beschaeftigte <- x_beschaeftigte[order(factor(names(x_beschaeftigte), levels = c('Frauen (MINT)', 'Männer (MINT)')))]

  df_auszubildende <- df %>% dplyr::filter(indikator == "Auszubildende")

  df_auszubildende$anzeige_geschlecht <- paste0(df_auszubildende$anzeige_geschlecht, " (", df_auszubildende$fachbereich, ")")

  # ensure proportions sum to 1
  x_auszubildende <- setNames(round_preserve_sum(as.numeric(df_auszubildende$proportion),0),
                              df_auszubildende$anzeige_geschlecht)

  x_auszubildende <- x_auszubildende[order(factor(names(x_auszubildende), levels = c('Frauen (MINT)', 'Männer (MINT)')))]


  # create plot objects for waffle charts
  waffle_aus <- waffle::waffle(x_auszubildende, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", "**Auszubildende**</span>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "left") +
    ggplot2::scale_fill_manual(
      values =  c("#b16fab",
                  "#b1b5c3"),
      na.value="#b1b5c3",
      limits = c("Frauen (MINT)", "Männer (MINT)"),
      guide = ggplot2::guide_legend(reverse = TRUE),
      labels = c(
        paste0("Frauen (MINT)",", ",x_auszubildende[1], "%"),
        paste0("Männer (MINT)",", ",x_auszubildende[2], "%"))) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=4,byrow=TRUE))



  waffle_be <- waffle::waffle(x_beschaeftigte, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", "**Beschäftigte**</span>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "right") +
      ggplot2::scale_fill_manual(
        values =  c("#b16fab",
                    "#b1b5c3"),
        na.value="#b1b5c3",
        limits = c("Frauen (MINT)", "Männer (MINT)"),
        guide = ggplot2::guide_legend(reverse = TRUE),
        labels = c(
          paste0("Frauen (MINT)",", ",x_beschaeftigte[1], "%"),
          paste0("Männer (MINT)",", ",x_beschaeftigte[2], "%"))) +
      ggplot2::guides(fill=ggplot2::guide_legend(nrow=4,byrow=TRUE))




  if (anforderung == "Gesamt"){

    title_help_sub <- " insgesamt"

  } else {

    title_help_sub <- paste0(" (",anforderung,")")

  }



  plot <- ggpubr::ggarrange(waffle_aus, NULL ,waffle_be, widths = c(1, 0.1, 1), nrow=1)
  text <- c(
    paste0("<span style='font-size:20.5pt; color:black'> Anteil von Frauen und Männer ", title_help_sub," an MINT und <br> allen anderen Berufszweigen in ", timerange))

  ggpubr::annotate_figure(plot, gridtext::richtext_grob(text = text))

}

#' A function to create a paired bar plot
#'
#' @description A function to return a paired bar plot for the second box inside
#' the tab "Beruf"
#'
#' @return The return value is a bar plot
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_absolut <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt

  anforderung <- r$anforderungsniveau_arbeitsmarkt


  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- df %>% dplyr::filter(anforderungsniveau == anforderung)

  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
    df[df$fachbereich == "MINT", "wert"]

  df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"

  # calculate the share of males
  values <- df %>%
    dplyr::group_by(fachbereich, indikator) %>%
    dplyr::mutate(wert = wert - dplyr::lead(wert)) %>% dplyr::select(wert) %>% na.omit()

  df[df$anzeige_geschlecht == "Gesamt", "wert"] <- values$wert

  df[df$anzeige_geschlecht == "Gesamt", "anzeige_geschlecht"] <- "Männer"


  if (anforderung == "Gesamt"){

    title_help_sub <- " insgesamt"

  } else {

    title_help_sub <- paste0(" (",anforderung,")")

  }
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
    ggplot2::scale_fill_manual(values = c("#b16fab", "#154194")) +
    ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
                                 "Arbeitnehmer*innen ", title_help_sub," in MINT und allen anderen Berufszweigen in ", timerange,
                                 "<br><br><br>"),
                  fill = "")


}


#' A function to plot the german map
#'
#' @description A function to plot the german map with all states that contain
#' information about the share of women in STEM
#'
#' @return The return value is the german map with information
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_bl_gender <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt_bl_gender

  anforderung <- r$anforderungsniveau_arbeitsmarkt_bl_gender

  indikator_choice <- r$level_arbeitsmarkt_bl_gender

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(fachbereich != "Alle")

  df <- df %>% dplyr::filter(indikator == indikator_choice)

  df <- calc_arbeitsmarkt_males(df)

  df <- calc_arbeitsmarkt_share_bl_gender(df)

  df <- df %>% dplyr::filter(anforderungsniveau == anforderung)

  values_female <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")
  values_male <- df %>% dplyr::filter(anzeige_geschlecht == "Männer")

  if (anforderung == "Gesamt"){

    title_help_sub <- " insgesamt"

  } else {

    title_help_sub <- paste0(" mit Anforderungsniveau ", anforderung)

  }

  highcharter::hw_grid(
    # plot
    highcharter::hcmap(
      "countries/de/de-all",
      data = values_female,
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
      highcharter::hc_title(
        text = paste0("Weibliche ", indikator_choice, title_help_sub),
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
      data = values_male,
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
      highcharter::hc_title(
        text = paste0("Männliche ", indikator_choice, title_help_sub),
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
#' @description A function to similar to 'arbeitsmarkt_einstieg_bar' but with the
#' difference that it returns a dataframe instead of plot.
#'
#' @return The return value is a dataframe
#' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

data_mix_beruf <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt

  anforderung <- r$anforderungsniveau_arbeitsmarkt

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(anforderungsniveau == anforderung)

  df <- df %>% dplyr::filter(fachbereich != "Alle")

  colnames(df) <- c("Region", "Fachbereich", "Anforderungsniveau", "Wert", "Indikator", "Jahr", "Geschlecht", "Bereich")

  return(df)

}




#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_verlauf <- function(df,r) {

  # load UI inputs from reactive value
  status_arbeitnehmer <- r$indikator_arbeitsmarkt_verlauf

  timerange <- r$date_arbeitsmarkt_verlauf

  states <- r$states_arbeitsmarkt_verlauf

  topic <- r$topic_arbeitsmarkt_verlauf

  anforderung <- r$anforderungsniveau_arbeitsmarkt_verlauf

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == status_arbeitnehmer)

  df <- df %>% dplyr::filter(anforderungsniveau == anforderung)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
    df[df$fachbereich == "MINT", "wert"]

  df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"

  # include "Osten" und "Westen" in Dataframe
  df <- prep_arbeitsmarkt_east_west(df)

  # order
  df <- df[with(df, order(region, fachbereich, jahr, decreasing = TRUE)), ]

  # calculate proportion
  df <-  df %>%
    dplyr::group_by(region, fachbereich, jahr) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"]/
                    wert[anzeige_geschlecht == "Gesamt"])

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt")


  df <- df %>% dplyr::filter(region %in% states)

  # filter MINT or remaining subjects
  df <- df %>% dplyr::filter(fachbereich == topic)

  # order years for plot
  df <- df[with(df, order(region, jahr, decreasing = TRUE)), ]

  df$props <- df$props * 100

  if (anforderung == "Gesamt"){

    title_help_sub_sub <- " insgesamt"

  } else {

    title_help_sub_sub <- paste0(" (",anforderung,")")

  }


  if (status_arbeitnehmer == "Auszubildende"){

    title_help_sub <- " in Ausbildung"

  }else{

    title_help_sub <- " in Beschäftigung"

  }

  if (topic == "MINT"){

    title_help <- paste0("MINT", title_help_sub, title_help_sub_sub)

  }else {

    title_help <- paste0("allen anderen Berufszweigen", title_help_sub, title_help_sub_sub)

  }

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(props), group = region)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil Frauen <br> Bundesland: {point.region} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), style = list(fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil von Frauen an ", title_help ," im Verlauf"),
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
#' @description A function to similar to 'arbeitsmarkt_einstieg_bar' but with the
#' difference that it returns a dataframe instead of plot.
#'
#' @return The return value is a dataframe
#' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

data_verlauf_beruf <- function(df,r) {

  # load UI inputs from reactive value
  status_arbeitnehmer <- r$indikator_arbeitsmarkt_verlauf

  timerange <- r$date_arbeitsmarkt_verlauf

  states <- r$states_arbeitsmarkt_verlauf

  topic <- r$topic_arbeitsmarkt_verlauf

  anforderung <- r$anforderungsniveau_arbeitsmarkt_verlauf

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == status_arbeitnehmer)

  df <- df %>% dplyr::filter(anforderungsniveau == anforderung)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
    df[df$fachbereich == "MINT", "wert"]

  df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"

  # include "Osten" und "Westen" in Dataframe
  df <- prep_arbeitsmarkt_east_west(df)

  # order
  df <- df[with(df, order(region, fachbereich, jahr, decreasing = TRUE)), ]


  df <-  df %>%
    dplyr::group_by(region, fachbereich, jahr, anforderungsniveau, indikator, bereich) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"]/
                    wert[anzeige_geschlecht == "Gesamt"])

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt")


  df <- df %>% dplyr::filter(region %in% states)

  # filter MINT or remaining subjects
  df <- df %>% dplyr::filter(fachbereich == topic)

  # order years for plot
  df <- df[with(df, order(region, jahr, decreasing = TRUE)), ]

  df$props <- df$props * 100

  colnames(df) <- c("Region", "Fachbereich", "Anforderungsniveau", "Wert","Indikator", "Jahr","Geschlecht",
                        "Bereich", "Anteil")

  return(df)

}

#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_verlauf_bl <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt_verlauf_bl

  states <- r$states_arbeitsmarkt_verlauf_bl

  topic <- r$topic_arbeitsmarkt_verlauf_bl

  anforderung <- r$anforderungsniveau_arbeitsmarkt_verlauf_bl

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(anforderungsniveau == anforderung)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
    df[df$fachbereich == "MINT", "wert"]

  df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"

  # include "Osten" und "Westen" in Dataframe
  df <- prep_arbeitsmarkt_east_west(df)

    # calculate the share of males
  df <- calc_arbeitsmarkt_males(df)

  # calculate new "Gesamt"
  df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  # calcualte proportions
  df <- df %>% dplyr::group_by(region, indikator, fachbereich, anzeige_geschlecht, jahr) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100



  df <- df %>% dplyr::filter(region %in% states)

  df <- df %>% dplyr::filter(fachbereich == topic)

  df$anzeige_geschlecht <- paste0(df$anzeige_geschlecht, " (", df$indikator, ")")



  # order years for plot
  df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

  if (anforderung == "Gesamt"){

    title_help_sub_sub <- " insgesamt"

  } else {

    title_help_sub_sub <- paste0(" (",anforderung,")")

  }


  if (topic == "MINT"){

    title_help <- paste0("MINT", title_help_sub_sub)

  }else {

    title_help <- paste0("allen anderen Berufszweigen", title_help_sub_sub)

  }


  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = anzeige_geschlecht)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil {point.anzeige_geschlecht} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), style = list(fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil von Frauen und Männer ", title_help ," im Verlauf"),
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
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

beruf_verlauf_single <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt_einstieg_verlauf

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  # remove
  df <- df %>% dplyr::filter(region == "Deutschland")
  df <- df %>% dplyr::filter(anforderungsniveau == "Gesamt")
  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

  df <- calc_arbeitsmarkt_mint(df)
  df <- calc_arbeitsmarkt_males(df)

  df <- df %>%
    dplyr::group_by(jahr, indikator) %>%
    dplyr::mutate(sum_wert = sum(wert))

  # calcualte proportions
  df <- df %>% dplyr::group_by(jahr, indikator, fachbereich) %>%
    dplyr::summarize(proportion = wert/sum_wert)

  df$proportion <- df$proportion * 100

  # order years for plot
  df <- df[with(df, order(jahr, decreasing = FALSE)), ]

  df <- df %>% dplyr::filter(fachbereich == "MINT")

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

#' A function to plot bar plot
#'
#' @description A function to plot bar plots
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

beruf_einstieg_vergleich <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt_einstieg_vergleich

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  # remove
  df <- df %>% dplyr::filter(region == "Deutschland")
  df <- df %>% dplyr::filter(anforderungsniveau == "Gesamt")
  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

  df <- calc_arbeitsmarkt_mint(df)
  df <- calc_arbeitsmarkt_males(df)

  df <- df %>%
    dplyr::group_by(jahr, indikator) %>%
    dplyr::mutate(sum_wert = sum(wert))

  # calcualte proportions
  df <- df %>% dplyr::group_by(jahr, indikator, fachbereich) %>%
    dplyr::summarize(proportion = wert/sum_wert)

  df$proportion <- df$proportion * 100

  # plot
  highcharter::hchart(df, 'bar', highcharter::hcaes(y = round(proportion), x = indikator, group = "fachbereich")) %>%
    highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Anteil"), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
    highcharter::hc_colors(c("#154194", "#b16fab")) %>%
    highcharter::hc_title(text = "MINT-Anteile im Vergleich",
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

arbeitsmarkt_bl_gender_verlauf <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_beruf_arbeitsmarkt_bl_gender_verlauf

  anforderung <- r$anforderungsniveau_beruf_arbeitsmarkt_bl_gender_verlauf

  indikator_choice <- r$indikator_beruf_arbeitsmarkt_bl_gender_verlauf

  states <- r$states_beruf_arbeitsmarkt_bl_gender_verlauf

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == indikator_choice)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- prep_arbeitsmarkt_east_west(df)

  df <- df %>% dplyr::filter(region %in% states)

  df <- calc_arbeitsmarkt_males(df)

  df <- calc_arbeitsmarkt_share_bl_gender(df)

  df <- df %>% dplyr::filter(anforderungsniveau %in% anforderung)

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")

  df <- df %>% dplyr::filter(fachbereich == "MINT")

  # order years for plot
  df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

  if (anforderung == "Gesamt"){

    title_help <- " insgesamt"

  } else {

    title_help <- paste0(" mit Anforderungsniveau ", anforderung)

  }

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = region)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil <br> Bundesland: {point.region} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Weibliche ", indikator_choice, " in MINT-Berufen",title_help),
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
#' @description A function to return a ranking of subject for both genders
#'
#' @return The return value is a bar plot
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_bl_gender_vergleich <- function(df, r) {

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt_bl_gender_vergleich

  anforderung <- r$anforderungsniveau_arbeitsmarkt_bl_gender_vergleich

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- calc_arbeitsmarkt_males(df)

  df <- calc_arbeitsmarkt_share_bl_gender(df)

  df <- df %>% dplyr::filter(anforderungsniveau == anforderung)

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")

  df <- df %>% dplyr::filter(fachbereich == "MINT")

  df <- df %>% dplyr::select(-"wert")

  # spread column
  df <- tidyr::spread(df, indikator, proportion)

  df <- df %>% tidyr::drop_na()

  df2 <- tidyr::gather(df, group, value, -region) %>%
    dplyr::filter(group %in% c("Beschäftigte", "Auszubildende")) %>%
    dplyr::mutate(value = as.numeric(value))

  df$region <- reorder(df$region, df$Beschäftigte)

  df2$region <- factor(df2$region, levels = levels(df$region))

  if (anforderung == "Gesamt"){

    title_help <- " insgesamt"

  } else {

    title_help <- paste0(" mit Anforderungsniveau ", anforderung)

  }

  ggplot2::ggplot(df,
                  ggplot2::aes(y = region)) +
    ggplot2::geom_point(data = df2, ggplot2::aes(x = value, color = group), size = 5) +
    ggalt::geom_dumbbell(
      ggplot2::aes(x = Auszubildende, xend = Beschäftigte),
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
                                 "Relativer Anteil von Arbeitnehmerinnen in Ausbildung und Beschäftigung", title_help, " in ",timerange,
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
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_bl <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt_bl

  anforderung <- r$anforderungsniveau_arbeitsmarkt_bl

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

  df <- calc_arbeitsmarkt_share_bl(df)

  df <- df %>% dplyr::filter(fachbereich == "MINT")

  df <- df %>% dplyr::filter(anforderungsniveau == anforderung)

  df_employed <- df %>% dplyr::filter(indikator == "Beschäftigte")
  df_trainee <- df %>% dplyr::filter(indikator == "Auszubildende")

  if (anforderung == "Gesamt"){

    title_help_sub <- " insgesamt"

  } else {

    title_help_sub <- paste0(" mit Anforderungsniveau ", anforderung)

  }

  highcharter::hw_grid(
    # plot
    highcharter::hcmap(
      "countries/de/de-all",
      data = df_trainee,
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
      highcharter::hc_title(
        text = paste0("Auszubildende: Anzeil an Beschäftigung", title_help_sub),
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
      data = df_employed,
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
      highcharter::hc_title(
        text = paste0("Beschäftigte: Anzeil an Beschäftigung", title_help_sub),
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
#' @description A function to plot the time series
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_bl_verlauf <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_beruf_arbeitsmarkt_bl_verlauf

  anforderung <- r$anforderungsniveau_beruf_arbeitsmarkt_bl_verlauf

  indikator_choice <- r$indikator_beruf_arbeitsmarkt_bl_verlauf

  states <- r$states_beruf_arbeitsmarkt_bl_verlauf

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == indikator_choice)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- prep_arbeitsmarkt_east_west(df)

  df <- df %>% dplyr::filter(region %in% states)

  df <- calc_arbeitsmarkt_males(df)

  df <- calc_arbeitsmarkt_share_bl(df)

  df <- df %>% dplyr::filter(anforderungsniveau %in% anforderung)

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

  df <- df %>% dplyr::filter(fachbereich == "MINT")

  # order years for plot
  df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

  if (anforderung == "Gesamt"){

    title_help <- " insgesamt"

  } else {

    title_help <- paste0(" mit Anforderungsniveau ", anforderung)

  }

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = region)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil <br> Bundesland: {point.region} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Weibliche ", indikator_choice, " in MINT-Berufen",title_help),
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

arbeitsmarkt_bl_vergleich <- function(df,r) {

  browser()
  # load UI inputs from reactive value
  timerange <- r$date_beruf_arbeitsmarkt_bl_vergleich

  anforderung <- r$anforderungsniveau_beruf_arbeitsmarkt_bl_vergleich

  indikator_choice <- r$indikator_beruf_arbeitsmarkt_bl_vergleich

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(indikator == indikator_choice)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  # df <- prep_arbeitsmarkt_east_west(df)

  df <- calc_arbeitsmarkt_males(df)

  df <- calc_arbeitsmarkt_share_bl(df)

  df <- df %>% dplyr::filter(anforderungsniveau %in% anforderung)

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

  df <- df %>% dplyr::filter(fachbereich == "MINT")

  # order years for plot
  df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

  if (anforderung == "Gesamt"){

    title_help <- " insgesamt"

  } else {

    title_help <- paste0(" mit Anforderungsniveau ", anforderung)

  }

  # plot
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
                                 "Anteil der Fächer im Vergleich",
                                 "<br><br><br>"),
                  fill = "") +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))
}
