#' A function to plot a waffle chart
#'
#' @description A function to create a waffle chart for the second box inside the
#' tab "Ausbildung".
#'
#' @return The return value is a waffle chart
#' @param df The dataframe "data_naa" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

ausbildungsvertraege_waffle <- function(df, r) {

  # load UI inputs from reactive value
  ausbildung_bereich <- r$indikator_ausbildungsvertraege

  timerange <- r$date_ausbildungsvertraege

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- df %>% dplyr::filter(fachbereich == ausbildung_bereich)

  df <- df %>% dplyr::group_by(anzeige_geschlecht, fachbereich) %>%
    dplyr::summarize(proportion = wert/ df[df$anzeige_geschlecht == "Gesamt", "wert"][[1]])

  df$proportion <- df$proportion * 100

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

  x_mint <- setNames(round_preserve_sum(as.numeric(df$proportion),0),
                     df$anzeige_geschlecht)

  # create plot objects for waffle charts
  waffle_mint <- waffle::waffle(x_mint, keep = FALSE, colors = colors_mint_vernetzt$gender) +
    ggplot2::labs(
      subtitle = paste0("<span style='font-size:16.0pt;'>" ,x_mint[1],"% <span style='color:#f5adac; font-size:16.0pt;'> Frauen</span> vs. ",
                        "<span style='font-size:16.0pt;'>", x_mint[2],"% <span style='color:#b1b5c3; font-size:16.0pt;'> Männer</span>"),
      title = paste0("<span style='color:#b16fab;'>", "**MINT**</span>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(family="serif", size = 14),
                   plot.margin = ggplot2::unit(c(2.5,0,0,0), "lines"))


  plot <- ggpubr::ggarrange(waffle_mint,
                            legend="bottom")

  text <- c(
    paste0("<span style='font-size:20pt; color:black; font-family: serif'> Anteil von Frauen und Männern im Bereich ", ausbildung_bereich,
           " für das Jahr ",timerange))
  ggpubr::annotate_figure(plot, gridtext::richtext_grob(text = text))


}

#' A function to create a bar plot
#'
#' @description A function to return a bar plot for the second box inside
#' the tab "Ausbildung"
#'
#' @return The return value is a bar plot
#' @param data The dataframe "data_naa" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

ausbildungsvertraege_absolut <- function(df,r) {

  # load UI inputs from reactive value
  ausbildung_bereich <- r$indikator_ausbildungsvertraege

  timerange <- r$date_ausbildungsvertraege

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- df %>% dplyr::filter(fachbereich == ausbildung_bereich)

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

  # plot
  ggplot2::ggplot(df, ggplot2::aes(x=anzeige_geschlecht, y=wert, fill = anzeige_geschlecht)) +
    ggplot2::geom_bar(stat="identity", position = "dodge") +
    ggplot2::geom_text(ggplot2::aes(label=wert, vjust = - 0.25),
                       position=ggplot2::position_dodge(width=0.9),
                       fontface = "bold") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      text = ggplot2::element_text(family="serif", size = 14),
      plot.title = ggtext::element_markdown(hjust = 0.5)) +
    ggplot2::xlab("") + ggplot2::ylab("Anzahl") +
    ggplot2::scale_fill_manual(values = colors_mint_vernetzt$gender) +
    ggplot2::labs(title = paste0("<span style='font-size:20pt; color:black; font-family: serif'>",
                                 "Neue Auszubildende im Bereich ", ausbildung_bereich," für das Jahr ", timerange,
                                 "<br><br><br>"),
                  fill = "")


}


#' A function to plot the german map
#'
#' @description A function to plot the german map with all states that contain
#' information about the share of women in STEM
#'
#' @return The return value is the german map with information
#' @param data The dataframe "data_naa" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

vertraege_map <- function(df,r) {

  # load UI inputs from reactive value
  ausbildung_bereich <- r$indikator_ausbildungsvertraege

  timerange <- r$date_ausbildungsvertraege

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(region != "Osten")

  df <- df %>% dplyr::filter(region != "Westen")

  df <- df %>% dplyr::filter(fachbereich == ausbildung_bereich)

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")

  df <- df %>% dplyr::group_by(region) %>%
    dplyr::summarize(proportion = dplyr::lead(wert)/wert) %>% na.omit()

  df$proportion <- df$proportion * 100

  # plot
  highcharter::hcmap(
    "countries/de/de-all",
    data = df,
    value = "proportion",
    joinBy = c("name", "region"),
    borderColor = "#FAFAFA",
    name = "Anteil Frauen an MINT",
    borderWidth = 0.1,
    tooltip = list(
      valueDecimals = 0,
      valueSuffix = "%"
    )
  ) %>%
    highcharter::hc_title(
      text = paste0("Anteil der Frauen im Bereich", ausbildung_bereich ," für das Jahr ", timerange),
      margin = 45,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "serif")
    ) %>%
    highcharter::hc_caption(
      text = "Quelle:",  style = list(fontSize = "12px")
    ) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "serif")
    )



}


#' A function to create a lollipop plot
#'
#' @description A function to return a ranking of subject for females
#'
#' @return The return value is a lollipop plot
#' @param data The dataframe "data_naa" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

vertraege_ranking <- function(df, r) {

  # load UI inputs from reactive value
  timerange <- r$date_ausbildungsvertraege

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")

  df <- df %>% dplyr::group_by(fachbereich) %>%
    dplyr::summarize(proportion = dplyr::lead(wert)/wert) %>% na.omit()

  df$proportion <- df$proportion * 100


  ggplot2::ggplot(df, ggplot2::aes(y = fachbereich, x = proportion, label = paste0(round(proportion),"%"))) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = reorder(fachbereich, proportion), xend = proportion, yend = fachbereich),
                          color = "#b16fab") +
    ggplot2::geom_point(size = 2, color = "#b16fab") +
    ggplot2::geom_text(nudge_x = 7) +
    ggplot2::theme_classic() +
    ggplot2::labs(title = paste0("<span style='font-size:20pt; color:black; font-family: serif'>",
                                 "Anteil von Frauen an neuen Ausbildungsverträgen für alle MINT-Bereiche", timerange,
                                 "<br><br><br>"),
                  y = "", x = "Anteil") +
    ggplot2::theme(text = ggplot2::element_text(family="serif", size = 14),
                   plot.title = ggtext::element_markdown(hjust = 0.5)) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100))



}


#' A function to create a lollipop plot
#'
#' @description A function to return a ranking of subject for females
#'
#' @return The return value is a bar plot
#' @param data The dataframe "data_naa" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

vertraege_verlauf <- function(df, r) {

  # load UI inputs from reactive value
  ausbildung_bereich <- r$indikator_ausbildung_verlauf

  states <- r$states_ausbildung_verlauf

  ost_west <- r$ost_west

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")

  df <- df %>% dplyr::filter(fachbereich == ausbildung_bereich)

  df <- df %>% dplyr::group_by(region, jahr) %>%
    dplyr::summarize(proportion = dplyr::lead(wert)/wert) %>% na.omit()

  df$proportion <- df$proportion * 100

  if (ost_west == "Ja"){

    df <- df %>% dplyr::filter((region == "Osten" | region == "Westen"))

  }else {

    df <- df %>% dplyr::filter((region != "Osten" | region != "Westen"))

    df <- df %>% dplyr::filter(region %in% states)

  }

  p_17 <- df %>% dplyr::filter(jahr == 2017) %>%
  ggplot2::ggplot(ggplot2::aes(y = region, x = proportion, label = paste0(round(proportion),"%"))) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = reorder(region, proportion), xend = proportion, yend = region),
                          color = "#b16fab") +
    ggplot2::geom_point(size = 2, color = "#b16fab") +
    ggplot2::geom_text(nudge_x = 7) +
    ggplot2::theme_classic() +
    ggplot2::labs(title = paste0("<span style='font-size:20pt; color:black; font-family: serif'>",
                                 "2017",
                                 "<br><br><br>"),
                  y = "", x = "Anteil") +
    ggplot2::theme(text = ggplot2::element_text(family="serif", size = 14),
                   plot.title = ggtext::element_markdown(hjust = 0.5),
                   plot.margin = ggplot2::unit(c(2.5,0,0,0), "lines")) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100))

  p_20 <- df %>% dplyr::filter(jahr == 2020) %>%
  ggplot2::ggplot(ggplot2::aes(y = region, x = proportion, label = paste0(round(proportion),"%"))) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = reorder(region, proportion), xend = proportion, yend = region),
                          color = "#b16fab") +
    ggplot2::geom_point(size = 2, color = "#b16fab") +
    ggplot2::geom_text(nudge_x = 7) +
    ggplot2::theme_classic() +
    ggplot2::labs(title = paste0("<span style='font-size:20pt; color:black; font-family: serif'>",
                                 "2020",
                                 "<br><br><br>"),
                  y = "", x = "Anteil") +
    ggplot2::theme(text = ggplot2::element_text(family="serif", size = 14),
                   plot.title = ggtext::element_markdown(hjust = 0.5),
                   plot.margin = ggplot2::unit(c(2.5,0,0,0), "lines")) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100))


  text <- c(
    paste0("<span style='font-size:20pt; color:black; font-family: serif'> Anteil von Frauen an neuen Ausbildungsverträgen für den Bereich ",
           ausbildung_bereich))


  plot <- ggpubr::ggarrange(p_17, NULL, p_20, widths = c(1, 0.2, 1), nrow=1)

  ggpubr::annotate_figure(plot, gridtext::richtext_grob(text = text))

}


