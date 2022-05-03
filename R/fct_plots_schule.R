

#' A function to plot a graph.
#'
#' @description A function to create a stacked bar chart for the first box
#' inside the tab "Schule".
#'
#' @return The return value is a plot
#' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

kurse_einstieg_bar <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_kurse_einstieg

  level_kurs <- r$indikator_kurse_einstieg

  switch_absolut <- r$switch_rel_abs

  geschlecht <- r$geschlecht_kurse_einstieg

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == level_kurs)

  df <- df %>% dplyr::filter(region == "Deutschland")

  # combine subjects to get numbers on share of MINT
  # make a function out of it
  subjects <- c("Mathematik", "Informatik", "Naturwissenschaften",  "Biologie", "Chemie",
                "Physik", "andere naturwiss.-technische Fächer",  "Erdkunde", "Alle Fächer")

  df <- df %>% dplyr::filter(fachbereich %in% subjects)

  df$fachbereich <- ifelse(df$fachbereich != "Alle Fächer", "MINT", "andere Fächer")

  df <- df %>% dplyr::group_by(fachbereich, anzeige_geschlecht, jahr) %>%
    dplyr::summarize(wert = sum(wert))


  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "andere Fächer", "wert"] <- df[df$fachbereich == "andere Fächer", "wert"] -
    df[df$fachbereich == "MINT", "wert"]


  df <- df %>% dplyr::filter(anzeige_geschlecht %in% geschlecht)


  values <- df %>% dplyr::group_by(anzeige_geschlecht, jahr) %>%
    dplyr::summarize(wert = sum(wert))

  tooltip_1 <- round((df[df$fachbereich == "andere Fächer", "wert"]/values$wert)*100)

  tooltip_2 <- round((df[df$fachbereich == "MINT", "wert"]/values$wert)*100)

  tooltip <- c(tooltip_1[[1]], tooltip_2[[1]])

  df$Anteil <- paste(tooltip,"%")

  # remove scientific notation
  options(scipen=999)

  if (level_kurs == "Grundkurse"){

    title_help <- "für die Grundkurse"

  }else{

    title_help <- "für die Leistungskurse"

  }
  names(df)[4] <- "Wert"

  p <- ggplot2::ggplot(df, ggplot2::aes(fill=fachbereich, y=Wert, x=anzeige_geschlecht, tooltip = Anteil)) +
    ggplot2::labs(caption = "Quelle:", title = paste0("Anteile an MINT und allen anderen Schulfächern ", title_help),
                  fill = "") +
    ggplot2::facet_grid(~jahr,
                        scales = "free_x",
                        space = "free_x",
                        switch = "x")  +
    ggplot2::theme(strip.placement = "outside",
                   plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
                   panel.background = ggplot2::element_rect(fill="white"),
                   strip.background = ggplot2::element_rect(fill = "white"),
                   axis.title = ggplot2::element_blank()) +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::scale_fill_manual(values = colors_mint_vernetzt$general)

  t <- list(
    family = "serif", size = 14)

  if(isTRUE(switch_absolut)){

    p <- p + ggplot2::geom_bar(position="stack", stat="identity")
    plotly::ggplotly(p, tooltip = "Wert") %>% plotly::layout(font=t)

  }else{

    p <- p + ggplot2::geom_bar(position="fill", stat="identity") +
      ggplot2::scale_y_continuous(labels = scales::percent_format())

    plotly::ggplotly(p, tooltip = "tooltip") %>% plotly::layout(font=t)

  }

}


#' A function to return a filtered dataset
#'
#' @description A function to similar to 'kurse_einstieg_bar' but with the
#' difference that it returns a dataframe instead of plot.
#'
#' @return The return value is a dataframe
#' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

data_einstieg_kurse <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_kurse_einstieg

  level_kurs <- r$indikator_kurse_einstieg

  switch_absolut <- r$switch_rel_abs

  geschlecht <- r$geschlecht_kurse_einstieg

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == level_kurs)

  df <- df %>% dplyr::filter(region == "Deutschland")

  # combine subjects to get numbers on share of MINT
  # make a function out of it
  subjects <- c("Mathematik", "Informatik", "Naturwissenschaften",  "Biologie", "Chemie",
                "Physik", "andere naturwiss.-technische Fächer",  "Erdkunde", "Alle Fächer")

  df <- df %>% dplyr::filter(fachbereich %in% subjects)

  df$fachbereich <- ifelse(df$fachbereich != "Alle Fächer", "MINT", "andere Fächer")

  df <- df %>% dplyr::group_by(fachbereich, anzeige_geschlecht, jahr) %>%
    dplyr::summarize(wert = sum(wert))


  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "andere Fächer", "wert"] <- df[df$fachbereich == "andere Fächer", "wert"] -
    df[df$fachbereich == "MINT", "wert"]


  # filter gender
  df <- df %>% dplyr::filter(anzeige_geschlecht %in% geschlecht)

  return(df)

}

#' A function to plot a waffle chart
#'
#' @description A function to create a waffle chart for the second box inside the
#' tab "Schule".
#'
#' @return The return value is a waffle chart
#' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

kurse_waffle <- function(df,r) {

  # load UI inputs from reactive value
  level_kurs <- r$indikator_kurse

  timerange <- r$date_kurse

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(indikator == level_kurs)

  df <- df %>% dplyr::filter(region == "Deutschland")


  # combine subjects to get numbers on share of MINT
  # make a function out of it
  subjects <- c("Mathematik", "Informatik", "Naturwissenschaften",  "Biologie", "Chemie",
                "Physik", "andere naturwiss.-technische Fächer",  "Erdkunde", "Alle Fächer")

  df <- df %>% dplyr::filter(fachbereich %in% subjects)

  df$fachbereich <- ifelse(df$fachbereich != "Alle Fächer", "MINT", "andere Fächer")

  df <- df %>% dplyr::group_by(fachbereich, anzeige_geschlecht, jahr) %>%
    dplyr::summarize(wert = sum(wert))

  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "andere Fächer", "wert"] <- df[df$fachbereich == "andere Fächer", "wert"] -
    df[df$fachbereich == "MINT", "wert"]


  # calculate proportions
  # remove "Männer" + "Frauen" does not add up to "Gesamt"
  # calculate new "Gesamt" based on "Männer" and "Frauen"
  df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(fachbereich) %>%
    dplyr::mutate(props = sum(wert))


  df <- df %>% dplyr::group_by(anzeige_geschlecht, fachbereich) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100


  x_mint <- setNames(round_preserve_sum(as.numeric(df[df$fachbereich == "MINT", "proportion"][[1]]),0),
                     df[df$fachbereich == "MINT", "anzeige_geschlecht"][[1]])

  x_rest <- setNames(round_preserve_sum(as.numeric(df[df$fachbereich == "andere Fächer", "proportion"][[1]]),0),
                     df[df$fachbereich == "andere Fächer", "anzeige_geschlecht"][[1]])

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

  waffle_rest <- waffle::waffle(x_rest, keep = FALSE, colors = colors_mint_vernetzt$gender) +
    ggplot2::labs(
      subtitle = paste0("<span style='font-size:16.0pt;'>" ,x_rest[1],"% <span style='color:#f5adac; font-size:16.0pt;'> Frauen </span> vs. ",
                        "<span style='font-size:16.0pt;'>", x_rest[2],"% <span style='color:#b1b5c3; font-size:16.0pt;'> Männer</span>"),
      title = paste0("<span style='color:#154194;'>", "**Andere Fächer**</span>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(family="serif", size = 14),
                   plot.margin = ggplot2::unit(c(2.5,0,0,0), "lines"))

  if (level_kurs == "Grundkurse"){

    title_help <- "für die Grundkurse"

  }else{

    title_help <- "für die Leistungskurse"

  }

  plot <- ggpubr::ggarrange(waffle_mint, NULL ,waffle_rest, widths = c(1, -0.15, 1), nrow=1, common.legend = T,
                            legend="bottom")
  text <- c(
    paste0("<span style='font-size:20pt; color:black; font-family: serif'> Anteil von Frauen und Männern an MINT und allen
           andere Fächern ", title_help,
           " in ",timerange))
  ggpubr::annotate_figure(plot, gridtext::richtext_grob(text = text))
}


#' A function to create a paired bar plot
#'
#' @description A function to return a paired bar plot for the second box inside
#' the tab "Schule"
#'
#' @return The return value is a bar plot
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

kurse_absolut <- function(df,r) {

  # load UI inputs from reactive value
  level_kurs <- r$indikator_kurse

  timerange <- r$date_kurse

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(indikator == level_kurs)

  df <- df %>% dplyr::filter(region == "Deutschland")


  # combine subjects to get numbers on share of MINT
  # make a function out of it
  subjects <- c("Mathematik", "Informatik", "Naturwissenschaften",  "Biologie", "Chemie",
                "Physik", "andere naturwiss.-technische Fächer",  "Erdkunde", "Alle Fächer")

  df <- df %>% dplyr::filter(fachbereich %in% subjects)

  df$fachbereich <- ifelse(df$fachbereich != "Alle Fächer", "MINT", "andere Fächer")

  df <- df %>% dplyr::group_by(fachbereich, anzeige_geschlecht, jahr) %>%
    dplyr::summarize(wert = sum(wert))

  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "andere Fächer", "wert"] <- df[df$fachbereich == "andere Fächer", "wert"] -
    df[df$fachbereich == "MINT", "wert"]

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt")


  df$fachbereich <- factor(df$fachbereich, levels = c('MINT', 'andere Fächer'))

  # plot
  ggplot2::ggplot(df, ggplot2::aes(x=fachbereich, y=wert, fill = anzeige_geschlecht)) +
    ggplot2::geom_bar(stat="identity", position = "dodge") +
    ggplot2::geom_text(ggplot2::aes(label=wert, vjust = - 0.25),
                       position=ggplot2::position_dodge(width=0.9),
                       fontface = "bold") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(family="serif", size = 14),
      axis.text.x = ggplot2::element_text(colour = c("#b16fab", "#154194"), size = 14),
      plot.title = ggtext::element_markdown(hjust = 0.5)) +
    ggplot2::xlab("Fachrichtung") + ggplot2::ylab("Anzahl") +
    ggplot2::scale_fill_manual(values = colors_mint_vernetzt$gender) +
    ggplot2::labs(title = paste0("<span style='font-size:20pt; color:black; font-family: serif'>",
                                 "Schüler*innen in MINT und allen anderen Fächern für das Jahr ", timerange,
                                 "<br><br><br>"),
                  fill = "")


}


#' A function to create a bar plot
#'
#' @description A function to return a ranking of subject for both genders
#'
#' @return The return value is a bar plot
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

kurse_ranking <- function(df,r, type) {

  # load UI inputs from reactive value
  level_kurs <- r$indikator_kurse

  timerange <- r$date_kurse

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(indikator == level_kurs)

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

  df <- df %>% dplyr::filter(fachbereich != "Alle Fächer")


  # plot
  top_five <- df %>%
    dplyr::arrange(desc(wert)) %>%
    dplyr::group_by(anzeige_geschlecht) %>%
    dplyr::slice(1:5) %>% dplyr::mutate(label_rank = "beliebt")

  low_five <- df %>%
    dplyr::arrange((wert)) %>%
    dplyr::group_by(anzeige_geschlecht) %>%
    dplyr::slice(1:5) %>% dplyr::mutate(label_rank = "weniger beliebt")

  df_ranked <- rbind(top_five, low_five)

  if (level_kurs == "Grundkurse"){

    title_help <- "Grundkurse"

  }else{

    title_help <- "Leistungskurse"

  }

  if(type == "first"){

    plot_1 <- df_ranked %>% dplyr::filter(anzeige_geschlecht == "Frauen") %>%
    ggplot2::ggplot(ggplot2::aes(x=reorder(fachbereich, wert), y=wert, fill = label_rank)) +
      ggplot2::geom_bar(stat="identity") +
      ggplot2::coord_flip() +
      ggplot2::facet_grid(label_rank ~ ., scales = "free", space = "free", switch = "both") +
      ggplot2::theme_bw() +
      ggplot2::scale_fill_manual(values = c("#00a87a", "#ee7775")) +
      ggplot2::theme(legend.position = "none",
                     strip.placement = "outside",
                     plot.title = ggplot2::element_text(family = 'serif'),
                     axis.text.y = ggplot2::element_text(family = 'serif'),
                     plot.margin = ggplot2::unit(c(2.5,0,0,0), "lines"),
                     strip.text.y = ggplot2::element_text(size = 14, face = "bold", family = 'serif'),
                     panel.grid.major.y = ggplot2::element_blank(),
                     panel.background = ggplot2::element_rect(fill="white"),
                     strip.background = ggplot2::element_blank()) +
     ggplot2::labs(title = "Frauen", x = "", y = "")

    plot_2 <- df_ranked %>% dplyr::filter(anzeige_geschlecht == "Männer") %>%
      ggplot2::ggplot(ggplot2::aes(x=reorder(fachbereich, wert), y=wert, fill = label_rank)) +
      ggplot2::geom_bar(stat="identity") +
      ggplot2::coord_flip() +
      ggplot2::facet_grid(label_rank ~ ., scales = "free", space = "free", switch = "both") +
      ggplot2::theme_bw() +
      ggplot2::scale_fill_manual(values = c("#00a87a", "#ee7775")) +
      ggplot2::theme(legend.position = "none",
                     strip.placement = "outside",
                     plot.title = ggplot2::element_text(family = 'serif'),
                     axis.text.y = ggplot2::element_text(family = 'serif'),
                     plot.margin = ggplot2::unit(c(2.5,0,0,0), "lines"),
                     strip.text.y = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_blank(),
                     panel.background = ggplot2::element_rect(fill="white"),
                     strip.background = ggplot2::element_blank()) +
      ggplot2::labs(title = "Männer", x = "", y = "")


    text <- c(
      paste0("<span style='font-size:20pt; color:black; font-family: serif'> Die 5 häufigsten und wenigsten belegten ", title_help,
             " für das Jahr ",timerange))


    plot <- ggpubr::ggarrange(plot_1, NULL, plot_2, widths = c(1, 0.2, 1), nrow=1)

    ggpubr::annotate_figure(plot, gridtext::richtext_grob(text = text))

  }else{

    df %>%
      ggplot2::ggplot(ggplot2::aes(x= wert, y= reorder(fachbereich,wert))) +
      ggplot2::geom_line(ggplot2::aes(group = fachbereich),color="black")+
      ggplot2::geom_point(ggplot2::aes(color=anzeige_geschlecht), size=6, alpha = 0.3) +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position="bottom",
                     legend.text= ggplot2::element_text(family = 'serif'),
                     panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
                     plot.title = ggtext::element_markdown(hjust = 0.5),
                     axis.text.y = ggplot2::element_text(size = 11, family = 'serif')) +
      ggplot2::scale_color_manual(values = colors_mint_vernetzt$gender) +
      ggplot2::ylab("") + ggplot2::xlab("") +
      ggplot2::labs(title = paste0("<span style='font-size:20pt; color:black; font-family: serif'>",
                                   "Ranking aller ", title_help, " für das Jahr ",timerange,
                                   "<br><br><br>"),
                    color = "")

  }
}


#' A function to plot the german map
#'
#' @description A function to plot the german map with all states that contain
#' information about the share of women in STEM
#'
#' @return The return value is the german map with information
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

kurse_map <- function(df,r) {

  # load UI inputs from reactive value
  level_kurs <- r$indikator_kurse

  timerange <- r$date_kurse

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(indikator == level_kurs)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(region != "Bayern")

  # combine subjects to get numbers on share of MINT
  # make a function out of it
  subjects <- c("Mathematik", "Informatik", "Naturwissenschaften",  "Biologie", "Chemie",
                "Physik", "andere naturwiss.-technische Fächer",  "Erdkunde", "Alle Fächer")

  df <- df %>% dplyr::filter(fachbereich %in% subjects)

  df$fachbereich <- ifelse(df$fachbereich != "Alle Fächer", "MINT", "andere Fächer")

  df <- df %>% dplyr::group_by(region, fachbereich, anzeige_geschlecht) %>%
    dplyr::summarize(wert = sum(wert, na.rm = T))


  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "andere Fächer", "wert"] <- df[df$fachbereich == "andere Fächer", "wert"] -
    df[df$fachbereich == "MINT", "wert"]


  # calculate proportions
  # remove "Männer" + "Frauen" does not add up to "Gesamt"
  # calculate new "Gesamt" based on "Männer" and "Frauen"
  df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich) %>%
    dplyr::mutate(props = sum(wert))

  df <- df %>% dplyr::filter(fachbereich != "andere Fächer")

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")

  df <- df %>% dplyr::group_by(region) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100

  if (level_kurs == "Grundkurse"){

    title_help <- "Grundkurse"

  }else{

    title_help <- "Leistungskurse"

  }

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
      text = paste0("Anteil der Frauen an MINT-", title_help ," für das Jahr ", timerange),
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


#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

kurse_verlauf <- function(df,r) {

  # load UI inputs from reactive value
  level_kurs <- r$indikator_kurse_verlauf

  timerange <- r$date_kurse_verlauf

  states <- r$states_kurse_verlauf

  topic <- r$topic_kurse_verlauf

  ost_west <- r$ost_west

  subject_aggregated <- r$subjects_aggregated

  subjects_select <- r$subject_selected

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == level_kurs)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(region != "Bayern")

  if (subject_aggregated == "aggregiert"){

  # combine subjects to get numbers on share of MINT
  # make a function out of it
  subjects <- c("Mathematik", "Informatik", "Naturwissenschaften",  "Biologie", "Chemie",
                "Physik", "andere naturwiss.-technische Fächer",  "Erdkunde", "Alle Fächer")

  df <- df %>% dplyr::filter(fachbereich %in% subjects)

  df$fachbereich <- ifelse(df$fachbereich != "Alle Fächer", "MINT", "andere Fächer")

  df <- df %>% dplyr::group_by(region, fachbereich, anzeige_geschlecht, jahr) %>%
    dplyr::summarize(wert = sum(wert, na.rm = T))


  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "andere Fächer", "wert"] <- df[df$fachbereich == "andere Fächer", "wert"] -
    df[df$fachbereich == "MINT", "wert"]

  # filter MINT or remaining subjects
  df <- df %>% dplyr::filter(fachbereich == topic)

  if (topic == "MINT"){

    title_help <- "MINT"

  }else {

    title_help <- "anderen Fächer"

  }

  }else {

    df <- df %>% dplyr::filter(fachbereich %in% subjects_select)

    title_help <- subjects_select
  }

  # calculate proportions
  # remove "Männer" + "Frauen" does not add up to "Gesamt"
  # calculate new "Gesamt" based on "Männer" and "Frauen"
  df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, jahr) %>%
    dplyr::mutate(props = sum(wert))

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")

  df <- df %>% dplyr::group_by(region, fachbereich, jahr) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100

  # order
  df <- df[with(df, order(region, fachbereich, jahr, decreasing = TRUE)), ]


  if (ost_west == "Nein") {

    df <- df %>% dplyr::filter(region %in% states)

  } else{

    df$dummy_west <- ifelse(df$region %in% states_east_west$west, "Westen", "Osten")

    df <- df %>% dplyr::group_by(jahr, fachbereich, dummy_west) %>%
      dplyr::summarise(proportion = mean(proportion))

    names(df)[3] <- "region"
  }

  # order years for plot
  df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = region)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil Schülerinnen <br> Bundesland: {point.region} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "serif")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "serif")) %>%
    highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil von Schülerinnen an ", title_help ," im Verlauf"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "serif", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "serif", fontSize = "14px")
    )


}
