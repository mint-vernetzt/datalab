################################################################################
################################# Arbeitsmarkt #################################
################################################################################



#' A function to plot a graph.
#'
#' @description A function to create a stacked bar chart for the first box
#' inside the tab "Beruf".
#'
#' @return The return value is a plot
#' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_einstieg_bar <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt_einstieg

  status_arbeitnehmer <- r$indikator_arbeitsmarkt_einstieg

  geschlecht <- r$geschlecht_arbeitsmarkt_einstieg

  switch_absolut <- r$switch_rel_abs

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == status_arbeitnehmer)

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- df %>% dplyr::filter(anforderungsniveau == "Gesamt")


  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
    df[df$fachbereich == "MINT", "wert"]

  df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"

  # calculate the share of males
  help_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt") %>%
    dplyr::group_by(jahr, fachbereich)

  help_weiblich <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen") %>%
    dplyr::group_by(jahr, fachbereich)

  wert_männlich <- help_gesamt$wert - help_weiblich$wert

  help_männlich <- help_weiblich

  help_männlich$wert <- wert_männlich

  help_männlich$anzeige_geschlecht <- "Männer"

  df <- rbind(df, help_männlich)

  df <- df[with(df, order(anzeige_geschlecht, jahr, decreasing = TRUE)), ]

  # filter gender
  df <- df %>% dplyr::filter(anzeige_geschlecht %in% geschlecht)

  values <- df %>% dplyr::group_by(anzeige_geschlecht, jahr) %>%
    dplyr::summarize(wert = sum(wert))

  values <- values[with(values, order(anzeige_geschlecht, jahr, decreasing = TRUE)), ]

  df$Anteil <- NA

  df[df$fachbereich == "andere Berufszweige", "Anteil"] <- round((df[df$fachbereich == "andere Berufszweige", "wert"]/values$wert)*100)

  df[df$fachbereich == "MINT", "Anteil"] <- round((df[df$fachbereich == "MINT", "wert"]/values$wert)*100)

  df$Anteil <- paste(df$Anteil,"%")

  # remove scientific notation
  options(scipen=999)

  if (status_arbeitnehmer == "Auszubildende"){

    title_help <- "für die Auszubildenden"

  }else{

    title_help <- "für die Beschäftigten"

  }
  names(df)[4] <- "Wert"

  p <- ggplot2::ggplot(df, ggplot2::aes(fill=fachbereich, y=Wert, x=anzeige_geschlecht, tooltip = Anteil)) +
    ggplot2::labs(caption = "Quelle:", title = paste0("Anteile an MINT und allen anderen Berufszweigen", title_help),
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
    plotly::ggplotly(p, tooltip = "Wert") %>% plotly::layout(font=t) %>%
      plotly::config(displayModeBar = FALSE,
                     displaylogo = FALSE)

  }else{

    p <- p + ggplot2::geom_bar(position="fill", stat="identity") +
      ggplot2::scale_y_continuous(labels = scales::percent_format())

    plotly::ggplotly(p, tooltip = "tooltip") %>% plotly::layout(font=t) %>%
      plotly::config(displayModeBar = FALSE,
                     displaylogo = FALSE)

  }
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

  status_arbeitnehmer <- r$indikator_arbeitsmarkt_einstieg

  geschlecht <- r$geschlecht_arbeitsmarkt_einstieg

  switch_absolut <- r$switch_rel_abs

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == status_arbeitnehmer)

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- df %>% dplyr::filter(anforderungsniveau == "Gesamt")

  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
    df[df$fachbereich == "MINT", "wert"]

  df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"



  # calculate the share of males
  help_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt") %>%
    dplyr::group_by(jahr, fachbereich)

  help_weiblich <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen") %>%
    dplyr::group_by(jahr, fachbereich)

  wert_männlich <- help_gesamt$wert - help_weiblich$wert

  help_männlich <- help_weiblich

  help_männlich$wert <- wert_männlich

  help_männlich$anzeige_geschlecht <- "Männer"

  df <- rbind(df, help_männlich)

  df <- df[with(df, order(fachbereich, jahr, decreasing = TRUE)), ]

  # filter gender
  df <- df %>% dplyr::filter(anzeige_geschlecht %in% geschlecht)

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
  status_arbeitnehmer <- r$indikator_arbeitsmarkt

  timerange <- r$date_arbeitsmarkt

  anforderung <- r$anforderungsniveau_arbeitsmarkt


  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(indikator == status_arbeitnehmer)

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- df %>% dplyr::filter(anforderungsniveau == anforderung)

  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
    df[df$fachbereich == "MINT", "wert"]

  df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"

  # calculate the share of males
  values <- df %>%
    dplyr::group_by(fachbereich) %>%
    dplyr::mutate(wert = wert - dplyr::lead(wert)) %>% dplyr::select(wert) %>% na.omit()

  df[df$anzeige_geschlecht == "Gesamt", "wert"] <- values$wert

  df[df$anzeige_geschlecht == "Gesamt", "anzeige_geschlecht"] <- "Männer"


  # calculate proportions
  df <- df %>% dplyr::group_by(fachbereich) %>%
    dplyr::mutate(props = sum(wert))


  df <- df %>% dplyr::group_by(anzeige_geschlecht, fachbereich) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100


  x_mint <- setNames(round_preserve_sum(as.numeric(df[df$fachbereich == "MINT", "proportion"][[1]]),0),
                     df[df$fachbereich == "MINT", "anzeige_geschlecht"][[1]])

  x_rest <- setNames(round_preserve_sum(as.numeric(df[df$fachbereich == "andere Berufszweige", "proportion"][[1]]),0),
                     df[df$fachbereich == "andere Berufszweige", "anzeige_geschlecht"][[1]])

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
      title = paste0("<span style='color:#154194;'>", "**Andere Berufszweige**</span>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(family="serif", size = 14),
                   plot.margin = ggplot2::unit(c(2.5,0,0,0), "lines"))

  if (status_arbeitnehmer == "Auszubildende"){

    title_help <- "in Ausbildung"

  }else{

    title_help <- "in Beschäftigung"

  }

  plot <- ggpubr::ggarrange(waffle_mint, NULL ,waffle_rest, widths = c(1, -0.15, 1), nrow=1, common.legend = T,
                            legend="bottom")
  text <- c(
    paste0("<span style='font-size:20pt; color:black; font-family: serif'> Anteile von Frauen und Männer ", title_help," an MINT und allen anderen Berufszweigen <br> für das Jahr ", timerange))

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
  status_arbeitnehmer <- r$indikator_arbeitsmarkt

  timerange <- r$date_arbeitsmarkt

  anforderung <- r$anforderungsniveau_arbeitsmarkt


  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(indikator == status_arbeitnehmer)

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- df %>% dplyr::filter(anforderungsniveau == anforderung)

  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
    df[df$fachbereich == "MINT", "wert"]

  df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"

  # calculate the share of males
  values <- df %>%
    dplyr::group_by(fachbereich) %>%
    dplyr::mutate(wert = wert - dplyr::lead(wert)) %>% dplyr::select(wert) %>% na.omit()

  df[df$anzeige_geschlecht == "Gesamt", "wert"] <- values$wert

  df[df$anzeige_geschlecht == "Gesamt", "anzeige_geschlecht"] <- "Männer"

  # plot
  ggplot2::ggplot(df, ggplot2::aes(x=reorder(fachbereich, wert), y=wert, fill = anzeige_geschlecht)) +
    ggplot2::geom_bar(stat="identity", position = "dodge") +
    ggplot2::geom_text(ggplot2::aes(label=wert, vjust = - 0.25),
                       position=ggplot2::position_dodge(width=0.9),
                       fontface = "bold") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(family="serif", size = 14),
      axis.text.x = ggplot2::element_text(colour = c("#b16fab", "#154194"), size = 14),
      plot.title = ggtext::element_markdown(hjust = 0.5)) +
    ggplot2::ylab("Anzahl") + ggplot2::xlab("Fachrichtung") +
    ggplot2::scale_fill_manual(values = colors_mint_vernetzt$gender) +
    ggplot2::labs(title = paste0("<span style='font-size:20pt; color:black; font-family: serif'>",
                               "Arbeitnehmer*innen in MINT und allen anderen Fächern für das Jahr ", timerange,
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

arbeitsmarkt_map <- function(df,r) {

  # load UI inputs from reactive value
  status_arbeitnehmer <- r$indikator_arbeitsmarkt

  timerange <- r$date_arbeitsmarkt

  anforderung <- r$anforderungsniveau_arbeitsmarkt

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(indikator == status_arbeitnehmer)

  df <- df %>% dplyr::filter(anforderungsniveau == anforderung)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")


  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
    df[df$fachbereich == "MINT", "wert"]

  df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"


  df <- df %>% dplyr::filter(fachbereich != "andere Berufszweige")

  values <- (df[df$anzeige_geschlecht == "Frauen", "wert"]/df[df$anzeige_geschlecht == "Gesamt", "wert"])*100

  values$region <- df[df$anzeige_geschlecht == "Frauen", "region"][[1]]

   if (status_arbeitnehmer == "Auszubildende"){

    title_help <- "in Ausbildung"

  }else{

    title_help <- "in Beschäftigung"

  }

  # plot
  highcharter::hcmap(
    "countries/de/de-all",
    data = values,
    value = "wert",
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
      text = paste0("Anteil der Frauen ", title_help ," an MINT-Berufen für das Jahr ", timerange),
      margin = 45,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "serif")
    ) %>%
    highcharter::hc_caption(
      text = "Quelle:", style = list(fontSize = "12px")
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

  ost_west <- r$ost_west

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

  # order
  df <- df[with(df, order(region, fachbereich, jahr, decreasing = TRUE)), ]

  # calculate proportion
  values <- df %>%
    dplyr::group_by(jahr, fachbereich, region) %>%
    dplyr::mutate(wert = dplyr::lead(wert)/wert) %>% dplyr::select(wert) %>% na.omit()


  values <- values %>% dplyr::filter(region %in% states)


  if (ost_west == "Nein") {

    values <- values %>% dplyr::filter(region %in% states)

  } else{

    values$dummy_west <- ifelse(values$region %in% states_east_west$west, "Westen", "Osten")

    values <- values %>% dplyr::group_by(jahr, fachbereich, dummy_west) %>%
      dplyr::summarise(wert = mean(wert))

    names(values)[3] <- "region"
  }

  # filter MINT or remaining subjects
  values <- values %>% dplyr::filter(fachbereich == topic)

  # order years for plot
  values <- values[with(values, order(region, jahr, decreasing = FALSE)), ]

  values$wert <- values$wert * 100

  if (topic == "MINT"){

    title_help <- "MINT"

  }else {

    title_help <- "allen anderen Berufszweigen"

  }

  # plot
  highcharter::hchart(values, 'line', highcharter::hcaes(x = jahr, y = round(wert), group = region)) %>%
    highcharter::hc_tooltip(pointFormat = "Bundesland: {point.region} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), style = list(fontFamily = "serif")) %>%
    highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil von Frauen an ", title_help ," im Verlauf"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "serif", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "serif", fontSize = "14px")
    )



}


