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

  df$Anteil <- paste0(df$Anteil,"%")

  # remove scientific notation
  options(scipen=999)

  if (status_arbeitnehmer == "Auszubildende"){

    title_help <- "für die Auszubildenden"

  }else{

    title_help <- "für die Beschäftigten"

  }
  df <- df[with(df, order(anzeige_geschlecht, jahr, decreasing = FALSE)), ]


  if(isTRUE(switch_absolut)){


    highchart_obj(df, geschlecht, type = "normal", andere_name = "andere Berufszweige", lehramt = "Nein") %>%
      highcharter::hc_title(text = paste0("Absoluter Anteil an MINT und allen anderen Berufszweigen ", title_help),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px"))

  }else{

    p <- highchart_obj(df, geschlecht, type = "percent", andere_name = "andere Berufszweige", lehramt = "Nein")

    p %>% highcharter::hc_yAxis(labels = list(format = "{value}%")) %>%
    highcharter::hc_tooltip(pointFormat = "{series.name} <br> Anteil: {point.percentage:.0f}%") %>%
      highcharter::hc_title(text = paste0("Relativer Anteil an MINT und allen anderen Berufszweigen ", title_help),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px"))


  }
}

#' A function to plot a graph.
#'
#' @description A function to create a stacked bar chart for the first box
#' inside the tab "Schule".
#'
#' @return The return value is a plot
#' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_einstieg_pie <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt_einstieg

  geschlecht <- r$gender_switch

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)


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


  if(geschlecht == FALSE) {

    df_beschaeftigte <- df %>% dplyr::filter(indikator == "Beschäftigte")

    df_beschaeftigte <- df_beschaeftigte %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

    df_beschaeftigte <- share_pie(df_beschaeftigte)

    df_beschaeftigte$anzeige_geschlecht <- df_beschaeftigte$fachbereich

    df_auszubildende <- df %>% dplyr::filter(indikator == "Auszubildende")

    df_auszubildende <- df_auszubildende %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

    df_auszubildende <- share_pie(df_auszubildende)

    df_auszubildende$anzeige_geschlecht <- df_auszubildende$fachbereich


  } else {


    df_beschaeftigte <- df %>% dplyr::filter(indikator == "Beschäftigte")

    df_beschaeftigte <- df_beschaeftigte %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

    df_beschaeftigte <- share_pie(df_beschaeftigte)

    df_beschaeftigte$anzeige_geschlecht <- paste0(df_beschaeftigte$anzeige_geschlecht, " (", df_beschaeftigte$fachbereich, ")")

    df_auszubildende <- df %>% dplyr::filter(indikator == "Auszubildende")

    df_auszubildende <- df_auszubildende %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

    df_auszubildende <- share_pie(df_auszubildende)

    df_auszubildende$anzeige_geschlecht <- paste0(df_auszubildende$anzeige_geschlecht, " (", df_auszubildende$fachbereich, ")")


  }

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


  if(geschlecht == FALSE) {

    plot_beschaeftigte <- plot_beschaeftigte %>% highcharter::hc_colors(c("#154194","#b16fab"))

    plot_auszubildende <- plot_auszubildende %>% highcharter::hc_colors(c("#154194","#b16fab"))

  } else {

    plot_beschaeftigte <- plot_beschaeftigte %>% highcharter::hc_colors(c("#154194", 'rgba(21, 65, 148, 0.50)',"#b16fab",
                                                    'rgba(177, 111, 171, 0.50)'))

    plot_auszubildende <- plot_auszubildende %>% highcharter::hc_colors(c("#154194", 'rgba(21, 65, 148, 0.50)',"#b16fab",
                                                    'rgba(177, 111, 171, 0.50)'))

  }


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
  help_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt") %>%
    dplyr::group_by(jahr, fachbereich)

  help_weiblich <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen") %>%
    dplyr::group_by(jahr, fachbereich)

  wert_männlich <- help_gesamt$wert - help_weiblich$wert

  help_männlich <- help_weiblich

  help_männlich$wert <- wert_männlich

  help_männlich$anzeige_geschlecht <- "Männer"

  df <- rbind(df, help_männlich)

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

  x_beschaeftigte <- setNames(round_preserve_sum(as.numeric(df_beschaeftigte$proportion),0),
                              df_beschaeftigte$anzeige_geschlecht)


  x_beschaeftigte <- x_beschaeftigte[order(factor(names(x_beschaeftigte), levels = c('Frauen (MINT)', 'Männer (MINT)')))]


  df_auszubildende <- df %>% dplyr::filter(indikator == "Auszubildende")


  df_auszubildende$anzeige_geschlecht <- paste0(df_auszubildende$anzeige_geschlecht, " (", df_auszubildende$fachbereich, ")")

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

arbeitsmarkt_map <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt

  anforderung <- r$anforderungsniveau_arbeitsmarkt

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(anforderungsniveau == anforderung)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(fachbereich != "Alle")

  values_beschaeftigte <- (df[(df$anzeige_geschlecht == "Frauen" & df$indikator == "Beschäftigte"),
                              "wert"]/df[(df$anzeige_geschlecht == "Gesamt"  & df$indikator == "Beschäftigte"), "wert"])*100

  values_auszubildende <- (df[(df$anzeige_geschlecht == "Frauen" &  df$indikator == "Auszubildende"),
                              "wert"]/df[(df$anzeige_geschlecht == "Gesamt"  & df$indikator == "Auszubildende"), "wert"])*100

  values_beschaeftigte$region <- df[(df$anzeige_geschlecht == "Frauen" & df$indikator == "Beschäftigte"), "region"][[1]]

  values_auszubildende$region <- df[(df$anzeige_geschlecht == "Frauen"  &  df$indikator == "Auszubildende"), "region"][[1]]

  if (anforderung == "Gesamt"){

    title_help_sub <- " insgesamt"

  } else {

    title_help_sub <- paste0(" (",anforderung,")")

  }


  highcharter::hw_grid(
    # plot
    highcharter::hcmap(
      "countries/de/de-all",
      data = values_auszubildende,
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
        text = paste0("Anteil von Frauen in Ausbildung <br>", title_help_sub," an MINT-Berufen in ", timerange),
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
      data = values_beschaeftigte,
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
        text = paste0("Anteil von Frauen in Beschäftigung <br>", title_help_sub," an MINT-Berufen in ", timerange),
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

  df <- prep_arbeitsmarkt_east_west(df)

  # order
  df <- df[with(df, order(region, fachbereich, jahr, decreasing = TRUE)), ]


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
    highcharter::hc_exporting(enabled=TRUE,
                              filename = "plot-Fächer",
                              #allowHTML = TRUE,
                              #formAttributes = list(target = "_blank"),
                              buttons = list(contextButton = list(menuItems = c("downloadPNG", "downloadJPEG",
                                                                                "downloadPDF"),
                                                                  text= '', symbolFill = '#000000'
                              )))

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

  df <- prep_arbeitsmarkt_east_west(df)

    # calculate the share of males
  help_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt") %>%
    dplyr::group_by(jahr, fachbereich, region, indikator)

  help_weiblich <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen") %>%
    dplyr::group_by(jahr, fachbereich, region, indikator)

  wert_männlich <- help_gesamt$wert - help_weiblich$wert

  help_männlich <- help_weiblich

  help_männlich$wert <- wert_männlich

  help_männlich$anzeige_geschlecht <- "Männer"

  df <- rbind(df, help_männlich)

  df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])



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
    highcharter::hc_exporting(enabled=TRUE,
                              filename = "plot-Fächer",
                              #allowHTML = TRUE,
                              #formAttributes = list(target = "_blank"),
                              buttons = list(contextButton = list(menuItems = c("downloadPNG", "downloadJPEG",
                                                                                "downloadPDF"),
                                                                  text= '', symbolFill = '#000000'
                              )))

}
