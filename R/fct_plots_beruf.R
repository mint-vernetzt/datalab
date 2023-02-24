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

  df <- df %>% dplyr::filter(anforderung == "Gesamt")

  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
    df[df$fachbereich == "MINT", "wert"]

  df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"

  # calculate the share of males
  df <- calc_arbeitsmarkt_males(df)

  df_beschaeftigte <- df %>% dplyr::filter(indikator == "Beschäftigte")

  df_beschaeftigte <- df_beschaeftigte %>% dplyr::filter(geschlecht == "Gesamt")

  # calculate proportions
  df_beschaeftigte <- share_pie_neu(df_beschaeftigte)

  df_beschaeftigte$geschlecht <- df_beschaeftigte$fachbereich

  df_auszubildende <- df %>% dplyr::filter(indikator == "Auszubildende")

  df_auszubildende <- df_auszubildende %>% dplyr::filter(geschlecht == "Gesamt")

  # calculate proportions
  df_auszubildende <- share_pie_neu(df_auszubildende)

  df_auszubildende$geschlecht <- df_auszubildende$fachbereich

  plot_auszubildende <- highcharter::hchart(df_auszubildende, size = 280, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
    highcharter::hc_tooltip(
      pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%

    highcharter::hc_colors(c("#efe8e6","#b16fab")) %>%
    highcharter::hc_title(text = paste0("Berufswahl (Auszubildende)", br(), timerange),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
    highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                           dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE))



  plot_beschaeftigte <- highcharter::hchart(df_beschaeftigte, size = 280, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
    highcharter::hc_tooltip(
      pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
    highcharter::hc_colors(c("#efe8e6","#b16fab")) %>%
    highcharter::hc_title(text = paste0("Berufswahl (Beschäftigte)", br(), timerange),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
    highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                           dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE))



    plot_beschaeftigte <- plot_beschaeftigte %>% highcharter::hc_colors(c("#efe8e6","#b16fab"))

    plot_auszubildende <- plot_auszubildende %>% highcharter::hc_colors(c("#efe8e6","#b16fab"))


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

  df <- df %>% dplyr::filter(anforderung == "Gesamt")

  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
    df[df$fachbereich == "MINT", "wert"]

  df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"

  # calculate the share of males
  df <-   df <- calc_arbeitsmarkt_males(df)

  colnames(df) <- c("Region", "Fachbereich", "anforderung", "Wert", "Indikator", "Jahr", "Geschlecht", "Bereich")

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

  anforderung <- r$anforderung_arbeitsmarkt

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- df %>% dplyr::filter(anforderung == anforderung)

  df <- df %>% dplyr::filter(fachbereich == "MINT")

  # calculate the share of males
  values <- df %>%
    dplyr::group_by(indikator) %>%
    dplyr::mutate(wert = wert - dplyr::lead(wert)) %>% dplyr::select(wert) %>% na.omit()

  df[df$geschlecht == "Gesamt", "wert"] <- values$wert

  df[df$geschlecht == "Gesamt", "geschlecht"] <- "Männer"


  # calculate proportions
  df <- df %>% dplyr::group_by(indikator) %>%
    dplyr::mutate(props = sum(wert))


  df <- df %>% dplyr::group_by(geschlecht, indikator, fachbereich) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100

  df_beschaeftigte <- df %>% dplyr::filter(indikator == "Beschäftigte")


  df_beschaeftigte$geschlecht <- paste0(df_beschaeftigte$geschlecht, " (", df_beschaeftigte$fachbereich, ")")

  # ensure proportions sum to 1
  x_beschaeftigte <- setNames(round_preserve_sum(as.numeric(df_beschaeftigte$proportion),0),
                              df_beschaeftigte$geschlecht)

  x_beschaeftigte <- x_beschaeftigte[order(factor(names(x_beschaeftigte), levels = c('Frauen (MINT)', 'Männer (MINT)')))]

  df_auszubildende <- df %>% dplyr::filter(indikator == "Auszubildende")

  df_auszubildende$geschlecht <- paste0(df_auszubildende$geschlecht, " (", df_auszubildende$fachbereich, ")")

  # ensure proportions sum to 1
  x_auszubildende <- setNames(round_preserve_sum(as.numeric(df_auszubildende$proportion),0),
                              df_auszubildende$geschlecht)

  x_auszubildende <- x_auszubildende[order(factor(names(x_auszubildende), levels = c('Frauen (MINT)', 'Männer (MINT)')))]


  # create plot objects for waffle charts
  waffle_aus <- waffle::waffle(x_auszubildende, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", "Berufswahl (Auszubildende)</span>", br(), timerange)) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "bottom") +
    ggplot2::scale_fill_manual(
      values =  c("#b16fab",
                  "#b1b5c3"),
      na.value="#b1b5c3",
      limits = c("Frauen (MINT)", "Männer (MINT)"),
      guide = ggplot2::guide_legend(reverse = TRUE),
      labels = c(
        paste0("Frauen (MINT)",", ",x_auszubildende[1], "%"),
        paste0("Männer (MINT)",", ",x_auszubildende[2], "%"))) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))



  waffle_be <- waffle::waffle(x_beschaeftigte, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", "Berufswahl (Beschäftigte)</span>", br(), timerange)) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "bottom") +
      ggplot2::scale_fill_manual(
        values =  c("#b16fab",
                    "#b1b5c3"),
        na.value="#b1b5c3",
        limits = c("Frauen (MINT)", "Männer (MINT)"),
        guide = ggplot2::guide_legend(reverse = TRUE),
        labels = c(
          paste0("Frauen (MINT)",", ",x_beschaeftigte[1], "%"),
          paste0("Männer (MINT)",", ",x_beschaeftigte[2], "%"))) +
      ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))




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

  anforderung <- r$anforderung_arbeitsmarkt


  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- df %>% dplyr::filter(anforderung == anforderung)

  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
    df[df$fachbereich == "MINT", "wert"]

  df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"

  # calculate the share of males
  values <- df %>%
    dplyr::group_by(fachbereich, indikator) %>%
    dplyr::mutate(wert = wert - dplyr::lead(wert)) %>% dplyr::select(wert) %>% na.omit()

  df[df$geschlecht == "Gesamt", "wert"] <- values$wert

  df[df$geschlecht == "Gesamt", "geschlecht"] <- "Männer"


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
    ggplot2::theme_minimal() +
    ggplot2::facet_grid(~ geschlecht) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      text = ggplot2::element_text(size = 14),
      plot.title = ggtext::element_markdown(hjust = 0.5)) +
    ggplot2::xlab("") + ggplot2::ylab("Anzahl") +
    ggplot2::scale_fill_manual(values = c("#efe8e6","#b16fab")) +
    ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
                                 "Arbeitnehmer*innen ", title_help_sub," in MINT und allen anderen Berufszweigen in ", timerange,
                                 "<br><br><br>"),
                  fill = "")


}


#' A function to plot the german map ::::box 6
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
  #timerange <- r$date_arbeitsmarkt_bl_gender

  #anforderung <- r$anforderung_arbeitsmarkt_bl_gender

  indikator_choice <- r$level_arbeitsmarkt_bl_gender

  fachbereich_choice <- r$fach_arbeitsmarkt_bl_gender

  # filter dataset based on UI inputs
 # df <- df %>% dplyr::filter(jahr == timerange)


  # remove - Deutschland nicht enthalten in DF
  #df <- df %>% dplyr::filter(region != "Deutschland")

  # im neuen DF doch Aggregate enthalten, ausfiltern das Folgecode weiter stimmt
  df <- df %>% dplyr::filter(landkreis != "alle Landkreise")


  # filtern nach Anforderungsniveau
  df <- df %>% dplyr::filter(anforderung == "Gesamt")

  # # filtern nach Geschlecht (liegt nicht in allen Indikatoren vor)
  # df <- df %>% dplyr::filter(indikator %in% c("Auszubildende",
  #                                             "Auszubildende (1. Jahr)",
  #                                             "Beschäftigte",
  #                                             "ausländische Beschäftigte"))

  # Aggregat auf Bundeslandebene berechnen und LKs ausschließen
  df <- df %>%
    dplyr::group_by(jahr, indikator, fachbereich, geschlecht, bundesland) %>%
    dplyr::summarize(wert = sum(wert))

  # Berechnung von andere Fächergruppen
  df_andere <- df %>% dplyr::filter(fachbereich=="Alle")
  df_mint <- df %>% dplyr::filter(fachbereich=="MINT")
  df_andere$wert <- df_andere$wert - df_mint$wert
  df_andere$fachbereich[df_andere$fachbereich == "Alle"]<-"Andere Berufsgruppen"

  df <- rbind(df, df_andere)

  #nicht nötig, da Männer schon in df berechnet
  #df <- calc_arbeitsmarkt_males(df)

  df <- df %>% dplyr::filter(indikator == indikator_choice)

  df_gesamt <- df %>%
    dplyr::filter(fachbereich == "Alle")
                  # ,
                  # anforderung == "Gesamt")

  df <- df %>%
    dplyr::left_join(df_gesamt, by = c("bundesland", "jahr", "geschlecht", "indikator")) %>%
    dplyr::rename(fachbereich = fachbereich.x,
                  wert = "wert.x",
                  wert_sum = "wert.y") %>%
    dplyr::select(-fachbereich.y) %>%
    dplyr::mutate(proportion = (wert/wert_sum)*100)%>%
    dplyr::filter(fachbereich == fachbereich_choice)


  values_female <- df %>% dplyr::filter(geschlecht == "Frauen")
  values_male <- df %>% dplyr::filter(geschlecht == "Männer")

  # if (anforderung == "Gesamt"){
  #
  #   title_help_sub <- " insgesamt"
  #
  # } else {
  #
  #   title_help_sub <- paste0(" mit anforderung ", anforderung)
  #
  # }

  highcharter::hw_grid(
    # plot
    highcharter::hcmap(
      "countries/de/de-all",
      data = values_female,
      value = "proportion",
      joinBy = c("name", "bundesland"),
      borderColor = "#FAFAFA",
      name = paste0("Anteil von MINT-Berufen"),
      borderWidth = 0.1,
      nullColor = "#A9A9A9",
      tooltip = list(
        valueDecimals = 0,
        valueSuffix = "%"
      )
    ) %>%
      highcharter::hc_colorAxis(min=0,labels = list(format = "{text}%")) %>%
      highcharter::hc_title(
        text = paste0("Frauen: Wahl von MINT-Berufen (", indikator_choice, ")", br(), "2021"
                      #, title_help_sub
                      ),
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
      joinBy = c("name", "bundesland"),
      borderColor = "#FAFAFA",
      name = "Anteil von MINT-Berufen",
      borderWidth = 0.1,
      nullColor = "#A9A9A9",
      tooltip = list(
        valueDecimals = 0,
        valueSuffix = "%"
      )
    ) %>%
      highcharter::hc_colorAxis(min=0,labels = list(format = "{text}%")) %>%
      highcharter::hc_title(
        text = paste0("Männer: Wahl von MINT-Berufen (", indikator_choice, ")", br(), "2021"
                      #, title_help_sub
                      ),
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

  anforderung <- r$anforderung_arbeitsmarkt

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(anforderung == anforderung)

  df <- df %>% dplyr::filter(fachbereich != "Alle")

  colnames(df) <- c("Region", "Fachbereich", "anforderung", "Wert", "Indikator", "Jahr", "Geschlecht", "Bereich")

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

  anforderung <- r$anforderung_arbeitsmarkt_verlauf

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == status_arbeitnehmer)

  df <- df %>% dplyr::filter(anforderung == anforderung)

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
    dplyr::mutate(props = wert[geschlecht == "Frauen"]/
                    wert[geschlecht == "Gesamt"])

  df <- df %>% dplyr::filter(geschlecht != "Gesamt")


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
    #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.", style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil von Frauen an ", title_help ," im Verlauf"),
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

  anforderung <- r$anforderung_arbeitsmarkt_verlauf

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == status_arbeitnehmer)

  df <- df %>% dplyr::filter(anforderung == anforderung)

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
    dplyr::group_by(region, fachbereich, jahr, anforderung, indikator, bereich) %>%
    dplyr::mutate(props = wert[geschlecht == "Frauen"]/
                    wert[geschlecht == "Gesamt"])

  df <- df %>% dplyr::filter(geschlecht != "Gesamt")


  df <- df %>% dplyr::filter(region %in% states)

  # filter MINT or remaining subjects
  df <- df %>% dplyr::filter(fachbereich == topic)

  # order years for plot
  df <- df[with(df, order(region, jahr, decreasing = TRUE)), ]

  df$props <- df$props * 100

  colnames(df) <- c("Region", "Fachbereich", "anforderung", "Wert","Indikator", "Jahr","Geschlecht",
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

  anforderung <- r$anforderung_arbeitsmarkt_verlauf_bl

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(anforderung == anforderung)

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
  df <-  df %>% dplyr::filter(geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr) %>%
    dplyr::mutate(props = wert[geschlecht == "Frauen"] +
                    wert[geschlecht == "Männer"])

  # calcualte proportions
  df <- df %>% dplyr::group_by(region, indikator, fachbereich, geschlecht, jahr) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100



  df <- df %>% dplyr::filter(region %in% states)

  df <- df %>% dplyr::filter(fachbereich == topic)

  df$geschlecht <- paste0(df$geschlecht, " (", df$indikator, ")")



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
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = geschlecht)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil {point.geschlecht} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), style = list(fontFamily = "SourceSans3-Regular")) %>%
    #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.", style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil von Frauen und Männer ", title_help ," im Verlauf"),
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
  df <- df %>% dplyr::filter(anforderung == "Gesamt")
  df <- df %>% dplyr::filter(geschlecht == "Gesamt")

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
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil von MINT-Berufen"),
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

#' A function to plot bar plot
#'
#' @description A function to plot bar plots
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

beruf_einstieg_vergleich <- function(df,r) {
# gibt nur 2021, daher keine Auswahl hier
  # load UI inputs from reactive value
 # timerange <- r$date_arbeitsmarkt_einstieg_vergleich

  # filter dataset based on UI inputs
 # df <- df %>% dplyr::filter(jahr == timerange)

  # filtern auch nach DE - neuer DF enthält das wieder
  df <- df %>% dplyr::filter(landkreis != "alle Landkreise")
 # df <- df %>% dplyr::filter(bundesland == "Deutschland")
  df <- df %>% dplyr::filter(anforderung == "Gesamt")
  df <- df %>% dplyr::filter(geschlecht == "Gesamt")

  # Deutschland gesamt berechnen und Landkreise/Bundesländer ausschließen -->nicht mehr nötig dann
  df <- df %>%
    dplyr::group_by(jahr, indikator, fachbereich) %>%
    dplyr::summarize(wert = sum(wert))

  df <- calc_arbeitsmarkt_mint(df)
  #df <- calc_arbeitsmarkt_males(df)

  df <- df %>%
    dplyr::group_by(jahr, indikator) %>%
    dplyr::mutate(sum_wert = sum(wert))

  # calcualte proportions
  df <- df %>% dplyr::group_by(jahr, indikator, fachbereich) %>%
    dplyr::summarize(proportion = wert/sum_wert)

  df$proportion <- df$proportion * 100

  #Umbennen von MINT/Andere Berufe
  df$fachbereich[df$fachbereich=="MINT"]<-"Berufe in MINT"

  # Auswahl Indikatoren
  df <- df %>% dplyr::filter(indikator %in% c("Auszubildende",
                                                "Auszubildende (1. Jahr)",
                                                "ausländische Auszubildende",
                                                "Beschäftigte",
                                                "ausländische Beschäftigte",
                                                "Beschäftigte u25",
                                                "Beschäftigte 25-55",
                                                "Beschäftigte ü55",
                                                "in Minijobs"))

  df$indikator <- factor(df$indikator, levels = c("Auszubildende",
                                                  "Auszubildende (1. Jahr)",
                                                  "ausländische Auszubildende",
                                                  "Beschäftigte",
                                                  "ausländische Beschäftigte",
                                                  "Beschäftigte u25",
                                                  "Beschäftigte 25-55",
                                                  "Beschäftigte ü55",
                                                  "in Minijobs"))

  # plot
  highcharter::hchart(df, 'bar', highcharter::hcaes(y = round(proportion), x = indikator, group = "fachbereich")) %>%
    highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = ""), categories = c("Auszubildende",
                                                                  "Auszubildende (1. Jahr)",
                                                                  "ausländische Auszubildende",
                                                                  "Beschäftigte",
                                                                  "ausländische Beschäftigte",
                                                                  "Beschäftigte u25",
                                                                  "Beschäftigte 25-55",
                                                                  "Beschäftigte ü55",
                                                                  "in Minijobs"
                                                                  )) %>%
    highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
    highcharter::hc_colors(c("#efe8e6","#b16fab")) %>%
    highcharter::hc_title(text = paste0("Anteil von MINT-Berufen (2021)"),
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

  #anforderung <- r$anforderung_beruf_arbeitsmarkt_bl_gender_verlauf

  indikator_choice <- r$indikator_beruf_arbeitsmarkt_bl_gender_verlauf

  states <- r$states_beruf_arbeitsmarkt_bl_gender_verlauf

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == indikator_choice)

  df <- prep_arbeitsmarkt_east_west(df)

  df <- df %>% dplyr::filter(region %in% states)

  df <- df %>% dplyr::filter(anforderung != "Keine Zuordnung möglich")

  df <- calc_arbeitsmarkt_males(df)

  df_gesamt <- df %>%
    dplyr::filter(fachbereich == "Alle",
                  anforderung == "Gesamt")

  df <- df %>% dplyr::filter(indikator == indikator_choice)

  df <- df %>%
    dplyr::left_join(df_gesamt, by = c("region", "jahr", "geschlecht", "indikator", "bereich")) %>%
    dplyr::rename(anforderung = "anforderung.x",
                  fachbereich = "fachbereich.x",
                  wert = "wert.x",
                  wert_sum = "wert.y") %>%
    dplyr::select(-c("fachbereich.y", "anforderung.y")) %>%
    dplyr::mutate(proportion = (wert/wert_sum)*100)%>%
    dplyr::filter(anforderung == "Gesamt",
                  fachbereich == "MINT")

  df <- df %>% dplyr::filter(anforderung %in% "Gesamt")

  df <- df %>% dplyr::filter(geschlecht == "Frauen")

  df <- df %>% dplyr::filter(fachbereich == "MINT")

  # order years for plot
  df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

  # if (anforderung == "Gesamt"){
  #
  #   title_help <- " insgesamt"
  #
  # } else {
  #
  #   title_help <- paste0(" mit anforderung ", anforderung)
  #
  # }

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = region)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil <br> Bundesland: {point.region} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Frauen: Wahl von MINT-Berufen (", indikator_choice, ")"
                                        #,title_help
                                        ),
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

  #anforderung <- r$anforderung_arbeitsmarkt_bl_gender_vergleich

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- calc_arbeitsmarkt_males(df)

  df_gesamt <- df %>%
    dplyr::filter(fachbereich == "Alle",
                  anforderung == "Gesamt")

  df <- df %>%
    dplyr::left_join(df_gesamt, by = c("region", "jahr", "geschlecht", "indikator", "bereich")) %>%
    dplyr::rename(anforderung = "anforderung.x",
                  fachbereich = "fachbereich.x",
                  wert = "wert.x",
                  wert_sum = "wert.y") %>%
    dplyr::select(-c("fachbereich.y", "anforderung.y")) %>%
    dplyr::mutate(proportion = (wert/wert_sum)*100)%>%
    dplyr::filter(anforderung == "Gesamt",
                  fachbereich == "MINT")

  df <- df %>% dplyr::filter(anforderung == "Gesamt")

  df <- df %>% dplyr::filter(geschlecht == "Frauen")

  df <- df %>% dplyr::filter(fachbereich == "MINT")

  df <- df %>% dplyr::select(-c("wert", "wert_sum"))

  # spread column
  df <- tidyr::spread(df, indikator, proportion)

  df <- df %>% tidyr::drop_na()

  df2 <- tidyr::gather(df, group, value, -region) %>%
    dplyr::filter(group %in% c("Beschäftigte", "Auszubildende")) %>%
    dplyr::mutate(value = as.numeric(value))

  df$region <- reorder(df$region, df$Beschäftigte)

  df2$region <- factor(df2$region, levels = levels(df$region))

  # if (anforderung == "Gesamt"){
  #
  #   title_help <- " insgesamt"
  #
  # } else {
  #
  #   title_help <- paste0(" mit anforderung ", anforderung)
  #
  # }

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
                                 "Frauen: Wahl von MINT-Berufen<br>"
                                 #, title_help
                                 ,timerange,
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

  #anforderung <- r$anforderung_arbeitsmarkt_bl

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(geschlecht == "Gesamt")

  df <- calc_arbeitsmarkt_share_bl(df)

  df <- df %>% dplyr::filter(fachbereich == "MINT")

  # df <- df %>% dplyr::filter(anforderung == anforderung) # kab

  df <- df %>% dplyr::filter(anforderung == "Gesamt")

  df_employed <- df %>% dplyr::filter(indikator == "Beschäftigte")
  df_trainee <- df %>% dplyr::filter(indikator == "Auszubildende")

  # if (anforderung == "Gesamt"){
  #
  #   title_help_sub <- " insgesamt"
  #
  # } else {
  #
  #   title_help_sub <- paste0(" mit anforderung ", anforderung)
  #
  # }

  highcharter::hw_grid(
    # plot
    highcharter::hcmap(
      "countries/de/de-all",
      data = df_trainee,
      value = "proportion",
      joinBy = c("name", "region"),
      borderColor = "#FAFAFA",
      name = "MINT-Anteil",
      borderWidth = 0.1,
      nullColor = "#A9A9A9",
      tooltip = list(
        valueDecimals = 0,
        valueSuffix = "%"
      )
    ) %>%
      highcharter::hc_colorAxis(min=0,minColor= "#f4f5f6", maxColor="#b16fab", labels = list(format = "{text}%")) %>%
      highcharter::hc_title(
        text = paste0("Anteil von MINT-Berufen (Auszubildende)", br(), timerange #, title_help_sub
                      ),
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
      name = "MINT-Anteil",
      borderWidth = 0.1,
      nullColor = "#A9A9A9",
      tooltip = list(
        valueDecimals = 0,
        valueSuffix = "%"
      )
    ) %>%
      highcharter::hc_colorAxis(min=0,minColor= "#f4f5f6", maxColor="#b16fab",labels = list(format = "{text}%")) %>%
      highcharter::hc_title(
        text = paste0("Anteil von MINT-Berufen (Beschäftigte)", br(), timerange #, title_help_sub
                      ),
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

  #anforderung <- r$anforderung_beruf_arbeitsmarkt_bl_verlauf kab

  indikator_choice <- r$indikator_beruf_arbeitsmarkt_bl_verlauf

  states <- r$states_beruf_arbeitsmarkt_bl_verlauf

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == indikator_choice)

  df <- prep_arbeitsmarkt_east_west(df)

  df <- df %>% dplyr::filter(region %in% states)

  df <- df %>% dplyr::filter(anforderung != "Keine Zuordnung möglich")

  df <- calc_arbeitsmarkt_males(df)

  df <- calc_arbeitsmarkt_share_bl(df)

  #df <- df %>% dplyr::filter(anforderung %in% anforderung) kab

  df <- df %>% dplyr::filter(anforderung == "Gesamt")


  df <- df %>% dplyr::filter(geschlecht == "Gesamt")

  df <- df %>% dplyr::filter(fachbereich == "MINT")

  # order years for plot
  df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

  # if (anforderung == "Gesamt"){
  #
  #   title_help <- " insgesamt"
  #
  # } else {
  #
  #   title_help <- paste0(" mit anforderung ", anforderung) kb
  #
  # }

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = region)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil <br> Bundesland: {point.region} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil von MINT-Berufen (", indikator_choice, ")"
                                        #, title_help
                                        ),
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

#' A function to plot time series
#'
#' @description A function to plot the time series
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_bl_vergleich <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_beruf_arbeitsmarkt_bl_vergleich

  #anforderung <- r$anforderung_beruf_arbeitsmarkt_bl_vergleich

  indikator_choice <- r$indikator_beruf_arbeitsmarkt_bl_vergleich

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(indikator == indikator_choice)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")


  # df <- prep_arbeitsmarkt_east_west(df)

  df <- calc_arbeitsmarkt_males(df)

  df <- calc_arbeitsmarkt_share_bl(df)

  #df <- df %>% dplyr::filter(anforderung %in% anforderung)

  df <- df %>% dplyr::filter(anforderung == "Gesamt")

  df <- df %>% dplyr::filter(geschlecht == "Gesamt")

  df <- df %>% dplyr::filter(fachbereich == "MINT")

  # nur nötig für stacked, machen wir hier doch nicht
  # #gegenwert Berechnen für jeweilige Auswahl
  # df_n <- df %>% dplyr::group_by(region, indikator) %>%
  #   dplyr::mutate(proportion = 100 - proportion)
  # df_n$fachbereich <- "andere Bereiche"
  #
  # df <- rbind(df, df_n)

  # order years for plot
  df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

  # if (anforderung == "Gesamt"){
  #
  #   title_help <- " insgesamt"
  #
  # } else {
  #
  #   title_help <- paste0(" mit anforderung ", anforderung)
  #
  # }

  # plot
  highcharter::hchart(df, 'bar', highcharter::hcaes(y = round(proportion), x = region)) %>%
    highcharter::hc_tooltip(pointFormat = "{point.fachbereich}-Anteil: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
   # highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
   # highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
    highcharter::hc_colors( "#b16fab") %>%
    highcharter::hc_title(text = paste0( "Anteil von MINT-Berufen (", indikator_choice, ")",
                                                                        br(), timerange,
                                                                        "<br><br><br>"),
                          margin = 20,
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

  # ggplot2::ggplot(df, ggplot2::aes(y=region, x=proportion)) +
  #   ggplot2::geom_bar(stat="identity", fill = "#b16fab") +
  #   ggplot2::geom_text(ggplot2::aes(label=paste(round(proportion),"%")), hjust = -0.3,
  #                      fontface = "bold") +
  #   ggplot2::theme_minimal() +
  #   ggplot2::theme(
  #     text = ggplot2::element_text(size = 14),
  #     plot.title = ggtext::element_markdown(hjust = 0.5)) +
  #   ggplot2::ylab("") + ggplot2::xlab("") +
  #   ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
  #                               "Anteil von MINT-Berufen (", indikator_choice, ")",
  #                                br(), timerange,
  #                                "<br><br><br>"),
  #                 fill = "") +
  #   ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))
}

#' A function to plot a waffle chart ::: b3
#'
#' @description A function to create a waffle chart for the tab "Beruf"
#'
#' @return The return value is a waffle chart
#' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_anforderungen_gender <- function(df,r) {


  #timerange <- r$date_arbeitsmarkt_anforderungen_gender

  indikator_choice <- r$level_arbeitsmarkt_anforderungen_gender

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == 2021)

  df <- df %>% dplyr::filter(bundesland == "Deutschland")

  df <- df %>% dplyr::filter(landkreis == "alle Landkreise")

  df <- df %>% dplyr::filter(anforderung == "Gesamt")

  #Indikatoren u25 - ü25 ausfiltern, da hier i nicht nach Geschlecht unterschieden werden kann
  df <- df %>% dplyr::filter(indikator %in% c("Auszubildende",
                                              "Auszubildende (1. Jahr)",
                                              "Beschäftigte",
                                              "ausländische Beschäftigte"))


  # Deutschland gesamt berechnen und Landkreise/Bundesländer ausschließen
  df <- df %>%
    dplyr::group_by(jahr, indikator, fachbereich, geschlecht) %>%
    dplyr::summarize(wert = sum(wert))

  # Auswahl der Berufsgruppen für Waffel
  df <- df %>% dplyr::filter(fachbereich %in% c("Alle", "MINT", "Mathematik, Naturwissenschaften",
                                                "Informatik", "Technik (gesamt)"))

  # Berechnung von andere Fächergruppen
  df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"]-
    df[df$fachbereich == "MINT", "wert"]
  df$fachbereich[df$fachbereich == "Alle"]<-"andere Fächergruppen"
  df <- df %>% dplyr::filter(fachbereich != "MINT")

  # Berechnen Männer
  df_m <- df %>% dplyr::group_by(jahr, indikator, fachbereich) %>%
    dplyr::summarise(wert = wert[geschlecht=="Gesamt"]-wert[geschlecht=="Frauen"])
  df_m$geschlecht <- "Männer"

  df <- df %>%dplyr::filter(geschlecht=="Frauen")

  df <- rbind(df, df_m)


  # Anteil berechnen
  df <- df %>%
    dplyr::group_by(indikator, geschlecht) %>%
    dplyr::mutate(props = sum(wert))

  df <- df %>% dplyr::group_by(fachbereich, indikator, geschlecht) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100


  # Ausgewählte Indikatoren filtern
  df <- df %>% dplyr::filter(indikator == indikator_choice)

  # nach Geschlechtern trennen
  # Frauen
  df_fr <- df %>% dplyr::filter(geschlecht=="Frauen")

  df_fr <- setNames(round_preserve_sum(as.numeric(df_fr$proportion),0),
                          df_fr$fachbereich)
  df_fr <- df_fr[order(factor(names(df_fr), levels = c("Mathematik, Naturwissenschaften",
                                                                "Informatik", "Technik (gesamt)",
                                                                'andere Fächergruppen')))]

  # Männer
  df_me <- df %>% dplyr::filter(geschlecht=="Männer")

  df_me <- setNames(round_preserve_sum(as.numeric(df_me$proportion),0),
                    df_me$fachbereich)
  df_me <- df_me[order(factor(names(df_me), levels = c("Mathematik, Naturwissenschaften",
                                                                "Informatik", "Technik (gesamt)",
                                                                'andere Fächergruppen')))]

  # Titel für Plots
  title_male <- paste0("Berufswahl von Männern <br>(", indikator_choice, ", 2021)")
  title_female <- paste0("Berufswahl von Frauen <br>(", indikator_choice, ", 2021)")

  #waffles
  waffle_fr <- waffle::waffle(df_fr, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", title_female, "<br>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "bottom") +
    # account for the possability that female has 0% share of "Experte
    # if (df_trainee[[3]] == 0) {
    # waffle_fr <- waffle_fr +
    ggplot2::scale_fill_manual(
      values =  c(# "#b16fab",
        "#ee7775",
                 "#fcc433",
                 "#00a87a",
                 '#b1b5c3'),
                 limits = c("Mathematik, Naturwissenschaften",
                            "Informatik", "Technik (gesamt)",
                            'Andere Fachbereiche'),
      na.value="#b1b5c3",
      guide = ggplot2::guide_legend(reverse = TRUE),
      labels = c(
        paste0("Mathematik, Naturwissenschaften",", ",df_fr[1], "%"),
        paste0("Informatik",", ",df_fr[2], "%"),
        paste0("Technik (gesamt)",", ",df_fr[3], "%"),
        paste0("Andere Fachbereiche",", ",df_fr[4], "%")
      )) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))


  waffle_me <- waffle::waffle(df_me, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", title_male ,"<br>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "bottom")+
    ggplot2::scale_fill_manual(
      values =  c(#"#b16fab",
        "#ee7775",
                 "#fcc433",
                 "#00a87a",
                 '#b1b5c3'),
                 limits = c("Mathematik, Naturwissenschaften",
                            "Informatik", "Technik (gesamt)",
                            'Andere Fachbereiche'),
      na.value="#b1b5c3",
      guide = ggplot2::guide_legend(reverse = TRUE),
      labels = c(
        paste0("Mathematik, Naturwissenschaften",", ",df_me[1], "%"),
        paste0("Informatik",", ",df_me[2], "%"),
        paste0("Technik (gesamt)",", ",df_me[3], "%"),
        paste0("Andere Fachbereiche",", ",df_me[4], "%")
      )) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))

  ggpubr::ggarrange(waffle_fr, NULL ,waffle_me, widths = c(1, 0.1, 1), nrow=1)

  # ALT - für alten df, ohne Fächer, nach Anforderungsniveau

  #df <- df %>% dplyr::filter(anforderung != "Helfer")

  # # calculate new "Gesamt
  # df_new_gesamt <- df %>% dplyr::filter(anforderung == "Gesamt")
  #   # dplyr::group_by(region, fachbereich, indikator, jahr, geschlecht, bereich) %>% ## Hier vllt drin lassen, da es die Grafik verfälscht
  #   # dplyr::summarise(wert = sum(wert)) %>%
  #   # dplyr::mutate(anforderung = "Gesamt") %>% dplyr::ungroup()
  #
  # df <- rbind(df %>% dplyr::filter(anforderung != "Gesamt"), df_new_gesamt)

  # df <- calc_arbeitsmarkt_males(df)
  #
  # df_total_gender <- calc_arbeitsmarkt_males(df_new_gesamt) %>%
  #   dplyr::filter(geschlecht != "Gesamt") %>%
  #   dplyr::filter(fachbereich == "Alle") %>%
  #   dplyr::select("region", "indikator", "jahr", "wert", "geschlecht") %>%
  #   dplyr::rename(wert_gesamt = "wert")
  #
  # df <- df %>% dplyr::left_join(df_total_gender, by = c("region", "indikator", "jahr", "geschlecht")) %>%
  #   dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
  #   dplyr::select(-c("wert", "wert_gesamt")) %>%
  #   dplyr::filter(geschlecht != "Gesamt",
  #                 ((fachbereich == "Andere Berufe") & (anforderung == "Gesamt")) | ((fachbereich != "Andere Berufe") & (anforderung != "Gesamt"))) %>%
  #   dplyr::mutate(anforderung = dplyr::case_when(anforderung == "Gesamt" ~ "Andere Berufe",
  #                                                       TRUE ~ anforderung))
  #
  #
  #   # male
  #   df_male <- df %>% dplyr::filter(geschlecht == "Männer")
  #
  #   df_male <- setNames(
  #     round_preserve_sum(as.numeric(df_male$proportion),0),
  #                       df_male$anforderung)
  #
  #   df_male <- df_male[order(factor(names(df_male), levels = c('Fachkraft', 'Spezialist',
  #                                                              'Experte',
  #                                                              'Andere Berufe')))]
  #
  #   df_male1 <- c("MINT"=df_male[1]+df_male[2]+df_male[3], "Andere Fachbereiche"= df_male[4]) # kab
  #
  #   attr(x = df_male1, which = "names") <- c("MINT", "Andere Fachbereiche")
  #
  #  # df_male <- df_male
  #
  #   # female
  #   df_female <- df %>% dplyr::filter(geschlecht == "Frauen")
  #
  #   df_female <- setNames(round_preserve_sum(as.numeric(df_female$proportion),0),
  #                         df_female$anforderung)
  #
  #   df_female <- df_female[order(factor(names(df_female), levels = c('Fachkraft', 'Spezialist',
  #                                                                    'Experte',
  #                                                                    'Andere Berufe')))]
  #
  #   df_female1 <- c("MINT"=df_female[1]+df_female[2]+df_female[3], "Andere Fachbereiche"= df_female[4]) # kab
  #
  #   attr(x = df_female1, which = "names") <- c("MINT", "Andere Fachbereiche")
  #
  #   title_male <- paste0("Berufswahl von Männern <br>(", indikator_choice, ", ", timerange, ")")
  #   title_female <- paste0("Berufswahl von Frauen <br>(", indikator_choice, ", ", timerange, ")")


  # create plot objects for waffle charts
  # waffle_male <- waffle::waffle(df_male1, keep = FALSE) +
  #   ggplot2::labs(
  #     fill = "",
  #     title = paste0("<span style='color:black;'>", title_male, "</span> <br>")) +
  #   ggplot2::theme(plot.title = ggtext::element_markdown(),
  #                  plot.subtitle = ggtext::element_markdown(),
  #                  text = ggplot2::element_text(size = 14),
  #                  plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
  #                  legend.position = "bottom")+
  #   ggplot2::scale_fill_manual(
  #     values =  c("#154194",
  #                 # "#fcc433"
  #                 # ,
  #                 # "#00a87a",
  #                 '#b1b5c3'
  #                 ),
  #     limits = c(
  #       # 'Fachkraft', 'Spezialist',
  #       #          'Experte',
  #       "MINT",
  #                'Andere Fachbereiche'),
  #     # na.value="#b1b5c3",
  #     guide = ggplot2::guide_legend(reverse = TRUE),
  #     labels = c(
  #       paste0("MINT",", ",df_male1[1], "%"),
  #       # paste0("MINT-Spezialist",", ",df_male[2], "%"),
  #       # paste0("MINT-Experte",", ",df_male[3], "%"),
  #       paste0("Andere Bereiche",", ",df_male1[2], "%"))) +
  #   ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))
  #
  #
  # waffle_female <- waffle::waffle(df_female1, keep = FALSE) +
  #   ggplot2::labs(
  #     fill = "",
  #     title = paste0("<span style='color:black;'>", title_female,"</span> <br>")) +
  #   ggplot2::theme(plot.title = ggtext::element_markdown(),
  #                  plot.subtitle = ggtext::element_markdown(),
  #                  text = ggplot2::element_text(size = 14),
  #                  plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
  #                  legend.position = "bottom")
  #
  # # account for the possability that female has 0% share of "Experte
  # #if (df_female[[3]] == 0) {
  #
  #   waffle_female1 <- waffle_female +
  #     ggplot2::scale_fill_manual(
  #       values =  c("#154194",
  #                   # "#fcc433",
  #                   # # "#00a87a",
  #                   '#b1b5c3'),
  #       limits = c("MINT",
  #         # 'Fachkraft',
  #         #          'Spezialist',
  #                  'Andere Fachbereiche'),
  #       guide = ggplot2::guide_legend(reverse = TRUE),
  #       # na.value="#b1b5c3",
  #       labels = c(
  #         paste0("MINT",", ",df_female1[1], "%"),
  #         #paste0("MINT-Spezialistin",", ",df_female[2], "%"),
  #         # paste0("Experte",", ",df_female[3], "%"),
  #         paste0("Andere Fachbereiche",", ",df_female1[2], "%"))) +
  #     ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))

  # } else{
  #
  #   waffle_female <- waffle_female +
  #     ggplot2::scale_fill_manual(
  #       values =  c("#ee7775",
  #                   # "#fcc433",
  #                   # "#00a87a",
  #                   '#b1b5c3'),
  #       limits = c("MINT",
  #         # 'Fachkraft', 'Spezialist',
  #         #          'Experte',
  #                  'Andere Fachbereiche'),
  #       na.value="#b1b5c3",
  #       guide = ggplot2::guide_legend(reverse = TRUE),
  #       labels = c(
  #         paste0("MINT-Beschäftigte",", ",df_female1[1], "%"),
  #         # paste0("Spezialist",", ",df_female[2], "%"),
  #         # paste0("Experte",", ",df_female[3], "%"),
  #         paste0("Andere Fachbereiche",", ",df_female1[2], "%"))) +
  #     ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))
  #
  # }

  # ggpubr::ggarrange(waffle_female1, NULL ,waffle_male, widths = c(1, 0.1, 1), nrow=1)

  # text <- c(
  #   paste0("<span style='font-size:20.5pt; color:black'> Anforderungslevel in MINT-Berufen im Vergleich"))

  # ggpubr::annotate_figure(plot, gridtext::richtext_grob(text = text))

}

#' A function to plot time series
#'
#' @description A function to plot the time series
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_anforderungen_verlauf_gender <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt_anforderungen_gender_verlauf

  anforderung <- r$level_arbeitsmarkt_anforderungen_gender_verlauf

  states <- r$states_arbeitsmarkt_anforderungen_gender_verlauf

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  # remove
  #df <- df %>% dplyr::filter(anforderung != "Helfer")### kab

  df <- prep_arbeitsmarkt_east_west(df)

  # calculate new "Gesamt
  df_new_gesamt <- df %>% dplyr::filter(anforderung == "Gesamt")
    # dplyr::group_by(region, fachbereich, indikator, jahr, geschlecht, bereich) %>%
    # dplyr::summarise(wert = sum(wert)) %>%
    # dplyr::mutate(anforderung = "Gesamt") %>% dplyr::ungroup()



  df <- rbind(df %>% dplyr::filter(anforderung != "Gesamt"), df_new_gesamt)

  df <- df %>% dplyr::filter(region %in% states)

  df <- calc_arbeitsmarkt_males(df)

  df <- calc_arbeitsmarkt_mint(df)

  df_total_gender <- calc_arbeitsmarkt_males(df_new_gesamt) %>%
    dplyr::filter(geschlecht != "Gesamt") %>%
    dplyr::filter(fachbereich == "Alle") %>%
    dplyr::select("region", "indikator", "jahr", "wert", "geschlecht") %>%
    dplyr::rename(wert_gesamt = "wert")

  df <- df %>% dplyr::left_join(df_total_gender, by = c("region", "indikator", "jahr", "geschlecht")) %>%
    dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
    dplyr::select(-c("wert", "wert_gesamt")) %>%
    dplyr::filter(geschlecht == "Frauen")

  df <- df %>% dplyr::filter(anforderung %in% anforderung)

  df <- df %>% dplyr::filter(fachbereich == "MINT")

  df$geschlecht <- paste0(df$geschlecht, " (", df$indikator, ")")

  # order years for plot
  df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

  if (anforderung == "Gesamt"){

    title_help <- " insgesamt"

  } else {

    title_help <- paste0(" mit anforderung ", anforderung)

  }

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = geschlecht)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil <br> Bundesland: {point.region} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
   # highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Arbeitnehmerinnen: ", title_help, " in ", states),
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

#' A function to create a dumbbell plot
#'
#' @description A function to return a ranking of subject for both genders
#'
#' @return The return value is a dumbbell plot
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_anforderungen_vergleich_gender <- function(df, r) {

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt_anforderungen_gender_vegleich

  states <- r$states_arbeitsmarkt_anforderungen_gender_vegleich

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  # remove
  # df <- df %>% dplyr::filter(anforderung != "Helfer")

  df <- prep_arbeitsmarkt_east_west(df)

  # calculate new "Gesamt
  df_new_gesamt <- df %>% dplyr::filter(anforderung == "Gesamt")
    # dplyr::group_by(region, fachbereich, indikator, jahr, geschlecht, bereich) %>%
    # dplyr::summarise(wert = sum(wert)) %>%
    # dplyr::mutate(anforderung = "Gesamt") %>% dplyr::ungroup()

  df <- rbind(df %>% dplyr::filter(anforderung != "Gesamt"), df_new_gesamt)

  df <- df %>% dplyr::filter(region %in% states)

  df <- calc_arbeitsmarkt_males(df)

  df <- calc_arbeitsmarkt_mint(df)

  df_total_gender <- calc_arbeitsmarkt_males(df_new_gesamt) %>%
    dplyr::filter(geschlecht != "Gesamt") %>%
    dplyr::filter(fachbereich == "Alle") %>%
    dplyr::select("region", "indikator", "jahr", "wert", "geschlecht") %>%
    dplyr::rename(wert_gesamt = "wert")

  df <- df %>% dplyr::left_join(df_total_gender, by = c("region", "indikator", "jahr", "geschlecht")) %>%
    dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
    dplyr::select(-c("wert", "wert_gesamt")) %>%
    dplyr::filter(geschlecht == "Frauen")

  df <- df %>% dplyr::filter(fachbereich == "MINT")

  # spread column
  df <- tidyr::spread(df, indikator, proportion)

  df <- df %>% tidyr::drop_na()

  df2 <- tidyr::gather(df, group, value, -anforderung) %>%
    dplyr::filter(group %in% c("Beschäftigte", "Auszubildende")) %>%
    dplyr::mutate(value = as.numeric(value))

  df$anforderung <- reorder(df$anforderung, df$Beschäftigte)

  df2$anforderung <- factor(df2$anforderung, levels = levels(df$anforderung))

  ggplot2::ggplot(df,
                  ggplot2::aes(y = anforderung)) +
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
                                 "Frauen: Wahl von MINT-Berufen <br>", states, " in ",timerange,
                                 "<br><br><br>"),
                  color = "") +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))

}

#' A function to plot a waffle chart
#'
#' @description A function to create a waffle chart for the tab "Beruf"
#'
#' @return The return value is a waffle chart
#' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_anforderungen <- function(df,r) {

  timerange <- r$date_arbeitsmarkt_anforderungen

  # filter dataset based on UI inputs
 # df <- df %>% dplyr::filter(jahr == timerange)

 # df <- df %>% dplyr::filter(bundesland == "Deutschland")
  # im neuen DF doch Aggregate enthalten, ausfiltern das Folgecode weiter stimmt
  df <- df %>% dplyr::filter(landkreis != "alle Landkreise")

  df <- df %>% dplyr::filter(geschlecht == "Gesamt")

  df <- df %>% dplyr::filter(anforderung == "Gesamt")

  # Deutschland gesamt berechnen und Landkreise/Bundesländer ausschließen
  df <- df %>%
    dplyr::group_by(jahr, indikator, fachbereich) %>%
    dplyr::summarize(wert = sum(wert))

  # Vorläufig nur Indiaktoren Beschäftigte, Auszubildende, später hier mehr Auswahl
  df <- df %>% dplyr::filter(indikator %in% c("Beschäftigte", "Auszubildende"))

  # Auswahl der Berufsgruppen für Waffel
  df <- df %>% dplyr::filter(fachbereich %in% c("Alle", "MINT", "Mathematik, Naturwissenschaften",
                                                "Informatik", "Technik (gesamt)"))

  # Berechnung von andere Fächergruppen
  df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"]-
    df[df$fachbereich == "MINT", "wert"]
  df$fachbereich[df$fachbereich == "Alle"]<-"andere Fächergruppen"
    df <- df %>% dplyr::filter(fachbereich != "MINT")


  # Anteil berechnen
    df <- df %>%
      dplyr::group_by(indikator) %>%
      dplyr::mutate(props = sum(wert))

    df <- df %>% dplyr::group_by(fachbereich, indikator) %>%
      dplyr::summarize(proportion = wert/props)

    df$proportion <- df$proportion * 100

  #  Aufteilung nach Indikator
    df_besch <- df
    df_besch <- df_besch %>% dplyr::filter(indikator == "Beschäftigte")
    df_azubi <- df
    df_azubi <- df_azubi %>% dplyr::filter(indikator == "Auszubildende")

    df_besch <- setNames(round_preserve_sum(as.numeric(df_besch$proportion),0),
                  df_besch$fachbereich)
    df_besch <- df_besch[order(factor(names(df_besch), levels = c("Mathematik, Naturwissenschaften",
                                                                  "Informatik", "Technik (gesamt)",
                                                                  'andere Fächergruppen')))]
    df_azubi <- setNames(round_preserve_sum(as.numeric(df_azubi$proportion),0),
                  df_azubi$fachbereich)
    df_azubi <- df_azubi[order(factor(names(df_azubi), levels = c("Mathematik, Naturwissenschaften",
                                                                  "Informatik", "Technik (gesamt)",
                                                                  'andere Fächergruppen')))]

  # plots
      waffle_employed <- waffle::waffle(df_besch, keep = FALSE) +
        ggplot2::labs(
          fill = "",
          title = paste0("<span style='color:black;'>", "Berufswahl (Beschäftigte)</span> <br>", timerange, "<br>")) +
        ggplot2::theme(plot.title = ggtext::element_markdown(),
                       plot.subtitle = ggtext::element_markdown(),
                       text = ggplot2::element_text(size = 14),
                       plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                       legend.position = "bottom") +
        ggplot2::scale_fill_manual(
          values =  c(# "#b16fab",
                        "#ee7775",
                       "#fcc433",
                       "#00a87a",
                      '#b1b5c3'),
          limits = c("Mathematik, Naturwissenschaften",
                     "Informatik", "Technik (gesamt)",
                     'Andere Fachbereiche'),
          na.value="#b1b5c3",
          guide = ggplot2::guide_legend(reverse = TRUE),
          labels = c(
            paste0("Mathematik, Naturwissenschaften",", ",df_besch[1], "%"),
            paste0("Informatik",", ",df_besch[2], "%"),
            paste0("Technik (gesamt)",", ",df_besch[3], "%"),
            paste0("Andere Fachbereiche",", ",df_besch[4], "%")
            )) +
        ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))


      waffle_trainee <- waffle::waffle(df_azubi, keep = FALSE) +
        ggplot2::labs(
          fill = "",
          title = paste0("<span style='color:black;'>", "Berufswahl (Auszubildende)</span> <br>", timerange, "<br>")) +
        ggplot2::theme(plot.title = ggtext::element_markdown(),
                       plot.subtitle = ggtext::element_markdown(),
                       text = ggplot2::element_text(size = 14),
                       plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                       legend.position = "bottom")+
          ggplot2::scale_fill_manual(
            values =  c(#"#b16fab",
                        "#ee7775",
                       "#fcc433",
                       "#00a87a",
                       '#b1b5c3'),
            limits = c("Mathematik, Naturwissenschaften",
                       "Informatik", "Technik (gesamt)",
                       'Andere Fachbereiche'),
            na.value="#b1b5c3",
            guide = ggplot2::guide_legend(reverse = TRUE),
            labels = c(
              paste0("Mathematik, Naturwissenschaften",", ",df_azubi[1], "%"),
              paste0("Informatik",", ",df_azubi[2], "%"),
              paste0("Technik (gesamt)",", ",df_azubi[3], "%"),
              paste0("Andere Fachbereiche",", ",df_azubi[4], "%")
            )) +
        ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))

#
#   # calculate new "Gesamt
#   df_new_gesamt <- df %>% dplyr::filter(anforderung == "Gesamt")
#     # dplyr::group_by(region, fachbereich, indikator, jahr, geschlecht, bereich) %>%
#     # dplyr::summarise(wert = sum(wert)) %>%
#     # dplyr::mutate(anforderung = "Gesamt") %>%
#     # dplyr::ungroup()
#
#   df <- rbind(df %>% dplyr::filter(anforderung != "Gesamt"), df_new_gesamt)
#
#   df <- calc_arbeitsmarkt_mint(df)
#
#   df_new_gesamt <- df_new_gesamt %>%
#     dplyr::filter(fachbereich == "Alle") %>%
#     dplyr::rename(wert_gesamt = "wert") %>%
#     dplyr::select(-c("fachbereich", "anforderung"))
#
#   df <- df %>% dplyr::left_join(df_new_gesamt, by = c("region", "indikator", "jahr", "geschlecht", "bereich")) %>%
#     dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
#     dplyr::select(-c("wert", "wert_gesamt")) %>%
#     dplyr::filter(((fachbereich == "Andere Berufe") & (anforderung == "Gesamt")) | ((fachbereich != "Andere Berufe") & (anforderung != "Gesamt"))) %>%
#     dplyr::mutate(anforderung = dplyr::case_when(anforderung == "Gesamt" ~ "Andere Berufe",
#                                                         TRUE ~ anforderung))
#
#   # employed
#   df_employed <- df %>% dplyr::filter(indikator == "Beschäftigte")
#
#   df_employed <- setNames(round_preserve_sum(as.numeric(df_employed$proportion),0),
#                       df_employed$anforderung)
#
#   df_employed <- df_employed[order(factor(names(df_employed), levels = c('Fachkraft', 'Spezialist',
#                                                              'Experte',
#                                                              'Andere Berufe')))]
#
#   df_employed1 <- c("MINT"=df_employed[1]+df_employed[2]+df_employed[3], "Andere Fachbereiche"= df_employed[4]) # kab
#
#   attr(x = df_employed1, which = "names") <- c("MINT", "Andere Fachbereiche")
#
#   # trainee
#   df_trainee <- df %>% dplyr::filter(indikator == "Auszubildende")
#
#   df_trainee <- setNames(round_preserve_sum(as.numeric(df_trainee$proportion),0),
#                         df_trainee$anforderung)
#
#   df_trainee <- df_trainee[order(factor(names(df_trainee), levels = c('Fachkraft', 'Spezialist',
#                                                                    'Experte',
#                                                                    'Andere Berufe')))]
#
#   df_trainee1 <- c("MINT"=df_trainee[1]+df_trainee[2]+df_trainee[3], "Andere Fachbereiche"= df_trainee[4]) # kab
#
#   attr(x = df_trainee1, which = "names") <- c("MINT", "Andere Fachbereiche")
#
#   # create plot objects for waffle charts
#   waffle_employed <- waffle::waffle(df_employed1, keep = FALSE) +
#     ggplot2::labs(
#       fill = "",
#       title = paste0("<span style='color:black;'>", "Berufswahl (Beschäftigte)</span> <br>", timerange, "<br>")) +
#     ggplot2::theme(plot.title = ggtext::element_markdown(),
#                    plot.subtitle = ggtext::element_markdown(),
#                    text = ggplot2::element_text(size = 14),
#                    plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
#                    legend.position = "bottom") +
#     ggplot2::scale_fill_manual(
#       values =  c("#b16fab",
#                   # "#fcc433",
#                   # "#00a87a",
#                   '#b1b5c3'),
#       limits = c("MINT",
#         # 'Fachkraft', 'Spezialist',
#         #          'Experte',
#                  'Andere Fachbereiche'),
#       na.value="#b1b5c3",
#       guide = ggplot2::guide_legend(reverse = TRUE),
#       labels = c(
#         paste0("MINT",", ",df_employed1[1], "%"),
#         # paste0("MINT-Spezialist:in",", ",df_employed[2], "%"),
#         # paste0("MINT-Expert:in",", ",df_employed[3], "%"),
#         paste0("Andere Fachbereche",", ",df_employed1[2], "%"))) +
#     ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))
#
#
#   waffle_trainee <- waffle::waffle(df_trainee1, keep = FALSE) +
#     ggplot2::labs(
#       fill = "",
#       title = paste0("<span style='color:black;'>", "Berufswahl (Auszubildende)</span> <br>", timerange, "<br>")) +
#     ggplot2::theme(plot.title = ggtext::element_markdown(),
#                    plot.subtitle = ggtext::element_markdown(),
#                    text = ggplot2::element_text(size = 14),
#                    plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
#                    legend.position = "bottom")
#
#   # account for the possability that female has 0% share of "Experte
#   # if (df_trainee[[3]] == 0) {
#
#     waffle_trainee <- waffle_trainee +
#       ggplot2::scale_fill_manual(
#         values =  c("#b16fab",
#                     # "#fcc433",
#                     # # "#00a87a",
#                     '#b1b5c3'),
#         limits = c("MINT",
#           # 'Fachkraft',
#           #          'Spezialist',
#                    'Andere Fachbereiche'),
#         guide = ggplot2::guide_legend(reverse = TRUE),
#         na.value="#b1b5c3",
#         labels = c(
#           paste0("MINT",", ",df_trainee1[1], "%"),
#           # paste0("MINT-Spezialist:in",", ",df_trainee[2], "%"),
#           # paste0("MINT-Expert:in",", ",df_trainee[3], "%"),
#           paste0("Andere Fachbereiche",", ",df_trainee1[2], "%"))) +
#       ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))

  # } else{
  #
  #   waffle_trainee <- waffle_trainee +
  #     ggplot2::scale_fill_manual(
  #       values =  c("#ee7775",
  #                   "#fcc433",
  #                   "#00a87a",
  #                   '#b1b5c3'),
  #       limits = c('Fachkraft', 'Spezialist',
  #                  'Experte',
  #                  'Andere Berufe'),
  #       na.value="#b1b5c3",
  #       guide = ggplot2::guide_legend(reverse = TRUE),
  #       labels = c(
  #         paste0("MINT-Fachkraft",", ",df_trainee[1], "%"),
  #         paste0("MINT-Spezialist:in",", ",df_trainee[2], "%"),
  #         paste0("MINT-Expert:in",", ",df_trainee[3], "%"),
  #         paste0("Andere Berufe",", ",df_trainee[4], "%"))) +
  #     ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))
  #
  # }

  ggpubr::ggarrange(waffle_trainee, NULL ,waffle_employed, widths = c(1, 0.1, 1), nrow=1)

}

#' A function to plot time series
#'
#' @description A function to plot the time series
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_anforderungen_verlauf <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt_anforderungen_verlauf

  states <- r$states_arbeitsmarkt_anforderungen_verlauf

  indikator_choice <- r$indikator_arbeitsmarkt_anforderungen_verlauf

  anforderung <- r$anforderung_arbeitsmarkt_anforderungen_verlauf

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  # remove
  df <- df %>% dplyr::filter(geschlecht == "Gesamt")

  # df <- df %>% dplyr::filter(anforderung != "Helfer")

  df <- prep_arbeitsmarkt_east_west(df)

  # calculate new "Gesamt
  df_new_gesamt <- df %>% dplyr::filter(anforderung == "Gesamt")
    # dplyr::group_by(region, fachbereich, indikator, jahr, geschlecht, bereich) %>%
    # dplyr::summarise(wert = sum(wert)) %>%
    # dplyr::mutate(anforderung = "Gesamt") %>%
    # dplyr::ungroup()

  df <- rbind(df %>% dplyr::filter(anforderung != "Gesamt"), df_new_gesamt)

  # df <- calc_arbeitsmarkt_mint(df)

  df_new_gesamt <- df_new_gesamt %>%
    dplyr::filter(fachbereich == "Alle") %>%
    dplyr::rename(wert_gesamt = "wert") %>%
    dplyr::select(-c("fachbereich", "anforderung"))

  df <- df %>% dplyr::left_join(df_new_gesamt, by = c("region", "indikator", "jahr", "geschlecht", "bereich")) %>%
    dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
    dplyr::select(-c("wert", "wert_gesamt"))

  df <- df %>% dplyr::filter(region == states)

  df <- df %>% dplyr::filter(indikator == indikator_choice)

  df <- df %>% dplyr::filter(fachbereich == "MINT")

  df <- df %>% dplyr::filter(anforderung %in% anforderung)

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = anforderung)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil <br> Bundesland: {point.region} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil der Arbeitnehmerinnen im Zeitverlauf in ", states),
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

#' A function to plot a bar plot
#'
#' @description A function to create a bar plot for the "Beruf" tab
#'
#' @return The return value is a waffle chart
#' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_anforderungen_vergleich <- function(df,r) {

  timerange <- r$date_arbeitsmarkt_anforderungen_vergleich

  states <- r$states_arbeitsmarkt_anforderungen_vergleich

  indikator_choice <- r$indikator_arbeitsmarkt_anforderungen_vergleich

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  # remove
  df <- df %>% dplyr::filter(geschlecht == "Gesamt")

  # df <- df %>% dplyr::filter(anforderung != "Helfer")

  df <- prep_arbeitsmarkt_east_west(df)

  # calculate new "Gesamt
  # df_new_gesamt <- df %>% dplyr::filter(anforderung != "Gesamt") %>%                       ###kab
  #   dplyr::group_by(region, fachbereich, indikator, jahr, geschlecht, bereich) %>%
  #   dplyr::summarise(wert = sum(wert)) %>%
  #   dplyr::mutate(anforderung = "Gesamt") %>%
  #   dplyr::ungroup()

  df_new_gesamt <- df %>% dplyr::filter(anforderung == "Gesamt")
      #dplyr::group_by(region, fachbereich, indikator, jahr, geschlecht, bereich) %>%
     # dplyr::summarise(wert = sum(wert)) %>%
      # dplyr::mutate(anforderung = "Gesamt") %>%
      #dplyr::ungroup()

  df <- rbind(df %>% dplyr::filter(anforderung != "Gesamt"), df_new_gesamt)

  # df <- calc_arbeitsmarkt_mint(df)

  df_new_gesamt <- df_new_gesamt %>%
    dplyr::filter(fachbereich == "Alle") %>%
    dplyr::rename(wert_gesamt = "wert") %>%
    dplyr::select(-c("fachbereich", "anforderung"))

  df <- df %>% dplyr::filter(region == states)

  df <- df %>% dplyr::left_join(df_new_gesamt, by = c("region", "indikator", "jahr", "geschlecht", "bereich")) %>%
    dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
    dplyr::select(-c("wert", "wert_gesamt"))

  df <- df %>% dplyr::filter(indikator == indikator_choice)

  df <- df %>% dplyr::filter(fachbereich == "MINT")

  reihenfolge <- c("Experte", "Spezialist", "Fachkraft", "Gesamt")

  df <- df %>%
    dplyr::mutate(anforderung =  factor(anforderung, levels = reihenfolge)) %>%
    dplyr::arrange(anforderung)

  # plot
  a <- ifelse(df$anforderung == "Gesamt", "#b16fab", "grey30")

  ggplot2::ggplot(df, ggplot2::aes(y=anforderung, x=proportion)) +
    ggplot2::geom_bar(stat="identity", fill = "#b16fab") +
    ggplot2::geom_text(ggplot2::aes(label=paste(round(proportion),"%")), hjust = -0.3,
                       fontface = "bold") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(colour = a),
      text = ggplot2::element_text(size = 14),
      plot.title = ggtext::element_markdown(hjust = 0.5)) +
    ggplot2::ylab("") + ggplot2::xlab("Anteil") +
    ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
                                 indikator_choice, ": Anteil der anforderungs im Vergleich in ", timerange,
                                 "<br><br><br>"),
                  fill = "") +
    ggplot2::scale_y_discrete(expand = c(0,0)) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))

}

#' A function to plot a pic charts
#'
#' @description A function to create pie charts for the tab "Beruf".
#'
#' @return The return value is a plot
#' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_einstieg_pie_gender <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt_einstieg_gender

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")

  # remove
  # df <- df %>% dplyr::filter(anforderung != "Helfer")

  # calculate new "Gesamt
  df_new_gesamt <- df %>% dplyr::filter(anforderung == "Gesamt")
    # dplyr::group_by(region, fachbereich, indikator, jahr, geschlecht, bereich) %>%      ## kab
    # dplyr::summarise(wert = sum(wert)) %>%
    # dplyr::mutate(anforderung = "Gesamt") %>%
    # dplyr::ungroup()
  df <- rbind(df %>% dplyr::filter(anforderung != "Gesamt"), df_new_gesamt)

  df <- df %>% dplyr::filter(anforderung == "Gesamt")

  df <- calc_arbeitsmarkt_mint(df)

  df <- calc_arbeitsmarkt_males(df)


  df_sub_new_gesamt <- df %>% dplyr::filter(geschlecht == "Gesamt") %>%
    dplyr::rename(wert_sub_gesamt = "wert") %>%
    dplyr::select(-c("geschlecht", "anforderung"))

  df_new_gesamt <- df_new_gesamt %>%
    dplyr::filter(fachbereich == "Alle",
                  geschlecht == "Gesamt") %>%
    dplyr::rename(wert_gesamt = "wert")

  df <- df %>%
    dplyr::left_join(df_sub_new_gesamt, by = c("region", "indikator", "jahr", "bereich", "fachbereich")) %>%
    dplyr::left_join(df_new_gesamt, by = c("region", "indikator", "jahr", "bereich")) %>%
    dplyr::rename(fachbereich = "fachbereich.x",
                  anforderung = "anforderung.x",
                  geschlecht = "geschlecht.x") %>%
    dplyr::mutate(proportion_fachbereich = (wert/wert_sub_gesamt)*100) %>%
    dplyr::mutate(proportion_gesamt = (wert/wert_gesamt)*100)%>%
    dplyr::select(-c("wert", "wert_gesamt", "fachbereich.y", "anforderung.y", "geschlecht.y"))

  # Datasets
  df_employed_mint <- df %>% dplyr::filter(indikator == "Beschäftigte",
                       fachbereich == "MINT",
                       geschlecht != "Gesamt")

  df_employed_andere <- df %>% dplyr::filter(indikator == "Beschäftigte",
                                       fachbereich == "Andere Berufe",
                                       geschlecht != "Gesamt")

  df_trainee_mint <- df %>% dplyr::filter(indikator == "Auszubildende",
                                       fachbereich == "MINT",
                                       geschlecht != "Gesamt")

  df_trainee_andere <- df %>% dplyr::filter(indikator == "Auszubildende",
                                         fachbereich == "Andere Berufe",
                                         geschlecht != "Gesamt")

  # Trainee plots
  plot_trainee_mint <- highcharter::hchart(df_trainee_mint, size = 280, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion_gesamt)) %>%
    highcharter::hc_tooltip(
      pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
    highcharter::hc_title(text = paste0("MINT-Berufe (Auszubildende)", br(), timerange),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
    highcharter::hc_legend(enabled = TRUE) %>%
    highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                           dataLabels = list(enabled = TRUE,  format='{point.percentage:.0f}%'), showInLegend = TRUE)) %>%
    highcharter::hc_colors(c("#154194","#efe8e6"))

  plot_trainee_andere <- highcharter::hchart(df_trainee_andere, size = 150, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion_gesamt)) %>%
    highcharter::hc_tooltip(
      pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
    highcharter::hc_title(text = paste0("Nicht-MINT-Berufen (Auszubildende)", br(), timerange),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
    highcharter::hc_legend(enabled = TRUE, y = -180) %>%
    highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                           dataLabels = list(enabled = TRUE,  format='{point.percentage:.0f}%'), showInLegend = TRUE,
                                           opacity = 0.7)) %>%
    highcharter::hc_colors(c("#154194","#efe8e6"))

  # Employed plots
  plot_employed_mint <- highcharter::hchart(df_employed_mint, size = 280, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion_gesamt)) %>%
    highcharter::hc_tooltip(
      pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
    highcharter::hc_title(text = paste0("MINT-Berufe (Beschäftigung)",br(), timerange),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
    highcharter::hc_legend(enabled = TRUE) %>%
    highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                           dataLabels = list(enabled = TRUE,  format='{point.percentage:.0f}%'), showInLegend = TRUE)) %>%
    highcharter::hc_colors(c("#154194","#efe8e6"))

  plot_employed_andere <- highcharter::hchart(df_employed_andere, size = 150, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion_gesamt)) %>%
    highcharter::hc_tooltip(
      pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
    highcharter::hc_title(text = paste0("Nicht-MINT-Berufe (Beschäftigte)",br(), timerange),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
    highcharter::hc_legend(enabled = TRUE, y = -180) %>%
    highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                           dataLabels = list(enabled = TRUE,  format='{point.percentage:.0f}%'), showInLegend = TRUE,
                                           opacity = 0.7)) %>%
    highcharter::hc_colors(c("#154194","#efe8e6"))

  # place plots inside grid
  highcharter::hw_grid(

    plot_trainee_mint,

    plot_employed_mint,

    plot_trainee_andere,

    plot_employed_andere,

    ncol = 2,
    browsable = TRUE
  )

}

#' A function to plot time series
#'
#' @description A function to plot the time series
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_einstieg_verlauf_gender <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt_einstieg_verlauf_gender

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(region == "Deutschland")

  # remove
  # df <- df %>% dplyr::filter(anforderung != "Helfer")

  # calculate new "Gesamt
  df_new_gesamt <- df %>% dplyr::filter(anforderung == "Gesamt")
    # dplyr::group_by(region, fachbereich, indikator, jahr, geschlecht, bereich) %>%
    # dplyr::summarise(wert = sum(wert)) %>%
    # dplyr::mutate(anforderung = "Gesamt") %>%
    # dplyr::ungroup()

  df <- rbind(df %>% dplyr::filter(anforderung != "Gesamt"), df_new_gesamt)

  df <- df %>% dplyr::filter(anforderung == "Gesamt")

  df <- calc_arbeitsmarkt_mint(df)

  df <- calc_arbeitsmarkt_males(df)

  df_sub_new_gesamt <- df %>% dplyr::filter(geschlecht == "Gesamt") %>%
    dplyr::rename(wert_sub_gesamt = "wert") %>%
    dplyr::select(-c("geschlecht", "anforderung"))

  df_new_gesamt <- df_new_gesamt %>%
    dplyr::filter(fachbereich == "Alle",
                  geschlecht == "Gesamt") %>%
    dplyr::rename(wert_gesamt = "wert")

  df <- df %>%
    dplyr::left_join(df_sub_new_gesamt, by = c("region", "indikator", "jahr", "bereich", "fachbereich")) %>%
    dplyr::left_join(df_new_gesamt, by = c("region", "indikator", "jahr", "bereich")) %>%
    dplyr::rename(fachbereich = "fachbereich.x",
                  anforderung = "anforderung.x",
                  geschlecht = "geschlecht.x") %>%
    dplyr::mutate(proportion_fachbereich = (wert/wert_sub_gesamt)*100) %>%
    dplyr::mutate(proportion_gesamt = (wert/wert_gesamt)*100) %>%
    dplyr::select(-c("wert", "wert_gesamt", "fachbereich.y", "anforderung.y", "geschlecht.y")) %>%
    dplyr::filter(geschlecht == "Frauen",
                  fachbereich == "MINT")


  # order years for plot
  df <- df[with(df, order(jahr, decreasing = FALSE)), ]

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion_fachbereich), group = indikator)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil Frauen  {point.indikator} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
   # highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil von Frauen in MINT-Berufen"),
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

#' A function to plot time series
#'
#' @description A function to plot a bar chart
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_einstieg_vergleich_gender <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt_einstieg_vergleich_gender

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")

  # remove
 #df <- df %>% dplyr::filter(anforderung != "Helfer")

  # calculate new "Gesamt
  df_new_gesamt <- df %>% dplyr::filter(anforderung == "Gesamt")
    # dplyr::group_by(region, fachbereich, indikator, jahr, geschlecht, bereich) %>%
    # dplyr::summarise(wert = sum(wert)) %>%
    # dplyr::mutate(anforderung = "Gesamt") %>%
    # dplyr::ungroup()

  df <- rbind(df %>% dplyr::filter(anforderung != "Gesamt"), df_new_gesamt)

  df <- df %>% dplyr::filter(anforderung == "Gesamt")

  df <- calc_arbeitsmarkt_mint(df)

  df <- calc_arbeitsmarkt_males(df)

  df_sub_new_gesamt <- df %>% dplyr::filter(geschlecht == "Gesamt") %>%
    dplyr::rename(wert_sub_gesamt = "wert") %>%
    dplyr::select(-c("geschlecht", "anforderung"))

  df_new_gesamt <- df_new_gesamt %>%
    dplyr::filter(fachbereich == "Alle",
                  geschlecht == "Gesamt") %>%
    dplyr::rename(wert_gesamt = "wert")

  df <- df %>%
    dplyr::left_join(df_sub_new_gesamt, by = c("region", "indikator", "jahr", "bereich", "fachbereich")) %>%
    dplyr::left_join(df_new_gesamt, by = c("region", "indikator", "jahr", "bereich")) %>%
    dplyr::rename(fachbereich = "fachbereich.x",
                  anforderung = "anforderung.x",
                  geschlecht = "geschlecht.x") %>%
    dplyr::mutate(proportion_fachbereich = (wert/wert_sub_gesamt)*100) %>%
    dplyr::mutate(proportion_gesamt = (wert/wert_gesamt)*100) %>%
    dplyr::select(-c("wert", "wert_gesamt", "fachbereich.y", "anforderung.y", "geschlecht.y")) %>%
    dplyr::filter(geschlecht == "Frauen",
                  fachbereich != "Gesamt")

  #gegenwert Berechnen für jeweilige Auswahl
  df_n <- df %>% dplyr::group_by(region, fachbereich, indikator, jahr) %>%
    dplyr::mutate(proportion_fachbereich = 100 - proportion_fachbereich)
  df_n$geschlecht <- "Männer"

  df <- rbind(df, df_n)

  # Indikator-Kombination benennen
  df$indikator <- ifelse(df$indikator == "Auszubildende" & df$fachbereich == "MINT", "Auszubildende in MINT", df$indikator)
  df$indikator <- ifelse(df$indikator == "Auszubildende" & df$fachbereich == "Andere Berufe", "Auszubildende in anderen Berufen", df$indikator)
  df$indikator <- ifelse(df$indikator == "Beschäftigte" & df$fachbereich == "MINT", "Beschäftigte in MINT", df$indikator)
  df$indikator <- ifelse(df$indikator == "Beschäftigte" & df$fachbereich == "Andere Berufe", "Beschäftigte in anderen Berufen", df$indikator)


  # plot
  highcharter::hchart(df, 'bar', highcharter::hcaes( x = indikator, y=round(proportion_fachbereich), group = geschlecht)) %>%
    highcharter::hc_tooltip(pointFormat = "{point.geschlecht}-Anteil: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  FALSE) %>%
    highcharter::hc_xAxis(title = list(text = ""), categories = c("Auszubildende in MINT", "Auszubildende in anderen Berufen",
                                                                  "Beschäftigte in MINT", "Beschäftigte in anderen Berufen")) %>%
    highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
    highcharter::hc_colors(c("#154194", "#efe8e6")) %>%
    highcharter::hc_title(text = paste0("Anteil von Frauen in MINT- und anderen Berufen (", timerange, ")",
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

  # ggplot2::ggplot(df, ggplot2::aes(x=indikator, y=proportion_fachbereich, fill = fachbereich)) +
  #   ggplot2::geom_bar(stat="identity", position = "dodge") +
  #   ggplot2::geom_text(ggplot2::aes(label=paste(round(proportion_fachbereich),"%"), vjust = - 0.25),
  #                      position=ggplot2::position_dodge(width=0.9),
  #                      fontface = "bold") +
  #   ggplot2::theme_minimal() +
  #   ggplot2::theme(
  #     text = ggplot2::element_text(size = 14),
  #     plot.title = ggtext::element_markdown(hjust = 0.5)) +
  #   ggplot2::xlab("") + ggplot2::ylab("") +
  #   ggplot2::scale_fill_manual(values = c("#efe8e6","#b16fab")) +
  #   ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
  #                                "Anteil von Frauen in MINT- und anderen Berufen (", timerange, ")",
  #                                "<br><br><br>"),
  #                 fill = "") +
  #   ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"))

}

#' A function to plot a single bundesland with landkreise
#'
#' @description A function to plot a map
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Arbeitsmarkt_detailliert.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_überblick_fächer <- function(df, r) {
  # load UI inputs from reactive value
  state <- r$state_arbeitsmarkt_überblick_fächer
  indikator_choice <- r$indikator_arbeitsmarkt_überblick_fächer

  # filtern nach Auswahl
  df <- df %>% dplyr::filter(bundesland == state)

  # Anforderung und Geschlecht auf gesamt setzten
  df <- df %>% dplyr::filter(anforderung == "Gesamt")
  df <- df %>% dplyr::filter(geschlecht == "Gesamt")

  # in neuen DF doch Aggregate enthalten, ausfiltern das Folgecode weiter stimmt
  df <- df %>% dplyr::filter(landkreis != "alle Landkreise")

  # Bundesland-Wert aus allen Landkreisen berechnen
  df <- df %>%
    dplyr::group_by(bundesland, jahr, indikator, fachbereich) %>%
    dplyr::summarize(wert = sum(wert))

  # # DE berechnen
  df_de <- df %>%
    dplyr::group_by(jahr, indikator, fachbereich) %>%
    dplyr::summarize(wert = sum(wert))
  df_de$bundesland <- "Deutschland"

  df <- rbind(df, df_de)

  df <- df %>% dplyr::filter(indikator == indikator_choice)

  # MINT direkt berechnen und nicht-MINT berechnen
  df[df$fachbereich == "MINT", "wert"] <- df[df$fachbereich == "Mathematik, Naturwissenschaften", "wert"]+
    df[df$fachbereich == "Informatik", "wert"]+df[df$fachbereich == "Technik (gesamt)", "wert"]
  df$fachbereich[df$fachbereich == "MINT"]<-"MINT-Berufsfelder (gesamt)"

# Berechnung von andere Fächergruppen
  # df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"]-
  #   df[df$fachbereich == "MINT-Berufsfelder (gesamt)"]
 df_andere <- df %>% dplyr::filter(fachbereich=="Alle")
 df_mint <- df %>% dplyr::filter(fachbereich=="MINT-Berufsfelder (gesamt)")
 df_andere$wert <- df_andere$wert - df_mint$wert
 df_andere$fachbereich[df_andere$fachbereich == "Alle"]<-"Alle Berufsfelder außer MINT (gesamt)"

 df <- rbind(df, df_andere)
 df <- df %>% dplyr::filter(fachbereich != "Alle")

  # Anteil Berechnen für aggregierte Werte MINT
  mint_agg <- df %>%
    dplyr::filter(fachbereich %in% c("MINT-Berufsfelder (gesamt)","Alle Berufsfelder außer MINT (gesamt)" )) %>%
    dplyr::mutate(prop = (wert/sum(wert))*100)
  mint_agg <-  mint_agg %>% dplyr::filter(fachbereich == "MINT-Berufsfelder (gesamt)")

  #Anteil Berechnen für Technik (gesamt)
  technik_agg <- df %>%
    dplyr::filter(fachbereich %in% c("Mathematik, Naturwissenschaften",
                              "Informatik", "Technik (gesamt)", "Alle Berufsfelder außer MINT (gesamt)" )) %>%
    dplyr::mutate(prop = (wert/sum(wert))*100)
 technik_agg <-  technik_agg %>% dplyr::filter(fachbereich == "Technik (gesamt)")

  #Anteil Berechnen für Technik-Gruppen
  df <- df %>%
    dplyr::filter(!(fachbereich %in% c("MINT-Berufsfelder (gesamt)", "Technik (gesamt)"))) %>%
    dplyr::mutate(prop = (wert/sum(wert))*100)

  #Alle Werte zusammenfügen
  df <- rbind(df, mint_agg, technik_agg)

  #für Überblick unterarten von Technik wieder raus
  df <- df %>% dplyr::filter(fachbereich %in% c("Alle Berufsfelder außer MINT (gesamt)",
                                                "MINT-Berufsfelder (gesamt)",
                                                "Mathematik, Naturwissenschaften",
                                                "Informatik",
                                                "Technik (gesamt)"))

  df$fachbereich[df$fachbereich == "Technik (gesamt)"]<-"Technik"

  # Reihenfolge sortieren für Plot
  df$fachbereich <- factor(df$fachbereich, levels = c("Alle Berufsfelder außer MINT (gesamt)",
                                                        "MINT-Berufsfelder (gesamt)",
                                                        "Mathematik, Naturwissenschaften",
                                                        "Informatik",
                                                        "Technik"))
 # df$fachbereich <- factor(df$fachbereich, levels = c("Alle Berufsfelder außer MINT (gesamt)",
 #                                                              "MINT-Berufsfelder (gesamt)",
 #                                                              "Mathematik, Naturwissenschaften",
 #                                                              "Informatik",
 #                                                              "Technik (gesamt)",
 #                                                              "Bau- und Gebäudetechnik",
 #                                                              "Gesundheitstechnik",
 #                                                              "Landtechnik",
 #                                                              "Produktionstechnik",
 #                                                              "Verkehrs-, Sicherheits- u. Veranstaltungstechnik"
 #   ))

  # titel-helper
  title_help <- paste0(indikator_choice, "n")
  title_help <- ifelse(grepl("ausl", indikator_choice), "ausländischen Beschäftigten", title_help)
  title_help <- ifelse(grepl("Jahr", indikator_choice), "Auszubildenden im 1. Jahr", title_help)
  title_help <- ifelse(grepl("u25", indikator_choice), "Beschäftigten unter 25 Jahren", title_help)
  title_help <- ifelse(grepl("25-55", indikator_choice), "Beschäftigten zwischen 25 und 55 Jahren", title_help)
  title_help <- ifelse(grepl("ü55", indikator_choice), "Beschäftigten über 55 Jahren", title_help)



  # plot
  highcharter::hchart(df, 'bar', highcharter::hcaes(y = round(prop), x = fachbereich)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil an allen Berufsfeldern: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = ""), categories =c("Alle Berufsfelder außer MINT (gesamt)",
                                                                 "MINT-Berufsfelder (gesamt)",
                                                                 "Mathematik, Naturwissenschaften",
                                                                 "Informatik",
                                                                 "Technik"
    )) %>%
    # highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
    # highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
    highcharter::hc_colors( "#b16fab") %>%
    highcharter::hc_title(text = paste0( "Überblick über die Berufsfelder von ", title_help,
                                         br(), "in ",state, " (2021)",
                                         "<br><br><br>"),
                          margin = 20,
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

arbeitsmarkt_lk_detail_map <- function(df,r) {

  # load UI inputs from reactive value
  states <- r$states_beruf_arbeitsmarkt_landkreis_karte

  # input values for first map
  category_1 <- r$kategorie_beruf_arbeitsmarkt_landkreis_karte1
  domain_1 <- r$fachbereich_beruf_arbeitsmarkt_landkreis_karte1
  indikator_azubi_1 <- r$indikator1_beruf_arbeitsmarkt_landkreis_karte1
  indikator_besch_1 <- r$indikator2_beruf_arbeitsmarkt_landkreis_karte1

  # input values for second map
  category_2 <- r$kategorie_beruf_arbeitsmarkt_landkreis_karte2
  domain_2 <- r$fachbereich_beruf_arbeitsmarkt_landkreis_karte2
  indikator_azubi_2 <- r$indikator1_beruf_arbeitsmarkt_landkreis_karte2
  indikator_besch_2 <- r$indikator2_beruf_arbeitsmarkt_landkreis_karte2

  # map states for state codes
  state_codes <- data.frame(
    state = c(
      "Baden-Württemberg",
      "Bayern",
      "Berlin",
      "Brandenburg",
      "Bremen",
      "Hamburg",
      "Hessen",
      "Mecklenburg-Vorpommern",
      "Niedersachsen",
      "Nordrhein-Westfalen",
      "Rheinland-Pfalz",
      "Saarland",
      "Sachsen",
      "Sachsen-Anhalt",
      "Schleswig-Holstein",
      "Thüringen"
    ),
    short = c(
      "bw",
      "by",
      "be",
      "bb",
      "hb",
      "hh",
      "he",
      "mv",
      "ni",
      "nw",
      "rp",
      "sl",
      "sn",
      "st",
      "sh",
      "th"
    )
  )

  state_code <- state_codes %>% dplyr::filter(state == states) %>% dplyr::pull()

  # calculate comparison map 1
  df1_list <- calculate_landkreis(df, states, category_1, domain_1, indikator_azubi_1, indikator_besch_1)

  df1_map <- df1_list[[1]]
  titel_gesamt1 <- df1_list[[2]]
  titel_sub1 <- df1_list[[3]]
  titel_sub1_2 <- df1_list[[4]]

  # calculate comparison map 2
  df2_list <- calculate_landkreis(df, states, category_2, domain_2, indikator_azubi_2, indikator_besch_2)

  df2_map <- df2_list[[1]]
  titel_gesamt2 <- df2_list[[2]]
  titel_sub2 <- df2_list[[3]]
  titel_sub2_2 <- df2_list[[4]]

  # adjust landkreis_nummer for correct mapping
  df1_map <- df1_map %>% dplyr::mutate(
    landkreis_nummer = paste0("de-", state_code, "-", landkreis_nummer, "000"))

  df2_map <- df2_map %>% dplyr::mutate(
    landkreis_nummer = paste0("de-", state_code, "-", landkreis_nummer, "000"))

  # create plots
  map1 <- highcharter::hcmap(
    paste0("countries/de/de-", state_code ,"-all"),
    data = df1_map,
    value = "prob",
    joinBy = c("hc-key","landkreis_nummer"),
    borderColor = "#FAFAFA",
    name = paste0("Anteil von ", titel_sub1_2, titel_gesamt1, titel_sub1_2, " in ", states, " (2021)"),
    borderWidth = 0.1,
    nullColor = "#A9A9A9",
    tooltip = list(
      valueDecimals = 0,
      valueSuffix = "%"
    )
  ) %>%
    highcharter::hc_colorAxis(min=0,labels = list(format = "{text}%")) %>%
    highcharter::hc_title(
      text = paste0("Anteil von ", titel_sub1_2, titel_gesamt1, titel_sub1_2, " in ", states, " (2021)"),
      margin = 10,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
    ) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular")
    ) %>% highcharter::hc_size(600, 550) %>%
    highcharter::hc_credits(enabled = FALSE) %>%
    highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                           verticalAlign = "bottom") %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/jpeg' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

  map2 <- highcharter::hcmap(
    paste0("countries/de/de-", state_code ,"-all"),
    data = df2_map,
    value = "prob",
    joinBy = c("hc-key", "landkreis_nummer"),
    borderColor = "#FAFAFA",
    name = paste0("Anteil von ", titel_sub2_2, titel_gesamt2, titel_sub2_2, " in ", states, " (2021)"),
    borderWidth = 0.1,
    nullColor = "#A9A9A9",
    tooltip = list(
      valueDecimals = 0,
      valueSuffix = "%"
    )
  ) %>%
    highcharter::hc_colorAxis(min=0,labels = list(format = "{text}%")) %>%
    highcharter::hc_title(
      text = paste0("Anteil von ", titel_sub2_2, titel_gesamt2, titel_sub2_2, " in ", states, " (2021)"),
      margin = 10,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
    ) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular")
    ) %>% highcharter::hc_size(600, 550) %>%
    highcharter::hc_credits(enabled = FALSE) %>%
    highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                           verticalAlign = "bottom") %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/jpeg' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

  highcharter::hw_grid(
    map1,
    map2,
    ncol = 2,
    browsable = TRUE
  )
}

#' A function to plot a bar chart
#'
#' @description A function to create a bar chart for detailed overview for landkreise
#'
#' @return The return value is a bar chart
#' @param df The dataframe "Arbeitsmarkt_detailliert.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_lk_detail_vergleich <- function(df, r) {

  # load UI inputs from reactive value
  states <- r$states_beruf_arbeitsmarkt_landkreis_vergleich

  # input values
  category <- r$kategorie_beruf_arbeitsmarkt_landkreis_vergleich
  domain <- r$fachbereich_beruf_arbeitsmarkt_landkreis_vergleich
  indikator_azubi <- r$indikator1_beruf_arbeitsmarkt_landkreis_vergleich
  indikator_besch <- r$indikator2_beruf_arbeitsmarkt_landkreis_vergleich
  display_form <- r$darstellung_beruf_arbeitsmarkt_landkreis_vergleich

  # calculate comparison
  df_compare_list <- calculate_landkreis(df, states, category, domain, indikator_azubi, indikator_besch)

  df_compare <- df_compare_list[[1]]
  titel_gesamt_1 <- df_compare_list[[2]]
  titel_sub <- df_compare_list[[3]]
  titel_sub2 <- df_compare_list[[4]]

    # titel_gesamt_2 <- df_compare_list[[3]]
  # titel_sub <- df_compare_list[[4]]

  # differentiate between relative and absolute
  if(display_form == "Relativ") {
    df_compare <- df_compare %>%
      dplyr::mutate(display_value = prob) %>%
      dplyr::arrange(display_value)

    legende <- paste0("{point.landkreis} <br> Anteil: {point.y} %")
    yAxis <- "{value}%"
    titel <- paste0("Anteil von ", titel_sub2, titel_gesamt_1, titel_sub2, " in ", states, " (2021)")

  } else {
    df_compare <- df_compare %>%
      dplyr::mutate(display_value = wert) %>%
      dplyr::arrange(display_value)

    legende <- paste0("{point.landkreis} <br> Wert: {point.y}")
    yAxis <- "{value}"
    titel_gesamt_1 <- stringr::str_remove(titel_gesamt_1, "an allen")
    titel <- paste0("Anzahl ", titel_sub, titel_gesamt_1, " in ", states, " (2021)")
  }

  #Vector für angepasste Größe des Plots
  länder <-   c("Baden-Württemberg",
      "Bayern",
      "Berlin",
      "Brandenburg",
      "Bremen",
      "Hamburg",
      "Hessen",
      "Mecklenburg-Vorpommern",
      "Niedersachsen",
      "Nordrhein-Westfalen",
      "Rheinland-Pfalz",
      "Saarland",
      "Sachsen",
      "Sachsen-Anhalt",
      "Schleswig-Holstein",
      "Thüringen")
  höhe <- c(10, 20, 3, 6, 3, 3, 8, 5, 11, 11, 10, 4, 6, 6, 6, 7)
  plt.add <- data.frame(länder, höhe)

  plt.add$subtitle <- "Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen."

  # create plot
  highcharter::hchart(df_compare, 'bar', highcharter::hcaes(y = display_value, x = landkreis)) %>%
    highcharter::hc_tooltip(pointFormat = legende) %>%
    highcharter::hc_yAxis(title = list(text = paste0(br(), br(),"Quelle der Daten: Bundesagentur für Arbeit, 2021, auf Anfrage, eigene Berechnungen.") , align="left"), labels = list(format = yAxis)) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_colors("#154194") %>%
    highcharter::hc_size(height = 80*plt.add$höhe[plt.add$länder == states]) %>%
    highcharter::hc_title(text = paste0(titel, "<br><br><br>"),
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
                                                              this.exportChart({ type: 'image/jpeg' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

}


#' A function to plot a table
#'
#' @description A function to create a table for detailed overview for landkreise
#'
#' @return The return value is a table
#' @param df The dataframe "Arbeitsmarkt_detailliert.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

arbeitsmarkt_lk_detail_table <- function(df, input_values, r) {

  # get input variables
  input_count <- stringr::str_sub(names(input_values), 1, 4) %>% unique()
  variable_counts <- input_count[input_count %>% stringr::str_detect("var")] %>% sort()

  state1 <- r[["mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_1-states_beruf_arbeitsmarkt_landkreis_table"]]
  state2 <- r[["mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_2-states_beruf_arbeitsmarkt_landkreis_table"]]
  state3 <- r[["mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_3-states_beruf_arbeitsmarkt_landkreis_table"]]

  region1 <- r[["mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_1-region_beruf_arbeitsmarkt_landkreis_table"]]
  region2 <- r[["mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_2-region_beruf_arbeitsmarkt_landkreis_table"]]
  region3 <- r[["mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_3-region_beruf_arbeitsmarkt_landkreis_table"]]


  # create empty dataframe
  df_steckbrief <- data.frame()

  # for each 'Betrachtung' = variable_counts, get detailed input, calculate
  # values and build display dataframe
  for(i in variable_counts){

    category <- input_values[[paste0(i, "-kategorie_beruf_arbeitsmarkt_landkreis_vergleich")]]
    domain <- input_values[[paste0(i, "-fachbereich_beruf_arbeitsmarkt_landkreis_vergleich")]]
    indikator_azubi <- input_values[[paste0(i, "-indikator1_beruf_arbeitsmarkt_landkreis_vergleich")]]
    indikator_besch <- input_values[[paste0(i, "-indikator2_beruf_arbeitsmarkt_landkreis_vergleich")]]

    df_compare_list_region1 <- calculate_landkreis(df, state1, category, domain, indikator_azubi, indikator_besch)
    df_compare_list_region1[[1]] <- df_compare_list_region1[[1]] %>% dplyr::filter(landkreis == region1)

    df_compare_list_region2 <- calculate_landkreis(df, state2, category, domain, indikator_azubi, indikator_besch)
    df_compare_list_region2[[1]] <- df_compare_list_region2[[1]] %>% dplyr::filter(landkreis == region2)

    df_compare_list_region3 <- calculate_landkreis(df, state3, category, domain, indikator_azubi, indikator_besch)
    df_compare_list_region3[[1]] <- df_compare_list_region3[[1]] %>% dplyr::filter(landkreis == region3)

    line_name <- paste(category, domain, ifelse(category=="Auszubildende", indikator_azubi, indikator_besch), sep = "-")
    df_var <- data.frame(line_name = line_name,
                         region1 = paste0(df_compare_list_region1[[1]]$wert, "<br/>(", df_compare_list_region1[[1]]$prob, "% ", df_compare_list_region1[[3]], " an ", df_compare_list_region1[[2]], ")"),
                         region2 = paste0(df_compare_list_region2[[1]]$wert, "<br/>(", df_compare_list_region2[[1]]$prob, "% ", df_compare_list_region2[[3]], " an ", df_compare_list_region2[[2]], ")"),
                         region3 = paste0(df_compare_list_region3[[1]]$wert, "<br/>(", df_compare_list_region3[[1]]$prob, "% ", df_compare_list_region3[[3]], " an ", df_compare_list_region3[[2]], ")"))


    df_steckbrief <- dplyr::bind_rows(df_steckbrief, df_var)

  }

  # adjust names for the dataframe
  names(df_steckbrief) <- c("", paste0("<b>", region1, "</b>"), paste0("<b>", region2, "</b>"), paste0("<b>", region3, "</b>"))

  return(df_steckbrief)

}
