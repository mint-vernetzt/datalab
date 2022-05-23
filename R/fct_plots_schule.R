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

  df <- df[with(df, order(anzeige_geschlecht, jahr, decreasing = FALSE)), ]

  if(isTRUE(switch_absolut)){

    highchart_obj(df, geschlecht, type = "normal", andere_name = "andere Fächer", lehramt = "Nein") %>%
      highcharter::hc_title(text = paste0("Absoluter Anteil an MINT und allen anderen Schulfächern ", title_help),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px"))



  }else{

    p <- highchart_obj(df, geschlecht, type = "percent", andere_name = "andere Fächer", lehramt = "Nein")

    p %>% highcharter::hc_yAxis(labels = list(format = "{value}%")) %>%
      highcharter::hc_tooltip(pointFormat = "{series.name} <br> Anteil: {point.percentage:.0f}%") %>%
      highcharter::hc_title(text = paste0("Relativer Anteil an MINT und allen anderen Schulfächern ", title_help),
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

kurse_einstieg_pie <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_kurse_einstieg

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)


  df <- df %>% dplyr::filter(region == "Deutschland")

  # combine subjects to get numbers on share of MINT
  # make a function out of it
  subjects <- c("Mathematik", "Informatik", "Naturwissenschaften",  "Biologie", "Chemie",
                "Physik", "andere naturwiss.-technische Fächer",  "Erdkunde", "Alle Fächer")

  df <- df %>% dplyr::filter(fachbereich %in% subjects)


  df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Leistungskurse"), "wert"] <-  df %>%
    dplyr::filter(indikator == "Leistungskurse") %>%
    dplyr::group_by(indikator, jahr) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"] +
                       wert[anzeige_geschlecht == "Männer"]) %>% dplyr::pull(wert)


  df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Grundkurse"), "wert"] <-  df %>%
    dplyr::filter(indikator == "Grundkurse") %>%
    dplyr::group_by(indikator, jahr) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"] +
                       wert[anzeige_geschlecht == "Männer"]) %>% dplyr::pull(wert)



  df$fachbereich <- ifelse(df$fachbereich != "Alle Fächer", "MINT", "andere Fächer")


  df <- df %>% dplyr::group_by(indikator, fachbereich, anzeige_geschlecht, jahr) %>%
    dplyr::summarize(wert = sum(wert))


  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "andere Fächer", "wert"] <- df[df$fachbereich == "andere Fächer", "wert"] -
    df[df$fachbereich == "MINT", "wert"]


  df_gk <- df %>% dplyr::filter(indikator == "Grundkurse")

  df_gk <- df_gk %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

  # calculate proportions
  df_gk$props <- sum(df_gk$wert)

  df_gk <- df_gk %>% dplyr::group_by(fachbereich, anzeige_geschlecht) %>%
    dplyr::summarize(proportion = wert/props)

  df_gk$proportion <- df_gk$proportion * 100

  df_gk$proportion <- round_preserve_sum(as.numeric(df_gk$proportion),0)

  df_gk$anzeige_geschlecht <- paste0(df_gk$anzeige_geschlecht, " (", df_gk$fachbereich, ")")


  df_lk <- df %>% dplyr::filter(indikator == "Leistungskurse")

  df_lk <- df_lk %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

  # calculate proportions
  df_lk$props <- sum(df_lk$wert)

  df_lk <- df_lk %>% dplyr::group_by(fachbereich, anzeige_geschlecht) %>%
    dplyr::summarize(proportion = wert/props)

  df_lk$proportion <- df_lk$proportion * 100

  df_lk$proportion <- round_preserve_sum(as.numeric(df_lk$proportion),0)

  df_lk$anzeige_geschlecht <- paste0(df_lk$anzeige_geschlecht, " (", df_lk$fachbereich, ")")


  highcharter::hw_grid(
    highcharter::hchart(df_gk, size = 280, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = proportion)) %>%
      highcharter::hc_tooltip(
        pointFormat=paste('Anteil: {point.percentage:.1f}%')) %>%
      highcharter::hc_colors(c("#154194", 'rgba(21, 65, 148, 0.50)',"#b16fab", 'rgba(177, 111, 171, 0.50)')) %>%
      highcharter::hc_title(text = paste0("Anteil von MINT an allen anderen Fächern für Grundkurse in ", timerange),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
      highcharter::hc_legend(enabled = TRUE) %>%
      highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                             dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),


  highcharter::hchart(df_lk, size = 280, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = proportion)) %>%
    highcharter::hc_tooltip(
      pointFormat=paste('Anteil: {point.percentage:.1f}%')) %>%
    highcharter::hc_colors(c("#154194", 'rgba(21, 65, 148, 0.50)',"#b16fab", 'rgba(177, 111, 171, 0.50)')) %>%
    highcharter::hc_title(text = paste0("Anteil von MINT an allen anderen Fächern für Leistungskurse in ", timerange),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
    highcharter::hc_legend(enabled = TRUE) %>%
    highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                           dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE)),

  ncol = 2,
  browsable = TRUE
  )


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

  # load UI inputs from reactive value
  timerange <- r$date_kurse_einstieg

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")

  # combine subjects to get numbers on share of MINT
  # make a function out of it
  subjects <- c("Mathematik", "Informatik", "Naturwissenschaften",  "Biologie", "Chemie",
                "Physik", "andere naturwiss.-technische Fächer",  "Erdkunde", "Alle Fächer")

  df <- df %>% dplyr::filter(fachbereich %in% subjects)


  df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Leistungskurse"), "wert"] <-  df %>%
    dplyr::filter(indikator == "Leistungskurse") %>%
    dplyr::group_by(indikator, jahr) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"] +
                       wert[anzeige_geschlecht == "Männer"]) %>% dplyr::pull(wert)


  df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Grundkurse"), "wert"] <-  df %>%
    dplyr::filter(indikator == "Grundkurse") %>%
    dplyr::group_by(indikator, jahr) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"] +
                       wert[anzeige_geschlecht == "Männer"]) %>% dplyr::pull(wert)



  df$fachbereich <- ifelse(df$fachbereich != "Alle Fächer", "MINT", "andere Fächer")


  df <- df %>% dplyr::group_by(indikator, fachbereich, anzeige_geschlecht, jahr) %>%
    dplyr::summarize(wert = sum(wert))


  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "andere Fächer", "wert"] <- df[df$fachbereich == "andere Fächer", "wert"] -
    df[df$fachbereich == "MINT", "wert"]

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

  colnames(df) <- c("Indikator", "Fachbereich" ,"Geschlecht", "Jahr", "Wert")

  return(df)

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

kurse_einstieg_verlauf <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_kurse_einstieg_2

  fachbereich_choice <- r$fachbereich_kurse_einstieg

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(region == "Deutschland")

  # combine subjects to get numbers on share of MINT
  # make a function out of it
  subjects <- c("Mathematik", "Informatik", "Naturwissenschaften",  "Biologie", "Chemie",
                "Physik", "andere naturwiss.-technische Fächer",  "Erdkunde", "Alle Fächer")

  df <- df %>% dplyr::filter(fachbereich %in% subjects)


  df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Leistungskurse"), "wert"] <-  df %>%
    dplyr::filter(indikator == "Leistungskurse") %>%
    dplyr::group_by(indikator, jahr) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"] +
                       wert[anzeige_geschlecht == "Männer"]) %>% dplyr::pull(wert)


  df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Grundkurse"), "wert"] <-  df %>%
    dplyr::filter(indikator == "Grundkurse") %>%
    dplyr::group_by(indikator, jahr) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"] +
                       wert[anzeige_geschlecht == "Männer"]) %>% dplyr::pull(wert)



  df$fachbereich <- ifelse(df$fachbereich != "Alle Fächer", "MINT", "andere Fächer")


  df <- df %>% dplyr::group_by(indikator, fachbereich, anzeige_geschlecht, jahr) %>%
    dplyr::summarize(wert = sum(wert))


  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "andere Fächer", "wert"] <- df[df$fachbereich == "andere Fächer", "wert"] -
    df[df$fachbereich == "MINT", "wert"]


  df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(fachbereich, jahr, indikator) %>%
    dplyr::mutate(props = sum(wert))

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")

  df <- df %>% dplyr::group_by(fachbereich, jahr, indikator, anzeige_geschlecht) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- round(df$proportion * 100)

  df <- df %>% dplyr::filter(fachbereich == fachbereich_choice)

  # order years for plot
  df <- df[with(df, order(indikator, jahr, decreasing = FALSE)), ]

  if (fachbereich_choice == "MINT"){

    title_help <- "MINT"

  }else {

    title_help <- "allen anderen Fächern"

  }
  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = proportion, group = indikator)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil Frauen: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Anteil"), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), style = list(fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil von Frauen an ", title_help ," im Verlauf"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    )



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

  timerange <- r$date_kurse

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")


  # combine subjects to get numbers on share of MINT
  # make a function out of it
  subjects <- c("Mathematik", "Informatik", "Naturwissenschaften",  "Biologie", "Chemie",
                "Physik", "andere naturwiss.-technische Fächer",  "Erdkunde", "Alle Fächer")

  df <- df %>% dplyr::filter(fachbereich %in% subjects)

  df <- df %>% dplyr::filter(fachbereich == "Mathematik" | fachbereich == "Informatik" |
                               fachbereich == "Naturwissenschaften" | fachbereich == "Alle Fächer")



  x_gk <- prep_kurse_proportion(df,"Grundkurse")

  x_lk <- prep_kurse_proportion(df,"Leistungskurse")

  x_gk <- x_gk[order(factor(names(x_gk), levels = c('Frauen (Informatik)', 'Frauen (Mathematik)',
                                            'Frauen (Naturwissenschaften)', 'Männer (Informatik)',
                                            'Männer (Mathematik)', 'Männer (Naturwissenschaften)')))]

  x_lk <- x_lk[order(factor(names(x_lk), levels = c('Frauen (Informatik)', 'Frauen (Mathematik)',
                                            'Frauen (Naturwissenschaften)', 'Männer (Informatik)',
                                            'Männer (Mathematik)', 'Männer (Naturwissenschaften)')))]


  # create plot objects for waffle charts
  waffle_gk <- waffle::waffle(x_gk, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", "**Grundkurse**</span>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "left") +
    ggplot2::scale_fill_manual(
                                values =  c("#ee7775",
                                            "#fcc433",
                                            "#00a87a",
                                            "#b1b5c3",
                                            "#b1b5c3",
                                            '#b1b5c3'),
                                limits = c("Frauen (Informatik)", "Frauen (Mathematik)",
                                           "Frauen (Naturwissenschaften)"),
                      guide = ggplot2::guide_legend(reverse = TRUE),
                      labels = c(
                                 paste0("Frauen (Informatik)",", ",x_gk[1], "%"),
                                 paste0("Frauen (Mathematik)",", ",x_gk[2], "%"),
                                 paste0("Frauen (Natwi)",", ",x_gk[3], "%"))) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=4,byrow=TRUE))



  waffle_lk <- waffle::waffle(x_lk, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", "**Leistungskurse**</span>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "right")


    if (x_lk[[1]] == 0) {

      waffle_lk <-  waffle_lk +
        ggplot2::scale_fill_manual(
          values =  c(#"#ee7775",
                      "#fcc433",
                      "#00a87a",
                      "#b1b5c3",
                      "#b1b5c3",
                      '#b1b5c3'),
          limits = c("Frauen (Mathematik)",
                     "Frauen (Naturwissenschaften)"),
          guide = ggplot2::guide_legend(reverse = TRUE),
          labels = c(
            #paste0("Frauen (Informatik)",", ",x_gk[1], "%"),
            paste0("Frauen (Mathematik)",", ",x_gk[2], "%"),
            paste0("Frauen (Natwi)",", ",x_gk[3], "%"))) +
        ggplot2::guides(fill=ggplot2::guide_legend(nrow=4,byrow=TRUE))



    } else{

      waffle_lk <- waffle_lk +
        ggplot2::scale_fill_manual(
          values =  c("#ee7775",
                      "#fcc433",
                      "#00a87a",
                      "#b1b5c3",
                      "#b1b5c3",
                      '#b1b5c3'),
          limits = c("Frauen (Informatik)", "Frauen (Mathematik)",
                     "Frauen (Naturwissenschaften)"),
          guide = ggplot2::guide_legend(reverse = TRUE),
          labels = c(
            paste0("Frauen (Informatik)",", ",x_gk[1], "%"),
            paste0("Frauen (Mathematik)",", ",x_gk[2], "%"),
            paste0("Frauen (Natwi)",", ",x_gk[3], "%"))) +
        ggplot2::guides(fill=ggplot2::guide_legend(nrow=4,byrow=TRUE))



    }



  plot <- ggpubr::ggarrange(waffle_gk, NULL ,waffle_lk, widths = c(1, 0.1, 1), nrow=1)
  text <- c(
    paste0("<span style='font-size:20.5pt; color:black'> Anteil von Schüler und Schülerinnen an MINT",
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

  df <- df %>% dplyr::filter(region == "Deutschland")


  # combine subjects to get numbers on share of MINT
  # make a function out of it
  subjects <- c("Mathematik", "Informatik", "Naturwissenschaften",  "Biologie", "Chemie",
                "Physik", "andere naturwiss.-technische Fächer",  "Erdkunde", "Alle Fächer")

  df <- df %>% dplyr::filter(fachbereich %in% subjects)

  df <- df %>% dplyr::filter(fachbereich == "Mathematik" | fachbereich == "Informatik" |
                               fachbereich == "Naturwissenschaften" | fachbereich == "Alle Fächer")


  df <- df[with(df, order(indikator, decreasing = TRUE)), ]


  df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

  df <- df %>% dplyr::filter(fachbereich != "Alle Fächer")

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
    ggplot2::scale_fill_manual(values = c("#ee7775", "#fcc433", "#00a87a")) +
    ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
                                 "Schüler und Schülerinnen in MINT und allen anderen Fächern" ," in ", timerange,
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
  level_kurs <- r$indikator_kurse_ranking

  timerange <- r$date_kurse_ranking

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(indikator == level_kurs)

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- df %>% dplyr::filter(fachbereich != "Alle Fächer")

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(fachbereich) %>%
    dplyr::mutate(props = sum(wert))

  df <- df %>% dplyr::group_by(fachbereich, anzeige_geschlecht) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100

  # plot
  top_five <- df %>%
    dplyr::arrange(desc(proportion)) %>%
    dplyr::group_by(anzeige_geschlecht) %>%
    dplyr::slice(1:5) %>% dplyr::mutate(label_rank = "beliebt")

  low_five <- df %>%
    dplyr::arrange((proportion)) %>%
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
    ggplot2::ggplot(ggplot2::aes(y=fachbereich, x=proportion, label = paste0(round(proportion),"%"), color = label_rank)) +
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = reorder(fachbereich, proportion), xend = proportion, yend = fachbereich)) +
      ggplot2::geom_point(size = 2) +
      ggplot2::geom_text(nudge_x = 7, color = "black") +
      #ggplot2::geom_bar(stat="identity") +
      #ggplot2::coord_flip() +
      ggplot2::facet_grid(label_rank~., scales = "free", space = "free", switch = "both") +
      ggplot2::theme_bw() +
      ggplot2::scale_color_manual(values = c("#00a87a", "#ee7775")) +
      ggplot2::theme(legend.position = "none",
                     strip.placement = "outside",
                     #plot.title = ggplot2::element_text(family = 'SourceSans3-Regular'),
                    # axis.text.y = ggplot2::element_text(family = 'SourceSans3-Regular'),
                     plot.margin = ggplot2::unit(c(2.5,0,0,0), "lines"),
                     strip.text.y = ggplot2::element_text(size = 14, face = "bold"),
                     panel.grid.major.y = ggplot2::element_blank(),
                     panel.background = ggplot2::element_rect(fill="white"),
                     strip.background = ggplot2::element_blank()) +
     ggplot2::labs(title = "Frauen", x = "", y = "") +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100))


    plot_2 <- df_ranked %>% dplyr::filter(anzeige_geschlecht == "Männer") %>%
      ggplot2::ggplot(ggplot2::aes(y=fachbereich, x=proportion, label = paste0(round(proportion),"%"), color = label_rank)) +
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = reorder(fachbereich, proportion), xend = proportion, yend = fachbereich)) +
      ggplot2::geom_point(size = 2) +
      ggplot2::geom_text(nudge_x = 7, color = "black") +
      #ggplot2::geom_bar(stat="identity") +
      #ggplot2::coord_flip() +
      ggplot2::facet_grid(label_rank~., scales = "free", space = "free", switch = "both") +
      ggplot2::theme_bw() +
      ggplot2::scale_color_manual(values = c("#00a87a", "#ee7775")) +
      ggplot2::theme(legend.position = "none",
                     strip.placement = "outside",
                     #plot.title = ggplot2::element_text(family = 'SourceSans3-Regular'),
                     #axis.text.y = ggplot2::element_text(family = 'SourceSans3-Regular'),
                     plot.margin = ggplot2::unit(c(2.5,0,0,0), "lines"),
                     strip.text.y = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_blank(),
                     panel.background = ggplot2::element_rect(fill="white"),
                     strip.background = ggplot2::element_blank()) +
      ggplot2::labs(title = "Männer", x = "", y = "") +
      ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100))



    text <- c(
      paste0("<span style='font-size:20.5pt; color:black'> Die 5 ", title_help, " mit dem größten und kleinsten Anteil der Schüler*innen",
             " in ",timerange))


    plot <- ggpubr::ggarrange(plot_1, NULL, plot_2, widths = c(1, 0.2, 1), nrow=1)

    ggpubr::annotate_figure(plot, gridtext::richtext_grob(text = text))

  }else{

    df <- tidyr::spread(df, anzeige_geschlecht, proportion)

    df2 <- tidyr::gather(df, group, value, -fachbereich)

    df$fachbereich <- reorder(df$fachbereich, df$Frauen)

    df2$fachbereich <- factor(df2$fachbereich, levels = levels(df$fachbereich))


    ggplot2::ggplot(df,
           ggplot2::aes(y = fachbereich)) +
      ggplot2::geom_point(data = df2, ggplot2::aes(x = value, color = group), size = 5) +
      ggalt::geom_dumbbell(
                    ggplot2::aes(x = Frauen, xend = Männer),
                    size = 0.5,
                    size_x = 5,
                    size_xend = 5,
                    colour = "black",
                    colour_x = "#f5adac66",
                    colour_xend = "#b1b5c366",
                    dot_guide=TRUE) +
      ggplot2::theme_minimal() +
      ggplot2::scale_color_manual(name = "", values = c("#f5adac66", "#b1b5c366")) +
      ggplot2::theme(legend.position="top",
                     #legend.text= ggplot2::element_text(family = 'SourceSans3-Regular'),
                     panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
                     plot.title = ggtext::element_markdown(hjust = 0.5),
                     axis.text.y = ggplot2::element_text(size = 11)) +
      ggplot2::ylab("") + ggplot2::xlab("") +
      ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
                                   "Relativer Anteil von Schüler*innen aller ", title_help, " in ",timerange,
                                   "<br><br><br>"),
                    color = "") +
      ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))



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
  timerange <- r$date_kurse

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(region != "Bayern")

  # combine subjects to get numbers on share of MINT
  # make a function out of it
  subjects <- c("Mathematik", "Informatik", "Naturwissenschaften",  "Biologie", "Chemie",
                "Physik", "andere naturwiss.-technische Fächer",  "Erdkunde", "Alle Fächer")

  df <- df %>% dplyr::filter(fachbereich %in% subjects)

  df$fachbereich <- ifelse(df$fachbereich != "Alle Fächer", "MINT", "andere Fächer")

  df <- df %>% dplyr::group_by(region, indikator, fachbereich, anzeige_geschlecht) %>%
    dplyr::summarize(wert = sum(wert, na.rm = T))


  # calculate proportions
  # remove "Männer" + "Frauen" does not add up to "Gesamt"
  # calculate new "Gesamt" based on "Männer" and "Frauen"
  df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator) %>%
    dplyr::mutate(props = sum(wert))

  df <- df %>% dplyr::filter(fachbereich != "andere Fächer")

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")

  df <- df %>% dplyr::group_by(region, indikator) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100

  highcharter::hw_grid(
  # plot
  highcharter::hcmap(
    "countries/de/de-all",
    data = df[df$indikator == "Grundkurse",],
    value = "proportion",
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
      text = paste0("Anteil der Schülerinnen an MINT für die <br>", "Grundkurse" ," in ", timerange),
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
    data = df[df$indikator == "Leistungskurse",],
    value = "proportion",
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
      text = paste0("Anteil der Schülerinnen an MINT für die <br>", "Leistungskurse" ," in ", timerange),
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
#' @description A function to similar to 'kurse_einstieg_bar' but with the
#' difference that it returns a dataframe instead of plot.
#'
#' @return The return value is a dataframe
#' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

data_mix_kurse <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_kurse

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  # remove
  df <- df %>% dplyr::filter(region != "Bayern")

  # combine subjects to get numbers on share of MINT
  # make a function out of it
  subjects <- c("Mathematik", "Informatik", "Naturwissenschaften",  "Biologie", "Chemie",
                "Physik", "andere naturwiss.-technische Fächer",  "Erdkunde", "Alle Fächer")

  df <- df %>% dplyr::filter(fachbereich %in% subjects)

  df$fachbereich <- ifelse(df$fachbereich != "Alle Fächer", "MINT", "andere Fächer")

  df <- df %>% dplyr::group_by(region, fachbereich, anzeige_geschlecht, jahr, indikator, bereich) %>%
    dplyr::summarize(wert = sum(wert, na.rm = T))


  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "andere Fächer", "wert"] <- df[df$fachbereich == "andere Fächer", "wert"] -
    df[df$fachbereich == "MINT", "wert"]



  colnames(df) <- c("Region", "Fachbereich", "Geschlecht", "Jahr", "Indikator", "Bereich", "Wert")

  return(df)

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

  subject_aggregated <- r$subjects_aggregated

  subjects_select <- r$subject_selected

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == level_kurs)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(region != "Bayern")

  if (level_kurs == "Grundkurse"){

    title_help_sub <- " für die Grundkurse"

  }else{

    title_help_sub <- " für die Leistungskurse"

  }

  df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  df <- prep_kurse_east_west(df)


  if (subject_aggregated == "aggregiert"){

  # combine subjects to get numbers on share of MINT
  # make a function out of it
  subjects <- c("Mathematik", "Informatik", "Naturwissenschaften",  "Biologie", "Chemie",
                "Physik", "andere naturwiss.-technische Fächer",  "Erdkunde", "Alle Fächer")

  df <- df %>% dplyr::filter(fachbereich %in% subjects)

  df$fachbereich <- ifelse(df$fachbereich != "Alle Fächer", "MINT", "andere Fächer")

  df <- df %>% dplyr::group_by(region,indikator, fachbereich, anzeige_geschlecht, jahr) %>%
    dplyr::summarize(wert = sum(wert, na.rm = T))


  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "andere Fächer", "wert"] <- df[df$fachbereich == "andere Fächer", "wert"] -
    df[df$fachbereich == "MINT", "wert"]


  df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])


  # filter MINT or remaining subjects
  df <- df %>% dplyr::filter(fachbereich == topic)


  if (topic == "MINT"){

    title_help <- paste0("MINT", title_help_sub)

  }else {

    title_help <- paste0("anderen Fächer", title_help_sub)

  }

  }else {

    df <- df %>% dplyr::filter(fachbereich %in% subjects_select)

    title_help <- paste0(subjects_select, title_help_sub)
  }

  df <- df %>% dplyr::group_by(region, fachbereich, anzeige_geschlecht, jahr) %>%
    dplyr::summarise(proportion = wert/props)

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")

  df$proportion <- df$proportion * 100

  # order
  df <- df[with(df, order(region, fachbereich, jahr, decreasing = TRUE)), ]


  df <- df %>% dplyr::filter(region %in% states)


  # order years for plot
  df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = region)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil Schülerinnen <br> Bundesland: {point.region} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil von Schülerinnen an ", title_help ," im Verlauf"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    )


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

data_verlauf_kurse <- function(df,r) {

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

    df <- df %>% dplyr::group_by(region, fachbereich, anzeige_geschlecht, jahr, indikator, bereich) %>%
      dplyr::summarize(wert = sum(wert, na.rm = T))


    # call function to calculate the share of MINT and the remaining subjects
    df[df$fachbereich == "andere Fächer", "wert"] <- df[df$fachbereich == "andere Fächer", "wert"] -
      df[df$fachbereich == "MINT", "wert"]

    # filter MINT or remaining subjects
    df <- df %>% dplyr::filter(fachbereich == topic)


  }else {

    df <- df %>% dplyr::filter(fachbereich %in% subjects_select)

    title_help <- subjects_select
  }

  # calculate proportions
  # remove "Männer" + "Frauen" does not add up to "Gesamt"
  # calculate new "Gesamt" based on "Männer" and "Frauen"
  df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, jahr, indikator, bereich) %>%
    dplyr::mutate(props = sum(wert))

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")

  df <- df %>% dplyr::group_by(region, fachbereich, jahr, indikator, bereich, anzeige_geschlecht) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- round(df$proportion * 100)


  df$proportion <- paste0(df$proportion , "%")

  # order
  df <- df[with(df, order(region, fachbereich, jahr, decreasing = TRUE)), ]


  if (ost_west == FALSE) {

    df <- df %>% dplyr::filter(region %in% states)

  } else{

    df$dummy_west <- ifelse(df$region %in% states_east_west$west, "Westen", "Osten")

    df <- df %>% dplyr::group_by(dummy_west, fachbereich, jahr, indikator, bereich, anzeige_geschlecht) %>%
      dplyr::summarise(proportion = mean(proportion))

    names(df)[1] <- "region"
  }


  colnames(df) <- c("Region", "Fachbereich", "Jahr", "Indikator", "Bereich", "Geschlecht", "Wert")

  return(df)

}



#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

kurse_verlauf_single_bl <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_kurse_verlauf_bl

  states <- r$states_kurse_verlauf_bl

  topic <- r$topic_kurse_verlauf_bl

  subject_aggregated <- r$subjects_aggregated_bl

  subjects_select <- r$subject_selected_bl

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(region != "Bayern")


  df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                       wert[anzeige_geschlecht == "Männer"])



  df <- prep_kurse_east_west(df)


  if (subject_aggregated == "aggregiert"){

    # combine subjects to get numbers on share of MINT
    # make a function out of it
    subjects <- c("Mathematik", "Informatik", "Naturwissenschaften",  "Biologie", "Chemie",
                  "Physik", "andere naturwiss.-technische Fächer",  "Erdkunde", "Alle Fächer")

    df <- df %>% dplyr::filter(fachbereich %in% subjects)


    df$fachbereich <- ifelse(df$fachbereich != "Alle Fächer", "MINT", "andere Fächer")

    df <- df %>% dplyr::group_by(region,indikator, fachbereich, anzeige_geschlecht, jahr) %>%
      dplyr::summarize(wert = sum(wert, na.rm = T))

    # call function to calculate the share of MINT and the remaining subjects
    df[df$fachbereich == "andere Fächer", "wert"] <- df[df$fachbereich == "andere Fächer", "wert"] -
      df[df$fachbereich == "MINT", "wert"]

    df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
      dplyr::group_by(region, fachbereich, indikator, jahr) %>%
      dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                      wert[anzeige_geschlecht == "Männer"])


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




  df <- df %>% dplyr::group_by(region, indikator, fachbereich, anzeige_geschlecht, jahr) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100



  df <- df %>% dplyr::filter(region %in% states)


  df$anzeige_geschlecht <- paste0(df$anzeige_geschlecht, " (", df$indikator, ")")



  # order years for plot
  df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = anzeige_geschlecht)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil {point.anzeige_geschlecht} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil von Schüler*innen an ", title_help ," im Verlauf"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    )


}


