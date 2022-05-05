#' A function to plot a graph.
#'
#' @description A function to create a stacked bar chart for the first box
#' inside the tab "Home".
#'
#' @return The return value is a plot
#' @param df The dataframe "zentral.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
home_einsitieg_waffle <- function(df,r, order) {

  # load UI inputs from reactive value
  timerange <- r$date_home_einstieg

  indikator_choice_1 <- r$indikator_start_einstieg_1

  indikator_choice_2 <- r$indikator_start_einstieg_2

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")


  # aggeregate every "bereich to Rest vs MINT
  df <- share_MINT(df)


  if (order == "first") {

    df <- df %>% dplyr::filter(indikator == indikator_choice_1)

  } else {

    df <- df %>% dplyr::filter(indikator == indikator_choice_2)

  }

  if (order == "first") {

    if ((indikator_choice_1 == "Auszubildende" | indikator_choice_1 == "Beschäftigte")) {

      title_help_sub <- "andere Berufszweige"

      title_help <- paste0("anderen Berufszweigen bei<br> ", indikator_choice_1,"n")

    } else {

      title_help_sub <- "andere Fächer"

      if ((indikator_choice_1 == "Habilitationen" | indikator_choice_1 == "Promotionen (angestrebt)")){

        title_help <- paste0("anderen Fächern bei<br> ", indikator_choice_1)

      }else{

        title_help <- paste0("anderen Fächern bei<br> ", indikator_choice_1,"n")
      }
    }

  } else {

    if ((indikator_choice_2 == "Auszubildende" | indikator_choice_2 == "Beschäftigte")) {

      title_help_sub <- "andere Berufszweige"

      title_help <- paste0("anderen Berufszweigen bei<br> ", indikator_choice_2,"n")

    } else {

      title_help_sub <- "andere Fächer"

      if ((indikator_choice_2 == "Habilitationen" | indikator_choice_2 == "Promotionen (angestrebt)")){

        title_help <- paste0("anderen Fächern bei<br> ", indikator_choice_2)

      }else{

        title_help <- paste0("anderen Fächern bei<br> ", indikator_choice_2,"n")
      }
    }
  }

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

  # calculate proportions
  df <- df %>% dplyr::mutate(props = sum(wert))


  df <- df %>% dplyr::group_by(fachbereich) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100



  x_mint_rest <- setNames(round_preserve_sum(as.numeric(df$proportion),0),
                     df$fachbereich)

  # create plot objects for waffle charts
  waffle_mint_rest <- waffle::waffle(x_mint_rest, keep = FALSE, colors = colors_mint_vernetzt$general) +
    ggplot2::labs(
      subtitle = paste0("<span style='font-size:16.0pt;'>", x_mint_rest[1],"% <span style='color:#154194; font-size:16.0pt;'>", title_help_sub,"</span> vs. <br>",
                        "<span style='font-size:16.0pt;'>" ,x_mint_rest[2],"% <span style='color:#b16fab; font-size:16.0pt;'>MINT</span>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(family="serif", size = 14),
                   plot.margin = ggplot2::unit(c(2.5,0,0,0), "lines"))

  plot <- ggpubr::ggarrange(waffle_mint_rest, legend="bottom")
  text <- c(
    paste0("<span style='font-size:20.5pt; color:black; font-family: serif'> Anteil von MINT an ", title_help,
           " in ",timerange))
  ggpubr::annotate_figure(plot, gridtext::richtext_grob(text = text))

}


#' A function to plot a graph.
#'
#' @description A function to create a stacked bar chart for the first box
#' inside the tab "Home".
#'
#' @return The return value is a plot
#' @param df The dataframe "zentral.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
home_einsitieg_waffle_female <- function(df,r, order) {

  # load UI inputs from reactive value
  timerange <- r$date_home_einstieg

  indikator_choice_1 <- r$indikator_start_einstieg_1

  indikator_choice_2 <- r$indikator_start_einstieg_2

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")


  # aggeregate every "bereich to Rest vs MINT
  df <- share_MINT(df)


  if (order == "first") {

    df <- df %>% dplyr::filter(indikator == indikator_choice_1)

  } else {

    df <- df %>% dplyr::filter(indikator == indikator_choice_2)

  }

  if (order == "first") {

    if ((indikator_choice_1 == "Auszubildende" | indikator_choice_1 == "Beschäftigte")) {

      title_help <- "anderen Berufszweigen"

    } else {

      title_help <- "anderen Fächern"

    }

  } else {

    if ((indikator_choice_2 == "Auszubildende" | indikator_choice_2 == "Beschäftigte")) {

      title_help <- "anderen Berufszweigen"

    } else {

      title_help <- "anderen Fächern"

    }
  }

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")

  # calculate proportions
  df <- df %>% dplyr::mutate(props = sum(wert))


  df <- df %>% dplyr::group_by(fachbereich) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100


  x_mint_rest <- setNames(round_preserve_sum(as.numeric(df$proportion),0),
                          df$fachbereich)

  # create plot objects for waffle charts
  waffle_mint_rest <- waffle::waffle(x_mint_rest, keep = FALSE, colors = colors_mint_vernetzt$general) +
    ggplot2::labs(
      subtitle = paste0("<span style='font-size:16.0pt;'>", x_mint_rest[1],"% <span style='color:#154194; font-size:16.0pt;'>", title_help,"</span> vs. ",
                        "<span style='font-size:16.0pt;'>" ,x_mint_rest[2],"% <span style='color:#b16fab; font-size:16.0pt;'>MINT</span>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(family="serif", size = 14),
                   plot.margin = ggplot2::unit(c(2.5,0,0,0), "lines"))

  plot <- ggpubr::ggarrange(waffle_mint_rest, legend="bottom")
  text <- c(
    paste0("<span style='font-size:20.5pt; color:black; font-family: serif'> Verhältnis zwischen MINT und  ", title_help,
           " <br> für Frauen in ",timerange))
  ggpubr::annotate_figure(plot, gridtext::richtext_grob(text = text))

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
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "serif")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "serif")) %>%
    highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = "Anteil von Frauen an MINT im Verlauf",
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "serif", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "serif", fontSize = "14px")
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

  # remove "Arbeitsmarkt"
  df <- df %>% dplyr::filter(bereich != "Arbeitsmarkt")

  # only MINT perspective
  df <- df %>% dplyr::filter(fachbereich == "MINT")


  # calculate the share of males for "Studierende/Studienanfänger"
  df_sub <- df %>% dplyr::filter(indikator == "Studienanfänger" | indikator == "Studierende")
  df <- df[!(df$indikator == "Studienanfänger" | df$indikator == "Studierende"),]

  df_sub <- calc_share_male(df_sub, "box_1")

  df <- rbind(df, df_sub)

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

  # values$indikator <- factor(values$indikator,levels = c("Habilitationen", "Promotionen (angestrebt)",
  #                                                        "Studierende", "Studienanfänger",
  #                                                        "Leistungskurse"))

  # values %>%
  #   ggplot2::ggplot(ggplot2::aes(x = proportion, y = indikator)) +
  #   ggplot2::geom_line(ggplot2::aes(group = indikator), color="black")+
  #   ggplot2::geom_point(ggplot2::aes(color=anzeige_geschlecht), size=6, alpha = 0.6) +
  #   ggplot2::theme_classic() +
  #   ggplot2::theme(legend.position="bottom",
  #                  legend.text= ggplot2::element_text(family = 'serif'),
  #                  panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
  #                  plot.title = ggtext::element_markdown(hjust = 0.5),
  #                  axis.text.y = ggplot2::element_text(size = 11, family = 'serif')) +
  #   ggplot2::scale_x_continuous(labels = scales::percent_format()) +
  #   ggplot2::scale_color_manual(values = colors_mint_vernetzt$gender) +
  #   ggplot2::ylab("") + ggplot2::xlab("") +
  #   ggplot2::labs(color = "")
  #

  values$indikator <- factor(values$indikator,levels = c("Leistungskurse", "Studienanfänger",
                                                         "Studierende", "Promotionen (angestrebt)",
                                                         "Habilitationen"))
  values$proportion <- values$proportion * 100

  values %>%
    ggplot2::ggplot(ggplot2::aes(y = proportion, x = indikator, color = anzeige_geschlecht, group = anzeige_geschlecht)) +
    ggplot2::geom_line(size = 1.5) +
    ggplot2::theme_classic() +
    ggplot2::theme(plot.title = ggtext::element_markdown(hjust = 0.5),
                   text = ggplot2::element_text(family="serif", size = 14),
                   panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
                   panel.grid.major.x = ggplot2::element_line(colour = "#D3D3D3")) +
    ggplot2::labs(x = "", y = "Anteil", color = "",
                                         title = paste0("<span style='font-size:20.5pt; color:black; font-family: serif'>",
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
home_rest_mint_verlauf <- function(df,r, order) {

  # load UI inputs from reactive value
  timerange <- r$date_start_multiple

  indikator_choice_1 <- r$indikator_start_multiple_1

  indikator_choice_2 <- r$indikator_start_multiple_2


  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(region == "Deutschland")


  # aggeregate every "bereich to Rest vs MINT
  df <- share_MINT(df)

  if (order == "first") {

    df <- df %>% dplyr::filter(indikator == indikator_choice_1)

  } else {

    df <- df %>% dplyr::filter(indikator == indikator_choice_2)

  }

  if (order == "first") {

    if ((indikator_choice_1 == "Auszubildende" | indikator_choice_1 == "Beschäftigte")) {

      title_help <- paste0("anderen Berufszweigen <br>bei ", indikator_choice_1,"n")

    } else {

      if ((indikator_choice_1 == "Habilitationen" | indikator_choice_1 == "Promotionen (angestrebt)")){

        title_help <- paste0("anderen Fächern <br>bei ", indikator_choice_1)

      }else{

        title_help <- paste0("anderen Fächern <br>bei ", indikator_choice_1,"n")
      }
    }

  } else {

    if ((indikator_choice_2 == "Auszubildende" | indikator_choice_2 == "Beschäftigte")) {

      title_help <- paste0("anderen Berufszweigen <br>bei ", indikator_choice_2,"n")

    } else {

      if ((indikator_choice_2 == "Habilitationen" | indikator_choice_2 == "Promotionen (angestrebt)")){

        title_help <- paste0("anderen Fächern <br>bei ", indikator_choice_2)

      }else{

        title_help <- paste0("anderen Fächern <br>bei ", indikator_choice_2,"n")
      }
    }
  }

  df <- share_female(df)

  ggplot2::ggplot(df, ggplot2::aes(x = jahr, y = proportion, color = fachbereich, group = fachbereich)) +
    ggplot2::geom_line(size = 1.5) +
    ggplot2::scale_color_manual(values = colors_mint_vernetzt$general) +
    ggplot2::labs(color = "", title = paste0("<span style='font-size:20.5pt; color:black; font-family: serif'>",
                                             "Anteil von Frauen an MINT und ", title_help,
                                             "<br><br><br>"),
                  x = "Jahre", y = "Anteil") +
    ggplot2::theme_classic() +
    ggplot2::theme(plot.title = ggtext::element_markdown(hjust = 0.5),
                   text = ggplot2::element_text(family="serif", size = 14),
                   panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
                   legend.position = "bottom") +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100))

}
