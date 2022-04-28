

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

  # remove scientific notation
  options(scipen=999)

  p <- ggplot2::ggplot(df, ggplot2::aes(fill=fachbereich, y=wert, x=anzeige_geschlecht)) +
    ggplot2::labs(caption = "Quelle:", title = paste0("Anteile an MINT und allen anderen Schulfächern"),
                  fill = "Bereich") +
    ggplot2::facet_grid(~jahr,
                        scales = "free_x",
                        space = "free_x",
                        switch = "x")  +
    ggplot2::theme(strip.placement = "outside",
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
                   panel.background = ggplot2::element_rect(fill="white"),
                   strip.background = ggplot2::element_rect(fill = "white"),
                   axis.title = ggplot2::element_blank()) +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::scale_fill_manual(values = colors_mint_vernetzt$general)

  if(isTRUE(switch_absolut)){

    p <- p + ggplot2::geom_bar(position="stack", stat="identity")
    plotly::ggplotly(p)

  }else{

    p <- p + ggplot2::geom_bar(position="fill", stat="identity") +
      ggplot2::scale_y_continuous(labels = scales::percent_format())
    plotly::ggplotly(p)

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
                   plot.margin = ggplot2::unit(c(2.5,0,0,0), "lines"))

  waffle_rest <- waffle::waffle(x_rest, keep = FALSE, colors = colors_mint_vernetzt$gender) +
    ggplot2::labs(
      subtitle = paste0("<span style='font-size:16.0pt;'>" ,x_rest[1],"% <span style='color:#f5adac; font-size:16.0pt;'> Frauen </span> vs. ",
                        "<span style='font-size:16.0pt;'>", x_rest[2],"% <span style='color:#b1b5c3; font-size:16.0pt;'> Männer</span>"),
      title = paste0("<span style='color:#154194;'>", "**Andere Fächer**</span>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   plot.margin = ggplot2::unit(c(2.5,0,0,0), "lines"))


  plot <- ggpubr::ggarrange(waffle_mint, NULL ,waffle_rest, widths = c(1, -0.15, 1), nrow=1, common.legend = T,
                            legend="bottom")

  ggpubr::annotate_figure(plot,
                          top = ggpubr::text_grob(paste0("Anteile der Geschlechter an MINT und allen anderen Fächern für das Jahr ", timerange),
                                                  face = "bold", size = 14))
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

  # plot
  ggplot2::ggplot(df, ggplot2::aes(x=reorder(fachbereich, wert), y=wert, fill = anzeige_geschlecht)) +
    ggplot2::geom_bar(stat="identity", position = "dodge") +
    ggplot2::geom_text(ggplot2::aes(label=wert, vjust = - 0.25),
                       position=ggplot2::position_dodge(width=0.9),
                       fontface = "bold") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(colour = colors_mint_vernetzt$general, size = 14),
      plot.title = ggtext::element_markdown()) +
    ggplot2::xlab("Anzahl") + ggplot2::ylab("Fachrichtung") +
    ggplot2::scale_fill_manual(values = colors_mint_vernetzt$gender) +
    ggplot2::labs(title = paste0("**Schüler*innn in MINT und allen anderen Fächern für das Jahr ", timerange,"**"))


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
    dplyr::slice(1:5) %>% dplyr::mutate(label_rank = "Top")

  low_five <- df %>%
    dplyr::arrange((wert)) %>%
    dplyr::group_by(anzeige_geschlecht) %>%
    dplyr::slice(1:5) %>% dplyr::mutate(label_rank = "Unbeliebt")

  df_ranked <- rbind(top_five, low_five)

if(type == "first"){

  plot_1 <- df_ranked %>% dplyr::filter(anzeige_geschlecht == "Frauen") %>%
  ggplot2::ggplot(ggplot2::aes(x=reorder(fachbereich, wert), y=wert, fill = label_rank)) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::coord_flip() +
    ggplot2::facet_grid(label_rank ~ ., scales = "free", space = "free") +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(values = c("#00a87a", "#ee7775")) +
    ggplot2::theme(legend.position = "none",
                   strip.placement = "outside",
                   panel.background = ggplot2::element_rect(fill="white"),
                   strip.background = ggplot2::element_rect(fill = "white")) +
   ggplot2::labs(title = "Frauen")

  plot_2 <- df_ranked %>% dplyr::filter(anzeige_geschlecht == "Männer") %>%
    ggplot2::ggplot(ggplot2::aes(x=reorder(fachbereich, wert), y=wert, fill = label_rank)) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::coord_flip() +
    ggplot2::facet_grid(label_rank ~ ., scales = "free", space = "free") +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(values = c("#00a87a", "#ee7775")) +
    ggplot2::theme(legend.position = "none",
                   strip.placement = "outside",
                   panel.background = ggplot2::element_rect(fill="white"),
                   strip.background = ggplot2::element_rect(fill = "white")) +
    ggplot2::labs(title = "Männer")


  plot <- ggpubr::ggarrange(plot_1, NULL, plot_2, widths = c(1, 0.2, 1), nrow=1)
  ggpubr::annotate_figure(plot,
                          top = ggpubr::text_grob(paste0("Die beliebtesten und unpopulärsten 5 Fächer ", timerange),
                                                  face = "bold", size = 14))
}else{

  df_ranked %>%
    ggplot2::ggplot(ggplot2::aes(x= wert, y= reorder(fachbereich,wert))) +
    ggplot2::geom_line(ggplot2::aes(group = fachbereich),color="grey")+
    ggplot2::geom_point(ggplot2::aes(color=anzeige_geschlecht), size=6) +
    ggplot2::labs(y="country", color="")+
    ggplot2::theme_classic(24)+
    ggplot2::theme(legend.position="top") +
    ggplot2::scale_color_manual(values = colors_mint_vernetzt$gender)

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
      valueDecimals = 2,
      valueSuffix = "%"
    )
  ) %>%
    highcharter::hc_title(
      text = paste0("<b>Anteil der Frauen</b> an MINT-Fächern für das Jahr ", timerange),
      margin = 20,
      align = "center",
      style = list(color = "black", useHTML = TRUE)
    ) %>%
    highcharter::hc_caption(
      text = "Quelle:"
    )



}
