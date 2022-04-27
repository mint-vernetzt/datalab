

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

