################################################################################
################################# Studienzahl #################################
################################################################################


#' A function to plot a graph.
#'
#' @description A function to create a stacked bar chart for the first box
#' inside the tab "Studium".
#'
#' @return The return value is a plot
#' @param df The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_einstieg_bar <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_studierende_einstieg

  status_studierende <- r$indikator_studierende_einstieg

  geschlecht <- r$geschlecht_studierende_einstieg

  switch_absolut <- r$switch_rel_abs

  lehramt_enthalten <- r$nurLehramt_studierende_einstieg


  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == status_studierende)

  df <- df %>% dplyr::filter(region == "Deutschland")



  # call function to calculate the share of MINT and the remaining subjects
  df <- calc_share_MINT(df)

  # calculate the share of males
  df <- calc_share_male(df, "box_1")

  # filter gender
  df <- df %>% dplyr::filter(anzeige_geschlecht %in% geschlecht)

  # remove scientific notation
  options(scipen=999)


  if (status_studierende == "Studierende"){

    title_help <- " von Studierenden"

  }else{

    title_help <- " von Studienanfängern"

  }


  if(isTRUE(lehramt_enthalten)){


    df <- df[!(df$nur_lehramt == "Nein" & df$hochschulform == "Uni"), ]

    df <- df[!(df$nur_lehramt == "Nein" & df$hochschulform == "FH"), ]

    # calculate share of teacher
    values <- df %>%
      dplyr::group_by(jahr, fachbereich, anzeige_geschlecht) %>%
      dplyr::mutate(wert = wert - dplyr::lead(wert)) %>% na.omit()

    df[df$hochschulform == "insgesamt", "wert"] <- values$wert

    df[df$nur_lehramt == "Ja", "fachbereich"] <- interaction(df[df$nur_lehramt == "Ja", "fachbereich"][[1]], " (Lehramt)", sep = "")

    df <- df[with(df, order(anzeige_geschlecht, jahr, decreasing = TRUE)), ]

    values <- df %>% dplyr::group_by(anzeige_geschlecht, jahr) %>%
      dplyr::summarize(wert = sum(wert))

    values <- values[with(values, order(anzeige_geschlecht, jahr, decreasing = TRUE)), ]


    df$Anteil <- NA

    df[df$fachbereich == "andere Studiengänge", "Anteil"] <- round((df[df$fachbereich == "andere Studiengänge", "wert"]/
                          values$wert)*100)

    df[df$fachbereich == "andere Studiengänge (Lehramt)", "Anteil"] <- round((df[df$fachbereich == "andere Studiengänge (Lehramt)", "wert"]/
                                                                              values$wert)*100)

    df[df$fachbereich == "MINT", "Anteil"] <- round((df[df$fachbereich == "MINT", "wert"]/
                                                     values$wert)*100)

    df[df$fachbereich == "MINT (Lehramt)", "Anteil"] <- round((df[df$fachbereich == "MINT (Lehramt)", "wert"]/
                                                               values$wert)*100)

    df$Anteil <- paste(df$Anteil,"%")

    names(df)[3] <- "Wert"

    # plot
   p <- ggplot2::ggplot(df, ggplot2::aes(fill=fachbereich, y=Wert, x=anzeige_geschlecht, tooltip = Anteil)) +
      ggplot2::facet_grid(~jahr,
                 scales = "free_x",
                 space = "free_x",
                 switch = "x")  +
      ggplot2::labs(caption = "Quelle:", title = paste0("<span style='font-size:20px; color:black; font-family: SourceSans3-Regular'>",
        "Anteile an MINT und allen anderen Studienfächer", title_help),
                    fill = "") +
      ggplot2::theme(strip.placement = "outside",
                     plot.title = ggtext::element_markdown(hjust = 0.5),
                     #plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
                     panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
                     panel.background = ggplot2::element_rect(fill="white"),
                     strip.background = ggplot2::element_rect(fill = "white"),
                     axis.title = ggplot2::element_blank()) +
      ggplot2::scale_fill_manual(values = c(ggplot2::alpha("#154194", 1),
                                            ggplot2::alpha("#154194", 0.5),
                                            ggplot2::alpha("#b16fab", 1),
                                            ggplot2::alpha("#b16fab", 0.5))) +
      ggplot2::scale_y_continuous(expand = c(0,0))


   t <- list(
     family = "SourceSans3-Regular", size = 14)


   if(isTRUE(switch_absolut)){

    p <- p + ggplot2::geom_bar(position="stack", stat="identity")

    plotly::ggplotly(p, tooltip = "Wert") %>% plotly::config(displayModeBar = FALSE,
                                                             displaylogo = FALSE
                                                             #editable = FALSE,
                                                             #showTips = FALSE,
                                                             #edits = FALSE,
                                                             #scrollZoom = FALSE,
    ) %>%
      plotly::layout(font = t, xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))

   }else{

     p <- p + ggplot2::geom_bar(position="fill", stat="identity") +
                ggplot2::scale_y_continuous(labels = scales::percent_format())

     plotly::ggplotly(p, tooltip = "tooltip") %>% plotly::config(displayModeBar = FALSE,
                                                                 displaylogo = FALSE
                                                                 #editable = FALSE,
                                                                 #showTips = FALSE,
                                                                 #edits = FALSE,
                                                                 #scrollZoom = FALSE,
     ) %>%
       plotly::layout(font = t, xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))

   }


  }else{

    df <- df %>% dplyr::filter(nur_lehramt != "Ja")

    df <- df %>% dplyr::filter(hochschulform == "insgesamt")

    df <- df[with(df, order(anzeige_geschlecht, jahr, decreasing = TRUE)), ]

    values <- df %>% dplyr::group_by(anzeige_geschlecht, jahr) %>%
      dplyr::summarize(wert = sum(wert))

    values <- values[with(values, order(anzeige_geschlecht, jahr, decreasing = TRUE)), ]


    df$Anteil <- NA

    df[df$fachbereich == "andere Studiengänge", "Anteil"] <- round((df[df$fachbereich == "andere Studiengänge", "wert"]/values$wert)*100)

    df[df$fachbereich == "MINT", "Anteil"] <- round((df[df$fachbereich == "MINT", "wert"]/values$wert)*100)


    df$Anteil <- paste(df$Anteil,"%")

    names(df)[3] <- "Wert"

    p <- ggplot2::ggplot(df, ggplot2::aes(fill=fachbereich, y=Wert, x=anzeige_geschlecht, tooltip = Anteil)) +
      ggplot2::labs(caption = "Quelle:", title = paste0("<span style='font-size:20px; color:black; font-family: SourceSans3-Regular'>",
        "Anteile an MINT und allen anderen Studienfächer", title_help),
                    fill = "") +
      ggplot2::facet_grid(~jahr,
                          scales = "free_x",
                          space = "free_x",
                          switch = "x")  +
      ggplot2::theme(strip.placement = "outside",
                     plot.title = ggtext::element_markdown(hjust = 0.5),
                    # plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
                     panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
                     panel.background = ggplot2::element_rect(fill="white"),
                     strip.background = ggplot2::element_rect(fill = "white"),
                     axis.title = ggplot2::element_blank()) +
      ggplot2::scale_y_continuous(expand = c(0,0)) +
      ggplot2::scale_fill_manual(values = colors_mint_vernetzt$general)

    t <- list(
      family = "SourceSans3-Regular", size = 14)

    if(isTRUE(switch_absolut)){

      p <- p + ggplot2::geom_bar(position="stack", stat="identity")
      plotly::ggplotly(p, tooltip = "Wert") %>% plotly::config(displayModeBar = FALSE,
                                                                     displaylogo = FALSE
                                                                     #editable = FALSE,
                                                                     #showTips = FALSE,
                                                                     #edits = FALSE,
                                                                     #scrollZoom = FALSE,
                                                               ) %>%
        plotly::layout(font = t, xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    }else{

     p <- p + ggplot2::geom_bar(position="fill", stat="identity") +
        ggplot2::scale_y_continuous(labels = scales::percent_format())
      plotly::ggplotly(p, tooltip = "tooltip") %>% plotly::config(displayModeBar = FALSE,
                                                                  displaylogo = FALSE
                                                                  #editable = FALSE,
                                                                  #showTips = FALSE,
                                                                  #edits = FALSE,
                                                                  #scrollZoom = FALSE,
      ) %>%
        plotly::layout(font = t, xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))

    }

  }

}


#' A function to return a filtered dataset
#'
#' @description A function to similar to 'studienzahl_einstieg_bar' but with the
#' difference that it returns a dataframe instead of plot.
#'
#' @return The return value is a dataframe
#' @param df The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

data_einstieg <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_studierende_einstieg

  status_studierende <- r$indikator_studierende_einstieg

  geschlecht <- r$geschlecht_studierende_einstieg

  lehramt_enthalten <- r$nurLehramt_studierende_einstieg

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(region == "Deutschland")

  # call function to calculate the share of MINT and the remaining subjects
  df <- calc_share_MINT(df)

  # calculate the share of males
  df <- calc_share_male(df, "box_1")

  # filter gender
  df <- df %>% dplyr::filter(anzeige_geschlecht %in% geschlecht)

  if(isTRUE(lehramt_enthalten)){

   # calculate the share of teacher
   df <- calc_share_teacher(df)

   df$hochschulform <- NULL

   colnames(df) <- c("Region", "Geschlecht", "Wert", "Indikator", "Lehramt", "Fachbereich", "Jahr", "Bereich")

    return(df)


  }else{

    df <- df %>% dplyr::filter(nur_lehramt != "Ja")

    df <- df %>% dplyr::filter(hochschulform == "insgesamt")

    df$hochschulform <- NULL

    colnames(df) <- c("Region", "Geschlecht", "Wert", "Indikator", "Lehramt", "Fachbereich", "Jahr", "Bereich")

    return(df)

  }

}



#' A function to plot a waffle chart
#'
#' @description A function to create a waffle chart for the second box inside the
#' tab "Studium".
#'
#' @return The return value is a waffle chart
#' @param df The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_waffle <- function(df,r) {

  # load UI inputs from reactive value
  status_studierende <- r$indikator_studierende

  timerange <- r$date_studierende

  lehramt <- r$nurLehramt_studierende

  hochschulform_select_1 <- r$hochschulform_studierende_1

  hochschulform_select_2 <- r$hochschulform_studierende_2

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(indikator == status_studierende)

  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  }

  df <- df %>% dplyr::filter(region == "Deutschland")

  # call function to calculate the share of MINT and the remaining subjects
  df <- calc_share_MINT(df)

  # calculate the share of males
  df <- calc_share_male(df, "box_2")

  # calculate proportions
  df <- df %>% dplyr::group_by(fachbereich) %>%
    dplyr::mutate(props = sum(wert))


  df <- df %>% dplyr::group_by(anzeige_geschlecht, fachbereich) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100


  x_mint <- setNames(round_preserve_sum(as.numeric(df[df$fachbereich == "MINT", "proportion"][[1]]),0),
                     df[df$fachbereich == "MINT", "anzeige_geschlecht"][[1]])

  x_rest <- setNames(round_preserve_sum(as.numeric(df[df$fachbereich == "andere Studiengänge", "proportion"][[1]]),0),
                     df[df$fachbereich == "andere Studiengänge", "anzeige_geschlecht"][[1]])

  # create plot objects for waffle charts
  waffle_mint <- waffle::waffle(x_mint, keep = FALSE, colors = colors_mint_vernetzt$gender) +
    ggplot2::labs(
      subtitle = paste0("<span style='font-size:16.0pt;'>" ,x_mint[1],"% <span style='color:#f5adac; font-size:16.0pt;'> Frauen</span> vs. ",
                        "<span style='font-size:16.0pt;'>", x_mint[2],"% <span style='color:#b1b5c3; font-size:16.0pt;'> Männer</span>"),
      title = paste0("<span style='color:#b16fab;'>", "**MINT**</span>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(2.5,0,0,0), "lines"))

  waffle_rest <- waffle::waffle(x_rest, keep = FALSE, colors = colors_mint_vernetzt$gender) +
    ggplot2::labs(
      subtitle = paste0("<span style='font-size:16.0pt;'>" ,x_rest[1],"% <span style='color:#f5adac; font-size:16.0pt;'> Frauen </span> vs. ",
                        "<span style='font-size:16.0pt;'>", x_rest[2],"% <span style='color:#b1b5c3; font-size:16.0pt;'> Männer</span>"),
      title = paste0("<span style='color:#154194;'>", "**Andere Studiengänge**</span>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(2.5,0,0,0), "lines"))



  if(lehramt == FALSE){

    title_help_sub_sub <- ""

    hochschulform <- hochschulform_select_1

  } else {

    title_help_sub_sub <- " (nur Lehramt)"

    hochschulform <- hochschulform_select_2

  }

  if (hochschulform == "Uni"){

    title_help_sub <- "an einer Uni"

  }else if (hochschulform == "FH"){

    title_help_sub <- "an einer FH"

  } else {

    title_help_sub <- "insgesamt"

  }


  if (status_studierende == "Studierende"){

    title_help <- paste0("bei Studierenden ", title_help_sub, title_help_sub_sub)

  }else{

    title_help <- paste0("bei Studienanfängern ", title_help_sub,  title_help_sub_sub)

  }

  plot <- ggpubr::ggarrange(waffle_mint, NULL ,waffle_rest, widths = c(1, -0.15, 1), nrow=1, common.legend = T,
                            legend="bottom")

  text <- c(
    paste0("<span style='font-size:20.5pt; color:black'> Anteil von Frauen und Männern an MINT und allen andere Studiengängen <br> ", title_help,
           " in ",timerange))
  ggpubr::annotate_figure(plot, gridtext::richtext_grob(text = text))

}

#' A function to create a paired bar plot
#'
#' @description A function to return a paired bar plot for the second box inside
#' the tab "Studium
#'
#' @return The return value is a bar plot
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_absolut <- function(df,r) {

  status_studierende <- r$indikator_studierende

  timerange <- r$date_studierende

  lehramt <- r$nurLehramt_studierende

  geschlecht <- r$geschlecht_studierende

  hochschulform_select_1 <- r$hochschulform_studierende_1

  hochschulform_select_2 <- r$hochschulform_studierende_2

  mint_vs_rest <- r$mint_vs_rest_studierende

  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(indikator == status_studierende)

  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  }


  df <- df %>% dplyr::filter(region == "Deutschland")

  # call function to calculate the share of MINT and the remaining subjects
  df <- calc_share_MINT(df)

  # calculate the share of males
  df <- calc_share_male(df, type = "box_2")

  if(lehramt == FALSE){


    title_help_sub_sub <- ""

    hochschulform <- hochschulform_select_1

  } else {

    title_help_sub_sub <- " (nur Lehramt)"

    hochschulform <- hochschulform_select_2

  }

  if (hochschulform == "Uni"){

    title_help_sub <- "an einer Uni"

  }else if (hochschulform == "FH"){

    title_help_sub <- "an einer FH"

  } else {

    title_help_sub <- "insgesamt"

  }


  if (status_studierende == "Studierende"){

    title_help <- paste0("von Studierenden ", title_help_sub, title_help_sub_sub)

  }else{

    title_help <- paste0("von Studienanfängern ", title_help_sub, title_help_sub_sub)

  }
  # plot
  ggplot2::ggplot(df, ggplot2::aes(x=reorder(fachbereich, wert), y=wert, fill = anzeige_geschlecht)) +
    ggplot2::geom_bar(stat="identity", position = "dodge") +
    ggplot2::geom_text(ggplot2::aes(label=wert, vjust = - 0.25),
                       position=ggplot2::position_dodge(width=0.9),
                       fontface = "bold") +
    ggplot2::theme_bw() +
    ggplot2::theme(# = ggplot2::element_text(hjust = 0.5),
      text = ggplot2::element_text(size = 14),
      axis.text.x = ggplot2::element_text(colour = c("#b16fab", "#154194"), size = 14, face="bold"),
      plot.title = ggtext::element_markdown(hjust = 0.5)) +
    ggplot2::ylab("Anzahl") + ggplot2::xlab("") +
    ggplot2::scale_fill_manual(values = colors_mint_vernetzt$gender) +
    ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
                                 "Frauen und Männern in MINT und allen andere Studiengängen <br> ", title_help,
                                 " in ",timerange,
                                 "<br><br>"),
                fill = "")


}



#' A function to plot the german map
#'
#' @description A function to plot the german map with all states that contain
#' information about the share of women in STEM
#'
#' @return The return value is the german map with information
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_map <- function(df,r) {

  # load UI inputs from reactive value
  status_studierende <- r$indikator_studierende

  timerange <- r$date_studierende

  lehramt <- r$nurLehramt_studierende

  hochschulform_select_1 <- r$hochschulform_studierende_1

  hochschulform_select_2 <- r$hochschulform_studierende_2

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(indikator == status_studierende)

  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  }

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(region != "Baden-Württemberg")

  # call function to calculate the share of MINT and the remaining subjects
  df <- calc_share_MINT(df)

  df <- df %>% dplyr::filter(fachbereich != "andere Studiengänge")

  values <- (df[df$anzeige_geschlecht == "Frauen", "wert"]/df[df$anzeige_geschlecht == "Gesamt", "wert"])*100

  values$region <- df[df$anzeige_geschlecht == "Frauen", "region"][[1]]


  if(lehramt == FALSE){


    title_help_sub_sub <- ""

    hochschulform <- hochschulform_select_1

  } else {

    title_help_sub_sub <- " (nur Lehramt)"

    hochschulform <- hochschulform_select_2

  }

  if (hochschulform == "Uni"){

    title_help_sub <- "an einer Uni"

  }else if (hochschulform == "FH"){

    title_help_sub <- "an einer FH"

  } else {

    title_help_sub <- "insgesamt"

  }

  if (status_studierende == "Studierende"){

    title_help <- paste0("Studentinnen ", title_help_sub, title_help_sub_sub)

  }else{

    title_help <- paste0("Studienanfängerinnen ", title_help_sub, title_help_sub_sub)

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
      text = paste0("Anteil der ",title_help ," an MINT in ", timerange),
      margin = 10,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
    ) %>%
    highcharter::hc_caption(
      text = "Quelle:",  style = list(fontSize = "12px")
    ) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular")
    ) %>% highcharter::hc_size(600, 440) %>%
    highcharter::hc_legend(align = "right", layout = "vertical")



}


#' A function to return a filtered dataset
#'
#' @description A function to create a dataframe for given filter
#'
#' @return The return value is a dataframe
#' @param df The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

data_mix_studium <- function(df,r) {

  # load UI inputs from reactive value
  status_studierende <- r$indikator_studierende

  timerange <- r$date_studierende

  lehramt <- r$nurLehramt_studierende

  hochschulform_select_1 <- r$hochschulform_studierende_1

  hochschulform_select_2 <- r$hochschulform_studierende_2

  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(indikator == status_studierende)

  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  }

  # call function to calculate the share of MINT and the remaining subjects
  df <- calc_share_MINT(df)

  # calculate the share of males
  values <- df %>%
    dplyr::group_by(region, fachbereich, indikator, nur_lehramt, hochschulform, jahr, bereich) %>%
    dplyr::mutate(wert = wert - dplyr::lead(wert)) %>% dplyr::select(wert) %>% na.omit()

  values$anzeige_geschlecht <- "Männer"

  values <- values[, colnames(df)]

  df <- rbind(df, values)

  colnames(df) <- c("Region", "Geschlecht", "Wert", "Indikator", "Lehramt", "Hochschulform", "Fachbereich", "Jahr", "Bereich")

  return(df)
}


#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_verlauf <- function(df,r) {

  # load UI inputs from reactive value
  status_studierende <- r$indikator_studierende_verlauf

  timerange <- r$date_studierende_verlauf

  lehramt <- r$nurLehramt_studierende_verlauf

  states <- r$states_studierende_verlauf

  topic <- r$topic_studierende_verlauf

  ost_west <- r$ost_west

  subject_aggregated <- r$subjects_aggregated

  subjects_select <- r$subject_selected

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == status_studierende)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(region != "Baden-Württemberg")

  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == "insgesamt")

    title_help_sub_sub <- " insgesamt"

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == "Uni")

    title_help_sub_sub <- " an einer Uni (nur Lehramt)"
  }




  if (subject_aggregated == "aggregiert"){

    # call function to calculate the share of MINT and the remaining subjects
    df <- calc_share_MINT(df)

    if (topic == "MINT"){

      title_help_sub <- "an MINT"

    } else {

      title_help_sub <- "an anderen Studienfächern"

    }

  }else {


    df <- df %>% dplyr::filter(fachbereich %in% subjects_select)

    title_help_sub <- dictionary_title_studium[[subjects_select]]
  }


  if (status_studierende == "Studierende"){

    title_help <- paste0("Studentinnen ", title_help_sub, title_help_sub_sub)

  }else{

    title_help <- paste0("Studienanfängerinnen ", title_help_sub, title_help_sub_sub)

  }




  # order
  df <- df[with(df, order(region, fachbereich, jahr, decreasing = TRUE)), ]

  # calculate proportion
  values <- df %>%
    dplyr::group_by(jahr, fachbereich, region) %>%
    dplyr::mutate(wert = dplyr::lead(wert)/wert) %>% dplyr::select(wert) %>% na.omit()


  if (ost_west == FALSE) {

    values <- values %>% dplyr::filter(region %in% states)

  } else{

    values$dummy_west <- ifelse(values$region %in% states_east_west$west, "Westen", "Osten")

    values <- values %>% dplyr::group_by(jahr, fachbereich, dummy_west) %>%
      dplyr::summarise(wert = mean(wert))

    names(values)[3] <- "region"
  }

  if (subject_aggregated == "aggregiert"){
    # filter MINT or remaining subjects
    values <- values %>% dplyr::filter(fachbereich == topic)
  }

  # order years for plot
  values <- values[with(values, order(region, jahr, decreasing = FALSE)), ]

  values$wert <- values$wert * 100



  # plot
  highcharter::hchart(values, 'line', highcharter::hcaes(x = jahr, y = round(wert), group = region)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil Frauen <br> Bundesland: {point.region} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil an ", title_help ," im Verlauf"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    )


}


#' A function to return a filtered dataset
#'
#' @description A function to create a dataframe for given filter
#'
#' @return The return value is a dataframe
#' @param df The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

data_verlauf_studium <- function(df,r) {

  # load UI inputs from reactive value
  status_studierende <- r$indikator_studierende_verlauf

  timerange <- r$date_studierende_verlauf

  lehramt <- r$nurLehramt_studierende_verlauf

  states <- r$states_studierende_verlauf

  topic <- r$topic_studierende_verlauf

  ost_west <- r$ost_west

  subject_aggregated <- r$subjects_aggregated

  subjects_select <- r$subject_selected

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == status_studierende)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(region != "Baden-Württemberg")

  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == "insgesamt")

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == "Uni")
  }


  if (subject_aggregated == "aggregiert"){

    # call function to calculate the share of MINT and the remaining subjects
    df <- calc_share_MINT(df)

    if (topic == "MINT"){

      title_help <- "MINT"

    } else {

      title_help <- "anderen Studienfächern"

    }

  }else {


    df <- df %>% dplyr::filter(fachbereich %in% subjects_select)

    title_help <- dictionary_title_studium[[subjects_select]]
  }

  # order
  df <- df[with(df, order(region, fachbereich, jahr, decreasing = TRUE)), ]

  # calculate proportion
  values <- df %>%
    dplyr::group_by(jahr, fachbereich, region, indikator, nur_lehramt, hochschulform, bereich) %>%
    dplyr::mutate(wert = dplyr::lead(wert)/wert) %>% dplyr::select(wert) %>% na.omit()


  if (ost_west == FALSE) {

    values <- values %>% dplyr::filter(region %in% states)

  } else{

    values$dummy_west <- ifelse(values$region %in% states_east_west$west, "Westen", "Osten")

    values <- values %>% dplyr::group_by(jahr, fachbereich, dummy_west,
                                         indikator, nur_lehramt, hochschulform, bereich) %>%
      dplyr::summarise(wert = mean(wert))

    names(values)[3] <- "region"
  }

  if (subject_aggregated == "aggregiert"){
    # filter MINT or remaining subjects
    values <- values %>% dplyr::filter(fachbereich == topic)
  }

  # order years for plot
  values <- values[with(values, order(region, jahr, decreasing = FALSE)), ]

  values$wert <- round(values$wert * 100)

  values$wert <- paste0(values$wert,"%")

  colnames(values) <- c("Jahr", "Fachbereich", "Region", "Indikator", "Lehramt", "Hochschulform", "Bereich", "Wert")


  return(values)
}












################################################################################
################################# Abschlusszahl ################################
################################################################################


#' make_plots_studium
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @param data
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

#change df to data
abschlusszahl_bar <- function(data,r){

  timestamp <- r$date_abschluss

  pass_fail <- r$durchgefallen

  gender_select <- r$geschlecht_abschluss

  subject <- r$ing_natwi

  df <- filter_data_abschluss(data)

  df <- df %>% dplyr::filter(jahr == timestamp)

  df <- df %>% dplyr::filter(prüfungsstatus == pass_fail)

  if(subject == "Gesamt"){

    df <- df %>% dplyr::group_by(status, Frauen_manner_alle, quelle) %>%
    dplyr::summarize(wert = sum(wert))

  } else if(subject == "ingenieur"){

    df <- df %>% dplyr::filter(fachbereich_alle_mint_mathe_ing == "ingenieur")

  } else {

    df <- df %>% dplyr::filter(fachbereich_alle_mint_mathe_ing == "mathe_natwi")

  }


  if(gender_select == "Ja"){

    df <- df %>% dplyr::filter(Frauen_manner_alle != "Gesamt")

    plot <- ggplot2::ggplot(df, ggplot2::aes(fill=Frauen_manner_alle, y=reorder(status,wert), x=wert)) +
      ggplot2::geom_bar(stat="identity", position = "dodge") +
      ggplot2::theme_bw() +
      ggplot2::xlab("Jahre") + ggplot2::ylab("akademischer Grad") +
      ggplot2::geom_text(ggplot2::aes(label=wert, hjust = "left"),
                         position=ggplot2::position_dodge(width=0.9))

    if(subject == "Gesamt"){

      plot + ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse ","(", pass_fail,")"," im Jahr ",
                                          timestamp, " im Gesamten MINT Fachebreich"), fill = "Geschlecht",
                                 caption = paste0("Quelle: ", unique(df$quelle)))
    }else{

      plot + ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse ","(", pass_fail,")"," im Jahr ",
                                          timestamp, " im Fachebreich ",
                                          dictionary_title_studium_abschluss[[subject]]),
                           fill = "Geschlecht",
                           caption = paste0("Quelle: ", unique(df$quelle)))
    }

  }else {

    df <- df %>% dplyr::filter(Frauen_manner_alle == "Gesamt")

    plot <- ggplot2::ggplot(df, ggplot2::aes(y=reorder(status,wert), x=wert)) +
      ggplot2::geom_bar(stat="identity", position = "dodge") +
      ggplot2::theme_bw() +
      ggplot2::xlab("Jahre") + ggplot2::ylab("akademischer Grad") +
      ggplot2::geom_text(ggplot2::aes(label=wert, hjust = "left"),
                         position=ggplot2::position_dodge(width=0.9))

    if(subject == "Gesamt"){

      plot + ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse ","(", pass_fail,")"," im Jahr ",
                                          timestamp, " im Gesamten MINT Fachebreich"),
                           caption = paste0("Quelle: ", unique(df$quelle)))
    }else{

      plot + ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse ","(", pass_fail,")"," im Jahr ",
                                          timestamp, " im Fachebreich ",
                                          dictionary_title_studium_abschluss[[subject]]),
                           caption = paste0("Quelle: ", unique(df$quelle)))
    }
  }
}



#' make_plots_studium
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @param data
#' @param r
#' @noRd

abschlusszahl_waffle <- function(data,r) {


  timestamp <- r$date_abschluss

  pass_fail <- r$durchgefallen

  gender_select <- r$geschlecht_abschluss

  subject <- r$ing_natwi

  df <- filter_data_abschluss(data)

  df <- df %>% dplyr::filter(jahr == timestamp)

  df <- df %>% dplyr::filter(prüfungsstatus == pass_fail)


  df$wert <- df$wert/1000


  if(subject == "Gesamt"){

    df <- df %>% dplyr::group_by(status, Frauen_manner_alle, quelle) %>%
      dplyr::summarize(wert = sum(wert))

  } else if(subject == "ingenieur"){

    df <- df %>% dplyr::filter(fachbereich_alle_mint_mathe_ing == "ingenieur")

  } else {

    df <- df %>% dplyr::filter(fachbereich_alle_mint_mathe_ing == "mathe_natwi")

  }


  if(gender_select == "Nein"){

  df <- df %>% dplyr::filter(Frauen_manner_alle == "Gesamt")

  x <- setNames(as.numeric(round(df$wert)), df$status)

    if(subject == "Gesamt"){


      waffle::waffle(x, keep = FALSE, rows = 5) +
        ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse"),
                      subtitle = "1 box = 1000 Personen",
                      caption = paste0("Quelle: ", unique(df$quelle))) +
        ggplot2::theme(strip.text.x = ggplot2::element_text(hjust = 0.5),
                       legend.position = "bottom")


    }else{

      waffle::waffle(x, keep = FALSE, rows = 5) +
        ggplot2::labs(title = paste0("Gesamttzahl Abschlüsse nach Fachbereich"),
                           subtitle = "1 box = 1000 Personen",
                           caption = paste0("Quelle: ", unique(df$quelle)))
    }

  }else{

    df <- df %>% dplyr::filter(Frauen_manner_alle != "Gesamtt")


    x_male <- setNames(as.numeric(round(df[df$Frauen_manner_alle == "männlich", "wert"][[1]])),
                       df[df$Frauen_manner_alle == "männlich", "status"][[1]])

    x_female <- setNames(as.numeric(round(df[df$Frauen_manner_alle == "weiblich", "wert"][[1]])),
                         df[df$Frauen_manner_alle == "weiblich", "status"][[1]])

  if(subject == "Gesamtt"){

      waffle_male <- waffle::waffle(x_male, rows = 5, pad = 0, keep = FALSE) +
        ggplot2::labs(title = paste0("Gesamttzahl Abschlüsse männlich")) +
        ggplot2::theme(strip.text.x = ggplot2::element_text(hjust = 0.5),
                       legend.position = "bottom")

      waffle_female <- waffle::waffle(x_female, rows = 5, pad = 7, keep = FALSE) +
        ggplot2::labs(title = paste0("Gesamttzahl Abschlüsse weiblich"),
                      caption = paste0("Quelle: ", unique(df$quelle),
                                       ", 1 box = 1000 Personen")) +
        ggplot2::theme(strip.text.x = ggplot2::element_text(hjust = 0.5),
                       legend.position = "bottom")

      waffle::iron(waffle_male,waffle_female)

    }else{

      waffle_male <- waffle::waffle(x_male,  pad = 0, rows = 5, keep = FALSE) +
        ggplot2::labs(title = paste0("Gesamttzahl Abschlüsse männlich")) +
        ggplot2::theme(strip.text.x = ggplot2::element_text(hjust = 0.5),
                       legend.position = "bottom")

        waffle_female <- waffle::waffle(x_female,  pad = 4, rows = 5, keep = FALSE) +
          ggplot2::labs(title = paste0("Gesamttzahl Abschlüsse weiblich"),
            caption = paste0("Quelle: ", unique(df$quelle),
                                         ", 1 box = 1000 Personen")) +
          ggplot2::theme(strip.text.x = ggplot2::element_text(hjust = 0.5),
                         legend.position = "bottom")

        waffle::iron(waffle_male,waffle_female)

    }

  }

}


#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @param data
#' @param r
#' @noRd

#change df to data
abschluss_bar_2 <- function(data,r){

  date_range <- r$date_abschluss_1

  pass_fail <- r$durchgefallen_1

  gender_select <- r$geschlecht_abschluss_1

  subject <- r$ing_natwi_1

  indikators <- r$indikator_abschluss_1


  df <- filter_data_abschluss(data)

  df <- df %>% dplyr::filter(jahr >= date_range[1] & jahr <= date_range[2])

  df <- df %>% dplyr::filter(prüfungsstatus == pass_fail)

  df <- df %>% subset(status %in% indikators)

  if(subject == "Gesamtt"){

    df <- df %>% dplyr::group_by(status, Frauen_manner_alle, jahr, quelle) %>%
      dplyr::summarize(wert = sum(wert))

  } else if(subject == "ingenieur"){

    df <- df %>% dplyr::filter(fachbereich_alle_mint_mathe_ing == "ingenieur")

  } else {

    df <- df %>% dplyr::filter(fachbereich_alle_mint_mathe_ing == "mathe_natwi")

  }


  if(gender_select == "Ja"){

    df <- df %>% dplyr::filter(Frauen_manner_alle != "Gesamtt")

    plot <- ggplot2::ggplot(df, ggplot2::aes(fill=status, y=wert, x=jahr)) +
      ggplot2::facet_wrap(~Frauen_manner_alle) +
      ggplot2::geom_bar(position="stack", stat="identity") +
      ggplot2::theme_bw() +
      ggplot2::xlab("Jahre") + ggplot2::ylab("Anzahl Abschlüsse")

    if(subject == "Gesamtt"){

      plot +  ggplot2::labs(title = paste0("Gesamttzahl Abschlüsse ","(", pass_fail,") /n",
                                           "für die Jahre ", date_range[1], " bis ", date_range[2],
                                           " im Gesamtten MINT Fachebreich"),fill = "Indikator",
                                 caption = paste0("Quelle: ", unique(df$quelle)))
    }else{

      plot + ggplot2::labs(title = paste0("Gesamttzahl Abschlüsse ","(", pass_fail,")",
                                          " für die Jahre ", date_range[1], " bis ", date_range[2],
                                          " im Fachebreich ",
                                          dictionary_title_studium_abschluss[[subject]]),
                           fill = "Indikator",
                           caption = paste0("Quelle: ", unique(df$quelle)))
    }

  }else {


    df <- df %>% dplyr::filter(Frauen_manner_alle == "Gesamtt")

    plot <- ggplot2::ggplot(df, ggplot2::aes(fill=status, y=wert, x=jahr)) +
      ggplot2::geom_bar(position="stack", stat="identity") +
      ggplot2::labs(title = paste0("Anzahl der Abschlüsse \n ",
                                   "für die Jahre ", date_range[1], " bis ", date_range[2]),
                    caption = paste0("Quelle: ", unique(df$quelle))) + ggplot2::theme_bw() +
      ggplot2::xlab("Jahre") + ggplot2::ylab("Anzahl Abschlüsse")

    if(subject == "Gesamtt"){

      plot +  ggplot2::labs(title = paste0("Gesamttzahl Abschlüsse ","(", pass_fail,") /n",
                                           " für die Jahre ", date_range[1], " bis ", date_range[2],
                                           " im Gesamtten MINT Fachebreich"),fill = "Indikator",
                            caption = paste0("Quelle: ", unique(df$quelle)))
    }else{

      plot + ggplot2::labs(title = paste0("Gesamttzahl Abschlüsse ","(", pass_fail,")",
                                          " für die Jahre ", date_range[1], " bis ", date_range[2],
                                          " im Fachebreich ",fill = "Indikator",
                                          dictionary_title_studium_abschluss[[subject]]),
                           caption = paste0("Quelle: ", unique(df$quelle)))
    }

  }
}



#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @param data
#' @param r
#' @noRd

#change df to data
abschluss_aenderung <- function(data,r){

  date_range <- r$date_abschluss_1

  pass_fail <- r$durchgefallen_1

  subject <- r$ing_natwi_1

  gender_select <- r$geschlecht_abschluss_1

  indikators <- r$indikator_abschluss_1

  df <- filter_data_abschluss(data)

  df <- df %>% dplyr::filter(jahr >= date_range[1] & jahr <= date_range[2])

  df <- df %>% dplyr::filter(prüfungsstatus == pass_fail)

  df <- df %>% subset(status %in% indikators)

  if(subject == "Gesamtt"){

    df <- df %>% dplyr::group_by(status, Frauen_manner_alle, jahr, quelle) %>%
      dplyr::summarize(wert = sum(wert))

  } else if(subject == "ingenieur"){

    df <- df %>% dplyr::filter(fachbereich_alle_mint_mathe_ing == "ingenieur")

  } else {

    df <- df %>% dplyr::filter(fachbereich_alle_mint_mathe_ing == "mathe_natwi")

  }

  df <- df %>% dplyr::group_by(status, Frauen_manner_alle, quelle) %>%
    dplyr::filter((jahr == min(jahr)) | (jahr == max(jahr)))

  df <- df %>% dplyr::group_by(status, Frauen_manner_alle, quelle) %>%
    dplyr::summarize(aenderung = (dplyr::lead(wert)/wert -1) *100) %>% na.omit()

  df$comb <- stringr::str_c(df$status, " ", df$Frauen_manner_alle)

  if(gender_select == "Ja"){

    df <- df %>% dplyr::filter(Frauen_manner_alle != "Gesamtt")

    color <- ifelse(df$aenderung < 0, "pink", "lightblue")

    plot <- ggplot2::ggplot(df, ggplot2::aes(x = reorder(comb, aenderung), y = aenderung)) +
      ggplot2::geom_bar(stat = "identity",
                        show.legend = FALSE, fill = color) +
      ggplot2::geom_text(ggplot2::aes(label = paste(round(aenderung, 2),"%")),
                         hjust = ifelse(df$aenderung < 0, 0, 1),
                         vjust = 0.5) +
      ggplot2::xlab("") +
      ggplot2::ylab("prozentuale Änderung") +
      ggplot2::coord_flip() + ggplot2::theme_bw() +
      ggplot2::labs(title = paste0("Prozentual Änderung der Abschlüsse: \n ",
                                   date_range[1], " vs ", date_range[2]),
                    caption = paste0("Quelle: ", unique(df$quelle)))

    if(subject == "Gesamtt"){

      plot +  ggplot2::labs(title = paste0("Gesamttzahl Abschlüsse ","(", pass_fail,") /n",
                                           " für die Jahre ", date_range[1], " bis ", date_range[2],
                                           " im Gesamtten MINT Fachebreich"),
                            caption = paste0("Quelle: ", unique(df$quelle)))
    }else{

      plot + ggplot2::labs(title = paste0("Gesamttzahl Abschlüsse ","(", pass_fail,")",
                                          " für die Jahre ", date_range[1], " bis ", date_range[2],
                                          " im Fachebreich ",
                                          dictionary_title_studium_abschluss[[subject]]),
                           caption = paste0("Quelle: ", unique(df$quelle)))
    }


  }else {


    df <- df %>% dplyr::filter(Frauen_manner_alle == "Gesamtt")

    color <- ifelse(df$aenderung < 0, "pink", "lightblue")

    plot <- ggplot2::ggplot(df, ggplot2::aes(x = reorder(comb, aenderung), y = aenderung)) +
      ggplot2::geom_bar(stat = "identity",
                        show.legend = FALSE, fill = color) +
      ggplot2::geom_text(ggplot2::aes(label = paste(round(aenderung, 2),"%")),
                         hjust = ifelse(df$aenderung < 0, 0, 1),
                         vjust = 0.5) +
      ggplot2::xlab("prozentuale Änderung") +
      ggplot2::ylab("Kategorie") +
      ggplot2::coord_flip() + ggplot2::theme_bw() +
      ggplot2::labs(title = paste0("Prozentual Änderung der Abschlüsse: \n ",
                                   date_range[1], " vs ", date_range[2]),
                    caption = paste0("Quelle: ", unique(df$quelle)))

    if(subject == "Gesamtt"){

      plot +  ggplot2::labs(title = paste0("Gesamttzahl Abschlüsse ","(", pass_fail,") /n",
                                           " für die Jahre ", date_range[1], " bis ", date_range[2],
                                           " im Gesamtten MINT Fachebreich"),
                            caption = paste0("Quelle: ", unique(df$quelle)))
    }else{

      plot + ggplot2::labs(title = paste0("Gesamttzahl Abschlüsse ","(", pass_fail,")",
                                          " für die Jahre ", date_range[1], " bis ", date_range[2],
                                          " im Fachebreich ",
                                          dictionary_title_studium_abschluss[[subject]]),
                           caption = paste0("Quelle: ", unique(df$quelle)))
    }
  }
}

#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @param data
#' @param r
#' @noRd

comparer_plot <- function(data, r, r_abschluss,
                          r_studienzahl, r_habil){

  timestamp <- r$date_compare

  df <- filter_data_compare(data)

  df <- df %>% dplyr::filter(jahr == timestamp)

  df <- df %>% dplyr::filter(Frauen_manner_alle != "Gesamtt")

  quelle <- unique(df$quelle)

################################# Abschluss ####################################

  # Abschluss reactives
  indikator_abschluss <- r_abschluss$indikator_compare_1
  durchgefallen_abschluss <- r_abschluss$durchgefallen_compare
  subject_abschluss <- r_abschluss$ing_natwi_compare_3

  # Abschluss dataset
  df_abschluss <- df %>% subset(prüfungsstatus %in% durchgefallen_abschluss)
  df_abschluss <- df_abschluss %>% subset(status %in% indikator_abschluss)
  df_abschluss <- filter_indikator(df_abschluss, subject_abschluss, "Abschluss")

  df_abschluss <- df_abschluss %>% dplyr::group_by(prüfungsstatus, status,
                                                   fachbereich_alle_mint_mathe_ing) %>%
    dplyr::mutate(props = sum(wert))

  df_abschluss <- df_abschluss %>% dplyr::group_by(prüfungsstatus, status,Frauen_manner_alle,
                                                     fachbereich_alle_mint_mathe_ing) %>%
    dplyr::summarize(proportion = wert/props)


  col_order <- c("Frauen_manner_alle", "fachbereich_alle_mint_mathe_ing", "status",
                 "prüfungsstatus", "proportion")
  df_abschluss <- df_abschluss[, col_order]

  df_abschluss$label <- stringr::str_c("Abschlussprüfungen: ",
                                       df_abschluss$fachbereich_alle_mint_mathe_ing,
                                         " (", df_abschluss$status, ", ", df_abschluss$prüfungsstatus, ")")

  df_abschluss <- df_abschluss[, c("label", "proportion", "Frauen_manner_alle")]

################################# Studienzahl ##################################
  # Abschluss reactives
  indikator_studienzahl <- r_studienzahl$indikator_compare_2
  subject_studienzahl <- r_studienzahl$ing_natwi_compare_2

  # Studienzahl dataset
  df_studienzahl <- df %>% subset(status %in% indikator_studienzahl)
  df_studienzahl <- filter_indikator(df_studienzahl, subject_studienzahl, "Studienzahl")


  df_studienzahl <- df_studienzahl %>% dplyr::group_by(prüfungsstatus, status,
                                                     fachbereich_alle_mint_mathe_ing) %>%
    dplyr::mutate(props = sum(wert))

  df_studienzahl <- df_studienzahl %>% dplyr::group_by(prüfungsstatus, status,Frauen_manner_alle,
                                                       fachbereich_alle_mint_mathe_ing) %>%
    dplyr::summarize(proportion = wert/props)


  col_order <- c("Frauen_manner_alle", "fachbereich_alle_mint_mathe_ing", "status",
                 "prüfungsstatus", "proportion")
  df_studienzahl <- df_studienzahl[, col_order]

  df_studienzahl$label <- stringr::str_c("Studierendenzahl:",
                                         df_studienzahl$fachbereich_alle_mint_mathe_ing,
                                       " (", df_studienzahl$status, ")")

  df_studienzahl <- df_studienzahl[, c("label", "proportion", "Frauen_manner_alle")]

################################# Habilitation #################################
  # Habilitation reactives
  subject_habilitation <- r_habil$ing_natwi_compare_1

  # Habilitation dataset
  df_habil <- filter_indikator(df, subject_habilitation, "Habilitation")

  df_habil <- df_habil %>% dplyr::group_by(fachbereich_alle_mint_mathe_ing) %>%
    dplyr::mutate(props = sum(wert))

  df_habil <- df_habil %>% dplyr::group_by(prüfungsstatus,Frauen_manner_alle,
                                                       fachbereich_alle_mint_mathe_ing) %>%
    dplyr::summarize(proportion = wert/props)

  col_order <- c("Frauen_manner_alle", "fachbereich_alle_mint_mathe_ing",
                 "prüfungsstatus", "proportion")
  df_habil <- df_habil[, col_order]

  df_habil$label <- stringr::str_c("Habilitationszahl:",
                                   df_habil$fachbereich_alle_mint_mathe_ing)

  df_habil <- df_habil[, c("label", "proportion", "Frauen_manner_alle")]

################################################################################


  df <- rbind(df_abschluss,df_studienzahl,df_habil)

  Males <- df %>%
    dplyr::filter(Frauen_manner_alle == "männlich")
  Females <- df %>%
    dplyr::filter(Frauen_manner_alle == "weiblich")


  ggplot2::ggplot(df) +
    ggplot2::geom_segment(data = Males,
                          ggplot2::aes(x = proportion, y = reorder(label, proportion),
                                       group = label,
                     yend = Females$label, xend = Females$proportion),
                 color = "#aeb6bf",
                 size = 4.5,
                 alpha = .5) +
    ggplot2::geom_point(ggplot2::aes(x = proportion, y = label, color = Frauen_manner_alle),
                        size = 4, show.legend = TRUE) +
    ggplot2::ylab(" ") +
    ggplot2::xlab("prozentualer Anteil") +
    ggplot2::xlim(0,1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste0("Verhältnis von Frauen und Männern im akademischen Bereich für den Bereich MINT
                                 in ", timestamp),
                  caption = paste0("Quelle: ", quelle),
                  color='Geschlecht')


}





