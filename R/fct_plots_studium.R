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

    df <- df[with(df, order(anzeige_geschlecht, jahr, decreasing = FALSE)), ]


   if(isTRUE(switch_absolut)){


     highchart_obj(df, geschlecht, type = "normal", andere_name = "andere Studiengänge", lehramt = "Ja") %>%
       highcharter::hc_title(text = paste0("Absoluter Anteil an MINT und allen anderen Studiengängen", title_help),
                             margin = 45,
                             align = "center",
                             style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px"))


   }else{

     p <- highchart_obj(df, geschlecht, type = "percent", andere_name = "andere Studiengänge", lehramt = "Ja")

     p %>% highcharter::hc_yAxis(labels = list(format = "{value}%")) %>%
       highcharter::hc_tooltip(pointFormat = "{series.name} <br> Anteil: {point.percentage:.0f}%") %>%
       highcharter::hc_title(text = paste0("Relativer Anteil an MINT und allen anderen Studiengängen", title_help),
                             margin = 45,
                             align = "center",
                             style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px"))



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

    df <- df[with(df, order(anzeige_geschlecht, jahr, decreasing = FALSE)), ]


    if(isTRUE(switch_absolut)){

      highchart_obj(df, geschlecht, type = "normal", andere_name = "andere Studiengänge", lehramt = "Nein") %>%
        highcharter::hc_title(text = paste0("Absouluter Anteil an MINT und allen anderen Studiengängen", title_help),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px"))



    }else{

      p <- highchart_obj(df, geschlecht, type = "percent", andere_name = "andere Studiengänge", lehramt = "Nein")

      p %>% highcharter::hc_yAxis(labels = list(format = "{value}%")) %>%
        highcharter::hc_tooltip(pointFormat = "{series.name} <br> Anteil: {point.percentage:.0f}%") %>%
        highcharter::hc_title(text = paste0("Relativer Anteil an MINT und allen anderen Studiengängen", title_help),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px"))


    }

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

studienzahl_einstieg_pie <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_studierende_einstieg

  lehramt_enthalten <- r$nurLehramt_studierende_einstieg

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)


  df <- df %>% dplyr::filter(region == "Deutschland")

  df2 <- calc_share_MINT(df[(df$indikator == "Studierende"), ])

  df3 <- calc_share_MINT(df[(df$indikator == "Studienanfänger"), ])

  df2 <- calc_share_male(df2, "box_1")

  df3 <- calc_share_male(df3, "box_1")

  df <- rbind(df2, df3)


  if(isTRUE(lehramt_enthalten)){


    df <- df[!(df$nur_lehramt == "Nein" & df$hochschulform == "Uni"), ]

    df <- df[!(df$nur_lehramt == "Nein" & df$hochschulform == "FH"), ]

    # calculate share of teacher
    values <- df %>%
      dplyr::group_by(jahr, fachbereich, anzeige_geschlecht, indikator) %>%
      dplyr::mutate(wert = wert - dplyr::lead(wert)) %>% na.omit()


    df[df$hochschulform == "insgesamt", "wert"] <- values$wert


    df[df$nur_lehramt == "Ja", "fachbereich"] <- interaction(df[df$nur_lehramt == "Ja", "fachbereich"][[1]], " (Lehramt)", sep = "")


    df_studierende <- df %>% dplyr::filter(indikator == "Studierende")

    df_studierende <- df_studierende %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

    df_studierende$props <- sum(df_studierende$wert)

    df_studierende <- df_studierende %>% dplyr::group_by(fachbereich, anzeige_geschlecht) %>%
      dplyr::summarize(proportion = wert/props)

    df_studierende$proportion <- df_studierende$proportion * 100

    df_studierende$proportion <- round_preserve_sum(as.numeric(df_studierende$proportion),0)

    df_studierende$anzeige_geschlecht <- paste0(df_studierende$anzeige_geschlecht, " (", df_studierende$fachbereich, ")")


    df_anfaenger <- df %>% dplyr::filter(indikator == "Studienanfänger")

    df_anfaenger <- df_anfaenger %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

    df_anfaenger$props <- sum(df_anfaenger$wert)

    df_anfaenger <- df_anfaenger %>% dplyr::group_by(fachbereich, anzeige_geschlecht) %>%
      dplyr::summarize(proportion = wert/props)

    df_anfaenger$proportion <- df_anfaenger$proportion * 100

    df_anfaenger$proportion <- round_preserve_sum(as.numeric(df_anfaenger$proportion),0)

    df_anfaenger$anzeige_geschlecht <- paste0(df_anfaenger$anzeige_geschlecht, " (", df_anfaenger$fachbereich, ")")


    highcharter::hw_grid(
      highcharter::hchart(df_studierende, size = 280, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.1f}%')) %>%
        highcharter::hc_colors(c("#154194", 'rgba(21, 65, 148, 0.75)','rgba(21, 65, 148, 0.5)', 'rgba(21, 65, 148, 0.25)',
                                 "#b16fab", 'rgba(177, 111, 171, 0.75)',  'rgba(177, 111, 171, 0.50)', 'rgba(177, 111, 171, 0.25)')) %>%
        highcharter::hc_title(text = paste0("Anteil von MINT an allen anderen Studienfächern für Studierende in ", timerange),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, align = "left", verticalAlign = 'middle') %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),


      highcharter::hchart(df_anfaenger, size = 280, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.1f}%')) %>%
        highcharter::hc_colors(c("#154194", 'rgba(21, 65, 148, 0.75)','rgba(21, 65, 148, 0.5)', 'rgba(21, 65, 148, 0.25)',
                                 "#b16fab", 'rgba(177, 111, 171, 0.75)',  'rgba(177, 111, 171, 0.50)', 'rgba(177, 111, 171, 0.25)')) %>%
        highcharter::hc_title(text = paste0("Anteil von MINT an allen anderen Studienfächern für Studienanfänger in ", timerange),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, align = "left", verticalAlign = 'middle') %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE)),

      ncol = 1,
      browsable = TRUE
    )


  }else{

    df <- df %>% dplyr::filter(nur_lehramt != "Ja")

    df <- df %>% dplyr::filter(hochschulform == "insgesamt")

    df <- df[with(df, order(indikator, anzeige_geschlecht, jahr, decreasing = TRUE)), ]

    df_studierende <- df %>% dplyr::filter(indikator == "Studierende")

    df_studierende <- df_studierende %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

    df_studierende$props <- sum(df_studierende$wert)

    df_studierende <- df_studierende %>% dplyr::group_by(fachbereich, anzeige_geschlecht) %>%
      dplyr::summarize(proportion = wert/props)

    df_studierende$proportion <- df_studierende$proportion * 100

    df_studierende$proportion <- round_preserve_sum(as.numeric(df_studierende$proportion),0)

    df_studierende$anzeige_geschlecht <- paste0(df_studierende$anzeige_geschlecht, " (", df_studierende$fachbereich, ")")


    df_anfaenger <- df %>% dplyr::filter(indikator == "Studienanfänger")

    df_anfaenger <- df_anfaenger %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

    df_anfaenger$props <- sum(df_anfaenger$wert)

    df_anfaenger <- df_anfaenger %>% dplyr::group_by(fachbereich, anzeige_geschlecht) %>%
      dplyr::summarize(proportion = wert/props)

    df_anfaenger$proportion <- df_anfaenger$proportion * 100

    df_anfaenger$proportion <- round_preserve_sum(as.numeric(df_anfaenger$proportion),0)

    df_anfaenger$anzeige_geschlecht <- paste0(df_anfaenger$anzeige_geschlecht, " (", df_anfaenger$fachbereich, ")")


    highcharter::hw_grid(
      highcharter::hchart(df_studierende, size = 280, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.1f}%')) %>%
        highcharter::hc_colors(c("#154194", 'rgba(21, 65, 148, 0.6)',
                                 "#b16fab", 'rgba(177, 111, 171, 0.6)')) %>%
        highcharter::hc_title(text = paste0("Anteil von MINT an allen anderen Studienfächern für Studierende in ", timerange),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE) %>%
        #highcharter::hc_legend(enabled = TRUE, align = "center", layout = "horizontal") %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),


      highcharter::hchart(df_anfaenger, size = 280, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.1f}%')) %>%
        highcharter::hc_colors(c("#154194", 'rgba(21, 65, 148, 0.6)',
                                 "#b16fab", 'rgba(177, 111, 171, 0.6)')) %>%
        highcharter::hc_title(text = paste0("Anteil von MINT an allen anderen Studienfächern für Studienanfänger in ", timerange),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
       # highcharter::hc_legend(enabled = TRUE, align = "center", layout = "horizontal") %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE)),

      ncol = 2,
      browsable = TRUE
    )



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

  #status_studierende <- r$indikator_studierende_einstieg

  #geschlecht <- r$geschlecht_studierende_einstieg

  lehramt_enthalten <- r$nurLehramt_studierende_einstieg

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(region == "Deutschland")

  # call function to calculate the share of MINT and the remaining subjects
  df <- calc_share_MINT(df)

  # calculate the share of males
  df <- calc_share_male(df, "box_1")

  # filter gender

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
  timerange <- r$date_studierende

  lehramt <- r$nurLehramt_studierende

  hochschulform_select_1 <- r$hochschulform_studierende_1

  hochschulform_select_2 <- r$hochschulform_studierende_2

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)


  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  }

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- df %>% dplyr::filter(fachbereich != "Alle")

  # calculate the share of males
  df <- calc_share_male(df, "box_2")

  x_studierende <- prep_studium_proportion(df[df$indikator == "Studierende",])

  x_studienanfaenger <- prep_studium_proportion(df[df$indikator == "Studienanfänger",])


  x_studierende <- x_studierende[order(factor(names(x_studierende), levels = c('Frauen (Ingenieur)', 'Frauen (Mathe)',
                                                                               'Männer (Ingenieur)', 'Männer (Mathe)')))]

  x_studienanfaenger <- x_studienanfaenger[order(factor(names(x_studienanfaenger),
                                                   levels = c('Frauen (Ingenieur)', 'Frauen (Mathe)',
                                                              'Männer (Ingenieur)', 'Männer (Mathe)')))]




  # create plot objects for waffle charts
  waffle_studierende <- waffle::waffle(x_studierende, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", "**Studierende**</span>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "bottom") +
    ggplot2::scale_fill_manual(
      values =  c("#ee7775",
                  "#f5adac4D",
                  "#23262F",
                  '#b1b5c3'),
      guide = ggplot2::guide_legend(reverse = TRUE),
      labels = c(
        paste0("Frauen (Ingenieur)",", ",x_studierende[1], "%"),
        paste0("Frauen (Mathe)",", ",x_studierende[2], "%"),
        paste0("Männer (Ingenieur)",", ",x_studierende[3], "%"),
        paste0("Männer (Mathe)",", ",x_studierende[4], "%"))) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=4,byrow=TRUE))



  waffle_studienanfaenger <- waffle::waffle(x_studienanfaenger, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", "**Studienanfänger**</span>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "bottom") +
    ggplot2::scale_fill_manual(
        values =  c("#ee7775",
                    "#f5adac4D",
                    "#23262F",
                    '#b1b5c3'),
        guide = ggplot2::guide_legend(reverse = TRUE),
        labels = c(
          paste0("Frauen (Ingenieur)",", ",x_studienanfaenger[1], "%"),
          paste0("Frauen (Mathe)",", ",x_studienanfaenger[2], "%"),
          paste0("Männer (Ingenieur)",", ",x_studienanfaenger[3], "%"),
          paste0("Männer (Mathe)",", ",x_studienanfaenger[4], "%"))) +
      ggplot2::guides(fill=ggplot2::guide_legend(nrow=4,byrow=TRUE))




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



  title_help <- paste0(title_help_sub, title_help_sub_sub)




  plot <- ggpubr::ggarrange(waffle_studienanfaenger, NULL ,
                            waffle_studierende, widths = c(1, 0.1, 1), nrow=1)
  text <- c(
    paste0("<span style='font-size:20.5pt; color:black'> Anteil von Frauen und Männern an MINT", title_help,
           " in ",timerange))
  ggpubr::annotate_figure(plot, gridtext::richtext_grob(text = text))


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

studienzahl_waffle_alternative <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_studierende

  lehramt <- r$nurLehramt_studierende

  hochschulform_select_1 <- r$hochschulform_studierende_1

  hochschulform_select_2 <- r$hochschulform_studierende_2

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)


  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  }

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- df %>% dplyr::filter(fachbereich != "Alle")

  # calculate the share of males
  df <- calc_share_male(df, "box_2")

  x_studierende <- prep_studium_proportion(df[df$indikator == "Studierende",])

  x_studienanfaenger <- prep_studium_proportion(df[df$indikator == "Studienanfänger",])


  x_studierende <- x_studierende[order(factor(names(x_studierende), levels = c('Frauen (Ingenieur)', 'Frauen (Mathe)',
                                                                               'Männer (Ingenieur)', 'Männer (Mathe)')))]

  x_studienanfaenger <- x_studienanfaenger[order(factor(names(x_studienanfaenger),
                                                        levels = c('Frauen (Ingenieur)', 'Frauen (Mathe)',
                                                                   'Männer (Ingenieur)', 'Männer (Mathe)')))]




  # create plot objects for waffle charts
  waffle_studierende_female <- waffle::waffle(x_studierende, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", "**Studierende**</span>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "right") +
    ggplot2::scale_fill_manual(
      values =  c("#00a87a",
                  "#fcc433",
                  "#b1b5c3",
                  '#b1b5c3'),
      na.value="#b1b5c3",
      limits = c("Frauen (Ingenieur)", "Frauen (Mathe)", "Männer (Ingenieur)"),
      guide = ggplot2::guide_legend(reverse = TRUE),
      labels = c(
        paste0("Frauen (Ingenieur)",", ",x_studierende[1], "%"),
        paste0("Frauen (Mathe)",", ",x_studierende[2], "%"),
        paste0("Männer (MINT)",", ",x_studierende[3] + x_studierende[4], "%"))) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=4,byrow=TRUE))



  waffle_studienanfaenger_female <- waffle::waffle(x_studienanfaenger, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", "**Studienanfänger**</span>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "left") +
    ggplot2::scale_fill_manual(
      values =  c("#00a87a",
                  "#fcc433",
                  "#b1b5c3",
                  '#b1b5c3'),
      na.value="#b1b5c3",
      limits = c("Frauen (Ingenieur)", "Frauen (Mathe)", "Männer (Ingenieur)"),
      guide = ggplot2::guide_legend(reverse = TRUE),
      labels = c(
        paste0("Frauen (Ingenieur)",", ",x_studienanfaenger[1], "%"),
        paste0("Frauen (Mathe)",", ",x_studienanfaenger[2], "%"),
        paste0("Männer (MINT)",", ",x_studienanfaenger[3] + x_studienanfaenger[4], "%"))) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=4,byrow=TRUE))



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



  title_help <- paste0(title_help_sub, title_help_sub_sub)

  plot <- ggpubr::ggarrange(waffle_studienanfaenger_female, NULL ,waffle_studierende_female,
                            widths = c(1, 0.1, 1), nrow=1)

  text <- c(
    paste0("<span style='font-size:20.5pt; color:black'> Anteil von Frauen und Männern an MINT ", title_help,
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

  timerange <- r$date_studierende

  lehramt <- r$nurLehramt_studierende

  hochschulform_select_1 <- r$hochschulform_studierende_1

  hochschulform_select_2 <- r$hochschulform_studierende_2


  df <- df %>% dplyr::filter(jahr == timerange)


  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  }


  df <- df %>% dplyr::filter(region == "Deutschland")


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


  title_help <- paste0(title_help_sub, title_help_sub_sub)


  df <- df %>% dplyr::filter(fachbereich != "Alle")

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
    ggplot2::scale_fill_manual(values = c("#00a87a", "#fcc433")) +
    ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
                                 "Frauen und Männer in MINT und allen anderen Studienfächern ",title_help,
                                 " in ", timerange,
                                 "<br><br><br>"),
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

  timerange <- r$date_studierende

  lehramt <- r$nurLehramt_studierende

  hochschulform_select_1 <- r$hochschulform_studierende_1

  hochschulform_select_2 <- r$hochschulform_studierende_2

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

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

  values_studierende <- (df[(df$anzeige_geschlecht == "Frauen" & df$indikator == "Studierende"),
                "wert"]/df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Studierende"), "wert"])*100

  values_studierende$region <- df[(df$anzeige_geschlecht == "Frauen" & df$indikator == "Studierende"), "region"][[1]]

  values_studienanfaenger <- (df[(df$anzeige_geschlecht == "Frauen" & df$indikator == "Studienanfänger"),
                            "wert"]/df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Studienanfänger"), "wert"])*100

  values_studienanfaenger$region <- df[(df$anzeige_geschlecht == "Frauen" & df$indikator == "Studienanfänger"), "region"][[1]]


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


  title_help <- paste0(title_help_sub, title_help_sub_sub)


  highcharter::hw_grid(
    # plot
    highcharter::hcmap(
      "countries/de/de-all",
      data = values_studierende,
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
        text = paste0("Anteil der Studentinnen <br>",title_help ," an MINT in ", timerange),
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
      data = values_studienanfaenger,
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
        text = paste0("Anteil der Studienanfängerinnen <br> ",title_help ," an MINT in ", timerange),
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
#' @description A function to create a dataframe for given filter
#'
#' @return The return value is a dataframe
#' @param df The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

data_mix_studium <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_studierende

  lehramt <- r$nurLehramt_studierende

  hochschulform_select_1 <- r$hochschulform_studierende_1

  hochschulform_select_2 <- r$hochschulform_studierende_2

  df <- df %>% dplyr::filter(jahr == timerange)


  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  }



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

  subject_aggregated <- r$subjects_aggregated

  subjects_select <- r$subject_selected

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == status_studierende)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(region != "Baden-Württemberg")


  df <- prep_studierende_east_west(df)


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


  values <-  df %>%
    dplyr::group_by(jahr, fachbereich, region) %>%
    dplyr::summarise(props = wert[anzeige_geschlecht == "Frauen"] /
                    wert[anzeige_geschlecht == "Gesamt"])


  if (subject_aggregated == "aggregiert"){
    # filter MINT or remaining subjects
    values <- values %>% dplyr::filter(fachbereich == topic)
  }

  # order years for plot
  values <- values[with(values, order(region, jahr, decreasing = FALSE)), ]

  values$props <- values$props * 100

  values <- values %>% dplyr::filter(region %in% states)

  # plot
  highcharter::hchart(values, 'line', highcharter::hcaes(x = jahr, y = round(props), group = region)) %>%
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



#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_verlauf_bl <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_studierende_verlauf_bl

  lehramt <- r$nurLehramt_studierende_verlauf_bl

  states <- r$states_studierende_verlauf_bl

  topic <- r$topic_studierende_verlauf_bl

  subject_aggregated <- r$subjects_aggregated_bl

  subjects_select <- r$subject_selected_bl

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(region != "Baden-Württemberg")


  df <- prep_studierende_east_west(df)


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
    df <- calc_share_MINT_bl(df)

    if (topic == "MINT"){

      title_help_sub <- "an MINT"

    } else {

      title_help_sub <- "an anderen Studienfächern"

    }

  }else {


    df <- df %>% dplyr::filter(fachbereich %in% subjects_select)

    title_help_sub <- dictionary_title_studium[[subjects_select]]
  }


  df <- calc_share_male_bl(df)

  title_help <- paste0(title_help_sub, title_help_sub_sub)

  df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  values <- df %>% dplyr::group_by(region, indikator, fachbereich, anzeige_geschlecht, jahr) %>%
    dplyr::summarize(proportion = wert/props)


  if (subject_aggregated == "aggregiert"){
    # filter MINT or remaining subjects
    values <- values %>% dplyr::filter(fachbereich == topic)
  }

  # order years for plot
  values <- values[with(values, order(region, jahr, decreasing = FALSE)), ]

  values$proportion <- values$proportion * 100

  values <- values %>% dplyr::filter(region %in% states)

  values$anzeige_geschlecht <- paste0(values$anzeige_geschlecht, " (", values$indikator, ")")

  # plot
  highcharter::hchart(values, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = anzeige_geschlecht)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil {point.anzeige_geschlecht} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil von Student*innen ", title_help ," im Verlauf"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    )


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





