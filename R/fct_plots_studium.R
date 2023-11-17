# Wer wählt MINT ----

#' A function to plot a graph.
#'
#' @description A function to create a pie chart for the first box
#' inside the tab "Schule".
#'
#' @return The return value is a plot
#' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd


studienzahl_test <- function(r){


# Box 1 Pie ab 2023

 # ui inputs
 testy1 <- r$testy
 testl1 <- r$testl

 # filtering
 df <- dplyr::tbl(con, from = "studierende") %>%
   dplyr::filter(jahr == testy1,
                 geschlecht == "Gesamt",
                 region == "Deutschland",
                 fachbereich %in% c("Nicht MINT", "MINT (Gesamt)", "Alle" )) %>%
   dplyr::collect()

 # calculating proportions
 df3 <- df %>%
   tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
   dplyr::mutate(dplyr::across(c("MINT (Gesamt)", "Nicht MINT"), ~./Alle))%>%
   dplyr::mutate(dplyr::across(c("Nicht MINT", "MINT (Gesamt)"), ~ round(.*100,1)))%>%
   dplyr::select(- Alle)%>%
   tidyr::pivot_longer(c("MINT (Gesamt)", "Nicht MINT"), names_to = "fachbereich", values_to = "proportion")

 # joining wert and prportion
 df4 <- df %>%
   dplyr::filter(fachbereich != "Alle")%>%
   dplyr::left_join(df3)


 #Trennpunkte für lange Zahlen ergänzen

 df4$wert <- prettyNum(df4$wert, big.mark = ".", decimal.mark = ",")


 # Ordering
 df4 <- within(df4, fachbereich <- factor(fachbereich, levels=c("Nicht MINT", "MINT (Gesamt)")))





  if(length(testl1) == 1) {

    df_pie <- df4 %>% dplyr::filter(indikator == testl1)

    #df_pie <- within(df_pie, fachbereich <- factor(fachbereich, levels=c("Nicht MINT", "MINT")))

    highcharter::hw_grid(

      df_pie %>%
        highcharter::hchart(
          "pie", highcharter::hcaes(x = fachbereich , y = proportion)
        )
      %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#b16fab" )) %>%
        highcharter::hc_title(text = paste0(testl1[1], " in ", testy1),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),

      ncol = 1,
      browsable = TRUE)



  } else if(length(testl1) == 2) {

    # filter for UI input and ensure proportions sum to 1
    df_1_pie <- df4 %>% dplyr::filter(indikator == testl1[1])

    df_2_pie <- df4 %>% dplyr::filter(indikator == testl1[2])


     highcharter::hw_grid(
       highcharter::hchart(df_1_pie, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion))

      %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
        highcharter::hc_title(text=paste0(testl1[1], " in ", testy1),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),


    highcharter:: hchart(df_2_pie, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich , y = proportion))

    %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage}% <br> Anzahl: {point.wert}'))%>%
        highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
      highcharter::hc_title(text=paste0(testl1[2], " in ", testy1),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE)),

      ncol = 2,
      browsable = TRUE
    )


  } else if(length(testl1) == 3) {

    # filter for UI input and ensure proportions sum to 1

    df_1_pie <- df4 %>% dplyr::filter(indikator == testl1[1])

    df_2_pie <- df4 %>% dplyr::filter(indikator == testl1[2])

    df_3_pie <- df4 %>% dplyr::filter(indikator == testl1[3])


     highcharter::hw_grid(
       highcharter::hchart(df_1_pie, size = 170, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion))
      %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
        highcharter::hc_title(text=paste0(testl1[1], " in ", testy1),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),


    highcharter::hchart(df_2_pie, size = 170, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion))


    %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
      highcharter::hc_title(text=paste0(testl1[2], " in ", testy1),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE)),

    highcharter::hchart(df_3_pie, size = 170, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion))


    %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
      highcharter::hc_title(text=paste0(testl1[3], " in ", testy1),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),



      ncol = 3,
      browsable = TRUE
    )



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

studienzahl_waffle_mint <- function(r) {

  # load UI inputs from reactive value
  timerange <- r$waffle_y

  label_w <- r$waffle_l


  # filter dataset based on UI inputs
  df <- dplyr::tbl(con, from = "studierende") %>%
    dplyr::filter(jahr == timerange,
                  region== "Deutschland",
                  geschlecht== "Gesamt")%>%
    dplyr::select(-region, -geschlecht, - jahr)%>%
    dplyr::collect()

  df <- df %>%
    tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
    dplyr::mutate(pro_mathe = `Mathematik, Naturwissenschaften`/Alle,
                  pro_nicht = `Nicht MINT`/Alle,
                  pro_ing = Ingenieurwissenschaften/Alle)%>%
    dplyr::select(indikator, pro_ing, pro_mathe, pro_nicht)

  # Überschriften vorbereiten
  überschrift_label <- function(label) {
    label <- ifelse(label == "Studienanfänger:innen (1.Fachsemester)", "Studienanfänger:innen <br> (1. Fachsemester)", label)
    label <- ifelse(label == "Studienanfänger:innen (1.Hochschulsemester)", "Studienanfänger:innen <br> (1. Hochschulsemester)" , label)
    label <- ifelse(label == "Studienanfänger:innen (Fachhochschulen, 1.Fachsemester)", "Studienanfänger:innen <br> (Fachhochschule, 1. Fachsemester)" , label)
    label <- ifelse(label == "Studienanfänger:innen (Fachhochschulen, 1.Hochschulsemester)", "Studienanfänger:innen <br> (Fachhochschule, 1. Hochschulsemester)" , label)
    label <- ifelse(label == "Studienanfänger:innen (Lehramt, Universität, 1.Fachsemester)", "Studienanfänger:innen <br> (Lehramt, Universität, 1. Fachsemester)" , label)
    label <- ifelse(label == "Studienanfänger:innen (Lehramt, Universität, 1.Hochschulsemester)", "Studienanfänger:innen <br> (Lehramt, Universität, 1. Hochschulsemester)" , label)
    label <- ifelse(label == "Studienanfänger:innen (Universität, 1.Fachsemester)", "Studienanfänger:innen <br> (Universität, 1. Fachsemester)" , label)
    label <- ifelse(label == "Studienanfänger:innen (Universität, 1.Hochschulsemester)", "Studienanfänger:innen <br> (Universität, 1. Hochschulsemester)" , label)
    label <- ifelse(label == "Studierende (Fachhochschulen)", "Studierende <br> (Fachhochschulen)" , label)
    label <- ifelse(label == "Studierende (Lehramt, Universität)", "Studierende <br> (Lehramt, Universität)" , label)
    label <- ifelse(label == "Studierende (Universität)", "Studierende <br> (Universität)" , label)
    return(label)
  }

  if(length(label_w)==1){

    waf_1 <- df %>%
      dplyr::filter(indikator==label_w)

    label_w <- überschrift_label(label_w)
    title_1 <- as.character(label_w)
    data_1 <- as.numeric(as.vector(waf_1[1,2:ncol(waf_1)]))
    data_1 <- round(data_1 * 100,0)
    names(data_1) <- colnames(waf_1[2:ncol(waf_1)])

    data_k <- data_1





    waffle_a <- waffle::waffle(data_1, keep = FALSE)+
      ggplot2::labs(
        fill = "",
        title = paste0("<span style='color:black;'>", title_1, "<br>", timerange, "<br>", sep='\n')) + ###Von mir. Gemeint: Tatsächlich alle Smester? kab
      ggplot2::theme(plot.title = ggtext::element_markdown(),
                     plot.subtitle = ggtext::element_markdown(),
                     text = ggplot2::element_text(size = 14),
                     plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                     legend.position = "bottom") +
      ggplot2::scale_fill_manual(
        values =  c("#35bd97",
                    "#fbbf24",
                    '#8893a7'),
        na.value='#8893a7',
        limits = c("pro_ing" ,  "pro_mathe" ,"pro_nicht"),
        guide = ggplot2::guide_legend(reverse = TRUE),
        labels = c(
          paste0("Ingenieurwissenschaften",", ",data_1[1], "%"),
          paste0("Mathematik/Naturwissenschaften",", ",data_1[2], "%"),
          paste0("andere Studiengänge",", ",data_1[3], "%"))) +
      ggplot2::guides(fill=ggplot2::guide_legend(nrow=3,byrow=TRUE))

    ggpubr::ggarrange(NULL, waffle_a ,NULL,
                      widths = c(1, 1, 1), nrow=1)

  } else if(length(label_w)==2){

    waf_1 <- df %>%
      dplyr::filter(indikator==label_w[1])

    waf_2 <- df %>%
      dplyr::filter(indikator==label_w[2])

    waf_1[1,1] <- überschrift_label(waf_1[1,1])
    title_1 <- as.character(as.vector(waf_1[1,1]))
    data_1 <- as.numeric(as.vector(waf_1[1,2:ncol(waf_1)]))
    data_1 <- round(data_1 * 100,0)

    names(data_1) <- colnames(waf_1[2:ncol(waf_1)])

    waf_2[1,1] <- überschrift_label(waf_2[1,1])
    title_2 <- as.character(as.vector(waf_2[1,1]))
    data_2 <- as.numeric(as.vector(waf_2[1,2:ncol(waf_2)]))
    data_2 <- round(data_2 * 100,0)

    names(data_2) <- colnames(waf_2[2:ncol(waf_2)])



    waffle_a <- waffle::waffle(data_1, keep = FALSE)+
      ggplot2::labs(
        fill = "",
        title = paste0("<span style='color:black;'>", title_1, "<br>", timerange, "<br>", sep='\n')) +
      ggplot2::theme(plot.title = ggtext::element_markdown(),
                     plot.subtitle = ggtext::element_markdown(),
                     text = ggplot2::element_text(size = 14),
                     plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                     legend.position = "bottom") +
      ggplot2::scale_fill_manual(
        values =  c("#35bd97",
                    "#fbbf24",
                    '#8893a7'),
        na.value='#8893a7',
        limits = c("pro_ing" ,  "pro_mathe" ,"pro_nicht"),
        guide = ggplot2::guide_legend(reverse = TRUE),
        labels = c(
          paste0("Ingenieurwissenschaften",", ",data_1[1], "%"),
          paste0("Mathematik/Naturwissenschaften",", ",data_1[2], "%"),
          paste0("andere Studiengänge",", ",data_1[3], "%"))) +
      ggplot2::guides(fill=ggplot2::guide_legend(nrow=3,byrow=TRUE))

    waffle_b <- waffle::waffle(data_2, keep = FALSE)+
      ggplot2::labs(
        fill = "",
        title = paste0("<span style='color:black;'>", title_2, "<br>", timerange, "<br>", sep='\n')) +
      ggplot2::theme(plot.title = ggtext::element_markdown(),
                     plot.subtitle = ggtext::element_markdown(),
                     text = ggplot2::element_text(size = 14),
                     plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                     legend.position = "bottom") +
      ggplot2::scale_fill_manual(
        values =  c("#35bd97",
                    "#fbbf24",
                    "#8893a7"),
        na.value="#8893a7",
        limits = c("pro_ing" ,  "pro_mathe" ,"pro_nicht"),
        guide = ggplot2::guide_legend(reverse = TRUE),
        labels = c(
          paste0("Ingenieurwissenschaften",", ",data_2[1], "%"),
          paste0("Mathematik/Naturwissenschaften",", ",data_2[2], "%"),
          paste0("andere Studiengänge",", ",data_2[3], "%"))) +
      ggplot2::guides(fill=ggplot2::guide_legend(nrow=3,byrow=TRUE))



    ggpubr::ggarrange(waffle_a, NULL ,waffle_b,
                      widths = c(1, 0.1, 1), nrow=1)

  } else if (length(label_w)==3){

    waf_1 <- df %>%
      dplyr::filter(indikator==label_w[1])

    waf_2 <- df %>%
      dplyr::filter(indikator==label_w[2])

    waf_3 <- df %>%
      dplyr::filter(indikator==label_w[3])

    waf_1[1,1] <- überschrift_label(waf_1[1,1])
    title_1 <- as.character(as.vector(waf_1[1,1]))
    data_1 <- as.numeric(as.vector(waf_1[1,2:ncol(waf_1)]))
    data_1 <- round(data_1 * 100,0)
    names(data_1) <- colnames(waf_1[2:ncol(waf_1)])

    waf_2[1,1] <- überschrift_label(waf_2[1,1])
    title_2 <- as.character(as.vector(waf_2[1,1]))
    data_2 <- as.numeric(as.vector(waf_2[1,2:ncol(waf_2)]))
    data_2 <- round(data_2 * 100,0)
    names(data_2) <- colnames(waf_2[2:ncol(waf_2)])

    waf_3[1,1] <- überschrift_label(waf_3[1,1])
    title_3 <- as.character(as.vector(waf_3[1,1]))
    data_3 <- as.numeric(as.vector(waf_3[1,2:ncol(waf_3)]))
    data_3 <- round(data_3 * 100,0)
    names(data_3) <- colnames(waf_3[2:ncol(waf_3)])



    waffle_a <- waffle::waffle(data_1, keep = FALSE)+
      ggplot2::labs(
        fill = "",
        title = paste0("<span style='color:black;'>", title_1, "<br>", timerange, "<br>", sep='\n')) + ###Von mir. Gemeint: Tatsächlich alle Smester? kab
      ggplot2::theme(plot.title = ggtext::element_markdown(),
                     plot.subtitle = ggtext::element_markdown(),
                     text = ggplot2::element_text(size = 14),
                     plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                     legend.position = "bottom") +
      ggplot2::scale_fill_manual(
        values =  c("#35bd97",
                    "#fbbf24",
                    "#8893a7"),
        na.value="#8893a7",
        limits = c("pro_ing" ,  "pro_mathe" ,"pro_nicht"),
        guide = ggplot2::guide_legend(reverse = TRUE),
        labels = c(
          paste0("Ingenieurwissenschaften",", ",data_1[1], "%"),
          paste0("Mathematik/Naturwissenschaften",", ",data_1[2], "%"),
          paste0("andere Studiengänge",", ",data_1[3], "%"))) +
      ggplot2::guides(fill=ggplot2::guide_legend(nrow=3,byrow=TRUE))

    waffle_b <- waffle::waffle(data_2, keep = FALSE)+
      ggplot2::labs(
        fill = "",
        title = paste0("<span style='color:black;'>", title_2, "<br>", timerange, "<br>", sep='\n')) +
      ggplot2::theme(plot.title = ggtext::element_markdown(),
                     plot.subtitle = ggtext::element_markdown(),
                     text = ggplot2::element_text(size = 14),
                     plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                     legend.position = "bottom") +
      ggplot2::scale_fill_manual(
        values =  c("#35bd97",
                    "#fbbf24",
                    "#8893a7"),
        na.value="#8893a7",
        limits = c("pro_ing" ,  "pro_mathe" ,"pro_nicht"),
        guide = ggplot2::guide_legend(reverse = TRUE),
        labels = c(
          paste0("Ingenieurwissenschaften",", ",data_2[1], "%"),
          paste0("Mathematik/Naturwissenschaften",", ",data_2[2], "%"),
          paste0("andere Studiengänge",", ",data_2[3], "%"))) +
      ggplot2::guides(fill=ggplot2::guide_legend(nrow=3,byrow=TRUE))

    waffle_c <- waffle::waffle(data_3, keep = FALSE)+
      ggplot2::labs(
        fill = "",
        title = paste0("<span style='color:black;'>", title_3, "<br>", timerange, "<br>")) +
      ggplot2::theme(plot.title = ggtext::element_markdown(),
                     plot.subtitle = ggtext::element_markdown(),
                     text = ggplot2::element_text(size = 14),
                     plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                     legend.position = "bottom") +
      ggplot2::scale_fill_manual(
        values =  c("#35bd97",
                    "#fbbf24",
                    "#8893a7"),
        na.value="#8893a7",
        limits = c("pro_ing" ,  "pro_mathe" ,"pro_nicht"),
        guide = ggplot2::guide_legend(reverse = TRUE),
        labels = c(
          paste0("Ingenieurwissenschaften",", ",data_3[1], "%"),
          paste0("Mathematik/Naturwissenschaften",", ",data_3[2], "%"),
          paste0("andere Studiengänge",", ",data_3[3], "%"))) +
      ggplot2::guides(fill=ggplot2::guide_legend(nrow=3,byrow=TRUE))



    ggpubr::ggarrange(waffle_a ,waffle_b, waffle_c,
                      widths = c(1, 1, 1), nrow=1)






  }


  # if(lehramt == FALSE){
  #
  #   df <- df %>% dplyr::filter(nur_lehramt == "Nein")
  #
  #   df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
  #
  # } else {
  #
  #   df <- df %>% dplyr::filter(nur_lehramt == "Ja")
  #
  #   df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  # }
  #
  # df <- df %>% dplyr::filter(region == "Deutschland")
  #
  # df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")


  # df_studierende <- df %>% dplyr::filter(indikator == "Studierende")
  #
  # x_studierende <- calc_share_waffle(df_studierende)
  #
  #
  #
  # df_anfaenger <- df %>% dplyr::filter(indikator == "Studienanfänger:innen")
  #
  # x_studienanfaenger <- calc_share_waffle(df_anfaenger)
  #
  #
  # # set order
  # x_studierende <- x_studierende[order(factor(names(x_studierende), levels = c('Ingenieurwissenschaften', 'Mathematik/Naturwissenschaften',
  #                                                                              'andere Studiengänge')))]
  #
  # x_studienanfaenger <- x_studienanfaenger[order(factor(names(x_studienanfaenger),
  #                                                       levels = c('Ingenieurwissenschaften', 'Mathematik/Naturwissenschaften',
  #                                                                  'andere Studiengänge')))]


  # create plot objects for waffle charts
  # waffle_studierende <- waffle::waffle(x_studierende, keep = FALSE) +
  #   ggplot2::labs(
  #     fill = "",
  #     title = paste0("<span style='color:black;'>", "Studienfachwahl (Studierende) <br>", timerange, "<br>")) + ###Von mir. Gemeint: Tatsächlich alle Smester? kab
  #   ggplot2::theme(plot.title = ggtext::element_markdown(),
  #                  plot.subtitle = ggtext::element_markdown(),
  #                  text = ggplot2::element_text(size = 14),
  #                  plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
  #                  legend.position = "bottom") +
  #   ggplot2::scale_fill_manual(
  #     values =  c("#00a87a",
  #                 "#fcc433",
  #                 '#b1b5c3'),
  #     na.value="#b1b5c3",
  #     limits = c("Ingenieurwissenschaften", "Mathematik/Naturwissenschaften", "andere Studiengänge"),
  #     guide = ggplot2::guide_legend(reverse = TRUE),
  #     labels = c(
  #       paste0("Ingenieurwissenschaften",", ",x_studierende[1], "%"),
  #       paste0("Mathematik/Naturwissenschaften",", ",x_studierende[2], "%"),
  #       paste0("andere Studiengänge",", ",x_studierende[3], "%"))) +
  #   ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))
  #
  #
  #
  # waffle_studienanfaenger <- waffle::waffle(x_studienanfaenger, keep = FALSE) +
  #   ggplot2::labs(
  #     fill = "",
  #     title = paste0("<span style='color:black;'>", "Studienfachwahl (Studienanfänger:innen)</span> <br>", timerange, "<br>")) +
  #   ggplot2::theme(plot.title = ggtext::element_markdown(),
  #                  plot.subtitle = ggtext::element_markdown(),
  #                  text = ggplot2::element_text(size = 14),
  #                  plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
  #                  legend.position = "bottom") +
  #   ggplot2::scale_fill_manual(
  #     values =  c("#00a87a",
  #                 "#fcc433",
  #                 '#b1b5c3'),
  #     na.value="#b1b5c3",
  #     limits = c("Ingenieurwissenschaften", "Mathematik/Naturwissenschaften", "andere Studiengänge"),
  #     guide = ggplot2::guide_legend(reverse = TRUE),
  #     labels = c(
  #       paste0("Ingenieurwissenschaften",", ",x_studienanfaenger[1], "%"),
  #       paste0("Mathematik/Naturwissenschaften",", ",x_studienanfaenger[2], "%"),
  #       paste0("andere Studiengänge",", ",x_studienanfaenger[3], "%"))) +
  #   ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))
  #
  #
  #
  # ggpubr::ggarrange(waffle_studienanfaenger, NULL ,waffle_studierende,
  #                           widths = c(1, 0.1, 1), nrow=1)



}



#' A function to plot time series
#'
#' @description A function to plot the time series
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_verlauf_single <- function(r) {

  # load UI inputs from reactive value
  indi_selct <- r$studienzahl_einstieg_verlauf_indi
  timerange <- r$date_studienzahl_einstieg_verlauf
  t  <- as.character(timerange[1]:timerange[2])

  abs_zahlen_selector <- r$abs_zahlen_einstieg_verlauf_indi


  # filter dataset based on UI inputs
  df <- dplyr::tbl(con, from = "studierende") %>%
    dplyr::filter(jahr %in% t,
                  geschlecht == "Gesamt",
                  region== "Deutschland")%>%
    dplyr::select( -region)%>%
    dplyr::collect()

  df <- df %>%
    tidyr::pivot_wider(names_from=fachbereich, values_from = wert)%>%
    #dplyr::rename("MINT (gesamt)" = MINT)%>%
    dplyr::mutate("MINT (Gesamt)_p"=`MINT (Gesamt)`/Alle)%>%
    dplyr::mutate("Nicht MINT_p"=`Nicht MINT`/Alle)%>%
    dplyr::select(-Ingenieurwissenschaften,- "Mathematik, Naturwissenschaften",-Alle)%>%
    tidyr::pivot_longer(c("MINT (Gesamt)", "Nicht MINT", "Nicht MINT_p", "MINT (Gesamt)_p"), names_to = "var", values_to = "wert")%>%
    dplyr::mutate(selector=dplyr::case_when(stringr::str_ends(.$var, "p")~"In Prozent",
                                            T~ "Anzahl"))%>%
    dplyr::filter(!var %in% c("Nicht MINT", "Nicht MINT_p"))


  if(abs_zahlen_selector == "In Prozent"){

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    df <- df %>%  dplyr::filter(selector=="In Prozent")


    df$wert <- df$wert *100
    df$wert <- round(df$wert,1)

    df <- df[with(df, order( jahr, decreasing = FALSE)), ]




    df <- df %>% dplyr::filter(indikator %in% indi_selct)

    highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group=indikator))%>%
      highcharter::hc_tooltip(pointFormat = "Anteil: {point.y}%") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = paste0("Anteil von Studierenden in MINT an allen Studierenden"),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(c("#b16fab", "#154194", "#66cbaf", "#fbbf24", "#AFF3E0", "#2D6BE1", "#008F68", "#8893a7", "#ee7775", "#9d7265", "#35bd97",
                               "#bfc6d3", "#5f94f9", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")) %>%
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

    # } else if(length(indi_selct)==2){
    #
    #   df5 <- df5 %>% dplyr::filter(indikator == indi_selct[1] | indikator == indi_selct [2])
    #
    #   highcharter::hchart(df5, 'line', highcharter::hcaes(x = jahr, y = wert, group=indikator))%>%
    #     highcharter::hc_tooltip(pointFormat = "Anteil: {point.y}%") %>%
    #     highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    #     highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    #     #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    #     highcharter::hc_title(text = paste0("Anteil von Studierenden in MINT an allen Studierenden"),
    #                           margin = 45,
    #                           align = "center",
    #                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    #     highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf")) %>%
    #     highcharter::hc_chart(
    #       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    #     ) %>%
    #     highcharter::hc_exporting(enabled = FALSE,
    #                               buttons = list(contextButton = list(
    #                                 symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
    #                                 onclick = highcharter::JS("function () {
    #                                                             this.exportChart({ type: 'image/png' }); }"),
    #                                 align = 'right',
    #                                 verticalAlign = 'bottom',
    #                                 theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
    #
    # } else if(length(indi_selct == 3)){
    #
    #   df5<- df5 %>% dplyr::filter(indikator == indi_selct[1] | indikator == indi_selct [2] | indikator == indi_selct [3])
    #
    #
    #   highcharter::hchart(df5, 'line', highcharter::hcaes(x = jahr, y = wert, group=indikator))%>%
    #     highcharter::hc_tooltip(pointFormat = "Anteil: {point.y}%") %>%
    #     highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    #     highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    #     #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    #     highcharter::hc_title(text = paste0("Anteil von Studierenden in MINT an allen Studierenden"),
    #                           margin = 45,
    #                           align = "center",
    #                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    #     highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf")) %>%
    #     highcharter::hc_chart(
    #       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    #     ) %>%
    #     highcharter::hc_exporting(enabled = FALSE,
    #                               buttons = list(contextButton = list(
    #                                 symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
    #                                 onclick = highcharter::JS("function () {
    #                                                             this.exportChart({ type: 'image/png' }); }"),
    #                                 align = 'right',
    #                                 verticalAlign = 'bottom',
    #                                 theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
    #

    #}

  }else if (abs_zahlen_selector == "Anzahl"){

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    df <- df %>%  dplyr::filter(selector=="Anzahl")

    df <- df[with(df6, order( jahr, decreasing = FALSE)), ]

    #if(length(indi_selct) == 1) {

    df <- df %>% dplyr::filter(indikator %in% indi_selct)

    highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group=indikator))%>%
      highcharter::hc_tooltip(pointFormat = "Anzahl: {point.y}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = paste0("Anzahl an Studierenden in MINT"),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(c("#b16fab", "#154194", "#66cbaf", "#fbbf24", "#AFF3E0", "#2D6BE1", "#008F68", "#8893a7", "#ee7775", "#9d7265", "#35bd97",
                               "#bfc6d3", "#5f94f9", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")) %>%
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

    # } else if(length(indi_selct)==2){
    #
    #   df6 <- df6 %>% dplyr::filter(indikator == indi_selct[1] | indikator == indi_selct [2])
    #
    #   highcharter::hchart(df6, 'line', highcharter::hcaes(x = jahr, y = wert, group=indikator))%>%
    #     highcharter::hc_tooltip(pointFormat = "Anzahl: {point.y}") %>%
    #     highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    #     highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    #     #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    #     highcharter::hc_title(text = paste0("Anzahl an Studierenden in MINT"),
    #                           margin = 45,
    #                           align = "center",
    #                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    #     highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf")) %>%
    #     highcharter::hc_chart(
    #       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    #     ) %>%
    #     highcharter::hc_exporting(enabled = FALSE,
    #                               buttons = list(contextButton = list(
    #                                 symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
    #                                 onclick = highcharter::JS("function () {
    #                                                           this.exportChart({ type: 'image/png' }); }"),
    #                                 align = 'right',
    #                                 verticalAlign = 'bottom',
    #                                 theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
    #
    # } else if(length(indi_selct == 3)){
    #
    #   df6<- df6 %>% dplyr::filter(indikator == indi_selct[1] | indikator == indi_selct [2] | indikator == indi_selct [3])
    #
    #   # hc_yAxis(title = list(text = "YourAxis"), #new part for axis labelling
    #   #          labels=list(format="{value:,f}"))
    #   highcharter::hchart(df6, 'line', highcharter::hcaes(x = jahr, y = wert, group=indikator))%>%
    #     highcharter::hc_tooltip(pointFormat = "Anzahl: {point.y}") %>%
    #     highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    #     highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    #     #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    #     highcharter::hc_title(text = paste0("Anzahl an Studierenden in MINT"),
    #                           margin = 45,
    #                           align = "center",
    #                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    #     highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf")) %>%
    #     highcharter::hc_chart(
    #       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    #     ) %>%
    #     highcharter::hc_exporting(enabled = FALSE,
    #                               buttons = list(contextButton = list(
    #                                 symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
    #                                 onclick = highcharter::JS("function () {
    #                                                           this.exportChart({ type: 'image/png' }); }"),
    #                                 align = 'right',
    #                                 verticalAlign = 'bottom',
    #                                 theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
    #
    #
    # }





  }


  # aggregate MINT
  # df2 <- calc_share_MINT(df[(df$indikator == "Studierende"), ])
  #
  # df3 <- calc_share_MINT(df[(df$indikator == "Studienanfänger:innen"), ])


  # df <- rbind(df2, df3)
  #
  # df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")
  #
  #
  # if(lehramt == FALSE){
  #
  #   df <- df %>% dplyr::filter(nur_lehramt == "Nein")
  #
  #   df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
  #
  #   df$indikator <- paste0(df$indikator, " (", df$hochschulform, ")")
  #
  # } else {
  #
  #   df <- df %>% dplyr::filter(nur_lehramt == "Ja")
  #
  #   df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  #
  #   df$indikator <- paste0(df$indikator, " (", "Lehramt, " ,df$hochschulform, ")")
  #
  # }
  #
  # # aggregate
  # df <- df %>%
  #   dplyr::group_by(jahr, region, indikator, anzeige_geschlecht, bereich) %>%
  #   dplyr::mutate(props = sum(wert))
  #
  #
  #
  # # calculate proportions
  # df <- df %>% dplyr::group_by(indikator, fachbereich, jahr) %>%
  #   dplyr::summarize(proportion = wert/props)
  #
  # df <- df %>% dplyr::filter(fachbereich == "MINT")
  #
  # df$proportion <- df$proportion * 100
  #
  # df <- df[with(df, order(jahr, decreasing = FALSE)), ]

  # plot


}




#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_verlauf_bl_subject <- function(r) {


  absolut_selector <- r$abs_zahlen_verlauf_subject_bl

  # load UI inputs from reactive value
  timerange <- r$date_verlauf_subject_bl
  t <- as.character(timerange[1]:timerange[2])

  states <- r$states_verlauf_subject_bl

  label_select <- r$verl_l

  df <- dplyr::tbl(con, from = "studierende") %>%
    dplyr::filter(jahr %in% t,
                  geschlecht=="Gesamt",
                  region == states,
                  indikator==label_select) %>%
    dplyr::collect()

  df <- df %>%
    tidyr::pivot_wider(names_from=fachbereich, values_from = wert)%>%
    #dplyr::rename("MINT (gesamt)" = MINT)%>%
    dplyr::mutate("MINT (Gesamt)_p"= `MINT (Gesamt)`/Alle)%>%
    dplyr::mutate(Ingenieurwissenschaften_p=Ingenieurwissenschaften/Alle)%>%
    dplyr::mutate("Mathematik, Naturwissenschaften_p"=`Mathematik, Naturwissenschaften`/Alle)%>%
    dplyr::select(-Alle,- `Nicht MINT`,- geschlecht)%>%
    tidyr::pivot_longer(c(4:9), names_to ="var", values_to = "wert")%>%
    dplyr::mutate(selector=dplyr::case_when(stringr::str_ends(.$var, "p")~"In Prozent",
                                            T~ "Anzahl"))

  df$var <- gsub("_p", "", df$var)

  #tidyr::pivot_longer(c(MINT, Ingenieurwissenschaften, `Mathematik/Naturwissenschaften`), names_to = "proportion", values_to = "wert")

  if(absolut_selector=="In Prozent"){

    df <- df %>%
      dplyr::filter(selector=="In Prozent")

    df$wert <- df$wert *100
    df$wert <- round(df$wert, 1)

    #df <- df %>% dplyr::filter(indikator==label_select)

    # Überschrift vorbereiten
    label_select <- ifelse(label_select == "Studierende", paste0(label_select, "n"), label_select)
    label_select <- ifelse(label_select == "Studierende (Fachhochschulen)", "Studierenden (Fachhochschulen)" , label_select)
    label_select <- ifelse(label_select == "Studierende (Lehramt, Universität)", "Studierenden (Lehramt, Universität)" , label_select)
    label_select <- ifelse(label_select == "Studierende (Universität)", "Studierenden (Universität)" , label_select)

    titel_help <- "Studierenden"
    titel_help <- ifelse(grepl("Studienanfänger:innen",label_select), "Studienanfänger:innen", titel_help)
    #
    # df6 <- df5 %>% dplyr::filter(region == states)

    df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
    # plot
    highcharter::hchart(df, 'line', highcharter::hcaes(x = reorder(jahr, wert), y = wert, group= var))%>%
      highcharter::hc_tooltip(pointFormat = "Anteil {point.proportion} <br> Wert: {point.y} %") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = paste0("Anteil von ", label_select, " in MINT an allen ", titel_help, " in ",states ),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf")) %>%
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

  } else if (absolut_selector == "Anzahl") {


    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    df <- df %>%
      dplyr::filter(selector=="Anzahl")


    df <- df %>% dplyr::filter(indikator==label_select)

    # Überschrift vorbereiten
    label_select <- ifelse(label_select == "Studierende", paste0(label_select, "n"), label_select)
    label_select <- ifelse(label_select == "Studierende (Fachhochschulen)", "Studierenden (Fachhochschulen)" , label_select)
    label_select <- ifelse(label_select == "Studierende (Lehramt, Universität)", "Studierenden (Lehramt, Universität)" , label_select)
    label_select <- ifelse(label_select == "Studierende (Universität)", "Studierenden (Universität)" , label_select)

    df <- df %>% dplyr::filter(region == states)

    df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
    # plot
    highcharter::hchart(df, 'line', highcharter::hcaes(x = reorder(jahr, wert), y = wert, group= var))%>%
      highcharter::hc_tooltip(pointFormat = "Anzahl: {point.y}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = paste0("Anzahl an ", label_select, " in MINT in ",states ),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf")) %>%
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


}



#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studierende_verlauf_multiple_bl <- function(r) {


  # load UI inputs from reactive value
  timerange <- r$date_studium_studienzahl_bl_verlauf
  t <- as.character(timerange[1]:timerange[2])

  absolut_selector <- r$abs_zahlen_studium_studienzahl_bl_verlauf

  subjects_select <- r$subject_studium_studienzahl_bl_verlauf

  bl_label <- r$verl_bl_l

  states <- r$states_studium_studienzahl_bl_verlauf

  # filter dataset based on UI inputs
  df <- dplyr::tbl(con, from = "studierende") %>%
    dplyr::filter(jahr %in% t,
                  geschlecht == "Gesamt",
                  indikator==bl_label) %>%
    dplyr::collect()

  df <- df %>%
    dplyr::select(-geschlecht)%>%
    dplyr::filter(!is.na(wert))%>%

    tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
    #dplyr::rename("MINT (Gesamt)"=MINT)%>%
    dplyr::mutate(Ingenieurwissenschaften_p=Ingenieurwissenschaften/Alle,
                  "Mathematik, Naturwissenschaften_p"= `Mathematik, Naturwissenschaften`/Alle,
                  "MINT (Gesamt)_p"=`MINT (Gesamt)`/Alle)%>%
    dplyr::select(-Alle)%>%
    dplyr::mutate(
      dplyr::across(
        c(Ingenieurwissenschaften_p, `Mathematik, Naturwissenschaften_p`, `MINT (Gesamt)_p`),  ~round(.*100,1)))%>%
    dplyr::select(-`Nicht MINT`)%>%
    tidyr::pivot_longer(c(4:9), values_to ="wert", names_to="fach")%>%
    dplyr::mutate(selector = dplyr::case_when(
      stringr::str_ends(.$fach, "_p") ~ "In Prozent",
      T~"Anzahl"
    ))

  df$fach <- gsub("_p", "", df$fach)

  df <- df %>% dplyr::filter(
    fach %in% subjects_select)


  df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

  df <- df %>%dplyr::filter(region%in%states)

  # Vorbereitung Überschrift

  bl_label <- ifelse(bl_label == "Studierende", paste0(bl_label, "n"), bl_label)
  bl_label <- ifelse(bl_label == "Studierende (Fachhochschulen)", "Studierenden (Fachhochschulen)" , bl_label)
  bl_label <- ifelse(bl_label == "Studierende (Lehramt, Universität)", "Studierenden (Lehramt, Universität)" , bl_label)
  bl_label <- ifelse(bl_label == "Studierende (Universität)", "Studierenden (Universität)" , bl_label)

  help <- "Studierenden"
  help <- ifelse(grepl("Studienanfänger:innen",bl_label), "Studienanfänger:innen", help)

  fach_label <- subjects_select
  fach_label<- ifelse(fach_label == "MINT (Gesamt)", "MINT", fach_label)

  # Plot

  if (absolut_selector=="In Prozent"){

    df <- df %>%
      dplyr::filter(selector=="In Prozent")

    highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group = region))%>%
      highcharter::hc_tooltip(pointFormat = "Anteil {point.region} <br> Wert: {point.y} %") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = paste0("Anteil von ", bl_label, " in ", fach_label, " an allen ", help),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                               "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")) %>%
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



  } else if(absolut_selector=="Anzahl"){


    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    df <- df %>%
      dplyr::filter(selector == "Anzahl")

    highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group = region))%>%
      highcharter::hc_tooltip(pointFormat = "Anzahl: {point.y}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = paste0("Anzahl an ", bl_label, " in ", help),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(c("#b16fab", "#154194", "#66cbaf", "#fbbf24", "#8893a7", "#ee7775", "#9d7265", "#35bd97", "#5d335a",
                               "#bfc6d3", "#5f94f9", "#B45309", "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")) %>%
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





  # include "Osten" und "Westen" in Dataframe
  # df <- prep_studierende_east_west(df)
  #
  # if(lehramt == FALSE){
  #
  #   df <- df %>% dplyr::filter(nur_lehramt == "Nein")
  #
  #   df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
  #
  #   title_help_sub_sub <- " insgesamt"
  #
  # } else {
  #
  #   df <- df %>% dplyr::filter(nur_lehramt == "Ja")
  #
  #   df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  #
  #   title_help_sub_sub <- " an einer Uni (nur Lehramt)"
  # }

  # # aggregate to MINT
  # df_sub <- calc_share_MINT_bl(df)
  #
  # df_sub <- df_sub[,colnames(df)]
  #
  # # # aggregate all subjects to calculate proportion later
  # df_sub <- df_sub %>% dplyr::group_by(region, indikator) %>%
  #   dplyr::mutate(props = sum(wert))
  #
  # df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT-Fächer (gesamt)"
  #
  # df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (gesamt)"
  #
  # # aggregate all subjects to calculate proportion later
  # df <- df %>% dplyr::group_by(jahr, region, indikator) %>%
  #   dplyr::mutate(sum_props = sum(wert))
  #
  # df <- rbind(df, df_sub)

  #df <- df %>% dplyr::filter(fachbereich == subject)


  # aggregate to MINT
  # df_sub <- calc_share_MINT_bl(df)
  #
  # df_sub <- df_sub[,colnames(df)]


  #df_sub <- calc_share_male_bl(df_sub)

  # calculate the new "Gesamt"
  #  df_sub <-  df_sub %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
  #    dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
  #    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
  #                    wert[anzeige_geschlecht == "Männer"])
  #
  # df_sub <- df_sub %>% dplyr::filter(anzeige_geschlecht != "Männer")

  # aggregate all subjects to calculate proportion later
  # df_sub <- df_sub %>% dplyr::group_by(jahr, region, indikator) %>%
  #   dplyr::mutate(sum_props = sum(wert))
  #
  #
  # df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT-Fächer (gesamt)"
  #
  # df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (gesamt)"

  # df <- calc_share_male_bl(df)

  # calculate new "Gesamt"
  # df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
  #   dplyr::group_by(region, fachbereich, indikator, jahr) %>%
  #   dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
  #                   wert[anzeige_geschlecht == "Männer"])
  #
  # df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")

  #   df <- df %>%
  #     dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
  #     dplyr::mutate(sum_props = sum(wert))
  #
  #   df <- rbind(df, df_sub)
  #
  #   # filter states
  #   df <- df %>% dplyr::filter(region %in% states)
  #
  #   df <- df %>% dplyr::filter(fachbereich %in% subjects_select)
  #
  #   df <- df %>% dplyr::filter(indikator %in% studium_level)
  #
  #   # calculate proportions
  #   df <- df %>% dplyr::group_by(jahr, region, indikator) %>%
  #     dplyr::summarize(proportion = wert/sum_props)
  #
  #   df$proportion <- df$proportion * 100
  #
  #   if(studium_level == "Studierende") {
  #
  #     title_help <- "Studierende"
  #
  #   }else {
  #
  #     title_help <- "Studienanfänger:innen"
  #
  #   }
  #
  #   help_title <- ifelse(subjects_select == "MINT-Fächer (gesamt)", "MINT-Fächern (gesamt)", subjects_select)
  # browser()
  #   # order years for plot
  #   df_scope <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

  # plot
  # highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = region)) %>%
  #   highcharter::hc_tooltip(pointFormat = "Anteil {point.region} <br> Wert: {point.y} %") %>%
  #   highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
  #   highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
  #   #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
  #   highcharter::hc_title(text = paste0("Anteil von ", help_title, " (",title_help, ")"),
  #                         margin = 45,
  #                         align = "center",
  #                         style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
  #   highcharter::hc_chart(
  #     style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
  #   ) %>%
  #   highcharter::hc_exporting(enabled = FALSE,
  #                             buttons = list(contextButton = list(
  #                               symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
  #                               onclick = highcharter::JS("function () {
  #                                                             this.exportChart({ type: 'image/png' }); }"),
  #                               align = 'right',
  #                               verticalAlign = 'bottom',
  #                               theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

}



#' A function to plot time series
#'
#' @description A function to plot a bar chart
#'
#' @return The return value, if any, from executing the function.
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_einstieg_comparison <- function(r) {

  # load UI inputs from reactive value
  timerange <- r$date_kurse_einstieg_comparison

  # filter dataset based on UI inputs
  df <- dplyr::tbl(con, from = "studierende") %>%
    dplyr::filter(jahr == timerange,
                  geschlecht == "Gesamt",
                  region== "Deutschland")%>%
    dplyr::collect()
  df <- df %>%
    tidyr::pivot_wider(names_from=fachbereich, values_from = wert)%>%
    #dplyr::rename("MINT (gesamt)" = MINT)%>%
    dplyr::select( -region, -Ingenieurwissenschaften,- `Mathematik, Naturwissenschaften`)

  # Calculating props

  df_props <- df %>%
    dplyr::mutate(dplyr::across(c("MINT (Gesamt)", "Nicht MINT"), ~round(./Alle * 100,1)))%>%
    dplyr::select(-Alle)%>%
    tidyr::pivot_longer(c("MINT (Gesamt)", "Nicht MINT"), values_to="prop", names_to = "proportion")

  # joining props and wert
  df <- df%>%
    dplyr::select(-Alle )%>%
    tidyr::pivot_longer(c("MINT (Gesamt)", "Nicht MINT"), values_to="wert", names_to = "proportion")%>%
    dplyr::left_join(df_props)


  #Trennpunkte für lange Zahlen ergänzen

  df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")


  df$indikator <-factor(df$indikator,levels= c("Studierende",
                                               "Studierende (Fachhochschulen)",
                                               "Studierende (Lehramt, Universität)",
                                               "Studierende (Universität)",
                                               "Studienanfänger:innen (1.Fachsemester)",
                                               "Studienanfänger:innen (1.Hochschulsemester)",
                                               "Studienanfänger:innen (Fachhochschulen, 1.Fachsemester)",
                                               "Studienanfänger:innen (Fachhochschulen, 1.Hochschulsemester)",
                                               "Studienanfänger:innen (Lehramt, Universität, 1.Fachsemester)",
                                               "Studienanfänger:innen (Lehramt, Universität, 1.Hochschulsemester)",
                                               "Studienanfänger:innen (Universität, 1.Fachsemester)",
                                               "Studienanfänger:innen (Universität, 1.Hochschulsemester)"
  )
  )



  # plot


  df <- within(df, proportion <- factor(proportion, levels=c("Nicht MINT", "MINT (Gesamt)")))


  highcharter::hchart(df, 'bar', highcharter::hcaes(y = prop, x = indikator, group = forcats::fct_rev(proportion)))%>%
    highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.proportion} <br> Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  F) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
    highcharter::hc_colors(c( "#b16fab","#efe8e6")) %>%
    highcharter::hc_title(text = paste0("Anteil von Studierenden in MINT an allen Studierenden", "(", timerange, ")"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
    highcharter::hc_exporting(enabled = FALSE,
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))


}


#' A function to plot a waffle chart
#'
#' @description A function to create a waffle chart inside the
#' tab "Studium".
#'
#' @return The return value is a waffle chart
#' @param df The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_waffle_choice_gender <- function(r) {


  # load UI inputs from reactive value
  timerange <- r$choice_y

  lab_cho <- r$choice_l

  # filter dataset based on UI inputs
  df_both <- dplyr::tbl(con, from = "studierende") %>%
    dplyr::filter(jahr == timerange,
                  region=="Deutschland",
                  geschlecht %in% c("Frauen", "Männer"))%>%
    dplyr::collect()

  df_both <- df_both %>%
    tidyr::pivot_wider(names_from = fachbereich, values_from=wert )%>%
    dplyr::mutate(dplyr::across(c(`Mathematik, Naturwissenschaften`, `Ingenieurwissenschaften` , `Nicht MINT`), ~ round(./Alle *100,1)))%>%
    dplyr::rename("andere Studiengänge" = `Nicht MINT`)%>%
    dplyr::filter(indikator==lab_cho)%>%
    dplyr::select(-Alle,-`MINT (Gesamt)`)


  # Überschriften vorbereiten
  lab_cho <- ifelse(lab_cho == "Studienanfänger:innen (1.Fachsemester)", "Studienanfänger:innen <br> (1. Fachsemester)", lab_cho)
  lab_cho <- ifelse(lab_cho == "Studienanfänger:innen (1.Hochschulsemester)", "Studienanfänger:innen <br> (1. Hochschulsemester)" , lab_cho)
  lab_cho <- ifelse(lab_cho == "Studienanfänger:innen (Fachhochschulen, 1.Fachsemester)", "Studienanfänger:innen <br> (Fachhochschule, 1. Fachsemester)" , lab_cho)
  lab_cho <- ifelse(lab_cho == "Studienanfänger:innen (Fachhochschulen, 1.Hochschulsemester)", "Studienanfänger:innen <br> (Fachhochschule, 1. Hochschulsemester)" , lab_cho)
  lab_cho <- ifelse(lab_cho == "Studienanfänger:innen (Lehramt, Universität, 1.Fachsemester)", "Studienanfänger:innen <br> (Lehramt, Universität, 1. Fachsemester)" , lab_cho)
  lab_cho <- ifelse(lab_cho == "Studienanfänger:innen (Lehramt, Universität, 1.Hochschulsemester)", "Studienanfänger:innen <br> (Lehramt, Universität, 1. Hochschulsemester)" , lab_cho)
  lab_cho <- ifelse(lab_cho == "Studienanfänger:innen (Universität, 1.Fachsemester)", "Studienanfänger:innen <br> (Universität, 1. Fachsemester)" , lab_cho)
  lab_cho <- ifelse(lab_cho == "Studienanfänger:innen (Universität, 1.Hochschulsemester)", "Studienanfänger:innen <br> (Universität, 1. Hochschulsemester)" , lab_cho)
  lab_cho <- ifelse(lab_cho == "Studierende (Fachhochschulen)", "Studierende <br> (Fachhochschulen)" , lab_cho)
  lab_cho <- ifelse(lab_cho == "Studierende (Lehramt, Universität)", "Studierende <br> (Lehramt, Universität)" , lab_cho)
  lab_cho <- ifelse(lab_cho == "Studierende (Universität)", "Studierende <br> (Universität)" , lab_cho)


  data_fr <- df_both %>%
    dplyr::filter(geschlecht=="Frauen")

  df_fr <- as.numeric(as.vector(data_fr[1,5:ncol(data_fr)]))
  names(df_fr) <- colnames(data_fr[5:ncol(data_fr)])


  data_ma <-df_both %>%
    dplyr::filter(geschlecht=="Männer")

  df_ma<- as.numeric(as.vector(data_ma[1,5:ncol(data_ma)]))
  names(df_ma ) <- colnames(data_ma[5:ncol(data_ma)])

  waffle_frauen <- waffle::waffle(df_fr, keep = FALSE)+
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", "Studienfachwahl von Frauen </span><br>(", lab_cho, ", <br>", timerange, ")<br>"))+
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "bottom") +
    ggplot2::scale_fill_manual(
      values =  c("#35bd97",
                  "#fbbf24",
                  '#8893a7'),
      na.value='#8893a7',
      limits = c("Ingenieurwissenschaften", "Mathematik, Naturwissenschaften", "andere Studiengänge"),

      guide = ggplot2::guide_legend(reverse = TRUE),
      labels = c(
        paste0("Ingenieurwissenschaften",", ",df_fr[1], "%"),
        paste0("Mathematik/Naturwissenschaften",", ",df_fr[2], "%"),
        paste0("andere Studiengänge",", ",df_fr[3], "%"))) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=3,byrow=TRUE))



  waffle_maenner <- waffle::waffle(df_ma, keep = FALSE)+
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", "Studienfachwahl von Männern </span><br>(", lab_cho, ", <br>", timerange, ")<br>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "bottom") +
    ggplot2::scale_fill_manual(
      values =  c("#35bd97",
                  "#fbbf24",
                  '#8893a7'),
      na.value='#8893a7',
      limits = c("Ingenieurwissenschaften", "Mathematik, Naturwissenschaften", "andere Studiengänge"),

      guide = ggplot2::guide_legend(reverse = TRUE),
      labels = c(
        paste0("Ingenieurwissenschaften",", ",df_ma[1], "%"),
        paste0("Mathematik/Naturwissenschaften",", ",df_ma[2], "%"),
        paste0("andere Studiengänge",", ",df_ma[3], "%"))) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=3,byrow=TRUE))



  ggpubr::ggarrange(waffle_frauen, NULL ,waffle_maenner,
                    widths = c(1, 0.1, 1), nrow=1)






  # if(lehramt == FALSE){
  #
  #   df <- df %>% dplyr::filter(nur_lehramt == "Nein")
  #
  #   df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
  #
  # } else {
  #
  #   df <- df %>% dplyr::filter(nur_lehramt == "Ja")
  #
  #   df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  # }
  #
  # df <- df %>% dplyr::filter(region == "Deutschland")
  #
  # df <- df %>% dplyr::filter(indikator == studium_level)
  #
  # # calculate new "Männer"
  # df <- calc_share_male(df, "box_2")
  #
  # df_maenner <- df %>% dplyr::filter(anzeige_geschlecht == "Männer")
  #
  # x_maenner <- calc_share_waffle(df_maenner)
  #
  #
  #
  # df_frauen <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")
  #
  # x_frauen <- calc_share_waffle(df_frauen)
  #
  #
  # # set order
  # x_maenner <- x_maenner[order(factor(names(x_maenner), levels = c('Ingenieurwissenschaften', 'Mathematik/Naturwissenschaften',
  #                                                                  'andere Studiengänge')))]
  #
  # x_frauen <- x_frauen[order(factor(names(x_frauen),
  #                                   levels = c('Ingenieurwissenschaften', 'Mathematik/Naturwissenschaften',
  #                                              'andere Studiengänge')))]
  #

  # create plot objects for waffle charts
  # waffle_maenner <- waffle::waffle(x_maenner, keep = FALSE) +
  #   ggplot2::labs(
  #     fill = "",
  #     title = paste0("<span style='color:black;'>", "Studienfachwahl von Männern </span><br>(", studium_level, ", ", timerange, ")<br>")) +
  #   ggplot2::theme(plot.title = ggtext::element_markdown(),
  #                  plot.subtitle = ggtext::element_markdown(),
  #                  text = ggplot2::element_text(size = 14),
  #                  plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
  #                  legend.position = "bottom") +
  #   ggplot2::scale_fill_manual(
  #     values =  c("#00a87a",
  #                 "#fcc433",
  #                 '#b1b5c3'),
  #     na.value="#b1b5c3",
  #     limits = c("Ingenieurwissenschaften", "Mathematik/Naturwissenschaften", "andere Studiengänge"),
  #     guide = ggplot2::guide_legend(reverse = TRUE),
  #     labels = c(
  #       paste0("Ingenieurwissenschaften",", ",x_maenner[1], "%"),
  #       paste0("Mathematik/Naturwissenschaften",", ",x_maenner[2], "%"),
  #       paste0("andere Studiengänge",", ",x_maenner[3], "%"))) +
  #   ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))
  #
  #
  #
  # waffle_frauen <- waffle::waffle(x_frauen, keep = FALSE) +
  #   ggplot2::labs(
  #     fill = "",
  #     title = paste0("<span style='color:black;'>", "Studienfachwahl von Frauen </span><br>(", studium_level, ", ", timerange, ")<br>")) +
  #   ggplot2::theme(plot.title = ggtext::element_markdown(),
  #                  plot.subtitle = ggtext::element_markdown(),
  #                  text = ggplot2::element_text(size = 14),
  #                  plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
  #                  legend.position = "bottom") +
  #   ggplot2::scale_fill_manual(
  #     values =  c("#00a87a",
  #                 "#fcc433",
  #                 '#b1b5c3'),
  #     na.value="#b1b5c3",
  #     limits = c("Ingenieurwissenschaften", "Mathematik/Naturwissenschaften", "andere Studiengänge"),
  #     guide = ggplot2::guide_legend(reverse = TRUE),
  #     labels = c(
  #       paste0("Ingenieurwissenschaften",", ",x_frauen[1], "%"),
  #       paste0("Mathematik/Naturwissenschaften",", ",x_frauen[2], "%"),
  #       paste0("andere Studiengänge",", ",x_frauen[3], "%"))) +
  #   ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))
  #
  #
  #
  # ggpubr::ggarrange(waffle_frauen, NULL ,waffle_maenner,
  #                   widths = c(1, 0.1, 1), nrow=1)

}

#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studierende_verlauf_single_bl_gender <- function(r) {


  # load UI inputs from reactive value
  timerange <- r$choice_V_y
  t <- as.character(timerange[1]:timerange[2])

  v_lab <- r$choice_l_v

  absolut_selector <- r$abs_zahlen_l_v

  subjects_select <- r$choice_v_f

  states <- r$choice_states

  df <- dplyr::tbl(con, from = "studierende") %>%
    dplyr::filter(jahr %in% t,
                  geschlecht=="Frauen",
                  region == states,
                  indikator %in%v_lab) %>%
    dplyr::collect()

  df <- df %>%
    #dplyr::select(-quelle, -bereich)%>%
    tidyr::pivot_wider(names_from=fachbereich, values_from = wert)%>%
    #dplyr::rename("MINT (Gesamt)" = MINT)%>%

    dplyr::mutate("Mathematik, Naturwissenschaften_p" =round(`Mathematik, Naturwissenschaften`/Alle*100,1),
                  "MINT (Gesamt)_p"= round(`MINT (Gesamt)`/Alle*100,1),
                  "Ingenieurwissenschaften_p"= round(Ingenieurwissenschaften/Alle*100,1))%>%
    dplyr::select(-Alle, -`Nicht MINT`)%>%
    tidyr::pivot_longer(c(5:10),names_to="fach",values_to="wert")%>%
    dplyr::mutate(selector= dplyr::case_when(stringr::str_ends(.$fach, "_p")~"In Prozent",
                                             T~"Anzahl"))

  df$fach <- gsub("_p", "", df$fach)



  df <- df %>%
    dplyr::filter(fach==subjects_select)

  fach_label <- subjects_select
  fach_label <- ifelse(fach_label == "MINT (Gesamt)", "MINT", fach_label)

  if (absolut_selector=="In Prozent"){

    df <- df %>%
      dplyr::filter(selector=="In Prozent")

    df <- df[with(df, order( jahr, decreasing = FALSE)), ]


    # df <- df %>%
    #   dplyr::filter(indikator %in%v_lab)

    df <- df %>%
      dplyr::mutate(jahr= as.numeric(.$jahr))

    df <- df[with(df, order( jahr, decreasing = F)), ]

    df <- df %>%
      dplyr::mutate(jahr= as.character(.$jahr))



    highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert,group=indikator))%>%
      highcharter::hc_tooltip(pointFormat = "Anteil {point.label} <br> Wert: {point.y} %") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"),reversed= F, allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = paste0("Anteil von weiblichen Studierenden, die ein Studium in ", fach_label, " gewählt haben, an allen weiblichen Studierenden in ", states),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(c("#b16fab", "#154194", "#66cbaf", "#fbbf24",
                               "#AFF3E0","#2D6BE1","#008F68","#8893a7", "#ee7775", "#9d7265", "#35bd97",
                               "#bfc6d3", "#5f94f9",  "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")) %>%
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

    # } else if(length(v_lab)==2){
    #
    #   dfggk <- dfgg%>%
    #     dplyr::filter(indikator==v_lab[1]|indikator==v_lab[2])
    #
    #   dfggk <- dfggk %>%
    #     dplyr::mutate(jahr= as.numeric(.$jahr))
    #
    #   dfggk <- dfggk[with(dfggk, order( jahr, decreasing = T)), ]
    #
    #   dfggk <- dfggk %>%
    #     dplyr::mutate(jahr= as.character(.$jahr))
    #
    #
    #
    #
    #   highcharter::hchart(dfggk, 'line', highcharter::hcaes(x = jahr, y = wert,group=indikator))%>%
    #     highcharter::hc_tooltip(pointFormat = "Anteil {point.indikator} <br> Wert: {point.y} %") %>%
    #     highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
    #     highcharter::hc_xAxis(title = list(text = "Jahr"),reversed= TRUE, allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
    #     #highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
    #     highcharter::hc_title(text = paste0("Anteil von weiblichen Studierenden, die ein Studium in ", fach_label, " gewählt haben, an allen weiblichen Studierenden in ", states),
    #                           margin = 45,
    #                           align = "center",
    #                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    #     highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf")) %>%
    #     highcharter::hc_chart(
    #       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    #     ) %>%
    #     highcharter::hc_exporting(enabled = FALSE,
    #                               buttons = list(contextButton = list(
    #                                 symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
    #                                 onclick = highcharter::JS("function () {
    #                                                             this.exportChart({ type: 'image/png' }); }"),
    #                                 align = 'right',
    #                                 verticalAlign = 'bottom',
    #                                 theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
    #
    #
    #
    #
    #
    #
    #
    #
    #
    # } else if(length(v_lab)==3){
    #
    #
    #   dfggü <- dfgg%>%
    #     dplyr::filter(indikator==v_lab[1]|indikator==v_lab[2]|indikator==v_lab[3])
    #
    #   dfggü <- dfggü %>%
    #     dplyr::mutate(jahr= as.numeric(.$jahr))
    #
    #   dfggü <- dfggü[with(dfggü, order( jahr, decreasing = T)), ]
    #
    #   dfggü <- dfggü %>%
    #     dplyr::mutate(jahr= as.character(.$jahr))
    #
    #
    #
    #
    #
    #   highcharter::hchart(dfggü, 'line', highcharter::hcaes(x = jahr, y = wert, group=indikator))%>%
    #     highcharter::hc_tooltip(pointFormat = "Anteil {point.indikator} <br> Wert: {point.y} %") %>%
    #     highcharter::hc_yAxis(title = list(text = ""),  labels = list(format = "{value}%")) %>%
    #     highcharter::hc_xAxis(title = list(text = "Jahr"),reversed= TRUE, allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
    #     #highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
    #     highcharter::hc_title(text = paste0("Anteil von weiblichen Studierenden, die ein Studium in ", fach_label, " gewählt haben, an allen weiblichen Studierenden in ", states),
    #                           margin = 45,
    #                           align = "center",
    #                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    #     highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf")) %>%
    #     highcharter::hc_chart(
    #       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    #     ) %>%
    #     highcharter::hc_exporting(enabled = FALSE,
    #                               buttons = list(contextButton = list(
    #                                 symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
    #                                 onclick = highcharter::JS("function () {
    #                                                             this.exportChart({ type: 'image/png' }); }"),
    #                                 align = 'right',
    #                                 verticalAlign = 'bottom',
    #                                 theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
    #

    #}
  }else if(absolut_selector=="Anzahl"){


    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)


    df <- df %>%
      dplyr::filter(selector=="Anzahl")



    df <- dfgg[with(df, order( jahr, decreasing = FALSE)), ]







    # df <- df %>%
    #   dplyr::filter(indikator%in%v_lab)

    df <- df %>%
      dplyr::mutate(jahr= as.numeric(.$jahr))

    df <- df[with(df, order( jahr, decreasing = F)), ]

    df <- df %>%
      dplyr::mutate(jahr= as.character(.$jahr))



    highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert,group=indikator))%>%
      highcharter::hc_tooltip(pointFormat = "Anzahl: {point.y}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"),reversed= TRUE, allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = paste0("Anzahl an weiblichen Studierenden, die ein Studium in ", fach_label, " gewählt haben, in ", states),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_colors(c("#b16fab", "#154194", "#66cbaf", "#fbbf24","#AFF3E0","#2D6BE1","#008F68","#8893a7", "#ee7775", "#9d7265", "#35bd97",
                               "#bfc6d3", "#5f94f9",  "#007655", "#fde68a", "#dc2626", "#d4c1bb", "#d0a9cd", "#fca5a5", "#112c5f")) %>%
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

    # } else if(length(v_lab)==2){
    #
    #   dfggk <- dfgg%>%
    #     dplyr::filter(indikator==v_lab[1]|indikator==v_lab[2])
    #
    #   dfggk <- dfggk %>%
    #     dplyr::mutate(jahr= as.numeric(.$jahr))
    #
    #   dfggk <- dfggk[with(dfggk, order( jahr, decreasing = T)), ]
    #
    #   dfggk <- dfggk %>%
    #     dplyr::mutate(jahr= as.character(.$jahr))
    #
    #
    #
    #
    #   highcharter::hchart(dfggk, 'line', highcharter::hcaes(x = jahr, y = wert,group=indikator))%>%
    #     highcharter::hc_tooltip(pointFormat = "Anzahl: {point.y}") %>%
    #     highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    #     highcharter::hc_xAxis(title = list(text = "Jahr"),reversed= TRUE, allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
    #     #highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
    #     highcharter::hc_title(text = paste0("Anzahl an weiblichen Studierenden, die ein Studium in ", fach_label, " gewählt haben, in ", states),
    #                           margin = 45,
    #                           align = "center",
    #                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    #     highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf")) %>%
    #     highcharter::hc_chart(
    #       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    #     ) %>%
    #     highcharter::hc_exporting(enabled = FALSE,
    #                               buttons = list(contextButton = list(
    #                                 symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
    #                                 onclick = highcharter::JS("function () {
    #                                                           this.exportChart({ type: 'image/png' }); }"),
    #                                 align = 'right',
    #                                 verticalAlign = 'bottom',
    #                                 theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
    #
    #
    #
    #
    #
    #
    #
    #
    #
    # } else if(length(v_lab)==3){
    #
    #
    #   dfggü <- dfgg%>%
    #     dplyr::filter(indikator==v_lab[1]|indikator==v_lab[2]|indikator==v_lab[3])
    #
    #   dfggü <- dfggü %>%
    #     dplyr::mutate(jahr= as.numeric(.$jahr))
    #
    #   dfggü <- dfggü[with(dfggü, order( jahr, decreasing = T)), ]
    #
    #   dfggü <- dfggü %>%
    #     dplyr::mutate(jahr= as.character(.$jahr))
    #
    #
    #
    #
    #
    #   highcharter::hchart(dfggü, 'line', highcharter::hcaes(x = jahr, y = wert, group=indikator))%>%
    #     highcharter::hc_tooltip(pointFormat = "Anzahl: {point.y}") %>%
    #     highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    #     highcharter::hc_xAxis(title = list(text = "Jahr"),reversed= TRUE, allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
    #     #highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
    #     highcharter::hc_title(text = paste0("Anzahl an weiblichen Studierenden, die ein Studium in ", fach_label, " gewählt haben, in ", states),
    #                           margin = 45,
    #                           align = "center",
    #                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    #     highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf")) %>%
    #     highcharter::hc_chart(
    #       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    #     ) %>%
    #     highcharter::hc_exporting(enabled = FALSE,
    #                               buttons = list(contextButton = list(
    #                                 symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
    #                                 onclick = highcharter::JS("function () {
    #                                                           this.exportChart({ type: 'image/png' }); }"),
    #                                 align = 'right',
    #                                 verticalAlign = 'bottom',
    #                                 theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
    #
    #
    # }




  }
}

# M-I-N-T ----

#' A function to create barplots, showing ranked study subjects
#'
#' @description A function to compare different subjects
#'
#' @return The return value is a barplot
#' @param data The dataframe "studierende_faecher.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

plot_ranking_top_faecher <- function(r) {


  # load UI inputs from reactive value
  timerange <- r$date_top_faecher

  states <- r$states_top_faecher

  subject <- r$subject_top_faecher

  abs_rel <- r$subject_abs_rel

  # filter dataset based on UI inputs
  df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    dplyr::filter(jahr == timerange,
                  indikator == "Studierende",
                  jahr == timerange,
                  !fach %in% c(
      "Außerhalb der Studienbereichsgliederung/Sonstige Fächer",
      "Weitere ingenieurwissenschaftliche Fächer",
      "Weitere naturwissenschaftliche und mathematische Fächer"
    )) %>%
    dplyr::collect()


  # # Calculate male numbers
  # df<- calc_share_male(df, "box_1")
  #
  # df_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

  # Calculate proportion


  df_props <- df %>%
    tidyr::pivot_wider(values_from = wert, names_from=geschlecht)%>%
    dplyr::mutate(dplyr::across(c("Männer", "Frauen"), ~round(./Gesamt*100,1)))%>%
    dplyr::select(-Gesamt)%>%
    tidyr::pivot_longer(c("Männer", "Frauen"), names_to="geschlecht", values_to = "prop")

  df <- df %>%
    dplyr::filter(geschlecht!="Gesamt")%>%
    dplyr::left_join(df_props)



  #Trennpunkte für lange Zahlen ergänzen


  if(subject == "MINT-Fächer"){

    df <- df %>% dplyr::filter(fachbereich %in% c("MINT",
                                                    "Mathematik, Naturwissenschaften",
                                                    "Ingenieurwissenschaften") & typ != "Aggregat")%>%
      dplyr::filter(region == states)




  }else {

    df <- df %>% dplyr::filter(typ == "Aggregat"& fach != "Alle Fächer")%>%
      dplyr::filter(region == states)

  }

  # Split dataframe by gender and create plots
  if(abs_rel == "In Prozent"){

    df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

    # female
    studierende_faecher_frauen <- df %>%
      dplyr::filter(geschlecht == "Frauen")%>%
      dplyr::arrange(desc(prop))%>%
      dplyr::slice(1:10)

    # male
    studierende_faecher_maenner <- df %>%
      dplyr::filter(geschlecht == "Männer") %>%
      dplyr::arrange(desc(prop)) %>%
      dplyr::slice(1:10)

    # Create female plot
    hc_frau <- highcharter::hchart(studierende_faecher_frauen, 'bar', highcharter::hcaes(y = prop, x = fach)) %>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, format = "{point.prop} %")
        )) %>%
      highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0, max = 100, tickInterval = 5) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_colors(c("#154194")) %>%
      highcharter::hc_title(text = paste0("Fächer mit dem höchsten Frauenanteil ", "(", timerange, ")"),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE)
    # %>%
    #   highcharter::hc_exporting(enabled = TRUE,
    #                             buttons = list(contextButton = list(
    #                               symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
    #                               onclick = highcharter::JS("function () {
    #                                                           this.exportChart({ type: 'image/jpeg' }); }"),
    #                               align = 'right',
    #                               verticalAlign = 'bottom',
    #                               theme = list(states = list(hover = list(fill = '#FFFFFF'))))))


    # Create male plot
    hc_mann <- highcharter::hchart(studierende_faecher_maenner, 'bar', highcharter::hcaes(y = prop, x = fach)) %>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, format = "{point.prop} %")
        )) %>%
      highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0, max = 100, tickInterval = 5) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_colors(c("#66cbaf")) %>%
      highcharter::hc_title(text = paste0("Fächer mit dem höchsten Männeranteil ", "(", timerange, ")"),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE)
    # %>%
    #   highcharter::hc_exporting(enabled = TRUE,
    #                             buttons = list(contextButton = list(
    #                               symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
    #                               onclick = highcharter::JS("function () {
    #                                                           this.exportChart({ type: 'image/jpeg' }); }"),
    #                               align = 'right',
    #                               verticalAlign = 'bottom',
    #                               theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

  } else if(abs_rel == "Anzahl"){

    # female
    studierende_faecher_frauen <- df %>%
      dplyr::filter(geschlecht == "Frauen") %>%
      dplyr::arrange(desc(wert)) %>%
      dplyr::slice(1:10)

    # male
    studierende_faecher_maenner <- df %>%
      dplyr::filter(geschlecht == "Männer") %>%
      dplyr::arrange(desc(wert)) %>%
      dplyr::slice(1:10)


    # Create female plot
    hc_frau <- highcharter::hchart(studierende_faecher_frauen, 'bar', highcharter::hcaes(y = wert, x = fach)) %>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, format = "{point.wert}")
        )) %>%

      highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}"), min = 0, max = plyr::round_any(max(studierende_faecher_frauen$wert), 1000, f = ceiling), tickInterval = 1000) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_colors(c("#154194")) %>%
      highcharter::hc_title(text = paste0("Am häufigsten gewählte Fächer von Frauen ", "(", timerange, ")"),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE)
    # %>%
    #   highcharter::hc_exporting(enabled = TRUE,
    #                             buttons = list(contextButton = list(
    #                               symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
    #                               onclick = highcharter::JS("function () {
    #                                                           this.exportChart({ type: 'image/jpeg' }); }"),
    #                               align = 'right',
    #                               verticalAlign = 'bottom',
    #                               theme = list(states = list(hover = list(fill = '#FFFFFF'))))))


    # Create male plot
    hc_mann <- highcharter::hchart(studierende_faecher_maenner, 'bar', highcharter::hcaes(y = wert, x = fach)) %>%
      highcharter::hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, format = "{point.wert}")
        )) %>%
      highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.prop} % <br> Absolut: {point.wert}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}"), min = 0, max = plyr::round_any(max(studierende_faecher_maenner$wert), 1000, f = ceiling), tickInterval = 1000) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_colors(c("#66cbaf")) %>%
      highcharter::hc_title(text = paste0("Am häufigsten gewählte Fächer von Männern ", "(", timerange, ")"),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = TRUE)
    # %>%
    #   highcharter::hc_exporting(enabled = TRUE,
    #                             buttons = list(contextButton = list(
    #                               symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
    #                               onclick = highcharter::JS("function () {
    #                                                           this.exportChart({ type: 'image/jpeg' }); }"),
    #                               align = 'right',
    #                               verticalAlign = 'bottom',
    #                               theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

  }


  highcharter::hw_grid(hc_frau,
                       hc_mann,
                       ncol = 2,
                       browsable = TRUE)

}

#' A function to plot the german map
#'
#' @description A function to plot the german map with all states that contain
#' information about the share of women in STEM
#'
#' @return The return value is the german map with information
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studierende_map <- function(r) {


  # load UI inputs from reactive value
  timerange <- r$map_y

  label_m <- r$map_l


  # Fach abhängig von Lehramt ja/nein zuweisen
  if(label_m == "Studierende (Lehramt)")  fach_m <- r$map_f_lehr
  if(label_m != "Studierende (Lehramt)")  fach_m <- r$map_f


  # filter dataset based on UI inputs
  df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    dplyr::filter(jahr == timerange,
                  region != "Deutschland") %>%
    dplyr::collect()

  df <- df %>%
    dplyr::select(-fachbereich,- mint_select, -typ )%>%
    tidyr::pivot_wider(names_from = fach, values_from = wert)%>%
    dplyr::mutate(dplyr::across(c(6:ncol(.)), ~round(./`Alle Fächer`*100,1)))%>%
    tidyr::pivot_longer(c(6:ncol(.)), values_to = "proportion", names_to ="fach")%>%
    dplyr::right_join(df)

  #Trennpunkte für lange Zahlen ergänzen
  df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

  df <- df %>%
    dplyr::select(indikator, region, jahr, fach, proportion, wert)%>%
    dplyr::filter(indikator== label_m)


  #Anteil mit weniger Nachkommerstellen für Hover
  # df7$prop <- df7$proportion
  # df7$prop <- round(df7$prop, 0)

  # Plot

  if (length(fach_m)==1)
  {
    # Vorbereitung Überschrift
    help_fach <- fach_m
    help_fach <- ifelse(help_fach == "Alle Nicht MINT-Fächer", "allen Fächern außer MINT", help_fach)
    help_fach <- ifelse(help_fach == "Alle MINT-Fächer", "MINT", help_fach)

    label_m <- ifelse(label_m == "Studierende", paste0(label_m, "n"), label_m)
    label_m <- ifelse(label_m == "Internationale Studierende", "internationalen Studierenden", label_m)
    label_m <- ifelse(grepl("Lehram", label_m), "Studierenden (Lehramt)", label_m)
    label_m <- ifelse(grepl("1. Hoch", label_m), "internationalen Studienanfänger:innen (1. Hochschulsemester)", label_m)

    help_l <- "Studierenden"
    help_l <- ifelse(grepl("1. Hoch", label_m), "internationalen Studienanfänger:innen", help_l)
    help_l <- ifelse(grepl("1. Fach", label_m), "Studienanfänger:innen", help_l)

    data_map_1 <- df %>% dplyr::filter(fach == fach_m)%>%
      dplyr::mutate(display = as.character(proportion))
    #title_map_1 <-

    highcharter::hw_grid(


      # plot
      highcharter::hcmap(
        "countries/de/de-all",
        data = data_map_1,
        value = "proportion",
        joinBy = c("name", "region"),
        borderColor = "#FAFAFA",
        name = paste0(fach_m),
        borderWidth = 0.1,
        nullColor = "#A9A9A9",
        tooltip = list(
          valueDecimals = 0,
          valueSuffix = "%"
        )
        #,
        #download_map_data = FALSE
      )
      %>%
        highcharter::hc_tooltip(pointFormat = "{point.region} <br> Anteil: {point.display} % <br> Anzahl: {point.wert}") %>%
        highcharter::hc_colorAxis(min=0, minColor= "#f4f5f6", maxColor="#b16fab",labels = list(format = "{text}%")) %>%
        highcharter::hc_title(
          text = paste0("Anteil von ", label_m, " in ", help_fach, " an allen ", help_l, " (", timerange, ")"),
          margin = 10,
          align = "center",
          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
        ) %>%
        # highcharter::hc_caption(
        #   text = "...",  style = list(color= "white", fontSize = "12px")
        # ) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular")
        ) %>% highcharter::hc_size(600, 550) %>%
        highcharter::hc_credits(enabled = FALSE) %>%
        highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                               verticalAlign = "bottom"))





  } else if(length(fach_m)==2){


    # Vorbereitung Überschrift
    help_fach <- fach_m[1]
    help_fach <- ifelse(help_fach == "Alle Nicht MINT-Fächer", "allen Fächern außer MINT", help_fach)
    help_fach <- ifelse(help_fach == "Alle MINT-Fächer", "MINT", help_fach)
    help_fach2 <- fach_m[2]
    help_fach2 <- ifelse(help_fach2 == "Alle Nicht MINT-Fächer", "allen Fächern außer MINT", help_fach2)
    help_fach2 <- ifelse(help_fach2 == "Alle MINT-Fächer", "MINT", help_fach2)

    label_m <- ifelse(label_m == "Studierende", paste0(label_m, "n"), label_m)
    label_m <- ifelse(label_m == "Internationale Studierende", "internationalen Studierenden", label_m)
    label_m <- ifelse(grepl("Lehram", label_m), "Studierenden (Lehramt)", label_m)
    label_m <- ifelse(grepl("1. Hoch", label_m), "internationalen Studienanfänger:innen (1. Hochschulsemester)", label_m)


    help_l <- label_m
    help_l <- ifelse(grepl("1. Hoch", label_m), "internationalen Studienanfänger:innen", help_l)
    help_l <- ifelse(grepl("1. Fach", label_m), "Studienanfänger:innen", help_l)


    data_map_1 <- df %>% dplyr::filter(fach == fach_m[1])%>%
      dplyr::mutate(display = as.character(proportion))

    data_map_2 <- df %>% dplyr::filter(fach == fach_m[2])%>%
      dplyr::mutate(display = as.character(proportion))


    highcharter::hw_grid(
      #title(paste0("MINT-", label_m, " in ", timerange)),


      # plot
      highcharter::hcmap(
        "countries/de/de-all",
        data = data_map_1,
        value = "proportion",
        joinBy = c("name", "region"),
        borderColor = "#FAFAFA",
        name = paste0(fach_m[1]),
        borderWidth = 0.1,
        nullColor = "#A9A9A9",
        tooltip = list(
          valueDecimals = 0,
          valueSuffix = "%"
        )
        #,
        #download_map_data = FALSE
      )
      %>%
        highcharter::hc_tooltip(pointFormat = "{point.region} <br> Anteil: {point.display} % <br> Anzahl: {point.wert}") %>%
        highcharter::hc_colorAxis(min=0, minColor= "#f4f5f6", maxColor="#b16fab",labels = list(format = "{text}%")) %>%
        highcharter::hc_title(
          text = paste0("Anteil von ", label_m, " in ", help_fach, " an allen ", help_l, " (", timerange, ")"),
          margin = 10,
          align = "center",
          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
        ) %>%
        # highcharter::hc_caption(
        #   text = "...",  style = list(color= "white", fontSize = "12px")
        # ) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular")
        ) %>% highcharter::hc_size(600, 550) %>%
        highcharter::hc_credits(enabled = FALSE) %>%
        highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                               verticalAlign = "bottom"),





      highcharter::hcmap(
        "countries/de/de-all",
        data = data_map_2,
        value = "proportion",
        joinBy = c("name", "region"),
        borderColor = "#FAFAFA",
        name = paste0(fach_m[2]),
        borderWidth = 0.1,
        nullColor = "#A9A9A9",
        tooltip = list(
          valueDecimals = 0,
          valueSuffix = "%"
        )
        #,
        #download_map_data = FALSE
      )
      %>%
        highcharter::hc_tooltip(pointFormat = "{point.region} <br> Anteil: {point.display} % <br> Anzahl: {point.wert}") %>%
        highcharter::hc_colorAxis(min=0, minColor= "#f4f5f6", maxColor="#b16fab",labels = list(format = "{text}%")) %>%
        highcharter::hc_title(
          text = paste0("Anteil von ", label_m, " in ", help_fach2, " an allen ", help_l, " (", timerange, ")"),
          margin = 10,
          align = "center",
          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
        ) %>%
        # highcharter::hc_caption(
        #   text = "...",  style = list(color= "white", fontSize = "12px")
        # ) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular")
        ) %>% highcharter::hc_size(600, 550) %>%
        highcharter::hc_credits(enabled = FALSE) %>%
        highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                               verticalAlign = "bottom"),



      ncol = 2,
      browsable = TRUE)

  } else if(length(fach_m)==3){

    data_map_1 <- df %>% dplyr::filter(fach == fach_m[1])
    data_map_2 <- df %>% dplyr::filter(fach == fach_m[2])
    data_map_3 <- df %>% dplyr::filter(fach == fach_m[3])


    k <- highcharter::hcmap(
      "countries/de/de-all",
      data = data_map_1,
      value = "proportion",
      joinBy = c("name", "region"),
      borderColor = "#FAFAFA",
      name = paste0(fach_m[1]),
      borderWidth = 0.1,
      nullColor = "#A9A9A9",
      tooltip = list(
        valueDecimals = 0,
        valueSuffix = "%"
      )
      #,
      #download_map_data = FALSE
    )
    # %>%
    #   highcharter::hc_colorAxis(min=0, minColor= "#f4f5f6", maxColor="#b16fab",labels = list(format = "{text}%")) %>%
    #   highcharter::hc_title(
    #     text = paste0("MINT-", label_m, " in ", timerange, " (", fach_m[1], ")"),
    #     margin = 10,
    #     align = "center",
    #     style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
    #   ) %>%
    #   # highcharter::hc_caption(
    #   #   text = "...",  style = list(color= "white", fontSize = "12px")
    #   # ) %>%
    #   highcharter::hc_chart(
    #     style = list(fontFamily = "SourceSans3-Regular")
    #   ) %>% highcharter::hc_size(600, 550) %>%
    #   highcharter::hc_credits(enabled = FALSE) %>%
    #   highcharter::hc_legend(layout = "horizontal", floating = FALSE,
    #                          verticalAlign = "bottom")

    l<-k
    f<-k


    htmltools::tags$div(
      class = "grid grid grid",
      tags$div(class = "col-2-3", k),
      tags$div(class = "col-1-3", l),
      tags$div(class = "col-2-3", k))


    # highcharter::hw_grid(
    #   #title(paste0("MINT-", label_m, " in ", timerange)),
    #
    #
    #   # plot
    #   highcharter::hcmap(
    #     "countries/de/de-all",
    #     data = data_map_1,
    #     value = "proportion",
    #     joinBy = c("name", "region"),
    #     borderColor = "#FAFAFA",
    #     name = paste0("Anteil ", fach_m[1]),
    #     borderWidth = 0.1,
    #     nullColor = "#A9A9A9",
    #     tooltip = list(
    #       valueDecimals = 0,
    #       valueSuffix = "%"
    #     )
    #   )
    #   %>%
    #     highcharter::hc_colorAxis(min=0, minColor= "#f4f5f6", maxColor="#b16fab",labels = list(format = "{text}%")) %>%
    #     highcharter::hc_title(
    #       text = paste0("MINT-", label_m, " in ", timerange, " (", fach_m[1], ")"),
    #       margin = 10,
    #       align = "center",
    #       style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
    #     ) %>%
    #     # highcharter::hc_caption(
    #     #   text = "...",  style = list(color= "white", fontSize = "12px")
    #     # ) %>%
    #     highcharter::hc_chart(
    #       style = list(fontFamily = "SourceSans3-Regular")
    #     ) %>% highcharter::hc_size(600, 550) %>%
    #     highcharter::hc_credits(enabled = FALSE) %>%
    #     highcharter::hc_legend(layout = "horizontal", floating = FALSE,
    #                            verticalAlign = "bottom")%>%
    #     highcharter::hc_size(width=),
    #
    #
    #
    #
    #
    #   highcharter::hcmap(
    #     "countries/de/de-all",
    #     data = data_map_2,
    #     value = "proportion",
    #     joinBy = c("name", "region"),
    #     borderColor = "#FAFAFA",
    #     name = paste0("Anteil ", fach_m[2]),
    #     borderWidth = 0.1,
    #     nullColor = "#A9A9A9",
    #     tooltip = list(
    #       valueDecimals = 0,
    #       valueSuffix = "%"
    #     )
    #   )
    #   %>%
    #     highcharter::hc_colorAxis(min=0, minColor= "#f4f5f6", maxColor="#b16fab",labels = list(format = "{text}%")) %>%
    #     highcharter::hc_title(
    #       text = paste0("MINT-", label_m, " in ", timerange, " (", fach_m[2], ")"),
    #       margin = 10,
    #       align = "center",
    #       style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
    #     ) %>%
    #     # highcharter::hc_caption(
    #     #   text = "...",  style = list(color= "white", fontSize = "12px")
    #     # ) %>%
    #     highcharter::hc_chart(
    #       style = list(fontFamily = "SourceSans3-Regular")
    #     ) %>% highcharter::hc_size(600, 550) %>%
    #     highcharter::hc_credits(enabled = FALSE) %>%
    #     highcharter::hc_legend(layout = "horizontal", floating = FALSE,
    #                            verticalAlign = "bottom"),
    #
    #
    #   highcharter::hcmap(
    #     "countries/de/de-all",
    #     data = data_map_3,
    #     value = "proportion",
    #     joinBy = c("name", "region"),
    #     borderColor = "#FAFAFA",
    #     name = paste0("Anteil ", fach_m[3]),
    #     borderWidth = 0.1,
    #     nullColor = "#A9A9A9",
    #     tooltip = list(
    #       valueDecimals = 0,
    #       valueSuffix = "%"
    #     )
    #   )
    #   %>%
    #     highcharter::hc_colorAxis(min=0, minColor= "#f4f5f6", maxColor="#b16fab",labels = list(format = "{text}%")) %>%
    #     highcharter::hc_title(
    #       text = paste0("MINT-", label_m, " in ", timerange, " (", fach_m[3], ")"),
    #       margin = 10,
    #       align = "center",
    #       style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
    #     ) %>%
    #     # highcharter::hc_caption(
    #     #   text = "...",  style = list(color= "white", fontSize = "12px")
    #     # ) %>%
    #     highcharter::hc_chart(
    #       style = list(fontFamily = "SourceSans3-Regular")
    #     ) %>% highcharter::hc_size(600, 550) %>%
    #     highcharter::hc_credits(enabled = FALSE) %>%
    #     highcharter::hc_legend(layout = "horizontal", floating = FALSE,
    #                            verticalAlign = "bottom"),
    #
    #
    #
    #   ncol = 3,
    #   browsable = TRUE)

  }















  #   if(length(label_m)==1){
  #
  #     map_1 <- df2 %>%
  #       dplyr::filter(label == label_m)
  #
  #     highcharter::hcmap(
  #       "countries/de/de-all",
  #       data = map_1,
  #       value = "proportion",
  #       joinBy = c("name", "region"),
  #       borderColor = "#FAFAFA",
  #       name = paste0("Anteil ", subjects),
  #       borderWidth = 0.1,
  #       nullColor = "#A9A9A9",
  #       tooltip = list(
  #         valueDecimals = 0,
  #         valueSuffix = "%"
  #       )
  #     ) %>%
  #       highcharter::hc_colorAxis(min=0, minColor= "#f4f5f6", maxColor="#b16fab",labels = list(format = "{text}%")) %>%
  #       highcharter::hc_title(
  #         text = paste0("Studienanfänger:innen: Anteil von ", help_title, "<br>", timerange),
  #         margin = 10,
  #         align = "center",
  #         style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
  #       ) %>%
  #       # highcharter::hc_caption(
  #       #   text = "...",  style = list(color= "white", fontSize = "12px")
  #       # ) %>%
  #       highcharter::hc_chart(
  #         style = list(fontFamily = "SourceSans3-Regular")
  #       ) %>% highcharter::hc_size(600, 550) %>%
  #       highcharter::hc_credits(enabled = FALSE) %>%
  #       highcharter::hc_legend(layout = "horizontal", floating = FALSE,
  #                              verticalAlign = "bottom")
  #
  #
  #   }
  #
  #
  #   # #alles in gleiche Struktur bringen ohne Frauen da hier nicht relevant
  #   # df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")
  #   #
  #   # if(lehramt == FALSE){
  #   #
  #   #   df <- df %>% dplyr::filter(nur_lehramt == "Nein")
  #   #
  #   #   df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
  #   #
  #   # } else {
  #   #
  #   #   df <- df %>% dplyr::filter(nur_lehramt == "Ja")
  #   #
  #   #   df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  #   #
  #   # }
  #   #
  #   # # aggregate to MINT
  #   # df_sub <- calc_share_MINT_bl(df)
  #   #
  #   # df_sub <- df_sub[,colnames(df)]
  #   #
  #   # df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT-Fächer (gesamt)"
  #   #
  #   # df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (gesamt)"
  #   #
  #   # #df_sub <-  calc_share_male_bl(df_sub)
  #   #
  #   # # df_sub <-  df_sub %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
  #   # #   dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
  #   # #   dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
  #   # #                   wert[anzeige_geschlecht == "Männer"])
  #   #
  #   # #df_sub <- df_sub %>% dplyr::filter(anzeige_geschlecht != "Männer")
  #   #
  #   # df_sub <- df_sub %>%
  #   #   dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
  #   #   dplyr::mutate(wert_sum = sum(wert))
  #
  # #  df <- calc_share_male_bl(df)
  #
  #   # # calculate the new "Gesamt"
  #   # df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
  #   #   dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
  #   #   dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
  #   #                   wert[anzeige_geschlecht == "Männer"])
  #   #
  #   # df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")
  #
  #   df <- df %>%
  #     dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
  #     dplyr::mutate(wert_sum = sum(wert))
  #
  #   df <- rbind(df, df_sub)
  #
  #   df <- df %>% dplyr::filter(fachbereich == subjects)
  #
  #   # calculate proportions
  #   df <- df %>% dplyr::group_by(region, indikator) %>%
  #     dplyr::summarize(proportion = wert/wert_sum)
  #
  #   df$proportion <- df$proportion * 100
  #
  # df_scope <- df
  #   help_title <- ifelse(subjects == "MINT-Fächer (gesamt)", "MINT-Fächern (gesamt)", subjects)
  #
  #
  # highcharter::hw_grid(
  #     # plot
  #     highcharter::hcmap(
  #       "countries/de/de-all",
  #       data = df[df$indikator == "Studienanfänger:innen",],
  #       value = "proportion",
  #       joinBy = c("name", "region"),
  #       borderColor = "#FAFAFA",
  #       name = paste0("Anteil ", subjects),
  #       borderWidth = 0.1,
  #       nullColor = "#A9A9A9",
  #       tooltip = list(
  #         valueDecimals = 0,
  #         valueSuffix = "%"
  #       )
  #     ) %>%
  #       highcharter::hc_colorAxis(min=0, minColor= "#f4f5f6", maxColor="#b16fab",labels = list(format = "{text}%")) %>%
  #       highcharter::hc_title(
  #         text = paste0("Studienanfänger:innen: Anteil von ", help_title, "<br>", timerange),
  #         margin = 10,
  #         align = "center",
  #         style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
  #       ) %>%
  #        # highcharter::hc_caption(
  #        #   text = "...",  style = list(color= "white", fontSize = "12px")
  #        # ) %>%
  #       highcharter::hc_chart(
  #         style = list(fontFamily = "SourceSans3-Regular")
  #       ) %>% highcharter::hc_size(600, 550) %>%
  #       highcharter::hc_credits(enabled = FALSE) %>%
  #       highcharter::hc_legend(layout = "horizontal", floating = FALSE,
  #                              verticalAlign = "bottom"),
  #
  #     highcharter::hcmap(
  #       "countries/de/de-all",
  #       data = df[df$indikator == "Studierende",],
  #       value = "proportion",
  #       joinBy = c("name", "region"),
  #       borderColor = "#FAFAFA",
  #       name = paste0("Anteil ", subjects),
  #       borderWidth = 0.1,
  #       nullColor = "#A9A9A9",
  #       tooltip = list(
  #         valueDecimals = 0,
  #         valueSuffix = "%"
  #       )
  #     ) %>%
  #       highcharter::hc_colorAxis(min=0, minColor= "#f4f5f6", maxColor="#b16fab",labels = list(format = "{text}%")) %>%
  #       highcharter::hc_title(
  #         text = paste0("Studierende: Anteil von ", help_title, "<br>", timerange),
  #         margin = 10,
  #         align = "center",
  #         style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
  #       ) %>%
  #        # highcharter::hc_caption(
  #        #   text = "Ausgegraut: Daten stehen nicht zur Verfügung",  style = list(fontSize = "12px")
  #        # ) %>%
  #       highcharter::hc_chart(
  #         style = list(fontFamily = "SourceSans3-Regular")
  #       ) %>% highcharter::hc_size(600, 550) %>%
  #       highcharter::hc_credits(enabled = FALSE) %>%
  #       highcharter::hc_legend(layout = "horizontal", floating = FALSE, verticalAlign = "bottom"),
  #
  #
  #     ncol = 2,
  #     browsable = TRUE
  #   )
}

#' A function to create a bar plot
#'
#' @description A function to return a ranking o
#'
#' @return The return value is a bar plot
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

ranking_bl_subject <- function(r) {


  # load UI inputs from reactive value

  timerange <- r$rank_y

  states <- r$rank_states

  r_lab <- r$rank_l

  df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    dplyr::filter(jahr == timerange,
                  mint_select == "MINT" | fach %in% c("Alle MINT-Fächer",
                                                      "Alle Fächer",
                                                      "Alle Nicht MINT-Fächer"),
                  geschlecht == "Gesamt",
                  region == states)%>%
    dplyr::select(-bereich,- fachbereich) %>%
    dplyr::collect()

  df_ges <- df %>% dplyr::filter(fach == "Alle Fächer")%>%
    tidyr::pivot_wider(names_from=fach, values_from = wert)%>%
    dplyr::select(indikator, jahr, region, `Alle Fächer`)

  df <- df %>%
    dplyr::select(indikator, jahr, region, fach, wert)%>%
    dplyr::filter(fach != "Alle Fächer")%>%
    dplyr::left_join(df_ges , by=c("indikator", "jahr", "region"))

  df <- df %>%
    dplyr::mutate(prop = round(wert /`Alle Fächer` *100, 1 ))


  df <- df %>%
    dplyr::filter(indikator == r_lab)%>%
    dplyr::filter(!is.na(wert))%>%
    dplyr::filter(prop != 0,
                  fach != "Naturwissenschaften")


  ticks <- c("Alle MINT-Fächer",
             "Mathematik, Naturwissenschaften",
             "Mathematik",
             # "Naturwissenschaften",
             "Biologie",
             "Chemie",
             "Physik, Astronomie",
             "Pharmazie",
             "Geowissenschaften und Geographie",
             "Ingenieurwissenschaften (inkl. Informatik)",
             "Informatik",
             #"Ingenieurwissenschaften ohne Informatik",
             "Maschinenbau/Verfahrenstechnik" ,
             "Elektrotechnik und Informationstechnik",
             "Verkehrstechnik, Nautik",
             "Architektur, Innenarchitektur",
             "Raumplanung",
             "Bauingenieurwesen",
             "Vermessungswesen",
             "Wirtschaftsingenieurwesen mit ingenieurwissenschaftlichem Schwerpunkt",
             "Materialwissenschaft und Werkstofftechnik",
             "Bergbau, Hüttenwesen",

             "Alle Nicht MINT-Fächer"
  )



  df_t <- df %>%
    dplyr::select(fach)%>%
    unique()%>%
    as.vector()%>%
    unlist()%>%
    unname()

  ticks1 <- ticks[ticks %in% df_t]

  df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

  highcharter::hchart(df, 'bar', highcharter::hcaes(y=prop, x= fach))%>%
    highcharter::hc_tooltip(pointFormat = "{point.region} <br> Anteil: {point.y} % <br> Anzahl: {point.wert}") %>% #Inhalt für Hover-Box
    highcharter::hc_yAxis(title = list(text=""), labels = list(format = "{value}%")) %>% #x-Achse -->Werte in %
    highcharter::hc_xAxis(title= list(text=""),
                          categories = ticks1
    ) %>%
    #highcharter::hc_size(height = 800)%>%
    highcharter::hc_plotOptions(bar = list(
      colorByPoint = TRUE,
      colors = ifelse(df$fach %in% c("Alle MINT-Fächer", "Alle Nicht MINT-Fächer",
                                      "Ingenieurwissenschaften (inkl. Informatik)",
                                      "Mathematik, Naturwissenschaften"
      ), "#b16fab", "#d0a9cd")))%>%#balken lila für MINT
    highcharter::hc_title(text = paste0( "Anteil einzelner Fächer an allen Fächern ", "(", r_lab, ")" , " in ",states,
                                         br(), timerange,
                                         "<br><br><br>"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px"))


}




#' A function to create a bar plot
#'
#' @description A function to return a ranking of MINT
#'
#' @return The return value is a bar plot
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

mint_anteile <- function(r) {


  states <- r$anteile_states

  indi <- r$anteile_indi

  ordering <- r$anteile_order

  df_ges <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    dplyr::filter(mint_select == "MINT" & typ == "Einzelauswahl"| fach == "Alle MINT-Fächer",
                  geschlecht == "Gesamt",
                  fach == "Alle MINT-Fächer") %>%
    dplyr::collect()
  df_ges <- df_ges %>%
    tidyr::pivot_wider(names_from = fach, values_from = wert)%>%
    dplyr::select(indikator, region, jahr, `Alle MINT-Fächer`)


  df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    dplyr::filter(mint_select == "MINT" & typ == "Einzelauswahl"| fach == "Alle MINT-Fächer",
                  geschlecht == "Gesamt") %>%
    dplyr::collect()

  df<- df %>%
    dplyr::select(indikator, region, jahr, fach, wert)%>%
    dplyr::left_join(df_ges , by=c("indikator", "region", "jahr"))%>%
    dplyr::filter(fach != "Alle MINT-Fächer")%>%
    dplyr::mutate(prop = round(wert /`Alle MINT-Fächer`*100, 1))

  df <- df %>%
    dplyr::filter(indikator == indi)%>%
    dplyr::filter(region == states)


  get_all_years <- function(year){

    df1 <- df %>%dplyr::filter(jahr == year)

    df2 <- df %>%dplyr::filter(jahr == year)%>%
      dplyr::slice_head()%>%
      dplyr::mutate(prop=round(100-sum(df1$prop),1))%>%
      dplyr::mutate(wert = round(`Alle MINT-Fächer`*(prop/100),0))%>%
      dplyr::mutate(fach = "Andere MINT-Fächer")

    return(df2)

  }

  year_vec <- df %>%
    dplyr::select(jahr)%>%
    unique()%>%
    as.vector%>%
    unlist()%>%
    unname()

  df_gegenwert <- purrr::map(.f=get_all_years, .x =year_vec)%>%
    purrr::list_rbind()

  df <- df %>%
    dplyr::bind_rows(df_gegenwert)


  # direkt neu einlesen so
  df2_ges <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    dplyr::filter(fach=="Informatik" | fach == "Ingenieurwissenschaften ohne Informatik"|
                    fach == "Mathematik, Naturwissenschaften" | fach == "Alle MINT-Fächer",
                  geschlecht == "Gesamt") %>%
    dplyr::collect()

  #hier
  df2_ges <- df2_ges %>% dplyr::filter(fach == "Alle MINT-Fächer")%>%
    tidyr::pivot_wider(names_from = fach, values_from = wert)%>%
    dplyr::select(indikator, region, jahr, `Alle MINT-Fächer`)

  #hier
  df2 <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    dplyr::filter(fach=="Informatik" | fach == "Ingenieurwissenschaften ohne Informatik"|
                    fach == "Mathematik, Naturwissenschaften" | fach == "Alle MINT-Fächer",
                  geschlecht == "Gesamt") %>%
    dplyr::collect()

  df2 <- df2 %>%
    dplyr::select(indikator, region, jahr, fach, wert)%>%
    dplyr::left_join(df2_ges , by=c("indikator", "region", "jahr"))%>%
    dplyr::filter(fach != "Alle MINT-Fächer")%>%
    dplyr::mutate(prop = round(wert /`Alle MINT-Fächer`*100, 1))

  df2 <- df2 %>%
    dplyr::filter(indikator == indi)%>%
    dplyr::filter(region == states)

  df2$wert <- prettyNum(df2$wert, big.mark = ".", decimal.mark = ",")

  df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

  if(ordering=="MINT-Fächer"){

    #Reihenfolge sortieren
    df$fach <- as.factor(df$fach)
    df$fach <- factor(df$fach, levels = c(
      "Mathematik",
      "Biologie",
      "Chemie",
      "Physik, Astronomie",
      "Pharmazie",
      "Geowissenschaften und Geographie",
      "Informatik",
      "Maschinenbau/Verfahrenstechnik" ,
      "Elektrotechnik und Informationstechnik",
      "Verkehrstechnik, Nautik",
      "Architektur, Innenarchitektur",
      "Raumplanung",
      "Bauingenieurwesen",
      "Vermessungswesen",
      "Wirtschaftsingenieurwesen mit ingenieurwissenschaftlichem Schwerpunkt",
      "Materialwissenschaft und Werkstofftechnik",
      "Bergbau, Hüttenwesen",
      "Andere MINT-Fächer"))

    #plotting
    highcharter::hchart(df, 'bar', highcharter::hcaes(y = prop, x = jahr, group = fach))%>%
      highcharter::hc_tooltip(pointFormat = "{point.fach} <br> Anteil: {point.y} % <br> Anzahl: {point.wert}")%>%
      highcharter::hc_plotOptions(bar = list(stacking = "percent"))%>%
      highcharter::hc_yAxis(title = list(text = "")
                            , labels = list(format = "{value} %")) %>%
      highcharter::hc_xAxis(title = list(text = ""))%>%
      highcharter::hc_colors(c("#b16fab","#DCBED9","#D0A9CD","#BE88BA", "#ECDBEA","#F1EBEA",
                               "#66CBAF", "#BBD1FC", "#5F94F9", "#1B54C0", "#2D6BE1", "#EDF3FF",
                               "#DDFFF6", "#AFF3E0", "#35BD97", "#F4EFEE",
                               "#EFE8E6", "#D4C1BB"
      ))
    # %>%
    #   highcharter::hc_legend(itemDistance=10)

    # %>%
    #
    # highcharter::hc_plotOptions(column = list(pointWidth = 70))%>%
    # highcharter::hc_colors(c("#efe8e6", "#66cbaf")) %>%
    # highcharter::hc_yAxis(max = 40)%>%
    # highcharter::hc_title(text = paste0("Anteil internationaler ", help, " an allen ", help2, " in ", fach_help , " in ", bl_select ),
    #                       align = "center",
    #                       style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    # highcharter::hc_chart(
    #   style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    # ) %>%
    # highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
    # highcharter::hc_exporting(enabled = FALSE,
    #                           buttons = list(contextButton = list(
    #                             symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
    #                             onclick = highcharter::JS("function () {
    #                                                         this.exportChart({ type: 'image/png' }); }"),
    #                             align = 'right',
    #                             verticalAlign = 'bottom',
    #                             theme = list(states = list(hover = list(fill = '#FFFFFF'))))))


  } else if (ordering=="MINT-Aggregate"){

    #plotting
    highcharter::hchart(df2, 'bar', highcharter::hcaes(y = prop, x = jahr, group = fach))%>%
      highcharter::hc_tooltip(pointFormat = "{point.fach} <br> Anteil: {point.y} % <br> Anzahl: {point.wert}")%>%
      highcharter::hc_plotOptions(bar = list(stacking = "percent"))%>%
      highcharter::hc_yAxis(title = list(text = "")
                            , labels = list(format = "{value} %")) %>%
      highcharter::hc_xAxis(title = list(text = ""))%>%
      highcharter::hc_colors(c("#66cbaf", "#154194", "#b16fab" ))




  }



}

#' A function to plot a bar chart
#'
#' @description A function to create a bar chart to compare different subjects
#' for different Bundesländer.
#'
#' @return The return value is a bar chart
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studierende_mint_vergleich_bl <- function(r) {

  # load UI inputs from reactive value

  timerange <- r$bl_date

  r_lab1 <- r$rank_bl_l

  # Fach abhängig von Lehramt ja/nein zuweisen
  if(r_lab1 == "Studierende (Lehramt)")  fach_bl <- r$bl_f_lehr
  if(r_lab1 != "Studierende (Lehramt)")  fach_bl <- r$bl_f_alle


  df_ges <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    dplyr::filter(geschlecht=="Gesamt",
                  jahr %in% timerange) %>%
    dplyr::collect()

  df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    dplyr::filter(geschlecht=="Gesamt",
                  jahr %in% timerange) %>%
    dplyr::collect()

  df <- df %>%
    dplyr::select(-fachbereich,- mint_select, -typ )%>%
    tidyr::pivot_wider(names_from = fach, values_from = wert)%>%
    dplyr::mutate(dplyr::across(c(6:ncol(.)), ~round(./`Alle Fächer`*100,1)))%>%
    tidyr::pivot_longer(c(6:ncol(.)), values_to = "proportion", names_to ="fach")%>%
    dplyr::right_join(df_ges)%>%

    dplyr::collect()


  #Trennpunkte für lange Zahlen ergänzen
  df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

  df <- df %>%
    dplyr::select(indikator, region, jahr, fach, proportion, wert)


  df <- df %>%dplyr::filter(indikator == r_lab1 )%>%
    dplyr::filter(fach==fach_bl)

  # NA aus fach entfernen für BULAs mit weniger Studienfachgruppen
  df <- stats::na.omit(df)

  # Vorbereitung Überschrift
  help_s <- fach_bl
  help_s <- ifelse(help_s == "Alle Nicht MINT-Fächer", "allen Fächern außer MINT", help_s)
  help_s <- ifelse(help_s == "Alle MINT-Fächer", "MINT", help_s)


  r_lab1 <- ifelse(r_lab1 == "Studierende", paste0(r_lab1, "n"), r_lab1)
  r_lab1 <- ifelse(r_lab1 == "Internationale Studierende", "internationalen Studierenden", r_lab1)
  r_lab1 <- ifelse(grepl("Lehr", r_lab1), "Studierenden (Lehramt)", r_lab1)
  r_lab1 <- ifelse(r_lab1 == "Internationale Studienanfänger:innen (1. Hochschulsemester)",
                   "internationalenen Studienanfänger:innen (1. Hochschulsemester)", r_lab1)
  help <- r_lab1
  help <- ifelse(help == "internationalenen Studienanfänger:innen (1. Hochschulsemester)", "internationalen Studienanfänger:innen", help)
  help <- ifelse(help == "Studienanfänger:innen (1. Fachsemester)" |
                   help == "Studienanfänger:innen (1. Hochschulsemester)", "Studienanfänger:innen", help)


  # Plot
  highcharter::hchart(df, 'bar', highcharter::hcaes(x= region, y = proportion))%>%
    highcharter::hc_tooltip(pointFormat = "{point.fach} <br> Anteil: {point.y} % <br> Anzahl: {point.wert}") %>% #Inhalt für Hover-Box
    highcharter::hc_yAxis(title = list(text=""), labels = list(format = "{value}%")) %>% #x-Achse -->Werte in %
    highcharter::hc_xAxis(title= list(text="")) %>% #Y-Achse - keine Beschriftung
    highcharter::hc_colors("#b16fab") %>% #balken lila für MINT
    highcharter::hc_title(text = paste0( "Anteil von ", r_lab1 ," in ", help_s," an allen ", help,  " (", timerange, ")",
                                         "<br><br><br>"),
                          margin = 25,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px"))   #Schrift-Formatierung Überschrift


  #   ggplot2::ggplot(df, ggplot2::aes(y=region, x=proportion)) +
  #     ggplot2::geom_bar(stat="identity", fill = "#154194") +
  #     ggplot2::geom_text(ggplot2::aes(label=paste(round(proportion),"%")), hjust = -0.3,
  #                        fontface = "bold") +
  #     ggplot2::theme_minimal() +
  #     ggplot2::theme(
  #       text = ggplot2::element_text(size = 14),
  #       plot.title = ggtext::element_markdown(hjust = 0.5)) +
  #     ggplot2::ylab("") + ggplot2::xlab("") +
  #     ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
  #                                 "Anteil von ", help_title ," (", studium_level,  ") <br>", timerange,
  #                                  "<br><br><br>"),
  #                   fill = "") +
  #     ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))
  #
}


# Frauen in MINT ----


#' A function to plot a graph.
#'
#' @description A function to create a pie chart for the first box
#' inside the tab "Schule".
#'
#' @return The return value is a plot
#' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_einstieg_pie_gender <- function(r) {

  timerange <- r$gen_y
  genl <- r$gen_l

  df <- dplyr::tbl(con, from = "studierende") %>%
    dplyr::filter(jahr == timerange,
                  region=="Deutschland",
                  !fachbereich %in% c("MINT (Gesamt)", "Nicht MINT")) %>%
    dplyr::collect()

  df <- df %>%
    tidyr::pivot_wider(names_from= fachbereich, values_from = wert)%>%
    dplyr::mutate("MINT (Gesamt)" = Ingenieurwissenschaften + `Mathematik, Naturwissenschaften` )%>%
    dplyr::mutate("Nicht MINT" = Alle - `MINT (Gesamt)`)%>%
    dplyr::select(- Ingenieurwissenschaften, -`Mathematik, Naturwissenschaften`, - Alle)%>%
    tidyr::pivot_longer(c("MINT (Gesamt)", "Nicht MINT" ), values_to = "wert", names_to = "fachbereich" )


  # calculation props
  df_gender_anteil <- df %>%
    tidyr::pivot_wider(values_from=wert, names_from=geschlecht)%>%
    dplyr::mutate(across(c("Männer", "Frauen"), ~round(./Gesamt*100,1)))%>%
    dplyr::select(-Gesamt)%>%
    tidyr::pivot_longer(c("Männer", "Frauen"), names_to = "geschlecht", values_to = "proportion")

  # joining
  df <- df %>%
    dplyr::right_join(df_gender_anteil)%>%
    dplyr::filter(fachbereich != "Nicht MINT")



  #Trennpunkte für lange Zahlen ergänzen
  df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")


  if(length(genl) == 1) {

    df_pie <- df %>% dplyr::filter(indikator == genl)

    highcharter::hw_grid(

      highcharter::hchart(df_pie, size = 280, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion))

      %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage}% <br> Anzahl: {point.wert}'))
      %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text = paste0("Frauenanteil unter MINT-",genl[1], " in ", timerange),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
        #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)
        ),

      ncol = 1,
      browsable = TRUE
    )

  } else if(length(genl) == 2) {

    # filter for UI input and ensure proportions sum to 1

    df_1_pie <- df %>% dplyr::filter(indikator == genl[1])

    df_2_pie <- df %>% dplyr::filter(indikator == genl[2])


    highcharter::hw_grid(
      highcharter::hchart(df_1_pie, size = 280, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion))

      %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text=paste0("Frauenanteil unter MINT-", genl[1], " in ", timerange),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),


      highcharter:: hchart(df_2_pie, size = 280, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion))
      %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage}% <br> Anzahl: {point.wert}'))%>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text=paste0("Frauenanteil unter MINT-",genl[2], " in ", timerange),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
        #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE)),

      ncol = 2,
      browsable = TRUE
    )


  } else if(length(genl) == 3) {

    # filter for UI input and ensure proportions sum to 1

    df_1_pie <- df %>% dplyr::filter(indikator == genl[1])

    df_2_pie <- df %>% dplyr::filter(indikator == genl[2])

    df_3_pie <- df %>% dplyr::filter(indikator == genl[3])


    highcharter::hw_grid(
      highcharter::hchart(df_1_pie, size = 170, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion))
      %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text=paste0("Frauenanteil unter MINT-",genl[1], " in ", timerange),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),


      highcharter::hchart(df_2_pie, size = 170, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion))


      %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
        highcharter::hc_title(text=paste0("Frauenanteil unter MINT-",genl[2], " in ", timerange),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE)),

      highcharter::hchart(df_3_pie, size = 170, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion))


      %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage}% <br> Anzahl: {point.wert}')) %>%
        highcharter::hc_colors(c( "#efe8e6", "#154194")) %>%
        highcharter::hc_title(text=paste0("Frauenanteil unter MINT-",genl[3], " in ", timerange),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
        #highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),



      ncol = 3,
      browsable = TRUE
    )
  }

  # df4 <- df2 %>% dplyr::filter(geschlecht == "gesamt")%>%
  #   dplyr::filter(region== "Deutschland")%>%
  #   dplyr::select(-hochschulform, -region)%>%
  #   tidyr::pivot_wider(names_from=fachbereich, values_from = wert)%>%
  #   dplyr::mutate("MINT (aggregiert)" = Mathematik_Naturwissenschaften+Ingenieurwissenschaften)%>%
  #   dplyr::mutate("Nicht MINt"= Alle-`MINT (aggregiert)`)%>%
  #   dplyr::mutate(MINT=`MINT (aggregiert)`/Alle)%>%
  #   dplyr::mutate("Nicht MINT"=`Nicht MINt`/Alle)%>%
  #   dplyr::select(-Ingenieurwissenschaften,- Mathematik_Naturwissenschaften,-Alle, -`MINT (aggregiert)`,- `Nicht MINt`)%>%
  #   tidyr::pivot_longer(c(MINT, `Nicht MINT`), names_to = "proportion", values_to = "wert")
  #
  # df4$wert <- df4$wert *100
  # df4$wert <- round(df4$wert, 0)
  #
  # # load UI inputs from reactive value
  # timerange <- r$date_studierende_einstieg_gender
  #
  # lehramt <- r$nurLehramt_studierende_einstieg_gender
  #
  # hochschulform_select_1 <- r$hochschulform_studierende_einstieg_1_gender
  #
  # hochschulform_select_2 <- r$hochschulform_studierende_einstieg_2_gender
  #
  # # filter dataset based on UI inputs
  # df <- df %>% dplyr::filter(jahr == timerange)
  #
  # # remove
  # df <- df %>% dplyr::filter(region == "Deutschland")
  #
  # df_gesamt <- df %>% dplyr::filter(fachbereich == "Alle",
  #                                   anzeige_geschlecht == "Gesamt",
  #                                   nur_lehramt == "Nein",
  #                                   hochschulform == "insgesamt") %>%
  #   dplyr::rename(wert_gesamt = "wert")
  #
  # df <- calc_share_MINT(df)
  #
  # df <- calc_share_male(df, "box_1")
  #
  # df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt")
  #
  # df <- df %>% dplyr::left_join(df_gesamt, by = c("region", "indikator",
  #                                           "jahr", "bereich")) %>%
  #   dplyr::rename(anzeige_geschlecht = "anzeige_geschlecht.x",
  #                 fachbereich = "fachbereich.x",
  #                 nur_lehramt = "nur_lehramt.x",
  #                 hochschulform = "hochschulform.x") %>%
  #   dplyr::select(-c("anzeige_geschlecht.y", "nur_lehramt.y",
  #                    "hochschulform.y", "fachbereich.y")) %>%
  #   dplyr::mutate(proportion = (wert/wert_gesamt)*100)
  #
  #
  # if(lehramt == FALSE){
  #
  #   df <- df %>% dplyr::filter(nur_lehramt == "Nein")
  #
  #   df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
  #
  #   df$indikator <- paste0(df$indikator, " (", df$hochschulform, ")")
  #
  # } else {
  #
  #   df <- df %>% dplyr::filter(nur_lehramt == "Ja")
  #
  #   df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  #
  # }
  #
  # df_studierende_mint <- df %>% dplyr::filter(fachbereich == "MINT",
  #                                             grepl("Studierende", indikator))
  #
  # df_anfaenger_mint <- df %>% dplyr::filter(fachbereich == "MINT",
  #                                             grepl("Studienanfänger:innen", indikator))
  #
  # df_studierende_rest <- df %>% dplyr::filter(fachbereich == "andere Studiengänge",
  #                                             grepl("Studierende", indikator))
  #
  # df_anfaenger_rest <- df %>% dplyr::filter(fachbereich == "andere Studiengänge",
  #                                             grepl("Studienanfänger:innen", indikator))
  #
  # plot_studierende_mint <- highcharter::hchart(df_studierende_mint, size = 280, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = proportion)) %>%
  #   highcharter::hc_tooltip(
  #     pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
  #   highcharter::hc_title(text = paste0("MINT-Fächer (Studierende) <br> ", timerange),
  #                         margin = 45,
  #                         align = "center",
  #                         style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
  #   highcharter::hc_chart(
  #     style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
  #   highcharter::hc_legend(enabled = TRUE) %>%
  #   highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
  #                                          dataLabels = list(enabled = TRUE,  format='{point.percentage:.0f}%'), showInLegend = TRUE))
  #
  #
  # plot_anfeanger_mint <- highcharter::hchart(df_anfaenger_mint, size = 280, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = proportion)) %>%
  #   highcharter::hc_tooltip(
  #     pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
  #   highcharter::hc_title(text = paste0("MINT-Fächer (Studienanfänger:innen) <br> ", timerange),
  #                         margin = 45,
  #                         align = "center",
  #                         style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
  #   highcharter::hc_chart(
  #     style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
  #   highcharter::hc_legend(enabled = TRUE) %>%
  #   highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
  #                                          dataLabels = list(enabled = TRUE, format='{point.percentage:.0f}%'), showInLegend = TRUE))
  #
  # plot_studierende_rest <- highcharter::hchart(df_studierende_rest, size = 150, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = proportion)) %>%
  #   highcharter::hc_tooltip(
  #     pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
  #   highcharter::hc_title(text = paste0("Nicht-MINT-Fächer (Studierende) <br> ", timerange),
  #                         margin = 45,
  #                         align = "center",
  #                         style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
  #   highcharter::hc_chart(
  #     style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
  #   highcharter::hc_legend(enabled = TRUE, y = -180) %>%
  #   highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
  #                                          dataLabels = list(enabled = TRUE,  format='{point.percentage:.0f}%'), showInLegend = TRUE,
  #                                          opacity = 0.7))
  #
  #
  # plot_anfeanger_rest <- highcharter::hchart(df_anfaenger_rest, size = 150, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = proportion)) %>%
  #   highcharter::hc_tooltip(
  #     pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
  #   highcharter::hc_title(text = paste0("Nicht-MINT-Fächer (Studienanfänger:innen) <br> ", timerange),
  #                         margin = 45,
  #                         align = "center",
  #                         style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
  #   highcharter::hc_chart(
  #     style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
  #   highcharter::hc_legend(enabled = TRUE, y = -180) %>%
  #   highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
  #                                          dataLabels = list(enabled = TRUE, format='{point.percentage:.0f}%'), showInLegend = TRUE,
  #                                          opacity = 0.7))
  #
  #
  # plot_anfeanger_mint <- plot_anfeanger_mint %>% highcharter::hc_colors(c("#154194","#efe8e6"))
  #
  # plot_studierende_mint <- plot_studierende_mint %>% highcharter::hc_colors(c("#154194","#efe8e6"))
  #
  # plot_anfeanger_rest <- plot_anfeanger_rest %>% highcharter::hc_colors(c("#154194", "#efe8e6"))
  #
  # plot_studierende_rest <- plot_studierende_rest %>% highcharter::hc_colors(c("#154194", "#efe8e6"))
  #
  # highcharter::hw_grid(
  #
  #   plot_anfeanger_mint,
  #   plot_studierende_mint,
  #
  #   plot_anfeanger_rest,
  #   plot_studierende_rest,
  #
  #   ncol = 2,
  #   browsable = TRUE
  # )
  #
  #
  #

}



#' A function to plot time series
#'
#' @description A function to plot the time series
#'
#' @return The return value, if any, from executing the function.
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_verlauf_single_gender <- function(r) {

  # load UI inputs from reactive value


  absolut_selector <- r$abs_zahlen

  timerange <- r$genz_date
  t <- as.character(timerange[1]:timerange[2])

  label_sel <- r$genzl

  # filter dataset based on UI inputs
  df <- dplyr::tbl(con, from = "studierende") %>%
    dplyr::filter(jahr %in% t,
                  region=="Deutschland",
                  fachbereich %in% c("MINT (Gesamt)", "Nicht MINT")) %>%
    dplyr::collect()

  # calculation props
  df_frauen <- df %>%
    tidyr::pivot_wider(values_from=wert, names_from=geschlecht)%>%
    dplyr::mutate(across(c("Männer", "Frauen"), ~round(./Gesamt*100,1)))%>%
    dplyr::select(-Gesamt,- Männer)%>%
    tidyr::pivot_longer( "Frauen", names_to = "geschlecht", values_to = "proportion")

  # joining
  df <- df %>%
    dplyr::right_join(df_frauen)%>%
    dplyr::filter(fachbereich != "Nicht MINT")%>%
    tidyr::pivot_longer(c("wert", "proportion"), values_to = "wert", names_to = "selector")%>%
    dplyr::mutate(selector=dplyr::case_when(
      selector=="wert" ~ "Anzahl",
      T~"In Prozent"
    ))


  if(absolut_selector=="In Prozent"){

    df <- df %>% dplyr::filter(selector=="In Prozent")

    df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

    if(length(label_sel) == 1) {


      df <- df %>% dplyr::filter(indikator== label_sel)


      highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group=indikator))%>%
        highcharter::hc_tooltip(pointFormat = "Anteil {point.indikator} <br> Wert: {point.y} %") %>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
        highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
        #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_title(text = paste0("Frauenanteil in MINT-Studienfächern"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf")) %>%
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

    } else if(length(label_sel)==2){


      df <- df %>% dplyr::filter(indikator == label_sel[1] | indikator == label_sel [2])


      highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group=indikator))%>%
        highcharter::hc_tooltip(pointFormat = "Anteil {point.indikator} <br> Wert: {point.y} %") %>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
        highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
        #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_title(text = paste0("Frauenanteil in MINT-Studienfächern"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf")) %>%
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

    } else if(length(label_sel == 3)){


      df<- df %>% dplyr::filter(indikator == label_sel[1] | indikator == label_sel [2] | indikator == label_sel [3])



      highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group=indikator))%>%
        highcharter::hc_tooltip(pointFormat = "Anteil {point.indikator} <br> Wert: {point.y} %") %>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
        highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
        #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
        highcharter::hc_title(text = paste0("Frauenanteil in MINT-Studienfächern"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf")) %>%
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


    }} else if(absolut_selector=="Anzahl"){

      hcoptslang <- getOption("highcharter.lang")
      hcoptslang$thousandsSep <- "."
      options(highcharter.lang = hcoptslang)

      df <- df%>%
        dplyr::filter(selector=="Anzahl")

      df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

      if(length(label_sel) == 1) {

        df <- df %>% dplyr::filter(label== label_sel)

        highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group=indikator))%>%
          highcharter::hc_tooltip(pointFormat = "Anzahl: {point.y}") %>%
          highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
          highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
          #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
          highcharter::hc_title(text = paste0("Anzahl an Frauen in MINT-Studienfächern"),
                                margin = 45,
                                align = "center",
                                style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
          highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf")) %>%
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

      } else if(length(label_sel)==2){


        df <- df %>% dplyr::filter(indikator == label_sel[1] | indikator == label_sel [2])


        highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group=indikator))%>%
          highcharter::hc_tooltip(pointFormat = "Anzahl: {point.y}") %>%
          highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
          highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
          #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
          highcharter::hc_title(text = paste0("Anzahl an Frauen in MINT-Studienfächern"),
                                margin = 45,
                                align = "center",
                                style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
          highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf")) %>%
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

      } else if(length(label_sel == 3)){


        df<- df %>% dplyr::filter(indikator == label_sel[1] | indikator == label_sel [2] | indikator == label_sel [3])



        highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = wert, group=indikator))%>%
          highcharter::hc_tooltip(pointFormat = "Anzahl: {point.y}") %>%
          highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
          highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
          #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
          highcharter::hc_title(text = paste0("Anzahl an Frauen in MINT-Studienfächern"),
                                margin = 45,
                                align = "center",
                                style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
          highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf")) %>%
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




      }}



  #highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = indikator)) %>%
  #   highcharter::hc_tooltip(pointFormat = "Anteil {point.indikator} <br> Wert: {point.y} %") %>%
  #   highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
  #   highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
  #   #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
  #   highcharter::hc_title(text = paste0("Anteil von Frauen in MINT-Studienfächern"),
  #                         margin = 45,
  #                         align = "center",
  #                         style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
  #   highcharter::hc_chart(
  #     style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
  #   ) %>%
  #   highcharter::hc_exporting(enabled = FALSE,
  #                             buttons = list(contextButton = list(
  #                               symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
  #                               onclick = highcharter::JS("function () {
  #                                                             this.exportChart({ type: 'image/png' }); }"),
  #                               align = 'right',
  #                               verticalAlign = 'bottom',
  #                               theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

  # aggregate MINT
  # df2 <- calc_share_MINT(df[(df$indikator == "Studierende"), ])
  #
  # df3 <- calc_share_MINT(df[(df$indikator == "Studienanfänger:innen"), ])
  #
  # df2 <- calc_share_male(df2, "box_1")
  #
  # df3 <- calc_share_male(df3, "box_1")
  #
  # df <- rbind(df2, df3)
  #
  # df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt")
  #
  # df <- df %>% dplyr::filter(fachbereich == "MINT")
  #
  #
  # if(lehramt == FALSE){
  #
  #   df <- df %>% dplyr::filter(nur_lehramt == "Nein")
  #
  #   df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
  #
  #   df$indikator <- paste0(df$indikator, " (", df$hochschulform, ")")
  #
  # } else {
  #
  #   df <- df %>% dplyr::filter(nur_lehramt == "Ja")
  #
  #   df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  #
  #   df$indikator <- paste0(df$indikator, " (", "Lehramt, " ,df$hochschulform, ")")
  #
  # }
  #
  # # aggregate
  # df <- df %>%
  #   dplyr::group_by(jahr, region, indikator, bereich) %>%
  #   dplyr::mutate(props = sum(wert))
  #
  #
  #
  # # calculate proportions
  # df <- df %>% dplyr::group_by(indikator, fachbereich, anzeige_geschlecht, jahr) %>%
  #   dplyr::summarize(proportion = wert/props)
  #
  # df <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")
  #
  # df$proportion <- df$proportion * 100
  #
  # df <- df[with(df, order(jahr, decreasing = FALSE)), ]
  #
  # # plot
  # highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = indikator)) %>%
  #   highcharter::hc_tooltip(pointFormat = "Anteil {point.indikator} <br> Wert: {point.y} %") %>%
  #   highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
  #   highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
  #   #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
  #   highcharter::hc_title(text = paste0("Anteil von Frauen in MINT-Studienfächern"),
  #                         margin = 45,
  #                         align = "center",
  #                         style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
  #   highcharter::hc_chart(
  #     style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
  #   ) %>%
  #   highcharter::hc_exporting(enabled = FALSE,
  #                             buttons = list(contextButton = list(
  #                               symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
  #                               onclick = highcharter::JS("function () {
  #                                                             this.exportChart({ type: 'image/png' }); }"),
  #                               align = 'right',
  #                               verticalAlign = 'bottom',
  #                               theme = list(states = list(hover = list(fill = '#FFFFFF'))))))


}


#' A function to plot time series
#'
#' @description A function to plot a bar chart
#'
#' @return The return value, if any, from executing the function.
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienzahl_einstieg_comparison_gender <- function(r) {

  # load UI inputs from reactive value
  timerange <- r$gen_f_y

  sel_bl1 <- r$gen_states

  # Zuweisung von r zu sel_f in abhängigkeit der Bundesländer

  if(sel_bl1 %in% c("Deutschland",
                    "Baden-Württemberg",
                    "Bayern",
                    "Berlin",
                    "Hamburg",
                    "Hessen",
                    "Nordrhein-Westfalen",
                    "Rheinland-Pfalz",
                    "Sachsen",
                    "Westdeutschland (o. Berlin)",
                    "Ostdeutschland (inkl. Berlin)")) {
    sel_f1 <- r$gen1_f
  }
  else {
    if(sel_bl1 == "Brandenburg") sel_f1 <- r$gen2_f
    if(sel_bl1 == "Bremen") sel_f1 <- r$gen3_f
    if(sel_bl1 == "Mecklenburg-Vorpommern") sel_f1 <- r$gen4_f
    if(sel_bl1 == "Niedersachsen") sel_f1 <- r$gen5_f
    if(sel_bl1 == "Saarland") sel_f1 <- r$gen6_f
    if(sel_bl1 == "Sachsen-Anhalt") sel_f1 <- r$gen7_f
    if(sel_bl1 == "Schleswig-Holstein") sel_f1 <- r$gen8_f
    if(sel_bl1 == "Thüringen") sel_f1 <- r$gen9_f
  }

  # filter dataset based on UI inputs
  df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
    dplyr::filter(jahr %in% timerange,
                  region==sel_bl1,
                  fach==sel_f1)%>%
    dplyr::collect()

  df_gen <- df %>%
    tidyr::pivot_wider(names_from = geschlecht, values_from = wert) %>%
    dplyr::mutate(across(c("Männer", "Frauen"), ~ round(./Gesamt*100,1)))%>%
    dplyr::select(-Gesamt)%>%
    tidyr::pivot_longer(c("Männer", "Frauen"), names_to = "geschlecht", values_to  = "proportion")%>%
    dplyr::filter(indikator !="Internationale Studienanfänger:innen (1. Hochschulsemester)"&indikator!= "Internationale Studierende"  )

  df <- df %>%
    tidyr::pivot_wider(names_from = geschlecht, values_from = wert) %>%
    dplyr::select(-Gesamt)%>%
    tidyr::pivot_longer(c("Männer", "Frauen"), values_to = "wert", names_to = "geschlecht")%>%
    dplyr::right_join(df_gen)%>%
    dplyr::filter(!is.nan(proportion))


  #Trennpunkte für lange Zahlen ergänzen
  df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")

  #überschrift vorbereiten
  fach_label <- sel_f1
  fach_label <- ifelse(fach_label == "Alle MINT-Fächer", "MINT", fach_label)


  highcharter::hchart(df, 'bar', highcharter::hcaes(x = indikator, y=proportion, group = geschlecht))%>%

    highcharter::hc_tooltip(pointFormat = "{point.geschlecht}-Anteil: {point.y} % <br> Anzahl: {point.wert}")%>%

    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  FALSE) %>%
    highcharter::hc_xAxis(title = list(text = "")
                          #                       , categories=c("Studienanfänger:innen in MINT",
                          #                                                             "Studienanfänger:innen in anderen Studiengängen",
                          #                                                             "Studierende in MINT",
                          #                                                             "Studierende in anderen Studiengängen",
                          #                                                             "Lehrarmt-Studienanfänger:innen in MINT",
                          #                                                             "Lehramt-Studienanfänger:innen in anderen Studiengängen",
                          #                                                             "Lehramt-Studierende in MINT",
                          #                                                             "Lehramt-Studierende in anderen Studiengängen"
                          # )
    ) %>%
    highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
    highcharter::hc_colors(c("#154194", "#efe8e6")) %>%
    highcharter::hc_title(text = ifelse(sel_f1 == "Alle Nicht MINT-Fächer", paste0("Frauenanteil in allen Studienfachgruppen außer MINT in ", sel_bl1, " (", timerange, ")",
                                                                                   "<br><br><br>"), paste0("Frauenanteil in der Studienfachgruppe ", fach_label, " in ", sel_bl1, " (", timerange, ")",
                                                                                                           "<br><br><br>")),
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




}

# Internationale Studis ----


plot_auslaender_mint <- function(r){



  bl_select <- r$states_studium_studienzahl_ausl

  year_select <- r$date_studium_studienzahl_ausl

  absolut_selector <- r$abs_zahlen_studium_studienzahl_ausl

  status_select <- r$status_ausl

  betr_ebene <- r$ebene_ausl


  df_aus <- df

  df_aus_0 <- df_aus

  marker_mint <- df_aus_0%>%
    dplyr::filter(mint_select=="MINT")%>%
    dplyr::select(fach)%>%
    unique()

  marker_mint1 <- as.vector(unlist(marker_mint))


  marker_nicht_mint <- df_aus_0%>%
    dplyr::filter(mint_select=="Nicht MINT")%>%

    dplyr::select(fach)%>%
    unique()

  marker_mint2 <- as.vector(unlist(marker_nicht_mint))

  df_aus_1 <- df_aus_0 %>%
    dplyr::filter(indikator %in% c("Internationale Studienanfänger:innen (1. Hochschulsemester)",
                                   "Internationale Studierende",
                                   "Studienanfänger:innen (1. Hochschulsemester)",
                                   "Studierende"))%>%
    # tidyr::pivot_wider(names_from=region, values_from=wert)%>%
    # dplyr::mutate(Deutschland=rowSums(dplyr::across(c(9:ncol(.))),na.rm=TRUE))%>% #es liegen NAs vor, damit Rest berechent wird na.rm = TRUE
    # dplyr::mutate("Westdeutschland (o. Berlin)" =rowSums(dplyr::across(c(`Nordrhein-Westfalen`, `Hamburg`,
    #                                                                      Bayern, `Baden-Württemberg`, Saarland,
    #                                                                      `Schleswig-Holstein`, Hessen, Niedersachsen,
    #                                                                      `Rheinland-Pfalz`, `Bremen`)), na.rm = TRUE))%>%
    # dplyr::mutate("Ostdeutschland (inkl. Berlin)"=rowSums(dplyr::across(c(Berlin, Sachsen, `Mecklenburg-Vorpommern`,
    #                                                                       `Sachsen-Anhalt`, Brandenburg, Thüringen)), na.rm = TRUE))%>%
    # tidyr::pivot_longer(c(9:ncol(.)), names_to = "region", values_to="wert")%>%
    dplyr::select(-mint_select,- fachbereich)%>%
    # tidyr::pivot_wider(values_from = wert, names_from =fach )%>%
    # dplyr::mutate(MINT=rowSums(dplyr::across(marker_mint1), na.rm =T))%>%
    # dplyr::mutate(Nicht_MINT=rowSums(dplyr::across(marker_mint2), na.rm =T))%>%
    # dplyr::mutate("Ingenieurwissenschaften und Informatik"=`Ingenieurwissenschaften ohne Informatik`+Informatik)%>%
    # tidyr::pivot_longer(c(7:ncol(.)), names_to="fach", values_to="wert")%>%
    tidyr::pivot_wider(names_from=indikator, values_from = wert)%>%
    #dplyr::rename("Internationale Studierende" = `Ausländische Studierende`,"Internationale Studienanfänger:innen (1. Hochschulsemester)" = `Auländische Studienanfänger:innen (1. Hochschulsemester)` )%>%
    dplyr::mutate("Deutsche Studierende" =`Studierende`-`Internationale Studierende`,
                  "Deutsche Studienanfänger:innen (1. Hochschulsemester)"=`Studienanfänger:innen (1. Hochschulsemester)`-
                    `Internationale Studienanfänger:innen (1. Hochschulsemester)`)%>%
    dplyr::mutate("Deutsche Studierende_p" =`Deutsche Studierende`/Studierende,
                  "Internationale Studierende_p"= `Internationale Studierende`/Studierende,
                  "Deutsche Studienanfänger:innen (1. Hochschulsemester)_p" =`Deutsche Studienanfänger:innen (1. Hochschulsemester)`/`Studienanfänger:innen (1. Hochschulsemester)`,
                  "Internationale Studienanfänger:innen (1. Hochschulsemester)_p"=`Internationale Studienanfänger:innen (1. Hochschulsemester)`/`Studienanfänger:innen (1. Hochschulsemester)`)%>%
    dplyr::select(-c(Studierende, `Studienanfänger:innen (1. Hochschulsemester)` ))%>%
    dplyr::filter(geschlecht=="Gesamt")%>%
    tidyr::pivot_longer(c(7:ncol(.)), names_to="indikator", values_to="wert")%>%
    dplyr::mutate(selector=dplyr::case_when(stringr::str_ends(.$indikator, "_p")~"Relativ",
                                            T~"Asolut"))%>%
    dplyr::mutate(selector=dplyr::case_when(stringr::str_ends(.$indikator, "_p") ~ "In Prozent",
                                            T ~ "Anzahl"))%>%
    dplyr::mutate(ausl_detect=dplyr::case_when(stringr::str_detect(.$indikator, "International")~"International",
                                               T~ "Deutsch"))%>%
    dplyr::filter(!fach %in% c("Weitere ingenieurwissenschaftliche Fächer",
                               "Weitere naturwissenschaftliche und mathematische Fächer",
                               "Außerhalb der Studienbereichsgliederung/Sonstige Fächer"))

  df_aus_1$indikator <- gsub("_p", "", df_aus_1$indikator)

  df_aus_1$indikator <- gsub("Deutsche ", "", df_aus_1$indikator)

  df_aus_1$indikator <- gsub("Internationale ", "", df_aus_1$indikator)

  #df_aus_1$fach <- gsub("Nicht_MINT", "Nicht MINT", df_aus_1$fach)


  df_aus_1$ausl_detect  <- factor(df_aus_1$ausl_detect, levels=c("Deutsch", "International"))

  df_aus_2 <- df_aus_1 %>%
    dplyr::filter(region==bl_select) %>%
    dplyr::filter(jahr ==year_select )%>%
    dplyr::filter(indikator==status_select)


  df_aus_3 <- df_aus_2 %>%
    dplyr::filter(fach %in% c("Geisteswissenschaften",
                              "Mathematik, Naturwissenschaften",
                              "Rechts-, Wirtschafts- und Sozialwissenschaften",
                              "Humanmedizin/Gesundheitswissenschaften",
                              "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin",
                              "Sport",
                              "Kunst, Kunstwissenschaft",
                              "Alle Fächer",
                              "Alle MINT-Fächer",
                              "Alle Nicht MINT-Fächer",
                              "Ingenieurwissenschaften (inkl. Informatik)"))


  df_aus_4 <- df_aus_2 %>%
    dplyr::filter(!fach %in% c("Geisteswissenschaften",
                               "Mathematik, Naturwissenschaften",
                               "Rechts-, Wirtschafts- und Sozialwissenschaften",
                               "Humanmedizin/Gesundheitswissenschaften",
                               "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin",
                               "Sport",
                               "Kunst, Kunstwissenschaft",
                               "Ingenieurwissenschaften ohne Informatik",
                               "Alle Nicht MINT-Fächer",
                               "Alle MINT-Fächer",
                               "Alle Fächer",
                               "Mathematik, Naturwissenschaften",
                               "Ingenieurwissenschaften (inkl. Informatik)"))
  df_aus_4$fach <- as.factor(df_aus_4$fach)

  #Faktor für Höhe des Grafens berechnen
  ebene <- c("Fachbereiche", "MINT-Fächer")
  höhe <- c(8, 11)
  plt.add <- data.frame(ebene, höhe)

  # NA aus fach entfernen für BULAs mit weniger Studienfachgruppen
  df_aus_3 <- stats::na.omit(df_aus_3)
  df_aus_3 <- df_aus_3 %>%
    dplyr::arrange(wert)
  df_aus_4 <- stats::na.omit(df_aus_4)
  df_aus_4 <- df_aus_4 %>%
    dplyr::arrange(wert)


  # Vorbereitung Überschrift
  help <- "Studierender"
  help <- ifelse(grepl("anfänger", status_select), "Studienanfänger:innen", help)

  help2 <- "Studierenden"
  help2 <- ifelse(grepl("anfänger", status_select), "Studienanfänger:innen", help2)


  if(betr_ebene=="Fachbereiche"){



    if(absolut_selector=="In Prozent"){

      dfhh1 <- df_aus_3 %>%
        dplyr::filter(selector == "In Prozent")%>%
        dplyr::mutate(wert = round(wert*100, 1))




      highcharter::hchart(dfhh1, 'bar', highcharter::hcaes(y = wert, x = fach, group = ausl_detect))%>%
        highcharter::hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anteil: {point.y} %")%>%
        highcharter::hc_size(height = 60*plt.add$höhe[plt.add$ebene == betr_ebene])%>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
        highcharter::hc_xAxis(title = list(text = ""), categories = c("Alle Fächer",
                                                                      "Alle MINT-Fächer",
                                                                      "Alle Nicht MINT-Fächer",
                                                                      "",
                                                                      "Ingenieurwissenschaften (inkl. Informatik)",
                                                                      "Kunst, Kunstwissenschaft",
                                                                      "Mathematik, Naturwissenschaften",
                                                                      "Geisteswissenschaften",
                                                                      "Rechts-, Wirtschafts- und Sozialwissenschaften",
                                                                      "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin",
                                                                      "Humanmedizin/Gesundheitswissenschaften",
                                                                      "Sport")) %>%
        highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
        highcharter::hc_colors(c("#efe8e6", "#66cbaf")) %>%
        highcharter::hc_title(text = paste0("Anteil internationaler ", help, " an allen ", help2, " in ", bl_select,  " (",year_select, ")" ),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
        ) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_exporting(enabled = FALSE,
                                  buttons = list(contextButton = list(
                                    symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                    onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                    align = 'right',
                                    verticalAlign = 'bottom',
                                    theme = list(states = list(hover = list(fill = '#FFFFFF'))))))





    } else if(absolut_selector =="Anzahl"){

      hcoptslang <- getOption("highcharter.lang")
      hcoptslang$thousandsSep <- "."
      options(highcharter.lang = hcoptslang)

      dfh <- df_aus_3 %>%
        dplyr::filter(selector == "Anzahl")




      highcharter::hchart(dfh, 'bar', highcharter::hcaes(y = wert, x = fach, group = ausl_detect))%>%
        highcharter::hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anzahl: {point.y}")%>%
        highcharter::hc_size(height = 60*plt.add$höhe[plt.add$ebene == betr_ebene])%>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}")) %>%
        highcharter::hc_xAxis(title = list(text = ""), categories = c("Alle Fächer",
                                                                      "Alle MINT-Fächer",
                                                                      "Alle Nicht MINT-Fächer",
                                                                      "",
                                                                      "Ingenieurwissenschaften (inkl. Informatik)",
                                                                      "Rechts-, Wirtschafts- und Sozialwissenschaften",
                                                                      "Mathematik, Naturwissenschaften",
                                                                      "Geisteswissenschaften",
                                                                      "Kunst, Kunstwissenschaft",
                                                                      "Humanmedizin/Gesundheitswissenschaften",
                                                                      "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin",
                                                                      "Sport")) %>%
        #highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
        highcharter::hc_colors(c("#efe8e6", "#66cbaf")) %>%
        highcharter::hc_title(text = paste0("Anzahl internationaler ", help, " in ", bl_select,  " (",year_select, ")" ),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
        ) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
        highcharter::hc_exporting(enabled = FALSE,
                                  buttons = list(contextButton = list(
                                    symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                    onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                    align = 'right',
                                    verticalAlign = 'bottom',
                                    theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

    }} else if(betr_ebene == "MINT-Fächer"){

      if(absolut_selector=="In Prozent"){


        dfhh <- df_aus_4 %>%
          dplyr::filter(selector == "In Prozent")%>%
          dplyr::mutate(wert = round(wert*100, 1)) %>%
          dplyr::arrange(wert)


        highcharter::hchart(dfhh, 'bar', highcharter::hcaes(y = wert, x = fach, group = ausl_detect))%>%
          highcharter::hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anteil: {point.y} %")%>%
          highcharter::hc_size(height = 60*plt.add$höhe[plt.add$ebene == betr_ebene])%>%
          highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
          highcharter::hc_xAxis(title = list(text = "")
                                #                     , categories = c(
                                # "Mathematik, Naturwissenschaften",
                                # "Mathematik",
                                # "Physik, Astronomie",
                                # "Chemie",
                                # "Biologie",
                                # "Pharmazie",
                                # "Ingenieurwissenschaften (inkl. Informatik)",
                                # "Informatik",
                                # "Maschinenbau/Verfahrenstechnik",
                                # "Elektortechnik und Informationstechnik",
                                # "Verkerhtstechnik,Nautik",
                                # "Architektur, Innenarchitektur",
                                # "Raumplanung",
                                # "Buingenieurwesen",
                                # "Vermessungswesen",
                                # "Wirtschaftingenieurwesen mit ingenieurwissenschaftlichem Schwerpunkt",
                                # "Materialwissenschaft und Werkstofftechnik",
                                # "Berbau, Hüttenwesen",
                                # "Geowissenschaften und Geographie")
          ) %>%
          highcharter::hc_plotOptions(bar = list(stacking = "percent"))%>%
          highcharter::hc_colors(c("#efe8e6", "#66cbaf")) %>%

          highcharter::hc_title(text = paste0("Anteil internationaler ", help, " an allen ", help2, " in ", bl_select,  " (",year_select, ")" ),
                                margin = 45,
                                align = "center",
                                style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
          highcharter::hc_chart(
            style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
          ) %>%
          highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
          highcharter::hc_exporting(enabled = FALSE,
                                    buttons = list(contextButton = list(
                                      symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                      onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                      align = 'right',
                                      verticalAlign = 'bottom',
                                      theme = list(states = list(hover = list(fill = '#FFFFFF'))))))





      } else if(absolut_selector =="Anzahl"){

        hcoptslang <- getOption("highcharter.lang")
        hcoptslang$thousandsSep <- "."
        options(highcharter.lang = hcoptslang)

        df <- df_aus_4 %>%
          dplyr::filter(selector == "Anzahl") %>%
          dplyr::arrange(wert)

        df <- df[order(-df$wert),]

        highcharter::hchart(df, 'bar', highcharter::hcaes(y = wert, x = fach, group = ausl_detect))%>%
          highcharter::hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anzahl: {point.y}")%>%
          highcharter::hc_size(height = 60*plt.add$höhe[plt.add$ebene == betr_ebene])%>%
          highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}")) %>%
          highcharter::hc_xAxis(title = list(text = "")) %>%
          #highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
          highcharter::hc_colors(c("#efe8e6", "#66cbaf")) %>%
          highcharter::hc_title(text = paste0("Anzahl internationaler ", help, " in ", bl_select,  " (",year_select, ")" ),
                                margin = 45,
                                align = "center",
                                style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
          highcharter::hc_chart(
            style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
          ) %>%
          highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
          highcharter::hc_exporting(enabled = FALSE,
                                    buttons = list(contextButton = list(
                                      symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                      onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                      align = 'right',
                                      verticalAlign = 'bottom',
                                      theme = list(states = list(hover = list(fill = '#FFFFFF'))))))






      }}






}



# Internationale Studierende im Zeitverlauf



plot_auslaender_mint_zeit <- function(df, r){

  bl_select <- r$states_studium_studienzahl_ausl_zeit

  absolut_selector <- r$abs_zahlen_studium_studienzahl_ausl_zeit

  status_select <- r$status_ausl_zeit

  if(bl_select %in% c("Deutschland",
                      "Baden-Württemberg",
                      "Bayern",
                      "Berlin",
                      "Hamburg",
                      "Hessen",
                      "Nordrhein-Westfalen",
                      "Rheinland-Pfalz",
                      "Sachsen",
                      "Westdeutschland (o. Berlin)",
                      "Ostdeutschland (inkl. Berlin)")) {
    fach_select <- r$fach1_studium_studienzahl_ausl_zeit
  }
  else {
    if(bl_select == "Brandenburg")fach_select <- r$fach2_studium_studienzahl_ausl_zeit
    if(bl_select == "Bremen")fach_select <- r$fach3_studium_studienzahl_ausl_zeit
    if(bl_select == "Mecklenburg-Vorpommern")fach_select <- r$fach4_studium_studienzahl_ausl_zeit
    if(bl_select == "Niedersachsen")fach_select <- r$fach5_studium_studienzahl_ausl_zeit
    if(bl_select == "Saarland")fach_select <- r$fach6_studium_studienzahl_ausl_zeit
    if(bl_select == "Sachsen-Anhalt")fach_select <- r$fach7_studium_studienzahl_ausl_zeit
    if(bl_select == "Schleswig-Holstein")fach_select <- r$fach8_studium_studienzahl_ausl_zeit
    if(bl_select == "Thüringen")fach_select <- r$fach9_studium_studienzahl_ausl_zeit
  }

  df_aus <- df

  df_aus_0 <- df_aus

  marker_mint <- df_aus_0%>%
    dplyr::filter(mint_select=="MINT")%>%
    dplyr::select(fach)%>%
    unique()

  marker_mint1 <- as.vector(unlist(marker_mint))


  marker_nicht_mint <- df_aus_0%>%
    dplyr::filter(mint_select=="Nicht MINT")%>%

    dplyr::select(fach)%>%
    unique()

  marker_mint2 <- as.vector(unlist(marker_nicht_mint))

  df_aus_1 <- df_aus_0 %>%
    dplyr::filter(indikator %in% c("Internationale Studienanfänger:innen (1. Hochschulsemester)",
                                   "Internationale Studierende",
                                   "Studienanfänger:innen (1. Hochschulsemester)",
                                   "Studierende"))%>%
    # tidyr::pivot_wider(names_from=region, values_from=wert)%>%
    # dplyr::mutate(Deutschland=rowSums(dplyr::across(c(7:ncol(.))), na.rm=TRUE))%>% #na.rm = TRUE, da nicht alle Länder alle Fächer haben, sonst NAs
    # dplyr::mutate("Westdeutschland (o. Berlin)" =rowSums(dplyr::across(c(`Nordrhein-Westfalen`, `Hamburg`,
    #                                                                      Bayern, `Baden-Württemberg`, Saarland,
    #                                                                      `Schleswig-Holstein`, Hessen, Niedersachsen,
    #                                                                      `Rheinland-Pfalz`, `Bremen`)),na.rm=TRUE))%>%
    # dplyr::mutate("Ostdeutschland (inkl. Berlin)"=rowSums(dplyr::across(c(Berlin, Sachsen, `Mecklenburg-Vorpommern`,
    #                                                                       `Sachsen-Anhalt`, Brandenburg, Thüringen)), na.rm=TRUE))%>%
    # tidyr::pivot_longer(c(7:ncol(.)), names_to = "region", values_to="wert")%>%
    dplyr::select(-mint_select,- fachbereich)%>%
    # tidyr::pivot_wider(values_from = wert, names_from =fach )%>%
    # dplyr::mutate(MINT=rowSums(dplyr::across(marker_mint1), na.rm =T))%>%
    # dplyr::mutate(Nicht_MINT=rowSums(dplyr::across(marker_mint2), na.rm =T))%>%
    # tidyr::pivot_longer(c(5:ncol(.)), names_to="fach", values_to="wert")%>%
    tidyr::pivot_wider(names_from=indikator, values_from = wert)%>%
    #dplyr::rename("Internationale Studierende" = `Ausländische Studierende`,"Internationale Studienanfänger:innen (1. Hochschulsemester)" = `Auländische Studienanfänger:innen (1. Hochschulsemester)` )%>%
    dplyr::mutate("Deutsche Studierende" =`Studierende`-`Internationale Studierende`,
                  "Deutsche Studienanfänger:innen (1. Hochschulsemester)"=`Studienanfänger:innen (1. Hochschulsemester)`-
                    `Internationale Studienanfänger:innen (1. Hochschulsemester)`)%>%
    dplyr::mutate("Deutsche Studierende_p" =`Deutsche Studierende`/Studierende,
                  "Internationale Studierende_p"= `Internationale Studierende`/Studierende,
                  "Deutsche Studienanfänger:innen (1. Hochschulsemester)_p" =`Deutsche Studienanfänger:innen (1. Hochschulsemester)`/`Studienanfänger:innen (1. Hochschulsemester)`,
                  "Internationale Studienanfänger:innen (1. Hochschulsemester)_p"=`Internationale Studienanfänger:innen (1. Hochschulsemester)`/`Studienanfänger:innen (1. Hochschulsemester)`)%>%
    dplyr::select(-c(Studierende, `Studienanfänger:innen (1. Hochschulsemester)` ))%>%
    dplyr::filter(geschlecht=="Gesamt")%>%
    tidyr::pivot_longer(c(7:ncol(.)), names_to="indikator", values_to="wert")%>%
    dplyr::mutate(selector=dplyr::case_when(stringr::str_ends(.$indikator, "_p")~"Relativ",
                                            T~"Asolut"))%>%
    dplyr::mutate(selector=dplyr::case_when(stringr::str_ends(.$indikator, "_p") ~ "In Prozent",
                                            T ~ "Anzahl"))%>%
    dplyr::mutate(ausl_detect=dplyr::case_when(stringr::str_detect(.$indikator, "International")~"International",
                                               T~ "Deutsch"))

  df_aus_1$indikator <- gsub("_p", "", df_aus_1$indikator)

  df_aus_1$indikator <- gsub("Deutsche ", "", df_aus_1$indikator)

  df_aus_1$indikator <- gsub("Internationale ", "", df_aus_1$indikator)

  #df_aus_1$fach <- gsub("Nicht_MINT", "Nicht MINT", df_aus_1$fach)


  df_aus_1$ausl_detect  <- factor(df_aus_1$ausl_detect, levels=c("Deutsch", "International"))

  df_aus_2 <- df_aus_1 %>%
    dplyr::filter(region==bl_select) %>%
    dplyr::filter(fach ==fach_select )%>%
    dplyr::filter(indikator==status_select)

  # df1 <- df
  #
  #
  # highcharter::hchart(df1, 'bar', highcharter::hcaes(y = wert, x = fach, group = label))

  # Vorbereitung Überschrift
  help <- "Studierender"
  help <- ifelse(grepl("anfänger", status_select), "Studienanfänger:innen", help)

  help2 <- "Studierenden"
  help2 <- ifelse(grepl("anfänger", status_select), "Studienanfänger:innen", help2)

  fach_help <- fach_select
  fach_help <- ifelse(fach_help == "Alle MINT-Fächer", "MINT", fach_help)

  # Plot

  if(absolut_selector=="In Prozent"){

    dfk <- df_aus_2 %>%
      dplyr::filter(selector == absolut_selector)%>%
      dplyr::mutate(dplyr::across(wert, ~round(.*100, 1)))



    highcharter::hchart(dfk, 'column', highcharter::hcaes(y = wert, x = jahr, group = ausl_detect))%>%
      highcharter::hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anteil: {point.y} %")%>%
      # highcharter::hc_size(height = 1000)%>%
      highcharter::hc_yAxis(title = list(text = "")
                            , labels = list(format = "{value} %")
      ) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_plotOptions(column = list(stacking = "percent")) %>%
      highcharter::hc_plotOptions(column = list(pointWidth = 70))%>%
      highcharter::hc_colors(c("#efe8e6", "#66cbaf")) %>%
      highcharter::hc_yAxis(max = 40)%>%
      highcharter::hc_title(text = paste0("Anteil internationaler ", help, " an allen ", help2, " in ", fach_help , " in ", bl_select ),
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
      highcharter::hc_exporting(enabled = FALSE,
                                buttons = list(contextButton = list(
                                  symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                  onclick = highcharter::JS("function () {
                                                            this.exportChart({ type: 'image/png' }); }"),
                                  align = 'right',
                                  verticalAlign = 'bottom',
                                  theme = list(states = list(hover = list(fill = '#FFFFFF'))))))


  } else if(absolut_selector=="Anzahl"){

    df <- df_aus_2 %>%
      dplyr::filter(selector == absolut_selector)

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)



    highcharter::hchart(df, 'column', highcharter::hcaes(y = wert, x = jahr, group = ausl_detect))%>%
      highcharter::hc_tooltip(pointFormat = "{point.ausl_detect} <br> Anzahl: {point.y}")%>%
      # highcharter::hc_size(height = 1000)%>%
      highcharter::hc_yAxis(title = list(text = "")
                            , labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")
      ) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      #highcharter::hc_plotOptions(column = list(stacking = "percent")) %>%
      highcharter::hc_colors(c("#efe8e6", "#66cbaf")) %>%
      highcharter::hc_title(text =  paste0("Anzahl internationaler ", help, " in ", fach_help, " in ", bl_select),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
      highcharter::hc_exporting(enabled = FALSE,
                                buttons = list(contextButton = list(
                                  symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                  onclick = highcharter::JS("function () {
                                                            this.exportChart({ type: 'image/png' }); }"),
                                  align = 'right',
                                  verticalAlign = 'bottom',
                                  theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
  }



}


# Rest ----


studienzahl_einstieg_pie <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_studierende_einstieg

  lehramt <- r$nurLehramt_studierende_einstieg

  hochschulform_select_1 <- r$hochschulform_studierende_einstieg_1

  hochschulform_select_2 <- r$hochschulform_studierende_einstieg_2

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  # remove
  df <- df %>% dplyr::filter(region == "Deutschland")

  # aggregate MINT
  df2 <- calc_share_MINT(df[(df$indikator == "Studierende"), ])

  df3 <- calc_share_MINT(df[(df$indikator == "Studienanfänger:innen"), ])


  df <- rbind(df2, df3)

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

    df$indikator <- paste0(df$indikator, " (", df$hochschulform, ")")

    df_studierende <- df %>% dplyr::filter(indikator == paste0("Studierende", " (", df$hochschulform, ")"))

    # calculate proportions
    df_studierende <- share_pie(df_studierende)

    df_anfaenger <- df %>% dplyr::filter(indikator == paste0("Studienanfänger:innen", " (", df$hochschulform, ")"))

    # calculate proportions
    df_anfaenger <- share_pie(df_anfaenger)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)

    df$indikator <- paste0(df$indikator, " (", "Lehramt, " ,df$hochschulform, ")")

    df_studierende <- df %>% dplyr::filter(indikator == paste0("Studierende", " (", "Lehramt, " ,df$hochschulform, ")"))

    # calculate proportions
    df_studierende <- share_pie(df_studierende)

    df_anfaenger <- df %>% dplyr::filter(indikator == paste0("Studienanfänger:innen", " (", "Lehramt, " ,df$hochschulform, ")"))

    # calculate proportions
    df_anfaenger <- share_pie(df_anfaenger)


  }


    plot_studierende <- highcharter::hchart(df_studierende, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
      highcharter::hc_tooltip(
        pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
      highcharter::hc_title(text = paste0("Studienfachwahl (Studierende)", br(), timerange),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
      highcharter::hc_legend(enabled = TRUE) %>%
      highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                             dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE))


  plot_anfeanger <- highcharter::hchart(df_anfaenger, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
      highcharter::hc_tooltip(
        pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
      highcharter::hc_title(text = paste0("Studienfachwahl (Studienanfänger:innen)", br(), timerange),
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
    highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                             dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE))


  plot_anfeanger <- plot_anfeanger %>% highcharter::hc_colors(c("#efe8e6","#b16fab"))

  plot_studierende <- plot_studierende %>% highcharter::hc_colors(c("#efe8e6","#b16fab"))

    highcharter::hw_grid(

      plot_anfeanger,
      plot_studierende,

      ncol = 2,
      browsable = TRUE
    )




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

  lehramt_enthalten <- r$nurLehramt_studierende_einstieg

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

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





#' A function to plot a waffle chart :: b3
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

  # calculate proportions
  x_studierende <- prep_studium_proportion(df[df$indikator == "Studierende",])

  x_studienanfaenger <- prep_studium_proportion(df[df$indikator == "Studienanfänger:innen",])

  # set order
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
                   legend.position = "bottom") +
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
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))



  waffle_studienanfaenger_female <- waffle::waffle(x_studienanfaenger, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", "**Studienanfänger:innen**</span>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "bottom") +
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
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))



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
    ggplot2::theme_minimal() +
    ggplot2::facet_grid(~ anzeige_geschlecht) +
    ggplot2::theme_minimal(
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

  # calculate the proportions
  values_studierende <- (df[(df$anzeige_geschlecht == "Frauen" & df$indikator == "Studierende"),
                "wert"]/df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Studierende"), "wert"])*100

  values_studierende$region <- df[(df$anzeige_geschlecht == "Frauen" & df$indikator == "Studierende"), "region"][[1]]

  values_studienanfaenger <- (df[(df$anzeige_geschlecht == "Frauen" & df$indikator == "Studienanfänger:innen"),
                            "wert"]/df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Studienanfänger:innen"), "wert"])*100

  values_studienanfaenger$region <- df[(df$anzeige_geschlecht == "Frauen" & df$indikator == "Studienanfänger:innen"), "region"][[1]]


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
      ),
      download_map_data = FALSE
    ) %>%
      highcharter::hc_colorAxis(min=0, max=60, labels = list(format = "{text}%")) %>%
      highcharter::hc_title(
        text = paste0("Anteil der Studentinnen <br>",title_help ," an MINT in ", timerange),
        margin = 10,
        align = "center",
        style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
      ) %>%
       highcharter::hc_caption(
         text = "...",  style = list(color= "white", fontSize = "12px")
       ) %>%
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
      ),
      download_map_data = FALSE
    ) %>%
      highcharter::hc_title(
        text = paste0("Anteil der Studienanfänger:inneninnen <br> ",title_help ," an MINT in ", timerange),
        margin = 10,
        align = "center",
        style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
      ) %>%
       highcharter::hc_caption(
         text = "Ausgegraut: Daten stehen nicht zur Verfügung",  style = list(fontSize = "12px")
       ) %>%
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

  # include "Osten" und "Westen" in Dataframe
  df <- prep_studierende_east_west(df)


  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

    title_help_sub_sub <- " insgesamt"

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)

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

    title_help <- paste0("Studienanfänger:inneninnen ", title_help_sub, title_help_sub_sub)

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
  highcharter::hchart(values, 'line', highcharter::hcaes(x = jahr, y = round(props,1), group = region)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil Frauen <br> Bundesland: {point.region} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
    #highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil an ", title_help ," im Verlauf"),
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

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
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

  values$wert <- round(values$wert * 100,1)

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

  # remove - Muss BY und BW nicht entfernen, da MINT vs nicht-MINT in Daten enthalten
  df <- df %>% dplyr::filter(region != "Deutschland")

  #df <- df %>% dplyr::filter(region != "Bayern")

  #df <- df %>% dplyr::filter(region != "Baden-Württemberg")

  #damit alle selbe Srucktur haben; Frauen nicht relevant hier
  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

  # include "Osten" und "Westen" in Dataframe
  df <- prep_studierende_east_west(df)


  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

    title_help_sub_sub <- " insgesamt"

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)

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

  }

  # # calculate share of males for states
  # df <- calc_share_male_bl(df)
  #
  # # calculate new "Gesamt"
  # df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
  #   dplyr::group_by(region, fachbereich, indikator, jahr) %>%
  #   dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
  #                   wert[anzeige_geschlecht == "Männer"])

  # calculate proportions
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
  highcharter::hchart(values, 'line', highcharter::hcaes(x = jahr, y = round(proportion,1), group = anzeige_geschlecht)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil {point.anzeige_geschlecht} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
    #highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil von Student*innen im Verlauf"),
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








  # # df <- df %>% dplyr::filter(fachbereich != "Alle Fächer")
  #
  # df <- df %>% dplyr::filter(region != "Bayern")
  #
  # df <- df %>% dplyr::filter(region != "Baden-Württemberg")
  #
  # # include "Osten" und "Westen" in Dataframe
  # df <- prep_studierende_east_west(df)
  #
  # df <- df %>% dplyr::filter(region %in% states)
  #
  # if(lehramt == FALSE){
  #
  #   df <- df %>% dplyr::filter(nur_lehramt == "Nein")
  #
  #   df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
  #
  # } else {
  #
  #   df <- df %>% dplyr::filter(nur_lehramt == "Ja")
  #
  #   df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  #
  # }
  #
  # # aggregate all subjects to calculate proportion later
  # df_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen",
  #                                   fachbereich == "Alle") %>%
  #   dplyr::group_by(region, anzeige_geschlecht, indikator, nur_lehramt, hochschulform, jahr) %>%
  #   dplyr::mutate(wert_gesamt = sum(wert)) %>%
  #   dplyr::select(c("region", "indikator", "nur_lehramt",
  #                   "hochschulform", "jahr", "wert_gesamt"))
  #
  # # aggregate to MINT
  # values_Mint <- df %>%
  #   dplyr::filter(fachbereich != "Alle") %>%
  #   dplyr::group_by(jahr, region, indikator, anzeige_geschlecht, hochschulform,
  #                   nur_lehramt) %>%
  #   dplyr::summarise(wert = sum(wert)) %>%
  #   dplyr::mutate(bereich = "Hochschule",
  #                 fachbereich = "MINT (aggregiert)") %>%
  #   dplyr::filter(anzeige_geschlecht == "Frauen")
  #
  # einzelne_faecher <- df %>%
  #   dplyr::filter(anzeige_geschlecht == "Frauen")
  #
  # df_andere <- calc_share_MINT(df) %>%
  #   dplyr::filter(fachbereich == "andere Studiengänge",
  #                 anzeige_geschlecht == "Frauen")
  #
  # df <- rbind(values_Mint, einzelne_faecher, df_andere)
  #
  # # # calculate proportion
  # values <- df %>%
  #   dplyr::left_join(df_gesamt, by = c("region", "indikator", "nur_lehramt",
  #                                      "hochschulform", "jahr")) %>%
  #   dplyr::select(-"anzeige_geschlecht.y") %>%
  #   dplyr::rename(anzeige_geschlecht = "anzeige_geschlecht.x") %>%
  #   dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
  #   dplyr::select(-c("wert","wert_gesamt")) %>%
  #   dplyr::filter(fachbereich != "Alle",
  #                 fachbereich != "andere Studiengänge")
  #
  #
  # # order years for plot
  # values <- values[with(values, order(region, jahr, decreasing = FALSE)), ]
  #
  # #ifelse falsch
  # #values <- values %>% dplyr::mutate(fachbereich = ifelse(fachbereich == "MINT (aggregiert)", "MINT-Fächer (gesamt)", values$fachbereich ))
  #
  # values$fachbereich <- ifelse(values$fachbereich == "MINT (aggregiert)", "MINT-Fächer (gesamt)", values$fachbereich)
  #
  #
  # #[values$fachbereich=="MINT (aggregiert)"] <- "MINT (gesamt)"
  #
  # #values$fachbereich  <- gsub("MINT (aggregiert)", "MINT-Fächer (gesamt)", values$fachbereich)
  #
  # values <- values %>%  dplyr::filter(fachbereich == subjects_select)
  #
  # values$anzeige_geschlecht <- paste0(values$anzeige_geschlecht, " (", values$indikator, ")")
  #
  #
  # #
  #
  #
  #
  # #values[values$fachbereich == "MINT", "fachbereich"] <- "MINT-Fächer (gesamt)"
  #
  #
  #
  # #values$fachbereich  <- gsub("MINT (aggregiert)", "MINT-Fächer (gesamt)", values$fachbereich)
  #
  # help_title <- ifelse(subjects_select == "MINT-Fächer (gesamt)", "MINT-Fächern (gesamt)", subjects_select)


  # plot
  # highcharter::hchart(values, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = anzeige_geschlecht)) %>%
  #   highcharter::hc_tooltip(pointFormat = "Anteil {point.anzeige_geschlecht} <br> Wert: {point.y} %") %>%
  #   highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
  #   highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
  #   #highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
  #   highcharter::hc_title(text = paste0("Frauen: Studienfachwahl von ", help_title, " in ", states),
  #                         margin = 45,
  #                         align = "center",
  #                         style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
  #   highcharter::hc_chart(
  #     style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
  #   ) %>%
  #   highcharter::hc_exporting(enabled = FALSE,
  #                             buttons = list(contextButton = list(
  #                               symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
  #                               onclick = highcharter::JS("function () {
  #                                                             this.exportChart({ type: 'image/png' }); }"),
  #                               align = 'right',
  #                               verticalAlign = 'bottom',
  #                               theme = list(states = list(hover = list(fill = '#FFFFFF'))))))



#' A function to create a dumbbell plot
#'
#' @description A function to return a ranking of subject for both genders
#'
#' @return The return value is a dumbbell plot
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studienfaecher_ranking <- function(df,r, type) {


  timerange <- r$dumb_date

  label_choice <- r$dumbb_l

  bl_choice <- r$dumbb_states

  dfert <- df %>% dplyr::filter(jahr == timerange)

  df89 <- dfert %>%
    dplyr::filter(region != "Bayern" & region!= "Baden-Württemberg")%>%
    dplyr::select(-indikator,-hochschulform,-quelle, -bereich)%>%
    tidyr::pivot_wider(values_from = wert, names_from=fachbereich)%>%
    dplyr::mutate("andere Fächer" = Alle- Ingenieurwissenschaften-  Mathematik_Naturwissenschaften)%>%
    tidyr::pivot_longer(c(`andere Fächer`, Alle, Ingenieurwissenschaften, Mathematik_Naturwissenschaften ), values_to="wert", names_to = "fachbereich")%>%
    tidyr::pivot_wider(names_from = region, values_from = wert)%>%
    dplyr::mutate(Westdeutschland=rowSums(dplyr::select(.,c(
      Bremen,
      Hamburg,Hessen,Niedersachsen,
      `Nordrhein-Westfalen`,`Rheinland-Pfalz`,
      Saarland,`Schleswig-Holstein`)
    ),na.rm = T),
    Ostdeutschland=rowSums(dplyr::select(., c(Berlin,
                                              Brandenburg,
                                              `Mecklenburg-Vorpommern`,
                                              Sachsen,`Sachsen-Anhalt`,
                                              Thüringen)
    ),na.rm = T))%>%
    tidyr::pivot_longer(c(`Berlin`:Ostdeutschland), values_to = "wert", names_to="region")%>%

    tidyr::pivot_wider(names_from=fachbereich, values_from = wert)%>%
    dplyr::rename("Mathematik/Naturwissenschaften"=Mathematik_Naturwissenschaften)%>%
    dplyr::mutate(MINT=`Mathematik/Naturwissenschaften`+Ingenieurwissenschaften)%>%
    dplyr::mutate(dplyr::across(c(5,8,7,9), ~ round(./Alle*100,1)))%>%
    tidyr::pivot_longer(c(5:9), names_to = "fach", values_to= "proportion")%>%
    dplyr::filter(geschlecht=="frauen")%>%
    dplyr::filter(indikator == "Studierende" | indikator=="Studienanfänger:innen (1.Fachsemester)")%>%
    dplyr::filter(fach!= "Alle")

    dffn <- df89 %>% dplyr::filter(region == bl_choice)


    df2 <- dffn %>% tidyr::pivot_wider(names_from=indikator,values_from = proportion)


 # dffn <- df89 %>% dplyr::filter(label == label_choice)



  ggplot2::ggplot(df2,
                  ggplot2::aes(y = fach)) +
    ggplot2::geom_point(data = dffn, ggplot2::aes(x = proportion, color = indikator), size = 5) +
    ggalt::geom_dumbbell(
      ggplot2::aes(x = `Studienanfänger:innen (1.Fachsemester)`, xend = `Studierende`),
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
                                 "Frauen: Studienfachwahl nach Fächergruppen in ", bl_choice, "<br>", timerange,
                                 "<br><br><br>"),
                  color = "") +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))


  # load UI inputs from reactive value
  # timerange <- r$date_studium_ranking_bl_subject_gender
  #
  # states <- r$states_studium_ranking_bl_subject_gender
  #
  # lehramt <- r$nurLehramt_studium_ranking_bl_subject_gender
  #
  # hochschulform_select_1 <- r$hochschulform_studium_ranking_bl_subject_gender_1
  #
  # hochschulform_select_2 <- r$hochschulform_studium_ranking_bl_subject_gender_2
#
#   # filter dataset based on UI inputs
#   df <- df %>% dplyr::filter(jahr == timerange)
#
#   #df <- df %>% dplyr::filter(region != "Deutschland")
#
#   # df <- df %>% dplyr::filter(fachbereich != "Alle Fächer")
#
#   df <- df %>% dplyr::filter(region != "Bayern")
#
#   df <- df %>% dplyr::filter(region != "Baden-Württemberg")
#
#   # include "Osten" und "Westen" in Dataframe
#   df <- prep_studierende_east_west(df)
#
#   df <- df %>% dplyr::filter(region %in% states)
#
#   # df <- df %>% dplyr::mutate(indikator = replace(indikator,
#   #                                                indikator == "Studienanfänger",
#   #                                                "Studienanfängerinnen"))
#
#   if(lehramt == FALSE){
#
#     df <- df %>% dplyr::filter(nur_lehramt == "Nein")
#
#     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
#
#   } else {
#
#     df <- df %>% dplyr::filter(nur_lehramt == "Ja")
#
#     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
#
#   }
#
#   # aggregate all subjects to calculate proportion later
#   df_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen",
#                                     fachbereich == "Alle") %>%
#     dplyr::group_by(region, anzeige_geschlecht, indikator, nur_lehramt, hochschulform, jahr) %>%
#     dplyr::mutate(wert_gesamt = sum(wert)) %>%
#     dplyr::select(c("region", "indikator", "nur_lehramt",
#                     "hochschulform", "jahr", "wert_gesamt"))
#
#   # aggregate to MINT
#   values_Mint <- df %>%
#     dplyr::filter(fachbereich != "Alle") %>%
#     dplyr::group_by(jahr, region, indikator, anzeige_geschlecht, hochschulform,
#                     nur_lehramt) %>%
#     dplyr::summarise(wert = sum(wert)) %>%
#     dplyr::mutate(bereich = "Hochschule",
#                   fachbereich = "MINT (aggregiert)") %>%
#     dplyr::filter(anzeige_geschlecht == "Frauen")
#
#   einzelne_faecher <- df %>%
#     dplyr::filter(anzeige_geschlecht == "Frauen")
#
#   df_andere <- calc_share_MINT(df) %>%
#     dplyr::filter(fachbereich == "andere Studiengänge",
#                   anzeige_geschlecht == "Frauen")
#
#   df <- rbind(values_Mint, einzelne_faecher, df_andere)
#
#   # # calculate proportion
#   df <- df %>%
#     dplyr::left_join(df_gesamt, by = c("region", "indikator", "nur_lehramt",
#                                        "hochschulform", "jahr")) %>%
#     dplyr::select(-"anzeige_geschlecht.y") %>%
#     dplyr::rename(anzeige_geschlecht = "anzeige_geschlecht.x") %>%
#     dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
#     dplyr::select(-c("wert","wert_gesamt")) %>%
#     dplyr::filter(fachbereich != "Alle")
#
#   df[df$fachbereich == "MINT (aggregiert)","fachbereich"] <- "MINT (gesamt)"
#
#
#   # spread column
#   df <- tidyr::spread(df, indikator, proportion)
#
#   df <- df %>% tidyr::drop_na()
#
#   df <- df %>% dplyr::select(-hochschulform, -region, -anzeige_geschlecht)
#
#   df2 <- tidyr::gather(df, group, value, -fachbereich)%>%
#     dplyr::filter(group %in% c("Studienanfänger:innen", "Studierende")) %>%
#     dplyr::mutate(value = as.numeric(value))
#
#   df2$fachbereich <- factor(df2$fachbereich, levels = levels(df$fachbereich))
#
# #hier sind keine Daten mehr in df, wenn Deutschland ausgewählt ist, sonst schon
#
#   colnames(df)[7] <- "Studienanfängerinnen"


  # browser()
  # df_lo <-df

  #highcharter::hchart(df, 'dumbbell', highcharter::hcaes(y = Studienanfängerinnen, x= Studierende, group ="indikator"))

#hier sind dann auf einmal die Werte von Hessen (Default Wahl) in df wenn man Deutschland auswählt,
  #und ein Fehler in der App wird angezeigt

  # ggplot2::ggplot(df,
  #                 ggplot2::aes(y = fachbereich)) +
  #   ggplot2::geom_point(data = df2, ggplot2::aes(x = value, color = group), size = 5) +
  #   ggalt::geom_dumbbell(
  #     ggplot2::aes(x = Studienanfängerinnen, xend = Studierende),
  #     size = 0.5,
  #     size_x = 5,
  #     size_xend = 5,
  #     colour = "black",
  #     colour_x = "#b1b5c366",
  #     colour_xend = "#f5adac66",
  #     dot_guide=TRUE) +
  #   ggplot2::theme_minimal() +
  #   ggplot2::scale_color_manual(name = "", values = c("#b1b5c366", "#f5adac66")) +
  #   ggplot2::theme(legend.position="top",
  #                  panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
  #                  plot.title = ggtext::element_markdown(hjust = 0.5),
  #                  axis.text.y = ggplot2::element_text(size = 11)) +
  #   ggplot2::ylab("") + ggplot2::xlab("") +
  #   ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
  #                                "Frauen: Studienfachwahl nach Fächergruppen in ", states, "<br>", timerange,
  #                                "<br><br><br>"),
  #                 color = "") +
  #   ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))

}
#   browser()
#
#   # load UI inputs from reactive value
#   timerange <- r$date_studium_ranking_bl_subject_gender
#
#   states <- r$states_studium_ranking_bl_subject_gender
#
#   lehramt <- r$nurLehramt_studium_ranking_bl_subject_gender
#
#   hochschulform_select_1 <- r$hochschulform_studium_ranking_bl_subject_gender_1
#
#   hochschulform_select_2 <- r$hochschulform_studium_ranking_bl_subject_gender_2
#
#   # filter dataset based on UI inputs
#   df <- df %>% dplyr::filter(jahr == timerange)
#
#   # df <- df %>% dplyr::filter(fachbereich != "Alle Fächer")
#
#   df <- df %>% dplyr::filter(region != "Bayern")
#
#   df <- df %>% dplyr::filter(region != "Baden-Württemberg")
#
#   # include "Osten" und "Westen" in Dataframe
#   df <- prep_studierende_east_west(df)
#
#   df <- df %>% dplyr::filter(region %in% states)
#
#   df <- df %>% dplyr::mutate(indikator = replace(indikator,
#                                                  indikator == "Studienanfänger",
#                                                    "Studienanfänger:inneninnen"))
#
#   if(lehramt == FALSE){
#
#     df <- df %>% dplyr::filter(nur_lehramt == "Nein")
#
#     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
#
#   } else {
#
#     df <- df %>% dplyr::filter(nur_lehramt == "Ja")
#
#     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
#
#   }
#
#   # aggregate all subjects to calculate proportion later
#   df_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen",
#                                     fachbereich == "Alle") %>%
#     dplyr::group_by(region, anzeige_geschlecht, indikator, nur_lehramt, hochschulform, jahr) %>%
#     dplyr::mutate(wert_gesamt = sum(wert)) %>%
#     dplyr::select(c("region", "indikator", "nur_lehramt",
#                     "hochschulform", "jahr", "wert_gesamt"))
#
#   # aggregate to MINT
#   values_Mint <- df %>%
#     dplyr::filter(fachbereich != "Alle") %>%
#     dplyr::group_by(jahr, region, indikator, anzeige_geschlecht, hochschulform,
#                     nur_lehramt) %>%
#     dplyr::summarise(wert = sum(wert)) %>%
#     dplyr::mutate(bereich = "Hochschule",
#                   fachbereich = "MINT (aggregiert)") %>%
#     dplyr::filter(anzeige_geschlecht == "Frauen")
#
#   einzelne_faecher <- df %>%
#     dplyr::filter(anzeige_geschlecht == "Frauen")
#
#   df_andere <- calc_share_MINT(df) %>%
#     dplyr::filter(fachbereich == "andere Studiengänge",
#                   anzeige_geschlecht == "Frauen")
#
#   df <- rbind(values_Mint, einzelne_faecher, df_andere)
#
#   # # calculate proportion
#   df <- df %>%
#     dplyr::left_join(df_gesamt, by = c("region", "indikator", "nur_lehramt",
#                                        "hochschulform", "jahr")) %>%
#     dplyr::select(-"anzeige_geschlecht.y") %>%
#     dplyr::rename(anzeige_geschlecht = "anzeige_geschlecht.x") %>%
#     dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
#     dplyr::select(-c("wert","wert_gesamt")) %>%
#     dplyr::filter(fachbereich != "Alle")
#
#   df1 <- df
#
#   # spread column
#   df <- tidyr::spread(df, indikator, proportion)
#
#   df <- df %>% tidyr::drop_na()
#
#   df <- df %>% dplyr::select(-hochschulform, -region, -anzeige_geschlecht)
#
#   df$group <- df %>% dplyr::mutate(group= ifelse("Studienanfänger:innen", "Studienanfänger", df$group ))
#
#   df2 <- tidyr::gather(df, group, value, -fachbereich) %>%
#     dplyr::filter(group %in% c("Studienanfänger", "Studierende")) %>%
#     dplyr::mutate(value = as.numeric(value))
#
#   df2$fachbereich <- factor(df2$fachbereich, levels = levels(df$fachbereich))
#
#
#
#
#    ggplot2::ggplot(df,
#                   ggplot2::aes(y = fachbereich)) +
#     ggplot2::geom_point(data = df2, ggplot2::aes(x = value, color = group), size = 5) +
#     ggalt::geom_dumbbell(
#       ggplot2::aes(x =Studienanfänger, xend = Studierende),
#       size = 0.5,
#       size_x = 5,
#       size_xend = 5,
#       colour = "black",
#       colour_x = "#b1b5c366",
#       colour_xend = "#f5adac66",
#       dot_guide=TRUE) +
#     ggplot2::theme_minimal() +
#     ggplot2::scale_color_manual(name = "", values = c("#b1b5c366", "#f5adac66")) +
#     ggplot2::theme(legend.position="top",
#                    panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
#                    plot.title = ggtext::element_markdown(hjust = 0.5),
#                    axis.text.y = ggplot2::element_text(size = 11)) +
#     ggplot2::ylab("") + ggplot2::xlab("") +
#     ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
#                                  "Relativer Anteil von Studientinnen als Studienanfänger:innen oder Studierende in ",timerange,
#                                  "<br><br><br>"),
#                   color = "") +
#     ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))
#
# }



#' A function to plot the german map ::box 6
#'
#' @description A function to plot the german map with all states that contain
#' information about the share of women in STEM
#'
#' @return The return value is the german map with information
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studierende_map_gender <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_studium_studienzahl_bl_gender_map

  subjects <- r$subject_studium_studienzahl_bl_gender_map

  lehramt <- r$nurLehramt_studium_studienzahl_bl_gender_map

  hochschulform_select_1 <- r$hochschulform_studium_studienzahl_bl_gender_map1

  hochschulform_select_2 <- r$hochschulform_studium_studienzahl_bl_gender_map2

  indikator_choice <- r$level_studium_choice_gender

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(region != "Baden-Württemberg")

  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)

  }

  # aggregate to MINT
  df_sub <- calc_share_MINT_bl(df)

  df_sub <- df_sub[,colnames(df)]

  df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT (aggregiert)"

  df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (aggregiert)"

  df_sub <-  calc_share_male_bl(df_sub)

  df_sub <-  df_sub %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  df_sub <- df_sub %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
    dplyr::mutate(wert_sum = sum(props))

  df <-  calc_share_male_bl(df)

  # calculate the new "Gesamt"
  df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  df <- df %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
    dplyr::mutate(wert_sum = sum(props))

  df <- rbind(df, df_sub)

  df <- df %>% dplyr::filter(fachbereich == subjects)

  df <- df %>% dplyr::filter(indikator == indikator_choice)

  # calculate proportions
  df <- df %>% dplyr::group_by(region, anzeige_geschlecht) %>%
    dplyr::summarize(proportion = wert/wert_sum)

  df$proportion <- df$proportion * 100

  highcharter::hw_grid(
    # plot
    highcharter::hcmap(
      "countries/de/de-all",
      data = df[df$anzeige_geschlecht == "Frauen",],
      value = "proportion",
      joinBy = c("name", "region"),
      borderColor = "#FAFAFA",
      name = "Anteil",
      borderWidth = 0.1,
      nullColor = "#A9A9A9",
      tooltip = list(
        valueDecimals = 0,
        valueSuffix = "%"
      )
      #,
      #download_map_data = FALSE
    ) %>%
      highcharter::hc_colorAxis(min=0,labels = list(format = "{text}%")) %>%
      highcharter::hc_title(
        text = paste0("Weibliche ", indikator_choice, ": Anteil an Belegungen <br> in ", subjects),
        margin = 10,
        align = "center",
        style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
      ) %>%
       highcharter::hc_caption(
         text = "...",  style = list(color= "white", fontSize = "12px")
       ) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular")
      ) %>% highcharter::hc_size(600, 550) %>%
      highcharter::hc_credits(enabled = FALSE) %>%
      highcharter::hc_legend(layout = "horizontal", floating = FALSE,
                             verticalAlign = "bottom"),

    highcharter::hcmap(
      "countries/de/de-all",
      data = df[df$anzeige_geschlecht == "Männer",],
      value = "proportion",
      joinBy = c("name", "region"),
      borderColor = "#FAFAFA",
      name = "Anteil",
      borderWidth = 0.1,
      nullColor = "#A9A9A9",
      tooltip = list(
        valueDecimals = 0,
        valueSuffix = "%"
      )
      #,
      #download_map_data = FALSE
    ) %>%
      highcharter::hc_colorAxis(min=0, labels = list(format = "{text}%")) %>%
      highcharter::hc_title(
        text = paste0("Männliche ", indikator_choice, ": Anteil an Belegungen <br> in ", subjects),
        margin = 10,
        align = "center",
        style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
      ) %>%
       highcharter::hc_caption(
         text = "Ausgegraut: Daten stehen nicht zur Verfügung",  style = list(fontSize = "12px")
       ) %>%
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
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

studierende_verlauf_multiple_bl_gender <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_studium_studienzahl_bl_gender_verlauf

  subjects_select <- r$subject_studium_studienzahl_bl_gender_verlauf

  lehramt <- r$nurLehramt_studium_studienzahl_bl_gender_verlauf

  hochschulform_select_1 <- r$hochschulform_studium_studienzahl_bl_gender_verlauf1

  hochschulform_select_2 <- r$hochschulform_studium_studienzahl_bl_gender_verlauf2

  studium_level <- r$level_studium_studienzahl_bl_gender_verlauf

  states <- r$states_studium_studienzahl_bl_gender_verlauf

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  # remove
  df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(region != "Baden-Württemberg")

  # include "Osten" und "Westen" in Dataframe
  df <- prep_studierende_east_west(df)

  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

    title_help_sub_sub <- " insgesamt"

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)

    title_help_sub_sub <- " an einer Uni (nur Lehramt)"
  }

  # aggregate to MINT
  df_sub <- calc_share_MINT_bl(df)

  df_sub <- df_sub[,colnames(df)]

  df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT (aggregiert)"

  df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (aggregiert)"

  df_sub <-  calc_share_male_bl(df_sub)

  df_sub <-  df_sub %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  df_sub <- df_sub %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
    dplyr::mutate(wert_sum = sum(props))

  df <-  calc_share_male_bl(df)

  # calculate the new "Gesamt"
  df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  df <- df %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
    dplyr::mutate(wert_sum = sum(props))

  df <- rbind(df, df_sub)

  df <- df %>% dplyr::filter(fachbereich == subjects_select)

  df <- df %>% dplyr::filter(indikator == studium_level)

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")

  df <- df %>% dplyr::filter(region %in% states)

  # calculate proportions
  df <- df %>% dplyr::group_by(region, fachbereich, jahr, anzeige_geschlecht) %>%
    dplyr::summarize(proportion = wert/wert_sum) %>% dplyr::ungroup()

  df$proportion <- df$proportion * 100

  if(studium_level == "Studierende") {

    title_help <- "Weibliche Studierende:"

  }else {

    title_help <- "Studienanfängerinnen:"

  }

  # order years for plot
  df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion,1), group = region)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil {point.region} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0(title_help, " Anteil an Belegungen in ", subjects_select),
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
#' @description A function to compare a subject for different Bundesländer
#'
#' @return The return value is a dumbbell plot
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

bundeslaender_ranking <- function(df,r, type) {

  # load UI inputs from reactive value
  timerange <- r$date_studium_studienzahl_bl_gender_vergleich

  lehramt <- r$nurLehramt_studium_studienzahl_bl_gender_vergleich

  hochschulform_select_1 <- r$hochschulform_studium_studienzahl_bl_gender_vergleich1

  hochschulform_select_2 <- r$hochschulform_studium_studienzahl_bl_gender_vergleich2

  subject <- r$subject_studium_studienzahl_bl_gender_vergleich

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region != "Deutschland")

  #df <- df %>% dplyr::filter(region != "Bayern")

 # df <- df %>% dplyr::filter(region != "Baden-Württemberg")

  df <- df %>% dplyr::mutate(indikator = replace(indikator,
                                                 indikator == "Studienanfänger:innen",
                                                 "Studienanfänger:inneninnen"))

  if(lehramt == FALSE){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)

  }

  # aggregate to MINT
  df_sub <- calc_share_MINT_bl(df)

  df_sub <- df_sub[,colnames(df)]

  df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT (aggregiert)"

  df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (aggregiert)"

  df_sub <-  calc_share_male_bl(df_sub)

  df_sub <-  df_sub %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  df_sub <- df_sub %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
    dplyr::mutate(wert_sum = sum(props))

  df <-  calc_share_male_bl(df)

  # calculate the new "Gesamt"
  df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  df <- df %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
    dplyr::mutate(wert_sum = sum(props))

  df <- rbind(df, df_sub)

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")

  df <- df %>% dplyr::filter(fachbereich == subject)

  df <- df %>% dplyr::group_by(region, indikator) %>%
    dplyr::summarize(proportion = (wert/wert_sum)*100)

  # spread column
  df <- tidyr::spread(df, indikator, proportion)

  df <- df %>% tidyr::drop_na()

  df2 <- tidyr::gather(df, group, value, -region) %>%
    dplyr::filter(group != "fachbereich") %>%
    dplyr::mutate(value = as.numeric(value))

  df$region <- reorder(df$region, df$Studierende)

  df2$region <- factor(df2$region, levels = levels(df$region))

  ggplot2::ggplot(df,
                  ggplot2::aes(y = region)) +
    ggplot2::geom_point(data = df2, ggplot2::aes(x = value, color = group), size = 5) +
    ggalt::geom_dumbbell(
      ggplot2::aes(x = Studienanfänger:inneninnen, xend = Studierende),
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
                                 "Frauen: Studienfachwahl von MINT-Fächergruppen in ", states, "<br>", timerange,
                                 "<br><br><br>"),
                  color = "") +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))

}


