#' A function to plot a graph.
#'
#' @description A function to create a pie chart for the first box
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

#
#   df <- df %>% dplyr::filter(region == "Deutschland")%>%
#     tidyr::pivot_wider(names_from=anzeige_geschlecht, values_from=wert)%>%
#     dplyr::mutate(Gesamt=Männer+Frauen)%>%
#     tidyr::pivot_longer(c("Gesamt", "Frauen", "Männer"), names_to = "anzeige_geschlecht", values_to = "wert")
#
#
#   df <- df %>%
#     tidyr::pivot_wider(values_from = wert, names_from = fachbereich)%>%
#     dplyr::mutate(MINT=Mathematik+Informatik+Physik+Biologie+Chemie,
#                   "andere Fächer" =`Alle Fächer`- MINT)%>%
#     tidyr::pivot_longer(c(6:19), values_to = "wert", names_to= "fachbereich")
#
#
#   df <- df %>%
#     tidyr::pivot_wider(values_from = wert, names_from = anzeige_geschlecht)%>%
#     tidyr::pivot_longer(c("Männer","Frauen"),names_to = "anzeige_geschlecht", values_to= "wert")%>%
#     dplyr::rename(wert_new = Gesamt)%>%
#     dplyr::filter(fachbereich=="MINT" | fachbereich == "andere Fächer")



  # aggregate to MINT
  df <- share_mint_kurse(df)

  # calcualte the new "Gesamt"
  # df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
  #   dplyr::group_by(region, fachbereich, indikator, jahr) %>%
  #   dplyr::mutate(wert_new = wert[anzeige_geschlecht == "Frauen"] +
  #                   wert[anzeige_geschlecht == "Männer"])

  df <- df %>% dplyr::group_by(region, fachbereich, indikator, jahr) %>%
    dplyr::mutate(wert_new = wert[anzeige_geschlecht == "Gesamt"] )%>%
    dplyr::filter(anzeige_geschlecht == "Gesamt")

  # df_test <<- df1 %>% tidyr:: pivot_wider(names_from = anzeige_geschlecht, values_from = wert)%>%
  #   dplyr::mutate(Gesamt= Frauen + Männer)%>%
  #   tidyr::pivot_longer(c(6:8), values_to = "wert", names_to = "anzeige_geschlecht")
  #
  #
  # df_test1 <<- share_mint_kurse(df_test)%>%
  #   tidyr::pivot_wider(names_from = anzeige_geschlecht, values_from = wert)%>%
  #   dplyr::rename(wert_new=Gesamt)%>%
  #   tidyr::pivot_longer(c(6,8), names_to = "anzeige_geschlecht", values_to = "wert")


  df = df[!duplicated(df$wert_new),]

  df$anzeige_geschlecht <- NULL


  df <- df %>%
    dplyr::group_by(jahr, indikator) %>%
    dplyr::mutate(sum_wert = sum(wert_new))


  # calculate proportions
  # df <- df %>% dplyr::group_by(jahr, indikator, fachbereich) %>%
  #   dplyr::summarize(proportion = wert_new/sum_wert) %>%
  #   dplyr::mutate(proportion = round(proportion, 2)*100)

  df<-  df %>% dplyr::group_by(jahr, indikator, fachbereich) %>%
    dplyr::summarize(proportion = wert_new/sum_wert)

  df$proportion <- df$proportion * 100

  df <- df %>% dplyr::group_by(jahr, indikator, fachbereich) %>%
    dplyr::mutate(proportion = round(proportion,0))


  df_gk <- df %>% dplyr::filter(indikator == "Grundkurse")



  df_lk <- df %>% dplyr::filter(indikator == "Leistungskurse")

  plot_gk <- highcharter::hchart(df_gk, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
    highcharter::hc_tooltip(
      pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
    highcharter::hc_title(text = paste0("Fächerwahl (Grundkurse)",br(), timerange),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
    highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                           dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE))

  plot_lk <- highcharter::hchart(df_lk, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
    highcharter::hc_tooltip(
      pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
    highcharter::hc_title(text = paste0("Fächerwahl (Leistungskurse)", br(), timerange),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
    highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                           dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE))


  plot_gk <- plot_gk %>% highcharter::hc_colors(c("#efe8e6","#b16fab"))

  plot_lk <- plot_lk %>% highcharter::hc_colors(c("#efe8e6","#b16fab"))


  # place plots inside grid
  highcharter::hw_grid(

    plot_gk,

    plot_lk,

    ncol = 2,
    browsable = TRUE
  )


}

#' A function to plot a graph.
#'
#' @description A function to create a pie chart for the first box
#' inside the tab "Schule".
#'
#' @return The return value is a plot
#' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

kurse_einstieg_pie_gender <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_kurse_pie_gender

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)


  df <- df %>% dplyr::filter(region == "Deutschland")


  # calcualte the new value for "Gesamt"
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

  # aggregate the share of the MINT subjects vs Rest
  df <- share_mint_kurse(df)

 # df_gk <- df %>% dplyr::filter(indikator == "Grundkurse")

# df_gk <- df_gk %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

  # calculate proportions
 # df_gk <- share_pie(df_gk)

#  df_lk <- df %>% dplyr::filter(indikator == "Leistungskurse")

 # df_lk <- df_lk %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

  # # calculate proportions
  # df_lk <- share_pie(df_lk)

  #Grundkurs
  df_gk <- df %>% dplyr::filter(indikator == "Grundkurse")

  # calculate proprotion female
  df_gk[df_gk$anzeige_geschlecht == "Frauen", "wert"] <-  df_gk %>% dplyr::group_by(fachbereich) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"]/
                       wert[anzeige_geschlecht == "Gesamt"]) %>%
    dplyr::arrange(-dplyr::row_number()) %>% dplyr::pull(wert)


  # calculate proprotion male
  df_gk[df_gk$anzeige_geschlecht == "Männer", "wert"] <- df_gk %>% dplyr::group_by(fachbereich) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Männer"]/
                       wert[anzeige_geschlecht == "Gesamt"]) %>%
    dplyr::arrange(-dplyr::row_number()) %>% dplyr::pull(wert)

  df_gk <- df_gk %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

  df_gk$wert <- df_gk$wert * 100

  #Leistungskurs
  df_lk <- df %>% dplyr::filter(indikator == "Leistungskurse")

  # calculate proprotion female
  df_lk[df_lk$anzeige_geschlecht == "Frauen", "wert"] <-  df_lk %>% dplyr::group_by(fachbereich) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"]/
                       wert[anzeige_geschlecht == "Gesamt"]) %>%
    dplyr::arrange(-dplyr::row_number()) %>% dplyr::pull(wert)


  # calculate proprotion male
  df_lk[df_lk$anzeige_geschlecht == "Männer", "wert"] <- df_lk %>% dplyr::group_by(fachbereich) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Männer"]/
                       wert[anzeige_geschlecht == "Gesamt"]) %>%
    dplyr::arrange(-dplyr::row_number()) %>% dplyr::pull(wert)

  df_lk <- df_lk %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

  df_lk$wert <- df_lk$wert * 100


  df_gk_mint <- df_gk %>% dplyr::filter(fachbereich == "MINT")

  df_lk_mint <- df_lk %>% dplyr::filter(fachbereich == "MINT")

  df_gk_rest <- df_gk %>% dplyr::filter(fachbereich == "andere Fächer")

  df_lk_rest <- df_lk %>% dplyr::filter(fachbereich == "andere Fächer")

  df_gk_mint$anzeige_geschlecht[df_gk_mint$anzeige_geschlecht == "Frauen"] <- "Mädchen"
  df_gk_mint$anzeige_geschlecht[df_gk_mint$anzeige_geschlecht == "Männer"] <- "Jungen"
  df_lk_mint$anzeige_geschlecht[df_lk_mint$anzeige_geschlecht == "Frauen"] <- "Mädchen"
  df_lk_mint$anzeige_geschlecht[df_lk_mint$anzeige_geschlecht == "Männer"] <- "Jungen"
  df_gk_rest$anzeige_geschlecht[df_gk_rest$anzeige_geschlecht == "Frauen"] <- "Mädchen"
  df_gk_rest$anzeige_geschlecht[df_gk_rest$anzeige_geschlecht == "Männer"] <- "Jungen"
  df_lk_rest$anzeige_geschlecht[df_lk_rest$anzeige_geschlecht == "Frauen"] <- "Mädchen"
  df_lk_rest$anzeige_geschlecht[df_lk_rest$anzeige_geschlecht == "Männer"] <- "Jungen"

  plot_gk_mint <- highcharter::hchart(df_gk_mint, size = 280, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
    highcharter::hc_tooltip(
      pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
    highcharter::hc_title(text = paste0("MINT-Fächer (Grundkurse)", br(), timerange),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px"), marginTop = 20, marginBottom = 7) %>%
    highcharter::hc_legend(enabled = TRUE) %>%
    highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                           dataLabels = list(enabled = TRUE,  format='{point.percentage:.0f}%'), showInLegend = TRUE)) %>%
    highcharter::hc_colors(c("#154194","#efe8e6"))


  plot_gk_rest <- highcharter::hchart(df_gk_rest, size = 150, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
    highcharter::hc_tooltip(
      pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%

    highcharter::hc_title(text = paste0(br(), "Zum Vergleich: Anteil von Schülerinnen an Nicht-MINT-Fächern in Grundkursen in ", timerange),

   # highcharter::hc_title(text = paste0("Vergleich: Andere Fächer (Grundkurse)", br(), timerange),

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


  plot_lk_mint <- highcharter::hchart(df_lk_mint, size = 280, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
    highcharter::hc_tooltip(
      pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
    highcharter::hc_title(text = paste0("MINT-Fächer (Leistungskurse) ", br(), timerange),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px"), marginTop = 20, marginBottom = 7) %>%
    highcharter::hc_legend(enabled = TRUE) %>%
    highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                           dataLabels = list(enabled = TRUE, format='{point.percentage:.0f}%'), showInLegend = TRUE)) %>%
    highcharter::hc_colors(c("#154194","#efe8e6"))

  plot_lk_rest <- highcharter::hchart(df_lk_rest, size = 150, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
    highcharter::hc_tooltip(
      pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%

    highcharter::hc_title(text = paste0(br(), "Zum Vergleich: Anteil von Schülerinnen an Nicht-MINT-Fächern in Leistungskursen in ", timerange),

   # highcharter::hc_title(text = paste0("Vergleich: Andere Fächer (Leistungskurse) ", br(), timerange),

                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
    highcharter::hc_legend(enabled = TRUE, y = -180) %>%
    highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                           dataLabels = list(enabled = TRUE, format='{point.percentage:.0f}%'), showInLegend = TRUE,
                                           opacity = 0.7)) %>%
    highcharter::hc_colors(c("#154194","#efe8e6"))


  # place plots inside grid
  highcharter::hw_grid(

    plot_gk_mint,

    plot_lk_mint,

    plot_gk_rest,

    plot_lk_rest,

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



  df <- df %>% dplyr::filter(region == "Deutschland")

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



  df <- share_mint_kurse(df)


  colnames(df) <- c("Jahr", "Region" ,"Indikator", "Geschlecht", "Bereich", "Wert", "Fachbereich")

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

kurse_waffle_mint <- function(df,r) {



  timerange <- r$date_kurse_mint

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")


  # calculate new "Gesamt"
  df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Leistungskurse"), "wert"] <-  df %>%
    dplyr::filter(indikator == "Leistungskurse") %>%
    dplyr::group_by(indikator, jahr) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Gesamt"]
                    # + wert[anzeige_geschlecht == "Frauen"]
                       ) %>% dplyr::pull(wert)


  df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Grundkurse"), "wert"] <-  df %>%
    dplyr::filter(indikator == "Grundkurse") %>%
    dplyr::group_by(indikator, jahr) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Gesamt"]
                    # + wert[anzeige_geschlecht == "Frauen"]
                     ) %>% dplyr::pull(wert)



  # combine subjects to get numbers on share of MINT
  # make a function out of it
  subjects_not <- c("Mathematik", "Informatik",  "Biologie", "Chemie",
                "Physik", "andere naturwiss.-technische Fächer")

  values_natwi <- df %>%
    dplyr::filter(fachbereich %in% c("Biologie", "Chemie", "Physik", "andere naturwiss.-technische Fächer" )) %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht, bereich) %>%
    dplyr::summarise(wert = sum(wert)) %>%
    dplyr::mutate(fachbereich = "Naturwissenschaften") %>%
    dplyr::ungroup()

  # aggregate all other values to "Rest" --> hier fehlen dann wieder die "sonstigen Fächer" die nur in "alle Fächer" drin sind
  # values_andere <- df %>%
  #   dplyr::filter(fachbereich %!in% subjects_not, fachbereich != "Alle Fächer") %>%
  #   dplyr::group_by(jahr, region, indikator, anzeige_geschlecht, bereich) %>%
  #   dplyr::summarise(wert = sum(wert)) %>%
  #   dplyr::mutate(fachbereich = "andere Fächer") %>%
  #   dplyr::ungroup()

  #calculating MINT to be able to calculate nicht MINT
  #calculating MINT
  values_Mint <- df %>%
    dplyr::filter(fachbereich %in% subjects_not)%>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht, bereich) %>%
    dplyr::summarise(wert = sum(wert))

  values_Mint$fachbereich <- "MINT"

  #calculating nicht MINT
  values_andere <- df %>% dplyr::filter(fachbereich == "Alle Fächer")

  #sorting for subtraction
  values_Mint <- values_Mint[with(values_Mint, order(jahr, region, anzeige_geschlecht, indikator, bereich, decreasing = FALSE)), ]
  values_andere <- values_andere[with(values_andere, order(jahr, region, anzeige_geschlecht, indikator, bereich, decreasing = FALSE)), ]

  values_andere$wert <- values_andere$wert - values_Mint$wert

  values_andere$fachbereich <- "andere Fächer"


  df <- rbind(df, values_andere, values_natwi)

  df <- df %>% dplyr::filter(fachbereich %in% c("Mathematik", "Informatik",
                                                "Naturwissenschaften", "andere Fächer"))

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")



  # extract new "Gesamt"
  df <- df %>% dplyr::group_by(indikator) %>%
    dplyr::mutate(props = sum(wert))


  # calculate proportion
  df <-  df %>% dplyr::group_by(fachbereich, indikator) %>%
    dplyr::summarize(proportion = wert/props)

  # set order
  x_gk <- df %>% dplyr::filter(indikator == "Grundkurse")

  x_gk$proportion <- x_gk$proportion * 100



  # ensure that proportions sum to 1
  x_gk <- setNames(round(as.numeric(x_gk$proportion),0),
                   x_gk$fachbereich)

  x_gk <- x_gk[order(factor(names(x_gk), levels = c('Informatik', 'Mathematik',
                                                    'Naturwissenschaften',
                                                    'andere Fächer')))]



  x_lk <- df %>% dplyr::filter(indikator == "Leistungskurse")

  x_lk$proportion <- x_lk$proportion * 100

  # ensure that proportions sum to 1
  x_lk <- setNames(round_preserve_sum(as.numeric(x_lk$proportion),0),
                   x_lk$fachbereich)

  x_lk <- x_lk[order(factor(names(x_lk), levels = c('Informatik', 'Mathematik',
                                                    'Naturwissenschaften',
                                                    'andere Fächer')))]  # Bis hier sind andere natTech fächer drin. werden in grafik nicht berücksichtigt


  # create plot objects for waffle charts
  waffle_gk <- waffle::waffle(x_gk, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", "Fächerwahl (Grundkurse) </span> <br>", timerange, "<br>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "bottom") +
    ggplot2::scale_fill_manual(
      values =  c("#ee7775",
                  "#fcc433",
                  "#00a87a",
                  '#b1b5c3'),
      limits = c('Informatik', 'Mathematik',
                 'Naturwissenschaften',
                 'andere Fächer'),
      na.value="#b1b5c3",
      guide = ggplot2::guide_legend(reverse = TRUE),
      labels = c(
        paste0("Informatik",", ",x_gk[1], "%"),
        paste0("Mathematik",", ",x_gk[2], "%"),
        paste0("Naturwissenschaften",", ",x_gk[3], "%"),
        paste0("andere Fächer",", ",x_gk[4], "%"))) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))



  waffle_lk <- waffle::waffle(x_lk, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", "Fächerwahl (Leistungskurse) </span> <br>", timerange, "<br>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "bottom")

    # account for the possability that female has 0% share of Informatik
    if (x_lk[[1]] == 0) {

      waffle_lk <-  waffle_lk +
        ggplot2::scale_fill_manual(
          values =  c(#"#ee7775",
            "#fcc433",
            "#00a87a",
            '#b1b5c3'),
          limits = c('Mathematik',
                     'Naturwissenschaften',
                     'andere Fächer'),
          guide = ggplot2::guide_legend(reverse = TRUE),
          na.value="#b1b5c3",
          labels = c(
            #paste0("Frauen (Informatik)",", ",x_gk[1], "%"),
            paste0("Mathematik",", ",x_lk[2], "%"),
            paste0("Naturwissenschaften",", ",x_lk[3], "%"),
            paste0("andere Fächer",", ",x_lk[4], "%"))) +
        ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))



    } else{

      waffle_lk <- waffle_lk +
        ggplot2::scale_fill_manual(
          values =  c("#ee7775",
                      "#fcc433",
                      "#00a87a",
                      '#b1b5c3'),
          limits = c("Informatik", "Mathematik",
                     "Naturwissenschaften", "andere Fächer"),
          na.value="#b1b5c3",
          guide = ggplot2::guide_legend(reverse = TRUE),
          labels = c(
            paste0("Informatik",", ",x_lk[1], "%"),
            paste0("Mathematik",", ",x_lk[2], "%"),
            paste0("Naturwissenschaften",", ",x_lk[3], "%"),
            paste0("andere Fächer",", ",x_lk[4], "%"))) +
        ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))



    }








  ggpubr::ggarrange(waffle_gk, NULL ,waffle_lk, widths = c(1, 0.1, 1), nrow=1,
                    labels="")

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

kurse_mint_comparison <- function(df,r) {

  timerange <- r$date_comparison_subject

  state <- r$state_comparison_subject

  indikator_comparison <- r$indikator_comparison_subject

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  # calculate new "Gesamt"
  # df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Leistungskurse"), "wert"] <-  df %>%
  #   dplyr::filter(indikator == "Leistungskurse") %>%
  #   dplyr::group_by(indikator, jahr) %>%
  #   dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"] +
  #                      wert[anzeige_geschlecht == "Männer"]) %>% dplyr::pull(wert)
  #
  # df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Grundkurse"), "wert"] <-  df %>%
  #   dplyr::filter(indikator == "Grundkurse") %>%
  #   dplyr::group_by(indikator, jahr) %>%
  #   dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"] +
  #                      wert[anzeige_geschlecht == "Männer"]) %>% dplyr::pull(wert)

  df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Leistungskurse"), "wert"] <-  df %>%
    dplyr::filter(indikator == "Leistungskurse") %>%
    dplyr::group_by(indikator, jahr) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Gesamt"]) %>% dplyr::pull(wert)

  df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Grundkurse"), "wert"] <-  df %>%
    dplyr::filter(indikator == "Grundkurse") %>%
    dplyr::group_by(indikator, jahr) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Gesamt"]) %>% dplyr::pull(wert)

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

  df <- prep_kurse_east_west(df)




  df_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt",
                                    fachbereich == "Alle Fächer") %>%
    dplyr::rename(wert_gesamt = "wert")

  # aggregate to MINT
  df_sub <- share_mint_kurse(df)

  df_sub <- df_sub[,colnames(df)]

  df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT-Fächer (gesamt)"

  df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (aggregiert)"

  #df_sub <- df_sub[df_sub$fachbereich!="andere Fächer",] # neu, da keine 100 % rauskommen, kab -->kann wieder rein weil jetzt share_mint angepasst

  df <- rbind(df, df_sub)

  # calculate proportion
  df <- df %>% dplyr::left_join(df_gesamt, by = c("jahr", "region", "indikator",
                                                  "bereich")) %>%
    dplyr::rename(anzeige_geschlecht = "anzeige_geschlecht.x",
                  fachbereich = "fachbereich.x") %>%
    dplyr::select(-c("anzeige_geschlecht.y", "fachbereich.y")) %>%
    dplyr::filter(fachbereich != "Alle Fächer") %>%
    dplyr::mutate(proportion = (wert/wert_gesamt)*100)

  df <- df %>% dplyr::filter(indikator == indikator_comparison)

  x <- c("MINT-Fächer (gesamt)", "Mathematik", "Informatik", "Physik", "Chemie",
         "Biologie", "andere naturwiss.-technische Fächer",
         "andere Fächer (aggregiert)", "Deutsch", "Fremdsprachen", "Gesellschaftswissenschaften",
         "Musik/Kunst", "Religion/Ethik", "Sport")

  df <- df %>%
    dplyr::mutate(fachbereich =  factor(fachbereich, levels = x)) %>%
    dplyr::arrange(fachbereich)

  df <- na.omit(df)

  df <- df %>%
    dplyr::ungroup()%>%
    dplyr::mutate(region= dplyr::case_when(
      region == "Westen" ~ "Westdeutschland (o. Berlin)",
      region == "Osten" ~ "Ostdeutschland (inkl. Berlin)",
      T ~ .$region
    ))

  df <- df %>% dplyr::filter(region == state)

  #Trennpunkte für lange Zahlen ergänzen
  df$wert <- prettyNum(df$wert, big.mark = ".")

  # plot
  highcharter::hchart(df, 'bar', highcharter::hcaes(y = round(proportion), x = fachbereich)) %>%
    highcharter::hc_tooltip(pointFormat = "{point.region} <br> Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    #highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
    #highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
    #highcharter::hc_colors("#b16fab") %>%
    highcharter::hc_plotOptions(bar = list(
      colorByPoint = TRUE,
      colors = ifelse(df$fachbereich %in% c("MINT-Fächer (gesamt)", "andere Fächer (aggregiert)"), "#b16fab", "#d0a9cd")
    )) %>%
    highcharter::hc_title(text = paste0( "Anteil einzelner Fächer in ",state, " (", indikator_comparison, ")",
                                                                        br(), timerange,
                                                                          "<br><br><br>"),
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

  # a <- ifelse(df$fachbereich == "MINT-Fächer (gesamt)"
  #
  #             | df$fachbereich == "andere Fächer (aggregiert)" # raus, kab --> rein vlg. oben
  #             , "#b16fab", "grey30")
  #
  # ggplot2::ggplot(df, ggplot2::aes(y=fachbereich, x=proportion)) +
  #   ggplot2::geom_bar(stat="identity", fill = "#154194") +
  #   ggplot2::geom_text(ggplot2::aes(label=paste(round(proportion),"%")), hjust = -0.3,
  #                      fontface = "bold") +
  #   ggplot2::theme_minimal() +
  #   ggplot2::theme(
  #     axis.text.y = ggplot2::element_text(colour = a),
  #     text = ggplot2::element_text(size = 14),
  #     plot.title = ggtext::element_markdown(hjust = 0.5)) +
  #   ggplot2::ylab("") + ggplot2::xlab("") +
  #   ggplot2::labs(title = paste0( "<span style='font-size:20.5pt; color:black'>",
  #                                 "Anteil einzelner Fächer (", indikator_comparison, ")",
  #                                br(), timerange,
  #                                  "<br><br><br>"),
  #                 fill = "") +
  #   ggplot2::scale_y_discrete(expand = c(0,0)) +
  #   ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))

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

kurse_mint_comparison_bl <- function(df,r) {


  timerange <- r$date_comparison_bl

  subject <- r$subject_comparison_bl

  indikator_comparison <- r$indikator_comparison_bl


  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region != "Deutschland")

  #braucht man aktuell für share_mint_kurse, da sonst "Sonstige Fächer" im Gesamt fehlen
  #df <- df %>% dplyr::filter(fachbereich != "Alle Fächer")

  #alles in gleiche Struktur bringen ohne Frauen/Männer da hier nicht relevant
  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

  df <- df %>% dplyr::filter(indikator == indikator_comparison)

  # calculate new "Gesamt"
  # df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
  #   dplyr::group_by(region, fachbereich, indikator, jahr) %>%
  #   dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
  #                   wert[anzeige_geschlecht == "Männer"])

  # df <- df %>% dplyr::group_by(region, fachbereich, indikator, jahr) %>%
  #   dplyr::mutate(props = wert[anzeige_geschlecht == "Gesamt"] ) %>%
  #   dplyr::filter(anzeige_geschlecht != "Gesamt")


#  df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")


 # df$wert <- df$props

 # df$props <- NULL

  # aggregate to MINT
  df_sub <- share_mint_kurse(df)

  df_sub <- df_sub[,colnames(df)]


  # # aggregate all subjects to calculate proportion later
  df_sub <- df_sub %>% dplyr::group_by(region, indikator) %>%
    dplyr::mutate(props = sum(wert))

  df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT-Fächer (gesamt)"

  df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (gesamt)"

  # aggregate all subjects to calculate proportion later
  df <- df %>% dplyr::group_by(region, indikator) %>%
  dplyr::mutate(props = sum(wert))

  df <- rbind(df, df_sub)

  df <- df %>% dplyr::filter(fachbereich == subject)

  # nicht mehr nötig, da nicht stacked
  # #gegenwert Berechnen für jeweilige Auswahl
  # df_n <- df %>% dplyr::group_by(region, indikator) %>%
  #   dplyr::mutate(wert = props - wert)
  # df_n$fachbereich <- "andere Fächer (gesamt)"
  #
  # df <- rbind(df, df_n)

  # calculate proportion
  df <- df %>% dplyr::group_by(region, fachbereich, indikator) %>%
    dplyr::mutate(proportion = wert/props)

  df$proportion <- round(df$proportion * 100)

  df <- subset(df, proportion >= 0.5)

  #Trennpunkte für lange Zahlen ergänzen
  df$wert <- prettyNum(df$wert, big.mark = ".")

  help_title <- ifelse(subject == "MINT-Fächer (gesamt)", "MINT-Fächern (gesamt)", subject)
  help_title <- ifelse(help_title == "andere Fächer (gesamt)", "anderen Fächern (gesamt)", help_title)

  highcharter::hchart(df, 'bar', highcharter::hcaes(y = round(proportion), x = region)) %>%
    highcharter::hc_tooltip(pointFormat = "{point.fachbereich} <br> Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    # highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
    # highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
    highcharter::hc_colors("#b16fab") %>%
    highcharter::hc_title(text = paste0( "Anteil von ", help_title, " nach Bundesländern (",  indikator_comparison, ")",br(), timerange,
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

#
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
#                                 "Anteil ", subject, " nach Bundesländern (",  indikator_comparison, ")",br(), timerange,
#                                  "<br><br><br>"),
#                   fill = "") +
#     ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))

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

  indikator_gender <- r$indikator_kurse_gender

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")


  # combine subjects to get numbers on share of MINT
  # make a function out of it
  subjects_not <- c("Mathematik", "Informatik",  "Biologie", "Chemie",
                    "Physik", "andere naturwiss.-technische Fächer")

  values_natwi <- df %>%
    dplyr::filter(fachbereich %in% c("Biologie", "Chemie", "Physik", "andere naturwiss.-technische Fächer")) %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht, bereich) %>%
    dplyr::summarise(wert = sum(wert)) %>%
    dplyr::mutate(fachbereich = "Naturwissenschaften") %>%
    dplyr::ungroup()

  #muss man anders machen, sonst fehlen "Sonstige Fächer" aus "Alle Fächer"
  # aggregate all other values to "Rest"
  # values_andere <- df %>%
  #   dplyr::filter(fachbereich %!in% subjects_not, fachbereich != "Alle Fächer") %>%
  #   dplyr::group_by(jahr, region, indikator, anzeige_geschlecht, bereich) %>%
  #   dplyr::summarise(wert = sum(wert)) %>%
  #   dplyr::mutate(fachbereich = "andere Fächer") %>%
  #   dplyr::ungroup()

  #calculating MINT to be able to calculate nicht MINT
  #calculating MINT
  values_Mint <- df %>%
    dplyr::filter(fachbereich %in% subjects_not)%>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht, bereich) %>%
    dplyr::summarise(wert = sum(wert))

  values_Mint$fachbereich <- "MINT"

  #calculating nicht MINT
  values_andere <- df %>% dplyr::filter(fachbereich == "Alle Fächer")

  #sorting for subtraction
  values_Mint <- values_Mint[with(values_Mint, order(jahr, region, anzeige_geschlecht, indikator, bereich, decreasing = FALSE)), ]
  values_andere <- values_andere[with(values_andere, order(jahr, region, anzeige_geschlecht, indikator, bereich, decreasing = FALSE)), ]

  values_andere$wert <- values_andere$wert - values_Mint$wert

  values_andere$fachbereich <- "andere Fächer"

  df <- rbind(df, values_andere, values_natwi)

  df <- df %>% dplyr::filter(fachbereich %in% c("Mathematik", "Informatik", "Naturwissenschaften", "andere Fächer"))


  df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt")


  # extract new "Gesamt"
  df <- df %>% dplyr::group_by(indikator, anzeige_geschlecht) %>%
    dplyr::mutate(props = sum(wert))


  # calculate proportion
  df <- df %>% dplyr::group_by(fachbereich, indikator, anzeige_geschlecht) %>%
    dplyr::summarize(proportion = wert/props)

  df <- df %>% dplyr::filter(indikator == indikator_gender)


  # set order
  x_male <- df %>% dplyr::filter(anzeige_geschlecht == "Männer")

  x_male$proportion <- x_male$proportion * 100

  # ensure that proportions sum to 1
  x_male <- setNames(round_preserve_sum(as.numeric(x_male$proportion),0),
                     x_male$fachbereich)

  x_male <- x_male[order(factor(names(x_male), levels = c('Informatik', 'Mathematik',
                                                    'Naturwissenschaften',
                                                    'andere Fächer')))]

  x_female <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")

  x_female$proportion <- x_female$proportion * 100

  # ensure that proportions sum to 1
  x_female <- setNames(round_preserve_sum(as.numeric(x_female$proportion),0),
                       x_female$fachbereich)

  x_female <- x_female[order(factor(names(x_female), levels = c('Informatik', 'Mathematik',
                                                    'Naturwissenschaften',
                                                    'andere Fächer')))]


  # create plot objects for waffle charts
  waffle_male <- waffle::waffle(x_male, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", "Fächerwahl von Jungen </span><br>(",indikator_gender, ", ", timerange, ")<br>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "bottom") +
    ggplot2::scale_fill_manual(
      values =  c("#ee7775",
                  "#fcc433",
                  "#00a87a",
                  '#b1b5c3'),
      limits = c('Informatik', 'Mathematik',
                 'Naturwissenschaften',
                 'andere Fächer'),
      na.value="#b1b5c3",
      guide = ggplot2::guide_legend(reverse = TRUE),
      labels = c(
        paste0("Informatik",", ",x_male[1], "%"),
        paste0("Mathematik",", ",x_male[2], "%"),
        paste0("Naturwissenschaften",", ",x_male[3], "%"),
        paste0("andere Fächer",", ",x_male[4], "%"))) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))



  waffle_female <- waffle::waffle(x_female, keep = FALSE) +
    ggplot2::labs(
      fill = "",
      title = paste0("<span style='color:black;'>", "Fächerwahl von Mädchen </span><br>(",indikator_gender, ", ", timerange, ")<br>")) +
    ggplot2::theme(plot.title = ggtext::element_markdown(),
                   plot.subtitle = ggtext::element_markdown(),
                   text = ggplot2::element_text(size = 14),
                   plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
                   legend.position = "bottom")

  # account for the possability that female has 0% share of Informatik
  if (x_female[[1]] == 0) {

    waffle_female <-  waffle_female +
      ggplot2::scale_fill_manual(
        values =  c(#"#ee7775",
          "#fcc433",
          "#00a87a",
          '#b1b5c3'),
        limits = c('Mathematik',
                   'Naturwissenschaften',
                   'andere Fächer'),
        guide = ggplot2::guide_legend(reverse = TRUE),
        na.value="#b1b5c3",
        labels = c(
          #paste0("Frauen (Informatik)",", ",x_gk[1], "%"),
          paste0("Mathematik",", ",x_female[2], "%"),
          paste0("Naturwissenschaften",", ",x_female[3], "%"),
          paste0("andere Fächer",", ",x_female[4], "%"))) +
      ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))



  } else{

    waffle_female <- waffle_female +
      ggplot2::scale_fill_manual(
        values =  c("#ee7775",
                    "#fcc433",
                    "#00a87a",
                    '#b1b5c3'),
        limits = c("Informatik", "Mathematik",
                   "Naturwissenschaften", "andere Fächer"),
        na.value="#b1b5c3",
        guide = ggplot2::guide_legend(reverse = TRUE),
        labels = c(
          paste0("Informatik",", ",x_female[1], "%"),
          paste0("Mathematik",", ",x_female[2], "%"),
          paste0("Naturwissenschaften",", ",x_female[3], "%"),
          paste0("andere Fächer",", ",x_female[4], "%"))) +
      ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))



  }


  ggpubr::ggarrange(waffle_female, NULL ,waffle_male, widths = c(1, 0.1, 1), nrow=1)

  # text <- c(
  #   paste0("<span style='font-size:20.5pt; color:black'> Fächerbelegung im Vergleich"))
  # ggpubr::annotate_figure(plot, gridtext::richtext_grob(text = text))

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
  subjects <- c("Mathematik", "Informatik", "Naturwissenschaften")

  df <- df %>% dplyr::filter(fachbereich %in% subjects)


  df <- df[with(df, order(indikator, decreasing = TRUE)), ]


  df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt")


  options(scipen=999)

  # plot
  ggplot2::ggplot(df, ggplot2::aes(x=indikator, y=wert, fill = fachbereich)) +
    ggplot2::geom_bar(stat="identity", position = "dodge") +
    ggplot2::geom_text(ggplot2::aes(label=wert, vjust = - 0.25),
                       position=ggplot2::position_dodge(width=0.9),
                       fontface = "bold") +
    ggplot2::theme_minimal() +
    ggplot2::facet_grid(~ anzeige_geschlecht) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      text = ggplot2::element_text(size = 14),
      plot.title = ggtext::element_markdown(hjust = 0.5)) +
    ggplot2::xlab("") + ggplot2::ylab("Anzahl") +
    ggplot2::scale_fill_manual(values = c("#ee7775", "#fcc433", "#00a87a")) +
    ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
                                 "Schüler und Schülerinnen in MINT" ," in ", timerange,
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
  timerange <- r$date_kurse_ranking



  states <- r$states_kurse_ranking

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(fachbereich != "Alle Fächer")
  df <- df %>% dplyr::filter(indikator != "Oberstufenbelegungen")

  # include "Osten" und "Westen" in Dataframe
  df <- prep_kurse_east_west(df)

  # calcualte the new "Gesamt"
  df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  df <- df %>%
    dplyr::ungroup()%>%
    dplyr::mutate(region = dplyr::case_when(
      region == "Westen" ~ "Westdeutschland (o. Berlin)",
      region == "Osten" ~ "Ostdeutschland (inkl. Berlin)",
      T ~ .$region
    ))

  df <- df %>% dplyr::filter(region == states)


  df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")

  # calcualte propotion
  df <- df %>% dplyr::group_by(fachbereich, anzeige_geschlecht, indikator) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100

  df$anzeige_geschlecht <- NULL

  # spread column
  df <- tidyr::spread(df, indikator, proportion)

  df <- df %>% tidyr::drop_na()

  df2 <<- tidyr::gather(df, group, value, -fachbereich)

  #df2$fachbereich <- reorder(df2$fachbereich, df2$Leistungskurse)

  df2$fachbereich <- factor(df2$fachbereich, levels = levels(df2$fachbereich))




  ggplot2::ggplot(df,
                  ggplot2::aes(y = fachbereich)) +
    ggplot2::geom_point(data = df2, ggplot2::aes(x = value, color = group), size = 5) +
    ggalt::geom_dumbbell(
      ggplot2::aes(x = Grundkurse, xend = Leistungskurse),
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
                   #legend.text= ggplot2::element_text(family = 'SourceSans3-Regular'),
                   panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
                   plot.title = ggtext::element_markdown(hjust = 0.5),
                   axis.text.y = ggplot2::element_text(size = 11)) +
    ggplot2::ylab("") + ggplot2::xlab("") +
    ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
                                 "Anteil von Mädchen nach Fächern in ", states, " (",timerange,")",
                                 "<br><br>"),
                  color = "") +
    ggplot2::scale_x_continuous(n.breaks = 7, labels = function(x) paste0(x, "%"))

}


#' A function to create a bar plot
#'
#' @description A function to return a ranking of subject for both genders
#'
#' @return The return value is a bar plot
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

kurse_ranking_gender <- function(df,r, type) {

  # load UI inputs from reactive value
  timerange <- r$date_kurse_ranking_gender

  subject <- r$subject_kurse_ranking_gender

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region != "Deutschland")
  df <- df %>% dplyr::filter(region != "Baden-Württemberg")

  df_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen",
                                    fachbereich == "Alle Fächer") %>%
    dplyr::rename(wert_sum = "wert")

  df <- df %>% dplyr::filter(fachbereich != "Alle Fächer")

  # aggregate to MINT
  df_sub <- share_mint_kurse(df)

  df_sub <- df_sub[,colnames(df)]

  df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT-Fächer (gesamt)"

  df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (gesamt)"

  df <- rbind(df, df_sub)

  df <- df %>% dplyr::filter(fachbereich == subject)

  # calcualte propotion
  df <- df %>% dplyr::left_join(df_gesamt, by=c("jahr", "region", "indikator",
                                                "bereich", "anzeige_geschlecht")) %>%
    dplyr::rename(fachbereich = "fachbereich.x") %>%
    dplyr::select(-fachbereich.y) %>%
    dplyr::mutate(proportion = (wert/wert_sum)*100) %>%
    dplyr::select(-c("wert", "wert_sum")) %>%
    dplyr::filter(anzeige_geschlecht=="Frauen")


  # spread column
  df <- tidyr::spread(df, indikator, proportion)

  df2 <- tidyr::gather(df, group, value, -region) %>%
    dplyr::filter(group %in% c("Grundkurse", "Leistungskurse")) %>%
    dplyr::mutate(value = as.numeric(value))

  df$region <- reorder(df$region, df$Leistungskurse)

  df2$region <- factor(df2$region, levels = levels(df$region))

  help_title <- ifelse(subject == "MINT-Fächer (gesamt)", "MINT-Fächern (gesamt)", subject)
  help_title <- ifelse(help_title == "andere Fächer (gesamt)", "anderen Fächern (gesamt)", help_title)

  ggplot2::ggplot(df,
                  ggplot2::aes(y = region)) +
    ggplot2::geom_point(data = df2, ggplot2::aes(x = value, color = group), size = 5) +
    ggalt::geom_dumbbell(
      ggplot2::aes(x = Grundkurse, xend = Leistungskurse),
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
                                 "Mädchen: Wahl von ", subject ,br(),timerange,
                                 "<br><br><br>"),
                  color = "") +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))

}


#' A function to create a bar plot
#'
#' @description A function to return a ranking o
#'
#' @return The return value is a bar plot
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

kurse_ranking_bl <- function(df,r, type) {

  # load UI inputs from reactive value
  timerange <- r$date_kurse_ranking_bl

  subjects <- r$subject_kurse_ranking_bl

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(fachbereich != "Alle Fächer")

  df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")

  # calcualte propotion
  df <- df %>% dplyr::group_by(fachbereich, anzeige_geschlecht, indikator) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100

  df$anzeige_geschlecht <- NULL

  # spread column
  df <- tidyr::spread(df, indikator, proportion)

  df <- df %>% tidyr::drop_na()

  df2 <- tidyr::gather(df, group, value, -fachbereich)

  df$fachbereich <- reorder(df$fachbereich, df$Leistungskurse)

  df2$fachbereich <- factor(df2$fachbereich, levels = levels(df$fachbereich))


  ggplot2::ggplot(df,
                  ggplot2::aes(y = fachbereich)) +
    ggplot2::geom_point(data = df2, ggplot2::aes(x = value, color = group), size = 5) +
    ggalt::geom_dumbbell(
      ggplot2::aes(x = Grundkurse, xend = Leistungskurse),
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
                   #legend.text= ggplot2::element_text(family = 'SourceSans3-Regular'),
                   panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
                   plot.title = ggtext::element_markdown(hjust = 0.5),
                   axis.text.y = ggplot2::element_text(size = 11)) +
    ggplot2::ylab("") + ggplot2::xlab("") +
    ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
                                 "Relativer Anteil von Schülerinnen an Grund- und Leistungskurse in ",timerange,
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
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

kurse_map <- function(df,r) {


  # load UI inputs from reactive value
  timerange <- r$date_map

  subjects <- r$subject_map

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region != "Deutschland")

  #df <- df %>% dplyr::filter(fachbereich != "Alle Fächer")

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

  # aggregate to MINT
  df_sub <- share_mint_kurse(df)

  df_sub <- df_sub[,colnames(df)]

  df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT-Fächer (gesamt)"

  df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (gesamt)"


    # df_sub <-  df_sub %>% dplyr::filter(anzeige_geschlecht != "Gesamt")
    # dplyr::group_by(region, fachbereich, indikator, jahr) %>%
    # dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
    #                 wert[anzeige_geschlecht == "Männer"])

  # df_sub <- df %>% dplyr::group_by(region, fachbereich, indikator, jahr) %>%
  #   dplyr::mutate(wert_new = wert[anzeige_geschlecht == "Gesamt"] ) %>%
  #   dplyr::filter(anzeige_geschlecht != "Gesamt")

  # df_sub <- df_sub %>% dplyr::group_by(region, fachbereich, indikator, jahr) %>%
  #   dplyr::mutate(props = wert[anzeige_geschlecht == "Gesamt"] ) %>%
  #   dplyr::filter(anzeige_geschlecht != "Gesamt")



  #df_sub <- df_sub %>% dplyr::filter(anzeige_geschlecht != "Männer")


  df_sub <- df_sub %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
    dplyr::mutate(wert_sum = sum(wert)) # Hier wird eine neuer Wert berechnet,
                                         # der nicht mit dem alle Fächer Wert der KMK über einstimmt. Wie ist hier zu verfahren
                                          #passt wieder, wenn man jetzt abgeleicht!

  # calculate the new "Gesamt"
  # df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
  #   dplyr::group_by(region, fachbereich, indikator, jahr) %>%
  #   dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
  #                   wert[anzeige_geschlecht == "Männer"])

  # df <- df %>% dplyr::group_by(region, fachbereich, indikator, jahr) %>%
  #   dplyr::mutate(props = wert[anzeige_geschlecht == "Gesamt"] ) %>%
  #   dplyr::filter(anzeige_geschlecht != "Gesamt")
  #
  # df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")

  #Falsch, enthält "Alle Fächer" in jeder Summe
  # df <- df %>%
  #   dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
  #   dplyr::mutate(wert_sum = sum(wert))

  df <- df %>%
    dplyr::filter(fachbereich != "Alle Fächer") %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
    dplyr::mutate(wert_sum = sum(wert))

  df <- rbind(df, df_sub)

  df <- df %>% dplyr::filter(fachbereich == subjects)

  # calculate proportions
  df <- df %>% dplyr::group_by(region, indikator) %>%
    dplyr::mutate(proportion = wert/wert_sum)

  df$proportion <- df$proportion * 100

  help_title <- ifelse(subjects == "MINT-Fächer (gesamt)", "MINT-Fächern (gesamt)", subjects)
  help_title <- ifelse(help_title == "andere Fächer (gesamt)", "anderen Fächern (gesamt)", help_title)

  #Extra gerundeten Proportions-Wert erstellen, für Anzeige in Hover
  df$prop <- df$proportion
  df$prop <- round(df$prop, 0)

  #Trennpunkte für lange Zahlen ergänzen
  df$wert <- prettyNum(df$wert, big.mark = ".")

  highcharter::hw_grid(
  # plot
  highcharter::hcmap(
    "countries/de/de-all",
    data = df[df$indikator == "Grundkurse",],
    value = "proportion",
    joinBy = c("name", "region"),
    borderColor = "#FAFAFA",
    name = paste0("Anteil ", subjects),
    borderWidth = 0.1,
    nullColor = "#A9A9A9",
    tooltip = list(
      valueDecimals = 0,
      valueSuffix = "%"
    )
  ) %>%
    highcharter::hc_tooltip(pointFormat = "{point.region} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}") %>%
    highcharter::hc_colorAxis(min=0,minColor= "#fcfcfd", maxColor="#b16fab", labels = list(format = "{text}%")) %>%
    highcharter::hc_title(
      text = paste0("Grundkurse: Anteil von ", help_title, br(), timerange),
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
    data = df[df$indikator == "Leistungskurse",],
    value = "proportion",
    joinBy = c("name", "region"),
    borderColor = "#FAFAFA",
    name = paste0("Anteil ", subjects),
    borderWidth = 0.1,
    nullColor = "#A9A9A9",
    tooltip = list(
      valueDecimals = 0,
      valueSuffix = "%"
    )
  ) %>%
    highcharter::hc_tooltip(pointFormat = "{point.region} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}") %>%
    highcharter::hc_colorAxis(min=0, minColor= "#fcfcfd", maxColor="#b16fab",labels = list(format = "{text}%")) %>%
    highcharter::hc_title(
      text = paste0("Leistungskurse: Anteil von ", help_title, br(), timerange),
      margin = 10,
      align = "center",
      style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
    ) %>%
    # highcharter::hc_caption(
    #    text = "Ausgegraut: Daten stehen nicht zur Verfügung",  style = list(fontSize = "12px")
    #  ) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular")
    ) %>% highcharter::hc_size(600, 550) %>%
    highcharter::hc_credits(enabled = FALSE) %>%
    highcharter::hc_legend(layout = "horizontal", floating = FALSE, verticalAlign = "bottom"),


    ncol = 2,
    browsable = TRUE
  )


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

kurse_map_gender <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_map_gender

  subjects <- r$subject_map_gender

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  # remove
  df <- df %>% dplyr::filter(region != "Deutschland")

  df_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen",
                                    fachbereich == "Alle Fächer") %>%
    dplyr::rename(wert_sum = "wert")

  #share_mint_kurse ntutz "alle Fächer"
  #df <- df %>% dplyr::filter(fachbereich != "Alle Fächer")

  # aggregate to MINT
  df_sub <- share_mint_kurse(df)

  df_sub <- df_sub[,colnames(df)]

  df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT-Fächer (gesamt)"

  df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (gesamt)"

  df <- rbind(df, df_sub)

  df <- df %>% dplyr::filter(fachbereich == subjects)

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")

  df <- df %>% dplyr::left_join(df_gesamt, by=c("jahr", "region", "indikator",
                                          "bereich", "anzeige_geschlecht")) %>%
    dplyr::rename(fachbereich = "fachbereich.x") %>%
    dplyr::select(-fachbereich.y) %>%
    dplyr::mutate(proportion = (wert/wert_sum)*100)

  help_title <- ifelse(subjects == "MINT-Fächer (gesamt)", "MINT-Fächern (gesamt)", subjects)
  help_title <- ifelse(help_title == "andere Fächer (gesamt)", "anderen Fächern (gesamt)", help_title)

  #Extra gerundeten Proportions-Wert erstellen, für Anzeige in Hover
  df$prop <- df$proportion
  df$prop <- round(df$prop, 0)

  #Trennpunkte für lange Zahlen ergänzen
  df$wert <- prettyNum(df$wert, big.mark = ".")

  highcharter::hw_grid(
    # plot
    highcharter::hcmap(
      "countries/de/de-all",
      data = df[df$indikator == "Grundkurse",],
      value = "proportion",
      joinBy = c("name", "region"),
      borderColor = "#FAFAFA",
      #name = paste0("Anteil ", subjects),
      borderWidth = 0.1,
      nullColor = "#A9A9A9",
      tooltip = list(
        valueDecimals = 0,
        valueSuffix = "%"
      )
    ) %>%
      highcharter::hc_tooltip(pointFormat = "{point.region} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}") %>%
      highcharter::hc_colorAxis(min=0,minColor= "#fcfcfd", maxColor="#154194", labels = list(format = "{text}%")) %>%
      highcharter::hc_title(
        text = paste0("Mädchen: Wahl von ", help_title, " (Grundkurse)", br(), timerange),
        margin = 10,
        align = "center",
        style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
      ) %>%
      highcharter::hc_caption(
        text = "...",  style = list(color="white",fontSize = "12px")
      ) %>%
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
      name = paste0("Anteil ", subjects),
      borderWidth = 0.1,
      nullColor = "#A9A9A9",
      tooltip = list(
        valueDecimals = 0,
        valueSuffix = "%"
      )
    ) %>%
      highcharter::hc_tooltip(pointFormat = "{point.region} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}") %>%
            highcharter::hc_colorAxis(min=0,minColor= "#fcfcfd", maxColor="#154194", labels = list(format = "{text}%")) %>%
      highcharter::hc_title(
        text = paste0("Mädchen: Wahl von ", subjects, " (Leistungskurse)", br(), timerange),
        margin = 10,
        align = "center",
        style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
      ) %>%
      # highcharter::hc_caption(
      #   text = "Ausgegraut: Daten stehen nicht zur Verfügung",  style = list(fontSize = "12px")
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
  subjects <- c("Mathematik", "Informatik", "Biologie", "Chemie",
                "Physik", "andere naturwiss.-technische Fächer")

  df <- df %>% dplyr::filter(fachbereich %in% subjects)


  colnames(df) <- c("Region", "Fachbereich", "Geschlecht", "Jahr", "Indikator", "Bereich", "Wert")

  return(df)

}

#' A function to plot time series
#'
#' @description A function to plot the time series
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

  subjects_select <- r$subject_selected

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(region != "Deutschland")

  df_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen",
                                    fachbereich == "Alle Fächer") %>%
    dplyr::rename(wert_sum = "wert")

  df <- df %>% dplyr::filter(indikator == level_kurs)

  if (level_kurs == "Grundkurse"){

    title_help <- "Grundkurs"

  }else{

    title_help <- "Leistungskurs"

  }

  # calcualte new "Gesamt"
  # df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
  #   dplyr::group_by(region, fachbereich, indikator, jahr) %>%
  #   dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
  #                   wert[anzeige_geschlecht == "Männer"])

  # include "Osten" und "Westen" in Dataframe
  df <- prep_kurse_east_west(df, "subjects")


  # aggregate to MINT
  df_sub <- share_mint_kurse(df)

  # df_sub <-  df_sub %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
  #   dplyr::group_by(region, fachbereich, indikator, jahr) %>%
  #   dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
  #                   wert[anzeige_geschlecht == "Männer"])


  df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT-Fächer (gesamt)"

  df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (gesamt)"

  df_sub <- df_sub[,colnames(df)]

  df <- rbind(df, df_sub)

  # filter states
  df <- df %>% dplyr::filter(region %in% states)

  df <- df %>% dplyr::filter(fachbereich %in% subjects_select)

  # calculate proportions
  df <- df %>% dplyr::left_join(df_gesamt, by=c("jahr", "region", "indikator",
                                                "bereich", "anzeige_geschlecht")) %>%
    dplyr::rename(fachbereich = "fachbereich.x") %>%
    dplyr::select(-fachbereich.y) %>%
    dplyr::mutate(proportion = (wert/wert_sum)*100)


  df <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")

  # order years for plot
  df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = region)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil Schülerinnen <br> Bundesland: {point.region} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_caption(text = "Quelle: KMK 2021, auf Anfrage; eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Schülerinnen: Anteil ", subjects_select, "(", title_help, ")", ),
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
#' @description A function to plot the time series
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

  # calculate new "Gesamt"
  df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  # include "Osten" und "Westen" in Dataframe
  df <- prep_kurse_east_west(df)

  if (subject_aggregated == "aggregiert"){

    # aggregate to MINT
    df <- share_mint_kurse(df)

    # calculate the new "Gesamt"
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

  # calcualte proportions
  df <- df %>% dplyr::group_by(region, fachbereich, anzeige_geschlecht, jahr, indikator) %>%
    dplyr::summarise(proportion = wert/props)

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")

  df$proportion <- df$proportion * 100

  # order
  df <- df[with(df, order(region, fachbereich, jahr, decreasing = TRUE)), ]


  df <- df %>% dplyr::filter(region %in% states)

  # order
  df <- df[with(df, order(region, fachbereich, jahr, decreasing = TRUE)), ]

  colnames(df) <- c("Region", "Fachbereich", "Geschlecht", "Jahr", "Indikator", "Anteil")

  return(df)

}

#' A function to plot time series
#'
#' @description A function to plot the time series
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

kurse_verlauf_single <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_kurse_einstieg_verlauf

  absolut_selector <- r$abs_zahlen_kurse_einstieg_verlauf


  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  # remove
  df <- df %>% dplyr::filter(region == "Deutschland")


   # aggregate to MINT
  df1 <<- share_mint_kurse(df)

   # calcualte the new "Gesamt"
   # df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
   #   dplyr::group_by(region, fachbereich, indikator, jahr) %>%
   #   dplyr::mutate(wert_new = wert[anzeige_geschlecht == "Frauen"] +
   #                   wert[anzeige_geschlecht == "Männer"])

  dfl <<- df1 %>% dplyr::group_by(region, fachbereich, indikator, jahr) %>%
    dplyr::mutate(wert_new = wert[anzeige_geschlecht == "Gesamt"] ) %>%
    dplyr::filter(anzeige_geschlecht != "Gesamt")


   dfl = dfl[!duplicated(dfl$wert_new),]

   dfl$anzeige_geschlecht <- NULL


   dfl <- dfl %>%
     dplyr::group_by(jahr, indikator) %>%
     dplyr::mutate(sum_wert = sum(wert_new))


  # calcualte proportions
  dfl <- dfl %>% dplyr::group_by(jahr, indikator, fachbereich, wert_new) %>%
    dplyr::summarize(wert = wert_new/sum_wert)%>%
    dplyr::rename(Absolut = wert_new, Relativ=wert)%>%
    tidyr::pivot_longer(c(Absolut, Relativ), names_to = "selector", values_to = "wert")%>%
    dplyr::filter(fachbereich == "MINT")%>%
    dplyr::mutate(selector=dplyr::case_when(
      selector == "Relativ" ~ "In Prozent",
      selector == "Absolut" ~ "Anzahl"
    ))




  if(absolut_selector == "In Prozent"){

  df <- dfl %>%
    dplyr::filter(selector == "In Prozent")

  df$wert <- df$wert * 100

  # order years for plot
  df <- df[with(df, order(jahr, decreasing = FALSE)), ]



  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(wert), group = indikator)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil {point.indikator} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil von MINT in der Schule"),
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

  } else if (absolut_selector=="Anzahl") {

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    df <- dfl %>%
      dplyr::filter(selector == "Anzahl")

    # order years for plot
    df <- df[with(df, order(jahr, decreasing = FALSE)), ]

    highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(wert), group = indikator)) %>%
      highcharter::hc_tooltip(pointFormat = "Anzahl: {point.y}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = paste0("Anteil von MINT in der Schule"),
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


}

#' A function to plot time series
#'
#' @description A function to plot a bar chart
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

kurse_einstieg_comparison <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_kurse_einstieg_comparison

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  # remove
  df <- df %>% dplyr::filter(region == "Deutschland")

  # aggregate to MINT
  df <- share_mint_kurse(df)

  # calcualte the new "Gesamt"
  # df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
  #   dplyr::group_by(region, fachbereich, indikator, jahr) %>%
  #   dplyr::mutate(wert_new = wert[anzeige_geschlecht == "Frauen"] +
  #                   wert[anzeige_geschlecht == "Männer"])

  df <- df %>% dplyr::group_by(region, fachbereich, indikator, jahr) %>%
    dplyr::mutate(wert_new = wert[anzeige_geschlecht == "Gesamt"] ) %>%
    dplyr::filter(anzeige_geschlecht != "Gesamt")


  df = df[!duplicated(df$wert_new),]

  df$anzeige_geschlecht <- NULL


  df <- df %>%
    dplyr::group_by(jahr, indikator) %>%
    dplyr::mutate(sum_wert = sum(wert_new))


  # calculate proportions
  df <- df %>% dplyr::group_by(jahr, indikator, fachbereich) %>%
    dplyr::mutate(proportion = wert_new/sum_wert)

  df$proportion <- df$proportion * 100

  #Trennpunkte für lange Zahlen ergänzen
  df$wert_new <- prettyNum(df$wert_new, big.mark = ".")

  # order years for plot
  dfü <- df[with(df, order(jahr, decreasing = FALSE)), ]

  # plot

  highcharter::hchart(dfü, 'bar', highcharter::hcaes(y = round(proportion), x = indikator, group = "fachbereich")) %>%
    highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.y} % <br> Anzahl: {point.wert_new}") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
    highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
    highcharter::hc_title(text = paste0("Anteil von MINT in der Schule (", timerange,")"),
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

kurse_verlauf_gender <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_kurse_verlauf_gender

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  # remove
  df <- df %>% dplyr::filter(region == "Deutschland")

  # aggregate to MINT
  df <- share_mint_kurse(df)

  # calcualte the new "Gesamt"
  df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen" & fachbereich == "MINT")

  # calcualte proportions
  df <- df %>% dplyr::group_by(indikator, fachbereich, anzeige_geschlecht, jahr) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100



  # order years for plot
  df <- df[with(df, order(jahr, decreasing = FALSE)), ]

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = indikator)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil Schülerinnen  {point.indikator} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil von Schülerinnen in MINT-Fächern im Zeitverlauf"),
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
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

kurse_comparison_gender <- function(df,r) {


  # load UI inputs from reactive value
  timerange <- r$date_kurse_comparison_gender

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  # remove
  df <- df %>% dplyr::filter(region == "Deutschland")

  # aggregate to MINT
  df <- share_mint_kurse(df)

  # calcualte the new "Gesamt"
  df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
    dplyr::group_by(region, fachbereich, indikator, jahr) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")

  #gegenwert Berechnen für jeweilige Auswahl
  df_n <- df %>% dplyr::group_by(region, fachbereich, indikator, jahr) %>%
    dplyr::mutate(wert = props - wert)
  df_n$anzeige_geschlecht <- "Männer"

  df <- rbind(df, df_n)

  # calcualte proportions
  df1 <- df %>% dplyr::group_by(indikator, fachbereich, anzeige_geschlecht, jahr) %>%
    dplyr::mutate(proportion = wert/props)

  df1$proportion <- df1$proportion * 100


  df1$fachbereich <- factor(df1$fachbereich, levels = c("MINT","andere Fächer"))

  df1$indikator <- ifelse(df1$indikator == "Grundkurse" & df1$fachbereich == "MINT", "Grundkurse MINT-Fächer", df1$indikator)
  df1$indikator <- ifelse(df1$indikator == "Grundkurse" & df1$fachbereich == "andere Fächer", "Grundkurse andere Fächer", df1$indikator)
  df1$indikator <- ifelse(df1$indikator == "Leistungskurse" & df1$fachbereich == "MINT", "Leistungskurse MINT-Fächer", df1$indikator)
  df1$indikator <- ifelse(df1$indikator == "Leistungskurse" & df1$fachbereich == "andere Fächer", "Leistungskurse andere Fächer", df1$indikator)
  df1$indikator <- ifelse(df1$indikator == "Oberstufenbelegungen" & df1$fachbereich == "MINT", "Oberstufenbelegungen MINT-Fächer", df1$indikator)
  df1$indikator <- ifelse(df1$indikator == "Oberstufenbelegungen" & df1$fachbereich == "andere Fächer", "Oberstufenbelegungen andere Fächer", df1$indikator)

  # order years for plot
  df1 <- df1[with(df, order(jahr, decreasing = FALSE)), ]

  df1$anzeige_geschlecht[df1$anzeige_geschlecht == "Frauen"] <- "Mädchen"
  df1$anzeige_geschlecht[df1$anzeige_geschlecht == "Männer"] <- "Jungen"

  #Trennpunkte für lange Zahlen ergänzen
  df1$wert <- prettyNum(df1$wert, big.mark = ".")

  # plot
  highcharter::hchart(df1, 'bar', highcharter::hcaes( x = indikator, y=round(proportion), group = anzeige_geschlecht)) %>%
    highcharter::hc_tooltip(pointFormat = "{point.anzeige_geschlecht}-Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  TRUE) %>%
    highcharter::hc_xAxis(title = list(text = ""), categories=c("Grundkurse MINT-Fächer",
                                                                "Grundkurse andere Fächer",
                                                                "Leistungskurse MINT-Fächer",
                                                                "Leistungskurse andere Fächer",
                                                                "Oberstufenbelegungen MINT-Fächer",
                                                                "Oberstufenbelegungen andere Fächer")) %>%
    highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
    highcharter::hc_colors(c("#efe8e6", "#154194")) %>%
    highcharter::hc_title(text = paste0("Anteil von Mädchen in MINT- und anderen Fächern ", "(", timerange, ")",
                                                                       "<br><br><br>"),
                          margin = 25,
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

  # ggplot2::ggplot(df1, ggplot2::aes(x=indikator, y=proportion, fill = fachbereich)) +
  #   ggplot2::geom_bar(stat="identity", position=ggplot2::position_dodge(width=0.5), width=0.5) +
  #   ggplot2::geom_text(ggplot2::aes(label=paste(round(proportion),"%"), vjust = - 0.25),
  #                      position=ggplot2::position_dodge(width=0.5),
  #                      fontface = "bold") +
  #   ggplot2::theme_minimal() +
  #   ggplot2::theme(
  #     text = ggplot2::element_text(size = 14),
  #     plot.title = ggtext::element_markdown(hjust = 0.5)) +
  #   ggplot2::xlab("") + ggplot2::ylab("") +
  #   ggplot2::scale_fill_manual(values = c("#b16fab", "#efe8e6")) +
  #   ggplot2::labs(title = paste0(paste0("<span style='font-size:20.5pt; color:black'>",
  #                                "Anteil von Mädchen in MINT- und anderen Fächern ", "(", timerange, ")",
  #                                "<br><br><br>")),
  #                 fill = "") +
  #   ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"))

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

  subjects_select <- r$subject_selected_bl

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  # remove
  # df <- df %>% dplyr::filter(region != "Deutschland")
  #
  # df <- df %>% dplyr::filter(region != "Bayern")

  # include "Osten" und "Westen" in Dataframe
  df <- prep_kurse_east_west(df)

  # aggregate to MINT
  df_sub <- share_mint_kurse(df)

  df_sub <- df_sub[,colnames(df)]

  # aggregate all subjects to calculate proportion later
  df_sub <- df_sub %>% dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
    dplyr::mutate(props = sum(wert))


  df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT-Fächer (gesamt)"

  df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (gesamt)"

  # calcualte the new "Gesamt"
  df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
      dplyr::group_by(region, fachbereich, indikator, jahr) %>%
      dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                      wert[anzeige_geschlecht == "Männer"])

  df <- rbind(df, df_sub)

  # filter states
  df <- df %>% dplyr::filter(region %in% states)

  df <- df %>% dplyr::filter(fachbereich %in% subjects_select)

  # calculate proportions
  df <- df %>% dplyr::group_by(region, indikator, fachbereich, anzeige_geschlecht, jahr) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")

  df$anzeige_geschlecht <- paste0(df$anzeige_geschlecht, " (", df$indikator, ")")


  # order years for plot
  df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = anzeige_geschlecht)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil {point.anzeige_geschlecht} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil von Schülerinnen in ", subjects_select ," in ", states),
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
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

kurse_verlauf_multiple_bl <- function(df,r) {

  # load UI inputs from reactive value


  absolut_selector <- r$abs_zahlen_kurse_verlauf_multiple

  timerange <- r$date_kurse_verlauf_multiple

  states <- r$states_kurse_verlauf_multiple

  subjects_select <- r$subject_selected_multiple

  indikator_select <- r$topic_selected_multiple

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == indikator_select)

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

  # include "Osten" und "Westen" in Dataframe
  df <- prep_kurse_east_west(df)

  # aggregate to MINT
  df_sub <- share_mint_kurse(df)

  df_sub <- df_sub[,colnames(df)]

  # calculate the new "Gesamt"
  # df_sub <-  df_sub %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
  #   dplyr::group_by(region, fachbereich, indikator, jahr) %>%
  #   dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
  #                   wert[anzeige_geschlecht == "Männer"])

  # df_sub <- df_sub %>% dplyr::group_by(region, fachbereich, indikator, jahr) %>%
  #   dplyr::mutate(props = wert[anzeige_geschlecht == "Gesamt"] ) %>%
  #   dplyr::filter(anzeige_geschlecht != "Gesamt")
  #
  # df_sub <- df_sub %>% dplyr::filter(anzeige_geschlecht != "Männer")

  # aggregate all subjects to calculate proportion later
  df_sub <- df_sub %>% dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
    dplyr::mutate(sum_props = sum(wert))


  df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT-Fächer (gesamt)"

  df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (gesamt)"

  # calculate the new "Gesamt"
  # df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
  #   dplyr::group_by(region, fachbereich, indikator, jahr) %>%
  #   dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
  #                   wert[anzeige_geschlecht == "Männer"])

  # df <- df %>% dplyr::group_by(region, fachbereich, indikator, jahr) %>%
  #   dplyr::mutate(props = wert[anzeige_geschlecht == "Gesamt"] ) %>%
  #   dplyr::filter(anzeige_geschlecht != "Gesamt")
  #
  # df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")

  #Falsch - Enthält "Alle Fächer" bei Summe
  # df <- df %>%
  #   dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
  #   dplyr::mutate(sum_props = sum(props))

  df <- df %>%
    dplyr::filter(fachbereich != "Alle Fächer") %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
    dplyr::mutate(sum_props = sum(wert))


  # dfl <- dfl %>% dplyr::group_by(jahr, indikator, fachbereich, wert_new) %>%
  #   dplyr::summarize(wert = wert_new/sum_wert)%>%
  #   dplyr::rename(Absolut = wert_new, Relativ=wert)%>%
  #   tidyr::pivot_longer(c(Absolut, Relativ), names_to = "selector", values_to = "wert")%>%
  #   dplyr::filter(fachbereich == "MINT")

  df <- rbind(df, df_sub)



  df4 <<- df %>% dplyr::filter(fachbereich %in% subjects_select)

  # calculate proportions
  df5 <<- df4 %>% dplyr::group_by(jahr, region, indikator) %>%
    dplyr::summarize(wert, proportion = wert/sum_props)%>%
    dplyr::rename(Relativ = proportion, Absolut=wert)%>%
    tidyr::pivot_longer(c(Absolut, Relativ), names_to = "selector", values_to = "wert")%>%
    dplyr::mutate(selector = dplyr::case_when(
      selector == "Relativ" ~ "In Prozent",
      selector == "Absolut" ~ "Anzahl"
    ))%>%
    dplyr::ungroup()%>%
    dplyr::mutate(region=dplyr::case_when(
      region == "Westen" ~ "Westdeutschland (o. Berlin)",
      region == "Osten" ~ "Ostdeutschland (inkl. Berlin)",
      T ~ .$region
    ))

  # fitler states
  df5 <- df5 %>% dplyr::filter(region %in% states)

  if(absolut_selector=="In Prozent"){

  df <- df5 %>%
    dplyr::filter(selector=="In Prozent")


  df$wert <- df$wert * 100


  if(indikator_select == "Grundkurse") {

    title_help <- "Grundkurse:"

  }else {

    title_help <- "Leistungskurse:"

  }

  # order years for plot
  df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]


  help_title <- ifelse(subjects_select == "MINT-Fächer (gesamt)", "MINT-Fächern (gesamt)", subjects_select)
  help_title <- ifelse(help_title == "andere Fächer (gesamt)", "anderen Fächern (gesamt)", help_title)

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(wert), group = region)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil {point.region} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0(title_help, " Anteil von ", help_title),
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

  } else if(absolut_selector =="Anzahl"){

    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

  df <- df5 %>%
    dplyr::filter(selector=="Anzahl")



    if(indikator_select == "Grundkurse") {

      title_help <- "Grundkurse:"

    }else {

      title_help <- "Leistungskurse:"

    }

    # order years for plot
    df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]


    help_title <- ifelse(subjects_select == "MINT-Fächer (gesamt)", "MINT-Fächern (gesamt)", subjects_select)
    help_title <- ifelse(help_title == "andere Fächer (gesamt)", "anderen Fächern (gesamt)", help_title)

    # plot
    highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(wert), group = region)) %>%
      highcharter::hc_tooltip(pointFormat = "Anzahl: {point.y}") %>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
      #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
      highcharter::hc_title(text = paste0(title_help, " Anteil von ", help_title),
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


}


#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

kurse_verlauf_subjects_bl <- function(df,r) {

  # load UI inputs from reactive value

  absolut_selector <- r$abs_zahlen_kurse_verlauf_subject_bl

  timerange <- r$date_kurse_verlauf_subject_bl

  states <- r$states_kurse_verlauf_subject_bl

  indikator_kurse <- r$topic_selected_subject_bl

  subjects_select <- r$subject_selected_bl_sub

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  # remove
  # df <- df %>% dplyr::filter(region != "Deutschland")

  # df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(indikator == indikator_kurse)


  df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Leistungskurse"), "wert"] <-  df %>%
    dplyr::filter(indikator == "Leistungskurse") %>%
    dplyr::group_by(indikator, jahr) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Gesamt"]) %>% dplyr::pull(wert)

  df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Grundkurse"), "wert"] <-  df %>%
    dplyr::filter(indikator == "Grundkurse") %>%
    dplyr::group_by(indikator, jahr) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Gesamt"]) %>% dplyr::pull(wert)

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

# include "Osten" und "Westen" in Dataframe
df <- prep_kurse_east_west(df)


# aggregate to MINT
df_sub <- share_mint_kurse(df)

df_sub <- df_sub[,colnames(df)]

# calculate the new "Gesamt"
# df_sub <-  df_sub %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
#   dplyr::group_by(region, fachbereich, indikator, jahr) %>%
#   dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
#                   wert[anzeige_geschlecht == "Männer"])
#
# df_sub <- df_sub %>% dplyr::group_by(region, fachbereich, indikator, jahr) %>%
#   dplyr::mutate(props = wert[anzeige_geschlecht == "Gesamt"] ) %>%
#   dplyr::filter(anzeige_geschlecht != "Gesamt")
#
# df_sub <- df_sub %>% dplyr::filter(anzeige_geschlecht != "Männer")


# aggregate all subjects to calculate proportion later
df_sub <- df_sub %>% dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
  dplyr::mutate(sum_props = sum(wert))

df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT-Fächer (gesamt)"

df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (gesamt)"

# calculate the new "Gesamt"
# df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
#   dplyr::group_by(region, fachbereich, indikator, jahr) %>%
#   dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
#                   wert[anzeige_geschlecht == "Männer"])

# df <- df %>% dplyr::group_by(region, fachbereich, indikator, jahr) %>%
#   dplyr::mutate(props = wert[anzeige_geschlecht == "Gesamt"] ) %>%
#   dplyr::filter(anzeige_geschlecht != "Gesamt")
#
# df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")

df <- df %>%
  dplyr::filter(fachbereich != "Alle Fächer") %>%
  dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
  dplyr::mutate(sum_props = sum(wert))


df <- rbind(df, df_sub)



df <- df %>% dplyr::filter(fachbereich %in% subjects_select)



# calculate proportions

dfm <<- df

dfm <<- dfm %>% dplyr::group_by(jahr, region, fachbereich) %>%
  dplyr::summarize(wert, proportion = wert/sum_props)%>%
  dplyr::rename(Relativ = proportion, Absolut=wert)%>%
  tidyr::pivot_longer(c(Absolut, Relativ), names_to = "selector", values_to = "wert")%>%
  dplyr::mutate(selector= dplyr::case_when(
    selector == "Absolut" ~ "Anzahl",
    selector == "Relativ" ~ "In Prozent"
  ))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(region= dplyr::case_when(
    region == "Osten" ~ "Ostdeutschland (inkl. Berlin)",
    region == "Westen" ~ "Westdeutschland (o. Berlin)",
    T ~ .$region
  ))


# fitler states
dfm <- dfm %>% dplyr::filter(region %in% states)

if(absolut_selector=="In Prozent"){

  df <- dfm %>%
    dplyr::filter(selector == "In Prozent")

df$wert <- df$wert * 100

df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]


  # order years for plot
  df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(wert), group = fachbereich)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil {point.fachbereich} <br> Wert: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
   # highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil einzelner Fächer in ", states, " (", indikator_kurse, ")"),
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

} else if (absolut_selector=="Anzahl"){

  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- "."
  options(highcharter.lang = hcoptslang)

  df <- dfm %>%
    dplyr::filter(selector == "Anzahl")

  df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]

  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(wert), group = fachbereich)) %>%
    highcharter::hc_tooltip(pointFormat = "Anzahl: {point.y}") %>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    # highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = paste0("Anteil einzelner Fächer in ", states, " (", indikator_kurse, ")"),
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
}



iqb_standard_zeitverlauf <- function(df, r){

  # reactive values übergeben
  bl_select <- r$land_iqb_standard_zeitverlauf
dfg <- df
  # Region filtern
  df1 <- dfg %>% dplyr::filter(region %in% bl_select)

  # Anforderungen, die wir nicht betrachten, ausfiltern
  dfk <- df1 %>% dplyr::filter(indikator == "Mindeststandard nicht erreicht")

  # title helper

  if (length(bl_select)==1){
    title_help <- paste0(bl_select)
  }else if(length(bl_select)==2){
    title_help <- paste0(bl_select[1], " & ", bl_select[2] )
  }else if (length(bl_select)==3){
    title_help <- paste0(bl_select[1], " , ", bl_select[2], " & ", bl_select[3])
  }


    highcharter::hchart(dfk, 'column', highcharter::hcaes(y = wert, x = region, group=jahr))%>%
      highcharter::hc_plotOptions(column = list(pointWidth = 90))%>%
      highcharter::hc_tooltip(pointFormat = "{point.jahr} <br> {point.y} % leistungsschwach")%>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value} %"), pointsWidth=100) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
    #  highcharter::hc_plotOptions(column = list(stacking = "percent")) %>%
      highcharter::hc_colors(c("#efe8e6","#D0A9CD",
                               "#b16fab")) %>%

      highcharter::hc_title(text = paste0("Anteil der leistungsschwachen Schüler und Schülerinnen in Mathematik in ", title_help),
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


iqb_mathe_mittel_zeitverlauf <- function(df, r){

  # reactive values übergeben
  bl_select <- r$land_iqb_mathe_mittel_zeitverlauf
  indikator_select <- r$indi_iqb_mathe_mittel_zeitverlauf

  # Region filtern
  df <- df %>% dplyr::filter(region == bl_select)

  # Jahr als Faktor speichern, für schönere x-Achse
  df$jahr <- as.factor(df$jahr)

  # nach gewählter Vergleichsgruppe filtern
  if (indikator_select == "nach Geschlecht") {
    df <- df %>% dplyr::filter(indikator == "Alle")

    df$geschlecht <- as.factor(df$geschlecht)
    df$geschlecht <- factor(df$geschlecht, levels = c("gesamt", "Mädchen", "Jungen"))

    df <- df %>% dplyr::mutate(geschlecht=dplyr::case_when(
      geschlecht=="gesamt" ~ "Gesamt",
      T~df$geschlecht
    ))%>%
      dplyr::filter(geschlecht != "Gesamt")

  }
  else{

    if (indikator_select == "nach Migrationsgeschichte"){
      df <- df %>% dplyr::filter(indikator %in% c("Alle", "Migrationsgeschichte", "keine Migrationsgeschichte"))
      df <- df %>% dplyr::filter(geschlecht == "gesamt")%>%
        dplyr::mutate(indikator=dplyr::case_when(indikator == "Alle" ~"Gesamt",
                                       indikator=="Migrationsgeschichte" ~ "Mit Migrationshintergrund",
                                       indikator=="keine Migrationsgeschichte"~"Ohne Migrationshintergrund"

        ))%>%
        dplyr::filter(indikator != "Gesamt")



      df <- df %>% dplyr::mutate(geschlecht=dplyr::case_when(
        geschlecht=="gesamt" ~ "Gesamt",
        T~df$geschlecht
      ))

      # df <- df %>% dplyr::mutate(indikator=case_when(
      #   indikator=="gesamt" ~ "Gesamt",
      #   T~df$indikator
      # ))

      df$indikator<- as.factor(df$indikator)
      df$indikator <- factor(df$indikator, levels = c("Gesamt", "Mit Migrationshintergrund",
                                                      "Ohne Migrationshintergrund" ))


    }

    else{
      if(indikator_select == "nach Bildungshintergrund")
      df <- df %>% dplyr::filter(indikator %in% c("Alle", "status_hoch", "status_niedrig"))
      df <- df %>% dplyr::filter(geschlecht == "gesamt")

      df <- df %>% dplyr::mutate(geschlecht=dplyr::case_when(
        geschlecht=="gesamt" ~ "Gesamt",
        T~df$geschlecht
      ))

      # Labels umbenennen
      df$indikator[df$indikator == "status_hoch"] <- "Mehr als 100 Bücher zuhause"
      df$indikator[df$indikator == "status_niedrig"] <- "100 Bücher oder weniger zuhause"

      df$indikator<- as.factor(df$indikator)
      df$indikator <- factor(df$indikator, levels = c("Alle", "100 Bücher oder weniger zuhause", "Mehr als 100 Bücher zuhause" ))

      df <- df %>%
        dplyr::filter(indikator!="Alle")
    }
  }

  if(indikator_select == "nach Geschlecht"){

    highcharter::hchart(df, 'column', highcharter::hcaes(y = wert, x = jahr, group = geschlecht))%>%
      highcharter::hc_plotOptions(column = list(pointWidth = 90))%>%
      highcharter::hc_tooltip(pointFormat = "{point.group} <br> Durchschnitt Mathe: {point.y}")%>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}")) %>%
      highcharter::hc_xAxis(title = list(text = ""), categories = c("2011",
                                                                    "2016",
                                                                    "2021")
      ) %>%
      #  highcharter::hc_plotOptions(column = list(stacking = "percent")) %>%
      highcharter::hc_colors(c("#efe8e6",
                               "#b16fab", "#D0A9CD"
                               #"#154194",
      )) %>%
      highcharter::hc_title(text = paste0("Durchschnittliche Leistung der Schüler und Schülerinnen in Mathematik nach Geschlecht in " , bl_select),
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
  else{
    highcharter::hchart(df, 'column', highcharter::hcaes(y = wert, x = jahr, group = indikator))%>%
      highcharter::hc_plotOptions(column = list(pointWidth = 90))%>%
      highcharter::hc_tooltip(pointFormat = "{point.group} <br> Durchschnitt Mathe : {point.y}")%>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}")) %>%
      highcharter::hc_xAxis(title = list(text = ""), categories = c("2011",
                                                                    "2016",
                                                                    "2021")) %>%
      #  highcharter::hc_plotOptions(column = list(stacking = "percent")) %>%
      highcharter::hc_colors(c("#efe8e6", "#D0A9CD",
                               "#b16fab"
                               #"#154194",
      )) %>%
      highcharter::hc_title(text = paste0("Durchschnittliche Leistung der Schüler und Schülerinnen in Mathematik ", indikator_select, " in " , bl_select),
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

}

