#' A function to plot a graph.
#'
#' @description A function to create a pie chart for the first box
#' inside the tab "Home".
#'
#' @return The return value is a plot
#' @param df The dataframe "zentral.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
home_einstieg_pie <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- 2020

  indikator_choice_1 <- r$indikator_start_einstieg_1

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")


  # call function to calculate the share of MINT for every "bereich"
  df <- share_MINT(df)

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

  df <- df %>% dplyr::filter(indikator %in% indikator_choice_1)


  # calculate proportions for MINT vs. Rest
  df <- df %>% dplyr::group_by(indikator) %>%
    dplyr::mutate(props = sum(wert))


  df <- df %>% dplyr::group_by(indikator, fachbereich) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100

  # create an if statement for the options of plotting 1, 2 or 3 graphs
  if(length(indikator_choice_1) == 1) {

  # ensure that proportion sum to 1
  df$proportion <- round_preserve_sum(as.numeric(df$proportion),0)

  title_help <- helper_title_home(indikator_choice_1)

  highcharter::hw_grid(
  df %>%
    highcharter::hchart(
      "pie", highcharter::hcaes(x = fachbereich, y = proportion)
      ) %>%
    highcharter::hc_tooltip(
               pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
    highcharter::hc_colors(c("#efe8e6", "#b16fab")) %>%
    highcharter::hc_title(text = paste0("Anteil MINT-", title_help, " in 2020"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
    highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                           dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),

  ncol = 1,
  browsable = TRUE
  )

  # blau: #154194
  # pink: #b16fab
  # helles pink: #d0a9cd

  } else if(length(indikator_choice_1) == 2) {

    # filter for UI input and ensure proportions sum to 1
    df_1 <- df %>% dplyr::filter(indikator == indikator_choice_1[1])

    df_1$proportion <- round_preserve_sum(as.numeric(df_1$proportion),0)

    title_help_1 <- helper_title_home(indikator_choice_1[1])

    df_2 <- df %>% dplyr::filter(indikator == indikator_choice_1[2])

    df_2$proportion <- round_preserve_sum(as.numeric(df_2$proportion),0)

    title_help_2 <- helper_title_home(indikator_choice_1[2])


    highcharter::hw_grid(
      highcharter::hchart(df_1, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
        highcharter::hc_colors(c("#efe8e6","#d0a9cd")) %>%
        highcharter::hc_title(text = paste0("Anteil MINT-",title_help_1, " in 2020"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),


      highcharter::hchart(df_2, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
        highcharter::hc_colors(c("#efe8e6","#b16fab")) %>%
        highcharter::hc_title(text = paste0("Anteil MINT-",title_help_2, " in 2020"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE)),

      ncol = 2,
      browsable = TRUE
    )


  } else if(length(indikator_choice_1) == 3) {

    # filter for UI input and ensure proportions sum to 1

    df_1 <- df %>% dplyr::filter(indikator == indikator_choice_1[1])

    df_1$proportion <- round_preserve_sum(as.numeric(df_1$proportion),0)

    title_help_1 <- helper_title_home(indikator_choice_1[1])

    df_2 <- df %>% dplyr::filter(indikator == indikator_choice_1[2])

    df_2$proportion <- round_preserve_sum(as.numeric(df_2$proportion),0)

    title_help_2 <- helper_title_home(indikator_choice_1[2])

    df_3 <- df %>% dplyr::filter(indikator == indikator_choice_1[3])

    df_3$proportion <- round_preserve_sum(as.numeric(df_3$proportion),0)

    title_help_3 <- helper_title_home(indikator_choice_1[3])


    highcharter::hw_grid(
      highcharter::hchart(df_1, size = 170, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
        highcharter::hc_colors(c("#d0a9cd","#b16fab")) %>%
        highcharter::hc_title(text = paste0("Anteil MINT- ",title_help_1, " in 2020"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),


      highcharter::hchart(df_2, size = 170, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
        highcharter::hc_colors(c("#154194","#b16fab")) %>%
        highcharter::hc_title(text = paste0("Anteil MINT- ",title_help_2, " in 2020"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE)),

      highcharter::hchart(df_3, size = 170, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
        highcharter::hc_colors(c("#154194","#b16fab")) %>%
        highcharter::hc_title(text = paste0("Anteil MINT-",title_help_3, " in 2020"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),



      ncol = 3,
      browsable = TRUE
    )

    }

}


#' A function to plot a graph.
#'
#' @description A function to create a pie chart for the first box
#' inside the tab "Home".
#'
#' @return The return value is a plot
#' @param df The dataframe "zentral.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
home_einstieg_pie_gender <- function(df, df_naa, r) {

  # load UI inputs from reactive value
  timerange <- 2020

  indikator_choice_1_gender <- r$indikator_start_einstieg_1_gender

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)
  df_naa  <- df_naa %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")
  df_naa <- df_naa %>% dplyr::filter(region == "Deutschland")


  # call function to calculate the share of MINT for every "bereich"
  df <- share_MINT(df)

  #rename
  df[df$fachbereich != "MINT", "fachbereich"] <- "Andere Fachbereiche"

  # order
  df <- df[with(df, order(indikator, anzeige_geschlecht, decreasing = TRUE)), ]

  # filter parts which need more invovled calculation of proportion
  df_sub_2 <- df %>% dplyr::filter(indikator == "Promotionen (angestrebt)" |
                                     indikator == "Habilitationen" | indikator == "Leistungskurse")

  # calculate proprotion female
  df_sub_2[df_sub_2$anzeige_geschlecht == "Frauen", "wert"] <-  df_sub_2 %>% dplyr::group_by(indikator, fachbereich) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"]/
                       wert[anzeige_geschlecht == "Gesamt"]) %>%
    dplyr::arrange(-dplyr::row_number()) %>% dplyr::pull(wert)

  # calculate proprotion male
  df_sub_2[df_sub_2$anzeige_geschlecht == "Männer", "wert"] <- df_sub_2 %>% dplyr::group_by(indikator, fachbereich) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Männer"]/
                       wert[anzeige_geschlecht == "Gesamt"]) %>%
    dplyr::arrange(-dplyr::row_number()) %>% dplyr::pull(wert)


  df_sub_2 <- df_sub_2 %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

  df_sub_2$wert <- df_sub_2$wert * 100

  # calcuate porportion of remaining topics
  df <- df %>% dplyr::filter(indikator != "Promotionen (angestrebt)",
                             indikator != "Habilitationen", indikator != "Leistungskurse") %>%
    dplyr::group_by(indikator, fachbereich) %>%
    dplyr::summarize(wert = dplyr::lead(wert)/wert) %>% na.omit()

  df$wert <- df$wert * 100

  df$anzeige_geschlecht <- "Frauen"

  df_sub <- df

  df_sub$anzeige_geschlecht <- "Männer"

  df_sub$wert <- 100 - df$wert

  df <- rbind(df, df_sub, df_sub_2)

  # calcualte proportion for "neue ausbildungsverträge"
  df_sub <- df_naa %>% dplyr::group_by(anzeige_geschlecht) %>%
    dplyr::summarize(wert = sum(wert))

  df_sub <- df_sub %>% dplyr::group_by(anzeige_geschlecht) %>%
    dplyr::summarize(wert = wert/df_sub[df_sub$anzeige_geschlecht == "Gesamt", "wert"][[1]])

  df_sub$wert <- df_sub$wert * 100

  df_sub <- df_sub %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

  df_sub$indikator <- "Neue Ausbildungsverträge"

  df_sub$fachbereich <- "MINT"


  df_sub <- df_sub[, c("indikator", "fachbereich", "wert", "anzeige_geschlecht")]

  df <- subset(df, select = c(indikator, fachbereich, wert, anzeige_geschlecht))

  df <- rbind(df, df_sub)

  df <- df %>% dplyr::filter(indikator %in% indikator_choice_1_gender) %>%
    dplyr::arrange(anzeige_geschlecht)

  # create an if statement for the options of plotting 1, 2 or 3 graphs
  if(length(indikator_choice_1_gender) == 1) {

    # ensure that proportion sum to 1
    df_mint <- df %>% dplyr::filter(fachbereich == "MINT")

    df_mint$wert <- round_preserve_sum(as.numeric(df_mint$wert),0)

    df_rest <- df %>% dplyr::filter(fachbereich == "Andere Fachbereiche")

    df_rest$wert <- round_preserve_sum(as.numeric(df_rest$wert),0)

    #title_help <- helper_title_home(indikator_choice_1_gender)

    highcharter::hw_grid(

        highcharter::hchart(df_mint, size = 280,
          "pie", highcharter::hcaes(x = anzeige_geschlecht, y = wert)
        ) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
        highcharter::hc_colors(c("#154194","#efe8e6")) %>%
        highcharter::hc_title(text = paste0("Anteil von Frauen im MINT-Bereich für ", indikator_choice_1_gender, " in 2020"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),

        highcharter::hchart(df_rest, size = 150,
                            "pie", highcharter::hcaes(x = anzeige_geschlecht, y = wert)
        ) %>%
          highcharter::hc_tooltip(
            pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
          highcharter::hc_colors(c("#154194","#efe8e6")) %>%
          highcharter::hc_title(text = paste0("Anteil von Frauen in anderen Bereichen für ", indikator_choice_1_gender, " in 2020"),
                                margin = 45,
                                align = "center",
                                style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
          highcharter::hc_chart(
            style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
          highcharter::hc_legend(enabled = TRUE) %>%
          highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                                 dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE,
                                                 opacity = 0.7)),

      ncol = 1,
      browsable = TRUE
    )


  } else if(length(indikator_choice_1_gender) == 2) {

    # filter for UI input and ensure proportions sum to 1
    df_mint <- df %>% dplyr::filter(fachbereich == "MINT")

    df_1_mint <- df_mint %>% dplyr::filter(indikator == indikator_choice_1_gender[1])

    df_1_mint$wert <- round_preserve_sum(as.numeric(df_1_mint$wert),0)

    df_2_mint <- df_mint %>% dplyr::filter(indikator == indikator_choice_1_gender[2])

    df_2_mint$wert <- round_preserve_sum(as.numeric(df_2_mint$wert),0)

    df_rest <- df %>% dplyr::filter(fachbereich == "Andere Fachbereiche")

    df_1_rest <- df_rest %>% dplyr::filter(indikator == indikator_choice_1_gender[1])

    df_1_rest$wert <- round_preserve_sum(as.numeric(df_1_rest$wert),0)

    df_2_rest<- df_rest %>% dplyr::filter(indikator == indikator_choice_1_gender[2])

    df_2_rest$wert <- round_preserve_sum(as.numeric(df_2_rest$wert),0)


    highcharter::hw_grid(



      highcharter::hchart(df_1_mint, size = 280, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
        highcharter::hc_colors(c("#154194","#efe8e6")) %>%
        highcharter::hc_title(text = paste0("Anteil von Frauen im MINT-Bereich für ", indikator_choice_1_gender[1], " in 2020"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),


      highcharter::hchart(df_2_mint, size = 280, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
        highcharter::hc_colors(c("#154194","#efe8e6")) %>%
        highcharter::hc_title(text = paste0("Anteil von Frauen im MINT-Bereich für ", indikator_choice_1_gender[2], " in 2020"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),


      highcharter::hchart(df_1_rest, size = 150, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
        highcharter::hc_colors(c("#154194","#efe8e6")) %>%
        highcharter::hc_title(text = paste0("Anteil von Frauen in anderen Bereichen für ", indikator_choice_1_gender[1], " in 2020"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE,
                                               opacity = 0.7)),


      highcharter::hchart(df_2_rest, size = 150, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
        highcharter::hc_colors(c("#154194","#efe8e6")) %>%
        highcharter::hc_title(text = paste0("Anteil von Frauen in anderen Bereichen für ", indikator_choice_1_gender[2], " in 2020"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE,
                                               opacity = 0.7)),

      ncol = 2,
      browsable = TRUE
    )


  } else if(length(indikator_choice_1_gender) == 3) {

    # filter for UI input and ensure proportions sum to 1
    # filter for UI input and ensure proportions sum to 1
    df_mint <- df %>% dplyr::filter(fachbereich == "MINT")

    df_1_mint <- df_mint %>% dplyr::filter(indikator == indikator_choice_1_gender[1])

    df_1_mint$wert <- round_preserve_sum(as.numeric(df_1_mint$wert),0)

    df_2_mint <- df_mint %>% dplyr::filter(indikator == indikator_choice_1_gender[2])

    df_2_mint$wert <- round_preserve_sum(as.numeric(df_2_mint$wert),0)

    df_3_mint <- df_mint %>% dplyr::filter(indikator == indikator_choice_1_gender[3])

    df_3_mint$wert <- round_preserve_sum(as.numeric(df_3_mint$wert),0)

    df_rest <- df %>% dplyr::filter(fachbereich == "Andere Fachbereiche")

    df_1_rest <- df_rest %>% dplyr::filter(indikator == indikator_choice_1_gender[1])

    df_1_rest$wert <- round_preserve_sum(as.numeric(df_1_rest$wert),0)

    df_2_rest<- df_rest %>% dplyr::filter(indikator == indikator_choice_1_gender[2])

    df_2_rest$wert <- round_preserve_sum(as.numeric(df_2_rest$wert),0)

    df_3_rest<- df_rest %>% dplyr::filter(indikator == indikator_choice_1_gender[3])

    df_3_rest$wert <- round_preserve_sum(as.numeric(df_3_rest$wert),0)

    highcharter::hw_grid(


      highcharter::hchart(df_1_mint, size = 170, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
        highcharter::hc_colors(c("#154194","#efe8e6")) %>%
        highcharter::hc_title(text = paste0("Anteil von Frauen im MINT-Bereich für ", indikator_choice_1_gender[1], " in 2020"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),


      highcharter::hchart(df_2_mint, size = 170, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
        highcharter::hc_colors(c("#154194","#efe8e6")) %>%
        highcharter::hc_title(text = paste0("Anteil von Frauen im MINT-Bereich für ", indikator_choice_1_gender[2], " in 2020"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),

      highcharter::hchart(df_3_mint, size = 170, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
        highcharter::hc_colors(c("#154194","#efe8e6")) %>%
        highcharter::hc_title(text = paste0("Anteil von Frauen im MINT-Bereich für ", indikator_choice_1_gender[3], " in 2020"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_legend(enabled = TRUE) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE)),


      highcharter::hchart(df_1_rest, size = 100, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
        highcharter::hc_colors(c("#154194","#efe8e6")) %>%
        highcharter::hc_title(text = paste0("Anteil von Frauen in anderen Bereichen für ", indikator_choice_1_gender[1], " in 2020"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE,
                                               opacity = 0.7)),


      highcharter::hchart(df_2_rest, size = 100, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
        highcharter::hc_colors(c("#154194","#efe8e6")) %>%
        highcharter::hc_title(text = paste0("Anteil von Frauen in anderen Bereichen für ", indikator_choice_1_gender[2], " in 2020"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE,
                                               opacity = 0.7)),

      highcharter::hchart(df_3_rest, size = 100, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
        highcharter::hc_colors(c("#154194","#efe8e6")) %>%
        highcharter::hc_title(text = paste0("Anteil von Frauen in anderen Bereichen für ", indikator_choice_1_gender[3], " in 2020"),
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_chart(
          style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
                                               dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE,
                                               opacity = 0.7)),




      ncol = 3,
      browsable = TRUE
    )

  }

}


#' A function to plot a graph.
#'
#' @description A function to create a stacked bar chart
#'
#' @return The return value is a plot
#' @param df The dataframe "zentral.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
home_stacked_comparison_gender <- function(df, df_naa, r) {

  # load UI inputs from reactive value
  timerange <- r$date_start_comparison_mint_gender

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)
  df_naa  <- df_naa %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")
  df_naa <- df_naa %>% dplyr::filter(region == "Deutschland")

  # call function to calculate the share of MINT for every "bereich"
  df <- share_MINT(df)

  #rename
  df[df$fachbereich != "MINT", "fachbereich"] <- "Andere Fachbereiche"

  df <- df %>% dplyr::filter(fachbereich != "Andere Fachbereiche")


  # calculate the new "Gesamt"
  df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Leistungskurse"), "wert"] <-  df %>%
    dplyr::filter(indikator == "Leistungskurse") %>%
    dplyr::group_by(indikator, jahr) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"] +
                       wert[anzeige_geschlecht == "Männer"]) %>% dplyr::pull(wert)

  # order
  df <- df[with(df, order(indikator, anzeige_geschlecht, decreasing = TRUE)), ]

  # filter parts which need more invovled calculation of proportion
  df_sub_2 <- df %>% dplyr::filter(indikator == "Promotionen (angestrebt)" |
                                   indikator == "Habilitationen" | indikator == "Leistungskurse")

  # calculate proprotion female
  df_sub_2[df_sub_2$anzeige_geschlecht == "Frauen", "wert"] <-  df_sub_2 %>% dplyr::group_by(indikator) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"]/
                       wert[anzeige_geschlecht == "Gesamt"]) %>%
    dplyr::arrange(-dplyr::row_number()) %>% dplyr::pull(wert)

  # calculate proprotion male
  df_sub_2[df_sub_2$anzeige_geschlecht == "Männer", "wert"] <- df_sub_2 %>% dplyr::group_by(indikator) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Männer"]/
                       wert[anzeige_geschlecht == "Gesamt"]) %>%
    dplyr::arrange(-dplyr::row_number()) %>% dplyr::pull(wert)


  df_sub_2 <- df_sub_2 %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

  df_sub_2$wert <- df_sub_2$wert * 100

  # calcuate porportion of remaining topics
  df <- df %>% dplyr::filter(indikator != "Promotionen (angestrebt)",
                             indikator != "Habilitationen", indikator != "Leistungskurse") %>%
    dplyr::group_by(indikator) %>%
    dplyr::summarize(wert = dplyr::lead(wert)/wert) %>% na.omit()

  df$wert <- df$wert * 100

  df$anzeige_geschlecht <- "Frauen"

  df_sub <- df

  df_sub$anzeige_geschlecht <- "Männer"

  df_sub$wert <- 100 - df$wert

  df <- rbind(df, df_sub, df_sub_2)

  # calcualte proportion for "neue ausbildungsverträge"
  df_sub <- df_naa %>% dplyr::group_by(anzeige_geschlecht) %>%
    dplyr::summarize(wert = sum(wert))

  df_sub <- df_sub %>% dplyr::group_by(anzeige_geschlecht) %>%
    dplyr::summarize(wert = wert/df_sub[df_sub$anzeige_geschlecht == "Gesamt", "wert"][[1]])

  df_sub$wert <- df_sub$wert * 100

  df_sub <- df_sub %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

  df_sub$indikator <- "Neue Ausbildungsverträge"

  df <- rbind(df, df_sub)

  df <- df %>% dplyr::filter(indikator %in% c("Leistungskurse",
                                              "Studienanfänger", "Studierende",
                                              "Auszubildende", "Beschäftigte"))

  # order
  df$indikator <- factor(df$indikator , levels=c("Leistungskurse",
                                                 "Studienanfänger", "Studierende",
                                                 "Auszubildende", "Beschäftigte"))


  ggplot2::ggplot(df, ggplot2::aes(x=indikator, y=wert, fill = anzeige_geschlecht)) +
    ggplot2::geom_bar(stat="identity", position = "dodge") +
    ggplot2::geom_text(ggplot2::aes(label=paste(round(wert),"%"), vjust = - 0.25),
                       position=ggplot2::position_dodge(width=0.9),
                       fontface = "bold") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 14),
      plot.title = ggtext::element_markdown(hjust = 0.5)) +
    ggplot2::xlab("") + ggplot2::ylab("Anteil") +
    ggplot2::scale_fill_manual(values = c("#154194","#b16fab")) +
    ggplot2::labs(title = paste0(paste0("<span style='font-size:20.5pt; color:black'>",
                                 "Frauenanteil in MINT im Vergleich in ", timerange,
                                 "<br><br><br>")),
                  fill = "") +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"))

}


#' A function to plot a graph.
#'
#' @description A function to create a stacked bar chart
#'
#' @return The return value is a plot
#' @param df The dataframe "zentral.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
home_stacked_comparison_mint <- function(df, r) {

  # load UI inputs from reactive value
  timerange <- r$date_start_comparison_mint

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(region == "Deutschland")

  # call function to calculate the share of MINT for every "bereich"
  df <- share_MINT(df)

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")

  df <- df %>% dplyr::filter(indikator %in% c("Leistungskurse",
                                              "Studienanfänger", "Studierende",
                                              "Auszubildende", "Beschäftigte"))

  # calculate proportions for MINT vs. Rest
  df <- df %>% dplyr::group_by(indikator) %>%
    dplyr::mutate(props = sum(wert))


  df <- df %>% dplyr::group_by(indikator, fachbereich) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100

  # order
  x <- ordered(factor(df$indikator), levels=c("Leistungskurse",
                                              "Studienanfänger", "Studierende",
                                              "Auszubildende", "Beschäftigte"))

  df <- df[order(x),]

  df[df$fachbereich != "MINT", "fachbereich"] <- "andere Fachbereiche"



  highcharter::hchart(df, 'bar', highcharter::hcaes(y = round(proportion), x = indikator, group = "fachbereich")) %>%
    highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Anteil"), labels = list(format = "{value}%")) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
    highcharter::hc_colors(c("#154194", "#b16fab")) %>%
    highcharter::hc_title(text = paste0("MINT-Anteile im Vergleich in ", timerange),
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
                                                              this.exportChart({ type: 'image/png' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

}


#' A function to plot a graph.
#'
#' @description A function to create a line chart
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


  # call function to calculate the share of MINT for every "bereich"
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
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"),
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular"),
                          min = 10, max = 45) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = "Anteil von Frauen an MINT im Verlauf",
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

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
home_rest_mint_verlauf <- function(df,r) {

  # load UI inputs from reactive value
  timerange <- r$date_start_multiple

  indikator_choice_1 <- r$indikator_start_multiple_1

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(region == "Deutschland")


  # call function to calculate the share of MINT for every "bereich"
  df <- share_MINT(df)

  df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")


  df <- df %>% dplyr::filter(indikator %in% indikator_choice_1)

  # calculate proportions
  df <- df %>% dplyr::group_by(indikator, jahr) %>%
    dplyr::mutate(props = sum(wert))


  df <- df %>% dplyr::group_by(indikator, jahr,fachbereich) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100


  #here only MINT
  df <- df %>% dplyr::filter(fachbereich == "MINT")

  # plot
  highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = indikator)) %>%
    highcharter::hc_tooltip(pointFormat = "Anteil MINT <br> Indikator: {point.indikator} <br> Anteil: {point.y} %") %>%
    highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"),
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
    #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
    highcharter::hc_title(text = "Anteil von MINT im Verlauf",
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_exporting(enabled = TRUE,
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                              this.exportChart({ type: 'image/png' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

}
