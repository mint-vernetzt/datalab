################################################################################
################################# Studienzahl #################################
################################################################################


#' make_plots_studium
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @param data
#' @param r
#' @noRd

studienzahl_einstieg_bar <- function(df,r) {

  options(scipen=999)

  timerange <- r$date_studierende_einstieg

  status_studierende <- r$indikator_studierende_einstieg

  geschlecht <- r$geschlecht_studierende_einstieg

  switch_absolut <- r$switch_rel_abs

  lehramt_enthalten <- r$nurLehramt_studierende_einstieg

  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == status_studierende)

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- calc_anteil_MINT(df)

  help_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "gesamt") %>%
    dplyr::group_by(jahr, fachbereich)

  help_weiblich <- df %>% dplyr::filter(anzeige_geschlecht == "frauen") %>%
    dplyr::group_by(jahr, fachbereich)

  wert_männlich <- help_gesamt$wert - help_weiblich$wert

  help_männlich <- help_weiblich

  help_männlich$wert <- wert_männlich
  help_männlich$anzeige_geschlecht <- "männer"

  df <- rbind(df, help_männlich)

  df <- df[with(df, order(fachbereich, jahr, decreasing = TRUE)), ]

  df <- df %>% dplyr::filter(anzeige_geschlecht %in% geschlecht)

  if(isTRUE(lehramt_enthalten)){

    values <- df %>%
      dplyr::group_by(jahr, fachbereich, anzeige_geschlecht) %>%
      dplyr::mutate(wert = wert - dplyr::lead(wert, n = 2)) %>% dplyr::select(wert) %>% na.omit()

    toDelete <- seq(2, nrow(values), 2)
    values <- values[-toDelete ,]

    df[df$hochschulform == "insgesamt", "wert"] <- values$wert

    df <- df[!(df$nur_lehramt == "Nein" & df$hochschulform == "Uni"), ]

    df <- df[!(df$nur_lehramt == "Nein" & df$hochschulform == "FH"), ]

    df[df$nur_lehramt == "Ja", "fachbereich"] <- interaction(df[df$nur_lehramt == "Ja", "fachbereich"][[1]], "_lehramt", sep = "")

   p <- ggplot2::ggplot(df, ggplot2::aes(fill=fachbereich, y=wert, x=anzeige_geschlecht)) +
      ggplot2::facet_grid(~jahr,
                 scales = "free_x",
                 space = "free_x",
                 switch = "x")  +
      ggplot2::labs(caption = "Quelle:", title = paste0("Anteile an MINT und allen anderen Studienfächer")) +
      ggplot2::theme(strip.placement = "outside",
                     panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
                     panel.background = ggplot2::element_rect(fill="white"),
                     strip.background = ggplot2::element_rect(fill = "white"),
                     axis.title = ggplot2::element_blank(),
                     panel.spacing.x = ggplot2::unit(0.5,"line")) +
      ggplot2::scale_fill_manual(values = c(ggplot2::alpha("#1F78B4", 1),
                                            ggplot2::alpha("#1F78B4", 0.5),
                                            ggplot2::alpha("#33A02C", 1),
                                            ggplot2::alpha("#33A02C", 0.5))) +
      ggplot2::scale_y_continuous(expand = c(0,0))


   if(isTRUE(switch_absolut)){

    p <- p + ggplot2::geom_bar(position="stack", stat="identity")

    plotly::ggplotly(p)

   }else{

     p <- p + ggplot2::geom_bar(position="fill", stat="identity") +
                ggplot2::scale_y_continuous(labels = scales::percent_format())

     plotly::ggplotly(p)

   }


  }else{

    df <- df %>% dplyr::filter(nur_lehramt != "Ja")

    df <- df %>% dplyr::filter(hochschulform == "insgesamt")

    p <- ggplot2::ggplot(df, ggplot2::aes(fill=fachbereich, y=wert, x=anzeige_geschlecht)) +
      ggplot2::labs(caption = "Quelle:", title = paste0("Anteile an MINT und allen anderen Studienfächer")) +
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
      ggplot2::scale_y_continuous(expand = c(0,0))

    if(isTRUE(switch_absolut)){

      p <- p + ggplot2::geom_bar(position="stack", stat="identity")
      plotly::ggplotly(p)

    }else{

     p <- p + ggplot2::geom_bar(position="fill", stat="identity") +
        ggplot2::scale_y_continuous(labels = scales::percent_format())
      plotly::ggplotly(p)

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

data_einstieg <- function(df,r) {

  timerange <- r$date_studierende_einstieg

  status_studierende <- r$indikator_studierende_einstieg

  geschlecht <- r$geschlecht_studierende_einstieg

  lehramt_enthalten <- r$nurLehramt_studierende_einstieg

  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(region == "Deutschland")


  df <- calc_anteil_MINT(df)


  help_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "gesamt") %>%
    dplyr::group_by(jahr, fachbereich)

  help_weiblich <- df %>% dplyr::filter(anzeige_geschlecht == "frauen") %>%
    dplyr::group_by(jahr, fachbereich)

  wert_männlich <- help_gesamt$wert - help_weiblich$wert

  help_männlich <- help_weiblich

  help_männlich$wert <- wert_männlich
  help_männlich$anzeige_geschlecht <- "männer"

  df <- rbind(df, help_männlich)

  df <- df[with(df, order(fachbereich, jahr, decreasing = TRUE)), ]

  df <- df %>% dplyr::filter(anzeige_geschlecht %in% geschlecht)

  if(isTRUE(lehramt_enthalten)){

    values <- df %>%
      dplyr::group_by(jahr, fachbereich, anzeige_geschlecht) %>%
      dplyr::mutate(wert = wert - dplyr::lead(wert, n = 2)) %>% dplyr::select(wert) %>% na.omit()

    toDelete <- seq(2, nrow(values), 2)
    values <- values[-toDelete ,]

    df[df$hochschulform == "insgesamt", "wert"] <- values$wert

    df <- df[!(df$nur_lehramt == "Nein" & df$hochschulform == "Uni"), ]

    df <- df[!(df$nur_lehramt == "Nein" & df$hochschulform == "FH"), ]

    df[df$nur_lehramt == "Ja", "fachbereich"] <- interaction(df[df$nur_lehramt == "Ja", "fachbereich"][[1]], "_lehramt", sep = "")

    df$hochschulform <- NULL

    return(df)


  }else{

    df <- df %>% dplyr::filter(nur_lehramt != "Ja")

    df <- df %>% dplyr::filter(hochschulform == "insgesamt")

    df$hochschulform <- NULL

    return(df)

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

studienzahl_waffle <- function(df,r) {

  status_studierende <- r$indikator_studierende

  timerange <- r$date_studierende

  lehramt <- r$nurLehramt_studierende

  geschlecht <- r$geschlecht_studierende

  hochschulform_select_1 <- r$hochschulform_studierende_1

  hochschulform_select_2 <- r$hochschulform_studierende_2

  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(indikator == status_studierende)

  if(lehramt == "Nein"){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  }

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- calc_anteil_MINT(df)


    values <- df %>%
      dplyr::group_by(jahr, fachbereich) %>%
      dplyr::mutate(wert = wert - dplyr::lead(wert)) %>% dplyr::select(wert) %>% na.omit()

    df[df$anzeige_geschlecht == "gesamt", "wert"] <- values$wert

    df[df$anzeige_geschlecht == "gesamt", "anzeige_geschlecht"] <- "männer"


    df <- df %>% dplyr::group_by(fachbereich) %>%
      dplyr::mutate(props = sum(wert))


      df <- df %>% dplyr::group_by(anzeige_geschlecht, fachbereich) %>%
        dplyr::summarize(proportion = wert/props)

      df$proportion <- df$proportion * 100

      df <- df %>% dplyr::group_by(anzeige_geschlecht, fachbereich) %>%
        dplyr::summarize(proportion = sum(proportion))

      x_mint <- setNames(round_preserve_sum(as.numeric(df[df$fachbereich == "MINT", "proportion"][[1]]),0),
                         df[df$fachbereich == "MINT", "anzeige_geschlecht"][[1]])

      x_rest <- setNames(round_preserve_sum(as.numeric(df[df$fachbereich == "Rest", "proportion"][[1]]),0),
                         df[df$fachbereich == "Rest", "anzeige_geschlecht"][[1]])

      waffle_mint <- waffle::waffle(x_mint, keep = FALSE, colors = c("#0072B2","#D55E00")) +
            ggplot2::labs(
              title = paste0("<span style='color:#0072B2;'>",x_mint[1],"% Frauen</span> vs.
        <span style='color:#D55E00;'>",x_mint[2],"% Männer</span>"),
              subtitle = "In MINT-Fächern") +
            ggplot2::theme(plot.title = ggtext::element_markdown())

      waffle_rest <- waffle::waffle(x_rest, keep = FALSE, colors = c("#0072B2","#D55E00")) +
        ggplot2::labs(
          title = paste0("<span style='color:#0072B2;'>",x_rest[1],"% Frauen</span> vs.
        <span style='color:#D55E00;'>",x_rest[2],"% Männer</span>"),
          subtitle = "Andere Studienfächer") +
        ggplot2::theme(plot.title = ggtext::element_markdown())


      plot <- ggpubr::ggarrange(waffle_rest, waffle_mint, nrow=1, common.legend = T,
                        legend="bottom")

      ggpubr::annotate_figure(plot,
                              top = ggpubr::text_grob(paste0("Anteile der Geschlechter an MINT und allen anderen Studienfächern für das Jahr", timerange),
                              face = "bold", size = 14))
}

#' make_plots_studium
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @param data
#' @param r
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

  if(lehramt == "Nein"){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  }


  df <- df %>% dplyr::filter(region == "Deutschland")


  df <- calc_anteil_MINT(df)


    values <- df %>%
      dplyr::group_by(jahr, fachbereich) %>%
      dplyr::mutate(wert = wert - dplyr::lead(wert)) %>% dplyr::select(wert) %>% na.omit()

    df[df$anzeige_geschlecht == "gesamt", "wert"] <- values$wert

    df[df$anzeige_geschlecht == "gesamt", "anzeige_geschlecht"] <- "männer"


      df <- df %>% dplyr::group_by(fachbereich, anzeige_geschlecht) %>% dplyr::summarise(mean_wert = round(mean(wert)))

      ggplot2::ggplot(df, ggplot2::aes(x=reorder(fachbereich, mean_wert), y=mean_wert, fill = anzeige_geschlecht)) +
        ggplot2::geom_bar(stat="identity", position = "dodge") +
        ggplot2::geom_text(ggplot2::aes(label=mean_wert, vjust = - 0.25),
                           position=ggplot2::position_dodge(width=0.9),
                           fontface = "bold") +
        ggplot2::theme_bw() +
        ggplot2::theme(# = ggplot2::element_text(hjust = 0.5),
                       plot.title = ggtext::element_markdown()) +
        ggplot2::xlab("Anzahl") + ggplot2::ylab("Fachrichtung") +
        ggplot2::labs(title = paste0("**Studierendenzahl in MINT und allen anderen Fächern für das Jahr ", timerange,"**"))


}



#' make_plots_studium
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @param data
#' @param r
#' @noRd

studienzahl_map <- function(df,r) {

  status_studierende <- r$indikator_studierende

  timerange <- r$date_studierende

  lehramt <- r$nurLehramt_studierende

  hochschulform_select_1 <- r$hochschulform_studierende_1

  hochschulform_select_2 <- r$hochschulform_studierende_2


  df <- df %>% dplyr::filter(jahr == timerange)

  df <- df %>% dplyr::filter(indikator == status_studierende)

  if(lehramt == "Nein"){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
  }


  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(region != "Baden-Württemberg")

  df <- calc_anteil_MINT(df)


  values <- df %>%
    dplyr::group_by(region, fachbereich) %>%
    dplyr::mutate(wert = wert - dplyr::lead(wert)) %>% dplyr::select(wert) %>% na.omit()

  df[df$anzeige_geschlecht == "gesamt", "wert"] <- values$wert

  df[df$anzeige_geschlecht == "gesamt", "anzeige_geschlecht"] <- "männer"

  df <- df %>% dplyr::group_by(region, fachbereich) %>%
    dplyr::mutate(props = sum(wert))



  df <- df %>% dplyr::group_by(region, anzeige_geschlecht, fachbereich) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100

  df <- tidyr::spread(df, key=anzeige_geschlecht, value=proportion)

  df <- df %>% dplyr::filter(fachbereich != "Rest")

  highcharter::hcmap(
    "countries/de/de-all",
    data = df,
    value = "frauen",
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
      text = paste0("<b>Anteil der Frauen</b> an MINT-Fächer für das Jahr ", timerange),
      margin = 20,
      align = "center",
      style = list(color = "black", useHTML = TRUE)
    ) %>%
    highcharter::hc_caption(
      text = "Für die Bundesländer Bayern und Baden-Württemberg sind leider keine Daten über den Anteil von Frauen
      an MINT-Fächern verfügbar."
    )



}



#' make_plots_studium
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @param data
#' @param r
#' @noRd

studienzahl_verlauf <- function(df,r) {

  status_studierende <- r$indikator_studierende_verlauf

  timerange <- r$date_studierende_verlauf

  lehramt <- r$nurLehramt_studierende_verlauf

  states <- r$states_studierende_verlauf

  topic <- r$topic_studierende_verlauf

  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == status_studierende)

  if(lehramt == "Nein"){

    df <- df %>% dplyr::filter(nur_lehramt == "Nein")

    df <- df %>% dplyr::filter(hochschulform == "insgesamt")

  } else {

    df <- df %>% dplyr::filter(nur_lehramt == "Ja")

    df <- df %>% dplyr::filter(hochschulform == "Uni")
  }


  df <- df %>% dplyr::filter(region != "Deutschland")

  df <- df %>% dplyr::filter(region != "Bayern")

  df <- df %>% dplyr::filter(region != "Baden-Württemberg")

  df <- calc_anteil_MINT(df)

  df <- df[with(df, order(region, fachbereich, jahr, decreasing = TRUE)), ]

  values <- df %>%
    dplyr::group_by(jahr, fachbereich, region) %>%
    dplyr::mutate(wert = dplyr::lead(wert)/wert) %>% dplyr::select(wert) %>% na.omit()

  values <- values %>% dplyr::filter(region %in% states)

  values <- values %>% dplyr::filter(fachbereich == topic)

  values <- values[with(values, order(region, jahr, decreasing = FALSE)), ]

  values$wert <- values$wert * 100

    highcharter::hchart(values, 'line', highcharter::hcaes(x = jahr, y = round(wert,2), group = region)) %>%
      highcharter::hc_tooltip(pointFormat = "Bundesland: {point.region} <br> Wert: {point.y} %") %>%
      highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%")) %>%
      highcharter::hc_xAxis(title = list(text = "Jahr")) %>%
      highcharter::hc_caption(text = "Quelle: ") %>%
      highcharter::hc_title(text = paste0("Anteil von Frauen an MINT im Verlauf für ausgewählte Bundesländer"))


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
#' @param r
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

    df <- df %>% dplyr::group_by(status, frauen_manner_alle, quelle) %>%
    dplyr::summarize(wert = sum(wert))

  } else if(subject == "ingenieur"){

    df <- df %>% dplyr::filter(fachbereich_alle_mint_mathe_ing == "ingenieur")

  } else {

    df <- df %>% dplyr::filter(fachbereich_alle_mint_mathe_ing == "mathe_natwi")

  }


  if(gender_select == "Ja"){

    df <- df %>% dplyr::filter(frauen_manner_alle != "gesamt")

    plot <- ggplot2::ggplot(df, ggplot2::aes(fill=frauen_manner_alle, y=reorder(status,wert), x=wert)) +
      ggplot2::geom_bar(stat="identity", position = "dodge") +
      ggplot2::theme_bw() +
      ggplot2::xlab("Jahre") + ggplot2::ylab("akademischer Grad") +
      ggplot2::geom_text(ggplot2::aes(label=wert, hjust = "left"),
                         position=ggplot2::position_dodge(width=0.9))

    if(subject == "Gesamt"){

      plot + ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse ","(", pass_fail,")"," im Jahr ",
                                          timestamp, " im gesamten MINT Fachebreich"), fill = "Geschlecht",
                                 caption = paste0("Quelle: ", unique(df$quelle)))
    }else{

      plot + ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse ","(", pass_fail,")"," im Jahr ",
                                          timestamp, " im Fachebreich ",
                                          dictionary_title_studium_abschluss[[subject]]),
                           fill = "Geschlecht",
                           caption = paste0("Quelle: ", unique(df$quelle)))
    }

  }else {

    df <- df %>% dplyr::filter(frauen_manner_alle == "gesamt")

    plot <- ggplot2::ggplot(df, ggplot2::aes(y=reorder(status,wert), x=wert)) +
      ggplot2::geom_bar(stat="identity", position = "dodge") +
      ggplot2::theme_bw() +
      ggplot2::xlab("Jahre") + ggplot2::ylab("akademischer Grad") +
      ggplot2::geom_text(ggplot2::aes(label=wert, hjust = "left"),
                         position=ggplot2::position_dodge(width=0.9))

    if(subject == "Gesamt"){

      plot + ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse ","(", pass_fail,")"," im Jahr ",
                                          timestamp, " im gesamten MINT Fachebreich"),
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

    df <- df %>% dplyr::group_by(status, frauen_manner_alle, quelle) %>%
      dplyr::summarize(wert = sum(wert))

  } else if(subject == "ingenieur"){

    df <- df %>% dplyr::filter(fachbereich_alle_mint_mathe_ing == "ingenieur")

  } else {

    df <- df %>% dplyr::filter(fachbereich_alle_mint_mathe_ing == "mathe_natwi")

  }


  if(gender_select == "Nein"){

  df <- df %>% dplyr::filter(frauen_manner_alle == "gesamt")

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
        ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse nach Fachbereich"),
                           subtitle = "1 box = 1000 Personen",
                           caption = paste0("Quelle: ", unique(df$quelle)))
    }

  }else{

    df <- df %>% dplyr::filter(frauen_manner_alle != "gesamt")


    x_male <- setNames(as.numeric(round(df[df$frauen_manner_alle == "männlich", "wert"][[1]])),
                       df[df$frauen_manner_alle == "männlich", "status"][[1]])

    x_female <- setNames(as.numeric(round(df[df$frauen_manner_alle == "weiblich", "wert"][[1]])),
                         df[df$frauen_manner_alle == "weiblich", "status"][[1]])

  if(subject == "Gesamt"){

      waffle_male <- waffle::waffle(x_male, rows = 5, pad = 0, keep = FALSE) +
        ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse männlich")) +
        ggplot2::theme(strip.text.x = ggplot2::element_text(hjust = 0.5),
                       legend.position = "bottom")

      waffle_female <- waffle::waffle(x_female, rows = 5, pad = 7, keep = FALSE) +
        ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse weiblich"),
                      caption = paste0("Quelle: ", unique(df$quelle),
                                       ", 1 box = 1000 Personen")) +
        ggplot2::theme(strip.text.x = ggplot2::element_text(hjust = 0.5),
                       legend.position = "bottom")

      waffle::iron(waffle_male,waffle_female)

    }else{

      waffle_male <- waffle::waffle(x_male,  pad = 0, rows = 5, keep = FALSE) +
        ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse männlich")) +
        ggplot2::theme(strip.text.x = ggplot2::element_text(hjust = 0.5),
                       legend.position = "bottom")

        waffle_female <- waffle::waffle(x_female,  pad = 4, rows = 5, keep = FALSE) +
          ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse weiblich"),
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

  if(subject == "Gesamt"){

    df <- df %>% dplyr::group_by(status, frauen_manner_alle, jahr, quelle) %>%
      dplyr::summarize(wert = sum(wert))

  } else if(subject == "ingenieur"){

    df <- df %>% dplyr::filter(fachbereich_alle_mint_mathe_ing == "ingenieur")

  } else {

    df <- df %>% dplyr::filter(fachbereich_alle_mint_mathe_ing == "mathe_natwi")

  }


  if(gender_select == "Ja"){

    df <- df %>% dplyr::filter(frauen_manner_alle != "gesamt")

    plot <- ggplot2::ggplot(df, ggplot2::aes(fill=status, y=wert, x=jahr)) +
      ggplot2::facet_wrap(~frauen_manner_alle) +
      ggplot2::geom_bar(position="stack", stat="identity") +
      ggplot2::theme_bw() +
      ggplot2::xlab("Jahre") + ggplot2::ylab("Anzahl Abschlüsse")

    if(subject == "Gesamt"){

      plot +  ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse ","(", pass_fail,") /n",
                                           "für die Jahre ", date_range[1], " bis ", date_range[2],
                                           " im gesamten MINT Fachebreich"),fill = "Indikator",
                                 caption = paste0("Quelle: ", unique(df$quelle)))
    }else{

      plot + ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse ","(", pass_fail,")",
                                          " für die Jahre ", date_range[1], " bis ", date_range[2],
                                          " im Fachebreich ",
                                          dictionary_title_studium_abschluss[[subject]]),
                           fill = "Indikator",
                           caption = paste0("Quelle: ", unique(df$quelle)))
    }

  }else {


    df <- df %>% dplyr::filter(frauen_manner_alle == "gesamt")

    plot <- ggplot2::ggplot(df, ggplot2::aes(fill=status, y=wert, x=jahr)) +
      ggplot2::geom_bar(position="stack", stat="identity") +
      ggplot2::labs(title = paste0("Anzahl der Abschlüsse \n ",
                                   "für die Jahre ", date_range[1], " bis ", date_range[2]),
                    caption = paste0("Quelle: ", unique(df$quelle))) + ggplot2::theme_bw() +
      ggplot2::xlab("Jahre") + ggplot2::ylab("Anzahl Abschlüsse")

    if(subject == "Gesamt"){

      plot +  ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse ","(", pass_fail,") /n",
                                           " für die Jahre ", date_range[1], " bis ", date_range[2],
                                           " im gesamten MINT Fachebreich"),fill = "Indikator",
                            caption = paste0("Quelle: ", unique(df$quelle)))
    }else{

      plot + ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse ","(", pass_fail,")",
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

  if(subject == "Gesamt"){

    df <- df %>% dplyr::group_by(status, frauen_manner_alle, jahr, quelle) %>%
      dplyr::summarize(wert = sum(wert))

  } else if(subject == "ingenieur"){

    df <- df %>% dplyr::filter(fachbereich_alle_mint_mathe_ing == "ingenieur")

  } else {

    df <- df %>% dplyr::filter(fachbereich_alle_mint_mathe_ing == "mathe_natwi")

  }

  df <- df %>% dplyr::group_by(status, frauen_manner_alle, quelle) %>%
    dplyr::filter((jahr == min(jahr)) | (jahr == max(jahr)))

  df <- df %>% dplyr::group_by(status, frauen_manner_alle, quelle) %>%
    dplyr::summarize(aenderung = (dplyr::lead(wert)/wert -1) *100) %>% na.omit()

  df$comb <- stringr::str_c(df$status, " ", df$frauen_manner_alle)

  if(gender_select == "Ja"){

    df <- df %>% dplyr::filter(frauen_manner_alle != "gesamt")

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

    if(subject == "Gesamt"){

      plot +  ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse ","(", pass_fail,") /n",
                                           " für die Jahre ", date_range[1], " bis ", date_range[2],
                                           " im gesamten MINT Fachebreich"),
                            caption = paste0("Quelle: ", unique(df$quelle)))
    }else{

      plot + ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse ","(", pass_fail,")",
                                          " für die Jahre ", date_range[1], " bis ", date_range[2],
                                          " im Fachebreich ",
                                          dictionary_title_studium_abschluss[[subject]]),
                           caption = paste0("Quelle: ", unique(df$quelle)))
    }


  }else {


    df <- df %>% dplyr::filter(frauen_manner_alle == "gesamt")

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

    if(subject == "Gesamt"){

      plot +  ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse ","(", pass_fail,") /n",
                                           " für die Jahre ", date_range[1], " bis ", date_range[2],
                                           " im gesamten MINT Fachebreich"),
                            caption = paste0("Quelle: ", unique(df$quelle)))
    }else{

      plot + ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse ","(", pass_fail,")",
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

  df <- df %>% dplyr::filter(frauen_manner_alle != "gesamt")

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

  df_abschluss <- df_abschluss %>% dplyr::group_by(prüfungsstatus, status,frauen_manner_alle,
                                                     fachbereich_alle_mint_mathe_ing) %>%
    dplyr::summarize(proportion = wert/props)


  col_order <- c("frauen_manner_alle", "fachbereich_alle_mint_mathe_ing", "status",
                 "prüfungsstatus", "proportion")
  df_abschluss <- df_abschluss[, col_order]

  df_abschluss$label <- stringr::str_c("Abschlussprüfungen: ",
                                       df_abschluss$fachbereich_alle_mint_mathe_ing,
                                         " (", df_abschluss$status, ", ", df_abschluss$prüfungsstatus, ")")

  df_abschluss <- df_abschluss[, c("label", "proportion", "frauen_manner_alle")]

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

  df_studienzahl <- df_studienzahl %>% dplyr::group_by(prüfungsstatus, status,frauen_manner_alle,
                                                       fachbereich_alle_mint_mathe_ing) %>%
    dplyr::summarize(proportion = wert/props)


  col_order <- c("frauen_manner_alle", "fachbereich_alle_mint_mathe_ing", "status",
                 "prüfungsstatus", "proportion")
  df_studienzahl <- df_studienzahl[, col_order]

  df_studienzahl$label <- stringr::str_c("Studierendenzahl:",
                                         df_studienzahl$fachbereich_alle_mint_mathe_ing,
                                       " (", df_studienzahl$status, ")")

  df_studienzahl <- df_studienzahl[, c("label", "proportion", "frauen_manner_alle")]

################################# Habilitation #################################
  # Habilitation reactives
  subject_habilitation <- r_habil$ing_natwi_compare_1

  # Habilitation dataset
  df_habil <- filter_indikator(df, subject_habilitation, "Habilitation")

  df_habil <- df_habil %>% dplyr::group_by(fachbereich_alle_mint_mathe_ing) %>%
    dplyr::mutate(props = sum(wert))

  df_habil <- df_habil %>% dplyr::group_by(prüfungsstatus,frauen_manner_alle,
                                                       fachbereich_alle_mint_mathe_ing) %>%
    dplyr::summarize(proportion = wert/props)

  col_order <- c("frauen_manner_alle", "fachbereich_alle_mint_mathe_ing",
                 "prüfungsstatus", "proportion")
  df_habil <- df_habil[, col_order]

  df_habil$label <- stringr::str_c("Habilitationszahl:",
                                   df_habil$fachbereich_alle_mint_mathe_ing)

  df_habil <- df_habil[, c("label", "proportion", "frauen_manner_alle")]

################################################################################


  df <- rbind(df_abschluss,df_studienzahl,df_habil)

  Males <- df %>%
    dplyr::filter(frauen_manner_alle == "männlich")
  Females <- df %>%
    dplyr::filter(frauen_manner_alle == "weiblich")


  ggplot2::ggplot(df) +
    ggplot2::geom_segment(data = Males,
                          ggplot2::aes(x = proportion, y = reorder(label, proportion),
                                       group = label,
                     yend = Females$label, xend = Females$proportion),
                 color = "#aeb6bf",
                 size = 4.5,
                 alpha = .5) +
    ggplot2::geom_point(ggplot2::aes(x = proportion, y = label, color = frauen_manner_alle),
                        size = 4, show.legend = TRUE) +
    ggplot2::ylab(" ") +
    ggplot2::xlab("prozentualer Anteil") +
    ggplot2::xlim(0,1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste0("Verhältnis von Frauen und Männern im akademischen Bereich für den Bereich MINT
                                 für das Jahr ", timestamp),
                  caption = paste0("Quelle: ", quelle),
                  color='Geschlecht')


}





