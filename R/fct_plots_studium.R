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

#change df to data
studienzahl_plot <- function(data,r){

  gender_select <- r$geschlecht

  date_range <- r$date

  indicator <- r$indikator

  df <- filter_data_studienanzahl(data)

  df <- df %>% dplyr::filter(jahr >= date_range[1] & jahr <= date_range[2])

  df <- df %>% dplyr::filter(status == indicator)

  if(gender_select == "Ja"){

    df <- df %>% dplyr::filter(frauen_manner_alle != "gesamt")

    ggplot2::ggplot(df, ggplot2::aes(fill=frauen_manner_alle, y=wert, x=jahr)) +
      ggplot2::facet_wrap(~fachbereich_alle_mint_mathe_ing) +
      ggplot2::geom_bar(position="stack", stat="identity") +
      ggplot2::labs(title = paste0("Gesamtzahl der Studenten \n ",
                                   dictionary_title_studium_studentenzahl[[indicator]],
                                   " für die Jahre ", date_range[1], " bis ", date_range[2]),
                    caption = paste0("Quelle: ", unique(df$quelle))) + ggplot2::theme_bw() +
      ggplot2::xlab("Wert") + ggplot2::ylab("Jahr")

  }else {


    df <- df %>% dplyr::filter(frauen_manner_alle == "gesamt")

    ggplot2::ggplot(df, ggplot2::aes(fill=frauen_manner_alle, y=wert, x=jahr)) +
      ggplot2::facet_wrap(~fachbereich_alle_mint_mathe_ing) +
      ggplot2::geom_bar(position="stack", stat="identity") +
      ggplot2::labs(title = paste0("Gesamtzahl der Studenten \n ",
                                   dictionary_title_studium_studentenzahl[[indicator]],
                                   " für die Jahre \n ", date_range[1], " bis ", date_range[2]),
                    caption = paste0("Quelle: ", unique(df$quelle))) + ggplot2::theme_bw() +
      ggplot2::xlab("Wert") + ggplot2::ylab("Jahr")
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

studienzahl_waffle <- function(data,r) {

  gender_select <- r$geschlecht_waffle

  timestamp <- r$date_waffle

  indicator <- r$indikator_waffle

  df <- filter_data_studienanzahl(data)

  df <- df %>% dplyr::filter(jahr == timestamp)

  df <- df %>% dplyr::filter(status == indicator)

  df$wert <- df$wert/1000



  if(gender_select == "Nein"){

    ggplot2::ggplot(df, ggplot2::aes(fill = fachbereich_alle_mint_mathe_ing, values = wert)) +
      ggplot2::expand_limits(x = c(0, 0), y = c(0, 0)) +
      ggplot2::coord_equal() +
      ggplot2::labs(fill = NULL, colour = NULL) +
      waffle::theme_enhance_waffle() +
      waffle::geom_waffle(
        n_rows = 5,
        flip = FALSE,
        color = "white",
        size = 0.33
      ) +
      ggplot2::theme_void() +  ggplot2::theme(strip.text.x = ggplot2::element_text(hjust = 0.5),
                                              legend.position = "bottom") +
      ggplot2::labs(title = paste0("Gesamtzahl der Studenten \n" ,
                                   dictionary_title_studium_studentenzahl[[indicator]],
                                   " für das Jahr ", timestamp),
                    subtitle = "1 box = 1000 Personen")

  }else{
    df <- df %>% dplyr::filter(frauen_manner_alle != "gesamt")

    ggplot2::ggplot(df, ggplot2::aes(fill = fachbereich_alle_mint_mathe_ing, values = wert)) +
      ggplot2::expand_limits(x = c(0, 0), y = c(0, 0)) +
      ggplot2::coord_equal() +
      ggplot2::labs(fill = NULL, colour = NULL) +
      waffle::theme_enhance_waffle() +
      waffle::geom_waffle(
        n_rows = 5,
        flip = FALSE,
        color = "white",
        size = 0.33
      ) +
      ggplot2::theme_void() +  ggplot2::theme(strip.text.x = ggplot2::element_text(hjust = 0.5),
                                              legend.position = "bottom")+
      ggplot2::labs(title = paste0("Gesamtzahl der Studenten \n" ,
                                   dictionary_title_studium_studentenzahl[[indicator]],
                                   " für das Jahr ", timestamp),
                    subtitle = "1 box = 1000 Personen") +
      ggplot2::facet_grid(~frauen_manner_alle)

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

studienzahl_line <- function(data, r){

  gender_select <- r$geschlecht

  date_range <- r$date

  df <- filter_data_studienanzahl(data)

  df <- df %>% dplyr::filter(jahr >= date_range[1] & jahr <= date_range[2])

  if(gender_select == "Nein"){

    df <- df %>% dplyr::filter(frauen_manner_alle == "gesamt")

    ggplot2::ggplot(df, ggplot2::aes(y=wert, x=jahr, color = frauen_manner_alle)) +
      ggplot2::geom_line() + ggplot2::theme_bw() +
      ggplot2::xlab("Wert") + ggplot2::ylab("Jahr")

  }else{

    df <- df %>% dplyr::filter(frauen_manner_alle != "gesamt")

    ggplot2::ggplot(df, ggplot2::aes(y=wert, x=jahr, color = frauen_manner_alle)) +
      ggplot2::geom_line() + ggplot2::theme_bw() +
      ggplot2::xlab("Wert") + ggplot2::ylab("Jahr")

  }

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
      ggplot2::xlab("Wert") + ggplot2::ylab("Jahr") +
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

  }else {

    df <- df %>% dplyr::filter(frauen_manner_alle == "gesamt")

    plot <- ggplot2::ggplot(df, ggplot2::aes(y=reorder(status,wert), x=wert)) +
      ggplot2::geom_bar(stat="identity", position = "dodge") +
      ggplot2::theme_bw() +
      ggplot2::xlab("Wert") + ggplot2::ylab("Jahr") +
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

   plot <-  ggplot2::ggplot(df, ggplot2::aes(fill = status, values = wert)) +
      ggplot2::expand_limits(x = c(0, 0), y = c(0, 0)) +
      ggplot2::coord_equal() +
      ggplot2::labs(fill = NULL, colour = NULL) +
      waffle::theme_enhance_waffle() +
      waffle::geom_waffle(
        n_rows = 5,
        flip = FALSE,
        color = "white",
        size = 0.33
      ) +
      ggplot2::theme_void() +  ggplot2::theme(strip.text.x = ggplot2::element_text(hjust = 0.5),
                                              legend.position = "bottom")

    if(subject == "Gesamt"){

      plot + ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse ","(", pass_fail,")"," im Jahr ",
                                          timestamp, " im gesamten MINT Fachebreich"),
                           subtitle = "1 box = 1000 Personen",
                           caption = paste0("Quelle: ", unique(df$quelle)))
    }else{

      plot + ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse ","(", pass_fail,")"," im Jahr ",
                                          timestamp, " im Fachebreich ",
                                          dictionary_title_studium_abschluss[[subject]]),
                           subtitle = "1 box = 1000 Personen",
                           caption = paste0("Quelle: ", unique(df$quelle)))
    }

  }else{

    df <- df %>% dplyr::filter(frauen_manner_alle != "gesamt")

    plot <- ggplot2::ggplot(df, ggplot2::aes(fill = status, values = wert)) +
      ggplot2::expand_limits(x = c(0, 0), y = c(0, 0)) +
      ggplot2::coord_equal() +
      ggplot2::labs(fill = NULL, colour = NULL) +
      waffle::theme_enhance_waffle() +
      waffle::geom_waffle(
        n_rows = 5,
        flip = FALSE,
        color = "white",
        size = 0.33
      ) +
      ggplot2::theme_void() +  ggplot2::theme(strip.text.x = ggplot2::element_text(hjust = 0.5),
                                              legend.position = "bottom") +
      ggplot2::facet_grid(~frauen_manner_alle)

    if(subject == "Gesamt"){

      plot + ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse ","(", pass_fail,")"," im Jahr ",
                                          timestamp, " im gesamten MINT Fachebreich"),
                           subtitle = "1 box = 1000 Personen",
                           caption = paste0("Quelle: ", unique(df$quelle)))
    }else{

      plot + ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse ","(", pass_fail,")"," im Jahr ",
                                          timestamp, " im Fachebreich ",
                                          dictionary_title_studium_abschluss[[subject]]),
                           subtitle = "1 box = 1000 Personen",
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
      ggplot2::xlab("Wert") + ggplot2::ylab("Jahr")

    if(subject == "Gesamt"){

      plot +  ggplot2::labs(title = paste0("Gesamtzahl Abschlüsse ","(", pass_fail,") /n",
                                           "für die Jahre ", date_range[1], " bis ", date_range[2],
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

    plot <- ggplot2::ggplot(df, ggplot2::aes(fill=status, y=wert, x=jahr)) +
      ggplot2::geom_bar(position="stack", stat="identity") +
      ggplot2::labs(title = paste0("Anzahl der Abschlüsse \n ",
                                   "für die Jahre ", date_range[1], " bis ", date_range[2]),
                    caption = paste0("Quelle: ", unique(df$quelle))) + ggplot2::theme_bw() +
      ggplot2::xlab("Wert") + ggplot2::ylab("Jahr")

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

comparer_plot <- function(data, r){

  timestamp <- r$date_compare

  df <- df %>% dplyr::filter(jahr == timestamp)

  df <- df %>% dplyr::filter(frauen_manner_alle != "gesamt")

################################# Abschluss ####################################

  # Abschluss reactives
  indikator_abschluss <- r$indikator_compare_1
  durchgefallen_abschluss <- r$durchgefallen_compare
  subject_abschluss <- r$ing_natwi_compare_3

  # Abschluss dataset
  df_abschluss <- df %>% subset(prüfungsstatus %in% durchgefallen_abschluss)
  df_abschluss <- df_abschluss %>% subset(status %in% indikator_abschluss)
  df_abschluss <- filter_data_compare(df_abschluss, subject_abschluss, "Abschluss")

  df_abschluss <- df_abschluss %>% dplyr::group_by(prüfungsstatus, status,
                                                   fachbereich_alle_mint_mathe_ing) %>%
    dplyr::mutate(props = sum(wert))

  df_abschluss <- df_abschluss_2 %>% dplyr::group_by(prüfungsstatus, status,frauen_manner_alle,
                                                     fachbereich_alle_mint_mathe_ing) %>%
    dplyr::summarize(proportion = wert/props)

################################# Studienzahl ##################################
  # Abschluss reactives
  indikator_studienzahl <- r$indikator_compare_2
  subject_studienzahl <- r$ing_natwi_compare_2

  # Studienzahl dataset
  df_studienzahl <- df %>% subset(status %in% indikator_studienzahl)
  df_studienzahl <- filter_data_compare(df_studienzahl, subject_studienzahl, "Studienzahl")


  df_studienzahl <- df_studienzahl %>% dplyr::group_by(prüfungsstatus, status,
                                                     fachbereich_alle_mint_mathe_ing) %>%
    dplyr::mutate(props = sum(wert))

  df_studienzahl <- df_studienzahl %>% dplyr::group_by(prüfungsstatus, status,frauen_manner_alle,
                                                       fachbereich_alle_mint_mathe_ing) %>%
    dplyr::summarize(proportion = wert/props)


################################# Habilitation #################################
  # Habilitation reactives
  subject_habilitation <- r$ing_natwi_compare_1

  # Habilitation dataset
  df_habil <- filter_data_compare(df, subject_habilitation, "Habilitation")

  df_habil <- df_habil %>% dplyr::group_by(prüfungsstatus, status,
                                                       fachbereich_alle_mint_mathe_ing) %>%
    dplyr::mutate(props = sum(wert))

  df_habil <- df_habil %>% dplyr::group_by(prüfungsstatus, status,frauen_manner_alle,
                                                       fachbereich_alle_mint_mathe_ing) %>%
    dplyr::summarize(proportion = wert/props)

################################################################################


  df <- rbind(df_studienzahl,df_studienzahl,df_habil)

  Males <- df_abschluss_2 %>%
    dplyr::filter(frauen_manner_alle == "männlich")
  Females <- df_abschluss_2 %>%
    dplyr::filter(frauen_manner_alle == "weiblich")

  ggplot2::ggplot(df_abschluss_2) +
    ggplot2::geom_segment(data = Males,
                          ggplot2::aes(x = proportion, y = status,
                     yend = Females$status, xend = Females$proportion), #use the $ operator to fetch data from our "Females" tibble
                 color = "#aeb6bf",
                 size = 4.5, #Note that I sized the segment to fit the points
                 alpha = .5) +
    ggplot2::geom_point(ggplot2::aes(x = proportion, y = status, color = frauen_manner_alle),
                        size = 4, show.legend = TRUE)

}





