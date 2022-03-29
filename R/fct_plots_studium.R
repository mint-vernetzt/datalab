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
      ggplot2::ylab("Anzahl Student:innen") + ggplot2::xlab("Jahre")

  }else {


    df <- df %>% dplyr::filter(frauen_manner_alle == "gesamt")

    ggplot2::ggplot(df, ggplot2::aes(fill=frauen_manner_alle, y=wert, x=jahr)) +
      ggplot2::facet_wrap(~fachbereich_alle_mint_mathe_ing) +
      ggplot2::geom_bar(position="stack", stat="identity") +
      ggplot2::labs(title = paste0("Gesamtzahl der Studenten \n ",
                                   dictionary_title_studium_studentenzahl[[indicator]],
                                   " für die Jahre \n ", date_range[1], " bis ", date_range[2]),
                    caption = paste0("Quelle: ", unique(df$quelle))) + ggplot2::theme_bw() +
      ggplot2::ylab("Anzahl Student:innen") + ggplot2::ylab("Jahre")
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

  studienfach <- r$fach_waffle

  timestamp <- r$date_waffle

  indicator <- r$indikator_waffle

  df <- filter_data_studienanzahl(data)

  df <- df %>% dplyr::filter(jahr == timestamp)

  df <- df %>% dplyr::filter(status == indicator)

  df <- df %>% dplyr::filter(frauen_manner_alle != "gesamt")



  if(studienfach == "Nein"){

    df <- df %>% dplyr::group_by(frauen_manner_alle) %>%
      dplyr::mutate(props = sum(wert))

    df <- df[!duplicated(df$props), ]

    df <- df %>% dplyr::group_by(frauen_manner_alle) %>%
      dplyr::summarize(proportion = wert/props)

    df$proportion <- df$proportion * 100

    ggplot2::ggplot(df, ggplot2::aes(fill = frauen_manner_alle, values = proportion)) +
      ggplot2::expand_limits(x = c(0, 0), y = c(0, 0)) +
      ggplot2::coord_equal() +
      ggplot2::labs(fill = NULL, colour = NULL) +
      waffle::theme_enhance_waffle() +
      waffle::geom_waffle(
        #n_rows = 5,
        flip = FALSE,
        color = "white",
        size = 0.33
      ) +
      ggplot2::theme_void() +  ggplot2::theme(strip.text.x = ggplot2::element_text(hjust = 0.5),
                                              legend.position = "bottom") +
      ggplot2::labs(title = paste0("Anteil der Studenten \n" ,
                                   dictionary_title_studium_studentenzahl[[indicator]],
                                   " für das Jahr ", timestamp),
                    subtitle = "1 box = 1%")

  }else{

    df <- df %>% dplyr::group_by(fachbereich_alle_mint_mathe_ing) %>%
      dplyr::mutate(props = sum(wert))

    df <- df %>% dplyr::group_by(frauen_manner_alle, fachbereich_alle_mint_mathe_ing) %>%
      dplyr::summarize(proportion = wert/props)

    df$proportion <- df$proportion * 100

    ggplot2::ggplot(df, ggplot2::aes(fill = frauen_manner_alle, values = proportion)) +
      ggplot2::expand_limits(x = c(0, 0), y = c(0, 0)) +
      ggplot2::coord_equal() +
      ggplot2::labs(fill = NULL, colour = NULL) +
      waffle::theme_enhance_waffle() +
      waffle::geom_waffle(
       # n_rows = 5,
        flip = FALSE,
        color = "white",
        size = 0.33
      ) +
      ggplot2::theme_void() +  ggplot2::theme(strip.text.x = ggplot2::element_text(hjust = 0.5),
                                              legend.position = "bottom")+
      ggplot2::labs(title = paste0("Anteil der Studenten \n" ,
                                   dictionary_title_studium_studentenzahl[[indicator]],
                                   " für das Jahr ", timestamp),
                    subtitle = "1 box = 1%") +
      ggplot2::facet_grid(~fachbereich_alle_mint_mathe_ing)

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

  quelle <- unique(df$quelle)

  df <- df %>% dplyr::filter(jahr >= date_range[1] & jahr <= date_range[2])

  if(gender_select == "Nein"){

    df <- df %>% dplyr::filter(frauen_manner_alle == "gesamt")

    ggplot2::ggplot(df, ggplot2::aes(y=wert, x=jahr, color = frauen_manner_alle)) +
      ggplot2::geom_line() + ggplot2::theme_bw() +
      ggplot2::ylab("Anzahl Studenten:innen") + ggplot2::ylab("Jahre") +
      ggplot2::labs(title = paste0("Anzahl der Studenten:innen für die Jahre von",
                    date_range[1], " bis ", date_range[2]),
                    caption = paste0("Quelle: ", quelle))

  }else{

    df <- df %>% dplyr::filter(frauen_manner_alle != "gesamt")

    ggplot2::ggplot(df, ggplot2::aes(y=wert, x=jahr, color = frauen_manner_alle)) +
      ggplot2::geom_line() + ggplot2::theme_bw() +
      ggplot2::ylab("Anzahl Studenten:innen") + ggplot2::xlab("Jahre") +
      ggplot2::labs(color = "Geschlecht",
                    title = paste0("Anzahl der Studenten:innen für die Jahre von",
                    date_range[1], " bis ", date_range[2]),
                    caption = paste0("Quelle: ", quelle))


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





