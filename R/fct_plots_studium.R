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

  gender_select <- r$geschlecht_abschluss

  df <- filter_data_abschluss(data)

  df <- df %>% dplyr::filter(jahr == timestamp)

  df <- df %>% dplyr::group_by(status, frauen_manner_alle, quelle) %>% dplyr::summarize(wert = sum(wert))

  if(gender_select == "Ja"){

    df <- df %>% dplyr::filter(frauen_manner_alle != "gesamt")

    ggplot2::ggplot(df, ggplot2::aes(fill=frauen_manner_alle, y=reorder(status,wert), x=wert)) +
      ggplot2::geom_bar(stat="identity", position = "dodge") +
      ggplot2::theme_bw() +
      ggplot2::xlab("Wert") + ggplot2::ylab("Jahr") + ggplot2::xlim(0, 65000) +
      ggplot2::labs(title = paste0("Gesamtzahl der Abschlüsse im Jahr ", timestamp),
                    caption = paste0("Quelle: ", unique(df$quelle))) +
      ggplot2::geom_text(ggplot2::aes(label=wert, hjust = "left"),
                         position=ggplot2::position_dodge(width=0.9))



  }else {

    df <- df %>% dplyr::filter(frauen_manner_alle == "gesamt")

    ggplot2::ggplot(df, ggplot2::aes(y=reorder(status,wert), x=wert)) +
      ggplot2::geom_bar(stat="identity", position = "dodge") +
      ggplot2::theme_bw() +
      ggplot2::xlab("Wert") + ggplot2::ylab("Jahr") +
      ggplot2::labs(title = paste0("Gesamtzahl der Abschlüsse im Jahr ", timestamp),
                    caption = paste0("Quelle: ", unique(df$quelle)))
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

  gender_select <- r$geschlecht_abschluss

  df <- filter_data_abschluss(data)

  df <- df %>% dplyr::filter(jahr == timestamp)

  df <- df %>% dplyr::group_by(status, frauen_manner_alle) %>% dplyr::summarize(wert = sum(wert))

  df$wert <- df$wert/1000



  if(gender_select == "Nein"){

    ggplot2::ggplot(df, ggplot2::aes(fill = status, values = wert)) +
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
      ggplot2::labs(title = paste0("Gesamtzahl der Abschlüsse im Jahr ", timestamp),
                    subtitle = "1 box = 1000 Personen")

  }else{
    df <- df %>% dplyr::filter(frauen_manner_alle != "gesamt")

    ggplot2::ggplot(df, ggplot2::aes(fill = status, values = wert)) +
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
      ggplot2::labs(title = paste0("Gesamtzahl der Abschlüsse im Jahr ", timestamp),
                    subtitle = "1 box = 1000 Personen") +
      ggplot2::facet_grid(~frauen_manner_alle)


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

  gender_select <- r$geschlecht_abschluss_1

  indikators <- r$indikator_abschluss_1


  df <- filter_data_abschluss(data)

  df <- df %>% dplyr::filter(jahr >= date_range[1] & jahr <= date_range[2])

  df <- df %>% subset(status %in% indikators)

  df <- df %>% dplyr::group_by(status, frauen_manner_alle, quelle, jahr) %>% dplyr::summarize(wert = sum(wert))


  if(gender_select == "Ja"){

    df <- df %>% dplyr::filter(frauen_manner_alle != "gesamt")

    ggplot2::ggplot(df, ggplot2::aes(fill=status, y=wert, x=jahr)) +
      ggplot2::facet_wrap(~frauen_manner_alle) +
      ggplot2::geom_bar(position="stack", stat="identity") +
      ggplot2::labs(title = paste0("Anzahl der Abschlüsse \n ",
                                   "für die Jahre ", date_range[1], " bis ", date_range[2]),
                    caption = paste0("Quelle: ", unique(df$quelle))) + ggplot2::theme_bw() +
      ggplot2::xlab("Wert") + ggplot2::ylab("Jahr")

  }else {


    df <- df %>% dplyr::filter(frauen_manner_alle == "gesamt")

    ggplot2::ggplot(df, ggplot2::aes(fill=status, y=wert, x=jahr)) +
      ggplot2::geom_bar(position="stack", stat="identity") +
      ggplot2::labs(title = paste0("Anzahl der Abschlüsse \n ",
                                   "für die Jahre ", date_range[1], " bis ", date_range[2]),
                    caption = paste0("Quelle: ", unique(df$quelle))) + ggplot2::theme_bw() +
      ggplot2::xlab("Wert") + ggplot2::ylab("Jahr")
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

  gender_select <- r$geschlecht_abschluss_1

  indikators <- r$indikator_abschluss_1

  df <- filter_data_abschluss(data)

  df <- df %>% dplyr::filter(jahr >= date_range[1] & jahr <= date_range[2])

  df <- df %>% subset(status %in% indikators)

  df <- df %>% dplyr::group_by(status, frauen_manner_alle, quelle, jahr) %>% dplyr::summarize(wert = sum(wert))

  df <- df %>% dplyr::group_by(status, frauen_manner_alle, quelle) %>%
    dplyr::filter((jahr == min(jahr)) | (jahr == max(jahr)))

  df <- df %>% dplyr::group_by(status, frauen_manner_alle, quelle) %>%
    dplyr::summarize(aenderung = (dplyr::lead(wert)/wert -1) *100) %>% na.omit()

  df$comb <- stringr::str_c(df$status, " ", df$frauen_manner_alle)

  if(gender_select == "Ja"){

    df <- df %>% dplyr::filter(frauen_manner_alle != "gesamt")

    color <- ifelse(df$aenderung < 0, "pink", "lightblue")

    ggplot2::ggplot(df, ggplot2::aes(x = reorder(comb, aenderung), y = aenderung)) +
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

  }else {


    df <- df %>% dplyr::filter(frauen_manner_alle == "gesamt")

    color <- ifelse(df$aenderung < 0, "pink", "lightblue")

    ggplot2::ggplot(df, ggplot2::aes(x = reorder(comb, aenderung), y = aenderung)) +
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


  }
}
