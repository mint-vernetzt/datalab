#' @description A function to calculate the share of MINT
#' @param data The dataframe "Zentrale_Datensatz.xlsx" needs to be used for this function
#' @return The return value is a dataframe
#'
#' @noRd
share_MINT <- function(df){



  # calculate the share of MINT for "Hochschule" and "Studienanfänger/Studierende"
    df_sub <- df %>% dplyr::filter(indikator == "Studienanfänger" | indikator == "Studierende")
    df <- df[!(df$indikator == "Studienanfänger" | df$indikator == "Studierende"),]

    ## call function
    df_sub <- calc_share_MINT(df_sub)

    df <- rbind(df, df_sub)


  # calculate the share of MINT for "Hochschule" and "Habilitationen"
    df_sub <- df %>% dplyr::filter(indikator == "Habilitationen")

    values <- df_sub %>%
      dplyr::filter(fachbereich == "Mathe/NaWi: Alle" | fachbereich == "Ingenieurwissenschaften: Alle") %>%
      dplyr::group_by(anzeige_geschlecht, jahr, bereich, indikator, region) %>%
      dplyr::summarise(
        wert = sum(wert))

    values$fachbereich <- "MINT"

    df_sub <- df_sub %>% dplyr::filter(fachbereich == "Alle")

    values <- values[, colnames(df_sub)]

    df_sub <- rbind(df_sub, values)

    df_sub[(df_sub$fachbereich == "Alle" &
            df_sub$anzeige_geschlecht == "Gesamt"), "wert"] <- df_sub[(df_sub$fachbereich == "Alle" & df_sub$anzeige_geschlecht == "Gesamt"), "wert"] -
      df_sub[(df_sub$fachbereich == "MINT" & df_sub$anzeige_geschlecht == "Gesamt"), "wert"]

    df_sub[(df_sub$fachbereich == "Alle" &
              df_sub$anzeige_geschlecht == "Frauen"), "wert"] <- df_sub[(df_sub$fachbereich == "Alle" & df_sub$anzeige_geschlecht == "Frauen"), "wert"] -
      df_sub[(df_sub$fachbereich == "MINT" & df_sub$anzeige_geschlecht == "Frauen"), "wert"]

    df_sub[(df_sub$fachbereich == "Alle" &
              df_sub$anzeige_geschlecht == "Männer"), "wert"] <- df_sub[(df_sub$fachbereich == "Alle" & df_sub$anzeige_geschlecht == "Männer"), "wert"] -
      df_sub[(df_sub$fachbereich == "MINT" & df_sub$anzeige_geschlecht == "Männer"), "wert"]

    df_sub[df_sub$fachbereich == "Alle", "fachbereich"] <- "andere Fächer (Habilitationen)"

    df <- df %>% dplyr::filter(indikator != "Habilitationen")

    df <- rbind(df, df_sub)


    # calculate the share of MINT for "Hochschule" and "Promotionen (angestrebt)"
    df_sub <- df %>% dplyr::filter(indikator == "Promotionen (angestrebt)")

    values <- df_sub %>%
      dplyr::filter(fachbereich == "Ingenieurwissenschaften" | fachbereich == "Mathematik, Naturwissenschaften") %>%
      dplyr::group_by(anzeige_geschlecht, jahr, bereich, indikator, region) %>%
      dplyr::summarise(
        wert = sum(wert))

    values$fachbereich <- "MINT"

    df_sub <- df_sub %>% dplyr::filter(fachbereich == "alle")

    values <- values[, colnames(df_sub)]

    df_sub <- rbind(df_sub, values)

    df_sub[(df_sub$fachbereich == "alle" &
              df_sub$anzeige_geschlecht == "Gesamt"), "wert"] <- df_sub[(df_sub$fachbereich == "alle" & df_sub$anzeige_geschlecht == "Gesamt"), "wert"] -
      df_sub[(df_sub$fachbereich == "MINT" & df_sub$anzeige_geschlecht == "Gesamt"), "wert"]

    df_sub[(df_sub$fachbereich == "alle" &
              df_sub$anzeige_geschlecht == "Frauen"), "wert"] <- df_sub[(df_sub$fachbereich == "alle" & df_sub$anzeige_geschlecht == "Frauen"), "wert"] -
      df_sub[(df_sub$fachbereich == "MINT" & df_sub$anzeige_geschlecht == "Frauen"), "wert"]

    df_sub[(df_sub$fachbereich == "alle" &
              df_sub$anzeige_geschlecht == "Männer"), "wert"] <- df_sub[(df_sub$fachbereich == "alle" & df_sub$anzeige_geschlecht == "Männer"), "wert"] -
      df_sub[(df_sub$fachbereich == "MINT" & df_sub$anzeige_geschlecht == "Männer"), "wert"]

    df_sub[df_sub$fachbereich == "alle", "fachbereich"] <- "andere Fächer (Promotionen)"

    df <- df %>% dplyr::filter(indikator != "Promotionen (angestrebt)")

    df <- rbind(df, df_sub)

    # calculate the share of MINT for "Schule" and "Leistungskurse"

    subjects <- c("Mathematik", "Informatik", "Naturwissenschaften",  "Biologie", "Chemie",
                  "Physik", "andere naturwiss.-technische Fächer",  "Erdkunde", "Alle Fächer")

    df_sub <- df %>% dplyr::filter(fachbereich %in% subjects)

    df_sub$fachbereich <- ifelse(df_sub$fachbereich != "Alle Fächer", "MINT", "andere Fächer")

    df_sub <- df_sub %>% dplyr::group_by(fachbereich, anzeige_geschlecht, jahr, region, indikator, bereich) %>%
      dplyr::summarize(wert = sum(wert))


    df_sub[df_sub$fachbereich == "andere Fächer", "wert"] <- df_sub[df_sub$fachbereich == "andere Fächer", "wert"] -
      df_sub[df_sub$fachbereich == "MINT", "wert"]

    df <- df %>% dplyr::filter(bereich != "Schule")

    df_sub <- df_sub[, colnames(df)]

    df <- rbind(df, df_sub)

    df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"

  return(df)
}


#' @description A function to calculate the share of females for MINT and Rest
#' @param data The dataframe "Zentrale_Datensatz.xlsx" needs to be used for this function
#' @return The return value is a dataframe
#'
#' @noRd
share_female <- function(df){

  # calculate the share of females for "Hochschule" and "Habilitationen"
  df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")

  df <- df %>% dplyr::group_by(indikator, fachbereich, jahr) %>%
    dplyr::summarise(proportion = wert[anzeige_geschlecht=="Frauen"]/
                    wert[anzeige_geschlecht=="Gesamt"])

  df$proportion <- df$proportion * 100


  return(df)
}


#' @description A function to calculate the share of females for MINT and Rest
#' @param data The dataframe "Zentrale_Datensatz.xlsx" needs to be used for this function
#' @return The return value is a dataframe
#'
#' @noRd
shares_gender <- function(df, indikator, gender){

  if ((indikator != "Leistungskurse") | (indikator != "Habilitationen") | (indikator != "Promotionen (angestrebt)")) {

    # calculate the share of males
    help_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt") %>%
      dplyr::group_by(jahr, fachbereich)

    help_weiblich <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen") %>%
      dplyr::group_by(jahr, fachbereich)

    wert_männlich <- help_gesamt$wert - help_weiblich$wert

    help_männlich <- help_weiblich

    help_männlich$wert <- wert_männlich

    help_männlich$anzeige_geschlecht <- "Männer"

    df <- rbind(df, help_männlich)

    df <- df[with(df, order(anzeige_geschlecht, jahr, decreasing = TRUE)), ]

    # filter gender
    df <- df %>% dplyr::filter(anzeige_geschlecht %in% geschlecht)

    values <- df %>% dplyr::group_by(anzeige_geschlecht, jahr) %>%
      dplyr::summarize(wert = sum(wert))

    values <- values[with(values, order(anzeige_geschlecht, jahr, decreasing = TRUE)), ]

    df$Anteil <- NA

    if ((indikator == "Studierende" | indikator == "Studienanfänger")) {

      help_string <- "andere Studiengänge"

    } else {

      help_string <- "andere Berufszweige"

    }

    df[df$fachbereich == help_string, "Anteil"] <- round((df[df$fachbereich == help_string, "wert"]/values$wert)*100)

    df[df$fachbereich == "MINT", "Anteil"] <- round((df[df$fachbereich == "MINT", "wert"]/values$wert)*100)

    df$Anteil <- paste0(df$Anteil,"%")

  } else {


    values <- df %>% dplyr::group_by(anzeige_geschlecht, jahr) %>%
      dplyr::summarize(wert = sum(wert))


    if (indikator == "Leistungskurse") {

      help_string <- "andere Fächer"

    } else if (indikator == "Promotionen (angestrebt)") {

      help_string <- "andere Fächer (Promotionen)"

    } else {

      help_string <- "andere Fächer (Habilitationen)"

    }


    tooltip_1 <- round((df[df$fachbereich == help_string, "wert"]/values$wert)*100)

    tooltip_2 <- round((df[df$fachbereich == "MINT", "wert"]/values$wert)*100)

    tooltip <- c(tooltip_1[[1]], tooltip_2[[1]])

    df$Anteil <- paste0(tooltip,"%")


  }


  return(df)
}

