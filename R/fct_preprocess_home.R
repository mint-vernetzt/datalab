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
    df_sub <- df %>% dplyr::filter(indikator == "Leistungskurse")


    df_sub[(df_sub$anzeige_geschlecht == "Gesamt" & df_sub$indikator == "Leistungskurse"), "wert"] <-  df_sub %>%
      dplyr::filter(indikator == "Leistungskurse") %>%
      dplyr::group_by(indikator, jahr) %>%
      dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"] +
                         wert[anzeige_geschlecht == "Männer"]) %>% dplyr::pull(wert)


    df_sub[(df_sub$anzeige_geschlecht == "Gesamt" & df_sub$indikator == "Grundkurse"), "wert"] <-  df_sub %>%
      dplyr::filter(indikator == "Grundkurse") %>%
      dplyr::group_by(indikator, jahr) %>%
      dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"] +
                         wert[anzeige_geschlecht == "Männer"]) %>% dplyr::pull(wert)



    df_sub <- share_mint_kurse(df_sub)



    df <- df %>% dplyr::filter(bereich != "Schule")

    df_sub <- df_sub[, colnames(df)]

    df <- rbind(df, df_sub)

    #rename
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

