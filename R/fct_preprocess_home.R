#' @description A function to calculate the share of MINT
#' @param data The dataframe "Zentrale_Datensatz.xlsx" needs to be used for this function
#' @return The return value is a dataframe
#'
#' @noRd
share_MINT <- function(df){



  # calculate the share of MINT for "Hochschule" and "Studienanfänger:innen/Studierende"
    df_sub1 <- df %>% dplyr::filter(indikator == "Studienanfänger" | indikator == "Studierende")
    df1 <- df[!(df$indikator == "Studienanfänger" | df$indikator == "Studierende"),]

    ## call function
    df_sub324 <- calc_share_MINT(df_sub1)

    df_k <- rbind(df1, df_sub324)


  # calculate the share of MINT for "Hochschule" and "Habilitationen"
    # df_sub <- df %>% dplyr::filter(indikator == "Habilitationen")
    #
    # values <- df_sub %>%
    #   dplyr::filter(fachbereich == "Mathe/NaWi: Alle" | fachbereich == "Ingenieurwissenschaften: Alle") %>%
    #   dplyr::group_by(anzeige_geschlecht, jahr, bereich, indikator, region) %>%
    #   dplyr::summarise(
    #     wert = sum(wert))
    #
    # values$fachbereich <- "MINT"
    #
    # df_sub <- df_sub %>% dplyr::filter(fachbereich == "Alle")
    #
    # values <- values[, colnames(df_sub)]
    #
    # df_sub <- rbind(df_sub, values)
    #
    # df_sub[(df_sub$fachbereich == "Alle" &
    #         df_sub$anzeige_geschlecht == "Gesamt"), "wert"] <- df_sub[(df_sub$fachbereich == "Alle" & df_sub$anzeige_geschlecht == "Gesamt"), "wert"] -
    #   df_sub[(df_sub$fachbereich == "MINT" & df_sub$anzeige_geschlecht == "Gesamt"), "wert"]
    #
    # df_sub[(df_sub$fachbereich == "Alle" &
    #           df_sub$anzeige_geschlecht == "Frauen"), "wert"] <- df_sub[(df_sub$fachbereich == "Alle" & df_sub$anzeige_geschlecht == "Frauen"), "wert"] -
    #   df_sub[(df_sub$fachbereich == "MINT" & df_sub$anzeige_geschlecht == "Frauen"), "wert"]
    #
    # df_sub[(df_sub$fachbereich == "Alle" &
    #           df_sub$anzeige_geschlecht == "Männer"), "wert"] <- df_sub[(df_sub$fachbereich == "Alle" & df_sub$anzeige_geschlecht == "Männer"), "wert"] -
    #   df_sub[(df_sub$fachbereich == "MINT" & df_sub$anzeige_geschlecht == "Männer"), "wert"]
    #
    # df_sub[df_sub$fachbereich == "Alle", "fachbereich"] <- "andere Fächer (Habilitationen)"
    #
    # df <- df %>% dplyr::filter(indikator != "Habilitationen")
    #
    # df <- rbind(df, df_sub)


    # calculate the share of MINT for "Hochschule" and "Promotionen (angestrebt)"
    # df_sub <- df %>% dplyr::filter(indikator == "Promotionen (angestrebt)")
    #
    # values <- df_sub %>%
    #   dplyr::filter(fachbereich == "Ingenieurwissenschaften" | fachbereich == "Mathematik, Naturwissenschaften") %>%
    #   dplyr::group_by(anzeige_geschlecht, jahr, bereich, indikator, region) %>%
    #   dplyr::summarise(
    #     wert = sum(wert))
    #
    # values$fachbereich <- "MINT"
    #
    # df_sub <- df_sub %>% dplyr::filter(fachbereich == "alle")
    #
    # values <- values[, colnames(df_sub)]
    #
    # df_sub <- rbind(df_sub, values)
    #
    # df_sub[(df_sub$fachbereich == "alle" &
    #           df_sub$anzeige_geschlecht == "Gesamt"), "wert"] <- df_sub[(df_sub$fachbereich == "alle" & df_sub$anzeige_geschlecht == "Gesamt"), "wert"] -
    #   df_sub[(df_sub$fachbereich == "MINT" & df_sub$anzeige_geschlecht == "Gesamt"), "wert"]
    #
    # df_sub[(df_sub$fachbereich == "alle" &
    #           df_sub$anzeige_geschlecht == "Frauen"), "wert"] <- df_sub[(df_sub$fachbereich == "alle" & df_sub$anzeige_geschlecht == "Frauen"), "wert"] -
    #   df_sub[(df_sub$fachbereich == "MINT" & df_sub$anzeige_geschlecht == "Frauen"), "wert"]
    #
    # df_sub[(df_sub$fachbereich == "alle" &
    #           df_sub$anzeige_geschlecht == "Männer"), "wert"] <- df_sub[(df_sub$fachbereich == "alle" & df_sub$anzeige_geschlecht == "Männer"), "wert"] -
    #   df_sub[(df_sub$fachbereich == "MINT" & df_sub$anzeige_geschlecht == "Männer"), "wert"]
    #
    # df_sub[df_sub$fachbereich == "alle", "fachbereich"] <- "andere Fächer (Promotionen)"
    #
    # df <- df %>% dplyr::filter(indikator != "Promotionen (angestrebt)")
    #
    # df <- rbind(df, df_sub)



    # calculate the share of MINT for "Schule" and "Leistungskurse"
    #df_2 <- df_k %>% dplyr::filter(indikator == "Leistungskurse")

#
# df_sub[(df_sub$anzeige_geschlecht == "Gesamt" & df_sub$indikator == "Leistungskurse"), "wert"] <-  df_sub %>%
#   dplyr::filter(indikator == "Leistungskurse") %>%
#   dplyr::group_by(indikator, jahr) %>%
#   dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"] +
#                      wert[anzeige_geschlecht == "Männer"]) %>% dplyr::pull(wert)
#
#
# df_sub[(df_sub$anzeige_geschlecht == "Gesamt" & df_sub$indikator == "Grundkurse"), "wert"] <-  df_sub %>%
#   dplyr::filter(indikator == "Grundkurse") %>%
#   dplyr::group_by(indikator, jahr) %>%
#   dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"] +
#                      wert[anzeige_geschlecht == "Männer"]) %>% dplyr::pull(wert)
#
#
#     df_sub <- share_mint_kurse(df_sub)

#
#     df_sub <- df %>%
#       tidyr::pivot_wider(values_from = wert, names_from = fachbereich)%>%
#       dplyr::mutate(MINT=Mathematik+Informatik+Physik+Biologie+Chemie,
#                     "andere Fächer" =`Alle Fächer`- MINT)%>%
#     tidyr::pivot_longer(c(6:20), values_to = "wert", names_to= "fachbereich")%>%
#       dplyr::filter(fachbereich=="MINT" | fachbereich == "andere Fächer")
#
#
#
#
#     df_sub <- df_sub %>% dplyr::filter(bereich != "Schule")
#
#     #df2<- df2[, colnames(df)]
#
#     df_sub<-  dplyr::bind_rows(df, df_sub)

    #Korrektur: prep for ratio
    df2 <- df_k %>% dplyr::filter(indikator == "Leistungskurse")

    df8 <- df2 %>%
      tidyr::pivot_wider(values_from = wert, names_from = fachbereich)%>%
      dplyr::mutate(MINT=Mathematik+Informatik+Physik+Biologie+Chemie,
                    "andere Fächer" =`Alle Fächer`- MINT)%>%
      tidyr::pivot_longer(c(6:20), values_to = "wert", names_to= "fachbereich")%>%
      dplyr::filter(fachbereich=="MINT" | fachbereich == "andere Fächer")


    df5 <- df_k %>% dplyr::filter(bereich != "Schule")

    #df2<- df2[, colnames(df)]


    df7 <-  dplyr::bind_rows(df5, df8)

    df7$indikator <- gsub("Studienanfänger", "Studienanfänger:innen",  df7$indikator)

    # df<- df %>%
    #   tidyr::pivot_wider(values_from = wert, names_from = anzeige_geschlecht)%>%
    #   tidyr::pivot_longer(c("Männer","Frauen", "Gesamt"),names_to = "anzeige_geschlecht", values_to= "wert")%>%
    #   dplyr::filter(fachbereich=="MINT" | fachbereich == "andere Fächer")


    # df <- df %>% dplyr::filter(bereich != "Schule")
    #
    # #df_sub <- df_sub[, colnames(df)]
    #
    # df <- rbind(df, df_sub)



    # calculate the share of MINT for "Beschäftigte" and "Auszubildende"

    #rename
    #df7[df7$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige" ## <- WRONG!

    # Korrektur: prep for Arbeitsmarkt

    df10 <- df7 %>% dplyr::filter(bereich=="Arbeitsmarkt")%>%
      tidyr::pivot_wider(names_from = fachbereich, values_from= wert)%>%
      dplyr::mutate("andere Berufszweige" = Alle-MINT)%>%
      dplyr::select(-Alle)%>%
      tidyr::pivot_longer(c("MINT", "andere Berufszweige"), names_to = "fachbereich", values_to = "wert")

    df12 <- df7 %>% dplyr::filter(bereich != "Arbeitsmarkt")

    df100 <- dplyr::bind_rows(df10, df12)


  return(df100)
}


#' @description A function to calculate the share of females for MINT and Rest
#' @param data The dataframe "Zentrale_Datensatz.xlsx" needs to be used for this function
#' @return The return value is a dataframe
#'
#' @noRd
share_female <- function(df){

  # calculate the new "Gesamt" for "Leistungskurse"
  df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Leistungskurse"), "wert"] <-  df %>%
    dplyr::filter(indikator == "Leistungskurse") %>%
    dplyr::group_by(indikator, jahr) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"] +
                       wert[anzeige_geschlecht == "Männer"]) %>% dplyr::pull(wert)

  # calculate the share of females for "Hochschule" and "Habilitationen"
  df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")

  df <- df %>% dplyr::group_by(indikator, fachbereich, jahr) %>%
    dplyr::summarise(proportion = wert[anzeige_geschlecht=="Frauen"]/
                    wert[anzeige_geschlecht=="Gesamt"])

  df$proportion <- df$proportion * 100


  return(df)
}

