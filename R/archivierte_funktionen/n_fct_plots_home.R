# funktionen die archiviert wurden


# funktion 1 ----


#' A function to plot a graph.
#'
#' @description A function to create a stacked bar chart
#'
#' @return The return value is a plot
#' @param df The dataframe "zentral.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
# home_stacked_comparison_mint <- function(r) {
#
#   # load UI inputs from reactive value
#   timerange <- r$date_start_comparison_mint
#
#
#   # filter dataset based on UI inputs
#   df <- dplyr::tbl(con, from = "zentral") %>%
#     dplyr::filter(jahr == timerange,
#                   region == "Deutschland",
#                   geschlecht=="Gesamt",
#                   fachbereich == "MINT" | fachbereich == "Alle"|  fachbereich == "Ingenieurwissenschaften" |fachbereich == "Mathematik_Naturwissenschaften"
#                   |fachbereich == "Alle Fächer") %>%
#     dplyr::select(bereich, indikator, fachbereich, wert) %>%
#     dplyr::collect()
#
#
#   # call function to calculate the share of MINT for every "bereich"
#   # df <- share_MINT(df)
#   #
#   # df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")
#   #
#   #
#   # df <- df %>% dplyr::filter(indikator %in% indikator_choice_1)
#   #
#   # # calculate proportions
#   # df <- df %>% dplyr::group_by(indikator, jahr) %>%
#   #   dplyr::mutate(props = sum(wert))
#   #
#   #
#   # df <- df %>% dplyr::group_by(indikator, jahr,fachbereich) %>%
#   #   dplyr::summarize(proportion = wert/props)
#   #
#   # df$proportion <- df$proportion * 100
#
#   # dfü <- df %>% dplyr::filter(jahr == timerange)
#
#   # dfk <- dfü %>% dplyr::filter(region == "Deutschland")
#
#
#
#   dfk2a3 <- df %>% dplyr::filter(bereich == "Hochschule")%>%
#   dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle")
#
#
#   dfk2c3 <- df %>% dplyr::filter(bereich == "Schule")%>%
#     dplyr::filter(fachbereich== "MINT" | fachbereich == "Alle Fächer")%>%
#     dplyr::mutate(indikator= paste0("Schüler:innen ", .$indikator ))
#
#
#   dfk2c3$fachbereich <- ifelse(grepl("Alle Fächer", dfk2c3$fachbereich), "Alle", dfk2c3$fachbereich)
#
#   dfk2b3 <- df %>% dplyr::filter(bereich != "Hochschule" & bereich != "Schule")%>%
#     unique()
#
#
#
#   dfk2_fn3 <- dplyr::bind_rows(dfk2b3, dfk2a3, dfk2c3)%>%
#     dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle")%>%
#     tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
#     dplyr::mutate("Nicht MINT" = Alle - MINT)%>%
#     dplyr::mutate(MINT_p= MINT/Alle*100)%>%
#     dplyr::mutate("Nicht MINT_p" = `Nicht MINT`/Alle*100)%>%
#     # dplyr::filter(geschlecht=="Gesamt")%>%
#     dplyr::select(- Alle)%>%
#     tidyr::pivot_longer(c(MINT, `Nicht MINT`, `Nicht MINT_p`, `MINT_p`), names_to = "fachbereich", values_to = "wert")%>%
#     dplyr::mutate(selector=dplyr::case_when(stringr::str_ends(.$fachbereich, "_p") ~ "In Prozent",
#                                             T~"Anzahl"))%>%
#     dplyr::filter(indikator %in% c("Schüler:innen Leistungskurse", "Studierende",
#                                    "Auszubildende", "Beschäftigte"))
#
#
#   # order
#   x <- ordered(factor(dfk2_fn3$indikator), levels=c("Schüler:innen Leistungskurse", "Studierende",
#                                                     "Auszubildende", "Beschäftigte"))
#
#   dfk2_fn3 <- dfk2_fn3[order(x),]
#
#   #df[df$fachbereich != "MINT", "fachbereich"] <- "andere Fachbereiche"
#
#   #Absoluten Wert speichern
#   df_wert <- dfk2_fn3 %>%
#     dplyr::filter(!(stringr::str_ends(.$fachbereich, "_p"))) %>%
#     dplyr::rename(
#       wert_abs = wert
#     )
#
#   wert_abs <- df_wert$wert_abs
#
#   # Protenz filtern und Runden
#   dfd3 <- dfk2_fn3 %>%
#     dplyr::filter(stringr::str_ends(.$fachbereich, "_p"))%>%
#     dplyr::mutate(fachbereich=dplyr::case_when(
#       fachbereich== "Nicht MINT_p" ~ "Nicht MINT",
#       fachbereich== "MINT_p" ~ "MINT"
#     ))%>% dplyr::mutate(wert = round(.$wert,1))
#
#   #Ansoluten Wert anhägnge
#   dfd3 <- dfd3 %>% dplyr::left_join(df_wert, by = c("bereich","indikator", "fachbereich"))
#
#   #Trennpunkte für lange Zahlen ergänzen
#   dfd3$wert_abs <- prettyNum(dfd3$wert_abs, big.mark = ".", decimal.mark = ",")
#
#   out <- highcharter::hchart(dfd3, 'bar', highcharter::hcaes(y = wert, x = indikator, group = "fachbereich"))%>%
#     highcharter::hc_tooltip(pointFormat = "Fachbereich: {point.fachbereich} <br> Anteil: {point.y} % <br> Anzahl: {point.wert_abs}") %>%
#     highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  FALSE) %>%
#     highcharter::hc_xAxis(title = list(text = "")) %>%
#     highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
#     highcharter::hc_colors(c("#b16fab", "#efe8e6")) %>%
#     # highcharter::hc_caption(text = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
#     highcharter::hc_title(text = paste0("Anteil von MINT nach Bildungsbereichen (", timerange,")"),
#                           margin = 45,
#                           align = "center",
#                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#     highcharter::hc_chart(
#       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
#     ) %>%
#     highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
#     highcharter::hc_exporting(enabled = FALSE,
#                               buttons = list(contextButton = list(
#                                 symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
#                                 onclick = highcharter::JS("function () {
#                                                               this.exportChart({ type: 'image/png' }); }"),
#                                 align = 'right',
#                                 verticalAlign = 'bottom',
#                                 theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
#
#   return(out)
# }


# funktion 2 ----



#' A function to plot a graph.
#'
#' @description A function to create a stacked bar chart
#'
#' @return The return value is a plot
#' @param df The dataframe "zentral.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
# home_stacked_comparison_gender <- function(r) {
#
#   # load UI inputs from reactive value
#   timerange <- r$date_start_comparison_mint_gender
#
#   # filter dataset based on UI inputs
#   df <- dplyr::tbl(con, from = "zentral") %>%
#     dplyr::filter(region == "Deutschland",
#                   jahr == timerange,
#                   fachbereich == "MINT" | fachbereich == "Alle"|  fachbereich == "Ingenieurwissenschaften" |fachbereich == "Mathematik_Naturwissenschaften"
#                   |fachbereich == "Alle Fächer",
#                   indikator %in% c("Leistungskurse",
#                                    "Studierende",
#                                    "Auszubildende", "Beschäftigte")) %>%
#     dplyr::select(bereich, indikator, geschlecht, fachbereich, wert) %>%
#     dplyr::collect()
#
#
#   df6a <- df %>% dplyr::filter(bereich == "Hochschule")%>%
#   dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle") %>%
#     tidyr::pivot_wider(names_from=geschlecht, values_from=wert)%>%
#     dplyr::mutate(Männer=Gesamt-Frauen)%>%
#     tidyr::pivot_longer(c("Männer", "Gesamt", "Frauen"), values_to = "wert", names_to="geschlecht")
#
#
#   df6c <- df %>% dplyr::filter(bereich == "Schule")%>%
#     dplyr::filter(fachbereich== "MINT" | fachbereich == "Alle Fächer")%>%
#     dplyr::mutate(indikator= paste0("Schülerinnen ", .$indikator ))%>%
#     tidyr::pivot_wider(names_from=geschlecht, values_from = wert)%>%
#     dplyr::mutate(Gesamt = Frauen + Männer)%>%
#     tidyr::pivot_longer(c("Gesamt", "Männer", "Frauen"), values_to = "wert", names_to = "geschlecht")
#
#   df6c$fachbereich <- ifelse(grepl("Alle Fächer", df6c$fachbereich), "Alle", df6c$fachbereich)
#
#   df6b <- df %>% dplyr::filter(bereich != "Hochschule" & bereich != "Schule")%>%
#     unique()%>%
#     tidyr::pivot_wider(names_from=geschlecht, values_from=wert)%>%
#     dplyr::mutate(Männer=Gesamt-Frauen)%>%
#     tidyr::pivot_longer(c("Männer", "Gesamt", "Frauen"), values_to = "wert", names_to="geschlecht")
#
#
#
#   df6_fn <- dplyr::bind_rows(df6b, df6a, df6c)%>%
#     dplyr::filter(fachbereich == "MINT" | fachbereich == "Alle")%>%
#     tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
#     dplyr::mutate("andere Fächer" = Alle - MINT)%>%
#     dplyr::select(- Alle)%>%
#     tidyr::pivot_longer(c(MINT, `andere Fächer`), names_to = "fachbereich", values_to = "wert")%>%
#     tidyr::pivot_wider(names_from = geschlecht, values_from = wert)
#
#   #Trennen um Wert abzuspeichern
#   df_wert <- df6_fn %>%
#     dplyr::select(- Gesamt)%>%
#     tidyr::pivot_longer(c(Männer, Frauen), names_to = "geschlecht", values_to = "wert")
#
#   #Berechnung des Anteils
#   df6_fn <- df6_fn %>%
#     dplyr::mutate(dplyr::across(c(Männer, Frauen), ~./Gesamt*100))%>%
#     dplyr::select(- Gesamt)%>%
#     tidyr::pivot_longer(c(Männer, Frauen), names_to = "geschlecht", values_to = "proportion")
#
#   #Wert anhängen
#   df6_fn <- df6_fn %>% dplyr::left_join(df_wert, by = c("bereich","indikator",  "fachbereich", "geschlecht"))
#
#   #Trennpunkte für lange Zahlen ergänzen
#   df6_fn$wert <- prettyNum(df6_fn$wert, big.mark = ".", decimal.mark = ",")
#
#
#   #sortieren
#   df6_fn <- df6_fn[with(df6_fn, order(fachbereich, decreasing = TRUE)), ]
#
#   #gewählte Indikatoren ausfiltern
#   df6_fn <- df6_fn %>% dplyr::filter(indikator %in% c("Schülerinnen Leistungskurse", "Studierende",
#                                                        "Auszubildende", "Beschäftigte"), fachbereich == "MINT")
#
#
#   # plot
#   hc_1 <- highcharter::hchart(df6_fn, 'bar', highcharter::hcaes( x = indikator, y=round(proportion,1), group = geschlecht)) %>%
#     highcharter::hc_tooltip(pointFormat = "{point.anzeige_geschlecht}Anteil: {point.y} % <br> Anzahl: {point.wert}") %>%
#     highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  FALSE) %>%
#     highcharter::hc_xAxis(title = list(text = ""), categories = c("Leistungskurse", "Studierende",
#                                                  "Auszubildende", "Beschäftigte")) %>%
#     highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
#     highcharter::hc_colors(c("#154194", "#efe8e6")) %>%
#     highcharter::hc_title(text = paste0("Anteil von Frauen in MINT nach Bildungsbereichen (", timerange, ")"),
#                           margin = 25,
#                           align = "center",
#                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#     highcharter::hc_chart(
#       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
#     ) %>%
#     highcharter::hc_legend(enabled = TRUE, reversed = FALSE) %>%
#     highcharter::hc_exporting(enabled = FALSE,
#                               buttons = list(contextButton = list(
#                                 symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
#                                 onclick = highcharter::JS("function () {
#                                                               this.exportChart({ type: 'image/png' }); }"),
#                                 align = 'right',
#                                 verticalAlign = 'bottom',
#                                 theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
#
#
#   out <- hc_1
#
#   return(out)
#
#   # ggplot2::ggplot(df, ggplot2::aes(x=indikator, y=wert, fill = anzeige_geschlecht)) +
#   #   ggplot2::geom_bar(stat="identity", position = "dodge") +
#   #   ggplot2::geom_text(ggplot2::aes(label=paste(round(wert),"%"), vjust = - 0.25),
#   #                      position=ggplot2::position_dodge(width=0.9),
#   #                      fontface = "bold") +
#   #   ggplot2::theme_minimal() +
#   #   ggplot2::theme(
#   #     text = ggplot2::element_text(size = 12),
#   #     plot.title = ggtext::element_markdown(hjust = 0.5)) +
#   #   ggplot2::xlab("") + ggplot2::ylab("Anteil") +
#   #   ggplot2::scale_fill_manual(values = c("#154194","#efe8e6")) +
#   #   ggplot2::labs(title = paste0(paste0("<span style='font-size:20.5pt; color:black'>",
#   #                                "Anteil von Frauen in MINT nach Bildungsbereichen (", timerange, ")",
#   #                                "<br><br><br>")),
#   #                 fill = ""
#   #                 ,
#   #            # caption = "Quellen: Statistisches Bundesamt, 2021; Bundesagentur für Arbeit, 2021; KMK, 2021, alle auf Anfrage, eigene Berechnungen."
#   #             ) +
#   #   ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"))
#
# }

