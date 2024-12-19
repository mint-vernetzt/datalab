# funktionen archiviert

# Funktion 1 ----

#' A function to plot the german map
#'
#' @description A function to plot the german map with all states that contain
#' information about the share of women in STEM
#'
#' @return The return value is the german map with information
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# kurse_map_gender <- function(r) {
#
#   # load UI inputs from reactive value
#   timerange <- r$date_map_gender
#   subjects <- r$subject_map_gender
#   kurs_select <- r$kurs_map_gender
#
#   color_fach <- c(
#     "MINT-Fächer (gesamt)" = "#b16fab",
#     "Informatik" = "#00a87a",
#     "Naturwissenschaften" = "#fcc433",
#     "Biologie" = "#fbbf24",
#     "Chemie" = "#D97706",
#     "Physik" = "#F59E0B",
#     "andere naturwiss.-technische Fächer" = "#fde68a",
#     "Mathematik" = "#ee7775",
#     "andere Fächer (gesamt)" = "#D4C1BB",
#     "Deutsch"= "#D4C1BB",
#     "Fremdsprachen"= "#D4C1BB",
#     "Gesellschaftswissenschaften" ="#D4C1BB",
#     "Musik/Kunst" = "#D4C1BB",
#     "Religion/Ethik"= "#D4C1BB",
#     "Sport"= "#D4C1BB"
#   )
#   # filter dataset based on UI inputs
#   df <- dplyr::tbl(con, from = "kurse") %>%
#     dplyr::filter(jahr == timerange,
#                   !(region %in% c("Deutschland", "Westen", "Osten")),
#                   indikator == kurs_select) %>%
#     dplyr::select(bereich, fachbereich, indikator, anzeige_geschlecht, region, jahr, wert)%>%
#     dplyr::collect()
#
#   df <- df %>%
#     dplyr::mutate(fachbereich = dplyr::case_when(fachbereich == "MINT"~ "MINT-Fächer (gesamt)",
#                                                  fachbereich == "andere Fächer" ~ "andere Fächer (gesamt)",
#                                                  T~ fachbereich))%>%
#     dplyr::group_by(indikator, anzeige_geschlecht, region, jahr)%>%
#     dplyr::summarise(fachbereich, indikator, anzeige_geschlecht, region, jahr, wert,wert_gesamt = wert[fachbereich == "Alle Fächer"])%>%
#     dplyr::ungroup()%>%
#     dplyr::filter(fachbereich != "Alle Fächer")%>%
#     dplyr::mutate(proportion = (wert/wert_gesamt)*100)%>%
#     dplyr::filter(fachbereich == subjects)
#
#
#   df_f <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")
#
#   df_m <- df %>% dplyr::filter(anzeige_geschlecht == "Männer")
#
#   help_title <- ifelse(subjects == "MINT-Fächer (gesamt)", "MINT-Fächern (gesamt)", subjects)
#   help_title <- ifelse(help_title == "andere Fächer (gesamt)", "anderen Fächern (gesamt)", help_title)
#   help_kurs <- ifelse(kurs_select == "Grundkurse", "Grundkurs-B", "Leistungskurs-B")
#   help_kurs <- ifelse(kurs_select == "Oberstufenbelegungen", "Oberstufen", help_kurs)
#
#
#   #Extra gerundeten Proportions-Wert erstellen, für Anzeige in Hover
#
#   df_f$prop <- df_f$proportion
#   df_f$prop <- round(df_f$prop, 0)
#
#   df_m$prop <- df_m$proportion
#   df_m$prop <- round(df_m$prop, 0)
#
#   #Trennpunkte für lange Zahlen ergänzen
#   df_f$wert <- prettyNum(df_f$wert, big.mark = ".", decimal.mark = ",")
#   df_m$wert <- prettyNum(df_m$wert, big.mark = ".", decimal.mark = ",")
#
#
#   # Plots
#   out1 <- highcharter::hcmap(
#     "countries/de/de-all",
#
#     data = df_f[df_f$indikator == kurs_select,],
#     value = "proportion",
#     joinBy = c("name", "region"),
#     borderColor = "#FAFAFA",
#     name = paste0(subjects),
#     borderWidth = 0.1,
#     nullColor = "#A9A9A9",
#     tooltip = list(
#       valueDecimals = 0,
#       valueSuffix = "%"
#     )
#     #,
#     #download_map_data = FALSE
#   ) %>%
#     highcharter::hc_tooltip(pointFormat = "{point.region} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}") %>%
#     highcharter::hc_colorAxis(min=0,minColor= "#fcfcfd", maxColor= as.character(color_fach[subjects]), labels = list(format = "{text}%")) %>%
#     highcharter::hc_title(
#       text = paste0(help_kurs, "elegungen von Mädchen in ", help_title, " (", timerange, ")"),
#       margin = 10,
#       align = "center",
#       style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
#     ) %>%
#     # highcharter::hc_caption(
#     #   text = "...",  style = list(color="white",fontSize = "12px")
#     # ) %>%
#     highcharter::hc_chart(
#       style = list(fontFamily = "SourceSans3-Regular")
#     ) %>% highcharter::hc_size(600, 550) %>%
#     highcharter::hc_credits(enabled = FALSE) %>%
#     highcharter::hc_legend(layout = "horizontal", floating = FALSE,
#                            verticalAlign = "bottom")
#
#   out2 <- highcharter::hcmap(
#     "countries/de/de-all",
#     data = df_m[df_m$indikator == kurs_select,],
#     value = "proportion",
#     joinBy = c("name", "region"),
#     borderColor = "#FAFAFA",
#     name = paste0(subjects),
#     borderWidth = 0.1,
#     nullColor = "#A9A9A9",
#     tooltip = list(
#       valueDecimals = 0,
#       valueSuffix = "%"
#     )
#     #,
#     #download_map_data = FALSE
#   ) %>%
#     highcharter::hc_tooltip(pointFormat = "{point.region} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}") %>%
#     highcharter::hc_colorAxis(min=0,minColor= "#fcfcfd", maxColor=as.character(color_fach[subjects]), labels = list(format = "{text}%")) %>%
#     highcharter::hc_title(
#       text = paste0(help_kurs, "elegungen von Jungen in ", help_title," (", timerange, ")"),
#       margin = 10,
#       align = "center",
#       style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
#     ) %>%
#     # highcharter::hc_caption(
#     #   text = "Ausgegraut: Daten stehen nicht zur Verfügung",  style = list(fontSize = "12px")
#     # ) %>%
#     highcharter::hc_chart(
#       style = list(fontFamily = "SourceSans3-Regular")
#     ) %>% highcharter::hc_size(600, 550) %>%
#     highcharter::hc_credits(enabled = FALSE) %>%
#     highcharter::hc_legend(layout = "horizontal", floating = FALSE, verticalAlign = "bottom")
#
#
#
#   out <- list(out1,out2)
#
#   return(out)
#
# }



# Funktion 2 ----



#' A function to create a bar plot
#'
#' @description A function to return a ranking of subject for both genders
#'
#' @return The return value is a bar plot
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# kurse_ranking <- function(r) {
#
#   # load UI inputs from reactive value
#   timerange <- r$date_kurse_ranking
#
#   states <- r$states_kurse_ranking
#
#   # filter dataset based on UI inputs
#   df <- dplyr::tbl(con, from = "kurse") %>%
#     dplyr::filter(jahr == timerange,
#                   fachbereich != "Alle Fächer",
#                   indikator != "Oberstufenbelegungen") %>%
#     dplyr::collect()
#
#   # include "Osten" und "Westen" in Dataframe
#   #df <- prep_kurse_east_west(df)
#
#   # calcualte the new "Gesamt"
#   df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
#     dplyr::group_by(region, fachbereich, indikator, jahr) %>%
#     dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
#                     wert[anzeige_geschlecht == "Männer"])
#
#   df <- df %>%
#     dplyr::ungroup()%>%
#     dplyr::mutate(region = dplyr::case_when(
#       region == "Westen" ~ "Westdeutschland (o. Berlin)",
#       region == "Osten" ~ "Ostdeutschland (inkl. Berlin)",
#       T ~ .$region
#     ))
#
#   df <- df %>% dplyr::filter(region == states)
#
#
#   df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")
#
#   # calcualte propotion
#   df <- df %>% dplyr::group_by(fachbereich, anzeige_geschlecht, indikator) %>%
#     dplyr::summarize(proportion = wert/props)%>%
#     dplyr::filter(!fachbereich %in% c("MINT", "andere Fächer" ))
#
#   df$proportion <- df$proportion * 100
#
#   df$anzeige_geschlecht <- NULL
#
#   # spread column
#   df <- tidyr::spread(df, indikator, proportion)
#
#   df <- df %>% tidyr::drop_na()
#
#   df2 <- tidyr::gather(df, group, value, -fachbereich)
#
#   #df2$fachbereich <- reorder(df2$fachbereich, df2$Leistungskurse)
#
#   df2$fachbereich <- factor(df2$fachbereich, levels = levels(df2$fachbereich))
#
#
#
#
#   ggplot2::ggplot(df,
#                   ggplot2::aes(y = fachbereich)) +
#     ggplot2::geom_point(data = df2, ggplot2::aes(x = value, color = group), size = 5) +
#     ggalt::geom_dumbbell(
#       ggplot2::aes(x = Grundkurse, xend = Leistungskurse),
#       size = 0.5,
#       size_x = 5,
#       size_xend = 5,
#       colour = "black",
#       colour_x = "#bfc6d3",
#       colour_xend = "#66cbaf",
#       dot_guide=TRUE) +
#     ggplot2::theme_minimal() +
#     ggplot2::scale_color_manual(name = "", values = c("#bfc6d3", "#66cbaf")) +
#     ggplot2::theme(legend.position="top",
#                    #legend.text= ggplot2::element_text(family = 'SourceSans3-Regular'),
#                    panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
#                    plot.title = ggtext::element_markdown(hjust = 0.5),
#                    axis.text.y = ggplot2::element_text(size = 11)) +
#     ggplot2::ylab("") + ggplot2::xlab("") +
#     ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
#                                  "Mädchen-Anteil nach Fächern in ", states, " (",timerange,")",
#                                  "<br><br>"),
#                   color = "") +
#     ggplot2::scale_x_continuous(n.breaks = 7, labels = function(x) paste0(x, "%"))
#
# }

# Funktion 3 ----




#' A function to plot a graph.
#'
#' @description A function to create a pie chart for the first box
#' inside the tab "Schule".
#'
#' @return The return value is a plot
#' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
#'
#' kurse_einstieg_pie <- function(df,r) {
#'
#'
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_kurse_einstieg
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr == timerange)
#'
#'   df <- df %>% dplyr::filter(region == "Deutschland")
#'
#' #
#' #   df <- df %>% dplyr::filter(region == "Deutschland")%>%
#' #     tidyr::pivot_wider(names_from=anzeige_geschlecht, values_from=wert)%>%
#' #     dplyr::mutate(Gesamt=Männer+Frauen)%>%
#' #     tidyr::pivot_longer(c("Gesamt", "Frauen", "Männer"), names_to = "anzeige_geschlecht", values_to = "wert")
#' #
#' #
#' #   df <- df %>%
#' #     tidyr::pivot_wider(values_from = wert, names_from = fachbereich)%>%
#' #     dplyr::mutate(MINT=Mathematik+Informatik+Physik+Biologie+Chemie,
#' #                   "andere Fächer" =`Alle Fächer`- MINT)%>%
#' #     tidyr::pivot_longer(c(6:19), values_to = "wert", names_to= "fachbereich")
#' #
#' #
#' #   df <- df %>%
#' #     tidyr::pivot_wider(values_from = wert, names_from = anzeige_geschlecht)%>%
#' #     tidyr::pivot_longer(c("Männer","Frauen"),names_to = "anzeige_geschlecht", values_to= "wert")%>%
#' #     dplyr::rename(wert_new = Gesamt)%>%
#' #     dplyr::filter(fachbereich=="MINT" | fachbereich == "andere Fächer")
#'
#'
#'
#'   # aggregate to MINT
#'   df <- share_mint_kurse(df)
#'
#'   # calcualte the new "Gesamt"
#'   # df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
#'   #   dplyr::group_by(region, fachbereich, indikator, jahr) %>%
#'   #   dplyr::mutate(wert_new = wert[anzeige_geschlecht == "Frauen"] +
#'   #                   wert[anzeige_geschlecht == "Männer"])
#'
#'   df <- df %>% dplyr::group_by(region, fachbereich, indikator, jahr) %>%
#'     dplyr::mutate(wert_new = wert[anzeige_geschlecht == "Gesamt"] )%>%
#'     dplyr::filter(anzeige_geschlecht == "Gesamt")
#'
#'   # df_test <- df1 %>% tidyr:: pivot_wider(names_from = anzeige_geschlecht, values_from = wert)%>%
#'   #   dplyr::mutate(Gesamt= Frauen + Männer)%>%
#'   #   tidyr::pivot_longer(c(6:8), values_to = "wert", names_to = "anzeige_geschlecht")
#'   #
#'   #
#'   # df_test1 <- share_mint_kurse(df_test)%>%
#'   #   tidyr::pivot_wider(names_from = anzeige_geschlecht, values_from = wert)%>%
#'   #   dplyr::rename(wert_new=Gesamt)%>%
#'   #   tidyr::pivot_longer(c(6,8), names_to = "anzeige_geschlecht", values_to = "wert")
#'
#'
#'   df = df[!duplicated(df$wert_new),]
#'
#'   df$anzeige_geschlecht <- NULL
#'
#'
#'   df <- df %>%
#'     dplyr::group_by(jahr, indikator) %>%
#'     dplyr::mutate(sum_wert = sum(wert_new))
#'
#'
#'   # calculate proportions
#'   # df <- df %>% dplyr::group_by(jahr, indikator, fachbereich) %>%
#'   #   dplyr::summarize(proportion = wert_new/sum_wert) %>%
#'   #   dplyr::mutate(proportion = round(proportion, 2)*100)
#'
#'   df<-  df %>% dplyr::group_by(jahr, indikator, fachbereich) %>%
#'     dplyr::summarize(proportion = wert_new/sum_wert)
#'
#'   df$proportion <- df$proportion * 100
#'
#'   df <- df %>% dplyr::group_by(jahr, indikator, fachbereich) %>%
#'     dplyr::mutate(proportion = round(proportion,0))
#'
#'
#'   df_gk <- df %>% dplyr::filter(indikator == "Grundkurse")
#'
#'
#'
#'   df_lk <- df %>% dplyr::filter(indikator == "Leistungskurse")
#'
#'   plot_gk <- highcharter::hchart(df_gk, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
#'     highcharter::hc_tooltip(
#'       pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#'     highcharter::hc_title(text = paste0("Fächerwahl (Grundkurse)",br(), timerange),
#'                           margin = 45,
#'                           align = "center",
#'                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#'     highcharter::hc_chart(
#'       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
#'     highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
#'     highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#'                                            dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE))
#'
#'   plot_lk <- highcharter::hchart(df_lk, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
#'     highcharter::hc_tooltip(
#'       pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#'     highcharter::hc_title(text = paste0("Fächerwahl (Leistungskurse)", br(), timerange),
#'                           margin = 45,
#'                           align = "center",
#'                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#'     highcharter::hc_chart(
#'       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
#'     highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
#'     highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#'                                            dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE))
#'
#'
#'   plot_gk <- plot_gk %>% highcharter::hc_colors(c("#efe8e6","#b16fab"))
#'
#'   plot_lk <- plot_lk %>% highcharter::hc_colors(c("#efe8e6","#b16fab"))
#'
#'
#'   # place plots inside grid
#'   highcharter::hw_grid(
#'
#'     plot_gk,
#'
#'     plot_lk,
#'
#'     ncol = 2,
#'     browsable = TRUE
#'   )
#'
#'
#' }
#'
#'
#'
# Funktion 4 ----

#'
#' #' A function to plot a graph.
#' #'
#' #' @description A function to create a pie chart for the first box
#' #' inside the tab "Schule".
#' #'
#' #' @return The return value is a plot
#' #' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' kurse_einstieg_pie_gender <- function(df,r) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_kurse_pie_gender
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr == timerange)
#'
#'
#'   df <- df %>% dplyr::filter(region == "Deutschland")
#'
#'
#'   # calcualte the new value for "Gesamt"
#'   df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Leistungskurse"), "wert"] <-  df %>%
#'     dplyr::filter(indikator == "Leistungskurse") %>%
#'     dplyr::group_by(indikator, jahr) %>%
#'     dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"] +
#'                        wert[anzeige_geschlecht == "Männer"]) %>% dplyr::pull(wert)
#'
#'
#'   df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Grundkurse"), "wert"] <-  df %>%
#'     dplyr::filter(indikator == "Grundkurse") %>%
#'     dplyr::group_by(indikator, jahr) %>%
#'     dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"] +
#'                        wert[anzeige_geschlecht == "Männer"]) %>% dplyr::pull(wert)
#'
#'   # aggregate the share of the MINT subjects vs Rest
#'   df <- share_mint_kurse(df)
#'
#'  # df_gk <- df %>% dplyr::filter(indikator == "Grundkurse")
#'
#' # df_gk <- df_gk %>% dplyr::filter(anzeige_geschlecht != "Gesamt")
#'
#'   # calculate proportions
#'  # df_gk <- share_pie(df_gk)
#'
#' #  df_lk <- df %>% dplyr::filter(indikator == "Leistungskurse")
#'
#'  # df_lk <- df_lk %>% dplyr::filter(anzeige_geschlecht != "Gesamt")
#'
#'   # # calculate proportions
#'   # df_lk <- share_pie(df_lk)
#'
#'   #Grundkurs
#'   df_gk <- df %>% dplyr::filter(indikator == "Grundkurse")
#'
#'   # calculate proprotion female
#'   df_gk[df_gk$anzeige_geschlecht == "Frauen", "wert"] <-  df_gk %>% dplyr::group_by(fachbereich) %>%
#'     dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"]/
#'                        wert[anzeige_geschlecht == "Gesamt"]) %>%
#'     dplyr::arrange(-dplyr::row_number()) %>% dplyr::pull(wert)
#'
#'
#'   # calculate proprotion male
#'   df_gk[df_gk$anzeige_geschlecht == "Männer", "wert"] <- df_gk %>% dplyr::group_by(fachbereich) %>%
#'     dplyr::summarise(wert = wert[anzeige_geschlecht == "Männer"]/
#'                        wert[anzeige_geschlecht == "Gesamt"]) %>%
#'     dplyr::arrange(-dplyr::row_number()) %>% dplyr::pull(wert)
#'
#'   df_gk <- df_gk %>% dplyr::filter(anzeige_geschlecht != "Gesamt")
#'
#'   df_gk$wert <- df_gk$wert * 100
#'
#'   #Leistungskurs
#'   df_lk <- df %>% dplyr::filter(indikator == "Leistungskurse")
#'
#'   # calculate proprotion female
#'   df_lk[df_lk$anzeige_geschlecht == "Frauen", "wert"] <-  df_lk %>% dplyr::group_by(fachbereich) %>%
#'     dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"]/
#'                        wert[anzeige_geschlecht == "Gesamt"]) %>%
#'     dplyr::arrange(-dplyr::row_number()) %>% dplyr::pull(wert)
#'
#'
#'   # calculate proprotion male
#'   df_lk[df_lk$anzeige_geschlecht == "Männer", "wert"] <- df_lk %>% dplyr::group_by(fachbereich) %>%
#'     dplyr::summarise(wert = wert[anzeige_geschlecht == "Männer"]/
#'                        wert[anzeige_geschlecht == "Gesamt"]) %>%
#'     dplyr::arrange(-dplyr::row_number()) %>% dplyr::pull(wert)
#'
#'   df_lk <- df_lk %>% dplyr::filter(anzeige_geschlecht != "Gesamt")
#'
#'   df_lk$wert <- df_lk$wert * 100
#'
#'
#'   df_gk_mint <- df_gk %>% dplyr::filter(fachbereich == "MINT")
#'
#'   df_lk_mint <- df_lk %>% dplyr::filter(fachbereich == "MINT")
#'
#'   df_gk_rest <- df_gk %>% dplyr::filter(fachbereich == "andere Fächer")
#'
#'   df_lk_rest <- df_lk %>% dplyr::filter(fachbereich == "andere Fächer")
#'
#'   df_gk_mint$anzeige_geschlecht[df_gk_mint$anzeige_geschlecht == "Frauen"] <- "Mädchen"
#'   df_gk_mint$anzeige_geschlecht[df_gk_mint$anzeige_geschlecht == "Männer"] <- "Jungen"
#'   df_lk_mint$anzeige_geschlecht[df_lk_mint$anzeige_geschlecht == "Frauen"] <- "Mädchen"
#'   df_lk_mint$anzeige_geschlecht[df_lk_mint$anzeige_geschlecht == "Männer"] <- "Jungen"
#'   df_gk_rest$anzeige_geschlecht[df_gk_rest$anzeige_geschlecht == "Frauen"] <- "Mädchen"
#'   df_gk_rest$anzeige_geschlecht[df_gk_rest$anzeige_geschlecht == "Männer"] <- "Jungen"
#'   df_lk_rest$anzeige_geschlecht[df_lk_rest$anzeige_geschlecht == "Frauen"] <- "Mädchen"
#'   df_lk_rest$anzeige_geschlecht[df_lk_rest$anzeige_geschlecht == "Männer"] <- "Jungen"
#'
#'   plot_gk_mint <- highcharter::hchart(df_gk_mint, size = 280, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
#'     highcharter::hc_tooltip(
#'       pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#'     highcharter::hc_title(text = paste0("MINT-Fächer (Grundkurse)", br(), timerange),
#'                           margin = 45,
#'                           align = "center",
#'                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#'     highcharter::hc_chart(
#'       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px"), marginTop = 20, marginBottom = 7) %>%
#'     highcharter::hc_legend(enabled = TRUE) %>%
#'     highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#'                                            dataLabels = list(enabled = TRUE,  format='{point.percentage:.0f}%'), showInLegend = TRUE)) %>%
#'     highcharter::hc_colors(c("#154194","#efe8e6"))
#'
#'
#'   plot_gk_rest <- highcharter::hchart(df_gk_rest, size = 150, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
#'     highcharter::hc_tooltip(
#'       pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#'
#'     highcharter::hc_title(text = paste0(br(), "Zum Vergleich: Anteil von Schülerinnen an Nicht-MINT-Fächern in Grundkursen in ", timerange),
#'
#'    # highcharter::hc_title(text = paste0("Vergleich: Andere Fächer (Grundkurse)", br(), timerange),
#'
#'                           margin = 45,
#'                           align = "center",
#'                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#'     highcharter::hc_chart(
#'       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
#'     highcharter::hc_legend(enabled = TRUE, y = -180) %>%
#'     highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#'                                            dataLabels = list(enabled = TRUE,  format='{point.percentage:.0f}%'), showInLegend = TRUE,
#'                                            opacity = 0.7)) %>%
#'     highcharter::hc_colors(c("#154194","#efe8e6"))
#'
#'
#'   plot_lk_mint <- highcharter::hchart(df_lk_mint, size = 280, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
#'     highcharter::hc_tooltip(
#'       pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#'     highcharter::hc_title(text = paste0("MINT-Fächer (Leistungskurse) ", br(), timerange),
#'                           margin = 45,
#'                           align = "center",
#'                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#'     highcharter::hc_chart(
#'       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px"), marginTop = 20, marginBottom = 7) %>%
#'     highcharter::hc_legend(enabled = TRUE) %>%
#'     highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#'                                            dataLabels = list(enabled = TRUE, format='{point.percentage:.0f}%'), showInLegend = TRUE)) %>%
#'     highcharter::hc_colors(c("#154194","#efe8e6"))
#'
#'   plot_lk_rest <- highcharter::hchart(df_lk_rest, size = 150, type = "pie", mapping = highcharter::hcaes(x = anzeige_geschlecht, y = wert)) %>%
#'     highcharter::hc_tooltip(
#'       pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#'
#'     highcharter::hc_title(text = paste0(br(), "Zum Vergleich: Anteil von Schülerinnen an Nicht-MINT-Fächern in Leistungskursen in ", timerange),
#'
#'    # highcharter::hc_title(text = paste0("Vergleich: Andere Fächer (Leistungskurse) ", br(), timerange),
#'
#'                           margin = 45,
#'                           align = "center",
#'                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#'     highcharter::hc_chart(
#'       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
#'     highcharter::hc_legend(enabled = TRUE, y = -180) %>%
#'     highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#'                                            dataLabels = list(enabled = TRUE, format='{point.percentage:.0f}%'), showInLegend = TRUE,
#'                                            opacity = 0.7)) %>%
#'     highcharter::hc_colors(c("#154194","#efe8e6"))
#'
#'
#'   # place plots inside grid
#'   highcharter::hw_grid(
#'
#'     plot_gk_mint,
#'
#'     plot_lk_mint,
#'
#'     plot_gk_rest,
#'
#'     plot_lk_rest,
#'
#'     ncol = 2,
#'     browsable = TRUE
#'   )
#'
#'
#' }
#'
#'


# Funktion 5 ----


#' #' A function to return a filtered dataset
#' #'
#' #' @description A function to similar to 'kurse_einstieg_bar' but with the
#' #' difference that it returns a dataframe instead of plot.
#' #'
#' #' @return The return value is a dataframe
#' #' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' # data_einstieg_kurse <- function(df,r) {
#' #
#' #   # load UI inputs from reactive value
#' #   timerange <- r$date_kurse_einstieg
#' #
#' #   # load UI inputs from reactive value
#' #   timerange <- r$date_kurse_einstieg
#' #
#' #   # filter dataset based on UI inputs
#' #   df <- df %>% dplyr::filter(jahr == timerange)
#' #
#' #   df <- df %>% dplyr::filter(region == "Deutschland")
#' #
#' #
#' #
#' #   df <- df %>% dplyr::filter(region == "Deutschland")
#' #
#' #   df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Leistungskurse"), "wert"] <-  df %>%
#' #     dplyr::filter(indikator == "Leistungskurse") %>%
#' #     dplyr::group_by(indikator, jahr) %>%
#' #     dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"] +
#' #                        wert[anzeige_geschlecht == "Männer"]) %>% dplyr::pull(wert)
#' #
#' #
#' #   df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Grundkurse"), "wert"] <-  df %>%
#' #     dplyr::filter(indikator == "Grundkurse") %>%
#' #     dplyr::group_by(indikator, jahr) %>%
#' #     dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"] +
#' #                        wert[anzeige_geschlecht == "Männer"]) %>% dplyr::pull(wert)
#' #
#' #
#' #
#' #   df <- share_mint_kurse(df)
#' #
#' #
#' #   colnames(df) <- c("Jahr", "Region" ,"Indikator", "Geschlecht", "Bereich", "Wert", "Fachbereich")
#' #
#' #   return(df)
#' #
#' # }
#'
#'
#'





# Funktion 6 ----
#'
#' #' A function to create a paired bar plot
#' #'
#' #' @description A function to return a paired bar plot for the second box inside
#' #' the tab "Schule"
#' #'
#' #' @return The return value is a bar plot
#' #' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' # kurse_absolut <- function(df,r) {
#' #
#' #   # load UI inputs from reactive value
#' #   level_kurs <- r$indikator_kurse
#' #
#' #   timerange <- r$date_kurse
#' #
#' #   # filter dataset based on UI inputs
#' #   df <- df %>% dplyr::filter(jahr == timerange)
#' #
#' #   df <- df %>% dplyr::filter(region == "Deutschland")
#' #
#' #
#' #   # combine subjects to get numbers on share of MINT
#' #   # make a function out of it
#' #   subjects <- c("Mathematik", "Informatik", "Naturwissenschaften")
#' #
#' #   df <- df %>% dplyr::filter(fachbereich %in% subjects)
#' #
#' #
#' #   df <- df[with(df, order(indikator, decreasing = TRUE)), ]
#' #
#' #
#' #   df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt")
#' #
#' #
#' #   options(scipen=999)
#' #
#' #   # plot
#' #   ggplot2::ggplot(df, ggplot2::aes(x=indikator, y=wert, fill = fachbereich)) +
#' #     ggplot2::geom_bar(stat="identity", position = "dodge") +
#' #     ggplot2::geom_text(ggplot2::aes(label=wert, vjust = - 0.25),
#' #                        position=ggplot2::position_dodge(width=0.9),
#' #                        fontface = "bold") +
#' #     ggplot2::theme_minimal() +
#' #     ggplot2::facet_grid(~ anzeige_geschlecht) +
#' #     ggplot2::theme(
#' #       strip.background = ggplot2::element_blank(),
#' #       text = ggplot2::element_text(size = 14),
#' #       plot.title = ggtext::element_markdown(hjust = 0.5)) +
#' #     ggplot2::xlab("") + ggplot2::ylab("Anzahl") +
#' #     ggplot2::scale_fill_manual(values = c("#ee7775", "#fcc433", "#00a87a")) +
#' #     ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
#' #                                  "Schüler und Schülerinnen in MINT" ," in ", timerange,
#' #                                  "<br><br><br>"),
#' #                   fill = "")
#' #
#' #
#' # }
#'
#'
# Funktion 7 ----

#' #' A function to create a bar plot
#' #'
#' #' @description A function to return a ranking o
#' #'
#' #' @return The return value is a bar plot
#' #' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' # kurse_ranking_bl <- function(df,r, type) {
#' #
#' #   # load UI inputs from reactive value
#' #   timerange <- r$date_kurse_ranking_bl
#' #
#' #   subjects <- r$subject_kurse_ranking_bl
#' #
#' #   # filter dataset based on UI inputs
#' #   df <- df %>% dplyr::filter(jahr == timerange)
#' #
#' #   df <- df %>% dplyr::filter(region != "Deutschland")
#' #
#' #   df <- df %>% dplyr::filter(fachbereich != "Alle Fächer")
#' #
#' #   df <- df %>% dplyr::filter(region != "Bayern")
#' #
#' #   df <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")
#' #
#' #   # calcualte propotion
#' #   df <- df %>% dplyr::group_by(fachbereich, anzeige_geschlecht, indikator) %>%
#' #     dplyr::summarize(proportion = wert/props)
#' #
#' #   df$proportion <- df$proportion * 100
#' #
#' #   df$anzeige_geschlecht <- NULL
#' #
#' #   # spread column
#' #   df <- tidyr::spread(df, indikator, proportion)
#' #
#' #   df <- df %>% tidyr::drop_na()
#' #
#' #   df2 <- tidyr::gather(df, group, value, -fachbereich)
#' #
#' #   df$fachbereich <- reorder(df$fachbereich, df$Leistungskurse)
#' #
#' #   df2$fachbereich <- factor(df2$fachbereich, levels = levels(df$fachbereich))
#' #
#' #
#' #   ggplot2::ggplot(df,
#' #                   ggplot2::aes(y = fachbereich)) +
#' #     ggplot2::geom_point(data = df2, ggplot2::aes(x = value, color = group), size = 5) +
#' #     ggalt::geom_dumbbell(
#' #       ggplot2::aes(x = Grundkurse, xend = Leistungskurse),
#' #       size = 0.5,
#' #       size_x = 5,
#' #       size_xend = 5,
#' #       colour = "black",
#' #       colour_x = "#b1b5c366",
#' #       colour_xend = "#f5adac66",
#' #       dot_guide=TRUE) +
#' #     ggplot2::theme_minimal() +
#' #     ggplot2::scale_color_manual(name = "", values = c("#b1b5c366", "#f5adac66")) +
#' #     ggplot2::theme(legend.position="top",
#' #                    #legend.text= ggplot2::element_text(family = 'SourceSans3-Regular'),
#' #                    panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
#' #                    plot.title = ggtext::element_markdown(hjust = 0.5),
#' #                    axis.text.y = ggplot2::element_text(size = 11)) +
#' #     ggplot2::ylab("") + ggplot2::xlab("") +
#' #     ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
#' #                                  "Relativer Anteil von Schülerinnen an Grund- und Leistungskurse in ",timerange,
#' #                                  "<br><br><br>"),
#' #                   color = "") +
#' #     ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))
#' #
#' # }
#'
# Funktion 8 ----

#' #' A function to return a filtered dataset
#' #'
#' #' @description A function to similar to 'kurse_einstieg_bar' but with the
#' #' difference that it returns a dataframe instead of plot.
#' #'
#' #' @return The return value is a dataframe
#' #' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' # data_mix_kurse <- function(df,r) {
#' #
#' #   # load UI inputs from reactive value
#' #   timerange <- r$date_kurse
#' #
#' #   # filter dataset based on UI inputs
#' #   df <- df %>% dplyr::filter(jahr == timerange)
#' #
#' #   # remove
#' #   df <- df %>% dplyr::filter(region != "Bayern")
#' #
#' #   # combine subjects to get numbers on share of MINT
#' #   # make a function out of it
#' #   subjects <- c("Mathematik", "Informatik", "Biologie", "Chemie",
#' #                 "Physik", "andere naturwiss.-technische Fächer")
#' #
#' #   df <- df %>% dplyr::filter(fachbereich %in% subjects)
#' #
#' #
#' #   colnames(df) <- c("Region", "Fachbereich", "Geschlecht", "Jahr", "Indikator", "Bereich", "Wert")
#' #
#' #   return(df)
#' #
#' # }
# Funktion 8 ----

#' #' A function to plot time series
#' #'
#' #' @description A function to plot the time series
#' #'
#' #' @return The return value, if any, from executing the function.
#' #' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' kurse_verlauf <- function(df,r) {
#'
#'   # load UI inputs from reactive value
#'   level_kurs <- r$indikator_kurse_verlauf
#'
#'   timerange <- r$date_kurse_verlauf
#'
#'   states <- r$states_kurse_verlauf
#'
#'   subjects_select <- r$subject_selected
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])
#'
#'   df <- df %>% dplyr::filter(region != "Deutschland")
#'
#'   df_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen",
#'                                     fachbereich == "Alle Fächer") %>%
#'     dplyr::rename(wert_sum = "wert")
#'
#'   df <- df %>% dplyr::filter(indikator == level_kurs)
#'
#'   if (level_kurs == "Grundkurse"){
#'
#'     title_help <- "Grundkurs"
#'
#'   }else{
#'
#'     title_help <- "Leistungskurs"
#'
#'   }
#'
#'   # calcualte new "Gesamt"
#'   # df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
#'   #   dplyr::group_by(region, fachbereich, indikator, jahr) %>%
#'   #   dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
#'   #                   wert[anzeige_geschlecht == "Männer"])
#'
#'   # include "Osten" und "Westen" in Dataframe
#'   df <- prep_kurse_east_west(df, "subjects")
#'
#'
#'   # aggregate to MINT
#'   df_sub <- share_mint_kurse(df)
#'
#'   # df_sub <-  df_sub %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
#'   #   dplyr::group_by(region, fachbereich, indikator, jahr) %>%
#'   #   dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
#'   #                   wert[anzeige_geschlecht == "Männer"])
#'
#'
#'   df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT-Fächer (gesamt)"
#'
#'   df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (gesamt)"
#'
#'   df_sub <- df_sub[,colnames(df)]
#'
#'   df <- rbind(df, df_sub)
#'
#'   # filter states
#'   df <- df %>% dplyr::filter(region %in% states)
#'
#'   df <- df %>% dplyr::filter(fachbereich %in% subjects_select)
#'
#'   # calculate proportions
#'   df <- df %>% dplyr::left_join(df_gesamt, by=c("jahr", "region", "indikator",
#'                                                 "bereich", "anzeige_geschlecht")) %>%
#'     dplyr::rename(fachbereich = "fachbereich.x") %>%
#'     dplyr::select(-fachbereich.y) %>%
#'     dplyr::mutate(proportion = (wert/wert_sum)*100)
#'
#'
#'   df <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")
#'
#'   # order years for plot
#'   df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
#'
#'   # plot
#'   highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = region)) %>%
#'     highcharter::hc_tooltip(pointFormat = "Anteil Schülerinnen <br> Bundesland: {point.region} <br> Wert: {point.y} %") %>%
#'     highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
#'     highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
#'     highcharter::hc_caption(text = "Quelle: KMK 2021, auf Anfrage; eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
#'     highcharter::hc_title(text = paste0("Schülerinnen: Anteil ", subjects_select, "(", title_help, ")", ),
#'                           margin = 45,
#'                           align = "center",
#'                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#'     highcharter::hc_colors(c("#b16fab", "#154194","#66cbaf")) %>%
#'     highcharter::hc_chart(
#'       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
#'     ) %>%
#'     highcharter::hc_exporting(enabled = FALSE,
#'                               buttons = list(contextButton = list(
#'                                 symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
#'                                 onclick = highcharter::JS("function () {
#'                                                               this.exportChart({ type: 'image/png' }); }"),
#'                                 align = 'right',
#'                                 verticalAlign = 'bottom',
#'                                 theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
#'
#'
#' }
#'
#'
#'
#'
# Funktion 9 ----

#' #' A function to return a filtered dataset
#' #'
#' #' @description A function to plot the time series
#' #'
#' #' @return The return value is a dataframe
#' #' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' # data_verlauf_kurse <- function(df,r) {
#' #
#' #   # load UI inputs from reactive value
#' #   level_kurs <- r$indikator_kurse_verlauf
#' #
#' #   timerange <- r$date_kurse_verlauf
#' #
#' #   states <- r$states_kurse_verlauf
#' #
#' #   topic <- r$topic_kurse_verlauf
#' #
#' #   subject_aggregated <- r$subjects_aggregated
#' #
#' #   subjects_select <- r$subject_selected
#' #
#' #   # filter dataset based on UI inputs
#' #   df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])
#' #
#' #   df <- df %>% dplyr::filter(indikator == level_kurs)
#' #
#' #   # remove
#' #   df <- df %>% dplyr::filter(region != "Deutschland")
#' #
#' #   df <- df %>% dplyr::filter(region != "Bayern")
#' #
#' #   if (level_kurs == "Grundkurse"){
#' #
#' #     title_help_sub <- " für die Grundkurse"
#' #
#' #   }else{
#' #
#' #     title_help_sub <- " für die Leistungskurse"
#' #
#' #   }
#' #
#' #   # calculate new "Gesamt"
#' #   df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
#' #     dplyr::group_by(region, fachbereich, indikator, jahr) %>%
#' #     dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
#' #                     wert[anzeige_geschlecht == "Männer"])
#' #
#' #   # include "Osten" und "Westen" in Dataframe
#' #   df <- prep_kurse_east_west(df)
#' #
#' #   if (subject_aggregated == "aggregiert"){
#' #
#' #     # aggregate to MINT
#' #     df <- share_mint_kurse(df)
#' #
#' #     # calculate the new "Gesamt"
#' #     df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
#' #       dplyr::group_by(region, fachbereich, indikator, jahr) %>%
#' #       dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
#' #                       wert[anzeige_geschlecht == "Männer"])
#' #
#' #     # filter MINT or remaining subjects
#' #     df <- df %>% dplyr::filter(fachbereich == topic)
#' #
#' #
#' #     if (topic == "MINT"){
#' #
#' #       title_help <- paste0("MINT", title_help_sub)
#' #
#' #     }else {
#' #
#' #       title_help <- paste0("anderen Fächer", title_help_sub)
#' #
#' #     }
#' #
#' #   }else {
#' #
#' #     df <- df %>% dplyr::filter(fachbereich %in% subjects_select)
#' #
#' #     title_help <- paste0(subjects_select, title_help_sub)
#' #   }
#' #
#' #   # calcualte proportions
#' #   df <- df %>% dplyr::group_by(region, fachbereich, anzeige_geschlecht, jahr, indikator) %>%
#' #     dplyr::summarise(proportion = wert/props)
#' #
#' #   df <- df %>% dplyr::filter(anzeige_geschlecht != "Männer")
#' #
#' #   df$proportion <- df$proportion * 100
#' #
#' #   # order
#' #   df <- df[with(df, order(region, fachbereich, jahr, decreasing = TRUE)), ]
#' #
#' #
#' #   df <- df %>% dplyr::filter(region %in% states)
#' #
#' #   # order
#' #   df <- df[with(df, order(region, fachbereich, jahr, decreasing = TRUE)), ]
#' #
#' #   colnames(df) <- c("Region", "Fachbereich", "Geschlecht", "Jahr", "Indikator", "Anteil")
#' #
#' #   return(df)
#' #
#' # }
#'
#'
# Funktion 10 ----

#' #' A function to plot time series
#' #'
#' #' @description A function to plot the time series
#' #'
#' #' @return The return value, if any, from executing the function.
#' #' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' kurse_verlauf_gender <- function(df,r) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_kurse_verlauf_gender
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])
#'
#'   # remove
#'   df <- df %>% dplyr::filter(region == "Deutschland")
#'
#'   # aggregate to MINT
#'   df <- share_mint_kurse(df)
#'
#'   # calcualte the new "Gesamt"
#'   df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
#'     dplyr::group_by(region, fachbereich, indikator, jahr) %>%
#'     dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
#'                     wert[anzeige_geschlecht == "Männer"])
#'
#'   df <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen" & fachbereich == "MINT")
#'
#'   # calcualte proportions
#'   df <- df %>% dplyr::group_by(indikator, fachbereich, anzeige_geschlecht, jahr) %>%
#'     dplyr::summarize(proportion = wert/props)
#'
#'   df$proportion <- df$proportion * 100
#'
#'
#'
#'   # order years for plot
#'   df <- df[with(df, order(jahr, decreasing = FALSE)), ]
#'
#'   # plot
#'   highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = indikator)) %>%
#'     highcharter::hc_tooltip(pointFormat = "Anteil Schülerinnen  {point.indikator} <br> Wert: {point.y} %") %>%
#'     highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
#'     highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
#'     #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
#'     highcharter::hc_title(text = paste0("Anteil von Schülerinnen in MINT-Fächern im Zeitverlauf"),
#'                           margin = 45,
#'                           align = "center",
#'                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#'     highcharter::hc_chart(
#'       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
#'     ) %>%
#'     highcharter::hc_exporting(enabled = FALSE,
#'                               buttons = list(contextButton = list(
#'                                 symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
#'                                 onclick = highcharter::JS("function () {
#'                                                               this.exportChart({ type: 'image/png' }); }"),
#'                                 align = 'right',
#'                                 verticalAlign = 'bottom',
#'                                 theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
#'
#'
#' }
#'
# Funktion 11 ----

#' #' A function to plot time series
#' #'
#' #' @description A function to plot the time series of the german states
#' #'
#' #' @return The return value, if any, from executing the function.
#' #' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' kurse_verlauf_single_bl <- function(df,r) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_kurse_verlauf_bl
#'
#'   states <- r$states_kurse_verlauf_bl
#'
#'   subjects_select <- r$subject_selected_bl
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])
#'
#'   # remove
#'   # df <- df %>% dplyr::filter(region != "Deutschland")
#'   #
#'   # df <- df %>% dplyr::filter(region != "Bayern")
#'
#'   # include "Osten" und "Westen" in Dataframe
#'   df <- prep_kurse_east_west(df)
#'
#'   # aggregate to MINT
#'   df_sub <- share_mint_kurse(df)
#'
#'   df_sub <- df_sub[,colnames(df)]
#'
#'   # aggregate all subjects to calculate proportion later
#'   df_sub <- df_sub %>% dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
#'     dplyr::mutate(props = sum(wert))
#'
#'
#'   df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT-Fächer (gesamt)"
#'
#'   df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (gesamt)"
#'
#'   # calcualte the new "Gesamt"
#'   df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
#'       dplyr::group_by(region, fachbereich, indikator, jahr) %>%
#'       dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
#'                       wert[anzeige_geschlecht == "Männer"])
#'
#'   df <- rbind(df, df_sub)
#'
#'   # filter states
#'   df <- df %>% dplyr::filter(region %in% states)
#'
#'   df <- df %>% dplyr::filter(fachbereich %in% subjects_select)
#'
#'   # calculate proportions
#'   df <- df %>% dplyr::group_by(region, indikator, fachbereich, anzeige_geschlecht, jahr) %>%
#'     dplyr::summarize(proportion = wert/props)
#'
#'   df$proportion <- df$proportion * 100
#'
#'   df <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")
#'
#'   df$anzeige_geschlecht <- paste0(df$anzeige_geschlecht, " (", df$indikator, ")")
#'
#'
#'   # order years for plot
#'   df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
#'
#'   # plot
#'   highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = anzeige_geschlecht)) %>%
#'     highcharter::hc_tooltip(pointFormat = "Anteil {point.anzeige_geschlecht} <br> Wert: {point.y} %") %>%
#'     highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
#'     highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
#'     #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
#'     highcharter::hc_title(text = paste0("Anteil von Schülerinnen in ", subjects_select ," in ", states),
#'                           margin = 45,
#'                           align = "center",
#'                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#'     highcharter::hc_chart(
#'       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
#'     ) %>%
#'     highcharter::hc_exporting(enabled = FALSE,
#'                               buttons = list(contextButton = list(
#'                                 symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
#'                                 onclick = highcharter::JS("function () {
#'                                                               this.exportChart({ type: 'image/png' }); }"),
#'                                 align = 'right',
#'                                 verticalAlign = 'bottom',
#'                                 theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
#'
#'
#' }
#'
