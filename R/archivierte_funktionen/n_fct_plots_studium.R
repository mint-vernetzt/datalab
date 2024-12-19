#archives




#

# funktion 1 ----

#' A function to plot time series
#'
#' @description A function to plot a bar chart
#'
#' @return The return value, if any, from executing the function.
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# studienzahl_einstieg_comparison_gender <- function(r) {
#
#   # load UI inputs from reactive value
#   timerange <- as.numeric(r$gen_f_y)
#
#   sel_bl1 <- r$gen_states
#
#   # Zuweisung von r zu sel_f in abhängigkeit der Bundesländer
#
#   if(sel_bl1 %in% c("Deutschland",
#                     "Baden-Württemberg",
#                     "Bayern",
#                     "Berlin",
#                     "Hamburg",
#                     "Hessen",
#                     "Nordrhein-Westfalen",
#                     "Rheinland-Pfalz",
#                     "Sachsen",
#                     "Westdeutschland (o. Berlin)",
#                     "Ostdeutschland (inkl. Berlin)")) {
#     sel_f1 <- r$gen1_f
#   }
#   else {
#     if(sel_bl1 == "Brandenburg") sel_f1 <- r$gen2_f
#     if(sel_bl1 == "Bremen") sel_f1 <- r$gen3_f
#     if(sel_bl1 == "Mecklenburg-Vorpommern") sel_f1 <- r$gen4_f
#     if(sel_bl1 == "Niedersachsen") sel_f1 <- r$gen5_f
#     if(sel_bl1 == "Saarland") sel_f1 <- r$gen6_f
#     if(sel_bl1 == "Sachsen-Anhalt") sel_f1 <- r$gen7_f
#     if(sel_bl1 == "Schleswig-Holstein") sel_f1 <- r$gen8_f
#     if(sel_bl1 == "Thüringen") sel_f1 <- r$gen9_f
#   }
#
#   # filter dataset based on UI inputs
#   df <- dplyr::tbl(con, from = "studierende_detailliert") %>%
#     dplyr::filter(jahr %in% timerange,
#                   region==sel_bl1,
#                   fach==sel_f1)%>%
#     dplyr::collect()
#
#   df_gen <- df %>%
#     tidyr::pivot_wider(names_from = geschlecht, values_from = wert) %>%
#     dplyr::mutate(across(c("Männer", "Frauen"), ~ round(./Gesamt*100,1)))%>%
#     dplyr::select(-Gesamt)%>%
#     tidyr::pivot_longer(c("Männer", "Frauen"), names_to = "geschlecht", values_to  = "proportion")%>%
#     dplyr::filter(indikator !="internationale Studienanfänger:innen (1. Hochschulsemester)"&indikator!= "internationale Studierende",
#                   !(indikator %in% c("ausländische Studienanfänger:innen (1. Hochschulsemester)", "ausländische Studierende")))
#
#   df <- df %>%
#     tidyr::pivot_wider(names_from = geschlecht, values_from = wert) %>%
#     dplyr::select(-Gesamt)%>%
#     tidyr::pivot_longer(c("Männer", "Frauen"), values_to = "wert", names_to = "geschlecht")%>%
#     dplyr::right_join(df_gen)%>%
#     dplyr::filter(!is.nan(proportion))
#
#
#   #Trennpunkte für lange Zahlen ergänzen
#   df$display_abs <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
#   df$display_rel <- prettyNum(df$proportion, big.mark = ".", decimal.mark = ",")
#
#   #überschrift vorbereiten
#   fach_label <- sel_f1
#   fach_label <- ifelse(fach_label == "Alle MINT-Fächer", "MINT", fach_label)
#
#
#   out <- highcharter::hchart(df, 'bar', highcharter::hcaes(x = indikator, y=proportion, group = geschlecht))%>%
#
#     highcharter::hc_tooltip(pointFormat = "{point.geschlecht}-Anteil: {point.display_rel} % <br> Anzahl: {point.display_abs}")%>%
#
#     highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"),  reversedStacks =  FALSE) %>%
#     highcharter::hc_xAxis(title = list(text = "")
#                           #                       , categories=c("Studienanfänger:innen in MINT",
#                           #                                                             "Studienanfänger:innen in anderen Studiengängen",
#                           #                                                             "Studierende in MINT",
#                           #                                                             "Studierende in anderen Studiengängen",
#                           #                                                             "Lehrarmt-Studienanfänger:innen in MINT",
#                           #                                                             "Lehramt-Studienanfänger:innen in anderen Studiengängen",
#                           #                                                             "Lehramt-Studierende in MINT",
#                           #                                                             "Lehramt-Studierende in anderen Studiengängen"
#                           # )
#     ) %>%
#     highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
#     highcharter::hc_colors(c("#154194", "#efe8e6")) %>%
#     highcharter::hc_title(text = ifelse(sel_f1 == "Alle Nicht MINT-Fächer", paste0("Frauenanteil in allen Studienfachgruppen außer MINT in ", sel_bl1, " (", timerange, ")"), paste0("Frauenanteil in der Studienfachgruppe ", fach_label, " in ", sel_bl1, " (", timerange, ")")),
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
#
# return(out)
# }



# funktion 2 ----


#'
#' studienzahl_einstieg_pie <- function(df,r) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_studierende_einstieg
#'
#'   lehramt <- r$nurLehramt_studierende_einstieg
#'
#'   hochschulform_select_1 <- r$hochschulform_studierende_einstieg_1
#'
#'   hochschulform_select_2 <- r$hochschulform_studierende_einstieg_2
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr == timerange)
#'
#'   # remove
#'   df <- df %>% dplyr::filter(region == "Deutschland")
#'
#'   # aggregate MINT
#'   df2 <- calc_share_MINT(df[(df$indikator == "Studierende"), ])
#'
#'   df3 <- calc_share_MINT(df[(df$indikator == "Studienanfänger:innen"), ])
#'
#'
#'   df <- rbind(df2, df3)
#'
#'   df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")
#'
#'   if(lehramt == FALSE){
#'
#'     df <- df %>% dplyr::filter(nur_lehramt == "Nein")
#'
#'     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
#'
#'     df$indikator <- paste0(df$indikator, " (", df$hochschulform, ")")
#'
#'     df_studierende <- df %>% dplyr::filter(indikator == paste0("Studierende", " (", df$hochschulform, ")"))
#'
#'     # calculate proportions
#'     df_studierende <- share_pie(df_studierende)
#'
#'     df_anfaenger <- df %>% dplyr::filter(indikator == paste0("Studienanfänger:innen", " (", df$hochschulform, ")"))
#'
#'     # calculate proportions
#'     df_anfaenger <- share_pie(df_anfaenger)
#'
#'   } else {
#'
#'     df <- df %>% dplyr::filter(nur_lehramt == "Ja")
#'
#'     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
#'
#'     df$indikator <- paste0(df$indikator, " (", "Lehramt, " ,df$hochschulform, ")")
#'
#'     df_studierende <- df %>% dplyr::filter(indikator == paste0("Studierende", " (", "Lehramt, " ,df$hochschulform, ")"))
#'
#'     # calculate proportions
#'     df_studierende <- share_pie(df_studierende)
#'
#'     df_anfaenger <- df %>% dplyr::filter(indikator == paste0("Studienanfänger:innen", " (", "Lehramt, " ,df$hochschulform, ")"))
#'
#'     # calculate proportions
#'     df_anfaenger <- share_pie(df_anfaenger)
#'
#'
#'   }
#'
#'
#'     plot_studierende <- highcharter::hchart(df_studierende, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
#'       highcharter::hc_tooltip(
#'         pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#'       highcharter::hc_title(text = paste0("Studienfachwahl (Studierende)", br(), timerange),
#'                             margin = 45,
#'                             align = "center",
#'                             style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#'       highcharter::hc_chart(
#'         style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
#'       highcharter::hc_legend(enabled = TRUE) %>%
#'       highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#'                                              dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE))
#'
#'
#'   plot_anfeanger <- highcharter::hchart(df_anfaenger, size = 280, type = "pie", mapping = highcharter::hcaes(x = fachbereich, y = proportion)) %>%
#'       highcharter::hc_tooltip(
#'         pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#'       highcharter::hc_title(text = paste0("Studienfachwahl (Studienanfänger:innen)", br(), timerange),
#'                             margin = 45,
#'                             align = "center",
#'                             style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#'       highcharter::hc_chart(
#'         style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
#'     highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
#'     highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#'                                              dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE))
#'
#'
#'   plot_anfeanger <- plot_anfeanger %>% highcharter::hc_colors(c("#efe8e6","#b16fab"))
#'
#'   plot_studierende <- plot_studierende %>% highcharter::hc_colors(c("#efe8e6","#b16fab"))
#'
#'     highcharter::hw_grid(
#'
#'       plot_anfeanger,
#'       plot_studierende,
#'
#'       ncol = 2,
#'       browsable = TRUE
#'     )
#'
#'
#'
#'
#' }
#'
#'
# funktion 3 ----
#'
#' #' A function to return a filtered dataset
#' #'
#' #' @description A function to similar to 'studienzahl_einstieg_bar' but with the
#' #' difference that it returns a dataframe instead of plot.
#' #'
#' #' @return The return value is a dataframe
#' #' @param df The dataframe "Studierende.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' data_einstieg <- function(df,r) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_studierende_einstieg
#'
#'   lehramt_enthalten <- r$nurLehramt_studierende_einstieg
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr == timerange)
#'
#'   df <- df %>% dplyr::filter(region == "Deutschland")
#'
#'   # call function to calculate the share of MINT and the remaining subjects
#'   df <- calc_share_MINT(df)
#'
#'   # calculate the share of males
#'   df <- calc_share_male(df, "box_1")
#'
#'   # filter gender
#'
#'   if(isTRUE(lehramt_enthalten)){
#'
#'    # calculate the share of teacher
#'    df <- calc_share_teacher(df)
#'
#'    df$hochschulform <- NULL
#'
#'    colnames(df) <- c("Region", "Geschlecht", "Wert", "Indikator", "Lehramt", "Fachbereich", "Jahr", "Bereich")
#'
#'     return(df)
#'
#'
#'   }else{
#'
#'     df <- df %>% dplyr::filter(nur_lehramt != "Ja")
#'
#'     df <- df %>% dplyr::filter(hochschulform == "insgesamt")
#'
#'     df$hochschulform <- NULL
#'
#'     colnames(df) <- c("Region", "Geschlecht", "Wert", "Indikator", "Lehramt", "Fachbereich", "Jahr", "Bereich")
#'
#'     return(df)
#'
#'   }
#'
#' }
#'
#'
#'
# funktion 4 ----
#'
#' #' A function to plot a waffle chart :: b3
#' #'
#' #' @description A function to create a waffle chart for the second box inside the
#' #' tab "Studium".
#' #'
#' #' @return The return value is a waffle chart
#' #' @param df The dataframe "Studierende.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' studienzahl_waffle_alternative <- function(df,r) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_studierende
#'
#'   lehramt <- r$nurLehramt_studierende
#'
#'   hochschulform_select_1 <- r$hochschulform_studierende_1
#'
#'   hochschulform_select_2 <- r$hochschulform_studierende_2
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr == timerange)
#'
#'
#'   if(lehramt == FALSE){
#'
#'     df <- df %>% dplyr::filter(nur_lehramt == "Nein")
#'
#'     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
#'
#'   } else {
#'
#'     df <- df %>% dplyr::filter(nur_lehramt == "Ja")
#'
#'     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
#'   }
#'
#'   df <- df %>% dplyr::filter(region == "Deutschland")
#'
#'   df <- df %>% dplyr::filter(fachbereich != "Alle")
#'
#'   # calculate the share of males
#'   df <- calc_share_male(df, "box_2")
#'
#'   # calculate proportions
#'   x_studierende <- prep_studium_proportion(df[df$indikator == "Studierende",])
#'
#'   x_studienanfaenger <- prep_studium_proportion(df[df$indikator == "Studienanfänger:innen",])
#'
#'   # set order
#'   x_studierende <- x_studierende[order(factor(names(x_studierende), levels = c('Frauen (Ingenieur)', 'Frauen (Mathe)',
#'                                                                                'Männer (Ingenieur)', 'Männer (Mathe)')))]
#'
#'   x_studienanfaenger <- x_studienanfaenger[order(factor(names(x_studienanfaenger),
#'                                                         levels = c('Frauen (Ingenieur)', 'Frauen (Mathe)',
#'                                                                    'Männer (Ingenieur)', 'Männer (Mathe)')))]
#'
#'
#'   # create plot objects for waffle charts
#'   waffle_studierende_female <- waffle::waffle(x_studierende, keep = FALSE) +
#'     ggplot2::labs(
#'       fill = "",
#'       title = paste0("<span style='color:black;'>", "**Studierende**</span>")) +
#'     ggplot2::theme(plot.title = ggtext::element_markdown(),
#'                    plot.subtitle = ggtext::element_markdown(),
#'                    text = ggplot2::element_text(size = 14),
#'                    plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
#'                    legend.position = "bottom") +
#'     ggplot2::scale_fill_manual(
#'       values =  c("#00a87a",
#'                   "#fcc433",
#'                   "#b1b5c3",
#'                   '#b1b5c3'),
#'       na.value="#b1b5c3",
#'       limits = c("Frauen (Ingenieur)", "Frauen (Mathe)", "Männer (Ingenieur)"),
#'       guide = ggplot2::guide_legend(reverse = TRUE),
#'       labels = c(
#'         paste0("Frauen (Ingenieur)",", ",x_studierende[1], "%"),
#'         paste0("Frauen (Mathe)",", ",x_studierende[2], "%"),
#'         paste0("Männer (MINT)",", ",x_studierende[3] + x_studierende[4], "%"))) +
#'     ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))
#'
#'
#'
#'   waffle_studienanfaenger_female <- waffle::waffle(x_studienanfaenger, keep = FALSE) +
#'     ggplot2::labs(
#'       fill = "",
#'       title = paste0("<span style='color:black;'>", "**Studienanfänger:innen**</span>")) +
#'     ggplot2::theme(plot.title = ggtext::element_markdown(),
#'                    plot.subtitle = ggtext::element_markdown(),
#'                    text = ggplot2::element_text(size = 14),
#'                    plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
#'                    legend.position = "bottom") +
#'     ggplot2::scale_fill_manual(
#'       values =  c("#00a87a",
#'                   "#fcc433",
#'                   "#b1b5c3",
#'                   '#b1b5c3'),
#'       na.value="#b1b5c3",
#'       limits = c("Frauen (Ingenieur)", "Frauen (Mathe)", "Männer (Ingenieur)"),
#'       guide = ggplot2::guide_legend(reverse = TRUE),
#'       labels = c(
#'         paste0("Frauen (Ingenieur)",", ",x_studienanfaenger[1], "%"),
#'         paste0("Frauen (Mathe)",", ",x_studienanfaenger[2], "%"),
#'         paste0("Männer (MINT)",", ",x_studienanfaenger[3] + x_studienanfaenger[4], "%"))) +
#'     ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))
#'
#'
#'
#'   if(lehramt == FALSE){
#'
#'     title_help_sub_sub <- ""
#'
#'     hochschulform <- hochschulform_select_1
#'
#'   } else {
#'
#'     title_help_sub_sub <- " (nur Lehramt)"
#'
#'     hochschulform <- hochschulform_select_2
#'
#'   }
#'
#'   if (hochschulform == "Uni"){
#'
#'     title_help_sub <- "an einer Uni"
#'
#'   }else if (hochschulform == "FH"){
#'
#'     title_help_sub <- "an einer FH"
#'
#'   } else {
#'
#'     title_help_sub <- "insgesamt"
#'
#'   }
#'
#'
#'
#'   title_help <- paste0(title_help_sub, title_help_sub_sub)
#'
#'   plot <- ggpubr::ggarrange(waffle_studienanfaenger_female, NULL ,waffle_studierende_female,
#'                             widths = c(1, 0.1, 1), nrow=1)
#'
#'   text <- c(
#'     paste0("<span style='font-size:20.5pt; color:black'> Anteil von Frauen und Männern an MINT ", title_help,
#'            " in ",timerange))
#'   ggpubr::annotate_figure(plot, gridtext::richtext_grob(text = text))
#'
#'
#' }
#'
#'
# funktion 5 ----
#' #' A function to create a paired bar plot
#' #'
#' #' @description A function to return a paired bar plot for the second box inside
#' #' the tab "Studium
#' #'
#' #' @return The return value is a bar plot
#' #' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' studienzahl_absolut <- function(df,r) {
#'
#'   timerange <- r$date_studierende
#'
#'   lehramt <- r$nurLehramt_studierende
#'
#'   hochschulform_select_1 <- r$hochschulform_studierende_1
#'
#'   hochschulform_select_2 <- r$hochschulform_studierende_2
#'
#'
#'   df <- df %>% dplyr::filter(jahr == timerange)
#'
#'
#'   if(lehramt == FALSE){
#'
#'     df <- df %>% dplyr::filter(nur_lehramt == "Nein")
#'
#'     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
#'
#'   } else {
#'
#'     df <- df %>% dplyr::filter(nur_lehramt == "Ja")
#'
#'     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
#'   }
#'
#'
#'   df <- df %>% dplyr::filter(region == "Deutschland")
#'
#'
#'   # calculate the share of males
#'   df <- calc_share_male(df, type = "box_2")
#'
#'   if(lehramt == FALSE){
#'
#'
#'     title_help_sub_sub <- ""
#'
#'     hochschulform <- hochschulform_select_1
#'
#'   } else {
#'
#'     title_help_sub_sub <- " (nur Lehramt)"
#'
#'     hochschulform <- hochschulform_select_2
#'
#'   }
#'
#'   if (hochschulform == "Uni"){
#'
#'     title_help_sub <- "an einer Uni"
#'
#'   }else if (hochschulform == "FH"){
#'
#'     title_help_sub <- "an einer FH"
#'
#'   } else {
#'
#'     title_help_sub <- "insgesamt"
#'
#'   }
#'
#'
#'   title_help <- paste0(title_help_sub, title_help_sub_sub)
#'
#'
#'   df <- df %>% dplyr::filter(fachbereich != "Alle")
#'
#'   options(scipen=999)
#'
#'   # plot
#'   ggplot2::ggplot(df, ggplot2::aes(x=indikator, y=wert, fill = fachbereich)) +
#'     ggplot2::geom_bar(stat="identity", position = "dodge") +
#'     ggplot2::geom_text(ggplot2::aes(label=wert, vjust = - 0.25),
#'                        position=ggplot2::position_dodge(width=0.9),
#'                        fontface = "bold") +
#'     ggplot2::theme_minimal() +
#'     ggplot2::facet_grid(~ anzeige_geschlecht) +
#'     ggplot2::theme_minimal(
#'       strip.background = ggplot2::element_blank(),
#'       text = ggplot2::element_text(size = 14),
#'       plot.title = ggtext::element_markdown(hjust = 0.5)) +
#'     ggplot2::xlab("") + ggplot2::ylab("Anzahl") +
#'     ggplot2::scale_fill_manual(values = c("#00a87a", "#fcc433")) +
#'     ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
#'                                  "Frauen und Männer in MINT und allen anderen Studienfächern ",title_help,
#'                                  " in ", timerange,
#'                                  "<br><br><br>"),
#'                   fill = "")
#'
#'
#'
#' }
#'
# funktion 6 ----
#'
#' #' A function to plot the german map
#' #'
#' #' @description A function to plot the german map with all states that contain
#' #' information about the share of women in STEM
#' #'
#' #' @return The return value is the german map with information
#' #' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' studienzahl_map <- function(df,r) {
#'
#'   timerange <- r$date_studierende
#'
#'   lehramt <- r$nurLehramt_studierende
#'
#'   hochschulform_select_1 <- r$hochschulform_studierende_1
#'
#'   hochschulform_select_2 <- r$hochschulform_studierende_2
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr == timerange)
#'
#'   if(lehramt == FALSE){
#'
#'     df <- df %>% dplyr::filter(nur_lehramt == "Nein")
#'
#'     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
#'
#'   } else {
#'
#'     df <- df %>% dplyr::filter(nur_lehramt == "Ja")
#'
#'     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
#'   }
#'
#'   # remove
#'   df <- df %>% dplyr::filter(region != "Deutschland")
#'
#'
#'   df <- df %>% dplyr::filter(region != "Bayern")
#'
#'   df <- df %>% dplyr::filter(region != "Baden-Württemberg")
#'
#'   # call function to calculate the share of MINT and the remaining subjects
#'   df <- calc_share_MINT(df)
#'
#'   df <- df %>% dplyr::filter(fachbereich != "andere Studiengänge")
#'
#'   # calculate the proportions
#'   values_studierende <- (df[(df$anzeige_geschlecht == "Frauen" & df$indikator == "Studierende"),
#'                 "wert"]/df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Studierende"), "wert"])*100
#'
#'   values_studierende$region <- df[(df$anzeige_geschlecht == "Frauen" & df$indikator == "Studierende"), "region"][[1]]
#'
#'   values_studienanfaenger <- (df[(df$anzeige_geschlecht == "Frauen" & df$indikator == "Studienanfänger:innen"),
#'                             "wert"]/df[(df$anzeige_geschlecht == "Gesamt" & df$indikator == "Studienanfänger:innen"), "wert"])*100
#'
#'   values_studienanfaenger$region <- df[(df$anzeige_geschlecht == "Frauen" & df$indikator == "Studienanfänger:innen"), "region"][[1]]
#'
#'
#'   if(lehramt == FALSE){
#'
#'
#'     title_help_sub_sub <- ""
#'
#'     hochschulform <- hochschulform_select_1
#'
#'   } else {
#'
#'     title_help_sub_sub <- " (nur Lehramt)"
#'
#'     hochschulform <- hochschulform_select_2
#'
#'   }
#'
#'   if (hochschulform == "Uni"){
#'
#'     title_help_sub <- "an einer Uni"
#'
#'   }else if (hochschulform == "FH"){
#'
#'     title_help_sub <- "an einer FH"
#'
#'   } else {
#'
#'     title_help_sub <- "insgesamt"
#'
#'   }
#'
#'
#'   title_help <- paste0(title_help_sub, title_help_sub_sub)
#'
#'
#'   highcharter::hw_grid(
#'     # plot
#'     highcharter::hcmap(
#'       "countries/de/de-all",
#'       data = values_studierende,
#'       value = "wert",
#'       joinBy = c("name", "region"),
#'       borderColor = "#FAFAFA",
#'       name = "Anteil Frauen an MINT",
#'       borderWidth = 0.1,
#'       nullColor = "#A9A9A9",
#'       tooltip = list(
#'         valueDecimals = 0,
#'         valueSuffix = "%"
#'       ),
#'       download_map_data = FALSE
#'     ) %>%
#'       highcharter::hc_colorAxis(min=0, max=60, labels = list(format = "{text}%")) %>%
#'       highcharter::hc_title(
#'         text = paste0("Anteil der Studentinnen <br>",title_help ," an MINT in ", timerange),
#'         margin = 10,
#'         align = "center",
#'         style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
#'       ) %>%
#'        highcharter::hc_caption(
#'          text = "...",  style = list(color= "white", fontSize = "12px")
#'        ) %>%
#'       highcharter::hc_chart(
#'         style = list(fontFamily = "SourceSans3-Regular")
#'       ) %>% highcharter::hc_size(600, 550) %>%
#'       highcharter::hc_credits(enabled = FALSE) %>%
#'       highcharter::hc_legend(layout = "horizontal", floating = FALSE,
#'                              verticalAlign = "bottom"),
#'
#'     highcharter::hcmap(
#'       "countries/de/de-all",
#'       data = values_studienanfaenger,
#'       value = "wert",
#'       joinBy = c("name", "region"),
#'       borderColor = "#FAFAFA",
#'       name = "Anteil Frauen an MINT",
#'       borderWidth = 0.1,
#'       nullColor = "#A9A9A9",
#'       tooltip = list(
#'         valueDecimals = 0,
#'         valueSuffix = "%"
#'       ),
#'       download_map_data = FALSE
#'     ) %>%
#'       highcharter::hc_title(
#'         text = paste0("Anteil der Studienanfänger:inneninnen <br> ",title_help ," an MINT in ", timerange),
#'         margin = 10,
#'         align = "center",
#'         style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
#'       ) %>%
#'        highcharter::hc_caption(
#'          text = "Ausgegraut: Daten stehen nicht zur Verfügung",  style = list(fontSize = "12px")
#'        ) %>%
#'       highcharter::hc_chart(
#'         style = list(fontFamily = "SourceSans3-Regular")
#'       ) %>% highcharter::hc_size(600, 550) %>%
#'       highcharter::hc_credits(enabled = FALSE) %>%
#'       highcharter::hc_legend(layout = "horizontal", floating = FALSE, verticalAlign = "bottom"),
#'
#'
#'     ncol = 2,
#'     browsable = TRUE
#'   )
#'
#'
#'
#' }
#'
# funktion 7 ----
#' #' A function to return a filtered dataset
#' #'
#' #' @description A function to create a dataframe for given filter
#' #'
#' #' @return The return value is a dataframe
#' #' @param df The dataframe "Studierende.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' data_mix_studium <- function(df,r) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_studierende
#'
#'   lehramt <- r$nurLehramt_studierende
#'
#'   hochschulform_select_1 <- r$hochschulform_studierende_1
#'
#'   hochschulform_select_2 <- r$hochschulform_studierende_2
#'
#'   df <- df %>% dplyr::filter(jahr == timerange)
#'
#'
#'   if(lehramt == FALSE){
#'
#'     df <- df %>% dplyr::filter(nur_lehramt == "Nein")
#'
#'     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
#'
#'   } else {
#'
#'     df <- df %>% dplyr::filter(nur_lehramt == "Ja")
#'
#'     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
#'   }
#'
#'
#'
#'   # calculate the share of males
#'   values <- df %>%
#'     dplyr::group_by(region, fachbereich, indikator, nur_lehramt, hochschulform, jahr, bereich) %>%
#'     dplyr::mutate(wert = wert - dplyr::lead(wert)) %>% dplyr::select(wert) %>% na.omit()
#'
#'   values$anzeige_geschlecht <- "Männer"
#'
#'   values <- values[, colnames(df)]
#'
#'   df <- rbind(df, values)
#'
#'   colnames(df) <- c("Region", "Geschlecht", "Wert", "Indikator", "Lehramt", "Hochschulform", "Fachbereich", "Jahr", "Bereich")
#'
#'   return(df)
#' }
#'
# funktion 8 ----
#' #' A function to plot time series
#' #'
#' #' @description A function to plot the time series of the german states
#' #'
#' #' @return The return value, if any, from executing the function.
#' #' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' studienzahl_verlauf <- function(df,r) {
#'
#'   # load UI inputs from reactive value
#'   status_studierende <- r$indikator_studierende_verlauf
#'
#'   timerange <- r$date_studierende_verlauf
#'
#'   lehramt <- r$nurLehramt_studierende_verlauf
#'
#'   states <- r$states_studierende_verlauf
#'
#'   topic <- r$topic_studierende_verlauf
#'
#'   subject_aggregated <- r$subjects_aggregated
#'
#'   subjects_select <- r$subject_selected
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])
#'
#'   df <- df %>% dplyr::filter(indikator == status_studierende)
#'
#'   # remove
#'   df <- df %>% dplyr::filter(region != "Deutschland")
#'
#'   df <- df %>% dplyr::filter(region != "Bayern")
#'
#'   df <- df %>% dplyr::filter(region != "Baden-Württemberg")
#'
#'   # include "Osten" und "Westen" in Dataframe
#'   df <- prep_studierende_east_west(df)
#'
#'
#'   if(lehramt == FALSE){
#'
#'     df <- df %>% dplyr::filter(nur_lehramt == "Nein")
#'
#'     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
#'
#'     title_help_sub_sub <- " insgesamt"
#'
#'   } else {
#'
#'     df <- df %>% dplyr::filter(nur_lehramt == "Ja")
#'
#'     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
#'
#'     title_help_sub_sub <- " an einer Uni (nur Lehramt)"
#'   }
#'
#'
#'   if (subject_aggregated == "aggregiert"){
#'
#'     # call function to calculate the share of MINT and the remaining subjects
#'     df <- calc_share_MINT(df)
#'
#'     if (topic == "MINT"){
#'
#'       title_help_sub <- "an MINT"
#'
#'     } else {
#'
#'       title_help_sub <- "an anderen Studienfächern"
#'
#'     }
#'
#'   }else {
#'
#'
#'     df <- df %>% dplyr::filter(fachbereich %in% subjects_select)
#'
#'     title_help_sub <- dictionary_title_studium[[subjects_select]]
#'   }
#'
#'
#'   if (status_studierende == "Studierende"){
#'
#'     title_help <- paste0("Studentinnen ", title_help_sub, title_help_sub_sub)
#'
#'   }else{
#'
#'     title_help <- paste0("Studienanfänger:inneninnen ", title_help_sub, title_help_sub_sub)
#'
#'   }
#'
#'
#'   values <-  df %>%
#'     dplyr::group_by(jahr, fachbereich, region) %>%
#'     dplyr::summarise(props = wert[anzeige_geschlecht == "Frauen"] /
#'                     wert[anzeige_geschlecht == "Gesamt"])
#'
#'
#'   if (subject_aggregated == "aggregiert"){
#'     # filter MINT or remaining subjects
#'     values <- values %>% dplyr::filter(fachbereich == topic)
#'   }
#'
#'   # order years for plot
#'   values <- values[with(values, order(region, jahr, decreasing = FALSE)), ]
#'
#'   values$props <- values$props * 100
#'
#'   values <- values %>% dplyr::filter(region %in% states)
#'
#'   # plot
#'   highcharter::hchart(values, 'line', highcharter::hcaes(x = jahr, y = round(props,1), group = region)) %>%
#'     highcharter::hc_tooltip(pointFormat = "Anteil Frauen <br> Bundesland: {point.region} <br> Wert: {point.y} %") %>%
#'     highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%")) %>%
#'     highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
#'     #highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
#'     highcharter::hc_title(text = paste0("Anteil an ", title_help ," im Verlauf"),
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
#'
#'
#'
#'
#'
#'
# funktion 9 ----
#'
#' #' A function to return a filtered dataset
#' #'
#' #' @description A function to create a dataframe for given filter
#' #'
#' #' @return The return value is a dataframe
#' #' @param df The dataframe "Studierende.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' data_verlauf_studium <- function(df,r) {
#'
#'   # load UI inputs from reactive value
#'   status_studierende <- r$indikator_studierende_verlauf
#'
#'   timerange <- r$date_studierende_verlauf
#'
#'   lehramt <- r$nurLehramt_studierende_verlauf
#'
#'   states <- r$states_studierende_verlauf
#'
#'   topic <- r$topic_studierende_verlauf
#'
#'   ost_west <- r$ost_west
#'
#'   subject_aggregated <- r$subjects_aggregated
#'
#'   subjects_select <- r$subject_selected
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])
#'
#'   df <- df %>% dplyr::filter(indikator == status_studierende)
#'
#'   # remove
#'   df <- df %>% dplyr::filter(region != "Deutschland")
#'
#'   df <- df %>% dplyr::filter(region != "Bayern")
#'
#'   df <- df %>% dplyr::filter(region != "Baden-Württemberg")
#'
#'   if(lehramt == FALSE){
#'
#'     df <- df %>% dplyr::filter(nur_lehramt == "Nein")
#'
#'     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
#'
#'   } else {
#'
#'     df <- df %>% dplyr::filter(nur_lehramt == "Ja")
#'
#'     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
#'   }
#'
#'
#'   if (subject_aggregated == "aggregiert"){
#'
#'     # call function to calculate the share of MINT and the remaining subjects
#'     df <- calc_share_MINT(df)
#'
#'     if (topic == "MINT"){
#'
#'       title_help <- "MINT"
#'
#'     } else {
#'
#'       title_help <- "anderen Studienfächern"
#'
#'     }
#'
#'   }else {
#'
#'
#'     df <- df %>% dplyr::filter(fachbereich %in% subjects_select)
#'
#'     title_help <- dictionary_title_studium[[subjects_select]]
#'   }
#'
#'   # order
#'   df <- df[with(df, order(region, fachbereich, jahr, decreasing = TRUE)), ]
#'
#'   # calculate proportion
#'   values <- df %>%
#'     dplyr::group_by(jahr, fachbereich, region, indikator, nur_lehramt, hochschulform, bereich) %>%
#'     dplyr::mutate(wert = dplyr::lead(wert)/wert) %>% dplyr::select(wert) %>% na.omit()
#'
#'
#'   if (ost_west == FALSE) {
#'
#'     values <- values %>% dplyr::filter(region %in% states)
#'
#'   } else{
#'
#'     values$dummy_west <- ifelse(values$region %in% states_east_west$west, "Westen", "Osten")
#'
#'     values <- values %>% dplyr::group_by(jahr, fachbereich, dummy_west,
#'                                          indikator, nur_lehramt, hochschulform, bereich) %>%
#'       dplyr::summarise(wert = mean(wert))
#'
#'     names(values)[3] <- "region"
#'   }
#'
#'   if (subject_aggregated == "aggregiert"){
#'     # filter MINT or remaining subjects
#'     values <- values %>% dplyr::filter(fachbereich == topic)
#'   }
#'
#'   # order years for plot
#'   values <- values[with(values, order(region, jahr, decreasing = FALSE)), ]
#'
#'   values$wert <- round(values$wert * 100,1)
#'
#'   values$wert <- paste0(values$wert,"%")
#'
#'   colnames(values) <- c("Jahr", "Fachbereich", "Region", "Indikator", "Lehramt", "Hochschulform", "Bereich", "Wert")
#'
#'
#'   return(values)
#' }
#'
# funktion 10 ----
#'
#' #' A function to plot time series
#' #'
#' #' @description A function to plot the time series of the german states
#' #'
#' #' @return The return value, if any, from executing the function.
#' #' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' studienzahl_verlauf_bl <- function(df,r) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_studierende_verlauf_bl
#'
#'   lehramt <- r$nurLehramt_studierende_verlauf_bl
#'
#'   states <- r$states_studierende_verlauf_bl
#'
#'   topic <- r$topic_studierende_verlauf_bl
#'
#'   subject_aggregated <- r$subjects_aggregated_bl
#'
#'   subjects_select <- r$subject_selected_bl
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])
#'
#'   # remove - Muss BY und BW nicht entfernen, da MINT vs nicht-MINT in Daten enthalten
#'   df <- df %>% dplyr::filter(region != "Deutschland")
#'
#'   #df <- df %>% dplyr::filter(region != "Bayern")
#'
#'   #df <- df %>% dplyr::filter(region != "Baden-Württemberg")
#'
#'   #damit alle selbe Srucktur haben; Frauen nicht relevant hier
#'   df <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt")
#'
#'   # include "Osten" und "Westen" in Dataframe
#'   df <- prep_studierende_east_west(df)
#'
#'
#'   if(lehramt == FALSE){
#'
#'     df <- df %>% dplyr::filter(nur_lehramt == "Nein")
#'
#'     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
#'
#'     title_help_sub_sub <- " insgesamt"
#'
#'   } else {
#'
#'     df <- df %>% dplyr::filter(nur_lehramt == "Ja")
#'
#'     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
#'
#'     title_help_sub_sub <- " an einer Uni (nur Lehramt)"
#'   }
#'
#'
#'   if (subject_aggregated == "aggregiert"){
#'
#'     # call function to calculate the share of MINT and the remaining subjects
#'     df <- calc_share_MINT_bl(df)
#'
#'     if (topic == "MINT"){
#'
#'       title_help_sub <- "an MINT"
#'
#'     } else {
#'
#'       title_help_sub <- "an anderen Studienfächern"
#'
#'     }
#'
#'   }else {
#'
#'
#'     df <- df %>% dplyr::filter(fachbereich %in% subjects_select)
#'
#'   }
#'
#'   # # calculate share of males for states
#'   # df <- calc_share_male_bl(df)
#'   #
#'   # # calculate new "Gesamt"
#'   # df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
#'   #   dplyr::group_by(region, fachbereich, indikator, jahr) %>%
#'   #   dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
#'   #                   wert[anzeige_geschlecht == "Männer"])
#'
#'   # calculate proportions
#'   values <- df %>% dplyr::group_by(region, indikator, fachbereich, anzeige_geschlecht, jahr) %>%
#'     dplyr::summarize(proportion = wert/props)
#'
#'
#'   if (subject_aggregated == "aggregiert"){
#'     # filter MINT or remaining subjects
#'     values <- values %>% dplyr::filter(fachbereich == topic)
#'   }
#'
#'   # order years for plot
#'   values <- values[with(values, order(region, jahr, decreasing = FALSE)), ]
#'
#'   values$proportion <- values$proportion * 100
#'
#'   values <- values %>% dplyr::filter(region %in% states)
#'
#'   values$anzeige_geschlecht <- paste0(values$anzeige_geschlecht, " (", values$indikator, ")")
#'
#'   # plot
#'   highcharter::hchart(values, 'line', highcharter::hcaes(x = jahr, y = round(proportion,1), group = anzeige_geschlecht)) %>%
#'     highcharter::hc_tooltip(pointFormat = "Anteil {point.anzeige_geschlecht} <br> Wert: {point.y} %") %>%
#'     highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%")) %>%
#'     highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
#'     #highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
#'     highcharter::hc_title(text = paste0("Anteil von Student:innen im Verlauf"),
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
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'   # # df <- df %>% dplyr::filter(fachbereich != "Alle Fächer")
#'   #
#'   # df <- df %>% dplyr::filter(region != "Bayern")
#'   #
#'   # df <- df %>% dplyr::filter(region != "Baden-Württemberg")
#'   #
#'   # # include "Osten" und "Westen" in Dataframe
#'   # df <- prep_studierende_east_west(df)
#'   #
#'   # df <- df %>% dplyr::filter(region %in% states)
#'   #
#'   # if(lehramt == FALSE){
#'   #
#'   #   df <- df %>% dplyr::filter(nur_lehramt == "Nein")
#'   #
#'   #   df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
#'   #
#'   # } else {
#'   #
#'   #   df <- df %>% dplyr::filter(nur_lehramt == "Ja")
#'   #
#'   #   df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
#'   #
#'   # }
#'   #
#'   # # aggregate all subjects to calculate proportion later
#'   # df_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen",
#'   #                                   fachbereich == "Alle") %>%
#'   #   dplyr::group_by(region, anzeige_geschlecht, indikator, nur_lehramt, hochschulform, jahr) %>%
#'   #   dplyr::mutate(wert_gesamt = sum(wert)) %>%
#'   #   dplyr::select(c("region", "indikator", "nur_lehramt",
#'   #                   "hochschulform", "jahr", "wert_gesamt"))
#'   #
#'   # # aggregate to MINT
#'   # values_Mint <- df %>%
#'   #   dplyr::filter(fachbereich != "Alle") %>%
#'   #   dplyr::group_by(jahr, region, indikator, anzeige_geschlecht, hochschulform,
#'   #                   nur_lehramt) %>%
#'   #   dplyr::summarise(wert = sum(wert)) %>%
#'   #   dplyr::mutate(bereich = "Hochschule",
#'   #                 fachbereich = "MINT (aggregiert)") %>%
#'   #   dplyr::filter(anzeige_geschlecht == "Frauen")
#'   #
#'   # einzelne_faecher <- df %>%
#'   #   dplyr::filter(anzeige_geschlecht == "Frauen")
#'   #
#'   # df_andere <- calc_share_MINT(df) %>%
#'   #   dplyr::filter(fachbereich == "andere Studiengänge",
#'   #                 anzeige_geschlecht == "Frauen")
#'   #
#'   # df <- rbind(values_Mint, einzelne_faecher, df_andere)
#'   #
#'   # # # calculate proportion
#'   # values <- df %>%
#'   #   dplyr::left_join(df_gesamt, by = c("region", "indikator", "nur_lehramt",
#'   #                                      "hochschulform", "jahr")) %>%
#'   #   dplyr::select(-"anzeige_geschlecht.y") %>%
#'   #   dplyr::rename(anzeige_geschlecht = "anzeige_geschlecht.x") %>%
#'   #   dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
#'   #   dplyr::select(-c("wert","wert_gesamt")) %>%
#'   #   dplyr::filter(fachbereich != "Alle",
#'   #                 fachbereich != "andere Studiengänge")
#'   #
#'   #
#'   # # order years for plot
#'   # values <- values[with(values, order(region, jahr, decreasing = FALSE)), ]
#'   #
#'   # #ifelse falsch
#'   # #values <- values %>% dplyr::mutate(fachbereich = ifelse(fachbereich == "MINT (aggregiert)", "MINT-Fächer (gesamt)", values$fachbereich ))
#'   #
#'   # values$fachbereich <- ifelse(values$fachbereich == "MINT (aggregiert)", "MINT-Fächer (gesamt)", values$fachbereich)
#'   #
#'   #
#'   # #[values$fachbereich=="MINT (aggregiert)"] <- "MINT (gesamt)"
#'   #
#'   # #values$fachbereich  <- gsub("MINT (aggregiert)", "MINT-Fächer (gesamt)", values$fachbereich)
#'   #
#'   # values <- values %>%  dplyr::filter(fachbereich == subjects_select)
#'   #
#'   # values$anzeige_geschlecht <- paste0(values$anzeige_geschlecht, " (", values$indikator, ")")
#'   #
#'   #
#'   # #
#'   #
#'   #
#'   #
#'   # #values[values$fachbereich == "MINT", "fachbereich"] <- "MINT-Fächer (gesamt)"
#'   #
#'   #
#'   #
#'   # #values$fachbereich  <- gsub("MINT (aggregiert)", "MINT-Fächer (gesamt)", values$fachbereich)
#'   #
#'   # help_title <- ifelse(subjects_select == "MINT-Fächer (gesamt)", "MINT-Fächern (gesamt)", subjects_select)
#'
#'
#'   # plot
#'   # highcharter::hchart(values, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = anzeige_geschlecht)) %>%
#'   #   highcharter::hc_tooltip(pointFormat = "Anteil {point.anzeige_geschlecht} <br> Wert: {point.y} %") %>%
#'   #   highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value}%")) %>%
#'   #   highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(fontFamily = "SourceSans3-Regular")) %>%
#'   #   #highcharter::hc_caption(text = "Quelle: ", style = list(fontSize = "12px") ) %>%
#'   #   highcharter::hc_title(text = paste0("Frauen: Studienfachwahl von ", help_title, " in ", states),
#'   #                         margin = 45,
#'   #                         align = "center",
#'   #                         style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#'   #   highcharter::hc_chart(
#'   #     style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
#'   #   ) %>%
#'   #   highcharter::hc_exporting(enabled = FALSE,
#'   #                             buttons = list(contextButton = list(
#'   #                               symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
#'   #                               onclick = highcharter::JS("function () {
#'   #                                                             this.exportChart({ type: 'image/png' }); }"),
#'   #                               align = 'right',
#'   #                               verticalAlign = 'bottom',
#'   #                               theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
#'
# funktion 11 ----
#'
#' #' A function to create a dumbbell plot
#' #'
#' #' @description A function to return a ranking of subject for both genders
#' #'
#' #' @return The return value is a dumbbell plot
#' #' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' studienfaecher_ranking <- function(df,r, type) {
#'
#'
#'   timerange <- r$dumb_date
#'
#'   label_choice <- r$dumbb_l
#'
#'   bl_choice <- r$dumbb_states
#'
#'   dfert <- df %>% dplyr::filter(jahr == timerange)
#'
#'   df89 <- dfert %>%
#'     dplyr::filter(region != "Bayern" & region!= "Baden-Württemberg")%>%
#'     dplyr::select(-indikator,-hochschulform,-quelle, -bereich)%>%
#'     tidyr::pivot_wider(values_from = wert, names_from=fachbereich)%>%
#'     dplyr::mutate("andere Fächer" = Alle- Ingenieurwissenschaften-  Mathematik_Naturwissenschaften)%>%
#'     tidyr::pivot_longer(c(`andere Fächer`, Alle, Ingenieurwissenschaften, Mathematik_Naturwissenschaften ), values_to="wert", names_to = "fachbereich")%>%
#'     tidyr::pivot_wider(names_from = region, values_from = wert)%>%
#'     dplyr::mutate(Westdeutschland=rowSums(dplyr::select(.,c(
#'       Bremen,
#'       Hamburg,Hessen,Niedersachsen,
#'       `Nordrhein-Westfalen`,`Rheinland-Pfalz`,
#'       Saarland,`Schleswig-Holstein`)
#'     ),na.rm = T),
#'     Ostdeutschland=rowSums(dplyr::select(., c(Berlin,
#'                                               Brandenburg,
#'                                               `Mecklenburg-Vorpommern`,
#'                                               Sachsen,`Sachsen-Anhalt`,
#'                                               Thüringen)
#'     ),na.rm = T))%>%
#'     tidyr::pivot_longer(c(`Berlin`:Ostdeutschland), values_to = "wert", names_to="region")%>%
#'
#'     tidyr::pivot_wider(names_from=fachbereich, values_from = wert)%>%
#'     dplyr::rename("Mathematik/Naturwissenschaften"=Mathematik_Naturwissenschaften)%>%
#'     dplyr::mutate(MINT=`Mathematik/Naturwissenschaften`+Ingenieurwissenschaften)%>%
#'     dplyr::mutate(dplyr::across(c(5,8,7,9), ~ round(./Alle*100,1)))%>%
#'     tidyr::pivot_longer(c(5:9), names_to = "fach", values_to= "proportion")%>%
#'     dplyr::filter(geschlecht=="frauen")%>%
#'     dplyr::filter(indikator == "Studierende" | indikator=="Studienanfänger:innen (1.Fachsemester)")%>%
#'     dplyr::filter(fach!= "Alle")
#'
#'     dffn <- df89 %>% dplyr::filter(region == bl_choice)
#'
#'
#'     df2 <- dffn %>% tidyr::pivot_wider(names_from=indikator,values_from = proportion)
#'
#'
#'  # dffn <- df89 %>% dplyr::filter(label == label_choice)
#'
#'
#'
#'   ggplot2::ggplot(df2,
#'                   ggplot2::aes(y = fach)) +
#'     ggplot2::geom_point(data = dffn, ggplot2::aes(x = proportion, color = indikator), size = 5) +
#'     ggalt::geom_dumbbell(
#'       ggplot2::aes(x = `Studienanfänger:innen (1.Fachsemester)`, xend = `Studierende`),
#'       size = 0.5,
#'       size_x = 5,
#'       size_xend = 5,
#'       colour = "black",
#'       colour_x = "#b1b5c366",
#'       colour_xend = "#f5adac66",
#'       dot_guide=TRUE) +
#'     ggplot2::theme_minimal() +
#'     ggplot2::scale_color_manual(name = "", values = c("#b1b5c366", "#f5adac66")) +
#'     ggplot2::theme(legend.position="top",
#'                    panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
#'                    plot.title = ggtext::element_markdown(hjust = 0.5),
#'                    axis.text.y = ggplot2::element_text(size = 11)) +
#'     ggplot2::ylab("") + ggplot2::xlab("") +
#'     ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
#'                                  "Frauen: Studienfachwahl nach Fächergruppen in ", bl_choice, "<br>", timerange,
#'                                  "<br><br><br>"),
#'                   color = "") +
#'     ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))
#'
#'
#'   # load UI inputs from reactive value
#'   # timerange <- r$date_studium_ranking_bl_subject_gender
#'   #
#'   # states <- r$states_studium_ranking_bl_subject_gender
#'   #
#'   # lehramt <- r$nurLehramt_studium_ranking_bl_subject_gender
#'   #
#'   # hochschulform_select_1 <- r$hochschulform_studium_ranking_bl_subject_gender_1
#'   #
#'   # hochschulform_select_2 <- r$hochschulform_studium_ranking_bl_subject_gender_2
#' #
#' #   # filter dataset based on UI inputs
#' #   df <- df %>% dplyr::filter(jahr == timerange)
#' #
#' #   #df <- df %>% dplyr::filter(region != "Deutschland")
#' #
#' #   # df <- df %>% dplyr::filter(fachbereich != "Alle Fächer")
#' #
#' #   df <- df %>% dplyr::filter(region != "Bayern")
#' #
#' #   df <- df %>% dplyr::filter(region != "Baden-Württemberg")
#' #
#' #   # include "Osten" und "Westen" in Dataframe
#' #   df <- prep_studierende_east_west(df)
#' #
#' #   df <- df %>% dplyr::filter(region %in% states)
#' #
#' #   # df <- df %>% dplyr::mutate(indikator = replace(indikator,
#' #   #                                                indikator == "Studienanfänger",
#' #   #                                                "Studienanfängerinnen"))
#' #
#' #   if(lehramt == FALSE){
#' #
#' #     df <- df %>% dplyr::filter(nur_lehramt == "Nein")
#' #
#' #     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
#' #
#' #   } else {
#' #
#' #     df <- df %>% dplyr::filter(nur_lehramt == "Ja")
#' #
#' #     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
#' #
#' #   }
#' #
#' #   # aggregate all subjects to calculate proportion later
#' #   df_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen",
#' #                                     fachbereich == "Alle") %>%
#' #     dplyr::group_by(region, anzeige_geschlecht, indikator, nur_lehramt, hochschulform, jahr) %>%
#' #     dplyr::mutate(wert_gesamt = sum(wert)) %>%
#' #     dplyr::select(c("region", "indikator", "nur_lehramt",
#' #                     "hochschulform", "jahr", "wert_gesamt"))
#' #
#' #   # aggregate to MINT
#' #   values_Mint <- df %>%
#' #     dplyr::filter(fachbereich != "Alle") %>%
#' #     dplyr::group_by(jahr, region, indikator, anzeige_geschlecht, hochschulform,
#' #                     nur_lehramt) %>%
#' #     dplyr::summarise(wert = sum(wert)) %>%
#' #     dplyr::mutate(bereich = "Hochschule",
#' #                   fachbereich = "MINT (aggregiert)") %>%
#' #     dplyr::filter(anzeige_geschlecht == "Frauen")
#' #
#' #   einzelne_faecher <- df %>%
#' #     dplyr::filter(anzeige_geschlecht == "Frauen")
#' #
#' #   df_andere <- calc_share_MINT(df) %>%
#' #     dplyr::filter(fachbereich == "andere Studiengänge",
#' #                   anzeige_geschlecht == "Frauen")
#' #
#' #   df <- rbind(values_Mint, einzelne_faecher, df_andere)
#' #
#' #   # # calculate proportion
#' #   df <- df %>%
#' #     dplyr::left_join(df_gesamt, by = c("region", "indikator", "nur_lehramt",
#' #                                        "hochschulform", "jahr")) %>%
#' #     dplyr::select(-"anzeige_geschlecht.y") %>%
#' #     dplyr::rename(anzeige_geschlecht = "anzeige_geschlecht.x") %>%
#' #     dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
#' #     dplyr::select(-c("wert","wert_gesamt")) %>%
#' #     dplyr::filter(fachbereich != "Alle")
#' #
#' #   df[df$fachbereich == "MINT (aggregiert)","fachbereich"] <- "MINT (gesamt)"
#' #
#' #
#' #   # spread column
#' #   df <- tidyr::spread(df, indikator, proportion)
#' #
#' #   df <- df %>% tidyr::drop_na()
#' #
#' #   df <- df %>% dplyr::select(-hochschulform, -region, -anzeige_geschlecht)
#' #
#' #   df2 <- tidyr::gather(df, group, value, -fachbereich)%>%
#' #     dplyr::filter(group %in% c("Studienanfänger:innen", "Studierende")) %>%
#' #     dplyr::mutate(value = as.numeric(value))
#' #
#' #   df2$fachbereich <- factor(df2$fachbereich, levels = levels(df$fachbereich))
#' #
#' # #hier sind keine Daten mehr in df, wenn Deutschland ausgewählt ist, sonst schon
#' #
#' #   colnames(df)[7] <- "Studienanfängerinnen"
#'
#'
#'   # browser()
#'   # df_lo <-df
#'
#'   #highcharter::hchart(df, 'dumbbell', highcharter::hcaes(y = Studienanfängerinnen, x= Studierende, group ="indikator"))
#'
#' #hier sind dann auf einmal die Werte von Hessen (Default Wahl) in df wenn man Deutschland auswählt,
#'   #und ein Fehler in der App wird angezeigt
#'
#'   # ggplot2::ggplot(df,
#'   #                 ggplot2::aes(y = fachbereich)) +
#'   #   ggplot2::geom_point(data = df2, ggplot2::aes(x = value, color = group), size = 5) +
#'   #   ggalt::geom_dumbbell(
#'   #     ggplot2::aes(x = Studienanfängerinnen, xend = Studierende),
#'   #     size = 0.5,
#'   #     size_x = 5,
#'   #     size_xend = 5,
#'   #     colour = "black",
#'   #     colour_x = "#b1b5c366",
#'   #     colour_xend = "#f5adac66",
#'   #     dot_guide=TRUE) +
#'   #   ggplot2::theme_minimal() +
#'   #   ggplot2::scale_color_manual(name = "", values = c("#b1b5c366", "#f5adac66")) +
#'   #   ggplot2::theme(legend.position="top",
#'   #                  panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
#'   #                  plot.title = ggtext::element_markdown(hjust = 0.5),
#'   #                  axis.text.y = ggplot2::element_text(size = 11)) +
#'   #   ggplot2::ylab("") + ggplot2::xlab("") +
#'   #   ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
#'   #                                "Frauen: Studienfachwahl nach Fächergruppen in ", states, "<br>", timerange,
#'   #                                "<br><br><br>"),
#'   #                 color = "") +
#'   #   ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))
#'
#' }
#' #   browser()
#' #
#' #   # load UI inputs from reactive value
#' #   timerange <- r$date_studium_ranking_bl_subject_gender
#' #
#' #   states <- r$states_studium_ranking_bl_subject_gender
#' #
#' #   lehramt <- r$nurLehramt_studium_ranking_bl_subject_gender
#' #
#' #   hochschulform_select_1 <- r$hochschulform_studium_ranking_bl_subject_gender_1
#' #
#' #   hochschulform_select_2 <- r$hochschulform_studium_ranking_bl_subject_gender_2
#' #
#' #   # filter dataset based on UI inputs
#' #   df <- df %>% dplyr::filter(jahr == timerange)
#' #
#' #   # df <- df %>% dplyr::filter(fachbereich != "Alle Fächer")
#' #
#' #   df <- df %>% dplyr::filter(region != "Bayern")
#' #
#' #   df <- df %>% dplyr::filter(region != "Baden-Württemberg")
#' #
#' #   # include "Osten" und "Westen" in Dataframe
#' #   df <- prep_studierende_east_west(df)
#' #
#' #   df <- df %>% dplyr::filter(region %in% states)
#' #
#' #   df <- df %>% dplyr::mutate(indikator = replace(indikator,
#' #                                                  indikator == "Studienanfänger",
#' #                                                    "Studienanfänger:inneninnen"))
#' #
#' #   if(lehramt == FALSE){
#' #
#' #     df <- df %>% dplyr::filter(nur_lehramt == "Nein")
#' #
#' #     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
#' #
#' #   } else {
#' #
#' #     df <- df %>% dplyr::filter(nur_lehramt == "Ja")
#' #
#' #     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
#' #
#' #   }
#' #
#' #   # aggregate all subjects to calculate proportion later
#' #   df_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen",
#' #                                     fachbereich == "Alle") %>%
#' #     dplyr::group_by(region, anzeige_geschlecht, indikator, nur_lehramt, hochschulform, jahr) %>%
#' #     dplyr::mutate(wert_gesamt = sum(wert)) %>%
#' #     dplyr::select(c("region", "indikator", "nur_lehramt",
#' #                     "hochschulform", "jahr", "wert_gesamt"))
#' #
#' #   # aggregate to MINT
#' #   values_Mint <- df %>%
#' #     dplyr::filter(fachbereich != "Alle") %>%
#' #     dplyr::group_by(jahr, region, indikator, anzeige_geschlecht, hochschulform,
#' #                     nur_lehramt) %>%
#' #     dplyr::summarise(wert = sum(wert)) %>%
#' #     dplyr::mutate(bereich = "Hochschule",
#' #                   fachbereich = "MINT (aggregiert)") %>%
#' #     dplyr::filter(anzeige_geschlecht == "Frauen")
#' #
#' #   einzelne_faecher <- df %>%
#' #     dplyr::filter(anzeige_geschlecht == "Frauen")
#' #
#' #   df_andere <- calc_share_MINT(df) %>%
#' #     dplyr::filter(fachbereich == "andere Studiengänge",
#' #                   anzeige_geschlecht == "Frauen")
#' #
#' #   df <- rbind(values_Mint, einzelne_faecher, df_andere)
#' #
#' #   # # calculate proportion
#' #   df <- df %>%
#' #     dplyr::left_join(df_gesamt, by = c("region", "indikator", "nur_lehramt",
#' #                                        "hochschulform", "jahr")) %>%
#' #     dplyr::select(-"anzeige_geschlecht.y") %>%
#' #     dplyr::rename(anzeige_geschlecht = "anzeige_geschlecht.x") %>%
#' #     dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
#' #     dplyr::select(-c("wert","wert_gesamt")) %>%
#' #     dplyr::filter(fachbereich != "Alle")
#' #
#' #   df1 <- df
#' #
#' #   # spread column
#' #   df <- tidyr::spread(df, indikator, proportion)
#' #
#' #   df <- df %>% tidyr::drop_na()
#' #
#' #   df <- df %>% dplyr::select(-hochschulform, -region, -anzeige_geschlecht)
#' #
#' #   df$group <- df %>% dplyr::mutate(group= ifelse("Studienanfänger:innen", "Studienanfänger", df$group ))
#' #
#' #   df2 <- tidyr::gather(df, group, value, -fachbereich) %>%
#' #     dplyr::filter(group %in% c("Studienanfänger", "Studierende")) %>%
#' #     dplyr::mutate(value = as.numeric(value))
#' #
#' #   df2$fachbereich <- factor(df2$fachbereich, levels = levels(df$fachbereich))
#' #
#' #
#' #
#' #
#' #    ggplot2::ggplot(df,
#' #                   ggplot2::aes(y = fachbereich)) +
#' #     ggplot2::geom_point(data = df2, ggplot2::aes(x = value, color = group), size = 5) +
#' #     ggalt::geom_dumbbell(
#' #       ggplot2::aes(x =Studienanfänger, xend = Studierende),
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
#' #                    panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
#' #                    plot.title = ggtext::element_markdown(hjust = 0.5),
#' #                    axis.text.y = ggplot2::element_text(size = 11)) +
#' #     ggplot2::ylab("") + ggplot2::xlab("") +
#' #     ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
#' #                                  "Relativer Anteil von Studientinnen als Studienanfänger:innen oder Studierende in ",timerange,
#' #                                  "<br><br><br>"),
#' #                   color = "") +
#' #     ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))
#' #
#' # }
#'
# funktion 12 ----
#'
#' #' A function to plot the german map ::box 6
#' #'
#' #' @description A function to plot the german map with all states that contain
#' #' information about the share of women in STEM
#' #'
#' #' @return The return value is the german map with information
#' #' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' studierende_map_gender <- function(df,r) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_studium_studienzahl_bl_gender_map
#'
#'   subjects <- r$subject_studium_studienzahl_bl_gender_map
#'
#'   lehramt <- r$nurLehramt_studium_studienzahl_bl_gender_map
#'
#'   hochschulform_select_1 <- r$hochschulform_studium_studienzahl_bl_gender_map1
#'
#'   hochschulform_select_2 <- r$hochschulform_studium_studienzahl_bl_gender_map2
#'
#'   indikator_choice <- r$level_studium_choice_gender
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr == timerange)
#'
#'   df <- df %>% dplyr::filter(region != "Deutschland")
#'
#'   df <- df %>% dplyr::filter(region != "Bayern")
#'
#'   df <- df %>% dplyr::filter(region != "Baden-Württemberg")
#'
#'   if(lehramt == FALSE){
#'
#'     df <- df %>% dplyr::filter(nur_lehramt == "Nein")
#'
#'     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
#'
#'   } else {
#'
#'     df <- df %>% dplyr::filter(nur_lehramt == "Ja")
#'
#'     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
#'
#'   }
#'
#'   # aggregate to MINT
#'   df_sub <- calc_share_MINT_bl(df)
#'
#'   df_sub <- df_sub[,colnames(df)]
#'
#'   df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT (aggregiert)"
#'
#'   df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (aggregiert)"
#'
#'   df_sub <-  calc_share_male_bl(df_sub)
#'
#'   df_sub <-  df_sub %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
#'     dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
#'     dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
#'                     wert[anzeige_geschlecht == "Männer"])
#'
#'   df_sub <- df_sub %>%
#'     dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
#'     dplyr::mutate(wert_sum = sum(props))
#'
#'   df <-  calc_share_male_bl(df)
#'
#'   # calculate the new "Gesamt"
#'   df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
#'     dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
#'     dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
#'                     wert[anzeige_geschlecht == "Männer"])
#'
#'   df <- df %>%
#'     dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
#'     dplyr::mutate(wert_sum = sum(props))
#'
#'   df <- rbind(df, df_sub)
#'
#'   df <- df %>% dplyr::filter(fachbereich == subjects)
#'
#'   df <- df %>% dplyr::filter(indikator == indikator_choice)
#'
#'   # calculate proportions
#'   df <- df %>% dplyr::group_by(region, anzeige_geschlecht) %>%
#'     dplyr::summarize(proportion = wert/wert_sum)
#'
#'   df$proportion <- df$proportion * 100
#'
#'   highcharter::hw_grid(
#'     # plot
#'     highcharter::hcmap(
#'       "countries/de/de-all",
#'       data = df[df$anzeige_geschlecht == "Frauen",],
#'       value = "proportion",
#'       joinBy = c("name", "region"),
#'       borderColor = "#FAFAFA",
#'       name = "Anteil",
#'       borderWidth = 0.1,
#'       nullColor = "#A9A9A9",
#'       tooltip = list(
#'         valueDecimals = 0,
#'         valueSuffix = "%"
#'       )
#'       #,
#'       #download_map_data = FALSE
#'     ) %>%
#'       highcharter::hc_colorAxis(min=0,labels = list(format = "{text}%")) %>%
#'       highcharter::hc_title(
#'         text = paste0("Weibliche ", indikator_choice, ": Anteil an Belegungen <br> in ", subjects),
#'         margin = 10,
#'         align = "center",
#'         style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
#'       ) %>%
#'        highcharter::hc_caption(
#'          text = "...",  style = list(color= "white", fontSize = "12px")
#'        ) %>%
#'       highcharter::hc_chart(
#'         style = list(fontFamily = "SourceSans3-Regular")
#'       ) %>% highcharter::hc_size(600, 550) %>%
#'       highcharter::hc_credits(enabled = FALSE) %>%
#'       highcharter::hc_legend(layout = "horizontal", floating = FALSE,
#'                              verticalAlign = "bottom"),
#'
#'     highcharter::hcmap(
#'       "countries/de/de-all",
#'       data = df[df$anzeige_geschlecht == "Männer",],
#'       value = "proportion",
#'       joinBy = c("name", "region"),
#'       borderColor = "#FAFAFA",
#'       name = "Anteil",
#'       borderWidth = 0.1,
#'       nullColor = "#A9A9A9",
#'       tooltip = list(
#'         valueDecimals = 0,
#'         valueSuffix = "%"
#'       )
#'       #,
#'       #download_map_data = FALSE
#'     ) %>%
#'       highcharter::hc_colorAxis(min=0, labels = list(format = "{text}%")) %>%
#'       highcharter::hc_title(
#'         text = paste0("Männliche ", indikator_choice, ": Anteil an Belegungen <br> in ", subjects),
#'         margin = 10,
#'         align = "center",
#'         style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
#'       ) %>%
#'        highcharter::hc_caption(
#'          text = "Ausgegraut: Daten stehen nicht zur Verfügung",  style = list(fontSize = "12px")
#'        ) %>%
#'       highcharter::hc_chart(
#'         style = list(fontFamily = "SourceSans3-Regular")
#'       ) %>% highcharter::hc_size(600, 550) %>%
#'       highcharter::hc_credits(enabled = FALSE) %>%
#'       highcharter::hc_legend(layout = "horizontal", floating = FALSE, verticalAlign = "bottom"),
#'
#'
#'     ncol = 2,
#'     browsable = TRUE
#'   )
#' }
#'
# funktion 13 ----
#' #' A function to plot time series
#' #'
#' #' @description A function to plot the time series of the german states
#' #'
#' #' @return The return value, if any, from executing the function.
#' #' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' studierende_verlauf_multiple_bl_gender <- function(df,r) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_studium_studienzahl_bl_gender_verlauf
#'
#'   subjects_select <- r$subject_studium_studienzahl_bl_gender_verlauf
#'
#'   lehramt <- r$nurLehramt_studium_studienzahl_bl_gender_verlauf
#'
#'   hochschulform_select_1 <- r$hochschulform_studium_studienzahl_bl_gender_verlauf1
#'
#'   hochschulform_select_2 <- r$hochschulform_studium_studienzahl_bl_gender_verlauf2
#'
#'   studium_level <- r$level_studium_studienzahl_bl_gender_verlauf
#'
#'   states <- r$states_studium_studienzahl_bl_gender_verlauf
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])
#'
#'   # remove
#'   df <- df %>% dplyr::filter(region != "Bayern")
#'
#'   df <- df %>% dplyr::filter(region != "Baden-Württemberg")
#'
#'   # include "Osten" und "Westen" in Dataframe
#'   df <- prep_studierende_east_west(df)
#'
#'   if(lehramt == FALSE){
#'
#'     df <- df %>% dplyr::filter(nur_lehramt == "Nein")
#'
#'     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
#'
#'     title_help_sub_sub <- " insgesamt"
#'
#'   } else {
#'
#'     df <- df %>% dplyr::filter(nur_lehramt == "Ja")
#'
#'     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
#'
#'     title_help_sub_sub <- " an einer Uni (nur Lehramt)"
#'   }
#'
#'   # aggregate to MINT
#'   df_sub <- calc_share_MINT_bl(df)
#'
#'   df_sub <- df_sub[,colnames(df)]
#'
#'   df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT (aggregiert)"
#'
#'   df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (aggregiert)"
#'
#'   df_sub <-  calc_share_male_bl(df_sub)
#'
#'   df_sub <-  df_sub %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
#'     dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
#'     dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
#'                     wert[anzeige_geschlecht == "Männer"])
#'
#'   df_sub <- df_sub %>%
#'     dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
#'     dplyr::mutate(wert_sum = sum(props))
#'
#'   df <-  calc_share_male_bl(df)
#'
#'   # calculate the new "Gesamt"
#'   df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
#'     dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
#'     dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
#'                     wert[anzeige_geschlecht == "Männer"])
#'
#'   df <- df %>%
#'     dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
#'     dplyr::mutate(wert_sum = sum(props))
#'
#'   df <- rbind(df, df_sub)
#'
#'   df <- df %>% dplyr::filter(fachbereich == subjects_select)
#'
#'   df <- df %>% dplyr::filter(indikator == studium_level)
#'
#'   df <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")
#'
#'   df <- df %>% dplyr::filter(region %in% states)
#'
#'   # calculate proportions
#'   df <- df %>% dplyr::group_by(region, fachbereich, jahr, anzeige_geschlecht) %>%
#'     dplyr::summarize(proportion = wert/wert_sum) %>% dplyr::ungroup()
#'
#'   df$proportion <- df$proportion * 100
#'
#'   if(studium_level == "Studierende") {
#'
#'     title_help <- "Weibliche Studierende:"
#'
#'   }else {
#'
#'     title_help <- "Studienanfängerinnen:"
#'
#'   }
#'
#'   # order years for plot
#'   df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
#'
#'   # plot
#'   highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion,1), group = region)) %>%
#'     highcharter::hc_tooltip(pointFormat = "Anteil {point.region} <br> Wert: {point.y} %") %>%
#'     highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
#'     highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
#'     #highcharter::hc_caption(text = "Quelle: ",  style = list(fontSize = "12px") ) %>%
#'     highcharter::hc_title(text = paste0(title_help, " Anteil an Belegungen in ", subjects_select),
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
#' }
#'
#'
#'
# funktion 14 ----
#' #' A function to create a dumbbell plot
#' #'
#' #' @description A function to compare a subject for different Bundesländer
#' #'
#' #' @return The return value is a dumbbell plot
#' #' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' bundeslaender_ranking <- function(df,r, type) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_studium_studienzahl_bl_gender_vergleich
#'
#'   lehramt <- r$nurLehramt_studium_studienzahl_bl_gender_vergleich
#'
#'   hochschulform_select_1 <- r$hochschulform_studium_studienzahl_bl_gender_vergleich1
#'
#'   hochschulform_select_2 <- r$hochschulform_studium_studienzahl_bl_gender_vergleich2
#'
#'   subject <- r$subject_studium_studienzahl_bl_gender_vergleich
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr == timerange)
#'
#'   df <- df %>% dplyr::filter(region != "Deutschland")
#'
#'   #df <- df %>% dplyr::filter(region != "Bayern")
#'
#'  # df <- df %>% dplyr::filter(region != "Baden-Württemberg")
#'
#'   df <- df %>% dplyr::mutate(indikator = replace(indikator,
#'                                                  indikator == "Studienanfänger:innen",
#'                                                  "Studienanfänger:inneninnen"))
#'
#'   if(lehramt == FALSE){
#'
#'     df <- df %>% dplyr::filter(nur_lehramt == "Nein")
#'
#'     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_1)
#'
#'   } else {
#'
#'     df <- df %>% dplyr::filter(nur_lehramt == "Ja")
#'
#'     df <- df %>% dplyr::filter(hochschulform == hochschulform_select_2)
#'
#'   }
#'
#'   # aggregate to MINT
#'   df_sub <- calc_share_MINT_bl(df)
#'
#'   df_sub <- df_sub[,colnames(df)]
#'
#'   df_sub[df_sub$fachbereich == "MINT", "fachbereich"] <- "MINT (aggregiert)"
#'
#'   df_sub[df_sub$fachbereich == "andere Fächer", "fachbereich"] <- "andere Fächer (aggregiert)"
#'
#'   df_sub <-  calc_share_male_bl(df_sub)
#'
#'   df_sub <-  df_sub %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
#'     dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
#'     dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
#'                     wert[anzeige_geschlecht == "Männer"])
#'
#'   df_sub <- df_sub %>%
#'     dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
#'     dplyr::mutate(wert_sum = sum(props))
#'
#'   df <-  calc_share_male_bl(df)
#'
#'   # calculate the new "Gesamt"
#'   df <-  df %>% dplyr::filter(anzeige_geschlecht != "Gesamt") %>%
#'     dplyr::group_by(region, fachbereich, indikator, jahr, nur_lehramt, hochschulform) %>%
#'     dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
#'                     wert[anzeige_geschlecht == "Männer"])
#'
#'   df <- df %>%
#'     dplyr::group_by(jahr, region, indikator, anzeige_geschlecht) %>%
#'     dplyr::mutate(wert_sum = sum(props))
#'
#'   df <- rbind(df, df_sub)
#'
#'   df <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen")
#'
#'   df <- df %>% dplyr::filter(fachbereich == subject)
#'
#'   df <- df %>% dplyr::group_by(region, indikator) %>%
#'     dplyr::summarize(proportion = (wert/wert_sum)*100)
#'
#'   # spread column
#'   df <- tidyr::spread(df, indikator, proportion)
#'
#'   df <- df %>% tidyr::drop_na()
#'
#'   df2 <- tidyr::gather(df, group, value, -region) %>%
#'     dplyr::filter(group != "fachbereich") %>%
#'     dplyr::mutate(value = as.numeric(value))
#'
#'   df$region <- reorder(df$region, df$Studierende)
#'
#'   df2$region <- factor(df2$region, levels = levels(df$region))
#'
#'   ggplot2::ggplot(df,
#'                   ggplot2::aes(y = region)) +
#'     ggplot2::geom_point(data = df2, ggplot2::aes(x = value, color = group), size = 5) +
#'     ggalt::geom_dumbbell(
#'       ggplot2::aes(x = Studienanfänger:inneninnen, xend = Studierende),
#'       size = 0.5,
#'       size_x = 5,
#'       size_xend = 5,
#'       colour = "black",
#'       colour_x = "#b1b5c366",
#'       colour_xend = "#f5adac66",
#'       dot_guide=TRUE) +
#'     ggplot2::theme_minimal() +
#'     ggplot2::scale_color_manual(name = "", values = c("#b1b5c366", "#f5adac66")) +
#'     ggplot2::theme(legend.position="top",
#'                    panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
#'                    plot.title = ggtext::element_markdown(hjust = 0.5),
#'                    axis.text.y = ggplot2::element_text(size = 11)) +
#'     ggplot2::ylab("") + ggplot2::xlab("") +
#'     ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
#'                                  "Frauen: Studienfachwahl von MINT-Fächergruppen in ", states, "<br>", timerange,
#'                                  "<br><br><br>"),
#'                   color = "") +
#'     ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))
#'
#' }



