# hier lager funktionen, die wir nicht mehr verwenden, aber eventuell später nochmal verwenden möchten




# Funktion 1 ----

#' A function to plot a waffle chart ::: b3
#'
#' @description A function to create a waffle chart for the tab "Beruf"
#'
#' @return The return value is a waffle chart
#' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# arbeitsmarkt_anforderungen_gender <- function(r) {
#
#
#   timerange <- r$date_arbeitsmarkt_anforderungen_gender
#
#   if(timerange ==2021) indikator_choice <- r$level_arbeitsmarkt_anforderungen_gender_21
#   if(timerange ==2022) indikator_choice <- r$level_arbeitsmarkt_anforderungen_gender_22
#
#
#   df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
#     dplyr::filter(jahr %in% timerange &
#                     bundesland == "Deutschland" &
#                     geschlecht != "Gesamt"&
#                     anforderung == "Gesamt" &
#                     indikator %in% c("Auszubildende",
#                                      "Auszubildende (1. Jahr)",
#                                      "Beschäftigte",
#                                      "ausländische Auszubildende",
#                                      "ausländische Beschäftigte")&
#                     fachbereich %in% c("Alle", "MINT", "Mathematik, Naturwissenschaften",
#                                        "Informatik", "Technik (gesamt)"))%>%
#     dplyr::select(indikator, fachbereich, wert, geschlecht) %>%
#     dplyr::collect()
#
#   # Berechnung von andere Fächergruppen
#   df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"]-
#     df[df$fachbereich == "MINT", "wert"]
#   df$fachbereich[df$fachbereich == "Alle"]<-"andere Fächergruppen"
#   df <- df %>% dplyr::filter(fachbereich != "MINT")
#
#   # Anteil berechnen
#   df <- df %>%
#     dplyr::group_by(indikator, geschlecht) %>%
#     dplyr::mutate(props = sum(wert))
#
#   df <- df %>% dplyr::group_by(fachbereich, indikator, geschlecht) %>%
#     dplyr::mutate(proportion = wert/props)
#
#   df$proportion <- df$proportion * 100
#
#
#   # Ausgewählte Indikatoren filtern
#   df <- df %>% dplyr::filter(indikator == indikator_choice)
#
#   # nach Geschlechtern trennen
#   # Frauen
#   df_fr <- df %>% dplyr::filter(geschlecht=="Frauen")
#
#   df_fr <- setNames(round_preserve_sum(as.numeric(df_fr$proportion),0),
#                     df_fr$fachbereich)
#   df_fr <- df_fr[order(factor(names(df_fr), levels = c("Mathematik, Naturwissenschaften",
#                                                        "Informatik", "Technik (gesamt)",
#                                                        'andere Fächergruppen')))]
#
#   # Männer
#   df_me <- df %>% dplyr::filter(geschlecht=="Männer")
#
#   df_me <- setNames(round_preserve_sum(as.numeric(df_me$proportion),0),
#                     df_me$fachbereich)
#   df_me <- df_me[order(factor(names(df_me), levels = c("Mathematik, Naturwissenschaften",
#                                                        "Informatik", "Technik (gesamt)",
#                                                        'andere Fächergruppen')))]
#
#   # Titel für Plots
#   title_help <- paste0(indikator_choice, "n <br>")
#   title_help <- ifelse(grepl("ausländische Beschäftigte", indikator_choice), "ausländischen <br> Beschäftigten", title_help)
#   title_help <- ifelse(grepl("ausländische Auszubildende", indikator_choice), "ausländischen <br> Auszubildenden", title_help)
#   title_help <- ifelse(grepl("Jahr", indikator_choice), "Auszubildenden <br> mit neuem Lehrvertrag <br>", title_help)
#
#
#   title_male <- paste0("Von männlichen ", title_help, " gewählte Berufsfelder <br> (", timerange, ")")
#   title_female <- paste0("Von weiblichen ", title_help, " gewählte Berufsfelder <br>(", timerange, ")")
#
#   #waffles
#   waffle_fr <- waffle::waffle(df_fr, keep = FALSE) +
#     ggplot2::labs(
#       fill = "",
#       title = paste0("<span style='color:black;'>", title_female, "<br>")) +
#     ggplot2::theme(plot.title = ggtext::element_markdown(),
#                    plot.subtitle = ggtext::element_markdown(),
#                    text = ggplot2::element_text(size = 14),
#                    plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
#                    legend.position = "bottom") +
#     # account for the possability that female has 0% share of "Experte
#     # if (df_trainee[[3]] == 0) {
#     # waffle_fr <- waffle_fr +
#     ggplot2::scale_fill_manual(
#       values =  c("#ee7775",
#                   "#fbbf24",
#                   "#35bd97",
#                   '#8893a7'),
#       limits = c("Mathematik, Naturwissenschaften",
#                  "Informatik", "Technik (gesamt)",
#                  'Andere Fachbereiche'),
#       na.value='#8893a7',
#       guide = ggplot2::guide_legend(reverse = TRUE),
#       labels = c(
#         paste0("Mathematik, Naturwissenschaften",", ",df_fr[1], "%"),
#         paste0("Informatik",", ",df_fr[2], "%"),
#         paste0("Technik (gesamt)",", ",df_fr[3], "%"),
#         paste0("Andere Fachbereiche",", ",df_fr[4], "%")
#       )) +
#     ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))
#
#
#   waffle_me <- waffle::waffle(df_me, keep = FALSE) +
#     ggplot2::labs(
#       fill = "",
#       title = paste0("<span style='color:black;'>", title_male ,"<br>")) +
#     ggplot2::theme(plot.title = ggtext::element_markdown(),
#                    plot.subtitle = ggtext::element_markdown(),
#                    text = ggplot2::element_text(size = 14),
#                    plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
#                    legend.position = "bottom")+
#     ggplot2::scale_fill_manual(
#       values =  c("#ee7775",
#                   "#fbbf24",
#                   "#35bd97",
#                   '#8893a7'),
#       limits = c("Mathematik, Naturwissenschaften",
#                  "Informatik", "Technik (gesamt)",
#                  'Andere Fachbereiche'),
#       na.value='#8893a7',
#       guide = ggplot2::guide_legend(reverse = TRUE),
#       labels = c(
#         paste0("Mathematik, Naturwissenschaften",", ",df_me[1], "%"),
#         paste0("Informatik",", ",df_me[2], "%"),
#         paste0("Technik (gesamt)",", ",df_me[3], "%"),
#         paste0("Andere Fachbereiche",", ",df_me[4], "%")
#       )) +
#     ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))
#
#   ggpubr::ggarrange(waffle_fr, NULL ,waffle_me, widths = c(1, 0.1, 1), nrow=1)
#
#
# }




# Funktion 2 ----

#' A function to plot the german map ::::box 6
#'
#' @description A function to plot the german map with all states that contain
#' information about the share of women in STEM
#'
#' @return The return value is the german map with information
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# arbeitsmarkt_bl_gender <- function(r) {
#
#   # load UI inputs from reactive value
#   timerange <- r$date_arbeitsmarkt_bl_gender
#
#   #anforderung <- r$anforderung_arbeitsmarkt_bl_gender
#
#   if(timerange == 2021) indikator_choice <- r$level_arbeitsmarkt_bl_gender_21
#   if(timerange == 2022) indikator_choice <- r$level_arbeitsmarkt_bl_gender_22
#
#   fachbereich_choice <- r$fach_arbeitsmarkt_bl_gender
#
#   df <-  dplyr::tbl(con, from = "arbeitsmarkt_detail")%>%
#     dplyr::filter(jahr %in% timerange &
#                     !bundesland %in% c("Deutschland",
#                                        "Westdeutschland (o. Berlin)",
#                                        "Ostdeutschland (einschl. Berlin)") &
#                     landkreis == "alle Landkreise" &
#                     geschlecht != "Gesamt"&
#                     anforderung == "Gesamt" )%>%
#     dplyr::select(indikator, fachbereich, wert, geschlecht, bundesland, jahr) %>%
#     dplyr::collect()
#
#   # filter dataset based on UI inputs
#   # df <- df %>% dplyr::filter(jahr == timerange)
#   # # filtern nach Anforderungsniveau
#   # df <- df %>% dplyr::filter(anforderung == "Gesamt")
#
#   #direkt angegebene Werte hier vollständiger als selbst berechnete Aggregate, daher Folgendes raus
#
#   # # remove - Deutschland nicht enthalten in DF
#   # df <- df %>% dplyr::filter(region != "Deutschland")
#   #
#   # # im neuen DF doch Aggregate enthalten, ausfiltern das Folgecode weiter stimmt
#   # df <- df %>% dplyr::filter(landkreis != "alle Landkreise")
#
#   # # Aggregat auf Bundeslandebene berechnen und LKs ausschließen
#   # df <- df %>%
#   #   dplyr::group_by(jahr, indikator, fachbereich, geschlecht, bundesland) %>%
#   #   dplyr::summarize(wert = sum(wert))
#
#   # Filtern nach Bundesländern
#   # df <- df %>%
#   #   dplyr::filter(landkreis == "alle Landkreise") %>%
#   #   dplyr::filter(!(bundesland %in% c("Deutschland", "Westdeutschland (o. Berlin)", "Ostdeutschland (einschl. Berlin)")))
#
#
#   # Berechnung von andere Fächergruppen
#   df_andere <- df %>% dplyr::filter(fachbereich=="Alle")
#   df_mint <- df %>% dplyr::filter(fachbereich=="MINT")
#   df_andere$wert <- df_andere$wert - df_mint$wert
#   df_andere$fachbereich[df_andere$fachbereich == "Alle"]<-"Andere Berufsgruppen"
#
#   df <- rbind(df, df_andere)
#
#   #nicht nötig, da Männer schon in df berechnet
#   #df <- calc_arbeitsmarkt_males(df)
#
#   df <- df %>% dplyr::filter(indikator == indikator_choice)
#
#   df_gesamt <- df %>%
#     dplyr::filter(fachbereich == "Alle")
#   # ,
#   # anforderung == "Gesamt")
#
#   df <- df %>%
#     dplyr::left_join(df_gesamt, by = c("bundesland", "jahr", "geschlecht", "indikator")) %>%
#     dplyr::rename(fachbereich = fachbereich.x,
#                   wert = "wert.x",
#                   wert_sum = "wert.y") %>%
#     dplyr::select(-fachbereich.y) %>%
#     dplyr::mutate(proportion = (wert/wert_sum)*100)%>%
#     dplyr::filter(fachbereich == fachbereich_choice)
#
#   #Gerundetes Prop für Hover:
#   df$prop <- round(df$proportion, 0)
#
#   #Trennpunkte für lange Zahlen ergänzen
#   df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")
#
#   values_female <- df %>% dplyr::filter(geschlecht == "Frauen")
#   values_male <- df %>% dplyr::filter(geschlecht == "Männer")
#
#   # if (anforderung == "Gesamt"){
#   #
#   #   title_help_sub <- " insgesamt"
#   #
#   # } else {
#   #
#   #   title_help_sub <- paste0(" mit anforderung ", anforderung)
#   #
#   # }
#
#   #Überschrift erstellen
#   title_help <- paste0(indikator_choice, "r")
#   title_help <- ifelse(grepl("ausländische Beschäftigte", indikator_choice), "ausländischer Beschäftigter", title_help)
#   title_help <- ifelse(grepl("ausländische Auszubildende", indikator_choice), "ausländischer Auszubildender", title_help)
#   title_help <- ifelse(grepl("Jahr", indikator_choice), "Auszubildender mit neuem Lehrvertrag", title_help)
#   # title_help <- ifelse(grepl("u25", indikator_choice), "Beschäftigten unter 25 Jahren", title_help)
#   # title_help <- ifelse(grepl("25-55", indikator_choice), "Beschäftigten zwischen 25 und 55 Jahren", title_help)
#   # title_help <- ifelse(grepl("ü55", indikator_choice), "Beschäftigten über 55 Jahren", title_help)
#
#   titel_w <- ifelse(fachbereich_choice == "Andere Berufsgruppen", paste0("Anteil weiblicher ", title_help, ", die kein MINT-Berufsfeld wählen (", timerange, ")"),
#                     paste0("Anteil weiblicher ", title_help, ", die das Berufsfeld ", fachbereich_choice, " wählen (", timerange, ")"))
#   titel_m <- ifelse(fachbereich_choice == "Andere Berufsgruppen", paste0("Anteil männlicher ", title_help, ", die kein MINT-Berufsfeld wählen (", timerange, ")"),
#                     paste0("Anteil männlicher ", title_help, ", die das Berufsfeld ", fachbereich_choice, " wählen (", timerange, ")"))
#
#
#
#   # plot
#   out_1 <- highcharter::hcmap(
#     "countries/de/de-all",
#     data = values_female,
#     value = "proportion",
#     joinBy = c("name", "bundesland"),
#     borderColor = "#FAFAFA",
#     name = paste0(fachbereich_choice),
#     borderWidth = 0.1,
#     nullColor = "#A9A9A9",
#     tooltip = list(
#       valueDecimals = 0,
#       valueSuffix = "%"
#     )
#     #,
#     #download_map_data = FALSE
#   ) %>%
#     highcharter::hc_tooltip(pointFormat = "{point.bundesland} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}") %>%
#     highcharter::hc_colorAxis(min=0,labels = list(format = "{text}%")) %>%
#     highcharter::hc_title(
#       text = titel_w,
#       margin = 10,
#       align = "center",
#       style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
#     ) %>%
#     # highcharter::hc_caption(
#     #   text = "Quelle:",  style = list(fontSize = "12px")
#     # ) %>%
#     highcharter::hc_chart(
#       style = list(fontFamily = "SourceSans3-Regular")
#     ) %>% highcharter::hc_size(600, 550) %>%
#     highcharter::hc_credits(enabled = FALSE) %>%
#     highcharter::hc_legend(layout = "horizontal", floating = FALSE,
#                            verticalAlign = "bottom")
#
#   out_2 <- highcharter::hcmap(
#     "countries/de/de-all",
#     data = values_male,
#     value = "proportion",
#     joinBy = c("name", "bundesland"),
#     borderColor = "#FAFAFA",
#     name = paste0(fachbereich_choice),
#     borderWidth = 0.1,
#     nullColor = "#A9A9A9",
#     tooltip = list(
#       valueDecimals = 0,
#       valueSuffix = "%"
#     )
#     #,
#     #download_map_data = FALSE
#   ) %>%
#     highcharter::hc_tooltip(pointFormat = "{point.bundesland} <br> Anteil: {point.prop} % <br> Anzahl: {point.wert}") %>%
#     highcharter::hc_colorAxis(min=0,labels = list(format = "{text}%")) %>%
#     highcharter::hc_title(
#       text = titel_m,
#       margin = 10,
#       align = "center",
#       style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")
#     ) %>%
#     # highcharter::hc_caption(
#     #   text = "Quelle:",  style = list(fontSize = "12px")
#     # ) %>%
#     highcharter::hc_chart(
#       style = list(fontFamily = "SourceSans3-Regular")
#     ) %>% highcharter::hc_size(600, 550) %>%
#     highcharter::hc_credits(enabled = FALSE) %>%
#     highcharter::hc_legend(layout = "horizontal", floating = FALSE, verticalAlign = "bottom")
#
#
#   out <- list(out_1, out_2)
#
#   return (out)
#
# }



# Funktion 3 ----

#' #' A function to plot a waffle chart
#' #'
#' #' @description A function to create a waffle chart for the second box inside the
#' #' tab "Beruf".
#' #'
#' #' @return The return value is a waffle chart
#' #' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#' arbeitnehmer_waffle <- function(df,r) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_arbeitsmarkt
#'
#'   anforderung <- r$anforderung_arbeitsmarkt
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr == timerange)
#'
#'   df <- df %>% dplyr::filter(region == "Deutschland")
#'
#'   df <- df %>% dplyr::filter(anforderung == anforderung)
#'
#'   df <- df %>% dplyr::filter(fachbereich == "MINT")
#'
#'   # calculate the share of males
#'   values <- df %>%
#'     dplyr::group_by(indikator) %>%
#'     dplyr::mutate(wert = wert - dplyr::lead(wert)) %>% dplyr::select(wert) %>% na.omit()
#'
#'   df[df$geschlecht == "Gesamt", "wert"] <- values$wert
#'
#'   df[df$geschlecht == "Gesamt", "geschlecht"] <- "Männer"
#'
#'
#'   # calculate proportions
#'   df <- df %>% dplyr::group_by(indikator) %>%
#'     dplyr::mutate(props = sum(wert))
#'
#'
#'   df <- df %>% dplyr::group_by(geschlecht, indikator, fachbereich) %>%
#'     dplyr::summarize(proportion = wert/props)
#'
#'   df$proportion <- df$proportion * 100
#'
#'   df_beschaeftigte <- df %>% dplyr::filter(indikator == "Beschäftigte")
#'
#'
#'   df_beschaeftigte$geschlecht <- paste0(df_beschaeftigte$geschlecht, " (", df_beschaeftigte$fachbereich, ")")
#'
#'   # ensure proportions sum to 1
#'   x_beschaeftigte <- setNames(round_preserve_sum(as.numeric(df_beschaeftigte$proportion),0),
#'                               df_beschaeftigte$geschlecht)
#'
#'   x_beschaeftigte <- x_beschaeftigte[order(factor(names(x_beschaeftigte), levels = c('Frauen (MINT)', 'Männer (MINT)')))]
#'
#'   df_auszubildende <- df %>% dplyr::filter(indikator == "Auszubildende")
#'
#'   df_auszubildende$geschlecht <- paste0(df_auszubildende$geschlecht, " (", df_auszubildende$fachbereich, ")")
#'
#'   # ensure proportions sum to 1
#'   x_auszubildende <- setNames(round_preserve_sum(as.numeric(df_auszubildende$proportion),0),
#'                               df_auszubildende$geschlecht)
#'
#'   x_auszubildende <- x_auszubildende[order(factor(names(x_auszubildende), levels = c('Frauen (MINT)', 'Männer (MINT)')))]
#'
#'
#'   # create plot objects for waffle charts
#'   waffle_aus <- waffle::waffle(x_auszubildende, keep = FALSE) +
#'     ggplot2::labs(
#'       fill = "",
#'       title = paste0("<span style='color:black;'>", "Berufswahl (Auszubildende)</span>", br(), timerange)) +
#'     ggplot2::theme(plot.title = ggtext::element_markdown(),
#'                    plot.subtitle = ggtext::element_markdown(),
#'                    text = ggplot2::element_text(size = 14),
#'                    plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
#'                    legend.position = "bottom") +
#'     ggplot2::scale_fill_manual(
#'       values =  c("#b16fab",
#'                   "#b1b5c3"),
#'       na.value="#b1b5c3",
#'       limits = c("Frauen (MINT)", "Männer (MINT)"),
#'       guide = ggplot2::guide_legend(reverse = TRUE),
#'       labels = c(
#'         paste0("Frauen (MINT)",", ",x_auszubildende[1], "%"),
#'         paste0("Männer (MINT)",", ",x_auszubildende[2], "%"))) +
#'     ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))
#'
#'
#'
#'   waffle_be <- waffle::waffle(x_beschaeftigte, keep = FALSE) +
#'     ggplot2::labs(
#'       fill = "",
#'       title = paste0("<span style='color:black;'>", "Berufswahl (Beschäftigte)</span>", br(), timerange)) +
#'     ggplot2::theme(plot.title = ggtext::element_markdown(),
#'                    plot.subtitle = ggtext::element_markdown(),
#'                    text = ggplot2::element_text(size = 14),
#'                    plot.margin = ggplot2::unit(c(1.5,0,0,0), "lines"),
#'                    legend.position = "bottom") +
#'       ggplot2::scale_fill_manual(
#'         values =  c("#b16fab",
#'                     "#b1b5c3"),
#'         na.value="#b1b5c3",
#'         limits = c("Frauen (MINT)", "Männer (MINT)"),
#'         guide = ggplot2::guide_legend(reverse = TRUE),
#'         labels = c(
#'           paste0("Frauen (MINT)",", ",x_beschaeftigte[1], "%"),
#'           paste0("Männer (MINT)",", ",x_beschaeftigte[2], "%"))) +
#'       ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))
#'
#'
#'
#'
#'   if (anforderung == "Gesamt"){
#'
#'     title_help_sub <- " insgesamt"
#'
#'   } else {
#'
#'     title_help_sub <- paste0(" (",anforderung,")")
#'
#'   }
#'
#'
#'
#'   plot <- ggpubr::ggarrange(waffle_aus, NULL ,waffle_be, widths = c(1, 0.1, 1), nrow=1)
#'   text <- c(
#'     paste0("<span style='font-size:20.5pt; color:black'> Anteil von Frauen und Männer ", title_help_sub," an MINT und <br> allen anderen Berufszweigen in ", timerange))
#'
#'   ggpubr::annotate_figure(plot, gridtext::richtext_grob(text = text))
#'
#' }




# Funktion 4 ----

#' A function to create a paired bar plot
#'
#' @description A function to return a paired bar plot for the second box inside
#' the tab "Beruf"
#'
#' @return The return value is a bar plot
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# arbeitsmarkt_absolut <- function(df,r) {
#
#   # load UI inputs from reactive value
#   timerange <- r$date_arbeitsmarkt
#
#   anforderung <- r$anforderung_arbeitsmarkt
#
#
#   # filter dataset based on UI inputs
#   df <- df %>% dplyr::filter(jahr == timerange)
#
#   df <- df %>% dplyr::filter(region == "Deutschland")
#
#   df <- df %>% dplyr::filter(anforderung == anforderung)
#
#   # call function to calculate the share of MINT and the remaining subjects
#   df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
#     df[df$fachbereich == "MINT", "wert"]
#
#   df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"
#
#   # calculate the share of males
#   values <- df %>%
#     dplyr::group_by(fachbereich, indikator) %>%
#     dplyr::mutate(wert = wert - dplyr::lead(wert)) %>% dplyr::select(wert) %>% na.omit()
#
#   df[df$geschlecht == "Gesamt", "wert"] <- values$wert
#
#   df[df$geschlecht == "Gesamt", "geschlecht"] <- "Männer"
#
#
#   if (anforderung == "Gesamt"){
#
#     title_help_sub <- " insgesamt"
#
#   } else {
#
#     title_help_sub <- paste0(" (",anforderung,")")
#
#   }
#   options(scipen=999)
#
#
#   # plot
#   ggplot2::ggplot(df, ggplot2::aes(x=indikator, y=wert, fill = fachbereich)) +
#     ggplot2::geom_bar(stat="identity", position = "dodge") +
#     ggplot2::geom_text(ggplot2::aes(label=wert, vjust = - 0.25),
#                        position=ggplot2::position_dodge(width=0.9),
#                        fontface = "bold") +
#     ggplot2::theme_minimal() +
#     ggplot2::facet_grid(~ geschlecht) +
#     ggplot2::theme(
#       strip.background = ggplot2::element_blank(),
#       text = ggplot2::element_text(size = 14),
#       plot.title = ggtext::element_markdown(hjust = 0.5)) +
#     ggplot2::xlab("") + ggplot2::ylab("Anzahl") +
#     ggplot2::scale_fill_manual(values = c("#efe8e6","#b16fab")) +
#     ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
#                                  "Arbeitnehmer*innen ", title_help_sub," in MINT und allen anderen Berufszweigen in ", timerange,
#                                  "<br><br><br>"),
#                   fill = "")
#
#
# }



# Funktion 5 ----

#' A function to return a filtered dataset
#'
#' @description A function to similar to 'arbeitsmarkt_einstieg_bar' but with the
#' difference that it returns a dataframe instead of plot.
#'
#' @return The return value is a dataframe
#' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# data_mix_beruf <- function(df,r) {
#
#   # load UI inputs from reactive value
#   timerange <- r$date_arbeitsmarkt
#
#   anforderung <- r$anforderung_arbeitsmarkt
#
#   # filter dataset based on UI inputs
#   df <- df %>% dplyr::filter(jahr == timerange)
#
#   df <- df %>% dplyr::filter(anforderung == anforderung)
#
#   df <- df %>% dplyr::filter(fachbereich != "Alle")
#
#   colnames(df) <- c("Region", "Fachbereich", "anforderung", "Wert", "Indikator", "Jahr", "Geschlecht", "Bereich")
#
#   return(df)
#
# }




# Funktion 6 ----


#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# arbeitsmarkt_verlauf <- function(df,r) {
#
#   # load UI inputs from reactive value
#   status_arbeitnehmer <- r$indikator_arbeitsmarkt_verlauf
#
#   timerange <- r$date_arbeitsmarkt_verlauf
#
#   states <- r$states_arbeitsmarkt_verlauf
#
#   topic <- r$topic_arbeitsmarkt_verlauf
#
#   anforderung <- r$anforderung_arbeitsmarkt_verlauf
#
#   # filter dataset based on UI inputs
#   df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])
#
#   df <- df %>% dplyr::filter(indikator == status_arbeitnehmer)
#
#   df <- df %>% dplyr::filter(anforderung == anforderung)
#
#   # remove
#   df <- df %>% dplyr::filter(region != "Deutschland")
#
#   # call function to calculate the share of MINT and the remaining subjects
#   df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
#     df[df$fachbereich == "MINT", "wert"]
#
#   df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"
#
#   # include "Osten" und "Westen" in Dataframe
#   df <- prep_arbeitsmarkt_east_west(df)
#
#   # order
#   df <- df[with(df, order(region, fachbereich, jahr, decreasing = TRUE)), ]
#
#   # calculate proportion
#   df <-  df %>%
#     dplyr::group_by(region, fachbereich, jahr) %>%
#     dplyr::mutate(props = wert[geschlecht == "Frauen"]/
#                     wert[geschlecht == "Gesamt"])
#
#   df <- df %>% dplyr::filter(geschlecht != "Gesamt")
#
#
#   df <- df %>% dplyr::filter(region %in% states)
#
#   # filter MINT or remaining subjects
#   df <- df %>% dplyr::filter(fachbereich == topic)
#
#   # order years for plot
#   df <- df[with(df, order(region, jahr, decreasing = TRUE)), ]
#
#   df$props <- df$props * 100
#
#   if (anforderung == "Gesamt"){
#
#     title_help_sub_sub <- " insgesamt"
#
#   } else {
#
#     title_help_sub_sub <- paste0(" (",anforderung,")")
#
#   }
#
#
#   if (status_arbeitnehmer == "Auszubildende"){
#
#     title_help_sub <- " in Ausbildung"
#
#   }else{
#
#     title_help_sub <- " in Beschäftigung"
#
#   }
#
#   if (topic == "MINT"){
#
#     title_help <- paste0("MINT", title_help_sub, title_help_sub_sub)
#
#   }else {
#
#     title_help <- paste0("allen anderen Berufszweigen", title_help_sub, title_help_sub_sub)
#
#   }
#
#   # plot
#   highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(props), group = region)) %>%
#     highcharter::hc_tooltip(pointFormat = "Anteil Frauen <br> Bundesland: {point.region} <br> Wert: {point.y} %") %>%
#     highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%")) %>%
#     highcharter::hc_xAxis(title = list(text = "Jahr"), style = list(fontFamily = "SourceSans3-Regular")) %>%
#     #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.", style = list(fontSize = "12px") ) %>%
#     highcharter::hc_title(text = paste0("Anteil von Frauen an ", title_help ," im Verlauf"),
#                           margin = 45,
#                           align = "center",
#                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#     highcharter::hc_chart(
#       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
#     ) %>%
#     highcharter::hc_exporting(enabled = FALSE,
#                               buttons = list(contextButton = list(
#                                 symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
#                                 onclick = highcharter::JS("function () {
#                                                               this.exportChart({ type: 'image/png' }); }"),
#                                 align = 'right',
#                                 verticalAlign = 'bottom',
#                                 theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
#
# }




# Funktion 7 ----


#' A function to return a filtered dataset
#'
#' @description A function to similar to 'arbeitsmarkt_einstieg_bar' but with the
#' difference that it returns a dataframe instead of plot.
#'
#' @return The return value is a dataframe
#' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# data_verlauf_beruf <- function(df,r) {
#
#   # load UI inputs from reactive value
#   status_arbeitnehmer <- r$indikator_arbeitsmarkt_verlauf
#
#   timerange <- r$date_arbeitsmarkt_verlauf
#
#   states <- r$states_arbeitsmarkt_verlauf
#
#   topic <- r$topic_arbeitsmarkt_verlauf
#
#   anforderung <- r$anforderung_arbeitsmarkt_verlauf
#
#   # filter dataset based on UI inputs
#   df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])
#
#   df <- df %>% dplyr::filter(indikator == status_arbeitnehmer)
#
#   df <- df %>% dplyr::filter(anforderung == anforderung)
#
#   # remove
#   df <- df %>% dplyr::filter(region != "Deutschland")
#
#   # call function to calculate the share of MINT and the remaining subjects
#   df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
#     df[df$fachbereich == "MINT", "wert"]
#
#   df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"
#
#   # include "Osten" und "Westen" in Dataframe
#   df <- prep_arbeitsmarkt_east_west(df)
#
#   # order
#   df <- df[with(df, order(region, fachbereich, jahr, decreasing = TRUE)), ]
#
#
#   df <-  df %>%
#     dplyr::group_by(region, fachbereich, jahr, anforderung, indikator, bereich) %>%
#     dplyr::mutate(props = wert[geschlecht == "Frauen"]/
#                     wert[geschlecht == "Gesamt"])
#
#   df <- df %>% dplyr::filter(geschlecht != "Gesamt")
#
#
#   df <- df %>% dplyr::filter(region %in% states)
#
#   # filter MINT or remaining subjects
#   df <- df %>% dplyr::filter(fachbereich == topic)
#
#   # order years for plot
#   df <- df[with(df, order(region, jahr, decreasing = TRUE)), ]
#
#   df$props <- df$props * 100
#
#   colnames(df) <- c("Region", "Fachbereich", "anforderung", "Wert","Indikator", "Jahr","Geschlecht",
#                         "Bereich", "Anteil")
#
#   return(df)
#
# }



# Funktion 8 ----


#' A function to plot time series
#'
#' @description A function to plot the time series of the german states
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
#
# arbeitsmarkt_verlauf_bl <- function(df,r) {
#
#   # load UI inputs from reactive value
#   timerange <- r$date_arbeitsmarkt_verlauf_bl
#
#   states <- r$states_arbeitsmarkt_verlauf_bl
#
#   topic <- r$topic_arbeitsmarkt_verlauf_bl
#
#   anforderung <- r$anforderung_arbeitsmarkt_verlauf_bl
#
#   # filter dataset based on UI inputs
#   df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])
#
#   df <- df %>% dplyr::filter(anforderung == anforderung)
#
#   # remove
#   df <- df %>% dplyr::filter(region != "Deutschland")
#
#   # call function to calculate the share of MINT and the remaining subjects
#   df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
#     df[df$fachbereich == "MINT", "wert"]
#
#   df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"
#
#   # include "Osten" und "Westen" in Dataframe
#   df <- prep_arbeitsmarkt_east_west(df)
#
#     # calculate the share of males
#   df <- calc_arbeitsmarkt_males(df)
#
#   # calculate new "Gesamt"
#   df <-  df %>% dplyr::filter(geschlecht != "Gesamt") %>%
#     dplyr::group_by(region, fachbereich, indikator, jahr) %>%
#     dplyr::mutate(props = wert[geschlecht == "Frauen"] +
#                     wert[geschlecht == "Männer"])
#
#   # calcualte proportions
#   df <- df %>% dplyr::group_by(region, indikator, fachbereich, geschlecht, jahr) %>%
#     dplyr::summarize(proportion = wert/props)
#
#   df$proportion <- df$proportion * 100
#
#
#
#   df <- df %>% dplyr::filter(region %in% states)
#
#   df <- df %>% dplyr::filter(fachbereich == topic)
#
#   df$geschlecht <- paste0(df$geschlecht, " (", df$indikator, ")")
#
#
#
#   # order years for plot
#   df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
#
#   if (anforderung == "Gesamt"){
#
#     title_help_sub_sub <- " insgesamt"
#
#   } else {
#
#     title_help_sub_sub <- paste0(" (",anforderung,")")
#
#   }
#
#
#   if (topic == "MINT"){
#
#     title_help <- paste0("MINT", title_help_sub_sub)
#
#   }else {
#
#     title_help <- paste0("allen anderen Berufszweigen", title_help_sub_sub)
#
#   }
#
#
#   # plot
#   highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = geschlecht)) %>%
#     highcharter::hc_tooltip(pointFormat = "Anteil {point.geschlecht} <br> Wert: {point.y} %") %>%
#     highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%")) %>%
#     highcharter::hc_xAxis(title = list(text = "Jahr"), style = list(fontFamily = "SourceSans3-Regular")) %>%
#     #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.", style = list(fontSize = "12px") ) %>%
#     highcharter::hc_title(text = paste0("Anteil von Frauen und Männer ", title_help ," im Verlauf"),
#                           margin = 45,
#                           align = "center",
#                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#     highcharter::hc_chart(
#       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
#     ) %>%
#     highcharter::hc_exporting(enabled = FALSE,
#                               buttons = list(contextButton = list(
#                                 symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
#                                 onclick = highcharter::JS("function () {
#                                                               this.exportChart({ type: 'image/png' }); }"),
#                                 align = 'right',
#                                 verticalAlign = 'bottom',
#                                 theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
#
# }



# Funktion 9 ----

#' A function to plot time series
#'
#' @description A function to plot the time series
#'
#' @return The return value, if any, from executing the function.
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# arbeitsmarkt_anforderungen_verlauf <- function(df,r) {
#
#   # load UI inputs from reactive value
#   timerange <- r$date_arbeitsmarkt_anforderungen_verlauf
#
#   states <- r$states_arbeitsmarkt_anforderungen_verlauf
#
#   indikator_choice <- r$indikator_arbeitsmarkt_anforderungen_verlauf
#
#   anforderung <- r$anforderung_arbeitsmarkt_anforderungen_verlauf
#
#   # filter dataset based on UI inputs
#   df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])
#
#   # remove
#   df <- df %>% dplyr::filter(geschlecht == "Gesamt")
#
#   # df <- df %>% dplyr::filter(anforderung != "Helfer")
#
#   df <- prep_arbeitsmarkt_east_west(df)
#
#   # calculate new "Gesamt
#   df_new_gesamt <- df %>% dplyr::filter(anforderung == "Gesamt")
#     # dplyr::group_by(region, fachbereich, indikator, jahr, geschlecht, bereich) %>%
#     # dplyr::summarise(wert = sum(wert)) %>%
#     # dplyr::mutate(anforderung = "Gesamt") %>%
#     # dplyr::ungroup()
#
#   df <- rbind(df %>% dplyr::filter(anforderung != "Gesamt"), df_new_gesamt)
#
#   # df <- calc_arbeitsmarkt_mint(df)
#
#   df_new_gesamt <- df_new_gesamt %>%
#     dplyr::filter(fachbereich == "Alle") %>%
#     dplyr::rename(wert_gesamt = "wert") %>%
#     dplyr::select(-c("fachbereich", "anforderung"))
#
#   df <- df %>% dplyr::left_join(df_new_gesamt, by = c("region", "indikator", "jahr", "geschlecht", "bereich")) %>%
#     dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
#     dplyr::select(-c("wert", "wert_gesamt"))
#
#   df <- df %>% dplyr::filter(region == states)
#
#   df <- df %>% dplyr::filter(indikator == indikator_choice)
#
#   df <- df %>% dplyr::filter(fachbereich == "MINT")
#
#   df <- df %>% dplyr::filter(anforderung %in% anforderung)
#
#   # plot
#   highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = anforderung)) %>%
#     highcharter::hc_tooltip(pointFormat = "Anteil <br> Bundesland: {point.region} <br> Wert: {point.y} %") %>%
#     highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
#     highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
#     #highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
#     highcharter::hc_title(text = paste0("Anteil der Arbeitnehmerinnen im Zeitverlauf in ", states),
#                           margin = 45,
#                           align = "center",
#                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#     highcharter::hc_chart(
#       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
#     ) %>%
#     highcharter::hc_exporting(enabled = FALSE,
#                               buttons = list(contextButton = list(
#                                 symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
#                                 onclick = highcharter::JS("function () {
#                                                               this.exportChart({ type: 'image/png' }); }"),
#                                 align = 'right',
#                                 verticalAlign = 'bottom',
#                                 theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
#
# }



# Funktion 10 ----

#' A function to plot a bar plot
#'
#' @description A function to create a bar plot for the "Beruf" tab
#'
#' @return The return value is a waffle chart
#' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# arbeitsmarkt_anforderungen_vergleich <- function(df,r) {
#
#   timerange <- r$date_arbeitsmarkt_anforderungen_vergleich
#
#   states <- r$states_arbeitsmarkt_anforderungen_vergleich
#
#   indikator_choice <- r$indikator_arbeitsmarkt_anforderungen_vergleich
#
#   # filter dataset based on UI inputs
#   df <- df %>% dplyr::filter(jahr == timerange)
#
#   # remove
#   df <- df %>% dplyr::filter(geschlecht == "Gesamt")
#
#   # df <- df %>% dplyr::filter(anforderung != "Helfer")
#
#   df <- prep_arbeitsmarkt_east_west(df)
#
#   # calculate new "Gesamt
#   # df_new_gesamt <- df %>% dplyr::filter(anforderung != "Gesamt") %>%                       ###kab
#   #   dplyr::group_by(region, fachbereich, indikator, jahr, geschlecht, bereich) %>%
#   #   dplyr::summarise(wert = sum(wert)) %>%
#   #   dplyr::mutate(anforderung = "Gesamt") %>%
#   #   dplyr::ungroup()
#
#   df_new_gesamt <- df %>% dplyr::filter(anforderung == "Gesamt")
#       #dplyr::group_by(region, fachbereich, indikator, jahr, geschlecht, bereich) %>%
#      # dplyr::summarise(wert = sum(wert)) %>%
#       # dplyr::mutate(anforderung = "Gesamt") %>%
#       #dplyr::ungroup()
#
#   df <- rbind(df %>% dplyr::filter(anforderung != "Gesamt"), df_new_gesamt)
#
#   # df <- calc_arbeitsmarkt_mint(df)
#
#   df_new_gesamt <- df_new_gesamt %>%
#     dplyr::filter(fachbereich == "Alle") %>%
#     dplyr::rename(wert_gesamt = "wert") %>%
#     dplyr::select(-c("fachbereich", "anforderung"))
#
#   df <- df %>% dplyr::filter(region == states)
#
#   df <- df %>% dplyr::left_join(df_new_gesamt, by = c("region", "indikator", "jahr", "geschlecht", "bereich")) %>%
#     dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
#     dplyr::select(-c("wert", "wert_gesamt"))
#
#   df <- df %>% dplyr::filter(indikator == indikator_choice)
#
#   df <- df %>% dplyr::filter(fachbereich == "MINT")
#
#   reihenfolge <- c("Experte", "Spezialist", "Fachkraft", "Gesamt")
#
#   df <- df %>%
#     dplyr::mutate(anforderung =  factor(anforderung, levels = reihenfolge)) %>%
#     dplyr::arrange(anforderung)
#
#   # plot
#   a <- ifelse(df$anforderung == "Gesamt", "#b16fab", "grey30")
#
#   ggplot2::ggplot(df, ggplot2::aes(y=anforderung, x=proportion)) +
#     ggplot2::geom_bar(stat="identity", fill = "#b16fab") +
#     ggplot2::geom_text(ggplot2::aes(label=paste(round(proportion),"%")), hjust = -0.3,
#                        fontface = "bold") +
#     ggplot2::theme_minimal() +
#     ggplot2::theme(
#       axis.text.y = ggplot2::element_text(colour = a),
#       text = ggplot2::element_text(size = 14),
#       plot.title = ggtext::element_markdown(hjust = 0.5)) +
#     ggplot2::ylab("") + ggplot2::xlab("Anteil") +
#     ggplot2::labs(title = paste0("<span style='font-size:20.5pt; color:black'>",
#                                  indikator_choice, ": Anteil der anforderungs im Vergleich in ", timerange,
#                                  "<br><br><br>"),
#                   fill = "") +
#     ggplot2::scale_y_discrete(expand = c(0,0)) +
#     ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))
#
# }



# Funktion 11 ----



#' A function to plot a graph.
#'
#' @description A function to create a pie chart for the first box
#' inside the tab "Beruf".
#'
#' @return The return value is a plot
#' @param df The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# arbeitsmarkt_einstieg_pie <- function(df,r) {
#
#   # load UI inputs from reactive value
#   timerange <- r$date_arbeitsmarkt_einstieg
#
#   df <- df %>% dplyr::filter(jahr == timerange)
#
#   df <- df %>% dplyr::filter(region == "Deutschland")
#
#   df <- df %>% dplyr::filter(anforderung == "Gesamt")
#
#   # call function to calculate the share of MINT and the remaining subjects
#   df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
#     df[df$fachbereich == "MINT", "wert"]
#
#   df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"
#
#   # calculate the share of males
#   df <- calc_arbeitsmarkt_males(df)
#
#   df_beschaeftigte <- df %>% dplyr::filter(indikator == "Beschäftigte")
#
#   df_beschaeftigte <- df_beschaeftigte %>% dplyr::filter(geschlecht == "Gesamt")
#
#   # calculate proportions
#   df_beschaeftigte <- share_pie_neu(df_beschaeftigte)
#
#   df_beschaeftigte$geschlecht <- df_beschaeftigte$fachbereich
#
#   df_auszubildende <- df %>% dplyr::filter(indikator == "Auszubildende")
#
#   df_auszubildende <- df_auszubildende %>% dplyr::filter(geschlecht == "Gesamt")
#
#   # calculate proportions
#   df_auszubildende <- share_pie_neu(df_auszubildende)
#
#   df_auszubildende$geschlecht <- df_auszubildende$fachbereich
#
#   plot_auszubildende <- highcharter::hchart(df_auszubildende, size = 280, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
#     highcharter::hc_tooltip(
#       pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#
#     highcharter::hc_colors(c("#efe8e6","#b16fab")) %>%
#     highcharter::hc_title(text = paste0("Berufswahl (Auszubildende)", br(), timerange),
#                           margin = 45,
#                           align = "center",
#                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#     highcharter::hc_chart(
#       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
#     highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
#     highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#                                            dataLabels = list(enabled = TRUE,  format='{point.y}%'), showInLegend = TRUE))
#
#
#
#   plot_beschaeftigte <- highcharter::hchart(df_beschaeftigte, size = 280, type = "pie", mapping = highcharter::hcaes(x = geschlecht, y = proportion)) %>%
#     highcharter::hc_tooltip(
#       pointFormat=paste('Anteil: {point.percentage:.0f}%')) %>%
#     highcharter::hc_colors(c("#efe8e6","#b16fab")) %>%
#     highcharter::hc_title(text = paste0("Berufswahl (Beschäftigte)", br(), timerange),
#                           margin = 45,
#                           align = "center",
#                           style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
#     highcharter::hc_chart(
#       style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
#     highcharter::hc_legend(enabled = TRUE, reversed = TRUE) %>%
#     highcharter::hc_plotOptions(pie = list(allowPointSelect = TRUE, curser = "pointer",
#                                            dataLabels = list(enabled = TRUE, format='{point.y}%'), showInLegend = TRUE))
#
#
#
#     plot_beschaeftigte <- plot_beschaeftigte %>% highcharter::hc_colors(c("#efe8e6","#b16fab"))
#
#     plot_auszubildende <- plot_auszubildende %>% highcharter::hc_colors(c("#efe8e6","#b16fab"))
#
#
#   highcharter::hw_grid(
#
#     plot_auszubildende,
#
#     plot_beschaeftigte,
#
#     ncol = 2,
#     browsable = TRUE
#   )
#
#
# }



# Funktion 12 ----



#' A function to return a filtered dataset
#'
#' @description A function to similar to 'arbeitsmarkt_einstieg_bar' but with the
#' difference that it returns a dataframe instead of plot.
#'
#' @return The return value is a dataframe
#' @param df The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

# data_einstieg_beruf <- function(df,r) {
#
#   # load UI inputs from reactive value
#   timerange <- r$date_arbeitsmarkt_einstieg
#
#   # filter dataset based on UI inputs
#   df <- df %>% dplyr::filter(jahr == timerange)
#
#   df <- df %>% dplyr::filter(region == "Deutschland")
#
#   df <- df %>% dplyr::filter(anforderung == "Gesamt")
#
#   # call function to calculate the share of MINT and the remaining subjects
#   df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
#     df[df$fachbereich == "MINT", "wert"]
#
#   df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"
#
#   # calculate the share of males
#   df <-   df <- calc_arbeitsmarkt_males(df)
#
#   colnames(df) <- c("Region", "Fachbereich", "anforderung", "Wert", "Indikator", "Jahr", "Geschlecht", "Bereich")
#
#   return(df)
#
# }



# Funktion 13 ----



#' #' A function to plot time series
#' #'
#' #' @description A function to plot the time series
#' #'
#' #' @return The return value, if any, from executing the function.
#' #' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' arbeitsmarkt_anforderungen_verlauf_gender <- function(df,r) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_arbeitsmarkt_anforderungen_gender_verlauf
#'
#'   anforderung <- r$level_arbeitsmarkt_anforderungen_gender_verlauf
#'
#'   states <- r$states_arbeitsmarkt_anforderungen_gender_verlauf
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])
#'
#'   # remove
#'   #df <- df %>% dplyr::filter(anforderung != "Helfer")### kab
#'
#'   df <- prep_arbeitsmarkt_east_west(df)
#'
#'   # calculate new "Gesamt
#'   df_new_gesamt <- df %>% dplyr::filter(anforderung == "Gesamt")
#'   # dplyr::group_by(region, fachbereich, indikator, jahr, geschlecht, bereich) %>%
#'   # dplyr::summarise(wert = sum(wert)) %>%
#'   # dplyr::mutate(anforderung = "Gesamt") %>% dplyr::ungroup()
#'
#'
#'
#'   df <- rbind(df %>% dplyr::filter(anforderung != "Gesamt"), df_new_gesamt)
#'
#'   df <- df %>% dplyr::filter(region %in% states)
#'
#'   df <- calc_arbeitsmarkt_males(df)
#'
#'   df <- calc_arbeitsmarkt_mint(df)
#'
#'   df_total_gender <- calc_arbeitsmarkt_males(df_new_gesamt) %>%
#'     dplyr::filter(geschlecht != "Gesamt") %>%
#'     dplyr::filter(fachbereich == "Alle") %>%
#'     dplyr::select("region", "indikator", "jahr", "wert", "geschlecht") %>%
#'     dplyr::rename(wert_gesamt = "wert")
#'
#'   df <- df %>% dplyr::left_join(df_total_gender, by = c("region", "indikator", "jahr", "geschlecht")) %>%
#'     dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
#'     dplyr::select(-c("wert", "wert_gesamt")) %>%
#'     dplyr::filter(geschlecht == "Frauen")
#'
#'   df <- df %>% dplyr::filter(anforderung %in% anforderung)
#'
#'   df <- df %>% dplyr::filter(fachbereich == "MINT")
#'
#'   df$geschlecht <- paste0(df$geschlecht, " (", df$indikator, ")")
#'
#'   # order years for plot
#'   df <- df[with(df, order(region, jahr, decreasing = FALSE)), ]
#'
#'   if (anforderung == "Gesamt"){
#'
#'     title_help <- " insgesamt"
#'
#'   } else {
#'
#'     title_help <- paste0(" mit anforderung ", anforderung)
#'
#'   }
#'
#'   # plot
#'   highcharter::hchart(df, 'line', highcharter::hcaes(x = jahr, y = round(proportion), group = geschlecht)) %>%
#'     highcharter::hc_tooltip(pointFormat = "Anteil <br> Bundesland: {point.region} <br> Wert: {point.y} %") %>%
#'     highcharter::hc_yAxis(title = list(text = "Wert"), labels = list(format = "{value}%"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
#'     highcharter::hc_xAxis(title = list(text = "Jahr"), allowDecimals = FALSE, style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")) %>%
#'     # highcharter::hc_caption(text = "Quelle: Bundesagentur für Arbeit 2021, auf Anfrage, eigene Berechnungen.",  style = list(fontSize = "12px") ) %>%
#'     highcharter::hc_title(text = paste0("Arbeitnehmerinnen: ", title_help, " in ", states),
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
#

# Funktion 14 -----



#'
#'
#' #' A function to create a dumbbell plot
#' #'
#' #' @description A function to return a ranking of subject for both genders
#' #'
#' #' @return The return value is a dumbbell plot
#' #' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' arbeitsmarkt_anforderungen_vergleich_gender <- function(df, r) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_arbeitsmarkt_anforderungen_gender_vegleich
#'
#'   states <- r$states_arbeitsmarkt_anforderungen_gender_vegleich
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr == timerange)
#'
#'   # remove
#'   # df <- df %>% dplyr::filter(anforderung != "Helfer")
#'
#'   df <- prep_arbeitsmarkt_east_west(df)
#'
#'   # calculate new "Gesamt
#'   df_new_gesamt <- df %>% dplyr::filter(anforderung == "Gesamt")
#'   # dplyr::group_by(region, fachbereich, indikator, jahr, geschlecht, bereich) %>%
#'   # dplyr::summarise(wert = sum(wert)) %>%
#'   # dplyr::mutate(anforderung = "Gesamt") %>% dplyr::ungroup()
#'
#'   df <- rbind(df %>% dplyr::filter(anforderung != "Gesamt"), df_new_gesamt)
#'
#'   df <- df %>% dplyr::filter(region %in% states)
#'
#'   df <- calc_arbeitsmarkt_males(df)
#'
#'   df <- calc_arbeitsmarkt_mint(df)
#'
#'   df_total_gender <- calc_arbeitsmarkt_males(df_new_gesamt) %>%
#'     dplyr::filter(geschlecht != "Gesamt") %>%
#'     dplyr::filter(fachbereich == "Alle") %>%
#'     dplyr::select("region", "indikator", "jahr", "wert", "geschlecht") %>%
#'     dplyr::rename(wert_gesamt = "wert")
#'
#'   df <- df %>% dplyr::left_join(df_total_gender, by = c("region", "indikator", "jahr", "geschlecht")) %>%
#'     dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
#'     dplyr::select(-c("wert", "wert_gesamt")) %>%
#'     dplyr::filter(geschlecht == "Frauen")
#'
#'   df <- df %>% dplyr::filter(fachbereich == "MINT")
#'
#'   # spread column
#'   df <- tidyr::spread(df, indikator, proportion)
#'
#'   df <- df %>% tidyr::drop_na()
#'
#'   df2 <- tidyr::gather(df, group, value, -anforderung) %>%
#'     dplyr::filter(group %in% c("Beschäftigte", "Auszubildende")) %>%
#'     dplyr::mutate(value = as.numeric(value))
#'
#'   df$anforderung <- reorder(df$anforderung, df$Beschäftigte)
#'
#'   df2$anforderung <- factor(df2$anforderung, levels = levels(df$anforderung))
#'
#'   ggplot2::ggplot(df,
#'                   ggplot2::aes(y = anforderung)) +
#'     ggplot2::geom_point(data = df2, ggplot2::aes(x = value, color = group), size = 5) +
#'     ggalt::geom_dumbbell(
#'       ggplot2::aes(x = Auszubildende, xend = Beschäftigte),
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
#'                                  "Frauen: Wahl von MINT-Berufen <br>", states, " in ",timerange,
#'                                  "<br><br><br>"),
#'                   color = "") +
#'     ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))
#'
#' }
#'


# Funktion 15 ----


#'
#'
#' #' A function to create a dumbbell plot
#' #'
#' #' @description A function to return a ranking of subject for both genders
#' #'
#' #' @return The return value is a dumbbell plot
#' #' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' arbeitsmarkt_anforderungen_vergleich_gender <- function(df, r) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_arbeitsmarkt_anforderungen_gender_vegleich
#'
#'   states <- r$states_arbeitsmarkt_anforderungen_gender_vegleich
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr == timerange)
#'
#'   # remove
#'   # df <- df %>% dplyr::filter(anforderung != "Helfer")
#'
#'   df <- prep_arbeitsmarkt_east_west(df)
#'
#'   # calculate new "Gesamt
#'   df_new_gesamt <- df %>% dplyr::filter(anforderung == "Gesamt")
#'   # dplyr::group_by(region, fachbereich, indikator, jahr, geschlecht, bereich) %>%
#'   # dplyr::summarise(wert = sum(wert)) %>%
#'   # dplyr::mutate(anforderung = "Gesamt") %>% dplyr::ungroup()
#'
#'   df <- rbind(df %>% dplyr::filter(anforderung != "Gesamt"), df_new_gesamt)
#'
#'   df <- df %>% dplyr::filter(region %in% states)
#'
#'   df <- calc_arbeitsmarkt_males(df)
#'
#'   df <- calc_arbeitsmarkt_mint(df)
#'
#'   df_total_gender <- calc_arbeitsmarkt_males(df_new_gesamt) %>%
#'     dplyr::filter(geschlecht != "Gesamt") %>%
#'     dplyr::filter(fachbereich == "Alle") %>%
#'     dplyr::select("region", "indikator", "jahr", "wert", "geschlecht") %>%
#'     dplyr::rename(wert_gesamt = "wert")
#'
#'   df <- df %>% dplyr::left_join(df_total_gender, by = c("region", "indikator", "jahr", "geschlecht")) %>%
#'     dplyr::mutate(proportion = (wert/wert_gesamt)*100) %>%
#'     dplyr::select(-c("wert", "wert_gesamt")) %>%
#'     dplyr::filter(geschlecht == "Frauen")
#'
#'   df <- df %>% dplyr::filter(fachbereich == "MINT")
#'
#'   # spread column
#'   df <- tidyr::spread(df, indikator, proportion)
#'
#'   df <- df %>% tidyr::drop_na()
#'
#'   df2 <- tidyr::gather(df, group, value, -anforderung) %>%
#'     dplyr::filter(group %in% c("Beschäftigte", "Auszubildende")) %>%
#'     dplyr::mutate(value = as.numeric(value))
#'
#'   df$anforderung <- reorder(df$anforderung, df$Beschäftigte)
#'
#'   df2$anforderung <- factor(df2$anforderung, levels = levels(df$anforderung))
#'
#'   ggplot2::ggplot(df,
#'                   ggplot2::aes(y = anforderung)) +
#'     ggplot2::geom_point(data = df2, ggplot2::aes(x = value, color = group), size = 5) +
#'     ggalt::geom_dumbbell(
#'       ggplot2::aes(x = Auszubildende, xend = Beschäftigte),
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
#'                                  "Frauen: Wahl von MINT-Berufen <br>", states, " in ",timerange,
#'                                  "<br><br><br>"),
#'                   color = "") +
#'     ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))
#'
#' }


# Funktion 16 ----


#'
#'
#' #' A function to create a bar plot
#' #'
#' #' @description A function to return a ranking of subject for both genders
#' #'
#' #' @return The return value is a bar plot
#' #' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' arbeitsmarkt_bl_gender_vergleich <- function(df, r) {
#'
#'   # load UI inputs from reactive value
#'   timerange <- r$date_arbeitsmarkt_bl_gender_vergleich
#'
#'   #anforderung <- r$anforderung_arbeitsmarkt_bl_gender_vergleich
#'
#'   # filter dataset based on UI inputs
#'   df <- df %>% dplyr::filter(jahr == timerange)
#'
#'   # remove
#'   df <- df %>% dplyr::filter(region != "Deutschland")
#'
#'   df <- calc_arbeitsmarkt_males(df)
#'
#'   df_gesamt <- df %>%
#'     dplyr::filter(fachbereich == "Alle",
#'                   anforderung == "Gesamt")
#'
#'   df <- df %>%
#'     dplyr::left_join(df_gesamt, by = c("region", "jahr", "geschlecht", "indikator", "bereich")) %>%
#'     dplyr::rename(anforderung = "anforderung.x",
#'                   fachbereich = "fachbereich.x",
#'                   wert = "wert.x",
#'                   wert_sum = "wert.y") %>%
#'     dplyr::select(-c("fachbereich.y", "anforderung.y")) %>%
#'     dplyr::mutate(proportion = (wert/wert_sum)*100)%>%
#'     dplyr::filter(anforderung == "Gesamt",
#'                   fachbereich == "MINT")
#'
#'   df <- df %>% dplyr::filter(anforderung == "Gesamt")
#'
#'   df <- df %>% dplyr::filter(geschlecht == "Frauen")
#'
#'   df <- df %>% dplyr::filter(fachbereich == "MINT")
#'
#'   df <- df %>% dplyr::select(-c("wert", "wert_sum"))
#'
#'   # spread column
#'   df <- tidyr::spread(df, indikator, proportion)
#'
#'   df <- df %>% tidyr::drop_na()
#'
#'   df2 <- tidyr::gather(df, group, value, -region) %>%
#'     dplyr::filter(group %in% c("Beschäftigte", "Auszubildende")) %>%
#'     dplyr::mutate(value = as.numeric(value))
#'
#'   df$region <- reorder(df$region, df$Beschäftigte)
#'
#'   df2$region <- factor(df2$region, levels = levels(df$region))
#'
#'   # if (anforderung == "Gesamt"){
#'   #
#'   #   title_help <- " insgesamt"
#'   #
#'   # } else {
#'   #
#'   #   title_help <- paste0(" mit anforderung ", anforderung)
#'   #
#'   # }
#'
#'   ggplot2::ggplot(df,
#'                   ggplot2::aes(y = region)) +
#'     ggplot2::geom_point(data = df2, ggplot2::aes(x = value, color = group), size = 5) +
#'     ggalt::geom_dumbbell(
#'       ggplot2::aes(x = Auszubildende, xend = Beschäftigte),
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
#'                                  "Frauen: Wahl von MINT-Berufen<br>"
#'                                  #, title_help
#'                                  ,timerange,
#'                                  "<br><br><br>"),
#'                   color = "") +
#'     ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))
#'
#' }
#'


# Funktion 17 ----

#'
#'
#' #' A function to plot a table
#' #'
#' #' @description A function to create a table for detailed overview for landkreise
#' #'
#' #' @return The return value is a table
#' #' @param df The dataframe "Arbeitsmarkt_detailliert.xlsx" needs to be used for this function
#' #' @param r Reactive variable that stores all the inputs from the UI
#' #' @noRd
#'
#' arbeitsmarkt_lk_detail_table <- function(df, input_values, r) {
#'
#'   # get input variables
#'   input_count <- stringr::str_sub(names(input_values), 1, 4) %>% unique()
#'   variable_counts <- input_count[input_count %>% stringr::str_detect("var")] %>% sort()
#'
#'   state1 <- r[["mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_1-states_beruf_arbeitsmarkt_landkreis_table"]]
#'   state2 <- r[["mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_2-states_beruf_arbeitsmarkt_landkreis_table"]]
#'   state3 <- r[["mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_3-states_beruf_arbeitsmarkt_landkreis_table"]]
#'
#'   region1 <- r[["mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_1-region_beruf_arbeitsmarkt_landkreis_table"]]
#'   region2 <- r[["mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_2-region_beruf_arbeitsmarkt_landkreis_table"]]
#'   region3 <- r[["mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_3-region_beruf_arbeitsmarkt_landkreis_table"]]
#'
#'
#'   # create empty dataframe
#'   df_steckbrief <- data.frame()
#'
#'   # for each 'Betrachtung' = variable_counts, get detailed input, calculate
#'   # values and build display dataframe
#'   for(i in variable_counts){
#'
#'     category <- input_values[[paste0(i, "-kategorie_beruf_arbeitsmarkt_landkreis_vergleich")]]
#'     domain <- input_values[[paste0(i, "-fachbereich_beruf_arbeitsmarkt_landkreis_vergleich")]]
#'     indikator_azubi <- input_values[[paste0(i, "-indikator1_beruf_arbeitsmarkt_landkreis_vergleich")]]
#'     indikator_besch <- input_values[[paste0(i, "-indikator2_beruf_arbeitsmarkt_landkreis_vergleich")]]
#'
#'     df_compare_list_region1 <- calculate_landkreis(df, state1, category, domain, indikator_azubi, indikator_besch, region1)
#'     if(region1 != "Gesamt"){
#'       df_compare_list_region1[[1]] <- df_compare_list_region1[[1]] %>% dplyr::filter(landkreis == region1)
#'     }
#'
#'     df_compare_list_region2 <- calculate_landkreis(df, state2, category, domain, indikator_azubi, indikator_besch, region2)
#'
#'     if(region2 != "Gesamt"){
#'       df_compare_list_region2[[1]] <- df_compare_list_region2[[1]] %>% dplyr::filter(landkreis == region2)
#'     }
#'
#'     df_compare_list_region3 <- calculate_landkreis(df, state3, category, domain, indikator_azubi, indikator_besch, region3)
#'
#'     if(region3 != "Gesamt"){
#'       df_compare_list_region3[[1]] <- df_compare_list_region3[[1]] %>% dplyr::filter(landkreis == region3)
#'     }
#'
#'     line_name <- paste(category, domain, ifelse(category=="Auszubildende", indikator_azubi, indikator_besch), sep = "-")
#'     df_var <- data.frame(line_name = line_name,
#'                          region1 = paste0(df_compare_list_region1[[1]]$wert, "<br/>(", df_compare_list_region1[[1]]$prob, "% ", df_compare_list_region1[[3]], " an ", df_compare_list_region1[[2]], ")"),
#'                          region2 = paste0(df_compare_list_region2[[1]]$wert, "<br/>(", df_compare_list_region2[[1]]$prob, "% ", df_compare_list_region2[[3]], " an ", df_compare_list_region2[[2]], ")"),
#'                          region3 = paste0(df_compare_list_region3[[1]]$wert, "<br/>(", df_compare_list_region3[[1]]$prob, "% ", df_compare_list_region3[[3]], " an ", df_compare_list_region3[[2]], ")"))
#'
#'
#'     df_steckbrief <- dplyr::bind_rows(df_steckbrief, df_var)
#'
#'   }
#'
#'   # adjust names for the dataframe
#'   names(df_steckbrief) <- c("", paste0("<b>", state1, "-" ,region1, "</b>"), paste0("<b>", state2, "-" ,region2, "</b>"), paste0("<b>", state3, "-" ,region3, "</b>"))
#'
#'   return(df_steckbrief)
#'
#' }
#'








