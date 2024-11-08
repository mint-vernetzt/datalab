#' ausserschulisch cp orgas Plot
#'
#' @noRd
plot_cp_orgas <- function(r){
  # hier bar-plot für orgas
  regio <- r$regio_cp_orgas
  charas <- r$chara_cp_orgas
  abs_rel_select <- r$abs_rel_cp_orgas #fehlt

  df <- dplyr::tbl(con, "ausserschulisch_cp_organisationen") %>%
    dplyr::filter(region == regio,
                  typ == charas) %>%
    dplyr::mutate(wert = as.numeric(wert)) %>%
    dplyr::collect()


  if(abs_rel_select == "In Prozent"){

    ges <- df$wert[df$indikator == "Gesamt"]
    df_ges <- df %>% dplyr::filter(indikator == "Gesamt") %>%
      dplyr::rename(wert_ges = wert) %>%
      dplyr::select(-indikator)
    df <- df %>% dplyr::filter(indikator != "Gesamt")

    df <- df %>%
      dplyr::left_join(df_ges, by = c("region","typ")) %>%
      dplyr::mutate(prop = round(wert/wert_ges*100, 1))

    # Titel
    titel <- paste0("Organisationen der Community-Plattform von MINTvernetzt nach ",
                    charas)
    subtitel <- paste0("Angaben von ",  ges, " Organisationen. Mehrfachantworten möglich.")


    # Plot
    out <- highcharter::hchart(df, 'column', highcharter::hcaes(y = prop, x = indikator))%>%
      highcharter::hc_plotOptions(column = list(pointWidth = 50,
                                                colorByPoint = TRUE,
                                                colors = c("#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
                                                           "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a",
                                                           "#007655", "#dc6262"))
                                  )%>%
      highcharter::hc_tooltip(pointFormat = "{point.y} %")%>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f} %"),
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular"), pointsWidth=100) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      # highcharter::hc_colors(c("#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
      #                          "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a")) %>%
      highcharter::hc_title(text = titel,
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_subtitle(text = subtitel,
                               align = "center",
                               style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "16px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
      highcharter::hc_exporting(enabled = FALSE,
                                buttons = list(contextButton = list(
                                  symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                  onclick = highcharter::JS("function () {
                                                            this.exportChart({ type: 'image/png' }); }"),
                                  align = 'right',
                                  verticalAlign = 'bottom',
                                  theme = list(states = list(hover = list(fill = '#FFFFFF'))))))
  }else{

    ges <- df$wert[df$indikator == "Gesamt"]
    df_ges <- df %>% dplyr::filter(indikator == "Gesamt") %>%
      dplyr::rename(wert_ges = wert) %>%
      dplyr::select(-indikator)
    df <- df %>% dplyr::filter(indikator != "Gesamt")

    # Titel
    titel <- paste0("Organisationen der Community-Plattform von MINTvernetzt nach ",
                    charas)
    subtitel <- paste0("Angaben von ",  ges, " Organisationen. Mehrfachantworten möglich.")


    # Plot
    out <- highcharter::hchart(df, 'column', highcharter::hcaes(y = wert, x = indikator))%>%
      highcharter::hc_plotOptions(column = list(pointWidth = 50,
                                                colorByPoint = TRUE,
                                                colors = c("#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
                                                           "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a",
                                                           "#007655", "#dc6262"))
      )%>%
      highcharter::hc_tooltip(pointFormat = "{point.y}")%>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"),
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular"), pointsWidth=100) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%highcharter::hc_title(text = titel,
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_subtitle(text = subtitel,
                               align = "center",
                               style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "16px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
      highcharter::hc_exporting(enabled = FALSE,
                                buttons = list(contextButton = list(
                                  symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                  onclick = highcharter::JS("function () {
                                                            this.exportChart({ type: 'image/png' }); }"),
                                  align = 'right',
                                  verticalAlign = 'bottom',
                                  theme = list(states = list(hover = list(fill = '#FFFFFF'))))))


  }

return(out)

}

# SkF ----

#' A function to create a bar plot
#'
#' @description A function to return the number of SkF certified organisations
#'
#' @return The return value is a bar plot
#' @param data The dataframe "iqb" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd


skf_einrichtungen <- function(r){

  # reactive values einlesen
  timerange <- r$date_skf_einrichtungen
  t <- as.numeric(timerange[1]:timerange[2])

  ort_select <- r$ort_skf_einrichtungen


  # Alle Einrichtungen berechnen und gewählte Einrichtung filtern
  if(ort_select == "Alle Einrichtungen"){
    df <- dplyr::tbl(con, from = "ausserschulisch_skf") %>%
      dplyr::filter(indikator %in% c("Einrichtungen mit SKf-Fortbildung",
                                     "zertifizierte Einrichtungen"),
                    jahr %in% t) %>%
      dplyr::select(-bereich) %>%
      dplyr::group_by(indikator, jahr) %>%
      dplyr::summarise(wert = sum(wert)) %>%
      dplyr::ungroup() %>%
      dplyr::collect()

    df$einrichtung <- "Alle Einrichtungen"
  }else{
    df <- dplyr::tbl(con, from = "ausserschulisch_skf") %>%
      dplyr::filter(indikator %in% c("Einrichtungen mit SKf-Fortbildung",
                                     "zertifizierte Einrichtungen"),
                    jahr %in% t,
                    einrichtung == ort_select) %>%
      dplyr::select(-bereich) %>%
      dplyr::collect()

  }

  # Gesamtanzahl für Hover-Box ergänzen
  df <- df %>%
    dplyr::group_by(einrichtung, jahr) %>%
    dplyr::mutate(gesamt = sum(wert)) %>%
    dplyr::ungroup()

  #Trennpunkte für lange Zahlen ergänzen
  df$gesamt <- prettyNum(df$gesamt, big.mark = ".", decimal.mark = ",")

  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- "."
  options(highcharter.lang = hcoptslang)

  # Hilfe für Überschrift
  helper <- ort_select
  helper <- ifelse(helper == "Alle Einrichtungen", "Kitas, Horte und Grundschulen", helper)
  helper <- ifelse(helper == "Grundschule", "Grundschulen", helper)
  helper <- ifelse(helper == "Kita", "Kitas", helper)
  helper <- ifelse(helper == "Hort", "Horte", helper)

  # Plot erstellem
  out <- highcharter::hchart(df, 'column', highcharter::hcaes(y = wert, x = jahr, group = indikator))%>%
    highcharter::hc_tooltip(pointFormat = "{point.indikator} <br> Anzahl: {point.y} <br> aktive Einrichtungen gesamt: {point.gesamt}")%>%
    # highcharter::hc_size(height = 1000)%>%
    highcharter::hc_yAxis(title = list(text = "")
                          , labels = list(format = "{value:, f}"), style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular")
    ) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_plotOptions(column = list(stacking = "normal")) %>%
    highcharter::hc_plotOptions(column = list(pointWidth = 50))%>%
    highcharter::hc_colors(c("#efe8e6", "#66cbaf")) %>%
    highcharter::hc_title(text = paste0(helper, ", die bei Stiftung Kinder forschen aktiv sind"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = T) %>%
    highcharter::hc_exporting(enabled = FALSE,
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                            this.exportChart({ type: 'image/png' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

  return(out)
}

#' A function to create a bar plot
#'
#' @description A function to return the number of SkF certified/educated personal
#'
#' @return The return value is a bar plot
#' @param data The dataframe "iqb" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd

skf_personal <- function(r){

  # reactive values einlesen
  timerange <- r$time_skf_personal
  t <- as.character(timerange[1]:timerange[2])

  ort_select <- r$ort_skf_personal

  # Datensatz filtern
  # Alle Einrichtungen berechnen und gewählte Einrichtung filtern
  if(ort_select == "Alle Einrichtungen"){
    df <- dplyr::tbl(con, from = "ausserschulisch_skf") %>%
      dplyr::filter(indikator %in% c("insgesamt fortgebildete Fach- / Lehrkräfte",
                                     "neu fortgebildete Fach- / Lehrkräfte"),
                    jahr %in% t) %>%
      dplyr::select(-bereich) %>%
      dplyr::group_by(indikator, jahr) %>%
      dplyr::summarise(wert = sum(wert)) %>%
      dplyr::ungroup() %>%
      dplyr::collect()

    df$einrichtung <- "Alle Einrichtungen"
  }else{
    df <- dplyr::tbl(con, from = "ausserschulisch_skf") %>%
      dplyr::filter(indikator %in% c("insgesamt fortgebildete Fach- / Lehrkräfte",
                                     "neu fortgebildete Fach- / Lehrkräfte"),
                    jahr %in% t,
                    einrichtung == ort_select) %>%
      dplyr::select(-bereich) %>%
      dplyr::collect()

  }


  # Labels anpassen
  df$indikator[df$indikator == "neu fortgebildete Fach- / Lehrkräfte"] <- "in diesem Jahr fortgebildet"
  df$indikator[df$indikator == "insgesamt fortgebildete Fach- / Lehrkräfte"] <- "bis jetzt insgesamt fortgebildet"

  # Plot
  out <- highcharter::hchart(df, 'column', highcharter::hcaes(y = wert, x = jahr, group=indikator))%>%
    highcharter::hc_plotOptions(column = list(pointWidth = 50))%>%
    highcharter::hc_tooltip(pointFormat = "{point.indikator}: {point.y}")%>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"),
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular"), pointsWidth=100) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
    #  highcharter::hc_plotOptions(column = list(stacking = "percent")) %>%
    highcharter::hc_colors(c("#66cbaf","#8893a7")) %>%
    highcharter::hc_title(text = paste0("Geschätzte Anzahl an Fach- und Lehrkräften, die an einer SKf-Fortbildung teilgenommen haben"),
                          margin = 45,
                          align = "center",
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
    highcharter::hc_exporting(enabled = FALSE,
                              buttons = list(contextButton = list(
                                symbol = 'url(https://upload.wikimedia.org/wikipedia/commons/f/f7/Font_Awesome_5_solid_download.svg)',
                                onclick = highcharter::JS("function () {
                                                            this.exportChart({ type: 'image/png' }); }"),
                                align = 'right',
                                verticalAlign = 'bottom',
                                theme = list(states = list(hover = list(fill = '#FFFFFF'))))))

  return(out)
}
