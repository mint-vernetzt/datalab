# Community Plattform ----

#' ausserschulisch cp orgas Plot
#'
#' @noRd
plot_cp_orgas <- function(r){

  charas <- r$chara_cp_orgas
  abs_rel_select <- r$abs_rel_cp_orgas
  if(charas != "Region"){
    regio <- r$regio_cp_orgas
  }else{
    regio <- "Gesamt"
  }

  if(charas != "Region"){
    regio <- r$regio_cp_orgas

    # df <- dplyr::tbl(con, "ausserschulisch_cp_organisationen") %>%
    #   dplyr::filter(region == regio,
    #                 typ == charas) %>%
    #   dplyr::mutate(wert = as.numeric(wert)) %>%
    #   dplyr::collect()


    df_query <- glue::glue_sql("
    SELECT *
    FROM ausserschulisch_cp_organisationen
    WHERE region = {regio}
    AND typ = {charas}
                               ", .con = con)
    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::mutate(wert = as.numeric(wert))

  }else{
    bula_anzeigen <- r$bula_cp_orgas

    # df <- dplyr::tbl(con, "ausserschulisch_cp_organisationen") %>%
    #   dplyr::filter(region == "Gesamt",
    #                 typ == charas) %>%
    #   dplyr::mutate(wert = as.numeric(wert)) %>%
    #   dplyr::collect()

    df_query <- glue::glue_sql("
    SELECT *
    FROM ausserschulisch_cp_organisationen
    WHERE region = 'Gesamt'
    AND typ = {charas}
                               ", .con = con)
    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::mutate(wert = as.numeric(wert))

    bula_de <- c("Gesamt",
                 "Bundesweit",
                 "Baden-Württemberg",
                 "Bayern",
                 "Berlin",
                 "Brandenburg",
                 "Bremen",
                 "Hamburg",
                 "Hessen",
                 "Mecklenburg-Vorpommern",
                 "Niedersachsen",
                 "Nordrhein-Westfalen",
                 "Rheinland-Pfalz",
                 "Saarland",
                 "Sachsen",
                 "Sachsen-Anhalt",
                 "Schleswig-Holstein",
                 "Thüringen")
    df_klein <- df %>%
      dplyr::filter(!(indikator %in% bula_de)) %>%
      dplyr::summarise(wert = sum(wert, na.rm = TRUE)) %>%
      dplyr::mutate(indikator = "lokal aktiv",
                    region = "Gesamt",
                    typ = "Region")

    if(bula_anzeigen == "Bundesländern zusammen anzeigen"){
      bula <- setdiff(bula_de, c("Bundesweit", "Gesamt"))

      df_bula <- df %>%
        dplyr::filter(indikator %in% bula) %>%
        dplyr::summarise(wert = sum(wert, na.rm = TRUE)) %>%
        dplyr::mutate(indikator = "auf Bundeslandebene aktiv",
                      region = "Gesamt",
                      typ = "Region")

      df <- df %>%
        dplyr::filter(indikator %in% c("Bundesweit", "Gesamt"))

      df <- rbind(df, df_bula, df_klein)
    }else{
      df <- df %>%
        dplyr::filter(indikator %in% bula_de)

      df <- rbind(df, df_klein)
    }

  }
  if(length(df$wert) == 0){

    titel <- paste0("Für die gewählten Eingaben hat keine Organisation eine Angabe gemacht.")

    tooltip <- "{point.indikator} <br> Anteil: {point.prop_disp} %"
    format <- "{value}%"
    color <- c("#b16fab", "#154194","#66cbaf","#112c5f", "#35bd97", "#5d335a",
               "#5f94f9", "#007655", "#d0a9cd")

    out <- linebuilder(df, titel, x = "indikator", y = "wert", group = "region", tooltip, format, color)


  }else if(abs_rel_select == "In Prozent"){

    ges <- df$wert[df$indikator == "Gesamt"]
    df_ges <- df %>% dplyr::filter(indikator == "Gesamt") %>%
      dplyr::rename(wert_ges = wert) %>%
      dplyr::select(-indikator)
    df <- df %>% dplyr::filter(indikator != "Gesamt")

    df <- df %>%
      dplyr::left_join(df_ges, by = c("region","typ")) %>%
      dplyr::mutate(prop = round(wert/wert_ges*100, 1))

    df <- df[with(df, order(prop, decreasing = TRUE)),]

    # Titel
    if(regio == "Gesamt"){
      regio_angabe <- ""
    }else if(regio == "Bundesweit"){
      regio_angabe <- paste0(" die ", regio, " tätig sind")
    }else{
      regio_angabe <- paste0(" die in ", regio, " tätig sind")
    }
    titel <- paste0("Anteile der Organisationen der Community-Plattform von MINTvernetzt nach ",
                    charas, regio_angabe)
    subtitel <- paste0("Angaben wurden von <b>", ges, "</b> Organisationen gemacht. Mehrfachangabe möglich.")


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

    df <- df[with(df, order(wert, decreasing = TRUE)),]

    # Titel
    if(regio == "Gesamt"){
      regio_angabe <- ""
    }else if(regio == "Bundesweit"){
      regio_angabe <- paste0(" die ", regio, " tätig sind")
    }else{
      regio_angabe <- paste0(" die in ", regio, " tätig sind")
    }
    titel <- paste0("Organisationen der Community-Plattform von MINTvernetzt nach ",
                    charas, regio_angabe)
    subtitel <- paste0("Angaben wurden von <b>", ges, "</b> Organisationen gemacht. Mehrfachangabe möglich.")

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

#' ausserschulisch cp projekte Plot
#'
#' @noRd
plot_cp_projekte <- function(r){

  charas <- r$chara_cp_pros
  abs_rel_select <- r$abs_rel_cp_pros
  if(charas != "Region"){
    regio <- r$regio_cp_pros
#
#     df <- dplyr::tbl(con, "ausserschulisch_cp_projekte") %>%
#       dplyr::filter(region == regio,
#                     typ == charas) %>%
#       dplyr::mutate(wert = as.numeric(wert)) %>%
#       dplyr::collect()


    df_query <- glue::glue_sql("
    SELECT *
    FROM ausserschulisch_cp_projekte
    WHERE region = {regio}
    AND typ = {charas}
                               ", .con = con)
    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::mutate(wert = as.numeric(wert))


  }else{
    bula_anzeigen<- r$bula_cp_pros
    regio <- ""
    # df <- dplyr::tbl(con, "ausserschulisch_cp_projekte") %>%
    #   dplyr::filter(region == "Gesamt",
    #                 typ == charas) %>%
    #   dplyr::mutate(wert = as.numeric(wert)) %>%
    #   dplyr::collect()


    df_query <- glue::glue_sql("
    SELECT *
    FROM ausserschulisch_cp_projekte
    WHERE region = 'Gesamt'
    AND typ = {charas}
                               ", .con = con)
    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::mutate(wert = as.numeric(wert))

    bula_de <- c("Gesamt",
                 "Bundesweit",
                 "Baden-Württemberg",
                 "Bayern",
                 "Berlin",
                 "Brandenburg",
                 "Bremen",
                 "Hamburg",
                 "Hessen",
                 "Mecklenburg-Vorpommern",
                 "Niedersachsen",
                 "Nordrhein-Westfalen",
                 "Rheinland-Pfalz",
                 "Saarland",
                 "Sachsen",
                 "Sachsen-Anhalt",
                 "Schleswig-Holstein",
                 "Thüringen")
    df_klein <- df %>%
      dplyr::filter(!(indikator %in% bula_de)) %>%
      dplyr::summarise(wert = sum(wert, na.rm = TRUE)) %>%
      dplyr::mutate(indikator = "lokal aktiv",
                    region = "Gesamt",
                    typ = "Region")


    if(bula_anzeigen == "Bundesländern zusammen anzeigen"){
      bula <- setdiff(bula_de, c("Bundesweit", "Gesamt"))

      df_bula <- df %>%
        dplyr::filter(indikator %in% bula) %>%
        dplyr::summarise(wert = sum(wert, na.rm = TRUE)) %>%
        dplyr::mutate(indikator = "auf Bundeslandebene aktiv",
                      region = "Gesamt",
                      typ = "Region")

      df <- df %>%
        dplyr::filter(indikator %in% c("Bundesweit", "Gesamt"))

      df <- rbind(df, df_bula, df_klein)
    }else{
      df <- df %>%
        dplyr::filter(indikator %in% bula_de)

      df <- rbind(df, df_klein)
    }

  }

  if(length(df$wert) == 0){

    titel <- paste0("Für die gewählten Eingaben hat keine Organisation eine Angabe gemacht.")

    tooltip <- "{point.indikator} <br> Anteil: {point.prop_disp} %"
    format <- "{value}%"
    color <- c("#b16fab", "#154194","#66cbaf","#112c5f", "#35bd97", "#5d335a",
               "#5f94f9", "#007655", "#d0a9cd")



    out <- linebuilder(df, titel, x = "indikator", y = "wert", group = "region", tooltip, format, color)


  }else if(abs_rel_select == "In Prozent"){

    ges <- df$wert[df$indikator == "Gesamt"]
    df_ges <- df %>% dplyr::filter(indikator == "Gesamt") %>%
      dplyr::rename(wert_ges = wert) %>%
      dplyr::select(-indikator)
    df <- df %>% dplyr::filter(indikator != "Gesamt")

    df <- df %>%
      dplyr::left_join(df_ges, by = c("region","typ")) %>%
      dplyr::mutate(prop = round(wert/wert_ges*100, 1))

    df <- df[with(df, order(prop, decreasing = TRUE)),]

    # Titel
    if(regio == "Gesamt" | charas == "Region"){
      regio_angabe <- ""
    }else if(charas == "Bundesweit"){
      regio_angabe <- paste0(" die ", regio, " tätig sind")
    }else{
      regio_angabe <- paste0(" die in ", regio, " tätig sind")
    }

    if(charas == "weitere Zielgruppe") charas <- "spezifischer Zielgruppe"
    if(charas == "weitere Disziplin") charas <- "weiterer Disziplin"

    titel <- paste0("Anteil der Projekte der Community-Plattform von MINTvernetzt nach ",
                    charas, regio_angabe)
    subtitel <- paste0("Angaben wurden von <b>", ges, "</b> Projekten gemacht. Mehrfachangabe möglich.")


    # Plot
    out <- highcharter::hchart(df, 'column', highcharter::hcaes(y = prop, x = indikator))%>%
      highcharter::hc_plotOptions(column = list( colorByPoint = TRUE,
                                                colors =  c("#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
                                                            "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a",
                                                            "#007655", "#dc6262", "#5d335a", "#112c7f", "#f59e0b", "#bbd1fc"))
      )%>%
      highcharter::hc_tooltip(pointFormat = "{point.y} %")%>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f} %"),
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular"), pointsWidth=100) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
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

    df <- df[with(df, order(wert, decreasing = TRUE)),]

    # Titel
    if(charas == "Gesamt"| charas == "Region"){
      regio_angabe <- ""
    }else if(charas == "Bundesweit"){
      regio_angabe <- paste0(" die ", regio, " tätig sind")
    }else{
      regio_angabe <- paste0(" die in ", regio, " tätig sind")
    }

    if(charas == "weitere Zielgruppe") charas <- "spezifischer Zielgruppe"
    if(charas == "weitere Disziplin") charas <- "weiterer Disziplin"

    titel <- paste0("Projekte der Community-Plattform von MINTvernetzt nach ",
                    charas, regio_angabe)
    subtitel <- paste0("Angaben wurden von <b>", ges, "</b> Projekten gemacht. Mehrfachangabe möglich.")

    # Plot
    out <- highcharter::hchart(df, 'column', highcharter::hcaes(y = wert, x = indikator))%>%
      highcharter::hc_plotOptions(column = list(colorByPoint = TRUE,
                                                colors =  c("#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
                                                            "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a",
                                                            "#007655", "#dc6262", "#5d335a", "#112c7f", "#f59e0b", "#bbd1fc"))
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

#' ausserschulisch cp profile Plot
#'
#' @noRd
plot_cp_profile <- function(r){

  charas <- r$chara_cp_prof
  abs_rel_select <- r$abs_rel_cp_prof

  if(charas != "Region"){
    charas <- c("Angebote", "Gesucht")
    regio <- r$regio_cp_prof
    anz <- r$anz_cp_prof

    if(anz == "Nur Gesuche anzeigen") charas <- "Gesucht"
    if(anz == "Nur Angebote anzeigen") charas <- "Angebote"

    # df <- dplyr::tbl(con, "ausserschulisch_cp_profile") %>%
    #   dplyr::filter(region == regio,
    #                 typ %in% charas) %>%
    #   dplyr::mutate(wert = as.numeric(wert)) %>%
    #   dplyr::collect()


    df_query <- glue::glue_sql("
    SELECT *
    FROM ausserschulisch_cp_profile
    WHERE region = {regio}
    AND typ IN ({charas*})
                               ", .con = con)
    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::mutate(wert = as.numeric(wert))


    df$typ[df$typ == "Gesucht"] <- "Gesuche"
  }else{
    bula_anzeigen<- r$bula_cp_prof
    regio <- ""
    df <- dplyr::tbl(con, "ausserschulisch_cp_projekte") %>%
      dplyr::filter(region == "Gesamt",
                    typ == charas) %>%
      dplyr::mutate(wert = as.numeric(wert)) %>%
      dplyr::collect()

    bula_de <- c("Gesamt",
                 "Bundesweit",
                 "Baden-Württemberg",
                 "Bayern",
                 "Berlin",
                 "Brandenburg",
                 "Bremen",
                 "Hamburg",
                 "Hessen",
                 "Mecklenburg-Vorpommern",
                 "Niedersachsen",
                 "Nordrhein-Westfalen",
                 "Rheinland-Pfalz",
                 "Saarland",
                 "Sachsen",
                 "Sachsen-Anhalt",
                 "Schleswig-Holstein",
                 "Thüringen")
    df_klein <- df %>%
      dplyr::filter(!(indikator %in% bula_de)) %>%
      dplyr::summarise(wert = sum(wert, na.rm = TRUE)) %>%
      dplyr::mutate(indikator = "lokal aktiv",
                    region = "Gesamt",
                    typ = "Region")


    if(bula_anzeigen == "Bundesländern zusammen anzeigen"){
      bula <- setdiff(bula_de, c("Bundesweit", "Gesamt"))

      df_bula <- df %>%
        dplyr::filter(indikator %in% bula) %>%
        dplyr::summarise(wert = sum(wert, na.rm = TRUE)) %>%
        dplyr::mutate(indikator = "auf Bundeslandebene aktiv",
                      region = "Gesamt",
                      typ = "Region")

      df <- df %>%
        dplyr::filter(indikator %in% c("Bundesweit", "Gesamt"))

      df <- rbind(df, df_bula, df_klein)
    }else{
      df <- df %>%
        dplyr::filter(indikator %in% bula_de)

      df <- rbind(df, df_klein)
    }

  }

  if(abs_rel_select == "In Prozent"){

    ges <- df$wert[df$indikator == "Gesamt"]
    df_ges <- df %>% dplyr::filter(indikator == "Gesamt") %>%
      dplyr::rename(wert_ges = wert) %>%
      dplyr::select(-indikator)
    df <- df %>% dplyr::filter(indikator != "Gesamt")

    df <- df %>%
      dplyr::left_join(df_ges, by = c("region","typ")) %>%
      dplyr::mutate(prop = round(wert/wert_ges*100, 1))

    df <- df[with(df, order(prop, decreasing = TRUE)),]

    if(length(unique(df$typ)) > 1){

      # Titel
      if(regio == "Gesamt"){
        regio_angabe <- ""
      }else if(regio == "Bundesweit"){
        regio_angabe <- paste0(" die bundesweit tätig sind")
      }else{
        regio_angabe <- paste0(" die in ", regio, " tätig sind")
      }
      titel <- paste0("Anteil der Profile der Community-Plattform von MINTvernetzt nach Angeboten und Gesuchen",
                      regio_angabe)
      subtitel <- paste0("Angaben zu ", charas[1], " wurden von <b>", ges[1], "</b> Personen gemacht.
                         Angaben zu ", charas[2], " von <b>", ges[2], "</b>. Mehrfachangaben möglich.")


      # Plot
      out <- highcharter::hchart(df, 'column', highcharter::hcaes(y = prop, x = indikator, group = typ))%>%
        highcharter::hc_colors(colors =  c( "#154194", "#00a87a")) %>%
        highcharter::hc_tooltip(pointFormat = "{point.y} %")%>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f} %"),
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular"), pointsWidth=100) %>%
        highcharter::hc_xAxis(title = list(text = "")) %>%
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

      # Titel
      if(regio == "Gesamt" | charas == "Region"){
        regio_angabe <- ""
      }else if(regio == "Bundesweit"){
        regio_angabe <- paste0(" die bundesweit tätig sind")
      }else{
        regio_angabe <- paste0(" die in ", regio, " tätig sind")
      }
      if(charas == "Gesucht") charas <- "Gesuchen"
      if(charas == "Angebote") charas <- "Angeboten"
      titel <- paste0("Anteil der Profile der Community-Plattform von MINTvernetzt nach ",
                      charas, regio_angabe)
      subtitel <- paste0("Angaben wurden von <b>", ges, "</b> Personen gemacht. Mehrfachangabe möglich.")


      # Plot
      out <- highcharter::hchart(df, 'column', highcharter::hcaes(y = prop, x = indikator))%>%
        highcharter::hc_plotOptions(column = list(#pointWidth = 50,
          colorByPoint = TRUE,
          colors =  c("#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
                      "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a",
                      "#007655", "#dc6262", "#5d335a", "#112c7f", "#f59e0b", "#bbd1fc"))
        )%>%
        highcharter::hc_tooltip(pointFormat = "{point.y} %")%>%
        highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f} %"),
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular"), pointsWidth=100) %>%
        highcharter::hc_xAxis(title = list(text = "")) %>%
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
    }


  }else{

    ges <- df$wert[df$indikator == "Gesamt"]
    df_ges <- df %>% dplyr::filter(indikator == "Gesamt") %>%
      dplyr::rename(wert_ges = wert) %>%
      dplyr::select(-indikator)
    df <- df %>% dplyr::filter(indikator != "Gesamt")

    df <- df[with(df, order(wert, decreasing = TRUE)),]

    if(length(unique(df$typ)) > 1){

      # Titel
      if(regio == "Gesamt"){
        regio_angabe <- ""
      }else if(regio == "Bundesweit"){
        regio_angabe <- paste0(" die bundesweit tätig sind")
      }else{
        regio_angabe <- paste0(" die in ", regio, " tätig sind")
      }
      titel <- paste0("Profile der Community-Plattform von MINTvernetzt nach Angeboten und Gesuchen",
                      regio_angabe)
      subtitel <- paste0("Angaben zu ", charas[1], " wurden von <b>", ges[1], "</b> Personen gemacht.
                         Angaben zu ", charas[2], " von <b>", ges[2], "</b>. Mehrfachangaben möglich.")


      # Plot
      out <- highcharter::hchart(df, 'column', highcharter::hcaes(y = wert, x = indikator, group = typ))%>%
        highcharter::hc_colors(colors = c( "#154194", "#00a87a")) %>%
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

    }else{

      # Titel
      if(regio == "Gesamt" | charas == "Region"){
        regio_angabe <- ""
      }else if(regio == "Bundesweit"){
        regio_angabe <- paste0(" die bundesweit tätig sind")
      }else{
        regio_angabe <- paste0(" die in ", regio, " tätig sind")
      }
      if(charas == "Gesucht") charas <- "Gesuchen"
      if(charas == "Angebote") charas <- "Angeboten"
      titel <- paste0("Profile der Community-Plattform von MINTvernetzt nach ",
                      charas, regio_angabe)
      subtitel <- paste0("Angaben wurden von <b>", ges, "</b> Personen gemacht. Mehrfachangabe möglich.")


      # Plot
      out <- highcharter::hchart(df, 'column', highcharter::hcaes(y = wert, x = indikator))%>%
        highcharter::hc_plotOptions(column = list(#pointWidth = 50,
          colorByPoint = TRUE,
          colors =  c("#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
                      "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a",
                      "#007655", "#dc6262", "#5d335a", "#112c7f", "#f59e0b", "#bbd1fc"))
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



  }

  return(out)

}

# Befragungen ----

#' ausserschulisch Akteursbefragung Plot
#'
#' @noRd

plot_mv_akteursb <- function(r){
  frage <- r$chara_mvb_akteur

  frage_typ <- ifelse(frage == "Arbeitsverhältnis", "arbeitsverhältnis",
                      ifelse(frage == "Kategorie", "kategorie", "sektoren"))
  frage_typ <- ifelse(frage == "Berufshintergrund", "berufshintergrund",
                      ifelse(frage == "Zielgruppen", "zielgruppen", frage_typ))

  # df <- dplyr::tbl(con, "ausserschulisch_akteursbefragung") %>%
  #   dplyr::filter(typ == frage_typ) %>%
  #   dplyr::collect()
  #

  df_query <- glue::glue_sql("
    SELECT *
    FROM ausserschulisch_akteursbefragung
    WHERE typ = {frage_typ}
                               ", .con = con)
  df <- DBI::dbGetQuery(con, df_query)


  df_ges <- df %>%
    dplyr::filter(indikator == "Gesamt") %>%
    dplyr::rename(wert_ges = wert) %>%
    dplyr::select(-indikator)
  df <- df %>% dplyr::filter(indikator != "Gesamt") %>%
    dplyr::left_join(df_ges, by = c("typ")) %>%
    dplyr::mutate(prop = round(wert/wert_ges*100, 1)) %>%
    dplyr::filter(prop > 1)

  df <- df[with(df, order(wert, decreasing = TRUE)),]
  titel <- paste0("Teilnehmende der Akteursbefragung 2024 nach ", frage)
  subtitel <- paste0("N = ", unique(df$wert_ges))


  if(frage %in% c("Arbeitsverhältnis", "Kategorie", "Sektor")){

    # plot <- df %>%
    color <-  c("#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
                "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a",
                "#007655", "#dc6262", "#5d335a", "#112c7f", "#f59e0b", "#bbd1fc")
    format <- '{point.prop} %'
    plot <- piebuilder(df, titel, x="indikator", y="wert", tooltip = paste('Anteil: {point.prop}%'), color, format)

  }else{

    abs_rel <- r$abs_rel_mvb_akteur

    if(abs_rel == "Anzahl"){
      df$prop <- df$wert
      tooltip <- "{point.y}"
      label <- "{value:, f}"
    }else{
      tooltip <- "{point.y} %"
      label <- "{value:, f} %"
    }

    subtitel <- paste0(subtitel, ", Mehrfachangabe möglich.")

    plot <- highcharter::hchart(df, 'column', highcharter::hcaes(y = prop, x = indikator))%>%
      highcharter::hc_plotOptions(column = list(#pointWidth = 50,
        colorByPoint = TRUE,
        colors = c("#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
                   "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a",
                   "#007655", "#dc6262", "#5d335a", "#112c7f", "#f59e0b", "#bbd1fc"))
      )%>%
      highcharter::hc_tooltip(pointFormat = tooltip)%>%
      highcharter::hc_yAxis(title = list(text = ""), labels = list(format = label),
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular"), pointsWidth=100) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
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

  }
  return(plot)
}

plot_mv_stimmung <- function(r){
  frage <- r$frage_mvb_stimmung
  gruppe <- r$gruppe_mvb_stimmung

  if(frage == "Nutzung des Ganztags"){
    frage_typ <-  c("Der Ganztag sollte eher für schulische Zwecke wie Hausaufgabenbetreuung genutzt werden.",
                    "Der Ganztag sollte eher für Freizeitangebote wie Sport, Kunst und Mustik genutzt werden.",
                    "Der Ganztag sollte als Bildungsort genutzt werden und dabei auch MINT-Bildungsangebote einbinden.")


    # df <- dplyr::tbl(con, "ausserschulisch_stimmungsbarometer") %>%
    #   dplyr::filter(typ %in% frage_typ,
    #                 indikator == gruppe) %>%
    #   dplyr::collect()


    df_query <- glue::glue_sql("
    SELECT *
    FROM ausserschulisch_stimmungsbarometer
    WHERE indikator = {gruppe}
                               ", .con = con)
    df <- DBI::dbGetQuery(con, df_query)

    df$antwort <- factor(df$antwort, levels = c("Kann ich nicht beurteilen",
                                                "Stimme nicht zu",
                                                "Stimme eher nicht zu",
                                                "Stimme eher zu",
                                                "Stimme volll zu"
    ))

    df <- df[with(df, order(typ, antwort)),]

    gruppe <- ifelse(gruppe == "Gesamt", "aller Befragten",
                     ifelse(gruppe == "Schule", "der schulischen Akteur:innen",
                            ifelse(gruppe == "außerschulische Akteur:innen", "der außerschulischen Akteur:innen", gruppe)))

    titel <- paste0("Antworten ", gruppe, " darauf, wie der Ganztag am besten genutzt werden sollte")
    subtitel <- "N = 453"
    subtitel <- ifelse(gruppe == "Schule", "N = 18", ifelse(gruppe == "außerschulische Akteur:innen",
                                                            "N = 24", subtitel))

    plot <- df %>%
      highcharter::hchart(
        "bar", highcharter::hcaes(group = antwort , y = wert, x = typ)
      )%>%
      highcharter::hc_plotOptions(bar = list(stacking = "percent")) %>%
      highcharter::hc_tooltip(
        pointFormat=paste('\"{point.antwort}\" <br> Anteil: {point.wert}%')) %>%
      highcharter::hc_colors( c("#efe8e6",
                                "#ee7775", "#fca5a5",
                                "#66cbaf", "#35bd97" )) %>%
      highcharter::hc_title(text = titel,
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_subtitle(text = subtitel,
                               align = "center",
                               style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "18px")) %>%
      highcharter::hc_yAxis(title = list(text = "")) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "SourceSans3-Regular", fontSize = "18px")) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = T)

    }else{

      frage_typ <- "Lernrückstände"
      df <- dplyr::tbl(con, "ausserschulisch_stimmungsbarometer") %>%
        dplyr::filter(typ %in% frage_typ,
                      indikator == gruppe) %>%
        dplyr::collect()

      gruppe <- ifelse(gruppe == "Gesamt", "aller Befragten",
                       ifelse(gruppe == "Schule", "der schulischen Akteur:innen",
                              ifelse(gruppe == "außerschulische Akteur:innen", "der außerschulischen Akteur:innen", gruppe)))

      titel <- paste0("Antworten ", gruppe, " darauf, ob der Ganztag zum Abbau von Leistungslücken in MINT genutzt werden soll")
      subtitel <- paste0("N = 464")
      subtitel <- ifelse(gruppe == "Schule", "N = 18", ifelse(gruppe == "außerschulische Akteur:innen",
                                                              "N = 24", subtitel))

      plot <- df %>%
        highcharter::hchart(
          "pie", highcharter::hcaes(x = antwort, y = wert)
        )%>%
        highcharter::hc_tooltip(
          pointFormat=paste('Anteil: {point.wert}%')) %>%
        highcharter::hc_colors( c("#b16fab", "#154194", "#66cbaf","#fbbf24")) %>%
        highcharter::hc_title(text = titel,
                              margin = 45,
                              align = "center",
                              style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
        highcharter::hc_subtitle(text = subtitel,
                                 align = "center",
                                 style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "16px")) %>%

        highcharter::hc_labels(style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
        highcharter::hc_plotOptions(
          pie = list(
            dataLabels = list(
              enabled = TRUE,
              distance = 50,
              style = list(
                fontSize = "13px",
                fontFamily = "SourceSans3-Regular"
              )
            )
          )
        ) %>%
        highcharter::hc_chart(
          marginTop = 80,
          marginBottom = 80,
          marginLeft = 100,
          marginRight = 100
        )
  }



    return(plot)
}

plot_mv_genderb <- function(){


    # df <- dplyr::tbl(con, "ausserschulisch_genderbefragung") %>%
    #   dplyr::filter(thema == "Vernetzungswunsch") %>%
    #   dplyr::collect()
    #

    df_query <- glue::glue_sql("
    SELECT *
    FROM ausserschulisch_genderbefragung
    WHERE thema = 'Vernetzungswunsch'
                               ", .con = con)
    df <- DBI::dbGetQuery(con, df_query)

    # Title und Texte vorbereiten
    titel <- "Aktivität und Vernetzung in MINT-Bildungsnetzwerken zum Thema MINT-Förderung
    für Mädchen und Frauen"
    subtitel <- "Angaben von 456 MINT-Bildungsanbieter:innen"

    df <- df %>%
      dplyr::mutate(
        gruppe = dplyr::case_when(
          gruppe == "Die moderat Aktiven" ~ "Die moderat Aktiven",
          gruppe == "Die hoch Aktiven" ~ "Die hoch Aktiven",
          gruppe == "Die moderat Passiven" ~ "Die moderat Passiven"
        )
      )

    plot <- df %>%
      highcharter::hchart(
        "pie", highcharter::hcaes(x = gruppe, y = wert)
      )%>%
      highcharter::hc_tooltip(
        pointFormat=paste('Anteil: {point.wert} %')) %>%
      highcharter::hc_colors( c("#b16fab", "#154194", "#66cbaf")) %>%
      highcharter::hc_title(text = titel,
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "20px")) %>%
      highcharter::hc_subtitle(text = subtitel,
                               align = "center",
                               style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular", fontSize = "16px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "SourceSans3-Regular", fontSize = "14px")) %>%
      highcharter::hc_caption(text =  "Die Gruppe der bislang moderat aktiven MINT-Bildungsanbieter:innen zeichnet
                              sich durch einen großen Vernetzungswunsch und eine hohe Motivation aus,
                              sich aktiv in Netzwerke zum Thema MINT-Förderung für Mädchen einzubringen,
                              was auf ein großes Aktivierungspotenzial hinweist.
                              Die Gruppe der hoch Aktiven ist bereits sehr motiviert und engagiert in ihrem Netzwerk. Die kleinste Gruppe der Befragten nimmt lieber passiv an Netzwerkaktivitäten teil.") %>%
      highcharter::hc_plotOptions(
        pie = list(
          dataLabels = list(
            style = list(
              fontSize = "14px",  # Schriftgröße für die Labels anpassen
              fontFamily = "SourceSans3-Regular"
            )
          )
        )
      )

  return(plot)
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
    # df <- dplyr::tbl(con, from = "ausserschulisch_skf") %>%
    #   dplyr::filter(indikator %in% c("Einrichtungen mit SKf-Fortbildung",
    #                                  "zertifizierte Einrichtungen"),
    #                 jahr %in% t) %>%
    #   dplyr::select(-bereich) %>%
    #   dplyr::group_by(indikator, jahr) %>%
    #   dplyr::summarise(wert = sum(wert)) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::collect()


    df_query <- glue::glue_sql("
    SELECT *
    FROM ausserschulisch_skf
    WHERE indikator IN ('Einrichtungen mit SKf-Fortbildung', 'zertifizierte Einrichtungen')
    AND jahr IN ({t*})
                               ", .con = con)
    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::select(-bereich) %>%
      dplyr::group_by(indikator, jahr) %>%
      dplyr::summarise(wert = sum(wert)) %>%
      dplyr::ungroup()

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
    # df <- dplyr::tbl(con, from = "ausserschulisch_skf") %>%
    #   dplyr::filter(indikator %in% c("insgesamt fortgebildete Fach- / Lehrkräfte",
    #                                  "neu fortgebildete Fach- / Lehrkräfte"),
    #                 jahr %in% t) %>%
    #   dplyr::select(-bereich) %>%
    #   dplyr::group_by(indikator, jahr) %>%
    #   dplyr::summarise(wert = sum(wert)) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::collect()


    df_query <- glue::glue_sql("
    SELECT *
    FROM ausserschulisch_skf
    WHERE indikator IN ('insgesamt fortgebildete Fach- / Lehrkräfte', 'neu fortgebildete Fach- / Lehrkräfte')
    AND jahr In ({t*})
                               ", .con = con)
    df <- DBI::dbGetQuery(con, df_query)

    df <- df %>%
      dplyr::select(-bereich) %>%
      dplyr::group_by(indikator, jahr) %>%
      dplyr::summarise(wert = sum(wert)) %>%
      dplyr::ungroup() %>%

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
  df$indikator[df$indikator == "neu fortgebildete Fach- / Lehrkräfte"] <- "In diesem Jahr fortgebildet"
  df$indikator[df$indikator == "insgesamt fortgebildete Fach- / Lehrkräfte"] <- "Bis jetzt insgesamt fortgebildet"

  # Plot
  out <- highcharter::hchart(df, 'column', highcharter::hcaes(y = wert, x = jahr, group=indikator))%>%
    highcharter::hc_plotOptions(column = list(pointWidth = 50))%>%
    highcharter::hc_tooltip(pointFormat = "{point.indikator}: {point.y}")%>%
    highcharter::hc_yAxis(title = list(text = ""), labels = list(format = "{value:, f}"),
                          style = list(color = "black", useHTML = TRUE, fontFamily = "SourceSans3-Regular"), pointsWidth=100) %>%
    highcharter::hc_xAxis(title = list(text = "")) %>%
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
