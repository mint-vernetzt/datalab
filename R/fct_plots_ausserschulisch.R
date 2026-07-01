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

    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", indikator, "</b><br>",
          "Jahr: ", jahr, "<br>",
          "Anteil: ", prop_disp, " %"
        )
      )

    color <- c("#b16fab", "#154194","#66cbaf","#112c5f", "#35bd97", "#5d335a",
               "#5f94f9", "#007655", "#d0a9cd")


    quelke <- "Quelle der Daten: MINTvernetzt Community Plattform, Stand 14. April 2025."
    out <- linebuilder(df, titel, x = "indikator", y = "wert", group = "region",
                       color = color, quelle = quelke)

    #keine quelle weil net relevant

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

    df <- df %>%
      dplyr::mutate(
        indikator_kurz = stringr::str_trunc(indikator, width = 30)
      )


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


    quelle <- "Quelle der Daten: MINTvernetzt Community Plattform, Stand 30. Juli 2025."



    order <- unique(df$indikator)

    df <- df %>%
      dplyr::mutate(
        .tooltip = paste0(
          "<b><span style='font-size:15px;'>", indikator, "</span></b><br>",
          "Anteil: ", prop, " %"
        ))


    x <- "indikator"
    y <- "prop"
    color <- c("#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
      "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a",
      "#007655", "#dc6262", "#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
      "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a",
      "#007655", "#dc6262")

    out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "v",percent=TRUE,
                                group=NULL, color=color, subtitel=subtitel,
                                tickvals = df$indikator, ticktext = df$indikator_kurz,
                                order=order, stacking = FALSE, quelle=quelle)




  }else{

    ges <- df$wert[df$indikator == "Gesamt"]
    df_ges <- df %>% dplyr::filter(indikator == "Gesamt") %>%
      dplyr::rename(wert_ges = wert) %>%
      dplyr::select(-indikator)
    df <- df %>% dplyr::filter(indikator != "Gesamt")

    df <- df[with(df, order(wert, decreasing = TRUE)),]

    df <- df %>%
      dplyr::mutate(
        indikator_kurz = stringr::str_trunc(indikator, width = 30)
      )

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


    quelle <- "Quelle der Daten: MINTvernetzt Community Plattform, Stand 30. Juli 2025."



    order <- unique(df$indikator)

    df <- df %>%
      dplyr::mutate(
        .tooltip = paste0(
          "<b><span style='font-size:15px;'>", indikator, "</span></b><br>",
          "Anzahl: ", wert
        ))


    x <- "indikator"
    y <- "wert"
    color <- c("#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
               "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a",
               "#007655", "#dc6262", "#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
               "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a",
               "#007655", "#dc6262")

    out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "v",percent=FALSE,
                                group=NULL, color=color, subtitel=subtitel,
                                tickvals = df$indikator, ticktext = df$indikator_kurz,
                                order=order, stacking = FALSE, quelle=quelle)





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


    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", indikator, "</b><br>",
          "Jahr: ", jahr, "<br>",
          "Anteil: ", prop_disp, " %"
        )
      )

    color <- c("#b16fab", "#154194","#66cbaf","#112c5f", "#35bd97", "#5d335a",
               "#5f94f9", "#007655", "#d0a9cd")

    out <- linebuilder(df, titel, x = "indikator", y = "wert", group = "region", color = color)


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

    df <- df %>%
      dplyr::mutate(
        indikator_kurz = stringr::str_trunc(indikator, width = 30)
      )

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





    quelle <- "Quelle der Daten: MINTvernetzt Community Plattform, Stand 30. Juli 2025."
    quelle_y <- -0.20


    order <- unique(df$indikator)

    df <- df %>%
      dplyr::mutate(
        .tooltip = paste0(
          "<b><span style='font-size:15px;'>", indikator, "</span></b><br>",
          "Anzahl: ", wert
        ))


    x <- "indikator"
    y <- "prop"
    color <- c("#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
               "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a",
               "#007655", "#dc6262", "#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
               "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a",
               "#007655", "#dc6262")

    out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "v",percent=FALSE,
                                group=NULL, color=color, subtitel=subtitel,quelle_y=quelle_y,
                                tickvals = df$indikator, ticktext = df$indikator_kurz,
                                order=order, stacking = FALSE, quelle=quelle)





  }else{

    ges <- df$wert[df$indikator == "Gesamt"]
    df_ges <- df %>% dplyr::filter(indikator == "Gesamt") %>%
      dplyr::rename(wert_ges = wert) %>%
      dplyr::select(-indikator)
    df <- df %>% dplyr::filter(indikator != "Gesamt")

    df <- df[with(df, order(wert, decreasing = TRUE)),]

    df <- df %>%
      dplyr::mutate(
        indikator_kurz = stringr::str_trunc(indikator, width = 30)
      )

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



    quelle <- "Quelle der Daten: MINTvernetzt Community Plattform, Stand 30. Juli 2025."
    quelle_y <- -0.20


    order <- unique(df$indikator)

    df <- df %>%
      dplyr::mutate(
        .tooltip = paste0(
          "<b><span style='font-size:15px;'>", indikator, "</span></b><br>",
          "Anzahl: ", wert
        ))


    x <- "indikator"
    y <- "wert"
    color <- c("#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
               "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a",
               "#007655", "#dc6262", "#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
               "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a",
               "#007655", "#dc6262")

    out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "v",percent=FALSE,
                                group=NULL, color=color, subtitel=subtitel,quelle_y=quelle_y,
                                tickvals = df$indikator, ticktext = df$indikator_kurz,
                                order=order, stacking = FALSE, quelle=quelle)





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

    df <- df %>%
      dplyr::mutate(
        indikator_kurz = stringr::str_trunc(indikator, width = 30)
      )

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




      quelle <- "Quelle der Daten: MINTvernetzt Community Plattform, Stand 30. Juli 2025."
      quelle_y <- -0.35


      order <- unique(df$indikator)

      df <- df %>%
        dplyr::mutate(
          .tooltip = paste0(
            "<b><span style='font-size:15px;'>", indikator, "</span></b><br>",
            "Anzahl: ", prop, " %"
          ))


      x <- "indikator"
      y <- "prop"
      group <- "typ"
      color <- c( "Angebote" = "#154194","Gesuche" = "#00a87a")


      out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "v",percent=TRUE,
                                  group=group, color=color, subtitel=subtitel,quelle_y=quelle_y,
                                  tickvals = df$indikator, ticktext = df$indikator_kurz,
                                  order=order, stacking = FALSE, quelle=quelle)






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





      quelle <- "Quelle der Daten: MINTvernetzt Community Plattform, Stand 30. Juli 2025."
      quelle_y <- -0.35


      order <- unique(df$indikator)

      df <- df %>%
        dplyr::mutate(
          .tooltip = paste0(
            "<b><span style='font-size:15px;'>", indikator, "</span></b><br>",
            "Anzahl: ", prop, " %"
          ))


      x <- "indikator"
      y <- "prop"

      color <- c("#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
                 "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a",
                 "#007655", "#dc6262", "#5d335a", "#112c7f", "#f59e0b", "#bbd1fc", "#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
                 "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a",
                 "#007655", "#dc6262", "#5d335a", "#112c7f", "#f59e0b", "#bbd1fc")


      out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "v",percent=TRUE,
                                  group=NULL, color=color, subtitel=subtitel,quelle_y=quelle_y,
                                  tickvals = df$indikator, ticktext = df$indikator_kurz,
                                  order=order, stacking = FALSE, quelle=quelle)


    }


  }else{

    ges <- df$wert[df$indikator == "Gesamt"]
    df_ges <- df %>% dplyr::filter(indikator == "Gesamt") %>%
      dplyr::rename(wert_ges = wert) %>%
      dplyr::select(-indikator)
    df <- df %>% dplyr::filter(indikator != "Gesamt")

    df <- df[with(df, order(wert, decreasing = TRUE)),]

    df <- df %>%
      dplyr::mutate(
        indikator_kurz = stringr::str_trunc(indikator, width = 30)
      )

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








      quelle <- "Quelle der Daten: MINTvernetzt Community Plattform, Stand 30. Juli 2025."
      quelle_y <- -0.35


      order <- unique(df$indikator)

      df <- df %>%
        dplyr::mutate(
          .tooltip = paste0(
            "<b><span style='font-size:15px;'>", indikator, "</span></b><br>",
            "Anzahl: ", wert
          ))


      x <- "indikator"
      y <- "wert"
      group <- "typ"
      color <- c( "Angebote" = "#154194","Gesuche" = "#00a87a")


      out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "v",percent=FALSE,
                                  group=group, color=color, subtitel=subtitel,quelle_y=quelle_y,
                                  tickvals = df$indikator, ticktext = df$indikator_kurz,
                                  order=order, stacking = FALSE, quelle=quelle)





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



      df <- df %>%
        dplyr::mutate(
          indikator_kurz = stringr::str_trunc(indikator, width = 30)
        )




      quelle <- "Quelle der Daten: MINTvernetzt Community Plattform, Stand 30. Juli 2025."
      quelle_y <- -0.35


      order <- unique(df$indikator)

      df <- df %>%
        dplyr::mutate(
          .tooltip = paste0(
            "<b><span style='font-size:15px;'>", indikator, "</span></b><br>",
            "Anzahl: ", wert
          ))


      x <- "indikator"
      y <- "wert"

      color <- c("#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
      "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a",
      "#007655", "#dc6262", "#5d335a", "#112c7f", "#f59e0b", "#bbd1fc", "#b16fab", "#154194", "#66cbaf","#fbbf24", "#ee7775", "#35bd97",
      "#d0a9cd", "#5f94f0", "#fca5a5", "#fde68a",
      "#007655", "#dc6262", "#5d335a", "#112c7f", "#f59e0b", "#bbd1fc")


      out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "v",percent=FALSE,
                                  group=NULL, color=color, subtitel=subtitel,quelle_y=quelle_y,
                                  tickvals = df$indikator, ticktext = df$indikator_kurz,
                                  order=order, stacking = FALSE, quelle=quelle)



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

    quelle <- "Quelle der Daten: MINTvernetzt 2024."

    plot <- piebuilder(df, titel, x="indikator", y="wert", tooltip = paste('Anteil: {point.prop}%'), color, format, quelle = quelle)


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
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular"), pointsWidth=100) %>%
      highcharter::hc_xAxis(title = list(text = "")) %>%
      highcharter::hc_title(text = titel,
                            margin = 45,
                            align = "center",
                            style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "20px")) %>%
      highcharter::hc_subtitle(text = subtitel,
                               align = "center",
                               style = list(color = "black", useHTML = TRUE, fontFamily = "Calibri Regular", fontSize = "16px")) %>%
      highcharter::hc_chart(
        style = list(fontFamily = "Calibri Regular", fontSize = "14px")
      ) %>%
      highcharter::hc_legend(enabled = TRUE, reversed = F) %>%
      highcharter::hc_caption(text = "Quelle der Daten: MINTvernetzt 2024.",
                              style = list(fontSize = "11px", color = "gray")) %>%
      highcharter::hc_exporting(enabled = TRUE,
                                buttons = list(
                                  contextButton = list(
                                    menuItems = list("downloadPNG", "downloadCSV",
                                                     list(
                                                       text = "Daten für GPT",
                                                       onclick = htmlwidgets::JS(sprintf(
                                                         "function () {
     var date = new Date().toISOString().slice(0,10);
     var chartTitle = '%s'.replace(/\\s+/g, '_');
     var filename = chartTitle + '_' + date + '.txt';

     var data = 'Titel: %s\\n' + this.getCSV();
     data += '\\n\\nQuelle der Daten: MINTvernetzt 2024';

     var blob = new Blob([data], { type: 'text/plain;charset=utf-8;' });
     if (window.navigator.msSaveBlob) {
       window.navigator.msSaveBlob(blob, filename);
     } else {
       var link = document.createElement('a');
       link.href = URL.createObjectURL(blob);
       link.download = filename;
       link.click();
     }
   }", gsub("'", "\\\\'", titel), gsub("'", "\\\\'", titel) )  #
                                                       )))
                                  ))
      )

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



    df_query <- glue::glue_sql("
    SELECT *
    FROM ausserschulisch_stimmungsbarometer
    WHERE indikator = {gruppe}
    AND typ In ({frage_typ*})
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



    df <- df %>%
      dplyr::mutate(
        typ_kurz = stringr::str_trunc(typ, width = 50)
      )



    quelle <- "Quelle der Daten: MINTvernetzt, 2024"
    quelle_y <- -0.20


    order <- unique(df$typ)

    df <- df %>%
      dplyr::mutate(
        .tooltip = paste0(
          "<b><span style='font-size:12px;'>", typ, "</span></b><br>",
          "<b><span style='font-size:15px;'>", antwort, "</span></b><br>",
          "Anzahl: ", wert, " %"
        ))



    x <- "typ"
    y <- "wert"
    group <- "antwort"
    color <- c("#efe8e6",
               "#ee7775", "#fca5a5",
               "#66cbaf", "#35bd97" )


    out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "h",percent=TRUE,
                                group=group, color=color, subtitel=subtitel,quelle_y=quelle_y,
                                tickvals = df$typ, ticktext = df$typ_kurz, legend_size=10,
                                order=order, stacking = TRUE, quelle=quelle)










    }else{

      frage_typ <- "Lernrückstände"
     df_query <- glue::glue_sql("
      SELECT *
      FROM ausserschulisch_stimmungsbarometer
      WHERE typ IN ({frage_typ*})
      AND indikator = {gruppe}
                               ", .con = con)
      df <- DBI::dbGetQuery(con, df_query)


      gruppe <- ifelse(gruppe == "Gesamt", "aller Befragten",
                       ifelse(gruppe == "Schule", "der schulischen Akteur:innen",
                              ifelse(gruppe == "außerschulische Akteur:innen", "der außerschulischen Akteur:innen", gruppe)))

      titel <- paste0("Antworten ", gruppe, " darauf, ob der Ganztag zum Abbau von Leistungslücken in MINT genutzt werden soll")
      subtitel <- paste0("N = 464")
      subtitel <- ifelse(gruppe == "Schule", "N = 18", ifelse(gruppe == "außerschulische Akteur:innen",
                                                              "N = 24", subtitel))



      df <- df %>%
        dplyr::mutate(
          tooltip = paste0(
            "<b><span style='font-size:12px;'>", antwort, "</span></b><br>",
            "Anteil: ", wert, " %"
          )
        )

      color <- c("#b16fab", "#154194", "#66cbaf","#fbbf24")

      quelle <- "Quelle der Daten: MINTvernetzt 2024."



      out <- piebuilder_plotly(df, titel, x="antwort", y = "wert", titel_y= 0.96,
                               legend_y = -0.01, quelle_y = -0.34, height = 450,
                               color=color, quelle=quelle) %>%
        plotly::layout(margin= list( t=90))




  }



    return(out)
}

plot_mv_genderb <- function(){

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
        ),
        wert = as.numeric(wert)
      )
    df <- df %>%
      dplyr::mutate(
        tooltip = paste0(
          "<b>", gruppe, "</b><br>",
          "Anteil: ", wert, " %<br>"
        )
      )

    plot <- piebuilder_plotly(df, x = "gruppe", y = "wert",
                              titel = titel,
                              subtitel = subtitel,
                              color = c("#b16fab", "#154194", "#66cbaf"),
                              quelle = "Quelle: MINTvernetzt") |>
      plotly::layout(
        margin = list(t = 130, b = 120, r = 50, l = 50)
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


    df_query <- glue::glue_sql("
    SELECT *
    FROM ausserschulisch_skf
    WHERE indikator IN ('Einrichtungen mit SKf-Fortbildung', 'zertifizierte Einrichtungen')
    AND jahr IN ({t*})
    AND einrichtung = {ort_select}
                               ", .con = con)
    df <- DBI::dbGetQuery(con, df_query)


    df <- df %>%
      dplyr::select(-bereich)


  }

  # Gesamtanzahl für Hover-Box ergänzen
  df <- df %>%
    dplyr::group_by(einrichtung, jahr) %>%
    dplyr::mutate(gesamt = sum(wert)) %>%
    dplyr::ungroup()

  #Trennpunkte für lange Zahlen ergänzen
  #df$gesamt <- prettyNum(df$gesamt, big.mark = ".", decimal.mark = ",")
 # df$wert <- prettyNum(df$wert, big.mark = ".", decimal.mark = ",")


  # Hilfe für Überschrift
  helper <- ort_select
  helper <- ifelse(helper == "Alle Einrichtungen", "Kitas, Horte und Grundschulen", helper)
  helper <- ifelse(helper == "Grundschule", "Grundschulen", helper)
  helper <- ifelse(helper == "Kita", "Kitas", helper)
  helper <- ifelse(helper == "Hort", "Horte", helper)

  titel <- paste0(helper, ", die bei Stiftung Kinder forschen aktiv sind")







  quelle <- "Quelle der Daten: Stiftung Kinder forschen, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."


  order <- unique(df$jahr)

  df <- df %>%
    dplyr::mutate(
      indikator = factor(
      indikator,
      levels = c(
        "zertifizierte Einrichtungen",
        "Einrichtungen mit SKf-Fortbildung")
    ),
      .tooltip = paste0(
        "<b><span style='font-size:15px;'>", jahr, "</span></b><br>",
        "<b><span style='font-size:15px;'>", indikator, "</span></b><br>",
        "Anzahl: ", (formatC(as.numeric(wert), format = "f", digits = 0, big.mark = ".")), "<br>",
        "aktive Einrichtungen gesamt: ", (formatC(as.numeric(gesamt), format = "f", digits = 0, big.mark = "."))
      ))



  x <- "jahr"
  y <- "wert"
  group <- "indikator"
  color <- c("Einrichtungen mit SKf-Fortbildung" = "#efe8e6", "zertifizierte Einrichtungen" = "#66cbaf")


  out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "v",percent=FALSE,
                              group=group, color=color,
                              order=order, stacking = TRUE, quelle=quelle)


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


    df_query <- glue:::glue_sql("
                                SELECT *
                                FROM ausserschulisch_skf
                                WHERE
                                indikator IN ('insgesamt fortgebildete Fach- / Lehrkräfte', 'neu fortgebildete Fach- / Lehrkräfte')
                                AND jahr In ({t*})
                                ANd einrichtung = {ort_select}
                                ", .con = con)
    df <- DBI::dbGetQuery(con, df_query)
    df <- df %>%
      dplyr::select(-bereich)
  }


  # Labels anpassen
  df$indikator[df$indikator == "neu fortgebildete Fach- / Lehrkräfte"] <- "In diesem Jahr fortgebildet"
  df$indikator[df$indikator == "insgesamt fortgebildete Fach- / Lehrkräfte"] <- "Bis jetzt insgesamt fortgebildet"

  titel <- paste0("Geschätzte Anzahl an Fach- und Lehrkräften, die an einer SKf-Fortbildung teilgenommen haben")




  quelle <- "Quelle der Daten: Stiftung Kinder forschen, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."


  order <- unique(df$jahr)

  df <- df %>%
    dplyr::mutate(
      .tooltip = paste0(
        "<b><span style='font-size:15px;'>", jahr, "</span></b><br>",
        "<b><span style='font-size:15px;'>", indikator, "</span></b><br>",
        "Anzahl: ", (formatC(as.numeric(wert), format = "f", digits = 0, big.mark = "."))
      ))



  x <- "jahr"
  y <- "wert"
  group <- "indikator"
  color <- c("In diesem Jahr fortgebildet" = "#8893a7", "Bis jetzt insgesamt fortgebildet" = "#66cbaf")


  out <- balkenbuilder_plotly(df=df, x=x, y=y, titel=titel, orientation = "v",percent=FALSE,
                              group=group, color=color,
                              order=order, stacking = FALSE, quelle=quelle)





  return(out)
}

