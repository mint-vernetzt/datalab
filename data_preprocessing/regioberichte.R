
# Regioberichte gebündelte Downloads


pkgload::load_all(".")
library(plotly)

con <- DBI::dbConnect(duckdb::duckdb(), "data/mint_db.duckdb", read_only = TRUE)

pfad1 <- "C:/Users/mis/OneDrive - Stifterverband/Dateiablage - MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/08 GPT_KI Analysehilfe/08_Regionalberichte/Abbildungen_2025"

# -------------------------------------------------------------------------------
# TODO:
# pfad anders definieren für die einmaligen grafiken
# code mit funktionen optimieren und durch die loopen
# -------------------------------------------------------------------------------


# Plotly-Grafik als PNG speichern (ohne Python, braucht Chrome oder Edge)

Sys.setenv(CHROMOTE_CHROME = "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")

speichere_png <- function(p, datei, breite, hoehe) {
  html <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(p, html, selfcontained = TRUE)
  webshot2::webshot(html, file = datei, vwidth = breite, vheight = hoehe, zoom = 2)
}

speichere_txt <- function(p, datei, kz, datenspeziell = NULL) {
  p_built <- plotly::plotly_build(p)

  datatxt <- if (is.null(datenspeziell)) as.data.frame(plotly::plotly_data(p)) else datenspeziell
  datatxt <- datatxt[, !grepl("^\\.?(tooltip|label|wert_disp|wert_besr|prop_disp|color|display_diff|label_wert)$", names(datatxt)), drop = FALSE]

  titel <- p_built$x$layout$title
  if (is.list(titel)) titel <- titel$text
  if (is.null(titel) || length(titel) == 0) titel <- ""

  titel <- gsub("<br\\s*/?>", " ", titel)
  titel <- trimws(titel)

  ann <- p_built$x$layout$annotations
  quelletxt <- vapply(ann, function(a) {
    if (is.numeric(a$y) && a$y < 0 && !is.null(a$text)) as.character(a$text) else ""
  }, character(1))
  quelle <- paste(quelletxt[nzchar(quelletxt)], collapse = " | ")

  txtcon <- file(datei, open = "wt", encoding = "UTF-8")
  on.exit(close(txtcon))

  writeLines(c(
    paste0("Titel: ", titel),
    paste0("Bundesland: ", kz),
    paste0("Quelle: ", quelle),
    "",
    "Daten:"
  ), txtcon)

  utils::write.table(datatxt, txtcon, sep = "\t", row.names = FALSE, quote = TRUE, na = "")
}

exportiere <- function(p, png_datei, pfad, kz, breite, hoehe, datenspeziell = NULL, txt_p = p) {
  speichere_png(p, file.path(pfad, png_datei), breite, hoehe)
  speichere_txt(txt_p, file.path(pfad, sub("\\.png$", ".txt", png_datei)),
                kz = kz, datenspeziell = datenspeziell)
}

# Enpgass Label: Horizontale Mitte jedes Anteils
# anteile: Anzahl Cubes pro Farbe
label_engpass <- function(anteile) {
  ende   <- cumsum(anteile)
  start  <- ende - anteile

  vapply(seq_along(anteile), function(i) {
    cubes <- start[i]:(ende[i] - 1)
    spalte <- cubes %/% 10
    mean((spalte + 0.5) / 10)
  }, numeric(1))
}

# Einheitliche Schriftgrößen für Linien- und Balkendiagramme
stil <- function(p) {
  plotly::layout(p,
                 font   = list(size = 16),
                 xaxis  = list(tickfont = list(size = 15)),
                 yaxis  = list(tickfont = list(size = 15)),
                 legend = list(font = list(size = 14))
  )
}


laender_alle <- c(
  BW = "Baden-Württemberg", BY = "Bayern", BE = "Berlin", BB = "Brandenburg",
  HB = "Bremen", HH = "Hamburg", HE = "Hessen", MV = "Mecklenburg-Vorpommern",
  NI = "Niedersachsen", NW = "Nordrhein-Westfalen", RP = "Rheinland-Pfalz",
  SL = "Saarland", SN = "Sachsen", ST = "Sachsen-Anhalt",
  SH = "Schleswig-Holstein", TH = "Thüringen"
)


# Auswahl für den Lauf: Kürzel hier nach Belieben ändern, für alle Länder: laender <- laender_alle
#laender <- laender_alle[c("BW", "BE")]
laender <- laender_alle

# Grafikfunktion Loop ----

for (kz in names(laender)) {

  bl <- laender[[kz]]

  pfad <- file.path(pfad1, bl)
  dir.create(pfad, recursive = TRUE, showWarnings = FALSE)

  regio_epa <- bl
  if (kz %in% c("BB", "BE")) regio_epa <- "Brandenburg / Berlin"
  if (kz %in% c("NI", "HB")) regio_epa <- "Niedersachsen / Bremen"
  if (kz %in% c("RP", "SL")) regio_epa <- "Rheinland-Pfalz / Saarland"
  if (kz %in% c("SH", "HH")) regio_epa <- "Schleswig-Holstein / Hamburg"

  f1 <- paste0(kz, "_1_beschaeftigte_verlauf.png")
  f2 <- paste0(kz, "_2_karte_landkreise.png")
  f3a <- paste0(kz, "_3a_berufswahl_frauen.png")
  f3b <- paste0(kz, "_3b_berufswahl_maenner.png")
  f4 <- paste0(kz, "_4_nachwuchs_verlauf.png")
  f5 <- paste0(kz, "_5_nachwuchs_verlauf_MINT.png")
  f6a <- paste0(kz, "_6a_IQB4.png")
  f6b <- paste0(kz, "_6b_IQB9.png")
  f7 <- paste0(kz, "_7_Kursbelegungen.png")
  f8 <- paste0(kz, "_8_Lehramtsstudierende.png")
  f9a <- paste0(kz, "_9a_Enpass_MINT.png")
  f9b <- paste0(kz, "_9b_Enpass_nichtMINT.png")
  f10 <- paste0(kz, "_10_demografie_beschaeftigte.png")

  r <- list(
    # plot 1, 4, 5, 10
    region_argumentationshilfe                      = bl,

    # plot 2
    states_beruf_arbeitsmarkt_landkreis_karte       = bl,
    date_beruf_arbeitsmarkt_landkreis_karte         = 2025,
    kategorie_beruf_arbeitsmarkt_landkreis_karte1   = "Beschäftigte",
    fachbereich_beruf_arbeitsmarkt_landkreis_karte1 = "MINT",
    indikator1_beruf_arbeitsmarkt_landkreis_karte1  = "Auszubildende",
    indikator2_beruf_arbeitsmarkt_landkreis_karte1  = "Beschäftigte",

    # plot 3
    ansicht_arbeitsmarkt_wahl_gender     = "Einzelansicht - Kuchendiagramm",
    region_arbeitsmarkt_wahl_gender_pie  = bl,
    date_arbeitsmarkt_wahl_gender_pie    = 2025,
    level_arbeitsmarkt_wahl_gender_pie   = "Beschäftigte",

    # plot 7
    date_kurse_einstieg_verlauf      = c(2017, 2024),
    region_kurse_einstieg_verlauf    = bl,
    abs_zahlen_kurse_einstieg_verlauf = "In Prozent",

    # plot 8
    date_studienzahl_einstieg_verlauf       = c(2018, 2025),
    region_studienzahl_einstieg_verlauf     = bl,
    studienzahl_einstieg_indi_verlauf       = c("Studierende (Lehramt)", "Absolvent:innen (Lehramt)"),
    abs_zahlen_einstieg_verlauf_indi        = "Anzahl",

    # plot 9
      y_fachkraft_epa_bulas              = 2025,
      regio_fachkraft_epa_bulas23_24     = regio_epa,
      f_fachkraft_epa_bulas              = c("MINT gesamt", "Nicht MINT"),
      bl_fachkraft_epa_bulas             = "Gesamt"

  )

    # plot 6
  r4 <- list(klasse_iqb_linien = "4. Klasse",
             land_iqb_linien_4 = c("Deutschland", bl))
  r9 <- list(klasse_iqb_linien = "9. Klasse",
             land_iqb_linien_9 = c("Deutschland", bl),
             fach_iqb_linien_9 = "Mathematik")



# 1 - beschäftigte verlauf ----
  p1 <- stil(plotly::plotly_build(argument_verlauf_1(r)))
  exportiere(p1, f1, pfad, kz, 1000, 600)

# 2 - landkarte ----
  if (!kz %in% c("BE", "HB", "HH")) {
    karte2 <- arbeitsmarkt_lk_detail_map(r)

    karte_daten <- as.data.frame(plotly::plotly_data(karte2))
    karte_daten <- karte_daten[!duplicated(karte_daten$landkreis),
                               c("landkreis", "wert", "prob")]
    karte_daten <- karte_daten[order(-as.numeric(karte_daten$prob)), , drop = FALSE]

    exportiere(karte2, f2, pfad, kz, 1000, 800, datenspeziell = karte_daten)
  }

# 3 - berufswahl ----
  plots <- arbeitsmarkt_wahl_gender(r)

  p3a <- plots[[1]] |>
    plotly::style(texttemplate = "%{value:.1f} %", textposition = "outside") |>
    plotly::layout(
      title = list(text = ""),
      annotations = list(),
      legend = list(y = -0.06, x = 0.5, xanchor = "center", yanchor = "top"),
      margin = list(t = 40, b = 140)
    )

  p3b <- plots[[2]] |>
    plotly::style(texttemplate = "%{value:.1f} %", textposition = "outside") |>
    plotly::layout(
      title = list(text = ""),
      annotations = list(),
      legend = list(y = -0.06, x = 0.5, xanchor = "center", yanchor = "top"),
      margin = list(t = 40, b = 140)
    )

  exportiere(p3a, f3a, pfad, kz, 400, 450, txt_p = plots[[1]])
  exportiere(p3b, f3b, pfad, kz, 400, 450, txt_p = plots[[2]])

# 4 - nachwuchs verlauf ----
  p4 <- stil(plotly::plotly_build(argument_verlauf_2(r)))
  exportiere(p4, f4, pfad, kz, 1000, 600)

# 5 - nachwuchs verlauf MINT ----
  p5 <- stil(plotly::plotly_build(argument_nachwuchs(r)))
  exportiere(p5, f5, pfad, kz, 1000, 600)

# 6 - IQB Mindeststandards ----

  p6a <- stil(plotly::plotly_build(iqb_linien(r4)))
  p6b <- stil(plotly::plotly_build(iqb_linien(r9)))

  # nur die Linie des Bundeslandes bekommt Labels + feste Farben
    for (i in seq_along(p6a$x$data)) {
      nm <- p6a$x$data[[i]]$name
      if (nm == "Deutschland") {
        p6a$x$data[[i]]$line$color   <- "#b16fab"
        p6a$x$data[[i]]$marker$color <- "#b16fab"
      }
      if (nm == bl) {
        p6a$x$data[[i]]$line$color   <- "#154194"
        p6a$x$data[[i]]$marker$color <- "#154194"
        p6a$x$data[[i]]$textfont     <- list(color = "#154194")
        p6a$x$data[[i]]$mode         <- "lines+markers+text"
        p6a$x$data[[i]]$text         <- paste0(formatC(p6a$x$data[[i]]$y, format = "f", digits = 1, decimal.mark = ","))
        p6a$x$data[[i]]$textposition <- "top center"
      }
    }

  for (i in seq_along(p6b$x$data)) {
    nm <- p6b$x$data[[i]]$name
    if (nm == "Deutschland") {
      p6b$x$data[[i]]$line$color   <- "#b16fab"
      p6b$x$data[[i]]$marker$color <- "#b16fab"
    }
    if (nm == bl) {
      p6b$x$data[[i]]$line$color   <- "#154194"
      p6b$x$data[[i]]$marker$color <- "#154194"
      p6b$x$data[[i]]$textfont     <- list(color = "#154194")
      p6b$x$data[[i]]$mode         <- "lines+markers+text"
      p6b$x$data[[i]]$text         <- paste0(formatC(p6b$x$data[[i]]$y, format = "f", digits = 1, decimal.mark = ","))
      p6b$x$data[[i]]$textposition <- "top center"
    }
  }

  exportiere(p6a, f6a, pfad, kz, 500, 550)
  exportiere(p6b, f6b, pfad, kz, 500, 550)

# 7 - Kursbelegungen ----
  p7 <- stil(plotly::plotly_build(kurse_verlauf_single(r)))

  # Labels für Leistungskurse und Grundkurse (label ausblenden wenn werte nah beieinander)
  for (i in seq_along(p7$x$data)) {
    nm <- p7$x$data[[i]]$name

    if (grepl("Leistungs|Grund", nm, ignore.case = TRUE)) {
      y <- as.numeric(unlist(p7$x$data[[i]]$y))

      p7$x$data[[i]]$mode <- "lines+markers+text"
      p7$x$data[[i]]$text <- formatC(y, format = "f", digits = 1, decimal.mark = ",")
      p7$x$data[[i]]$textposition <- "top center"
    }
  }
  x_all <- lapply(p7$x$data, function(tr) as.character(unlist(tr$x)))
  y_all <- lapply(p7$x$data, function(tr) as.numeric(unlist(tr$y)))

  for (i in seq_along(p7$x$data)) {
    nm <- p7$x$data[[i]]$name
    if (is.null(nm) || !grepl("Grund", nm, ignore.case = TRUE)) next

    labels <- p7$x$data[[i]]$text

    for (j in seq_along(y_all[[i]])) {
      werte_im_jahr <- unlist(lapply(seq_along(p7$x$data), function(k) {
        index <- which(x_all[[k]] == x_all[[i]][j])
        y_all[[k]][index]
      }))

      werte_im_jahr <- werte_im_jahr[is.finite(werte_im_jahr)]

      if (diff(range(werte_im_jahr)) <= 5) {
        labels[j] <- ""
      } }
    p7$x$data[[i]]$text <- labels
  }

  exportiere(p7, f7, pfad, kz, 1000, 600)

# 8 - Lehramt Studizahlen ----
  p8 <- stil(plotly::plotly_build(studienzahl_verlauf_single(r)))

  # labels
  for (i in seq_along(p8$x$data)) {
    y <- as.numeric(unlist(p8$x$data[[i]]$y))

    p8$x$data[[i]]$mode <- "lines+markers+text"
    p8$x$data[[i]]$text <- prettyNum(round(y),big.mark = ".",decimal.mark = ",")
    p8$x$data[[i]]$textposition <- "top center"
  }

  exportiere(p8, f8, pfad, kz, 1000, 600)


# 9 - Enpassrisiko ----
  p9 <- plot_fachkraft_epa_bulas(r)
  p9 <- lapply(p9, plotly::plotly_build)

  farben <- c("#EE7775", "#FBBF24", "#35BD97")

  for (j in seq_along(p9)) {
    traces <- p9[[j]]$x$data

    prozente <- vapply(traces, function(tr) {
      hover <- if (!is.null(tr$text)) tr$text[1] else tr$hovertext[1]
      sub(".*Anteil: ([0-9]+) %.*", "\\1%", hover)
    }, character(1))

    werte <- as.numeric(sub("%", "", prozente))
    x_pos <- label_engpass(werte)

    ann <- p9[[j]]$x$layout$annotations

    for (i in seq_along(prozente)) {
      ann[[length(ann) + 1]] <- list(
        text = paste0("<b>", prozente[i], "</b>"),
        x = x_pos[i], y = 0.95,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 14, color = farben[i])
      )
      p9[[j]]$x$data[[i]]$marker$size <- 36
    }

    p9[[j]] <- plotly::layout(
      p9[[j]],
      legend = list(font = list(size = 14), x = 0.2, y = -0.14),
      annotations = ann,
      margin = list(b = 120)
    )
  }

  d9a <- as.data.frame(plotly::plotly_data(p9[[1]]))
  d9a <- d9a[!duplicated(d9a$value), !names(d9a) %in% c("beruf_num", "group_text", "group_col", "XX", "YY"), drop = FALSE]
  exportiere(p9[[1]], f9a, pfad, kz, 500, 700, datenspeziell = d9a)

  d9b <- as.data.frame(plotly::plotly_data(p9[[2]]))
  d9b <- d9b[!duplicated(d9b$value), !names(d9b) %in% c("beruf_num", "group_text", "group_col", "XX", "YY"), drop = FALSE]
  exportiere(p9[[2]], f9b, pfad, kz, 500, 700, datenspeziell = d9b)

# 10 - Altergruppen Beschäftigte ----
  p10 <- plotly::plotly_build(argument_demografie(r))|>
    plotly::layout(
      font  = list(size = 16),
      xaxis = list(tickfont = list(size = 15)),
      yaxis = list(tickfont = list(size = 14)),
      legend = list(font = list(size = 14)),
      margin = list(r = 80)
    )

  for (i in seq_along(p10$x$data)) {
    werte <- as.numeric(unlist(p10$x$data[[i]]$x))
    p10$x$data[[i]]$text <- prettyNum(werte, big.mark = ".", decimal.mark = ",")
    p10$x$data[[i]]$texttemplate <- "%{text}"
    p10$x$data[[i]]$textposition <- "outside"
    p10$x$data[[i]]$textfont <- list(size = 13, color = "black")
    p10$x$data[[i]]$cliponaxis <- FALSE
  }

  exportiere(p10, f10, pfad, kz, 1000, 600)

}

# Einmalige Grafiken ----

# 1 - GK/LK Belegungen Karte ----
r_map <- list(
  ansicht_mint_map = "Übersicht - Kartendiagramm",
  date_mint_map = 2024
)

karten_mint <- kurse_mint_map(r_map)

# Grundkurse
d_gk <- as.data.frame(plotly::plotly_data(karten_mint[[1]]))
d_gk <- d_gk[!duplicated(d_gk$NAME_1), c("NAME_1", "wert", "wert_sum", "prop")]
d_gk <- d_gk[order(-d_gk$prop), , drop = FALSE]
# Leistungskurse
d_lk <- as.data.frame(plotly::plotly_data(karten_mint[[2]]))
d_lk <- d_lk[!duplicated(d_lk$NAME_1), c("NAME_1", "wert", "wert_sum", "prop")]
d_lk <- d_lk[order(-d_lk$prop), , drop = FALSE]

exportiere(karten_mint[[1]], "DE_kurse_karte_grundkurse.png", pfad1, kz = "Deutschland", 1000, 800, datenspeziell = d_gk)
exportiere(karten_mint[[2]], "DE_kurse_karte_leistungskurse.png", pfad1, kz = "Deutschland", 1000, 800, datenspeziell = d_lk)


# 2 - Wirkungshebel ----
p_wh <- plotly::plotly_build(argument_wirkhebel(list())) |>
  plotly::layout(
    font  = list(size = 16),
    xaxis = list(tickfont = list(size = 15)),
    yaxis = list(tickfont = list(size = 14)),
    legend = list(font = list(size = 14)),
    margin = list(r = 80)
  )

for (i in seq_along(p_wh$x$data)) {
  werte <- as.numeric(unlist(p_wh$x$data[[i]]$x))
  p_wh$x$data[[i]]$text <- prettyNum(round(werte), big.mark = ".", decimal.mark = ",")
  p_wh$x$data[[i]]$texttemplate <- "%{text}"
  p_wh$x$data[[i]]$textposition <- "outside"
  p_wh$x$data[[i]]$textfont <- list(size = 13, color = "black")
  p_wh$x$data[[i]]$cliponaxis <- FALSE
}

exportiere(p_wh, "DE_wirkhebel.png", pfad1, kz = "Deutschland", 1000, 600)








