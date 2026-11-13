
# Regioberichte gebündelte Downloads


pkgload::load_all(".")
library(plotly)

con <- DBI::dbConnect(duckdb::duckdb(), "data/mint_db.duckdb", read_only = TRUE)

pfad <- "C:/Users/mis/OneDrive - Stifterverband/Dateiablage - MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/08 GPT_KI Analysehilfe/08_Regionalberichte"

# -------------------------------------------------------------------------------
# TODO:
# accompanying txt datei mit allen daten die für label in canva und für gpt zum bericht schreiben gebraucht werden
# -------------------------------------------------------------------------------


# Grafikfunktion ----

# Plotly-Grafik als PNG speichern (ohne Python, braucht Chrome oder Edge)

Sys.setenv(CHROMOTE_CHROME = "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")

speichere_png <- function(p, datei, breite, hoehe) {
  html <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(p, html, selfcontained = TRUE)
  webshot2::webshot(html, file = datei, vwidth = breite, vheight = hoehe, zoom = 2)
}


laender <- c(
  BW = "Baden-Württemberg", BY = "Bayern", BE = "Berlin", BB = "Brandenburg",
  HB = "Bremen", HH = "Hamburg", HE = "Hessen", MV = "Mecklenburg-Vorpommern",
  NI = "Niedersachsen", NW = "Nordrhein-Westfalen", RP = "Rheinland-Pfalz",
  SL = "Saarland", SN = "Sachsen", ST = "Sachsen-Anhalt",
  SH = "Schleswig-Holstein", TH = "Thüringen"
)

laender <- laender["BY"]

for (kz in names(laender)) {

  bl <- laender[[kz]]
  tmp <- tempdir()

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

  r <- list(
    # plot 1, 4, 5
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



# 1 - beschäftigte verlauf
  p1 <- argument_verlauf_1(r) |>
    plotly::style(textposition = "top left") |>
    plotly::layout(
      font  = list(size = 18),
      xaxis = list(tickfont = list(size = 16)),
      yaxis = list(tickfont = list(size = 16))
    )

  n <- length(p1$x$data[[1]]$x)
  p1$x$data[[1]]$textposition <- c("middle right", rep("top left", n - 3), "top center", "top center")


  speichere_png(p1, file.path(tmp, f1), 1000, 600)
  files <- f1

# 2 - landkarte
# hier die top 3 lk daten iwie mitgeben lassen dass man das in canva nachbauen kann
  if (!kz %in% c("BE", "HB", "HH")) {
    speichere_png(arbeitsmarkt_lk_detail_map(r), file.path(tmp, f2), 1000, 800)
    files <- c(f1, f2)
  }

# 3 - berufswahl
# EVTL LABEL IN CANVA
  plots <- arbeitsmarkt_wahl_gender(r)

  speichere_png(
    plots[[1]] |>
      plotly::style(texttemplate = "%{value:.1f} %", textposition = "outside") |>
      plotly::layout(title = list(text = ""), annotations = list(),
                     legend = list(y = -0.04, x = 0.5, xanchor = "center", yanchor = "top"),
                     margin = list(t = 40, b = 140)),
    file.path(tmp, f3a), 400, 450)

  speichere_png(
    plots[[2]] |>
      plotly::style(texttemplate = "%{value:.1f} %", textposition = "outside") |>
      plotly::layout(title = list(text = ""), annotations = list(),
                     legend = list(y = -0.04, x = 0.5, xanchor = "center", yanchor = "top"),
                     margin = list(t = 40, b = 140)),
    file.path(tmp, f3b), 400, 450)
  files <- c(files, f3a, f3b)

# 4 - nachwuchs verlauf
  p4 <- argument_verlauf_2(r) |>
    plotly::layout(
      font  = list(size = 18),
      xaxis = list(tickfont = list(size = 16)),
      yaxis = list(tickfont = list(size = 16)),
      legend = list(font = list(size = 13))
    )
  speichere_png(p4, file.path(tmp, f4), 1000, 600)
  files <- c(files, f4)

# 5 - nachwuchs verlauf MINT
  p5 <- argument_nachwuchs(r) |>
    plotly::layout(
      font  = list(size = 18),
      xaxis = list(tickfont = list(size = 16)),
      yaxis = list(tickfont = list(size = 16)),
      legend = list(font = list(size = 13))
    )

  speichere_png(p5, file.path(tmp, f5), 1000, 600)
  files <- c(files, f5)

# 6 - IQB Mindeststandards

  p6a <- iqb_linien(r4)
  p6b <- iqb_linien(r9)

  # nur die Linie des Bundeslandes bekommt Labels
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

  p6a <- plotly::layout(p6a,
                        font  = list(size = 16),
                        xaxis = list(tickfont = list(size = 16)),
                        yaxis = list(tickfont = list(size = 16)),
                        legend = list(font = list(size = 14))
  )
  p6b <- plotly::layout(p6b,
                        font  = list(size = 16),
                        xaxis = list(tickfont = list(size = 16)),
                        yaxis = list(tickfont = list(size = 16)),
                        legend = list(font = list(size = 14))
  )


  speichere_png(p6a, file.path(tmp, f6a), 500, 550)
  speichere_png(p6b, file.path(tmp, f6b), 500, 550)

  files <- c(files, f6a, f6b)

# 7 - Kursbelegungen
  p7 <- kurse_verlauf_single(r)|>
    plotly::layout(
      font  = list(size = 18),
      xaxis = list(tickfont = list(size = 16)),
      yaxis = list(tickfont = list(size = 16)),
      legend = list(font = list(size = 13))
    )

  # Labels für Leistungskurse oben, Grundkurse unten
  for (i in seq_along(p7$x$data)) {
    nm <- p7$x$data[[i]]$name

    if (grepl("Leistungs", nm, ignore.case = TRUE)) {
      position <- "top center"
    } else if (grepl("Grund", nm, ignore.case = TRUE)) {
      position <- "bottom center"
    } else {
      next
    }

    y <- as.numeric(unlist(p7$x$data[[i]]$y))

    p7$x$data[[i]]$mode <- "lines+markers+text"
    p7$x$data[[i]]$text <- paste0(formatC(y, format = "f", digits = 1, decimal.mark = ","))
    p7$x$data[[i]]$textposition <- position
  }

  speichere_png(p7, file.path(tmp, f7), 1000, 600)
  files <- c(files, f7)

# 8 - Lehramt Studizahlen
  p8 <- studienzahl_verlauf_single(r)|>
    plotly::layout(
      font  = list(size = 18),
      xaxis = list(tickfont = list(size = 16)),
      yaxis = list(tickfont = list(size = 16)),
      legend = list(font = list(size = 13))
    )

  # labels
  for (i in seq_along(p8$x$data)) {
    y <- as.numeric(unlist(p8$x$data[[i]]$y))

    p8$x$data[[i]]$mode <- "lines+markers+text"
    p8$x$data[[i]]$text <- prettyNum(round(y),big.mark = ".",decimal.mark = ",")
    p8$x$data[[i]]$textposition <- "top center"
  }

  speichere_png(p8, file.path(tmp, f8), 1000, 600)
  files <- c(files, f8)

# 9 - Enpassrisiko
  p9 <- plot_fachkraft_epa_bulas(r)

  p9[[1]] <- plotly::layout(
    p9[[1]],
    legend = list(font = list(size = 13), x = 0.2)
  )

  p9[[2]] <- plotly::layout(
    p9[[2]],
    legend = list(font = list(size = 13), x = 0.2)
  )

  speichere_png(p9[[1]], file.path(tmp, f9a), 500, 700)
  speichere_png(p9[[2]], file.path(tmp, f9b), 500, 700)

  files <- c(files, f9a, f9b)


# alle in ein zip pro bula
  zip::zip(file.path(pfad, paste0(kz, ".zip")),
           files = files, root = tmp, mode = "cherry-pick")
}
