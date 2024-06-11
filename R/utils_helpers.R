#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

helper_title_home <- function(indikator){

  if ((indikator == "Auszubildende" | indikator == "Beschäftigte")) {

    title_help_sub <- "andere Berufszweige"

    title_help <- paste0("anderen Berufszweigen bei<br> ", indikator,"n")

  } else {

    title_help_sub <- "andere Fächer"

    if ((indikator == "Habilitationen" | indikator == "Promotionen (angestrebt)")){

      title_help <- paste0("anderen Fächern bei<br> ", indikator)

    }else{

      title_help <- paste0("anderen Fächern bei<br> ", indikator,"n")
    }
  }

  return(title_help)

}


#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

`%!in%` <- Negate(`%in%`)


#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

dictionary_title_studium_studentenzahl <- list("eingeschrieben" = "die insgesamt eingeschrieben sind",
                                               "1hs" = "die im 1. Hochschulsemester eingeschrieben sind",
                                               "1fs" = "die im 1. Fachsemester eingeschrieben sind")


#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

dictionary_title_studium <- list("Mathe" = "in Mathematik",
                                 "Ingenieur" = "am Ingenieurswesen")


#' helpers
#'
#' @description A utils function to make sure that rounding does not cause
#' a value below or above 100
#'
#' @return The return value is a vector of values
#'
#' @noRd
round_preserve_sum <- function(x, digits = 0) {

  up <- 10 ^ digits

  x <- x * up

  y <- floor(x)

  indices <- tail(order(x-y), round(sum(x)) - sum(y))

  y[indices] <- y[indices] + 1

  y <- y / up

  return(y)
}


#' helpers
#'
#' @description A utils list which contains the hex codes of colors from the
#' design guide
#'
#' @noRd
# Veraltet - nicht die richtigen Farben und nicht richtige Kategroriezuordnung
colors_mint_vernetzt <- list(general = c("#154194", "#b16fab", "#00a87a"),
                             attention = c("#00a87a", "#fcc433", "#ee7775"),
                             short = c("#154194", "#b16fab")
                             #neutral = c("#141416", "#e6e8ec"),
                             #gender = c("#f5adac", "#b1b5c3"))
)


#' helpers
#'
#' @description A utils list which contains the all states of the former west and
#' east of germany
#'
#' @noRd
states_east_west <- list(west = c("Baden-Württemberg", "Bayern", "Bremen", "Hamburg",
                                  "Hessen", "Niedersachsen", "Nordrhein-Westfalen",
                                  "Rheinland-Pfalz", "Saarland", "Schleswig-Holstein"),
                         east = c("Brandenburg", "Mecklenburg-Vorpommern", "Sachsen",
                                  "Sachsen-Anhalt", "Thüringen", "Berlin"))


#' @description A function to create the value box
#'
#' @return The return is a value box
#' @param value The value to display in the box. Usually a number or short text.
#' @param title Title of the value box
#' @param subtitle Subtitle below the big number
#' @param icon An icon tag
#' @param color color of the box
#' @param width Width of the box
#' @param href An optional URL to link to.
#' @param info Text of information helper.
#' @noRd
#'
valueBox2 <- function(value, title, subtitle, icon = NULL, color = "aqua", width = 4, href = NULL,
                      info = NULL, type = "andere"){

  if (type == "Frauen"){

    style <- paste0("background-color: ", "#f5adac; color:white;")

  } else {

    style <- paste0("background-color: ", "#b1b5c3; color:white;")

  }

  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")

  info_icon <- tags$small(
    tags$i(
      class = "fa fa-info-circle fa-lg",
      title = info,
      `data-toggle` = "tooltip",
      style = "color: rgba(255, 255, 255, 0.75);"
    ),
    class = "pull-right"
  )

  boxContent <- div(
    class = "small-box",
    style = style,
    div(
      class = "inner",
      info_icon,
      tags$small(title),
      h3(value),
      p(subtitle)
    ),
    if (!is.null(icon)) div(class = "icon-large", icon, style = "z-index; 0")
  )

  if (!is.null(href))
    boxContent <- a(href = href, boxContent)

  div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    boxContent
  )
}

#' preprocess_schule
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

share_pie <- function(df) {
  # calculate proportions
  df$props <- sum(df$wert)

  df <- df %>% dplyr::group_by(fachbereich, anzeige_geschlecht) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100

  df$proportion <- round_preserve_sum(as.numeric(df$proportion),0)

  return(df)
}

share_pie_neu <- function(df) {
  # calculate proportions
  df$props <- sum(df$wert)

  df <- df %>% dplyr::group_by(fachbereich, geschlecht) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100

  df$proportion <- round_preserve_sum(as.numeric(df$proportion),0)

  return(df)
}

# funktion zur ordnung der fachauswahl für studierende_detailliert
studi_det_ui_faecher <-function(spezif_i, spezif_r){

  require(magrittr)

  #load("data/studierende_detailliert.rda")


  if(missing(spezif_i)&missing(spezif_r)){

    df1 <- dplyr::tbl(con, from = "studierende_detailliert") %>%
      dplyr::filter(mint_select == "MINT"  | fach %in% c("Alle MINT-Fächer", "Alle Nicht MINT-Fächer")) %>%
      dplyr::select(fach)%>%
      dplyr::collect()

    df1 <- df1 %>%
      unique()%>%
      as.vector()%>%
      unlist()%>%
      unname()

    df1 <- sort(df1)

  } else if (missing(spezif_i)){

    df1 <- dplyr::tbl(con, from = "studierende_detailliert") %>%
      dplyr::filter(mint_select == "MINT"  | fach %in% c("Alle MINT-Fächer", "Alle Nicht MINT-Fächer"))%>%
      dplyr::filter(region %in%  spezif_r) %>%
      dplyr::collect()

    df1 <- df1 %>%dplyr::select(fach)%>%
      unique()%>%
      as.vector()%>%
      unlist()%>%
      unname()

    df1 <- sort(df1)

  } else if(missing(spezif_r)){

    df1 <- dplyr::tbl(con, from = "studierende_detailliert") %>%
      dplyr::filter(mint_select == "MINT"  | fach %in% c("Alle MINT-Fächer", "Alle Nicht MINT-Fächer"))%>%
      dplyr::filter(indikator %in%  spezif_i) %>%
      dplyr::collect()

    df1 <- df1 %>%dplyr::select(fach)%>%
      unique()%>%
      as.vector()%>%
      unlist()%>%
      unname()

    df1 <- sort(df1)

  }


}


# Funktion zur Fachauswahl bei international Daten
international_ui_faecher <- function(region = "EU") {

  selection <- NULL
  if (region == "OECD") {

    selection <- c("MINT",
                   "--- Naturwissenschaften, Mathematik und Statistik" = "Naturwissenschaften, Mathematik und Statistik",
                   "--- Informatik & Kommunikationstechnologie" = "Informatik & Kommunikationstechnologie",
                   "--- Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe" = "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                   "Pädagogik",
                   "Geisteswissenschaften und Künste",
                   "Sozialwissenschaften, Journalismus und Informationswesen",
                   "Wirtschaft, Verwaltung und Recht",
                   "Dienstleistungen",
                   "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin")
  }

  if (region == "EU") {

    selection <- c("Alle MINT-Fächer",
                   "--- Naturwissenschaften, Mathematik und Statistik" = "Naturwissenschaften, Mathematik und Statistik",
                   "--- Informatik & Kommunikationstechnologie" = "Informatik & Kommunikationstechnologie",
                   "--- Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe" = "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe"
    )
  }

  # for arbeitsmarkt international
  if (region == "arbeit") {
    #load(file = system.file(package="datalab","data/schule_timss.rda"))

    selection <- dplyr::tbl(con, from = "arbeitsmarkt_anfaenger_absolv_oecd") %>%
      dplyr::filter(geschlecht == "Gesamt" &
                      variable %in% c("Anteil Absolvent*innen nach Fach an allen Fächern",
                                      "Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern")
      ) %>%
      dplyr::pull(fachbereich) %>%
      unique() %>%
      sort()
  }

  return(selection)

}

# Funktion zur Jahresauswahl bei internationalen Daten
international_ui_years <- function(region = "EU") {

  selection <- NULL

  # for studium international
  if (region == "OECD") {
    #load(file = system.file(package="datalab","data/studierende_anzahl_oecd.rda"))

    selection <- dplyr::tbl(con, from = "studierende_anzahl_oecd") %>%
      dplyr::filter(geschlecht == "Gesamt") %>%
      dplyr::pull(jahr) %>%
      unique() %>%
      sort()
  }

  if (region == "EU") {
    #load(file = system.file(package="datalab","data/studierende_europa.rda"))

    selection <- dplyr::tbl(con, from = "studierende_europa") %>%
      dplyr::filter(geschlecht == "Gesamt"  &
                      mint_select == "mint" &
                      indikator == "Fächerwahl") %>%
      dplyr::pull(jahr) %>%
      unique() %>%
      sort()
  }

  if (region == "Weltweit"){
    selection <- dplyr::tbl(con, from = "studierende_absolventen_weltweit") %>%
      dplyr::filter(geschlecht == "Insgesamt") %>%
      dplyr::filter(jahr != "2022") %>%
      dplyr::pull(jahr) %>%
      unique() %>%
      sort()
  }

  # for schule international
  if (region == "TIMSS") {
    #load(file = system.file(package="datalab","data/schule_timss.rda"))

    selection <- dplyr::tbl(con, from = "schule_timss") %>%
      dplyr::filter(ordnung %in% c("Achievement",
                                   "Benchmarks") &
                      indikator %in% c("Mittlerer int'l. Maßstab (475)",
                                       "Insgesamt")
      ) %>%
      dplyr::pull(jahr) %>%
      unique() %>%
      sort()
  }

  if (region == "PISA") {
    #load(file = system.file(package="datalab","data/schule_pisa.rda"))

    selection <- dplyr::tbl(con, from = "schule_pisa") %>%
      dplyr::filter(bereich == "Ländermittel" &
                      indikator == "Insgesamt" &
                      !is.na(wert)) %>%
      dplyr::pull(jahr) %>%
      unique() %>%
      sort()
  }

  # for arbeitsmarkt international
  if (region == "arbeit") {
    #load(file = system.file(package="datalab","data/schule_timss.rda"))

    selection <- dplyr::tbl(con, from = "arbeitsmarkt_anfaenger_absolv_oecd") %>%
      dplyr::filter(
        geschlecht == "Gesamt" &
          # filter year, since before there are not all infos available
          jahr >= 2013 &
          variable %in% c("Anteil Absolvent*innen nach Fach an allen Fächern",
                          "Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern")

      ) %>%
      dplyr::pull(jahr) %>%
      unique() %>%
      sort()
  }

  return(selection)
}

# Funktion zur Länderauswahl bei internationalen Daten
international_ui_country <- function(type = "arbeit", n = NA) {

  selection <- NULL

  for_year <- dplyr::tbl(con, from = "arbeitsmarkt_anfaenger_absolv_oecd") %>%
    dplyr::filter(
      geschlecht == "Gesamt"
    ) %>%
    dplyr::collect()
  year <- max(for_year$jahr)

  # for studium international
  if (type == "arbeit") {
    #load(file = system.file(package="datalab","data/studierende_anzahl_oecd.rda"))

    tmp_df <-  dplyr::tbl(con, from = "arbeitsmarkt_anfaenger_absolv_oecd") %>%
      dplyr::filter(geschlecht == "Gesamt" &
                      jahr == year &
                      variable %in% c("Anteil Absolvent*innen nach Fach an allen Fächern",
                                      "Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern")
      ) %>%
      dplyr::collect()



    if (!is.na(n)) {
      tmp_df <- tmp_df %>%
        dplyr::filter(
          fachbereich == "MINT" &
            variable == "Anteil Absolvent*innen nach Fach an allen Fächern") %>%
        dplyr::group_by(land) %>%
        dplyr::summarise(wert = sum(wert)) %>%
        dplyr::arrange(desc(wert)) %>%
        head(n = 10)
    }

    selection <- tmp_df %>%
      dplyr::pull(land) %>%
      unique() %>%
      sort()
  }

}

# Funktion zur Jahresauswahl bei Fachkraft Daten
fachkraft_ui_years <- function() {


  selection <- NULL


  selection <- dplyr::tbl(con, from = "arbeitsmarkt_epa_detail") %>%
    dplyr::filter(indikator == "Engpassindikator") %>%
    dplyr::pull(jahr) %>%
    unique() %>%
    sort()


  return(selection)
}


# Funktion zur Fachauswahl bei Fachkraft Daten
fachkraft_ui_faecher <- function(exclude = c()) {


  selection <- NULL

  # selection <- arbeitsmarkt_epa_detail %>%
  #   dplyr::filter(indikator == "Engpassindikator") %>%
  #   dplyr::pull(mint_zuordnung) %>%
  #   unique() %>%
  #   append("MINT")

  # manual selection to have correct order and naming
  selection <- c(
   "Alle Berufe" ="Gesamt",
    "MINT gesamt", #"MINT",
    "Informatik",
    "Landtechnik",
    "Produktionstechnik",
    "Bau- und Gebäudetechnik",
    "Mathematik, Naturwissenschaften",
    "Verkehrs-, Sicherheits- und Veranstaltungstechnik",
    "Gesundheitstechnik",
    "Nicht MINT"
  )

  selection <- setdiff(selection, exclude)


  return(selection)

}

# Funktion zur Berufslevelauswahl bei Fachkraft Daten
fachkraft_ui_berufslevel <- function() {


  selection <- NULL

  # selection <- arbeitsmarkt_epa_detail %>%
  #   dplyr::filter(indikator == "Engpassindikator") %>%
  #   dplyr::pull(anforderung) %>%
  #   unique() %>%
  #   append("Gesamt")
  # manual selection to have correct order and adding "gesamt"
  selection <- c(
    "Gesamt",
    "Fachkräfte",
    "Spezialist*innen",
    "Expert*innen"
  )


  return(selection)
}

fachkraft_ui_berufe <- function(level = "Fachkräfte", zeitpunkt = 2023) {


  selection <- NULL

  selection <- dplyr::tbl(con, from = "arbeitsmarkt_epa_detail") %>%
    dplyr::filter(indikator == "Engpassindikator" &
                    anforderung == level &
                    jahr == zeitpunkt &
                    !is.na(wert)) %>%
    dplyr::pull(beruf) %>%
    unique()


  return(selection)
}

fachkraft_ui_wirkhebel <- function(version = "lang") {

  selection <- NULL

  if(version == "lang"){

    selection <- c(
      "Gesamteffekt",
      "MINT-Nachwuchs fördern" = "MINT-Bildung",
      "Mädchen und Frauen in MINT fördern" = "Frauen in MINT",
      "Zuwanderung MINT-Fachkräfte" = "Internationale MINT-Fachkräfte",
      "Verbleib älterer MINT-Fachkräfte" = "Beteiligung älterer MINT-Fachkräfte"
    )
  }else if (version == "kurz"){
    selection <- c(
      "MINT-Nachwuchs fördern" = "MINT-Bildung",
      "Mädchen und Frauen in MINT fördern" = "Frauen in MINT",
      "Zuwanderung MINT-Fachkräfte" = "Internationale MINT-Fachkräfte",
      "Verbleib älterer MINT-Fachkräfte" = "Beteiligung älterer MINT-Fachkräfte"
    )
  }
  return(selection)
}

fachkraft_ui_scenario <- function(wirkhebel) {

  selection <- NULL

  if(wirkhebel %in% c("MINT-Bildung", "Gesamteffekt")){
    selection <- c(
      "positives Szenario" = "Verbesserung",
      "negatives Szenario" = "Verschlechterung"
    )
  } else if(wirkhebel == "Internationale MINT-Fachkräfte"){
    selection <- c(
      "positives Szenario" = "Verbesserung",
      "Rückgang im Positivtrend der Zuwanderung" = "Verschlechterung",
      "vollständiger Stillstand der Zuwanderung" = "Stillstand"


    )
  } else if(wirkhebel == "Beteiligung älterer MINT-Fachkräfte"){
    selection <- c("positives Szenario" = "Verbesserung")

  } else if(wirkhebel == "Frauen in MINT"){
    selection <- c(
      "positives Szenario" = "Verbesserung",
      "kombiniertes Szenario" = "starke Verbesserung"
    )
  } else if(wirkhebel == "Basis-Szenario"){
    selection <- c(
      "Fortschreibung aktueller Lage" = "Status-quo"
    )
  }

  return(selection)
}

fachkraft_ui_prognose_gruppen <- function() {

  selection <- NULL

  selection <- c(
    "Berufslevel",
    "Geschlecht",
    "Nationalität"
  )

  return(selection)
}


# function to extract a plot title from a highcharter object
get_plot_title <- function(plot, path = ".") {

  shiny::req(highcharter::is.highchart(plot))

  out <- file.path(
    path,
    paste0(
      ifelse(
        is.null(plot$x$hc_opts$title$text),
        "MINTvernetzt_PLOT",
        plot$x$hc_opts$title$text),
      "_", format(Sys.time(), "%Y%M%d_%H%M"),
      ".png")
  )
  return(out)
}

# Funktion zur Jahreswahl bei Arbeit-Fachkraft Daten
arbeit_fachkraft_ui_years <- function() {

  selection <- NULL


  selection <- dplyr::tbl(con, from = "arbeitsmarkt_fachkraefte") %>%
    dplyr::filter(indikator %in% c("Abgeschlossene Vakanzzeit",
                                   "Arbeitslosen-Stellen-Relation")) %>%
    dplyr::pull(jahr) %>%
    unique() %>%
    sort()


  return(selection)
}

# Funktion zur Region bei Arbeit-Fachkraft Daten
arbeit_fachkraft_ui_region <- function() {

  selection <- NULL


  selection <- dplyr::tbl(con, from = "arbeitsmarkt_fachkraefte") %>%
    dplyr::filter(indikator %in% c("Abgeschlossene Vakanzzeit",
                                   "Arbeitslosen-Stellen-Relation")) %>%
    dplyr::pull(region) %>%
    unique()


  return(selection)
}

# function to download plots with added cption and logo
add_caption_and_download <- function(
    hc,
    filename = "plot.png",
    labelformat = '{point.y}',
    with_labels = TRUE
    ,
    width = 450,
    height = 300
    ) {

  require(highcharter)
  require(webshot2)
  require(htmlwidgets)


  ## roll back webshot2

  remove.packages("webshot2")
  # packageurl <- "https://cran.r-project.org/src/contrib/Archive/webshot2/webshot2_0.1.0.tar.gz"
  # install.packages(packageurl, repos=NULL, type="source")
  #
  # packageVersion("webshot2")
  #

  # set chromote, determine chromium variant
  Sys.setenv(
    CHROMOTE_CHROME = "C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe"
  )

  # force the use of pagedown to install chrome on shinyapps.io (this is a workaround)
  require(pagedown)
  # force the use of curl because chromote needs it (see https://github.com/rstudio/chromote/issues/37)
  require(curl)

  # hc <- plot
  shiny::req(highcharter::is.highchart(hc))

  shiny::showNotification(ui = "Plot wird gespeichert...",
                          type = "message",
                          duration = NULL,
                          id = "download_notification")
#browser()
  hc_out <- hc %>%
    # Add the caption to the plot
    highcharter::hc_size(width = width, height = height) %>%
    highcharter::hc_caption(text = paste0(
      '<div style="width: ',width - 10, 'px;',
      ' display: flex; justify-content: space-between;">',
      '<span>',
      # '<span style="font-size: 10px;">', # max-width: ', width - 50, 'px;
      'Quellen: Statistisches Bundesamt, 2022; Bundesagentur für Arbeit, 2022;',
      ' KMK, 2022, alle auf Anfrage,<br>',
      ' Eigene Berechnungen durch MINTvernetzt</span>',
      '<span>',
      '<span style="padding-right: 10px;">',
      #'<img src="https://mint-vernetzt.de/static/e99e5a7a75c99c8651863585408242bb/mintvernetzt_og-img.png"',
      '<img src="https://raw.githubusercontent.com/mint-vernetzt/datalab/main/inst/app/www/MINTvernetztLogo_klein.png"',
      #'<img src="www/MINTvernetztLogo_klein.png"',
      'alt="MINT vernetzt Logo" width="30" height="30">',
      '</span>',
      '</div>'
    ),
    useHTML = TRUE,
    align = "right") %>%
    # TODO add correct font
    highcharter::hc_title(
      # only overwrite needed values
      style = list(fontFamily = "Calibri")
    ) %>%
    highcharter::hc_chart(
      style = list(fontFamily = "Calibri")
    )

  if (with_labels) {
    hc_out <- hc_out %>%
      highcharter::hc_plotOptions(
        series = list(
          # Add value labels
          dataLabels = list(
            enabled = TRUE,
            format = paste(labelformat)
          )
        )
      )
  }

  #browser()
  #print(hc_out)
  # Save the plot as a standalone HTML file
  html_file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(hc_out, file = html_file, selfcontained = TRUE)
  print(html_file)
  # # Capture the HTML as a PNG image
  webshot2::webshot(url = html_file,
                    file = filename
                    ,
                    delay = 2,
                    zoom = 2,
                    vwidth = width,
                    vheight = height
                    )

  shiny::showNotification(
    ui = paste0("Gespeichert als '", filename, "'"),
    type = "message",
    id = "download_notification")

  return(NULL)
}


