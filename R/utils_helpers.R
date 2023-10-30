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
colors_mint_vernetzt <- list(general = c("#154194", "#b16fab", "#efe8e6"),
                             attention = c("#00a87a", "#fcc433", "#ee7775"),
                             neutral = c("#141416", "#e6e8ec"),
                             gender = c("#f5adac", "#b1b5c3"))


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

  load("data/studierende_detailliert.rda")


  if(missing(spezif_i)&missing(spezif_r)){

    df1 <- studierende_detailliert %>%
      dplyr::filter(mint_select == "MINT"  | fach %in% c("Alle MINT-Fächer", "Alle Nicht MINT-Fächer"))

    df1 <- df1 %>%dplyr::select(fach)%>%
      unique()%>%
      as.vector()%>%
      unlist()%>%
      unname()

    df1 <- sort(df1)

  } else if (missing(spezif_i)){

    df1 <- studierende_detailliert %>%
      dplyr::filter(mint_select == "MINT"  | fach %in% c("Alle MINT-Fächer", "Alle Nicht MINT-Fächer"))%>%
      dplyr::filter(region %in%  spezif_r)

    df1 <- df1 %>%dplyr::select(fach)%>%
      unique()%>%
      as.vector()%>%
      unlist()%>%
      unname()

    df1 <- sort(df1)

  } else if(missing(spezif_r)){

    df1 <- studierende_detailliert %>%
      dplyr::filter(mint_select == "MINT"  | fach %in% c("Alle MINT-Fächer", "Alle Nicht MINT-Fächer"))%>%
      dplyr::filter(indikator %in%  spezif_i)

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

  logger::log_debug("set internatial ui selection for faecher")
  # names(studierende_europa)
  # names(studierende_anzahl_oecd)
  # names(studierende_absolventen_weltweit)

  # region <- "OECD"
  # weltweit kommt nicht vor, da es keine tiefere Unterteilung gibt
  # if (region == "Weltweit") {
  #
  #   df <- studierende_absolventen_weltweit  %>%
  #     dplyr::filter(fach == "Alle MINT-Fächer")
  # }
  selection <- NULL
  if (region == "OECD") {
    #load(file = system.file(package="datalab","data/studierende_anzahl_oecd.rda"))

    # selection <- studierende_anzahl_oecd %>%
    #   dplyr::filter(geschlecht == "Gesamt" &
    #                   fachbereich != "Alle") %>%
    #   dplyr::pull(fachbereich) %>%
    #   unique()
    selection <- c("Alle MINT-Fächer" = "MINT",
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
    #load(file = system.file(package="datalab","data/studierende_europa.rda"))

    # selection <- studierende_europa %>%
    #   dplyr::filter(geschlecht == "Gesamt"  &
    #                   mint_select == "mint" &
    #                   indikator == "Fächerwahl") %>%
    #   dplyr::pull(fach) %>%
    #   unique() %>%
    #   # extra hinzufügen da es sonst mit filter mint_select = FALSE wäre
    #   c(., "Alle MINT-Fächer")
    selection <- c("Alle MINT-Fächer",
                   "--- Naturwissenschaften, Mathematik und Statistik" = "Naturwissenschaften, Mathematik und Statistik",
                   "--- Informatik & Kommunikationstechnologie" = "Informatik & Kommunikationstechnologie",
                   "--- Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe" = "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe"
                   )
  }

  return(selection)

}

# Funktion zur Jahresauswahl bei internationalen Daten
international_ui_years <- function(region = "EU") {

  logger::log_debug("set internatial ui selection for years")
  selection <- NULL

  # for studium international
  if (region == "OECD") {
    #load(file = system.file(package="datalab","data/studierende_anzahl_oecd.rda"))

    selection <- studierende_anzahl_oecd %>%
      dplyr::filter(geschlecht == "Gesamt") %>%
      dplyr::pull(jahr) %>%
      unique() %>%
      sort()
  }

  if (region == "EU") {
    #load(file = system.file(package="datalab","data/studierende_europa.rda"))

    selection <- studierende_europa %>%
      dplyr::filter(geschlecht == "Gesamt"  &
                      mint_select == "mint" &
                      indikator == "Fächerwahl") %>%
      dplyr::pull(jahr) %>%
      unique() %>%
      sort()
  }

  if (region == "Weltweit"){
    selection <- studierende_absolventen_weltweit %>%
      dplyr::filter(geschlecht == "Insgesamt") %>%
      dplyr::filter(jahr != "2022") %>%
      dplyr::pull(jahr) %>%
      unique() %>%
      sort()
  }

  # for schule international
  if (region == "TIMSS") {
    #load(file = system.file(package="datalab","data/schule_timss.rda"))

    selection <- schule_timss %>%
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

    selection <- schule_pisa %>%
      dplyr::filter(bereich == "Ländermittel" &
                       indikator == "Insgesamt" &
                      !is.na(wert)) %>%
      dplyr::pull(jahr) %>%
      unique() %>%
      sort()
  }

  return(selection)
}
