## code to prepare `hochschulpersonal` dataset goes here

bundeslaender <-
  c(
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
    "Thüringen"
  )

enrich_bundeslaender <- function(list_element) {

  if (length(setdiff(bundeslaender, list_element %>% dplyr::pull(region)) >
             0)) {

    missing_regions <-
      data.frame(
        region = setdiff(bundeslaender, list_element %>% dplyr::pull(region)),
        insgesamt = "0",
        geistes = 0,
        sport = 0,
        rechts_wirt_sozial = 0,
        mathe_natwi = 0,
        humanmedizin = 0,
        agrar_fort_ernaerungs_veterinaer = 0,
        ingenieur = 0,
        kunst = 0,
        zentral_exkl_klinik = 0,
        zentral_klinik = 0,
        group_col = "0"
      )

  } else {

    missing_regions <- data.frame()

  }

  all_regions <- dplyr::bind_rows(list_element, missing_regions)

  return(all_regions)
}


get_all_hochschulpersonal_data <- function(filename) {

  sheets <- readxl::excel_sheets(filename)

  x <- lapply(sheets, function(X) get_hochschulpersonal_data(filename, sheet = X))

  hochschulpersonal_data <- purrr::reduce(x, rbind)
  return(hochschulpersonal_data)
}

get_hochschulpersonal_data <- function(filename, sheet) {

  hochschulpersonal_read <-
    suppressMessages(readxl::read_xlsx(filename,
                                       sheet = sheet,
                                       skip = 11)) %>%
    janitor::clean_names() %>%
    janitor::remove_empty(c("rows", "cols"))


  if (as.numeric(sheet) > 2014) {

    colnames(hochschulpersonal_read) <-
      c(
        "region",
        "insgesamt",
        "geistes",
        "sport",
        "rechts_wirt_sozial",
        "mathe_natwi",
        "humanmedizin",
        "agrar_fort_ernaerungs_veterinaer",
        "ingenieur",
        "kunst",
        "zentral_exkl_klinik",
        "zentral_klinik"
      )

  } else {

    colnames(hochschulpersonal_read) <-
      c(
        "region",
        "insgesamt",
        "geistes",
        "sport",
        "rechts_wirt_sozial",
        "mathe_natwi",
        "humanmedizin",
        "agrar_fort_ernaerungs",
        "veterinaer",
        "ingenieur",
        "kunst",
        "zentral_exkl_klinik",
        "zentral_klinik"
      )

    hochschulpersonal_read <- hochschulpersonal_read %>%
      dplyr::mutate(agrar_fort_ernaerungs = agrar_fort_ernaerungs + veterinaer) %>%
      dplyr::rename(agrar_fort_ernaerungs_veterinaer = agrar_fort_ernaerungs) %>%
      dplyr::select(-veterinaer)

  }


  hochschulpersonal_read <- hochschulpersonal_read %>%
    dplyr::mutate(region = gsub("\\.", "", region)) %>%
    dplyr::mutate(region = stringr::str_trim(region, side = c("both"))) %>%
    dplyr::filter(!grepl("___", region)) %>%
    dplyr::filter(!grepl("Berichtsjahr", region)) %>%
    dplyr::filter(!grepl("Ergebnisse nach", region)) %>%
    dplyr::filter(!grepl("Statistisches Bundesamt", region)) %>%
    dplyr::mutate(insgesamt = dplyr::case_when(is.na(insgesamt) ~ region,
                                               TRUE ~  as.character(insgesamt))) %>%
    dplyr::mutate(region = dplyr::case_when(insgesamt != region ~ region,
                                            TRUE ~  as.character(NA)))

  if (any(
    hochschulpersonal_read %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::filter(region %in% bundeslaender) %>%
    dplyr::pull(count) != 18
  )) {
    hochschulpersonal_read <- hochschulpersonal_read %>%
      dplyr::mutate(group_col = dplyr::case_when(
        region %in% c("Insgesamt", "Zusammen") ~ paste0(region, insgesamt),
        TRUE ~ as.character(NA)
      )) %>%
      dplyr::filter(!grepl("___", region)) %>%
      dplyr::filter(!grepl("Berichtsjahr", region)) %>%
      dplyr::filter(!grepl("Ergebnisse nach", region)) %>%
      dplyr::filter(!grepl("Statistisches Bundesamt", region)) %>%
      tidyr::fill(group_col, .direction = "up") %>%
      dplyr::mutate(group_col = factor(
        group_col,
        levels = unique(group_col),
        labels = letters[1:length(unique(group_col))]
      )) %>%
      dplyr::group_split(group_col, .keep = TRUE) %>%
      purrr::map_dfr(.f = enrich_bundeslaender) %>%
      dplyr::select(-group_col)
  }

  hochschulpersonal_data <- hochschulpersonal_read %>%
    dplyr::filter(!grepl("___", region)) %>%
    dplyr::filter(!grepl("Berichtsjahr", region)) %>%
    dplyr::filter(!grepl("Ergebnisse nach", region)) %>%
    dplyr::filter(!grepl("Statistisches Bundesamt", region)) %>%
    dplyr::filter(!region %in% "Zusammen") %>%
    dplyr::filter(!region %in% "Insgesamt") %>%
    dplyr::slice(-(purrr::map2(
      which(.$insgesamt == "Personal insgesamt"),
      (which(.$insgesamt == "Personal insgesamt") + 16),
      ~ .x:.y
    ) %>% unlist())) %>%
    dplyr::slice(-(purrr::map2(
      which(.$insgesamt == "Hauptberufliches Personal insgesamt"),
      (which(.$insgesamt == "Hauptberufliches Personal insgesamt") + 16),
      ~ .x:.y
    ) %>% unlist())) %>%
    dplyr::slice(-(purrr::map2(
      which(.$insgesamt == "Nebenberufliches Personal insgesamt"),
      (which(.$insgesamt == "Nebenberufliches Personal insgesamt") + 16),
      ~ .x:.y
    ) %>% unlist())) %>%
    dplyr::slice(-(purrr::map2(
      grep("zusammen", .$insgesamt), (grep("zusammen", .$insgesamt) + 16),
      ~ .x:.y
    ) %>% unlist())) %>%
    dplyr::slice(-c(37, 73, 110)) %>%
    dplyr::mutate(
      geschlecht_aggregat = dplyr::case_when(
        insgesamt %in% c("Insgesamt", "Weiblich") ~ insgesamt,
        TRUE ~ as.character(NA)
      )
    ) %>%
    dplyr::mutate(personal = dplyr::case_when(
      insgesamt %in% c(
        "Wissenschaftliches und künstlerisches Personal",
        "Verwaltungs-, technisches und sonstiges Personal"
      ) ~ insgesamt,
      TRUE ~ as.character(NA)
    )) %>%
    dplyr::mutate(taetigkeit = dplyr::case_when(
      insgesamt %in% c("Hauptberuflich", "Nebenberuflich") ~ insgesamt,
      TRUE ~ as.character(NA)
    )) %>%
    tidyr::fill(geschlecht_aggregat, personal, taetigkeit) %>%
    dplyr::filter(!is.na(region)) %>%
    dplyr::mutate(jahr = as.numeric(sheet))

  return(hochschulpersonal_data)

}

hochschulpersonal <-
  get_all_hochschulpersonal_data(
    filename = system.file(
      package = "datalab",
      "data-raw/DES051_HP_n_BL_FG_ab1998_bzw_ab2005_mitW.xlsx"
    )
  )

usethis::use_data(hochschulpersonal, overwrite = TRUE)
