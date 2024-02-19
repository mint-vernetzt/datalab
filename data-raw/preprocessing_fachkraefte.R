
# Pfade und Packages ------------------------------------------------------

# akro <- "kbr"
# pfad <- paste0("C:/Users/", akro,
#                "/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/")

library(dplyr)

# Daten einlesen ----------------------------------------------------------
szenario <- c("Basis", "Bildung hoch", "Bildung niedrig", "Frauen plus Bildung Basis",
              "Frauen plus Bildung hoch", "Migration null", "Migration niedrig",
              "Migration hoch", "Beteiligung Ältere hoch",
              "Maximalszenario", "Minimalszenario")

fkd <- readxl::read_excel(paste0(pfad,"001_Datenblätter Szenarien MINT Beschäftigung.xlsx"), sheet = 1, col_names = TRUE)
fkd$szenario <- szenario[1]

for(i in 2:11){
  temp <- readxl::read_excel(paste0(pfad,"001_Datenblätter Szenarien MINT Beschäftigung.xlsx"), sheet = i, col_names = TRUE)
  temp$szenario <- szenario[i]
  temp <- temp[c(-1, -2),]
  fkd <- rbind(fkd, temp)
  }


colnames <- c("jahr",
              "männer deutsch expert",
              "männer deutsch sepzia",
              "männer deutsch fachkr",
              "männer ausl expert",
              "männer ausl sepzia",
              "männer ausl fachkr",
              "frauen deutsch expert",
              "frauen deutsch sepzia",
              "frauen deutsch fachkr",
              "frauen ausl expert",
              "frauen ausl sepzia",
              "frauen ausl fachkr",
              "gesamt",
              "szenario")

colnames(fkd) <- colnames
fkd <- fkd[c(-1, -2),]

fkd <- fkd %>%
  dplyr::mutate_at(vars("jahr":"gesamt"), as.numeric,
                   vars("szenario"), as.character)


# Daten umstrukturieren ---------------------------------------------------

fkd <- tidyr::pivot_longer(fkd, cols = "männer deutsch expert":"gesamt",
                         values_to = "wert", names_to = "wirkungshebel")

# umbennenen
fkd <- fkd %>%
  dplyr::mutate(
    indikator = dplyr::case_when(
      szenario == "Basis" ~ "Status-quo",
      szenario == "Bildung hoch" ~ "Verbesserung",
      szenario == "Bildung niedrig" ~ "Verschlechterung",
      szenario == "Frauen plus Bildung Basis" ~ "Verbesserung",
      szenario == "Frauen plus Bildung hoch" ~ "starke Verbesserung",
      szenario == "Migration null" ~ "Stillstand",
      szenario == "Migration niedrig" ~"Verschlechterung",
      szenario == "Migration hoch" ~"Verbesserung",
      grepl("Ältere", szenario) ~ "Verbesserung",
      szenario == "Maximalszenario" ~ "Verbesserung",
      szenario == "Minimalszenario" ~ "Verschlechterung",
      T ~ NA
  ),
    anforderung = dplyr::case_when(
      grepl("expert", wirkungshebel) ~ "Expert:innen",
      grepl("sepzia", wirkungshebel) ~ "Spezialist:innen",
      grepl("fachkr", wirkungshebel) ~ "Fachkräfte",
      wirkungshebel == "gesamt" ~ "Gesamt",
      T ~ NA
    ),
    geschlecht = dplyr::case_when(
      grepl("frauen", wirkungshebel) ~ "Frauen",
      grepl("männer", wirkungshebel) ~ "Männer",
      wirkungshebel == "gesamt" ~ "Gesamt",
      T ~ NA
  ),
    nationalitaet = dplyr::case_when(
      grepl("ausl", wirkungshebel) ~ "Keine deutsche Staatsangehörigkeit",
      grepl("deutsch", wirkungshebel) ~ "deutsche Staatsangehörigkeit",
      wirkungshebel == "gesamt" ~ "Gesamt",
      T ~ NA
    ),
    wirkhebel = dplyr::case_when(
      szenario == "Basis" ~ "Basis-Szenario",
      szenario %in% c("Bildung hoch", "Bildung niedrig") ~ "MINT-Bildung",
      grepl("Fraue", szenario) ~ "Frauen in MINT",
      grepl("Migrat", szenario) ~ "Internationale MINT-Fachkräfte",
      grepl("Ältere", szenario) ~ "Beteiligung älterer MINT-Fachkräfte",
      szenario %in% c("Maximalszenario", "Minimalszenario") ~ "Gesamteffekt",
      T ~ NA
    )
  ) %>%
  dplyr::select(-szenario, -wirkungshebel)

fkd <- fkd %>%
  dplyr::filter(nationalitaet != "Gesamt",
                geschlecht != "Gesamt",
                anforderung != "Gesamt")

# nationalität gesamt berechnen
nat_ges <- fkd %>%
  dplyr::filter(nationalitaet != "Gesamt") %>%
  dplyr::group_by(indikator, anforderung, geschlecht, wirkhebel, jahr) %>%
  dplyr::summarise(wert = sum(wert)) %>%
  dplyr::mutate(nationalitaet = "Gesamt") %>%
  dplyr::ungroup()

fkd <- rbind(fkd, nat_ges)

# geschlecht gesamt berechnen
gen_ges <- fkd %>%
  dplyr::filter(geschlecht != "Gesamt") %>%
  dplyr::group_by(indikator, anforderung, nationalitaet, wirkhebel, jahr) %>%
  dplyr::summarise(wert = sum(wert)) %>%
  dplyr::mutate(geschlecht = "Gesamt") %>%
  dplyr::ungroup()

fkd <- rbind(fkd, gen_ges)

# anforderung gesamt berechnen
anf_ges <- fkd %>%
  dplyr::filter(anforderung != "Gesamt") %>%
  dplyr::group_by(indikator, geschlecht, nationalitaet, wirkhebel, jahr) %>%
  dplyr::summarise(wert = sum(wert)) %>%
  dplyr::mutate(anforderung = "Gesamt") %>%
  dplyr::ungroup()

fkd <- rbind(fkd, anf_ges)


fkd <- fkd[,c("wirkhebel", "indikator", "geschlecht", "nationalitaet", "anforderung", "jahr", "wert")]

fachkraefte_prognose <- fkd
usethis::use_data(fachkraefte_prognose, overwrite = T)

#save(fachkraefte_prognose, file = "C:/Users/kbr/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/03_Backup_aus_RProjekt/data/fachkraefte_prognose.rda")
