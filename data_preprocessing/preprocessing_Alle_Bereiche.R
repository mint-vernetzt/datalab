library(magrittr)
library(dplyr)
library(DBI)


# daten laden
setwd("~/datalab")
con <- DBI::dbConnect(duckdb::duckdb(), "data/mint_db.duckdb")
kurse <- DBI::dbGetQuery(con,
                        "SELECT * FROM kurse")
studierende_detailliert <- DBI::dbGetQuery(con,
                                           "SELECT * FROM studierende_detailliert")
arbeitsmarkt_detail <- DBI::dbGetQuery(con,
                                       "SELECT * FROM arbeitsmarkt_detail")
#
# Schule
data_schule <-kurse %>%
  select(bereich, indikator, anzeige_geschlecht, fachbereich, jahr, region, wert)%>%
  #rename(jahr=Jahr)%>%
  mutate(across(jahr, ~ as.numeric(.)))%>%
  rename(geschlecht=anzeige_geschlecht) %>%
  filter(fachbereich %in% c("Alle Fächer", "MINT", "andere Fächer"),
         indikator == "Leistungskurse")
duplikate <- data_schule %>%
  janitor::get_dupes()
data_schule$fachbereich[data_schule$fachbereich == "Alle Fächer"] <- "Alle"
data_schule$fachbereich[data_schule$fachbereich == "andere Fächer"] <- "Nicht MINT"

# Arbeitsmarkt
data_arbeitsmarkt <- arbeitsmarkt_detail %>%
  filter(anforderung=="Gesamt") %>%
  mutate(across(wert, ~ as.numeric(.)))%>%
  #rename(jahr=Jahr)%>%
  mutate(across(jahr, ~ as.numeric(.)))%>%
  filter(landkreis == "alle Landkreise",
         fachbereich %in% c("Alle", "MINT"),
         indikator %in% c("Beschäftigte", "Auszubildende")) %>%
  rename(region = bundesland) %>%
  select(bereich, indikator, geschlecht, fachbereich, jahr, region, wert)
alle <- data_arbeitsmarkt %>%
  filter(fachbereich == "Alle") %>%
  rename(wert_ges = wert) %>%
  select(-c(bereich, fachbereich))
nm <- data_arbeitsmarkt %>%
  left_join(alle, by = c("indikator", "geschlecht", "jahr", "region")) %>%
  mutate(wert = wert_ges - wert) %>%
  select(-wert_ges) %>%
  filter(fachbereich != "Alle")
nm$fachbereich <- "Nicht MINT"
data_arbeitsmarkt <- rbind(data_arbeitsmarkt, nm)

# Studierende
data_studierende <- studierende_detailliert%>%
  filter(indikator=="Studierende",
         fachbereich %in% c("MINT", "Nicht MINT", "Gesamt")) %>%
  mutate(across(wert, ~ as.numeric(.)))%>%
  mutate(across(jahr, ~ as.numeric(.)))%>%
  mutate(bereich = "Hochschule") %>%
  select(bereich, indikator, geschlecht, fachbereich, jahr, region, wert)
duplikate <- data_studierende%>%
  janitor::get_dupes(-wert)
data_studierende$fachbereich[data_studierende$fachbereich == "Gesamt"] <- "Alle"

zentral <- bind_rows(data_schule,  data_arbeitsmarkt,  data_studierende)

setwd("C:/Users/kbr/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/02_data/data/")
save(zentral, file = "zentral.rda")

setwd("~/datalab2")

library(DBI)

dbWriteTable(con, 'zentral', zentral, overwrite = TRUE, append = FALSE)

dbDisconnect(con, shutdown = TRUE)
