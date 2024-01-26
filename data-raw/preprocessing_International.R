library(dplyr)
require(stringr)

# Studierende
#studierende_europa
#studierende_anzahl_oecd
#studierende_absolventen_weltweit
#studierende_mobil_eu_absolut

# Arbeitsmarkt
# arbeitsmarkt_anfaenger_absolv_oecd
# arbeitsmarkt_beschaeftigte_eu


# Studierende Europa

studi_eu <- studierende_europa %>%
  filter(fach %in% c("Alle MINT-Fächer",
                      "Informatik & Kommunikationstechnologie",
                      "Naturwissenschaften, Mathematik und Statistik",
                      "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe")
         )%>%
  mutate(gruppe = paste(indikator, geschlecht),
         gruppe = case_when(
           gruppe == "Fächerwahl Frauen" ~ "MINT-Studierende unter Frauen",
           gruppe == "Fächerwahl Männer" ~ "MINT-Studierende unter Männern",
           gruppe == "Fächerwahl Gesamt" ~ "MINT-Studierende",
           gruppe == "Frauen-/Männeranteil Frauen" ~ "weilbiche MINT-Studierende",
           gruppe == "Frauen-/Männeranteil Männer" ~ "männliche MINT-Studierende"
         ),
         fach = case_when(
           fach == "Alle MINT-Fächer" ~ "MINT",
           T ~ fach
         ),
         population = case_when(
           T ~ "Europa"
         ),
         quelle = case_when(
           T ~ "Eurostat, 2023"
         ),
         hinweis = case_when(
           gruppe == "MINT-Studierende unter Frauen" ~ "Anteil von weiblichen Studierenden in MINT bzw. in dem MINT-Fach an allen weiblichen Studierenden.",
           gruppe == "MINT-Studierende unter Männern" ~ "Anteil von männlicher Studierenden in MINT bzw. in dem MINT-Fach an allen männlichen Studierenden.",
           gruppe == "MINT-Studierende" ~ "Anteil von Studierenden in MINT bzw. in dem MINT-Fach an allen Studierenden.",
           gruppe == "weilbiche MINT-Studierende" ~ "Frauenanteil in MINT bzw. in dem MINT-Fach.",
           gruppe == "männliche MINT-Studierende" ~ "Männeranteil in MINT bzw. in dem MINT-Fach."
         )
         ) %>%
  na.omit() %>%
  rename(wert_prozent = wert,
         region = population) %>%
  select(-c(ebene, mint_select, fachbereich, indikator, geschlecht, typ))

studi_eu$wert_absolut <- NA

studi_eu <- studi_eu[,c("region", "land", "bereich", "fach", "gruppe","jahr",
                        "wert_prozent", "wert_absolut", "hinweis", "quelle")]


# Studierende OECD
studi_oecd <- studierende_anzahl_oecd %>%
  filter(ebene == 1,
         anforderung != "kurzes tertiäres Bildungsprogramm (allgemeinbildend)") %>%
  dplyr::group_by(fach, geschlecht, jahr, land) %>%
  dplyr::filter(dplyr::n_distinct(anforderung) == 3) %>%
  dplyr::ungroup() %>%
  group_by(bereich, geschlecht, land, jahr, fach, quelle, population) %>%
  summarise(wert = sum(wert)) %>%
  ungroup()

alle <- studi_oecd %>%
  filter(fach == "Alle") %>%
  rename(total = wert) %>%
  select(-c("fach"))

studi_oecd <- left_join(studi_oecd, alle,
                        by = join_by(bereich, quelle,
                                    geschlecht, population,land, jahr),
                        relationship = "many-to-many")


studi_oecd <- studi_oecd %>%
  filter(fach %in% c("MINT",
                     "Informatik & Kommunikationstechnologie",
                     "Naturwissenschaften, Mathematik und Statistik",
                     "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe")
  )%>%
  mutate(wert_prozent = round(wert/total*100,1),
        gruppe = case_when(
          geschlecht == "Frauen" ~ "MINT-Studierende unter Frauen",
          geschlecht == "Männer" ~ "MINT-Studierende unter Männern",
          geschlecht == "Gesamt" ~ "MINT-Studierende",
        ),
         population = case_when(
           T ~ "OECD Länder"
         ),
         quelle = case_when(
           T ~ "OECD, 2023"
         )
  ) %>%
  na.omit() %>%
  rename(region = population,
         wert_absolut = wert) %>%
  select(-total)

mint_ges <- studi_oecd %>%
  filter(geschlecht == "Gesamt") %>%
  rename(ges_total = wert_absolut) %>%
  select(ges_total, land, jahr, fach)

studi_oecd2 <- left_join(studi_oecd, mint_ges,
                         by = join_by(land, jahr, fach),
                        relationship = "many-to-many")

studi_oecd2 <- studi_oecd2 %>%
  mutate(wert_prozent = round(wert_absolut/ges_total*100,1),
         gruppe = case_when(
           geschlecht == "Frauen" ~ "weibliche MINT-Studierende",
           geschlecht == "Männer" ~ "männliche MINT-Studierende"
         )) %>%
  na.omit() %>%
  select(-geschlecht, -ges_total)

studi_oecd <- studi_oecd %>% select(-geschlecht)

studi_oecd <- rbind(studi_oecd, studi_oecd2)

studi_oecd <- studi_oecd %>%
  mutate(hinweis = case_when(
    gruppe == "MINT-Studierende unter Frauen" ~ "Anteil von weiblichen Studierenden in MINT bzw. in dem MINT-Fach an allen weiblichen Studierenden.",
    gruppe == "MINT-Studierende unter Männern" ~ "Anteil von männlicher Studierenden in MINT bzw. in dem MINT-Fach an allen männlichen Studierenden.",
    gruppe == "MINT-Studierende" ~ "Anteil von Studierenden in MINT bzw. in dem MINT-Fach an allen Studierenden.",
    gruppe == "weilbiche MINT-Studierende" ~ "Frauenanteil in MINT bzw. in dem MINT-Fach.",
    gruppe == "männliche MINT-Studierende" ~ "Männeranteil in MINT bzw. in dem MINT-Fach."
  ))

studi_oecd <- studi_oecd[,c("region", "land", "bereich", "fach", "gruppe","jahr",
                        "wert_prozent", "wert_absolut", "hinweis", "quelle")]


# arbeitsmarkt_anfaenger_absolv_oecd -----

arb_aao1 <- arbeitsmarkt_anfaenger_absolv_oecd %>%
  filter(fachbereich %in% c( "Informatik & Kommunikationstechnologie",
                             "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                             "MINT",
                             "Naturwissenschaften, Mathematik und Statistik"
                             ),
         anforderung %in% c("Erstausbildung (ISCED 35)",
                            "Ausbildung (ISCED 45)"
                            )) %>%
  rename(fach = fachbereich, wert_prozent = wert)%>%
  mutate(wert_absolut = NA,
         region = "OECD Länder",
         quelle = "OECD, 2023")%>%
  mutate(gruppe = case_when(
    #
    # männer u männer
    variable  == "Anteil Absolvent*innen nach Fach an allen Fächern" & geschlecht == "Gesamt"
    ~ paste0("MINT-Absolvent*innen " , "(", .$anforderung,")" ), #Annteil MINT
    variable  == "Anteil Absolvent*innen nach Fach an allen Fächern" & geschlecht == "Männer"
    ~ paste0("MINT-Absolvent*innen unter Männern " , "(", .$anforderung,")" ),
    variable  == "Anteil Absolvent*innen nach Fach an allen Fächern" & geschlecht == "Frauen"
    ~ paste0("MINT-Absolvent*innen unter Frauen " , "(", .$anforderung,")" ),
    #
    # frauen u frauen
    variable  == "Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern" & geschlecht == "Gesamt"
    ~ paste0("MINT-Ausbildungs-/Studiumsanfänger*innen " , "(", .$anforderung,")" ),
    variable  == "Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern" & geschlecht == "Männer"
    ~ paste0("MINT-Ausbildungs-/Studiumsanfänger*innen unter Männern " , "(", .$anforderung,")" ),
    variable  == "Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern" & geschlecht == "Frauen"
    ~ paste0("MINT-Ausbildungs-/Studiumsanfänger*innen unter Frauen " , "(", .$anforderung,")" ),
    #
    # männer frauen gegenwert
    variable  == "Frauen-/Männeranteil Absolvent*innen nach Fachbereichen" & geschlecht == "Männer"
    ~ paste0("männliche Absolvent*innen " , "(", .$anforderung,")" ),
    variable  == "Frauen-/Männeranteil Absolvent*innen nach Fachbereichen" & geschlecht == "Frauen"
    ~ paste0("weibliche Absolvent*innen " , "(", .$anforderung,")" ),
    #
    # männer frauen gegenwert
    variable == "Frauen-/Männeranteil Ausbildungs-/Studiumsanfänger*innen nach Fachbereichen" & geschlecht == "Männer"
    ~ paste0("männliche Ausbildungs-/Studiumsanfänger*innen " , "(", .$anforderung,")" ),
    variable  == "Frauen-/Männeranteil Ausbildungs-/Studiumsanfänger*innen nach Fachbereichen" & geschlecht == "Frauen"
    ~ paste0("weibliche Ausbildungs-/Studiumsanfänger*innen " , "(", .$anforderung,")" )))%>%
  select(-c(typ, population, anforderung, geschlecht ))


arb_aao2 <- arb_aao1 %>%
  mutate(hinweis = case_when(
    #
    startsWith(.$gruppe, "MINT-Absolvent*innen") ~ "Anteil aller Absolvent*innen in MINT bzw. Fach and allen Absolvent*innen",
    startsWith(.$gruppe, "MINT-Absolvent*innen unter Männern" ) ~ "Anteil aller Absolvent*innen in MINT bzw. Fach and allen männlichen Absolvent*innen",
    startsWith(.$gruppe, "MINT-Absolvent*innen unter Frauen" ) ~ "Anteil aller Absolvent*innen in MINT bzw. Fach and allen weiblichen Absolvent*innen",
    #
    startsWith(.$gruppe, "MINT-Ausbildungs-/Studiumsanfänger*innen") ~ "Anteil aller Ausbildungs-/Studiumsanfänger*innen in MINT bzw. Fach and allen Ausbildungs-/Studiumsanfänger*innenn",
    startsWith(.$gruppe, "MINT-Ausbildungs-/Studiumsanfänger*innen unter Männern" ) ~ "Anteil aller Ausbildungs-/Studiumsanfänger*innen in MINT bzw. Fach and allen männlichen Ausbildungs-/Studiumsanfänger*innenn",
    startsWith(.$gruppe, "MINT-Ausbildungs-/Studiumsanfänger*innen unter Frauen" ) ~ "Anteil aller Ausbildungs-/Studiumsanfänger*innen in MINT bzw. Fach and allen weiblichen Ausbildungs-/Studiumsanfänger*innenn",
    #
    startsWith(.$gruppe, "Männliche MINT-Absolvent*innen" ) ~ "Anteil männlicher Absolvent*innen an allen Absolvent*innen in MINT bzw. Fach",
    startsWith(.$gruppe, "Weibliche MINT-Absolvent*innen" ) ~ "Anteil weiblicher Absolvent*innen an allen Absolvent*innen in MINT bzw. Fach",
    #
    startsWith(.$gruppe, "Männliche MINT-Ausbildungs-/Studiumsanfänger*innen") ~ "Anteil männlicher Ausbildungs-/Studiumsanfänger*innen an allen Ausbildungs-/Studiumsanfänger*innen in MINT bzw. Fach",
    startsWith(.$gruppe, "Weibliche MINT-Ausbildungs-/Studiumsanfänger*innen" ) ~ "Anteil weiblicher Ausbildungs-/Studiumsanfänger*innen an allen Ausbildungs-/Studiumsanfänger*innen in MINT bzw. Fach"
  ))%>%
  select(-variable)



# abspeichern
international_zentral <- rbind(studi_eu, studi_oecd, arb_aao2)

usethis::use_data(international_zentral, overwrite = T)

