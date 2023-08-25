# Aug 23
# kab

# Funktion zur Aufbereitung der ISCED F 13 Schlüssel 


iscedf13_transform <- function(data_in) {
  
  # spalte mit den codes muss bereits fach heißen 
  # spalte mit werten muss wert heißen 
  
  require(magrittr)
  require(dplyr)
  
  # fächer benennen
  
  dat1 <- data_in%>%
    mutate(fach = case_when(str_ends("F00", .$fach)~ "Allgemeine Bildungsgänge und Qualifikationen",
                            str_ends("F01", .$fach)~ "Pädagogik",
                            str_ends("F02", .$fach) ~ "Geisteswissenschaften und Künste",
                            str_ends("F03", .$fach) ~ "Sozialwissenschaften, Journalismus und Informationswesen",
                            str_ends("F04", .$fach) ~ "Wirtschaft, Verwaltung und Recht",
                            str_ends("F05", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik",
                            str_ends("F06", .$fach) ~ "Informatik & Kommunikationstechnologie",
                            str_ends("F07", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                            str_ends("F08", .$fach) ~ "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin",
                            str_ends("F09", .$fach) ~ "Gesundheit, Medizin und Sozialwesen",
                            str_ends("F10", .$fach) ~ "Dienstleistungen",
                            str_ends("F050", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht näher definiert",
                            str_ends("F051", .$fach) ~ "Biologie und verwandte Wissenschaften",
                            str_ends("F052", .$fach) ~ "Umwelt",
                            str_ends("F053", .$fach) ~ "Exakte Naturwissenschaften",
                            str_ends("F054", .$fach) ~ "Mathematik und Statistik",
                            str_ends("F058", .$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Naturwissenschaften,
Mathematik und Statistik",
                            str_ends("F059", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht andernorts klassifiziert",
                            str_ends("F070", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht näher definiert",
                            str_ends("F071", .$fach) ~ "Ingenieurwesen und Technische Berufe",
                            str_ends("F072", .$fach) ~ "Verarbeitendes Gewerbe und Bergbau",
                            str_ends("F073", .$fach) ~ "Architektur und Baugewerbe",
                            str_ends("F078", .$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Ingenieurwesen,
 verarbeitendes Gewerbe und Baugewerbe",
                            str_ends("F079", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht andernorts klassifiziert",
                            str_detect("TOTAL", .$fach) ~ "Insgesamt",
                            str_ends("UNK", .$fach) ~ "Unbekannt"))
  
  
  # aggregate erstellen
  
  
  ## hier noch pivot_longer verbesseren
  
  dat2 <- dat1 %>%
    pivot_wider(names_from = fach, values_from = wert)%>%
    mutate("Weitere Ingenieurwesen, verarbeitendes Gewerbe, Baugewerbe" = `Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht andernorts klassifiziert`+
             `Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht näher definiert`,
           "Weitere Naturwissenschaften, Mathematik und Statistik"= `Naturwissenschaften, Mathematik und Statistik nicht andernorts klassifiziert`+
             `Naturwissenschaften, Mathematik und Statistik nicht näher definiert`)%>%
    mutate("Alle MINT-Fächer" =rowSums( select(.,c(`Naturwissenschaften, Mathematik und Statistik`,
                                                   `Informatik & Kommunikationstechnologie`,
                                                   `Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe`,
                                                   `Weitere Ingenieurwesen, verarbeitendes Gewerbe, Baugewerbe`,
                                                   `Weitere Naturwissenschaften, Mathematik und Statistik`,
                                                   `Biologie und verwandte Wissenschaften`,
                                                   `Umwelt`,
                                                   `Exakte Naturwissenschaften`,
                                                   `Mathematik und Statistik`,
                                                   `Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Naturwissenschaften,\nMathematik und Statistik`, `Ingenieurwesen und Technische Berufe`,
                                                   `Verarbeitendes Gewerbe und Bergbau`,
                                                   `Architektur und Baugewerbe`,
                                                   `Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Ingenieurwesen,\n verarbeitendes Gewerbe und Baugewerbe`)),na.rm = T))%>%
    mutate("Alle Nicht MINT-Fächer" = Insgesamt - `Alle MINT-Fächer`)%>%
    select(-c("Naturwissenschaften, Mathematik und Statistik nicht näher definiert",
              "Naturwissenschaften, Mathematik und Statistik nicht andernorts klassifiziert",
              "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht näher definiert",
              "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht andernorts klassifiziert"))%>%
    pivot_longer(c("Allgemeine Bildungsgänge und Qualifikationen",
                   "Pädagogik",
                   "Geisteswissenschaften und Künste",
                   "Sozialwissenschaften, Journalismus und Informationswesen",
                   "Wirtschaft, Verwaltung und Recht",
                   "Naturwissenschaften, Mathematik und Statistik",
                   "Informatik & Kommunikationstechnologie",
                   "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                   "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin",
                   "Gesundheit, Medizin und Sozialwesen",
                   "Dienstleistungen",
                   "Insgesamt",
                   "Unbekannt",
                   "Alle MINT-Fächer",
                   "Alle Nicht MINT-Fächer",
                   "Architektur und Baugewerbe",
                   "Biologie und verwandte Wissenschaften",
                   "Exakte Naturwissenschaften",
                   "Ingenieurwesen und Technische Berufe","Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Ingenieurwesen,\n verarbeitendes Gewerbe und Baugewerbe",
                   "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Naturwissenschaften,\nMathematik und Statistik",
                   "Mathematik und Statistik",
                   "Umwelt",
                   "Verarbeitendes Gewerbe und Bergbau",
                   "Weitere Ingenieurwesen, verarbeitendes Gewerbe, Baugewerbe",
                   "Weitere Naturwissenschaften, Mathematik und Statistik"
                   ), values_to = "wert", names_to="fach")
  
  
  # ebenen und mint-selektor
  
  
  dat3 <- dat2 %>%
  mutate(mint_select= case_when(fach %in% c("Naturwissenschaften, Mathematik und Statistik",
                                            "Informatik & Kommunikationstechnologie",
                                            "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                            "Weitere Ingenieurwesen, verarbeitendes Gewerbe, Baugewerbe",
                                            "Weitere Naturwissenschaften, Mathematik und Statistik",
                                            "Biologie und verwandte Wissenschaften",
                                            "Umwelt",
                                            "Exakte Naturwissenschaften",
                                            "Mathematik und Statistik",
                                            "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Naturwissenschaften,
Mathematik und Statistik","Ingenieurwesen und Technische Berufe",
                                            "Verarbeitendes Gewerbe und Bergbau",
                                            "Architektur und Baugewerbe",
                                            "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Ingenieurwesen,
 verarbeitendes Gewerbe und Baugewerbe")~ "mint",
                                T ~ "nicht mint"
  ))%>%
    mutate(ebene= case_when(fach %in% c("Allgemeine Bildungsgänge und Qualifikationen",
                                        "Pädagogik",
                                        "Geisteswissenschaften und Künste",
                                        "Sozialwissenschaften, Journalismus und Informationswesen",
                                        "Wirtschaft, Verwaltung und Recht",
                                        "Naturwissenschaften, Mathematik und Statistik",
                                        "Informatik & Kommunikationstechnologie",
                                        "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                        "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin",
                                        "Gesundheit, Medizin und Sozialwesen",
                                        "Dienstleistungen",
                                        "Insgesamt",
                                        "Unbekannt",
                                        "Alle MINT-Fächer",
                                        "Alle Nicht MINT-Fächer")~ "1",
                            T~"2"))%>%
    
    
  # fachbereich und fach diffenrenzieren 
    
  dat4 <- dat3 %>%
    mutate(fachbereich = case_when(fach %in% c("Allgemeine Bildungsgänge und Qualifikationen",
                                               "Pädagogik",
                                               "Geisteswissenschaften und Künste",
                                               "Sozialwissenschaften, Journalismus und Informationswesen",
                                               "Wirtschaft, Verwaltung und Recht",
                                               "Naturwissenschaften, Mathematik und Statistik",
                                               "Informatik & Kommunikationstechnologie",
                                               "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                               "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin",
                                               "Gesundheit, Medizin und Sozialwesen",
                                               "Dienstleistungen",
                                               "Insgesamt",
                                               "Unbekannt",
                                               "Alle MINT-Fächer",
                                               "Alle Nicht MINT-Fächer")~.$fach,
                                   fach == "Architektur und Baugewerbe" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                   fach == "Biologie und verwandte Wissenschaften" ~ "Naturwissenschaften, Mathematik und Statistik",
                                   fach == "Exakte Naturwissenschaften" ~ "Naturwissenschaften, Mathematik und Statistik",
                                   fach == "Ingenieurwesen und Technische Berufe" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                   fach == "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Ingenieurwesen,\n verarbeitendes Gewerbe und Baugewerbe" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                   fach == "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Naturwissenschaften,\nMathematik und Statistik" ~ "Naturwissenschaften, Mathematik und Statistik",
                                   fach == "Mathematik und Statistik" ~ "Naturwissenschaften, Mathematik und Statistik",
                                   fach == "Umwelt" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                   fach == "Verarbeitendes Gewerbe und Bergbau" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                   fach == "Weitere Ingenieurwesen, verarbeitendes Gewerbe, Baugewerbe" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                   fach == "Weitere Naturwissenschaften, Mathematik und Statistik" ~ "Naturwissenschaften, Mathematik und Statistik"))
           
    
    
           
           
                            
  
}