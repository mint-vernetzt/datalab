################################################################################
#
# Data Lab
# Funktion übersetzten für internationale Daten
# Author: Katharina Brunner, August 2023
#
################################################################################

# bis jetzt nur für OECD1
# ins Deutsche übersetzten - als Fkt für weitere intern. Datensätze - ausbauen
übersetzen <- function(df){
  df <- df %>%
  dplyr::mutate(

    geschlecht = dplyr::case_when(
      geschlecht == "Men" ~ "Männer",
      geschlecht == "Women" ~ "Frauen",
      geschlecht == "Total" ~ "Gesamt",
      T ~ geschlecht
    ),

    fachbereich = dplyr::case_when(
      fachbereich == "Agriculture, forestry,fisheries and veterinary" ~
        "Landwirtschaft, Forstwirtschaft, Fischerei und Veterinärwesen",
      fachbereich == "Arts" ~ "Kunst",
      fachbereich == "Arts and humanities" ~ "Kunst und Geisteswissenschaften",
      fachbereich == "Arts, humanities, social sciences, journalism and information" ~
        "Kunst, Geisteswissenschaften, Sozialwissenschaften, Journalismus und Informationswissenschaft",
      fachbereich == "Business and administration" ~ "Wirtschaft und Verwaltung",
      fachbereich == "Business, administration and law " ~ "Wirtschaft, Verwaltung und Recht",
      fachbereich == "Education" ~ "Bildungswesen",
      fachbereich == "Engineering, manufacturing & construction" ~ "Ingenieurwesen, Fertigung und Bauwesen",
      fachbereich == "Generic programmes and qualifications" ~ "Allgemeine Programme und Qualifikationen",
      fachbereich == "Health (Medical & Dental)" ~ "Human- und Zahnmedizin",
      fachbereich == "Health (Nursing and associate health fields)" ~ "Krankenpflege und Gesundheitsbereich",
      fachbereich == "Health and welfare" ~ "Gesundheit und Wohlfahrt",
      fachbereich == "Humanities (except languages)" ~ "Geisteswissenschaften (außer Sprachen)",
      fachbereich == "Humanities (except languages) & Social Sciences, Journalism and information" ~
        "Geisteswissenschaften (außer Sprachen) & Sozialwissenschaften, Journalismus und Informationswissenschaften",
      fachbereich == "Information communication technologies (ICTs)" ~ "Informations- und Kommunikationstechnologien",
      fachbereich == "Law" ~ "Recht",
      fachbereich == "Natural sciences, mathematics and statistics" ~ " Naturwissenschaften, Mathematik und Statistik",
      fachbereich == "Other fields (F00, F08, F10)" ~ "Sonstige Bereiche",
      fachbereich == "Science, technology, engineering, and mathematics (STEM)" ~
        "Naturwissenschaft, Technologie, Ingenieurwesen und Mathematik (MINT)",
      fachbereich == "Services" ~ "Dienstleistungen",
      fachbereich == "Social sciences, journalism and information" ~ "Sozialwissenschaften, Journalismus und Informationswissenschaften",
      T ~ fachbereich

    )
  )

  if(!(is.null(df$indikator))){
    df <- df %>% dplyr::mutate(
      indikator = dplyr::case_when(
        indikator == "Employment rate" ~ "Beschäftigungsquote",
        indikator == "Inactivity rate" ~ "Nichterwerbsquote",
        indikator == "Unemployment rate" ~ "Erwerbslosenquote",
        indikator == "Share of population by field of study" ~ "Anteil an Bevölkerung nach Studienbereich",
        T ~ indikator
      )
    )
  }

}
