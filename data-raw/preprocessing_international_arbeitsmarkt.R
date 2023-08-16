################################################################################
#
# Data Lab
# Vorbereitung Datensatz: Arbeitsmarkt international
# Author: Katharina Brunner, August 2023
#
################################################################################


# Rohdaten einlesen -------------------------------------------------------

data <- read.csv("data-raw/raw/OCED001_employment_per_field.csv", header = TRUE,
                 sep = ",", dec = ".")


# Datensatz in passende Form bringen --------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, googleLanguageR)

data <- data %>% dplyr::filter(Measure == "Value") #SE ausfiltern

data <- data %>%
  dplyr::select(Country, ISCED.2011.A.education.level, Gender, Age, Field,
                  INDICATOR.1, Reference.year, Value) %>%
  dplyr::rename(land = Country,
                anforderung = ISCED.2011.A.education.level,
                geschlecht = Gender,
                ag = Age,
                fachbereich = Field,
                indikator = INDICATOR.1,
                jahr = Reference.year,
                wert = Value)

# ins Deutsche übersetzten - als Fkt für weitere intern. Datensätze - ausbauen
übersetzen <- function(df){
  df <- df %>%
  dplyr::mutate(
    land = dplyr::case_when(
      land == "Australia" ~ "Australien",
      land == "Austria" ~ "Östereich",
      land == "Belgium" ~ "Belgien",
      land == "Czech Republic" ~ "Tschechien",
      land == "Denmark" ~ "Dänemark",
      land == "Estonia" ~ "Estland",
      land == "European Union 23 members in OECD" ~ "OECD-Mitglieder aus der EU",
      land == "Finland" ~ "Finnland",
      land == "France" ~ "Frankreich",
      land == "Germany" ~ "Deutschland",
      land == "Greece" ~ "Griechenland",
      land == "Hungary" ~ "Ungarn",
      land == "Iceland" ~ "Island",
      land == "Italy" ~ "Italien",
      land == "Latvia" ~ "Lettland",
      land == "Lithuania" ~ "Litauen",
      land == "Luxembourg" ~ "Luxenburg",
      land == "Mexico" ~ "Mexiko",
      land == "Netherlands" ~ "Niederlande",
      land == "Norway" ~ "Norwegen",
      land == "OECD - Average" ~ "OECD",
      land == "Poland" ~ "Polen",
      land == "Slovak Republic" ~ "Slovakei",
      land == "Slovenia" ~ "Slowenien",
      land == "Spain" ~ "Spanien",
      land == "Sweden" ~ "Schweden",
      land == "Switzerland" ~ "Schweiz",
      land == "Türkiye" ~ "Türkei",
      land == "United Kingdom" ~ "Vereinigtes Königreich (UK)",
      land == "United States" ~ "USA",
      T ~ land
    ),
    anforderung = dplyr::case_when(
      anforderung == "Short-cycle tertiary education" ~ "kurzzeitige terziäre Bildung",
      anforderung == "Tertiary education" ~ "gesamt",
      anforderung == "Master’s, Doctoral or equivalent education" ~ "Master, Doktor",
      anforderung == "Bachelor’s or equivalent education" ~ "Bachelor",
      T ~ anforderung
    ),
    geschlecht = dplyr::case_when(
      geschlecht == "Men" ~ "Männer",
      geschlecht == "Women" ~ "Frauen",
      geschlecht == "Total" ~ "gesamt",
      T ~ geschlecht
    ),
    indikator = dplyr::case_when(
      indikator == "Employment rate" ~ "Beschäftigungsquote",
      indikator == "Inactivity rate" ~ "Erwerbslosenquote",
      indikator == "Share of population by field of study" ~ "Anteil an Bevölkerung nach Studienbereich",
      T ~ indikator
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
      fachbereich == "Social sciences, journalism and information" ~ "Sozialwissenschaften, Journalismus und Informationswissenschaften"

    )
  )
}
data <- übersetzen(data)

# Filtern - welche fachbereiche machen Sinn - da müssten Zwischensummen mit drin sein



