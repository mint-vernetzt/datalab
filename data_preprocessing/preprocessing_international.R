
### internationale Schuldaten ####

# Erstellt schule_pisa ----

# kab
# Aug23

##### Alle ####

# file_path <- paste0("C:/Users/", akro, "/OneDrive - Stifterverband/AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten")
#
# path_kek <- "C:/Users/kab/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/"
#
# file_path <- path_kek

dat_pisa_g <- read_xls(paste0(pfad, "PISA003_Länderscores_Immigration_Books.xls"), sheet = 3)

sub_cindex <- which(stringr::str_detect(dat_pisa_g[,everything(dat_pisa_g)], "mathematics|science"))
sub_pisa_g <- dat_pisa_g[,sub_cindex]
sub_pisa_g <- sub_pisa_g %>%
  stringr::str_extract(., "mathematics|science")

coln_cindex <- which(stringr::str_detect(dat_pisa_g [,everything(dat_pisa_g )], "Year/Study"))

coln_rindex <- which(dat_pisa_g[,coln_cindex]=="Year/Study")

dat_pisa_g1 <- dat_pisa_g%>%
  slice((coln_rindex-1):nrow(.))

coln_annex <- dat_pisa_g1%>%
  slice(1)%>%
  as.vector()%>%
  unname()%>%
  unlist()%>%
  zoo::na.locf(na.rm = F)

colnames(dat_pisa_g1) <-paste0(dat_pisa_g1[2,], "_", coln_annex)
colnames(dat_pisa_g1) <- gsub("_NA", "", colnames(dat_pisa_g1))

dat_pisa_g2<- dat_pisa_g1[-c(1:2),]

dat_pisa_g3 <- dat_pisa_g2 %>%
  mutate(across(`Year/Study`, ~ zoo::na.locf(.)))

dat_pisa_g4 <- dat_pisa_g3 %>%
  tidyr::pivot_longer(-c("Jurisdiction", "Year/Study"),
                      names_to ="platzhalter", values_to = "wert")%>%
  tidyr::separate_wider_delim(platzhalter, delim="_", names=c("typ","indikator"))%>%
  mutate(fach=sub_pisa_g)%>%
  mutate(fach=case_when(fach=="mathematics" ~ "Mathematik",
                        fach== "science" ~ "Naturwissenschaften",
                        T ~ .$fach)) %>%
  rename(jahr = "Year/Study", land = Jurisdiction)

# dat_pisa_g4 <- dat_pisa_g3 %>%
#   tidyr::pivot_longer(c("Average_All students", "Standard Error_All students"),
#                       names_to ="platzhalter", values_to = "wert")%>%
#   tidyr::separate_wider_delim(platzhalter, delim="_", names=c("typ","indikator"))%>%
#   mutate(fach=sub_pisa_g)
#
#
# dat_pisa_g4$Jurisdiction <- countrycode::countrycode(dat_pisa_g4$Jurisdiction, origin = 'country.name',destination = 'country.name', custom_match = c("International Average (OECD)" = "OECD Durchschnitt"))


pisa_extract <- function(pisa_list_dat, pisa_list_sheeet) {

  # setting path

  dat_pisa_g <- read_xls(paste0(pfad,pisa_list_dat ), sheet = pisa_list_sheeet)

  # fach auslesen
  sub_cindex <- which(stringr::str_detect(dat_pisa_g[,everything(dat_pisa_g)], "mathematics|science"))
  sub_pisa_g <- dat_pisa_g[,sub_cindex]
  sub_pisa_g <- sub_pisa_g %>%
    stringr::str_extract(., "mathematics|science")

  coln_cindex <- which(stringr::str_detect(dat_pisa_g [,everything(dat_pisa_g )], "Year/Study"))

  coln_rindex <- which(dat_pisa_g[,coln_cindex]=="Year/Study")

  dat_pisa_g1 <- dat_pisa_g%>%
    slice((coln_rindex-1):nrow(.))

  coln_annex <- dat_pisa_g1%>%
    slice(1)%>%
    as.vector()%>%
    unname()%>%
    unlist()%>%
    zoo::na.locf(na.rm = F)

  colnames(dat_pisa_g1) <-paste0(dat_pisa_g1[2,], "_", coln_annex)
  colnames(dat_pisa_g1) <- gsub("_NA", "", colnames(dat_pisa_g1))

  dat_pisa_g2<- dat_pisa_g1[-c(1:2),]

  dat_pisa_g3 <- dat_pisa_g2 %>%
    mutate(across(`Year/Study`, ~ zoo::na.locf(.)))
  # %>%
  #   mutate(across(-c(`Year/Study`, `Jurisdiction`),~ stringr::str_remove(., "\\footnotesize")))

  # Hier versuchen Zeug rauszunehmen ^

  dat_pisa_g4 <- dat_pisa_g3 %>%
    tidyr::pivot_longer(-c("Jurisdiction", "Year/Study"),
                        names_to ="platzhalter", values_to = "wert")%>%
    tidyr::separate_wider_delim(platzhalter, delim="_", names=c("typ","indikator"))%>%
    mutate(fach=sub_pisa_g)%>%
    mutate(fach=case_when(fach=="mathematics" ~ "Mathematik",
                          fach== "science" ~ "Naturwissenschaften",
                          T ~ .$fach)) %>%
    rename(jahr = "Year/Study", land = Jurisdiction)


  dat_pisa_g4$land <- countrycode::countrycode(dat_pisa_g4$land, origin = 'country.name',destination = 'country.name.de', custom_match = c("International Average (OECD)" = "OECD Durchschnitt",
                                                                                                                                           "Selected countries and jurisdictions" = "Ausgewählte Regionen"))

  dat_pisa_g4$source <- pisa_list_dat

  dat_pisa_g5 <- dat_pisa_g4 %>%
    filter(jahr %in% c("2000", "2003", "2006", "2009", "2012", "2015","2018", "2022"))

  return(dat_pisa_g5)

}

# Hier Dateiname pro rlevantem Excel Arbeitsblatt einfügen
pisa_list_dat <- c("PISA001_Länderscore_Insgesamt_Gender.xls",
                   "PISA001_Länderscore_Insgesamt_Gender.xls",
                   "PISA001_Länderscore_Insgesamt_Gender.xls",
                   "PISA001_Länderscore_Insgesamt_Gender.xls",
                   "PISA002_Länderscores_Edu_Parents.xls",
                   "PISA002_Länderscores_Edu_Parents.xls",
                   "PISA003_Länderscores_Immigration_Books.xls",
                   "PISA003_Länderscores_Immigration_Books.xls",
                   "PISA003_Länderscores_Immigration_Books.xls",
                   "PISA003_Länderscores_Immigration_Books.xls",
                   "PISA004_Länderscores_Income_Computer.xls",
                   "PISA004_Länderscores_Income_Computer.xls",
                   "PISA004_Länderscores_Income_Computer.xls",
                   "PISA004_Länderscores_Income_Computer.xls",
                   "PISA005_Länderscore_Insgesamt_Gender_22.xls",
                   "PISA005_Länderscore_Insgesamt_Gender_22.xls",
                   "PISA005_Länderscore_Insgesamt_Gender_22.xls",
                   "PISA005_Länderscore_Insgesamt_Gender_22.xls",
                   "PISA008_Länderscores_Immigration_Books_22.xls",
                   "PISA008_Länderscores_Immigration_Books_22.xls",
                   "PISA008_Länderscores_Immigration_Books_22.xls",
                   "PISA008_Länderscores_Immigration_Books_22.xls"
)

# Dazu die Indizes der relevanten Sheets einfügen
pisa_list_sheeet <- c(3,4,5,6,2,3,3,4,5,6,3,4,5,6,
                      1,2,3,4, # PISA005_Länderscore_Insgesamt_Gender_22.xls
                      1,2,3,4 # PISA007_Länderscores_Immigration_Books_22.xls
)



pisa_list_output <- purrr::map2(.x = pisa_list_dat, .y =pisa_list_sheeet, .f=pisa_extract )

pisa_list_output <- purrr::list_rbind(pisa_list_output)

pisa_list_output1 <- pisa_list_output %>%
  filter(!wert %in% c("†"))%>%
  mutate(wert = as.numeric(wert),
         typ= case_when(typ == "Average" ~ "Druchschnitt",
                        typ =="Standard Error" ~ "Standardfehler",
                        T ~ .$typ))%>%
  mutate(ordnung = case_when(
    indikator %in% c("101-200 books" , "0-10 books" ,
                     "11-25 books" , "201-500 books" , "26-100 books",
                     "More than 500 books", "None", "1-10 books", "11-25 books",
                     "26-100 books", "101-200 books", "201-500 books", "There are no books.") ~ "Bücher im Haushalt",
    indikator %in% c( "Less than [$A]",
                      "[$A] or more but less than [$B]",
                      "[$B] or more but less than [$C]",
                      "[$C] or more but less than [$D]",
                      "[$D] or more but less than [$E]",
                      "[$E] or more") ~ "Haushaltseinkommen",
    indikator %in% c("All students", "Female", "Male")~ "Ländermittel",
    indikator %in% c("First-Generation", "Second-Generation", "Native") ~ "Migrationshintergrund",
    indikator %in% c("ISCED 1", "ISCED 3A, ISCED 4", "ISCED 5A, 6",
                     "ISCED 2", "ISCED 3B, C", "ISCED 5B") ~ "Bildungshintergrund",
    indikator %in% c("Yes", "No") ~ "Computerverfügbarkeit"
  ))

pisa_list_output2 <- pisa_list_output1%>%
  select(-source)

pisa1 <- pisa_list_output2 %>%
  filter(ordnung == "Ländermittel")%>%
  rename(geschlecht = indikator, indikator = ordnung)%>%
  mutate(geschlecht = case_when(geschlecht=="All students" ~ "Insgesamt",
                                geschlecht=="Female" ~ "Weiblich",
                                geschlecht=="Male" ~ "Männlich"))%>%
  mutate(ausprägung = geschlecht)

pisa1d <- pisa1 %>%
  janitor::get_dupes(-wert)

pisa2  <- pisa_list_output2 %>%
  filter(ordnung != "Ländermittel")%>%
  mutate(geschlecht = "Insgesamt")%>%
  rename(ausprägung = indikator, indikator = ordnung)%>%
  mutate(ausprägung = case_when( ausprägung =="0-10 books" ~ "0-10",
                                 ausprägung =="101-200 books" ~ "101-200",
                                 ausprägung =="11-25 books" ~ "11-25",
                                 ausprägung =="201-500 books" ~ "201-500",
                                 ausprägung =="26-100 books" ~ "26-100",
                                 ausprägung =="First-Generation" ~ "Erste Generation",
                                 ausprägung =="ISCED 1" ~ "Primarbereich" ,
                                 ausprägung =="ISCED 2" ~  "Sekundarbereich I",
                                 ausprägung =="ISCED 3A, ISCED 4" ~ "Sekundarbereich II (Allgemeinbildend), Postsekundäre Bildung",
                                 ausprägung =="ISCED 3B, C" ~ "Sekundarbereich II (Berufsgebunden), Sekundarbereich II (Nicht Weiterführend)",
                                 ausprägung =="ISCED 5A, 6"~ "Tertiärbereich (Erste Stufe, außer Praxisgebunden), Tertiärbereich (Forschungsqualifikation)",
                                 ausprägung =="ISCED 5B" ~ "Teriärbereich (Erste Stufe, Praxisgebunden)",
                                 ausprägung =="More than 500 books" ~ "Mehr als 500",
                                 ausprägung =="Native" ~ "Keiner",
                                 ausprägung =="Yes" ~ "Verfügbar",
                                 ausprägung =="No" ~ "Nicht Verfügbar",
                                 ausprägung =="Second-Generation" ~ "Zweite Generation",
                                 ausprägung =="None" ~ "Keine",
                                 ausprägung =="or more" ~ "Mehr",
                                 T ~ .$ausprägung))


pisa <- bind_rows(pisa1, pisa2)%>%
  mutate(alter_n = "15 Jahre")%>%
  rename(bereich = indikator, indikator = ausprägung)%>%
  select(-geschlecht)%>%
  pivot_wider(names_from = typ, values_from = wert )%>%
  rename(wert = Druchschnitt)%>%
  mutate(typ = "Durchschnitt")%>%
  filter(land !="Ausgewählte Regionen" &
           !indikator %in% c("Computerverfügbarkeit",
                             "Bildungshintergrund",
                             "Haushaltseinkommen"))



usethis::use_data(pisa, overwrite = T)


# Erstellt schule_timss ----

# kab
# Sep 23

##### TIMSS Acheivement----

##### Gender ----
#file_path <- paste0("C:/Users/", akro, "/OneDrive - Stifterverband/AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten")
pfad <- paste0("C:/Users/tko/OneDrive - Stifterverband/2_MINT-Lücke schließen/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/")
akro <- "kbr"
pfad <- paste0("C:/Users/", akro,
               "/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/")


# funktion
library(readxl)
library(stringr)

timss_gender_transform <- function(dat){

  timss_g <- read_xlsx(paste0(pfad, dat))

  timss_g_sub<- colnames(timss_g)[1]

  timss_g_sub1<- stringr::str_extract(timss_g_sub, "Mathematics|Science")

  timss_g1<- timss_g %>%
    slice(1:which(timss_g[1]==	"Benchmarking Participants")-1)%>%
    select(-c(1,2,4))

  timss_g_colanmes <- timss_g1%>%
    slice(which(timss_g1[1]==	"Country"))%>%
    as.vector()%>%
    unlist()%>%
    unname()%>%
    zoo::na.locf()

  timss_g_colanmes2<-timss_g1%>%
    filter(if_any(everything(), ~ .=="Girls"))%>%
    as.vector()%>%
    unlist()%>%
    unname()%>%
    zoo::na.locf(na.rm = F)

  timss_g_colanmes3 <-timss_g1%>%
    filter(if_any(everything(), ~ .=="Girls"))%>%
    mutate(across(everything(), ~case_when(
      is.na(.)~"Signifikanz",
    )))%>%
    as.vector()%>%
    unlist()%>%
    unname()


  k <- paste0(timss_g_colanmes, "_", timss_g_colanmes2, "_", timss_g_colanmes3)

  k <- str_remove_all(k, "_NA")

  colnames(timss_g1) <- k

  colnames(timss_g1)[1] <- "land"

  #genauer fehler:
  timss_g1 <- timss_g1 %>%
    select(-matches("^1995_Boys_Signifikanz$"))

  timss_g2 <- timss_g1%>%
    slice(which(if_any(everything(), ~. == "Girls"))+1 : nrow(.))%>%
    pivot_longer(-"land", values_to = "dummy", names_to = "indikator")%>%
    separate(indikator, c ("jahr", "geschlecht", "indikator"), sep = "_")%>%
    mutate(indikator = case_when(is.na(indikator)~ "Achievement",
                                 T~.$indikator))%>%
    rename(wert = dummy )%>%
    mutate(indikator= case_when(
      indikator == "Achievement" ~ "Score",
      T~.$indikator
    ))

  timss_g2$fach <- timss_g_sub1

  timss_g3 <- timss_g2 %>%
    rename(typ = indikator, indikator = geschlecht)%>%
    mutate(fach = case_when(
      fach == "Mathematics"~ "Mathematik",
      fach == "Science" ~ "Naturwissenschaften"),
      indikator=case_when(indikator == "Boys" ~ "Jungen",
                          indikator == "Girls" ~ "Mädchen",
                          T ~ .$indikator))%>%
    pivot_wider(names_from = typ, values_from = wert)%>%
    rename("Test-Punktzahl" = Score, signifikant = Signifikanz)%>%
    pivot_longer(`Test-Punktzahl`, names_to = "typ", values_to = "wert")%>%
    mutate(signifikant= case_when(signifikant == "p" ~ "Ja",
                                  T ~ NA),
           ordnung = "Gender")%>%
    rename("signifikant höher gegenüber anderem geschlecht" = signifikant)



  timss_g3$land <- countrycode::countrycode(timss_g3$land, origin = 'country.name',
                                            destination = 'country.name.de',
                                            custom_match = c('England' = 'England',
                                                             "Northern Ireland" = "Nordirland",
                                                             "Ontario, Canada" = "Ontario, Kanada",
                                                             "Quebec, Canada" = "Quebec, Kanada",
                                                             "Abu Dhabi, UAE" = "Abu Dhabi, UAE",
                                                             "Dubai, UAE" = "Dubai, UAE",
                                                             "Belgium (Flemish)" = "Belgien (Flemisch)",
                                                             "Belgium (French)" = "Belgien (Französisch)",
                                                             "International Average" = "Interantionaler Durchschnitt"))
  return(timss_g3)
}

#l <- timss_gender_transform("TIMSS002_achievement-gender-trends-M4.xlsx")
#k <- timss_gender_transform("TIMSS005_achievement-gender-trends-S4.xlsx")

m23 <- timss_gender_transform("TIMSS009_ach-g4m-trend-gender.xlsx")
s23 <- timss_gender_transform("TIMSS010_sam.xlsx")

#timss_gender_old <- bind_rows(l,k)
timmss_gender <- bind_rows(m23, s23)

#### Achievement ----

# funktion

timss_achv_extract<-function(dat){
  dat1 <- read_excel(paste0(pfad, dat))

  timss_a_sub<- colnames(dat1)[1]

  timss_a_sub1<- stringr::str_extract(timss_a_sub, "Mathematics|Science")

  timss_a1<- dat1 %>%
    slice(1:which(dat1[1]==	"Benchmarking Participants")-1)%>%
    janitor::remove_empty("cols")%>%
    select(-1)

  timss_a_colnames1 <- timss_a1 %>%
    slice(which(if_any(everything(), ~ .=="Country")))%>%
    as.vector()%>%
    unlist()%>%
    unname()%>%
    zoo::na.locf()


  timss_a_colnames2 <- timss_a1 %>%
    slice(which(if_any(everything(), ~ .=="Country")))%>%
    mutate(across(everything(), ~case_when(
      is.na(.)~"standardfehler"
    )))%>%
    as.vector()%>%
    unlist()%>%
    unname()

  timss_a_colnames_fn <- paste0(timss_a_colnames1, "_", timss_a_colnames2)%>%
    str_remove_all(., "_NA")

  colnames(timss_a1) <- timss_a_colnames_fn


  timss_a2 <- timss_a1 %>%
    filter(!is.na(Country))%>%
    filter(Country != "Country")%>%
    pivot_longer(-c(`Country`), names_to="indikator", values_to="dummy")%>%
    separate_wider_delim(indikator, delim = "_", names=c("jahr", "indikator"), too_few = "align_start")%>%
    mutate(indikator=case_when(is.na(indikator)~"wert",
                               T~.$indikator))%>%
    pivot_wider(names_from = indikator, values_from = dummy)%>%
    rename(land = Country)%>%
    mutate(indikator = "Score")

  timss_a2$land <- countrycode::countrycode(timss_a2$land, origin = 'country.name',
                                            destination = 'country.name.de',
                                            custom_match = c('England' = 'England',
                                                             "Northern Ireland" = "Nordirland",
                                                             "Ontario, Canada" = "Ontario, Kanada",
                                                             "Quebec, Canada" = "Quebec, Kanada",
                                                             "Abu Dhabi, UAE" = "Abu Dhabi, UAE",
                                                             "Dubai, UAE" = "Dubai, UAE",
                                                             "Belgium (Flemish)" = "Belgien (Flemisch)",
                                                             "Belgium (French)" = "Belgien (Französisch)",
                                                             "International Average" = "Interantionaler Durchschnitt"))

  timss_a2$fach <-  timss_a_sub1

  timss_a3 <- timss_a2 %>%
    mutate(fach = case_when(
      fach == "Mathematics" ~ "Mathematik",
      fach== "Science" ~ "Naturwissenschaften"
    ),
    indikator = "Insgesamt",
    typ= "Test-Punktzahl",
    ordnung = "Achievement")





  return(timss_a3)

}

j <- timss_achv_extract("TIMSS001_achievement-trends-M4.xlsx")
h <- timss_achv_extract("TIMSS004_achievement-trends-S4.xlsx")

timss_achievement <- bind_rows(j,h)


# addition of TIMSS 2023 data for it

achie_23 <- function(pfad, file, fach){

  dat1 <- read_excel(paste0(pfad, file))

  timss_a1<- dat1 %>%
    slice(1:which(dat1[1]==	"Benchmarking Participants")-1)%>%
    janitor::remove_empty("cols")%>%
    select(-1)


  timss_a_colnames1 <- timss_a1 %>%
    slice(which(if_any(everything(), ~ .=="Country")))%>%
    as.vector()%>%
    unlist()%>%
    unname()%>%
    zoo::na.locf()


  timss_a_colnames2 <- timss_a1 %>%
    slice(which(if_any(everything(), ~ .=="Country")))%>%
    mutate(across(everything(), ~case_when(
      is.na(.)~"standardfehler"
    )))%>%
    as.vector()%>%
    unlist()%>%
    unname()

  timss_a_colnames_fn <- paste0(timss_a_colnames1, "_", timss_a_colnames2)%>%
    str_remove_all(., "_NA")

  colnames(timss_a1) <- timss_a_colnames_fn

  extract <- timss_a1 %>% select(1:3)
  extract <- extract %>% slice(-c(1:4))
  extract <- extract %>% slice(-59)
  extract <- extract %>% mutate(jahr = 2023)
  extract <- extract %>% mutate(jahr = as.character(jahr))
  extract <- extract %>% mutate(fach = fach)
  extract <- extract %>% mutate(typ = "Test-Punktahl")
  extract <- extract %>% mutate(ordnung = "Achievement")
  extract <- extract %>% mutate(indikator = "Insgesamt")
  colnames(extract)[1] <- "land"
  colnames(extract)[2] <- "wert"
  extract <- extract %>% mutate(wert = as.double(wert))
  colnames(extract)[3] <- "standardfehler"

  extract$land <- countrycode::countrycode(extract$land, origin = 'country.name',
                                           destination = 'country.name.de',
                                           custom_match = c('England' = 'England',
                                                            "Northern Ireland" = "Nordirland",
                                                            "Ontario, Canada" = "Ontario, Kanada",
                                                            "Quebec, Canada" = "Quebec, Kanada",
                                                            "Abu Dhabi, UAE" = "Abu Dhabi, UAE",
                                                            "Dubai, UAE" = "Dubai, UAE",
                                                            "Belgium (Flemish)" = "Belgien (Flemisch)",
                                                            "Belgium (French)" = "Belgien (Französisch)",
                                                            "International Average" = "Interantionaler Durchschnitt"))

  return(extract)
}

w = achie_23(pfad, "TIMSS012_ach-g4m-dist.xlsx", "Mathematik")
u = achie_23(pfad, "TIMSS015_ach-g4s-dist.xlsx", "Naturwissenschaften")

dat2023a <- rbind(w,u)


timss_achievement <- bind_rows(timss_achievement, dat2023a)


#### Home resources ----


# funktion

timss_res_extract <- function(dat3, jahr, fach){

  #file_path <- paste0("C:/Users/", akro, "/OneDrive - Stifterverband/AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten")


  dat2 <- read_excel(paste0(pfad, dat3))

  # timss_res_sub<- colnames(dat2)[1]
  #
  # timss_res_sub<- stringr::str_extract(timss_res_sub, "Mathematics|Science")

  timss_res<- dat2 %>%
    slice(1:which(dat2[1]==	"Benchmarking Participants")-1)%>%
    select(-c(1,2,4))%>%
    janitor::remove_empty()

  timss_res_coln <- timss_res %>%
    filter(if_any(everything(),~ . =="Country"))%>%
    as.vector()%>%
    unlist()%>%
    unname()



  bem <- which(str_detect(timss_res_coln, "Country"))

  timss_res_coln[bem+1]<- "bemerkung"

  timss_res_coln1 <- zoo::na.locf(timss_res_coln)

  timss_res_coln2 <- timss_res %>%
    slice(which(if_any(everything(),~. =="Percent of Students")))%>%
    as.vector()%>%
    unlist()%>%
    unname()%>%
    zoo::na.locf(na.rm = F)

  timss_res_coln2 <- rev(timss_res_coln2)

  timss_res_coln1 <- rev(timss_res_coln1)

  timss_res_coln2[1:2] <- timss_res_coln1[1:2]

  timss_res_coln2 <- rev(timss_res_coln2)

  timss_res_coln1 <- rev(timss_res_coln1)


  timss_res_coln3 <- timss_res %>%
    slice(which(if_any(everything(),~. =="Percent of Students")))%>%
    mutate(across(-c("...3", "...5","...25"),~case_when(
      is.na(.)~"standardfehler"
    )))%>%
    as.vector()%>%
    unlist()%>%
    unname()


  timss_res_coln_fn <- paste0(timss_res_coln1,"_",timss_res_coln2, "_" ,timss_res_coln3)

  colnames(timss_res) <-timss_res_coln_fn

  timss_res2 <- timss_res %>%
    filter(!if_any(everything(), ~ .%in%c("Percent of Students", "Country")))

  colnames(timss_res2) <- str_remove(colnames(timss_res2), "_NA")
  colnames(timss_res2) <- str_remove(colnames(timss_res2), "_NA")

  timss_res2 <- timss_res2 %>%
    rename(land = Country)

  timss_res3 <- timss_res2 %>%
    pivot_longer(-c(land, bemerkung), values_to = "wert", names_to="indikator")%>%
    separate(indikator, sep="_", into=c("gruppe", "indikator", "typ"))%>%
    mutate(typ = case_when(
      is.na(typ) ~"wert",
      T~.$typ))%>%
    pivot_wider(names_from = typ, values_from = wert)%>%
    mutate("fach" = fach,
           "jahr" =jahr)%>%
    mutate(across(c("wert", "standardfehler"), ~ as.numeric(.)))%>%
    rename(typ = indikator, indikator = gruppe, fußnote_zu_n = bemerkung)%>%
    mutate(typ = case_when(typ == "Average Achievement" ~ "Gemittelte Test-Punktzahl",
                           typ == "Percent of Students" ~ "Prozentsatz der Schüler:innen",
                           T ~ .$typ),
           indikator = case_when(indikator =="Some Resources"~ "Einige Ressourcen",
                                 indikator =="Few Resources"~ "Wenige Ressourcen",
                                 indikator =="Many Resources"~ "Viele Ressourcen",
                                 T~ .$indikator),
           ordnung = "Ressourcen")

  timss_res3$land <- countrycode::countrycode(timss_res3$land, origin = 'country.name',
                                              destination = 'country.name.de',
                                              custom_match = c('England' = 'England',
                                                               "Northern Ireland" = "Nordirland",
                                                               "Ontario, Canada" = "Ontario, Kanada",
                                                               "Quebec, Canada" = "Quebec, Kanada",
                                                               "Abu Dhabi, UAE" = "Abu Dhabi, UAE",
                                                               "Dubai, UAE" = "Dubai, UAE",
                                                               "Belgium (Flemish)" = "Belgien (Flemisch)",
                                                               "Belgium (French)" = "Belgien (Französisch)",
                                                               "International Average" = "Interantionaler Durchschnitt"))

  return(timss_res3)
}

o <- timss_res_extract("TIMSS007_home-resources-M4.xlsx", "2019", "Mathematik")
p <- timss_res_extract("TIMSS008_home-resources-S4.xlsx" ,"2019", "Naturwissenschaften")
timss_res_dat_2 <- bind_rows(o,p)


timss_res_extract <- function(dat3, jahr, fach, sheet){

  #file_path <- paste0("C:/Users/", akro, "/OneDrive - Stifterverband/AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten")


  dat2 <- read_excel(paste0(pfad, dat3), sheet = sheet)

  dat2 <- cbind(dat2[1:3], ..4 = NA, dat2[4:ncol(dat2)])

  # Spaltennamen anpassen
  colnames(dat2) <- paste0("...", seq_len(ncol(dat2)))

  timss_res_sub<- colnames(dat2)[1]
  timss_res_sub<- stringr::str_extract(timss_res_sub, "Mathematics|Science")

  timss_res<- dat2 %>%
    slice(1:which(dat2[1]==	"Benchmarking Participants")-1)%>%
    select(-c(1,2,4))%>%
    janitor::remove_empty()

  timss_res_coln <- timss_res %>%
    filter(if_any(everything(),~ . =="Country"))%>%
    as.vector()%>%
    unlist()%>%
    unname()



  bem <- which(str_detect(timss_res_coln, "Country"))

  timss_res_coln[bem+1]<- "bemerkung"

  timss_res_coln1 <- zoo::na.locf(timss_res_coln)

  timss_res_coln2 <- timss_res %>%
    slice(which(if_any(everything(),~. =="Percent of Students")))%>%
    as.vector()%>%
    unlist()%>%
    unname()%>%
    zoo::na.locf(na.rm = F)

  timss_res_coln2 <- rev(timss_res_coln2)

  timss_res_coln1 <- rev(timss_res_coln1)

  timss_res_coln2[1:2] <- timss_res_coln1[1:2]

  timss_res_coln2 <- rev(timss_res_coln2)

  timss_res_coln1 <- rev(timss_res_coln1)


  timss_res_coln3 <- timss_res %>%
    slice(which(if_any(everything(),~. =="Percent of Students")))%>%
    mutate(across(-c("...3", "...5","...25"),~case_when(
      is.na(.)~"standardfehler"
    )))%>%
    as.vector()%>%
    unlist()%>%
    unname()


  timss_res_coln_fn <- paste0(timss_res_coln1,"_",timss_res_coln2, "_" ,timss_res_coln3)

  colnames(timss_res) <-timss_res_coln_fn

  timss_res2 <- timss_res %>%
    filter(!if_any(everything(), ~ .%in%c("Percent of Students", "Country")))

  colnames(timss_res2) <- str_remove(colnames(timss_res2), "_NA")
  colnames(timss_res2) <- str_remove(colnames(timss_res2), "_NA")

  timss_res2 <- timss_res2 %>%
    rename(land = Country)

  timss_res3 <- timss_res2 %>%
    pivot_longer(-c(land, bemerkung), values_to = "wert", names_to="indikator")%>%
    separate(indikator, sep="_", into=c("gruppe", "indikator", "typ"))%>%
    mutate(typ = case_when(
      is.na(typ) ~"wert",
      T~.$typ))%>%
    pivot_wider(names_from = typ, values_from = wert)%>%
    mutate("fach" = fach,
           "jahr" =jahr)%>%
    mutate(across(c("wert", "standardfehler"), ~ as.numeric(.)))%>%
    rename(typ = indikator, indikator = gruppe, fußnote_zu_n = bemerkung)%>%
    mutate(typ = case_when(typ == "Average Achievement" ~ "Gemittelte Test-Punktzahl",
                           typ == "Percent of Students" ~ "Prozentsatz der Schüler:innen",
                           T ~ .$typ),
           indikator = case_when(indikator =="Some Resources"~ "Einige Ressourcen",
                                 indikator =="Few Resources"~ "Wenige Ressourcen",
                                 indikator =="Many Resources"~ "Viele Ressourcen",
                                 T~ .$indikator),
           ordnung = "Ressourcen")

  timss_res3$land <- countrycode::countrycode(timss_res3$land, origin = 'country.name',
                                              destination = 'country.name.de',
                                              custom_match = c('England' = 'England',
                                                               "Northern Ireland" = "Nordirland",
                                                               "Ontario, Canada" = "Ontario, Kanada",
                                                               "Quebec, Canada" = "Quebec, Kanada",
                                                               "Abu Dhabi, UAE" = "Abu Dhabi, UAE",
                                                               "Dubai, UAE" = "Dubai, UAE",
                                                               "Belgium (Flemish)" = "Belgien (Flemisch)",
                                                               "Belgium (French)" = "Belgien (Französisch)",
                                                               "International Average" = "Interantionaler Durchschnitt"))

  return(timss_res3)
}

o_neu <- timss_res_extract("TIMSS011_con-hom-ses.xlsx", "2023", "Mathematik", sheet = 2)
p_neu <- timss_res_extract("TIMSS011_con-hom-ses.xlsx", "2023", "Naturwissenschaften", sheet = 3)

timss_res_dat <- bind_rows(o_neu,p_neu)

timss_res_dat$indikator[timss_res_dat$indikator == "Higher \r\nSocioeconomic Status"] <- "Viele Ressourcen"
timss_res_dat$indikator[timss_res_dat$indikator == "Middle \r\nSocioeconomic Status"] <- "Einige Ressourcen"
timss_res_dat$indikator[timss_res_dat$indikator == "Lower\r\n Socioeconomic Status"] <- "Wenige Ressourcen"

timss_res_dat <- rbind(timss_res_dat_2, timss_res_dat)


#### Benchmarks-----

# funktion

timss_benchmarks_extract <- function(dat7){
  #file_path <- paste0("C:/Users/", akro, "/OneDrive - Stifterverband/AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten")


  dat8 <- read_excel(paste0(pfad, dat7))

  timss_b_sub<- colnames(dat8)[1]

  timss_b_sub1<- stringr::str_extract(timss_b_sub, "Mathematics|Science")

  dat_bench <- dat8 %>%
    slice(1:which(if_any(everything(),~ str_detect(.,"Benchmarking Participants")))-1)%>%
    janitor::remove_empty("cols")

  dat_bench_coln_1 <- dat_bench %>%
    filter(if_any(everything(), ~.== "Country"))%>%
    as.vector()%>%
    unlist()%>%
    unname()%>%
    zoo::na.locf()

  dat_bench_coln_2 <- dat_bench %>%
    filter(if_any(everything(), ~ str_detect(., "Percent")))%>%
    as.vector()%>%
    unlist()%>%
    unname()%>%
    zoo::na.locf(na.rm = F)

  dat_bench_coln_3 <- dat_bench %>%
    filter(if_any(everything(), ~ str_detect(., "2019")))%>%
    as.vector()%>%
    unlist()%>%
    unname()%>%
    zoo::na.locf(na.rm = F)

  dat_bench_coln_4 <- dat_bench %>%
    filter(if_any(everything(), ~ str_detect(., "2019")))%>%
    mutate(across(everything(), ~ case_when(
      is.na(.) ~ "signifikanz"
    )))%>%
    as.vector()%>%
    unlist()%>%
    unname()

  dat_bench_coln_4[1] <- NA


  dat_bench_coln_fn <- paste0(dat_bench_coln_1,"_", dat_bench_coln_2,"_", dat_bench_coln_3,"_", dat_bench_coln_4 )

  dat_bench_coln_fn <- str_remove_all(dat_bench_coln_fn, "_NA")

  colnames(dat_bench) <- dat_bench_coln_fn

  dat_bench1a <- dat_bench %>%
    slice(which(if_any(everything(),~ str_detect(., "2019")))+1:nrow(.))%>%
    select(Country, contains("signifikanz"))%>%
    pivot_longer(-Country, names_to="indikator", values_to = "signifikanz")%>%
    separate(indikator, into = c("indikator", "typ", "jahr"), sep="_")

  dat_bench1b <- dat_bench %>%
    slice(which(if_any(everything(),~ str_detect(., "2019")))+1:nrow(.))%>%
    select(Country, !contains("signifikanz"))%>%
    mutate(across(-c("Country"), ~ as.numeric(.)))%>%
    pivot_longer(-Country, names_to="indikator", values_to = "wert")%>%
    separate(indikator, into = c("indikator", "typ", "jahr" ), sep="_")

  dat_bench2 <- dat_bench1b %>%
    left_join(dat_bench1a) %>%
    rename(land = Country, signifikant = signifikanz)%>%
    mutate(typ = case_when(typ=="Percent of Students"~ "Prozentsatz der Schüler:innen",
                           T ~ .$typ),
           ordnung = "Benchmarks",
           bereich = "Schule",
           signifikant = case_when(signifikant == "p"~ "Höher",
                                   signifikant == "s" ~ "Niedriger",
                                   T ~ .$signifikant),
           indikator= case_when(indikator == "Advanced \r\nInternational Benchmark \r\n(625)" ~ "Höchster int'l. Maßstab (625)",
                                indikator == "High \r\nInternational Benchmark\r\n(550)" ~ "Hoher int'l. Maßstab (550)",
                                indikator == "Intermediate \r\nInternational Benchmark \r\n(475)" ~ "Mittlerer int'l. Maßstab (475)",
                                indikator == "Low \r\nInternational Benchmark \r\n(400)" ~ "Niedriger int'l. Maßstab (400)",
                                T ~ .$indikator))%>%
    rename( "aktuell_signifikant" = signifikant)%>%
    mutate(fach =case_when(timss_b_sub1 == "Mathematics"~ "Mathematik",
                           timss_b_sub1 == "Science"~ "Naturwissenschaften"))

  dat_bench2$land <- countrycode::countrycode(dat_bench2$land,
                                              origin = 'country.name',
                                              destination = 'country.name.de',
                                              custom_match = c('England' = 'England',
                                                               "Northern Ireland" = "Nordirland",
                                                               "Ontario, Canada" = "Ontario, Kanada",
                                                               "Quebec, Canada" = "Quebec, Kanada",
                                                               "Abu Dhabi, UAE" = "Abu Dhabi, UAE",
                                                               "Dubai, UAE" = "Dubai, UAE",
                                                               "Belgium (Flemish)" = "Belgien (Flemisch)",
                                                               "Belgium (French)" = "Belgien (Französisch)",
                                                               "International Average" = "Interantionaler Durchschnitt"))


  return(dat_bench2)
}


x <- timss_benchmarks_extract("TIMSS003_benchmarks-trends-M4.xlsx")
y <- timss_benchmarks_extract("TIMSS006_benchmarks-trends-S4.xlsx")

# y1 <- y %>%
#   select(1:4)%>%
#   unique()
#
# x1 <- x %>%
#   select(1:4)%>%
#   unique()
#
# d <- setdiff(x1,y1)

# Südafika ist neu dazugekommen; SA und Norw. nur 5 Landesteile

timss_benchmarks <- bind_rows(x,y)



new_benchmark <- function(pfad, file, fach){

  dat8 <- read_excel(paste0(pfad, file))

  timss_b_sub<- colnames(dat8)[1]

  timss_b_sub1<- stringr::str_extract(timss_b_sub, "Mathematics|Science")

  dat8 <- dat8[,c("...3", "...5", "...6", "...7", "...8")]


  colnames(dat8) <- dat8[6, ]
  dat8 <- dat8[6:62,]
  dat8 <- dat8[-1, ]
  colnames(dat8)[1] <- "land"

  dat8_long <- dat8 %>%
    pivot_longer(
      cols = starts_with("Advanced (625)"):ends_with("Low \r\n(400)"),
      names_to = "indikator",
      values_to = "wert"
    )

  dat8_long$typ <- "Prozentsatz der Schüler:innen"
  dat8_long$aktuell_signifikant <- NA #diese info fehlt in dem neuen datensatz
  dat8_long$ordnung <- "Benchmarks"
  dat8_long$bereich <- "Schule"
  dat8_long$fach <- fach
  dat8_long$jahr <- 2023

  dat8_long$indikator[dat8_long$indikator == "Advanced (625)"] <- "Höchster int'l. Maßstab (625)"
  dat8_long$indikator[dat8_long$indikator == "High \r\n(550)"] <- "Hoher int'l. Maßstab (550)"
  dat8_long$indikator[dat8_long$indikator == "Intermediate (475)"] <- "Mittlerer int'l. Maßstab (475)"
  dat8_long$indikator[dat8_long$indikator == "Low \r\n(400)" ] <- "Niedriger int'l. Maßstab (400)"

  indikator_reihenfolge <- unique(timss_benchmarks$indikator)
  dat8_long <- dat8_long %>%
    mutate(indikator = factor(indikator, levels = indikator_reihenfolge)) %>%
    arrange(indikator)


  dat8_long$land <- countrycode::countrycode(dat8_long$land,
                                             origin = 'country.name',
                                             destination = 'country.name.de',
                                             custom_match = c('England' = 'England',
                                                              "Northern Ireland" = "Nordirland",
                                                              "Ontario, Canada" = "Ontario, Kanada",
                                                              "Quebec, Canada" = "Quebec, Kanada",
                                                              "Abu Dhabi, UAE" = "Abu Dhabi, UAE",
                                                              "Dubai, UAE" = "Dubai, UAE",
                                                              "Belgium (Flemish)" = "Belgien (Flemisch)",
                                                              "Belgium (French)" = "Belgien (Französisch)",
                                                              "International Average" = "Interantionaler Durchschnitt"))

  dat8_long <- dat8_long %>%
    filter(!is.na(land))

}

a <- new_benchmark(pfad, "TIMSS013_ach-g4m-bmk-trend.xlsx", "Mathematics")
b <- new_benchmark(pfad, "TIMSS014_ach-g4s-bmk-trend.xlsx", "Naturwissenschaften")

benchmark_temp <- rbind(a, b)


timss_benchmarks <- rbind(timss_benchmarks, benchmark_temp)



###scienza

# dat8 <- read_excel(paste0(pfad, "TIMSS014_ach-g4s-bmk-trend.xlsx"))
#
# timss_b_sub<- colnames(dat8)[1]
#
# timss_b_sub1<- stringr::str_extract(timss_b_sub, "Mathematics|Science")
#
# dat8 <- dat8[,c("...3", "...5", "...6", "...7", "...8")]
#
#
# colnames(dat8) <- dat8[6, ]
# dat8 <- dat8[6:62,]
# dat8 <- dat8[-1, ]
# colnames(dat8)[1] <- "land"
#
# dat8_long <- dat8 %>%
#   pivot_longer(
#     cols = starts_with("Advanced (625)"):ends_with("Low \r\n(400)"),
#     names_to = "indikator",
#     values_to = "wert"
#   )
#
# dat8_long$typ <- "Prozentsatz der Schüler:innen"
# dat8_long$aktuell_signifikant <- NA #diese info fehlt in dem neuen datensatz
# dat8_long$ordnung <- "Benchmarks"
# dat8_long$bereich <- "Schule"
# dat8_long$fach <- "Naturwissenschaften"
# dat8_long$jahr <- 2023
#
# dat8_long$indikator[dat8_long$indikator == "Advanced (625)"] <- "Höchster int'l. Maßstab (625)"
# dat8_long$indikator[dat8_long$indikator == "High \r\n(550)"] <- "Hoher int'l. Maßstab (550)"
# dat8_long$indikator[dat8_long$indikator == "Intermediate (475)"] <- "Mittlerer int'l. Maßstab (475)"
# dat8_long$indikator[dat8_long$indikator == "Low \r\n(400)" ] <- "Niedriger int'l. Maßstab (400)"
#
# indikator_reihenfolge <- unique(timss_benchmarks$indikator)
# dat8_long <- dat8_long %>%
#   mutate(indikator = factor(indikator, levels = indikator_reihenfolge)) %>%
#   arrange(indikator)
#
#
# dat8_long$land <- countrycode::countrycode(dat8_long$land,
#                                            origin = 'country.name',
#                                            destination = 'country.name.de',
#                                            custom_match = c('England' = 'England',
#                                                             "Northern Ireland" = "Nordirland",
#                                                             "Ontario, Canada" = "Ontario, Kanada",
#                                                             "Quebec, Canada" = "Quebec, Kanada",
#                                                             "Abu Dhabi, UAE" = "Abu Dhabi, UAE",
#                                                             "Dubai, UAE" = "Dubai, UAE"))
#
# dat8_long <- dat8_long %>%
#   filter(!is.na(land))
#
#
# timss_benchmarks <- rbind(timss_benchmarks, dat8_long)


#### TIMSS Merger ----

timss <- bind_rows(
  timss_benchmarks %>% mutate(across(wert,~ as.numeric(.))),
  timss_res_dat %>% mutate(across(wert,~ as.numeric(.))),
  timss_achievement %>% mutate(across(wert,~ as.numeric(.))),
  timmss_gender %>% mutate(across(wert,~ as.numeric(.)))
)

# timss <- bind_rows(
#   timss_benchmarks %>% mutate(across(wert,~ as.numeric(.))),
#   timss_res_dat %>% mutate(across(wert,~ as.numeric(.))),
#   timss_achievement %>% mutate(across(wert,~ as.numeric(.))),
#   timss_gender %>% mutate(across(wert,~ as.numeric(.)))
# )


timss$fach <- replace(timss$fach, timss$fach == "Mathematics", "Mathematik")
timss$jahr <- as.numeric(timss$jahr)

#usethis::use_data(timss, overwrite=T)

save(timss, file = "schule_timss.rda")

# Internationale Studierendendaten ----

# Erstellt


