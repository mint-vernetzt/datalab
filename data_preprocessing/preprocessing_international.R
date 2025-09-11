
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

### Eurostat ------

#### Erstellt arbeitsmarkt_beschaeftigte_eu ------------

##### Rohdaten einlesen -----
# akro <- "kbr"

pfad <- "C:/Users/tko/OneDrive - Stifterverband/2_MINT-Lücke schließen/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/"



dat <- read.csv(paste0(pfad, "EUROSTAT006_hrst_st_nsecsex2__custom_17955862_linear_2_0.csv"),
                header = TRUE, sep = ",", dec = ".")




dat <- dat %>%
  dplyr::select(category, sex, geo, TIME_PERIOD, unit, OBS_VALUE, age, nace_r2) %>%
  dplyr::rename(land = geo,
                indikator = category,
                jahr = TIME_PERIOD,
                variable = unit,
                geschlecht = sex,
                wert = OBS_VALUE,
                ) %>%
  dplyr::filter(
    nace_r2 == "C",
    age == "Y15-74"
  )

# Land zuweisen / übersetzen
dat$land <- countrycode::countrycode(dat$land, origin = "eurostat", destination = "country.name.de")

# Indikator & Einheit zuweisen
dat <- dat %>%
  dplyr::mutate(
    indikator = dplyr::case_when(
      indikator ==  "HRST" ~ "Ausgebildete und/oder Beschäftigte",
      indikator ==  "HRSTE" ~ "Ausgebildete",
      indikator ==  "HRSTO" ~ "Beschäftigte",
      indikator ==  "HRSTC" ~ "Ausgebildet und Beschäftigte",
      indikator ==  "SE" ~ "Naturwissenschaftler*innen und Ingenieur*innen",
      T ~ indikator
    ),
    variable = dplyr::case_when(
      variable == "THS_PER" ~ "Anzahl in Tsd.",
      variable == "PC_EMP" ~ "Anteil an Gesamtbevölkerung",
      variable == "PC_ACT" ~ "Anteil an arbeitender Bevölkerung"
    ),
    geschlecht = dplyr::case_when(
      geschlecht == "F" ~ "Frauen",
      geschlecht == "M" ~ "Männer",
      geschlecht == "T" ~ "Gesamt"
    )
  )


# missings ausfiltern
dat <- na.omit(dat)

# bereich ergänze und in Reihenfolge bringen
dat$bereich <- "Arbeitsmarkt"
dat$anforderung <- "Gesamt"
dat$fachbereich <- "MINT"
dat$quelle <- "EUROSTAT"
dat$population <- "EU"
dat$typ <- ifelse(grepl("Anzah", dat$variable), "Anzahl", "In Prozent")

# Spalten in logische Reihenfolge bringen
dat<- dat[,c("bereich", "quelle", "variable", "typ", "indikator", "fachbereich",
             "geschlecht", "population", "land", "jahr", "anforderung", "wert")]
# umbenennen
arbeitsmarkt_beschaeftigte_eu <- dat


# speichern:

save(arbeitsmarkt_beschaeftigte_eu, file = "arbeitsmarkt_beschaeftigte_eu.rda")
usethis::use_data(arbeitsmarkt_beschaeftigte_eu , overwrite = T)







#### Erstellt studierende_europa ------------

#
# dat <- readr::read_csv(paste0(pfad, "EUROSTAT001_custom_Studi_Fach_Gender_original.csv.gz"))
#
# dat1 <- dat %>%
#   dplyr::select("iscedf13", "sex", "geo",
#                 "TIME_PERIOD", "OBS_VALUE")%>%
#   dplyr::rename(fach = iscedf13, geschlecht = sex, land = geo, jahr= TIME_PERIOD, wert = OBS_VALUE)%>%
#   dplyr::mutate(indikator = "Studierende",
#                 typ= "In Prozent")


dat007 <- readr::read_csv(paste0(pfad, "EUROSTAT007_educ_uoe_enrt04__custom_17956503_linear_2_0.csv"))

dat007 <- dat007 %>%
  select("iscedf13", "sex", "geo", "TIME_PERIOD", "OBS_VALUE", "isced11") %>%
  rename(
    fach       = iscedf13,
    geschlecht = sex,
    land       = geo,
    jahr       = TIME_PERIOD,
    wert       = OBS_VALUE,
    ebene      = isced11
  ) %>%
  filter(
    ebene == "ED5-8"
  ) %>%
  mutate(indikator = "Studierende", typ = "In Prozent") %>%
  select(-ebene)


# dat_all <- bind_rows(dat1, dat007)

dat007 <- unique(dat007)

dat_dupes<- dat007 %>%
  janitor::get_dupes()

dat007$land <- countrycode::countrycode(dat007$land, origin = "eurostat", destination="country.name.de", custom_match = c("EU28" = "EU (28)", "EU27_2020" = "EU (27), seit 2020"))

dat2 <- dat007 %>%
  dplyr::mutate(geschlecht= dplyr::case_when(
    geschlecht == "F" ~ "Frauen",
    geschlecht == "M" ~ "Männer",
    T ~ "Gesamt"))%>%
  dplyr::mutate(fachbereich = dplyr::case_when(
    stringr::str_ends("F00", .$fach) ~ "Allgemeine Bildungsgänge und Qualifikationen",
    stringr::str_ends("F01", .$fach) ~ "Pädagogik",
    stringr::str_ends("F02", .$fach) ~ "Geisteswissenschaften und Künste",
    stringr::str_ends("F03", .$fach) ~ "Sozialwissenschaften, Journalismus und Informationswesen",
    stringr::str_ends("F04", .$fach) ~ "Wirtschaft, Verwaltung und Recht",
    stringr::str_ends("F05", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik",
    stringr::str_ends("F06", .$fach) ~ "Informatik & Kommunikationstechnologie",
    stringr::str_ends("F07", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
    stringr::str_ends("F08", .$fach) ~ "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin",
    stringr::str_ends("F09", .$fach) ~ "Gesundheit, Medizin und Sozialwesen",
    stringr::str_ends("F10", .$fach) ~ "Dienstleistungen",
    stringr::str_detect("Total", .$fach) ~ "Insgesamt",
    stringr::str_ends("UNK", .$fach) ~ "Unbekannt"))%>%
  dplyr::mutate(fach = dplyr::case_when(
    stringr::str_ends("F00", .$fach)~ "Allgemeine Bildungsgänge und Qualifikationen",
    stringr::str_ends("F01", .$fach)~ "Pädagogik",
    stringr::str_ends("F02", .$fach) ~ "Geisteswissenschaften und Künste",
    stringr::str_ends("F03", .$fach) ~ "Sozialwissenschaften, Journalismus und Informationswesen",
    stringr::str_ends("F04", .$fach) ~ "Wirtschaft, Verwaltung und Recht",
    stringr::str_ends("F05", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik",
    stringr::str_ends("F06", .$fach) ~ "Informatik & Kommunikationstechnologie",
    stringr::str_ends("F061", .$fach) ~ "Informatik & Kommunikationstechnologie allgemein",
    stringr::str_ends("F068", .$fach) ~ "Interdisziplinäre Programme mit Schwerpunkt Informatik & Kommunikationstechnologie",
    stringr::str_ends("F07", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
    stringr::str_ends("F08", .$fach) ~ "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin",
    stringr::str_ends("F09", .$fach) ~ "Gesundheit, Medizin und Sozialwesen",
    stringr::str_ends("F10", .$fach) ~ "Dienstleistungen",
    stringr::str_ends("F050", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht näher definiert",
    stringr::str_ends("F051", .$fach) ~ "Biologie und verwandte Wissenschaften",
    stringr::str_ends("F052", .$fach) ~ "Umwelt",
    stringr::str_ends("F053", .$fach) ~ "Exakte Naturwissenschaften",
    stringr::str_ends("F054", .$fach) ~ "Mathematik und Statistik",
    stringr::str_ends("F058", .$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Naturwissenschaften,
Mathematik und Statistik",
    stringr::str_ends("F059", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht andernorts klassifiziert",
    stringr::str_ends("F070", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht näher definiert",
    stringr::str_ends("F071", .$fach) ~ "Ingenieurwesen und Technische Berufe",
    stringr::str_ends("F072", .$fach) ~ "Verarbeitendes Gewerbe und Bergbau",
    stringr::str_ends("F073", .$fach) ~ "Architektur und Baugewerbe",
    stringr::str_ends("F078", .$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Ingenieurwesen,
 verarbeitendes Gewerbe und Baugewerbe",
    stringr::str_ends("F079", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht andernorts klassifiziert",
    stringr::str_detect("TOTAL", .$fach) ~ "Insgesamt",
    stringr::str_ends("UNK", .$fach) ~ "Unbekannt",
    T~.$fach
  ))%>%
  dplyr::select(-fachbereich)


# %>%
#   arrange(fach, fachbereich)
#
# # warum hat fach bitte NAs????
# # hat es aktuell nicht (vgl. length(dat2$fach[is.na(dat2$fach) == TRUE]))
#
#   dat2$fachbereich <- zoo::na.locf(dat2$fachbereich)

# lel <- is.na(dat2$wert)




dat3 <- dat2 %>%
  tidyr::pivot_wider(names_from = fach, values_from = wert)%>%
  dplyr::mutate(
    "Weitere Ingenieurwesen, verarbeitendes Gewerbe, Baugewerbe" = `Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht andernorts klassifiziert`+
      `Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht näher definiert`,
    "Weitere Naturwissenschaften, Mathematik und Statistik"= `Naturwissenschaften, Mathematik und Statistik nicht andernorts klassifiziert`+
      `Naturwissenschaften, Mathematik und Statistik nicht näher definiert`) %>%
  dplyr::mutate("Alle MINT-Fächer" = rowSums(
    dplyr::select(.,c(`Naturwissenschaften, Mathematik und Statistik`,
                      `Informatik & Kommunikationstechnologie`,
                      `Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe`)), na.rm = T )) %>%
  dplyr::mutate("Alle Nicht MINT-Fächer" = Insgesamt - `Alle MINT-Fächer`) %>%
  dplyr::select(-c("Naturwissenschaften, Mathematik und Statistik nicht näher definiert",
                   "Naturwissenschaften, Mathematik und Statistik nicht andernorts klassifiziert",
                   "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht näher definiert",
                   "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht andernorts klassifiziert"))%>%
  tidyr::pivot_longer(c("Allgemeine Bildungsgänge und Qualifikationen":ncol(.)), values_to = "wert", names_to="fach")


dat4<- dat3 %>%
  dplyr::mutate(bereich = "Studium",
                quelle = "Eurostat",
  )%>%
  dplyr::mutate(mint_select= dplyr::case_when(fach %in% c("Naturwissenschaften, Mathematik und Statistik",
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
  dplyr::mutate(ebene= dplyr::case_when(fach %in% c("Allgemeine Bildungsgänge und Qualifikationen",
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
  dplyr::mutate(fachbereich = dplyr::case_when(fach %in% c("Allgemeine Bildungsgänge und Qualifikationen",
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
                                               fach == "Weitere Naturwissenschaften, Mathematik und Statistik" ~ "Naturwissenschaften, Mathematik und Statistik"))%>%
  dplyr::mutate(population = "Europäische Union+")

# Anteil nach Geschlecht berechnen
## ausfilter, da wo es kein "Gesamt" zur berechnung gibt
f <- dat4 %>%
  group_by(fach, land, jahr) %>%
  count()

### filtern über left_join und alle mit 2 raus
eu_gat_e1 <- dat4
eu_gat_e1 <- eu_gat_e1 %>%
  dplyr::left_join(f, by = c("fach", "land", "jahr")) %>%
  dplyr::filter(n == 3)

### dann berechnung - unelegant, nächste lösung wäre auch hier schöner (Selbstkritik kbr)

eu_gat_e1_f <- eu_gat_e1 %>%
  dplyr::group_by(fach, land, jahr) %>%
  dplyr::mutate((FA = wert[geschlecht == "Frauen"]/wert[geschlecht == "Gesamt"]*100),
                indikator = "Frauen-/Männeranteil") %>%
  dplyr::filter(geschlecht == "Frauen") %>%
  dplyr::select(-c(n, wert)) %>%
  dplyr::rename("wert" = "(...)")

eu_gat_e1_m <- eu_gat_e1 %>%
  dplyr::group_by(fach, land, jahr) %>%
  dplyr::mutate(Männer =wert[geschlecht == "Männer"]/wert[geschlecht == "Gesamt"]*100,
                indikator = "Frauen-/Männeranteil") %>%
  dplyr::filter(geschlecht == "Männer") %>%
  dplyr::select(-c(n, wert)) %>%
  dplyr::rename("wert" = "Männer")

eu_gat_e1 <- eu_gat_e1 %>%
  dplyr::group_by(fach, land, jahr) %>%
  dplyr::mutate(Gesamt = wert[geschlecht == "Gesamt"]/wert[geschlecht == "Gesamt"]*100,
                indikator = "Frauen-/Männeranteil") %>%
  dplyr::filter(geschlecht == "Gesamt") %>%
  dplyr::select(-c(n, wert)) %>%
  dplyr::rename("wert" = "Gesamt")

eu_gat_e1 <- rbind(eu_gat_e1, eu_gat_e1_f, eu_gat_e1_m)

# Anteil wer wählt was berechnen

eu_ges <- dat4 %>%
  dplyr::filter(fach == "Insgesamt")

eu_ww <- dat4 %>%
  dplyr::left_join(eu_ges, by =c("geschlecht", "land", "jahr", "indikator", "typ",
                                 "bereich", "quelle",
                                 "population" )) %>%
  dplyr::mutate(wert = wert.x / wert.y * 100) %>%
  dplyr::select(-c(fach.y, fachbereich.y, wert.x, wert.y, mint_select.y, ebene.y)) %>%
  dplyr::rename(fach = fach.x,
                fachbereich = fachbereich.x,
                ebene = ebene.x,
                mint_select = mint_select.x) %>%
  dplyr::mutate(indikator = "Fächerwahl")

# Zusammenfassen

studierende_europa <- rbind(eu_ww, eu_gat_e1)

# ordnen
studierende_europa <- studierende_europa[,c("bereich", "quelle", "population", "typ",
                                            "indikator", "ebene", "mint_select", "fachbereich", "fach",
                                            "geschlecht", "land", "jahr", "wert")]

# Anpassung mint_select
studierende_europa$mint_select <- ifelse(studierende_europa$fachbereich == "Alle MINT-Fächer", "mint", studierende_europa$mint_select)


save(studierende_europa, file = "studierende_europa.rda")
usethis::use_data(studierende_europa, overwrite = T)


#### Erstellt  studierende_mobil_eu_absolut ---------
library(rio)
library(dplyr)
library(tidyr)
library(magrittr)
library(stringr)
library(readxl)
library(janitor)
library(purrr)
library(readr)
library(countrycode)

#dat_eust <- read_csv(paste0(pfad,"EUROSTAT003_custom_intern_studis_educ_uoe_mobs01__custom_7521082_linear.csv.gz"))

dat_eust <-  read_csv(paste0(pfad,"EUROSTAT005_educ_uoe_mobs01__custom_17945314_linear_2_0.csv"))


dat_eust1 <- dat_eust %>%
  select(-c(1:4))%>%
  rename(geschlecht = sex, land = geo, jahr = TIME_PERIOD, wert = OBS_VALUE,
         kommentar = OBS_FLAG, fach = iscedf13, anforderung = isced11)%>%
  mutate(across(land, ~ countrycode(., origin = "eurostat", destination="country.name.de", custom_match = c("EU28" = "EU (28)", "EU27_2020" = "EU (27), seit 2020"))))%>%
  mutate(geschlecht=case_when(geschlecht == "M"~ "Männlich",
                              geschlecht == "F" ~ "Weiblich",
                              T ~ "Gesamt"))%>%
  dplyr::mutate(ebene= dplyr::case_when(stringr::str_ends(.$fach,"F00|F01|F02|F03|F04|F05|F06|F07|F08|F09|F10|TOTAL") ~ 1,
                                        T ~2))%>%
  dplyr::mutate(mint_select= dplyr::case_when(stringr::str_detect(.$fach,"F05|F07|F06") ~ "mint",
                                              T ~"nicht mint"))%>%
  dplyr::mutate(fach = dplyr::case_when(
    stringr::str_ends("F00", .$fach)~ "Allgemeine Bildungsgänge und Qualifikationen",
    stringr::str_ends("F01", .$fach)~ "Pädagogik",
    stringr::str_ends("F02", .$fach) ~ "Geisteswissenschaften und Künste",
    stringr::str_ends("F03", .$fach) ~ "Sozialwissenschaften, Journalismus und Informationswesen",
    stringr::str_ends("F04", .$fach) ~ "Wirtschaft, Verwaltung und Recht",
    stringr::str_ends("F05", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik",
    stringr::str_ends("F06", .$fach) ~ "Informatik & Kommunikationstechnologie",
    stringr::str_ends("F061", .$fach) ~ "Informatik & Kommunikationstechnologie allgemein",
    stringr::str_ends("F068", .$fach) ~ "Interdisziplinäre Programme mit Schwerpunkt Informatik & Kommunikationstechnologie",
    stringr::str_ends("F07", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
    stringr::str_ends("F08", .$fach) ~ "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin",
    stringr::str_ends("F09", .$fach) ~ "Gesundheit, Medizin und Sozialwesen",
    stringr::str_ends("F10", .$fach) ~ "Dienstleistungen",
    stringr::str_ends("F050", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht näher definiert",
    stringr::str_ends("F051", .$fach) ~ "Biologie und verwandte Wissenschaften",
    stringr::str_ends("F052", .$fach) ~ "Umwelt",
    stringr::str_ends("F053", .$fach) ~ "Exakte Naturwissenschaften",
    stringr::str_ends("F054", .$fach) ~ "Mathematik und Statistik",
    stringr::str_ends("F058", .$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Naturwissenschaften,
Mathematik und Statistik",
    stringr::str_ends("F059", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht andernorts klassifiziert",
    stringr::str_ends("F070", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht näher definiert",
    stringr::str_ends("F071", .$fach) ~ "Ingenieurwesen und Technische Berufe",
    stringr::str_ends("F072", .$fach) ~ "Verarbeitendes Gewerbe und Bergbau",
    stringr::str_ends("F073", .$fach) ~ "Architektur und Baugewerbe",
    stringr::str_ends("F078", .$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Ingenieurwesen,
 verarbeitendes Gewerbe und Baugewerbe",
    stringr::str_ends("F079", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht andernorts klassifiziert",
    stringr::str_detect("TOTAL", .$fach) ~ "Insgesamt",
    stringr::str_ends("UNK", .$fach) ~ "Unbekannt",
    T~.$fach))%>%
  mutate(anforderung = dplyr::case_when(anforderung ==  "ED5" ~ "kurzes tertiäres Bildungsprogramm (ISCED 5)",
                                        anforderung ==  "ED6" ~ "Bachelor oder vergleichbar (ISCED 6)",
                                        anforderung ==  "ED7" ~ "Master oder vergleichbar (ISCED 7)",
                                        anforderung ==  "ED8" ~ "Promotion (ISCED 8)",
                                        anforderung == "ED5-8" ~ "Tertiäre Bildung (gesamt)",
                                        T ~ anforderung
  ))%>%
  mutate(kommentar=case_when(kommentar == "b" ~ "break in time series",
                             kommentar == "e" ~ "estimated",
                             kommentar == "d" ~ "definition differs (see metadata)",
                             kommentar == "z" ~ "not applicable",
                             T ~ kommentar))%>%
  mutate(bereich = "hochschule",
         indikator = "Ausländische mobile Studierende",
         typ = "Anzahl")

dat_eust1_1<- dat_eust1 %>%
  dplyr::filter(ebene == 1)%>%
  dplyr::group_by(land, anforderung, jahr, geschlecht, mint_select)%>%
  dplyr::summarise(land, anforderung, jahr, geschlecht, ebene, mint_select, bereich, indikator, wert= sum(wert, na.rm=T))%>%
  dplyr::ungroup()%>%
  unique()%>%
  dplyr::mutate(fach = case_when(mint_select == "mint" ~ "MINT",
                                 T ~ "Nicht MINT",
  ),typ= "Anzahl")%>%
  dplyr::filter(fach != "Nicht MINT" )




dat_eust1_2 <- dat_eust1 %>%
  filter(fach == "Insgesamt")%>%
  select(- ebene,- mint_select)%>%
  full_join(dat_eust1_1 %>% select(- ebene,- mint_select), by=c("anforderung", "fach", "geschlecht", "land",
                                                                "jahr",  "bereich",
                                                                "indikator", "typ"))%>%
  mutate(wert= coalesce(wert.x, wert.y))%>%
  select(-wert.y, -wert.x)%>%
  pivot_wider(values_from = wert, names_from = fach)%>%
  mutate("Nicht MINT" = Insgesamt - MINT)%>%
  pivot_longer(c(Insgesamt, MINT, `Nicht MINT`), values_to = "wert", names_to = "fach" )%>%
  mutate(ebene= 1,
         mint_select = case_when(fach %in% c("Insgesamt", "Nicht MINT")~ "nicht mint",
                                 T ~ "mint"))

dat_eust1_3 <- dat_eust1 %>%
  filter(fach != "Insgesamt")%>%
  bind_rows(dat_eust1_2)

# warum ist der kleinste wert < 0 ???

studierende_mobil_eu_absolut <- dat_eust1_2


save(studierende_mobil_eu_absolut, file = "studierende_mobil_eu_absolut.rda")
usethis::use_data(studierende_mobil_eu_absolut, overwrite = T)



# DAS WIRD AKTUELL NICHT VERWENDET und wird vermutlich auslaufen ~~~ Turan
#### Erstellt  studierende_mobil_eu_share -------

dat_euro4 <- readr::read_csv(paste0(pfad, "EUROSTAT004_educ_uoe_mobs04__custom_8027463_linear.csv.gz"))

dat_euro4_1 <- dat_euro4 %>%
  dplyr::select(- c("DATAFLOW", "freq"  , "LAST UPDATE", "unit"))%>%
  dplyr::rename(geschlecht = sex, land = geo, jahr = TIME_PERIOD, wert = OBS_VALUE,
                kommentar = OBS_FLAG, fach = iscedf13, anforderung = isced11)%>%
  mutate(across(land, ~ countrycode(., origin = "eurostat", destination="country.name.de", custom_match = c("EU28" = "EU (28)", "EU27_2020" = "EU (27), seit 2020"))))%>%
  mutate(geschlecht=case_when(geschlecht == "M"~ "Männlich",
                              geschlecht == "F" ~ "Weiblich",
                              T ~ "Gesamt"))%>%
  dplyr::mutate(ebene= dplyr::case_when(stringr::str_ends(.$fach,"F00|F01|F02|F03|F04|F05|F06|F07|F08|F09|F10|TOTAL") ~ 1,
                                        T ~2))%>%
  dplyr::mutate(mint_select= dplyr::case_when(stringr::str_detect(.$fach,"F05|F07|F06") ~ "mint",
                                              T ~"nicht mint"))%>%
  dplyr::mutate(fach = dplyr::case_when(
    stringr::str_ends("F00", .$fach)~ "Allgemeine Bildungsgänge und Qualifikationen",
    stringr::str_ends("F01", .$fach)~ "Pädagogik",
    stringr::str_ends("F02", .$fach) ~ "Geisteswissenschaften und Künste",
    stringr::str_ends("F03", .$fach) ~ "Sozialwissenschaften, Journalismus und Informationswesen",
    stringr::str_ends("F04", .$fach) ~ "Wirtschaft, Verwaltung und Recht",
    stringr::str_ends("F05", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik",
    stringr::str_ends("F06", .$fach) ~ "Informatik & Kommunikationstechnologie",
    stringr::str_ends("F061", .$fach) ~ "Informatik & Kommunikationstechnologie allgemein",
    stringr::str_ends("F068", .$fach) ~ "Interdisziplinäre Programme mit Schwerpunkt Informatik & Kommunikationstechnologie",
    stringr::str_ends("F07", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
    stringr::str_ends("F08", .$fach) ~ "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin",
    stringr::str_ends("F09", .$fach) ~ "Gesundheit, Medizin und Sozialwesen",
    stringr::str_ends("F10", .$fach) ~ "Dienstleistungen",
    stringr::str_ends("F050", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht näher definiert",
    stringr::str_ends("F051", .$fach) ~ "Biologie und verwandte Wissenschaften",
    stringr::str_ends("F052", .$fach) ~ "Umwelt",
    stringr::str_ends("F053", .$fach) ~ "Exakte Naturwissenschaften",
    stringr::str_ends("F054", .$fach) ~ "Mathematik und Statistik",
    stringr::str_ends("F058", .$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Naturwissenschaften,
Mathematik und Statistik",
    stringr::str_ends("F059", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht andernorts klassifiziert",
    stringr::str_ends("F070", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht näher definiert",
    stringr::str_ends("F071", .$fach) ~ "Ingenieurwesen und Technische Berufe",
    stringr::str_ends("F072", .$fach) ~ "Verarbeitendes Gewerbe und Bergbau",
    stringr::str_ends("F073", .$fach) ~ "Architektur und Baugewerbe",
    stringr::str_ends("F078", .$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Ingenieurwesen,
 verarbeitendes Gewerbe und Baugewerbe",
    stringr::str_ends("F079", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht andernorts klassifiziert",
    stringr::str_detect("TOTAL", .$fach) ~ "Insgesamt",
    stringr::str_ends("UNK", .$fach) ~ "Unbekannt",
    T~.$fach))%>%
  mutate(anforderung = dplyr::case_when(anforderung ==  "ED5" ~ "kurzes tertiäres Bildungsprogramm (ISCED 5)",
                                        anforderung ==  "ED6" ~ "Bachelor oder vergleichbar (ISCED 6)",
                                        anforderung ==  "ED7" ~ "Master oder vergleichbar (ISCED 7)",
                                        anforderung ==  "ED8" ~ "Promotion (ISCED 8)",
                                        anforderung == "ED5-8" ~ "Tertiäre Bildung (gesamt)",
                                        T ~ anforderung
  ))%>%
  mutate(kommentar=case_when(kommentar == "b" ~ "break in time series",
                             kommentar == "e" ~ "estimated",
                             kommentar == "d" ~ "definition differs (see metadata)",
                             kommentar == "z" ~ "not applicable",
                             T ~ kommentar))%>%
  mutate(bereich = "hochschule",
         indikator = "Ausländische mobile Studierende",
         typ = "Anteil")

dat_euro4_2 <- dat_euro4_1 %>%
  dplyr::filter(mint_select == "mint" & ebene == 1)%>%
  dplyr::group_by(land, anforderung, jahr, geschlecht)%>%
  dplyr::summarise(land, anforderung, jahr, geschlecht, ebene, mint_select, bereich, indikator, MINT= sum(wert, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate("Nicht MINT" = 100 - MINT,
                typ= "Anteil")%>%
  tidyr::pivot_longer(c("MINT", "Nicht MINT"), names_to = "fach", values_to = "wert" )%>%
  unique()

dat_euro4_3 <- dat_euro4_1 %>%
  dplyr::bind_rows(dat_euro4_2)



studierende_mobil_eu_share <- dat_euro4_3

# warum ist der kleinste wert < 0 ???

usethis::use_data(studierende_mobil_eu_share, overwrite = T)




iscedf13_transform_lang <- function(dat) {

  # fächer benennen

  dat <- dat %>%
    dplyr::mutate(fach = dplyr::case_when(stringr::str_ends("F00", dat$fach)~ "Allgemeine Bildungsgänge und Qualifikationen",
                                          stringr::str_ends("F01", dat$fach)~ "Pädagogik",
                                          stringr::str_ends("F02", dat$fach) ~ "Geisteswissenschaften und Künste",
                                          stringr::str_ends("F03", dat$fach) ~ "Sozialwissenschaften, Journalismus und Informationswesen",
                                          stringr::str_ends("F04", dat$fach) ~ "Wirtschaft, Verwaltung und Recht",
                                          stringr::str_ends("F05", dat$fach) ~ "Naturwissenschaften, Mathematik und Statistik",
                                          stringr::str_ends("F06", dat$fach) ~ "Informatik & Kommunikationstechnologie",
                                          stringr::str_ends("F07", dat$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                          stringr::str_ends("F08", dat$fach) ~ "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin",
                                          stringr::str_ends("F09", dat$fach) ~ "Gesundheit, Medizin und Sozialwesen",
                                          stringr::str_ends("F10", dat$fach) ~ "Dienstleistungen",
                                          stringr::str_ends("F050", dat$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht näher definiert",
                                          stringr::str_ends("F051", dat$fach) ~ "Biologie und verwandte Wissenschaften",
                                          stringr::str_ends("F052", dat$fach) ~ "Umwelt",
                                          stringr::str_ends("F053", dat$fach) ~ "Exakte Naturwissenschaften",
                                          stringr::str_ends("F054", dat$fach) ~ "Mathematik und Statistik",
                                          stringr::str_ends("F058", dat$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Naturwissenschaften,
Mathematik und Statistik",
                                          stringr::str_ends("F059", dat$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht andernorts klassifiziert",
                                          stringr::str_ends("F070", dat$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht näher definiert",
                                          stringr::str_ends("F071", dat$fach) ~ "Ingenieurwesen und Technische Berufe",
                                          stringr::str_ends("F072", dat$fach) ~ "Verarbeitendes Gewerbe und Bergbau",
                                          stringr::str_ends("F073", dat$fach) ~ "Architektur und Baugewerbe",
                                          stringr::str_ends("F078", dat$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Ingenieurwesen,
 verarbeitendes Gewerbe und Baugewerbe",
                                          stringr::str_ends("F079", dat$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht andernorts klassifiziert",

                                          stringr::str_ends("F05T07", dat$fach) ~ "MINT",
                                          stringr::str_ends("F050_59", dat$fach) ~ "Weitere Naturwissenschaften, Mathematik und Statistik",
                                          stringr::str_ends("F070_79", dat$fach) ~ "Weitere Ingenieurwesen, verarbeitendes Gewerbe, Baugewerbe",

                                          stringr::str_detect("TOTAL", dat$fach) ~ "Alle",
                                          stringr::str_detect("_T", dat$fach) ~ "Alle",
                                          stringr::str_ends("UNK", dat$fach) ~ "Unbekannt",
                                          stringr::str_ends("F99", dat$fach) ~ "Unbekannt",
                                          T ~ dat$fach))

  return(dat)
}



### OECD ----

#### Erstellt arbeitsmarkt_anzahl_azubis_oecd -------------
#
#dat <- read.csv(paste0(pfad,"OECD005_Anzahl_Studi_Azubi_nach_Fach_Sex.csv"),
#                header = TRUE, sep = ",", dec = ".")




dat <- read.csv(paste0(pfad,"OECD008_studis_azubis_nach_fach.csv"),
                header = TRUE, sep = ",", dec = ".")



dat <- dat %>%
  dplyr::select(REF_AREA, Reference.area, EDUCATION_LEV, Sex, EDUCATION_FIELD,
                TIME_PERIOD, OBS_VALUE) %>%
  dplyr::rename(land_code = REF_AREA,
                land = Reference.area,
                anforderung = EDUCATION_LEV,
                geschlecht = Sex,
                fach = EDUCATION_FIELD,
                jahr = TIME_PERIOD,
                wert = OBS_VALUE)

#dat <- dat %>%
#  dplyr::select(COUNTRY, Country, EDUCATION_LEV, Gender, EDUCATION_FIELD,
#                Year, Value) %>%
#  dplyr::rename(land_code = COUNTRY,
#                land = Country,
#                anforderung = EDUCATION_LEV,
#                geschlecht = Gender,
#                fach = EDUCATION_FIELD,
#                jahr = Year,
#                wert = Value)

# Land zuweisen / übersetzen

dat$land <- countrycode::countryname(dat$land, destination = "country.name.de")


# Anforderungsniveau zuweisen
dat <- dat %>%
  dplyr::mutate(anforderung = dplyr::case_when(
    anforderung ==  "ISCED11_35" ~ "Erstausbildung (ISCED 35)",
    anforderung ==  "ISCED11_45" ~ "Ausbildung (ISCED 45)",
    anforderung ==  "ISCED11_5" ~ "kurzes tertiäres Bildungsprogramm (ISCED 5)",
    anforderung ==  "ISCED11_54" ~ "kurzes tertiäres Bildungsprogramm (allgemeinbildend)",
    anforderung ==  "ISCED11_55" ~ "kurzes tertiäres Bildungsprogramm (berufsorientiert)",
    anforderung ==  "ISCED11_6" ~ "Bachelor oder vergleichbar (ISCED 6)",
    anforderung ==  "ISCED11_64" ~ "Bachelor oder vergleichbar (akademisch)",
    anforderung ==  "ISCED11_65" ~ "Bachelor oder vergleichbar (berufsorientiert)",
    anforderung ==  "ISCED11_7" ~ "Master oder vergleichbar (ISCED 7)",
    anforderung ==  "ISCED11_74" ~ "Master oder vergleichbar (akademisch)",
    anforderung ==  "ISCED11_75" ~ "Master oder vergleichbar (berufsorientiert)",
    anforderung ==  "ISCED11_8" ~ "Promotion (ISCED 8)",
    anforderung == "ISCED11_5T8" ~ "tertiäre Bildung (gesamt)",
    T ~ anforderung
  ))

# Fachbereich zuweisen - mit Kekelis Funktion
## weitere Naturwissenschaften/Ingen-Wissenschaften berechnen
dat_nw <- dat %>%
  dplyr::filter(fach %in% c("F050", "F059")) %>%
  dplyr::group_by(land_code, land, anforderung, geschlecht, jahr) %>%
  dplyr::summarise(wert = sum(wert)) %>%
  dplyr::ungroup()
dat_nw$fach <- "F050_59"
dat_iw <- dat %>%
  dplyr::filter(fach %in% c("F070", "F079")) %>%
  dplyr::group_by(land_code, land, anforderung, geschlecht, jahr) %>%
  dplyr::summarise(wert = sum(wert)) %>%
  dplyr::ungroup()
dat_iw$fach <- "F070_79"
## einzelnen löschen
dat <- dat %>% filter(!(fach %in% c("F050", "F059", "F070", "F079")))

dat <- rbind(dat, dat_iw, dat_nw)

dat <- iscedf13_transform_lang(dat)

# übersetzen
dat <- dat %>%
  dplyr::mutate(
    geschlecht = dplyr::case_when(
      geschlecht == "Male" ~ "Männer",
      geschlecht == "Female" ~ "Frauen",
      geschlecht == "Total" ~ "Gesamt",
      T ~ geschlecht
    )
  )

# indikator nach akademisch vs. berufsorientiert ergänzen
dat$indikator <- ifelse(grepl("berufsorientiert", dat$anforderung) |
                          grepl("usbildung", dat$anforderung), "berufsorientiert", "akademisch")

dat$indikator <- ifelse(dat$anforderung == "kurzes tertiäres Bildungsprogramm (ISCED 5)" |
                          dat$anforderung == "Bachelor oder vergleichbar (ISCED 6)" |
                          dat$anforderung == "Master oder vergleichbar (ISCED 7)" |
                          dat$anforderung == "tertiäre Bildung (gesamt)", "Alle", dat$indikator)



# missings ausfiltern
dat <- na.omit(dat)

# bereich ergänze und in Reihenfolge bringen
dat$bereich <- "Arbeitsmarkt"
#dat$variable <- "Anzahl Studierende/Auszubildende"
dat$quelle <- "OECD"
dat$population <- "OECD"
dat$typ <- "Anzahl"

# Spalten in logische Reihenfolge bringen
dat<- dat[,c("bereich", "quelle", "typ", "indikator", "fach",
             "geschlecht", "population", "land", "jahr", "anforderung", "wert")]


# aus irgendeinem Grund sind hinter den absoluten Anzhalen an Studis Kommastellen teils - auch schon in Rohdaten
# Bei Abgleich mit Tabellen der Datenbank - Zahlen Stimmen, wenn man die Kommastellen rundet (also auch aufrundet)
# mach ich hier:
dat$wert <- round(dat$wert)

# umbenennen
arbeitsmarkt_anzahl_azubis_oecd <- dat

# speichern
save(arbeitsmarkt_anzahl_azubis_oecd, file = "arbeitsmarkt_anzahl_azubis_oecd.rda")
usethis::use_data(arbeitsmarkt_anzahl_azubis_oecd, overwrite = T)









#### Erstellt studierende_anzahl_oecd ---------



pfad <- "C:/Users/tko/OneDrive - Stifterverband/2_MINT-Lücke schließen/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/"

#dat <- read.csv(paste0(pfad,
#                       "OECD005_Anzahl_Studi_Azubi_nach_Fach_Sex.csv"),
#                header = TRUE, sep = ",", dec = ".")

dat <- read.csv(paste0(pfad,
                       "OECD008_studis_azubis_nach_fach.csv"),
                header = TRUE, sep = ",", dec = ".")



dat <- dat %>%
  dplyr::select(REF_AREA, Reference.area, EDUCATION_LEV, Sex, EDUCATION_FIELD,
                TIME_PERIOD, OBS_VALUE, MEASURE) %>%
  dplyr::rename(land_code = REF_AREA,
                land = Reference.area,
                anforderung = EDUCATION_LEV,
                geschlecht = Sex,
                fach = EDUCATION_FIELD,
                jahr = TIME_PERIOD,
                wert = OBS_VALUE) %>%
  dplyr::filter(
    MEASURE == "ENRL"
  ) %>%
  dplyr::select(
    -MEASURE
  )


# dat <- dat %>%
#   dplyr::select(COUNTRY, Country, EDUCATION_LEV, Gender, EDUCATION_FIELD,
#                 Year, Value) %>%
#   dplyr::rename(land_code = COUNTRY,
#                 land = Country,
#                 anforderung = EDUCATION_LEV,
#                 geschlecht = Gender,
#                 fach = EDUCATION_FIELD,
#                 jahr = Year,
#                 wert = Value)

# Land zuweisen / übersetzen

dat$land <- countrycode::countryname(dat$land, destination = "country.name.de")


# Anforderungsniveau zuweisen
# filtern am ende auf:
# [1] "kurzes tertiäres Bildungsprogramm (allgemeinbildend)" "Promotion (ISCED 8)"
# [3] "Bachelor oder vergleichbar (akademisch)"              "Master oder vergleichbar (akademisch)"
dat <- dat %>%
  dplyr::mutate(anforderung = dplyr::case_when(
    anforderung ==  "ISCED11_35" ~ "Erstausbildung (ISCED 35)",
    anforderung ==  "ISCED11_45" ~ "Ausbildung (ISCED 45)",
    anforderung ==  "ISCED11_5" ~ "kurzes tertiäres Bildungsprogramm (ISCED 5)",
    anforderung ==  "ISCED11_54" ~ "kurzes tertiäres Bildungsprogramm (allgemeinbildend)",
    anforderung ==  "ISCED11_55" ~ "kurzes tertiäres Bildungsprogramm (berufsorientiert)",
    anforderung ==  "ISCED11_6" ~ "Bachelor oder vergleichbar (ISCED 6)",
    anforderung ==  "ISCED11_64" ~ "Bachelor oder vergleichbar (akademisch)",
    anforderung ==  "ISCED11_65" ~ "Bachelor oder vergleichbar (berufsorientiert)",
    anforderung ==  "ISCED11_7" ~ "Master oder vergleichbar (ISCED 7)",
    anforderung ==  "ISCED11_74" ~ "Master oder vergleichbar (akademisch)",
    anforderung ==  "ISCED11_75" ~ "Master oder vergleichbar (berufsorientiert)",
    anforderung ==  "ISCED11_8" ~ "Promotion (ISCED 8)",
    anforderung == "ISCED11_5T8" ~ "tertiäre Bildung (gesamt)",
    T ~ anforderung
  ))

# Fachbereich zuweisen - mit Kekelis Funktion
## weitere Naturwissenschaften/Ingen-Wissenschaften berechnen
dat_nw <- dat %>%
  dplyr::filter(fach %in% c("F05", "F059")) %>%
  dplyr::group_by(land_code, land, anforderung, geschlecht, jahr) %>%
  dplyr::summarise(wert = sum(wert)) %>%
  dplyr::ungroup()
dat_nw$fach <- "F050_59"
dat_iw <- dat %>%
  dplyr::filter(fach %in% c("F07", "F079")) %>%
  dplyr::group_by(land_code, land, anforderung, geschlecht, jahr) %>%
  dplyr::summarise(wert = sum(wert)) %>%
  dplyr::ungroup()
dat_iw$fach <- "F070_79"
## einzelnen löschen
dat <- dat %>%
  dplyr::filter(!(fach %in% c("F05", "F059", "F07", "F079")))

dat <- rbind(dat, dat_iw, dat_nw)

dat <- dat %>%
  dplyr::mutate(fach = dplyr::case_when(stringr::str_ends("F00", dat$fach)~ "Allgemeine Bildungsgänge und Qualifikationen",
                                        stringr::str_ends("F01", dat$fach)~ "Pädagogik",
                                        stringr::str_ends("F02", dat$fach) ~ "Geisteswissenschaften und Künste",
                                        stringr::str_ends("F03", dat$fach) ~ "Sozialwissenschaften, Journalismus und Informationswesen",
                                        stringr::str_ends("F04", dat$fach) ~ "Wirtschaft, Verwaltung und Recht",
                                        stringr::str_ends("F05", dat$fach) ~ "Naturwissenschaften, Mathematik und Statistik",
                                        stringr::str_ends("F06", dat$fach) ~ "Informatik & Kommunikationstechnologie",
                                        stringr::str_ends("F061", dat$fach) ~ "Informatik & Kommunikationstechnologie allgemein",
                                        stringr::str_ends("F068", dat$fach) ~ "Interdisziplinäre Programme mit Schwerpunkt Informatik & Kommunikationstechnologie",
                                        stringr::str_ends("F07", dat$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                        stringr::str_ends("F08", dat$fach) ~ "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin",
                                        stringr::str_ends("F09", dat$fach) ~ "Gesundheit, Medizin und Sozialwesen",
                                        stringr::str_ends("F10", dat$fach) ~ "Dienstleistungen",
                                        stringr::str_ends("F050", dat$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht näher definiert",
                                        stringr::str_ends("F051", dat$fach) ~ "Biologie und verwandte Wissenschaften",
                                        stringr::str_ends("F052", dat$fach) ~ "Umwelt",
                                        stringr::str_ends("F053", dat$fach) ~ "Exakte Naturwissenschaften",
                                        stringr::str_ends("F054", dat$fach) ~ "Mathematik und Statistik",
                                        stringr::str_ends("F058", dat$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Naturwissenschaften,
Mathematik und Statistik",
                                        stringr::str_ends("F059", dat$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht andernorts klassifiziert",
                                        stringr::str_ends("F070", dat$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht näher definiert",
                                        stringr::str_ends("F071", dat$fach) ~ "Ingenieurwesen und Technische Berufe",
                                        stringr::str_ends("F072", dat$fach) ~ "Verarbeitendes Gewerbe und Bergbau",
                                        stringr::str_ends("F073", dat$fach) ~ "Architektur und Baugewerbe",
                                        stringr::str_ends("F078", dat$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Ingenieurwesen,
 verarbeitendes Gewerbe und Baugewerbe",
                                        stringr::str_ends("F079", dat$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht andernorts klassifiziert",

                                        stringr::str_ends("F05T07", dat$fach) ~ "MINT",
                                        stringr::str_ends("F050_59", dat$fach) ~ "Weitere Naturwissenschaften, Mathematik und Statistik",
                                        stringr::str_ends("F070_79", dat$fach) ~ "Weitere Ingenieurwesen, verarbeitendes Gewerbe, Baugewerbe",

                                        stringr::str_detect("TOTAL", dat$fach) ~ "Alle",
                                        stringr::str_detect("_T", dat$fach) ~ "Alle",
                                        stringr::str_ends("UNK", dat$fach) ~ "Unbekannt",
                                        stringr::str_ends("F99", dat$fach) ~ "Unbekannt",
                                        T ~ dat$fach),
                fachbereich = dplyr::case_when(
                  stringr::str_ends("F00", dat$fach)~ "Allgemeine Bildungsgänge und Qualifikationen",
                  stringr::str_ends("F01", dat$fach)~ "Pädagogik",
                  stringr::str_ends("F02", dat$fach) ~ "Geisteswissenschaften und Künste",
                  stringr::str_ends("F03", dat$fach) ~ "Sozialwissenschaften, Journalismus und Informationswesen",
                  stringr::str_ends("F04", dat$fach) ~ "Wirtschaft, Verwaltung und Recht",
                  stringr::str_ends("F05", dat$fach) ~ "Naturwissenschaften, Mathematik und Statistik",
                  stringr::str_ends("F06", dat$fach) ~ "Informatik & Kommunikationstechnologie",
                  stringr::str_ends("F061", dat$fach) ~ "Informatik & Kommunikationstechnologie",
                  stringr::str_ends("F068", dat$fach) ~ "Informatik & Kommunikationstechnologie",
                  stringr::str_ends("F07", dat$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                  stringr::str_ends("F08", dat$fach) ~ "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin",
                  stringr::str_ends("F09", dat$fach) ~ "Gesundheit, Medizin und Sozialwesen",
                  stringr::str_ends("F10", dat$fach) ~ "Dienstleistungen",
                  stringr::str_ends("F050", dat$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht näher definiert",
                  stringr::str_ends("F051", dat$fach) ~ "Biologie und verwandte Wissenschaften",
                  stringr::str_ends("F052", dat$fach) ~ "Umwelt",
                  stringr::str_ends("F053", dat$fach) ~ "Exakte Naturwissenschaften",
                  stringr::str_ends("F054", dat$fach) ~ "Mathematik und Statistik",
                  stringr::str_ends("F058", dat$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Naturwissenschaften,
Mathematik und Statistik",
                  stringr::str_ends("F059", dat$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht andernorts klassifiziert",
                  stringr::str_ends("F070", dat$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht näher definiert",
                  stringr::str_ends("F071", dat$fach) ~ "Ingenieurwesen und Technische Berufe",
                  stringr::str_ends("F072", dat$fach) ~ "Verarbeitendes Gewerbe und Bergbau",
                  stringr::str_ends("F073", dat$fach) ~ "Architektur und Baugewerbe",
                  stringr::str_ends("F078", dat$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Ingenieurwesen,
 verarbeitendes Gewerbe und Baugewerbe",
                  stringr::str_ends("F079", dat$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht andernorts klassifiziert",

                  stringr::str_ends("F05T07", dat$fach) ~ "MINT",
                  stringr::str_ends("F050_59", dat$fach) ~ "Weitere Naturwissenschaften, Mathematik und Statistik",
                  stringr::str_ends("F070_79", dat$fach) ~ "Weitere Ingenieurwesen, verarbeitendes Gewerbe, Baugewerbe",

                  stringr::str_detect("TOTAL", dat$fach) ~ "Alle",
                  stringr::str_detect("_T", dat$fach) ~ "Alle",
                  stringr::str_ends("UNK", dat$fach) ~ "Unbekannt",
                  stringr::str_ends("F99", dat$fach) ~ "Unbekannt"
                ))

# Fachbereich und mint_select ergänzen
dat$fachbereich <- dat$fach
dat <- dat %>%
  dplyr::mutate(fachbereich = dplyr::case_when(
    fach == "Biologie und verwandte Wissenschaften" ~ "Naturwissenschaften, Mathematik und Statistik",
    fach == "Umwelt" ~ "Naturwissenschaften, Mathematik und Statistik",
    fach == "Exakte Naturwissenschaften" ~ "Naturwissenschaften, Mathematik und Statistik",
    fach == "Mathematik und Statistik" ~ "Naturwissenschaften, Mathematik und Statistik",
    fach == "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Naturwissenschaften,
Mathematik und Statistik" ~ "Naturwissenschaften, Mathematik und Statistik",
    fach == "Naturwissenschaften, Mathematik und Statistik nicht andernorts klassifiziert" ~ "Naturwissenschaften, Mathematik und Statistik",

    fach ==  "Informatik & Kommunikationstechnologie allgemein" ~ "Informatik & Kommunikationstechnologie",
    fach == "Interdisziplinäre Programme mit Schwerpunkt Informatik & Kommunikationstechnologie" ~ "Informatik & Kommunikationstechnologie",

    fach == "Ingenieurwesen und Technische Berufe" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
    fach == "Verarbeitendes Gewerbe und Bergbau" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
    fach == "Architektur und Baugewerbe" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
    fach == "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Ingenieurwesen,
 verarbeitendes Gewerbe und Baugewerbe" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
    fach == "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht andernorts klassifiziert" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",

    fach == "Weitere Naturwissenschaften, Mathematik und Statistik" ~ "Naturwissenschaften, Mathematik und Statistik",
    fach == "Weitere Ingenieurwesen, verarbeitendes Gewerbe, Baugewerbe" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
    T ~ fachbereich
  ))

dat$mint_select <- "nicht mint"
dat <- dat %>%
  dplyr::mutate(mint_select = dplyr::case_when(
    fachbereich == "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe" |
      fachbereich == "Naturwissenschaften, Mathematik und Statistik" |
      fachbereich == "Informatik & Kommunikationstechnologie" |
      fachbereich == "MINT" ~ "mint",
    T ~ mint_select
  ),
  ebene = dplyr::case_when(
    fachbereich %in% c("Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                       "Naturwissenschaften, Mathematik und Statistik",
                       "Informatik & Kommunikationstechnologie") &
      !(fach %in% c("Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                    "Naturwissenschaften, Mathematik und Statistik",
                    "Informatik & Kommunikationstechnologie")) ~ 2,
    fachbereich %in% c("Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                       "Naturwissenschaften, Mathematik und Statistik",
                       "Informatik & Kommunikationstechnologie") &
      fach %in% c("Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                  "Naturwissenschaften, Mathematik und Statistik",
                  "Informatik & Kommunikationstechnologie") ~ 1,
    !(fachbereich %in% c("Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                         "Naturwissenschaften, Mathematik und Statistik",
                         "Informatik & Kommunikationstechnologie")) ~ 1
  ))


# übersetzen
dat <- dat %>%
  dplyr::mutate(
    geschlecht = dplyr::case_when(
      geschlecht == "Male" ~ "Männer",
      geschlecht == "Female" ~ "Frauen",
      geschlecht == "Total" ~ "Gesamt",
      T ~ geschlecht
    )
  )

# indikator nach akademisch vs. berufsorientiert ergänzen
dat$indikator <- ifelse(grepl("berufsorientiert", dat$anforderung) |
                          grepl("usbildung", dat$anforderung), "berufsorientiert", "akademisch")

dat$indikator <- ifelse(dat$anforderung == "kurzes tertiäres Bildungsprogramm (ISCED 5)" |
                          dat$anforderung == "Bachelor oder vergleichbar (ISCED 6)" |
                          dat$anforderung == "Master oder vergleichbar (ISCED 7)" |
                          dat$anforderung == "tertiäre Bildung (gesamt)", "Alle", dat$indikator)

# filtern nach akademisch
# dat <- dat %>%
#   dplyr::filter(indikator == "akademisch") %>%
#   dplyr::select(-indikator)

# missings ausfiltern
dat <- na.omit(dat)
dat$wert <- round(dat$wert)

# bereich ergänze und in Reihenfolge bringen
dat$bereich <- "Studium"
dat$indikator <- "Anzahl Studierende"
dat$quelle <- "OECD"
dat$population <- "OECD"
dat$typ <- "Anzahl"


# Spalten in logische Reihenfolge bringen
dat<- dat[,c("bereich", "quelle", "typ", "indikator", "mint_select",
             "ebene", "fachbereich", "fach",
             "geschlecht", "population", "land_code", "land", "jahr", "anforderung", "wert")]
studierende_anzahl_oecd <- dat

# speichern
usethis::use_data(studierende_anzahl_oecd, overwrite = T)

save(studierende_anzahl_oecd, file =  "studierende_anzahl_oecd.rda")











iscedf13_transform_kurz <- function(dat) {

  # fächer benennen

  dat <- dat %>%
    mutate(fach = case_when(stringr::str_ends("F00", dat$fach)~ "Allgemeine Bildungsgänge und Qualifikationen",
                            stringr::str_ends("F01", dat$fach)~ "Pädagogik",
                            stringr::str_ends("F02", dat$fach) ~ "Geisteswissenschaften und Künste",
                            stringr::str_ends("F03", dat$fach) ~ "Sozialwissenschaften, Journalismus und Informationswesen",
                            stringr::str_ends("F04", dat$fach) ~ "Wirtschaft, Verwaltung und Recht",
                            stringr::str_ends("F05", dat$fach) ~ "Naturwissenschaften, Mathematik und Statistik",
                            stringr::str_ends("F06", dat$fach) ~ "Informatik & Kommunikationstechnologie",
                            stringr::str_ends("F07", dat$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                            stringr::str_ends("F08", dat$fach) ~ "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin",
                            stringr::str_ends("F09", dat$fach) ~ "Gesundheit, Medizin und Sozialwesen",
                            stringr::str_ends("F10", dat$fach) ~ "Dienstleistungen",

                            stringr::str_ends("F05T07", dat$fach) ~ "MINT",
                            stringr::str_detect("TOTAL", dat$fach) ~ "Alle",
                            stringr::str_detect("_T", dat$fach) ~ "Alle",
                            stringr::str_ends("UNK", dat$fach) ~ "Unbekannt",
                            stringr::str_ends("F99", dat$fach) ~ "Unbekannt",
                            T ~ dat$fach))

  return(dat)
}

##





#### Erstelle arbeitsmarkt_anfaenger_absolv_oecd -------------

pfad <- "C:/Users/tko/OneDrive - Stifterverband/2_MINT-Lücke schließen/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/"


# akro <- "kbr"
#data <- read.csv(paste0(pfad,"OECD003_Anteil_Absol_nach_Feld_an_allen_Feldern_OECD.csv"),
#               header = TRUE, sep = ",", dec = ".")

dat <- read.csv(paste0(pfad,"OECD008_studis_azubis_nach_fach.csv"),
                header = TRUE, sep = ",", dec = ".")



# dat <- dat %>%
#   dplyr::select(COUNTRY, Country, Indicator, EDUCATION_LEV, Gender, EDUCATION_FIELD,
#                 Year, Value) %>%
#   dplyr::rename(land_code = COUNTRY,
#                 land = Country,
#                 anforderung = EDUCATION_LEV,
#                 geschlecht = Gender,
#                 fach = EDUCATION_FIELD,
#                 variable = Indicator,
#                 jahr = Year,
#                 wert = Value)





dat <- dat %>%
  dplyr::select(REF_AREA, Reference.area, EDUCATION_LEV, Sex, EDUCATION_FIELD,
                TIME_PERIOD, OBS_VALUE, Measure, STRUCTURE_NAME) %>%
  dplyr::rename(land_code = REF_AREA,
                land = Reference.area,
                anforderung = EDUCATION_LEV,
                variable = STRUCTURE_NAME,
                geschlecht = Sex,
                fach = EDUCATION_FIELD,
                jahr = TIME_PERIOD,
                wert = OBS_VALUE) %>%
  dplyr::filter(
    Measure == "Students enrolled"
  ) %>%
  select(-Measure)

# Land zuweisen / übersetzen
dat_agg <- dat %>%
  dplyr::filter(land_code %in% c("E22", "OAVG")) %>%
  dplyr::mutate(land = dplyr::case_when(
    land_code == "E22" ~ "OECD-Mitglieder aus der EU",
    land_code == "OAVG" ~ "OECD"
  ))
dat <- dat %>% dplyr::filter(!(land_code %in% c("E22", "OAVG")))
dat$land <- countrycode::countryname(dat$land, destination = "country.name.de")

dat <- rbind(dat, dat_agg)

# Anforderungsniveau zuweisen
dat <- dat %>%
  dplyr::mutate(anforderung = dplyr::case_when(
    anforderung ==  "ISCED11_35" ~ "Erstausbildung (ISCED 35)",
    anforderung ==  "ISCED11_45" ~ "Ausbildung (ISCED 45)",
    anforderung ==  "ISCED11_5" ~ "kurzes tertiäres Bildungsprogramm (ISCED 5)",
    anforderung ==  "ISCED11_6" ~ "Bachelor oder vergleichbar (ISCED 6)",
    anforderung ==  "ISCED11_7" ~ "Master oder vergleichbar (ISCED 7)",
    anforderung ==  "ISCED11_8" ~ "Promotion (ISCED 8)",
    anforderung == "ISCED11_5T8" ~ "tertiäre Bildung (gesamt)"
  ))

# Fachbereich zuweisen - mit Kekelis Funktion
dat <- iscedf13_transform_lang(dat)

# übersetzen
dat <- dat %>%
  dplyr::mutate(
    geschlecht = dplyr::case_when(
      geschlecht == "Male" ~ "Männer",
      geschlecht == "Female" ~ "Frauen",
      geschlecht == "Total" ~ "Gesamt",
      T ~ geschlecht
    ),
    variable = dplyr::case_when(
      variable == "Distribution of new entrants by field of education" ~
        "Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern",
      variable == "Share of graduates by field" ~
        "Anteil Absolvent*innen nach Fach an allen Fächern",
      variable == "Share of graduates by gender in fields of education" ~
        "Frauen-/Männeranteil Absolvent*innen nach Fachbereichen",
      variable == "Share of new entrants for each field of education by gender" ~
        "Frauen-/Männeranteil Ausbildungs-/Studiumsanfänger*innen nach Fachbereichen",
      variable == "Number of enrolled students, graduates and new entrants by field of education",
      T ~ variable
    )
  )

# missings ausfiltern
dat <- na.omit(dat)

# bereich ergänze und in Reihenfolge bringen
dat$bereich <- "Arbeitsmarkt"
dat$typ <- "In Prozent"
dat$quelle <- "OECD"
dat$population <- "OECD"
# dat$indikator <- "Alle"


# Spalten in logische Reihenfolge bringen
dat<- dat[,c("bereich", "quelle", "variable", "typ", "fach",
             "geschlecht", "population", "land", "jahr", "anforderung", "wert")]
colnames(dat)[5] <- "fachbereich"

# mint berechnen
mint <- dat %>%
  dplyr::filter(!(grepl("Frauen-/Männ", variable))) %>%
  dplyr::filter(fachbereich %in% c("Naturwissenschaften, Mathematik und Statistik",
                                   "Informatik & Kommunikationstechnologie",
                                   "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe")) %>%
  dplyr::group_by(bereich, quelle, variable, typ, geschlecht, population, land, jahr, anforderung) %>%
  dplyr::summarise(wert = sum(wert)) %>%
  dplyr::mutate(fachbereich = "MINT") %>%
  dplyr::ungroup()
nicht_mint <- dat %>%
  dplyr::filter(!(grepl("Frauen-/Männ", variable))) %>%
  dplyr::filter(!(fachbereich %in% c("Naturwissenschaften, Mathematik und Statistik",
                                     "Informatik & Kommunikationstechnologie",
                                     "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                     "Alle"))) %>%
  dplyr::group_by(bereich, quelle, variable, typ, geschlecht, population, land, jahr, anforderung) %>%
  dplyr::summarise(wert = sum(wert)) %>%
  dplyr::mutate(fachbereich = "Alle Bereiche außer MINT") %>%
  dplyr::ungroup()

dat <- rbind(dat, mint, nicht_mint)

# umbenennen
arbeitsmarkt_anfaenger_absolv_oecd <- dat

# speichern
save(arbeitsmarkt_anfaenger_absolv_oecd, file = "arbeitsmarkt_anfaenger_absolv_oecd.rda")
usethis::use_data(arbeitsmarkt_anfaenger_absolv_oecd, overwrite = T)








