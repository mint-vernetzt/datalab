library(data.table)
# library(SnowballC)
library(tm)
# library(splitstackshape)
library(stringr)
library(dplyr)
# library(tidytext)
#library(tokenizers)

# pfad ändern
pfad_kab <- "C:/Users/kab/OneDrive - Stifterverband/AP7 MINT-DataLab/02 Datenmaterial/04_Input_Suchfunktion/"
pfad_kbr <- "C:/Users/kbr/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/04_Input_Suchfunktion/"

pfad <- pfad_kab

suchtabelle <- read.csv2(paste0(pfad, "Suchtabelle.csv"))

# vektor mit cols, die relvant für suche sind für spätere select-operation
relevant <- c("Tab.Name",
#suchtabelle$Titel,
"Plotart",
"Interpret..Hover",
"Tags.I",
"Tags.II",
"Tags.III",
"Tags.IV")


suchtabelle <- suchtabelle %>%
  # neue col "Rgisterkarte
  mutate(Registerkarte = Tab.Name )%>%
  # lowecase
  mutate(across(relevant, ~ tolower(.) ))%>%
  # stopwords raus
  mutate(across(relevant, ~ tm::removeWords(., stopwords("german"))))%>%
  mutate(across(relevant, ~ str_trim(.)))%>%
  # white spaces raus
  mutate(across(relevant, ~ str_replace_all(., " ", ",")))%>%
  mutate(across(relevant, ~ str_replace_all(., ",,,", ",")))%>%
  mutate(across(relevant, ~ str_replace_all(., ",,", ",")))%>%
  # comb als summe aller relevanter serach cols
  mutate(comb= paste(.$Tab.Name,
                      #suchtabelle$Titel,
                      .$Plotart,
                      .$Interpret..Hover,
                      .$Tags.I,
                      .$Tags.II,
                      .$Tags.III,
                      .$Tags.IV,
                      sep = "," ))



  # satzzeichen aus search term raus
   l <- quanteda::tokens(suchtabelle$comb, remove_punct = TRUE)
  # stemming
   l <- quanteda::tokens_wordstem(l, language = "de")

  # tokes object in separaten dataframe überführen
  term <- data.frame(
    id = seq_along(l),
    term = sapply(l, paste, collapse = " "),
    row.names = NULL
  )

# term und suchrabelle zusammenführen
suchtabelle <- bind_cols(suchtabelle, term)

# commas raus?

# Save data
save(suchtabelle, file = "data/suchtabelle.rda")
