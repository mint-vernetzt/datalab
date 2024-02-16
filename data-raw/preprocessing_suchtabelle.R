library(data.table)
library(SnowballC)
library(splitstackshape)
library(stringr)
library(dplyr)
library(tidytext)
library(tokenizers)

# pfad ändern
pfad_kab <- "C:/Users/kab/OneDrive - Stifterverband/AP7 MINT-DataLab/02 Datenmaterial/04_Input_Suchfunktion/"

pfad <- pfad_kab

suchtabelle <- read.csv2(paste0(pfad, "Suchtabelle.csv"))

relevant <- c("Tab.Name",
#suchtabelle$Titel,
"Plotart",
"Interpret..Hover",
"Tags.I",
"Tags.II",
"Tags.III",
"Tags.IV")


suchtabelle <- suchtabelle %>%
  mutate(across(relevant, ~ tolower(.) ))%>%
  mutate(across(relevant, ~removeWords(., stopwords("german"))))%>%
  mutate(across(relevant, ~ str_trim(.)))%>%
  mutate(across(relevant, ~ str_replace_all(., " ", ",")))%>%
  mutate(across(relevant, ~ str_replace_all(., ",,,", ",")))%>%
  mutate(across(relevant, ~ str_replace_all(., ",,", ",")))%>%
  mutate(term = paste(.$Tab.Name,
                      #suchtabelle$Titel,
                      .$Plotart,
                      .$Interpret..Hover,
                      .$Tags.I,
                      .$Tags.II,
                      .$Tags.III,
                      .$Tags.IV,
                      sep = "," ))%>%
  mutate(across(term, ~ tokenize_words(.) ))


term <- suchtabelle$term

counter <- length(c(1:79))

stem_func <- function(counter) {
  out <- paste(SnowballC::wordStem(suchtabello$term[[counter]], language = "de"), collapse = ",")

  }

x <- purrr::map(.f=stem_func, .x=c(1:79))

suchtabelle <- suchtabelle %>%
  mutate(term = x)

# commas raus?

# Save data
save(suchtabelle, file = "data/suchtabelle.rda")
