library(data.table)
library(TM)

# pfad ändern
pfad_kab <- "C:/Users/kab/OneDrive - Stifterverband/AP7 MINT-DataLab/02 Datenmaterial/04_Input_Suchfunktion/"

pfad <- pfad_kab

suchtabelle <- read.csv2(paste0(pfad, "Suchtabelle.csv"))


suchtabelle$term <- paste(suchtabelle$Tab.Name,
                         #suchtabelle$Titel,
                         suchtabelle$Plotart,
                         suchtabelle$Interpret..Hover,
                         suchtabelle$Tags.I,
                         suchtabelle$Tags.II,
                         suchtabelle$Tags.III,
                         suchtabelle$Tags.IV,
                         sep = ","
                         # ,
                         # collapse = ","
                         )
# # remove stopwords
# suchtabelle$term <- tm_map(suchtabelle$term, removeWords, stopwords("german"))
# # stem document
# suchtabelle$term <- tm_map(suchtabelle$term, stemDocument)
# # strip white spaces (always at the end)
# suchtabelle$term <- tm_map(suchtabelle$term, stripWhitespace)


# Save data
save(suchtabelle, file = "data/suchtabelle.rda")
