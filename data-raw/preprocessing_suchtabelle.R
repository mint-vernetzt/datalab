library(data.table)

suchtabelle <- read.csv2("data-raw/raw/Suchtabelle.csv")


suchtabelle$term <- paste0(suchtabelle$Tab.Name,
                         suchtabelle$Titel,
                         suchtabelle$Plotart,
                         suchtabelle$Interpret..Hover,
                         suchtabelle$Tags.I,
                         suchtabelle$Tags.II,
                         suchtabelle$Tags.III)

# Save data
save(suchtabelle, file = "data/suchtabelle.rda")
