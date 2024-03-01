#' A function to search relevants subpages
#'
#' @description A function that searches the given keywords and filters the lookup table to present suitable subpages and plots
#'
#' @return The return value is a plot
#' @param term a string with the search term
#' @noRd

get_search_data <- function(term, session) {
  # lookup table
  # term <- "test"
  # term <- ""

  # term zu lower case
  this_search <- tolower(term)
  # stopwords raus, satzzeichen raus, stemming
  this_search <- tm::removeWords(this_search, tm::stopwords("german"))
  this_search <- quanteda::tokens(this_search, remove_punct = TRUE)
  this_search <- quanteda::tokens_wordstem(this_search, language = "de")
  this_search <- paste(this_search)


  # this_search <- SnowballC::wordStem(this_search, language = "de")
  # this_search <- paste0(unlist(strsplit(x = this_search, split = " ")))
  # this_search <- "speed car distance"
  search_text <- suchtabelle$term
  # search for each term and then return any findings
  search_idx <- lapply(
    X = this_search,
    FUN = function(term) {
      grepl(pattern = term, search_text
            #, max.distance = 2
            )
    }
  )
  # combine searches and only thos with all search terms present are used
  search_idx <- Reduce("&", search_idx)

  if (is.null(search_idx)) {
    out <- suchtabelle
  } else {
    out <- suchtabelle[search_idx,]
  }

  out <- out %>%
    dplyr::select(Bereich, Registerkarte, Plotart, menuItem..tabName, Box..ID)%>%
    dplyr::mutate(Plotart = stringr::str_to_title(Plotart))%>%
    dplyr::mutate(Grafiktyp = Plotart)

  return(out)
}

