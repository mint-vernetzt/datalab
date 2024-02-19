#' A function to search relevants subpages
#'
#' @description A function that searches the given keywords and filters the lookup table to present suitable subpages and plots
#'
#' @return The return value is a plot
#' @param term a string with the search term
#' @noRd

get_search_data <- function(term, session) {
  # lookup table
  # term <- "International"
  # term <- ""


  this_search <- tolower(term)
  this_search <- tm::removeWords(this_search, tm::stopwords("german"))
  this_searh <- SnowballC::wordStem(this_search, language = "de")
  this_search <- paste0(unlist(strsplit(x = this_search, split = " ")))
  # this_search <- "speed car distance"
  search_text <- tolower(suchtabelle$term)


  # search for each term and then return any findings
  search_idx <- lapply(
    X = this_search,
    FUN = function(term) {
      agrepl(pattern = term, search_text, max.distance = 1)
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
    dplyr::select(Bereich, Tab.Name, Plotart, menuItem..tabName, Box..ID)

  return(out)
}
