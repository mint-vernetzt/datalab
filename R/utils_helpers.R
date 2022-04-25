#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

dictionary_title_studium_studentenzahl <- list("eingeschrieben" = "die insgesamt eingeschrieben sind",
                                               "1hs" = "die im 1. Hochschulsemester eingeschrieben sind",
                                               "1fs" = "die im 1. Fachsemester eingeschrieben sind")


#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

dictionary_title_studium_abschluss <- list("mathe_natwi" = "Mathe & Naturwissenschaften",
                                               "ingenieur" = "Ingenieurswesen")


#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}
