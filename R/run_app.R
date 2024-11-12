#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart =  function() {
    # Sys.setlocale(category = "LC_ALL", locale = "German_Germany.utf8")
    #

    # con <<- DBI::dbConnect(RSQLite::SQLite(), "data/mint_db.sqlite", encoding = "UTF-8")
     con <<- DBI::dbConnect(duckdb::duckdb(), "data/mint_db.duckdb", read_only = TRUE)
     # con <<- DBI::dbConnect(duckdb::duckdb(), "mint_db.duckdb", read_only = TRUE)

     onStop(function() {
       DBI::dbDisconnect(con, shutdown = TRUE)

     })

  },
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",

  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
