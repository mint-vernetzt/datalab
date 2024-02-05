# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)

# world_map <- highcharter::download_map_data(url = "custom/world", showinfo = FALSE)
# europa_map <- highcharter::download_map_data(url = "custom/europe", showinfo = FALSE)
# germany_map <- highcharter::download_map_data( url = "countries/de/de-all", showinfo = FALSE)

datalab::run_app(
  # world_map = world_map,
  # europa_map = europa_map,
  # germany_map = germany_map
)


