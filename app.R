# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

logger::log_info("load package")
pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)

logger::log_info("Laden von Karte: Start")
world_map <- highcharter::download_map_data(url = "custom/world", showinfo = FALSE)
europa_map <- highcharter::download_map_data(url = "custom/europe", showinfo = FALSE)
germany_map <- highcharter::download_map_data( url = "countries/de/de-all", showinfo = FALSE)
logger::log_info("Laden von Karte: Done")


logger::log_info("Run the application")
run_app(world_map = world_map,
        europa_map = europa_map,
        germany_map = germany_map)
