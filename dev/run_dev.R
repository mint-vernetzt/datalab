require(logger)

logger::log_info("Setting options")
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
logger::log_info("Setting random Port")
options(shiny.port = httpuv::randomPort())

logger::log_info("Detach all loaded packages and clean your environment")
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

logger::log_info("Document and reload your package")
golem::document_and_reload()


logger::log_info("Laden von Karte: Start")
world_map <- highcharter::download_map_data(url = "custom/world", showinfo = FALSE)
europa_map <- highcharter::download_map_data(url = "custom/europe", showinfo = FALSE)
germany_map <- highcharter::download_map_data( url = "countries/de/de-all", showinfo = FALSE)
logger::log_info("Laden von Karte: Done")


logger::log_info("Run the application")
run_app(world_map = world_map,
        europa_map = europa_map,
        germany_map = germany_map)
