

"C:/Program Files (x86)/Microsoft/Edge/Application"


#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shiny.info
#' @noRd
app_ui <- function(request) {
  logger::log_info("Start APP UI")
  logger::log_threshold(level = Sys.getenv("LOG_LEVEL", "INFO"))

#Sys.setenv(CHROMOTE_CHROME = "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Your application UI logic
    shinydashboard::dashboardPage(
      skin = "blue",
      #title="MINTvernetzt - DataLab",

      header = shinydashboard::dashboardHeader(
          title = div(
          img(
            src = "www/MINTvernetztLogo_klein.png",
            height = 45
          ),
          "    MINT-DataLab",
          style = "text-align: justify; color:#154194;"
        ),

        shiny::tags$li(
          class = "dropdown",
          id = "search-input-all",
          mod_suche_eingabe_ui("suche_eingabe_1")
        )
      ),


      # Create our navigation menu that links to each of the tabs we defined --- MIT ICONS
      # sidebar = shinydashboard::dashboardSidebar(
      #   shinydashboard::sidebarMenu(
      #     # Setting id makes input$tabs give the tabName of currently-selected tab
      #     id = "tabs",
      #     shinydashboard::menuItem("Startseite", icon = shiny::icon("door-open"), tabName = "startseite"),
      #     shinydashboard::menuItem("Alle Bildungsbereiche", icon = shiny::icon("warehouse"), tabName = "home"),
      #     shinydashboard::menuItem("Schule im Detail", icon = shiny::icon("school"), tabName = "schule"),
      #     shinydashboard::menuItem("Studium im Detail", icon = shiny::icon("university"), tabName = "studium"),
      #     # shinydashboard::menuItem("Ausbildung", icon = shiny::icon("pencil-ruler"), tabName = "ausbildung"),
      #     shinydashboard::menuItem("Ausbildung & Beruf im Detail", icon = shiny::icon("building"), tabName = "beruf"),
      #     shinydashboard::menuItem("Hinweise & Datenquellen", icon = shiny::icon("book"), tabName = "quellen"),
      #     shinydashboard::menuItem("BETAVERSION", icon = shiny::icon("gear"), tabName = "BETAVERSION")
      #     #shinydashboard::menuItem("Impressum", icon = shiny::icon("address-card"), tabName = "impressum")
      #     #shinydashboard::menuItem("Datenschutz", icon = shiny::icon("address-card"), tabName = "datenschutz")
      #
      #
      #   )
      # ),

      # Create our navigation menu that links to each of the tabs we defined
      sidebar = shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          # Setting id makes input$tabs give the tabName of currently-selected tab
          style = "position: fixed;",
          id = "tabs",
          shinydashboard::menuItem("Startseite",  tabName = "startseite"),
          shinydashboard::menuItem("Alle Bildungsbereiche",  tabName = "home"),
          shinydashboard::menuItem("Schule",  tabName = "schule"),
          shinydashboard::menuItem("Studium",  tabName = "studium"),
          # shinydashboard::menuItem("Ausbildung", icon = shiny::icon("pencil-ruler"), tabName = "ausbildung"),
          shinydashboard::menuItem("Ausbildung & Beruf",  tabName = "beruf"),
          shinydashboard::menuItem("International",  tabName = "international"),
          shinydashboard::menuItem("Fachkräfte",  tabName = "fachkraft"),
          shinydashboard::menuItem("Hinweise & Datenquellen",  tabName = "quellen"),
          shinydashboard::menuItem("Suche",  tabName = "suche"),
          shinydashboard::menuItem("BETAVERSION", tabName = "BETAVERSION"),
          #shinydashboard::menuItem("Impressum",  tabName = "impressum")
          #shinydashboard::menuItem("Datenschutz", tabName = "datenschutz")
          tags$li(class = "dropdown",
                  tags$li(
                    class = "dropdown",
                    shiny::uiOutput("debug")
                  )
          )

        )
      ),

      # Show the appropriate tab's content in the main body of our dashboard when we select it
      body = shinydashboard::dashboardBody(
        # display shiny version
        shiny.info::version(ver = "1.0.0:9006",
                            position = "bottom left"),

        # Matomo einbinden
        tags$head(HTML(
          "<script>
      var _paq = _paq || [];
      _paq.push(['trackPageView']);
      _paq.push(['enableLinkTracking']);
      _paq.push(['enableHeartBeatTimer']);
      (function() {
        var u ='https://analytics.datalab.mint-vernetzt.de/';
        _paq.push(['setTrackerUrl', u+'matomo.php']);
        _paq.push(['setSiteId', '1']);
        var d=document,
            g=d.createElement('script'),
            s=d.getElementsByTagName('script')[0];
            g.type='text/javascript';
            g.async=true; g.defer=true;
            g.src=u+'matomo.js';
            s.parentNode.insertBefore(g,s);
      })();
      var _mtm = window._mtm = window._mtm || [];
          _mtm.push({'mtm.startTime': (new Date().getTime()), 'event': 'mtm.Start'});
          var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
          g.async=true; g.src='https://analytics.datalab.mint-vernetzt.de/js/container_wPdOg8s5.js'; s.parentNode.insertBefore(g,s);
    </script>"

        )),
        tags$script(HTML(
          "$('body').addClass('fixed');"
          )),
        tags$script(HTML("
          Shiny.addCustomMessageHandler('delayedScroll', function(message) {
            setTimeout(function() {
              document.getElementById(message.id).scrollIntoView({
                block: 'start',
                behavior: 'smooth',
              });
            }, message.delay);
          });
        ")),

          shinydashboard::tabItems(
          shinydashboard::tabItem(tabName ="startseite", mod_startseite_ui("startseite_ui_1")),
          shinydashboard::tabItem(tabName ="home", mod_home_ui("home_ui_1")),
          shinydashboard::tabItem(tabName ="schule", mod_schule_ui("schule_ui_1")),
          shinydashboard::tabItem(tabName ="studium", mod_studium_ui("studium_ui_1")),
          # shinydashboard::tabItem(tabName ="ausbildung", mod_ausbildung_ui("ausbildung_ui_1")),
          shinydashboard::tabItem(tabName ="international", mod_international_ui("international_ui_1")),
          shinydashboard::tabItem(tabName ="fachkraft", mod_fachkraft_ui("fachkraft_1")),
          shinydashboard::tabItem(tabName ="beruf", mod_beruf_ui("beruf_ui_1")),
          shinydashboard::tabItem(tabName ="quellen", mod_quellen_ui("quellen_ui_1")),
          shinydashboard::tabItem(tabName ="suche", mod_suche_ui("suche_1")),


          shinydashboard::tabItem(tabName ="BETAVERSION", mod_betaversion_ui("betaversion_ui_1")),
          shinydashboard::tabItem(tabName ="kontakt", mod_kontakt_ui("kontakt_ui_1")),
          shinydashboard::tabItem(tabName ="impressum", mod_impressum_ui("impressum_ui_1")),
          shinydashboard::tabItem(tabName ="datenschutz", mod_datenschutz_ui("datenschutz_ui_1"))
          )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(
      ico = "mint-logo",
      rel = "shortcut icon",
      resources_path = "www",
      ext = "png"
    ),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "DataLab"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
