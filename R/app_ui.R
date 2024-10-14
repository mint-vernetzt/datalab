#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
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
          "           MINT-DataLab",
          style = "text-align: justify; color:#154194; font-size: 22; font-family: SourceSans3-Bold;"
        )
        #,

        # shiny::tags$li(
        #   class = "dropdown",
        #   id = "search-input-all",
        #   mod_suche_eingabe_ui("suche_eingabe_1")
        # )
      ),


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
          shinydashboard::menuItem("Fokus: MINT International", tabName = "international"),
          shinydashboard::menuItem("Fokus: MINT-Fachkräfte",  tabName = "fachkraft"),
        # shinydashboard::menuItem("BETA: Suche",  tabName = "suche"),
          shinydashboard::menuItem("Hinweise & Datenquellen",  tabName = "quellen")
        #,
          #shinydashboard::menuItem("BETAVERSION", tabName = "BETAVERSION")
          #shinydashboard::menuItem("Impressum",  tabName = "impressum")
          #shinydashboard::menuItem("Datenschutz", tabName = "datenschutz")


        )
      ),

      # Show the appropriate tab's content in the main body of our dashboard when we select it
      body = shinydashboard::dashboardBody(
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

          shinydashboard::tabItems(
          shinydashboard::tabItem(tabName ="startseite", mod_startseite_ui("startseite_ui_1")),
          shinydashboard::tabItem(tabName ="home", mod_home_ui("home_ui_1")),
          shinydashboard::tabItem(tabName ="schule", mod_schule_ui("schule_ui_1")),
          shinydashboard::tabItem(tabName ="studium", mod_studium_ui("studium_ui_1")),
          # shinydashboard::tabItem(tabName ="ausbildung", mod_ausbildung_ui("ausbildung_ui_1")),
          shinydashboard::tabItem(tabName ="beruf", mod_beruf_ui("beruf_ui_1")),
          shinydashboard::tabItem(tabName = "international", mod_international_ui("mod_international_ui_1")),
          shinydashboard::tabItem(tabName ="fachkraft", mod_fachkraft_ui("fachkraft_ui_1")),

          # shinydashboard::tabItem(tabName ="suche", mod_suche_ui("suche_1")),
          shinydashboard::tabItem(tabName ="quellen", mod_quellen_ui("quellen_ui_1")),
          #shinydashboard::tabItem(tabName ="BETAVERSION", mod_betaversion_ui("betaversion_ui_1")),

          shinydashboard::tabItem(tabName ="kontakt", mod_kontakt_ui("kontakt_ui_1")),
          shinydashboard::tabItem(tabName ="impressum", mod_impressum_ui("impressum_ui_1")),
          shinydashboard::tabItem(tabName ="datenschutz", mod_datenschutz_ui("datenschutz_ui_1"))
        )
      )
))
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
