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
          "    MINT-DataLab",
          style = "text-align: justify; color:#154194;"
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
          id = "tabs",
          shinydashboard::menuItem("Startseite",  tabName = "startseite"),
          shinydashboard::menuItem("Alle Bildungsbereiche",  tabName = "home"),
          shinydashboard::menuItem("Schule",  tabName = "schule"),
          shinydashboard::menuItem("Studium",  tabName = "studium"),
          # shinydashboard::menuItem("Ausbildung", icon = shiny::icon("pencil-ruler"), tabName = "ausbildung"),
          shinydashboard::menuItem("Ausbildung & Beruf",  tabName = "beruf"),
          shinydashboard::menuItem("Hinweise & Datenquellen",  tabName = "quellen"),
          shinydashboard::menuItem("BETAVERSION", tabName = "BETAVERSION")
          #shinydashboard::menuItem("Impressum",  tabName = "impressum")
          #shinydashboard::menuItem("Datenschutz", tabName = "datenschutz")


        )
      ),







      # Show the appropriate tab's content in the main body of our dashboard when we select it
      body = shinydashboard::dashboardBody(
        #so sollte man alle Fraben aller Elemente (Sidebar, obere Leiste über dem Banner, Hintergrund) ändern können, aber das einzige, das funktioniert, ist der Hintergrund der App (habe ich deshalb in /* */ gesetzt)
        tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #FFFFFF;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #FFFFFF;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #FFFFFF;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #FFFFFF;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #FFFFFF;
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #FFFFFF;
                                color: #000000;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #154194;
                                color: #FFFFFF;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #154194;
                                }

                                /*body*/
                                .content-wrapper, .right-side {
                                background-color: #FFFFFF;
                                }

                                '))),
          shinydashboard::tabItems(
          shinydashboard::tabItem(tabName ="startseite", mod_startseite_ui("startseite_ui_1")),
          shinydashboard::tabItem(tabName ="home", mod_home_ui("home_ui_1")),
          shinydashboard::tabItem(tabName ="schule", mod_schule_ui("schule_ui_1")),
          shinydashboard::tabItem(tabName ="studium", mod_studium_ui("studium_ui_1")),
          # shinydashboard::tabItem(tabName ="ausbildung", mod_ausbildung_ui("ausbildung_ui_1")),
          shinydashboard::tabItem(tabName ="beruf", mod_beruf_ui("beruf_ui_1")),
          shinydashboard::tabItem(tabName ="quellen", mod_quellen_ui("quellen_ui_1")),


          shinydashboard::tabItem(tabName ="BETAVERSION", mod_betaversion_ui("betaversion_ui_1")),
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
