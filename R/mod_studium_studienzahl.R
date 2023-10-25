#' studium_studienzahl UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#'

tab1_name <- "Vergleich"

mod_studium_studienzahl_ui <- function(id){
  ns <- NS(id)
  tagList(

    # Banner
    fluidRow(
      shinydashboard::box(
        width = 12,
        img(src='www/Banner_Studium_BB.jpg',
            class = "img-responsive",
            #height = "150px", width = "150px",
            alt = "Banner Studium",
            style="display: block; margin-left: auto; margin-right: auto;"
        ))),


    # Info-Texte

    fluidRow(
      shinydashboard::box(
        title = "Auf dieser Seite",
        width = 7,
        p(style = "text-align: left; font-size = 16px",
          "Auf dieser Seite zeigen wir statistische Kennzahlen zum Thema MINT-Fächer studieren. Wir zeigen, wie hoch der Anteil
           von MINT-Fächern gemessen an allen gewählten Studienfächern ist. Dazu zeigen wir Vergleiche nach männlichen und
           weiblichen Studierenden, einzelnen Fächern und nach Bundesländern.")
        ),

      shinydashboard::box(
        title = "Fragen oder Feedback?",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          "Sind alle Zahlen und Grafiken verständlich dargestellt?", br(), "Wir freuen uns über Rückfragen oder Feedback ", tags$a(href = "mailto:katharina.brunner@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per Email"),"oder über unsere kurze",
          tags$a(href="https://survey.lamapoll.de/MINT-DataLab_Feedback/", "Umfrage", target="_blank"), "!"
        ))
    ),

    fluidRow(
      shinydashboard::box(
        title = "Übersicht Fragestellungen",
        width = 7,
        p(
          style = "text-align: left; font-size = 16px",tags$a(href="#studium_mint",
                                                              span(tags$b(span("Fächerwahl MINT:")))), "Wie hoch ist der Anteil von Studierenden, die ein MINT-Fach studieren?"
        ),

        p(style = "text-align: left; font-size = 16px",tags$a(href="#studium_fach",
                                                              span(tags$b(span("M-I-N-T:")))), "Blick auf die einzelnen Fächer und Fachbereiche."

        ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#studium_frauen",
                                                              span(tags$b(span("Frauen in MINT:")))), "Wie hoch ist der Anteil von Frauen in den MINT-Fächern?"
        ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#studium_international",
                                                              span(tags$b(span("Internationale Studierende in MINT:")))), "Wie hoch ist der Anteil von internationalen Studierenden in den MINT-Fächern?"
        )),

      shinydashboard::box(
        title = "Datenquellen",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          "Studierendenzahlen in Deutschland: Destatis 2023, auf Anfrage")

      )
    ),


  # Box 1

    fluidRow( id="studium_mint",
      shinydashboard::box(
        title = "Fächerwahl MINT: Wie hoch ist der Anteil von Studierenden, die ein MINT-Fach studieren?",
        width = 12,
        p("In diesen interaktiven Diagrammen beleuchten wir den Anteil von MINT-Fächern an allen Studienfächern in Deutschland.
          Dabei betrachten wir sowohl Studienanfänger:innen als auch Studierende allgemein. Darüber hinaus werfen wir ein Schlaglicht auf die Verteilung von Männern und Frauen in MINT"),
        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich Anteil MINT nach Studierendengruppen", br(),

                             shiny::sidebarPanel(width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_test_ui("mod_studium_studienzahl_test_ui_1")
                               ),
                             shiny::mainPanel(width = 9,
                               htmlOutput(ns("test")),
                               p(style="font-size:12px;color:grey",
                                 "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_studium_mint_1", title = "",
                                                  content = paste0("In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Für Studierende im Lehramt wird das belegte Hauptfach für die Kategorisierung in &quotMINT&quot oder &quotNicht MINT&quot betrachtet.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_studium_mint_1")
                        )),
                    tabPanel("Vergleich Anteil MINT nach Studierendengruppen II", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_choice_1_ui("mod_studium_studienzahl_choice_ui_1_1")),
                             shiny::mainPanel(
                               width = 9,
                               plotOutput(ns("plot_waffle")),
                               p(),
                               p(style="font-size:12px;color:grey",
                                 "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_studium_mint_2", title = "",
                                                  content = paste0("In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Für Studierende im Lehramt wird das belegte Hauptfach für die Kategorisierung in &quotMINT&quot oder &quotNicht MINT&quot betrachtet.", "<br> <br> Durch Rundungen kann es zu minimalen Abbweichungen zwischen den Grafiken kommen.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_studium_mint_2"))
                    ),

                    tabPanel("Vergleich Anteil MINT nach Studierendengruppen im Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_verlauf_ui("mod_studium_studienzahl_einstieg_verlauf_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_einstieg_verlauf")),
                               p(style="font-size:12px;color:grey",
                                 "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_studium_mint_3", title = "",
                                                  content = paste0("In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Für Studierende im Lehramt wird das belegte Hauptfach für die Kategorisierung in &quotMINT&quot oder &quotNicht MINT&quot betrachtet.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_studium_mint_3"))

                    ),

                    tabPanel("Vergleich Anteil MINT nach Bundesländern im Zeitverlauf", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_verlauf_bl_subject_ui("mod_studium_studienzahl_verlauf_bl_subject_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_verlauf_studienzahl_bl_subject"))
                               ,p(style="font-size:12px;color:grey",
                                  "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_studium_mint_4", title = "",
                                                  content = paste0("In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Für Studierende im Lehramt wird das belegte Hauptfach für die Kategorisierung in &quotMINT&quot oder &quotNicht MINT&quot betrachtet.", "<br> <br> Seit dem Jahr 2015 zählt Informatik zu den Ingenieurwissenschaften und nicht mehr zu den Naturwissenschaften.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_studium_mint_4")

                             )
                    ),

                    tabPanel("Vergleich Anteil MINT nach Bundesländer im Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_bl_verlauf_ui("mod_studium_studienzahl_bl_verlauf")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_studienzahl_bl_verlauf")),
                               p(style="font-size:12px;color:grey",
                                 "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_studium_mint_5", title = "",
                                                  content = paste0("In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Für Studierende im Lehramt wird das belegte Hauptfach für die Kategorisierung in &quotMINT&quot oder &quotNicht MINT&quot betrachtet.", "<br> <br> Seit dem Jahr 2015 zählt Informatik zu den Ingenieurwissenschaften und nicht mehr zu den Naturwissenschaften.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_studium_mint_5")
                             )
                    ),
                    tabPanel("Alle Studierendengruppen auf einen Blick", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_comparison_ui("mod_studium_studienzahl_einstieg_comparison_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_einstieg_comparison")),
                               p(style="font-size:12px;color:grey",
                                 "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_studium_mint_6", title = "",
                                                  content = paste0("In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Für Studierende im Lehramt wird das belegte Hauptfach für die Kategorisierung in &quotMINT&quot oder &quotNicht MINT&quot betrachtet.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_studium_mint_6"))
                    ),


                    tabPanel("Vergleich Studienfachwahl zwischen Frauen & Männer", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_choice_gender_ui("mod_studium_studienzahl_choice_gender_ui")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               plotOutput(ns("plot_waffle_choice_gender")),
                               p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_studium_mint_7", title = "",
                                                  content = paste0("In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Für Studierende im Lehramt wird das belegte Hauptfach für die Kategorisierung in &quotMINT&quot oder &quotNicht MINT&quot betrachtet.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_studium_mint_7")
                             )
                    ),

                    tabPanel("Studienfachwahl Frauen im Zeitverlauf", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_verlauf_bl_subject_gender_ui("mod_studium_studienzahl_verlauf_bl_subject_gender_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_verlauf_studienzahl_bl_subject_gender")),
                               p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_studium_mint_8", title = "",
                                                  content = paste0("In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Für Studierende im Lehramt wird das belegte Hauptfach für die Kategorisierung in &quotMINT&quot oder &quotNicht MINT&quot betrachtet.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_studium_mint_8")
                             )
                    ),
                    # tabPanel("Überblick Frauen und Männer", br(),
                    #
                    #          tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                    #          .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                    #          shiny::sidebarPanel(
                    #            width = 3,
                    #            mod_studium_studienzahl_ranking_bl_subject_gender_ui("mod_studium_studienzahl_ranking_bl_subject_gender_ui_1")
                    #          ),
                    #          shiny::mainPanel(
                    #            width = 9,
                    #            #highcharter::highchartOutput(ns("plot_ranking_studienzahl_bl_subject_gender1")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt.")
                    #            plotOutput(ns("plot_ranking_studienzahl_bl_subject_gender")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt.")
                    #          )
                    # )

                  # ,
                  #
                  #
                  # tabPanel("Datensatz", br(),
                  #
                  #          tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                  #              .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                  #          shiny::sidebarPanel(
                  #            tags$style(".well {background-color:#FFFFFF;}"),
                  #            tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                  #            mod_studium_studienzahl_einstieg_ui("mod_schule_kurse_einstieg_ui_1")),
                  #          shiny::mainPanel(
                  #            div(DT::dataTableOutput(ns("data_table_einstieg")),
                  #                style = "font-size: 75%; width: 75%"),
                  #            shiny::downloadButton(ns("download_data_box1"), label = "",
                  #                                  class = "butt",
                  #                                  icon = shiny::icon("download")))
                  # )
        ))),

    # Box 2

  fluidRow( id="studium_fach",
      shinydashboard::box(
        title = "M-I-N-T: Blick auf die einzelnen Fächer und Fachbereiche",
        width = 12,
        p("Hier zeigen wir, wie häufig MINT-Fächer im Vergleich zu anderen Studienfächern in Deutschland gewählt werden.
          Außerdem kann man den Anteil von MINT-Fächern zwischen den Bundesländern vergleichen."),

        tabsetPanel(type = "tabs",


                    tabPanel("TOP-10-Fächer", br(),
                        #tags$head(
                       # tags$style(
                       #   ".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                       #      .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                      shiny::sidebarPanel(
                        width = 3,
                        mod_studium_top_faecher_ui("mod_studium_top_faecher")),
                        # p(style = "font-size:12px;color:grey",
                        #   "Interpretationshilfe: In der ersten Einstellung sind die TOP-10-Fächer in Bayern in MINT bezogen auf den Frauen- bzw.
                        #   Männeranteil zu sehen. Die Fächer mit dem höchsten Frauenanteil in MINT sind Pharmazie (74 % Frauen) und Biologie (65 % Frauen).
                        #   Die Fächer mit dem höchsten Männeranteil
                        #   in MINT sind dagegen Verkehrstechnik / Nautik mit 86 % Männern und Elektrotechnik und Informationstechnik mit 84 %.")
                        # ),
                      shiny::mainPanel(
                        width = 9,
                        htmlOutput(ns("plot_top_faecher")),
                        p(style = "font-size:12px;color:grey",
                          "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                        br(),
                        shinyBS::bsPopover(id="h_studium_fach_1", title="",
                                           content = paste0("In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                           placement = "top",
                                           trigger = "hover"),
                        tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_studium_fach_1"))
                    ),

                    tabPanel("Vergleich Fächer (Karte)", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_bl_map_ui("mod_studium_studienzahl_bl_map"),

                             ),
                             shiny::mainPanel(
                               width = 9,
                               htmlOutput(ns("plot_studienzahl_map")),
                               p(style="font-size:12px;color:grey",
                                 "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),

                               shinyBS::bsPopover(id="h_studium_fach_2", title="",
                                                  content = paste0("In manchen Bundesländern sind einzelne Studienfachgruppen nicht definiert. In diesen Fällen werden die betroffenen Bundesländer als grau schattiert angezeigt.", "<br> <br> In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_studium_fach_2")
                             )
                    ),


                    # Fehler in der Boxgrösse, muss noch behoben werden

                   tabPanel("Alle Fächer auf einen Blick", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_ranking_bl_subject_ui("mod_studium_studienzahl_ranking_bl_subject_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_verlauf_studienzahl_bl_subject1"), height = "550px"),
                               p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt." ),
                               shinyBS::bsPopover(id="h_studium_fach_3", title="",
                                                  content = paste0("Durch Rundung kann es dazu kommen, dass einzelne Studienfachgruppen für bestimmte Indikatoren oder Regionen nicht in der Darstellung angezeigt werden.", "<br> <br> In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_studium_fach_3")
                               #plotOutput(ns("plot_ranking_bl_subject")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen.")
                             )
                    ),

                   tabPanel("MINT-Anteile im Zeitverlauf", br(),

                            tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                            shiny::sidebarPanel(
                              width = 3,
                              mod_studium_studienzahl_mint_anteile_ui("mod_studium_studienzahl_mint_anteile_ui")),
                            shiny::mainPanel(
                              width = 9,
                              highcharter::highchartOutput(ns("mint_anteil")),
                              p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt.")
                               ,
                              shinyBS::bsPopover(id="h_studium_fach_4", title="",
                                                 content = paste0("Durch Rundung kann es dazu kommen, dass einzelne Studienfachgruppen für bestimmte Indikatoren oder Regionen nicht in der Darstellung angezeigt werden.", "<br> <br> In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                 placement = "top",
                                                 trigger = "hover"),
                              tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_studium_fach_4")
                              #plotOutput(ns("plot_ranking_bl_subject")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen.")
                            )
                   ),

                    tabPanel("Vergleich nach Bundesländern", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_bl_vergleich_ui("studium_studienzahl_bl_vergleich")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_vergleich_bl1")),
                               p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),

                               shinyBS::bsPopover(id="h_studium_fach_5", title="",
                                                  content = paste0("In manchen Bundesländern sind einzelne Studienfachgruppen nicht definiert. In diesen Fällen werden nur die Bundesländer angezeigt, welche die betrachtete Studienfachgruppe aufweisen.", "<br><br> In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_studium_fach_5")

                             )
                    )
        ))),

    fluidRow(id="studium_frauen",
      shinydashboard::box(
        title = "Frauen in MINT: Wie hoch ist der Anteil von Frauen in den MINT-Fächern?",
        width = 12,
        p("Hier schauen wir uns die Verteilung von Frauen und Männern in Deutschland innerhalb der MINT-Studienfächer an.
          Zum Vergleich zeigen wir auch den Anteil in den anderen, nicht-MINT-Fächern.
          Die verschiedenen Diagramme bieten außerdem Fächer- und Bundeslandvergleiche."),

        tabsetPanel(type = "tabs",
                    tabPanel("Vergleich Anteil Frauen in MINT zwischen Studienanfänger:innen und Studierenden", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_gender_ui("mod_studium_studienzahl_einstieg_gender_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               htmlOutput(ns("plot_einstieg_pie_gender")),
                               p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_studium_frauen_1", title="",
                                                  content = paste0("In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Für Studierende im Lehramt wird das belegte Hauptfach für die Kategorisierung in &quotMINT&quot oder &quotNicht MINT&quot betrachtet.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_studium_frauen_1")
                               )
                    ),

                    tabPanel("Anteil Frauen im Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_verlauf_gender_ui("mod_studium_studienzahl_einstieg_verlauf_gender_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               highcharter::highchartOutput(ns("plot_einstieg_verlauf_gender")),
                               p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_studium_frauen_2", title="",
                                                  content = paste0("In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Für Studierende im Lehramt wird das belegte Hauptfach für die Kategorisierung in &quotMINT&quot oder &quotNicht MINT&quot betrachtet.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_studium_frauen_2")
                               )

                  ),

                  tabPanel("Anteil Frauen nach Bundesländern", br(),

                           shiny::sidebarPanel(
                             width = 3,
                             tags$style(".well  {background-color:#FFFFFF;}"),
                             tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                             mod_studium_studienzahl_einstieg_comparison_gender_ui("mod_studium_studienzahl_einstieg_comparison_gender_ui_1")),
                           shiny::mainPanel(
                             width = 9,
                             highcharter::highchartOutput(ns("plot_einstieg_comparison_gender")),
                             p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                             shinyBS::bsPopover(id="h_studium_frauen_3", title="",
                                                content = paste0("In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden.", "<br> <br> Studierende auf Lehramt werden für Fächergruppen, die nicht oder nur von einer sehr geringen Anzahlen an Lehramtstudierenden deutschlandweit als Hauptfach studiert werden, nicht in der Übersicht angezeigt. In einzelnen Bundesländern kann es für spezfisiche Fächergruppen zu keinen oder sehr geringen Leharamtstudierendenzahlen kommen. Achten Sie hierbei auf die absoluten Zahlenangaben zur Anzahl der Studierenden.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                placement = "top",
                                                trigger = "hover"),
                             tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_studium_frauen_3")
                             )
                  )
        ))),

  fluidRow(id="studium_international",
           shinydashboard::box(
             title = "Internationale Studierende in MINT: Wie hoch ist der Anteil internationaler Studierender in den MINT-Fächern?",
             width = 12,
             p("Diese Darstellungen zeigen, wie hoch der Anteil internationale Studierender an allen Studierenden eines Fachbereichs oder eines Studienfachs ist.
               Außerdem zeigen wir hier, wie viele internationale Studierende in welchen Fächern studieren. Als 'internationale Studierende' fassen wir alle
               Studierenden zusammen, welche in Deutschland studieren, aber keine deutsche Staatsbürgerschaft besitzen."),
             tabsetPanel(type = "tabs",
                         tabPanel("Anteil von internationalen Studierenden nach Fächern", br(),
                                  shiny::sidebarPanel(
                                    width = 3,
                                    tags$style(".well {background-color:#FFFFFF;}"),
                                    tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                                    mod_studium_studienzahl_ausl_ui("mod_studium_studienzahl_ausl_ui")
                                    ),
                                  shiny::mainPanel(
                                    width = 9,
                                    tags$head(tags$style(HTML(".small-box {height: 400px}"))),
                                    highcharter::highchartOutput(ns("plot_auslaender_test"), height = "650px"),

                                    p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                                    shinyBS::bsPopover(id="d_studium_international_1", title = "",
                                                       content = paste0("internationale Studierende = Studierende, die in Deutschland studieren aber keine deutsche Staatsbürgerschaft besitzen."),
                                                       placement = "top",
                                                       trigger = "hover"),
                                    tags$a(paste0("Definition der Begriffe"), icon("info-circle"), id = "d_studium_international_1"),
                                    p(),
                                    shinyBS::bsPopover(id="h_studium_international_1", title="",
                                                       content = paste0("In manchen Bundesländern sind einzelne Studienfachgruppen nicht definiert. In diesen Fällen werden nur die vorhandenen Studienfachgruppen angezeigt.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                       placement = "top",
                                                       trigger = "hover"),
                                    tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_studium_international_1"))
                                  )
                         ,

                         tabPanel("Anteil von internationalen Studierenden im Zeitvergleich", br(),

                                  shiny::sidebarPanel(
                                    width = 3,
                                    tags$style(".well  {background-color:#FFFFFF;}"),
                                    tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                                    mod_studium_studienzahl_ausl_zeit_ui("mod_studium_studienzahl_ausl_zeit_ui")),
                                  shiny::mainPanel(
                                    width = 9,
                                    highcharter::highchartOutput(ns("plot_auslaender_zeit")),
                                    p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                                    shinyBS::bsPopover(id="d_studium_international_2", title = "",
                                                       content = paste0("internationale Studierende = Studierende, die in deutschland studieren aber keine deutsche Staatsbürgerschaft besitzen."),
                                                       placement = "top",
                                                       trigger = "hover"),
                                    tags$a(paste0("Definition der Begriffe"), icon("info-circle"), id = "d_studium_international_2"),
                                    p(),
                                    shinyBS::bsPopover(id="h_studium_international_2", title="",
                                                       content = paste0("In manchen Bundesländern sind einzelne Studienfachgruppen nicht definiert. In diesen Fällen werden nur die vorhandenen Studienfachgruppen angezeigt.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                       placement = "top",
                                                       trigger = "hover"),
                                    tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_studium_international_2"))

                                  )

                         # tabPanel("Anteil von internationalen Studierenden im Zeitgergleich", br(),
                         #          shiny::sidebarPanel(
                         #            width = 3,
                         #            tags$style(".well {background-color:#FFFFFF;}"),
                         #            tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                         #            mod_studium_studienzahl_ausl_zeit_ui("mod_studium_studienzahl_ausl_zeit_ui_1")),
                         #          shiny::mainPanel(
                         #            width = 9,
                         #            tags$head(tags$style(HTML(".small-box {height: 400px}"))),
                         #            highcharter::highchartOutput(ns("plot_auslaender_zeit")), p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."))



             ))),




    #Footer
  funct_footer()
  ) # Tagslist zu



    # fluidRow(
    #   shinydashboard::box(
    #     title = "Nicht zuordbar",
    #     width = 12,
    #     p("Hier finden Sie den Anteil an Belegungen von Frauen und Männern in MINT-Fächern für die Bundesländer im Vergleich. "),
    #     tabsetPanel(type = "tabs",
    #                 tabPanel("Karte", br(),
    #
    #                          shiny::sidebarPanel(
    #                            mod_studium_studienzahl_bl_map_gender_ui("mod_studium_studienzahl_bl_map_gender")
    #                          ),
    #                          shiny::mainPanel(
    #                            htmlOutput(ns("plot_studienzahl_map_gender"))
    #                          )
    #                 ),
    #                 tabPanel("Vergleich (Bundesländer)", br(),
    #
    #                          shiny::sidebarPanel(
    #                            mod_studium_studienzahl_bl_verlauf_gender_ui("mod_studium_studienzahl_bl_verlauf_gender")
    #                          ),
    #                          shiny::mainPanel(
    #                            highcharter::highchartOutput(ns("plot_studienzahl_bl_verlauf_gender"))
    #                          )
    #                 ),
    #
    #                 tabPanel("Überblick (Bundsländer)", br(),
    #
    #                          shiny::sidebarPanel(
    #                            mod_studium_studienzahl_bl_vergleich_gender_ui("mod_studium_studienzahl_bl_vergleich_gender_ui")
    #                          ),
    #                          shiny::mainPanel(
    #                            plotOutput(ns("plot_ranking_studienzahl_bl_vergleich_gender"))
    #                          )
    #                 ),
    #
    #
    #
    #                 # zeigt Anteile MINT und nicht Gender
    #
    #                 tabPanel("Überblick (doppelt)", br(),
    #
    #                          shiny::sidebarPanel(
    #                            tags$style(".well {background-color:#FFFFFF;}"),
    #                            tags$head(tags$style(HTML(".small-box {height: 140px}"))),
    #                            mod_studium_studienzahl_einstieg_comparison_gender_ui("mod_studium_studienzahl_einstieg_comparison_gender_ui_1")),
    #                          shiny::mainPanel(
    #                            plotOutput(ns("plot_einstieg_comparison_gender")))
    #
    #                 ),
    #
    #
    #     ))),


}

#' studium_studienzahl Server Functions
#'
#' @noRd
mod_studium_studienzahl_server <- function(id, data_studierende,
                                           data_studierende_detailliert,
                                           r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    # Box 2
    output$plot_einstieg_pie <- renderUI({
      studienzahl_einstieg_pie(data_studierende,r)
    })

    output$plot_einstieg_verlauf <- highcharter::renderHighchart({
      studienzahl_verlauf_single(data_studierende,r)
    })#

    # all_mint_23_react <- reactive({
    #   studienzahl_all_mint_23(data_studierende2, r)
    # })



    output$plot_einstieg_comparison <- highcharter::renderHighchart({
      studienzahl_einstieg_comparison(data_studierende,r)#
    })

    # data_table_einstieg_react <- reactive({
    #   data_einstieg(data_studierende, r)
    # })

    # output$data_table_einstieg <- DT::renderDT({
    #   data_table_einstieg_react()
    # })

    # Box 3
    output$plot_einstieg_pie_gender <- renderUI({
      studienzahl_einstieg_pie_gender(data_studierende,r)#
    })

    output$plot_einstieg_verlauf_gender <- highcharter::renderHighchart({
      studienzahl_verlauf_single_gender(data_studierende,r)#
    })

    output$plot_einstieg_comparison_gender <- highcharter::renderHighchart({
      studienzahl_einstieg_comparison_gender(data_studierende_detailliert,r)
    })

    output$plot_verlauf_studienzahl_bl_subject1 <- highcharter::renderHighchart({
      ranking_bl_subject(data_studierende_detailliert,r)
    })


    # output$plot_verlauf_studienzahl_bl1 <- highcharter::renderHighchart({
    #   ranking_bl_subject(data_studierende,r)
    # })

    output$test <- renderUI({
      studienzahl_test(data_studierende, r)#
    })

    # Box 4
    plot_waffle_react <- reactive({
      studienzahl_waffle_mint(data_studierende,r)#
    })

    output$plot_waffle <- renderPlot({
      plot_waffle_react()#
    })

    output$plot_verlauf_studienzahl_bl_subject <- highcharter::renderHighchart({
      studienzahl_verlauf_bl_subject(data_studierende,r)#
    })

    # output$plot_ranking_bl_subject <- renderPlot({
    #   ranking_bl_subject(data_studierende,r)
    # })

    # Box 5
    plot_waffle_choice_gender_react <- reactive({
      studienzahl_waffle_choice_gender(data_studierende,r)
    })

    output$plot_waffle_choice_gender <- renderPlot({
      plot_waffle_choice_gender_react()
    })

    output$plot_verlauf_studienzahl_bl_subject_gender <- highcharter::renderHighchart({
      studierende_verlauf_single_bl_gender(data_studierende,r)#
    })

    # plot_ranking_studienzahl_bl_subject_gender_react <- reactive({
    #   studienfaecher_ranking(data_studierende2, r, type="other")
    # })

    # output$plot_ranking_studienzahl_bl_subject_gender <- renderPlot({
    #   plot_ranking_studienzahl_bl_subject_gender_react()
    # })

    # output$plot_ranking_studienzahl_bl_subject_gender1 <- highcharter::renderHighchart({
    #   plot_ranking_studienzahl_bl_subject_gender_react(data_studierende,r)
    # })



    # Box 6
    output$plot_studienzahl_map <- renderUI({
      studierende_map(data_studierende_detailliert,r)
    })

    output$plot_studienzahl_bl_verlauf <- highcharter::renderHighchart({
      studierende_verlauf_multiple_bl(data_studierende,r)#
    })


    #  output$plot_vergleich_bl <-  renderPlot({
    #   studierende_mint_vergleich_bl(data_studierende,r)
    # })

     output$plot_vergleich_bl1 <- highcharter::renderHighchart({
       studierende_mint_vergleich_bl(data_studierende_detailliert,r)
     })


    # Box 7
    # output$plot_studienzahl_map_gender <- renderUI({
    #   studierende_map_gender(data_studierende,r)
    # })

    # output$plot_studienzahl_bl_verlauf_gender <- highcharter::renderHighchart({
    #   studierende_verlauf_multiple_bl_gender(data_studierende,r)
    # })

    # plot_ranking_studienzahl_bl_vergleich_gender_react <- reactive({
    #   bundeslaender_ranking(data_studierende, r, type="other")
    # })

    # output$plot_ranking_studienzahl_bl_vergleich_gender <- renderPlot({
    #   plot_ranking_studienzahl_bl_vergleich_gender_react()
    # })

    # Box 8
    output$plot_top_faecher <-  renderUI({
      plot_ranking_top_faecher(data_studierende_detailliert, r)
    })

    # Box Ausländer

    output$plot_auslaender_test <-  highcharter::renderHighchart({
      plot_auslaender_mint(data_studierende_detailliert, r)
    })

    output$plot_auslaender_zeit <-  highcharter::renderHighchart({
      plot_auslaender_mint_zeit(data_studierende_detailliert, r)
    })

    #neu

    output$mint_anteil <-  highcharter::renderHighchart({
      mint_anteile(data_studierende_detailliert, r)
    })





    # downloader
    # output$download_data_box1 <- shiny::downloadHandler(
    #   filename = function() {
    #     paste("data_studium", "csv", sep = ".")
    #   },
    #   content = function(file){
    #     write.csv(data_table_einstieg_react(), file)
    #   }
    # )

  })
}

## To be copied in the UI
# mod_studium_studienzahl_ui("studium_studienzahl_1")

## To be copied in the server
# mod_studium_studienzahl_server("studium_studienzahl_1")
