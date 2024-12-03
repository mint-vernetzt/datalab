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
        img(src='www/Banner_Studium.jpg',
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
           von MINT-Fächern, gemessen an allen gewählten Studienfächern, ist. Dazu zeigen wir Vergleiche nach männlichen und
           weiblichen Studierenden, einzelnen Fächern und nach Bundesländern.")
        ),

      shinydashboard::box(
        title = "Fragen oder Feedback?",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          "Sind alle Zahlen und Grafiken verständlich dargestellt?", br(), "Wir freuen uns über Rückfragen oder Feedback ", tags$a(href = "mailto:katharina.brunner@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per E-Mail"),"oder die Teilnahme an unserer kurzen",
          tags$a(href="https://survey.lamapoll.de/MINT-DataLab_Feedback/", "Umfrage", target="_blank"), "!"
        ))
    ),

    fluidRow(
      shinydashboard::box(
        title = "Links zu den Themen dieser Seite",
        width = 7,
        p(
          style = "text-align: left; font-size = 16px",tags$a(href="#studium_mint",
                                                              span(tags$b(span("MINT-Anteil:")))), "Ein Drittel studiert MINT."
        ),

        p(style = "text-align: left; font-size = 16px",tags$a(href="#studium_fach",
                                                              span(tags$b(span("M-I-N-T:")))), "Knapp drei Viertel aus MINT studieren Ingenieurwissenschaften/Informatik."

        ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#studium_frauen",
                                                              span(tags$b(span("Frauen in MINT:")))), "Frauen in MINT und vor allem den Ingenieurwissenschaften/Informatik eine Minderheit."
        ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#studium_internationale_studis",
                                                              span(tags$b(span("Internationale Studierende:")))), "Ihre Anzahl wächst und am häufigsten studieren sie MINT."
        )
        ),

      shinydashboard::box(
        title = "Datenquellen",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          "Studierendenzahlen in Deutschland: Destatis, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt.")

      )
    ),


  # Box 1 ----

    fluidRow( id="studium_mint",
      shinydashboard::box(
        title = "MINT-Anteil: Ein Drittel studiert MINT.",
        width = 12,
        column(
          width = 8,
        p("Im Jahr 2023 studieren 37 % MINT-Fächer, 63 % Fächer aus anderen Bereichen.
        Bei den Studienanfänger:innen ist der Anteil derer, die MINT studieren, bei 37 %.
          Unter den Absolvent:innen haben 36 % ein MINT-Studium abgeschlossen."),
        p("Die Zahl an MINT-Studierenden hat sich in den letzten zehn Jahren nur leicht verändert,
          von 1.035.841 im Jahr 2014 auf 1.049.902 im Jahr 2023. Der MINT-Anteil unter Studienanfänger:innen
          ist lange konstant gesunken und erstmals von 2021 auf 2022 wieder leicht gestiegen, von 36,5 % auf 37,2 %.")
        ),
        column(
          width = 12,
        tabsetPanel(type = "tabs",
                    tabPanel("Aktueller MINT-Anteil", br(),

                             shiny::sidebarPanel(width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_anteil_ui("mod_studium_studienzahl_anteil_ui_1")
                               ),
                             shiny::mainPanel(width = 9,
                              shinycssloaders::withSpinner(htmlOutput(ns("mint_anteil_studium")),
                                                           color = "#154194"),

                               p(style="font-size:12px;color:grey",
                                 "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_studium_mint_1", title = "",
                                                  content = paste0("In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierenden, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten Mal ein Studium aufnehmen.", "<br><br>Für Studierende im Lehramt wird das belegte Hauptfach für die Kategorisierung in &quotMINT&quot oder &quotNicht-MINT&quot betrachtet.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_studium_mint_1")
                        )),
                    tabPanel("MINT-Anteil im Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_verlauf_ui("mod_studium_studienzahl_einstieg_verlauf_ui_1"),
                               # br(),br(),
                               # downloadButton(
                               #   outputId = ns("download_btn_plot_einstieg_verlauf"),
                               #   label = "Download",
                               #   icon = icon("download")),
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_einstieg_verlauf")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey",
                                 "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_studium_mint_3", title = "",
                                                  content = paste0("In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierenden, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten Mal ein Studium aufnehmen.", "<br><br>Für Studierende im Lehramt wird das belegte Hauptfach für die Kategorisierung in &quotMINT&quot oder &quotNicht-MINT&quot betrachtet.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_studium_mint_3"))

                    ),

                    tabPanel("Bundeslandvergleich", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_bundeslandvergleich_ui("mod_studium_studienzahl_bundeslandvergleich_ui_1"),

                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_studienzahl_bula_mint")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey",
                                 "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),

                               shinyBS::bsPopover(id="h_studium_fach_2", title="",
                                                  content = paste0("In manchen Bundesländern sind einzelne Studienfachgruppen nicht definiert. In diesen Fällen werden die betroffenen Bundesländer als grau schattiert angezeigt.", "<br> <br> In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_studium_fach_2")
                             )
                    ),

                    # tabPanel("Vergleich Anteil MINT nach Bundesländern im Zeitverlauf", br(),
                    #
                    #          tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                    #          .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                    #          shiny::sidebarPanel(
                    #            width = 3,
                    #            mod_studium_studienzahl_verlauf_bl_subject_ui("mod_studium_studienzahl_verlauf_bl_subject_ui_1"),
                    #            # br(),br(),
                    #            # downloadButton(
                    #            #   outputId = ns("download_btn_plot_verlauf_studienzahl_bl_subject"),
                    #            #   label = "Download",
                    #            #   icon = icon("download")),
                    #          ),
                    #          shiny::mainPanel(
                    #            width = 9,
                    #            shinycssloaders::withSpinner(htmlOutput(ns("plot_verlauf_studienzahl_bl_subject")),
                    #                                         color = "#154194"),
                    #
                    #            p(style="font-size:12px;color:grey",
                    #               "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                    #            shinyBS::bsPopover(id="h_studium_mint_4", title = "",
                    #                               content = paste0("In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Für Studierende im Lehramt wird das belegte Hauptfach für die Kategorisierung in &quotMINT&quot oder &quotNicht MINT&quot betrachtet.", "<br> <br> Seit dem Jahr 2015 zählt Informatik zu den Ingenieurwissenschaften und nicht mehr zu den Naturwissenschaften.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                    #                               placement = "top",
                    #                               trigger = "hover"),
                    #            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_studium_mint_4")
                    #
                    #          )
                    # ),
                    #
                    # tabPanel("Vergleich Anteil MINT nach Bundesländer im Zeitverlauf", br(),
                    #
                    #          shiny::sidebarPanel(
                    #            width = 3,
                    #            mod_studium_studienzahl_bl_verlauf_ui("mod_studium_studienzahl_bl_verlauf"),
                    #            # br(),br(),
                    #            # downloadButton(
                    #            #   outputId = ns("download_btn_plot_studienzahl_bl_verlauf"),
                    #            #   label = "Download",
                    #            #   icon = icon("download")),
                    #          ),
                    #          shiny::mainPanel(
                    #            width = 9,
                    #            shinycssloaders::withSpinner(htmlOutput(ns("plot_studienzahl_bl_verlauf")),
                    #                                         color = "#154194"),
                    #
                    #            p(style="font-size:12px;color:grey",
                    #              "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                    #            shinyBS::bsPopover(id="h_studium_mint_5", title = "",
                    #                               content = paste0("In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Für Studierende im Lehramt wird das belegte Hauptfach für die Kategorisierung in &quotMINT&quot oder &quotNicht MINT&quot betrachtet.", "<br> <br> Seit dem Jahr 2015 zählt Informatik zu den Ingenieurwissenschaften und nicht mehr zu den Naturwissenschaften.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                    #                               placement = "top",
                    #                               trigger = "hover"),
                    #            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_studium_mint_5")
                    #          )
                    # ),
                    # tabPanel("Alle Studierendengruppen auf einen Blick", br(),
                    #
                    #          shiny::sidebarPanel(
                    #            width = 3,
                    #            tags$style(".well {background-color:#FFFFFF;}"),
                    #            tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                    #            mod_studium_studienzahl_einstieg_comparison_ui("mod_studium_studienzahl_einstieg_comparison_ui_1"),
                    #            # br(),br(),
                    #            # downloadButton(
                    #            #   outputId = ns("download_btn_plot_einstieg_comparison"),
                    #            #   label = "Download",
                    #            #   icon = icon("download")),
                    #          ),
                    #          shiny::mainPanel(
                    #            width = 9,
                    #            shinycssloaders::withSpinner(htmlOutput(ns("plot_einstieg_comparison")),
                    #                                         color = "#154194"),
                    #
                    #            p(style="font-size:12px;color:grey",
                    #              "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                    #            shinyBS::bsPopover(id="h_studium_mint_6", title = "",
                    #                               content = paste0("In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Für Studierende im Lehramt wird das belegte Hauptfach für die Kategorisierung in &quotMINT&quot oder &quotNicht MINT&quot betrachtet.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                    #                               placement = "top",
                    #                               trigger = "hover"),
                    #            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_studium_mint_6"))
                    # ),
                    #
                    # tabPanel("Studienfachwahl Frauen im Zeitverlauf", br(),
                    #
                    #          tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                    #          .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                    #          shiny::sidebarPanel(
                    #            width = 3,
                    #            mod_studium_studienzahl_verlauf_bl_subject_gender_ui("mod_studium_studienzahl_verlauf_bl_subject_gender_ui_1"),
                    #            # br(),br(),
                    #            # downloadButton(
                    #            #   outputId = ns("download_btn_plot_verlauf_studienzahl_bl_subject_gender"),
                    #            #   label = "Download",
                    #            #   icon = icon("download")),
                    #          ),
                    #          shiny::mainPanel(
                    #            width = 9,
                    #            shinycssloaders::withSpinner(htmlOutput(ns("plot_verlauf_studienzahl_bl_subject_gender")),
                    #                                         color = "#154194"),
                    #
                    #            p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                    #            shinyBS::bsPopover(id="h_studium_mint_8", title = "",
                    #                               content = paste0("In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Für Studierende im Lehramt wird das belegte Hauptfach für die Kategorisierung in &quotMINT&quot oder &quotNicht MINT&quot betrachtet.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                    #                               placement = "top",
                    #                               trigger = "hover"),
                    #            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_studium_mint_8")
                    #          )
                    # ),
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
        )
        ))),

  # Kurzanalyse-Box ----
  div(class = "content-box",
      div(class = "inner-box",
      p(br(),"KURZANALYSE", br()),
      p(style = "font-size = 24",
        strong("Die Anzahl an MINT-Studienanfänger:innen ist in den letzten Jahren zurückgegangen, von 197.000 neuen Studierenden
               in MINT im Jahre 2015 auf 176.000 im Jahr 2022. Ein ähnliches Bild zeigt sich auch bei den Auszubildenden.
               In einer Kurzanalyse fassen wir die Entwicklungen im MINT-Nachwuchs zusammen."),
        br(), br(),
        tags$a(href = "https://www.mint-vernetzt.de/content/uploads/2024/02/MIN_Kurzanalyse_Interesse_final.pdf",
               target = "_blank", "Link zur Kurzanalyse"),
        br(), br())
      )
  ),

    # Box 2 ----

  fluidRow( id="studium_fach",
      shinydashboard::box(
        title = "M-I-N-T: Knapp drei Viertel aus dem MINT-Bereich studieren Ingenieurwissenschaften/Informatik.",
        width = 12,
        column(
          width = 8,
        p("Zoomt man auf den MINT-Fachbereich, studieren 70 % der MINT-Studierenden
          Ingenieurwissenschaften/Informatik. Die restlichen 30 % befinden sich in einem Studium
          der Mathematik/Naturwissenschaft."),
        p("Während der Anteil und die Zahl der Informatikstudierenden in den letzten Jahren immer
          weiter angestiegen sind, nehmen die Studierendenzahlen in anderen Ingenieurwissenschaften ab,
          in Mathematik/Naturwissenschaften sind die Zahlen in den letzten Jahren stabil.")
          ),
        column(
          width = 12,

        tabsetPanel(type = "tabs",
                    tabPanel("Aktueller Anteil MINT-Fächer", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_mint_fach_ui("mod_studium_studienzahl_mint_fach_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_anteil_mint_faecher")),
                                                            color = "#154194"),
                               p(),
                               p(style="font-size:12px;color:grey",
                                 "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_studium_mint_2", title = "",
                                                  content = paste0("In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierenden, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Für Studierende im Lehramt wird das belegte Hauptfach für die Kategorisierung in &quotMINT&quot oder &quotNicht-MINT&quot betrachtet.", "<br> <br> Durch Rundungen kann es zu minimalen Abbweichungen zwischen den Grafiken kommen.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_studium_mint_2"))
                    ),
                    tabPanel("MINT-Fächer im Zeitverlauf", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                             .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_mint_anteile_ui("mod_studium_studienzahl_mint_anteile_ui_1"),
                               # br(),br(),
                               # downloadButton(
                               #   outputId = ns("download_btn_mint_anteil"),
                               #   label = "Download",
                               #   icon = icon("download")),
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("mint_anteil")),
                                                            color = "#154194"),

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
                    tabPanel("Bundeslandvergleich MINT-Fächer", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_studium_studienzahl_bulas_faecher_ui("mod_studium_studienzahl_bulas_faecher_ui_1"),

                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_studienzahl_bula_faecher_mint")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey",
                                 "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),

                               shinyBS::bsPopover(id="h_studium_fach_2", title="",
                                                  content = paste0("In manchen Bundesländern sind einzelne Studienfachgruppen nicht definiert. In diesen Fällen werden die betroffenen Bundesländer als grau schattiert angezeigt.", "<br> <br> In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_studium_fach_2")
                             )
                    ),

                   #  tabPanel("Vergleich Anteil MINT nach Studierendengruppen II", br(),
                   #
                   #           tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                   #           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                   #           shiny::sidebarPanel(
                   #             width = 3,
                   #             mod_studium_studienzahl_choice_1_ui("mod_studium_studienzahl_choice_ui_1_1")),
                   #           shiny::mainPanel(
                   #             width = 9,
                   #             shinycssloaders::withSpinner(plotOutput(ns("plot_waffle")),
                   #                                          color = "#154194"),
                   #             p(),
                   #             p(style="font-size:12px;color:grey",
                   #               "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                   #             shinyBS::bsPopover(id="h_studium_mint_2", title = "",
                   #                                content = paste0("In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Für Studierende im Lehramt wird das belegte Hauptfach für die Kategorisierung in &quotMINT&quot oder &quotNicht MINT&quot betrachtet.", "<br> <br> Durch Rundungen kann es zu minimalen Abbweichungen zwischen den Grafiken kommen.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                   #                                placement = "top",
                   #                                trigger = "hover"),
                   #             tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_studium_mint_2"))
                   #  ),
                   #
                   #  tabPanel("Vergleich Fächer (Karte)", br(),
                   #
                   #           shiny::sidebarPanel(
                   #             width = 3,
                   #             mod_studium_studienzahl_bl_map_ui("mod_studium_studienzahl_bl_map"),
                   #
                   #           ),
                   #           shiny::mainPanel(
                   #             width = 9,
                   #             shinycssloaders::withSpinner(htmlOutput(ns("plot_studienzahl_map")),
                   #                                          color = "#154194"),
                   #
                   #             p(style="font-size:12px;color:grey",
                   #               "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                   #
                   #             shinyBS::bsPopover(id="h_studium_fach_2", title="",
                   #                                content = paste0("In manchen Bundesländern sind einzelne Studienfachgruppen nicht definiert. In diesen Fällen werden die betroffenen Bundesländer als grau schattiert angezeigt.", "<br> <br> In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                   #                                placement = "top",
                   #                                trigger = "hover"),
                   #             tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_studium_fach_2")
                   #           )
                   #  ),
                   #
                   #
                   #  # Fehler in der Boxgrösse, muss noch behoben werden
                   #
                   # tabPanel("Alle Fächer auf einen Blick", br(),
                   #
                   #           tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                   #           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                   #           shiny::sidebarPanel(
                   #             width = 3,
                   #             mod_studium_studienzahl_ranking_bl_subject_ui("mod_studium_studienzahl_ranking_bl_subject_ui_1"),
                   #             # br(),br(),
                   #             # downloadButton(
                   #             #   outputId = ns("download_btn_plot_verlauf_studienzahl_bl_subject1"),
                   #             #   label = "Download",
                   #             #   icon = icon("download"))
                   #
                   #           ),
                   #           shiny::mainPanel(
                   #             width = 9,
                   #             shinycssloaders::withSpinner(htmlOutput(ns("plot_verlauf_studienzahl_bl_subject1"), height = "550px"),
                   #                                          color = "#154194"),
                   #
                   #             p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt." ),
                   #             shinyBS::bsPopover(id="h_studium_fach_3", title="",
                   #                                content = paste0("Durch Rundung kann es dazu kommen, dass einzelne Studienfachgruppen für bestimmte Indikatoren oder Regionen nicht in der Darstellung angezeigt werden.", "<br> <br> In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                   #                                placement = "top",
                   #                                trigger = "hover"),
                   #             tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_studium_fach_3")
                   #             #plotOutput(ns("plot_ranking_bl_subject")),p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen.")
                   #           )
                   #  ),
                   #  tabPanel("Vergleich nach Bundesländern", br(),
                   #
                   #           shiny::sidebarPanel(
                   #             width = 3,
                   #             mod_studium_studienzahl_bl_vergleich_ui("studium_studienzahl_bl_vergleich"),
                   #             # br(),br(),
                   #             # downloadButton(
                   #             #   outputId = ns("download_btn_plot_vergleich_bl1"),
                   #             #   label = "Download",
                   #             #   icon = icon("download")),
                   #           ),
                   #           shiny::mainPanel(
                   #             width = 9,
                   #             shinycssloaders::withSpinner(htmlOutput(ns("plot_vergleich_bl1")),
                   #                                          color = "#154194"),
                   #
                   #             p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                   #
                   #             shinyBS::bsPopover(id="h_studium_fach_5", title="",
                   #                                content = paste0("In manchen Bundesländern sind einzelne Studienfachgruppen nicht definiert. In diesen Fällen werden nur die Bundesländer angezeigt, welche die betrachtete Studienfachgruppe aufweisen.", "<br><br> In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                   #                                placement = "top",
                   #                                trigger = "hover"),
                   #             tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_studium_fach_5")
                   #
                   #           )
                   #  )
        )
        ))),

  # Box 3 ----
    fluidRow(id="studium_frauen",
      shinydashboard::box(
        title = "Frauen in MINT: Frauen in MINT und vor allem in den Ingenieurwissenschaften/Informatik eine Minderheit.",
        width = 12,
        column(
          width = 8,
        p("Der Frauenanteil in MINT-Studienfächern liegt bei nur 33 %.
        Im Vergleich dazu: Betrachtet man alle \"Nicht-MINT\"-Studienbereiche zusammen,
        ist der Frauenanteil bei mehr als der Hälfte. Doch in den letzten zehn Jahren gab es eine leichte Positiventwicklung.
        Der Frauenanteil nahm von etwa 29 % auf 33 % zu."),
        p("Es gibt auch MINT-Fachbereiche, in denen es ganz anders aussieht. In Pharmazie oder auch Biologie studieren mehr Frauen als Männer.
        Wechselt man die Perspektive und blickt auf die Gruppe der Studentinnen,
          wird die unterschiedliche Attraktivität des MINT-Studiums für Frauen vs.
          Männer nochmal deutlich: Von allen Studentinnen wählen nur 23 % MINT.
          Bei den Studenten sind es die Hälfte.")
        ),
        column(
          width = 12,

        tabsetPanel(type = "tabs",
                    tabPanel("Frauenanteil in MINT", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_gender_ui("mod_studium_studienzahl_einstieg_gender_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_einstieg_gender")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_studium_frauen_1", title="",
                                                  content = paste0("Ab dem Jahr 2023 werden zusätzliche Differenzierungen angezeigt, die zu einer Erhöhung der Balkenanzahl führt. So werden die Hochschultypen (Universität, Fachhochschulen) separat gelistet. <br>  In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Für Studierende im Lehramt wird das belegte Hauptfach für die Kategorisierung in &quotMINT&quot oder &quotNicht MINT&quot betrachtet.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_studium_frauen_1")
                               )
                    ),
                    # tabPanel("Anteil Frauen nach Bundesländern", br(),
                    #
                    #          shiny::sidebarPanel(
                    #            width = 3,
                    #            tags$style(".well  {background-color:#FFFFFF;}"),
                    #            tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                    #            #mod_studium_studienzahl_einstieg_comparison_gender_ui("mod_studium_studienzahl_einstieg_comparison_gender_ui_1"),
                    #            # br(),br(),
                    #            # downloadButton(
                    #            #   outputId = ns("download_btn_plot_einstieg_comparison_gender"),
                    #            #   label = "Download",
                    #            #   icon = icon("download")),
                    #          ),
                    #          shiny::mainPanel(
                    #            width = 9,
                    #            # shinycssloaders::withSpinner(htmlOutput(ns("plot_einstieg_comparison_gender")),
                    #            #                              color = "#154194"),
                    #
                    #            p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                    #            shinyBS::bsPopover(id="h_studium_frauen_3", title="",
                    #                               content = paste0("In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden.", "<br> <br> Studierende auf Lehramt werden für Fächergruppen, die nicht oder nur von einer sehr geringen Anzahlen an Lehramtstudierenden deutschlandweit als Hauptfach studiert werden, nicht in der Übersicht angezeigt. In einzelnen Bundesländern kann es für spezfisiche Fächergruppen zu keinen oder sehr geringen Leharamtstudierendenzahlen kommen. Achten Sie hierbei auf die absoluten Zahlenangaben zur Anzahl der Studierenden.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                    #                               placement = "top",
                    #                               trigger = "hover"),
                    #            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_studium_frauen_3")
                    #          )
                    # ),

                    tabPanel("Anteil Frauen im Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_studium_studienzahl_einstieg_verlauf_gender_ui("mod_studium_studienzahl_einstieg_verlauf_gender_ui_1"),
                               # br(),br(),
                               # downloadButton(
                               #   outputId = ns("download_btn_plot_einstieg_verlauf_gender"),
                               #   label = "Download",
                               #   icon = icon("download")),
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_einstieg_verlauf_gender")),
                                                            color = "#154194"),

                               p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                               shinyBS::bsPopover(id="h_studium_frauen_2", title="",
                                                  content = paste0("In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Für Studierende im Lehramt wird das belegte Hauptfach für die Kategorisierung in &quotMINT&quot oder &quotNicht MINT&quot betrachtet.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_studium_frauen_2")
                               )

                  ),
                  # tabPanel("MINT-Wahlverhalten nach Geschlecht", br(),
                  #
                  #          tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                  #                          .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                  #          shiny::sidebarPanel(
                  #            width = 3,
                  #            mod_schule_kurse_multiple_ui("mod_schule_kurse_multiple_ui_1")),
                  #          shiny::mainPanel(
                  #            width = 9,
                  #            shinycssloaders::withSpinner(htmlOutput(ns("plot_wahl")),
                  #                                         color = "#154194"),
                  #
                  #            p(style="font-size:12px;color:grey", br(),"Quelle der Daten: KMK, 2022, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                  #            shinyBS::bsPopover(id="h_schule_mint_4", title = "",
                  #                               content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotnicht MINT&quot bezieht sich auf die Belegungszaheln in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden." , "<br> <br> Durch Rundungen kann es zu minimalen Abbweichungen zwischen den Grafiken kommen."),
                  #                               placement = "top",
                  #                               trigger = "hover"),
                  #            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_mint_4")
                  #          )
                  # ),

                  tabPanel("MINT-Wahlverhalten nach Geschlecht", br(),

                           tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                           shiny::sidebarPanel(
                             width = 3,
                             mod_studium_choice_gender_ui("mod_studium_studienzahl_choice_gender_ui")
                           ),
                           shiny::mainPanel(
                             width = 9,
                             shinycssloaders::withSpinner(htmlOutput(ns("plot_choice_gender")),
                                                          color = "#154194"),

                             p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                             shinyBS::bsPopover(id="h_studium_mint_7", title = "",
                                                content = paste0("In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Für Studierende im Lehramt wird das belegte Hauptfach für die Kategorisierung in &quotMINT&quot oder &quotNicht MINT&quot betrachtet.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                placement = "top",
                                                trigger = "hover"),
                             tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_studium_mint_7")
                           )
                  ),

                  tabPanel("TOP-Studienfächer nach Geschlecht", br(),
                           #tags$head(
                           # tags$style(
                           #   ".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                           #      .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                           shiny::sidebarPanel(
                             width = 3,
                             mod_studium_top_faecher_ui("mod_studium_top_faecher"),
                             # br()
                             # ,
                             # downloadButton(
                             #   outputId = ns("download_btn_plot_top_faecher_1"),
                             #   label = "Download (links)",
                             #   icon = icon("download")),
                             # br(),br(),
                             # downloadButton(
                             #   outputId = ns("download_btn_plot_top_faecher_2"),
                             #   label = "Download (rechts)",
                             #   icon = icon("download")),
                           ),
                           shiny::mainPanel(
                             width = 9,
                             shinycssloaders::withSpinner(htmlOutput(ns("plot_top_faecher")),
                                                          color = "#154194"),

                             p(style = "font-size:12px;color:grey",
                               "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                             br(),
                             shinyBS::bsPopover(id="h_studium_fach_1", title="",
                                                content = paste0("In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                placement = "top",
                                                trigger = "hover"),
                             tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_studium_fach_1"))
                  )
        )
        ))),

  # Box 4 ----

  fluidRow(id="studium_internationale_studis",
           shinydashboard::box(
             title = "Internationale Studierende: Ihre Anzahl wächst - und am häufigsten studieren sie MINT.",
             width = 12,
             column(
               width = 8,
             p("Deutschland ist bei internationalen Studierenden beliebt.
             Und ihre Zahl ist in den letzten zehn Jahren stark gestiegen: von rund 106.542
             auf rund 204.006 internationale Studierende allein in MINT.
             Damit machen sie 2023 19 % der MINT-Studierenden aus."),
            p("Vergleicht man die Fachbereiche, studieren internationale Studierende besonders oft MINT-Fächer.
              etwa 159.000 von ihnen studieren Ingenieurswissenschaften/Informatik,
              gefolgt von rund 97.000 in Rechts-, Wirtschafts- und Sozialwissenschaften und rund 45.000 in
              Mathematik/Naturwissenschaften.")
            ),
            column(
              width = 12,
             tabsetPanel(type = "tabs",
                         tabPanel("Anteil von internationalen Studierenden nach Fächern", br(),
                                  shiny::sidebarPanel(
                                    width = 3,
                                    tags$style(".well {background-color:#FFFFFF;}"),
                                    tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                                    mod_studium_studienzahl_ausl_ui("mod_studium_studienzahl_ausl_ui"),
                                    # br(),br(),
                                    # downloadButton(
                                    #   outputId = ns("download_btn_plot_auslaender_test"),
                                    #   label = "Download",
                                    #   icon = icon("download")),
                                  ),
                                  shiny::mainPanel(
                                    width = 9,
                                    tags$head(tags$style(HTML(".small-box {height: 400px}"))),
                                    shinycssloaders::withSpinner(htmlOutput(ns("plot_auslaender_test"), height = "650px"),
                                                                 color = "#154194"),
                                    # p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                                    # shinyBS::bsPopover(id="d_studium_international_1", title = "",
                                    #                    content = paste0("internationale Studierende = Studierende, die in Deutschland studieren aber keine deutsche Staatsbürgerschaft besitzen."),
                                    #                    placement = "top",
                                    #                    trigger = "hover"),
                                    # tags$a(paste0("Definition der Begriffe"), icon("info-circle"), id = "d_studium_international_1"),
                                    # p(),
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
                                    mod_studium_studienzahl_ausl_zeit_ui("mod_studium_studienzahl_ausl_zeit_ui"),
                                    # br(),br(),
                                    # downloadButton(
                                    #   outputId = ns("download_btn_plot_auslaender_zeit"),
                                    #   label = "Download",
                                    #   icon = icon("download")),
                                  ),
                                  shiny::mainPanel(
                                    width = 9,
                                    shinycssloaders::withSpinner(htmlOutput(ns("plot_auslaender_zeit")),
                                                                 color = "#154194"),

                                    # p(style="font-size:12px;color:grey", "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                                    # shinyBS::bsPopover(id="d_studium_international_2", title = "",
                                    #                    content = paste0("internationale Studierende = Studierende, die in deutschland studieren aber keine deutsche Staatsbürgerschaft besitzen."),
                                    #                    placement = "top",
                                    #                    trigger = "hover"),
                                    # tags$a(paste0("Definition der Begriffe"), icon("info-circle"), id = "d_studium_international_2"),
                                    # p(),
                                    shinyBS::bsPopover(id="h_studium_international_2", title="",
                                                       content = paste0("In manchen Bundesländern sind einzelne Studienfachgruppen nicht definiert. In diesen Fällen werden nur die vorhandenen Studienfachgruppen angezeigt.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                       placement = "top",
                                                       trigger = "hover"),
                                    tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_studium_international_2"))

                                  ),



                         tabPanel("Bundeslandvergleich", br(),

                                  shiny::sidebarPanel(
                                    width = 3,
                                    mod_studium_studienzahl_international_bundeslandvergleich_ui("mod_studium_studienzahl_international_bundeslandvergleich_ui"),

                                  ),
                                  shiny::mainPanel(
                                    width = 9,
                                    shinycssloaders::withSpinner(htmlOutput(ns("plot_auslaender_international_bula")),
                                                                 color = "#154194"),

                                    p(style="font-size:12px;color:grey",
                                      "Quelle der Daten: Destatis, 2023, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),

                                    shinyBS::bsPopover(id="p_auslaender_bula_1", title="",
                                                       content = paste0("In manchen Bundesländern sind einzelne Studienfachgruppen nicht definiert. In diesen Fällen werden die betroffenen Bundesländer als grau schattiert angezeigt.", "<br> <br> In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierende, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                                       placement = "top",
                                                       trigger = "hover"),
                                    tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="p_auslaender_bula_1")
                                  )
                         ),

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


              )
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
mod_studium_studienzahl_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Box 1 - Wer Wählt MINT ----

    ## Pies MINT
    output$mint_anteil_studium <- renderUI({
      studienzahl_mint(r)
    })

    ## Zeitverlauf
    # output$plot_einstieg_verlauf <- highcharter::renderHighchart({
    #   studienzahl_verlauf_single(r)
    # })

    output$plot_einstieg_verlauf <- renderUI({
      plot_list <- studienzahl_verlauf_single(r)
      r$plot_einstieg_verlauf <- plot_list

      r$plot_einstieg_verlauf_title <- get_plot_title(
        plot = r$plot_einstieg_verlauf
      )

      plot_list
    })

    output$download_btn_plot_einstieg_verlauf <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_einstieg_verlauf_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_einstieg_verlauf,
          filename =  r$plot_einstieg_verlauf_title,
          width = 700,
          height = 400)

        file.copy(r$plot_einstieg_verlauf_title, file)
        file.remove(r$plot_einstieg_verlauf_title)
      }
    )

    ## Zeitverlauf BULAS Fächer
    # output$plot_verlauf_studienzahl_bl_subject <- highcharter::renderHighchart({
    #   studienzahl_verlauf_bl_subject(r)#

      output$plot_verlauf_studienzahl_bl_subject <- renderUI({
        plot_list <- studienzahl_verlauf_bl_subject(r)
        r$plot_verlauf_studienzahl_bl_subject <- plot_list

        r$plot_verlauf_studienzahl_bl_subject_title <- get_plot_title(
          plot = r$plot_verlauf_studienzahl_bl_subject
        )

        plot_list
      })

      output$download_btn_plot_verlauf_studienzahl_bl_subject <- downloadHandler(
        contentType = "image/png",
        filename = function() {r$plot_verlauf_studienzahl_bl_subject_title},
        content = function(file) {
          # creating the file with the screenshot and prepare it to download

          add_caption_and_download(
            hc = r$plot_verlauf_studienzahl_bl_subject,
            filename =  r$plot_verlauf_studienzahl_bl_subject_title,
            width = 700,
            height = 400)

          file.copy(r$plot_verlauf_studienzahl_bl_subject_title, file)
          file.remove(r$plot_verlauf_studienzahl_bl_subject_title)
        }
      )


    ## Fächer
    # output$plot_studienzahl_bl_verlauf <- highcharter::renderHighchart({
    #   studierende_verlauf_multiple_bl(r)#
    # })

    output$plot_studienzahl_bl_verlauf <- renderUI({
      plot_list <- studierende_verlauf_multiple_bl(r)
      r$plot_studienzahl_bl_verlauf <- plot_list

      r$plot_studienzahl_bl_verlauf_title <- get_plot_title(
        plot = r$plot_studienzahl_bl_verlauf
      )

      plot_list
    })

    output$download_btn_plot_studienzahl_bl_verlauf <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_studienzahl_bl_verlauf_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_studienzahl_bl_verlauf,
          filename =  r$plot_studienzahl_bl_verlauf_title,
          width = 700,
          height = 400)

        file.copy(r$plot_studienzahl_bl_verlauf_title, file)
        file.remove(r$plot_studienzahl_bl_verlauf_title)
      }
    )

    ## Balken Vergleich
    # output$plot_einstieg_comparison <- highcharter::renderHighchart({
    #   studienzahl_einstieg_comparison(r)#
    # })

    # output$plot_einstieg_comparison <- renderUI({
    #   plot_list <- studienzahl_einstieg_comparison(r)
    #   r$plot_einstieg_comparison <- plot_list
    #
    #   r$plot_einstieg_comparison_title <- get_plot_title(
    #     plot = r$plot_einstieg_comparison
    #   )
    #
    #   plot_list
    # })

    output$download_btn_plot_einstieg_comparison <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_einstieg_comparison_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_einstieg_comparison,
          filename =  r$plot_einstieg_comparison_title,
          width = 700,
          height = 400)

        file.copy(r$plot_einstieg_comparison_title, file)
        file.remove(r$plot_einstieg_comparison_title)
      }
    )

    output$plot_choice_gender <- renderUI({
      studienzahl_choice_gender(r)
    })

    ## Zeitverlauf Gender
    # output$plot_verlauf_studienzahl_bl_subject_gender <- highcharter::renderHighchart({
    #   studierende_verlauf_single_bl_gender(r)#
    # })

    output$plot_verlauf_studienzahl_bl_subject_gender <- renderUI({
      plot_list <- studierende_verlauf_single_bl_gender(r)
      r$plot_verlauf_studienzahl_bl_subject_gender <- plot_list

      r$plot_verlauf_studienzahl_bl_subject_gender_title <- get_plot_title(
        plot = r$plot_verlauf_studienzahl_bl_subject_gender
      )

      plot_list
    })

    output$download_btn_plot_verlauf_studienzahl_bl_subject_gender <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_verlauf_studienzahl_bl_subject_gender_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_verlauf_studienzahl_bl_subject_gender,
          filename =  r$plot_verlauf_studienzahl_bl_subject_gender_title,
          width = 700,
          height = 400)

        file.copy(r$plot_verlauf_studienzahl_bl_subject_gender_title, file)
        file.remove(r$plot_verlauf_studienzahl_bl_subject_gender_title)
      }
    )



    # Box 2 - M-I-N-T ----

    ## MINT-Fächer-Anteil

    output$plot_anteil_mint_faecher <- renderUI(
      plot_mint_faecher(r)
    )

    output$plot_studienzahl_bula_faecher_mint <- renderUI(
      plot_studierende_bula_faecher(r)
    )

    ## Waffle
    plot_waffle_react <- reactive({
      studienzahl_waffle_mint(r)#
    })
    output$plot_waffle <- renderPlot({
      plot_waffle_react()#
    })



    ## Top 10
    # output$plot_top_faecher <-  renderUI({
    #   plot_ranking_top_faecher(r)
    # })

    output$plot_top_faecher <- renderUI({
      plot_list <- plot_ranking_top_faecher(r)
      r$plot_top_faecher_left <- plot_list[[1]]
      r$plot_top_faecher_right <- plot_list[[2]]

      r$plot_top_faecher_left_title <- get_plot_title(
        plot = r$plot_top_faecher_left
      )
      r$plot_top_faecher_right_title <- get_plot_title(
        plot = r$plot_top_faecher_right
      )

      # return plots
      out <- highcharter::hw_grid(
        plot_list,
        ncol = 2)
      out

    })

    output$download_btn_plot_top_faecher_1 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_top_faecher_left_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_top_faecher_left,
          filename =  r$plot_top_faecher_left_title,
          width = 700,
          height = 400,
          with_labels = FALSE)

        file.copy(r$plot_top_faecher_left_title, file)
        file.remove(r$plot_top_faecher_left_title)
      }
    )

    output$download_btn_plot_top_faecher_2 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_top_faecher_right_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download
        add_caption_and_download(
          hc = r$plot_top_faecher_right,
          filename =  r$plot_top_faecher_right_title,
          width = 700,
          height = 400,
          with_labels = FALSE)

        file.copy(r$plot_top_faecher_right_title, file)
        file.remove(r$plot_top_faecher_right_title)
      }
    )

    ## Karte
    output$plot_studienzahl_bula_mint <- renderUI({
      studierende_bula_mint(r)
    })

    ## Zeitverlauf BuLas
    # output$plot_verlauf_studienzahl_bl_subject1 <- highcharter::renderHighchart({
    #   ranking_bl_subject(r)
    # })

    output$plot_verlauf_studienzahl_bl_subject1 <- renderUI({
      plot_list <- ranking_bl_subject(r)
      r$plot_verlauf_studienzahl_bl_subject1 <- plot_list

      r$plot_verlauf_studienzahl_bl_subject1_title <- get_plot_title(
        plot = r$plot_verlauf_studienzahl_bl_subject1
      )

      plot_list
    })

    output$download_btn_plot_verlauf_studienzahl_bl_subject1 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_verlauf_studienzahl_bl_subject1_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_verlauf_studienzahl_bl_subject1,
          filename =  r$plot_verlauf_studienzahl_bl_subject1_title,
          width = 700,
          height = 400)

        file.copy(r$plot_verlauf_studienzahl_bl_subject1_title, file)
        file.remove(r$plot_verlauf_studienzahl_bl_subject1_title)
      }
    )

    ## Balken MINT
    # output$mint_anteil <-  highcharter::renderHighchart({
    #   mint_anteile(r)
    # })

    output$mint_anteil <- renderUI({
      plot_list <- mint_anteile(r)
      r$mint_anteil <- plot_list

      r$mint_anteil_title <- get_plot_title(
        plot = r$mint_anteil
      )

      plot_list

    })

    output$download_btn_mint_anteil <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$mint_anteil_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$mint_anteil,
          filename =  r$mint_anteil_title,
          width = 700,
          height = 400)

        file.copy(r$mint_anteil_title, file)
        file.remove(r$mint_anteil_title)
      }
    )

    ## Balken MINT BULAs
    # output$plot_vergleich_bl1 <- highcharter::renderHighchart({
    #   studierende_mint_vergleich_bl(r)
    # })

    output$plot_vergleich_bl1 <- renderUI({
      plot_list <- studierende_mint_vergleich_bl(r)
      r$plot_vergleich_bl1 <- plot_list

      r$plot_vergleich_bl1_title <- get_plot_title(
        plot = r$plot_vergleich_bl1
      )

      plot_list

    })

    output$download_btn_plot_vergleich_bl1 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_vergleich_bl1_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_vergleich_bl1,
          filename =  r$plot_vergleich_bl1_title,
          width = 700,
          height = 400)

        file.copy(r$plot_vergleich_bl1_title, file)
        file.remove(r$plot_vergleich_bl1_title)
      }
    )




    # Box 3 - Frauen -----

    ## Pie Gender
    output$plot_einstieg_gender <- renderUI({
      studienzahl_einstieg_gender(r)#
    })



    ## Verlauf Gender
    # output$plot_einstieg_verlauf_gender <- highcharter::renderHighchart({
    #   studienzahl_verlauf_single_gender(r)#
    # })
    #
    output$plot_einstieg_verlauf_gender <- renderUI({
      plot_list <- studienzahl_verlauf_single_gender(r)
      r$plot_einstieg_verlauf_gender <- plot_list

      r$plot_einstieg_verlauf_gender_title <- get_plot_title(
        plot = r$plot_einstieg_verlauf_gender
      )

      plot_list

    })

    output$download_btn_plot_einstieg_verlauf_gender <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_einstieg_verlauf_gender_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_einstieg_verlauf_gender,
          filename =  r$plot_einstieg_verlauf_gender_title,
          width = 700,
          height = 400)

        file.copy(r$plot_einstieg_verlauf_gender_title, file)
        file.remove(r$plot_einstieg_verlauf_gender_title)
      }
    )

    ## Balken Frauen
    # output$plot_einstieg_comparison_gender <- highcharter::renderHighchart({
    #   studienzahl_einstieg_comparison_gender(r)
    # })

    output$plot_einstieg_comparison_gender <- renderUI({
      plot_list <- studienzahl_einstieg_comparison_gender(r)
      r$plot_einstieg_comparison_gender <- plot_list

      r$plot_einstieg_comparison_gender_title <- get_plot_title(
        plot = r$plot_einstieg_comparison_gender
      )

      plot_list
    })

    output$download_btn_plot_einstieg_comparison_gender <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_einstieg_comparison_gender_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_einstieg_comparison_gender,
          filename =  r$plot_einstieg_comparison_gender_title,
          width = 700,
          height = 400)

        file.copy(r$plot_einstieg_comparison_gender_title, file)
        file.remove(r$plot_einstieg_comparison_gender_title)
      }
    )

    # Box 4 - Internationale Studierende ----

    # Tab 1
    # output$plot_auslaender_test <-  highcharter::renderHighchart({
    #   plot_auslaender_mint( r)
    # })

    output$plot_auslaender_test <- renderUI({
      plot_list <- plot_auslaender_mint(r)
      r$plot_auslaender_test <- plot_list

      r$plot_auslaender_test_title <- get_plot_title(
        plot = r$plot_auslaender_test
      )

      plot_list
    })

    output$download_btn_plot_auslaender_test <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_auslaender_test_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_auslaender_test,
          filename =  r$plot_auslaender_test_title,
          width = 700,
          height = 400)

        file.copy(r$plot_auslaender_test_title, file)
        file.remove(r$plot_auslaender_test_title)
      }
    )

    # Tab 2
    # output$plot_auslaender_zeit <-  highcharter::renderHighchart({
    #   plot_auslaender_mint_zeit(r)
    # })

    output$plot_auslaender_zeit <- renderUI({
      plot_list <- plot_auslaender_mint_zeit(r)
      r$plot_auslaender_zeit <- plot_list

      r$plot_auslaender_zeit_title <- get_plot_title(
        plot = r$plot_auslaender_zeit
      )

      plot_list
    })

    output$download_btn_plot_auslaender_zeit <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_auslaender_zeit_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_auslaender_zeit,
          filename =  r$plot_auslaender_zeit_title,
          width = 700,
          height = 400)

        file.copy(r$plot_auslaender_zeit_title, file)
        file.remove(r$plot_auslaender_zeit_title)
      }
    )




    # Tab 3

    output$plot_auslaender_international_bula <- renderUI({
      plot_list <- studierende_international_bula_mint(r)
      r$plot_auslaender_international_bula <- plot_list

      r$studierende_international_bula_mint <- get_plot_title(
        plot = r$plot_auslaender_international_bula
      )

      plot_list
    })




    output$download_btn_plot_auslaender_zeit <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$studierende_international_bula_mint},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_auslaender_international_bula,
          filename =  r$studierende_international_bula_mint,
          width = 700,
          height = 400)

        file.copy(r$studierende_international_bula_mint, file)
        file.remove(r$studierende_international_bula_mint)
      }
    )




    # REST


    # # Box 2
    # output$plot_einstieg_pie <- renderUI({
    #   studienzahl_einstieg_pie(data_studierende,r)
    # })
    #


    # all_mint_23_react <- reactive({
    #   studienzahl_all_mint_23(data_studierende2, r)
    # })




    # data_table_einstieg_react <- reactive({
    #   data_einstieg(data_studierende, r)
    # })

    # output$data_table_einstieg <- DT::renderDT({
    #   data_table_einstieg_react()
    # })

    # Box 3









    # output$plot_verlauf_studienzahl_bl1 <- highcharter::renderHighchart({
    #   ranking_bl_subject(data_studierende,r)
    # })



    # Box 4



    # output$plot_ranking_bl_subject <- renderPlot({
    #   ranking_bl_subject(data_studierende,r)
    # })

    # Box 5



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




    #  output$plot_vergleich_bl <-  renderPlot({
    #   studierende_mint_vergleich_bl(data_studierende,r)
    # })



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
