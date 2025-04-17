#' schule_kurse UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_ui <- function(id){
  ns <- NS(id)
  tagList(

    # Banner
    fluidRow(
      div(class = "clean-box",
          column(
            width = 12,
            img(src='www/Banner_Schule.jpg',
                class = "img-responsive",
                #height = "150px", width = "150px",
                alt = "Banner Schule",
                style="display: block; margin-left: auto; margin-right: auto; margin-bottom: 20px;"
            )))),


    # Info-Texte

    fluidRow(
      shinydashboard::box(
        title = "Auf dieser Seite",
        width = 7,
        p(style = "text-align: left; font-size = 16px",
          "Auf dieser Seite zeigen wir, wie hoch der Anteil von MINT-Fächern an allen Schulfächern gemessen an allen gewählten Grund- und Leistungskursen ist.
          Je nach Bundesland wählen alle Oberstufen-Schüler:innen mehrere Grund- und Leistungskurse.
          Anhand dieser Belegungszahlen haben wir den Anteil von MINT-Fächern in der Schule berechnet. ")


      ),

      shinydashboard::box(
        title = "Fragen oder Feedback?",
        width = 5,
        p(style = "text-align: left; font-size = 16px",
          "Sind alle Zahlen und Grafiken verständlich dargestellt?", br(), "Wir freuen uns über Rückfragen oder Feedback ", tags$a(href = "mailto:katharina.brunner@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per E-Mail"),"oder über eine Teilnahme bei unserer kurzen",
          tags$a(href="https://survey.lamapoll.de/MINT-DataLab_Feedback/", "Umfrage", target="_blank"), "!"
        ))


    ),

    fluidRow(
      shinydashboard::box(
        title = "Links zu den Themen dieser Seite",
        width = 7,

        p(style = "text-align: left; font-size = 16px",tags$a(href="#schule_mint",
                                                              span(tags$b(span("MINT-Anteil:")))),"Ein Drittel der Leistungskursbelegungen sind in MINT."
        ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#schule_fach",
                                                              span(tags$b(span("M-I-N-T:")))), "Die Hälfte der MINT-Leistungskurse sind in Mathematik."

        ),
        p(style = "text-align: left; font-size = 16px",tags$a(href="#schule_frauen",
                                                              span(tags$b(span("Frauen in MINT:")))),"MINT-Belegungen unter Mädchen und Jungen ungefähr gleich häufig."),

        p(style = "text-align: left; font-size = 16px",tags$a(href="#schule_kompetenz",
                                                              span(tags$b(span("MINT-Kompetenzen:")))),"MINT-Kompetenzen nehmen weiter ab. IQB-Ergebnisse der letzten Jahre.")

      ),
      shinydashboard::box(
        title = "Datenquellen",
        width = 5,
        p("Die amtlichen Statistiken zeigen das aktuellste verfügbare Berichtsjahr 2023."),
        p(style = "text-align: left; font-size = 16px",
          tags$b(span("Schüler:innenzahlen der Oberstufe: Kultusministerkonferenz (KMK), 2024, auf Anfrage")),
          "Daten des Berichtjahres 2024 ca. ab Dezember 2025 verfügbar."),
        p(style = "text-align: left; font-size = 16px",
          tags$b(span("Kompetenzdaten in Deutschland: Institut zur Qualitätsentwicklung im Bildungswesen (IQB), 2023, freier Download.")),
          "Die Auswertung des IQB-Bildungstends 2024 mit Fokus auf MINT wird voraussichtlich im Herbst 2025 veröffentlicht."),
        p(style = "text-align: left; font-size = 16px",
          "Weitere Statistiken über die Belegung von MINT-Fächern in anderen Klassenstufen oder Schulformen liegen uns derzeit nicht vor.")
        )

      ),

  # Box 1 -----

    fluidRow(id="schule_mint",
      shinydashboard::box(
        title = "MINT-Anteil: Ein Drittel der Leistungskursbelegungen sind in MINT.",
        width = 12,
        column(
          width = 8,
        p("Im Jahr 2023 entfallen 24 % der Grundkursbelegungen auf ein MINT-Fach.
        Der MINT-Anteil an Leistungskursbelegungen ist noch einmal etwas höher:
        knapp 32 % der Belegungen sind in MINT."),
        p("Wie viel MINT in grundlegendem und gehobenem Leistungsniveau belegt werden kann,
        ist von den Wahlmöglichkeiten in den Bundesländern abhängig. Das ist bei einer Betrachtung
        einzelner Bundesländer zu berücksichtigen."),
        p("In den letzten zehn Jahren hat sich der MINT-Anteil an den gesamten
          Oberstufenbelegungen nicht verändert.
          Dabei hat der Anteil von MINT in Grundkursen leicht zu und der MINT-Anteil
          in Leistungskursen leicht abgenommen.")
        ),
        column(
          width = 12,

                tabsetPanel(type = "tabs",
                    tabPanel("Aktueller MINT-Anteil", br(),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_einstieg_comparison_ui("mod_schule_kurse_einstieg_comparison_ui_1"),
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_einstieg_comparison")),
                                                            color = "#154194"),
                               shinyBS::bsPopover(id="h_schule_mint_2", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotNicht-MINT&quot bezieht sich auf die Belegungszahlen in den Grund- und Leistungskursen der Oberstufe. Es wird der Anteil von MINT-Belegungen an allen Belegungen betrachtet. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br><br> Mit Grundkursen sind nach der Definition der KMK Fächer mit bis zu 3 Wochenstunden gemeint.<br> Mit Leistungskursen Fächer mit mindestens 4 Wochenstunden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_mint_2")
                               )
                             ),
                    tabPanel("MINT-Anteil im Zeitverlauf", br(),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_einstieg_verlauf_ui("mod_schule_kurse_einstieg_verlauf_ui_1"),


                               ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_einstieg_verlauf")),
                                                            color = "#154194"),
                               shinyBS::bsPopover(id="h_schule_mint_3", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotNicht-MINT&quot bezieht sich auf die Belegungszahlen in den Grund- und Leistungskursen der Oberstufe. Es wird der Anteil von MINT-Belegungen an allen Belegungen betrachtet. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br><br> Mit Grundkursen sind nach der Definition der KMK Fächer mit bis zu 3 Wochenstunden gemeint.<br> Mit Leistungskursen Fächer mit mindestens 4 Wochenstunden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_mint_3")
                               )
                             ),

                    tabPanel("Bundeslandvergleich MINT-Anteil", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_mint_map_ui("mod_schule_kurse_mint_map_ui_1"),
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_mint_map_kurse")),
                                                            color = "#154194"),
                               shinyBS::bsPopover(id="h_schule_fach_1", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotNicht-MINT&quot bezieht sich auf die Belegungszahlen in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei Vorgaben der Bundesländer", "<br> <br> In Bayern sind Deutsch & Mathematik vergleichbar mit Leistungskursen, weiter Kurse können, müssen aber nicht demselben Niveau entsprechen.", "<br><br> Mit Grundkursen sind nach der Definition der KMK Fächer mit bis zu 3 Wochenstunden gemeint.<br> Mit Leistungskursen Fächer mit mindestens 4 Wochenstunden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_fach_1")
                             )
                    )

      )))),

  # Box 2 ----

    fluidRow(id="schule_fach",
      shinydashboard::box(
        title = "M-I-N-T: Die Hälfte der MINT-Leistungskursbelegungen sind in Mathematik.",
        width = 12,
        column(
          width = 8,
        p("Zoomt man auf die MINT-Fächer, zeigt sich: In den Leistungskursen heißt
          MINT zum Großteil Mathematik. 47 % der MINT-Leistungskursbelegungen sind in Mathematik, gefolgt von Biologie (30 %)
          und Physik (11 %).
          Kaum Oberstufenbelegungen entfallen dagegen auf Informatik."),
        p("Das hängt mit den Wahlmöglichkeiten in der Oberstufe zusammen.
          Während Mathematik oft ein Pflichtfach für das Abitur ist und auf gehobenem Leistungsniveau
          angeboten wird, sind Oberstufenangebote in Informatik weniger verbreitet.",
        )
        ),
        column(
          width = 12,
        tabsetPanel(type = "tabs",
                    tabPanel("Aktueller Anteil MINT-Fächer", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_multiple_mint_ui("mod_schule_kurse_multiple_mint_ui_1")
                             ),

                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(uiOutput(ns("plot_waffle_mint")),
                                                            color = "#154194"),
                               shinyBS::bsPopover(id="h_schule_mint_1", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotNicht-MINT&quot bezieht sich auf die Belegungszahlen in den Grund- und Leistungskursen der Oberstufe. Es wird der Anteil von MINT-Belegungen an allen Belegungen betrachtet. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> Durch Rundungen kann es zu minimalen Abbweichungen zwischen den Grafiken kommen.", "<br><br> Mit Grundkursen sind nach der Definition der KMK Fächer mit bis zu 3 Wochenstunden gemeint.<br> Mit Leistungskursen Fächer mit mindestens 4 Wochenstunden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_mint_1")

                             )
                    ),
                    tabPanel("MINT-Fächer im Zeitverlauf", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_verlauf_bl_subjects_ui("mod_schule_kurse_verlauf_bl_subjects_ui_1"),
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_verlauf_kurse_bl_subjects")),
                                                            color = "#154194"),
                               shinyBS::bsPopover(id="h_schule_fach_3", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotNicht-MINT&quot bezieht sich auf die Belegungszahlen in den Grund- und Leistungskursen der Oberstufe.", "<br><br> Mit Grundkursen sind nach der Definition der KMK Fächer mit bis zu 3 Wochenstunden gemeint.<br> Mit Leistungskursen Fächer mit mindestens 4 Wochenstunden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_fach_3")

                             )),

                    tabPanel("Bundeslandvergleich MINT-Fächer", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_map_ui("mod_schule_kurse_map_ui_1"),
                               ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_map_kurse")),
                                                            color = "#154194"),
                               shinyBS::bsPopover(id="h_schule_fach_1l", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotNicht-MINT&quot bezieht sich auf die Belegungszahlen in den Grund- und Leistungskursen der Oberstufe.", "<br><br> Mit Grundkursen sind nach der Definition der KMK Fächer mit bis zu 3 Wochenstunden gemeint.<br> Mit Leistungskursen Fächer mit mindestens 4 Wochenstunden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_fach_1l")
                               )
                    ),

        )

        ))),
  # Box 3 ----
    fluidRow(id="schule_frauen",
      shinydashboard::box(
        title = "Frauen in MINT: MINT-Belegungen unter Mädchen und Jungen ungefähr gleich häufig.",
        width = 12,
        column(
          width = 8,
        p("Der Mädchenanteil in MINT-Oberstufenfächern liegt bei gut der Hälfte.
        In \"Nicht-MINT\"-Fächern machen Mädchen 55 % der Kursteilnehmenden aus.
        Diese Zahlen zeigen auf subtile Weise, dass insgesamt mehr Jungen als Mädchen auf Gymnasien sind
        und dass Jungen im Verhältnis etwas wahrscheinlicher MINT wählen."),
        p("Diese ähnliche Kurswahl hängt vielleicht auch mit Kursbelegungsvorgaben zusammen,
        nach denen oft alle mindestens ein oder mehrere MINT-Fächer wählen müssen.
        Was sich unterscheidet, ist, welche MINT-Fächer eher von Jungen oder Mädchen belegt werden.
          Der Mädchenanteil in Biologie-Leistungs- und -Grundkursen liegt bei um die 60 %.
          In Physik-Leistungskursen ist der Mädchenanteil dagegen bei knapp einem Viertel,
          in Informatik-Leistungskursen bei rund 16 %.")
        ),
        column(
          width = 12,
        tabsetPanel(type = "tabs",

                    tabPanel("Aktueller Anteil Mädchen in MINT", br(),


                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_schule_kurse_comparison_gender_ui("mod_schule_kurse_comparison_gender_ui_1"),
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(uiOutput(ns("plot_comparison_gender")),
                                                            color = "#154194"),
                               shinyBS::bsPopover(id="h_schule_frauen_1", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotNicht-MINT&quot bezieht sich auf die Belegungszahlen in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden.", "<br><br> Mit Grundkursen sind nach der Definition der KMK Fächer mit bis zu 3 Wochenstunden gemeint.<br> Mit Leistungskursen Fächer mit mindestens 4 Wochenstunden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_frauen_1")
                               )
                    ),
                    tabPanel("Zeitverlauf Mädchenanteil in MINT", br(), #kann raus

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_schule_kurse_verlauf_gender_ui("mod_schule_kurse_verlauf_gender_ui_1")
                               ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_verlauf_gender")),
                                                            color = "#154194"),
                               shinyBS::bsPopover(id="h_schule_frauen_123", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotNicht-MINT&quot bezieht sich auf die Belegungszahlen in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_frauen_123")

                             )
                    ),

                    tabPanel("MINT-Wahlverhalten nach Geschlecht", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_schule_kurse_multiple_ui("mod_schule_kurse_multiple_ui_1")),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_wahl")),
                                                            color = "#154194"),
                               shinyBS::bsPopover(id="h_schule_mint_4", title = "",
                                                  content = paste0("Der Anteil und die Anzahl von &quotMINT&quot vs. &quotNicht-MINT&quot bezieht sich auf die Belegungszahlen in den Grund- und Leistungskursen der Oberstufe. Die möglichen Belegungen sind dabei auch von den Vorgaben der Bundesländer und dem Angebot der Schulen abhängig.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden." , "<br> <br> Durch Rundungen kann es zu minimalen Abbweichungen zwischen den Grafiken kommen.", "<br><br> Mit Grundkursen sind nach der Definition der KMK Fächer mit bis zu 3 Wochenstunden gemeint.<br> Mit Leistungskursen Fächer mit mindestens 4 Wochenstunden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_schule_mint_4")
                             )
                    )
                        )

                    ))),
  # Box 4 ----

      fluidRow(id="schule_kompetenz",
           shinydashboard::box(
             title = "MINT-Kompetenzen: MINT-Kompetenzen nehmen weiter ab. IQB-Ergebnisse der letzten Jahre.",
             width = 12,
             column(
               width = 8,
             p("Diese interaktiven Diagramme geben einen Einblick in die Mathematik-Kompetenzen von Schüler:innen der 4. und 9. Klassen.
             Die Daten stammen aus der Befragung des Instituts zur Qualitätsentwicklung im Bildungswesen e. V. (IQB), das in regelmäßigen Abständen
             die Leistung von Schüler:innen in verschiedenen Fächern testet. Dafür werden deutschlandweit mehr als 1.300 Schulen und
               über 26.000 Schüler:innen befragt."),
             p("Die Ergebnisse aus dem Jahr 2021 zeigen: Jede:r fünfte Viertklässler:in beherrscht die nötigen Grundlagen in der Mathematik nicht.
               In der Befragung zehn Jahre zuvor waren es nur jede:r zehnte. Mädchen sowie Schüler:innen mit Zuwanderungsgeschichte und niedrigem sozialem Status
               schneiden im Mittel schlechter ab. Dabei zeigt ein näherer Blick auf die Unterschiede zwischen Mädchen und Jungen, dass sich Mädchen auch weniger für Mathematik
               interessieren und ihre Fähigkeiten im Fach schlechter einschätzen."),
             p(),
             shinyBS::bsPopover(id="i_schule_kompetenz_1", title = "",
                                content = paste0("Im Bericht des IQB-Bildungstrends 2021 kann man weitere Informationen und eine Einordnung der dargestellten Daten finden. <br> <a>https://www.iqb.hu-berlin.de/bt/BT2021/Bericht/</a> <br><br> Weitere Informationen zum Thema Diversität und soziale Herkunft in der Bildungswelt können in der Diversitätsstudie von MINTvernetzt nachgelesen werden. <br> <a> https://mint-vernetzt.de/data/daten-fakten#mint-studie-diversitaet </a>"),
                                placement = "right",
                                trigger = "click"),
             tags$a(paste0("Interessieren Sie sich für weitere Informationen zum Thema, klicken Sie hier:"), icon("info-circle"), id = "i_schule_kompetenz_1"),
             p()
             ),
             column(
               width = 12,
             tabsetPanel(type = "tabs",

                         tabPanel("Leistungsschwache Schüler:innen in Mathematik", br(),

                                  tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                                  shiny::sidebarPanel(
                                    width = 3,
                                    mod_schule_kurse_iqb_standard_zeitverlauf_ui("mod_schule_kurse_iqb_standard_zeitverlauf_ui_1"),
                                  ),
                                  shiny::mainPanel(
                                    width = 9,
                                    shinycssloaders::withSpinner(htmlOutput(ns("plot_iqb_standard_zeitverlauf")),
                                                                 color = "#154194"),
                                    shinyBS::bsPopover(id="h_schule_kompetenz_1", title = "",
                                                       content = paste0("Für Mecklenburg-Vorpommern liegen keine Daten vor, da pandemiebedingt nicht genug Testungen realisiert werden konnten.", "<br> <br> Gesamte realisierte Stichprobengröße:", "<br> 2021: 1.464 Schulen mit N = 26.844 Schüler:innen", "<br> 2018: 1.462 Schulen mit N = 44.941 Schüler:innen", "<br> 2016: 1.508 Schulen mit N = 29.259 Schüler:innen", "<br> 2012: 1.326 Schulen mit N = 44.584 Schüler:innen", "<br> 2011: 1.349 Schulen mit N = 27.081 Schüler:innen"),
                                                       placement = "top",
                                                       trigger = "hover"),
                                    tags$a(paste0("Hinweis zu den Daten und Stichprobengröße"), icon("info-circle"), id = "h_schule_kompetenz_1")

                                  )
                         ),
                         tabPanel("Leistung Mathematik im Gruppenvergleich", br(),

                                  tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                                  shiny::sidebarPanel(
                                    width = 3,
                                    mod_schule_kurse_iqb_mathe_mittel_zeitverlauf_ui("mod_schule_kurse_iqb_mathe_mittel_zeitverlauf_ui_1"),
                                  ),
                                  shiny::mainPanel(
                                    width = 9,
                                    shinycssloaders::withSpinner(htmlOutput(ns("plot_iqb_mathe_mittel_zeitverlauf")),
                                                                 color = "#154194"),
                                    shinyBS::bsPopover(id="def_schule_kompetenz_2", title = "",
                                                       content = paste0("Mit Zuwanderungsgeschichte = Kinder, deren beider Eltern nach Deutschland zugewandert sind. Zuwanderungsgeschichten 1. Generation (auch Kind ist nach Deutschland zugewandert) und 2. Generation (Kind ist in Deutschland geboren) werden zusammengefasst. <br> Ohne Zuwanderungsgeschichte = Kinder, deren beider Eltern in Deutschland geboren wurden.", "<br> <br> Bildungskapital = Ressourcen, Kinder durch (kulturelle) Bildung zu fördern, und Indikator für den sozialen Status der Eltern. Erfasst wurde das Bildungskapital durch die Anzahl an Büchern im Haushalt (hoch = mehr als 100 Bücher zuhause).", "<br><br>sozialer Status = die soziale Position der Eltern. Hier wurden sozialer Status und Anforderungen der Berufe betrachtet."),
                                                       placement = "top",
                                                       trigger = "hover"),
                                    tags$a(paste0("Definition der Begriffe"), icon("info-circle"), id = "def_schule_kompetenz_2"),
                                    br(),
                                    br(),
                                    shinyBS::bsPopover(id="h_schule_kompetenz_2", title = "",
                                                       content = paste0("Für Mecklenburg-Vorpommern liegen keine Daten vor, da pandemiebedingt nicht genug Testungen realisiert werden konnten.", "<br><br>Für einzelne Bundesländer liegen in bestimmten Bedingungen oder zu bestimmten Zeitpunkten keine Daten vor. In diesen Fällen stehen die betroffenen Bundesländer nicht zur Auswahl oder betroffene Jahre werden nicht angezeigt.", "<br> <br> Gesamte realisierte Stichprobengröße:", "<br> 2021: 1.464 Schulen mit N = 26.844 Schüler:innen", "<br> 2018: 1.462 Schulen mit N = 44.941 Schüler:innen", "<br> 2016: 1.508 Schulen mit N = 29.259 Schüler:innen", "<br> 2012: 1.326 Schulen mit N = 44.584 Schüler:innen", "<br> 2011: 1.349 Schulen mit N = 27.081 Schüler:innen"),
                                                       placement = "top",
                                                       trigger = "hover"),
                                    tags$a(paste0("Hinweis zu den Daten und Stichprobengröße"), icon("info-circle"), id = "h_schule_kompetenz_2")
                                     )
                         ),
                         tabPanel("Interesse und Selbsteinschätzung", br(),

                                  tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                                  shiny::sidebarPanel(
                                    width = 3,
                                    mod_schule_kurse_iqb_fragen_ui("mod_schule_kurse_iqb_fragen_ui_1"),
                                  ),

                                  shiny::mainPanel(
                                    width = 9,
                                    shinycssloaders::withSpinner(htmlOutput(ns("plot_iqb_fragebogen")),
                                                                 color = "#154194"),
                                    shinyBS::bsPopover(id="h_schule_kompetenz_3", title = "",
                                                       content = paste0("Das Interesse und die Einschätzung der eigenen Fähigkeiten (fachspezifisches Selbstkonzept) wurden durch mehrere Fragen in einem Fragebogen erfasst, auf einer Skala von 1 bis 4.", "<br> <br> Gesamte realisierte Stichprobengröße:", "<br> 2021: 1.464 Schulen mit N = 26.844 Schüler:innen", "<br> 2016: 1.508 Schulen mit N = 29.259 Schüler:innen", "<br> 2011: 1.349 Schulen mit N = 27.081 Schüler:innen"),
                                                       placement = "top",
                                                       trigger = "hover"),
                                    tags$a(paste0("Hinweis zu den Daten und zur Stichprobengröße"), icon("info-circle"), id = "h_schule_kompetenz_3")
                                  )
                         )
             )
             ))),


  # Kurzanalyse-Box ----
  div(class = "content-box",
      div(class = "inner-box",
      p(br(),"KURZANALYSE", br()),
      p(style = "font-size = 24",
        strong("MINT-Kompetenztests wie die IQB-Befragung zeigen systematische Gruppenunterschiede in den MINT-Kompetenzen.
               Zwei dieser Unterschiede ordnen wir in jeweils einer Kurzanalyse ein: Wir schauen darauf, woher die Unterschiede
               zwischen Jungen und Mädchen in Mathematik kommen können und wie man Mädchen hier fördern kann. Außerdem betrachten wir
               in unserer Kurzanalyse zum Thema Teilhabe den Einfluss der sozialen Herkunft auf die Schulzeit und wie MINT-Förderung zu Chancengerechtigkeit
                beitragen kann."),
        br(), br(),
        tags$a(href = "https://www.mint-vernetzt.de/content/uploads/2024/02/MIN_Kurzanalyse_Gender_final.pdf",
               target = "_blank", "Link zu der Kurzanalyse Schule: Mädchen & MINT"),
        p("Veröffentlichung: 16. März 2024",
          br(),
          "Zitiervorschlag: MINTvernetzt (2024). Kurzanalyse. Schule: Mädchen & MINT."),
        br(),
        tags$a(href = "https://www.mint-vernetzt.de/content/uploads/2024/02/MIN_Kurzanalyse_Teilhabe_final.pdf",
               target = "_blank", "Link zu der Kurzanalyse Mit MINT-Förderung zu mehr Chancengerechtigkeit"),
        br(), br(),
        p("Veröffentlichung: 16. Februar 2024",
          br(),
          "Zitiervorschlag: MINTvernetzt (2024). Kurzanalyse. Mit MINT-Förderung zu mehr Chancengerechtigkeit"))
      )
  ),


    # Footer
  funct_footer())

}

#' schule_kurse Server Functions
#'
#' @noRd
mod_schule_kurse_server <- function(id, r){

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Box 1 - Wer wählt MINT ----
    ## Waffle
    output$plot_waffle_mint <- renderUI({
      kurse_waffle_mint(r)
    })

    ## Balkendiagramm

    output$plot_einstieg_comparison <- renderUI({
      plot_list <- kurse_einstieg_comparison(r)
      r$plot_einstieg_comparison <- plot_list

      r$plot_einstieg_comparison_title <- get_plot_title(
        plot = r$plot_einstieg_comparison
      )

      plot_list

    })

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

    output$plot_einstieg_verlauf <- renderUI({
      plot_list <- kurse_verlauf_single(r)
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

    ## Waffle Geschlecht
    output$plot_wahl <- renderUI({
      kurse_wahl(r)
    })

    ## Karte Gender

    output$plot_map_kurse_gender <- renderUI({
      plot_list <- kurse_map_gender(r)

      # return plots
      out <- highcharter::hw_grid(
        plot_list,
        ncol = 2)
      out

    })


    output$plot_mint_map_kurse <- renderUI({
      plot_list <- kurse_mint_map(r)


      plot_list

    })



    # Box 2 -  M-I-N-T ----

    output$plot_map_kurse <- renderUI({
      kurse_map(r)
    })

    ## Karte Fächer

    output$plot_verlauf_multiple <- renderUI({
      plot_list <- kurse_verlauf_multiple_bl(r)
      r$plot_verlauf_multiple <- plot_list

      r$plot_verlauf_multiple_title <- get_plot_title(
        plot = r$plot_verlauf_multiple
      )

      plot_list

    })

    output$download_btn_plot_verlauf_multiple <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_verlauf_multiple_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_verlauf_multiple,
          filename =  r$plot_verlauf_multiple_title,
          width = 700,
          height = 400)

        file.copy(r$plot_verlauf_multiple_title, file)
        file.remove(r$plot_verlauf_multiple_title)
      }
    )


    output$plot_verlauf_kurse_bl_subjects <- renderUI({
      plot_list <- kurse_verlauf_subjects_bl(r)
      r$plot_verlauf_kurse_bl_subject <- plot_list

      r$plot_verlauf_kurse_bl_subject_title <- get_plot_title(
        plot = r$plot_verlauf_kurse_bl_subject
      )

      plot_list

    })

    output$download_btn_plot_verlauf_kurse_bl_subjects <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_verlauf_kurse_bl_subject_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_verlauf_kurse_bl_subject,
          filename =  r$plot_verlauf_kurse_bl_subject_title,
          width = 700,
          height = 400)

        file.copy(r$plot_verlauf_kurse_bl_subject_title, file)
        file.remove(r$plot_verlauf_kurse_bl_subject_title)
      }
    )

    ## Dumbbell-Plot Mädchen
    output$plot_ranking_gender <- renderPlot({
      kurse_ranking_gender(r)
    })

    output$plot_comparison_subjects <- renderUI({
      plot_list <- kurse_mint_comparison(r)
      r$plot_comparison_subjects <- plot_list

      r$plot_comparison_subjects_title <- get_plot_title(
        plot = r$plot_comparison_subjects
      )

      plot_list

    })

    output$download_btn_plot_comparison_subjects <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_comparison_subjects_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_comparison_subjects,
          filename =  r$plot_comparison_subjects_title,
          width = 700,
          height = 400)

        file.copy(r$plot_comparison_subjects_title, file)
        file.remove(r$plot_comparison_subjects_title)
      }
    )


    ## Balken Übersicht BuLas

    output$plot_comparison_bl <- renderUI({
      plot_list <- kurse_mint_comparison_bl(r)
      r$plot_comparison_bl <- plot_list

      r$plot_comparison_bl_title <- get_plot_title(
        plot = r$plot_comparison_bl
      )

      plot_list

    })

    output$download_btn_plot_comparison_bl <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_comparison_bl_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_comparison_bl,
          filename =  r$plot_comparison_bl_title,
          width = 700,
          height = 400)

        file.copy(r$plot_comparison_bl_title, file)
        file.remove(r$plot_comparison_bl_title)
      }
    )


    # Box 3 - Frauen ----

    output$plot_comparison_gender <- renderUI({
      kurse_comparison_gender(r)


    })


    output$plot_verlauf_gender <- highcharter::renderHighchart({
      kurse_verlauf_gender(r)
    })



    # Box 4  Kompetenzdaten / IQB ----

    # Tab 1

    output$plot_iqb_standard_zeitverlauf <- renderUI({
      plot_list <- iqb_standard_zeitverlauf(r)
      r$plot_iqb_standard_zeitverlauf <- plot_list

      r$plot_iqb_standard_zeitverlauf_title <- get_plot_title(
        plot = r$plot_iqb_standard_zeitverlauf
      )

      plot_list

    })

    output$download_btn_plot_iqb_standard_zeitverlauf <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_iqb_standard_zeitverlauf_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_iqb_standard_zeitverlauf,
          filename =  r$plot_iqb_standard_zeitverlauf_title,
          width = 700,
          height = 400)

        file.copy(r$plot_iqb_standard_zeitverlauf_title, file)
        file.remove(r$plot_iqb_standard_zeitverlauf_title)
      }
    )
    # Tab 2



    output$plot_iqb_mathe_mittel_zeitverlauf <- renderUI({
      plot_list <- iqb_mathe_mittel_zeitverlauf(r)
      r$plot_iqb_mathe_mittel_zeitverlauf <- plot_list

      r$plot_iqb_mathe_mittel_zeitverlauf_title <- get_plot_title(
        plot = r$plot_iqb_mathe_mittel_zeitverlauf
      )

      plot_list

    })

    output$download_btn_plot_iqb_mathe_mittel_zeitverlauf <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_iqb_mathe_mittel_zeitverlauf_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_iqb_mathe_mittel_zeitverlauf,
          filename =  r$plot_iqb_mathe_mittel_zeitverlauf_title,
          width = 700,
          height = 400)

        file.copy(r$plot_iqb_mathe_mittel_zeitverlauf_title, file)
        file.remove(r$plot_iqb_mathe_mittel_zeitverlauf_title)
      }
    )

    # Tab 3

    output$plot_iqb_fragebogen <- renderUI({
      plot_list <- iqb_fragebogen(r)
      r$plot_iqb_fragebogen <- plot_list

      r$plot_iqb_fragebogen_title <- get_plot_title(
        plot = r$plot_iqb_fragebogen
      )

      plot_list

    })

    output$download_btn_plot_iqb_fragebogen <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_iqb_fragebogen_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_iqb_fragebogen,
          filename =  r$plot_iqb_fragebogen_title,
          width = 700,
          height = 400)

        file.copy(r$plot_iqb_fragebogen_title, file)
        file.remove(r$plot_iqb_fragebogen_title)
      }
    )

    ### Rest




  })
}

## To be copied in the UI
# mod_schule_kurse_ui("schule_kurse_1")

## To be copied in the server
# mod_schule_kurse_server("schule_kurse_1")
