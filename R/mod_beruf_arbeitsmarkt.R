#' beruf_arbeitsmarkt UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_ui <- function(id){
  ns <- NS(id)
  tagList(

    # Banner
      fluidRow(
        div(class = "clean-box",
            column(
              width = 12,
              img(src='www/Banner_Ausbildung_Beruf.avif',
                  class = "img-responsive",
                  #height = "150px", width = "150px",
                  alt = "Banner Beruf",
                  style="display: block; margin-left: auto; margin-right: auto; margin-bottom: 20px;"
              )))),


      # Info-Texte

      fluidRow(
        shinydashboard::box(
          h2("Auf dieser Seite"),
          width = 7,
          p(style = "text-align: left; font-size = 16px",
            "Auf dieser Seite zeigen wir statistische Kennzahlen rund um MINT im Bereich Arbeitsmarkt.
           Dabei unterscheiden wir zwischen Auszubildenden und (sozialversicherungspflichtigen) Beschäftigten.
           Die Kategorisierung der MINT-Fächer entspricht den Klassifikationen der Bundesagentur für Arbeit.
            Weitere Informationen dazu finden Sie auf der Unterseite \"Hinweise & Datenquellen\".")
        ),

        shinydashboard::box(
          title = "Fragen oder Feedback?",
          width = 5,
          p(style = "text-align: left; font-size = 16px",
            "Sind alle Zahlen und Grafiken verständlich dargestellt?", br(), "Wir freuen uns über Rückfragen oder Feedback ", tags$a(href = "mailto:katharina.brunner@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per E-Mail"),"oder über eine Teilnahme an unserer kurzen",
            tags$a(href="https://survey.lamapoll.de/MINT-DataLab_Feedback/", "Umfrage", target="_blank"), "!"
          ))
      ),

      fluidRow(
        shinydashboard::box(
          h2("Themenübersicht"),
          width = 7,
          p(style = "text-align: left; font-size = 16px",tags$a(href="#beruf_mint",

            span(tags$b(span("MINT-Anteil:")))),"Jede:r Fünfte arbeitet in MINT-Berufen."

            ),
          p(style = "text-align: left; font-size = 16px",tags$a(href="#beruf_fach",
            span(tags$b(span("→ M-I-N-T:")))),"MINT oder IT - über 90 % sind in Informatik- oder Technikberufen tätig."

            ),
          p(style = "text-align: left; font-size = 16px",tags$a(href="#beruf_frauen",

            span(tags$b(span("→ Frauen in MINT:")))),"Frauenanteil in MINT-Berufen bei 18 %."
            ),
          p(style = "text-align: left; font-size = 16px",tags$a(href="#beruf_regional",
            span(tags$b(span("→ Regionale Daten:")))),"MINT-Daten aus Ihrem Landkreis."
            ),
           p(style = "text-align: left; font-size = 16px",tags$a(href="#beruf_entgelt",
                    span(tags$b(span("→ Entgelte: ")))),"In MINT-Berufen verdient man überdurchschnittlich gut."
                  )),


        shinydashboard::box(
          title = "Datenquellen",
          width = 5,
          p("Die amtlichen Statistiken zeigen das aktuellste verfügbare Berichtsjahr 2023."),
          p(tags$b(span("Auszubildenden- und Beschäftigtenzahlen: Bundesagentur für Arbeit 2024, auf Anfrage.")),
          "Daten des Berichtsjahres 2024 ca. ab Juli 2025 verfügbar.")

        )
      ),
###h

      # Box 1 ----

    fluidRow( id="beruf_mint",
      shinydashboard::box(

        h2("MINT-Anteil: Jede:r Fünfte arbeitet in MINT-Berufen."),

        width = 12,
        column(
          width = 8,
        p("Im Jahr 2023 arbeiten 23 % der sozialversicherungspflichtig Beschäftigten in einem MINT-Beruf und 77 % in anderen Bereichen.
        Bei den Auszubildenden ist der Anteil derer, die in einem MINT-Beruf lernen, sogar bei 30 %."),
        p("Die Zahl an MINT-Beschäftigten ist in den letzten 10 Jahren leicht gestiegen,
          von 7 Mio. im Jahr 2014 auf 7,9 Mio. im Jahr 2023. Die Zahl der Auszubildenden hat im selben Zeitraum etwas abgenommen,
          von rund 406.000 auf 390.000.")
        ),
        column(
          width = 12,

        tabsetPanel(type = "tabs",
                    # Tab 1
                    tabPanel("Aktueller MINT-Anteil", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_vergleich_ui("mod_beruf_arbeitsmarkt_einstieg_vergleich_ui_1"),
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_einstieg_vergleich")),
                                                            color = "#154194"),

                               shinyBS::bsPopover(id = "h_beruf_mint_3", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot"),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_mint_3")
                             )
                    ),
                    # Tab 2
                    tabPanel("MINT-Anteil im Zeitverlauf", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_verlauf_ui("mod_beruf_arbeitsmarkt_einstieg_verlauf_ui_1"),
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_einstieg_verlauf")),
                                                            color = "#154194"),

                               shinyBS::bsPopover(id = "h_beruf_mint_2", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot"),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_mint_2"),

                               )
                    ),
                    # tabPanel("Pie (RAUS)", br(),

                    tabPanel("Bundeslandvergleich", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_mint_bula_ui("mod_beruf_arbeitsmarkt_mint_bula_ui_1"),
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_beruf_arbeitsmarkt_mint_bulas")),
                                                            color = "#154194"),


                               shinyBS::bsPopover(id = "h_beruf_mint_7", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot"),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_mint_7")
                             )
                    ),

        )
                    ))),

    # Kurzanalyse-Box ----
    div(class = "content-box",
        div(class = "inner-box",
        p(br(),"KURZANALYSE", br()),
        p(style = "font-size = 24",
          strong("Die Anzahl an MINT-Auszubildenden ist in den letzten Jahren rückläufig gewesen.
          Während es 2020 rund 450.000 Auszubildende in MINT gab, sind es 2022 noch 400.000.
          Auch die Zahl der Studienanfänger:innen hat in den letzten Jahren stetig abgenommen.
          In einer Kurzanalyse fassen wir die Entwicklungen im MINT-Nachwuchs zusammen."),
          br(), br(),
          tags$a(href = "https://www.mint-vernetzt.de/content/uploads/2024/02/MIN_Kurzanalyse_Interesse_final.pdf",
                 target = "_blank", "Link zur Kurzanalyse"),
          br(), br())
        ),
        p("Veröffentlichung: 16. Februar 2024",
          br(),
          "Zitiervorschlag: MINTvernetzt (2024). Kurzanalyse. Wird MINT unbeliebter?.")
    ),

    # Box 2 ----

    fluidRow(id="beruf_fach",
      shinydashboard::box(
        h2("M-I-N-T: MINT oder I/T - über 90 % sind in Informatik- oder Technikberufen tätig."),
        width = 12,
        column(
          width = 8,
        p("Zoomt man auf den MINT-Fachbereich, sieht man, dass rund 80 % der MINT-Beschäftigten in einem technischen oder Ingenieurberuf arbeiten.
          Weitere gut 14 % sind beruflich im Bereich Informatik tätig. Die restlichen 6 % arbeiten in Mathematik oder Naturwissenschaft.")
        ),
        column(
          width = 12,

        tabsetPanel(type = "tabs",
                    # Tab Boxen Fächer
                    tabPanel("Aktueller Anteil MINT-Disziplinen", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_anforderungen_ui("mod_beruf_arbeitsmarkt_anforderungen_ui_1"),

                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_arbeitsmarkt_faecher_anteil")),
                                                            color = "#154194"),



                               shinyBS::bsPopover(id = "h_beruf_mint_1", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot", "<br> <br> Durch Rundung der berechneten Werte kann es zu minimalen Abweichungen zwischen den Grafiken kommen."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_mint_1")
                             )
                    ),


                    tabPanel("MINT-Disziplinen im Zeitverlauf", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             column(
                               width = 3,
                               shiny::sidebarPanel(
                                 width = 12,
                                 mod_beruf_arbeitsmarkt_faecher_verlauf_ui("mod_beruf_arbeitsmarkt_faecher_verlauf_ui_1"),
                               ),
                               tags$img(src = "www/ti.png", style = "margin-top: 10px; width: 90%; display: block; margin-left: auto; margin-right: auto;")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_arbeitsmarkt_verlauf_faecher")),
                                                            color = "#154194"),

                               shinyBS::bsPopover(id = "h_beruf_fach_mint_2", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot"),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_fach_mint_2")
                             )
                    ),
                    tabPanel("Bundeslandvergleich MINT-Disziplinen", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_bula_faecher_ui("mod_beruf_arbeitsmarkt_bula_faecher_ui_1"),

                               #   icon = icon("download")),
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_arbeitsmarkt_faecher_bl")),
                                                            color = "#154194"),

                               shinyBS::bsPopover(id = "h_beruf_fach_1", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot"),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_fach_1")
                             )
                    )
        )
                    #
        ))),

    # Box 3 ----

    fluidRow(id="beruf_frauen",
      shinydashboard::box(

        h2("Frauen in MINT: Frauenanteil in MINT-Berufen bei 18 %."),

        width = 12,
        column(
          width = 8,
        p("Der Frauenanteil in MINT-Berufen liegt bei nur 17,6 %, in Ausbildungen sogar nur bei 13,5 %.
        In \"Nicht-MINT\"-Berufen und -Ausbildungen sind Frauen dagegen die Mehrheit.
        In den letzten zehn Jahren ist der Frauenanteil nur leicht gestiegen.
        Blickt man auf die absolute Anzahl an weiblichen MINT-Beschäftigten, ist diese in den letzten zehn Jahren allerdings durchaus gewachsen
        - um 265.000. Das spricht dafür, dass zwar mehr Frauen, aber auch mehr Menschen insgesamt in MINT-Berufen arbeiten."),
        p("Manche MINT-Ausbildungen sind bei Frauen besonders beliebt.
          2024 waren die Top-Ausbildungsberufe mit den meisten neuen weiblichen Azubis
          Augenoptiker:in,  Kfz-Mechatroniker:in und Mediengestalter:in.")
        ),
        column(
          width = 12,

        tabsetPanel(type = "tabs",
              # tab 1
                    tabPanel("Frauenanteil in MINT", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_gender_ui("mod_beruf_arbeitsmarkt_einstieg_gender_ui_1")
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_einstieg_pie_gender")),
                                                            color = "#154194"),
                               shinyBS::bsPopover(id = "h_beruf_frauen_1", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot", "<br> <br> In den vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_frauen_1")
                             )
                    ),
            # tab 2
                    tabPanel("Zeitverlauf Frauen in MINT", br(),
                             shiny::sidebarPanel(
                               width = 3,
                               tags$style(".well {background-color:#FFFFFF;}"),
                               tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                               mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_ui("mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_ui_1"),

                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_einstieg_verlauf_gender")),
                                                            color = "#154194"),
                               shinyBS::bsPopover(id = "h_beruf_frauen_2", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot", "<br> <br> In den vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_frauen_2")
                             )
                    ),

            # Wahl nach Geschlecht in Boxen
            tabPanel("Berufswahlverhalten nach Geschlecht", br(),

                     tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                     shiny::sidebarPanel(
                       width = 3,
                       mod_beruf_arbeitsmarkt_anforderungen_gender_ui("mod_beruf_arbeitsmarkt_anforderungen_gender_ui_1")
                     ),
                     shiny::mainPanel(
                       width = 9,
                       shinycssloaders::withSpinner(htmlOutput(ns("plot_arbeitsmarkt_wahl_gender")),
                                                    color = "#154194"),


                       shinyBS::bsPopover(id = "h_beruf_mint_4", title = "",
                                          content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot", "<br> <br> In den vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden.", "<br> <br> Durch Rundung der berechneten Werte kann es zu minimalen Abweichungen zwischen den Grafiken kommen."),
                                          placement = "top",
                                          trigger = "hover"),
                       tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_mint_4")
                     )
            ),
            tabPanel("Top-MINT-Ausbildungsberufe nach Geschlecht", br(),

                     shiny::sidebarPanel(
                       width = 3,
                       mod_beruf_arbeitsmarkt_top10_ui("mod_beruf_arbeitsmarkt_top10_ui_1"),
                     ),
                     shiny::mainPanel(
                       width = 9,
                       shinycssloaders::withSpinner(htmlOutput(ns("plot_arbeitsmarkt_top10")),
                                                    color = "#154194"),

                       shinyBS::bsPopover(id = "h_beruf_fach_4", title = "",
                                          content = paste0("Hier gezeigt werden nur neue Auszubildende im Fachbereich MINT des jeweiligen Jahres. Ausbildungsberufe mit weniger als 50 neuen Vertragsabschlüssen für das betrachtete Jahr wurden ausgeschlossen. <br><br>In manchen Fällen weisen mehr als zehn Berufe einen Männeranteil von 100 % auf. In diesen Fällen sind die zehn Berufe mit einem Männeranteil von 100 % angezeigt, welche die meisten Neu-Auszubildenden haben.", "<br> <br> In den vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden."),
                                          placement = "top",
                                          trigger = "hover"),
                       tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_fach_4")
                     )
            ),
            tabPanel("Frauenanteil im MINT-Fächervergleich", br(),

                     tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                           .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                     shiny::sidebarPanel(
                                         width = 3,
                                         mod_beruf_arbeitsmarkt_anforderungen_frauen_ui("mod_beruf_arbeitsmarkt_anforderungen_frauen_ui_1"),

                     ),
                     shiny::mainPanel(
                       width = 9,
                       shinycssloaders::withSpinner(htmlOutput(ns("plot_arbeitsmarkt_faecher_anteil_frauen")),
                                                    color = "#154194"),



                       shinyBS::bsPopover(id = "h_beruf_mint_1_frauen", title = "",
                                          content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot", "<br> <br> Durch Rundung der berechneten Werte kann es zu minimalen Abweichungen zwischen den Grafiken kommen."),
                                          placement = "top",
                                          trigger = "hover"),
                       tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_mint_1_frauen")
                     )
            ),

            # ,
            # tabPanel("Frauenanteil in MINT-Fächern", br(),
            #
            #          shiny::sidebarPanel(
            #            width = 3,
            #            mod_beruf_arbeitsmarkt_frauenfaecherteil_ui("mod_beruf_arbeitsmarkt_top10_ui_1"),
            #            # br(),br()
            #            # ,
            #            # downloadButton(
            #            #   outputId = ns("download_btn_plot_arbeitsmarkt_top10_1"),
            #            #   label = "Download (links)",
            #            #   icon = icon("download")),
            #            # downloadButton(
            #            #   outputId = ns("download_btn_plot_arbeitsmarkt_top10_2"),
            #            #   label = "Download (rechts)",
            #            #   icon = icon("download")),
            #          ),
            #          shiny::mainPanel(
            #            width = 9,
            #            shinycssloaders::withSpinner(htmlOutput(ns("plot_arbeitsmarkt_top10")),
            #                                         color = "#154194"),
            #
            #            p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
            #            shinyBS::bsPopover(id = "h_beruf_fach_4", title = "",
            #                               content = paste0("Hier gezeigt werden nur neue Auszubildende im Fachbereich MINT des jeweiligen Jahres. Ausbildungsberufe mit weniger als zehn neuen Vertragsabschlüssen für das betrachtete Jahr wurden ausgeschlossen. <br><br>In manchen Fällen weisen mehr als zehn Berufe einen Männeranteil von 100 % auf. In diesen Fällen sind die zehn Berufe mit einem Männeranteil von 100 % angezeigt, welche die meisten Neu-Auszubildenden haben.", "<br> <br> In den vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden."),
            #                               placement = "top",
            #                               trigger = "hover"),
            #            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_fach_4")
            #          )
            # ),
        )
        ))),

    # Kurzanalyse-Box ----
    div(class = "content-box",
        div(class = "inner-box",
        p(br(),"KURZANALYSE", br()),
        p(style = "font-size = 24",
          strong("Nur 8 % der Frauen üben einen MINT-Beruf aus, bei Männern sind es über ein Drittel.
       Außerdem sinkt der Frauenanteil entlang der Bildungskette.
      Diese Zahlen werden in unserer Kurzanalyse \"Arbeitswelt: Frauen & MINT\" eingeordnet und mit Empfehlungen, was man
              für einen höheren Frauenanteil tun könnte, verknüpft."),
          br(), br(),
          tags$a(href = "https://www.mint-vernetzt.de/content/uploads/2024/02/MIN_Kurzanalyse_Frauen_Berufe_final.pdf",
                 target = "_blank", "Link zur Kurzanalyse"),
          br(), br(),
          p("Veröffentlichung: 16. Februar 2024",
            br(),
            "Zitiervorschlag: MINTvernetzt (2024). Kurzanalyse. Arbeitswelt: Frauen & MINT."))
        )
    ),

    # Box Regional ----

    fluidRow(id="beruf_regional",
      shinydashboard::box(
        h2("Regionale Daten: MINT-Daten aus Ihrem Landkreis."),
        width = 12,
        column(
          width = 8,
        p("Hier können Sie ausgewählte Statistiken dieser Seite auf Regionalebene betrachten und vergleichen, z. B.:
          Wie hoch ist der MINT-Anteil unter Beschäftigten in Ludwigshafen am Rhein?
          Welcher Landkreis aus Sachsen-Anhalt hat den höchsten Frauenanteil unter MINT-Auszubildenden?
          Welche Landkreise sind besonders international geprägt,
          mit höheren Anteilen an ausländischen Auszubildenden und Beschäftigten als im Bundeslanddurchschnitt?")
        ),
        column(
          width = 12,

        tabsetPanel(type = "tabs",
              # tab 1
                    tabPanel("Anteil MINT im Landkreisvergleich", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_landkreis_map_ui("mod_beruf_arbeitsmarkt_landkreis_map_ui_1"),
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_arbeitsmarkt_detail_map"),height = "1600px"),
                                                            color = "#154194"),
                               shinyBS::bsPopover(id = "h_beruf_regional_1", title = "",
                                                  content = paste0("Manche Landkreise sind grau dargestellt oder fehlen in der Darstellung. Das liegt daran, dass die zugrundeliegenden Karten vereinzelt alte oder falsche Landkreiszuordnungen (in Niedersachen, Sachsen-Anhalt) enthalten oder einzelne Regionen gar nicht enthalten (Bremen, in Sachsen). Daten zu den fehlenden Regionen sind in der Darstellung im nächstne Tab zu finden.", "<br> <br> Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot", "<br> <br> In den vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden.", "<br> <br>In die Kategorie &quotAuszubildende mit neuem Lehrvertrag&quot fallen sowohl neue Auszubilndende als auch Auszubildende nach Vertragswechsel."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_regional_1")
                             )
                    ),
              # tab 2
                    tabPanel("Ranking MINT in Landkreisen", br(),

                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_landkreis_vergleich_ui("mod_beruf_arbeitsmarkt_landkreis_vergleich_ui_1"),
                             ),
                             shiny::mainPanel(
                               width = 9,

                               # ),
                               shinycssloaders::withSpinner(
                                 htmlOutput(ns("plot_arbeitsmarkt_detail_vergleich"),height = "1600px"),
                                color = "#154194"),

                               shinyBS::bsPopover(id = "h_beruf_regional_23", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot", "<br> <br> In den vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden.", "<br> <br>In die Kategorie &quotAuszubildende mit neuem Lehrvertrag&quot fallen sowohl neue Auszubildende als auch Auszubildende nach Vertragswechsel."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_regional_23")
                               )
                             # ,
                             # p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt.")
                    ),
                    tabPanel("Zeitverlauf MINT in Landkreisen", br(),

                             tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #000000;}
                                                 .butt{border-color:#FFFFFF;} .butt{float: right;} .butt:hover{background-color: #FFFFFF; border-color:#FFFFFF}")),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_beruf_arbeitsmarkt_regional_verlauf_ui("mod_beruf_arbeitsmarkt_regional_verlauf_ui_1"),
                               #
                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("fachbereich_beruf_arbeitsmarkt_landkreis_verlauf")),
                                                            color = "#154194"),
                               shinyBS::bsPopover(id = "h_beruf_fach_mint_2", title = "",
                                                  content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot"),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_fach_mint_2")
                             )
                    )#,
                    # Tabelle noch nicht fertig gelayoutet
                    # tabPanel("Tabelle", br(),
                    #
                    #          fluidRow(
                    #            shiny::sidebarPanel(
                    #              width = 3
                    #          ),
                    #          shiny::sidebarPanel(
                    #            width = 3,
                    #            mod_beruf_arbeitsmarkt_landkreis_table_lk_ui("mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_1")
                    #          ),
                    #          shiny::sidebarPanel(
                    #            width = 3,
                    #            mod_beruf_arbeitsmarkt_landkreis_table_lk_ui("mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_2")
                    #          ),
                    #          shiny::sidebarPanel(
                    #            width = 3,
                    #            mod_beruf_arbeitsmarkt_landkreis_table_lk_ui("mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_3")
                    #          )),
                    #          fluidRow(
                    #            shiny::sidebarPanel(
                    #              width = 12,
                    #              p(),
                    #              mod_beruf_arbeitsmarkt_landkreis_table_lk_analysis_ui(ns("var1")),
                    #              h5(""),
                    #              actionButton(ns("insertBtn"), "Weitere Betrachtung hinzufügen"),
                    #              actionButton(ns("runBtn"), "Betrachtungen anzeigen")
                    #
                    #              ),
                    #          shiny::mainPanel(
                    #            width = 12,
                    #            DT::DTOutput(ns("table_lk_analysis"))
                    #          ),
                    #          p(style="font-size:12px;color:grey", "Quelle der Daten: Bundesagentur für Arbeit, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt.")
                    # ))
        )

            ))),




# Box 5 ----

fluidRow( id="beruf_entgelt",
          shinydashboard::box(
            title = "Entgelte: In MINT-Berufen verdient man überdurchschnittlich gut.",
            width = 12,
            column(
              width = 8,
              p("Das mittlere Brutto-Monatsgehalt (Median) in Deutschland 2024 liegt
                bei 4.013 Euro bei einer Vollzeitanstellung. Zum Vergleich sind es
                in MINT-Berufen 4.600 Euro. Betrachtet man die verschiedenen MINT-Berufsfelder, gibt es das höchste
                mittlere Entgelt im Bereich Informatik, mit 5.900 Euro.")
            ),
            column(
              width = 12,

              tabsetPanel(type = "tabs",
                          # Tab 1
                          tabPanel("Aktuelle Entgelt-Anteile", br(),

                                   shiny::sidebarPanel(
                                     width = 3,
                                     tags$style(".well {background-color:#FFFFFF;}"),
                                     tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                                     mod_beruf_arbeitsmarkt_entgelt_vergleich_ui("mod_beruf_arbeitsmarkt_entgelt_vergleich_ui_1"),
                                   ),
                                   shiny::mainPanel(
                                     width = 9,
                                     shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_entgelt_vergleich")),
                                                                  color = "#154194"),

                                     shinyBS::bsPopover(id = "h_beruf_mint_3_entgel", title = "",
                                                        content = paste0("Es wird der Median Bruttoentgelt bei sozialversicherungspflichtiger Vollzeitbeschäftigung dargestellt.", br(),br(), "Der Strichtag der Angabe ist der 31.12. des jeweils betrachteten Jahres."),
                                                        placement = "top",
                                                        trigger = "hover"),
                                     tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_mint_3_entgel")
                                   )
                          ),
                          # Tab 2
                          tabPanel("MINT-Anteil im Zeitverlauf", br(),

                                   shiny::sidebarPanel(
                                     width = 3,
                                     tags$style(".well {background-color:#FFFFFF;}"),
                                     tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                                     mod_beruf_arbeitsmarkt_entgelt_verlauf_ui("mod_beruf_arbeitsmarkt_entgelt_verlauf_ui_1"),
                                   ),
                                   shiny::mainPanel(
                                     width = 9,
                                     shinycssloaders::withSpinner(htmlOutput(ns("plot_entgelt_verlauf")),
                                                                  color = "#154194"),

                                     shinyBS::bsPopover(id = "h_beruf_mint_2__entgelt", title = "",
                                                        content = paste0(""),
                                                        placement = "top",
                                                        trigger = "hover"),
                                     tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_beruf_mint_2__entgelt"),

                                   )
                          ),

                          tabPanel("Balkendiagramm", br(),

                                   shiny::sidebarPanel(
                                     width = 3,
                                     tags$style(".well  {background-color:#FFFFFF;}"),
                                     tags$head(tags$style(HTML(".small-box {height: 140px}"))),
                                     mod_beruf_arbeitsmarkt_balken_entgelt_ui("mod_beruf_arbeitsmarkt_balken_entgelt_ui_1"),

                                   ),
                                   shiny::mainPanel(
                                     width = 9,
                                     shinycssloaders::withSpinner(htmlOutput(ns("plot_balken_entgelt")),
                                                                  color = "#154194"),

                                     shinyBS::bsPopover(id="h_studium_international_2_entgelt", title="",
                                                        content = paste0("  "),
                                                        placement = "top",
                                                        trigger = "hover"),
                                     tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_studium_international_2_entgelt"))

                          ),

                          tabPanel("top 10 entgel", br(),

                                   shiny::sidebarPanel(
                                     width = 3,
                                     mod_beruf_arbeitsmarkt_top_entgelt_ui("mod_beruf_arbeitsmarkt_top_entgelt_ui_1"),
                                   ),
                                   shiny::mainPanel(
                                     width = 9,
                                     shinycssloaders::withSpinner(htmlOutput(ns("plot_top_entgelte")),
                                                                  color = "#154194"),

                                     br(),
                                     shinyBS::bsPopover(id="h_studium_fach_entgelte1", title="",
                                                        content = paste0("I"),
                                                        placement = "top",
                                                        trigger = "hover"),
                                     tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id="h_studium_fach_entgelte1"))
                          ),


              )
            )))








    ,funct_footer()
    )
}


#' beruf_arbeitsmarkt Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Box 1 ----
    # Tab
    output$plot_arbeitsmarkt_faecher_anteil <- renderUI({
      arbeitsmarkt_faecher_anteil(r)
    })

    ##
    output$plot_arbeitsmarkt_faecher_anteil_frauen <- renderUI({
      arbeitsmarkt_faecher_anteil_frauen(r)
    })


    output$plot_einstieg_verlauf <- renderUI({
      plot_list <- beruf_verlauf_single(r)
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


    # Tab 3

    output$plot_einstieg_vergleich <- renderUI({
      plot_list <- beruf_einstieg_vergleich(r)
      plot_list
    })



    # Tab 4
    output$plot_arbeitsmarkt_waffle_gender <- renderPlot({
      arbeitsmarkt_anforderungen_gender(r)
    })

    # Tab 5

    output$plot_arbeitsmarkt_bl_gender <- renderUI({
      plot_list <- arbeitsmarkt_bl_gender(r)


      # return plots
      out <- highcharter::hw_grid(
        plot_list,
        ncol = 2)
      out

    })



    # Tab 6


      output$plot_beruf_arbeitsmarkt_bl_gender_verlauf <- renderUI({
        plot_list <- arbeitsmarkt_bl_gender_verlauf(r)
        r$plot_beruf_arbeitsmarkt_bl_gender_verlauf <- plot_list

        r$plot_beruf_arbeitsmarkt_bl_gender_verlauf_title <- get_plot_title(
          plot = r$plot_beruf_arbeitsmarkt_bl_gender_verlauf
        )

        plot_list
      })

      output$download_btn_plot_beruf_arbeitsmarkt_bl_gender_verlauf <- downloadHandler(
        contentType = "image/png",
        filename = function() {r$plot_beruf_arbeitsmarkt_bl_gender_verlauf_title},
        content = function(file) {
          # creating the file with the screenshot and prepare it to download

          add_caption_and_download(
            hc = r$plot_beruf_arbeitsmarkt_bl_gender_verlauf,
            filename =  r$plot_beruf_arbeitsmarkt_bl_gender_verlauf_title,
            width = 700,
            height = 400)

          file.copy(r$plot_beruf_arbeitsmarkt_bl_gender_verlauf_title, file)
          file.remove(r$plot_beruf_arbeitsmarkt_bl_gender_verlauf_title)
        }
      )




    # Tab 7

    output$plot_beruf_arbeitsmarkt_mint_bulas <- renderUI({
      arbeitsmarkt_mint_bulas(r)
    })

    output$plot_beruf_arbeitsmarkt_bl_verlauf  <- renderUI({
      plot_list <- arbeitsmarkt_bl_verlauf(r)
      r$plot_beruf_arbeitsmarkt_bl_verlauf <- plot_list

      r$plot_beruf_arbeitsmarkt_bl_verlauf_title <- get_plot_title(
        plot = r$plot_beruf_arbeitsmarkt_bl_verlauf
      )

      plot_list
    })

    output$download_btn_plot_beruf_arbeitsmarkt_bl_verlauf <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_beruf_arbeitsmarkt_bl_verlauf_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_beruf_arbeitsmarkt_bl_verlauf,
          filename =  r$plot_beruf_arbeitsmarkt_bl_verlauf_title,
          width = 700,
          height = 400)

        file.copy(r$plot_beruf_arbeitsmarkt_bl_verlauf_title, file)
        file.remove(r$plot_beruf_arbeitsmarkt_bl_verlauf_title)

})


    # Box 2 ----

    #Tab 1

    #Tab 2
    output$plot_arbeitsmarkt_verlauf_faecher <- renderUI({
      beruf_verlauf_faecher(r)
    })

    # ALT
    # Tab 1

    output$plot_arbeitsmarkt_faecher_bl <- renderUI({
      plot_list <- arbeitsmarkt_bula_faecher(r)


      # return plots
      plot_list

    })



    # tab 2


    output$plot_arbeitsmarkt_überblick_fächer  <- renderUI({
      plot_list <- arbeitsmarkt_überblick_fächer(r)
      r$plot_arbeitsmarkt_überblick_fächer <- plot_list

      r$plot_arbeitsmarkt_überblick_fächer_title <- get_plot_title(
        plot = r$plot_arbeitsmarkt_überblick_fächer
      )

      plot_list
    })

    output$download_btn_plot_arbeitsmarkt_überblick_fächer <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_arbeitsmarkt_überblick_fächer_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_arbeitsmarkt_überblick_fächer,
          filename =  r$plot_arbeitsmarkt_überblick_fächer_title,
          width = 700,
          height = 400)

        file.copy(r$plot_arbeitsmarkt_überblick_fächer_title, file)
        file.remove(r$plot_arbeitsmarkt_überblick_fächer_title)

      })

    # Tab 3


    output$download_btn_plot_arbeitsmarkt_bl_vergleich <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_arbeitsmarkt_bl_vergleich_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_arbeitsmarkt_bl_vergleich,
          filename =  r$plot_arbeitsmarkt_bl_vergleich_title,
          width = 700,
          height = 400)

        file.copy(r$plot_arbeitsmarkt_bl_vergleich_title, file)
        file.remove(r$plot_arbeitsmarkt_bl_vergleich_title)

      })

    # Tab 4


    output$plot_arbeitsmarkt_top10 <- renderUI({
      plot_list <- arbeitsmarkt_top10(r)
      r$plot_arbeitsmarkt_top10_left <- plot_list[[1]]
      r$plot_arbeitsmarkt_top10_right <- plot_list[[2]]

      r$plot_arbeitsmarkt_top10_left_title <- get_plot_title(
        plot = r$plot_arbeitsmarkt_top10_left
      )
      r$plot_arbeitsmarkt_top10_right_title <- get_plot_title(
        plot = r$plot_arbeitsmarkt_top10_right
      )

      # return plots
      out <- highcharter::hw_grid(
        plot_list,
        ncol = 2)
      out

    })

    output$download_btn_plot_arbeitsmarkt_top10_1 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_arbeitsmarkt_top10_left_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_arbeitsmarkt_top10_left,
          filename =  r$plot_arbeitsmarkt_top10_left_title,
          width = 700,
          height = 400,
          with_labels = FALSE)

        file.copy(r$plot_arbeitsmarkt_top10_left_title, file)
        file.remove(r$plot_arbeitsmarkt_top10_left_title)
      }
    )

    output$download_btn_plot_arbeitsmarkt_top10_2 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_arbeitsmarkt_top10_right_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download
        add_caption_and_download(
          hc = r$plot_arbeitsmarkt_top10_right,
          filename =  r$plot_arbeitsmarkt_top10_right_title,
          width = 700,
          height = 400,
          with_labels = FALSE)

        file.copy(r$plot_arbeitsmarkt_top10_right_title, file)
        file.remove(r$plot_arbeitsmarkt_top10_right_title)
      }
    )



    # Box3 ----

    # tab 1
    output$plot_einstieg_pie_gender <- renderUI({
      arbeitsmarkt_einstieg_pie_gender(r)
    })


    output$plot_einstieg_verlauf_gender  <- renderUI({
      plot_list <- arbeitsmarkt_einstieg_verlauf_gender(r)
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

      })


    # tab 3

    output$plot_arbeitsmarkt_wahl_gender <- renderUI({
      arbeitsmarkt_wahl_gender(r)
    })


    output$plot_einstieg_vergleich_gender  <- renderUI({
      plot_list <- arbeitsmarkt_einstieg_vergleich_gender(r)
      r$plot_einstieg_vergleich_gender <- plot_list

      r$plot_einstieg_vergleich_gender_title <- get_plot_title(
        plot = r$plot_einstieg_vergleich_gender
      )

      plot_list
    })

    output$download_btn_plot_einstieg_vergleich_gender <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_einstieg_vergleich_gender_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_einstieg_vergleich_gender,
          filename =  r$plot_einstieg_vergleich_gender_title,
          width = 700,
          height = 400)

        file.copy(r$plot_einstieg_vergleich_gender_title, file)
        file.remove(r$plot_einstieg_vergleich_gender_title)

      })


    # Box Regional ----

    # tab 1

    output$plot_arbeitsmarkt_detail_map <- renderUI({
     arbeitsmarkt_lk_detail_map(r)


    })




    # tab 2



    observeEvent(input$search_in_bar_chart, {
      r$search_in_bar_chart <- input$search_in_bar_chart
    })

    output$plot_arbeitsmarkt_detail_vergleich <- renderUI({
      arbeitsmarkt_lk_detail_vergleich(r)

    })



  # tab 3

   output$fachbereich_beruf_arbeitsmarkt_landkreis_verlauf <- renderUI({
     arbeitsmarkt_lk_verlauf(r)
   })






   # Box 5 ----
   # Tab

   output$plot_entgelt_vergleich <- highcharter::renderHighchart({
     out <- entgelte_vergleich_1(r)
   })

   # output$plot_entgelt_vergleich <- renderUI({
   #   plot_list <- entgelte_vergleich_1(r)
   #   highcharter::highchartOutput("hc_tmp")
   # })
   #
   # output$hc_tmp <- highcharter::renderHighchart({
   #   entgelte_vergleich_1(r)
   # })
   #





   output$plot_entgelt_verlauf <- renderUI({
     entgelte_verlauf_1(r)
   })

   output$plot_balken_entgelt <- renderUI({
     entgelte_balken_1(r)
   })



   output$plot_top_entgelte <- renderUI({

     plot_list <- plot_ranking_top_entgeltee(r)
     # r$plot_top_faecher_left <- plot_list[[1]]
     # r$plot_top_faecher_right <- plot_list[[2]]
     #
     # r$plot_top_faecher_left_title <- get_plot_title(
     #   plot = r$plot_top_faecher_left
     # )
     # r$plot_top_faecher_right_title <- get_plot_title(
     #   plot = r$plot_top_faecher_right
     # )

     # return plots
     out <- highcharter::hw_grid(
       plot_list,
       ncol = 2)

   })





   # ##
   # output$plot_arbeitsmarkt_faecher_anteil_frauen <- renderUI({
   #   arbeitsmarkt_faecher_anteil_frauen(r)
   # })
   #
   #
   # output$plot_einstieg_verlauf <- renderUI({
   #   plot_list <- beruf_verlauf_single(r)
   #   r$plot_einstieg_verlauf <- plot_list
   #
   #   r$plot_einstieg_verlauf_title <- get_plot_title(
   #     plot = r$plot_einstieg_verlauf
   #   )
   #
   #   plot_list
   # })
   #
   # output$download_btn_plot_einstieg_verlauf <- downloadHandler(
   #   contentType = "image/png",
   #   filename = function() {r$plot_einstieg_verlauf_title},
   #   content = function(file) {
   #     # creating the file with the screenshot and prepare it to download
   #
   #     add_caption_and_download(
   #       hc = r$plot_einstieg_verlauf,
   #       filename =  r$plot_einstieg_verlauf_title,
   #       width = 700,
   #       height = 400)
   #
   #     file.copy(r$plot_einstieg_verlauf_title, file)
   #     file.remove(r$plot_einstieg_verlauf_title)
   #   }
   # )
   #
   #
   # # Tab 3
   #
   # output$plot_einstieg_vergleich <- renderUI({
   #   plot_list <- beruf_einstieg_vergleich(r)
   #   plot_list
   # })
   #
   #
   #
   # # Tab 4
   # output$plot_arbeitsmarkt_waffle_gender <- renderPlot({
   #   arbeitsmarkt_anforderungen_gender(r)
   # })
   #
   # # Tab 5
   #
   # output$plot_arbeitsmarkt_bl_gender <- renderUI({
   #   plot_list <- arbeitsmarkt_bl_gender(r)
   #
   #
   #   # return plots
   #   out <- highcharter::hw_grid(
   #     plot_list,
   #     ncol = 2)
   #   out
   #
   # })
   #
   #
   #
   # # Tab 6
   #
   #
   # output$plot_beruf_arbeitsmarkt_bl_gender_verlauf <- renderUI({
   #   plot_list <- arbeitsmarkt_bl_gender_verlauf(r)
   #   r$plot_beruf_arbeitsmarkt_bl_gender_verlauf <- plot_list
   #
   #   r$plot_beruf_arbeitsmarkt_bl_gender_verlauf_title <- get_plot_title(
   #     plot = r$plot_beruf_arbeitsmarkt_bl_gender_verlauf
   #   )
   #
   #   plot_list
   # })
   #
   # output$download_btn_plot_beruf_arbeitsmarkt_bl_gender_verlauf <- downloadHandler(
   #   contentType = "image/png",
   #   filename = function() {r$plot_beruf_arbeitsmarkt_bl_gender_verlauf_title},
   #   content = function(file) {
   #     # creating the file with the screenshot and prepare it to download
   #
   #     add_caption_and_download(
   #       hc = r$plot_beruf_arbeitsmarkt_bl_gender_verlauf,
   #       filename =  r$plot_beruf_arbeitsmarkt_bl_gender_verlauf_title,
   #       width = 700,
   #       height = 400)
   #
   #     file.copy(r$plot_beruf_arbeitsmarkt_bl_gender_verlauf_title, file)
   #     file.remove(r$plot_beruf_arbeitsmarkt_bl_gender_verlauf_title)
   #   }
   # )
   #
   #
   #
   #
   # # Tab 7
   #
   # output$plot_beruf_arbeitsmarkt_mint_bulas <- renderUI({
   #   arbeitsmarkt_mint_bulas(r)
   # })
   #
   # output$plot_beruf_arbeitsmarkt_bl_verlauf  <- renderUI({
   #   plot_list <- arbeitsmarkt_bl_verlauf(r)
   #   r$plot_beruf_arbeitsmarkt_bl_verlauf <- plot_list
   #
   #   r$plot_beruf_arbeitsmarkt_bl_verlauf_title <- get_plot_title(
   #     plot = r$plot_beruf_arbeitsmarkt_bl_verlauf
   #   )
   #
   #   plot_list
   # })
   #
   # output$download_btn_plot_beruf_arbeitsmarkt_bl_verlauf <- downloadHandler(
   #   contentType = "image/png",
   #   filename = function() {r$plot_beruf_arbeitsmarkt_bl_verlauf_title},
   #   content = function(file) {
   #     # creating the file with the screenshot and prepare it to download
   #
   #     add_caption_and_download(
   #       hc = r$plot_beruf_arbeitsmarkt_bl_verlauf,
   #       filename =  r$plot_beruf_arbeitsmarkt_bl_verlauf_title,
   #       width = 700,
   #       height = 400)
   #
   #     file.copy(r$plot_beruf_arbeitsmarkt_bl_verlauf_title, file)
   #     file.remove(r$plot_beruf_arbeitsmarkt_bl_verlauf_title)
   #
   #   })













  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_ui("beruf_arbeitsmarkt_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_server("beruf_arbeitsmarkt_1")
