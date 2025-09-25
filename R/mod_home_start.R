#' home_start UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_start_ui <- function(id){
  ns <- NS(id)
  tagList(


    # Banner
       fluidRow(
         div(class = "clean-box",
             column(
               width = 12,
               img(src='www/Banner_Alle_Bereiche.avif',
                   class = "img-responsive",
                   alt = "Banner MINT entlang der Bildungskette",
                   style="display: block; margin-left: auto; margin-right: auto; margin-bottom: 20px;"
               )))),

 # Info-Texte

    fluidRow(
      shinydashboard::box(
        h2("Auf dieser Seite"),
        width = 7,
        p(
           "Auf dieser Überblickseite geben wir einen ersten Einblick in die vorhandenen Daten und vergleichen die
             Bildungsbereiche miteinander. Auf den folgenden bereichsspezifischen Unterseiten gehen wir je Bildungsbereich
             mehr ins Detail und bieten zusätzlich Vergleiche auf Fächer- und Bundeslandebene.")),
      shinydashboard::box(
        title = "Fragen oder Feedback?",
        width = 5,
        p(
          "Sind alle Zahlen und Grafiken verständlich? Gibt es Darstellungsschwierigkeiten? Wünschen Sie sich weitere Daten?", br(),
          "Wir freuen uns über Rückfragen und Feedback ", tags$a(href = "mailto:katharina.brunner@mint-vernetzt.de?subject= Feedback MINT-Datalab", "per E-Mail"),"oder über eine Teilnahme an unserer kurzen",
          tags$a(href="https://survey.lamapoll.de/MINT-DataLab_Feedback/", "Umfrage", target="_blank"), "!"
        ))
    ),

    fluidRow(
      shinydashboard::box(
        h2("Themenübersicht"),
        width = 7,
        p(style = "text-align: left; font-size = 22px",tags$a(href="#alle_mint",
                                                              span(tags$b(span("→ MINT-Anteil:")))),"ein Drittel MINT in Schule, Studium und Ausbildung, ein knappes Viertel im Beruf."
        ),

        p(style = "text-align: left; font-size = 22px",tags$a(href="#alle_frauen",
                                                                    span(tags$b(span("→ Frauen in MINT:")))),"Der Frauenanteil nimmt entlang der Bildungskette ab."))
      ,

      shinydashboard::box(
        title = "Datenquellen",
        width = 5,
        p("Die amtlichen Statistiken zeigen die aktuellsten verfügbaren Berichtsjahre 2024 (für Studierenden- und Berufsdaten) und 2023 (für Schuldaten)."),
        p(tags$b(span("Studierendenzahlen: Destatis 2025, auf Anfrage. ")),"Daten des Berichtsjahres 2025 ca. ab September 2026 verfügbar."),
        p(tags$b(span("Schülerzahlen: KMK 2024, auf Anfrage. ")), "Daten des Berichtjahres 2024 ca. ab Dezember 2025 verfügbar."),
        p(tags$b(span("Auszubildenden- und Beschäftigtenzahlen: Bundesagentur für Arbeit 2025, auf Anfrage. ")), "Daten des Berichtsjahres
            2025 ca. ab Juli 2026 verfügbar.")
        )
      ),

   # Box 1 ----

    fluidRow(id="alle_mint",
      shinydashboard::box(
        h2("MINT-Anteil: ein Drittel MINT in Schule, Studium und Ausbildung, ein knappes Viertel im Beruf."),
        width = 12,
        column(
          width = 8,
          p("Wie groß ist der MINT-Anteil entlang der Bildungskette von Schule bis Beruf?
          Gut ein Drittel der Leistungskursbelegungen ist 2023 in einem MINT-Fach. 2024 lernen
          37 % der Studierenden und rund ein Drittel der Auszubildenden in MINT.
          Unter den Beschäftigten ist der MINT-Anteil geringer. Hier üben Stand 2024 rund 23 %
          der sozialversicherungspflichtigen Beschäftigten eine MINT-Tätigkeit aus.
          In den letzten zehn Jahren sind die prozentualen Anteile relativ konstant geblieben.
          Leichte Veränderungen sieht man in den absoluten Zahlen.")
        ),
        column(
          width = 12,
        tabsetPanel(type = "tabs",
                    tabPanel("aktueller MINT-Anteil", br(),
                             shiny::sidebarPanel(
                               width = 3,
                               mod_home_start_einstieg_ui("mod_home_start_einstieg_ui_1"),

                             ),
                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_mint_rest_einstieg_1")),
                                                            color = "#154194"),
                        shinyBS::bsPopover(id="h_alle_mint_1", title = "",
                                           content = paste0("Anders als z. B. bei Studierenden wählen Schüler:innen mehrere Grund- und Leistungskurse. Um dennoch einen Anteil von &quotMINT&quot vs. &quotNicht-MINT&quot angeben zu können, nutzen wir die Kursbelegungszahlen der Schüler:innen."),
                                           placement = "top",
                                           trigger = "hover"),
                         tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_alle_mint_1")
                        )
                            ),
                    tabPanel("MINT-Anteil im Zeitverlauf", br(),
                        shiny::sidebarPanel(
                          width = 3,
                          mod_home_start_multiple_ui("mod_home_start_multiple_ui_1"),
                          ),
                        shiny::mainPanel(
                          width = 9,
                          shinycssloaders::withSpinner(htmlOutput(ns("plot_mint_1")),
                                                       color = "#154194"),
                          shinyBS::bsPopover(id="h_alle_mint_2", title = "",
                                             content = paste0("Anders als z. B. bei Studierenden wählen Schüler:innen mehrere Grund- und Leistungskurse. Um dennoch einen Anteil von &quotMINT&quot vs. &quotNicht-MINT&quot angeben zu können, nutzen wir die Kursbelegungszahlen der Schüler:innen."),
                                             placement = "top",
                                             trigger = "hover"),
                          tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_alle_mint_2")
                          )

                        )

         ))
         )),
 # Box 2 ----
    fluidRow(id="alle_frauen",
      shinydashboard::box(
        h2("Frauen in MINT: Der Frauenanteil nimmt entlang der Bildungskette ab."),
        width = 12,
        column(
          width = 8,
          p("In den MINT-Leistungskursen sind 2023 46 % der Schüler:innen weiblich.
          In der weiterführenden Bildung, in Ausbildung und Studium, liegt der Frauenanteil 2024 bei 13 % beziehungsweise 33 %.
          In den MINT-Berufen sind nur 17 % Frauen vertreten. Im Vergleich dazu liegt der Frauenanteil in \"Nicht-MINT\"-Berufen bei 55 %,
          in \"Nicht-MINT\"-Studiengängen sogar bei über 60 %.", br(),
            "In den letzten zehn Jahren gab es diesbezüglich nur geringe Veränderungen.
        Es kam zu leichten Zunahmen des Frauenanteils im Studium und im Beruf.")
        ),
        column(
          width = 12,
        tabsetPanel(type = "tabs",
                    tabPanel("aktueller Anteil Frauen in MINT", br(),  # Verlgeich
                             shiny::sidebarPanel(
                               width = 3,
                               mod_home_start_einstieg_gender_ui("mod_home_start_einstieg_gender_ui_1")
                               ),

                             shiny::mainPanel(
                               width = 9,
                               shinycssloaders::withSpinner(htmlOutput(ns("plot_pie_mint_gender")),
                                                            color = "#154194"),
                               shinyBS::bsPopover(id="h_alle_frauen_1", title = "",
                                                  content = paste0("Anders als z. B. bei Studierenden wählen Schüler:innen mehrere Grund- und Leistungskurse. Um dennoch einen Anteil von &quotMINT&quot vs. &quotNicht-MINT&quot angeben zu können, nutzen wir die Kursbelegungszahlen der Schüler:innen.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden. <br><br>Baden-Württemberg erfasst keine geschelchterspezifischen Kursbelegungszahlen von Schüler:innen."),
                                                  placement = "top",
                                                  trigger = "hover"),
                               tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_alle_frauen_1")
                               )
                            )
                    ,
                    tabPanel("Zeitverlauf", br(),
                        shiny::sidebarPanel(
                          width = 3,
                          mod_home_start_comparison_ui("mod_home_start_comparison_ui_1"),
                          ),
                        shiny::mainPanel(
                          width = 9,
                          shinycssloaders::withSpinner(htmlOutput(ns("plot_verlauf_mint")),
                                                       color = "#154194"),
                          shinyBS::bsPopover(id="h_alle_frauen_2", title = "",
                                             content = paste0("Anders als z. B. bei Studierenden wählen Schüler:innen mehrere Grund- und Leistungskurse. Um dennoch einen Anteil von &quotMINT&quot vs. &quotNicht-MINT&quot angeben zu können, nutzen wir die Kursbelegungszahlen der Schüler:innen.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden.<br><br>Baden-Württemberg erfasst keine geschelchterspezifischen Kursbelegungszahlen von Schüler:innen."),
                                             placement = "top",
                                             trigger = "hover"),
                          tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_alle_frauen_2")
                                        )
                            ),
                    )
          )
        )
    ) ,

 # Kurzanalyse-Box ----
 div(class = "content-box",
     div(class = "inner-box",
     p(br(),"KURZANALYSE", br()),
     p(style = "font-size = 24",
       strong("Der Frauenanteil sinkt entlang der Bildungskette.
       Nur 8 % der Frauen üben einen MINT-Beruf aus, bei Männern sind es über ein Drittel.
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


# Footer
funct_footer())
}

#' home_start Server Functions
#'
#' @noRd
mod_home_start_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # Box 1, Tab1 ----


    output$plot_mint_rest_einstieg_1 <- renderUI({
      home_einstieg( r)
    })


    output$plot_pie_mint_gender <- renderUI({
      home_einstieg_gender( r)
    })

    ### Downloads ----

    #### Box 1 ----

    #tab 2
    output$plot_mint_1 <- renderUI({
      plot_list <- home_rest_mint_verlauf(r)
      r$plot_mint_1 <- plot_list

      r$plot_mint_1_title <- get_plot_title(
        plot = r$plot_mint_1
      )

      plot_list
    })

    output$download_btn_plot_mint_1 <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_mint_1_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_mint_1,
          filename =  r$plot_mint_1_title,
          width = 700,
          height = 400)

        file.copy(r$plot_mint_1_title, file)
        file.remove(r$plot_mint_1_title)
      }
    )


    #### Box 2 ----



    # tab 2

    output$plot_verlauf_mint <- renderUI({
      plot_list <- home_comparison_line(r)
      r$plot_verlauf_mint <- plot_list

      r$plot_verlauf_mint_title <- get_plot_title(
        plot = r$plot_verlauf_mint
      )

      plot_list
    })

    output$download_btn_plot_verlauf_mint <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_verlauf_mint_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_verlauf_mint,
          filename =  r$plot_verlauf_mint_title,
          width = 700,
          height = 400)

        file.copy(r$plot_verlauf_mint_title, file)
        file.remove(r$plot_verlauf_mint_title)
      }
    )

    # tab 3
    output$plot_comparison_gender <- renderUI({
      plot_list <-home_stacked_comparison_gender(r)
      r$plot_comparison_gender <- plot_list

      r$plot_comparison_gender_title <- get_plot_title(
        plot = r$plot_comparison_gender
      )

      plot_list
    })

    output$download_btn_plot_comparison_gender <- downloadHandler(
      contentType = "image/png",
      filename = function() {r$plot_comparison_gender_title},
      content = function(file) {
        # creating the file with the screenshot and prepare it to download

        add_caption_and_download(
          hc = r$plot_comparison_gender,
          filename =  r$plot_comparison_gender_title,
          width = 700,
          height = 400)

        file.copy(r$plot_comparison_gender_title, file)
        file.remove(r$plot_comparison_gender_title)
      }
    )




  })
}

## To be copied in the UI
# mod_home_start_ui("home_start_1")

## To be copied in the server
# mod_home_start_server("home_start_1")







# Alte Start-Box












