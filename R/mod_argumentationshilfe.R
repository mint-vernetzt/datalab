
#' Argumentationshilfe UI Function + Server
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

# UI Funktion der Seite
mod_argumentation_ui <- function(id){
  ns <- NS(id)
  tagList(

    # für Bilder-Download
    tags$head(
      tags$script(src = "https://cdn.jsdelivr.net/npm/jszip@3.10.1/dist/jszip.min.js"),
      tags$script(src = "https://cdn.jsdelivr.net/npm/file-saver@2.0.5/dist/FileSaver.min.js"),
      tags$script(src = "https://cdn.jsdelivr.net/npm/canvg@3.0.10/lib/umd.min.js"),
      tags$style(HTML("
    /* optional: sorge für weißen Hintergrund in Charts */
    .dl-chart { background:#fff; padding:8px; }
    .dl-chart .highcharts-container { background:#fff; }
  "))
    ),

    useShinyjs(),

    # Seiteninhalt
    fluidRow(
      # noch austauschen
      div(class = "clean-box",
          column(
            width = 12,
            img(src='www/Banner_KI-Analysehilfe.png',
                class = "img-responsive",
                height = "300px",
                alt = "Banner KI-Analysehilfe",
                style="display: block; margin-left: auto; margin-right: auto;"
            )))),


    ## Einleitungstext ----
    fluidRow(
      div(class = "clean-box",
          column(
            width = 8,
            h1("Datenbasierte Berichte und Argumentationen mit KI erstellen"),
            # p(
            #   "Ob für Förderanträge, Kommunikation oder strategische Entscheidungen -
            #   Daten helfen dabei, die MINT-Bildungswelt zu verstehen und überzeugend
            #   zu argumentieren. Sie wollen Ihre Arbeit mit Zahlen und Fakten unterstreichen,
            #   zum Beispiel, um in Finanzierungsanträgen zu überzeugen, im Diskurs mit
            #   Politiker:innen die Relevanz von MINT-Förderung zu bestärken oder
            #   das eigene Projekt wirksam auszurichten?"
            # p(
            # "Hier finden Sie die Werkzeuge, um einen aussagestarken MINT-Bericht erstellen
            # zu lassen und eine starke Argumentationskette für die MINT-Bildungsförderung
            # in Ihrer Region aufzubauen."),
            p("Daten helfen dabei, die MINT-Bildungswelt zu verstehen. Gleichzeitig sind
              Daten das Fundament, um wirkungsvolle Entscheidungen zu treffen und
              erfolgreich für MINT-Förderung zu argumentieren.
              Doch wie lassen sich die Daten aus dem MINT-DataLab korrekt interepretieren?"),
            p("Bei dieser Frage können Sie sich von KI unterstützen lassen: dem MINT-DataLab-GPT"),

            strong("Der Chatbot unterstützt konkret in drei Situationen:"),
            p(style = "margin-left: 20px; margin-top: 10px;", "1. Er erstellt einen MINT-Bericht für ein ausgewähltes Bundesland"),
            p(style = "margin-left: 20px;", "2. Er hilft, für MINT-Förderung zu argumentieren"), #Er erstellt eine Argumentation für die Förderung von MINT-Bildung
            p(style = "margin-left: 20px;", "3. Er hilft, Daten grundlegend zu interpretieren"),
            br(),
            p("Der MINT-DataLab-GPT kann bei der Interpretation aller Daten im MINT-DataLab assistieren."),

            p("Auf dieser Seite finden Sie eine Anleitung, wie Sie mit dem MINT-DataLab-GPT einen MINT-Bericht erstellen können.", br(),
            "Häufig gestellte Fragen beantworten wir weiter unten auf der Seite. ", tags$a(href = "#faq",
                                                                       style = "color: #000000; text-decoration: underline;",
                                                                       "→ zu den FAQs")),

            p("Beachten Sie bitte auch die Nutzungshinweise am Ende der Seite."), #Für die Nutzung des GPT benötigen Sie ein (kostenfreies) OpenAI-Konto.

            ),
          column(
            width = 4,
            tags$div(
              style = "display: flex; flex-direction: column; align-items: flex-start; justify-content: flex-start;",
              tags$strong(
                "Beispielbericht für Hamburg:",
                style = "margin: 20px 0px 0px 60px"
              ),
              tags$a(
                href = "www/MINTvernetzt_Argumentationskette_Hamburg.pdf",
                target = "_blank",
                tags$img(
                  src = "www/Bild_Beispielbericht.png",
                  alt = "Cover Beispielbericht Hamburg",
                  style = "max-width: 20%; height: auto; cursor: pointer; margin: 10px 0px 0px 70px;
                  border: 1px solid #EFE8E6;"
                )
              )
            ),
            tags$div(
              style = "display: flex; flex-direction: column; align-items: flex-start; justify-content: flex-start;",
              tags$strong(
                "Dierkt eine Datenanalyse mit KI starten:",
                style = "margin: 40px 0px 0px 60px"
              ),
                      class = "linked-image",
                      style = "flex: 0 0 20%;",
                      tags$a(
                        href = "https://chatgpt.com/g/g-67e4f41fd91881919a753f4309194bf7-test-mint-datalab-assistent-test",
                        target = "_blank",
                        tags$img(
                          src = "www/Bild_MINT-DataLab-GPT.png",
                          alt = "MINT-DataLab-GPT Symbolbild",
                          style = "max-width: 30%; height: auto; cursor: pointer;
                          margin: 10px 0px 0px 70px; border-radius: 10px;"
                        )
                      )
                    )
          ),

        ## Infos zu GPT ----

        # column(
        #   width = 8,
        #   # h2("MINT-DataLab-GPT - die KI-Assistenz für Datenanalysen:"),
        #   # p("Der MINT-DataLab-GPT ist von MINTvernetzt für Datenanalysen erstellt
        #   #   worden und ist eine spezialisierte Form des Chat-GPT von OpenAI.
        #   #   Der MINT-DataLab-GPT nutzt das KI-Modell von OpenAI, für die Nutzung muss
        #   #   dort ein (kostenfreier) Account angelegt werden."),
        #   # br(),
        #   tags$strong("Das sind die Vorteile des MINT-DataLab-GPT:", style = "font-size: 16px;"),
        #
        #   div(class = "content-box",
        #       style = "background-color: #ee777530;
        #        color: #000;
        #        border: 2px solid #ee7775;
        #        margin-left: 20px;
        #        width: 90%;
        #        border-radius: 10px;
        #        display: flex;
        #        align-items: center;
        #        padding: 10px;",
        #       div(
        #         class = "linked-image",
        #         style = "flex: 0 0 20%;",
        #         tags$a(
        #           href = "https://chatgpt.com/g/g-67e4f41fd91881919a753f4309194bf7-test-mint-datalab-assistent-test",
        #           target = "_blank",
        #           tags$img(
        #             src = "www/Bild_MINT-DataLab-GPT.png",
        #             alt = "MINT-DataLab-GPT Symbolbild",
        #             style = "max-width: 100%; height: auto; cursor: pointer; margin: 0px; border-radius: 10px;"
        #           )
        #         )
        #       ),
        #       div(
        #         style = "flex: 1; padding-left: 20px; display: flex;
        #         flex-direction: column; justify-content: center;
        #         text-align: left;",
        #         tags$strong("Maßgeschneidert für MINT-Daten und Analysen"),
        #         p("Unser GPT ist speziell konfiguriert darauf, Datenberichte zu erstellen.
        #  Es ist ausgerichtet auf unsere Datensätze, und verknüpft diese mit den Fakten
        #  aus den Kurzanalysen des MINT-DataLabs."),
        #         tags$strong("Persönliche Assistenz für überzeugende Berichte und Argumente"),
        #         p("Unser GPT begleitet durch den Prozess und kann Daten nicht nur
        #  interpretieren, sondern Analysen an Ihrem individuellen Bedarf ausrichten und um Informationen
        #  anreichern, für starke Aussagen und Argumente."),
        #         tags$strong("Erhöhte Quellensicherheit"),
        #         p("Unser GPT nutzt als Daten-Quelle die aus dem MINT-DataLab bereitgestellten
        #  Statistiken und erstellt automatisch Quellenverzeichnisse. So können
        #  die KI-Vorteile genutzt werden, bei erhöhter Quellensicherheit.")
        #       )
        #   ),
        #
        #   # p("Auf dieser Seite finden Sie einen Einstieg in die Nutzung des MINT-DataLab-GPT. Neben
        #   # einer FAQ-Sektion am Ende der Seite finden Sie hier einer Datenvorauswahl für
        #   # Ihren Schnellstart in die Erstellung datenbasierter Berichte und Argumentationen.
        #   #   Folgen Sie dafür den Schritten im nächsten Abschnitt."),
        #   # p("Darüber hinaus können alle Daten des MINT-DataLab für eine Analyse mit dem MINT-DataLab-GPT
        #   #   heruntergeladen werden. Die Download-Funktion finden Sie oben rechts an den interkativen Grafiken
        #   #   auf den jeweiligen Unterseiten.")
        # ),



        ## 4 Schritte ----
        column(
          width = 12,
          h2("In vier Schritten zu Ihrem MINT-Bericht oder Ihrer Argumentationskette", #Schnellstart: So analyserien Sie Daten mit dem MINT-DataLab-GPT
            style = "margin-top: 30px;"),
         # hr(style = "border-top: 2px solid #ee7775; margin-top: 15px; margin-bottom: 15px;")
        ),

        column(
          width = 2,
          tags$span(#icon("1", style = "margin: 10px; font-size: 17px;"),
                    style = "font-weight: 600; font-size: 16px;
                    display: block; height: 70px; margin-bottom: 10px;",
                    "1. Wählen Sie eine Region für die Analyse aus."),
          tags$a(href="#region", img(src='www/gpt_schritt_1.png',
              class = "img-responsive",
              height = "150px",
              alt = "Symbol Schritt 1 Region wählen",
              style="display: block;
                margin-top: 20px; height: 200px; border: 2px solid #B16FAB;
                border-radius: 15px; text-align: left;"))
        ),
        column(
          width = 2,
          tags$span(#icon("2", style = "margin: 10px; font-size: 17px;"),
                    style = "font-weight: 600; font-size: 16px;
                    display: block; height: 70px; margin-bottom: 10px;",
                    "2. Laden Sie die Datengrundlage herunter."),
          tags$a(href="#download_section", img(src='www/gpt_schritt_2.png',
              class = "img-responsive",
              height = "150px",
              alt = "Symbol Schritt 2 Datendownload",
              style="display: block;
                margin-top: 20px; height: 200px; border: 2px solid #B16FAB;
                border-radius: 15px;"))
        ),
        column(
          width = 2,
          tags$span(#icon("3", style = "margin: 10px; font-size: 17px;"),
                    style = "font-weight: 600; font-size: 16px;
                    display: block; height: 70px; margin-bottom: 10px;",
                    "3. Wechseln Sie zum MINT-DataLab-GPT und folgen den Anweisungen."),
          tags$a(href="#MINT-DataLab-GPT", img(src='www/gpt_schritt_3.png',
              class = "img-responsive",
              height = "150px",
              alt = "Symbol Schritt 3 GPT-Chat",
              style="display: block;
                margin-top: 20px; height: 200px; border: 2px solid #B16FAB;
                border-radius: 15px;"))
        ),
        column(
          width = 2,
          tags$span(#icon("4", style = "margin: 10px; font-size: 17px;"),
                    style = "font-weight: 600; font-size: 16px;
                    display: block; height: 70px; margin-bottom: 10px;",
                    "4. Ergänzen Sie den KI-Bericht zur Veranschaulichung mit Grafiken."),
          tags$a(href="#grafiken", img(src='www/gpt_schritt_4.png',
              class = "img-responsive",
              height = "150px",
              alt = "Symbol Schritt 4 Grafiken ergänzen",
              style="display: block;
                margin-top: 20px; height: 200px; border: 2px solid #B16FAB;
                border-radius: 15px;"))
          ),
        column(
          width = 12,
         # hr(style = "border-top: 2px solid #ee7775; margin-top: 30px; margin-bottom: 15px;")
        ),

      )
    ),

   div(
      style = "margin-top: 40px;",

       ## Region-Filter ----

    column(
      width = 8,
      style = "display: flex; align-items: center; margin-bottom: 30px;",
      div(
        style = "margin: 0px 25px 20px 0px;",
        img(src='www/gpt_schritt_1.png',
            class = "img-responsive",
            alt = "Bild Schritt 1 klein",
            style="display: block;
                margin-top: 10px; border: 2px solid #B16FAB;
                border-radius: 15px; max-width: 50px;")
      ),
      div(id = "region",

        p(strong(style = "text-align: justify; font-size: 18px;",
                 "1. Wählen Sie eine Region für die Analyse aus.")),

        shinyWidgets::pickerInput(
          inputId = ns("region_argumentationshilfe"),
          choices = c("Deutschland",
                      "Baden-Württemberg",
                      "Bayern",
                      "Berlin",
                      "Brandenburg",
                      "Bremen",
                      "Hamburg",
                      "Hessen",
                      "Mecklenburg-Vorpommern",
                      "Niedersachsen",
                      "Nordrhein-Westfalen",
                      "Rheinland-Pfalz",
                      "Saarland",
                      "Sachsen",
                      "Sachsen-Anhalt",
                      "Schleswig-Holstein",
                      "Thüringen"
          ),
          multiple = FALSE,
          selected = c("Deutschland")
        )
      )
    ),



      ## Daten-Download ----

    column(
      width = 8,
      style = "display: flex; align-items: center; margin-bottom: 15px;",
       div(
        style = "margin: 0px 25px 100px 0px;",
        img(src='www/gpt_schritt_2.png',
            class = "img-responsive",
            alt = "Bild Schritt 2 klein",
            style="display: block;
                  margin-top: 10px; border: 2px solid #B16FAB;
                  border-radius: 15px; max-width: 50px;")
      ),
      div(id = "download_section",
        style = "flex: 1; margin-bottom: 15px;",

        # p(strong(style = "text-align: justify; font-size: 18px;",
        #          "2. Laden Sie die gewünschten Daten herunter")),
        # p(style = "font-size : 15px;", "Als Basis für den Datenbericht sowie
        # die datenbasierte Argumentation haben wir fünf Statistiken aus dem MINT-DataLab ausgewählt.
        # Um welche Statistiken es sich handelt, sehen Sie weiter unten auf dieser Seite,
        #   wo sie grafisch eingebunden sind."),
        #
        # downloadButton(style = "marign-bottom: 5px;",
        #                ns("download_txt"), "   Vorausgewählte Daten herunterladen"),
        # p( "Hinweis: Die Daten öffnen sich in einem Text-Dokument und können auf den
        #   ersten Blick verwirrend aussehen. Kopieren Sie den Inhalt der Datei und fügen
        #   Sie diesen direkt in das Chat-Fenster des GPT ein.")


        fluidRow(
          p(strong(style = "text-align: justify; font-size: 18px; margin-left: 15px;",
                   "2. Laden Sie die Datengrundlage herunter.")),
          column(
            width = 6,  # Text in der linken Spalte

            p(style = "font-size: 15px;",
              "Als Basis für den Datenbericht sowie die datenbasierte Argumentation
              haben wir fünf Statistiken aus dem MINT-DataLab ausgewählt.")
          ),
          column(
            width = 5,
            div(style = "margin-left: 30px;",
            p(style = "font-size: 15px;",
              tags$a(href = "#daten_grafiken",
                     style = "color: #000000; text-decoration: underline;",
                     "→ Betrachten Sie die Daten in den interaktiven Grafiken weiter unten auf dieser Seite."))
          ))
        ),

        fluidRow(
          column(
            width = 6,  # Text in der linken Spalte
            p("Laden sie hier die Daten als txt.-Dokument herunter. Kopieren
              Sie den Inhalt des Dokuments in den Chat des MINT-DataLab-GPT oder
              hängen Sie die Datei an.")
          ),
          column(
            width = 5,
            div(style = "margin-left: 30px;",
            downloadButton(style = "margin-bottom: 5px;",
                           ns("download_txt"),
                           "   Daten herunterladen")
          )
          )
        ),

        # Hinweis unter den Download-Optionen
      #   p(style = "font-size: 15px;",
      #     "Hinweis: Die Daten öffnen sich in einem Text-Dokument und können auf den
      # ersten Blick verwirrend aussehen. Kopieren Sie den Inhalt der Datei und fügen
      # Sie diesen direkt in das Chat-Fenster des GPT ein.")
      ),



  #     tags$head(
  #       tags$script(HTML("
  #   function copyToClipboard(id) {
  #     var pre = document.querySelector('#' + id + ' pre');
  #     var text = pre ? pre.innerText : '';
  #     navigator.clipboard.writeText(text).then(function() {
  #       alert('Inhalt wurde in die Zwischenablage kopiert!');
  #     }, function(err) {
  #       alert('Fehler beim Kopieren: ' + err);
  #     });
  #   }
  # "))
  #     ),


      # tagList(
      #   fluidRow(
      #     column(
      #       width = 8,
      #
      #       # Unsichtbarer Textblock zum Kopieren
      #       div(
      #         id = "copy_target",
      #         style = "visibility: hidden; height: 0; overflow: hidden;",
      #
      #         verbatimTextOutput("clipboard_text")
      #
      #       ),
      #
      #       # Kopier-Button
      #       actionButton("copy_btn", "Daten direkt kopieren"),
      #
      #       tags$script(HTML("
      #     $(document).on('click', '#copy_btn', function(){
      #       copyToClipboard('copy_target');
      #     });
      #   ")),


# )))

    ),

      ## MINT-DataLab-GPT ----

    column(
      width = 8,
      style = "display: flex; align-items: center; margin-bottom: 15px;",
      div(
        style = "margin: 0px 25px 50px 0px;",
        img(src='www/gpt_schritt_3.png',
            class = "img-responsive",
            alt = "Bild Schritt 3 klein",
            style="display: block;
                  margin-top: 10px; border: 2px solid #B16FAB;
                  border-radius: 15px; max-width: 50px;")
      ),
      div(id = "MINT-DataLab-GPT",
          style = "flex: 1; margin-bottom: 15px;",
          fluidRow(
            p(strong(style = "text-align: justify; font-size: 18px; margin-bottom: 15px; margin-left: 15px;",
                     "3. Wechseln Sie zum MINT-DataLab GPT und folgen den Anweisungen")),
            column(
              width = 6,  # Text in der linken Spalte

              p(style = "font-size: 15px;",
                "Sobald Sie auf den Link zum MINT-DataLab-GPT klicken, öffnet sich ein Chatfenster in ChatGPT.
                 Wählen Sie aus, ob sie eine Argumentation oder eine Bericht wollen,
                 der MINT-DataLab-GPT führt Sie durch die Erstellung der Analyse.")
            ),
            column(
              width = 5,
              div(style = "margin-left: 30px;",
              actionButton(label = tagList(icon("arrow-up-right-from-square"), "    Zum MINT-DataLab-GPT"), inputId = "GPT_link",
                           onclick = 'window.open("https://chatgpt.com/g/g-67e4f41fd91881919a753f4309194bf7-mint-datalab-gpt", "_blank");')
            )
          )
          )
      ),


        # p(style = "font-size : 15px;",
        #   "Sobald Sie auf den Link zum MINT-DataLab-GPT klicken, öffnet sich ein Chatfenster in ChatGPT.
        #   Wählen Sie aus, ob sie eine Argumentation oder eine Bericht wollen und laden Sie die Daten hoch.
        #   Der MINT-DataLab-GPT führt Sie durch die Erstellung der Analyse."),
        # tags$a(href="https://chatgpt.com/g/g-67e4f41fd91881919a753f4309194bf7-test-mint-datalab-assistent-test",
        #        "→ Zum MINT-DataLab-GPT", target="_blank",
        #        style = "color: #b16fab; font-weight: 600; font-size: 17px;"),
       # actionButton(label = "→ Zum MINT-DataLab-GPT", inputId = "GPT_link",
       #              onclick = 'window.open("https://chatgpt.com/g/g-67e4f41fd91881919a753f4309194bf7-mint-datalab-gpt", "_blank");')

      # )
     ),


      ## Grafiken ----
    column(
      width = 8,
      style = "display: flex; align-items: center; margin-bottom: 20px;",
      div(
        style = "margin: 0px 25px 90px 0px;",
        img(src='www/gpt_schritt_4.png',
            class = "img-responsive",
            alt = "Bild Schritt 4 klein",
            style="display: block;
                  margin-top: 10px; border: 2px solid #B16FAB;
                  border-radius: 15px; max-width: 50px;")
      ),
      div(id = "grafiken",
          style = "flex: 1; margin-bottom: 15px;",
          fluidRow(
            p(strong(style = "text-align: justify; font-size: 18px; margin-left: 15px;",
                     "4. Ergänzen Sie den KI-Bericht zur Veranschaulichung mit Grafiken")),
            column(
              width = 6,
              p(style = "font-size : 15px;",
                "Der Bericht wird anschaulicher, wenn Sie den Texten des MINT-DataLab-GPT Grafiken hinzuzufügen.
                Laden Sie die passenden Grafiken gesammelt hier herunter oder wählen Sie einzelne Grafiken
                im folgenden Abschnitt aus und fügen
                Sie Text und Grafiken für Ihren Bericht zusammen.")
            ),
        #     column(
        #       width = 5,
        #       div(style = "margin-left: 30px;",
        #       p(stlye="margin-left: 20px;",
        #         "→ Die Download-Option für alle Grafiken des MINT-DataLab finden Sie rechts oben an den Grafiken.")
        #     )
        # ),

        column(
          width = 5,
          div(style = "margin-left: 30px; margin-top: 10px;",
              p(stlye="margin-left: 20px;",
                 "→ Die Download-Option für alle Grafiken des MINT-DataLab finden Sie rechts oben an den Grafiken."),
              actionButton(
                ns("download_all_png_client"),
                label = tagList(icon("download"), "Alle Grafiken herunterladen (ZIP)")
              )
          )
        ),


        tags$script(HTML(sprintf("
(function() {
  function dateStr(){ return new Date().toISOString().slice(0,10); }
  function blobFromCanvas(canvas, type, quality){
    return new Promise(function(resolve){ canvas.toBlob(function(b){ resolve(b); }, type || 'image/png', quality || 1.0); });
  }

    function sanitize(name){
  return name
    .replace(/[\\/:*?'<>|]+/g, '_')   // : und andere unzulässige Zeichen
                         .replace(/_+/g, '_')
                         .replace(/^_+|_+$/g, '');
  }

  function filenameFromChart(chart, idx){
    var t = chart && chart.title && chart.title.textStr ? chart.title.textStr : null;
    var base = t ? sanitize(t) : ('chart' + (idx+1));
    return base + '.png';
  }

  function filenameFromWrapper(chart, idx){
    try {
      var wrap = chart.renderTo && chart.renderTo.closest ? chart.renderTo.closest('.dl-chart') : null;
      var fn = wrap && wrap.getAttribute ? wrap.getAttribute('data-filename') : null;
      if (fn && fn.trim()) return fn;
    } catch(e){}
    return null;
  }

  async function chartToPNGBlob(chart, scale){
    // Chartgröße lesen
  var w = Math.max(chart.chartWidth || 0, 800);
  var h = Math.round(w * 9 / 16);
  var s = 1; // für scharfes Ergebnis


    // Highcharts-SVG mit export-Optionen holen
    var svgStr = chart.getSVG({
      exporting: { sourceWidth: w * s, sourceHeight: h * s }
    });

    // Canvas vorbereiten
    var canvas = document.createElement('canvas');
    canvas.width  = w * s;
    canvas.height = h * s;

    var ctx = canvas.getContext('2d');
    // canvg rendert die SVG in das Canvas
    var v = await canvg.Canvg.fromString(ctx, svgStr, { ignoreMouse: true, ignoreAnimation: true });
    await v.render();

    return await blobFromCanvas(canvas, 'image/png', 1.0);
  }

  document.addEventListener('click', async function(ev){
    var btn = ev.target.closest('#%s');
    if (!btn) return;

    // Alle Highcharts-Instanzen einsammeln
   var charts = (window.Highcharts && Highcharts.charts ? Highcharts.charts : [])
  .filter(function(c){
    return c && c.renderTo && c.renderTo.offsetParent; // sichtbar im DOM
  });

    // gegen Doppelte absichern
    var seen = new Set();
    charts = charts.filter(function(c){
      var key = c.renderTo;
      if (seen.has(key)) return false;
      seen.add(key);
      return true;
    });

    if (!charts.length){ alert('Keine Highcharts-Instanzen gefunden.'); return; }


    // Hinweis: Charts müssen sichtbar gerendert sein (kein versteckter Tab)
    var old = btn.innerText; btn.disabled = true; btn.innerText = 'Erzeuge ZIP...';

    try {
      var zip = new JSZip();

      for (var i=0; i<charts.length; i++){
        var chart = charts[i];
        var name = filenameFromWrapper(chart, i) || filenameFromChart(chart, i);
        try {
          var blob = await chartToPNGBlob(chart, 2); // scale=2
          zip.file(name, blob);
        } catch(e) {
          console.error('Fehler beim Rendern von', name, e);
        }
      }

      var content = await zip.generateAsync({ type: 'blob', compression: 'STORE' });
      saveAs(content, 'alle_grafiken_' + dateStr() + '.zip');
    } catch(e){
      console.error(e);
      alert('Fehler beim Erzeugen des ZIP.');
    } finally {
      btn.disabled = false; btn.innerText = old;
    }
  }, false);
})();
", ns("download_all_png_client"))))

      )
      )
    )
),

# Überleitung zu Grafiken ----
  column(
    id = "daten_grafiken",
    width = 12,
    hr(style = "border-top: 2px solid #154194; margin-top: 20px;"),

    h2("Die Datengrundlage Ihres MINT-Berichts als Grafiken", #So geht der MINT-DataLab-GPT bei der Analyse vor
       style= "margin-bottom: 30px; margin-top: 40px;"),

    column(
      style = "margin-bottom: 40px;",
      width = 8,
      p("Im Folgenden finden Sie die Daten, die Sie dem MINT-DataLab-GPT eingespeist haben,
        als Grafiken dargestellt. Diese können Sie herunterladen und Ihrem Bericht hinzufügen.", br(),
        "Wir erläutern an dieser Stelle jedoch auch, wie die KI die Daten interpretiert –
        und bieten Ihnen so die Möglichkeit, die Ergebnisse kontrollieren zu können.", br(),
        # "Einen Beispielbericht für Hamburg können sie ",tags$a(href = "www/MINTvernetzt_Argumentationskette_Hamburg.pdf",
        #                                                       target = "_blank",
        #
        #"hier") , " herunterladen. "
        ),
      p("Hilfestellung für die weiteren Schritte:"),
      p(stlye="margin-left: 20px;",
        "→ Die Download-Option für alle Grafiken des MINT-DataLab finden Sie rechts oben an den Grafiken.", br(),
        # "→ Die ", tags$span("blauen Boxen", style = "color: #154194;"),
        # " rechts neben den Grafiken geben Impulse, welche weiteren
        #    Statistiken in einem MINT-Bericht ergänzt werden könnten.", br(),

        "→ Die ", tags$span("grünen Boxen", style = "color: #00a87a;"),
        "unter den Grafiken zeigen beispielhaft, wie man anhand
          der Statistiken für die MINT-Bildungsförderung argumentieren kann."
      )
    )
  ),

    ## Box Zeitverlauf MINT----

    fluidRow(id = "box1",
             shinydashboard::box(
               title = "Den regionalen Status-Quo analysieren",
               width = 12,
               column(
                 width = 9,
                 p("Als Einstieg in eine Argumentation kann ein kurzer Überblick über die MINT-Strukturen
                   der eigenen Region geigenet sein. Hierfür können Sie selbst rechercherien oder den
                   MINT-DataLab-GPT um eine Recherche bitten."),
                 p("Ergänzend dazu kann auf die Entwicklung der Zahlen von Beschäftigten, Studierenden und Auszubildenden
                   in MINT geschaut werden.
                   Das kann ein Indikator dafür sein, wie zentral der MINT-Sektor für die Region ist und ob
                   die Relevanz eher steigt oder der MINT-Bereich eher hinter anderen
                   Bereichen zurück fällt."),
                 shinyBS::bsPopover(id="anz_argument_1", title = "",
                                    content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, verändern Sie bitte kurz die Fenstergröße, indem Sie die Menü-Übersicht links ein- und wieder ausklappen oder indem Sie die Seite kurz verkleinern und wieder maximieren. Dann stellen sich die Größenverhältnisse der Grafiken korrekt ein."),
                                    placement = "top",
                                    trigger = "hover"),
                 tags$a(paste0("Ist die Grafik abgeschnitten dargestellt?"), icon("question-circle"), id = "anz_argument_1"),
                 br(),br(),
               ),
                 column(
                   width = 9,
                   shiny::mainPanel(
                     width = 12,
                     shinycssloaders::withSpinner(htmlOutput(ns("plot_argument_verlauf")),
                                                  color = "#154194"),
                      shinyBS::bsPopover(id="h_argument_1a", title = "",
                                       content = paste0("Es werden nur sozialversicherungspflichtige Beschäftigte betrachtet. Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Weitere Informationen finden Sie unter dem Reiter \"Datenquellen und Hinweise\"."),
                                       placement = "top",
                                       trigger = "hover"),
                    tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_argument_1a"),
                    br(),
                    shinyBS::bsPopover(id="i_argument_1", title = "",
                                       content = paste0("Die linke Grafik stellt den Zeitverlauf der Beschäftigen dar. Die sind in der ersten Einstellung, d.h. für Gesamtdeutschland im Jahr 2023 auf mehr als 7.8 Mio angestiegen, ein Plus von 500.000 gegenüber 2017. Rechts werden die Studierenden und Auszubildenden dargestellt. Diese waren für Gesamtdeutschland konstant."),
                                       placement = "top",
                                       trigger = "hover"),
                    tags$a(paste0("Interpretationshilfe"), icon("info-circle"), id = "i_argument_1")
                   )
                 ),
                 # column(
                 #   width = 3,
                 #   div(class = "content-box",
                 #       style = "background-color: #15419430;
                 #              color: #154194;
                 #              border: 2px solid #154194;
                 #              margin-left: 20px;
                 #              width: 90%;
                 #              border-radius: 10px;",
                 #              p("Weitere Statistiken, die hier ergänzt werden könnten:"),
                 #              p("MINT-Anteil:  \"Alle Bildungsbereiche\", aktueller MINT-Anteil + MINT-Anteil im Zeitverlauf"),
                 #              p("Bundeslandvergleich: \"Ausbildung & Beruf\", aktueller MINT-Anteil + Bundeslandvergleich")
                 #   )
                 # ),
               column(
                 width = 12,

                 column(
                   width = 3,
                   br(),
                   div(class = "content-box",
                       style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                       p(strong("Wenn die Kurve steigt:")),
                       p("Die Relevanz von MINT für die Region wächst.
                     MINT-Kenntnisse müssen ausgebaut werden, um steigenden Bedarfen an
                     MINT-Kompetenzen begegnen zu können.
                     In MINT-Angebote zu investieren heißt, in die Zukunft zu investieren.")
                   )
                 ),
                 column(
                   width = 3,
                   br(),
                   div(class = "content-box",
                       style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                       p(strong("Wenn die Kurve gleich bleibt:")),
                       p("Der MINT-Bereich ist konstant eine wichtige Säule der Region.
                     Gleichzeitig werden MINT-Kompetenzen aufgrund von Digitalisierung
                     und Technologisierung immer wichtiger. In MINT-Angebote zu
                       investieren heißt, in die Zukunft zu investieren.")
                   )
                 ),
                 column(
                   width = 3,
                   br(),
                   div(class = "content-box",
                       style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                       p(strong("Wenn die Kurve sinkt:")),
                       p("Der MINT-Bereich der Region wird kleiner, obwohl aufgrund
                    von Digitalisierung und Technologisierung MINT die Zukunft des
                    Wirtschaft- und Bildungsbereichs prägt. Stärker in MINT-Angebote
                      zu investieren ist für die zukünftige Wettbewerbsfähigkeit
                      der Region essenziell.")
                   )
               )
               )
             )
            ),

    ## Box Fachkräftemagel ----
    fluidRow(id = "box2",
      shinydashboard::box(
        title = "Den nicht gedackten Bedarf an Fachkräften verdeutlichen",
        width = 12,
        column(
          width = 8,
          p("Der Bedarf an MINT-Fachkräften ist bundesweit hoch. Das zeigt z. B. die MINT-Fachkräftelücke
          aus dem MINT-Report des IW Köln. Für 2023 wird diese Lücke deutschlandweit auf 209.000 geschätzt.
          So viele MINT-Fachkräfte fehlen also deutschlandweit.
          Diese Zahl liegt für die einzelnen Bundesländer so nicht vor.", br(),

          "Dafür kann die Engpassanalyse der Bundesagentur für Arbeit betrachtet werden. Sie zeigt das Ausmaß des
            akuten Fachkräfteengpasses. Die Zahlen unterstreichen, wie wichtig MINT-Förderung ist,
          um den Fachkräftemangel zu reduzieren.")
        ),
        column(
          width = 9,
          shiny::mainPanel(
            width=12,
            shinycssloaders::withSpinner(htmlOutput(ns("plot_argument_fachkraft")),
                                         color = "#154194"),
            shinyBS::bsPopover(id="h_argument_2", title = "",
                               content = paste0("Es werden nur sozialversicherungspflichtige Beschäftigte betrachtet. Auf Bundesebene gibt es detaillierte Daten zu Fachkräfteengpässen in einzelnen Berufsgattungen, z. B. Mechatronik. Für die Bundesländer liegen nur zusammengefasste Informationen zu MINT-dominierten Berufsgruppen wie Mechatronik und Automatisierungstechnik vor. Mehr Infos dazu finden Sie unter der Seite \"MINT-Fachkräfte\"."),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_argument_2"),
            br(),
            shinyBS::bsPopover(id="i_argument_2", title = "",
                               content = paste0("In der ersten Einstellung ist zu sehen, dass sowohol in MINT-Berufen als auch in Nicht-MINT-Berufen ca. ein Drittel jeweils als Engpassberufe galten. Schaut man sich die genauen Zahlen mit dem Hover an, dass dies in MINT-Berufen 36 % bzw. 69 Berufe betrifft, bei Nicht-MINT-Berufen auch 36 %, allerdings 122 Berufe."),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Interpretationshilfe"), icon("info-circle"), id = "i_argument_2")
          )
        ),
        # column(
        #   width = 3,
        #   div(class = "content-box",
        #       style = "background-color: #15419430;
        #                       color: #154194;
        #                       border: 2px solid #154194;
        #                       margin-left: 20px;
        #                       width: 90%;
        #                       border-radius: 10px;",
        #       p("Weitere Statistiken, die hier ergänzt werden könnten:"),
        #       p("Fachkräfte-Engpass nach MINT-Disziplin: \"Fokusseite MINT-Fachkräfte\",
        #         unter \"Berufsgruppen: aktueller Fachkräftebedarf in MINT\", Fachkräfteengpass der Bundesländer"),
        #       p("Anteil und Entwicklung der MINT-Disziplinen: \"Ausbildung & Beruf\", unter M-I-N-T, aktueller Anteil MINT-Disziplinen")
        #
        #   )
        # ),
        column(
          width = 12,
          column(
            width = 3,
            br(),
            div(class = "content-box",
                style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                p(strong("Erhöhter Fachkräftemangel in MINT:")),
                p("Während viele Branchen mit Fachkräftemangel zu kämpfen haben,
                ist die Lage in MINT-Berufen, und insbesondere im Technik-Bereich,
                besonders schlecht. Das unterstreicht: Wir brauchen mehr Menschen,
                  die sich für MINT interessieren und MINT-Kompetenzen entwickeln,
                  z. B. durch MINT-Bildungsförderung.")
            )
          ),
          column(
            width = 3,
            br(),
            div(class = "content-box",
                style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                p(strong("Ähnlich hoher Fachkräftemangel in MINT und Nicht-MINT:")),
                p("Viele Branchen haben mit Fachkräftemangel zu kämpfen,
                so auch der MINT-Bereich. Damit man dem Fachkräftemangel in Zukunft
                  begegnen kann, braucht es mehr Menschen, die sich für MINT
                  interessieren und MINT-Kompetenzen entwickeln, z. B.
                  durch MINT-Bildungsförderung.")
            )
          )

        )
      )),

    ## Box Demografie ----

    fluidRow(id = "box3",
      shinydashboard::box(
        title = "Demografische Zukunftstrends in die Argumentation integrieren",
        width = 12,
        column(
          width = 8,
          p("Die Grafik zuvor zeigt: Schon heute fehlen in vielen Regionen besonders MINT-Fachkräfte.
            Der demografische Wandel wird die Situation weiter verschärfen,
            da ein großer Teil der MINT-Beschäftigten in den nächsten Jahren aus
            der Berufstätigkeit ausscheiden wird.", br(),
            "Das veranschaulicht die folgende Grafik, wenn man die Zahl der MINT-Beschäftigten über 55 Jahren, welche
            in den nächsten rund 10 Jahren in Rente gehen werden, mit der Anzahl an MINT-Beschäftigten
            unter 25 Jahren, die in den Berufen nachfolgen, vergleicht.")
        ),
        column(
          width = 9,
          shiny::mainPanel(
            width = 12,
            shinycssloaders::withSpinner(htmlOutput(ns("plot_argument_demografie")),
                                         color = "#154194"),
            shinyBS::bsPopover(id="h_argument_31", title = "",
                               content = paste0("Es werden nur sozialversicherungspflichtige Beschäftigte betrachtet. Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Weitere Informationen finden Sie unter dem Reiter \"Datenquellen und Hinweise\"."),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_argument_31"),
            br(),
            shinyBS::bsPopover(id="i_argument_3", title = "",
                               content = paste0("In der ersten Einstellung ist zu sehen, dass es 2023 mehr als 7.8 Mio Beschäftigte in MINT in Deutschland gab. Dabei macht die Altersgruppe ü55 1.8 Mio aus (23 % aller Beschäftigten), es kommen aber nur knapp 800.000 der Altersgruppe u25 nach (10 %)."),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Interpretationshilfe"), icon("info-circle"), id = "i_argument_3")
          )
        ),
        # column(
        #   width = 3,
        #   div(class = "content-box",
        #       style = "background-color: #15419430;
        #                       color: #154194;
        #                       border: 2px solid #154194;
        #                       margin-left: 20px;
        #                       width: 90%;
        #                       border-radius: 10px;",
        #       p("Weitere Statistiken, die hier ergänzt werden könnten:"),
        #       p("MINT-Anteil nach Gruppen: \"Ausbildung & Beruf\", aktueller MINT-Anteil + Gruppenvergleich – Balkendiagramm, Auswahl unter Berufsgruppen treffen"),
        #
        #   )
        # ),
        column(
          width = 12,
          column(
            width = 3,
            br(),
            div(class = "content-box",
                style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                p(strong("Mehr MINT-Beschäftigte scheiden aus dem Berufsleben aus als nachkommen:")),
                p("Das wird den Fachkräftemangel verschärfen. Jetzt ist der letzte Moment,
                  um mit gezielter MINT-Bildungsförderung junge Menschen für MINT zu interessieren,
                  und die Folgen des demografischen Wandels noch abmildern zu können.")
            )
          )

        )
      )),

    ## Box Nachwuchs ----
    fluidRow(id = "box4",
      shinydashboard::box(
        title = "Die aktuelle Nachwuchssituation analysieren",
        width = 12,
        column(
          width = 8,
          p("Viele Faktoren werden zusammen kommen müssen, um die Fachkräftelage
            in MINT zu stabilisieren. Auch, weil der Bedarf an MINT-Kräften steigt.
            Ein Schlüssel ist, mehr MINT-Nachwuchs zu gewinnen, doch in vielen
            MINT-Bereichen steht es aktuell nicht gut um den Nachwuchs, wie die folgende Grafik zeigt.")
        ),
        column(
          width = 9,
          shiny::mainPanel(
            width = 12,
            shinycssloaders::withSpinner(htmlOutput(ns("plot_argument_nachwuchs")),
                                         color = "#154194"),
            shinyBS::bsPopover(id="h_argument_4", title = "",
                               content = paste0("Nachwuchs bezeichnet hier die gemeinsame Betrachtung von Auszubildenden und Studierenden."),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_argument_4"),
            br(),
            shinyBS::bsPopover(id="h_argument_41", title = "",
                               content = paste0("In der ersten Einstellung ist zu sehen, dass in Deutschland die Anzahl des Nachwuchses in den Ingenieurswissenschaften deutlich über der Informatik und den Mathematik/Naturwissenschaften liegt. Während der Nachwuchs in Informatik deutschlanweit zunimmt (+23,7 % sei 2017), nimmt er in den anderen Disziplinen ab (-6,6 % bzw. 5,7 %)."),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Interpretationshilfe"), icon("info-circle"), id = "h_argument_41")
          )
        ),
        # column(
        #   width = 3,
        #     div(class = "content-box",
        #         style = "background-color: #15419430;
        #                       color: #154194;
        #                       border: 2px solid #154194;
        #                       margin-left: 20px;
        #                       width: 90%;
        #                       border-radius: 10px;",
        #         p("Weitere Statistiken, die hier ergänzt werden könnten:"),
        #         p("Getrennte Betrachtung von Studierenden und Auszubildenden: \"Ausbildung & Beruf\" bzw. \"Studium\",
        #           M-I-N-T, Anteil MINT-Fächer im Zeitverlauf"),
        #     )
        # ),
        column(
          width = 12,
          column(
            width = 3,
            br(),
            div(class = "content-box",
                style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                p(strong("Wenn die Anzahl steigt:")),
                p("Mehr junge Menschen interessieren sich für diese Disziplin.
                Doch in den meisten MINT-Bereichen herrscht bereits Fachkräftemangel,
                welcher sich durch die demografische Entwicklung noch verschärfen wird.
                Deshalb ist Förderung notwendig, um positive Tendenzen zu unterstreichen
                und zu verstärken.")
            )
          ),
          column(
            width = 3,
            br(),
            div(class = "content-box",
                style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                p(strong("Wenn die Anzahl gleich bleibt:")),
                p("Die Entwicklung des Nachwuchses in diesem MINT-Bereich ist stabil.
                  Doch der Bedarf an Kompetenzen aus dieser Disziplin kann weiter
                  steigen und somit auch der Bedarf an Nachwuchs. Ohne Förderinitiativen
                  kann es auch hier zu einer Verschärfung der Fachkräftelage kommen.")
            )
          ),
          column(
            width = 3,
            br(),
            div(class = "content-box",
                style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                p(strong("Wenn die Anzahl sinkt")),
                p("Weniger junge Menschen scheinen sich für diesen Bereich zu interessieren.
                  Falls hier schon Fachkräftemangel besteht, wird sich dieser weiter verschärfen.
                  Vor allem hier müssen jungen Menschen ihre Perspektiven gezeigt und Interesse geweckt werden.")
            )
          )

        )
      )),

    ## Box Wirkhebel Förderung ----
    fluidRow(id = "box5",
      shinydashboard::box(
        title = "Das Potenzial von Nachwuchsförderung herausstellen",
        width = 12,
        column(
          width = 8,
          p("Um dem Fachkräftemangel im MINT-Bereich wirksam zu begegnen,
            sind verschiedene Maßnahmen nötig. Besonders entscheidend ist dabei
            die Förderung des MINT-Nachwuchses, aber auch
            die gezielte Unterstützung von Frauen.")
        ),
        column(
          width = 9,
          shiny::mainPanel(
            width = 12,
            shinycssloaders::withSpinner(htmlOutput(ns("plot_argument_wirkhebel")),
                                         color = "#154194"),
            shinyBS::bsPopover(id="erkl_wirkhebel_argument", title="",
                               content = paste0("Gesamteffekt: Wirkung aller Hebel kombiniert.", br(),br(), "MINT-Nachwuchs fördern: Zunahme von MINT-Fachkräften unter 35 zwischen 2012 und 2022 setzt sich so in den nächsten Jahren fort.", br(),br(), "Mädchen- und Frauen-Förderung in MINT: Zunahme von weiblichen MINT-Fachkräften unter 35 zwischen 2012 und 2022 setzt sich so in den nächsten Jahren fort.", br(),br(), "Zuwanderung MINT-Fachkräfte: „Hohe Zuwanderung“-Szenario der 15. koordinierten Bevölkerungsvorausberechnung des Statistischen Bundesamts.", br(),br(), "Verbleib älterer MINT-Fachkräfte: Anteil an erwerbstätigen MINT-Fachkräften unter den 55-59-, 60-64-, und 65-69-Jährigen wächst weiterhin so an wie zwischen 2012-2022."),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Das bedeuten die Wirkhebel"), icon("info-circle"), id="erkl_wirkhebel_argument"),
            br(),
            shinyBS::bsPopover(id="h_argument_5", title = "",
                               content = paste0("Weitere Informationen zu den Berechnungen des IW Köln im Auftrag von MINTvernetzt lassen sich auf der Seite \"MINT-Fachkräfte\" nachlesen."),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Methodenhinweis"), icon("info-circle"), id = "h_argument_5"),
            br(),
            shinyBS::bsPopover(id="i_argument_5", title = "",
                               content = paste0("Spielen alle Wirkhebel zusammen, können bis 2037 1,4 Mio. zusätzliche MINT-Fachkräfte gewonnen werden. Der stärkste Hebel, mit rund +670.000 MINT-Fachkräften ist die Förderung des MINT-Nachwuchses."),
                               placement = "top",
                               trigger = "hover"),
            tags$a(paste0("Interpretationshilfe"), icon("info-circle"), id = "i_argument_5")
          )
        ),
        # column(
        #   width = 3,
        #   div(class = "content-box",
        #       style = "background-color: #15419430;
        #                       color: #154194;
        #                       border: 2px solid #154194;
        #                       margin-left: 20px;
        #                       width: 90%;
        #                       border-radius: 10px;",
        #       p("Weitere Statistiken, die hier ergänzt werden könnten:"),
        #       p("Alle Ergebnisse der Zukunftsszenarien für MINT-Fachkräfte: “Fokusseite MINT-Fachkräfte“ Zukunftsszenarien + Wirkhebel"),
        #   )
        # ),
        column(
          width = 12,
          column(
            width = 3,
            br(),
            div(class = "content-box",
                style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                p(strong("MINT-Nachwuchs Förderung")),
                p("Der größte Wirkhebel, um dem akuten MINT-Fachkräftemangel entgegenzuwirken,
                  sind mehr junge Menschen in MINT. Ohne Nachwuchsförderung, z. B. allein über Zuwanderung,
                  wird sich die MINT-Lücke nicht schließen lassen. MINT-Bildungsförderung ist die beste Chance, die MINT-Industrie
                  langfristig lebendig zu halten.")
            )
          ),
          column(
            width = 3,
            br(),
            div(class = "content-box",
                style = "width: 250px;
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                p(strong("Mädchen und Frauen in MINT fördern")),
                p("Auch das ist ein starker Hebel, um den Fachkräftemangel in MINT zu reduzieren.
                  Mehr junge Frauen, die sich beruflich für MINT entscheiden,
                  tragen außerdem zu diverseren Perspektiven in MINT bei und so zu
                  einer höheren Qualität in MINT-Forschung und -Entwicklung.")
            )
          )

        )

      )),

    ## FAQ  ----

    hr(style = "border-top: 2px solid #154194; margin-top: 30px; margin-bottom: 5px;"),



    br(),
    fluidRow(id = "faq",
      column(
        width = 9,
        # h3("Fragen und Antworten"),
        # reactable::reactableOutput(ns("faq_table")),
        h2("Fragen und Antworten"),

        tags$details(
          tags$summary(strong(class = "faq-summary",
                              "Welche Technologie steckt hinter dem MINT-DataLab-GPT")),
          br(),
          p("Der MINT-DataLab-GPT ist eine KI-Anwendung, die auf der Grundlage von ChatGPT funktioniert.
              GPT steht für Generative Pre-Trained Transformer und ist eine Erweiterung der Funktionen von ChatGPT.
              Diese Erweiterung besteht darin, dass ChatGPT mit Wissen aus dem MINT-DataLab
              trainiert wurde und darauf spezialisiert ist, Nutzende bei der Analyse von Daten zu unterstützen.")
          ),

        tags$details(
          tags$summary(strong(class = "faq-summary",
                              "Kann ich den MINT-DataLab-GPT auch ohne OpenAI-Konto nutzen?")),
          br(),
          p("Nein, die Nutzung des MINT-DataLab-GPT erfordert ein aktives OpenAI-Konto.
          Da es sich um einen individualisierten GPT handelt, läuft der Zugriff über die Infrastruktur von OpenAI.
          Die Registrierung ist kostenlos und in wenigen Schritten möglich.")
        ),

        tags$details(
          tags$summary(strong(class = "faq-summary",
                              "Kann ich dem Bericht vertrauen, den die KI generiert?")),
          br(),
          p("Die KI generiert den Bericht auf der Grundlage von den hochgeladenen Daten
            aus dem MINT-DataLab. Die KI wurde trainiert, keine externen Daten hinzuzunehmen,
            außer sie wird explizit darum gebeten. Wir haben sie auch angewiesen,
            nicht zu halluzinieren. Es kann jedoch zum aktuellen Zeitpunkt bei der Arbeit mit KI
            nie komplett ausgeschlossen werden, dass doch Halluzinationen stattfinden.
            Wir empfehlen, die Angaben im Bericht nach der Erstellung noch einmal zu kontrollieren.
            Dazu können die Grafiken und Erläuterungen auf dieser Seite genutzt werden.")
        ),

        tags$details(
          tags$summary(strong(class = "faq-summary",
                              "Welche Quellen muss ich angeben, wenn ich den MINT-DataLab-GPT nutze?")),
          br(),
          HTML("
    <p>Beim Herunterladen der Grafiken oder Daten werden die zugrundeliegenden Datenquellen automatisch mitgeliefert.</p>
    <p>Der MINT-DataLab-GPT ergänzt zusätzlich Quellen von Kurzanalysen des MINT-DataLabs, die er genutzt hat,
    oder Online-Recherchen, die zusätzlich angefragt wurden.</p>
    <p>Je nach Kontext oder Zweck der Nutzung empfehlen wir, aus Gründen der Transparenz
    auf die Unterstützung durch ein KI-Modell und den Einsatz des MINT-DataLab-GPT hinzuweisen.
    Bitte prüfen sie den rechtlichen Rahmen Ihres Anwendungsbereichs und ob die Verwendung von KI ggf. verpflichtend transparent gemacht werden muss.
    <p>Empfohlene Formulierung:</p>
    <blockquote>Erstellt unter Verwendung des angepassten GPT-Sprachmodells von MINTvernetzt (MINT-DataLab-GPT) auf Basis von OpenAI-Technologie.</blockquote>
  ")
        ),

        tags$details(
          tags$summary(class = "faq-summary",
                       "Kann ich auch andere Daten mit dem MINT-DataLab-GPT analysieren?"),
          br(),
          HTML("
    <p>Der MINT-DataLab-GPT ist auf die Daten des MINT-DataLabs spezialisiert, kann jedoch grundsätzlich auch andere oder eigene Daten verarbeiten.</p>
    <p>Hinweis: Externe Formate können zu Lesefehlern führen. Bitte prüfen Sie die Korrektheit Ihrer Daten sorgfältig.</p>
  ")
        ),

        tags$details(
          tags$summary(strong(class = "faq-summary",
                              "Wie kann ich das Ergebnis der Analyse exportieren?")),
          br(),
          HTML("
    <p>Der MINT-DataLab-GPT liefert direkt nutzbare Textbausteine, die sich flexibel
    in Berichte oder Anträge kopieren lassen und dort weiterbearbeiten. Wie bei der Nutzung anderer
    KI-Chats kann zusätzlich das Format des gewünschten Outputs spezifiziert werden.</p>
    <p>Der Daten-Export für die Nutzung mit dem GPT läuft über eine <code>.txt</code>-Datei. Der Inhalt der Datei kann
    durch \"Copy & Paste\" direkt in den Chat kopiert werden.</p>  ")
          # <p>Beispielhafter Export:</p>
          # <img src='www/beispiel_export.png' alt='Beispiel Export' style='max-width: 100%; border: 1px solid #ccc; border-radius: 8px;'>
        ),

        tags$details(
          tags$summary(strong(class = "faq-summary","Ich habe ein Fehlverhalten festgestellt, wo kann ich das melden?")),
          br(),
          HTML("
    <p>Wir entwickeln den MINT-DataLab-GPT kontinuierlich weiter und auch das
    zugrundeliegende KI-Modell von OpenAI kann sich verändern.</p>
    <p>Falls Ihnen ein Fehlverhalten auffällt oder Sie Feedback haben, wie die Nutzung
    zukünftig noch verbessert werden könnte, schreiben Sie uns gerne eine kurze Nachricht per E-Mail an:</p>
    <p><a href='mailto:katharina.brunner@mint-vernetzt.de?subject=Feedback%20Argumentationshilfe'>katharina.brunner@mint-vernetzt.de</a></p>
    <p>Vielen Dank!</p>
  ")
        ),

        p("Sollten Sie weitere Fragen haben, schreiben Sie gerne eine Nachricht per ",
          tags$a(href = "mailto:katharina.brunner@mint-vernetzt.de?subject= ausserschulische Daten MINT-Datalab", "E-Mail"),
          ". Da der MINT-DataLab-GPT sich noch in der Testphase befindet,
              freuen wir uns auch über Hinweise, wenn es Irritationen oder Auffälligkeiten gab."),

        br(),

        # Nutzungshinweis ####
        h2("Nutzungshinweis"),
        p("Der MINT-DataLab-GPT ist eine KI-Anwendung, die auf Technologie von OpenAI basiert.
        Die Nutzung des MINT-DataLab-GPT erfordert daher ein aktives OpenAI-Konto.
        Die bereitgestellten Inhalte werden automatisiert generiert und können unvollständig,
        fehlerhaft oder veraltet sein. Die Nutzer:innen sind selbst für eine kritische Prüfung der
        Ausgaben verantwortlich."),
        p("Für den MINT-DataLab-GPT gelten folgende Nutzungsbedingungen, auf die auch
        zu Beginn des Chats hingewiesen wird: ",
          tags$a(href = "www/Nutzungshinweis_Haftungsausschluss_GPT.pdf", target = "_blank", "Nutzungshinweis MINT-DataLab-GPT")),
        p("MINTvernetzt und der Stifterverband übernehmen keine Haftung für Schäden
          oder Nachteile, die aus der Verwendung der bereitgestellten Informationen entstehen.
          Es gelten die allgemeinen Haftungsausschlüsse, wie im Impressum hinterlegt."),
        p(),
        p("Je nach Kontext und Zweck der Nutzung empfehlen wir aus Gründen der Transparenz,
               auf die Unterstützung durch ein KI-Modell und insbesondere auf den Einsatz des
               MINT-DataLab-GPT hinzuweisen. Dafür kann folgende Formulierung genutzt werden:",
          br(),
          "\"Erstellt unter Verwendung des angepassten GPT-Sprachmodells von
               MINTvernetzt (MINT-DataLab-GPT) auf Basis von OpenAI-Technologie.\"")
      )
    ),


    funct_footer()

    )
}


# Argumentation Server

mod_argumentation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    r <- reactiveValues()

    observeEvent(input$region_argumentationshilfe, {
      r$region_argumentationshilfe <- input$region_argumentationshilfe
    })


    # Download der gesemmelten Daten

    output$download_txt <- downloadHandler(
      filename = function() paste0("daten_export_", Sys.Date(), ".txt"),
      contentType = "text/plain; charset=UTF-8",
      content = function(file) {
        req(r$region_argumentationshilfe)
        tryCatch({
          txt_df <- daten_download(r)  # -> gib hier ein data.frame zurück, nicht den formatierten String
          readr::write_delim(txt_df, file = file, delim = "\t", na = "", append = FALSE)
        }, error = function(e) {
          showNotification(paste("Download fehlgeschlagen:", e$message), type = "error")
          stop(e)
        })
      }
    )

    # output$download_txt <- downloadHandler(
    #   filename = function() {
    #     paste0("daten_export_", Sys.Date(), ".txt")
    #   },
    #   content = function(file) {
    #     daten <- daten_download(r)#
    #
    #     writeLines(daten, con = file)
    #   },
    #   contentType = "text/plain"
    # )


#
#     output$clipboard_text <- renderText({
#       daten_text()
#     })
#


    output$plot_argument_verlauf <- renderUI({
      argument_verlauf(r)
    })

    output$plot_argument_fachkraft <- renderUI({

      plots <- argument_fachkraft(r)

        div(
          style = "width: 1000px;",
          plots
          )


    })

    output$plot_argument_demografie <- renderUI({
      argument_demografie(r)
    })

    output$plot_argument_nachwuchs <- renderUI({
      argument_nachwuchs(r)
    })

    output$plot_argument_wirkhebel <- renderUI({
      argument_wirkhebel(r)
    })

    # output$plot_argument_frauen <- renderUI({
    #   argument_frauen(r)
    # })



    # output$download_txt1 <- downloadHandler(
    #   filename = function() {
    #     paste0("test_", Sys.Date(), ".zip")
    #   },
    #   content = function(file) {
    #     tmpdir <- tempdir()
    #     files <- c()
    #
    #     # einfache Testgrafik
    #     chart <- highcharter::highchart() %>%
    #       highcharter::hc_title(text = "Testplot") %>%
    #       highcharter::hc_add_series(data = c(1, 3, 2, 4))
    #
    #     htmlfile <- file.path(tmpdir, "test.html")
    #     pngfile  <- file.path(tmpdir, "test.png")
    #
    #     htmlwidgets::saveWidget(chart, file = htmlfile, selfcontained = TRUE)
    #     webshot2::webshot(htmlfile, file = pngfile, vwidth = 800, vheight = 600)
    #
    #     zipfile <- file.path(tmpdir, "charts.zip")
    #     zip::zip(zipfile, pngfile)
    #     file.copy(zipfile, file)
    #   },
    #   contentType = "application/zip"
    # )


    # output$download_txt1 <- downloadHandler(
    #   filename = function() {
    #     paste0("alle_grafiken_", Sys.Date(), ".zip")
    #   },
    #   content = function(file) {
    #     tmpdir <- tempdir()
    #     files <- c()
    #
    #     # charts <- list(
    #     #   verlauf     = argument_verlauf(r),
    #     #   fachkraft   = argument_fachkraft(r),
    #     #
    #     #   nachwuchs   = argument_nachwuchs(r),
    #     #   wirkhebel   = argument_wirkhebel(r)
    #     # )
    #
    #     charts <- list(
    #       verlauf     = argument_verlauf(r),
    #       fachkraft   = argument_fachkraft(r),
    #       demografie  = argument_demografie(r),
    #       nachwuchs   = argument_nachwuchs(r),
    #       wirkhebel   = argument_wirkhebel(r)
    #     )
    #
    #     for (name in names(charts)) {
    #       htmlfile <- file.path(tmpdir, paste0(name, ".html"))
    #       pngfile  <- file.path(tmpdir, paste0(name, ".png"))
    #       htmlwidgets::saveWidget(charts[[name]], file = htmlfile, selfcontained = TRUE)
    #       webshot2::webshot(htmlfile, file = pngfile, vwidth = 800, vheight = 600)
    #       files <- c(files, pngfile)
    #     }
    #
    #     zipfile <- file.path(tmpdir, "charts.zip")
    #     zip::zip(zipfile, files)
    #     file.copy(zipfile, file)
    #   },
    #   contentType = "application/zip"
    # )


    # output$download_txt1 <- downloadHandler(
    #   filename = function() paste0("testplot_", Sys.Date(), ".png"),
    #   content = function(file) {
    #     testchart <- highcharter::highchart() |>
    #       highcharter::hc_title(text = "Testplot") |>
    #       highcharter::hc_add_series(data = c(1,3,2,4))
    #
    #     save_widget_to_png(testchart, file)
    #   },
    #   contentType = "image/png"
    # )


    # output$download_txt1 <- downloadHandler(
    #   filename = function() paste0("alle_grafiken_", Sys.Date(), ".zip"),
    #   content  = function(file) {
    #     # Arbeitsordner
    #     tmpdir <- tempfile("charts_")
    #     dir.create(tmpdir, showWarnings = FALSE)
    #     on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)
    #
    #     # 1) Eure bestehenden Funktionen (müssen highcharter-Widgets zurückgeben!)
    #     charts <- list(
    #       verlauf    = argument_verlauf(r),
    #       fachkraft  = argument_fachkraft(r),
    #       demografie = argument_demografie(r),
    #       nachwuchs  = argument_nachwuchs(r),
    #       wirkhebel  = argument_wirkhebel(r)
    #     )
    #
    #     # 2) Sicherheitscheck
    #     bad <- names(charts)[!vapply(charts, htmlwidgets::is.htmlwidget, logical(1))]
    #     if (length(bad)) {
    #       stop(sprintf("Folgende Einträge sind KEINE htmlwidgets: %s",
    #                    paste(bad, collapse = ", ")))
    #     }
    #
    #     # 3) In PNGs konvertieren
    #     png_files <- character(0)
    #     for (nm in names(charts)) {
    #       pngfile <- file.path(tmpdir, paste0(nm, ".png"))
    #       save_widget_to_png(charts[[nm]], pngfile, vwidth = 1200, vheight = 800, zoom = 2)
    #       png_files <- c(png_files, pngfile)
    #     }
    #
    #     # 4) ZIP bauen (zipr ist hier am bequemsten)
    #     zipfile <- file.path(tmpdir, "charts.zip")
    #     zip::zipr(zipfile, files = png_files)
    #
    #     # 5) Ausliefern
    #     ok <- file.copy(zipfile, file, overwrite = TRUE)
    #     if (!ok) stop("Konnte ZIP nicht an den Download-Stream kopieren.")
    #   },
    #   contentType = "application/zip"
    # )




  })
}

