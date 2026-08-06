
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
            img(src='www/Banner_KI_Analysehilfe_beta.png',
                class = "img-responsive",
                # height = "300px",
                alt = "Banner KI-Analysehilfe",
                style="display: block; margin-left: auto; margin-right: auto; margin-bottom: 20px;"
            )))),


    ## Einleitungstext ----
    fluidRow(
      div(class = "clean-box",
          column(
            width = 8,
            h1("Datenbasierte Berichte und Argumentationen mit KI erstellen"),

            p("Daten helfen dabei, die MINT-Bildungswelt zu verstehen. Gleichzeitig sind
              Daten das Fundament, um wirkungsvolle Entscheidungen zu treffen und
              erfolgreich für MINT-Förderung zu argumentieren.
              Doch wie lassen sich die Daten aus dem MINT-DataLab korrekt interepretieren?")
            )
          )),
    #         p("Bei dieser Frage können Sie sich von KI unterstützen lassen: dem MINT-DataLab-GPT"),
    #
    #         strong("Der Chatbot unterstützt konkret in drei Situationen:"),
    #         tags$ol(
    #           tags$li("Er erstellt einen MINT-Bericht für ein ausgewähltes Bundesland"),
    #           tags$li("Er hilft, für MINT-Förderung zu argumentieren"),
    #           tags$li("Er hilft, Daten grundlegend zu interpretieren"),
    #         )
    #       ),
    #       column(
    #         width = 4,
    #         tags$div(
    #           style = "display: flex; flex-direction: column; align-items: flex-start; justify-content: flex-start;",
    #           tags$strong(
    #             "Direkt Analyse starten:",
    #             style = "margin: 40px 0px 0px 60px"
    #           ),
    #                   class = "linked-image",
    #                   style = "flex: 0 0 20%;",
    #                   tags$a(
    #                     href = "https://chatgpt.com/g/g-695cd1fa74f881918a54b0517af8163e-mint-datalab-gpt",
    #                     target = "_blank",
    #                     tags$img(
    #                       src = "www/Bild_MINT-DataLab-GPT.png",
    #                       alt = "MINT-DataLab-GPT Symbolbild",
    #                       style = "max-width: 28%; height: auto; cursor: pointer;
    #                       margin: 10px 0px 0px 70px; border-radius: 10px;"
    #                     )
    #                   )
    #                 ),
    #         tags$a(
    #           href = "https://chatgpt.com/g/g-695cd1fa74f881918a54b0517af8163e-mint-datalab-gpt",
    #           target = "_blank",
    #           p("Link MINT-DataLab-GPT", style = "text-decoration: underline; color: #b16fab;
    #             margin-left: 60px;")
    #         )
    #       ),

        # Datenanalyse mit KI ----

        column(
          width = 12,
          h2("Datenanalyse mit KI: Einfach und individuell eigene Analysen erstellen", #Schnellstart: So analysieren Sie Daten mit dem MINT-DataLab-GPT, Catchy-Intro
            style = "margin-top: 30px;"),
         # hr(style = "border-top: 2px solid #ee7775; margin-top: 15px; margin-bottom: 15px;")
        ),

        ### Einleitung/GPT ----
        fluidRow(
          div(class = "clean-box",
              style = "margin-bottom:0px;
              border-top: 2px solid #154194;
              border-bottom: 2px solid #154194;
              padding-top: 30px;
              padding-bottom: 300px;
              ",

              column(
                width = 2,
                tags$div(
                  style = "display: flex; flex-direction: column; align-items: flex-start; justify-content: flex-start;",
                  tags$strong(
                    "Direkt Analyse mit KI starten:",
                    style = "margin: 40px 0px 0px 60px"
                  ),
                  class = "linked-image",
                  style = "flex: 0 0 20%;",
                  tags$a(
                    href = "https://chatgpt.com/g/g-695cd1fa74f881918a54b0517af8163e-mint-datalab-gpt",
                    target = "_blank",
                    tags$img(
                      src = "www/Bild_MINT-DataLab-GPT.png",
                      alt = "MINT-DataLab-GPT Symbolbild",
                      style = "max-width: 28%; height: auto; cursor: pointer;
                          margin: 10px 0px 0px 70px; border-radius: 10px;"
                    )
                  )
                ),
                tags$a(
                  href = "https://chatgpt.com/g/g-695cd1fa74f881918a54b0517af8163e-mint-datalab-gpt",
                  target = "_blank",
                  p("Link MINT-DataLab-GPT", style = "text-decoration: underline; color: #b16fab;
                margin-left: 60px;")
                )),
                column(
                  width = 8,
                  style = "margin-top: 40px",

                  p("Du willst mit DataLab-Daten argumentieren?"),
                  p("Mithilfe unseres Custom-GPTs kannst du schnell und einfach
                    ganze Berichte und Argumentationsketten erstellen"),

                  strong("Der Chatbot unterstützt konkret in drei Situationen:"),
                  tags$ol(
                    tags$li("Er interpretiert Daten grundlegend"),
                    tags$li("Er argumentiert für Ihr Fokusthema"),
                    tags$li("Er schreibt ganze Berichte"),
                  )
                )
              )
              ),



    #     column(
    #       width = 2,
    #       tags$span(#icon("1", style = "margin: 10px; font-size: 17px;"),
    #                 style = "font-weight: 600; font-size: 16px;
    #                 display: block; height: 70px; margin-bottom: 10px;",
    #                 "1. Wählen Sie eine Region für die Analyse aus."),
    #       tags$a(href="#region", img(src='www/gpt_schritt_1.png',
    #           class = "img-responsive",
    #           height = "150px",
    #           alt = "Symbol Schritt 1 Region wählen",
    #           style="display: block;
    #             margin-top: 20px; height: 200px; border: 2px solid #B16FAB;
    #             border-radius: 15px; text-align: left;"))
    #     ),
    #     column(
    #       width = 2,
    #       tags$span(#icon("2", style = "margin: 10px; font-size: 17px;"),
    #                 style = "font-weight: 600; font-size: 16px;
    #                 display: block; height: 70px; margin-bottom: 10px;",
    #                 "2. Laden Sie die Datengrundlage herunter."),
    #       tags$a(href="#download_section", img(src='www/gpt_schritt_2.png',
    #           class = "img-responsive",
    #           height = "150px",
    #           alt = "Symbol Schritt 2 Datendownload",
    #           style="display: block;
    #             margin-top: 20px; height: 200px; border: 2px solid #B16FAB;
    #             border-radius: 15px;"))
    #     ),
    #     column(
    #       width = 2,
    #       tags$span(#icon("3", style = "margin: 10px; font-size: 17px;"),
    #                 style = "font-weight: 600; font-size: 16px;
    #                 display: block; height: 70px; margin-bottom: 10px;",
    #                 "3. Wechseln Sie zum MINT-DataLab-GPT und folgen den Anweisungen."),
    #       tags$a(href="#MINT-DataLab-GPT", img(src='www/gpt_schritt_3.png',
    #           class = "img-responsive",
    #           height = "150px",
    #           alt = "Symbol Schritt 3 GPT-Chat",
    #           style="display: block;
    #             margin-top: 20px; height: 200px; border: 2px solid #B16FAB;
    #             border-radius: 15px;"))
    #     ),
    #     column(
    #       width = 2,
    #       tags$span(#icon("4", style = "margin: 10px; font-size: 17px;"),
    #                 style = "font-weight: 600; font-size: 16px;
    #                 display: block; height: 70px; margin-bottom: 10px;",
    #                 "4. Ergänzen Sie den KI-Bericht zur Veranschaulichung mit Grafiken."),
    #       tags$a(href="#grafiken", img(src='www/gpt_schritt_4.png',
    #           class = "img-responsive",
    #           height = "150px",
    #           alt = "Symbol Schritt 4 Grafiken ergänzen",
    #           style="display: block;
    #             margin-top: 20px; height: 200px; border: 2px solid #B16FAB;
    #             border-radius: 15px;"))
    #       ),
    #
    #     column(
    #       width = 12,
    #      # hr(style = "border-top: 2px solid #ee7775; margin-top: 30px; margin-bottom: 15px;")
    #     ),
    #
    #   )
    # ),


    ## 1. Region-Filter ----


      h2("In Fünf Schritten zur individuellen Datenanalyse",
         style = "margin-top: 30px;"),

    div(
      style = "display:flex; align-items:flex-start; margin-top:40px;",

      # Bild
      div(
        style = "margin: 0px 25px 20px 10px;",
        img(
          src = "www/gpt_schritt_1.png",
          class = "img-responsive",
          style = "
        display:block;
        margin-top:10px;
        border:2px solid #B16FAB;
        border-radius:15px;
        max-width:100px;
      "
        )
      ),

      # Text + Dropdown rechts
      div(

        p(
          strong("1. Wählen Sie eine Region für die Analyse aus."),
          style = "margin-bottom:10px;"
        ),

        shinyWidgets::pickerInput(
          inputId = ns("region_argumentationshilfe"),
          label = NULL,
          choices = c(
            "Deutschland",
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
          selected = "Deutschland",
          multiple = FALSE
        )
      )
    ),







  ##  2. Fokus-Switch ----

    column(
      width = 8,
      id = ns("ziel_col"),
      style = "margin-top: 40px;",



      div(
        id = "fokus-auswahl",

        p(
          strong("2. Legen Sie hier Ihren inhaltlichen Schwerpunkt fest."),
          style = "margin-top: 20px; margin-left: 60px"
        ),

        shinyWidgets::pickerInput(
          inputId = ns("frauen_fokus"),
          label = NULL,
          choices = c(
            "MINT-Nachwuchsförderung allgemein" = FALSE,
            "Mädchen- und Frauenförderung in MINT" = TRUE
          ),
          selected = "FALSE"
        ))),



      #  div(
      #   id = "fokus-auswahl",
      #
      #   p(strong("2. Legen Sie hier Ihren inhaltlichen Schwerpunkt fest."),
      #     style = "margin-top: 20px; margin-left: 60px"),
      #
      #   div(
      #     style = "display:flex; gap:20px; align-items:center; width:100%;
      #   margin-bottom: 40px; margin-top: 20px;",
      #     p(style="margin:0; flex:1; text-align:right;",
      #       "MINT-Nachwuchsförderung allgemein"),
      #
      #     div(
      #       style= "align-content: center; width: 60px;",
      #       shinyWidgets::materialSwitch(
      #         inputId = ns("frauen_fokus"),
      #         value = FALSE
      #       )
      #     ),
      #
      #     p(style="margin:0; flex:1; text-align:left; margin-right: 10px;",
      #       "Mädchen- und Frauenförderung in MINT")
      #   )
      # )),
      #


      ## 3. Daten-Download ----

      column(
        width = 12,
        style = "display: flex; align-items: center; margin-bottom: 15px;",
        div(
          style = "margin: 0px 25px 100px 0px;",
          img(src='www/gpt_schritt_2.png',
              class = "img-responsive",
              alt = "Bild Schritt 2 klein",
              style="display: block;
                  margin-top: 10px; border: 2px solid #B16FAB;
                  border-radius: 15px; max-width: 100px;")
        ),
        div(id = "download_section",
            style = "flex: 1; margin-bottom: 15px;",

            fluidRow(
              p(strong(style = "text-align: left; font-size: 18px; margin-left: 15px; margin-top: 20px",
                       "3. Laden Sie die Datengrundlage herunter.")),
              column(
                width = 3,
                div(style = "margin-left: 30px;",
                    downloadButton(style = "margin-top: 10px; margin-bottom: 5px;",
                                   outputId = ns("download_txt"),
                                   label = "   Daten herunterladen",
                                   class = "rosa-button")
                )
               ),

              column(
                width = 5,  # Text in der linken Spalte

                p(
                  "Als Basis für den Datenbericht sowie die datenbasierte Argumentation
              haben wir fünf Statistiken aus dem MINT-DataLab ausgewählt."),
                p("Das heruntergeladene txt.-Dokument Kopieren Sie in den Chat des MINT-DataLab-GPT oder
              hängen die Datei an.")

            )),

            fluidRow(
              column(
                width = 4,
                div(style = "margin-left: 30px;",
                    p(
                      tags$a(href = "#daten_grafiken",
                             style = "color: #000000; text-decoration: underline;",
                             "→ Die in dem Download enthaltenen Daten sind in den interaktiven Grafiken weiter unten auf dieser Seite dargestellt."))
                )
              )
            ))),

      ## 4. MINT-DataLab-GPT ----

      column(
        width = 12,
        style = "display: flex; align-items: center; ",
        div(
          style = "margin: 0px 25px 50px 0px;",
          img(src='www/gpt_schritt_3.png',
              class = "img-responsive",
              alt = "Bild Schritt 3 klein",
              style="display: block;
                  margin-top: 10px; border: 2px solid #B16FAB;
                  border-radius: 15px; max-width: 100px;")
        ),
        div(id = "MINT-DataLab-GPT",
            style = "flex: 1; margin-bottom: 15px;",
            fluidRow(
              p(strong(style = "text-align: left; font-size: 18px; margin-bottom: 15px; margin-left: 15px;",
                       "4. Wechseln Sie zum MINT-DataLab-GPT und folgen Sie den Anweisungen.")),
              column(
                width = 3,
                div(style = "margin-left: 30px;",
                    actionButton(label = tagList(icon("arrow-up-right-from-square"), "    Zum MINT-DataLab-GPT"), inputId = "GPT_link",
                                 onclick = 'window.open("https://chatgpt.com/g/g-695cd1fa74f881918a54b0517af8163e-mint-datalab-gpt", "_blank");')
                )
              ),
              column(
                width = 5,  # Text in der linken Spalte

                p(
                  "Sobald Sie auf den Link zum MINT-DataLab-GPT klicken, öffnet sich ein Chatfenster in ChatGPT.
                 Wählen Sie aus, ob sie eine Argumentation oder eine Bericht wollen,
                 der MINT-DataLab-GPT führt Sie durch die Erstellung der Analyse.")
              )

            )
        )
      ),



      ### Prompt-Bibliothek ----------
      column(
        width = 10,
        div(
          style = "margin-left:200px;margin-top:0px;margin-bottom: 30px; ",

          actionButton(
            style = "
              cursor:pointer;
              font-weight:600;
              padding:10px 15px;
              background-color:#F7EFF6;
              border:2px solid #B16FAB;
              border-radius:8px;
              display:inline-block;
              list-style:none;
              ",
            ns("funktionsprompt"),
            label = " Funktionsprompt für andere KIs",
           ),

          br(),

          tags$details(

            tags$summary(
              style = "
              cursor:pointer;
              font-weight:600;
              padding:10px 15px;
              background-color:#F7EFF6;
              border:2px solid #B16FAB;
              border-radius:8px;
              display:inline-block;
              list-style:none;
              ",
              "💡 Prompt-Vorschläge anzeigen"
            ),

            div(
              style = "
                  margin-top:10px;
                  padding:12px;
                  background-color:#F7EFF6;;
                  ",

            tags$table(
              class = "table table-bordered table-sm",
              style = "margin-top:0px;",

              tags$thead(
                tags$tr(
                  tags$th("Prompt"),
                  tags$th("Anwendung"),
                  tags$th("xx")
                )
              ),

              tags$tbody(
                tags$tr(
                  tags$td(
                    actionLink(
                      ns("prompt_bericht"),
                      "Bericht erstellen"
                    )
                  ),
                  tags$td("xx"),
                  tags$td("yy")
                )
              )
            )
          )
        )
      )),



      ## 5. Grafiken ----
      column(
        width = 12,
        style = "display: flex; align-items: center; margin-bottom: 20px;",
        div(
          style = "margin: 0px 25px 90px 0px;",
          img(src='www/gpt_schritt_4.png',
              class = "img-responsive",
              alt = "Bild Schritt 4 klein",
              style="display: block;
                  margin-top: 10px; border: 2px solid #B16FAB;
                  border-radius: 15px; max-width: 100px;")
        ),
        div(id = "grafiken",
            style = "flex: 1; margin-bottom: 15px;",
            fluidRow(
              p(strong(style = "text-align: left; font-size: 18px; margin-left: 15px;",
                       "5. Ergänzen Sie den KI-Bericht zur Veranschaulichung mit Grafiken.")),
              column(
                width = 3,
                div(style = "margin-left: 30px; margin-top: 10px;",
                    actionButton(
                      ns("download_all_png_client"),
                      label = tagList(icon("download"), "Alle Grafiken herunterladen (ZIP)",),
                      class = "rosa-button"),
                    )
                )
            ,
              column(
                width = 5,
                p(" Die passenden Grafiken zu dem Datenbündel können Sie her herunterladen
                  um Ihren Bericht damit fertigzustellen."),
                p(stlye="margin-left: 20px;",
                  "→ Falls Sie weitere Grafiken des MINT-DataLabs darstellen wollen, finden Sie eine Download-Option
                  immer rechts oben an den Grafiken.")
              )),



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
        ),

# Beispiel Bericht und Cheatsheet ----

column(
  id = "bsp_cheatsheet",
  width = 12,

  hr(style = "border-top: 2px solid #154194; margin-top: 40px; margin-bottom: 30px;"),
  h2("So könnte Ihr MINT-Bericht aussehen", style = "margin-bottom: 30px;"),

  div(
    style = "
      display: grid;
      grid-template-columns: 41.6667% 41.6667%;
      column-gap: 30px;
      row-gap: 0px;
      align-items: start;
    ",

    div(
      tags$strong("Beispielbericht für Hamburg:"),
      p("Hier können Sie exemplarisch sehen, wie man die Textbausteine und Grafiken
        zu einem Bericht integrieren kann. Außerdem finden Sie Tipps dazu,
        welche Daten darüber hinaus ergänzt werden können,
        um den Bericht abzurunden.")
    ),

    div(
      tags$strong("Cheatsheet Argumentation für Mädchenförderung"),
      p("Ihr Projekt fokussiert sich auf die Förderung von Mädchen in MINT?
        In diesem Cheatsheet finden Sie Tipps dazu, wie das MINT-DataLab-GPT Sie
        bei der Argumentation und Datenauswertung unterstützen kann.")
    ),

    div(
      tags$a(
        href = "www/MINTvernetzt_Argumentationskette_Hamburg.pdf",
        target = "_blank",
        tags$img(
          src = "www/Bild_Beispielbericht.png",
          alt = "Cover Beispielbericht Hamburg",
          style = "
            display:block;
            max-width: 15%;
            height: auto;
            cursor: pointer;
            margin: 10px 0px 10px 60px;
            border: 1px solid #EFE8E6;
          "
        )
      ),
      tags$a(
        href = "www/MINTvernetzt_Argumentationskette_Hamburg.pdf",
        target = "_blank",
        "MINTvernetzt_Argumentationskette_Hamburg.pdf",
        style = "text-decoration: underline; display: block;"
      )
    ),

    div(
      tags$a(
        href = "www/MINTvernetzt_Cheatsheet_MINT-DataLab-GPT_Maedchenfoerderung.pdf",
        target = "_blank",
        tags$img(
          src = "www/Cover_Cheatsheet.png",
          alt = "Cover Cheatsheet Maedchenfoerderung",
          style = "
            display:block;
            max-width: 15%;
            height: auto;
            cursor: pointer;
            margin: 10px 0px 10px 60px;
            border: 1px solid #EFE8E6;
          "
        )
      ),
      tags$a(
        href = "www/MINTvernetzt_Cheatsheet_MINT-DataLab-GPT_Maedchenfoerderung.pdf",
        target = "_blank",
        "MINTvernetzt_Cheatsheet_Maedchenfoerderung.pdf",
        style = "text-decoration: underline; display: block;"
      )
    )
  )
),


# Überleitung zu Grafiken ----
  column(
    id = "daten_grafiken",
    width = 12,
    hr(style = "border-top: 2px solid #154194; margin-top: 40px;"),

    h2("Die Datengrundlage Ihres MINT-Berichts als Grafiken", #So geht der MINT-DataLab-GPT bei der Analyse vor
       style= "margin-bottom: 30px; margin-top: 30px;"),

    column(
      style = "margin-bottom: 40px;",
      width = 8,
      p("Im Folgenden finden Sie die Daten, die Sie dem MINT-DataLab-GPT eingespeist haben,
        als Grafiken dargestellt. Diese können Sie herunterladen und Ihrem Bericht hinzufügen.", br(),
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

        "→ Die ", tags$span("grünen Boxen", style = "color: #007655;"),
        "unter den Grafiken zeigen beispielhaft, wie man anhand
          der Statistiken für die MINT-Bildungsförderung argumentieren kann."
      )
    )
  ),

    ## Grafik-Box einbinden ----

div(

   uiOutput(ns("grafiken_output"))

    ),

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

ui_mint_gesamt <- function(id){

  ns <- shiny::NS(id)

  ## Box MINT Zeitverlauf ----
div(

  fluidRow(id = "box1",
           shinydashboard::box(
             title = "Den regionalen Status-Quo analysieren.",
             width = 12,
             column(
               width = 9,
               p("Als Einstieg in eine Argumentation kann ein kurzer Überblick über die MINT-Strukturen
                   der eigenen Region geeignet sein. Hierfür können Sie selbst recherchieren oder den
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

                   shinycssloaders::withSpinner(htmlOutput(ns("plot_argument_verlauf_1")),
                                                color = "#154194"),

                   shinycssloaders::withSpinner(htmlOutput(ns("plot_argument_verlauf_2")),
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

             column(
               width = 12,

               column(
                 width = 3,
                 br(),
                 div(class = "content-box", #width: 320px;
                     style = "
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
                 div(class = "content-box", #width: 320px;
                     style = "
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
                 div(class = "content-box", #width: 320px;
                     style = "
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
             title = "Den nicht gedeckten Bedarf an Fachkräften verdeutlichen.",
             width = 12,
             column(
               width = 8,
               p("Der Bedarf an MINT-Fachkräften ist bundesweit hoch. Das zeigt z. B. die MINT-Fachkräftelücke
          aus dem MINT-Report des IW Köln. Für 2025 wird diese Lücke deutschlandweit auf rund 150.000 geschätzt.
          So viele MINT-Fachkräfte fehlen also deutschlandweit.
          Diese Zahl liegt für die einzelnen Bundesländer so nicht vor.", br(),

                 "Dafür kann die Engpassanalyse der Bundesagentur für Arbeit betrachtet werden. Sie zeigt das Ausmaß des
            akuten Fachkräfteengpasses. Die Zahlen unterstreichen, wie wichtig MINT-Förderung ist,
          um den Fachkräftemangel zu reduzieren.")
             ),
             br(),br(),
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
                 div(class = "content-box", #width: 320px;
                     style = "
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
                 div(class = "content-box", #width: 320px;
                     style = "
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
             title = "Demografische Zukunftstrends in die Argumentation integrieren.",
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
             br(),br(),
             column(
               width = 9,
               shiny::mainPanel(
                 width = 12,
                 shinycssloaders::withSpinner(plotly::plotlyOutput(ns("plot_argument_demografie")),
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
                 div(class = "content-box", #width: 320px;
                     style = "
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
             title = "Die aktuelle Nachwuchssituation analysieren.",
             width = 12,
             column(
               width = 8,
               p("Viele Faktoren werden zusammen kommen müssen, um die Fachkräftelage
            in MINT zu stabilisieren. Auch, weil der Bedarf an MINT-Kräften steigt.
            Ein Schlüssel ist, mehr MINT-Nachwuchs zu gewinnen, doch in vielen
            MINT-Bereichen steht es aktuell nicht gut um den Nachwuchs, wie die folgende Grafik zeigt.")
             ),
             br(),br(),
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
                 div(class = "content-box", #width: 320px;
                     style = "
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
                 div(class = "content-box", #width: 320px;
                     style = "
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
                 div(class = "content-box", #width: 320px;
                     style = "
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
             title = "Das Potenzial von Nachwuchsförderung herausstellen.",
             width = 12,
             column(
               width = 8,
               p("Um dem Fachkräftemangel im MINT-Bereich wirksam zu begegnen,
            sind verschiedene Maßnahmen nötig. Besonders entscheidend ist dabei
            die Förderung des MINT-Nachwuchses, aber auch
            die gezielte Unterstützung von Frauen.")
             ),
             br(),br(),
             column(
               width = 9,
               shiny::mainPanel(
                 width = 12,
                 shinycssloaders::withSpinner(plotly::plotlyOutput(ns("plot_argument_wirkhebel")),
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
                 div(class = "content-box", #width: 320px;
                     style = "
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
                 div(class = "content-box", #width: 320px;
                     style = "
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

           ))
)
}

ui_mint_frauen <- function(id){
  ns <- shiny::NS(id)

  ### Frauen entlang der Bildungskette ----
div(
  fluidRow(id="box_frauen1",
       htmltools::tagAppendAttributes(
           shinydashboard::box(

             title = "Den Frauenanteil entlang der Bildungskette analysieren.",
             width = 12,
             column(
               width = 9,
               p("Ein erster Überblick zeigt die Entwicklung des Frauenanteils in MINT von
                 Leistungskursen in der Schule über Studium und Ausbildung bis zum Beruf.", br(),
                 "So wird sichtbar, an welchen Stellen Mädchen und Frauen den MINT-Bereich
                 verlassen.")
             ),
             column(
               width = 9,

                shiny::mainPanel(
                  width = 12,
                  shinycssloaders::withSpinner(htmlOutput(ns("agrument_frauen_verlassen")),
                                               color = "#154194"),
                  shinyBS::bsPopover(id="h_arg_frauen_1", title = "",
                                     content = paste0("Anders als z. B. bei Studierenden wählen Schüler:innen mehrere Grund- und Leistungskurse. Um dennoch einen Anteil von &quotMINT&quot vs. &quotNicht-MINT&quot angeben zu können, nutzen wir die Kursbelegungszahlen der Schüler:innen.", "<br> <br> In den uns vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden. <br><br>Baden-Württemberg erfasst keine geschelchterspezifischen Kursbelegungszahlen von Schüler:innen."),
                                     placement = "top",
                                     trigger = "hover"),
                  tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_arg_frauen_1")
                )

             ),
             column(
               width = 12,

               column(
                 width = 3,
                 br(),
                 div(class = "content-box", #width: 320px;
                     style = "
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                     p(strong("Wenn die Balken von Schule zu Beruf abnehmen:")),
                     p("Mädchen kommen in der Schule mit MINT in Kontakt und wählen
                       MINT Kurse. Doch in weiteren Bildungs- und Berufswegen, verlassen sie MINT.
                       Frühe Berührungspunkte allein reichen oft nicht aus,
                       um Mädchen dauerhaft im MINT-Bereich zu halten.")
                 )
               ),
                 column(
                   width = 3,
                   br(),
                   div(class = "content-box", #width: 320px;
                       style = "
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                       p(strong("Ist der Frauenanteil geringer:")),
                       p("Der Anteil von Frauen und Männern, die einen MINT-Beruf wählen,
                       unterscheidet sich deutlich. Ein so deutlicher statistischer Unterschied weißt auf
                       strukturelle Einflüsse hin.")

                 )
               )
            )
         ), class = "frauen_box"

        )
    ),

  ### Frauen im Beruf ----
  fluidRow(id="beruf_frauen",
           htmltools::tagAppendAttributes(
           shinydashboard::box(
             title = "Strukturelle Einflüsse in der Fachwahl verdeutlichen.",
             width = 12,
             column(
               width = 9,
               p("Dass Frauen sich seltener beruflich für MINT-Bereiche entscheiden,
                 wird manchmal als individuelle Präferenz dargestellt. Das Ausmaß des
                 Unterschieds bei den Anteilen von Frauen und Männern in MINT-Berufen zeigt jedoch:
                 Hier wirken strukturelle Faktoren, die hinterfragt und verändert werden müssen.")
             ),
             column(
               width = 9,
               shiny::mainPanel(
                 width = 12,
                 shinycssloaders::withSpinner(htmlOutput(ns("argument_frauen_beruf")),
                                              color = "#154194"),


                 shinyBS::bsPopover(id = "h_arg_mint_2", title = "",
                                    content = paste0("Die Kategorisierung in MINT entspricht der Zuordnung durch die Bundesagentur für Arbeit. Beschäftigte werden nur als MINT klassifiziert, wenn sie einer so definierten MINT-Tätigkeit nachgehen. Der akademische Hintergrund, z. B. ein Studium in einem MINT-Fach, ist nicht ausschlaggebend. Weitere Infos dazu unter &quotDatenquellen und Hinweise&quot", "<br> <br> In den vorliegenden Daten wird nur zwischen &quotweiblich&quot und &quotmännlich&quot unterschieden.", "<br> <br> Durch Rundung der berechneten Werte kann es zu minimalen Abweichungen zwischen den Grafiken kommen."),
                                    placement = "top",
                                    trigger = "hover"),
                 tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_arg_mint_2")
               )

             ),
             column(
               width = 12,

               column(
                 width = 3,
                 br(),
                 div(class = "content-box", #width: 320px;
                     style = "
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                     p(strong("Wenn die Anteile der Frauen geringer sind:")),
                     p("Diese MINT-Berufe werden seltener von Frauen gewählt und bleiben männerdominiert.
                       Deutliche Unterschiede weißen auf starke soziale, stereotypebedingte Einflüsse hin,
                       die auf die Berufswahl einwirken.")
                 )
               )
             )

           ),
           class = "frauen_box"
           )
  ),

  ### Selbstkonzept in MINT ----

  fluidRow(id="schule_kompetenz",
           htmltools::tagAppendAttributes(
           shinydashboard::box(
             title = "Mögliche Gründe für den geringeren Anteil von Mädchen in MINT aufzeigen",

             width = 12,
             column(
               width = 9,
               p("Strukturelle Einflüsse darauf, warum Mädchen sich seltener für MINT entscheiden,
                 können anhaltende Stereotype, geringere Förderung oder
                 fehlende Rollenmodelle sein. An dieser Stelle hilft es, wissenschaftliche Erkenntnisse zu diesen Themen miteinzubeziehen.
                 Kurzanalysen dazu sind im GPT hinterlegt.
                 Weitere Infos finden sich ", tags$a(href = "https://www.mint-vernetzt.de/gender/",
                                            target = "_blank",
                                            "hier", style = "text-decoration: underline;"), " auf der MINTvernetzt Website. "),
               p("Ergänzend zeigen Statisiken zum fachlichen Selbstkonzept von Mädchen vs.
                 Jungen in MINT, wie unterschiedlich schon in der Mittelstufe die
                 eigenen MINT-Kompetenzen bewertet werden."),

               p()
             ),
             column(
               width = 9,
               shiny::mainPanel(
                 width = 12,
                 shinycssloaders::withSpinner(htmlOutput(ns("argument_frauen_selbstkonzept")),
                                              color = "#154194"),
                 shinyBS::bsPopover(id="h_arg_kompetenz_3", title = "",
                                    content = paste0("Das Interesse und die Einschätzung der eigenen Fähigkeiten (fachspezifisches Selbstkonzept) wurden durch mehrere Fragen auf einer Skala von 1 bis 4 erfasst. Es werden Gruppenmittelwerte berichtet.", "<br> <br> Gesamte realisierte Stichprobengröße:", "<br> 2024: 1.556 Schulen mit N = 48.279 Schüler:innen", "<br> 2021: 1.464 Schulen mit N = 26.844 Schüler:innen", "<br> 2016: 1.508 Schulen mit N = 29.259 Schüler:innen", "<br> 2011: 1.349 Schulen mit N = 27.081 Schüler:innen"),
                                    placement = "top",
                                    trigger = "hover"),
                 tags$a(paste0("Hinweis zu den Daten und zur Stichprobengröße"), icon("info-circle"), id = "h_arg_kompetenz_3")
               )

             ),
             column(
               width = 12,

               column(
                 width = 3,
                 br(),
                 div(class = "content-box", #width: 320px;
                     style = "
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                     p(strong("Ist der blaue Balken niedriger:")),
                     p("Mädchen trauen sich in diesen MINT-Fach systematisch
                       weniger zu. Das liegt nicht an unterschiedlichen Voraussetzungen,
                       sondern an stereotypen Erwartungen und gesellschaftlichen Einflüssen.")
                 )
               ),
               column(
                 width = 3,
                 br(),
                 div(class = "content-box", #width: 320px;
                     style = "
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                     p(strong("Ist der beige Balken ist höher:")),
                     p("Jungen trauen sich in diesem MINT-Fach systematisch
                       weniger zu. Auch sie können von stereotypen Erwartungen und gesellschaftlichen Einflüssen
                       negativ beeinflusst werden.")
                 )
               ),
               column(
                 width = 3,
                 br(),
                 div(class = "content-box", #width: 320px;
                     style = "
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                     p(strong("Sind die Balken etwa gleich:")),
                     p("In diesem MINT-Bereich zeigt sich ein Ziel der Mädchenförderung: Jungen
                       und Mädchen schätzen ihre Fähigkeiten ähnlich ein. Unterschiede entstehen
                       hier eher durch individuelle Interessen als durch strukturelle Einflüsse.")
                 )
               )
             )

           ), class = "frauen_box"
        )
  ),

  ### Fächerwahl von Frauen ----

  fluidRow(id="studium_frauen",
        htmltools::tagAppendAttributes(
           shinydashboard::box(
             title = "Zeigen, wo Mädchen und Frauen bereits in MINT vertreten sind.",

             width = 12,
             column(
               width = 8,
               p("Dass sich Mädchen und Frauen in MINT sehen und wohlfühlen können, zeigen
                 einzelne MINT-Bereiche bereits heute. In Biologie, Pharmazie oder Architektur
                 sind mehr Frauen als Männer vertreten. Solche Beispiele können verdeutlichen,
                 dass Veränderungen möglich sind, wenn Stereotype abgebaut werden und Rollenmodelle existieren."),

             ),
             column(
               width = 9,
               shiny::mainPanel(
                 width = 12,
                 shinycssloaders::withSpinner(htmlOutput(ns("argument_frauen_faecherwahl")),
                                              color = "#154194"),
                 p(),
                 p(style="font-size:12px;color:grey",
                   "Quelle der Daten: Destatis, 2024, auf Anfrage, eigene Berechnungen durch MINTvernetzt."),
                 shinyBS::bsPopover(id="h_arg_mint_2_fruen", title = "",
                                    content = paste0("In die Kategorie &quotStudienanfänger:innen (1. Fachsemester)&quot fallen alle Studierenden, die das betrachtete Studium aktuell im ersten Semester studieren. Hierbei werden z. B. auch Studierende mitgezählt, die einen Master beginnen oder in das betrachtete Fach hineingewechselt sind. <br> Unter &quotStudienanfänger:innen (1. Hochschulsemester)&quot nehmen wir nur die Personen in den Blick, die zum ersten mal ein Studium aufnehmen.", "<br><br>Für Studierende im Lehramt wird das belegte Hauptfach für die Kategorisierung in &quotMINT&quot oder &quotNicht-MINT&quot betrachtet.", "<br> <br> Durch Rundungen kann es zu minimalen Abbweichungen zwischen den Grafiken kommen.", "<br><br>Die Zahlen beziehen sich auf die eingeschriebenen Studierenden des Herbst-/Wintersemesters im betrachteten Jahr."),
                                    placement = "top",
                                    trigger = "hover"),
                 tags$a(paste0("Hinweis zu den Daten"), icon("info-circle"), id = "h_arg_mint_2_fruen")
                 )
             ),
             column(
               width = 12,
               column(
                 width = 3,
                 br(),
                 div(class = "content-box", #width: 320px;
                     style = "
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                     p(strong("Ist der Anteil unter 50 %:")),
                     p("Dieser MINT-Bereich ist weiterhin stark männerdominiert.
                       Ohne gezielte Förderung fehlen oft weibliche Rollenmodelle,
                       und bestehende Stereotype können sich weiter verfestigen.")
                 )
               ),
               column(
                 width = 3,
                 br(),
                 div(class = "content-box", #width: 320px;
                     style = "
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                     p(strong("Ist der Anteil über 50 %:")),
                     p("In diesem MINT-Bereich sind Frauen bereits stärker vertreten.
                       Solche Beispiele zeigen, dass Geschlechterunterschiede in MINT
                       sich je nach Fach unterscheiden. Etwa in Pharmazie spielt Gesundheit
                       und sozialer Nutzen eine große Rolle
                       - Arbeitsbereiche die häufig mit Frauen assoziiert werden.")
                 )
               )
             )
           ), class = "frauen_box"
        )
  ),

  ### Wirkhebel ----
  fluidRow(id = "box5",
         htmltools::tagAppendAttributes(
           shinydashboard::box(
             title = "Das Potenzial von Mädchenförderung hervorheben",

             width = 12,
             column(
               width = 8,
               p("Mädchen- und Frauenförderung in MINT trägt nicht nur zu mehr Chancengerechtigkeit und
               Perspektivenvielfalt bei. Sie ist auch mit Blick auf den Fachkräftemangel wichtig.
               Mädchenförderung ist ein zentraler Hebel, um künftig mehr MINT-Fachkräfte zu gewinnen."),
               p("Mehr Argumente zum Thema Fachkräftemangel und -entwicklung finden Sie hier,
                 wenn Sie oben den Fokus zu MINT-Nachwuchsförderung allgemein ändern.")
             ),
             column(
               width = 9,
               shiny::mainPanel(
                 width = 12,
                 shinycssloaders::withSpinner(htmlOutput(ns("argument_frauen_wirkhebel")),
                                              color = "#154194"),
                 shinyBS::bsPopover(id="erkl_wirkhebel_argument", title="",
                                    content = paste0("Gesamteffekt: Wirkung aller Hebel kombiniert.", br(),br(), "MINT-Nachwuchs fördern: Zunahme von MINT-Fachkräften unter 35 zwischen 2012 und 2022 setzt sich so in den nächsten Jahren fort.", br(),br(), "Mädchen- und Frauen-Förderung in MINT: Zunahme von weiblichen MINT-Fachkräften unter 35 zwischen 2012 und 2022 setzt sich so in den nächsten Jahren fort.", br(),br(), "Zuwanderung MINT-Fachkräfte: „Hohe Zuwanderung“-Szenario der 15. koordinierten Bevölkerungsvorausberechnung des Statistischen Bundesamts.", br(),br(), "Verbleib älterer MINT-Fachkräfte: Anteil an erwerbstätigen MINT-Fachkräften unter den 55-59-, 60-64-, und 65-69-Jährigen wächst weiterhin so an wie zwischen 2012-2022."),
                                    placement = "top",
                                    trigger = "hover"),
                 tags$a(paste0("Das bedeuten die Wirkhebel"), icon("info-circle"), id="erkl_wirkhebel_argument"),
                 br(),
                 shinyBS::bsPopover(id="h_arg_frauen_5", title = "",
                                    content = paste0("Weitere Informationen zu den Berechnungen des IW Köln im Auftrag von MINTvernetzt lassen sich auf der Seite \"MINT-Fachkräfte\" nachlesen."),
                                    placement = "top",
                                    trigger = "hover"),
                 tags$a(paste0("Methodenhinweis"), icon("info-circle"), id = "h_arg_frauen_5"),
                 br(),
                 shinyBS::bsPopover(id="i_arg_frauen_5", title = "",
                                    content = paste0("Spielen alle Wirkhebel zusammen, können bis 2037 1,4 Mio. zusätzliche MINT-Fachkräfte gewonnen werden. Der stärkste Hebel, mit rund +670.000 MINT-Fachkräften ist die Förderung des MINT-Nachwuchses."),
                                    placement = "top",
                                    trigger = "hover"),
                 tags$a(paste0("Interpretationshilfe"), icon("info-circle"), id = "i_arg_frauen_5")
               )
             ),
             column(
               width = 12,
               column(
                 width = 3,
                 br(),
                 div(class = "content-box", #width: 320px;
                     style = "
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                     p(strong("Mädchen und Frauen in MINT fördern")),
                     p("Mädchen- und Frauenförderung ist ein wichtiger Hebel, um
                     den Fachkräftemangel in MINT zu verringern.
                  Mehr junge Frauen, die sich beruflich für MINT entscheiden,
                  tragen außerdem zu diverseren Perspektiven in MINT bei und so zu
                  einer höheren Qualität in MINT-Forschung und -Entwicklung.")
                 )
               ),
               column(
                 width = 3,
                 br(),
                 div(class = "content-box", #width: 320px;
                     style = "
                              margin-left: 0;
                              background-color: #00a87a30;
                              border: 2px solid #00a87a;
                              color: #154194;
                              border-radius: 10px;",
                     p(strong("MINT-Nachwuchs Förderung")),
                     p("Der größte Hebel gegen den akuten Fachkräftemangel ist ein stärkerer
                       MINT-Nachwuchs. Ohne Nachwuchsförderung lässt sich die MINT-Lücke auch durch
                       Maßnahmen wie Zuwanderung allein nicht schließen. Da die Hälfte des
                       Nachwuchses Mädchen sind, spielt ihre Förderung dabei eine zentrale Rolle.")
                 )
               )
             )
           ), class = "frauen_box"
         )

  )

)

}



# Argumentation Server

mod_argumentation_server <- function(id){

  moduleServer( id, function(input, output, session){
    ns <- session$ns
   r <- reactiveValues(frauen_fokus = FALSE)

    observeEvent(input$region_argumentationshilfe, {
      r$region_argumentationshilfe <- input$region_argumentationshilfe
    })

    observeEvent(input$frauen_fokus, {
      r$frauen_fokus <- input$frauen_fokus
    }, ignoreInit = FALSE)


    ## Farb-Wechsel ----
    observeEvent(input$frauen_fokus, {
      if (as.logical(input$frauen_fokus)) {
        shinyjs::runjs(sprintf(
          "document.getElementById('%s').setAttribute('style', '%s');",
          ns("ziel_col"),
          "border: 1px solid #154194; border-radius: 15px; background-color: #15419420; margin-top: 40px;"
        ))
      } else {
        shinyjs::runjs(sprintf(
          "document.getElementById('%s').setAttribute('style', '%s');",
          ns("ziel_col"),
          "margin-top: 40px;"  # oder dein Default-Style
        ))
      }
    })


    ## Prompt-Bibliothek -------------


    observeEvent(input$funktionsprompt, {

      showModal(
        modalDialog(
          title = "Funktionsprompt",

          textAreaInput(
            ns("funktionsprompt"),
            label = NULL,
            value = "
# Rolle
Du bist eine Assistenz für das MINT-DataLab, die Plattform für MINT-Daten von MINTvernetzt. Du bist eine erfahrene Datenexpertin, deren Stärke darin liegt, Statistiken zu interpretieren und verständlich zu erklären.
# Aufgabe
Deine Aufgabe ist es, Nutzerinnen und Nutzer bei der Erstellung von datenbasierten Berichten oder Argumentationen zu MINT und MINT-Bildung zu unterstützen und bei der Dateninterpretation zu unterstützen.
# Arbeitsschritte
## Stelle dich vor
Wichtig: Zu Beginn des Chats - spreche immer folgende Punkte an:
-	Stelle dich als Assistenz des MINT-DataLab vor und heiße die Nutzer:innen willkommen.
-	Weise darauf hin, dass KI-generierte Inhalte fehlerhaft sein können und immer kritisch
              geprüft werden sollten.
-	Verlinke den Nutzungshinweis: https://mintvernetzt.shinyapps.io/datalab/_w_c6b6cc73299d4103a69c853cc9457f27/www/Nutzungshinweis_Haftungsausschluss_GPT.pdf
-	Weise klar darauf hin, dass dieser Hinweis mit weiterer Nutzung anerkannt wird.
## Daten für deine Arbeit anfragen
-	Bitte die Nutzer:innen darum, ihre Daten aus dem MINT-DataLab im Chat hochzuladen, damit du mit deiner Arbeit beginnen kannst.
-	Verweise auf das MINT-DataLab unter https://mint-vernetzt.shinyapps.io/datalab/ und speziell auf die Unterseite „Datenanalyse mit KI“, wo eine Vorauswahl an Daten direkt heruntergeladen werden kann. Warte, bis die Person geantwortet und Daten hochgeladen hat.
-	Beginne immer erst nach dem Datenupload mit der Analyse. Das gilt auch, wenn freie Anfragen gestellt werden. Das Vorstellen und der Verweis auf das MINT-DataLab sind immer der Start - dann kannst du antworten und analysieren. Du erkennst den Datenupload daran, dass ein txt-Text der Daten beinhaltet in den Chat kopiert wird oder txt/csv/excel-Dateien hochgeladen werden.
## Du hast drei konkrete Aufgaben, die du anhand der Stichworte erkennen kannst:
1.	Argumentation (Stichworte: Argumentation, Argumentationshilfe, argumentieren)
2.	Interpretation (Stichworte: Interpretation, interpretiere)
3.	Bericht (Stichworte: Bericht, berichten, Übersicht)
Details zu den drei Aufgaben:
1.	Argumentation
Deine Aufgabe ist es, basierend auf den Daten eine Argumentationskette zu entwickeln, die die Notwendigkeit von MINT-Bildungsförderung/MINT-Nachwuchsförderung unterstreicht. Leite für jede Statistik, die dir gegeben wird konkret Argumente ab. Betrachte die Daten auch im Zusammenspiel, um die Relevanz für Bildungsförderung aussagekräftig unterstreichen zu können. Mach das in Form eines Pitchs.
Nach deiner Antwort, biete den Nutzenden an, die Argumentation nachzuschärfen. Frage dafür nach ihrem Projekt- bzw. Themenschwerpunkt oder nach dem Anlass, für den sie die Argumentationskette verwenden wollen.
2.	Interpretation
Du interpretierst die Daten, die die Nutzer hochladen. Du erklärst, was die Daten bedeuten, was auffällt, wie sich Werte entwickelt haben (wenn sinnvoll), was es bedeutet, wenn sich Trends fortsetzen. Deine Aufgabe ist es, ein gutes Verständnis für Zahlen und Zusammenhänge zu ermöglichen. Formuliere deine Antwort als Fließtext.
3.	Bericht
Du erstellst auf Grundlage der hochgeladenen Daten einen sachlichen Bericht über den Status Quo der MINT-Situation. Ziel ist ein deskriptiver Überblick, keine Argumentation. Der Bericht enthält u.a. eine Übersicht über die aktuelle Situation, Bedeutung von MINT für das Bundesland oder die Region, Herausforderungen, Entwicklungen und Nachwuchsprobleme, Zukunftsausblick, Fazit und Quellen.
Du schreibst diesen Bericht als Fließtext – ohne Tabellen oder Visualisierungen. Verweise darauf, dass Nutzer im MINT-DataLab Grafiken herunterladen können. Schreibe Berichte ausschließlich auf Basis der vom Nutzer bereitgestellten Daten. Füge am Ende des Berichts folgenden Hinweistext ein:
„Erstellt unter Verwendung des angepassten GPT-Sprachmodells von MINTvernetzt (MINT-DataLab-GPT) auf Basis von OpenAI-Technologie.“
Vertiefe deine erste Antwort danach iterativ mit der Nutzer:in. Frage dafür:
- Sind das alle Daten oder sollen weitere Daten ergänzt werden?
- Schlage mögliche Daten oder Themen zur Vertiefung vor. Verweise darauf, dass es im MINT-DataLab weitere Daten und Kurzanalysen zu verschiedenen Themen (MINT-Fachkräfte, Frauen in MINT, Oberstufenbelegungen, und vieles mehr) gibt.
- Frage nach dem konkreten Anlass für den Bericht, wie z.B. einen Förderantrag?
- Weise außerdem darauf hin, dass die passenden Grafiken im MINT-DataLab auf der -Datenanlyse mit KI- Unterseite gesammelt heruntergeladen und ergänzt werden können.
# Regeln für das Bearbeiten der Aufgaben:
## Für die Bearbeitung aller Aufgaben gilt:
-	Erstelle niemals automatisch Tabellen oder andere Visualisierungen.
-	Halluziniere nicht. Stelle niemals generierte, spekulative oder vermutete Inhalte als Fakten dar. Wenn du etwas nicht überprüfen kannst, sage: „Ich kann das nicht überprüfen.“ „Ich habe keinen Zugriff auf diese Informationen.“ etc. Kennzeichne unbestätigte Inhalte mit [Vermutung] oder [Nicht verifiziert]. Bitte um Klarstellung, statt Lücken zu füllen („Könntest du das genauer erläutern?“). Wenn ein Teil nicht verifiziert ist, kennzeichne die gesamte Antwort als unbestätigt. Falls du dagegen verstößt, korrigiere dich: „Korrektur: Ich habe zuvor eine unbestätigte Behauptung aufgestellt“.
-	Gib keine rechtlichen, finanziellen, medizinischen oder wirtschaftlichen Ratschläge.
-	Halte dich kurz und folge in deiner Sprache dem PDF „Sprachleitfaden“ aus deinem Wissen.
-	Nutze als weitere Informationsgrundlage für die Aufgaben die „Kurzanalysen“ aus deinem Wissen.
-	Gib immer Quellen an. Sind Quellenangaben bei hochgeladenen Daten dabei, müssen diese Quellenangaben wörtlich übernommen werden.
-	Wenn du externe Onlinequellen verwendest, gib sie vollständig inklusive Link an.
-	Ordne alle Quellen den passenden Stellen im Text zu, und kennzeichne sie wie folgt: (1), (2), etc. Gib am Ende immer ein vollständiges Quellenverzeichnis an.
-	Füge am Ende jeder Aufgabe ein Quellenverzeichnis mit folgenden Hinweistext ein: „Erstellt unter Verwendung des angepassten GPT-Sprachmodells von MINTvernetzt (MINT-DataLab-GPT) auf Basis von OpenAI-Technologie.“
-	Werden PDFs als Zusatzquellen hochgeladen, zitiere auch diese Quelle entsprechend.
## Beispiele für Quellenangaben:
– „Destatis, 2024, eigene Berechnungen durch MINTvernetzt“. (Die txt und csv Files aus dem MINT-DataLab enthalten Quellen. Bei einem Datenupload mit mehreren, verschiedenen Datentabellen unterscheiden sich die Quellen, gebe immer die korrekte Quelle der einzelnen Daten an.)
– „MINTvernetzt: Kurzanalyse ‚Frauen in MINT-Berufen‘, 2024“ (Als Beispiel für eine Kurzanalyse aus dem MINT-DataLab)
",
            width = "100%",
            height = "600px"
          ),

          easyClose = TRUE,
          size = "xl"
        )
      )

    })


    observeEvent(input$prompt_bericht, {

      showModal(
        modalDialog(

          title = "Prompt: Bericht erstellen",

          textAreaInput(
            ns("bericht_prompt"),
            label = NULL,
            value = "xxxxxxx",        # hier prompt reinschreiben
            width = "100%",
            height = "300px"
          ),

          easyClose = TRUE,
          size = "l"
        )
      )

    })


    ## Download der gesammelten Daten ----

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

    ## Grafiken MINT allgemein ----

    # Texte/Rahmen rendern
    observeEvent(input$frauen_fokus, {

      output$grafiken_output <- renderUI({

        if (as.logical(input$frauen_fokus)) {

          ui_mint_frauen(id = id)
        } else {
          ui_mint_gesamt(id = id)

        }
      })

      # if(input$frauen_fokus == FALSE){
      #   output$grafiken_output <- ui_mint_gesamt(ns)
      # }else{
      #   output$grafiken_output <- ui_mint_frauen(ns)
      # }

    }, ignoreInit = FALSE)


    ### Grafik-Outputs rendern - allgemein ----

    output$plot_argument_verlauf_1 <- renderUI({

      argument_verlauf_1(r)
    })

    output$plot_argument_verlauf_2 <- renderUI({

      argument_verlauf_2(r)
    })

    output$plot_argument_fachkraft <- renderUI({

      plots <- argument_fachkraft(r)

      fluidRow(
        column(
          width = 6,
          plots[1]
        ),
        column(
          width = 6,
          plots[2]
        )
      )
    })


    output$plot_argument_demografie <- plotly::renderPlotly({
      argument_demografie(r)
    })

    output$plot_argument_nachwuchs <- renderUI({
      argument_nachwuchs(r)
    })

    output$plot_argument_wirkhebel <- plotly::renderPlotly({
      argument_wirkhebel(r)
    })

    ### Grafiken render Frauen ----

    output$agrument_frauen_verlassen <- renderUI({
      argument_frauen_bildungskette(r)
    })


    output$argument_frauen_beruf <- renderUI({
      plots <- argument_großer_unterschied(r)
      fluidRow(
        column(
          width = 6,
          plots[1]
        ),
        column(
          width = 6,
          plots[2]
        )
      )
    })



    output$argument_frauen_selbstkonzept <- renderUI({
      argument_selbstkonzept(r)
    })

    output$argument_frauen_faecherwahl <- renderUI({
      argument_faecherverteilung(r)
    })

    output$argument_frauen_wirkhebel <- renderUI({
      argument_wirkhebel(r)
    })



  })
}

