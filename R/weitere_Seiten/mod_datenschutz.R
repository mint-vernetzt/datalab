#' datenschutz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_datenschutz_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' kontakt Server Functions
#'
#' @noRd
mod_datenschutz_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

mod_datenschutz_ui <- function(id){
  ns <- NS(id)
  tagList(

    # Banner
    # fluidRow(
    #   shinydashboard::box(
    #     width = 12,
    #     img(src='www/Banner_Kontakt.jpg',
    #         class = "img-responsive",
    #         #height = "150px", width = "150px",
    #         alt = "Banner Schule",
    #         style="display: block; margin-left: auto; margin-right: auto;"
    #     ))),



    fluidRow(
      shinydashboard::box(
        width = 12,
        tags$h2("Datenschutzerklärung"),
        column(width = 12,

               div(style="text-align:justify", p("

MINTvernetzt ist die Service- und Anlaufstelle für die Community der MINT-Akteur:innen in Deutschland.
Diese werden durch Vernetzungsräume, Transferangebote und Innovationsimpulse unterstützt, innovative und
nachhaltige MINT-Bildungsangebote zu machen, die noch breitere und diversere Zielgruppen ansprechen.
Hierzu zählen insbesondere Mädchen und junge Frauen. MINTvernetzt wird vom Bundesministerium für Bildung
und Forschung (BMBF) gefördert und von Mitarbeitenden der Körber-Stiftung, der matrix gGmbH, dem Nationalen
MINT Forum e.V., der Universität Regensburg und dem Stifterverband gemeinsam umgesetzt.

Das DataLab dient in diesem Zusammenhang als Informationsplattform für Interessierte; Akteur:innen und
Beteiligte für amtliche Statistiken sowie durch MINTvernetzt erhobene Umfrageergebnisse aus dem Bereich
MINT-Bildung.",

br(),
br(),

tags$b(span("Verantwortlicher und Datenschutzbeauftragter")),

br(),


"Verantwortlich für die Verarbeitung von personenbezogenen Daten über die Plattform ist der Stifterverband für die Deutsche Wissenschaft e.V.:",

br(),

"Dr. Volker Meyer-Guckel",

br(),

"Baedekerstraße 1",

br(),

"45128 Essen",

br(),
br(),

"Der Datenschutzbeauftragte des Verantwortlichen ist:",

br(),

"TÜV Informationstechnik GmbH",

br(),

"Unternehmensgruppe TÜV NORD",

br(),

"IT Security, Business Security & Privacy",

br(),

"Langemarckstraße 20",

br(),

"45141 Essen",

br(),

"Deutschland",

br(),

"T 0201 8999-461",

br(),

"F 0201 8999-666",

br(),

"E-Mail: privacyguard@tuvit.de"),


br(),
br(),


p(tags$b(span("I. Zwecke der Verarbeitung")),

br(),
br(),

tags$b(span("1. Aufruf und Nutzung des Datalabs")),

       br(),

"Wir erfassen personenbezogene Daten von den Webseitenbesucher:innen, um die Webseite funktionsfähig zu halten. Hierzu gehört das Erheben der IP-Adresse, Logdaten, etc. Die Rechtsgrundlage ist insoweit Art. 6 Abs. 1 lit. b, f und e DSGVO (bzw. § 28 Abs. 1 S. 1 Nr. 1 BDSG).

Die von uns verfolgten berechtigten Interessen liegen in der Bereitstellung eines Datalab mit zeitgemäßen Informationsmöglichkeiten der Nutzer:innen mit untereinander für die oben genannten Ziele und Zwecke.

Darüber hinaus erheben und verarbeiten wir Informationen über die Nutzung des Datalabs, so z.B. den verwendeten Browsertyp sowie Datum und Uhrzeit der Plattform-Nutzungen. Wir verarbeiten diese Daten zur Optimierung der Plattform. Die Rechtsgrundlage ist Art. 6 Abs. 1 lit. f und e DSGVO (bzw. § 28 Abs. 1 S. 1 Nr. 2 BDSG). Unser berechtigtes Interesse an dieser Datenverarbeitung liegt dabei in unserem Bedürfnis, Ihnen eine bedarfsgerecht gestaltete und an die verwendeten Endgeräte optimierte Plattform zur Verfügung zu stellen."),

br(),
br(),

tags$b(span("2. Analyse durch Matomo")),
br(),

"Wir nutzen den Webanalyse-Dienst Matomo zur Analyse der Nutzung unserer Webseite und Ausgestaltung unseres Online-Angebots. Matomo ist ein Dienst der InnoCraft Ltd., 150 Willis St, 6011 Wellington, Neuseeland.

Dabei verwenden wir standardmäßig keine Cookies für die Web-Analyse und setzen kein Device-Fingerprinting ein. Durch den Aufruf des Analysescripts sendet der auf Deinem Gerät verwendete Browser automatisch Informationen an unseren Webseiten-Server. Dort werden die folgenden Informationen über Dich temporär in sogenannten Logfiles gespeichert. Hieraus lassen sich u.a. Angaben zur Besucheranzahl, Browser, Geräten und Betriebssystem erzeugen. Deine IP-Adresse wird hierbei anonymisiert.

Die Zulässigkeit der Verarbeitung richtet sich nach Art. 6 Abs. 1 f) DS-GVO (berechtigtes Interesse). Unsere berechtigten Interessen liegen in der Verbesserung und Optimierung unseres Online-Angebots sowie unseres Internetauftritts.

Weitere Informationen zu den Nutzungsbedingungen und den Datenschutzbestimmungen bei Matomo erhältst Du direkt bei Matomo.",

HTML(
  '<div id="matomo-opt-out"></div>
  <script src="https://analytics.datalab.mint-vernetzt.de/index.php?module=CoreAdminHome&action=optOutJS&divId=matomo-opt-out&language=auto&showIntro=1"></script>'
),


br(),
br(),


p(tags$b(span("3. Feedbackformular")),

br(),

"Wir nutzen zur Durchführung von Onlineumfragen auf unserer Informationsplattform das Tool „LamaPoll“ der Lamano GmbH & Co. KG, Frankfurter Allee 69, 10247 Berlin. Durch die Nutzung des Tools werden verschiedene Daten, unter anderem mit Personenbezug, (z.B. IP-Adresse, Logfiles etc.) erhoben und sog. Session-Cookies auf Ihrem Endgerät gespeichert.

Die Cookies werden nur gespeichert, um die jeweilige Umfrage richtig darzustellen und Ihnen zum Beispiel ein Vor- und Zurückklicken innerhalb der Umfrage zu ermöglichen. Rechtsgrundlage für die Speicherung der Cookies auf Ihrem Endgerät ist § 25 Abs. 2 TTDSDG. Die Cookies werden nach der Nutzung wieder gelöscht.

Die Rechtsgrundlage für die Datenverarbeitung ist der Art. 6 Abs. 1 f) DS-GVO (berechtigtes Interesse). Unser berechtigtes Interesse liegt dabei in unserem Bedürfnis, unsere Angebote stetig zu verbessern und auf den Bedarf unserer Netzwerkmitglieder genau zuschneiden zu können. Diese Daten werden durch die Lamano GmbH & Co. KG für die Dauer von höchstens 14 Tagen aufbewahrt. Eine weitere Speicherung oder Aufbewahrung durch uns findet nicht statt.

Weitere Informationen zu der Verarbeitung der Daten und Speicherung durch die Lamano GmbH und Co. KG finden Sie unter:", tags$a(href="https://www.lamapoll.de/Support/Datenschutz", "Lamapoll, Datenschutz", target = "_blank")),

br(),
br(),

p(tags$b(span("II. Datenlöschung und Speicherdauer")),

br(),

"Die personenbezogenen Daten der betroffenen Person werden gelöscht oder gesperrt, sobald der Zweck der Speicherung entfällt oder eine Einwilligung widerrufen wurde. Eine Speicherung kann darüber hinaus dann erfolgen, wenn dies durch den europäischen oder nationalen Gesetzgeber in unionsrechtlichen Verordnungen, Gesetzen oder sonstigen Vorschriften, denen der Verantwortliche unterliegt, vorgesehen wurde. Eine Sperrung oder Löschung der Daten erfolgt auch dann, wenn eine durch die genannten Normen vorgeschriebene Speicherfrist abläuft, es sei denn, dass eine Erforderlichkeit zur weiteren Speicherung der Daten für einen Vertragsabschluss oder eine Vertragserfüllung besteht."),

br(),
br(),


p(tags$b(span("III. Rechte der betroffenen Person")),

br(),

"Werden personenbezogene Daten von Ihnen verarbeitet, sind Sie Betroffener i.S.d. DSGVO, und es stehen Ihnen folgende Rechte gegenüber dem Verantwortlichen zu.",

br(),
br(),

tags$b(span("1. Auskunftsrecht")),

br(),

"Sie können von dem Verantwortlichen eine Bestätigung darüber verlangen, ob personenbezogene Daten, die Sie betreffen, von uns verarbeitet werden. Liegt eine solche Verarbeitung vor, können Sie von dem Verantwortlichen über folgende Informationen Auskunft verlangen:",

br(),
br(),

"(1) die Zwecke, zu denen die personenbezogenen Daten verarbeitet werden;",
br(),

"(2) die Kategorien von personenbezogenen Daten, welche verarbeitet werden;",
br(),

"(3) die Empfänger bzw. die Kategorien von Empfängern, gegenüber denen die Sie betreffenden personenbezogenen Daten offengelegt wurden oder noch offengelegt werden;",
br(),

"(4) die geplante Dauer der Speicherung der Sie betreffenden personenbezogenen Daten oder, falls konkrete Angaben hierzu nicht möglich sind, Kriterien für die Festlegung der Speicherdauer;",
br(),

"(5) das Bestehen eines Rechts auf Berichtigung oder Löschung der Sie betreffenden personenbezogenen Daten, eines Rechts auf Einschränkung der Verarbeitung durch den Verantwortlichen oder eines Widerspruchsrechts gegen diese Verarbeitung;",
br(),

"(6) das Bestehen eines Beschwerderechts bei einer Aufsichtsbehörde;",
br(),

"(7) alle verfügbaren Informationen über die Herkunft der Daten, wenn die personenbezogenen Daten nicht bei der betroffenen Person erhoben werden;",
br(),

"(8) das Bestehen einer automatisierten Entscheidungsfindung einschließlich Profiling gemäß Art. 22 Abs. 1 und 4 DSGVO und – zumindest in diesen Fällen – aussagekräftige Informationen über die involvierte Logik sowie die Tragweite und die angestrebten Auswirkungen einer derartigen Verarbeitung für die betroffene Person.",
br(),
br(),

"Ihnen steht das Recht zu, Auskunft darüber zu verlangen, ob die Sie betreffenden personenbezogenen Daten in ein Drittland oder an eine internationale Organisation übermittelt werden. In diesem Zusammenhang können Sie verlangen, über die geeigneten Garantien gem. Art. 46 DSGVO im Zusammenhang mit der Übermittlung unterrichtet zu werden.",
br(),
br(),

tags$b(span("2. Recht auf Berichtigung")),

br(),

"Sie haben ein Recht auf Berichtigung und/oder Vervollständigung gegenüber dem Verantwortlichen, sofern die verarbeiteten personenbezogenen Daten, die Sie betreffen, unrichtig oder unvollständig sind. Der Verantwortliche hat die Berichtigung unverzüglich vorzunehmen.",
br(),
br(),

tags$b(span("3. Recht auf Einschränkung der Verarbeitung")),
br(),

"Unter den folgenden Voraussetzungen können Sie die Einschränkung der Verarbeitung der Sie betreffenden personenbezogenen Daten verlangen:",
br(),
br(),

"(1) wenn Sie die Richtigkeit der Sie betreffenden personenbezogenen für eine Dauer bestreiten, die es dem Verantwortlichen ermöglicht, die Richtigkeit der personenbezogenen Daten zu überprüfen;",
br(),

"(2) die Verarbeitung unrechtmäßig ist und Sie die Löschung der personenbezogenen Daten ablehnen und stattdessen die Einschränkung der Nutzung der personenbezogenen Daten verlangen;",
br(),

"(3) der Verantwortliche die personenbezogenen Daten für die Zwecke der Verarbeitung nicht länger benötigt, Sie diese jedoch zur Geltendmachung, Ausübung oder Verteidigung von Rechtsansprüchen benötigen, oder",
br(),

"(4) wenn Sie Widerspruch gegen die Verarbeitung gemäß Art. 21 Abs. 1 DSGVO eingelegt haben und noch nicht feststeht, ob die berechtigten Gründe des Verantwortlichen gegenüber Ihren Gründen überwiegen.",
br(),

"
Wurde die Verarbeitung der Sie betreffenden personenbezogenen Daten eingeschränkt, dürfen diese Daten – von ihrer Speicherung abgesehen – nur mit Ihrer Einwilligung oder zur Geltendmachung, Ausübung oder Verteidigung von Rechtsansprüchen oder zum Schutz der Rechte einer
anderen natürlichen oder juristischen Person oder aus Gründen eines wichtigen öffentlichen Interesses der Union oder eines Mitgliedstaats verarbeitet werden. Wurde die Einschränkung der Verarbeitung nach den o.g. Voraussetzungen eingeschränkt, werden Sie von dem Verantwortlichen
unterrichtet bevor die Einschränkung aufgehoben wird.",
br(),
br(),


tags$b(span("4. Recht auf Löschung")),

br(),
br(),

tags$b(span("Löschungspflicht")),
br(),

"Sie können von dem Verantwortlichen verlangen, dass die Sie betreffenden personenbezogenen Daten unverzüglich gelöscht werden, und der Verantwortliche ist verpflichtet, diese Daten unverzüglich zu löschen, sofern einer der folgenden Gründe zutrifft:",

br(),
br(),

"(1) Die Sie betreffenden personenbezogenen Daten sind für die Zwecke, für die sie erhoben oder auf sonstige Weise verarbeitet wurden, nicht mehr notwendig.",
br(),

"(2) Sie widerrufen Ihre Einwilligung, auf die sich die Verarbeitung gem. Art. 6 Abs. 1 lit. a oder Art. 9 Abs. 2 lit. a DSGVO stützte, und es fehlt an einer anderweitigen Rechtsgrundlage für die Verarbeitung.",
br(),

"(3) Sie legen gem. Art. 21 Abs. 1 DSGVO Widerspruch gegen die Verarbeitung ein und es liegen keine vorrangigen berechtigten Gründe für die Verarbeitung vor, oder Sie legen gem. Art. 21 Abs. 2 DSGVO Widerspruch gegen die Verarbeitung ein.",
br(),

"(4) Die Sie betreffenden personenbezogenen Daten wurden unrechtmäßig verarbeitet.",
br(),

"(5) Die Löschung der Sie betreffenden personenbezogenen Daten ist zur Erfüllung einer rechtlichen Verpflichtung nach dem Unionsrecht oder dem Recht der Mitgliedstaaten erforderlich, dem der Verantwortliche unterliegt.",
br(),

"(6) Die Sie betreffenden personenbezogenen Daten wurden in Bezug auf angebotene Dienste der Informationsgesellschaft gemäß Art. 8 Abs. 1 DSGVO erhoben.",
br(),
br(),

tags$b(span("Information an Dritte")),
br(),

"Hat der Verantwortliche die Sie betreffenden personenbezogenen Daten öffentlich gemacht und ist er gem.
Art. 17 Abs. 1 DSGVO zu deren Löschung verpflichtet, so trifft er unter Berücksichtigung der verfügbaren Technologie
und der Implementierungskosten angemessene Maßnahmen, auch technischer Art, um für die Datenverarbeitung Verantwortliche,
die die personenbezogenen Daten verarbeiten, darüber zu informieren, dass Sie als betroffene Person von ihnen die Löschung
aller Links zu diesen personenbezogenen Daten oder von Kopien oder Replikationen dieser personenbezogenen Daten verlangt haben.",

br(),
br(),


tags$b(span("Ausnahmen")),
br(),

"Das Recht auf Löschung besteht nicht, soweit die Verarbeitung erforderlich ist",
br(),
br(),

"(1) zur Ausübung des Rechts auf freie Meinungsäußerung und Information;",
br(),

"(2) zur Erfüllung einer rechtlichen Verpflichtung, die die Verarbeitung nach dem Recht der Union oder der Mitgliedstaaten,
dem der Verantwortliche unterliegt, erfordert, oder zur Wahrnehmung einer Aufgabe, die im öffentlichen Interesse liegt oder
in Ausübung öffentlicher Gewalt erfolgt, die dem Verantwortlichen übertragen wurde;",
br(),

"(3) für im öffentlichen Interesse liegende Archivzwecke, wissenschaftliche oder historische Forschungszwecke oder für
statistische Zwecke gem. Art. 89 Abs. 1 DSGVO, soweit das unter Abschnitt a) genannte Recht voraussichtlich die Verwirklichung
der Ziele dieser Verarbeitung unmöglich macht oder ernsthaft beeinträchtigt, oder",
br(),

"(4) zur Geltendmachung, Ausübung oder Verteidigung von Rechtsansprüchen.",

br(),
br(),

tags$b(span("5. Recht auf Unterrichtung")),
br(),

"Haben Sie das Recht auf Berichtigung, Löschung oder Einschränkung der Verarbeitung gegenüber dem Verantwortlichen geltend gemacht,
ist dieser verpflichtet, allen Empfängern, denen die Sie betreffenden personenbezogenen Daten offengelegt wurden, diese Berichtigung
oder Löschung der Daten oder Einschränkung der Verarbeitung mitzuteilen, es sei denn, dies erweist sich als unmöglich oder ist mit
einem unverhältnismäßigen Aufwand verbunden. Ihnen steht gegenüber dem Verantwortlichen das Recht zu, über diese Empfänger
unterrichtet zu werden.",

br(),
br(),



tags$b(span("6. Recht auf Datenübertragbarkeit")),
br(),

"Sie haben das Recht, die Sie betreffenden personenbezogenen Daten, die Sie dem Verantwortlichen bereitgestellt haben,
in einem strukturierten, gängigen und maschinenlesbaren Format zu erhalten. Außerdem haben Sie das Recht, diese Daten
einem anderen Verantwortlichen ohne Behinderung durch den Verantwortlichen, dem die personenbezogenen Daten bereitgestellt
wurden, zu übermitteln, sofern",

br(),
br(),

"(1) die Verarbeitung auf einer Einwilligung gem. Art. 6 Abs. 1 lit. a DSGVO oder Art. 9 Abs. 2 lit. a DSGVO oder auf einem Vertrag gem. Art. 6 Abs. 1 lit. b DSGVO beruht und",
br(),

"(2) die Verarbeitung mithilfe automatisierter Verfahren erfolgt.",
br(),
br(),

"In Ausübung dieses Rechts haben Sie ferner das Recht, zu erwirken, dass die Sie betreffenden personenbezogenen Daten
direkt von einem Verantwortlichen einem anderen Verantwortlichen übermittelt werden, soweit dies technisch machbar ist.
Freiheiten und Rechte anderer Personen dürfen hierdurch nicht beeinträchtigt werden. Das Recht auf Datenübertragbarkeit
gilt nicht für eine Verarbeitung personenbezogener Daten, die für die Wahrnehmung einer Aufgabe erforderlich ist, die im
öffentlichen Interesse liegt oder in Ausübung öffentlicher Gewalt erfolgt, die dem Verantwortlichen übertragen wurde.",

br(),
br(),


tags$b(span("7. Widerspruchsrecht")),

br(),

"Sie haben das Recht, aus Gründen, die sich aus ihrer besonderen Situation ergeben, jederzeit gegen die
Verarbeitung der Sie betreffenden personenbezogenen Daten, die aufgrund von Art. 6 Abs. 1 lit. e oder f
DSGVO erfolgt, Widerspruch einzulegen; dies gilt auch für ein auf diese Bestimmungen gestütztes Profiling.
Der Verantwortliche verarbeitet die Sie betreffenden personenbezogenen Daten nicht mehr, es sei denn, er kann
zwingende schutzwürdige Gründe für die Verarbeitung nachweisen, die Ihre Interessen, Rechte und Freiheiten überwiegen,
oder die Verarbeitung dient der Geltendmachung, Ausübung oder Verteidigung von Rechtsansprüchen. Werden die Sie betreffenden
personenbezogenen Daten verarbeitet, um Direktwerbung zu betreiben, haben Sie das Recht, jederzeit Widerspruch gegen die Verarbeitung
der Sie betreffenden personenbezogenen Daten zum Zwecke derartiger Werbung einzulegen; dies gilt auch für das Profiling,
soweit es mit solcher Direktwerbung in Verbindung steht. Widersprechen Sie der Verarbeitung für Zwecke der Direktwerbung,
so werden die Sie betreffenden personenbezogenen Daten nicht mehr für diese Zwecke verarbeitet. Sie haben die Möglichkeit,
im Zusammenhang mit der Nutzung von Diensten der Informationsgesellschaft – ungeachtet der Richtlinie 2002/58/EG –
hr Widerspruchsrecht mittels automatisierter Verfahren auszuüben, bei denen technische Spezifikationen verwendet werden.",
br(),
br(),




tags$b(span("8. Recht auf Widerruf der datenschutzrechtlichen Einwilligungserklärung")),
br(),

"Sie haben das Recht, Ihre datenschutzrechtliche Einwilligungserklärung jederzeit zu widerrufen.
Durch den Widerruf der Einwilligung wird die Rechtmäßigkeit der aufgrund der Einwilligung bis zum Widerruf
erfolgten Verarbeitung nicht berührt.",

br(),
br(),



tags$b(span("9. Recht auf Beschwerde bei einer Aufsichtsbehörde")),
br(),

"Unbeschadet eines anderweitigen verwaltungsrechtlichen oder gerichtlichen
Rechtsbehelfs steht Ihnen das Recht auf Beschwerde bei einer Aufsichtsbehörde,
insbesondere in dem Mitgliedstaat ihres Aufenthaltsorts, ihres Arbeitsplatzes oder des
Orts des mutmaßlichen Verstoßes, zu, wenn Sie der Ansicht sind, dass die Verarbeitung der
Sie betreffenden personenbezogenen Daten gegen die DSGVO verstößt. Die Aufsichtsbehörde,
bei der die Beschwerde eingereicht wurde, unterrichtet den Beschwerdeführer über den Stand und
die Ergebnisse der Beschwerde einschließlich der Möglichkeit eines gerichtlichen Rechtsbehelfs
nach Art. 78 DSGVO."),
br(),
br(),



p(tags$b(span("IV. Empfänger")),
br(),

"Die bei Aufruf und Nutzung der Plattform erhobenen Daten und die individuell bereitgestellten Angaben werden an unsere Server
(diese werden gegebenenfalls bei Dritten gehostet) übermittelt und dort gespeichert. Im Übrigen können
personenbezogene Daten an Personen bei den Verantwortlichen weitergebenen werden, die mit der Verarbeitung
befasst sind. Weitere potenzielle Empfänger:innen sind Auftragsverarbeitende oder Vertragspartner:innen, etc.
Die Weitergabe an diese Empfänger:innen erfolgt entweder auf Basis einer gesetzlichen Verpflichtung der wir unterliegen,
zur oder im Rahmen einer Auftragsverarbeitung.",

br(),
br(),

p(tags$b(span("V. Links zu Drittseiten")),
  br(),

"Bei Nutzung der Plattform könnten Inhalte angezeigt werden, die mit den Webseiten Dritter verlinkt sind.
Wir haben keinen Zugang zu den Cookies oder anderen Funktionen, die von Drittseiten eingesetzt werden,
noch können wir diese kontrollieren. Solche Drittseiten unterliegen nicht dieser Datenschutzerklärung."),

br(),br(),

p(tags$b(span("VI. Weitergabe Ihrer Daten an Dritte")),
  br(),

"Um unsere Website für Sie als Nutzer so angenehm und komfortable wie möglich gestalten zu können, setzen
wir vereinzelt Dienste externer Dienstleister ein. Nachfolgenden haben Sie Möglichkeit sich über die
Datenschutzbestimmungen zum Einsatz und Verwendung der eingesetzten Dienste und Funktionen zu informieren,
um ggf. auch bei den Dienstleistern Ihre Rechte wahrnehmen zu können. ")))))),

funct_footer()
)}
