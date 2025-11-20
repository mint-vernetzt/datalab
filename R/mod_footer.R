funct_footer <- function(){
  foot <- tags$footer(style="text-align:left;background-color:white; margin-top:20px;",

  div(style="display: inline-block;position: relative;padding: 1em;",

      tags$a(href="https://mint-vernetzt.de/",
             img(src='www/MINTv_tranparent.png',
                 class = "img-responsive",
                 height = "100px", width = "100px",
                 alt = "Logo MINT", target="_blank",
                 style="display: inline-block; margin-left: auto; margin-right:10%;"))),

  div(style="display: inline-block;position: relative;padding: 1em;",

      p(style = "font-size: 14px;",
        tags$a("Impressum", href="#shiny-tab-impressum",
               onclick="Shiny.setInputValue('footer_nav', 'impressum')",
               style = "font-size: 14px;")," | ",
        tags$a("Kontakt", href="#shiny-tab-kontakt",
               onclick="Shiny.setInputValue('footer_nav', 'kontakt')",
               style = "font-size: 14px;")," | ",
        tags$a("Datenschutz", href="#shiny-tab-datenschutz",
               onclick="Shiny.setInputValue('footer_nav', 'datenschutz')",
               style = "font-size: 14px;"), " | ",
        tags$a("Barrierefreiheit", href="#shiny-tab-barrierefreiheit",
               onclick="Shiny.setInputValue('footer_nav', 'barrierefreiheit')",
               style = "font-size: 14px;"),
        HTML('&nbsp;'),HTML('&nbsp;'),
        "Copyright © 2025. Alle Rechte vorbehalten. Stifterverband")),

  div(style="display: inline-block;position: relative;padding: 1em;",

      tags$a(#href="https://www.bmbf.de/bmbf/de/home/home_node.html",
        img(src='www/BMBFSFJ_logo.svg',

            class = "img-responsive",

            height = "250px", width = "250px",

            alt = "Logo BMBFSFJ", target="_blank",

            style="display: inline-block; margin-left: auto; margin-right: auto;"))),

  div(style="display: inline-block;width: 100%;",

      " ")
  )

  return(foot)
}
