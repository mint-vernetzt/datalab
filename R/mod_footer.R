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

      p(tags$a("Impressum", href="#shiny-tab-impressum", "data-toggle" = "tab")," | ",
        tags$a("Kontakt", href="#shiny-tab-kontakt", "data-toggle" = "tab")," | ",
        tags$a("Datenschutz", href="#shiny-tab-datenschutz", "data-toggle"="tab"),HTML('&nbsp;'),HTML('&nbsp;'),
        "Copyright © 2024. Alle Rechte vorbehalten. Stifterverband")),

  div(style="display: inline-block;position: relative;padding: 1em;",

      tags$a(#href="https://www.bmbf.de/bmbf/de/home/home_node.html",
        img(src='www/BMBF-Logo_transp1.png',

            class = "img-responsive",

            height = "200px", width = "200px",

            alt = "Logo BMBF", target="_blank",

            style="display: inline-block; margin-left: auto; margin-right: auto;"))),

  div(style="display: inline-block;width: 100%;",

      " ")
  )

  return(foot)
}
