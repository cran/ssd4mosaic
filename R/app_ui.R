#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    shinybusy::add_busy_gif(src = "www/img/wait-animation-tr.gif",
                            position = "top-right",
                            height = 117, width = 249,
                            margins = c(120, 10)),
    # Your application UI logic
    fluidPage(
      div(style = "justify-content: space-between;display: flex;",
          div(
            a(img(src = "www/img/logo.png", alt = "Logo"),
              href = "http://mosaic.univ-lyon1.fr", TARGET="_blank")
          ),
          div(
            tags$nav(
              tags$ul(id = "navigation_menu",
                      tags$li(a(
                        img(src = "www/img/ssd-logo.png", alt = "SSD", class = "logo"),
                        href = "", TARGET = "_blank") # target to self
                      ),
                      tags$li(
                        img(src = "www/img/surv-logo.png", alt = "Survival", class = "logo"),
                        tags$ul(class = "submenu",
                                tags$li(a("Standard",
                                          href = "https://mosaic.univ-lyon1.fr/survival",
                                          TARGET = "_blank")),
                                tags$li(a("GUTS-fit",
                                          href = "https://mosaic.univ-lyon1.fr/guts",
                                          TARGET = "_blank")),
                                tags$li(a("GUTS-predict",
                                          href = "https://lbbe-shiny.univ-lyon1.fr/guts-predict/",
                                          TARGET = "_blank"))
                                )
                      ),
                      tags$li(a(
                        img(src = "www/img/repro-logo.png", alt = "Reproduction", class = "logo"),
                        href = "https://mosaic.univ-lyon1.fr/repro", TARGET = "_blank")
                      ),
                      tags$li(a(
                        img(src = "www/img/growth-logo.png", alt = "Growth", class = "logo"),
                        href = "https://mosaic.univ-lyon1.fr/growth", TARGET = "_blank")
                      ),
                      tags$li(a(
                        img(src = "www/img/bioacc-logo.png", alt = "Bioaccumulation", class = "logo"),
                        href = "https://mosaic.univ-lyon1.fr/bioacc", TARGET = "_blank")
                      ),
                      tags$li(a(
                        img(src = "www/img/info-logo.png", alt = "Info"),
                        href = "https://mosaic.univ-lyon1.fr/info", TARGET = "_blank")
                      ),
                      tags$li(a(
                        img(src = "www/img/courses-logo.png", alt = "Courses",),
                        href = "https://mosaic.univ-lyon1.fr/courses", TARGET = "_blank",
                        div("Courses"))
                      ),
                      tags$li(a(
                        img(src = "www/img/team-logo.png", alt = "Team"),
                        href = "https://mosaic.univ-lyon1.fr/team", TARGET = "_blank",
                        div("Team"))
                      ),
                      tags$li(a(
                        img(src = "www/img/contact-logo.png", alt = "Contact"),
                        href = "https://mosaic.univ-lyon1.fr/contact", TARGET = "_blank",
                        div("Contact"))
                      )
              )
            ))
      ),
      #### Tool presentation ####
      div(style = "margin:2em 8% 0",
          fluidRow(
            column(width = 2, img(src = "www/img/ssd-application-logo.png", alt = "Big logo")),
            column(width = 10,
                   p(tags$b("Species sensitivity distribution"), "(SSD) is an approach to
           define safe levels for toxic compounds in an ecosystem. It is based
           on the assumption that species sensitivity to a given contaminant can
           be described by a probability distribution estimated from toxicity
           experiments."),
                   p("Based on this framework, ",HTML("<b>MOSAIC<sub><i>SSD</i></sub></b>"),
                     " is able to estimate the so-called ", tags$b("hazardous concentration
           for x% of the species"), " (HCx), even when the toxicity values are
           censored",
                     mod_infobutton_ui("infobutton_cens")),
                   p(HTML("<b>MOSAIC<sub><i>SSD</i></sub></b>")," is based on the ",
                     a("R", href = "http://www.r-project.org/", TARGET = "_blank"),
                     " package ",
                     a("fitdistrplus",
                       href = "http://cran.r-project.org/web/packages/fitdistrplus/index.html",
                       TARGET = "_blanl"), ". For more details, please consult the companion ",
                     a("paper", href = "http://arxiv.org/abs/1311.5772", TARGET = "_blank"),
                     "."),
                   actionButton("tutorial", label = "Tutorial",
                                onclick = "window.open('www/tutorial.html')"),
                   actionButton("faq", label = "FAQ",
                                onclick = "window.open('https://mosaic.univ-lyon1.fr/ssd-faq/')",
                                style = "margin-left:10px;")
            )
          ),

          div(id = "tabs_right",
              #### Input ####
              tabsetPanel(id = "tabs",
                          tabPanel("Input",
                                   wellPanel( style = "border-top-style: none;",
                                              div(style = "float:right;",
                                                  mod_infobutton_ui("infobutton_data"),
                                                  tags$label(class = "input-group-prepend",
                                                             tags$span(class = "btn btn-default btn-file smallButton",
                                                                       "Load from file",
                                                                       tags$input(id = "loadFile", name = "loadFile", type = "file",
                                                                                  style = "position: absolute !important; top: -99999px !important; left: -99999px !important;",
                                                                                  class = "shiny-bound-input")
                                                                       )
                                                             ),
                                                  actionButton("open_example", label = "Try with an example", class = "smallButton"),
                                                  actionButton("reset", label = "Reset", class = "smallButton",
                                                               onclick = "Shiny.setInputValue('first_plot_done', null);")
                                              ),
                                              tags$b("Dataset name: "),
                                              div(class = "inline-input", style = "width: 25em;",
                                                  textInput("dataName", label = NULL)),
                                              fluidRow(style = "margin-top: 2em;",
                                                       column(width = 7, style = "border-right: 1px solid #333;",
                                                              column(width = 7,
                                                                     rhandsontable::rHandsontableOutput("hot")
                                                              ),
                                                              column(width = 5,
                                                                     br(),
                                                                     switchInput("censoredYN", "Censored data", FALSE),
                                                                     div(tags$b("Censored data"), class = "toggle-text"),
                                                                     mod_infobutton_ui("infobutton_cens_format"),
                                                                     br(),
                                                                     switchInput("col1YN", "Use label 1", FALSE),
                                                                     div(tags$b("Use label 1"), class = "toggle-text"),
                                                                     fluidRow(style = "margin-top:-1.75em;",
                                                                               column(width = 2, offset = 4,
                                                                                      div("As :", style = "display: inline; vertical-align:-0.5em;")
                                                                               ),
                                                                               column(width = 6,
                                                                                      selectInput("col1Use", label = NULL, choices = c("Names", "Groups"))
                                                                               )
                                                                     ),
                                                                     switchInput("col2YN", "Use label 2", FALSE),
                                                                     div(tags$b("Use label 2"), class = "toggle-text"),
                                                                     fluidRow(style = "margin-top:-1.75em;",
                                                                       column(width = 2, offset = 4,
                                                                              div("As :", style = "display: inline; vertical-align:-0.5em;")
                                                                       ),
                                                                       column(width = 6,
                                                                              selectInput("col2Use", label = NULL, choices = c("Names", "Groups"))
                                                                       )
                                                                     )
                                                              )
                                                       ),
                                                       column(width = 4,
                                                              tags$b("Concentration unit"),
                                                              div(class = "inline-input", style = "width: 10em; vertical-align: -0.9em;",
                                                                  selectInput("conc", NULL,
                                                                              choices = c("Arbitrary unit",
                                                                                          "\u03bcg/L",
                                                                                          "\u03bcmol/L",
                                                                                          "mg/L",
                                                                                          "pg/L",
                                                                                          "ng/L",
                                                                                          "g/L",
                                                                                          "mS/cm\u00b2",
                                                                                          "g/ha"),
                                                                              selected = "\u03bcg/L")),
                                                              br(),
                                                              tags$b("Show distributions"),
                                                              mod_infobutton_ui("infobutton_dist"),
                                                              div(class = "inline-input",
                                                                  checkboxGroupInput("logDist", label = NULL, choices = c("log-normal" = "lnorm", "log-logistic" = "llogis"), selected = "lnorm", inline = T)
                                                              )
                                                       )
                                              ),
                                              actionButton("run", "Run", icon = icon("rocket"),
                                                           style="font-size:150%; float: right; margin-top: -2.5em;",
                                                           onclick = "Shiny.setInputValue('first_plot_done', null);")
                                   )),
                          #### Output ####
                          tabPanel("Results",
                                   wellPanel( style = "border-top-style: none;",
                                              h2(
                                                div(style = "display: inline;font-family: Comfortaa Regular; color: #ee7202;", "Results"),
                                                "for dataset:", tags$i(textOutput("dataNameDisp", inline = T))),
                                              fluidRow(style="display:flex;", class = "full-height",
                                                       column(width = 3,
                                                              wellPanel(style = "border-color: #ccc;",
                                                                        checkboxGroupInput("distDisp", label = "Displayed distributions", choices = c("log-normal" = 1, "log-logistic" = 2), selected = c(1,2)),
                                                                        switchInput("logscale", "Change x axis scale", TRUE),
                                                                        div("Log scale on x axis", class = "toggle-text"),
                                                                        br(),
                                                                        switchInput("ciDispYN", "Display CI", TRUE),
                                                                        div("Confidence interval(s)", class = "toggle-text"),
                                                                        br(),
                                                                        div(style = "margin-top: -1.2em; margin-left: 67px; margin-bottom: 0px;",
                                                                        radioButtons("CIlevel", label = NULL,
                                                                                     choices = c("95% CI" = 0.95, "90% CI" = 0.9),
                                                                                     selected = 0.95, inline = TRUE),
                                                                        mod_infobutton_ui("infobutton_CIlevel_plot")),
                                                                        switchInput("namesDispYN", "Display names", FALSE),
                                                                        div("Species Names", class = "toggle-text"),
                                                                        br(),
                                                                        switchInput("groupsDispYN", "Display groups", FALSE),
                                                                        div("Group labels", class = "toggle-text"),

                                                              )
                                                       ),
                                                       column(width = 6,
                                                              wellPanel(style = "background-color: #fff;",
                                                                        plotOutput("p", height = "auto")
                                                              )
                                                       ),
                                                       column(width = 3,
                                                              mod_infobutton_ui("infobutton_CIlevel_values"),
                                                              tabsetPanel(
                                                                tabPanel(title = "Parameters",
                                                                         wellPanel(style = "background-color: #fff;",
                                                                                   h4("Parameters values:", style = "display:inline;"),
                                                                                   mod_infobutton_ui("infobutton_param_values"),
                                                                                   br(),
                                                                                   htmlOutput("parameters")
                                                                         )
                                                                ),
                                                                tabPanel(title = "HCx",
                                                                         wellPanel(style = "background-color: #fff;",
                                                                                   h4("Estimation of the hazardous concentrations"),
                                                                                   wellPanel(style = "justify-content: center;",
                                                                                             # may need further centering with real values table
                                                                                             tableOutput("HCx")
                                                                                   )
                                                                         )
                                                                )
                                                              ),
                                                              wellPanel( style = "height: auto;",
                                                                         h4("Download report"),
                                                                         fluidRow(
                                                                           column(width = 5,
                                                                                  radioButtons("reportFormat", label = NULL, choices = c("pdf", "html", "md"), selected = "pdf")
                                                                           ),
                                                                           column(width = 7,
                                                                                  downloadButton("report", label = "Download", class = "smallButton", style = "margin-top: 1.5em;")
                                                                           )
                                                                         )
                                                              )
                                                       )
                                              ),
                                              h3("R script"),
                                              verbatimTextOutput("codeR", placeholder = T)
                                   )
                          )
              )),
          wellPanel(style = "background-color: #fff;",
            h1(tags$b("Staff and contributors"), style = "font-size: large;"),
            "Elise Billoir, Universit\u00e9 de Lorraine", br(),
            "Sandrine Charles, Laboratory of Biometry and Evolutionary Biology", br(),
            "Marie Laure Delignette-Muller, VetAgro Sup", br(),
            "Mil\u00e9na Kaag, Laboratory of Biometry and Evolutionary Biology", br(),
            "Guillaume Kon Kam King, French National Research Institute for Agriculture, Food and the Environment", br(),
            "Philippe Veber, Laboratory of Biometry and Evolutionary Biology"
          )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "MOSAIC SSD"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
