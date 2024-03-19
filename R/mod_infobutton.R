#' infobutton UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_infobutton_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("info"), label = "", icon = icon("circle-question"),
                 class = "infoButton")
  )
}

#' infobutton Server Functions
#'
#' @noRd
mod_infobutton_server <- function(
    id,
    content
    ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
      showModal(modalDialog(
        title = content$title,
        content$message,
        footer = tagList(
          modalButton("Close")
        ),
        easyClose = TRUE
      ))
    }) |> bindEvent(input$info, ignoreInit = TRUE)
  })
}

## To be copied in the UI
# mod_infobutton_ui("infobutton_1")

## To be copied in the server
# mod_infobutton_server("infobutton_1")

# titles and messages
#' @noRd
#' @import shiny
get_infobutton_content <- function() {
  info_censored <- list(
    title = "Why use censored data?",
    message = tagList(
      tags$b("What is censored data?"),
      p("Censored data is a general name given to data which is not in the form
        of fixed values but belong to an interval (bounded or not) i.e. the
        exact concentration is unknown, but lower and/or upper values are
        available."),
      tags$b("Where does it come from?"),
      p("Censored sensitivity data occurs when it is not possible to determine a
        CEC (critical effect concentration) for a given species. Possible
        reasons are 1) that the highest concentration tested does not have any
        noticeable effect, 2) that only a tiny amount of contaminant already
        stamps out all the individuals, 3) that the measurement is simply too
        imprecise to be reasonably described by a single value instead of an
        interval. In such cases, it is only possible to give a lower bound, a
        higher bound or an interval to the CEC. Censored data can also occur
        when there are multiple values for the sensitivity of one species to a
        given toxicant."),
      tags$b("Why should I use it?"),
      p("Censored data is very different from doubtful or questionable data,
        obtained from failed experiments. It is produced using a valid
        experimental procedure and it contains information as valid as
        non-censored data. Censorship is very common, especially for rare
        species where there is scant data available and for which no standard
        test procedure exists. There is a downside in discarding censored data,
        as it could represent the better part of an extended dataset.")
    )
  )

  info_distribution <- list(
    title = "Which distribution should I use?",
    message = tagList(
      p("With ",
      HTML("MOSAIC<sub><i>SSD</i></sub>"),
      ", you can choose among two probability distributions to model your CEC
      (critical effect concentration) data: log-normal or log-logistic
      distribution. These two distributions are the most widely used, and
      parameter estimation appears robust enough to accommodate for most
      datasets, as they contain only two parameters."),
      p("For more information on these two distributions, see our",
        a("distribution summary page", href = 'www/prob-distr.html',
          target = "_blank")),
      p("If uncertain,
      select both and perform a qualitative assessment by looking at the
      representative curves. The value of the likelihood function for each
      model can then be used as a further decision criterion. The log-logistic
      distribution has heavier tails than the log-normal and is therefore more
      conservative in the determination of the HC5.")
    )
  )

  info_data <- list(
    title = "Data format",
    message = tagList(
      p("The expected data for the application is a set of critical
      concentration values of a given toxic compound measured for various
      species. You can upload your data as simple text files, with one specie
      per line. The exact syntax of the lines differs whether you are dealing
      with pointwise data (precisely known values) or censored data (left
      and/or right bounded concentration). In any case, only positive values
      are accepted. Up to 6 decimal places are registered. If your data has more,
        please consider changing the concentration unit to avoid precision loss.
        Finally, the columns ",tags$b("should not")," have name headers."),
      tags$b("Pointwise data"),
      p("The file should contain one positive value per line. Additionally, one
        to two labels can be added after the concentration value (naming label
        and/or grouping label). Each column should be separated by the TAB
        character. See the following example:"),
      tags$pre("1.45  Specie1  GroupA
2.31  Specie2  GroupB
0.56  Specie3  GroupA"),
      p("For a full example, please see the uncensored example data set: ",
        a("Endosulfan", href = 'www/data/endosulfan.txt', target = "_blank"),
        "."),
      tags$b("Censored data"),
      p("In that case, each line should have two concentrations (a lower bound
        and an upper bound) and they should be separated by a TAB character.
        Missing bounds should be denoted with NA. If one concentration is known
        precisely, enter it twice, as lower bound and upper bound. As for
        pointwise data, up to two labels can be added on each line after the
        concentration values. See the following example: "),
      tags$pre(
        "1.45  1.85  GroupC
2.31  NA    GroupA
NA    0.99  GroupA
1.11  1.11  GroupB"
      ),
      p("For a full example, please see one of the censored example data
        sets: ",
        a("Fluazinam", href = 'www/data/fluazinam.txt', target = "_blank"),
        ", ",
        a("Salinity (order)", href = 'www/data/salinity_order.txt',
          target = "_blank"),
        ", ",
        a("Salinity (family)", href = 'www/data/salinity_family.txt',
          target = "_blank"),
        "."),
    )
  )

  info_censored_format <- list(
    title = "Working with censored data",
    message = tagList(
      p("Censored data is described by two concentration columns. The lower
      bounds should be in the fist column and the higher bounds in the second
      one. For a given line, the lower bound can be equal to the higher bound,
      when dealing with precisely known critical concentrations, but the lower
      bound should never be superior. Missing bounds should be denoted with NA.
      See the following example of censored data in the application table:"),
      tags$pre("Conc 1.  Conc.2  Label 1  Label 2
2.31     NA
NA       0.99
1.11     1.11
1.25     1.35")
    )
  )

  info_param_values <- list(
    title = "Parameters meanning for both distributions",
    message = tagList(
      tags$b("Log-normal distribution"),
      HTML("<p>
           If the potentially affected fraction (<math>PAF</math>) follows a
           log-normal distribution, then <math>ln(PAF)</math> has a normal
           distribution. Parameters <code>meanlog</code> and <code>sdlog</code>
           are the mean and standard deviation of <math>ln(PAF)</math>, meaning
           that the median of <math>PAF</math> equals <code>exp(meanlog)</code>.
           </p>"),
      tags$b("Log-logistic distribution"),
      HTML("<p>
           If the potentially affected fraction (<math>PAF</math>) follows a
           log-logistic distribution, then parameter <code>scale</code> equals
           the median of <math>PAF</math>, while parameter <code>shape</code> is
           related to the precision of <math>PAF</math>: the larger
           <code>shape</code> is, the more precise is <math>PAF<math>.
           </p>"),
      p("For additional information, see our ",
        a("distribution summary page", href = 'www/prob-distr.html',
                                                  target = "_blank"))
    )
  )

  info_CIlevel_plot <- list(
    title = "Choosing the confidence intervals",
    message = tagList(
      "You can choose to display either a 90% or 95% confidence interval around
      the fitted distribution(s) curve(s). This choice also impacts the
      confidence intervals displayed for the parameter values and HCx values."
    )
  )

  info_CIlevel_values <- list(
    title = "Choosing the confidence intervals",
    message = tagList(
      "You can choose to display either 90% or 95% confidence intervals for both
      the parameter values and the HCx values. The choice is made from the left
      panel of the result page, just like the selection of the interval(s)
      displayed on the plot."
    )
  )

  return(list(info_censored = info_censored,
              info_distribution = info_distribution,
              info_data = info_data,
              info_censored_format = info_censored_format,
              info_param_values = info_param_values,
              info_CIlevel_plot = info_CIlevel_plot,
              info_CIlevel_values = info_CIlevel_values))
}

