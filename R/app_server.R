#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  #### Reactive values definition ####
  variables <- reactiveValues(
    data = NULL,
    data_prep = NULL,
    censored = NULL,
    use_names = NULL,
    use_groups = NULL,
    unit = NULL,
    distributions = NULL,
    distributions_disp = NULL,
    fits = NULL,
    bts = NULL,
    bts_conv = NULL
  )

  #### reset ####
  observe({
    update_input(get_reset_list())
    for (n in names(variables)) {
      variables[[n]] <- NULL
    }
  }) |> bindEvent(input$reset, ignoreInit = TRUE)

  #### example modal ####
  observe({
    showModal(modalDialog(
      title = "Load an example",
      example_choice_input("example_select"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("okBtn", "OK")
      )
    ))
  }) |> bindEvent(input$open_example, ignoreInit = TRUE)

  observe({
    choice <- input$example_select
    data <- utils::read.delim(system.file(choice, package = "ssd4mosaic"),
                              header = FALSE)
    # update inputs accordingly
    elements <- get_example_info(choice)
    update_input(elements)

    variables$data <- clean_table(data)
    removeModal()
  }) |> bindEvent(input$okBtn, ignoreInit = TRUE)

  #### fetch and modify uploaded file ####
  observe({
    if (!is.null(input$loadFile)) {
      name <- input$loadFile$name
      extension <- unlist(strsplit(name, "[.]"))
      extension <- extension[length(extension)]
      if (extension != "txt") {
        showNotification("Uploaded file should be a .txt file",
                         type = "error", duration = 15)
        return()
      }
      updateTextInput(session = session,"dataName",
                      value = input$loadFile$name)
      data <- utils::read.delim(input$loadFile$datapath, header = FALSE)
      variables$data <- clean_table(data)
    }
  })

  #### Input hands on table ####
  output$hot <- rhandsontable::renderRHandsontable({
    DT <- variables$data
    if (is.null(DT)) {
      DT <- data.frame(
        "Conc. 1" = rep(NA, 10),
        "Conc. 2" = rep(NA, 10),
        "Label 1" = rep(NA, 10),
        "Label 2" = rep(NA, 10),
        check.names = FALSE
      )
    }
  # A maximum of 6 decimal places are taken into account. Rounding after that
    my_rhandsontable(DT, height = 400, stretchH = "all", rowHeaders = TRUE,
                     digits = NA) |>
      rhandsontable::hot_col(
        col = c("Conc. 1", "Conc. 2"),
        type = "numeric",
        format = htmlwidgets::JS("{trimMantissa: true, mantissa: 6}")
        ) |>
      rhandsontable::hot_validate_numeric(col = c(1,2), min = 0) |>
      rhandsontable::hot_col(col = c("Label 1", "Label 2"), type = "text")
  })

  observe({
    table <- input$hot
    # Forces columns classes to avoid conversion to integer by hot_to_r
    table$params$rColClasses <- list("Conc. 1" = "numeric",
                                     "Conc. 2" = "numeric",
                                     "Label 1" = "character",
                                     "Label 2" = "character")
    suppressWarnings(variables$data <- rhandsontable::hot_to_r(table))
  }) |> bindEvent(input$hot, ignoreInit = TRUE)

  observe({
    nb_conc <- sum(vapply(X = variables$data[,1:2],
                          FUN = function(x) !all(is.na(x)),
                          FUN.VALUE = FALSE))
    updateCheckboxInput(inputId = "censoredYN", value = (nb_conc == 2))
  }) |> bindEvent(variables$data, ignoreInit = TRUE)

  # Notification when selecting censored despite no 2 cols filled
  observe({
    if (!input$censoredYN) {
      return()
    }
    filled_concs <- vapply(X = variables$data[, 1:2],
                             FUN = function(x) !all(is.na(x)),
                             FUN.VALUE = FALSE)
    if (sum(filled_concs[1:2]) < 2) {
      showNotification(ui = "You have indicated that the data is censored, but
                       less than two concentration columns have values in them.
                       You will need to fill in both columns before running the
                       application.",
                       duration = 10,
                       type = "message")
    }
  }) |> bindEvent(input$censoredYN, ignoreInit = TRUE)

  # Activate label column selection
  observe({
    shinyjs::toggleState("col1Use", condition = input$col1YN)
    shinyjs::toggleState("col2Use", condition = input$col2YN)
  })

  # Update label column selection so that both columns can't have the same use
  observe({
    choices <- c("Groups", "Names")
    if (input$col1YN) {
      updateSelectInput(inputId = "col2Use",
                        selected = choices[!choices %in% input$col1Use])
    }
  })

  observe({
    choices <- c("Groups", "Names")
    if (input$col2YN) {
      updateSelectInput(inputId = "col1Use",
                        selected = choices[!choices %in% input$col2Use])
    }
  })

  #### RUN button logic ####
  observe({
    shinyjs::toggleState("run")
    # get values from toggle switches
    # and lock relevant display toggles if option not available
    variables$censored <- input$censoredYN
    variables$use_names <- (input$col1YN && input$col1Use == "Names") ||
      (input$col2YN && input$col2Use == "Names")
    variables$use_groups <- (input$col1YN && input$col1Use == "Groups") ||
      (input$col2YN && input$col2Use == "Groups")
    variables$unit <- input$conc
    shinyjs::toggleState("namesDispYN", condition = variables$use_names)
    shinyjs::toggleState("groupsDispYN", condition = variables$use_groups)

    variables$distributions <- as.list(input$logDist)
    # Hide distribution selection if only one is available
    shinyjs::toggle("distDisp",
                    condition = length(variables$distributions) != 1)

    variables$distributions_disp <- switch(length(variables$distributions),
                                           c(1), c(1,2))

    # In case of rerun
    variables$bts <- NULL
    updateCheckboxInput(inputId = "namesDispYN", value = FALSE)
    updateCheckboxInput(inputId = "groupsDispYN", value = FALSE)

    # examine the data
    # which columns do we have? Are they compatible with the toggle switches?
    data <- variables$data
    filled_columns <- cbind(
      vapply(X = data[, 1:2], FUN = function(x) !all(is.na(x)),
             FUN.VALUE = FALSE),
      vapply(X = data[, c(3,4)],
             FUN = function(x) !all(x == "") && !all(x == "NA"),
             FUN.VALUE = FALSE)
    )
    # remove rows with no concentration
    data <- data[rowSums(is.na(data[, 1:2])) != 2, ]
    tests <- expression(
      # no data or
      # no concentration provided (only text columns) -> also wrong column sep
      sum(filled_columns[1:2]) == 0,
      # only one concentration provided
      nrow(data) == 1,
      # No distribution selected
      length(variables$distributions) == 0,
      # selected censored but no second concentrations
      variables$censored && sum(filled_columns[1:2]) < 2,
      # some con1 are higher than con2
      if (variables$censored) {
        central <- data[!is.na(data[, 1]) & !is.na(data[, 2]),]
        any(central[, 1] > central[, 2])
      } else {
        FALSE
      },
      # Label col 1 selected but empty
      input$col1YN && !filled_columns[3],
      # Label col 2 selected but empty
      input$col2YN && !filled_columns[4]
    )

    messages <- c(
      "No concentration provided!",
      "More than one concentration should be provided.",
      "No distribution selected!",
      "Censored data option selected but less than two concentration columns
      contain values.",
      "Some lower bounds (first column) have higher value than higher bounds
      (secon column)",
      "First label column selected but does not contain values",
      "Second label column selected but does not contain values"
    )

    for (i in 1:length(tests)) {
      if(eval(tests[i])) {
        showNotification(messages[i], type = "error", duration = 15)
        shinyjs::toggleState("run")
        return()
      }
    }

    # Information messages when some columns are not used
    tests <- expression(
      # no label used even if one or more column is filled
      (!variables$use_names && !variables$use_groups) &&
        sum(filled_columns[3:4]) > 0,
      # one label used even if two available
      !(variables$use_names && variables$use_groups) &&
        sum(filled_columns[3:4]) == 2,
      # two columns of concentration available but only one used
      !variables$censored && sum(filled_columns[1:2]) == 2
    )
    messages <- c(
      "Some label columns are not used.",
      "Some label columns are not used.",
      "Lower bounds of intervals data used as punctual data"
    )
    for (i in 1:length(tests)) {
      if (eval(tests[i])) {
        showNotification(messages[i], duration = 5, type = "message")
      }
    }

    # compute fit
    ## Rename columns for compatibility with functions
    data_conc <- data[1:2][filled_columns[1:2]]
    if (variables$censored) {
      colnames(data_conc)[1:2] <- c("left", "right")
    } else {
      colnames(data_conc)[1] <- "conc"
    }

    data_label <- data[3:4]
    if(input$col1YN) {
      colnames(data_label)[1] <- ifelse(input$col1Use == "Groups", "group", "name")
    }
    if(input$col2YN) {
      colnames(data_label)[2] <- ifelse(input$col2Use == "Groups", "group", "name")
    }
    data <- cbind(data_conc, data_label)
    variables$data_prep <- data

    variables$fits <- get_fits(data, variables$distributions,
                               variables$censored)
    # switch tab
    updateTabsetPanel(inputId = "tabs", selected = "Results")
    shinyjs::toggleState("run")
  }) |> bindEvent(input$run, ignoreInit = TRUE)

  #### Bootstrap logic ####
  observe({
    # after the plot with only fits was refreshed
    if (is.null(input$first_plot_done)) {
      return()
    }
    showNotification("Bootstrap in progress...",
                     duration = NULL,
                     closeButton = FALSE,
                     id = "bts_progress",
                     type = "warning")
    bts <- get_bootstrap(variables$fits)
    removeNotification("bts_progress")
    variables$bts <- bts[[1]]
    variables$bts_conv <- all(unlist(bts[[2]]))
    if (variables$bts_conv) {
      showNotification("Bootstrap finished sucessfully!", duration = 15,
                       type = "message")
    } else {
      showNotification("The bootstrap procedure did not converge, the confidence
                       intervals are displayed but may not be accurate.",
                       duration = 15, type = "error")
    }
  })

  observe({
    if(length(input$distDisp) == 0) {
      updateCheckboxGroupInput(inputId = "distDisp", selected = 1)
      showNotification("At least one distribution should always be displayed",
                       type = "warning")
    }
  }) |> bindEvent(input$distDisp, ignoreInit = TRUE, ignoreNULL = FALSE)

  # ignoreNULL = TRUE: no action when no distribution is selected
  observe({
    variables$distributions_disp <- as.numeric(input$distDisp)
    }) |> bindEvent(input$distDisp)

  #### Plot render ####
  output$p <- renderPlot({
    if(is.null(variables$fits))
      return()
    h <- !input$groupsDispYN
    l <- input$namesDispYN
    p <- options_plot(variables$fits[variables$distributions_disp],
                      variables$unit, input$logscale,
                      variables$data_prep,
                      use_names = input$namesDispYN,
                      use_groups = input$groupsDispYN,
                      horizontals = h,
                      lines_display = l)
    if (input$ciDispYN && !is.null(variables$bts)) {
      p <- add_CI_plot(p, variables$bts[variables$distributions_disp],
                       input$logscale, CI.level = as.numeric(input$CIlevel))
    }
    p
  }, height = function(){ifelse(input$groupsDispYN,
                                get_group_plot_height(variables$data_prep),
                                400)})

  #### Other renders ####
  output$dataNameDisp <- renderText(input$dataName)

  output$parameters <- renderUI({
    if(is.null(variables$fits))
      return()
    HTML(get_parameters_html(variables$fits, variables$bts,
                             CI.level = as.numeric(input$CIlevel)))
  })

  output$HCx <- renderTable({
    if(is.null(variables$fits))
      return()
    get_HCx_table(variables$fits, variables$distributions, variables$bts,
                  as.numeric(input$CIlevel))
  }, rownames = TRUE, width = "100%")
  outputOptions(output, "HCx", suspendWhenHidden = FALSE)

   output$codeR <- renderPrint({
     if(is.null(variables$fits))
       return()
     data <- subset(variables$data_prep,
                    select = apply(variables$data_prep, MARGIN = 2,
                                    function(x) !all(is.na(x))))
     cat(code_r_ssd(
       data = data,
       distributions = variables$distributions,
       censored = variables$censored,
       logscale = input$logscale,
       unit = variables$unit,
       names = variables$use_names,
       groups = variables$use_groups,
       CI.level = as.numeric(input$CIlevel)
     )
     )
   })

   #### Download ####
   output$report <- downloadHandler(
     filename = function() {paste0("report-", format(Sys.time(), '%Y-%m-%d'),
                                   ".", ifelse(input$reportFormat == "md",
                                               "zip", input$reportFormat))},
     content = function(file) {
       showNotification("Building report...",
                        duration = NULL,
                        closeButton = FALSE,
                        id = "report_progress",
                        type = "warning")
       params <- list(
         title = input$dataName,
         unit = variables$unit,
         distributions = variables$distributions,
         censored = variables$censored,
         data = variables$data_prep,
         fits = variables$fits,
         bts = variables$bts,
         ok_bts = variables$bts_conv,
         CI.level = as.numeric(input$CIlevel),
         logscale = input$logscale,
         use_names = variables$use_names,
         use_groups = variables$use_groups
       )
       src <- system.file("app/www/report.Rmd", package = "ssd4mosaic")
       owd <- setwd(tempdir())
       on.exit(setwd(owd))
       file.copy(src, "report.Rmd", overwrite = TRUE)
       out <- render_report(input = "report.Rmd",
              output_format = paste0(input$reportFormat, "_document"),
              params = params)

       # if markdown format, additional image files need to be included for the
       # plots of the report
       if (input$reportFormat == "md") {
         utils::zip("archive.zip", files = c("report_files"))
         utils::zip("archive.zip", files = c(out), flags = "-g -j")
         out <- "archive.zip"
       }
       file.rename(out, file)
       removeNotification("report_progress")
     }
   )

   #### Modals ####
   mod_infobutton_server("infobutton_cens",
                         get_infobutton_content()$info_censored)
   mod_infobutton_server("infobutton_dist",
                         get_infobutton_content()$info_distribution)
   mod_infobutton_server("infobutton_data",
                         get_infobutton_content()$info_data)
   mod_infobutton_server("infobutton_cens_format",
                         get_infobutton_content()$info_censored_format)
   mod_infobutton_server("infobutton_param_values",
                         get_infobutton_content()$info_param_values)
   mod_infobutton_server("infobutton_CIlevel_plot",
                         get_infobutton_content()$info_CIlevel_plot)
   mod_infobutton_server("infobutton_CIlevel_values",
                         get_infobutton_content()$info_CIlevel_values)

}
