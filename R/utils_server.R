# Ensure 4 columns named "Conc. 1", "Conc. 2", "Label 1", "Label 2"
clean_table <- function(data) {
  types <- sapply(data, class)
  # rename columns that are numeric
  colnames(data)[types == "numeric"] <- paste("Conc.",
                                              1:sum(types == "numeric"))
  colnames(data)[types == "character"] <- paste("Label",
                                                1:sum(types == "character"))

  for (column in c("Conc. 1", "Conc. 2", "Label 1", "Label 2")) {
    if (!column %in% colnames(data)) {
      data[column] <- NA
    }
  }
  # reorder columns
  data[c("Conc. 1", "Conc. 2", "Label 1", "Label 2")]

}

get_example_info <- function(choice) {
  info <- switch(choice,
         "app/www/data/endosulfan.txt" = list(title = "Endosulfan",
                                              unit = "\u03bcg/L",
                                              col1 = TRUE,
                                              col2 = TRUE,
                                              use1 = "Names",
                                              use2 = "Groups"),
         "app/www/data/fluazinam.txt" = list(title = "Fluazinam",
                                             unit = "\u03bcg/L",
                                             col1 = TRUE,
                                             col2 = TRUE,
                                             use1 = "Names",
                                             use2 = "Groups"),
         "app/www/data/salinity_order.txt" = list(title = "Salinity (order)",
                                            unit = "mS/cm\u00b2",
                                            col1 = TRUE,
                                            col2 = FALSE,
                                            use1 = "Groups",
                                            use1 = "Names"),
         "app/www/data/salinity_family.txt" = list(title = "Salinity (family)",
                                                  unit = "mS/cm\u00b2",
                                                  col1 = TRUE,
                                                  col2 = FALSE,
                                                  use1 = "Groups",
                                                  use2 = "Names")
  )
  list(id = list("dataName",
                 "conc",
                 "col1YN",
                 "col2YN",
                 "col1Use",
                 "col2Use"),
       value = list(info$title,
                    info$unit,
                    info$col1,
                    info$col2,
                    info$use1,
                    info$use2),
       type = as.factor(c("text",
                          "select",
                          "check",
                          "check",
                          "select",
                          "select"))
       )
}

get_reset_list <- function() {
  list(id = list("dataName",
                 "censoredYN",
                 "speciesYN",
                 "groupsYN",
                 "conc",
                 "logDist",
                 "distDisp",
                 "ciDispYN",
                 "col1YN",
                 "col2YN",
                 "reportFormat",
                 "logscale"),
       value = list("",
                    FALSE,
                    FALSE,
                    FALSE,
                    "\u03bcg/L",
                    "lnorm",
                    c(1,2),
                    TRUE,
                    FALSE,
                    FALSE,
                    "pdf",
                    TRUE),
       type = as.factor(c("text",
                          "check",
                          "check",
                          "check",
                          "select",
                          "check_group",
                          "check_group",
                          "check",
                          "check",
                          "check",
                          "radio",
                          "check"))
  )
}

# input: list with three sub lists: id, value, and type
update_input <- function(elements) {
  for (t in c("check", "select", "check_group", "radio", "text")) {
    inputs <- lapply(elements, "[", elements$type == t)
    if (length(inputs$id) == 0) {
      next
    }
    switch (t,
      check = mapply(shiny::updateCheckboxInput,
                       inputId = inputs$id, value = inputs$value),
      select = mapply(shiny::updateSelectInput,
                        inputId = inputs$id, selected = inputs$value),
      check_group = mapply(shiny::updateCheckboxGroupInput,
                             inputId = inputs$id, selected = inputs$value),
      radio = mapply(shiny::updateRadioButtons,
                       inputId = inputs$id, selected = inputs$value),
      text = mapply(shiny::updateTextInput,
                      inputId = inputs$id, value = inputs$value)
    )
  }
}
