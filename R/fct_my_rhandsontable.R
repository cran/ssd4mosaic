#' @inherit rhandsontable::rhandsontable
#'
#' @description Create a Handsontable.js widget. Function extracted from the
#' {rhandsontable} package with minor modifications to make the NA values
#' display as string instead of being invisible.
#'
#' source: <https://github.com/jrowen/rhandsontable>
#'
#' @noRd
my_rhandsontable <- function(data, colHeaders, rowHeaders, comments = NULL,
                          useTypes = TRUE, readOnly = NULL,
                          selectCallback = FALSE,
                          width = NULL, height = NULL, digits = 4,
                          debug = NULL, search = FALSE, ...) {
  rColHeaders = colnames(data)
  if (.row_names_info(data) > 0L)
    rRowHeaders = rownames(data)
  else
    rRowHeaders = NULL

  if (missing(colHeaders))
    colHeaders = colnames(data)
  if (missing(rowHeaders))
    rowHeaders = rownames(data)

  rClass = class(data)
  if ("matrix" %in% rClass) {
    rColClasses = class(data[1, 1])
  } else {
    rColClasses = lapply(data, class)
    rColClasses[grepl("factor", rColClasses)] = "factor"
  }

  if ("tbl_df" %in% rClass) {
    # temp fix for tibbles
    data = as.data.frame(data)
  } else if ("data.table" %in% rClass) {
    # temp fix for data.table with S3 class
    data = as.data.frame(data)
  }

  if (!useTypes) {
    data = do.call(
      "cbind",
      structure(
        lapply(seq_len(ncol(data)), function(x){
          if (inherits(x = data[, x], what = "Date")) {
            as.character(data[, x], format = "%m/%d/%Y")
          } else {
            as.character(data[, x])
          }
        }),
        names = colnames(data)
      )
    )
    data = as.matrix(data, rownames.force = TRUE)
    cols = NULL
  } else {
    # get column data types
    col_typs = my_get_col_types(data)

    # format date for display
    dt_inds = which(col_typs == "date")
    if (length(dt_inds) > 0L) {
      for (i in dt_inds)
        data[, i] = as.character(data[, i], format = "%m/%d/%Y")
    }

    cols = lapply(seq_along(col_typs), function(i) {
      type = col_typs[i]
      if (type == "factor") {
        res = list(type = "dropdown",
                   source = levels(data[, i]),
                   allowInvalid = FALSE
        )
      } else if (type == "numeric") {
        res = list(type = "numeric",
                   numericFormat = list(pattern = "0.00"))
      } else if (type == "integer") {
        res = list(type = "numeric",
                   numericFormat = list(pattern = "0"))
      } else if (type == "date") {
        res = list(type = "date",
                   correctFormat = TRUE,
                   dateFormat = "MM/DD/YYYY")
      } else {
        res = list(type = type)
      }
      res$readOnly = readOnly
      res$renderer = htmlwidgets::JS("customRenderer")
      res$default = NA
      res
    })
  }

  x = list(
    data = jsonlite::toJSON(data, na = "string", rownames = FALSE,
                            digits = digits),
    rClass = rClass,
    rColClasses = rColClasses,
    rColnames = as.list(colnames(data)),
    rColHeaders = rColHeaders,
    rRowHeaders = rRowHeaders,
    rDataDim = dim(data),
    selectCallback = selectCallback,
    colHeaders = colHeaders,
    rowHeaders = rowHeaders,
    columns = cols,
    width = width,
    height = height,
    debug = ifelse(is.null(debug) || is.na(debug) || !is.numeric(debug),
                   0, debug),
    search = search
  )

  # create widget
  hot = htmlwidgets::createWidget(
    name = 'rhandsontable',
    x,
    width = width,
    height = height,
    package = 'rhandsontable',
    sizingPolicy = htmlwidgets::sizingPolicy(
      padding = 5,
      defaultHeight = "100%",
      defaultWidth = "100%"
    )
  )

  if (!is.null(readOnly) && !is.logical(hot$x$colHeaders)) {
    for (x in hot$x$colHeaders)
      hot = hot |> rhandsontable::hot_col(x, readOnly = readOnly)
  }

  hot = hot |> rhandsontable::hot_table(enableComments = !is.null(comments),
                                         ...)

  if (!is.null(comments)) {
    inds = as.data.frame(which(!is.na(comments), arr.ind = TRUE))
    for (i in 1:nrow(inds))
      hot = hot |>
        rhandsontable::hot_cell(inds$row[i], inds$col[i],
                 comment = comments[inds$row[i], inds$col[i]])
    #hot$x$rComments = jsonlite::toJSON(comments)
  }

  hot
}

#' Internal function from the {rhandsontable} package used in `my_rhandsontable`
#'
#' source: <https://github.com/jrowen/rhandsontable>
#'
#' @noRd
my_get_col_types = function(data) {
  if (is.matrix(data))  {
    types = rep(typeof(data), ncol(data))
  } else if (is.data.frame(data)){
    types = as.character(lapply(data, class))
  } else{
    stop("Unsupported object type: ", class(data),
         " Can't extract column types.")
  }

  types <- sapply(types, function(type) {
    if (grepl("factor", type)) return("factor")

    switch(type,
           integer="integer",
           double="numeric",
           numeric="numeric",
           character="text",
           logical="checkbox",
           Date="date",
           "text")
  })
  as.character(types)
}
