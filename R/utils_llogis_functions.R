# the functions must be exported for the exists() in fitdistrplus package to
# pass...

# inheritParams from actuar package does not work properly (does only part of
# the parameters) so copy.

#' @importFrom actuar dllogis
#' @export
actuar::dllogis

#' @importFrom actuar pllogis
#' @export
actuar::pllogis

#' @importFrom actuar qllogis
#' @export
actuar::qllogis

#' @importFrom actuar rllogis
#' @export
actuar::rllogis
