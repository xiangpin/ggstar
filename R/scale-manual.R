#' Create your own discrete scale
#' 
#' @param values a set of aesthetic values to map data values to. If this
#'   is a named vector, then the values will be matched based on the names.
#'   If unnamed, values will be matched in order (usually alphabetical) with
#'   the limits of the scale. Any data values that don't match will be
#'   given `na.value`.
#' @param aesthetic The names of the aesthetics that this scale works with.
#' @inheritDotParams ggplot2::discrete_scale -expand -position -aesthetics
#' @name scale_manual
#' @return starshape scale constructor
#' @aliases NULL
NULL

#' @rdname scale_manual
#' @export
scale_starshape_manual <- function(..., values, aesthetic = 'starshape'){
    manual_scale(aesthetic = aesthetic, values = values, ...)
}

#' @rdname scale_manual
#' @export
scale_angle_manual <- function(
    ...,
    values,
    aesthetic = "angle"
    ){
    manual_scale(aesthetic = aesthetic, values = values, ...)
}

#' @importFrom ggplot2 discrete_scale waiver
#' @importFrom rlang is_missing caller_call current_call
# This is the internal function of ggplot2 (no-export)
manual_scale <- function(aesthetic, values = NULL, breaks = waiver(),
                         name = waiver(), ...,
                         limits = NULL, call = caller_call()) {
  call <- call %||% current_call()
  # check for missing `values` parameter, in lieu of providing
  # a default to all the different scale_*_manual() functions
  if (is_missing(values)) {
    values <- NULL
  } else {
    force(values)
  }

  if (is.null(limits) && !is.null(names(values))) {
    # Limits as function to access `values` names later on (#4619)
    force(aesthetic)
    limits <- function(x) {
      x <- intersect(x, c(names(values), NA)) %||% character()
      if (length(x) < 1) {
        cli::cli_warn(paste0(
          "No shared levels found between {.code names(values)} of the manual ",
          "scale and the data's {.field {aesthetic}} values."
        ))
      }
      x
    }
  }

  # order values according to breaks
  if (is.vector(values) && is.null(names(values)) && !is_waiver(breaks) &&
      !is.null(breaks) && !is.function(breaks)) {
    if (length(breaks) <= length(values)) {
      names(values) <- breaks
    } else {
      names(values) <- breaks[seq_along(values)]
    }
  }

  pal <- function(n) {
    if (n > length(values)) {
      cli::cli_abort("Insufficient values in manual scale. {n} needed but only {length(values)} provided.")
    }
    values
  }
  discrete_scale(
    aesthetic, name = name,
    palette = pal, breaks = breaks, limits = limits,
    call = call, ...
  )
}

is_waiver <- function(x){
  inherits(x, "waiver")
}
