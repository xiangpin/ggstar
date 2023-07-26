#' Create your own discrete scale
#' 
#' @param values a set of aesthetic values to map data values to. If this
#'   is a named vector, then the values will be matched based on the names.
#'   If unnamed, values will be matched in order (usually alphabetical) with
#'   the limits of the scale. Any data values that don't match will be
#'   given `na.value`.
#' @param aesthetics The names of the aesthetics that this scale works with.
#' @inheritDotParams ggplot2::discrete_scale -expand -position -aesthetics
#' @name scale_manual
#' @return starshape scale constructor
#' @aliases NULL
NULL

#' @rdname scale_manual
#' @export
scale_starshape_manual <- function(..., values, aesthetics = 'starshape'){
    manual_scale(aesthetics, values, ...)
}

#' @rdname scale_manual
#' @export
scale_angle_manual <- function(
    ...,
    values,
    aesthetics = "angle"
    ){
    manual_scale(aesthetics, values, ...)
}

#' @importFrom utils getFromNamespace
#' @keywords internal
manual_scale <- getFromNamespace("manual_scale", "ggplot2")
