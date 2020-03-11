#' Create your own discrete scale
#' 
#' @param values a set of aesthetic values to map data values to. If this
#'   is a named vector, then the values will be matched based on the names.
#'   If unnamed, values will be matched in order (usually alphabetical) with
#'   the limits of the scale. Any data values that don't match will be
#'   given `na.value`.
#' @inheritDotParams ggplot2::discrete_scale -expand -position -aesthetics
#' @name scale_manual
#' @aliases NULL
NULL

#' @rdname scale_manual
#' @export
scale_starshape_manual <- function(values,...){
    manual_scale("starshape", values, ...)
}

#' @importFrom utils getFromNamespace
#' @keywords internal
manual_scale <- getFromNamespace("manual_scale", "ggplot2")
