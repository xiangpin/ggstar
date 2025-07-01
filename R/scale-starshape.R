#' Scales for starshapes, aka glyphs 
#'
#' `scale_starshape` maps discrete variables to nine easily discernible shapes ('starshapes').
#' If you have more than 9 levels, you will get a warning message, and the
#' seventh and subsequence levels will not appear on the plot. Use
#' [scale_starshape_manual()] to supply your own values. You can not map
#' a continuous variable to shape.
#'
#' @param default should the starshapes be default?
#' @inheritDotParams ggplot2::discrete_scale -expand -position -scale_name
#' @rdname scale_starshape
#' @importFrom ggplot2 discrete_scale
#' @export
scale_starshape <- function(..., default=TRUE){
    discrete_scale(aesthetics="starshape", palette = starshape_pal(default), ...)
}

#' @rdname scale_starshape
#' @export
#' @usage NULL
scale_starshape_discrete <- scale_starshape

#' @rdname scale_starshape
#' @export
#' @usage NULL
scale_starshape_ordinal <- function(...) {
  warning("Using starshapes for an ordinal variable is not advised", call. = FALSE)
  scale_starshape(...)
}
 
#' @rdname scale_starshape
#' @export
#' @usage NULL
scale_starshape_continuous <- function(...) {
  stop("A continuous variable can not be mapped to starshape", call. = FALSE)
}               

#' starshape palette (discrete)
#' @param default should starshapes be reorder (1, 13, 15, 11, 12, 14, 29, 2, 27) or not?
#' @export
starshape_pal <- function(default=TRUE){
    force(default)
    function(n){
        if (n > 9) {
            msg <- paste("The starshape palette can deal with a maximum of 9 discrete ",
                         "values because more than 9 becomes difficult to discriminate; ",
                         "you have ", n, ". Consider specifying starshapes manually if you ",
                         "must have them.", sep = "")
            warning(paste(strwrap(msg), collapse = "\n"), call. = FALSE)
        }
        if (default){
            res <- c(1, 13, 15, 11, 12, 14, 29, 2, 27)[seq_len(n)]
        }else{
            res <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)[seq_len(n)]
        }
        return (res)
    }
}

