#' Key drawing functions
#'
#' Each Geom has an associated function that draws the key when the geom needs
#' to be displayed in a legend. These are the options built into ggplot2.
#'
#' @param data A single row data frame containing the scaled aesthetics to
#'      display in this key
#' @param params A list of additional parameters supplied to the geom.
#' @param size Width and height of key in mm.
#' @return A grid grob.
#' @rdname draw_key
#' @name draw_key
#' @export
#' @importFrom scales alpha
#' @importFrom grid polygonGrob gpar
#' @importFrom ggplot2 zeroGrob
draw_key_star <- function(data, params, size){
    if (is.null(data$starshape)) {
        data$starshape <- 1
    } else if (! is.numeric(data$starshape)) {
        data$starshape <- translate_starshape(data$starshape)
    }
    if (is.na(data$starshape)){
        zeroGrob()
    }else{
        starGrob(x=0.5, y=0.5,
                 starshape=data$starshape,
                 gp=gpar(fill=alpha(data$fill, data$alpha),
                         col =alpha(data$colour, data$alpha),
                         fontsize = ((data$size %||% 1.5) * .pt + (data$starstroke %||% 0.5) * .starstroke / 2)/5,
                         lwd = (data$starstroke %||% 0.5) * .starstroke / 2))
    }
}
