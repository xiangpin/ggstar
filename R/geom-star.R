#' Star layer
#' 
#' geom_star provides the siogon layer to easily discernible 
#' starshapes for ggplot2, you can use it to create scatterplots.
#'
#' Note: the 'left-triangle' and 'right-triangle' are developed to
#' plot the 'triangle-heatmap'. Their centers are not in their internal,
#' but the center of hypotenuse.
#'
#' @eval rd_aesthetics("geom", "star")
#' @inheritParams ggplot2::layer
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'     a warning. If `TRUE`, missing values are silently removed.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{layer}}.  
#' @return polygonal layer
#' @importFrom ggplot2 layer
#' @author Shuangbin Xu
#' @export
#' @examples
#' library(ggplot2)
#' p <- ggplot(iris, aes(x=Sepal.Length, 
#'                       y=Sepal.Width, 
#'                       starshape=Species)) + 
#'      geom_star(size=4)
#' p
geom_star <- function(mapping = NULL, 
                      data= NULL, 
                      na.rm = FALSE,
                      stat = 'identity',
                      position = 'identity',
                      show.legend = NA, 
                      inherit.aes = TRUE, 
                      ...){
    layer(data = data, 
          mapping = mapping, 
          geom = GeomStar,
          stat = stat,
          position = position,
          params = list(na.rm=na.rm, ...),
          show.legend = show.legend, 
          inherit.aes = inherit.aes)
}

#' GeomStar
#' @importFrom ggplot2 aes ggproto Geom
#' @importFrom grid viewport gpar
#' @author Shuangbin Xu
#' @rdname ggstar-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomStar <- ggproto("GeomStar", 
                    Geom, 
                    required_aes = c("x", "y"),
                    non_missing_aes = c("size", "starshape"),
                    default_aes = aes(size = 1.5, colour = "black", starshape=1, 
                                      angle=0, fill = NA, alpha = 1,  
                                      phase=0, starstroke=0.5),
                    draw_key = draw_key_star,
                    draw_panel=function(data, panel_params, coord){
                        if (!is.numeric(data$starshape)){
                            data$starshape <- translate_starshape(data$starshape)
                        }
                        coords <- coord$transform(data, panel_params)
                        grobs <- starGrob(x=coords$x,
                                          y=coords$y,
                                          gp=gpar(fill = alpha(coords$fill, coords$alpha),
                                                  col = alpha(coords$colour, coords$alpha),
                                                  fontsize = (coords$size * .pt + coords$starstroke * .starstroke/2)/5,
                                                  lwd = coords$starstroke * .starstroke / 2),
                                          starshape = coords$starshape,
                                          angle = coords$angle,
                                          phase = coords$phase,
                                         )
                        ggname("geom_star", grobs)
                    }
            )

translate_starshape <- function(starshape){
    if (is.factor(starshape)){
        starshape <- as.numeric(starshape)
    }
    if (is.character(starshape)){
        starshape <- translate_starshape_string(starshape)
    }
    return(starshape)
}

starshape_table <- c(
                 "pentagram"                = 1,
                 "magen david"              = 2,
                 "seven pointed star"       = 3,
                 "anise star"               = 4,
                 "regular pentagon"         = 5,
                 "hexagon"                  = 6,
                 "regular heptagon"         = 7,
                 "regular octagon"          = 8,
                 "anise star2"              = 9,
                 "anise star3"              = 10,
                 "regular triangle"         = 11,
                 "rhombus"                  = 12,
                 "square"                   = 13,
                 "four-pointed star"        = 14,
                 "circle"                   = 15,
                 "heart"                    = 16,
                 "left-triangle1"           = 17,
                 "right-triangle1"          = 18,
                 "left-triangle2"           = 19,
                 "right-triangle2"          = 20,
                 "rectangle"                = 21,
                 "triangle star"            = 22,
                 "regular triangle down"    = 23,
                 "hexagonal star"           = 24,
                 "ellipse"                  = 25,
                 "thin triangle"            = 26,
                 "anise star4"              = 27,
                 "square diamond"           = 28,
                 "plus filled"              = 29,
                 "antiparallelogram"        = 30)

# reference ggplot2
translate_starshape_string <- function(starshape_string){

    starshape_match <- charmatch(starshape_string, names(starshape_table))
    invalid_strings <- is.na(starshape_match)
    nonunique_strings <- starshape_match == 1

    if (any(invalid_strings)) {
        bad_string <- unique(starshape_string[invalid_strings])
        n_bad <- length(bad_string)

        collapsed_names <- sprintf("\n* '%s'", bad_string[1:min(9, n_bad)])
        more_problems <- if (n_bad > 9) {
            sprintf("\n* ... and %d more problem%s", n_bad - 9, ifelse(n_bad > 10, "s", ""))
        }
        stop("Can't find starshape name:", collapsed_names,
             more_problems, call. = FALSE)
    }
    if (any(nonunique_strings)) {
        bad_string <- unique(starshape_string[nonunique_strings])
        n_bad <- length(bad_string)
        
        n_matches <- vapply(
	     bad_string[1:min(9, n_bad)],
	     function(starshape_string) sum(grepl(paste0("^", starshape_string), names(starshape_table))),
	     integer(1))

        collapsed_names <- sprintf("\n* '%s' partially matches %d starshape names",
				   bad_string[1:min(9, n_bad)], n_matches)

        more_problems <- if (n_bad > 9) {sprintf("\n* ... and %d more problem%s", 
                                                n_bad - 9, ifelse(n_bad > 10, "s", ""))}

	stop("starshape names must be unambiguous:",
             collapsed_names,
             more_problems,
             call. = FALSE)
    }
    unname(starshape_table[starshape_match])
}
