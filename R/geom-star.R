#' Star layer
#' 
#' geom_star provides the siogon layer to easily discernible 
#' starshapes for ggplot2, you can use it to create scatterplots.
#'
#' @eval rd_aesthetics("geom", "star")
#' @inheritParams ggplot2::layer
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'     a warning. If `TRUE`, missing values are silently removed.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{layer}}.  
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
                      show.legend = NA, 
                      inherit.aes = TRUE, 
                      ...){
    layer(data = data, 
          mapping = mapping, 
          geom = GeomStar,
          stat = "identity",
          position = 'identity',
          params = list(na.rm=na.rm,...),
          show.legend = show.legend, 
          inherit.aes = inherit.aes)
}

#' @importFrom ggplot2 aes ggproto Geom
#' @importFrom grid viewport gpar
#' @author Shuangbin Xu
GeomStar <- ggproto("GeomStar", 
                    Geom, 
                    required_aes = c("x", "y"),
                    non_missing_aes = c("size", "starshape"),
                    default_aes = aes(size = 6, fill = "black", starshape=1, 
                                      angle=0, colour = NA, alpha = 1,  
                                      phase=0, starstroke=0.5),
                    draw_key = draw_key_star,
                    draw_panel=function(data, panel_params, coord){
                        if (!is.numeric(data$starshape)){
                            data$starshape <- translate_starshape(data$starshape)
                        }
                        coords <- coord$transform(data, panel_params)
                        coords <- lapply(coords, function(x)x)
                        coords$size <- (coords$size *.pt)/10
                        coords$starstroke <- coords$starstroke * .starstroke / 2
                        if (2 %in% coords$starshape || 4 %in% coords$starshape){
                            allattr <- mapply(build_starshape_attr, starshape=coords$starshape, 
                                              fill=coords$fill, alpha=coords$alpha, 
                                              col=coords$colour, lwd=coords$starstroke, 
                                              SIMPLIFY=FALSE)
                            coords$fill <- mutate_attr(allattr, "fill")
                            coords$colour <- mutate_attr(allattr, "col")
                            coords$alpha <- mutate_attr(allattr, "alpha")
                            coords$starstroke <- mutate_attr(allattr, "lwd")
                        }
                        grobs <- starGrob(x=coords$x,
                                          y=coords$y,
                                          gp=gpar(fill = coords$fill,
                                                  col = coords$colour,
                                                  alpha = coords$alpha,
                                                  lwd = coords$starstroke),
                                          size = coords$size,
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
                 "pentagram" = 1,
                 "magen david" = 2,
                 "seven pointed star" = 3,
                 "anise star" = 4,
                 "regular pentagon" = 5,
                 "hexagon" = 6,
                 "regular heptagon" = 7,
                 "regular octagon" = 8,
                 "anise star2" = 9,
                 "anise star3" = 10,
                 "regular triangle" = 11,
                 "rhombus" = 12)

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
