#' @title Create interactive scales for ggstar shapes
#' @description These scales are based on
#' [scale_starshape], [scale_starshape_manual], [scale_starshape_discrete]
#' see the document for those function for more details
#' @param ... arguments passed to base function,
#' plus any of the [interactive_parameters].
#' @name scale_starshape_interactive
#' @export
#' @examples
#' library(ggplot2)
#' library(ggiraph)
#' iris$id <- seq(nrow(iris)) 
#' sps <- as.character(unique(iris$Species))
#' names(sps) <- sps
#' p <- ggplot(iris, aes(x=Sepal.Length,
#'                       y=Sepal.Width,
#'                       fill = Species,
#'                       starshape = Species,
#'                       tooltip = Species,
#'                       data_id = id
#'                      )
#'      ) +
#'      geom_star_interactive(size=2.5, alpha=.8) +
#'      scale_starshape_manual_interactive(
#'        values = c(1, 12, 15),
#'        tooltip = sps,
#'        data_id = sps,
#'      )
#' girafe(ggobj=p)
scale_starshape_interactive <- function(...){
  scale_interactive(scale_starshape, ...)
}    


#' @rdname scale_starshape_interactive
#' @export
scale_starshape_manual_interactive <- function(...){
  scale_interactive(scale_starshape_manual, ...)
}


#' @rdname scale_starshape_interactive
#' @export
scale_starshape_discrete_interactive <- function(...){
  scale_interactive(scale_starshape_discrete, ...)
}


