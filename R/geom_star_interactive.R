#' @title Create interactive star points
#' @description
#' The geometry is based on [geom_star()].
#' See the documentation for those functions for more details.
#' @param ... see also the [geom_star()].
#' @export
#' @examples
#' library(ggplot2)
#' library(ggiraph)
#' p <- ggplot(iris, aes(x=Sepal.Length,
#'                       y=Sepal.Width,
#'                       fill = Species,
#'                       starshape = Species,
#'                       tooltip = Species)
#'      ) +
#'      geom_star_interactive(size=3)
#' girafe(ggobj=p)
geom_star_interactive <- function(...){
  rlang::check_installed('ggiraph', "for `geom_star_interactive()`.")
  layer_interactive(geom_star, ...)
}

# the internal functions of ggiraph
layer_interactive <- getFromNamespace("layer_interactive", "ggiraph")
add_default_interactive_aes <- getFromNamespace("add_default_interactive_aes", "ggiraph")
interactive_geom_parameters <- getFromNamespace("interactive_geom_parameters", "ggiraph")
interactive_geom_draw_key <- getFromNamespace("interactive_geom_draw_key", "ggiraph")
IPAR_NAMES <- getFromNamespace("IPAR_NAMES", "ggiraph")
add_interactive_attrs <- getFromNamespace("add_interactive_attrs", "ggiraph")
scale_interactive <- getFromNamespace("scale_interactive", "ggiraph")

#' @title ggproto classes for ggiraph
#' @description 
#' ggproto classes for ggiraph
#' @format NULL
#' @usage NULL
#' @export
GeomInteractiveStar <- ggproto(
  "GeomInteractiveStar",
  GeomStar,
  default_aes = add_default_interactive_aes(GeomStar),
  parameters = interactive_geom_parameters,
  draw_key = interactive_geom_draw_key, 
  draw_panel = function(self, data, panel_params, coord, ..., .ipar = IPAR_NAMES){
    zz <- GeomStar$draw_panel(data, panel_params, coord, ...)
    if (!.check_ipar_params(data)){
      return(zz)
    }
    coords <- coord$transform(data, panel_params)
    grobs <- add_interactive_attrs(zz, coords, ipar = .ipar)
    grobs <- .adjust_iparvalue_length(grobs)
    grobs
  } 
)

.adjust_iparvalue_length <- function(x,...){
  ipar <- x$`.ipar`
  ip <- x$`.interactive`
  anames <- intersect(names(ip), ipar)
  anames <- Filter(x = anames,
        function(a) {
            !is.null(ip[[a]])
        }) 
  if (length(anames)>0){
     for (a in anames){
        ip[[a]] <- rep(ip[[a]], x$id.lengths)
     }
     x$`.interactive` <- ip
  } 
  return(x)
}

.check_ipar_params <- function(x){
  any(colnames(x) %in% IPAR_NAMES)
}
