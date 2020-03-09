#' @title Star layer
#' 
#' @inheritParams ggplot2::layer
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'     a warning. If `TRUE`, missing values are silently removed.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{layer}}.  
#' @importFrom ggplot2 layer
#' @author Shuangbin Xu
#' @export
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg, fill=cyl)) + 
#'      geom_star(size=3)
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

#' @importFrom grid polygonGrob gpar
starGrob <- function(fill=NULL,col=NULL,alpha=NULL, vp=NULL, name=NULL){
    # the polar coordinate angle
    t = 0
    # the radius
    r = 0.5
    # the sides of polygons
    n = 5
    starcoord = polygens(t = t, n = n, r = r)
    x = starcoord$x
    y = starcoord$y
    # for star, if not the shape will be regular polygons
    x = x[c(1,3,5,2,4)]
    y = y[c(1,3,5,2,4)]
    polygonGrob(x, y,
                gp=gpar(fill=fill,
                        col=col,
                        alpha=alpha),
                vp=vp,
                name=name)
}

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
#' @keywords internal
#' @author Shuangbin Xu
#' @export
#' @importFrom scales alpha
#' @importFrom grid polygonGrob gpar
draw_key_star <- function(data, params, size){
    starcoord = polygens(t = 0, n = 5, r = 0.3)
    x = starcoord$x[c(1, 3, 5, 2, 4)]
    y = starcoord$y[c(1, 3, 5, 2, 4)]
    polygonGrob(x = x,
                y = y,
                gp=gpar(fill=alpha(data$fill, data$alpha),
                        col=alpha(data$colour, data$alpha)))
}

#' @importFrom ggplot2 aes ggproto Geom
#' @importFrom grid viewport gTree
GeomStar <- ggproto("GeomStar", 
                    Geom, 
		    required_aes = c("x", "y"),
		    default_aes = aes(size = 3.5, fill = "black", angle=90, colour = NA, alpha = 1),
                    draw_key = draw_key_star,
                    draw_panel=function(data, panel_scales, coord){
                        coords <- coord$transform(data, panel_scales)
                        coords$size <- coords$size/100
                        grobs <- lapply(seq_len(nrow(coords)), function(i){
                            vp <- viewport(x=coords$x[i], y=coords$y[i],
                                           width=coords$size[i], height=coords$size[i],
                                           angle = data$angle[i],
                                           just = c("center", "center"),
                                           default.units = "native")
                            starGrob(fill = coords$fill[i],
                                     col = coords$colour[i],
                                     alpha = coords$alpha[i],
                                     vp = vp, 
                                     name = i)
                        })
                    class(grobs) <- "gList"
                    ggname("geom_star",gTree(children = grobs))
                    }
            )

#' @importFrom utils getFromNamespace
#' @keywords internal
ggname <- getFromNamespace("ggname", "ggplot2")
