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
    x = c(0.5, 0.8, 0.1, 0.9, 0.2)
    y = c(0.1, 0.9, 0.4, 0.4, 0.9)
    polygonGrob(x, y,
                gp=gpar(fill=fill,
                        col=col,
                        alpha=alpha),
                vp=vp,
                name=name)
}

#' @importFrom ggplot2 aes ggproto Geom draw_key_polygon
#' @importFrom grid viewport gTree
GeomStar <- ggproto("GeomStar", 
                    Geom, 
		    required_aes = c("x", "y"),
		    default_aes = aes(size = 3.5, fill = "black", angle=180, colour = NA, alpha = 1),
                    draw_key = draw_key_polygon,
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
