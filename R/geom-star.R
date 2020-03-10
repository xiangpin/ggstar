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
#' p <- ggplot(iris, aes(x=Sepal.Length, 
#'                       y=Sepal.Width, 
#'                       color=Species)) + 
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

#' @importFrom grid polygonGrob gpar
starGrob <- function(size=NULL, fill=NULL, col=NULL, alpha=NULL, vp=NULL, name=NULL){
    stargrid(x = 0.5, 
             y = 0.5, 
             size = size,
             gp=gpar(fill = fill,
                     col = col,
                     alpha = alpha),
             vp = vp,
             name = name)
}

#' @importFrom ggplot2 aes ggproto Geom
#' @importFrom grid viewport gTree
GeomStar <- ggproto("GeomStar", 
                    Geom, 
                    required_aes = c("x", "y"),
                    default_aes = aes(size = 4.5, fill = "black", angle=0, colour = NA, alpha = 1),
                    draw_key = draw_key_star,
                    draw_panel=function(data, panel_scales, coord){
                        coords <- coord$transform(data, panel_scales)
                        coords$size <- (coords$size *.pt )/10
                        grobs <- lapply(seq_len(nrow(coords)), function(i){
                            vp <- viewport(x=coords$x[i], y=coords$y[i],
                                           width=coords$size[i], 
                                           height=coords$size[i],
                                           angle = data$angle[i],
                                           just = c("center", "center"),
                                           default.units = "native")
                            starGrob(fill = coords$fill[i],
                                     col = coords$colour[i],
                                     alpha = coords$alpha[i],
                                     size = coords$size[i],
                                     vp = vp, 
                                     name = i)
                        })
                    class(grobs) <- "gList"
                    ggname("geom_star", gTree(children = grobs))
                    }
            )

