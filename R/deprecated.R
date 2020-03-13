# these function has been replace by more active functions
#

#' @importFrom grid unit convertX convertY is.unit 
#' @importFrom gridExtra polygon_regular
starGrob2 <- function(x=0.5, y=0.5,
                     starshape=1, size=2, 
                     angle=0, ar=1,
                     phase = 0,
                     gp = gpar(col=NA, 
                               fill="black"),
                     position.units = "npc", 
                     size.units="mm", ...){
    stopifnot(length(x)==1)
    stopifnot(length(x)==length(y))
    if (!is.unit(x)){x <- unit(x, position.units)}
    if (!is.unit(y)){y <- unit(y, position.units)}
    
    xv <- convertX(x, position.units, TRUE)
    yv <- convertY(y, position.units, TRUE)
    
    stargrob <- .starGrob(xv=xv, yv=yv,
                           starshape=starshape,
                           size = size,
                           angle = angle, 
                           ar = ar,
                           phase = phase,
                           gp = gp,
                           position.units=position.units,
                           size.units=size.units, ...)
    return (stargrob)
}

#' @importFrom grid grid.draw
grid.star2 <- function(x=0.5, y=0.5,
                      starshape=1, size=2,
                      angle=0, ar=1,
                      phase = 0,
                      gp = NULL,
                      position.units = "npc",
                      size.units="mm",
                      draw = TRUE, vp = NULL, ...){
    sg <- starGrob2(x = x, y = y, 
                   starshape = starshape, size = size, 
                   angle = angle, 
                   ar = ar,
                   gp = gp, 
                   position.units = position.units,
                   size.units = size.units,
                   vp = vp,...)
    if (draw){
        grid.draw(sg)
    }
    invisible(sg)
}

#' @importFrom gridExtra polygon_regular
#' @importFrom grid gList polygonGrob gTree
.starGrob <- function(xv, yv, 
                       starshape, size,
                       angle, ar,
                       phase,
                       gp,
                       position.units,
                       size.units, ...){
    if (!starshape%in%seq_len(12)){
        stop("the starshape should be one of 1 to 12 !")
    }
    if (starshape==1){
        locoord <- polygon_regular(n=5, phase=phase)
    }
    else if (starshape==2){
        locoord <- polygon_regular(n=6, phase=phase)
    }
    else if (starshape==3){
        locoord <- polygon_regular(n=7, phase=phase)
    }
    else if (starshape==4 || starshape==8 || starshape==9 || starshape==10){
        locoord <- polygon_regular(n=8, phase=phase)
    }
    else if (starshape==5){
        locoord <- polygon_regular(n=5, phase=phase)
    }
    else if (starshape==6){
        locoord <- polygon_regular(n=6, phase=phase)
    }
    else if (starshape==7){
        locoord <- polygon_regular(n=7, phase=phase)
    }
    else if (starshape==11){
        locoord <- polygon_regular(n=3, phase=phase)
    }
    else if (starshape==12){
        locoord <- polygon_regular(n=4, phase=phase)
        ar <- 0.5
    }
    if (starshape == 10) {ar <- 0.5}
    lxy <- stretch_rotate_move(p=locoord,
                               size = size,
                               ar = ar,
                               angle = angle,
                               x = xv, y = yv,
                               position.units = position.units,
                               size.units=size.units)
    if (starshape==1){
        x <- lxy$x[c(1, 3, 5, 2, 4)]
        y <- lxy$y[c(1, 3, 5, 2, 4)]
    }
    else if (starshape==2 || starshape==4){
        x1 <- oddelement(lxy$x)
        y1 <- oddelement(lxy$y) 
        x2 <- evenelement(lxy$x)
        y2 <- evenelement(lxy$y)
    }
    else if (starshape==3){
        x <- lxy$x[c(1, 4, 7, 3, 6, 2, 5)]
        y <- lxy$y[c(1, 4, 7, 3, 6, 2, 5)]
    }
    else if (starshape==9 || starshape==10){
        x <- lxy$x[c(1, 6, 3, 8, 5, 2, 7, 4)]
        y <- lxy$y[c(1, 6, 3, 8, 5, 2, 7, 4)]
    }
    else{
        x <- lxy$x
        y <- lxy$y
    }
    if (starshape==2 || starshape==4){
        grobres <- gTree(children=gList(polygonGrob(x1, y1, gp = gp),
                         polygonGrob(x2, y2, gp = gp)), ...)
    }
    else{
        grobres <- polygonGrob(x, y, gp = gp, ...)
    }
    return(grobres)
}

evenelement <- function(x){
    elelist <- seq_len(length(x))-1
    x[is.even(elelist)]
}

oddelement <- function(x){
    elelist <- seq_len(length(x))-1
    x[!is.even(elelist)]
}

is.even <- function(x) x%%2==0


####' @importFrom ggplot2 aes ggproto Geom
####' @importFrom grid viewport gTree gpar
####' @author Shuangbin Xu
#GeomStar2 <- ggproto("GeomStar", 
#                    Geom, 
#                    required_aes = c("x", "y"),
#                    non_missing_aes = c("size", "starshape"),
#                    default_aes = aes(size = 6, fill = "black", starshape=1, 
#                                      angle=0, colour = NA, alpha = 1, ar=1, 
#                                      phase=0, starstroke=0.5),
#                    draw_key = draw_key_star,
#                    draw_panel=function(data, panel_params, coord){
#                        if (!is.numeric(data$starshape)){
#                            data$starshape <- translate_starshape(data$starshape)
#                        }
#                        coords <- coord$transform(data, panel_params)
#                        coords$size <- (coords$size *.pt )/10
#                        grobs <- lapply(seq_len(nrow(coords)), function(i){
#                            vp <- viewport(x=coords$x[i], y=coords$y[i],
#                                           width=coords$size[i], 
#                                           height=coords$size[i],
#                                           just = c("center", "center"),
#                                           default.units = "native")
#                        grobs <- starGrob(gp=gpar(fill = coords$fill[i],
#                                                  col = coords$colour[i],
#                                                  alpha = coords$alpha[i],
#                                                  lwd = coords$starstroke[i] * .starstroke / 2),
#                                          size = coords$size[i],
#                                          starshape = coords$starshape[i],
#                                          angle = coords$angle[i],
#                                          ar = coords$ar[i],
#                                          phase = coords$phase[i],
#                                          vp = vp, 
#                                          name = i)
#                        })
#                    class(grobs) <- "gList"
#                    ggname("geom_star", gTree(children = grobs))
#                    }
#            )

