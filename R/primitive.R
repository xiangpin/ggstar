#' @importFrom grid unit convertX convertY is.unit 
#' @importFrom gridExtra polygon_regular
grid.star <- function(x, y,
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
    
    stargrob <- .grid.star(xv=xv, yv=yv,
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

#' @importFrom gridExtra polygon_regular
#' @importFrom grid gList polygonGrob
.grid.star <- function(xv, yv, 
                       starshape, size,
                       angle, ar,
                       phase,
                       gp,
                       position.units,
                       size.units, ...){
    if (!starshape%in%seq_len(10)){
        stop("the starshape should be one of 1 to 10 !")
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
    if (starshape == 10) {ar=0.5}
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

# reference the gridExtra
stretch_rotate_move <- function(p, size, 
                                ar, angle, x, 
                                y, position.units, size.units){
    central <- size * p %*%
    diag(c(sqrt(ar), 1/sqrt(ar))) %*%
         rbind(c(cos(angle), -sin(angle)),
         c(sin(angle),  cos(angle)))
    list(x = unit(central[,1], size.units) + unit(x, position.units),
	 y = unit(central[,2], size.units) + unit(y, position.units))
}
