#' @importFrom grid unit convertX convertY is.unit 
#' @importFrom gridExtra polygon_regular
stargrid <- function(x, y,
                     n=5, size=2, 
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
    
    locoord <- polygon_regular(n=n, phase=phase)
    lxy <- stretch_rotate_move(p=locoord,
                               size = size,
                               ar = ar,
			       angle = angle,
			       x = xv, y = yv,
                               position.units = position.units,
                               size.units=size.units)
    newx <- lxy$x
    newy <- lxy$y
    newx <- newx[c(1, 3, 5, 2, 4)]
    newy <- newy[c(1, 3, 5, 2, 4)]
    polygonGrob(newx, newy, gp = gp, ...)
}

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
