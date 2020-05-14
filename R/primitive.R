#' @importFrom grid unit convertX convertY is.unit unit.c gpar
starGrob <- function(x=0.5, y=0.5,
                     starshape=1, 
                     angle=0, 
                     phase = 0,
                     gp = gpar(fill="black",
                               fontsize=2,
                               alpha=1,
                               col=NA,
                               lwd=0.5),
                     position.units = "npc", 
                     size.units="mm", ...){
    if (! all(starshape %in% seq_len(12))){
        stop("the starshape should be one of 1 to 12 !")
    }
    N <- length(x)
    stopifnot(length(y)==N)
    if (!is.unit(x)){x <- unit(x, position.units)}
    if (!is.unit(y)){y <- unit(y, position.units)}
    xv <- convertX(x, position.units, TRUE)
    yv <- convertY(y, position.units, TRUE)
    n <- match_n(starshape)
    if (is.null(gp)){
        size <- 2
    }else{
        size <- gp$fontsize
    }
    lnxy <- mapply(build_polygenxy_id.lengths, 
                       starshape=starshape, 
                       phase=phase, SIMPLIFY=FALSE)
    vertices <- unlist(lapply(lnxy, function(x)nrow(x)))
    # ar is the aspect ratio. It can control the 
    # height and width ratio of shapes.
    ar <- match_ar(starshape)
    lxy <- mapply(stretch_rotate_move, p=lnxy,
                  size=size, ar=ar, angle=angle, 
                  x=xv, y=yv, 
                  MoreArgs=list(position.units=position.units,
                                size.units=size.units),
                  SIMPLIFY = FALSE)
    allx <- do.call("unit.c", lapply(lxy, "[[", 1))
    ally <- do.call("unit.c", lapply(lxy, "[[", 2))
    grobs <- polygonGrob(allx, ally, id.lengths = vertices, gp = gp, ...)
    return(grobs)
}

# index of starshape = numbers of edge (n)
starshape_ntab <- c(5, 6, 7, 8,
                    5, 6, 7, 8,
		    8, 8, 3, 4)

names(starshape_ntab) <- seq_len(12)

match_n <- function(starshape){
    n <- starshape_ntab[match(starshape,names(starshape_ntab))]
    return(unname(n))
}

# index of starshape = aspect ratio (ar) 
starshape_artab <- c(rep(1, 9),0.5, 1, 0.5)
names(starshape_artab) <- seq_len(12)

match_ar <- function(starshape){
    ar <- starshape_artab[match(starshape,names(starshape_artab))]
    return(unname(ar))
}

#' @importFrom gridExtra polygon_regular
build_polygenxy_id.lengths <- function(starshape, phase){
    # the edge numbers
    n <- match_n(starshape)
    if (starshape==1){
        plxy <- polygon_regular(n=n, phase=phase)
        tmpx <- plxy[,1][c(1, 3, 5, 2, 4)]
        tmpy <- plxy[,2][c(1, 3, 5, 2, 4)]
        plxy <- cbind(tmpx, tmpy, deparse.level = 0)
    }else if(starshape==3){
        plxy <- polygon_regular(n=n, phase=phase)
        tmpx <- plxy[,1][c(1, 4, 7, 3, 6, 2, 5)]
        tmpy <- plxy[,2][c(1, 4, 7, 3, 6, 2, 5)]
        plxy <- cbind(tmpx, tmpy, deparse.level = 0)
    }else if (starshape==2 || starshape==4){
        phase2 <- phase + pi/n
        tmpplxy <- mapply(polygon_regular, 
               phase=c(phase, phase2), 
               n=rep(n, 2), SIMPLIFY=FALSE)
        if (starshape==2){
            tmpplxy[[2]] <- 0.556 * tmpplxy[[2]]
        }else{
            tmpplxy[[2]] <- 0.756 * tmpplxy[[2]]
        }
        tmpplxy <- lapply(tmpplxy,function(x)data.frame(x[-nrow(x),]))
        plxy <- as.matrix(mapply(function(x,y){rbind(x,y)},tmpplxy[[1]],tmpplxy[[2]]))
        colnames(plxy) <- c("x", "y")
    }else if(starshape==9 || starshape==10){
        plxy <- polygon_regular(n=n, phase=phase)
        tmpx <- plxy[,1][c(1, 6, 3, 8, 5, 2, 7, 4)]
        tmpy <- plxy[,2][c(1, 6, 3, 8, 5, 2, 7, 4)]
        plxy <- cbind(tmpx, tmpy, deparse.level = 0)
    }
    else{
        plxy <- polygon_regular(n=n, phase=phase)
    }
    return (plxy)
}

#' @importFrom grid grid.draw
grid.star <- function(x=0.5, y=0.5,
                      starshape=1,
                      angle=0, 
                      phase = 0,
                      gp = NULL,
                      position.units = "npc",
                      size.units="mm",
                      draw = TRUE, vp = NULL, ...){
    sg <- starGrob(x = x, y = y, 
                   starshape = starshape,
                   angle = angle, 
                   gp = gp, 
                   position.units = position.units,
                   size.units = size.units,
                   vp = vp,...)
    if (draw){
        grid.draw(sg)
    }
    invisible(sg)
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

