#' @importFrom grid unit convertX convertY is.unit unit.c gpar
starGrob <- function(x=0.5, y=0.5,
                     starshape=1, size=2, 
                     angle=0, 
                     phase = 0,
                     gp = gpar(fill="black",
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
    plxy_ids <- mapply(build_polygenxy_id.lengths, 
                       starshape=starshape, 
                       phase=phase, SIMPLIFY=FALSE)
    lnxy <- lapply(plxy_ids, function(x)x$plxy)
    vertices <- unlist(lapply(plxy_ids, function(x)x$ids))
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
starshape_ntab <- c(5, 3, 7, 4,
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

build_starshape_attr <- function(starshape, fill, col, lwd, alpha){
    if (starshape==2 || starshape==4){
        fill <- rep(fill, 2)
        col <- rep(col, 2)
        lwd <- rep(lwd, 2)
        alpha <- rep(alpha, 2)
    }
    allattr <- list(fill=fill, col=col, lwd=lwd, alpha=alpha)
    return(allattr)
}

mutate_attr <- function(mapplyres, attrstr){
    res <- unlist(lapply(mapplyres, function(x)x[[attrstr]]))
    return(res)
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
        ids <- nrow(plxy)
    }else if(starshape==3){
        plxy <- polygon_regular(n=n, phase=phase)
        tmpx <- plxy[,1][c(1, 4, 7, 3, 6, 2, 5)]
        tmpy <- plxy[,2][c(1, 4, 7, 3, 6, 2, 5)]
        plxy <- cbind(tmpx, tmpy, deparse.level = 0)
        ids <- nrow(plxy)
    }else if (starshape==2 || starshape==4){
        phase2 <- phase + pi/n
        plxy <- mapply(polygon_regular, 
               phase=c(phase, phase2), 
               n=rep(n, 2), SIMPLIFY=FALSE)
        plxy <- do.call("rbind", plxy)
        ids <- rep(nrow(plxy)/2, 2)
    }else if(starshape==9 || starshape==10){
        plxy <- polygon_regular(n=n, phase=phase)
        tmpx <- plxy[,1][c(1, 6, 3, 8, 5, 2, 7, 4)]
        tmpy <- plxy[,2][c(1, 6, 3, 8, 5, 2, 7, 4)]
        plxy <- cbind(tmpx, tmpy, deparse.level = 0)
        ids <- nrow(plxy)
    }
    else{
        plxy <- polygon_regular(n=n, phase=phase)
        ids <- nrow(plxy)
    }
    return(list(plxy=plxy, ids=ids))
}

#' @importFrom grid grid.draw
grid.star <- function(x=0.5, y=0.5,
                      starshape=1, size=2,
                      angle=0, 
                      phase = 0,
                      gp = NULL,
                      position.units = "npc",
                      size.units="mm",
                      draw = TRUE, vp = NULL, ...){
    sg <- starGrob(x = x, y = y, 
                   starshape = starshape, size = size, 
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

