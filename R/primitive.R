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
    if (! all(starshape %in% seq_len(30))){
        stop("the starshape should be one of 1 to 30 !")
    }
    N <- length(x)
    stopifnot(length(y)==N)
    angle <- deg2rad(x=angle)
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

deg2rad <- function(x){x * pi / 180}
rad2deg <- function(x){x * 180 / pi}

# index of starshape = numbers of edge (n)
starshape_ntab <- c(5, 6, 7, 8,
                    5, 6, 7, 8,
                    8, 8, 3, 4,
                    4, 4, 50, 0,
                    0, 0, 0, 0,
                    0, 3, 3, 6,
                    50, 3, 0, 4,
                    0, 0)

names(starshape_ntab) <- seq_len(30)

match_n <- function(starshape){
    n <- starshape_ntab[match(starshape,names(starshape_ntab))]
    return(unname(n))
}

# index of starshape = aspect ratio (ar) 
starshape_artab <- c(rep(1, 9), 0.5, 1, 0.5, rep(1,12),0.5, 0.18, 1, 1, 1, 1)
names(starshape_artab) <- seq_len(30)

match_ar <- function(starshape){
    ar <- starshape_artab[match(starshape,names(starshape_artab))]
    return(unname(ar))
}

#' @importFrom gridExtra polygon_regular
build_polygenxy_id.lengths <- function(starshape, phase){
    # the edge numbers
    n <- match_n(starshape)
    if (starshape %in% c(1, 2, 3, 4, 9, 10, 14, 22, 24)){
        phase2 <- phase + pi/n
        tmpplxy <- mapply(polygon_regular, 
               phase=c(phase, phase2), 
               n=rep(n, 2), SIMPLIFY=FALSE)
        if (starshape==1){
            tmpplxy[[2]] <- 0.38 * tmpplxy[[2]]
        }else if (starshape==2){
            tmpplxy[[2]] <- 0.556 * tmpplxy[[2]]
        }else if (starshape==3){
            tmpplxy[[2]] <- 0.32 * tmpplxy[[2]]
        }else if (starshape==4){
            tmpplxy[[2]] <- 0.756 * tmpplxy[[2]]
        }else if (starshape==14){
            tmpplxy[[2]] <- 0.35 * tmpplxy[[2]]
        }else if (starshape==22){
            tmpplxy[[2]] <- 0.2 * tmpplxy[[2]]
        }else if (starshape==24){
            tmpplxy[[2]] <- 0.26 * tmpplxy[[2]]
        }else{
            tmpplxy[[2]] <- 0.5 * tmpplxy[[2]]
        }
        tmpplxy <- lapply(tmpplxy,function(x)data.frame(x))
        plxy <- as.matrix(mapply(function(x,y){rbind(x,y)},tmpplxy[[1]],tmpplxy[[2]]))
        colnames(plxy) <- c("x", "y")
    }else if (starshape==16){
        t <- seq(0, 2*pi, by=0.08)
        plxy <- 0.06 *as.matrix(data.frame(x=16 * sin(t)^3,
                     y=13*cos(t) - 5*cos(2*t) - 2*cos(3*t) - cos(4*t) + 2))
    }else if (starshape==13){
        phase <- phase + pi/n
        plxy <- polygon_regular(n=n, phase=phase)
    }else if (starshape==17){
        plxy <- 1.2*matrix(c(-1, -1, 1, 0.5, -0.5, -0.5),nrow=3)
    }else if (starshape==18){
        plxy <- 1.2*matrix(c(-1, 1, 1, 0.5, 0.5, -0.5),nrow=3) 
    }else if (starshape==19){
        plxy <- matrix(c(-1,-1,1,1,-1,-1), nrow=3)
    }else if (starshape==20){
        plxy <- matrix(c(-1,1,1,1,1,-1), nrow=3)
    }else if (starshape==21){
        plxy <- 0.8 * matrix(c(-1, 1, 1, -1,
	                           0.5, 0.5, -0.5, -0.5), nrow=4)
    }else if (starshape==23){
        phase <- phase + pi/n       
        plxy <- 0.8*polygon_regular(n=n, phase=phase)
    }else if (starshape==26){
        phase <- phase + pi/n
        plxy <- 0.7*polygon_regular(n=n, phase=phase)
    }else if (starshape==27){
        plxy <- 0.7*data.frame(x=c(0, -0.25, -0.65, -0.5, -1.1, -0.5, -0.65, 
                                   -0.25, 0, 0.25, 0.65, 0.5, 1.1, 0.5, 0.65, 0.25),
                               y=c(1.4, 0.5, 0.65, 0.25, 0, -0.25, -0.65, -0.5,
                                   -1.4, -0.5, -0.65, -0.25, 0, 0.25, 0.65, 0.5)) 
        plxy <- as.matrix(plxy)
    }else if (starshape==29){
        plxy <- 0.7 * data.frame(x=c(-0.2, -0.2, -1, -1, -0.2, -0.2, 0.2, 0.2, 1, 1, 0.2, 0.2),
                                 y=c(1, 0.2, 0.2, -0.2, -0.2, -1, -1, -0.2, -0.2, 0.2, 0.2, 1))
        plxy <- as.matrix(plxy)
    }else if (starshape==30){
        plxy <- 0.58 * data.frame(x=c(-1,-1.6, 1.6, 1),
                                 y=c(1, -1, -1, 1))
        plxy <- as.matrix(plxy)
    }else{
        plxy <- 0.8*polygon_regular(n=n, phase=phase)
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

