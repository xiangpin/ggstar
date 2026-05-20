#' Create and draw star grobs
#'
#' Construct grid grobs for ggstar star shapes. These functions follow the
#' argument style of [grid::pointsGrob()] and [grid::grid.points()], while
#' using ggstar-specific `starshape` values. The returned grob can be drawn
#' directly with grid or supplied to custom ggplot2 guides such as
#' `ggplot2::guide_custom()`.
#'
#' @param x,y Numeric or unit vectors specifying the star positions.
#' @param starshape Integer vector of star shape identifiers from 1 to 32.
#' @param size Numeric or unit vector giving the star size. Numeric values are
#'   interpreted in millimetres.
#' @param angle Rotation angle in degrees.
#' @param phase Phase shift in radians used when constructing the polygon.
#' @param default.units Character string giving the units for `x` and `y` when
#'   they are numeric.
#' @param name A character identifier for the grob.
#' @param gp A `grid::gpar()` object describing graphical parameters.
#' @param vp A grid viewport object.
#' @param ... Additional arguments passed to [grid::polygonGrob()]. Legacy
#'   `position.units` and `size.units` arguments are still accepted for
#'   compatibility.
#' @param draw Logical indicating whether to draw the grob.
#'
#' @return `starGrob()` returns a grid grob representing one or more stars.
#'   `grid.star()` draws the grob and invisibly returns it.
#' @seealso [draw_key_star()]
#' @examples
#' sg <- starGrob(
#'     x = 0.5,
#'     y = 0.5,
#'     starshape = 15,
#'     size = grid::unit(8, "mm"),
#'     gp = grid::gpar(fill = "gold", col = "grey30")
#' )
#' grid::grid.newpage()
#' grid::grid.draw(sg)
#' @export
#' @importFrom grid unit convertX convertY convertWidth is.unit unit.c gpar polygonGrob
starGrob <- function(x=0.5, y=0.5,
                     starshape=1,
                     size = unit(2, "mm"),
                     angle=0,
                     phase = 0,
                     default.units = "npc",
                     name = NULL,
                     gp = gpar(fill="black",
                               alpha=1,
                               col=NA,
                               lwd=0.5),
                     vp = NULL,
                     ...){
    size_missing <- missing(size)
    legacy <- resolve_legacy_star_args(
        size = size,
        default.units = default.units,
        dots = list(...)
    )
    size <- legacy$size
    default.units <- legacy$default.units
    dots <- legacy$dots

    if (size_missing && !is.null(gp) && !is.null(gp$fontsize)){
        size <- unit(gp$fontsize, "mm")
    }

    N <- max(length(x), length(y), length(starshape),
             length(size), length(angle), length(phase))
    x <- recycle_star_grob_arg(x, N, "x")
    y <- recycle_star_grob_arg(y, N, "y")
    starshape <- recycle_star_grob_arg(starshape, N, "starshape")
    size <- normalize_star_size(size, N)
    angle <- recycle_star_grob_arg(angle, N, "angle")
    phase <- recycle_star_grob_arg(phase, N, "phase")

    if (! all(starshape %in% seq_len(32))){
        stop("the starshape should be one of 1 to 32 !")
    }

    angle <- deg2rad(x=angle)
    if (!is.unit(x)){x <- unit(x, default.units)}
    if (!is.unit(y)){y <- unit(y, default.units)}
    xv <- convertX(x, default.units, TRUE)
    yv <- convertY(y, default.units, TRUE)
    size_mm <- convertWidth(size, "mm", TRUE)

    lnxy <- mapply(build_polygenxy_id.lengths,
                   starshape = starshape,
                   phase = phase,
                   SIMPLIFY = FALSE)
    vertices <- vapply(lnxy, function(x)nrow(x), integer(1))
    # ar is the aspect ratio. It can control the
    # height and width ratio of shapes.
    ar <- match_ar(starshape)
    lxy <- lapply(seq_len(N), function(i){
        stretch_rotate_move(p = lnxy[[i]],
                            size = size_mm[i],
                            ar = ar[i],
                            angle = angle[i],
                            x = xv[i],
                            y = yv[i],
                            default.units = default.units)
    })
    allx <- do.call("unit.c", lapply(lxy, "[[", "x"))
    ally <- do.call("unit.c", lapply(lxy, "[[", "y"))

    grobs <- do.call(
        polygonGrob,
        c(
            list(x = allx,
                 y = ally,
                 id.lengths = vertices,
                 gp = gp,
                 name = name,
                 vp = vp),
            dots
        )
    )
    return(grobs)
}

resolve_legacy_star_args <- function(size, default.units, dots){
    if (!is.null(dots$position.units)){
        default.units <- dots$position.units
        dots$position.units <- NULL
    }
    if (!is.null(dots$size.units)){
        if (!is.unit(size)){
            size <- unit(size, dots$size.units)
        }
        dots$size.units <- NULL
    }
    list(size = size, default.units = default.units, dots = dots)
}

recycle_star_grob_arg <- function(x, n, arg){
    len <- length(x)
    if (len %in% c(1, n)){
        return(x[rep(seq_len(len), length.out = n)])
    }
    stop(arg, " must have length 1 or ", n, " !")
}

normalize_star_size <- function(size, n){
    if (!is.unit(size)){
        size <- unit(size, "mm")
    }
    recycle_star_grob_arg(size, n, "size")
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
                    0, 0, 50, 50)

names(starshape_ntab) <- seq_len(32)

match_n <- function(starshape){
    n <- starshape_ntab[match(starshape,names(starshape_ntab))]
    return(unname(n))
}

# index of starshape = aspect ratio (ar) 
starshape_artab <- c(rep(1, 9), 0.5, 1, 0.5, rep(1,12),0.5, 0.18, 1, 1, 1, 1,1,1)
names(starshape_artab) <- seq_len(32)

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
    }else if (starshape==31){
        plxy <- polygon_regular(n=n+1, phase=phase)
        endxy <- matrix(apply(plxy[c(nrow(plxy)/2, nrow(plxy)/2+1),], 2, mean), nrow = 1)
        plxy <- plxy[1:(nrow(plxy)/2), ]
        plxy <- rbind(plxy, endxy)
    }else if (starshape == 32){
        phase <- pi/2
        plxy <- 0.85*polygon_regular(n = n+1, phase = phase)
        endxy1 <- matrix(apply(plxy[c(nrow(plxy)/2, nrow(plxy)/2+1),], 2, mean), nrow = 1)
        endxy2 <- matrix(c(max(plxy[,1]), max(plxy[,2]), min(plxy[,1]), max(plxy[,2])), ncol=2, byrow=T)
        plxy <- plxy[seq(nrow(plxy)/2),]
        plxy <- rbind(plxy, endxy1, endxy2)
    }else{
        plxy <- 0.8*polygon_regular(n=n, phase=phase)
    }
    return (plxy)
}

#' @rdname starGrob
#' @export
#' @importFrom grid grid.draw
grid.star <- function(x=0.5, y=0.5,
                      starshape=1,
                      size = unit(2, "mm"),
                      angle=0,
                      phase = 0,
                      default.units = "npc",
                      name = NULL,
                      gp = gpar(fill="black",
                                alpha=1,
                                col=NA,
                                lwd=0.5),
                      draw = TRUE,
                      vp = NULL, ...){
    sg <- starGrob(x = x, y = y,
                   starshape = starshape,
                   size = size,
                   angle = angle,
                   phase = phase,
                   default.units = default.units,
                   name = name,
                   gp = gp,
                   vp = vp, ...)
    if (draw){
        grid.draw(sg)
    }
    invisible(sg)
}

# reference the gridExtra
stretch_rotate_move <- function(p, size, 
                                ar, angle, x, 
                                y, default.units,
                                size.unit = "mm"){
    central <- size * p %*%
    diag(c(sqrt(ar), 1/sqrt(ar))) %*%
         rbind(c(cos(angle), -sin(angle)),
         c(sin(angle),  cos(angle)))
    list(x = unit(central[,1], size.unit) + unit(x, default.units),
	 y = unit(central[,2], size.unit) + unit(y, default.units))
}
