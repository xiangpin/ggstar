#' @importFrom utils getFromNamespace
#' @keywords internal
ggname <- getFromNamespace("ggname", "ggplot2")

.pt <- 72.27 / 25.4
.starstroke <- 96 / 25.4

#' @title Show the total star shapes
#' @param ... see also \code{\link[ggplot2]{theme}}.
#' @importFrom ggplot2 ggplot theme element_blank
#' @importFrom ggplot2 xlim ylim xlab ylab geom_text
#' @return gg object
#' @author Shuangbin Xu
#' @export
#' @examples
#' p <- show_starshapes()
#' p
show_starshapes <- function(...){
    x <- y <- group <- NULL
    data <- data.frame(x=rep(c(1:4),3),
                       y=rep(1:3, each = 4, len = 12),
                       group=letters[c(1:12)])
    p <- ggplot(data=data, aes(x=x,y=y)) +
         geom_star(aes(starshape=group),show.legend=FALSE) +
         geom_text(aes(label=names(starshape_table[c(1:12)])),
                       nudge_y=0.3, size=3, fontface="bold.italic") +
         geom_text(aes(label=as.character(c(1:12))), nudge_y=-0.2) + 
         scale_starshape_manual(values=c(1:12)) +
         xlim(0.5, 4.5) + ylim(0.5, 3.5) +
         xlab(NULL) + ylab(NULL) +
         theme(axis.ticks=element_blank(),
               axis.text=element_blank(),...) 
    p
}

#' @importFrom utils getFromNamespace
rd_aesthetics <- getFromNamespace("rd_aesthetics", "ggplot2")
