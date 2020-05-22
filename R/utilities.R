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
    data <- data.frame(x=rep(c(1:5),5),
                       y=rep(1:5, each = 5, len = 25),
                       group=letters[seq_len(25)])
    p <- ggplot(data=data, aes(x=x,y=y)) +
         geom_star(aes(starshape=group),fill="red", size=3, show.legend=FALSE) +
         geom_text(aes(label=names(starshape_table[c(1:25)])),
                       nudge_y=0.3, size=3, fontface="bold.italic") +
         geom_text(aes(label=as.character(seq_len(25))), nudge_y=-0.2) + 
         scale_starshape_manual(values=c(seq_len(25))) +
         xlim(0.5, 5.5) + ylim(0.5, 5.5) +
         xlab(NULL) + ylab(NULL) +
         theme(axis.ticks=element_blank(),
               axis.text=element_blank(),...) 
    p
}

#' @importFrom utils getFromNamespace
rd_aesthetics <- getFromNamespace("rd_aesthetics", "ggplot2")

"%||%" <- function(a, b) {
    if (!is.null(a)) a else b
}
