#' @importFrom utils getFromNamespace
#' @keywords internal
ggname <- getFromNamespace("ggname", "ggplot2")

.pt <- 72.27 / 25.4
.starstroke <- 96 / 25.4

#' @title Show the total star shapes
#' @param ... see also \code{\link[ggplot2]{theme}}.
#' @importFrom ggplot2 ggplot theme element_blank aes_
#' @importFrom ggplot2 xlim ylim xlab ylab geom_text
#' @return gg object
#' @author Shuangbin Xu
#' @export
#' @examples
#' p <- show_starshapes()
#' p
show_starshapes <- function(...){
    data <- data.frame(p=seq_len(30),
                       x=c(0, c(seq_len(30)%%5)[-30]),
                       y=c(0, c(seq_len(30)%/%5)[-30]))
    dat1 <- data.frame(label=names(starshape_table[seq_len(30)]),
                       x=c(0,c(seq_len(30)%%5)[-30]),
                       y=c(0,c(seq_len(30)%/%5)[-30])+0.25)
    dat2 <- data.frame(p=seq_len(30),
                       x=c(0,c(seq_len(30)%%5)[-30]),
                       y=c(0,c(seq_len(30)%/%5)[-30])-0.25)
    p <- ggplot() +
         geom_star(data=data, aes_(x=~x, y=~y, starshape=~p), 
                   fill="red", size=3, show.legend=FALSE) +
         geom_text(data=dat1, aes_(x=~x, y=~y, label=~label),
                   size=3, fontface="bold.italic") +
         geom_text(data=dat2, aes_(x=~x, y=~y, label=~p), 
                   size=3, fontface="bold") +
         scale_starshape_identity() +
         xlim(-0.15, 4.3) + ylim(-0.25, 5.3) +
         xlab(NULL) +
         ylab(NULL) +
         theme(axis.ticks=element_blank(),
               axis.text=element_blank(),
               ...)
    #p <- ggplot(data=data, aes(x=x,y=y)) +
    #     geom_star(aes(starshape=group),fill="red", size=3, show.legend=FALSE) +
    #     geom_text(aes(label=names(starshape_table[seq_len(30)])),
    #                   nudge_y=0.3, size=3, fontface="bold.italic") +
    #     geom_text(aes(label=as.character(seq_len(30))), nudge_y=-0.2) + 
    #     scale_starshape_manual(values=c(seq_len(30))) +
    #     xlim(0.5, 5.5) + ylim(0.5, 5.5) +
    #     xlab(NULL) + ylab(NULL) +
    #     theme(axis.ticks=element_blank(),
    #           axis.text=element_blank(),...) 
    p
}

#' @importFrom utils getFromNamespace
rd_aesthetics <- getFromNamespace("rd_aesthetics", "ggplot2")

"%||%" <- function(a, b) {
    if (!is.null(a)) a else b
}

#' @import cli
