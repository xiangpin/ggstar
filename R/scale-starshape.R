#' @importFrom ggplot2 discrete_scale
scale_starshape <- function(default=TRUE,...){
    discrete_scale("starshape", "starshape", starshape_pal(default), ...)
}

starshape_pal <- function(default=TRUE){
    force(default)
    function(n){
    if (n > 9) {
        msg <- paste("The starshape palette can deal with a maximum of 9 discrete ",
                     "values because more than 9 becomes difficult to discriminate; ",
                     "you have ", n, ". Consider specifying shapes manually if you ",
                     "must have them.", sep = "")
        warning(paste(strwrap(msg), collapse = "\n"), call. = FALSE)
    }
    if (default){
        c(1, 2, 4, 9, 3, 5, 6, 7, 8)[seq_len(n)]
    }else{
        c(1, 2, 3, 4, 5, 6, 7, 8, 9)
    }
}
}
