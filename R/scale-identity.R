#' Use values without scaling for ggstar
#' 
#' @inheritDotParams ggplot2::continuous_scale
#' @param guide Guide to use for this scale. Defaults to `"none"`.
#' @seealso \code{\link[ggplot2]{scale_shape_identity}}
#' @importFrom ggplot2 continuous_scale ScaleContinuousIdentity
#' @return identical (default) starshape scale constructor
#' @export
scale_starshape_identity <- function(..., guide = "none") {
    sc <- continuous_scale(aesthetics="starshape", palette=scales::identity_pal(), transform="identity", ..., guide = guide,
                           super = ScaleContinuousIdentity)
    sc
}
