#' @title Create the coordinates of regular polygens
#' @param t the polar coordinate angle at (0, 0)
#' @param r the radius of regular polygens
#' @param n the number of side of regular polygens
#' @keywords internal
# reference https://math.stackexchange.com/questions/1990504/how-to-find-the-coordinates-of-the-vertices-of-a-pentagon-centered-at-the-origin
polygens <- function(t, n, r){
    x = r * cos(t + seq_len(n)*2*pi/n) + 0.5
    y = r * sin(t + seq_len(n)*2*pi/n) + 0.5
    polygenscoord <- structure(list(x=x, y=y),class="polygenscoord")
    return (polygenscoord)
}
