#' Draws arrows between pairs of points with filling arrow head.
#'
#' @param x0 coordinates of points from which to draw.
#' @param y0 coordinates of points from which to draw.
#' @param x1 coordinates of points to which to draw.
#' @param y1 coordinates of points to which to draw.
#' @param length length of the edges of the arrow head (ratio towards length of arrow).
#' @param length.fixed if \code{T}, argment \code{length} indicates abusolutely length (not related to length of arrow).
#' @param angle angle from the shaft of the arrow to the edge of the arrow head.
#' @param ... graphical parameters such as the line characteristics.
#'
#' @importFrom myfs vnorm
#' @importFrom myfs overwriteEllipsis
#' @importFrom myfs angle
#'
#' @export
#'
arrows.fill <- function(x0, y0, x1, y1, length = 0.1, length.fixed = F, angle = pi/6, ...){
  assert_that(is.number(angle) && cos(angle) != 0)

  start <- c(x0,y0)
  end <- c(x1,y1)
  if(identical(start,end)) stop("start point and end point is same.")

  # direct <- pi + myfs::angle(end - start, c(1, 0))
  direct <- atan2((end - start)[2], (end - start)[1])

  debugText(end - start, direct)
  direct <- direct + pi
  if(length.fixed) alpha <- length
  else alpha <- myfs::vnorm(end - start) * length / cos(angle)

  right <- end + c(cos(angle + direct), sin(angle + direct)) * alpha
  left <- end + c(cos(-angle + direct), sin(-angle + direct)) * alpha

  xy <- rbind(end, left, right)
  debugText(end,left,right, angle/pi , direct/pi)
  ellipsis <- list(...)
  new.ellipsis <- overwriteEllipsis(..., x = xy[, 1], y = xy[, 2],
                                    col = ellipsis$col, border = ellipsis$col)
  do.call(graphics::polygon,new.ellipsis)

  graphics::lines(c(x0, x1), c(y0, y1), ...)
}
