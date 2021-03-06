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

  direct <- atan2((end - start)[2], (end - start)[1])
  if(length.fixed) alpha <- length
  else alpha <- myfs::vnorm(end - start) * length / cos(angle)

  right <- end + c(cos(angle + direct + pi), sin(angle + direct + pi)) * alpha
  left <- end + c(cos(-angle + direct + pi), sin(-angle + direct + pi)) * alpha

  xy <- rbind(end, left, right)
  ...list <- list(...)
  elp <- myfs::overwriteEllipsis(..., x = xy[, 1], y = xy[, 2], border = ...list$col)
  do.call(graphics::polygon, elp)

  graphics::lines(c(x0, x1), c(y0, y1), ...)
}
