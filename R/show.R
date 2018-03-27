#' Plot a \code{matrix} or \code{array} expressing a image.
#'
#' @param img showed \code{matrix} or \code{array} expressing a image.
#' @param ... arguments to be passed to plot function.
#'
#' @importFrom graphics rasterImage
#' @importFrom graphics plot
#' @importFrom assertthat assert_that
#'
#' @export
#' @examples
#' img <- jpeg::readJPEG(paste(R.home(),"/doc/html/logo.jpg",sep=""))
#' showImage(img)
#'
showImage <- function(img, ...){
  assert_that(is.matrix(img) || is.array(img))

  nr <- nrow(img)
  nc <- ncol(img)

  # normalization
  if(min(img) < 0) img[which(img < 0)] <- 0
  if(max(img) > 1) img <- img/max(img)

  # overloads img
  if (class(img) == "matrix"){
    if(class(img[1, 1]) == "logical") img <- apply(img, 2, as.numeric)
    img <- array(as.vector(img), dim = c(nr, nc, 3))
  }

  # process ...
  args <- list(...)
  args$x <- 0
  args$type <- "n"
  args$xlab <- ""
  args$ylab <- ""
  args$xlim <- c(0, nc)
  args$ylim <- c(nr, 0)
  if(is.null(args$cex.axis)) args$cex.axis <- 1.5

  do.call(plot,args)
  graphics::rasterImage(img, 0, nr, nc, 0)
}
