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

#' Apply noize to the image.
#'
#' @param img applied \code{matrix} or \code{array} expressing the image.
#' @param salt.rate salt (bleaching) rate on the each pixels.
#' @param papper.rate papper (darking) rate on the each pixels.
#'
#' @importFrom assertthat is.number
#' @importFrom stats runif
#' @importFrom myfs is.prob
#'
#' @return image applied noize.
#' @export
#'
noizeSaltPapper <- function(img, salt.rate = 0.01, papper.rate = 0.01){
  assert_that(is.matrix(img) || is.array(img))
  assert_that(is.prob(salt.rate))
  assert_that(is.prob(papper.rate))

  r <- runif(nrow(img)*ncol(img))
  # salt noize flag
  nz <- ifelse(r < (salt.rate + papper.rate), 1, 0)
  # papper noize flag
  nz <- ifelse(r < papper.rate, -1, nz)

  # applying noize and normalization
  nzimg <- img + nz
  nzimg <- ifelse(nzimg > 1, 1, nzimg)
  nzimg <- ifelse(nzimg < 0, 0, nzimg)

  dim(nzimg) <- dim(img)

  return(nzimg)
}
