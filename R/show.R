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


#' Plot many listed image at once.
#'
#' @param lst image list.
#' @param row_col number of images that is plot at onece.
#' if \code{row_col == 3}, 9 images plot as 3 times 3 layout.
#' if \code{row_col == c(4,5)}, 20 images plot as 4 times 5 layout.
#' @param plot.mar margins of each images.
#' default is \code{c(2, 2, 2, 1), indicates top, left, bottom, right, respectively.}
#' @param zero.mar if \code{T}, plot.mar is set as \code{c(0, 0, 0, 0)}.
#' @param ... optional argments for plotting image.
#'
#' @importFrom assertthat is.count
#' @importFrom myfs setMargin
#' @importFrom myfs keywait
#'
#' @export
#'
showImageList <- function(lst, row_col = 3, plot.mar = c(2, 2, 2, 1), zero.mar = F, ...){
  if(!is.count(row_col) && !(length(row_col ==2 && is.numeric(row_col))))
    stop("row_col must be length 2 or 3 and be nature number")
  if(length(row_col) == 1) row_col <- c(row_col, row_col)
  par(mfrow = row_col)
  if(zero.mar) setMargin(top.4 = 0, left.4 = 0, bottom.5 = 0, right.2 = 0)
  else  setMargin(top.4 = plot.mar[1], left.4 = plot.mar[2],
                  bottom.5 = plot.mar[3], right.2 = plot.mar[4])

  i <- 0
  n <- 1
  repeat{
    for (i in n:(n + prod(row_col) - 1)){
      if(i > length(lst)) break
      showImage(lst[[i]], ...)
    }
    if(i > length(lst)) break
    n <- i
    if(n != length(lst)) keywait()
  }
}
