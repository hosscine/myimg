#' Plot a image with `matrix` or `array` formed.
#'
#' @param img image `matrix` or `array`.
#' @param ... optional arguments to be passed to [plot()] function.
#' @importFrom assertthat assert_that
#' @export
#' @examples
#' img <- jpeg::readJPEG(paste(R.home(),"/doc/html/logo.jpg",sep=""))
#' showImage(img)
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

  # process ... for plot
  elp <- myfs::overwriteEllipsis(..., x = 0, type = "n", xlim = c(0, nc), ylim = c(nr, 0))
  elp <- myfs::softwriteEllipsis(..., append = elp, xlab = "", ylab = "", cex.axis = 1.3)

  do.call(graphics::plot, elp)
  graphics::rasterImage(img, 0, nr, nc, 0)
}


#' Plot listed images at once.
#'
#' @param lst image list.
#' @param row_col number of images that is plot at onece.
#' if `row_col == 3`, 9 images plot as 3 times 3 layout.
#' if `row_col == c(4,5)`, 20 images plot as 4 times 5 layout.
#' @param plot.mar margins of each images.
#'   Default is `c(2, 2, 2, 1)`, indicates top, left, bottom, right, respectively.
#' @param zero.mar if `T`, `plot.mar` is set as `c(1, 0, 0, 0)`.
#'   When `titles` is `NULL`, `plot.mar` is set as `c(0, 0, 0, 0)`.
#' @param titles titles for each image. To specify nothing, set as `NULL`.
#' @param ... optional argments to be passed to [plot()] function.
#' @seealso [showImage()]
#' @export
showImageList <- function(lst, row_col = 3, titles = 1:length(lst),
                          plot.mar = c(2, 2, 2, 1), zero.mar = F, ...){
  if(!assertthat::is.count(row_col) && !(length(row_col ==2 && is.numeric(row_col))))
    stop("row_col must be length 2 or 3 and be nature number")
  if(length(row_col) == 1) row_col <- c(row_col, row_col)
  graphics::par(mfrow = row_col)

  axes <- T
  if(zero.mar) {
    axes <- F
    if (is.null(titles)) myfs::setMargin(top.4 = 0, left.4 = 0, bottom.5 = 0, right.2 = 0)
    else myfs::setMargin(top.4 = 1, left.4 = 0, bottom.5 = 0, right.2 = 0)
  }
  else  myfs::setMargin(top.4 = plot.mar[1], left.4 = plot.mar[2],
                        bottom.5 = plot.mar[3], right.2 = plot.mar[4])

  i <- 0
  n <- 0
  repeat{
    for (i in (n + 1):(n + prod(row_col))){
      if(i > length(lst)) break
      showImage(lst[[i]], main = titles[i], axes = axes, ...)
    }
    if(i > length(lst)) break
    n <- i
    if(n != length(lst)) myfs::keywait()
  }
}

#' Wlot image without axis.
#'
#' @param img image.
#' @param ... optional argments to be passed to [plot()] function.
#' @seealso [showImage()]
#' @export
showRawImage <- function(img, ...) {
  myfs::setMarginZero()
  showImage(img, axes = F, ann = F)
  myfs::setMargin()
}
