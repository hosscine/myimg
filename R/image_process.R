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

#' Converts a image to gray scaled.
#'
#' @param img target image formed \code{array}.
#'
#' @return converted image formed \code{matrix}.
#' @export
#'
convert2Gray <- function(img){
  assert_that(is.array(img))
  return(matrix((img[,,1] + img[,,2] + img[,,3]) / 3, nrow(img), ncol(img)))
}
