% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/show.R
\name{showImageList}
\alias{showImageList}
\title{Plot listed images at once.}
\usage{
showImageList(lst, row_col = 3, titles = 1:length(lst),
  plot.mar = c(2, 2, 2, 1), zero.mar = F, ...)
}
\arguments{
\item{lst}{image list.}

\item{row_col}{number of images that is plot at onece.
if `row_col == 3`, 9 images plot as 3 times 3 layout.
if `row_col == c(4,5)`, 20 images plot as 4 times 5 layout.}

\item{titles}{titles for each image. To specify nothing, set as `NULL`.}

\item{plot.mar}{margins of each images.
Default is `c(2, 2, 2, 1)`, indicates top, left, bottom, right, respectively.}

\item{zero.mar}{if `T`, `plot.mar` is set as `c(1, 0, 0, 0)`.
When `titles` is `NULL`, `plot.mar` is set as `c(0, 0, 0, 0)`.}

\item{...}{optional argments to be passed to [plot()] function.}
}
\description{
Plot listed images at once.
}
\seealso{
[showImage()]
}
