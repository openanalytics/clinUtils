% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots-palettes.R
\name{clinColors}
\alias{clinColors}
\title{Colors of 'clinUtils'}
\usage{
clinColors(n, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
}
\arguments{
\item{n}{The number of colors (\eqn{\ge 1}) to be in the palette.}

\item{alpha}{The alpha transparency, a number in [0,1], see argument alpha in
\code{\link[grDevices]{hsv}}.}

\item{begin}{The (corrected) hue in [0,1] at which the color map begins.}

\item{end}{The (corrected) hue in [0,1] at which the color map ends.}

\item{direction}{Sets the order of colors in the scale. If 1, the default,
colors are ordered from darkest to lightest. If -1, the order of colors is
reversed.}

\item{option}{see similar parameter in \code{\link[viridisLite]{viridis}}}
}
\value{
The \code{\link[viridisLite]{viridis}} function.
}
\description{
Default color palette is the color-blind viridis palette.
See documentation of \code{\link[viridisLite]{viridis}}.
}
