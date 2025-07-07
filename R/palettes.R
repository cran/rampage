#' Color gradient ramps
#'
#' Contains functions produced by the \code{\link[grDevices:colorRamp]{colorRampPalette}} function.
#' 
# \cr
#' You can also view single palettes individually. The following color palettes are implemented:
#' \itemize{
#' \item \code{gradinv()}: inverse heatmap, primarily intended to emphasize distinctions between no-change (yellow) and change (blue/red) cases.
#' Based on the color blindness simulator Coblis, the palette is very color disability-friendly, except for monochromacy/achromatopsia, where the two change scenarios (red and blue) become very difficult to distinguish.
#' Additional markings (e.g. labels indicating high and low) are recommended for these cases.
#' }
#' 
#' @return A character vector of color values.
#' @name ramps
#' @param n (\code{numeric}) Number of different colors to generate from the palette
#' 
NULL


#' An inverse heatmap
#' @rdname ramps
#' 
#' @export 
#' @examples
#' cols <- gradinv(20)
#' plot(1:20, col=cols, pch=16, cex=2)
gradinv <- grDevices::colorRampPalette(c("#33358a", "#76acce", "#fff99a",  "#e22c28", "#690720"))


#' Topographic color palettes with tiepoints
#'
#' The object contains \code{data.frame}-class objects to be used with the \code{\link{expand}} function to produce full calibrated color ramps.
#'
#' @format A \code{list} with 6 \code{data.frame} elements:
#' \describe{
#' \item{\code{demcmap}}{: The "demcmap" theme, based on MatLab's \code{demcmap}. }
#' \item{\code{etopo}}{: The "etopo" theme, approximate elevation-color assignments based on the the ETOPO Global Relief Model poster (https://www.ncei.noaa.gov/media/3340). }
#' \item{\code{jakarta}}{: The "Jakarta" theme, color values by Deviantart user \emph{Arcanographia}. }
#' \item{\code{havanna2}}{: The "Havanna-2" theme, color values by Deviantart user \emph{Arcanographia}.}
#' \item{\code{tokio1}}{: The "Tokio-1" theme, color values by Deviantart user \emph{Arcanographia}.}
#' \item{\code{zagreb}}{: The "Zagreb" theme, color values by Deviantart user \emph{Arcanographia}.}
#' }
#' @usage data(topos)
#'
#' @examples
#' data(topos)
#' jakExp <- expand(topos$jakarta, n=200)
#' plot(jakExp)
"topos"


#' Topographic gradient map color map of the PALEOMAP project
#'
#' The elevation to color bindings are the work of C. Scotese. Data from Scotese, C. R., Vérard, C., Burgener, L., Elling, R. P., & Kocsis, A. T. (2025). The Cretaceous World: Plate Tectonics, Paleogeography, and Paleoclimate. Geological Society, London, Special Publications, 544(1), SP544–2024..
#' @format A \code{calibramp}-class \code{list} with 3 \code{numeric}s:
#' \describe{
#' \item{\code{col}}{: The color levels as hexadecimal RGB values. }
#' \item{\code{breaks}}{: The boundaries for the individual levels.}
#' \item{\code{mid}}{: The mid values of the color levels.}
#' }
#' @source \url{https://zenodo.org/records/10659112}
#' @usage data(paleomap)
#'
"paleomap"

