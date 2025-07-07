#' Limit range of values in a \code{SpatRaster} object
#'
#' Shorthand for limiting maximum and minimum values in a \code{SpatRaster}-class object.
#'
#' @param x Object to limit, a \code{SpatRaster}-class object.
#' @param y Either a range (i.e. a \code{numeric} vector with two values), or \code{data.frame} with positioned color values (column \code{z} indicates values), or a calibrated ramp (e.g. produced with \code{expand}).
#' @param min If \code{y} is not given, the minimum value to have in the \code{data.frame}.
#' @param max If \code{y} is not given, the maximum value to have in the \code{data.frame}.
#' @return A \code{SpatRaster} object.
#' 
#' @export
#' @examples
#' # This function relies on the terra extension
#' if(requireNamespace("terra", quietly=TRUE)){
#'  library(terra)
#' 	# Example 1. Using specific values
#' 	# a SpatRaster object
#' 	r<- terra::rast()
#' 	# populate with a Gaussian distribution
#' 	terra::values(r) <- rnorm(terra::ncell(r), 0.5,1 )
#' 	# and limit
#' 	rLimit <- limit(r, min=-0.2, max=0.2)
#' 	plot(rLimit)
#'
#'
#' 	# Example 2. Using an expanded color ramp
#' 	# Create a data.frame
#' 	df <- data.frame(
#' 		z=c(-1, -0.2, 0, 0.2, 1),
#' 		color=rev(gradinv(5))
#' 	)
#' 	ramp <- expand(df, n=200)
#' 	rLimited <- limit(r, y=ramp)
#' 	# default
#' 	plot(rLimited)
#'
#' 	# manual ramping.
#' 	plot(rLimited, breaks=ramp$breaks, col=ramp$col,
#' 		legend=FALSE)
#'
#' 	# temporary solution for manual legend
#' 	# Marginal ramps will be implemented later
#' 	ramplegend(x=140, y=90, ramp=ramp, cex=0.5,
#' 		at=c(-1, 0, 1), label=c("< -1", 0, "> +1"))
#'
#'
#' }
limit <- function(x, y=NULL, min=NULL, max=NULL){

	if(!requireNamespace("terra", quietly=TRUE)) stop("This function requires the 'terra' package. ")

	# defened against missing
	if(!is.null(min)) if(length(min)>1 | any(!is.finite(min)))
		stop("The 'min' argument has to be a single, finite, numeric value.")

	if(!is.null(max)) if(length(max)>1 | any(!is.finite(max)))
		stop("The 'max' argument has to be a single, finite, numeric value.")

	# if it is a calibrated ramp, reduce to range of breaks
	if(inherits(y, "calibramp"))  y <- range(y$mid)

	# if it is a tiepoint data.frame, reduce to range of z
	if(is.data.frame(y)){
		if(all("z"!=colnames(y))) stop("The data.frame argument 'y' has to have a column called \"z\".")
		y <- range(y$z)
	}

	if(length(y)>2 | any(!is.finite(y))) stop("Invalid 'y' argument. ")

	# if these were not given, assign them
	if(is.null(min)) min <- sort(y)[1]
	if(is.null(max)) max <- sort(y)[2]

	# do the limiting of the raster
	terra::values(x)[which(terra::values(x) > max)]<-max
	terra::values(x)[which(terra::values(x) < min)]<- min

	return(x)
}

#' Trimming a calibrated color ramp object.
#'
#' Modify the minimum and maximum values in a \code{calibramp}-class object produced by the \code{\link{expand}} function.
#'
#' @param x A calibrated color ramp (e.g. \code{calibramp}-class object.
#' @param low A single \code{numeric} value, the minimum value in the calibrated ramp.
#' @param high A single \code{numeric} value, the maximum value in the calibrated ramp.
#' @return A trimmed version of \code{x}, another \code{calibramp}-class object.
#' @export
#' @examples
#' data(paleomap)
#' trimmed <- trimramp(paleomap, low=-500, high=1500)
#' plot(trimmed)
trimramp <- function(x, low=NULL, high=NULL){
	if(is.null(low) & is.null(high)) stop("Please provide at least either a 'low' or a 'high' value.")
	# adjust the minimum
	if(!is.null(low)){
		if(length(low) > 1 | !is.numeric(low)) stop("Please provide a single numeric 'low' argument.")
		if(!is.finite(low)) stop("The 'low' argument has to be a finite number.")

		if(low > max(x$breaks)) stop("The 'low' argument must be lower than the highest value in the breaks.")
		# needs to be added
		if(low < min(x$breaks)){
			# adjust the low
			x$breaks[which.min(x$breaks)] <- low
			x$mid[which.min(x$mid)] <- mean(sort(x$breaks)[1:2])

		# need to remove some entries
		}else{
			# the breaks
			index <- which(low < x$breaks)
			x$breaks <- c(low, x$breaks[index])

			# keep the mids and the cols
			x$col <- x$col[index-1]
			x$mid <- x$mid[index-1]

			x$mid[which.min(x$mid)] <- mean(sort(x$breaks)[1:2])

		}
	}

	# adjust the maximum
	if(!is.null(high)){
		if(length(high) > 1 | !is.numeric(high)) stop("Please provide a single numeric 'high' argument.")
		if(!is.finite(high)) stop("The 'high' argument has to be a finite number.")
		if(high < min(x$breaks)) stop("The 'high' argument must be higher than the the lowest value in the breaks.")
		# needs to be added
		if(high >  max(x$breaks)){
			# adjust the low
			x$breaks[which.max(x$breaks)] <- high
			x$mid[which.max(x$mid)] <- mean(sort(x$breaks)[(length(x$breaks)-1):length(x$breaks)])

		# need to remove some entries
		}else{
			# the breaks
			index <- which(high > x$breaks)
			x$breaks <- c(x$breaks[index], high)

			# keep the mids and the cols
			x$col <- x$col[index]
			x$mid <- x$mid[index]

			x$mid[which.max(x$mid)] <- mean(sort(x$breaks)[(length(x$breaks)-1):length(x$breaks)])

		}
	}
	# return the changed object
	return(x)
}
