#' Create a calibrated color ramp object from color-tiepoint data.frames
#'
#' Create ramp palettes from fixed color positions.
#'
#' The function creates objects of the S3 class \code{calibramp}. The \code{calibramp}-class lists have three elements: \code{col} hexadecimal color values, \code{mid}: z-values of midpoints (one for every color), and \code{breaks}: separator borders between color values.
#' Color interpolation will be executed linearly, using \code{\link[grDevices]{colorRampPalette}}, the order of the values will be forced to ascending, the values in \code{mid} will be halfway between breaks.
#' @param x A \code{data.frame} object with two columns: \code{color} for hexadecimal color values, \code{z} for their position.
#' @param n A single \code{integer} number.
#' @param color A \code{chraracter} value, the column name of the colors in \code{x}, defaults to \code{"color"}. Alternatively, a \code{character} vector of hexadecimal color values, with the same length as \code{z}.
#' @param z A \code{character} value, the column name of the values in \code{x}, defaults to \code{"z"}. Alternatively, a \code{numeric} vector of color values (must have the same length as \code{color}.
#' @param ... Arguments passed to the \code{colorRampPalette} function.
#'
#' @return A \code{calibramp}-class object (see description above).
#' @examples
#' library(rampage)
#' data(topos)
#' ramp <- expand(topos$havanna2, n=200)
#' plot(ramp)
#'
#' @export
expand <- function(x=NULL, n, color="color", z="z", ...){
	if(is.null(n)) stop("Shame! ... Shame! ... Shame!")
	if(length(n)!=1) stop("Provide a single positive integer 'n'!")
	if(n%%1!=0) stop("Only integer values are allowed for 'n'!")

	if(is.numeric(z)){
		if(!is.null(x)) stop("The argument 'z' has numeric values, 'x' will be ignored.")
		if(length(color) != length(z)) stop("The argument 'color' and 'z' have to have the same length.")

	}else{
		if(is.null(x)) stop("The argument 'x' is required if numeric 'z' and a character 'color' vector are not provided separately.")
		if(!inherits(x, "data.frame")) stop("The argument 'x' has to be a 'data.frame'-class object.")
		if(any(!c(color, z)%in%colnames(x))) stop("'color' and 'z' 'x' must be the column names of 'x'.")
		# separate data
		color <- x[,color]
		z <- x[,z]
	}
	if(any(is.na(z))) stop("'z' must not contain missing values!")

	# test for duplicates in z
	if(any(duplicated(z))) stop("'z' must not contain duplicates!")

	# enforce ordering
	zOrder <- order(z)
	z <- z[zOrder]
	color <- color[zOrder]

	# generate sequence between minimum and maximum
	centers <- seq(min(z), max(z), length.out=n)
	# difference between the bins
	di <- diff(centers)[1]

	# the breaks (ready for return)
	breaks <- c(centers-di/2, centers[length(centers)]+di/2)

	# find the closest midpoint to every given z value
	col <- rep(NA, n)

	# where are the given color values
	index <- rep(NA, length(z))
	for(i in 1:length(z)){
		# differences
		absDiffs <- abs(z[i] - centers)
		index[i] <- which(min(absDiffs)==absDiffs)[1]
	}

	# construct the ramps
	for(i in 2:length(index)){
		# the indices where this should go
		fit <- index[i-1]:index[i]
		# generate ramp generator function
		thisChunkFunc <- grDevices::colorRampPalette(c(color[i-1], color[i]), ...)
		# construct this part of the ramp
		colorSubset <- thisChunkFunc(length(fit))

		# fit this bit into the complete palette
		col[fit] <- colorSubset
	}

	# create final object
	y<- list(
		col=col,
		breaks=breaks,
		mid=centers
	)
	class(y) <- "calibramp"

	return(y)
}
