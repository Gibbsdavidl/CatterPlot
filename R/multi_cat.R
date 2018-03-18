#
# CatterPlots
#
# Copyright (c) 2016 David L Gibbs
# email: gibbsdavidl@gmail.com
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

#' Multi-cat!  More cats. More colors.
#'
#' @import stats
#' @import png
#' @import grDevices
#' @import graphics
#' @import utils
#' @param xs a vector of numbers
#' @param ys another vector of numbers
#' @param size the size of the cat (0.1 is a good starting point)
#' @param cat a vector of cats
#' @param catcolor a list of modifier vectors to the png matrix (try c(1,0,0,1))
#' @param linecolor color of plotted lines
#' @param type the type of plot ... justcats, or line
#' @param canvas the plotting area
#' @param xat vector of x-axis ticks
#' @param yat vector of y-axis ticks
#' @param ... additional parameters to pass to plot()
#'
#' @return a cat plot object... to plot more cats.
#' @examples
#' x <- -10:10
#' y <- -x^2 + 10
#' purr <- catplot(xs=x, ys=y, cat=c(1,2,3), catcolor=list(c(1,0,0,1), c(0,1,1,1)))
#' cats(purr, -x, -y, cat=4, catcolor=c(1,0,1,1))'
#' @export
multicat <- function(xs, ys,
					size=0.1, cat=c(1,2),
					catcolor = '#000000FF',
					linecolor=1, type="justcats",
					canvas=c(0,1.1,0,1.1), 
					xat = NULL,
					yat = NULL,
					...) {
	args <- list(...)

	plot(x=xs, y=ys, col=0, xaxt="n", yaxt="n", ...)
	par(usr=canvas)

	scaledData <- scaleData(xs,ys,args)
	xscale <- scaledData$xscale
	yscale <- scaledData$yscale

	# axis ticks
	if( is.null(xat) ) {
	    xat = seq(min(xscale), max(xscale), length.out = length(xscale))
	    xaxtlab = round(seq(min(xs), max(xs),length.out=length(xat)),1)
	} else {
	    atscale = scaleData(xat, NA, args)
	    xaxtlab = round(xat, 1)
	    xat = atscale$xscale # overwrite x-ticks with scaled version
	}
	
	if( is.null(yat) ) {
	    yat = seq(min(yscale), max(yscale), length.out = length(yscale))
	    yaxtlab = round(seq(min(ys), max(ys),length.out=length(yat)),1)
	} else {
	    atscale = scaleData(NA, yat, args)
	    yaxtlab = round(yat, 1)
	    yat = atscale$yscale # overwrite y-ticks with scaled version
	}
	
	axis(side=1, at=xat, labels=xaxtlab)
	axis(side=2, at=yat, labels=yaxtlab)

	if (type == "line") {
		points(x=xscale, y=yscale, col=linecolor, type="l")
	}

	cats <- rep_len(cat, length(xscale))
	catcolors <- rep_len(catcolor, length(xscale))

	# color the images
	imgs = mapply(colorMod, catdat[cats], catcolors, SIMPLIFY = FALSE)

	# draw them
	invisible(mapply(rasterImage, imgs, xscale - size/2, yscale - size/2, xscale + size/2, yscale + size/2, interpolate=TRUE))

	list(xs=xs, ys=ys, args=args, canvas=canvas)
}


multipoint <- function(xs, ys,
					ptsize=0.1,
					catcolor = '#000000FF',
					linecolor=1,
					canvas=c(0,1.1,0,1.1),
                     ...) {

	args <- list(...)

	plot(x=xs, y=ys, col=0, xaxt="n", yaxt="n", ...)
	par(usr=canvas)

	scaledData <- scaleData(xs,ys,args)
	xscale <- scaledData$xscale
	yscale <- scaledData$yscale

    points(x=xscale, y=yscale, col=catcolor, xaxt="n", yaxt="n", pch=15, cex=ptsize, ...)

	xat = seq(min(xscale), max(xscale), length.out=length(xscale))
	yat = seq(min(yscale), max(yscale), length.out=length(yscale))
	xaxtlab = round(seq(min(xs), max(xs),length.out=length(xat)),1)
	yaxtlab = round(seq(min(ys), max(ys),length.out=length(xat)),1)
	axis(side=1, at=xat, labels=xaxtlab)
	axis(side=2, at=yat, labels=yaxtlab)

	list(xs=xs, ys=ys, args=args, canvas=canvas)
}


#' Plot even more cats! In more colors and shapes!
#'
#' @param obj a catplot object, returned from catplot
#' @param xs a vector of numbers
#' @param ys another vector of numbers
#' @param size the size of the cat (0.1 is a good starting point)
#' @param cat the cat model, 1 through 12
#' @param catcolor a modifier vector to the png matrix (try c(1,0,0,1))
#' @param linecolor color of plotted lines
#' @param type the type of plot ... justcats, or line
#' @param yshift shifts the cat up or down, within the scaled space
#' @param xshift shifts the cat left or right, within the scaled space.
#' @param color whether or not to apply color to image(s)
#'
#' @return a cat plot object... to plot more cats.
#' @examples
#' x <- -10:10
#' y <- -x^2 + 10
#' purr <- multicat(xs=x, ys=y, cat=c(1,2,3), catcolor=c(0,1,1,1))
#' cats(purr, -x, -y, cat=4, catcolor=c(1,0,1,1))'
#' @export
morecats <- function(obj=NULL, xs, ys, size=0.1, cat=c(4,5,6), catcolor = c('#0000FFFF', '#00FF00FF'),
										linecolor=1, type="justcats", yshift=0, xshift=0, color=TRUE) {
	# needs a plot already up, and the catObj returned from it.
	if(is.null(obj)) {
		print("Please feed the cats!  cat_food <- catplot(...);  cats(cat_food, ...)")
	}

	scaledData <- catsScaleData(obj,xs,ys)
	xscale <- scaledData$xscale + xshift
	yscale <- scaledData$yscale + yshift

	if (type == "line") {
		points(x=xscale, y=yscale, col=linecolor, type="l")
	}


	cats <- rep_len(cat, length(xscale))
	catcolors <- rep_len(catcolor, length(xscale))

	# color the images
	if (color){
	  imgs = mapply(colorMod, catdat[cats], catcolors, SIMPLIFY = FALSE)
	} else {
	  imgs = catdat[cats]
	}

	# draw them
	invisible(mapply(rasterImage, imgs, xscale - size/2, yscale - size/2, xscale + size/2, yscale + size/2, interpolate=TRUE))

	cat(paste(apply(matrix(c(xscale - size/2, yscale - size/2, xscale + size/2, yscale + size/2), ncol=4),
	                1, paste, collapse='  '),
	          collapse='\n'))
}


morepoints <- function(obj=NULL, xs, ys, ptsize=0.1, catcolor = '#000000FF', yshift=0, xshift=0) {
	# needs a plot already up, and the catObj returned from it.
	if(is.null(obj)) {
		print("Please feed the cats!  cat_food <- catplot(...);  cats(cat_food, ...)")
	}

	scaledData <- catsScaleData(obj,xs,ys)
	xscale <- scaledData$xscale + xshift
	yscale <- scaledData$yscale + yshift

	points(x=xscale, y=yscale, col=catcolor, xaxt="n", yaxt="n", pch=15, cex=ptsize)
}
