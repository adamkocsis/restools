#' Function to open graphical devices of all kind
#' 
#' This wrapper function will open image devices, without having to call different functions.
#' 
#' @param name Character string. The name of the file without the extension. Leaving this the default \code{NULL} will open an x11 window.
#' @param path Character strings. The path to the file that you want. I decided to keep this separate as many times I use separater variables to indicate folder structure.
#' @param format Character string. The used device/file extension. If left as \code{NULL}, then the function will open an x11 window.
#' @param width The width of the plot
#' @param height The height of the plot.
#' @rdname openimage
#' @export
openimage<-function(name=NULL, path="", format=NULL, width=NULL, height=NULL){
	# save the image in a certain format
	if(!is.null(format)){
		if(format=="svg"){
			grDevices::svg(paste(path, name,".", format, sep=""), width=width, height=height)
		}

#		if(format=="jpg"){
#			grDevices::jpg(file.path(path, "/", name,".", format), width=width, height=height)
#		}
#
#		if(format=="png"){
#			grDevices::png(file.path(path, "/", name,".", format), width=width, height=height)
#		}

		if(format=="pdf"){
			grDevices::pdf(paste(path,  name,".", format, sep=""), width=width, height=height)
		}
	}

	# open graphics window
	if(is.null(name) | is.null(format)){
		if(is.null(width)) width <- 8
		if(is.null(height)) width <- 8
		grDevices::x11(width=width, height=height)
	}
}

#' Put a panel indicator on an R plot
#' 
#' Panel indicator function.
#' 
#' The function is stolen from:
#' https://waterprogramming.wordpress.com/2015/12/02/easy-labels-for-multi-panel-plots-in-r/
#' with minor modifications.
#' @param label Character string. The label you want to put on the panel.
#' @param location Character string, similar to that of legend().
#' @param x Numeric, the x parameter of the label.
#' @param y Numeric, the y parameter of the label. 
#' @param offset offset from the calculated coordinates.
#' @param ... arguments passed to the text() function.
#' @rdname panelID
#' @export
panelID <- function(label, location="topleft", x=NULL, y=NULL, 
                           offset=c(0, 0), ...) {
  if(length(label) > 1) {
    warning("length(label) > 1, using label[1]")
  }
  if(is.null(x) | is.null(y)) {
    coords <- switch(location,
                     topleft = c(0.015,0.98),
                     topcenter = c(0.5525,0.98),
                     topright = c(0.985, 0.98),
                     bottomleft = c(0.015, 0.02), 
                     bottomcenter = c(0.5525, 0.02), 
                     bottomright = c(0.985, 0.02),
                     c(0.015, 0.98) )
  } else {
    coords <- c(x,y)
  }
  this.x <- graphics::grconvertX(coords[1] + offset[1], from="nfc", to="user")
  this.y <- graphics::grconvertY(coords[2] + offset[2], from="nfc", to="user")
  graphics::text(labels=label[1], x=this.x, y=this.y, xpd=T, ...)
}



#' Predefined multipanel plotting. 
#' 
#' The function is useful for the effective combination of multipanel plots. 
#' 
#' @param a Expression or path (as character string). Panel A plotting insturctions.
#' @param b Expression or path (as character string). Panel B plotting insturctions.
#' @param format The extension of the final image/the used plotting device.
#' @param path Character string, the path in which the final plot should be placed.
#' @param width The width of the plot.
#' @param height Integer vector. The heights of the plots in inches.
#' @param pind Character vector of the panel identifiers.
#' @rdname pan
#' @export
pan11 <- function(a, name=NULL, format=NULL, path=NULL, width=NULL, height=NULL, pind="small"){
	# 1. open image
	openimage(name=name, format=format, path=path, width=width, height=height)

	# 2. evaluate image expression
	if(is.expression(a)) eval(a)

	# 3. if format is not NULL, close the device
	if(!is.null(format) & !is.null(name)) grDevices::dev.off()

}

#' @param outmai Numeric vector, the outer margins of the plot in inches (as in par()$mai).
#' @param intop Numeric value. The top margin of the lower plot in inches. If outmai is NULL then the top margin of the plot.
#' @param inbot Numeric value. The bottom marign of the upper plot. 
#' @rdname pan
#' @export
pan21 <- function(a,b,name=NULL, format=NULL, path=NULL, width=NULL, height=NULL, pind=c("A", "B"), outmai=NULL, intop=NULL, inbot=NULL, pcx=1){

	
	if(is.null(inbot)) inbot <- 0
	if(is.null(intop)) intop <- 2.1/5

	# default height, based on valuable plotting area
	if(is.null(width)) width<-6
	if(is.null(height)){
		heightOrig<-c(width,width)
	}else{
		heightOrig <- height
	}

	# use R default plotting margins as a template
	origmai <- c(1.02, 0.82, 0.82, 0.42)

	# set the outer plot margins
	if(!is.null(outmai)){
		if(length(outmai)!=4) stop("you need 4 numeric margin values")

	}else{
		outmai <- c(1.02, 0.82, intop, 0.42)
	}

	# valueable plotting area
	plotar <- heightOrig-origmai[1]-origmai[3]

	# the calculated height
	height<- c(
		outmai[3]+inbot+plotar[1],
		outmai[1]+intop+plotar[2]
	)

	# mod
	maxHeight <- ceiling(sum(height))
	diffh<- maxHeight-sum(height)

	height <- c(height,diffh)


	# 1. open image
	openimage(name=name, format=format, path=path, width=width, height=maxHeight)

	# structure plot
	ratio <- matrix(c(1,2,3), ncol=1)
	#layout(ratio)
	graphics::layout(mat=ratio, heights=height*100)


	
	# 2call the plots, one by one
	# panel a
		graphics::par(mai=c(inbot, outmai[2:4]))
		
		if(is.character(a)) source(a)
		if(is.expression(a)) eval(a)

		#put the panel id on it
		panelID(pind[1],  cex=height[1]*pcx, pos=4,, offset=c(0, -0.05))

	# panel b
		graphics::par(mai=c(outmai[1:2], intop, outmai[4]))
		
		if(is.character(b)) source(b)
		if(is.expression(b)) eval(b)

		#put the panel id on it
		panelID(pind[2],  cex=height[1]*pcx, pos=4, offset=c(0, -0.05))

	# 3. if format is not NULL, close the device
	if(!is.null(format) & !is.null(name)) dev.off()
}







#	# height and widht are vectors!
#	pan22 <- function(a,b,c,d, name=NULL, format=NULL, path=NULL, width=NULL, height=NULL, pind="small",){
#	
#	
#	
#	
#	
#	
#	
#	}


#	# condensensed layout
#	clay <- function(x){
#		# list(c(2,3), c(1,2))
#	
#	}