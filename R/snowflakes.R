snowflakes <-
function(xCoor, yCoor, radius, orientation = pi/6, deltaCoef = 15, color="#00007744", anotherColor = color, aspectRatio = NULL, seeds = NULL){
	if(is.null(seeds)){
		seeds = sample(x = 1:(length(yCoor)*10), size = length(yCoor))
	}else{
		seeds = rep(seeds, length.out=length(xCoor))
	}
	radius = rep(radius, length.out=length(xCoor))
	orientation = rep(orientation, length.out=length(xCoor))
	color = rep(color, length.out=length(xCoor))
	anotherColor = rep(anotherColor, length.out=length(xCoor))
  deltaCoef = rep(deltaCoef, length.out=length(xCoor))
	delta = radius/deltaCoef
  edgeRadius = NULL ## it used to be an argument but I've changed my mind
	if(is.null(edgeRadius)){
		edgeRadius = radius/runif(length(radius), 4, 5)
	}
	if(is.null(aspectRatio)){
  ######## fix the aspect ratio: reference: https://stat.ethz.ch/pipermail/r-help/2005-October/080598.html
		w <- par("pin")[1]/diff(par("usr")[1:2])
		h <- par("pin")[2]/diff(par("usr")[3:4])
		aspectRatio <- rep(w/h, length.out=length(xCoor))
	}else{
		aspectRatio <- rep(aspectRatio, length.out=length(xCoor))
	}
  aspectAdjustedOrientation = orientation
  ### under construction
  #aspectAdjustedOrientation = asin(sin(orientation)*aspectRatio)
	if(any(deltaCoef <= 0)) stop("deltaCoef has to be positive")
  if(length(unique(c(length(xCoor), length(yCoor), length(radius))))>2) stop("xCoor, yCoor, radius have to have the same length")
	#####################################  loop for each snowflake
	for (i in 1:length(xCoor)){
		#points(xCoor[i], yCoor[i])
		#cat("plotting", xCoor[i], yCoor[i], i, radius[i], color[i], anotherColor, "\n")
		set.seed(seeds[i])
		samplingForHexRadius = rbeta(20, 1, 4)/1
		edgeCoorList = snowflakeWithHex(center = c(xCoor[i], yCoor[i]), radius = radius[i], hexRadius = radius[i]*samplingForHexRadius, orientation = aspectAdjustedOrientation[i], edgeRadius=edgeRadius[i], delta = delta[i], color=color[i], anotherColor = anotherColor[i], aspectRatio = aspectRatio[i])
		#####################################  loop for each level of smaller cristal clusters
    newEdgeRadius = edgeRadius[i]*c(1, runif(length(edgeCoorList)-1, 2*delta[i], .5*radius/edgeRadius[i]))
		
		for (edgeInd in 1:length(edgeCoorList)){
			samplingForNewHexRadiusEdges = rbeta(10, 1, 4)/1
	    hexRadiusEdges = newEdgeRadius[edgeInd]*samplingForNewHexRadiusEdges
			
			#####################################  loop for each smaller cristal cluster on a given level
			for (j in 1:nrow(edgeCoorList[[edgeInd]])){
				edgeX = edgeCoorList[[edgeInd]][j,'edgeX']
				edgeY = edgeCoorList[[edgeInd]][j,'edgeY']
				snowflakeWithHex(center = c(edgeX, edgeY), radius = newEdgeRadius[edgeInd], orientation = aspectAdjustedOrientation[i], hexRadius = hexRadiusEdges, delta = delta[i], color=color[i], anotherColor = anotherColor[i], aspectRatio = aspectRatio[i])
		  }
	  }
  }
	seeds
}
