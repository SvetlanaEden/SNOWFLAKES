
plotCristals = function(centers, radius, angle, delta, aspectRatio=1, color = "#44444444"){
	  ### centers is a nx2 matri of x and y
	radius = rep(radius, length.out=nrow(centers))
	angle = rep(angle, length.out=nrow(centers))
	delta = rep(delta, length.out=nrow(centers))/2
	for (i in 1:nrow(centers)){
		matrix1 = matrix(centers[i,], nrow=3, ncol=2, byrow=TRUE)
    threeCenters = matrix1 + delta[i]*matrix(c(sin(angle[i]), cos(angle[i])*aspectRatio, 0, 0, -sin(angle[i]), -cos(angle[i])*aspectRatio), ncol=2, byrow=TRUE)
		#segments(x0=segmentStartX, y0=segmentStartY, x1 = segmentEndX, y1 = segmentEndY, col=color, lwd=lwd)
		allCenters = rbind(threeCenters, threeCenters[3:1,])
		adjRadius = radius[i] + c(0, delta[i]*tan(pi/6), 0, 0, delta[i]*tan(pi/6), 0)
		signVector = c(-1, -1, -1, 1, 1, 1)
		shiftMatr = cbind(adjRadius*cos(angle[i])*signVector, -adjRadius*sin(angle[i])*signVector*aspectRatio)
    polygonCoor = allCenters + shiftMatr
		polygon(x = polygonCoor[c(1:6, 1), 1], y = polygonCoor[c(1:6, 1), 2], col = color, border = NA)
	}
}

plotHexagons = function(center, radius, orientation, delta, aspectRatio=1, color = "#44444444"){
	### centers is a nx2 matri of x and y
	if (length(center)==2) center = matrix(center, nrow=1)
	radius = rep(radius, length.out=nrow(center))
	hexDelta = delta/(cos(pi/6))
	angle = (0:5)*(pi/3) + orientation
	xInner = center[1] + max(0, radius-hexDelta/2)*cos(angle)
	yInner = center[2] + max(0, radius-hexDelta/2)*sin(angle)*aspectRatio
	xOuter = center[1] + (radius+hexDelta/2)*cos(angle)
	yOuter = center[2] + (radius+hexDelta/2)*sin(angle)*aspectRatio
	#cat(c(xInner[c(1:6,1)], xOuter[c(6:1,6)]), "\n")
	polygon(x = c(xInner[c(1:6,1)], xOuter[c(6:1,6)]), y = c(yInner[c(1:6,1)], yOuter[c(6:1,6)]), col = color, border = NA)
}

snowflakeWithHex = function(center = c(1, 1), radius = 1, hexRadius = radius*rbeta(20, 1, 3)/1.5, edgeRadius=radius/5, orientation, delta = radius/100, color="#00007744", anotherColor = color, aspectRatio=1){
  ### orientation - from 0    (one of the cristal is parallel to X axis)
  ### orientation -   to pi/6 (one of the cristal is parallel to Y axis)
	### hexRadius - cannot be greater than radius
	if (max(hexRadius)>radius) stop("hexRadius cannot be greater than radius")
	if (edgeRadius>radius) stop("edgeRadius cannot be greater than radius")
	angle = (0:2)*(pi/3) + orientation
  centers = matrix(center, ncol=2, nrow=3, byrow=TRUE)
	plotCristals(centers, radius, angle=angle, delta=delta, aspectRatio=aspectRatio, color = anotherColor)

	for (i in 1:length(hexRadius)){
		plotHexagons(center=center, radius=hexRadius[i], orientation=-orientation, delta=delta, aspectRatio=aspectRatio, color = color)
	}
	
	#### return the centers for edges:
  #### sample number of side stars:
	numOfSideStars = sample(3:5, 1, prob = c(0.4, 0.35, 0.25))
	#numOfSideStars = sample(c(1, 1), 1, prob = c(0.35, 0.25))

  ############################## sample radii for new cristal clusters.
	#edgeRadii = c(edgeRadius, rbeta(numOfSideStars - 1, 4, 2)*(radius))
	#radiiFromCenter = radius - c(edgeRadius, runif(numOfSideStars - 1, min(2*edgeRadius, radius), radius))
	radiiFromCenter = c(radius - edgeRadius, rbeta(numOfSideStars - 1, 3, 3) * max(0, radius - 1.5*edgeRadius))

	angle = (0:5)*(pi/3) - orientation
	resList = list()
	for (i in 1:numOfSideStars){
		xNew = center[1] + radiiFromCenter[i]*cos(angle)
		yNew = center[2] + radiiFromCenter[i]*sin(angle)*aspectRatio		
	  resList[[i]] = data.frame(edgeX = xNew, edgeY = yNew)
  }
	resList
}

snowflakes = function(xCoor, yCoor, radius, orientation = pi/6, edgeRadius = NULL, deltaCoef = 15, color="#00007744", anotherColor = color, aspectRatio = NULL, seeds = NULL){
	if(is.null(seeds)){
		seeds = sample(1:(length(yCoor)*10))
	}else{
		seeds = rep(seeds, length.out=length(xCoor))
	}
	radius = rep(radius, length.out=length(xCoor))
	orientation = rep(orientation, length.out=length(xCoor))
	color = rep(color, length.out=length(xCoor))
	delta = radius/deltaCoef
	if(is.null(edgeRadius)){
		edgeRadius = radius/runif(length(radius), 4, 5)
	}
	if(is.null(aspectRatio)){
  ######## fix the aspect ratio: reference: https://stat.ethz.ch/pipermail/r-help/2005-October/080598.html
		w <- par("pin")[1]/diff(par("usr")[1:2])
		h <- par("pin")[2]/diff(par("usr")[3:4])
		aspectRatio <- w/h
	}
	if(length(unique(c(length(xCoor), length(yCoor), length(radius))))>2) stop("xCoor, yCoor, radius have to have the same length")
	#####################################  loop for each snowflake
	for (i in 1:length(xCoor)){
		#points(xCoor[i], yCoor[i])
		#cat("plotting", xCoor[i], yCoor[i], i, radius[i], color[i], anotherColor, "\n")
		set.seed(seeds[i])
		samplingForHexRadius = rbeta(20, 1, 4)/1
		edgeCoorList = snowflakeWithHex(center = c(xCoor[i], yCoor[i]), radius = radius[i], hexRadius = radius[i]*samplingForHexRadius, orientation = orientation[i], edgeRadius=edgeRadius[i], delta = delta[i], color=color[i], anotherColor = anotherColor, aspectRatio = aspectRatio)
		#####################################  loop for each level of smaller cristal clusters
    newEdgeRadius = edgeRadius[i]*c(1, runif(length(edgeCoorList)-1, 2*delta[i], .5*radius/edgeRadius[i]))
		
		for (edgeInd in 1:length(edgeCoorList)){
			samplingForNewHexRadiusEdges = rbeta(10, 1, 4)/1
	    hexRadiusEdges = newEdgeRadius[edgeInd]*samplingForNewHexRadiusEdges
			
			#####################################  loop for each smaller cristal cluster on a given level
			for (j in 1:nrow(edgeCoorList[[edgeInd]])){
				edgeX = edgeCoorList[[edgeInd]][j,'edgeX']
				edgeY = edgeCoorList[[edgeInd]][j,'edgeY']
				snowflakeWithHex(center = c(edgeX, edgeY), radius = newEdgeRadius[edgeInd], orientation = orientation[i], hexRadius = hexRadiusEdges, delta = delta[i], color=color[i], anotherColor = anotherColor, aspectRatio=aspectRatio)
		  }
	  }
  }
	seeds
}


setwd("/Users/svetlanaeden/Documents/GRADSCHOOL/2016_Sept/Leena/EXAMS/FINAL_PROJECT/SNOW_FLAKE_STUFF")


########### make transparent color
colorCoord = as.hexmode(col2rgb("blue"))
transpBlue =paste("#", paste(colorCoord, collapse=""), "44", sep="")
colorCoord = as.hexmode(col2rgb("white"))
transpWhite =paste("#", paste(colorCoord, collapse=""), "66", sep="")

w <- par("pin")[1]/diff(par("usr")[1:2])
h <- par("pin")[2]/diff(par("usr")[3:4])
aspectRatio <- w/h

set.seed(4)
selectSeeds = sample(1:100000, 100)
xCoor = rep(1:10, 10)
yCoor = rep(1:10, each = 10)
radiusCoef = .4
radius = radiusCoef

#pdf("sampledSnowflakes.pdf", width=7, height=7)
plot(range(xCoor), range(yCoor), type="n", axes = FALSE, ylab="", xlab="")

snowflakes(xCoor = xCoor[1], yCoor = yCoor[1], radius = radius, color = transpBlue, anotherColor = "#33333333", seeds = selectSeeds[1])
for (i in 2:100){
  snowflakes(xCoor = xCoor[i], yCoor = yCoor[i], radius = radius, color = transpBlue, anotherColor = "#33333333", seeds = selectSeeds[i])
	snowflakes(xCoor = xCoor[i-1], yCoor = yCoor[i-1], radius = radius, color = transpWhite, anotherColor = transpWhite, seeds = selectSeeds[i-1])
	text(xCoor[i-1], yCoor[i-1], selectSeeds[i-1], cex=.5, pos = 3)
}
snowflakes(xCoor = xCoor[i], yCoor = yCoor[i], radius = radius, color = transpWhite, anotherColor = transpWhite, seeds = selectSeeds[i])
text(xCoor[i-1], yCoor[i-1], selectSeeds[i], cex=.5, pos = 3)
#dev.off()




####### just one snowflake
plot(c(0,6), c(0, 2), type="n", axes = TRUE, ylab="", xlab="")
radius = radiusCoef*runif(1, .1, .15)
snowflakes(xCoor = c(1, 2, 3, 4, 5), yCoor = c(1, 1, 1, 1, 1), radius = radius, edgeRadius = radius/runif(1, 4, 5), orientation = rep(0, 5), deltaCoef = 15, seeds = c(73197, 8419, 14318, 83885, 29343))
snowflakes(xCoor = c(1, 2, 3, 4, 5), yCoor = c(1, 1, 1, 1, 1), radius = 1*rep(radius, 5), orientation = rep(0, 5), deltaCoef = 15, color = transpWhite, anotherColor = transpWhite, seeds = c(73197, 8419, 14318, 83885, 29343))

someSeeds = c(50497, 37906, 17295, 9250, 30595, 74555, 12669)

pdf("selectedSnowflakes.pdf", width=7, height=7)
par(mar = c(0, 0, 0, 0))
plot(c(0,length(someSeeds)+1), c(0, 2), type="n", axes = TRUE, ylab="", xlab="")
radiusCoef = .3
radius = radiusCoef*rep(1, length(someSeeds))

snowflakes(xCoor = 1:length(someSeeds), yCoor = rep(1, length(someSeeds)), radius = radius, deltaCoef = 15, orientation = 0, color = transpBlue, anotherColor = "#33333333", seeds = someSeeds)

snowflakes(xCoor = 1:length(someSeeds), yCoor = rep(1, length(someSeeds)), radius = radius, deltaCoef = 15, orientation = 0, color = transpWhite, anotherColor = transpWhite, seeds = someSeeds)
dev.off()


pdf("onePDFSnowflake.pdf", width=7, height=7)
par(mar = c(0, 0, 0, 0))
plot(c(.7, 1.3), c(.7, 1.3), type="n", axes = TRUE, ylab="", xlab="")
radiusCoef = .3
radius = radiusCoef*rep(1, length(someSeeds))

someSeeds = c(50497)
snowflakes(xCoor = 1:length(someSeeds), yCoor = rep(1, length(someSeeds)), radius = radius, deltaCoef = 15, orientation = pi/12, color = transpBlue, anotherColor = "#33333333", seeds = someSeeds)

snowflakes(xCoor = 1:length(someSeeds), yCoor = rep(1, length(someSeeds)), radius = radius, deltaCoef = 15, orientation = pi/12, color = transpWhite, anotherColor = transpWhite, seeds = someSeeds)
dev.off()


for (seeds in c(37906, 17295, 9250, 30595, 74555, 12669)){
pdf(paste("oneSVGSnowflake", seeds, ".pdf", sep=""), width=1, height=1)
  par(mar = c(0, 0, 0, 0))
  plot(c(.6, 1.5), c(.6, 1.5), type="n", axes = FALSE, ylab="", xlab="")
  polygon(c(.6, .6, 1.4, 1.4, .6), c(.4, 1.4, 1.4, .4, .4), col="black")
  radiusCoef = .3
  radius = radiusCoef*rep(1, length(someSeeds))
  someSeeds = seeds
  #snowflakes(xCoor = 1:length(someSeeds), yCoor = rep(1, length(someSeeds)), radius = radius, deltaCoef = 15, orientation = pi/12, color = transpBlue, anotherColor = "#33333333", seeds = someSeeds)
  
  snowflakes(xCoor = 1:length(someSeeds), yCoor = rep(1, length(someSeeds)), radius = radius, deltaCoef = 15, orientation = pi/12, color = "#33333333", anotherColor = "#33333333", seeds = someSeeds)

  colorCoord = as.hexmode(col2rgb("white"))
  transpWhite =paste("#", paste(colorCoord, collapse=""), "33", sep="")
  snowflakes(xCoor = 1:length(someSeeds), yCoor = rep(1, length(someSeeds)), radius = radius, deltaCoef = 15, orientation = pi/12, color = transpWhite, anotherColor = transpWhite, seeds = someSeeds)
dev.off()
}

svg("oneSVGSnowflakeDarker.svg", width=1, height=1)
  colorCoord = as.hexmode(col2rgb("white"))
  transpWhite =paste("#", paste(colorCoord, collapse=""), "11", sep="")
  par(mar = c(0, 0, 0, 0))
  plot(c(.7, 1.3), c(.7, 1.3), type="n", axes = FALSE, ylab="", xlab="")
  radiusCoef = .3
  radius = radiusCoef*rep(1, length(someSeeds))
  someSeeds = c(50497)
  #nowflakes(xCoor = 1:length(someSeeds), yCoor = rep(1, length(someSeeds)), radius = radius, deltaCoef = 15, orientation = pi/12, color = "#11111111", anotherColor = "#11111111", seeds = someSeeds)
  snowflakes(xCoor = 1:length(someSeeds), yCoor = rep(1, length(someSeeds)), radius = radius, deltaCoef = 15, orientation = pi/12, color = transpWhite, anotherColor = transpWhite, seeds = someSeeds)
dev.off()
