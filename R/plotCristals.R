plotCristals <-
function(centers, radius, angle, delta, aspectRatio=1, color = "#44444444"){
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
