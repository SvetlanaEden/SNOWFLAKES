plotHexagons <-
function(center, radius, orientation, delta, aspectRatio=1, color = "#44444444"){
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
