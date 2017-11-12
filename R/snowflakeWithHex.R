snowflakeWithHex <-
function(center = c(1, 1), radius = 1, hexRadius = radius*rbeta(20, 1, 3)/1.5, edgeRadius=radius/5, orientation, delta = radius/100, color="#00007744", anotherColor = color, aspectRatio=1){
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
