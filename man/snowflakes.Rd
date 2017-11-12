\name{snowflakes}
\alias{snowflakes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Plots Randomly Generated Snowflakes
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The function generates snowflakes with a given diameter, width of the crystal, color, and random seed. The snowflakes are plotted in such way that they always remain round, no matter what the aspect is of the plot is. The snowflakes can be created using transparent colors, including transparent white. This creates a special effect of a crystal and can be displayed on a darker background. The images of snowflakes can be saved as a svg file and used as a moving image when creating websites.
}
\usage{
snowflakes(xCoor, yCoor, radius, orientation = pi/6, edgeRadius = NULL, 
    deltaCoef = 15, color = "#00007744", anotherColor = color, 
    aspectRatio = NULL, seeds = NULL) 
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xCoor}{
  The X coordinate of the snowflake. This argument can be a vector when multiple snowflakes are plotted.
}
  \item{yCoor}{
  The Y coordinate of the snowflake. This argument can be a vector when multiple snowflakes are plotted.
}
  \item{radius}{
  The radius of the snowflake.
}
  \item{orientation}{
  The angle of the snowflake relatively to its center, measured in radians. Note that the rotation of the snowflake around its center can be visible only within a range from 0 to on sixth of a pi radians becasuse the snowflake has 6 axes.
}
  \item{edgeRadius}{
  The radius of the inner circle of the crystal.
}
  \item{deltaCoef}{
  By how much the radius is lager than the width of the crystal. The width of the crystal is computed as a ratio of the \code{"radius"} over \code{"deltaCoef"}
}
  \item{color}{
  The color of the snowflake (can be transparent).
}
  \item{anotherColor}{
  The color of a part of the snowflake to make it more interesting.
}
  \item{aspectRatio}{
  Aspect ratio chosen automatically so that the snowflake remains round. If changed, the snowflake might not be round any longer.
}
  \item{seeds}{
  Random seed that defines a unique snowflake given that other parameters are assigned to their defaults.
}
}
\details{
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
  Returns seeds that were used in order to generate snowflakes, so you can save them and use the ones that were responsible for generating the snowflakes that appealed to you the most.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Svetlana Eden <svetlana.eden@vanderbilt.edu>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
########### make transparent color
colorCoord = as.hexmode(col2rgb("blue"))
transpBlue =paste("#", paste(colorCoord, collapse=""), "44", sep="")
colorCoord = as.hexmode(col2rgb("white"))
transpWhite =paste("#", paste(colorCoord, collapse=""), "66", sep="")

plot(range(xCoor)+c(-1, 1), range(yCoor)+c(-1, 1), type="n", axes = FALSE, ylab="", xlab="")
snowflakes(xCoor = xCoor[1], yCoor = yCoor[1], radius = radius, color = transpBlue, anotherColor = "#33333333", seeds = selectSeeds[1])
for (i in 2:numSnowflPerRow^2){
  snowflakes(xCoor = xCoor[i], yCoor = yCoor[i], radius = radius, color = transpBlue, anotherColor = "#33333333", seeds = selectSeeds[i])
	snowflakes(xCoor = xCoor[i-1], yCoor = yCoor[i-1], radius = radius, color = transpWhite, anotherColor = transpWhite, seeds = selectSeeds[i-1])
	text(xCoor[i-1], yCoor[i-1], selectSeeds[i-1], cex=.5, pos = 3)
}
snowflakes(xCoor = xCoor[i], yCoor = yCoor[i], radius = radius, color = transpWhite, anotherColor = transpWhite, seeds = selectSeeds[i])
text(xCoor[i], yCoor[i], selectSeeds[i], cex=.5, pos = 3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~snowflakes }
\keyword{ ~plot }
