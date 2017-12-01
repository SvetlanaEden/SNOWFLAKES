\name{snowflakes}
\alias{snowflakes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Plots Randomly Generated Snowflakes
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The function generates and plots random snowflakes. Each snowflake is defined by a given diameter, width of the crystal, color, and random seed. Snowflakes are plotted in such way that they always remain round, no matter what the aspect ratio of the plot is. Snowflakes can be created using transparent colors, which create a more interesting and more realistic image. Images of the snowflakes can be separately saved in svg files and used in websites as static or animated images.
}
\usage{
snowflakes(xCoor, yCoor, radius, orientation = pi/6, 
    deltaCoef = 15, color = "#00007744", anotherColor = color, 
    aspectRatio = NULL, seeds = NULL) 
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xCoor}{
  The X coordinate of the snowflake. This argument may be a vector when multiple snowflakes are plotted.
}
  \item{yCoor}{
  The Y coordinate of the snowflake. This argument may be a vector when multiple snowflakes are plotted.
}
  \item{radius}{
  The radius of the snowflake. This argument may be a vector when multiple snowflakes are plotted.
}
  \item{orientation}{
  The angle of the snowflake relatively to its center, measured in radians. Note that the rotation of the snowflake around its center can be visible only within a range from 0 to the sixth of the constant pi because the snowflake has 6 axes. This argument may be a vector when multiple snowflakes are plotted.
}
  \item{deltaCoef}{
  By how much the radius is lager than the width of the crystal. The width of the crystal is computed as a ratio of the \code{"radius"} over \code{"deltaCoef"}. This argument may be a vector when multiple snowflakes are plotted.
}
  \item{color}{
  The color of the snowflake (can be transparent). This argument may be a vector when multiple snowflakes are plotted.
}
  \item{anotherColor}{
  The color of the main frame of the snowflake. This argument may be a vector when multiple snowflakes are plotted.
}
  \item{aspectRatio}{
  Aspect ratio of the snowflake. Aspect ratio is chosen automatically so that the snowflake remains round. If changed, the snowflake might not be round any longer. This argument may be a vector when multiple snowflakes are plotted.
}
  \item{seeds}{
  Random seed that defines a unique snowflake given that other parameters are assigned to their defaults.
}
}
\details{
  See the vignette for more details and examples.
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
  Returns seeds that were used to generate the snowflakes. The user can save them and use the ones that are associated with the most appealing snowflakes.
}
\references{
https://github.com/SvetlanaEden/SNOWFLAKES
}
\author{
%%  ~~who you are~~
Svetlana Eden <svetlana.eden@vanderbilt.edu>
}

\examples{
t = seq(0, 5*pi, .5)
xCoor = t*cos(t)
yCoor = t*sin(t)
radius = 1
orientation = runif(length(xCoor))*(pi/6)

set.seed(1)

plot(xCoor, yCoor, type="l", axes = TRUE, ylab="", xlab="",
ylim = range(yCoor) + radius*c(-1, 1)*3,
xlim = range(xCoor) + radius*c(-1, 1)*0, col=gray(.9))

returnedSeeds = snowflakes(xCoor = xCoor, yCoor = yCoor, radius = radius,
  orientation = orientation, seeds = 1:3,
  color = gray((1:length(xCoor))/(length(xCoor)+1)), anotherColor = "gray")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~snowflakes }
\keyword{ ~plot }
