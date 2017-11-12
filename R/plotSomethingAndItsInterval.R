plotSomethingAndItsInterval <-
function(sthgX, sthgY, lowerlimit, upperlimit, color="black", ...){
  polygon(x=c(sthgX, sthgX[length(sthgX):1]), y=c(upperlimit, lowerlimit[length(lowerlimit):1]),
	  col = transpCol(color, transp="45"), border=transpCol(color, transp="45"))
	lines(sthgX, sthgY, col=color, ...)
}
