transpCol <-
function(myColor, transp="45"){
	### color in character format
	### transparency in hexadecimal format: 00-completely transparent (invisible) 25-very transparent, 57-more transparent, FF-not transparent
	### returns the color in hexadecimal format
	if(!(myColor %in% colors())) stop("There is no such color name")
	tmp = col2rgb(myColor)
	paste(rgb(tmp[1,1], tmp[2,1], tmp[3,1], maxColorValue=255), transp, sep="")
}
