reportQuestion <-
function(x, label="x", textX=median(x), textY=NULL, roundNum = 2, cexArg=0.7, color="gray", border="white", ylim=NULL, ...){
	cexArg = 0.7
	tmp = hist(x, plot=FALSE)
	if (is.null(ylim)) ylim = c(0, max(tmp$counts))
	hist(x, cex.axis=cexArg, main="", xlab=label, col=color, border=border, ylim=ylim, ...)
	if (is.null(textY)) textY = ylim[2]
	text(textX, textY, paste("Post. mean = ", round(mean(x, na.rm=TRUE), roundNum), sep=""), pos=4, cex=cexArg)
	text(textX, textY*.95, paste("Post. SD = ", round(sd(x, na.rm=TRUE), roundNum), sep=""), pos=4, cex=cexArg)
	text(textX, textY*.9, paste("Median, 95% CIs:"), pos=4, cex=cexArg)
	text(textX, textY*.85, paste("  ", bootCIs(x)), pos=4, cex=cexArg)
}
