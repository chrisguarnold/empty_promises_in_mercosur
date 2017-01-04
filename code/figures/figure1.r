##########################################################
# DESCRIPTIVES: All tables and figures                  ##
#                                                       ##
# "Empty Promises and Non-Incorporation in Mercosur"    ##
# International Interactions                            ##
# Chris Arnold                                          ##
# May 2016                                              ##
##########################################################






# ==============================================================
# = Figure 1: Incorporation record of Mercosur’s member states =
# = over the course of the years.                              =
# ==============================================================




# This function gives you the incorporation rate as if it were year x
# requires: subsetted data and summary of data in in dat.a
incineachX <-  function(tincX, needsincX){
	inc.cum <- 0 			# the vector for the incorporated
	needsinc.cum <- 0		# the vector for those which need incorporation
	# Data enters
	dat.a$tinc.date <- as.Date(tincX, origin ="1904-01-01")		# make years of incorporation for the respective country
	dat.a$tinc.year <- as.integer(format(dat.a$tinc.date, "%Y"))
	dat.a$tinc.year[dat.a$tinc.year == 1904] <- 2100
	for (i in 1995:2009){
		inc.temp <- rep(0, length(dat.a$id))
		needsinc.temp <- rep(0, length(dat.a$id))
		inc.temp[dat.a$year < i & dat.a$tinc.year < i ] <- 1		# gets a counter for every norm incorporated in that year
		inc.cum <- append(inc.cum,sum(inc.temp,na.rm=TRUE))
		needsinc.temp[dat.a$year < i & needsincX == 1] <- 1	        # Enter second var. Second Counter
		needsinc.cum <- append(needsinc.cum,sum(needsinc.temp,na.rm=TRUE))		
	}	
	rate <- inc.cum/needsinc.cum
	return(rate)
}


# In general: 
# if the decision date (tdec) is equal the incorporation date (tincFOO), then the policy does not require active incorporation. 
# They concern the organisation of Mercosur itself and do not need further attention by member states.

# For Argentina
dat.a <- subset(dat, dat$tdec != dat$tincarg)
ratearg <- incineachX(dat.a$tincarg, dat.a$needsincarg)

# Brazil
dat.a <- subset(dat, dat$tdec != dat$tincbra)
ratebra <- incineachX(dat.a$tincbra, dat.a$needsincbra)

# Par
dat.a <- subset(dat, dat$tdec != dat$tincpar)
ratepar <- incineachX(dat.a$tincpar, dat.a$needsincpar)

# Uru
dat.a <- subset(dat, dat$tdec != dat$tincuru)
rateuru <- incineachX(dat.a$tincuru, dat.a$needsincuru)

# PLotting Function
incrateplot <- function(ratearg, ratebra, ratepar, rateuru){
	year <- c(1993:2008) # because I append on a 0, there is one year more in it in the beginning...             
	# Empty Canvas
	plot(year, ratearg, 
		type = "n", # blank all
		ylim = c(0,1), xlim = c(1994,2009),
		ylab = " ", xlab= "Year", 
		yaxt= "n", xaxt="n", bty = "n"#ticks
		# axes = FALSE
	)
	# Add Contours
	mtext(side = 2, 'Proportion of Policies Incorporated', line = 4)
	axis(1, c(seq(1994,2008,2)), cex.axis = 1)
	axis(2, cex.axis = 1
			, las = 1
			, at= c(seq(0,1,.2)), labels = c("0%", "20%","40%","60%","80%","100%"))
	# Plot Data
	lines(year, ratearg, col ="black", lwd = 2)
	points(year, ratearg, col ="black", pch = 16, cex = 1.3)
	lines(year, ratebra, col ="black", lty = 2,lwd = 2)
	points(year,ratebra, col ="black", pch = 16 , cex = 1.3)
	points(year,ratebra, col ="white", pch = 16, cex = 1 )		# create the white whole in the middle
	lines(year, ratepar, col ="grey",lwd = 2)
	points(year,ratepar, col ="grey", pch = 16, cex = 1.3)
	lines(year, rateuru, col ="grey", lty = 2,lwd = 2)
	points(year,rateuru, col ="grey" , pch = 16, cex = 1.3)
	points(year,rateuru, col ="white" , pch = 16, cex = 1)		# create the white whole in the middle
}



# Write out as .pdf

pdf(file="output/asineachyear.pdf", width = 6, height = 5)
par(mar = c(3, 5, .4, .4))
incrateplot(ratearg, ratebra, ratepar, rateuru)
legend(2005.2,0.39,col=c("black","black", "grey", "grey")
	, bg = "white"	
	, box.col = 'white'
	, pch = c(16,1,16,1)
	, legend=c("Argentina ","Brazil ","Paraguay", "Uruguay ")
	, cex = .8 
	)
dev.off()


