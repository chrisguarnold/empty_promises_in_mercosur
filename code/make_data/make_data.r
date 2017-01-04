##########################################################
# Create Data                                           ##
# Use Master Data, create long data sets                ##
# "Empty Promises and Non-Incorporation in Mercosur"    ##
# International Interactions                            ##
# Chris Arnold                                          ##
# May 2016                                              ##
##########################################################




# 1 Make Long Data Format -------------------------------------------------

# This step excludes those incorporated before. 
# What you get are those that need ACTIVE incorporation
datarg2 <- subset(dat, dat$tdec <= dat$tcensarg)
datbra2 <- subset(dat, dat$tdec <= dat$tcensbra)
datpar2 <- subset(dat, dat$tdec <= dat$tcenspar)
daturu2 <- subset(dat, dat$tdec <= dat$tcensuru)
datarg <- subset(datarg2, datarg2$needsincarg == 1)
datbra <- subset(datbra2, datbra2$needsincbra == 1)
datpar <- subset(datpar2, datpar2$needsincpar == 1)
daturu <- subset(daturu2, daturu2$needsincuru == 1)

# A dummy for the countries
# country identifier
datarg$country <- rep(1 ,length(datarg$id))
datbra$country <- rep(2 ,length(datbra$id))
datpar$country <- rep(3 ,length(datpar$id))
daturu$country <- rep(4 ,length(daturu$id))
country <- c(datarg$country, datbra$country, datpar$country, daturu$country)
# country dummies
arg <- rep(0, length(country))
arg[country == 1] <- 1
bra <- rep(0, length(country))
bra[country == 2] <- 1
par <- rep(0, length(country))
par[country == 3] <- 1
uru <- rep(0, length(country))
uru[country == 4] <- 1

# Assemble Explanatory Variables
id 	        <- c(datarg$id,datbra$id,datpar$id,daturu$id)
incdummy    <- c(datarg$incarg, datbra$incbra, datpar$incpar, daturu$incuru)
tdec        <- c(datarg$tdec,datbra$tdec,datpar$tdec,daturu$tdec)
tcens       <- c(datarg$tcensarg,datbra$tcensbra,datpar$tcenspar,daturu$tcensuru)
time        <- tcens - tdec
year        <- c(datarg$year,datbra$year,datpar$year,daturu$year)
country     <- c(datarg$country, datbra$country, datpar$country, daturu$country)
decbody     <- c(datarg$decbody,datbra$decbody,datpar$decbody,daturu$decbody)
cmc	        <- c(datarg$cmc,datbra$cmc,datpar$cmc,daturu$cmc)
gmc	        <- c(datarg$gmc,datbra$gmc,datpar$gmc,daturu$gmc)
tc	        <- c(datarg$tc,datbra$tc,datpar$tc,daturu$tc)
dderrogates <- c(datarg$d.derrogates, datbra$d.derrogates, datpar$d.derrogates, daturu$d.derrogates)
annex       <- c(datarg$annex, datbra$annex, datpar$annex, daturu$annex)
confl.level <- c(datarg$confl.level, datbra$confl.level, datpar$confl.level, daturu$confl.level)
# create var identifying meeting in own country
decountry.temp <- c(datarg$decountry,datbra$decountry, datpar$decountry, daturu$decountry)
decountry <- heimspiel <- rep(0, length(decountry.temp)) # long vectors
decountry[decountry.temp == "arg"] <- 1
decountry[decountry.temp == "bra"] <- 2
decountry[decountry.temp == "par"] <- 3
decountry[decountry.temp == "uru"] <- 4
heimspiel[decountry == country] <- 1
pol.dim.type6 <- c(datarg$pol.dim.type6, datbra$pol.dim.type6, datpar$pol.dim.type6, daturu$pol.dim.type6)
complexity.pc 	<- c(datarg$complexity.pc, datbra$complexity.pc, datpar$complexity.pc, daturu$complexity.pc)
vpindex 	<- c(datarg$vpindex.a,datbra$vpindex.b,datpar$vpindex.p,daturu$vpindex.u) 

# Put data in a data frame
datlong <- data.frame(
	id
	,time
	,incdummy	
	,tdec
	,tcens	
	,year
	,country
	,arg
	,bra
	,par
	,uru	
	,decbody 
	,cmc	
	,gmc	
	,tc	
	,dderrogates    
	,annex	
	,confl.level
	,heimspiel 
	,pol.dim.type6 
	,complexity.pc
	,vpindex
)
# dummies
# Decision Body
decbody <- data.frame(dummy(datlong$decbody))
names(decbody) <- c("decbody.1", "decbody.2", "decbody.3")
# Country
country <- data.frame(dummy(datlong$country))
names(country) <- c("country.1","country.2","country.3","country.4")
# Years
year <- data.frame(dummy(datlong$year))
names(year) <- c("year.1994"
                 ,"year.1995"
                 ,"year.1996"
                 ,"year.1997"
                 ,"year.1998"
                 ,"year.1999"
                 ,"year.2000"
                 ,"year.2001"
                 ,"year.2002"
                 ,"year.2003"
                 ,"year.2004"
                 ,"year.2005"
                 ,"year.2006"
                 ,"year.2007"
                 ,"year.2008")

# type of regulation
pol.dim.type6 <- data.frame(dummy(datlong$pol.dim.type6))
names(pol.dim.type6) <- paste("pol.dim.type6", sort(unique(dat$pol.dim.type6)), sep = ".")
names(pol.dim.type6)[-(1:(max(length(names(pol.dim.type6))-1)))] <- "pol.dim.type6.NA"

# build data set
datlong <- cbind(datlong,decbody,country,pol.dim.type6,year)




# 2 Build Data Sets with Imputed Values -------------------------------------

# Create 5 data sets with (different) imputed values
datlong1 <- datlong2 <- datlong3 <- datlong4 <- datlong5 <- datlong

load("data/dat_imputed_1.RData")
load("data/dat_imputed_2.RData")
load("data/dat_imputed_3.RData")
load("data/dat_imputed_4.RData")
load("data/dat_imputed_5.RData")

# Overview over data
prom.arg <- c(dat.tscs1$prom.arg,dat.tscs2$prom.arg,dat.tscs3$prom.arg,dat.tscs4$prom.arg,dat.tscs5$prom.arg)
prom.bra <- c(dat.tscs1$prom.bra,dat.tscs2$prom.bra,dat.tscs3$prom.bra,dat.tscs4$prom.bra,dat.tscs5$prom.bra)
prom.par <- c(dat.tscs1$prom.par,dat.tscs2$prom.par,dat.tscs3$prom.par,dat.tscs4$prom.par,dat.tscs5$prom.par)
prom.uru <- c(dat.tscs1$prom.uru,dat.tscs2$prom.uru,dat.tscs3$prom.uru,dat.tscs4$prom.uru,dat.tscs5$prom.uru)
prom <- c(prom.arg, prom.bra, prom.par, prom.uru)
prom_temp <- c(prom.arg, prom.bra, prom.par, prom.uru)

# Add imputed vars
# idea: per line, get the value of the episode and look it up in the vector
assign.fourc <- function(foreign.vec.arg, foreign.vec.bra, foreign.vec.par, foreign.vec.uru){
	var <- rep(NA, length(datlong[,1]))
	for (i in 1: length(datlong[,1])){
		if (datlong$country[i] == 1){
			a <- datlong$year[i] - 1993
			var[i] <- foreign.vec.arg[a]
		}
		else if (datlong$country[i] == 2){
			a <- datlong$year[i] - 1993
			var[i] <- foreign.vec.bra[a]
		}
		else if (datlong$country[i] == 3){
			a <- datlong$year[i] - 1993
			var[i] <- foreign.vec.par[a]
		}
		else if (datlong$country[i] == 4){
			a <- datlong$year[i] - 1993
			var[i] <- foreign.vec.uru[a]
		}
		else {
			a <- datlong$episode[i]
			var[i] <- NA
		}

	}
	return(var)
}

# Set 1
# support: prom
datlong1$prom <- assign.fourc(dat.tscs1$prom.arg,dat.tscs1$prom.bra,dat.tscs1$prom.par,dat.tscs1$prom.uru)
# goveff
datlong1$goveff <- assign.fourc(dat.tscs1$goveff.arg,dat.tscs1$goveff.bra,dat.tscs1$goveff.par,dat.tscs1$goveff.uru)

# Set 2
# support: prom
datlong2$prom <- assign.fourc(dat.tscs2$prom.arg,dat.tscs2$prom.bra,dat.tscs2$prom.par,dat.tscs2$prom.uru)
# goveff
datlong2$goveff <- assign.fourc(dat.tscs2$goveff.arg,dat.tscs2$goveff.bra,dat.tscs2$goveff.par,dat.tscs2$goveff.uru)

# Set 3
# support: prom
datlong3$prom <- assign.fourc(dat.tscs3$prom.arg,dat.tscs3$prom.bra,dat.tscs3$prom.par,dat.tscs3$prom.uru)
# goveff
datlong3$goveff <- assign.fourc(dat.tscs3$goveff.arg,dat.tscs3$goveff.bra,dat.tscs3$goveff.par,dat.tscs3$goveff.uru)

# Set 4
# support: prom
datlong4$prom <- assign.fourc(dat.tscs4$prom.arg,dat.tscs4$prom.bra,dat.tscs4$prom.par,dat.tscs4$prom.uru)
# goveff
datlong4$goveff<- assign.fourc(dat.tscs4$goveff.arg,dat.tscs4$goveff.bra,dat.tscs4$goveff.par,dat.tscs4$goveff.uru)

# Set 5
# support: prom
datlong5$prom <- assign.fourc(dat.tscs5$prom.arg,dat.tscs5$prom.bra,dat.tscs5$prom.par,dat.tscs5$prom.uru)
# goveff
datlong5$goveff <- assign.fourc(dat.tscs5$goveff.arg,dat.tscs5$goveff.bra,dat.tscs5$goveff.par,dat.tscs5$goveff.uru)

