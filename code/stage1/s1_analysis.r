##########################################################
# File for the anaysis of the cotracting stage          ##
#                                                       ##
# "Empty Promises and Non-Incorporation in Mercosur"    ##
# International Interactions                            ##
# Chris Arnold                                          ##
# May 2016                                              ##
##########################################################



# =================
# = Assemble Data =
# =================
dat.ni2 <- subset(dat, dat$needsinc == TRUE)
dat.ni <- data.frame(
dat.ni2$pol.dim.type6
,dat.ni2$confl.level
,dat.ni2$complexity.pc
,dat.ni2$annex
,dat.ni2$d.derrogates
,dat.ni2$preamble
,dat.ni2$decountry
,dat.ni2$cmc
,dat.ni2$gmc
,dat.ni2$tc
)

names(dat.ni) <- c('pol.dim.type6'
					,'confl.level'
      				,'complexity.pc'
      				,'annex'
					,'dderrogates'
      				,'preamble'
      				,'decountry'
					,'cmc'
      				,'gmc'
      				,'tc')

dat.ni$govcoop <- rep(0, length(dat.ni[,1]))
dat.ni$govcoop[dat.ni$pol.dim.type6 == 'gov_coop'] <- 1
dat.ni$annex <- car::recode(dat.ni$annex, "TRUE = '1'; FALSE = '0'")


#=================
# = Analyse Data =
#=================


log.confl.m1.glm <- glm(govcoop ~ 
		confl.level
		+ gmc + tc				
		# Controls		
		+ complexity.pc
		+ annex
		+ dderrogates
		+ as.factor(decountry)		
		, family = binomial
		, data = dat.ni
)

# Formatting the table
stargazer(log.confl.m1.glm, out = "output/OI1.tex"#, summary=FALSE
  , digits = 2, dep.var.labels.include =FALSE, style = "apsr", single.row = FALSE
	, align = TRUE
	, covariate.labels = c("Conflict Level","CMG","TC","Complexity","Annex","Overruling Existing Policy Policies"
	, "Adopted at the Meeting", "Brazil", "Paraguay","Uruguay")
  , title = "Estimation results from a logit model. The reference categories for the dummy variables are as follows. Countries: Argentina; Decision bodies: Common Market Council."
)



#==============
# = Postpreds =
#==============

# create dummies
dat.ni$arg <- dat.ni$bra <- dat.ni$par <- dat.ni$uru <- 0
dat.ni$arg[dat.ni$decountry == "arg"] <- 1
dat.ni$bra[dat.ni$decountry == "bra"] <- 1
dat.ni$par[dat.ni$decountry == "par"] <- 1
dat.ni$uru[dat.ni$decountry == "uru"] <- 1



# model in Zelig 5.0
log.confl.m <- zelig(govcoop ~ 
		confl.level
		+ gmc + tc				
		# Controls		
		+ complexity.pc
		+ annex
		+ dderrogates
		+ bra + par + uru
		, model = "logit"
		, data = dat.ni
)


# Range of value to calculate postpreds
# First difference
log.confl.xl <- setx(log.confl.m, confl.level=0)
log.confl.xh <- setx(log.confl.m, confl.level=18.5)
log.confl.sfd <- sim(log.confl.m, x=log.confl.xl, x1=log.confl.xh, num = 1e5)

all.h <- high(log.confl.sfd$sim.out$x1$fd[[1]])
all.m <- mean(log.confl.sfd$sim.out$x1$fd[[1]])
all.l <- low(log.confl.sfd$sim.out$x1$fd[[1]])


#=============
# = Plotting =
#=============

# plotting the first difference
pdf("output/s1_confl_fd.pdf", height = 5, width = 3)
par(mar = c(3,5,.4,.4))

plot(1,1, type = "n"
		, bty = "n"
		, las = 1
		, xlim = c(0,2) , ylim = c(-.1,.5)
		, xaxt = "n" #, yaxt = "n"
		, ylab = "Expected Probability Difference"
		, xlab = ""
		)
abline(h=0, lty = 2, lwd = 2, col = "grey")
lines(c(1,1),c(all.h, all.l),  lwd = 3, col = "black")
points(1,all.m, pch = 16 , cex = 1.8, col = "black")

dev.off()


