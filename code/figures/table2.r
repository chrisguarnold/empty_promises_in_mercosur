##########################################################
# DESCRIPTIVES: All tables and figures                  ##
#                                                       ##
# "Empty Promises and Non-Incorporation in Mercosur"    ##
# International Interactions                            ##
# Chris Arnold                                          ##
# May 2016                                              ##
##########################################################


# =====================================================================================================================
# = Table 2: Descriptive statistics for variables measuring the political context and attributes of the regulations. ==
# =====================================================================================================================


# quick function to take the correct values and bring them in their right order
correct.order <- function(sd.out, smmry.out){
	out <- c(smmry.out[4],sd.out, smmry.out[1], smmry.out[6])
	names(out) <- c("Mean", "S.D.", "Min", "Max")
	return(out)
}


# ============================================
# = 1 Context Variables on the basis of tscs =
# ============================================

### Mercosur Support
prom.sum <- summary(prom)
prom.sd <- sd(prom)
prom.smry <- correct.order(prom.sd, prom.sum)


### Veto Players
library(foreign)
dat.vp <- read.dta(file = 'data/DPI2010.dta')
# create vars for Argentina, Brazil, Paraguay and Uruguay 
a <- b <- p <- u <- NULL
at <- bt <- pt <- ut <- NULL
# grab from data set
at <- dat.vp$checks[dat.vp$countryname == "Argentina"]
bt <- dat.vp$checks[dat.vp$countryname == "Brazil"]
pt <- dat.vp$checks[dat.vp$countryname == "Paraguay"]
ut <- dat.vp$checks[dat.vp$countryname == "Uruguay"]
# check the right years
a <- at[20:34]
b <- bt[20:34]
p <- pt[20:34]
u <- ut[20:34]
all.vp <- c(a,b,p,u)
all.vp.sum  <- summary(all.vp)
all.vp.sd  <- sd(all.vp)
all.vp.smry<-correct.order(all.vp.sd, all.vp.sum)



#### Conflict Level
gmera <- read.csv(file = "data/gomezmera_crisislevel.csv", header = TRUE, stringsAsFactors = FALSE)
gmera.sum <- summary(gmera[,2])
gmera.sd <- sd(gmera[,2])
gmera.smry<-correct.order(gmera.sd, gmera.sum)



### Meeting in the Own Country 
# from dat.eha
heimspiel.sum <- summary(datlong$heimspiel)
heimspiel.sd <- sd(datlong$heimspiel)
heimspiel.smry <- correct.order(heimspiel.sd, heimspiel.sum)




# ==================================================================
# 2 Information on Policies                                        =
#   Subset to include only the policies that require incorporation =
# ==================================================================

dat.sub <- subset(dat, (dat$needsincarg == 1 | dat$needsincbra == 1 | dat$needsincpar == 1 | dat$needsincuru == 1))

### Complexity
compl.sum <- summary(dat.sub$complexity.pc)
compl.sd <- sd(dat.sub$complexity.pc)
compl.smry <- correct.order(compl.sd, compl.sum)

### Annex
annex.sum <- summary(as.numeric(dat.sub$annex))
annex.sd <- sd(dat.sub$annex)
annex.smry <- correct.order(annex.sd, annex.sum)

### Overruling existing policies
# Saved in the date when it overrules. Create dummy:
dat.sub$d.derrogates <- rep(0, length(dat.sub$derrogates))
dat.sub$d.derrogates[dat.sub$derrogates != 0] <- 1
overrule.sum <- summary(dat.sub$d.derrogates)
overrule.sd <- sd(dat.sub$d.derrogates)
overrule.smry <- correct.order(overrule.sd, overrule.sum)



# ====================
# = 3 Report Results =
# ====================

res.mat <- rbind(prom.smry, all.vp.smry, gmera.smry,heimspiel.smry, compl.smry, annex.smry, overrule.smry)
rownames(res.mat) <- c("Mercosur Support"
                     ,"Veto Players"
                     ,"Conflict Level"
                     ,"Meeting in the Own Country"
                     ,"Complexity of Policy Annex"
                     ,"Annex"
                     ,"Overrules Existing Policy")
rowlabels <- rownames(res.mat)


stargazer(res.mat
          , out = "output/descr2.tex", summary=FALSE, style = "ajps"
          , digits = 2, rownames = TRUE
          , title = "Descriptive statistics for variables measuring the political context and attributes of the regulations.")







