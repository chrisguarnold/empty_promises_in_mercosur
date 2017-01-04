##########################################################
# Main Model and corresponding Post-Predictions         ##
#                                                       ##
# "Empty Promises and Non-Incorporation in Mercosur"    ##
# International Interactions                            ##
# Chris Arnold                                          ##
# May 2016                                              ##
##########################################################



# =========
# = Model =
# =========

# Complete Model with interactions
weib <- zelig(Surv((tcens - tdec+1), incdummy) ~ 
				confl.level
				+ cmc + tc
				+ prom
				+ prom:cmc
				+ prom:tc				
				+ pol.dim.type6.gov_coop:confl.level             
				+ pol.dim.type6.interna:confl.level 
				+ pol.dim.type6.internal_market:confl.level  
				+ pol.dim.type6.tariff_exception:confl.level  
				+ pol.dim.type6.technical_regulations:confl.level
				+ pol.dim.type6.NA:confl.level
				+ pol.dim.type6.gov_coop        
				+ pol.dim.type6.interna              
				+ pol.dim.type6.internal_market      
				+ pol.dim.type6.tariff_exception   
				# Reference Categories  
#				+ pol.dim.type6.tariff_nomenclature  
#				+ pol.dim.type6.tariff_rest          
				+ pol.dim.type6.technical_regulations
				+ pol.dim.type6.NA 				
				+ arg + bra + par
				# Controls for the context
				+ vpindex + heimspiel
				# Controls for the policy
				+ complexity.pc + annex + dderrogates	
			,model = "weibull"
			,data= data.frame(datlong1, datlong2, datlong3, datlong4, datlong5)
)


## Result from the main model
# No stargazer for this one. Results have to be assembled by hand into a LaTex table...
summary(weib)

# ===================
# = Postpredictions =
# ===================


# Setting the x values for the popularity simulations
xout.cmc.gc.h <- setx(weib, pol.dim.type6.gov_coop                	= 1
                           	, pol.dim.type6.interna                 = 0
                           	, pol.dim.type6.internal_market         = 0
                           	, pol.dim.type6.tariff_exception        = 0 
                           	, pol.dim.type6.technical_regulations   = 0
                           	, pol.dim.type6.NA 		       		   	= 0
							, cmc = 1
							, tc = 0
							,prom=89.8)
xout.cmc.gc.l <- setx(weib, pol.dim.type6.gov_coop                	= 1
                          	, pol.dim.type6.interna                 = 0
                          	, pol.dim.type6.internal_market         = 0
                          	, pol.dim.type6.tariff_exception        = 0 
                          	, pol.dim.type6.technical_regulations   = 0
                          	, pol.dim.type6.NA 		       		   	= 0
						, cmc = 1
						, tc = 0
						, prom=70.4)								
xout.ncmc.gc.h <- setx(weib, pol.dim.type6.gov_coop                	    = 1
                    			, pol.dim.type6.interna                 = 0
                    			, pol.dim.type6.internal_market         = 0
                    			, pol.dim.type6.tariff_exception        = 0 
                    			, pol.dim.type6.technical_regulations   = 0
                    			, pol.dim.type6.NA 		       		   	= 0
							, cmc = 0
							, prom=89.8)
 xout.ncmc.gc.l <- setx(weib, pol.dim.type6.gov_coop                	= 1
                     			, pol.dim.type6.interna                 = 0
                     			, pol.dim.type6.internal_market         = 0
                     			, pol.dim.type6.tariff_exception        = 0 
                     			, pol.dim.type6.technical_regulations   = 0
                     			, pol.dim.type6.NA 		       		   	= 0
 							, cmc = 0
 							, prom=70.4)

# Setting the x values for the conflict simulations
weib.confl.gc.l.x <- setx(weib,  confl.level =  0, pol.dim.type6.gov_coop =1)
weib.confl.gc.h.x <- setx(weib,  confl.level =  18.5, pol.dim.type6.gov_coop =1)
weib.confl.ngc.l.x <- setx(weib,  confl.level =  0, pol.dim.type6.gov_coop =0)
weib.confl.ngc.h.x <- setx(weib,  confl.level =  18.5, pol.dim.type6.gov_coop =0)



# Simulating
sim.cmc.gc   	<- sim(weib, x= xout.cmc.gc.l  	  ,x1= xout.cmc.gc.h   ,num = 10000)
sim.ncmc.gc    	<- sim(weib, x= xout.ncmc.gc.l    ,x1= xout.ncmc.gc.h  ,num = 10000)
sim.confl.gc <- sim(weib, x1 = weib.confl.gc.h.x, x = weib.confl.gc.l.x,num = 10000)
sim.confl.ngc <- sim(weib, x1 = weib.confl.ngc.h.x, x = weib.confl.ngc.l.x,num = 10000)



# Aim: Get the respective results from the simulations
# Functions 'low' and 'high' defined in master.r
mhl.maker <- function(sims){
	mean <-  mean(sims)
	high <-  high(sims)
	low  <-  low (sims)
	result <- c(mean, high, low)
	return(result)
}

# Expected durations for the scenarios
cmc.gc    <- mhl.maker(sim.cmc.gc$sim.out$x1$fd[[1]]) 
ncmc.gc   <- mhl.maker(sim.ncmc.gc$sim.out$x1$fd[[1]])
confl.gc  <- mhl.maker(sim.confl.gc$sim.out$x1$fd[[1]])
confl.ngc <- mhl.maker(sim.confl.ngc$sim.out$x1$fd[[1]])





# ==========
# = Graphs =
# ==========

# Popularity Simulation
pdf("output/s2_pop_fd_gc.pdf", height = 5, width = 3.8)
par(mar = c(3,5,.4,.4))
# canvas
plot(1,1, type = "n"
		, bty = "n"
		, xlim = c(0, 3)
		, ylim = c(-2000,10000)
		, xaxt = "n" , yaxt = "n"
		, xlab = " "
		, ylab = " "
		)
#axes etc.
axis(2, las = 1)
mtext('Expected Duration Difference (days)', 2, line = 4)
abline(h=0, col = "grey", lwd = 2, lty = 2)		
lines(c(1,1), c(cmc.gc[2], cmc.gc[3]), lwd = 3, col = "black")
lines(c(2,2), c(ncmc.gc[2], ncmc.gc[3]), lwd = 3, col = "black")
points(c(1,2),c(cmc.gc[1], ncmc.gc[1]), pch = 16, col = c("black"), cex = 1.8)
mtext(at = c(1,2)
	,text = c("CMC \n "
			, "CMG & TC \n")
	, side = 1, line = 1, cex = .95
)
dev.off()


# Governmental Conflict Simulation
pdf("output/s2_confl_fd.pdf", height = 5, width = 3.8)
par(mar = c(3,5,.4,.4))
# canvas
plot(1,1, type = "n"
		, bty = "n"
		, xlim = c(0, 3)
		, ylim = c(-2000,10000)
		, xaxt = "n" , yaxt = "n"
		, xlab = " "
		, ylab = " "
		)
#axes etc.
axis(2, las = 1)
mtext('Expected Duration Difference (days)', 2, line = 4)
abline(h=0, col = "grey", lwd = 2, lty = 2)		
lines(c(2,2), c(confl.ngc[2], confl.ngc[3]), lwd = 3, col = "black")
lines(c(1,1), c(confl.gc[2], confl.gc[3]), lwd = 3, col = "black")
points(c(1,2),c(confl.gc[1], confl.ngc[1]), pch = 16, col =  "black", cex = 1.8)
mtext(at = c(1,2)
	,text = c("Government \n Cooperation"
			, "Other \n Policies")
	, side = 1, line = 1, cex = .95
)
dev.off()

