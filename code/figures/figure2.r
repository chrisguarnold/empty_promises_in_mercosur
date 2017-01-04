##########################################################
# DESCRIPTIVES: All tables and figures                  ##
#                                                       ##
# "Empty Promises and Non-Incorporation in Mercosur"    ##
# International Interactions                            ##
# Chris Arnold                                          ##
# May 2016                                              ##
##########################################################



# =====================================================
# = Figure 2: Incorporation duration of incorporated  =
# =   and non-incorporated policies in Mercosur.      =
# =====================================================

# Calculate Duration
datlong$duration <- datlong$tcens - datlong$tdec 

# Plot: Combine the boxplot with the histogram
# Histogramm
pdf('output/inc_dur_coun.pdf', width = 6, height = 5)
par(mar = c(3,5,.4,.4))
boxplot(duration ~ incdummy*country, horizontal = FALSE, axes = FALSE
	, col = c("white", "grey")
	#, border = c("black", "black")
	, data = datlong
	, boxwex = .4
	, ylim = c(0,5500) 
)
mtext(side = 1
	, at = c(1.5,3.5,5.5,7.5)
	, text=c('Argentina','Brazil','Paraguay','Uruguay')
	, line = 1)
# Vertical Axis
axis(2, las = 1)
mtext('Duration (days)', 2, line = 4)
# This makes the small brakets at the bottom
axis(1, at = c(1,2), labels = c(NA, NA), tck = 0.015, line = .45)
axis(1, at = c(3,4), labels = c(NA, NA), tck = 0.015, line = .45)
axis(1, at = c(5,6), labels = c(NA, NA), tck = 0.015, line = .45)
axis(1, at = c(7,8), labels = c(NA, NA), tck = 0.015, line = .45)
legend('topright'
	, legend = c("Incorporated", "Not in- \ncorporated")
	, col = c("grey", "black"), box.col = "white"
	, pch = c(15, 22)
	, cex = .75)
dev.off()



