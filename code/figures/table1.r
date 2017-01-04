##########################################################
# DESCRIPTIVES: All tables and figures                  ##
#                                                       ##
# "Empty Promises and Non-Incorporation in Mercosur"    ##
# International Interactions                            ##
# Chris Arnold                                          ##
# May 2016                                              ##
##########################################################


# ===================================================================
# = Table 1: Descriptive statistics for three categorial variables. =
# ===================================================================


cmc.d <- table(datlong$cmc)
cmc.1 <- table(datlong$cmc)[2]
cmc.0 <- table(datlong$cmc)[1]

a <- table(datlong$arg)[2]
b <- table(datlong$bra)[2]
p <- table(datlong$par)[2]
u <- table(datlong$uru)[2]

# Assemble Data Frame
poltype <- table(datlong$pol.dim.type6)
poltype.2 <- c(poltype[5]+poltype[6], poltype[1], poltype[2], poltype[3], poltype[4], poltype[7])
poltype.table <- c(poltype.2, 3560 - sum(poltype.2))
data.table1 <- c(cmc.d, a, b, p, u, poltype.table)
str.table <- c("CMC"
			,"CMG and TC"
			,"Argentina"
			,"Brazil"
			,"Paraguay"
			,"Uruguay" 
			,"Common External Tariff"
			,"Governmental Cooperation"
			,"Mercosur Interna"
			,"Internal Market"
			,"Tariff Exception"
			,"Technical Regulations"
			,"Others")
table1 <- cbind(str.table, data.table1)


# Write out table
stargazer(table1, out = "output/descr1.tex", summary=FALSE
  , digits = 2, rownames = FALSE
  , title = "Descriptive statistics for three categorial variables."
)






















