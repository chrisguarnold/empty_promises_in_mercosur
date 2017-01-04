##########################################################
# MASTER                                                ##
#                                                       ##
# "Empty Promises and Non-Incorporation in Mercosur"    ##
# International Interactions                            ##
# Chris Arnold                                          ##
# May 2016                                              ##
# Implemented on Zelig 5 and R 3.2.3                    ##
##########################################################


# ============
# = Packages =
# ============

library(survival)
library(Zelig)
library(lattice)
library(dummies)
library(MASS)
library(stargazer)


# ========
# = Data =
# ========

setwd(getwd())

# load data sets
load("data/master.Rdata")

# make long data sets
source("code/make_data/make_data.r")



# =============
# = Functions =
# =============

# Functions for quantiles
low <- function(x){
	quantile(x, .025, na.rm = TRUE)
}
high <- function(x){
	quantile(x, .975, na.rm = TRUE)
}


# ================
# = Descriptives =
# ================

source("code/figures/figure1.r")
source("code/figures/figure2.r")
source("code/figures/table1.r")
source("code/figures/table2.r")




# ============================
# = Stage 1: Decision making =
# ============================

# Analyse data and make postpreds for the Observable Implication 
# Figure and Table from the appendix
source("code/stage1/s1_analysis.r")


# ==========================
# = Stage 2: Incorporation =
# ==========================

# This does the analysis 
# Also charts results and creates figures 3a) and 3b)
source("code/stage2/s2_analysis.r")
