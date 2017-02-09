# This file loads the required libraries into R working Directory
# Created: 20130207
library("ggplot2")
library("sciplot")
library("stringr")
library("plyr")
library("gdata")
library("grid")
library("doBy")
library("car")
library("ez")
library("Amelia")
require("lme4")
require("plotrix");#axis.break
require("longpower")
#require("pgirmess");# perfoms non-parametric analysis: kruskal wallis, multiple comparisons etc. This pakage could not be installed due to library issues with one of its dependent package 'rgdal'. As alternative installed package 'nparcomp'
#to adhere to the sum-to-zero convention for effect weights, you should always do this before running anovas in R
require("nparcomp");
require("influence.ME");
options(contrasts=c("contr.sum","contr.poly"))
