##########################################################################
#PS4
#DATE:5th Oct, 2023
#Author: Keshav Bhusal
#AREC 559 Advanced Applied Econometric 
#Affiliation: Department of Agriculture and Resource Economics(AREC)
##########################################################################

#Setting up the environment
library(rio)
library(ggplot2)  
library(dplyr)            
library(tidyr)  
library(readr)
library(VIM)
library(zoo)

#Load the necessary datasets and creating the combined dataset 
load("~/Library/CloudStorage/Box-Box/Project AMEX/myca_mob.RData")
myca_mob <- x
load("~/Library/CloudStorage/Box-Box/Project AMEX/myca_web.RData")
myca_web <- x
load("~/Library/CloudStorage/Box-Box/Project AMEX/Imputed_EarlyLegal_Dataset.RData")
early_legal <- x