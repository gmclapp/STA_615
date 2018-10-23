# Change this file to a *.rmd (RMarkdown) file.
---
title: "STA 610/615 Homework 02 markup"
author: "Glenn Clapp"
date: "02-September-2018"
output: word_document
---
## ----Setup---------------------------------------------------------------
library(tidyverse)
library(ggformula)
library(mosaic)
library(readr)
library(moments)

## ------------------------------------------------------------------------
ex1_4_2 <- read.csv('EXA_C01_S04_01.csv')

# These lines don't appear to work. The data set was successfully imported using the GUI however.

#' 
## ------------------------------------------------------------------------
histogram(~AGE,data=ex1_4_2)
ex1_4_2 %>% gf_histogram(~AGE)
#'Collect two 10 item samples.
sample01 <- ex1_4_2 %>% sample_n(10)
sample02 <- ex1_4_2 %>% sample_n(10)

sample01 %>% tally(~SUBJ, data=.)
sample02 %>% tally(~SUBJ, data=.)
tally(c(sample01$SUBJ, sample02$SUBJ)) # This will tally the occurances of each SUBJ in both sets. 2 would indicate a subject occuring in both.


histogram(~AGE, data=sample01)
histogram(~AGE, data=sample02)

sample01 %>% summary()
favstats(~AGE, data=sample01)

sample02 %>% summary()
favstats(~AGE, data=sample02)

ex2_3_2 <- read.csv('EXR_C02_S03_02.csv')

favstats(~sizes, data = ex2_3_2)
histogram(~sizes, data = ex2_3_2, type = 'count', labels = TRUE) #Histogram

freqpolygon(~sizes, data = ex2_3_2) #frequency polygon
## ------------------------------------------------------------------------
BIG = max(ex2_3_2$sizes)
SMALL = min(ex2_3_2$sizes)
bins <- seq(SMALL, BIG, 2)
Scores <- cut(ex2_3_2$sizes, bins)
freq_table <- transform(table(Scores))
transform(freq_table, Rel_Freq=prop.table(Freq), Cum_Freq=cumsum(Freq))
plot(freq_table)

ex2_5_6 <- read.csv('EXR_C02_S05_06.csv')
favstats(ex2_5_6$Months)

getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(ex2_5_6$Months)
var(ex2_5_6$Months)
std_dev = sd(ex2_5_6$Months)
Mean = mean(ex2_5_6$Months)
coefficient_of_variation = std_dev/Mean

bwplot(ex2_5_6$Months)

exercise_12_data = data.frame(
  control = c(131,115,124,131,122,117,88,114,150,169),
  sci = c(60,150,130,180,163,130,121,119,130,148)
  )
#exercise_12_data  
favstats(exercise_12_data$control)
favstats(exercise_12_data$sci)

#Maternal age
NC_data <- read.csv('LDS_C02_NCBIRTH800.csv')
MAGEmean = mean(NC_data$mage)
MAGEmedian = median(NC_data$mage)
MAGEsd = sd(NC_data$mage)
MAGEIQR = iqr(NC_data$mage)
MAGErange = max(NC_data$mage)-min(NC_data$mage)

histogram(NC_data$mage)
bwplot(NC_data$mage)
skewness(NC_data$mage)
kurtosis(NC_data$mage)

NC_married_subset <- subset(NC_data, marital == 1)
NC_unmarried_subset <- subset(NC_data, marital == 2)
bwplot(NC_married_subset$mage)
bwplot(NC_unmarried_subset$mage)

#Weeks of gestation
favstats(NC_data$weeks)
WEEKSmean = mean(NC_data$weeks,na.rm=TRUE)
WEEKSmedian = median(NC_data$weeks,na.rm=TRUE)
WEEKSsd = sd(NC_data$weeks,na.rm=TRUE)
WEEKSIQR = iqr(NC_data$weeks,na.rm=TRUE)
WEEKSrange = max(NC_data$weeks,na.rm=TRUE)-min(NC_data$weeks,na.rm=TRUE)

histogram(NC_data$weeks)
bwplot(NC_data$weeks)
skewness(NC_data$weeks,na.rm=TRUE)
kurtosis(NC_data$weeks,na.rm=TRUE)

#Weight gain
favstats(NC_data$gained)
GAINEDmean = mean(NC_data$gained,na.rm=TRUE)
GAINEDmedian = median(NC_data$gained,na.rm=TRUE)
GAINEDsd = sd(NC_data$gained,na.rm=TRUE)
GAINEDIQR = iqr(NC_data$gained,na.rm=TRUE)
GAINEDrange = max(NC_data$gained,na.rm=TRUE)-min(NC_data$gained,na.rm=TRUE)

histogram(NC_data$gained)
bwplot(NC_data$gained)
skewness(NC_data$gained,na.rm=TRUE)
kurtosis(NC_data$gained,na.rm=TRUE)

#weight of child (ounces)
favstats(NC_data$tounces)
TOZmean = mean(NC_data$tounces,na.rm=TRUE)
TOZmedian = median(NC_data$tounces,na.rm=TRUE)
TOZsd = sd(NC_data$tounces,na.rm=TRUE)
TOZIQR = iqr(NC_data$tounces,na.rm=TRUE)
TOZrange = max(NC_data$tounces,na.rm=TRUE)-min(NC_data$tounces,na.rm=TRUE)

histogram(NC_data$tounces)
bwplot(NC_data$tounces)
skewness(NC_data$tounces,na.rm=TRUE)
kurtosis(NC_data$tounces,na.rm=TRUE)

NC_smoke_subset <- subset(NC_data, smoke == 1)
NC_nosmoke_subset <- subset(NC_data, smoke == 0)
bwplot(NC_smoke_subset$tounces,na.rm=TRUE)
bwplot(NC_nosmoke_subset$tounces,na.rm=TRUE)

#weight of child (grams)
favstats(NC_data$tgrams)
Tgmean = mean(NC_data$tgrams,na.rm=TRUE)
Tgmedian = median(NC_data$tgrams,na.rm=TRUE)
Tgsd = sd(NC_data$tgrams,na.rm=TRUE)
TgIQR = iqr(NC_data$tgrams,na.rm=TRUE)
Tgrange = max(NC_data$tgrams,na.rm=TRUE)-min(NC_data$tgrams,na.rm=TRUE)

histogram(NC_data$tgrams)
bwplot(NC_data$tgrams)
skewness(NC_data$tgrams,na.rm=TRUE)
kurtosis(NC_data$tgrams,na.rm=TRUE)


