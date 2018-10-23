#' ---
#' title: "STA 610/615 Week 01"
#' author: "D. Zeitler"
#' date: "August, 2017"
#' output: word_document
#' ---
#' 
#' Get some packages installed/loaded.
#' _library(tidyverse)_
#' *bold*
#' 
## ----Setup---------------------------------------------------------------
library(tidyverse)
#install.packages('ggformula')
library(mosaic)
library(ggformula)

#' 
#' We might want to change some preferences in RStudio.
#' 
#' ![Preferences](PreferencePanel.png)
#' 
#' Now let's take a look at a data file. We'll start with Fisher's_Iris.xls from Moodle.
## ------------------------------------------------------------------------
#install.packages('readxl')
library(readxl)
Fisher_s_Iris <- read_excel("Fishers_Iris.xls",
                            skip = 3,
                            col_names = c('SpeciesNum','Species',
                                          'PetalWidth','PetalLength',
                                          'SepalLength', 'SepalWidth')
                            )
Fisher_s_Iris

#' 
#' We can save data frames in CSV format.
## ---- eval=FALSE, include=FALSE------------------------------------------
## library(readr)
## write_csv(Fisher_s_Iris,path='FishersIris.csv')
## Fisher_s_Iris %>% write_csv('FishersIris.csv')

#' 
#' They are then easily read back in.
## ------------------------------------------------------------------------
#fi <- read_csv('FishersIris.csv')
#read_csv('FishersIris.csv') -> fi
#read_csv('FishersIris.csv') %>% summary()

#' 
#' The mosaic package.
#' 
#' goal(y~x|z, data=..., groups=...)
#' y - dependent variable or vertical axis
#' x - independent variable or horizontal axis
#' z - conditioning variable (panels)
#' groups - conditioning variable (overlay)
#' 
#' Let's try it.
## ------------------------------------------------------------------------
mosaic::bargraph(~Species,data=Fisher_s_Iris)
gf_bar(~Species,data=Fisher_s_Iris)

#' 
#' Alternatively, using the magittr pipes.
## ------------------------------------------------------------------------
Fisher_s_Iris %>% gf_bar(~Species)

#' 
#' Something more interesting. Note we can drop the 'data = ' if the order of paramters in the function call match their defined order. It's probably a good idea to get in the habit of explicitely using 'data ='. 
## ------------------------------------------------------------------------
histogram(~PetalWidth,data=Fisher_s_Iris)
Fisher_s_Iris %>% gf_histogram(~PetalWidth)

## ------------------------------------------------------------------------
mean(~PetalWidth,data=Fisher_s_Iris)
sd(~PetalWidth,data=Fisher_s_Iris)
favstats(~PetalWidth,data=Fisher_s_Iris)
favstats(PetalWidth~Species,data=Fisher_s_Iris)

#' 
#' Now let's get a box plot of exactly the same data.
## ------------------------------------------------------------------------
bwplot(~PetalWidth,data=Fisher_s_Iris)
# Note single box plots with ggformula currently doesn't work as expected
#Fisher_s_Iris %>% gf_boxplot(~PetalWidth)
# We can get it with this, but I prefer bwplot.
Fisher_s_Iris %>% gf_boxplot(PetalWidth~Species)
# Actually, single sample box plots are rather useless. Use histograms instead.

#' 
## ------------------------------------------------------------------------
xyplot( PetalWidth ~ SepalWidth, data = Fisher_s_Iris)
gf_point(Fisher_s_Iris, PetalWidth ~ SepalWidth)

gf_point(Fisher_s_Iris, PetalWidth ~ SepalWidth) %>%
  gf_smooth()

gf_point(Fisher_s_Iris, PetalWidth ~ SepalWidth) %>%
  gf_smooth(se=T)

# The following code picks a random sample with n items from the dataset Fisher_s_Iris
Fisher_s_Iris %>%
  sample_n(10) %>% summary()

Fisher_s_Iris %>%
  sample_n(10) %>%
  select(PetalWidth) #This slices the data specifically for the variable PetalWidth
