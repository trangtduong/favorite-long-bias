# Please set the working directory: 
#     Session > Set Working Directory > To Source File Location
# Or change the following code to the path to the folder that contains this
# file and data
setwd("~/Duong_thesis_data_and_codes")

#--------------------------------------------------------------------------------------
# Install and Load required packages:
install.packages('tidyverse')
install.packages('dplyr')
install.packages("lubridate")
install.packages("ggplot2")

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

#--------------------------------------------------------------------------------------
# Create read_plus function - customize function for reading files
read_plus <- function(flnm) {
  read.csv(flnm, header = FALSE, col.names = paste0('V', seq(1,30,by = 1)), sep = ';',colClasses = c('character'), na.strings = c('', 'NA', 'N/A'), fill = TRUE, strip.white = TRUE, stringsAsFactors = FALSE, blank.lines.skip = TRUE) %>% 
    mutate(file_path = flnm)
}
#--------------------------------------------------------------------------------------
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#---------------------------------------------------------------------------------
# Binomial test function
binomial.test.p.value <- function(x, n, p, alternative = c('two.sided', 'less', 'greater'), conf.level = 0.95) {
  binom.test(x, n, p, alternative, conf.level) $p.value
}
binomial.test.estimate <- function(x, n, p, alternative = c('two.sided', 'less', 'greater'), conf.level = 0.95) {
  binom.test(x, n, p, alternative, conf.level) $estimate
}
binomial.test.conf.int <- function(x, n, p, alternative = c('two.sided', 'less', 'greater'), conf.level = 0.95) {
  binom.test(x, n, p, alternative, conf.level) $conf.int
}

binomial.test.conf.int1 <- function(x, n, conf.level = 0.95) {
  binom.test(x, n, conf.level = conf.level)$conf.int[1]
}
binomial.test.conf.int2 <- function(x, n, conf.level = 0.95) {
  binom.test(x, n, conf.level = conf.level)$conf.int[2]
}
#---------------------------------------------------------------------------------

