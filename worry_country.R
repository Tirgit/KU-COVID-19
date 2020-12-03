library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(readxl)
setwd("C:/Users/vrw657/Documents/Projects/Corona_SJPH")

#worry comparison
worry <- read_excel("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Data/worry_timeseries_all.xlsx")
worry$Date <- as.Date(worry$Date)

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p <- ggplot(data = worry, aes(x=Date, y= worry, color=cohort)) + 
  scale_colour_manual(values=cbp1) +
  geom_point() +
  geom_errorbar(aes(ymin=worry_lCI, ymax=worry_uCI), width=2) +
  geom_line() +
  labs(x = '' , y = 'Mean level of worries') +
  theme_bw() +
  scale_y_continuous(limits = c(0,10), expand = c(0, 0)) +
  scale_x_date(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

pdf("Comp_plots/worries.pdf", width = 9, height = 4)
p
dev.off()
