library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(readxl)
setwd("C:/Users/vrw657/Documents/Projects/Corona_SJPH")

#Denmark
corona <- read.delim("Data/Hospital_death_data_DK.txt", header = T)
corona$Date <- as.Date(corona$Date, tryFormats = c("%d/%m/%Y"))
mindate <- min(corona$Date)
maxdate <- max(corona$Date)
population <- 5806000
corona$Death_cumulative <- (corona$Death_cumulative * 100000)/population
corona$Hospitalization <- (corona$Hospitalization * 100000)/population
shrink_n <- 10
#upper_lim <- max(corona$Death_cumulative/shrink_n, na.rm = T)*1.1

p <- ggplot(corona, aes(x = Date, y = Hospitalization)) + 
  geom_line(aes(y=Hospitalization), color="#E69F00", size = 1) +
  geom_point(aes(y=Hospitalization), color="#E69F00") +
  geom_area(aes(y=Death_cumulative/shrink_n), fill="#0072B2", alpha=0.5) +
  labs(x = '' , y = 'New hospitalizations / 100,000') +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(limits = c(0,4), expand = c(0, 0),
                     sec.axis = sec_axis(~ . * shrink_n, name = "Cumulative number of deaths / 100,000")) +
  scale_x_date(limits = as.Date(c(mindate,maxdate)), expand = c(0, 0),
               date_breaks = "months" , date_labels = "%b") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  theme(axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(color = "#E69F00"),
        axis.title.y.right = element_text(color = "#0072B2"))


pdf("Comp_plots/Andrea_DK_plot.pdf", width = 9, height = 5)
p
dev.off()

