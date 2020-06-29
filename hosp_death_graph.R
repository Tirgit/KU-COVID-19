library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyverse)
setwd("/Users/med-tv_/Documents/Projects/Corona_SJPH")

#Denmark
corona <- read.delim("Hospital_death_data_DK.txt", header = T)
corona$Date <- as.Date(corona$Date)
mindate <- min(corona$Date)
maxdate <- max(corona$Date)
upper_lim <- max(corona$Hospitalization, na.rm = T)*1.1

p <- ggplot(corona, aes(x = Date, y = Hospitalization)) + 
  geom_line(aes(y=Hospitalization), color="#E69F00", size = 1) +
  geom_point(aes(y=Hospitalization), color="#E69F00") +
  geom_area(aes(y=Death_cumulative), fill="#0072B2", alpha=0.5) +
  labs(x = '' , y = '') +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(limits = c(0,upper_lim), expand = c(0, 0)) +
  scale_x_date(limits = as.Date(c(mindate,maxdate))) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


pdf("pred_year_DK.pdf", width = 9, height = 3)
p
dev.off()
  
  
#Netherlands
corona <- read.delim("Hospital_death_data_NL.txt", header = T)
corona$Date <- as.Date(corona$Date)
mindate <- min(corona$Date)
maxdate <- max(corona$Date)
upper_lim <- max(corona$Death_cumulative, na.rm = T)*1.1

p <- ggplot(corona, aes(x = Date, y = Hospitalization)) + 
  geom_line(aes(y=Hospitalization), color="#E69F00", size = 1) +
  geom_point(aes(y=Hospitalization), color="#E69F00") +
  geom_area(aes(y=Death_cumulative), fill="#0072B2", alpha=0.5) +
  labs(x = '' , y = '') +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(limits = c(0,upper_lim), expand = c(0, 0)) +
  scale_x_date(limits = as.Date(c(mindate,maxdate))) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


pdf("pred_year_NL.pdf", width = 9, height = 3)
p
dev.off()



#France
corona <- read.delim("Hospital_death_data_FR.txt", header = T)
corona$Date <- as.Date(corona$Date)
mindate <- min(corona$Date)
maxdate <- max(corona$Date)
upper_lim <- max(corona$Hospitalization, na.rm = T)*1.1

p <- ggplot(corona, aes(x = Date, y = Hospitalization)) + 
  geom_line(aes(y=Hospitalization), color="#E69F00", size = 1) +
  geom_point(aes(y=Hospitalization), color="#E69F00") +
  geom_area(aes(y=Death_cumulative), fill="#0072B2", alpha=0.5) +
  labs(x = '' , y = '') +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(limits = c(0,upper_lim), expand = c(0, 0)) +
  scale_x_date(limits = as.Date(c(mindate,maxdate))) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


pdf("pred_year_FR.pdf", width = 9, height = 3)
p
dev.off()

