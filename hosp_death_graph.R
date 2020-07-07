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


# add second axis for deaths!
+ 
  scale_y_continuous(
    "mpg (US)", 
    sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
  )


pdf("DK_plots/pred_year_DK.pdf", width = 9, height = 3)
p
dev.off()
  
  
#Netherlands
corona <- read.delim("Data/Hospital_death_data_NL.txt", header = T)
corona$Date <- as.Date(corona$Date, tryFormats = c("%d/%m/%Y"))
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


pdf("NL_plots/pred_year_NL.pdf", width = 9, height = 3)
p
dev.off()


#UK
corona <- read.delim("Data/Hospital_death_data_UK.txt", header = T)
corona$Date <- as.Date(corona$Date, tryFormats = c("%d/%m/%Y"))
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


pdf("UK_plots/pred_year_UK.pdf", width = 9, height = 3)
p
dev.off()



#France
hosp <- read_excel("Data/FR_newly_admitted_hospital.xlsx")
hosp$Date <- as.Date(hosp$jour)

FR_daily_hosp <- hosp %>%
  group_by(Date) %>%
  summarise(Hospitalization = sum(incid_hosp))

# sum(FR_daily_hosp$Hospitalization)

death <- read.delim("Data/Hospital_death_data_FR.txt", header = T)
death$Date <- as.Date(death$Date, tryFormats = c("%d/%m/%Y"))
corona <- full_join(death, FR_daily_hosp, by = "Date")

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


pdf("FR_plots/pred_year_FR.pdf", width = 9, height = 3)
p
dev.off()








