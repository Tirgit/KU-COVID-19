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
upper_lim <- max(corona$Death_cumulative/4, na.rm = T)*1.1

p <- ggplot(corona, aes(x = Date, y = Hospitalization)) + 
  geom_line(aes(y=Hospitalization), color="#E69F00", size = 1) +
  geom_point(aes(y=Hospitalization), color="#E69F00") +
  geom_area(aes(y=Death_cumulative/4), fill="#0072B2", alpha=0.5) +
  labs(x = '' , y = 'New hospitalizations') +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(limits = c(0,upper_lim), expand = c(0, 0),
                     sec.axis = sec_axis(~ . * 4, name = "Cumulative number of deaths")) +
  scale_x_date(limits = as.Date(c(mindate,maxdate)), expand = c(0, 0)) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(color = "#E69F00"),
        axis.title.y.right = element_text(color = "#0072B2"))
  
axis.line.y.right = element_line(color = "red")
  


# add second axis for deaths!


pdf("DK_plots/pred_year_DK.pdf", width = 9, height = 3)
p
dev.off()
  
  
#Netherlands
corona <- read.delim("Data/Hospital_death_data_NL.txt", header = T)
corona$Date <- as.Date(corona$Date, tryFormats = c("%d/%m/%Y"))
mindate <- min(corona$Date)
maxdate <- max(corona$Date)
upper_lim <- max(corona$Death_cumulative/4, na.rm = T)*1.1

p <- ggplot(corona, aes(x = Date, y = Hospitalization)) + 
  geom_line(aes(y=Hospitalization), color="#E69F00", size = 1) +
  geom_point(aes(y=Hospitalization), color="#E69F00") +
  geom_area(aes(y=Death_cumulative/4), fill="#0072B2", alpha=0.5) +
  labs(x = '' , y = 'New hospitalizations') +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(limits = c(0,upper_lim), expand = c(0, 0),
                     sec.axis = sec_axis(~ . * 4, name = "Cumulative number of deaths")) +
  scale_x_date(limits = as.Date(c(mindate,maxdate)), expand = c(0, 0)) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(color = "#E69F00"),
        axis.title.y.right = element_text(color = "#0072B2"))


pdf("NL_plots/pred_year_NL.pdf", width = 9, height = 3)
p
dev.off()


#UK
corona <- read.delim("Data/Hospital_death_data_UK.txt", header = T)
corona$Date <- as.Date(corona$Date, tryFormats = c("%d/%m/%Y"))
mindate <- min(corona$Date)
maxdate <- max(corona$Date)
upper_lim <- max(corona$Death_cumulative/4, na.rm = T)*1.1

p <- ggplot(corona, aes(x = Date, y = Hospitalization)) + 
  geom_line(aes(y=Hospitalization), color="#E69F00", size = 1) +
  geom_point(aes(y=Hospitalization), color="#E69F00") +
  geom_area(aes(y=Death_cumulative/4), fill="#0072B2", alpha=0.5) +
  labs(x = '' , y = 'New hospitalizations') +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(limits = c(0,upper_lim), expand = c(0, 0),
                     sec.axis = sec_axis(~ . * 4, name = "Cumulative number of deaths")) +
  scale_x_date(limits = as.Date(c(mindate,maxdate)), expand = c(0, 0)) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(color = "#E69F00"),
        axis.title.y.right = element_text(color = "#0072B2"))

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
upper_lim <- max(corona$Death_cumulative/4, na.rm = T)*1.1

p <- ggplot(corona, aes(x = Date, y = Hospitalization)) + 
  geom_line(aes(y=Hospitalization), color="#E69F00", size = 1) +
  geom_point(aes(y=Hospitalization), color="#E69F00") +
  geom_area(aes(y=Death_cumulative/4), fill="#0072B2", alpha=0.5) +
  labs(x = '' , y = 'New hospitalizations') +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(limits = c(0,upper_lim), expand = c(0, 0),
                     sec.axis = sec_axis(~ . * 4, name = "Cumulative number of deaths")) +
  scale_x_date(limits = as.Date(c(mindate,maxdate)), expand = c(0, 0)) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(color = "#E69F00"),
        axis.title.y.right = element_text(color = "#0072B2"))

pdf("FR_plots/pred_year_FR.pdf", width = 9, height = 3)
p
dev.off()








