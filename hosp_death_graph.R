library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(readxl)
setwd("C:/Users/vrw657/Documents/Projects/Corona_SJPH")

#stringency comparison
stringency <- read_excel("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Data/OxCGRT_timeseries_all.xlsx")

str_sel <- stringency[stringency$country_name == "Denmark" | 
                         stringency$country_name == "Netherlands" |
                         stringency$country_name == "France" |
                         stringency$country_name == "United Kingdom",]
countries <- str_sel$country_name
t_str_sel <- as.data.frame(t(str_sel[,3:ncol(str_sel)]))
colnames(t_str_sel) <- countries

daterange <- seq(from = as.Date("2020-01-01"), to = as.Date("2020-11-30"), by = 'day')
t_str_sel$date <- daterange

str_plot <- t_str_sel[t_str_sel$date >= "2020-02-27" &
                        t_str_sel$date <= "2020-07-04",]

str_plot_long <- tidyr::gather(str_plot, country, value, Denmark:Netherlands, factor_key=TRUE)

p <- ggplot(data = str_plot_long, aes(x=date, y= value, color=country)) + 
  geom_line() +
  labs(x = '' , y = 'OxCGRT Score') +
  theme_bw() +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  scale_x_date(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = c(.6, .1),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(1, 1, 1, 1))

pdf("Comp_plots/stringency.pdf", width = 9, height = 3)
p
dev.off()

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
  scale_y_continuous(limits = c(0,7.5), expand = c(0, 0),
                     sec.axis = sec_axis(~ . * shrink_n, name = "Cumulative number of deaths / 100,000")) +
  scale_x_date(limits = as.Date(c(mindate,maxdate)), expand = c(0, 0),
               date_breaks = "1 month", date_labels = "%B %Y") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  theme(axis.title.x=element_blank(),
        axis.title.y = element_text(color = "#E69F00"),
        axis.title.y.right = element_text(color = "#0072B2"))


tiff("Comp_plots/DK_plot_08JUL2021.tiff", units="in", width=9, height=5, res=300)
p
dev.off()

pdf("Comp_plots/pred_year_DK.pdf", width = 9, height = 2.4)
p
dev.off()
  
  
#Netherlands
corona <- read.delim("Data/Hospital_death_data_NL.txt", header = T)
corona$Date <- as.Date(corona$Date, tryFormats = c("%d/%m/%Y"))
mindate <- min(corona$Date)
maxdate <- max(corona$Date)
population <- 17280000
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
  scale_y_continuous(limits = c(0,7.5), expand = c(0, 0),
                     sec.axis = sec_axis(~ . * shrink_n, name = "Cumulative number of deaths / 100,000")) +
  scale_x_date(limits = as.Date(c(mindate,maxdate)), expand = c(0, 0)) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(color = "#E69F00"),
        axis.title.y.right = element_text(color = "#0072B2"))


pdf("Comp_plots/pred_year_NL.pdf", width = 9, height = 2.4)
p
dev.off()


#UK
corona <- read.delim("Data/Hospital_death_data_UK.txt", header = T)
corona$Date <- as.Date(corona$Date, tryFormats = c("%d/%m/%Y"))
mindate <- min(corona$Date)
maxdate <- max(corona$Date)
population <- 66650000
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
  scale_y_continuous(limits = c(0,7.5), expand = c(0, 0),
                     sec.axis = sec_axis(~ . * shrink_n, name = "Cumulative number of deaths / 100,000")) +
  scale_x_date(limits = as.Date(c(mindate,maxdate)), expand = c(0, 0)) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(color = "#E69F00"),
        axis.title.y.right = element_text(color = "#0072B2"))

pdf("Comp_plots/pred_year_UK.pdf", width = 9, height = 2.4)
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
population <- 66990000
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
  scale_y_continuous(limits = c(0,7.5), expand = c(0, 0),
                     sec.axis = sec_axis(~ . * shrink_n, name = "Cumulative number of deaths / 100,000")) +
  scale_x_date(limits = as.Date(c(mindate,maxdate)), expand = c(0, 0)) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(color = "#E69F00"),
        axis.title.y.right = element_text(color = "#0072B2"))

pdf("Comp_plots/pred_year_FR.pdf", width = 9, height = 2.4)
p
dev.off()









