#### LOAD PACKAGES & SET WORKING DIRECTORY
library(readxl)
library(knitr)
library(haven)
library(radarchart)
library(readxl)
library(wesanderson)
library(dplyr) 
library(ggplot2) 
library(GDAtools)
library(tidyverse)
library(lubridate)

setwd("/Users/med-tv_/Dropbox/Data")

#### LOAD LATEST CITIZEN SCIENCE DATA
d_con <- read_excel("Data stripped of free text/KUSUND_2004_AJM.xlsx")
d_con_q <- d_con[1,] # We save the questions in another data set
d_con <- d_con[-1,] # Removing the line with the questions
d_con$date <- as.character(as.Date(as.numeric(d_con$DataCollection_StartTime_Exported), origin = "1899-12-30"))
d_con$date <- as.Date(d_con$date)

### check date distribution
datecoll <- as.data.frame(table(d_con$date))
colnames(datecoll) <- c("Date", "Participants")
# first date is 26th March, so define 25th March as origin
datecoll$Date <- as.Date(as.numeric(datecoll$Date), origin = "2020-03-25")
p <- ggplot(datecoll, aes(Date, Participants)) + 
  geom_bar(stat = 'identity') +
  ggtitle("Citizen Science reports per day") + 
  theme(plot.title = element_text(hjust = 0.5))

pdf("/Users/med-tv_/Documents/Projects/Corona_SJPH/citizen_science_reps.pdf", width = 6, height = 3)
p
dev.off()

### UCLA loneliness
d_con$q16_1_num <-recode(d_con$q16_1_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_con$q16_2_num <-recode(d_con$q16_2_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_con$q16_3_num <-recode(d_con$q16_3_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_con$q16_sum <- d_con$q16_1_num + d_con$q16_2_num + d_con$q16_3_num

### removing not needed variables
varkeep <- c("q1", "q2", "q15", "q16_sum", "q18", "q20", "date")

d_con_selected = subset(d_con, select = varkeep )
colnames(d_con_selected) <- c("Sex", "Age", "Social_Isolation",
                              "Loneliness", "Quality_of_Life",
                              "Worried", "Date")
# recode variables
table(d_con_selected$Sex)
d_con_selected$Sex[d_con_selected$Sex == "Mand"] <- "Male"
d_con_selected$Sex[d_con_selected$Sex == "Kvinde"] <- "Female"
d_con_selected$Sex[d_con_selected$Sex == "Andet"] <- "Other/Not specified"
d_con_selected$Sex[d_con_selected$Sex == "Ønsker ikke at oplyse"] <- "Other/Not specified"
d_con_selected$Sex <- as.factor(d_con_selected$Sex)
levels(d_con_selected$Sex)
table(d_con_selected$Sex)
n_female <- 8744
n_male <- 2576
n_other <- 36
d_con_selected$Age <- as.numeric(d_con_selected$Age)

# recode extremes
d_con_selected$Social_Isolation[d_con_selected$Social_Isolation == "1 – Slet ikke"] <- "1"
d_con_selected$Social_Isolation[d_con_selected$Social_Isolation == "10 – Fuldstændigt"] <- "10"
d_con_selected$Social_Isolation <- as.numeric(d_con_selected$Social_Isolation)

d_con_selected$Quality_of_Life[d_con_selected$Quality_of_Life == "1 – Dårlig"] <- "1"
d_con_selected$Quality_of_Life[d_con_selected$Quality_of_Life == "10 - Fremragende"] <- "10"
d_con_selected$Quality_of_Life <- as.numeric(d_con_selected$Quality_of_Life)

d_con_selected$Worried[d_con_selected$Worried == "1 – Ikke bekymret"] <- "1"
d_con_selected$Worried[d_con_selected$Worried == "10 – Meget bekymret"] <- "10"
d_con_selected$Worried <- as.numeric(d_con_selected$Worried)

summary(d_con_selected)

# reverse quality of life
d_con_selected$Rev_Quality_of_Life <- 11 - d_con_selected$Quality_of_Life

# convert loneliness to a scale 1-10
d_con_selected$Loneliness_recode <- NULL
d_con_selected$Loneliness_recode[d_con_selected$Loneliness == 3] <- 1
d_con_selected$Loneliness_recode[d_con_selected$Loneliness == 4] <- 2.5
d_con_selected$Loneliness_recode[d_con_selected$Loneliness == 5] <- 4
d_con_selected$Loneliness_recode[d_con_selected$Loneliness == 6] <- 5.5
d_con_selected$Loneliness_recode[d_con_selected$Loneliness == 7] <- 7
d_con_selected$Loneliness_recode[d_con_selected$Loneliness == 8] <- 8.5
d_con_selected$Loneliness_recode[d_con_selected$Loneliness == 9] <- 10

# now worries, loneliness and quality are coded the same
summary(d_con_selected)
d_con_selected$Quality_of_Life <- NULL
d_con_selected$Loneliness <- NULL

colnames(d_con_selected) <- c("Sex", "Age", "Social Isolation",
                              "Worries", "Date", "Quality of Life",
                              "Loneliness")

d_con_melt <- as.data.frame(reshape2::melt(d_con_selected, id.var = c("Sex", "Age", "Date")))

results <- d_con_melt %>%
  group_by(Date, variable) %>%
  summarise(mean.value = mean(value, na.rm = T),
            sd.value = sd(value, na.rm = T),
            n.value = n()) 
#remove first four lines (NAs for first day bc of not enough data)
results <- results[-(1:4),]
results_ci <- results %>%
  mutate(se.value = sd.value / sqrt(n.value),
         lower.ci = mean.value - qt(1 - (0.05 / 2), n.value - 1) * se.value,
         upper.ci = mean.value + qt(1 - (0.05 / 2), n.value - 1) * se.value)


p <- ggplot(results_ci, aes(Date)) + 
  facet_wrap(~ variable) +
  geom_line(aes(y=mean.value), colour="blue") + 
  geom_ribbon(aes(ymin=lower.ci, ymax=upper.ci), alpha=0.2) +
  ylab("Mean value") +
  ggtitle("Citizen Science Survey Results - progress with time") +
  theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5))

pdf("/Users/med-tv_/Documents/Projects/Corona_SJPH/citizen_science_measures_time.pdf", width = 6, height = 3)
p
dev.off()






  
  
  
  
  
  