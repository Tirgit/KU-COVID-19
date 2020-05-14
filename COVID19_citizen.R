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

setwd("/Users/med-tv_/Dropbox/Data")

#### LOAD LATEST CITIZEN SCIENCE DATA
d_con <- read_excel("Data stripped of free text/KUSUND_2004_AJM.xlsx")
d_con_q <- d_con[1,] # We save the questions in another data set
d_con <- d_con[-1,] # Removing the line with the questions
d_con$date <- as.Date(as.numeric(d_con$DataCollection_StartTime_Exported), origin = "1899-12-30")

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
varkeep <- c("q1", "q2", "q5", "q15", "q16_sum", "q18", "q20", "date")

d_con_selected = subset(d_con, select = varkeep )
colnames(d_con_selected) <- c("Sex", "Age", "Education", "Social_Isolation",
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
sextable <- table(d_con_selected$Sex)
n_female <- as.numeric(sextable[1])
n_male <- as.numeric(sextable[2])
n_other <- as.numeric(sextable[3])
d_con_selected$Age <- as.numeric(d_con_selected$Age)
#table(d_con_selected$Education)
d_con_selected$Education[d_con_selected$Education == "Andet, skriv:"] <- "Short-term education/Other"
d_con_selected$Education[d_con_selected$Education == "Erhvervsuddannelse/faglært"] <- "Short-term education/Other"
d_con_selected$Education[d_con_selected$Education == "Folkeskole"] <- "Short-term education/Other"
d_con_selected$Education[d_con_selected$Education == "Gymnasial uddannelse"] <- "Short-term education/Other"
d_con_selected$Education[d_con_selected$Education == "Kort videregående uddannelse"] <- "Short-term education/Other"
d_con_selected$Education[d_con_selected$Education == "Mellemlang videregående uddannelse"] <- "Middle-term education"
d_con_selected$Education[d_con_selected$Education == "Lang videregående uddannelse"] <- "Long-term education"
d_con_selected$Education <- as.factor(d_con_selected$Education)

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
d_con_selected$Date <- NULL

colnames(d_con_selected) <- c("Sex", "Age", "Education","Social Isolation",
                              "Worries", "Quality of Life",
                              "Loneliness")

# melting data
d_con_melt <- as.data.frame(reshape2::melt(d_con_selected, id.var = c("Sex", "Age", "Education")))

# all population means box and violin plots
tryp <- ggplot(data = d_con_melt, aes(x=variable, y=value)) +
  geom_violin() # can be changed to geom_boxplot()
pdf("/Users/med-tv_/Documents/Projects/Corona_SJPH/citizen_science_measures_box.pdf", width = 6, height = 3)
tryp
dev.off()

results <- d_con_melt %>%
  group_by(variable, value) %>%
  summarise(total_n = n() ) %>%
  mutate(countT = 11356) %>%
  mutate(percent = round(100*total_n/countT,2))

results_sex <- d_con_melt %>%
  group_by(variable, value, Sex) %>%
  summarise(total_n = n() )

results_sex$countT <- numeric(nrow(results_sex))
results_sex$countT[results_sex$Sex == "Female"] <- n_female
results_sex$countT[results_sex$Sex == "Male"] <- n_male
results_sex$countT[results_sex$Sex == "Other/Not specified"] <- n_other

results_sex <- results_sex %>%
  mutate(percent = round(100*total_n/countT,2))


# barplots
q <- ggplot(data = results, aes(x =variable , y = percent, fill = value, label = percent)) +
  geom_bar(stat="identity", width = 0.7) +
  #geom_text(size = 2, position = position_stack(vjust = 0.5)) +
  coord_flip() +
  ylab("Percentage") + 
  xlab("Question") +
  theme(axis.text=element_text(size=8),
       axis.title=element_text(size=8,face="bold")) +
  ggtitle("Citizen Science Survey Results") +
  theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5))

qs <- ggplot(data = results_sex, aes(x =variable , y = percent, fill = value, label = percent)) +
  facet_wrap(~ Sex) +
  geom_bar(stat="identity", width = 0.7) +
  #geom_text(size = 2, position = position_stack(vjust = 0.5)) +
  coord_flip() +
  ylab("Percentage") + 
  xlab("Question") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold")) +
  ggtitle("Citizen Science Survey Results - sex stratified") +
  theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5))

pdf("/Users/med-tv_/Documents/Projects/Corona_SJPH/citizen_science_measures.pdf", width = 6, height = 3)
q
dev.off()

pdf("/Users/med-tv_/Documents/Projects/Corona_SJPH/citizen_science_measures_sex.pdf", width = 6, height = 3)
qs
dev.off()


+
  geom_text(size = 3, position = position_stack(vjust = 0.5))

