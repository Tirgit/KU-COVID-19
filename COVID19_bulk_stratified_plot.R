#### LOAD PACKAGES & SET WORKING DIRECTORY
library(readxl)
library(knitr)
library(dplyr) 
library(ggplot2) 
library(tidyverse)
library(lubridate)

setwd("C:/Users/vrw657/Dropbox/Data")

#### LOAD LATEST CITIZEN SCIENCE DATA
d_con <- read_excel("Data stripped of free text/KUSUND_1805_AJM.xlsx")
d_con_q <- d_con[1,] # We save the questions in another data set
d_con <- d_con[-1,] # Removing the line with the questions
d_con$date <- as.Date(as.numeric(d_con$DataCollection_StartTime_Exported), origin = "1899-12-30")

### check date distribution
datecoll <- as.data.frame(table(d_con$date))
colnames(datecoll) <- c("Date", "Participants")
# first date is 26th March, so define 25th March as origin
datecoll$Date <- as.Date(as.numeric(datecoll$Date), origin = "2020-03-25")
#p <- ggplot(datecoll, aes(Date, Participants)) + 
#  geom_bar(stat = 'identity') +
#  ggtitle("Citizen Science reports per day") + 
#  theme(plot.title = element_text(hjust = 0.5))

#pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/citizen_science_reps.pdf", width = 6, height = 3)
#p
#dev.off()

### UCLA loneliness
d_con$q16_1_num <-recode(d_con$q16_1_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_con$q16_2_num <-recode(d_con$q16_2_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_con$q16_3_num <-recode(d_con$q16_3_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_con$q16_sum <- d_con$q16_1_num + d_con$q16_2_num + d_con$q16_3_num


### removing not needed variables
varkeep <- c("q1", "q2", "q5", "q12", "q14", "q15", "q16_sum", "q20")

d_con_selected <- subset(d_con, select = varkeep )
colnames(d_con_selected) <- c("Sex", "Age", "Education", "Chronic_disease", "Mental_disease",
                              "Social_Isolation", "UCLA_loneliness","Worried")

# recode variables
table(d_con_selected$Sex)
d_con_selected$Sex[d_con_selected$Sex == "Mand"] <- "Male"
d_con_selected$Sex[d_con_selected$Sex == "Kvinde"] <- "Female"
d_con_selected$Sex[d_con_selected$Sex == "Andet"] <- "Other/Not specified"
d_con_selected$Sex[d_con_selected$Sex == "Ønsker ikke at oplyse"] <- "Other/Not specified"
d_con_selected$Sex <- as.factor(d_con_selected$Sex)
levels(d_con_selected$Sex)
d_con_selected$Age <- as.numeric(d_con_selected$Age)
d_con_selected$Age_cat <- "Under 30"
d_con_selected$Age_cat[d_con_selected$Age >= 30 & d_con_selected$Age < 60] <- "Between 30 and 60"
d_con_selected$Age_cat[d_con_selected$Age >= 60] <- "Above 60"
d_con_selected$Age_cat <- as.factor(d_con_selected$Age_cat)
d_con_selected$Education[d_con_selected$Education == "Andet, skriv:"] <- "Short-term education/Other"
d_con_selected$Education[d_con_selected$Education == "Erhvervsuddannelse/faglært"] <- "Short-term education/Other"
d_con_selected$Education[d_con_selected$Education == "Folkeskole"] <- "Short-term education/Other"
d_con_selected$Education[d_con_selected$Education == "Gymnasial uddannelse"] <- "Short-term education/Other"
d_con_selected$Education[d_con_selected$Education == "Kort videregående uddannelse"] <- "Short-term education/Other"
d_con_selected$Education[d_con_selected$Education == "Mellemlang videregående uddannelse"] <- "Middle-term education"
d_con_selected$Education[d_con_selected$Education == "Lang videregående uddannelse"] <- "Long-term education"
d_con_selected$Education <- as.factor(d_con_selected$Education)
table(d_con_selected$Chronic_disease)
d_con_selected$Chronic_disease[d_con_selected$Chronic_disease == "Nej"] <- "No chronic disease"
d_con_selected$Chronic_disease[d_con_selected$Chronic_disease == "Ja"] <- "Previous chronic disease"
d_con_selected$Chronic_disease <- as.factor(d_con_selected$Chronic_disease)
levels(d_con_selected$Chronic_disease)
table(d_con_selected$Mental_disease)
d_con_selected$Mental_disease[d_con_selected$Mental_disease == "Nej"] <- "No mental illness"
d_con_selected$Mental_disease[d_con_selected$Mental_disease == "Ja"] <- "Previous mental illness"
d_con_selected$Mental_disease <- as.factor(d_con_selected$Mental_disease)
levels(d_con_selected$Mental_disease)

# recode extremes
d_con_selected$Social_Isolation[d_con_selected$Social_Isolation == "1 – Slet ikke"] <- "1"
d_con_selected$Social_Isolation[d_con_selected$Social_Isolation == "10 – Fuldstændigt"] <- "10"
d_con_selected$Social_Isolation <- as.numeric(d_con_selected$Social_Isolation)

d_con_selected$Worried[d_con_selected$Worried == "1 – Ikke bekymret"] <- "1"
d_con_selected$Worried[d_con_selected$Worried == "10 – Meget bekymret"] <- "10"
d_con_selected$Worried <- as.numeric(d_con_selected$Worried)

# categorize worries and isolation
d_con_selected$Social_Isolation_high <- "No"
d_con_selected$Worries_high <- "No"
d_con_selected$Lonely_high <- "No"
d_con_selected$Social_Isolation_high[d_con_selected$Social_Isolation > 6] <- "Yes"
d_con_selected$Worries_high[d_con_selected$Worried > 6] <- "Yes"
d_con_selected$Lonely_high[d_con_selected$UCLA_loneliness > 6] <- "Yes"
d_con_selected$Social_Isolation_high <- as.factor(d_con_selected$Social_Isolation_high)
d_con_selected$Worries_high <- as.factor(d_con_selected$Worries_high)
d_con_selected$Lonely_high <- as.factor(d_con_selected$Lonely_high)

d_con_selected$Worried <- NULL
d_con_selected$Social_Isolation <- NULL
d_con_selected$UCLA_loneliness <- NULL
d_con_selected$Age <- NULL

summary(d_con_selected)
colnames(d_con_selected) <- c("Sex", "Education", "Chronic", "Mental", "Age",
                              "Very isolated", "Very worried", "Very lonely")

#load and merge Epinion
epinion <- readRDS("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Data/Epinion_bulk.rds")
merged_data <- rbind(d_con_selected, epinion)
merged_data$`Very isolated` <- NULL
merged_data$`Very worried` <- NULL

#load and merge DNBC
DNBC <- readRDS("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Data/DNBC_bulk.rds")
merged_data <- rbind(merged_data, DNBC)


# melting data
d_con_melt <- as.data.frame(reshape2::melt(merged_data, id.var = c("Sex", "Age", 
                                                                      "Education", 
                                                                      "Chronic", "Mental")))
#all
results <- d_con_melt %>%
  group_by(variable, value) %>%
  summarise(total_n = n() ) %>%
  mutate(countT = nrow(merged_data)) %>%
  mutate(percent = round(100*total_n/countT,2))
results <- results[results$value == "Yes",] 
results$Strata <- "All"

#sex stratified
results_sex <- d_con_melt %>%
  group_by(variable, value, Sex) %>%
  summarise(total_n = n() )
results_sex$countT <- c(0)
for (i in unique(merged_data$Sex)) {
  results_sex$countT[results_sex$Sex == i] <- sum(merged_data$Sex == i)
}
results_sex <- results_sex %>%
  mutate(percent = round(100*total_n/countT,2))
results_sex <- results_sex[results_sex$value == "Yes",] 
results_sex$Sex <- as.character(results_sex$Sex)
results_sex$Strata <- as.character(results_sex$Sex)
results_sex$Sex <- NULL

#age stratified
results_age <- d_con_melt %>%
  group_by(variable, value, Age) %>%
  summarise(total_n = n() )
results_age$countT <- c(0)
for (i in unique(merged_data$Age)) {
  results_age$countT[results_age$Age == i] <- sum(merged_data$Age == i)
}
results_age <- results_age %>%
  mutate(percent = round(100*total_n/countT,2))
results_age <- results_age[results_age$value == "Yes",] 
results_age$Age <- as.character(results_age$Age)
results_age$Strata <- as.character(results_age$Age)
results_age$Age <- NULL


#educ stratified
results_educ <- d_con_melt %>%
  group_by(variable, value, Education) %>%
  summarise(total_n = n() )
results_educ <- results_educ[!is.na(results_educ$Education),]
results_educ$countT <- c(0)
for (i in unique(merged_data$Education)[1:3]) {
  results_educ$countT[results_educ$Education == i] <- sum(merged_data$Education == i, na.rm = T)
}
results_educ <- results_educ %>%
  mutate(percent = round(100*total_n/countT,2))
results_educ <- results_educ[results_educ$value == "Yes",] 
results_educ$Strata <- as.character(results_educ$Education)
results_educ$Education <- NULL

#chronic stratified
results_chron <- d_con_melt %>%
  group_by(variable, value, Chronic) %>%
  summarise(total_n = n() )
results_chron$countT <- c(0)
for (i in unique(merged_data$Chronic)) {
  results_chron$countT[results_chron$Chronic == i] <- sum(merged_data$Chronic == i)
}
results_chron <- results_chron %>%
  mutate(percent = round(100*total_n/countT,2))
results_chron <- results_chron[results_chron$value == "Yes",] 
results_chron$Strata <- as.character(results_chron$Chronic)
results_chron$Chronic <- NULL

#mental stratified
results_ment <- d_con_melt %>%
  group_by(variable, value, Mental) %>%
  summarise(total_n = n() )
results_ment$countT <- c(0)
for (i in unique(merged_data$Mental)) {
  results_ment$countT[results_ment$Mental == i] <- sum(merged_data$Mental == i)
}
results_ment <- results_ment %>%
  mutate(percent = round(100*total_n/countT,2))
results_ment <- results_ment[results_ment$value == "Yes",] 
results_ment$Strata <- as.character(results_ment$Mental)
results_ment$Mental <- NULL


all_res <- rbind(results,results_sex,results_age,results_educ,results_chron,results_ment)
all_res$barcolor <- "red"
all_res$barcolor[all_res$Strata == "All"] <- "blue"
all_res <- all_res[all_res$Strata != "Other/Not specified",]
res_lonely <- all_res[all_res$variable == "Very lonely",] 
res_lonely$Category <- c("All", "Gender", "Gender", "Age", "Age", "Age",
                         "Education", "Education", "Education",
                         "Chronic disease", "Chronic disease",
                         "Mental illness", "Mental illness")
res_lonely$Category <- as.factor(res_lonely$Category)
res_lonely$Category <- factor(res_lonely$Category, levels=c("All", "Gender", "Age", 
                                                            "Education", "Chronic disease",
                                                            "Mental illness"))


#loneliness plot
q <- ggplot(data = res_lonely, aes(x = reorder(Strata, percent), y = percent, fill = barcolor)) +
  facet_grid(~Category, scale = "free", space = "free") +
  geom_bar(stat="identity", width = 0.7) +
  xlab("Isolation") +
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_y_continuous(limits = c(0,30), expand = c(0, 0)) +
  #ggtitle("Proportion of individuals with high levels of loneliness") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(face = c('plain','plain','plain','plain','plain',
                                            'plain','bold','plain','plain','plain',
                                            'plain','plain'))) +
  theme(axis.text.x = element_text(size = c(8,8,8,8,8,8,12,8,8,8,8,8))) + 
  theme(strip.text.x = element_text(size = 7))
#scale_x_discrete(labels=c("All"=expression(bold("All")), parse=TRUE))


pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/DK_stratified_loneliness.pdf", width = 6, height = 3)
q
dev.off()


