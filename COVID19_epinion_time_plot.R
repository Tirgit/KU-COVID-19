#### LOAD PACKAGES & SET WORKING DIRECTORY
library(knitr)
library(dplyr) 
library(ggplot2)
library(tidyverse)
library(lubridate)

# run shared .Rmd file until load of these 3 are done and save
#saveRDS(d_pop, "C:/Users/vrw657/Documents/Projects/Corona_SJPH/d_pop.rds")
#saveRDS(d_fam, "C:/Users/vrw657/Documents/Projects/Corona_SJPH/d_fam.rds")
#saveRDS(d_eld, "C:/Users/vrw657/Documents/Projects/Corona_SJPH/d_eld.rds")

# merging 3 datasets
d_pop <- readRDS("C:/Users/vrw657/Documents/Projects/Corona_SJPH/d_pop.rds")
d_fam <- readRDS("C:/Users/vrw657/Documents/Projects/Corona_SJPH/d_fam.rds")
d_eld <- readRDS("C:/Users/vrw657/Documents/Projects/Corona_SJPH/d_eld.rds")

varkeep <- c("q1", "q2", "q5", "q12", "q15", "q16_1_resp",
             "q16_2_resp", "q16_3_resp", "q20", "date")

d_pop_selected = subset(d_pop, select = varkeep )
d_fam_selected = subset(d_fam, select = varkeep )
d_eld_selected = subset(d_eld, select = varkeep )

d_merged <- rbind(d_pop_selected,d_fam_selected,d_eld_selected)
table(d_merged$date)

### UCLA loneliness
d_merged$q16_1_num <-recode(d_merged$q16_1_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_merged$q16_2_num <-recode(d_merged$q16_2_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_merged$q16_3_num <-recode(d_merged$q16_3_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_merged$q16_sum <- d_merged$q16_1_num + d_merged$q16_2_num + d_merged$q16_3_num

varkeep <- c("q1", "q2", "q5", "q12", "q15", "q16_sum", "q20", "date")
d_con_selected <- subset(d_merged, select = varkeep )
colnames(d_con_selected) <- c("Sex", "Age", "Education", "Chronic_disease",
                        "Social_Isolation", "UCLA_loneliness", "Worried", "Date")


### check date distribution
#datecoll <- as.data.frame(table(d_con_selected$Date))
#colnames(datecoll) <- c("Date", "Participants")
# first date is 20th March, so define 19th March as origin
#datecoll$Date <- as.Date(as.numeric(datecoll$Date), origin = "2020-03-19")
#p <- ggplot(datecoll, aes(Date, Participants)) + 
#  geom_bar(stat = 'identity') +
#  ggtitle("Epinion reports per day") + 
#  theme(plot.title = element_text(hjust = 0.5))

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
d_con_selected$Chronic_disease[d_con_selected$Chronic_disease == "Ja"] <- "Chronic disease"
d_con_selected$Chronic_disease <- as.factor(d_con_selected$Chronic_disease)
levels(d_con_selected$Chronic_disease)

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
colnames(d_con_selected) <- c("Sex", "Education", "Chronic", "Date", "Age",
                              "Very isolated", "Very worried", "Very lonely")

# save Epinion for bulk merge
#drops <- c("Date")
#d_con_bulkmerge <- d_con_selected[ , !(names(d_con_selected) %in% drops)]
#col_order <- c("Sex", "Age", "Education", "Chronic", 
#               "Very isolated", "Very worried", "Very lonely")
#d_con_bulkmerge <- d_con_bulkmerge[, col_order]
#saveRDS(d_con_bulkmerge, "C:/Users/vrw657/Documents/Projects/Corona_SJPH/Epinion_bulk.rds")

d_con_selected$Sex <- NULL
d_con_selected$Education <- NULL
d_con_selected$Chronic <- NULL
d_con_selected$Age <- NULL

# melting data
d_con_melt <- as.data.frame(reshape2::melt(d_con_selected, id.var = c("Date")))

# calculating proportions
results <- d_con_melt %>%
  group_by(variable, value, Date) %>%
  summarise(total_n = n() ) 
results$countT <- c(0)
for (i in unique(d_con_selected$Date)) {
  results$countT[results$Date == i] <- sum(d_con_selected$Date == i)
}
results <- results[results$value == "Yes",] 
results <- results %>%
  mutate(percent = round(100*total_n/countT,2))

varkeep <- c("variable", "Date", "percent")
res <- subset(results, select = varkeep )
res_worry <- res[res$variable == "Very worried",] 


p <- ggplot(data = res_worry, aes(x = Date, y = percent, fill = "red")) +
  geom_bar(stat="identity", width = 0.7) +
  scale_x_date(date_labels="%d %b",date_breaks  ="3 day") + 
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_y_continuous(limits = c(0,70), expand = c(0, 0)) +
  expand_limits(x = as.Date(c("2020-02-28", "2020-05-17"))) +
  ggtitle("Proportion of individuals who are very worried about the crisis") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none") 

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Epinion_worry_time.pdf", width = 9, height = 3)
p
dev.off()





