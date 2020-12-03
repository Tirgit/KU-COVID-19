#### LOAD PACKAGES & SET WORKING DIRECTORY
library(haven)
library(knitr)
library(dplyr) 
library(ggplot2)
library(tidyverse)
library(lubridate)
library(naniar)
library(stats)

# read original SAS files and saveRDS
#W1 <- read_sas("P:/W1_23april2020_rettet11maj.sas7bdat")
#saveRDS(W1, "P:/DNBC_W1.rds")

# merging 3 datasets
W1 <- readRDS("P:/DNBC_W1.rds")

# narrow down to youth / mothers
# youth
# W1 <- W1[W1$Besvarelse_type == 2,]
# mothers
# W1 <- W1[W1$Besvarelse_type == 1,]


varkeep <- c("H_alder", "H002", "H017", "H048_1", 
             "H048_2", "H048_3", "H048_4", "H048_5", 
             "H048_6", "H048_7", "H048_8", "H048_9A", 
             "H049", "H052", "H057", "H058", "H059")

W1_selected <- subset(W1, select = varkeep )
W1_selected[W1_selected == 99] <- NA
W1_selected[W1_selected == "99"] <- NA
W1_selected[W1_selected == "100"] <- NA

### UCLA loneliness
W1_selected$UCLA_loneliness <- NA
W1_selected$UCLA_loneliness <- W1_selected$H057 + W1_selected$H058 + W1_selected$H059
summary(W1_selected$UCLA_loneliness)
W1_selected$Lonely_high <- NA
W1_selected$Lonely_high[W1_selected$UCLA_loneliness > 6] <- "Yes"
W1_selected$Lonely_high[W1_selected$UCLA_loneliness <= 6] <- "No"
W1_selected$Lonely_high <- as.factor(W1_selected$Lonely_high)

# exclude loneliness NAs
W1_selected <- W1_selected[!is.na(W1_selected$Lonely_high),]

# recode variables
# sex
table(W1_selected$H002, exclude = NULL)
W1_selected$Sex <- c("")
W1_selected$Sex[W1_selected$H002 == 2] <- "Male"
W1_selected$Sex[W1_selected$H002 == 1] <- "Female"
W1_selected$Sex[W1_selected$H002 == 3] <- "Other/Not specified"
W1_selected$Sex[W1_selected$H002 == 4] <- "Other/Not specified"
W1_selected$Sex <- as.factor(W1_selected$Sex)
levels(W1_selected$Sex)
table(W1_selected$Sex)

# age category
summary(W1_selected$H_alder)
W1_selected$Age_cat <- "Under 30"
W1_selected$Age_cat[W1_selected$H_alder >= 30 & W1_selected$H_alder < 60] <- "Between 30 and 60"
W1_selected$Age_cat[W1_selected$H_alder >= 60] <- "Above 60"
W1_selected$Age_cat <- as.factor(W1_selected$Age_cat)
table(W1_selected$Age_cat)

# education
table(W1_selected$H017, exclude = NULL)
W1_selected$Education <- NA
W1_selected$Education[W1_selected$H017 == 1] <- "Short-term education/Other"
W1_selected$Education[W1_selected$H017 == 2] <- "Short-term education/Other"
W1_selected$Education[W1_selected$H017 == 3] <- "Short-term education/Other"
W1_selected$Education[W1_selected$H017 == 4] <- "Short-term education/Other"
W1_selected$Education[W1_selected$H017 == 7] <- "Short-term education/Other"
W1_selected$Education[W1_selected$H017 == 5] <- "Middle-term education"
W1_selected$Education[W1_selected$H017 == 6] <- "Long-term education"
W1_selected$Education <- as.factor(W1_selected$Education)
levels(W1_selected$Education)
table(W1_selected$Education, exclude = NULL)

# assess free text diseases
freetext_disease <- as.data.frame(table(W1_selected$H048_9A))
mental_d <- c("PTSD", "depression", "ADHD", "psykisk", "ADD",
              "angst", "stress", "OCD", "depressiv", "adhd",
              "psyk", "Skitzofreni", "Angst", "Depression",
              "ptsd", "Psyk", "Stress")
freetext_disease_remain <- freetext_disease[!(grepl(paste(mental_d,collapse="|"),freetext_disease$Var1)),]
mental_d_matches <- grepl(paste(mental_d,collapse="|"),
                          W1_selected$H048_9A)
chronic_d_matches <- !is.na(W1_selected$H048_9A) & !mental_d_matches

# chronic disease
W1_selected$Chronic_disease <- "No chronic disease"
W1_selected$Chronic_disease[W1_selected$H048_1 == 2] <- "Previous chronic disease"
W1_selected$Chronic_disease[W1_selected$H048_2 == 1] <- "Previous chronic disease"
W1_selected$Chronic_disease[W1_selected$H048_3 == 1] <- "Previous chronic disease"
W1_selected$Chronic_disease[W1_selected$H048_4 == 1] <- "Previous chronic disease"
W1_selected$Chronic_disease[W1_selected$H048_5 == 1] <- "Previous chronic disease"
W1_selected$Chronic_disease[W1_selected$H048_6 == 1] <- "Previous chronic disease"
W1_selected$Chronic_disease[W1_selected$H048_7 == 1] <- "Previous chronic disease"
W1_selected$Chronic_disease[W1_selected$H048_8 == 1] <- "Previous chronic disease"
W1_selected$Chronic_disease[W1_selected$H049 == 1] <- "Previous chronic disease"
W1_selected$Chronic_disease[chronic_d_matches] <- "Previous chronic disease"
W1_selected$Chronic_disease <- as.factor(W1_selected$Chronic_disease)
levels(W1_selected$Chronic_disease)
table(W1_selected$Chronic_disease)

# mental illness
table(W1_selected$H052, exclude = NULL)
W1_selected$Mental_disease <- "No mental illness"
W1_selected$Mental_disease[W1_selected$H052 == 1] <- "Previous mental illness"
W1_selected$Mental_disease[W1_selected$H052 == 2] <- "Previous mental illness"
W1_selected$Mental_disease[mental_d_matches] <- "Previous mental illness"
W1_selected$Mental_disease <- as.factor(W1_selected$Mental_disease)
levels(W1_selected$Mental_disease)
table(W1_selected$Mental_disease)


varkeep <- c("Lonely_high", "Sex", "Age_cat", "Education", 
             "Chronic_disease", "Mental_disease")

W1_work <- subset(W1_selected, select = varkeep )
colnames(W1_work) <- c("Very lonely", "Sex", "Age", "Education", 
                       "Chronic", "Mental")
DNBC <- W1_work[, c(2, 4, 5, 6, 3, 1)]

saveRDS(DNBC, "C:/Users/vrw657/Documents/Projects/Corona_SJPH/Data/DNBC_bulk.rds")

