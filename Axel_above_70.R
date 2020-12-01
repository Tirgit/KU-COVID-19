#### LOAD PACKAGES & SET WORKING DIRECTORY
library(knitr)
library(dplyr) 
library(ggplot2)
library(tidyverse)
library(lubridate)
library(survey)
library(readxl)
library(Hmisc) 

# run shared .Rmd file until load of these 3 are done and save
#saveRDS(d_pop, "C:/Users/vrw657/Documents/Projects/Corona_SJPH/Data/d_pop.rds")

# load data
d_pop <- readRDS("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Data/d_pop.rds")
d_fam <- readRDS("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Data/d_fam.rds")
d_eld <- readRDS("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Data/d_eld.rds")

d_pop_70 <- d_pop[as.numeric(d_pop$q2)>=70,]
d_fam_70 <- d_fam[as.numeric(d_fam$q2)>=70,]
d_eld_70 <- d_eld[as.numeric(d_eld$q2)>=70,]

varkeep <- c("q15", "q16_1_resp",
             "q16_2_resp", "q16_3_resp", "q18", "q20", 
             "q22_1", "q22_2","q22_3","q22_4","q22_5","q22_6",
             "date")

d_pop_selected = subset(d_pop_70, select = varkeep )
d_fam_selected = subset(d_fam_70, select = varkeep )
d_eld_selected = subset(d_eld_70, select = varkeep )

d_merged <- rbind(d_pop_selected,d_fam_selected,d_eld_selected)
table(d_merged$date)

### UCLA loneliness
d_merged$q16_1_num <-recode(d_merged$q16_1_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_merged$q16_2_num <-recode(d_merged$q16_2_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_merged$q16_3_num <-recode(d_merged$q16_3_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_merged$q16_sum <- d_merged$q16_1_num + d_merged$q16_2_num + d_merged$q16_3_num

varkeep <- c("q15", "q16_sum", "q18", "q20", 
             "q22_1", "q22_2","q22_3","q22_4","q22_5","q22_6",
             "date")
d_con_selected <- subset(d_merged, select = varkeep )
colnames(d_con_selected) <- c("Social_Isolation", "UCLA_loneliness", 
                              "Qualityoflife","Worried", 
                              "q22_1", "q22_2","q22_3","q22_4","q22_5","q22_6",
                              "Date")

d_con_selected$Worried[d_con_selected$Worried == "1 – Ikke bekymret"] <- "1"
d_con_selected$Worried[d_con_selected$Worried == "10 – Meget bekymret"] <- "10"
d_con_selected$Worried <- as.numeric(d_con_selected$Worried)

d_con_selected$Social_Isolation[d_con_selected$Social_Isolation == "1 – Slet ikke"] <- "1"
d_con_selected$Social_Isolation[d_con_selected$Social_Isolation == "10 – Fuldstændigt"] <- "10"
d_con_selected$Social_Isolation <- as.numeric(d_con_selected$Social_Isolation)

d_con_selected$Qualityoflife[d_con_selected$Qualityoflife == "1 – Dårlig"] <- "1"
d_con_selected$Qualityoflife[d_con_selected$Qualityoflife == "10 - Fremragende"] <- "10"
d_con_selected$Qualityoflife <- as.numeric(d_con_selected$Qualityoflife)


# worries calculating proportions
results <- d_con_selected %>%
  group_by(Date) %>%
  summarise(mean_worry = mean(Worried),
            sd_worry = sd(Worried))

# loneliness calculating proportions
results <- d_con_selected %>%
  group_by(Date) %>%
  summarise(mean_worry = mean(UCLA_loneliness),
            sd_worry = sd(UCLA_loneliness))

# social isolation calculating proportions
results <- d_con_selected %>%
  group_by(Date) %>%
  summarise(mean_worry = mean(Social_Isolation),
            sd_worry = sd(Social_Isolation))

# Quality of life calculating proportions
results <- d_con_selected %>%
  group_by(Date) %>%
  summarise(mean_worry = mean(Qualityoflife),
            sd_worry = sd(Qualityoflife))

# proportions for precautions
results <- d_con_selected %>%
  group_by(Date) %>%
  summarise(prop_1 = 100*sum(q22_1 == "Yes")/sum(q22_1 == "Yes" | q22_1 == "No"),
            prop_2 = 100*sum(q22_2 == "Yes")/sum(q22_2 == "Yes" | q22_2 == "No"),
            prop_3 = 100*sum(q22_3 == "Yes")/sum(q22_3 == "Yes" | q22_3 == "No"),
            prop_4 = 100*sum(q22_4 == "Yes")/sum(q22_4 == "Yes" | q22_4 == "No"),
            prop_5 = 100*sum(q22_5 == "Yes")/sum(q22_5 == "Yes" | q22_5 == "No"),
            prop_6 = 100*sum(q22_6 == "Yes")/sum(q22_6 == "Yes" | q22_6 == "No"))

# Increased hand washing and use of hand sanitiser
# Keeping physical distance from strangers
# Avoiding physical contact (except co-habitation/relationship)
# Covering nose and mouth in public
# Avoiding public transportation
# Avoiding traveling







