#library(devtools)
#devtools::install_github("sebastianbarfort/mapDK")
library(readxl)
library(mapDK)
library(mapproj)
library(knitr)
library(dplyr) 
library(ggplot2) 
library(tidyverse)
library(lubridate)

setwd("C:/Users/vrw657/Dropbox/Data")

# read map data
zip <- read_excel("Methods/Denmark_ZIP_clean.xls")
polling_zip <- read_excel("Methods/CPH_polling_districts_original.xlsx")
kommun <- read_excel("Methods/DK_kommun_names.xlsx")
polling <- read_excel("Methods/CPH_polling_names.xlsx")

# sync kommun names
for (i in (unique(zip$ADRESSERINGSNAVN_1))) {
  zip$ADRESSERINGSNAVN_1[zip$ADRESSERINGSNAVN_1 == i] <- kommun$mapDK_name[kommun$full_name == i]
}

# sync polling names
for (i in (unique(polling_zip$Afstem))) {
  if (i %in% polling$full_name) {
    polling_zip$Afstem[polling_zip$Afstem == i] <- polling$mapDK_name[polling$full_name == i]
  } else {
    polling_zip$Afstem[polling_zip$Afstem == i] <- NA
  }
}

polling_zip <- polling_zip[!is.na(polling_zip$Afstem),]


# read in Convenient Sample
d_con <- read_excel("Data stripped of free text/KUSUND_1805_AJM.xlsx")
d_con <- d_con[-1,] # Removing the line with the questions
### define UCLA loneliness
d_con$q16_1_num <-recode(d_con$q16_1_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_con$q16_2_num <-recode(d_con$q16_2_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_con$q16_3_num <-recode(d_con$q16_3_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_con$q16_sum <- d_con$q16_1_num + d_con$q16_2_num + d_con$q16_3_num

#define kommun variable in data
d_con$kommun <- c("")

for (i in (unique(d_con$q3))) {
  if(i %in% zip$POSTNR) {
    d_con$kommun[d_con$q3 == i] <- zip$ADRESSERINGSNAVN_1[zip$POSTNR == i]
  } else {
    d_con$kommun[d_con$q3 == i] <- NA
  }
}


# average loneliness per ZIP and kommun
results_lonely_zip <- d_con %>%
  group_by(q3) %>%
  summarise(Loneliness = mean(q16_sum, na.rm=T))

results_lonely_kommun <- d_con %>%
  group_by(kommun) %>%
  summarise(Loneliness = mean(q16_sum, na.rm=T))

p <- mapDK(values = "Loneliness", 
           id = "q3", 
           data = results_lonely_zip, 
           detail = "zip",
           guide.label = "Average levels \nof loneliness \nper postnr")

q <- mapDK(values = "Loneliness", 
           id = "kommun", 
           data = results_lonely_kommun, 
           detail = "municipal",
           guide.label = "Average levels \nof loneliness \nper kommun")

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Citizen_Science_loneliness_DK_ZIP.pdf", width = 6, height = 3)
p
dev.off()

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Citizen_Science_loneliness_DK_kommun.pdf", width = 6, height = 3)
q
dev.off()

# subplot for Copenhagen


d_con_cph <- d_con[d_con$kommun == "koebenhavn",]

# average loneliness per ZIP and kommun
results_lonely_zip <- d_con_cph %>%
  group_by(q3) %>%
  summarise(Loneliness = mean(q16_sum, na.rm=T))
#results_lonely_zip <- results_lonely_zip[1:220,]

mapDK(values = "Loneliness", id = "q3", 
      data = results_lonely_zip,
      detail = "zip",
      guide.label = "Average levels \nof loneliness \nper postnr",
      sub.plot = "koebenhavn")



mapDK(values = "stemmer", id = "id", 
      data = subset(mapDK::votes, navn == "socialdemokratiet"),
      detail = "polling", show_missing = FALSE,
      guide.label = "Stemmer \nSocialdemokratiet (pct)",
      sub.plot = "koebenhavn")


polll <- mapDK::polling
polll_cph <- polll[polll$KommuneNav == "koebenhavn",]

unique(polll_cph$id)
