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

#Calculating weights for convenient sample
##See also tutorial: see: http://tophcito.blogspot.com/2014/04/survey-computing-your-own-post.html
setwd("C:/Users/vrw657/Dropbox/Data")

zip <- read_excel("Methods/Denmark_ZIP_clean.xls")

conv <- d_pop
conv <- conv[-1,] # Removing the line with the questions

#renaming zip file
unique(zip$ADRESSERINGSNAVN)
zip$ADRESSERINGSNAVN[zip$ADRESSERINGSNAVN == "Region Hovedstaden"] <- "Capital region"
zip$ADRESSERINGSNAVN[zip$ADRESSERINGSNAVN == "Region Sjælland"] <- "Region zealand"
zip$ADRESSERINGSNAVN[zip$ADRESSERINGSNAVN == "Region Syddanmark"] <- "Region of southern denmark"
zip$ADRESSERINGSNAVN[zip$ADRESSERINGSNAVN == "Region Midtjylland"] <- "Central denmark region"
zip$ADRESSERINGSNAVN[zip$ADRESSERINGSNAVN == "Region Nordjylland"] <- "North denmark region"
unique(zip$ADRESSERINGSNAVN)

#recoding postalcode
table(conv$q3)
length(unique(conv$q3))
conv$regionD <- c("")

for (i in (unique(conv$q3))) {
  if(i %in% zip$POSTNR) {
    conv$regionD[conv$q3 == i] <- zip$ADRESSERINGSNAVN[zip$POSTNR == i]
  } else {
    conv$regionD[conv$q3 == i] <- NA
  }
}

table(conv$regionD, useNA="always")
conv$q3[is.na(conv$regionD)]

#Age
table(conv$q2, useNA="always")
#excluding participants younger than 15 years and older than 79 years because of few observations
conv$q2[conv$q2 < 15 ] <- NA
conv$q2[conv$q2 >= 15 & conv$q2 <= 19] <- "15-19"
conv$q2[conv$q2 >= 20 & conv$q2 <= 24] <- "20-24"
conv$q2[conv$q2 >= 25 & conv$q2 <= 29] <- "25-29"
conv$q2[conv$q2 >= 30 & conv$q2 <= 34] <- "30-34"
conv$q2[conv$q2 >= 35 & conv$q2 <= 39] <- "35-39"
conv$q2[conv$q2 >= 40 & conv$q2 <= 44] <- "40-44"
conv$q2[conv$q2 >= 45 & conv$q2 <= 49] <- "45-49"
conv$q2[conv$q2 >= 50 & conv$q2 <= 54] <- "50-54"
conv$q2[conv$q2 >= 55 & conv$q2 <= 59] <- "55-59"
conv$q2[conv$q2 >= 60 & conv$q2 <= 64] <- "60-64"
conv$q2[conv$q2 >= 65 & conv$q2 <= 69] <- "65-69"
conv$q2[conv$q2 >= 70 & conv$q2 <= 74] <- "70-74"
conv$q2[conv$q2 >= 75 & conv$q2 <= 79] <- "75-79"
conv$q2[conv$q2 >79] <- NA
table(conv$q2, useNA="always")

#Gender 
table(conv$q1, useNA="always")
conv$q1[conv$q1 == "Kvinde" ] <- "Woman"
conv$q1[conv$q1 == "Mand" ] <- "Man"
conv$q1[conv$q1 == "?nsker ikke at oplyse" ] <- NA
conv$q1[conv$q1 == "Andet" ] <- NA
table(conv$q1, useNA="always")


#create dataset without missing data on age, gender and region. 134 participants are excluded
conv <- subset(conv, is.na(conv$q1)==FALSE & is.na(conv$q2)==FALSE & is.na(conv$regionD)==FALSE)

##Create unweighted survey object
conv.svy.unweighted <- svydesign(ids=~1, data=conv)

#Setting up population dataframes. Data comes from register FOLK1A, 1st quarter 2020 (2020 K1): https://statistikbanken.dk/statbank5a/selectvarval/define.asp?PLanguage=0&MainTable=FOLK1A&TabStrip=Select
## distrubuion on region and gender is restricted to agegroup 15-79 

gender.dist <- data.frame(q1 = c("Man", "Woman"),
                          Freq = nrow(conv) * c(0.50, 0.50))

age.dist <- data.frame(q2 = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64","65-69", "70-74","75-79"),
                       Freq = nrow(conv) * c(0.07, 0.08, 0.09, 0.08, 0.07, 0.08, 0.09, 0.09, 0.08, 0.07, 0.07, 0.07, 0.05))

region.dist <- data.frame(regionD = c("Capital region", "Region zealand", "Region of southern denmark", "Central denmark region","North denmark region"),
                          Freq = nrow(conv) * c(0.32, 0.14, 0.21, 0.23, 0.10))


#Calculating weights
conv.svy.rake <- rake(design = conv.svy.unweighted,
                      sample.margins = list(~q1, ~q2, ~regionD),
                      population.margins = list(gender.dist, age.dist,region.dist))

summary(weights(conv.svy.rake))

#Trim weights if to small ()smaller than 0.3 or too large (larger than 3)
conv.svy.rake.trim <- trimWeights(conv.svy.rake, lower=0.3, upper=3,
                                  strict=TRUE) 
summary(weights(conv.svy.rake.trim))

#Append weights to dataset
convW <- cbind(conv, Weights = weights(conv.svy.rake.trim))
d_pop <- convW

# worriedness time plot
varkeep <- c("q20", "date", "Weights")
d_pop_selected <- subset(d_pop, select = varkeep )
colnames(d_pop_selected) <- c("Worried", "Date", "Weights")
d_pop_selected$Worried[d_pop_selected$Worried == "1 – Ikke bekymret"] <- "1"
d_pop_selected$Worried[d_pop_selected$Worried == "10 – Meget bekymret"] <- "10"
d_pop_selected$Worried <- as.numeric(d_pop_selected$Worried)


# calculating proportions
results <- d_pop_selected %>%
  group_by(Date) %>%
  summarise(n = n(),
            mean_worry = wtd.mean(Worried, Weights),
            sd_worry = sqrt(wtd.var(Worried)),
            lci_worry = mean_worry - 1.96*(sd_worry/sqrt(n)),
            uci_worry = mean_worry + 1.96*(sd_worry/sqrt(n)))
results$sd_worry <- NULL
results$n <- NULL

p <- ggplot(data = results, aes(x = Date, y = mean_worry)) +
  geom_point() +
  geom_errorbar(aes(ymin=lci_worry, ymax=uci_worry), width=.2) +
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
  ylab("Mean levels of worries") +
  scale_y_continuous(limits = c(0,10), expand = c(0, 0)) +
  expand_limits(x = as.Date(c("2020-03-03", "2020-06-29"))) +
  #ggtitle("Mean levels of worriedness about the crisis") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none")

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/Epinion_worry_time.pdf", width = 9, height = 3)
p
dev.off()

