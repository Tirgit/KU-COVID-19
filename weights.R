#Calculating weights for convenient sample
##See also tutorial: see: http://tophcito.blogspot.com/2014/04/survey-computing-your-own-post.html
setwd("C:/Users/vrw657/Dropbox/Data")
library(survey)
library(readxl)

zip <- read_excel("Methods/Denmark_ZIP_clean.xls")

conv <- read_excel("Data stripped of free text/KUSUND_1805_AJM.xlsx")
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
#two ZIP codes = 800, these are set to NA

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
conv$q1[conv$q1 == "Ønsker ikke at oplyse" ] <- NA
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

