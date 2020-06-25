library(readxl)
library(writexl)

# DOWNLOAD FILES TO DOWNLOADS
# THE ONLY LINE TO MODIFY IS THE FOLLOWING. SPECIFY THE WEEK #
weekno <- "W19"

setwd("C:/Users/vrw657/Downloads")
genpop_origfile <- paste0("Corona_Befolkningen_", weekno, ".xlsx")
elderly_origfile <- paste0("Corona_Aeldre_", weekno, ".xlsx")
family_origfile <- paste0("Corona_Boernefamilier_", weekno, ".xlsx")
genpop_outfile <- paste0("C:/Users/vrw657/Dropbox/Data/Data stripped of free text/Corona_Befolkningen_", weekno, "_AJM.xlsx")
elderly_outfile <- paste0("C:/Users/vrw657/Dropbox/Data/Data stripped of free text/Corona_Aeldre_", weekno, "_AJM.xlsx")
family_outfile <- paste0("C:/Users/vrw657/Dropbox/Data/Data stripped of free text/Corona_Boernefamilier_", weekno, "_AJM.xlsx")

genpop <- read_excel(genpop_origfile)
elderly <- read_excel(elderly_origfile)
family <- read_excel(family_origfile)

genpop$q4_o7 <- NULL
genpop$q5_o7 <- NULL
genpop$q6_o6 <- NULL
genpop$q7_o5 <- NULL
genpop$q8_o7 <- NULL
genpop$q13 <- NULL
genpop$q21_o8 <- NULL
genpop$q22_o7 <- NULL
genpop$q23_o5 <- NULL
genpop$outro <- NULL
genpop$q24_o1 <- NULL
genpop$q24_o2 <- NULL
genpop$q24_o3 <- NULL
genpop$q24_o4 <- NULL
genpop$q24_o5 <- NULL

elderly$q4_o7 <- NULL
elderly$q5_o7 <- NULL
elderly$q6_o6 <- NULL
elderly$q7_o5 <- NULL
elderly$q8_o7 <- NULL
elderly$q13 <- NULL
elderly$q21_o8 <- NULL
elderly$q22_o7 <- NULL
elderly$q23_o5 <- NULL
elderly$outro <- NULL
elderly$q24_o1 <- NULL
elderly$q24_o2 <- NULL
elderly$q24_o3 <- NULL
elderly$q24_o4 <- NULL
elderly$q24_o5 <- NULL

family$q4_o7 <- NULL
family$q5_o7 <- NULL
family$q6_o6 <- NULL
family$q7_o5 <- NULL
family$q8_o7 <- NULL
family$q13 <- NULL
family$q21_o8 <- NULL
family$q22_o7 <- NULL
family$q23_o5 <- NULL
family$outro <- NULL
family$q24_o1 <- NULL
family$q24_o2 <- NULL
family$q24_o3 <- NULL
family$q24_o4 <- NULL
family$q24_o5 <- NULL

write_xlsx(genpop, genpop_outfile)
write_xlsx(elderly, elderly_outfile)
write_xlsx(family, family_outfile)



##WEBSITE 05-18-2020
hjemmesiden_1 <- read_excel("C:\\Users\\mkf710\\Desktop\\epinion\\Data fra hjemmesiden\\KUSUND_1805.xlsx")

hjemmesiden_1$q4_o7 <- NULL
hjemmesiden_1$q5_o7 <- NULL
hjemmesiden_1$q6_o6 <- NULL
hjemmesiden_1$q7_o5 <- NULL
hjemmesiden_1$q8_o7 <- NULL
hjemmesiden_1$q13 <- NULL
hjemmesiden_1$q21_o8 <- NULL
hjemmesiden_1$q22_o7 <- NULL
hjemmesiden_1$q23_o5 <- NULL
hjemmesiden_1$outro <- NULL
hjemmesiden_1$q24_o1 <- NULL
hjemmesiden_1$q24_o2 <- NULL
hjemmesiden_1$q24_o3 <- NULL
hjemmesiden_1$q24_o4 <- NULL
hjemmesiden_1$q24_o5 <- NULL

write_xlsx(hjemmesiden_1,"C:\\Users\\mkf710\\Desktop\\epinion\\Data fra hjemmesiden\\KUSUND_1805_AJM.xlsx")
