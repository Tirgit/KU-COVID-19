####################################################################
####################################################################
################# SENSITIVITY & SUBGROUP ANALYSIS ##################
######################### META-REGRESSION ##########################
####################################################################
####################################################################

# INSTALLING PACKAGES - only needs to be done once (ever)
# install.packages("readxl")
# install.packages("meta")
# install.packages("metafor")
# If you want to play with dmetar (devtools), and you use Windows
# WINDOWS: https://cran.r-project.org/bin/windows/Rtools/
# install.packages("devtools")
# devtools::install_github("MathiasHarrer/dmetar")

# LOADING PACKAGES - needs to be done every time
library(readxl)
library(meta)
library(metafor)
# see the above information for dmetar
# library(dmetar)

# SET WORKING DIRECTORY
setwd("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Corona_intl_housing")

# LOADING DATA
data <- read_excel("meta_master.xlsx")

# variable conversions
data$Outcome <- as.factor(data$Outcome)
data$Cohort <- as.factor(data$Cohort)
data$Independent <- as.factor(data$Independent)
data$Subgroup <- as.factor(data$Subgroup)
data$Country <- as.factor(data$Country)

# calculating log of ORs and CIs
# data$logOR <- log(data$OR)
# data$logl_CI <- log(data$l_CI)
# data$logu_CI <- log(data$u_CI)

setwd("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Corona_intl_housing/Plots")


for (i in unique(data$Outcome)) {
        for (j in unique(data$Independent)) {

data_needed <- data[data$Outcome == i & 
                            data$Independent == j,]
outplot <- paste0(i,"_",j,".tiff")

# random effects meta-analysis
m_random <- metagen(TE=log(OR),
                    lower=log(l_CI),
                    upper=log(u_CI),
                    data=data_needed,
                    studlab=paste(Cohort),
                    comb.fixed = F,
                    comb.random = T,
                    method.tau = "SJ",
                    prediction=TRUE,
                    sm="OR")

# SUBGROUP ANALYSIS
# using the random effects model
meta.subgroup <- update.meta(m_random, 
                             byvar=Subgroup, 
                             comb.random = T, 
                             comb.fixed = F)

tiff(outplot, units="in", width=9, height=7, res=300)
forest(meta.subgroup, sortvar=OR, leftcols = "studlab", colgap.forest.left = "35mm")
dev.off()

}}





for (i in unique(data$Outcome)) {
  for (j in unique(data$Independent)) {
    
    data_needed <- data[data$Outcome == i & 
                          data$Independent == j,]
    outplot <- paste0(i,"_",j,"_country.tiff")
    
    # random effects meta-analysis
    m_random <- metagen(TE=log(OR),
                        lower=log(l_CI),
                        upper=log(u_CI),
                        data=data_needed,
                        studlab=paste(Cohort),
                        comb.fixed = F,
                        comb.random = T,
                        method.tau = "SJ",
                        prediction=TRUE,
                        sm="OR")
    
    # SUBGROUP ANALYSIS
    # using the random effects model
    meta.subgroup <- update.meta(m_random, 
                                 byvar=Country, 
                                 comb.random = T, 
                                 comb.fixed = F)
    
    tiff(outplot, units="in", width=9, height=7, res=300)
    forest(meta.subgroup, sortvar=OR, leftcols = "studlab", colgap.forest.left = "35mm")
    dev.off()
    
  }}

