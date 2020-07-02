library(haven)
library(knitr)
library(dplyr) 
library(ggplot2) 
library(tidyverse)
library(lubridate)

# read original SAS files and saveRDS
# W1 <- read_sas("P:/W1_23april2020_rettet11maj.sas7bdat")
# W2 <- read_sas("P:/W2_11maj2020.sas7bdat")
# W3 <- read_sas("P:/W3_11maj2020.sas7bdat")
# W4 <- read_sas("P:/W4_11juni2020.sas7bdat")
# W5 <- read_sas("P:/W5_25juni2020.sas7bdat")
# W6 <- read_sas("P:/W6_25juni2020.sas7bdat")
# W7 <- read_sas("P:/W7_29juni2020.sas7bdat")

# saveRDS(W1, "P:/DNBC_W1.rds")
# saveRDS(W2, "P:/DNBC_W2.rds")
# saveRDS(W3, "P:/DNBC_W3.rds")
# saveRDS(W4, "P:/DNBC_W4.rds")
# saveRDS(W5, "P:/DNBC_W5.rds")
# saveRDS(W6, "P:/DNBC_W6.rds")
# saveRDS(W7, "P:/DNBC_W7.rds")

# load data
W1 <- readRDS("P:/DNBC_W1.rds")
W2 <- readRDS("P:/DNBC_W2.rds")
W3 <- readRDS("P:/DNBC_W3.rds")
W4 <- readRDS("P:/DNBC_W4.rds")
W5 <- readRDS("P:/DNBC_W5.rds")
W6 <- readRDS("P:/DNBC_W6.rds")
W7 <- readRDS("P:/DNBC_W7.rds")

# remove non needed vars
W1 <- subset(W1, select = c("Completed_Date", "H077") )
W2 <- subset(W2, select = c("Completed_Date", "La055") )
W3 <- subset(W3, select = c("Completed_Date", "Lb055") )
W4 <- subset(W4, select = c("Completed_Date", "Lc055") )
W5 <- subset(W5, select = c("Completed_Date", "Ld055") )
W6 <- subset(W6, select = c("Completed_Date", "Le055") )
W7 <- subset(W7, select = c("Completed_Date", "Lf055") )

# define dataframe list
df.list <- list(W1, W2, W3, W4, W5, W6, W7)

# change 99s to NAs
NAchange <- function(x) {
  x[x == 99] <- NA
  return(x)
}
dfs <- lapply(df.list, NAchange)

# change variable names
ChangeNames <- function(x) {
  names(x) <- c("Datetime", "Worried" )  
  return(x)
}
dfs2 <- lapply(dfs, ChangeNames)

# restrict to complete cases
res <- lapply(dfs2, function(x) x[complete.cases(x),])

# merge all timepoints
DNBC_worries <- rbind(res[[1]], res[[2]], res[[3]], res[[4]], 
                      res[[5]], res[[6]], res[[7]])

# sneak peak to check that variables are coded OK
summary(DNBC_worries)

# date from datetime
DNBC_worries$Date <- as.Date(DNBC_worries$Datetime)

# calculating proportions
results <- DNBC_worries %>%
  group_by(Date) %>%
  summarise(mean_worry = mean(Worried),
            sd_worry = sd(Worried),
            n = n())

# plotting
p <- ggplot(data = results, aes(x = Date, y = mean_worry)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean_worry-sd_worry, ymax=mean_worry+sd_worry), width=.2) +
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
  expand_limits(x = as.Date(c("2020-02-28", "2020-06-30"))) +
  ggtitle("Mean levels of worriedness about the crisis") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none")

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/DNBC_worry_time.pdf", width = 9, height = 3)
p
dev.off()

















