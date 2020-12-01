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
W1 <- subset(W1, select = c("Completed_Date", "Besvarelse_type", "H077", "H078_1", "H078_2", "H078_3", "H078_4", "H078_5", "H078_6", "H078_7", "H078_8", "H079_1", "H079_2", "H079_3", "H079_4", "H079_5", "H079_6", "H079_7", "H079_8") )
W2 <- subset(W2, select = c("Completed_Date", "Besvarelse_type", "La055", "La056_1", "La056_2", "La056_3", "La056_4", "La056_5", "La056_6", "La056_7", "La056_8", "La057_1", "La057_2", "La057_3", "La057_4", "La057_5", "La057_6", "La057_7", "La057_8") )
W3 <- subset(W3, select = c("Completed_Date", "Besvarelse_type", "Lb055", "Lb056_1", "Lb056_2", "Lb056_3", "Lb056_4", "Lb056_5", "Lb056_6", "Lb056_7", "Lb056_8", "Lb057_1", "Lb057_2", "Lb057_3", "Lb057_4", "Lb057_5", "Lb057_6", "Lb057_7", "Lb057_8") )
W4 <- subset(W4, select = c("Completed_Date", "Besvarelse_type", "Lc055", "Lc056_1", "Lc056_2", "Lc056_3", "Lc056_4", "Lc056_5", "Lc056_6", "Lc056_7", "Lc056_8", "Lc057_1", "Lc057_2", "Lc057_3", "Lc057_4", "Lc057_5", "Lc057_6", "Lc057_7", "Lc057_8") )
W5 <- subset(W5, select = c("Completed_Date", "Besvarelse_type", "Ld055", "Ld056_1", "Ld056_2", "Ld056_3", "Ld056_4", "Ld056_5", "Ld056_6", "Ld056_7", "Ld056_8", "Ld057_1", "Ld057_2", "Ld057_3", "Ld057_4", "Ld057_5", "Ld057_6", "Ld057_7", "Ld057_8") )
W6 <- subset(W6, select = c("Completed_Date", "Besvarelse_type", "Le055", "Le056_1", "Le056_2", "Le056_3", "Le056_4", "Le056_5", "Le056_6", "Le056_7", "Le056_8", "Le057_1", "Le057_2", "Le057_3", "Le057_4", "Le057_5", "Le057_6", "Le057_7", "Le057_8") )
W7 <- subset(W7, select = c("Completed_Date", "Besvarelse_type", "Lf055", "Lf056_1", "Lf056_2", "Lf056_3", "Lf056_4", "Lf056_5", "Lf056_6", "Lf056_7", "Lf056_8", "Lf057_1", "Lf057_2", "Lf057_3", "Lf057_4", "Lf057_5", "Lf057_6", "Lf057_7", "Lf057_8") )

# define dataframe list
df.list <- list(W1, W2, W3, W4, W5, W6, W7)

# change 99s to NAs
NAchange <- function(x) {
  x[x == 99 | x == 100] <- NA
  return(x)
}
dfs <- lapply(df.list, NAchange)

# change variable names
ChangeNames <- function(x) {
  names(x) <- c("Datetime", "Category", "Worried", "selfill", "closeill", "financeprob", "losejob",
                "reglife", "cantsee", "economycrisis", "inequality",
                "handwash", "distance", "contact", "covernose", "transport",
                "travel", "cleanmore", "ventilatemore")  
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

DNBC_worries$Week <- NA
DNBC_worries$Week[DNBC_worries$Date <= "2020-05-24"] <- "Week 21 (18−24 May)"
DNBC_worries$Week[DNBC_worries$Date <= "2020-05-17"] <- "Week 20 (11−17 May)"
DNBC_worries$Week[DNBC_worries$Date <= "2020-05-10"] <- "Week 19 (4−10 May)"
DNBC_worries$Week[DNBC_worries$Date <= "2020-05-03"] <- "Week 18 (27 Apr − 3 May)"
DNBC_worries$Week[DNBC_worries$Date <= "2020-04-26"] <- "Week 17 (20−26 Apr)"
DNBC_worries$Week[DNBC_worries$Date <= "2020-04-19"] <- "Week 16 (13−19 Apr)"
DNBC_worries$Week[DNBC_worries$Date <= "2020-04-12"] <- "Week 15 (6−12 Apr)"
DNBC_worries$Week[DNBC_worries$Date <= "2020-04-05"] <- "Week 14 (30 Mar − 5 Apr)"

DNBC_worries <- DNBC_worries[!is.na(DNBC_worries$Week),]
varorder <- sort(unique(DNBC_worries$Week))
DNBC_worries$Week <- factor(DNBC_worries$Week,
                            levels=varorder)
table(DNBC_worries$Week)

# calculating proportions
results_all <- DNBC_worries %>%
  group_by(Week) %>%
  summarise(mean_worry = mean(Worried),
            sd_worry = sd(Worried),
            n = n())
results_all$Category <- 0
results_strat <- DNBC_worries %>%
  group_by(Week, Category) %>%
  summarise(mean_worry = mean(Worried),
            sd_worry = sd(Worried),
            n = n())
colord <- colnames(results_strat)
results_all <- results_all[colord]
results <- rbind(results_all, results_strat)
results$Category <- as.factor(results$Category)
levels(results$Category) <- c("All","Mothers","Offspring")

# plotting
p <- ggplot(data = results, aes(x = Week, y = mean_worry, color=Category)) +
  #geom_point(position=position_dodge(width=1)) +
  #geom_errorbar(aes(ymin=mean_worry-sd_worry, ymax=mean_worry+sd_worry), width=.2, position=position_dodge(width=1)) +
  geom_pointrange(aes(ymin = mean_worry-sd_worry, ymax = mean_worry+sd_worry), 
                  position=position_jitter(width=0.2), 
                  linetype='dotted') +
  #scale_x_date(date_labels="%d %b",date_breaks  ="3 day") + 
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Mean levels of worries") +
  scale_y_continuous(limits = c(0,10), expand = c(0, 0)) +
  #expand_limits(x = as.Date(c("2020-02-28", "2020-06-30"))) +
  #ggtitle("Mean levels of worriedness about the crisis") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(plot.margin=unit(c(0.3,0.1,0.1,2),"cm"))

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/DNBC_worry_stratified.pdf", width = 6, height = 3)
p
dev.off()


# specific worries and precautions
results_all <- DNBC_worries %>%
  group_by(Week) %>%
  summarise(mean_worry = sum(selfill == 1)/sum(selfill == 1 | selfill == 2))
results_all$Category <- 0
results_strat <- DNBC_worries %>%
  group_by(Week, Category) %>%
  summarise(mean_worry = sum(selfill == 1)/sum(selfill == 1 | selfill == 2))
colord <- colnames(results_strat)
results_all <- results_all[colord]
results <- rbind(results_all, results_strat)
results$Category <- as.factor(results$Category)
levels(results$Category) <- c("All","Mothers","Offspring")
results$mean_worry <- 100*results$mean_worry

# plotting
p <- ggplot(data = results, aes(x = Week, y = mean_worry, color=Category)) +
  #geom_point(position=position_dodge(width=1)) +
  #geom_errorbar(aes(ymin=mean_worry-sd_worry, ymax=mean_worry+sd_worry), width=.2, position=position_dodge(width=1)) +
  geom_point(stat='summary', fun=sum, position=position_dodge(width = 0.2)) +
  stat_summary(fun=sum, geom="line") +
  #scale_x_date(date_labels="%d %b",date_breaks  ="3 day") + 
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Percent worried (%)") +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  #expand_limits(x = as.Date(c("2020-02-28", "2020-06-30"))) +
  ggtitle("Becoming seriously ill") +
  theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(plot.margin=unit(c(0.3,0.1,0.1,2),"cm"))

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/DNBC_worry_selfill.pdf", width = 6, height = 3)
p
dev.off()

# specific worries and precautions
results_all <- DNBC_worries %>%
  group_by(Week) %>%
  summarise(mean_worry = sum(closeill == 1)/sum(closeill == 1 | closeill == 2))
results_all$Category <- 0
results_strat <- DNBC_worries %>%
  group_by(Week, Category) %>%
  summarise(mean_worry = sum(closeill == 1)/sum(closeill == 1 | closeill == 2))
colord <- colnames(results_strat)
results_all <- results_all[colord]
results <- rbind(results_all, results_strat)
results$Category <- as.factor(results$Category)
levels(results$Category) <- c("All","Mothers","Offspring")
results$mean_worry <- 100*results$mean_worry

# plotting
p <- ggplot(data = results, aes(x = Week, y = mean_worry, color=Category)) +
  #geom_point(position=position_dodge(width=1)) +
  #geom_errorbar(aes(ymin=mean_worry-sd_worry, ymax=mean_worry+sd_worry), width=.2, position=position_dodge(width=1)) +
  geom_point(stat='summary', fun=sum, position=position_dodge(width = 0.2)) +
  stat_summary(fun=sum, geom="line") +
  #scale_x_date(date_labels="%d %b",date_breaks  ="3 day") + 
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Percent worried (%)") +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  #expand_limits(x = as.Date(c("2020-02-28", "2020-06-30"))) +
  ggtitle("Someone close to you becoming seriously ill") +
  theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(plot.margin=unit(c(0.3,0.1,0.1,2),"cm"))

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/DNBC_worry_closeill.pdf", width = 6, height = 3)
p
dev.off()


# specific worries and precautions
results_all <- DNBC_worries %>%
  group_by(Week) %>%
  summarise(mean_worry = sum(financeprob == 1)/sum(financeprob == 1 | financeprob == 2))
results_all$Category <- 0
results_strat <- DNBC_worries %>%
  group_by(Week, Category) %>%
  summarise(mean_worry = sum(financeprob == 1)/sum(financeprob == 1 | financeprob == 2))
colord <- colnames(results_strat)
results_all <- results_all[colord]
results <- rbind(results_all, results_strat)
results$Category <- as.factor(results$Category)
levels(results$Category) <- c("All","Mothers","Offspring")
results$mean_worry <- 100*results$mean_worry

# plotting
p <- ggplot(data = results, aes(x = Week, y = mean_worry, color=Category)) +
  #geom_point(position=position_dodge(width=1)) +
  #geom_errorbar(aes(ymin=mean_worry-sd_worry, ymax=mean_worry+sd_worry), width=.2, position=position_dodge(width=1)) +
  geom_point(stat='summary', fun=sum, position=position_dodge(width = 0.2)) +
  stat_summary(fun=sum, geom="line") +
  #scale_x_date(date_labels="%d %b",date_breaks  ="3 day") + 
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Percent worried (%)") +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  #expand_limits(x = as.Date(c("2020-02-28", "2020-06-30"))) +
  ggtitle("You or your family experiencing serious financial problems") +
  theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(plot.margin=unit(c(0.3,0.1,0.1,2),"cm"))

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/DNBC_worry_financeprob.pdf", width = 6, height = 3)
p
dev.off()


# specific worries and precautions
results_all <- DNBC_worries %>%
  group_by(Week) %>%
  summarise(mean_worry = sum(losejob == 1)/sum(losejob == 1 | losejob == 2))
results_all$Category <- 0
results_strat <- DNBC_worries %>%
  group_by(Week, Category) %>%
  summarise(mean_worry = sum(losejob == 1)/sum(losejob == 1 | losejob == 2))
colord <- colnames(results_strat)
results_all <- results_all[colord]
results <- rbind(results_all, results_strat)
results$Category <- as.factor(results$Category)
levels(results$Category) <- c("All","Mothers","Offspring")
results$mean_worry <- 100*results$mean_worry

# plotting
p <- ggplot(data = results, aes(x = Week, y = mean_worry, color=Category)) +
  #geom_point(position=position_dodge(width=1)) +
  #geom_errorbar(aes(ymin=mean_worry-sd_worry, ymax=mean_worry+sd_worry), width=.2, position=position_dodge(width=1)) +
  geom_point(stat='summary', fun=sum, position=position_dodge(width = 0.2)) +
  stat_summary(fun=sum, geom="line") +
  #scale_x_date(date_labels="%d %b",date_breaks  ="3 day") + 
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Percent worried (%)") +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  #expand_limits(x = as.Date(c("2020-02-28", "2020-06-30"))) +
  ggtitle("Losing your job") +
  theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(plot.margin=unit(c(0.3,0.1,0.1,2),"cm"))

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/DNBC_worry_losejob.pdf", width = 6, height = 3)
p
dev.off()


# specific worries and precautions
results_all <- DNBC_worries %>%
  group_by(Week) %>%
  summarise(mean_worry = sum(reglife == 1)/sum(reglife == 1 | reglife == 2))
results_all$Category <- 0
results_strat <- DNBC_worries %>%
  group_by(Week, Category) %>%
  summarise(mean_worry = sum(reglife == 1)/sum(reglife == 1 | reglife == 2))
colord <- colnames(results_strat)
results_all <- results_all[colord]
results <- rbind(results_all, results_strat)
results$Category <- as.factor(results$Category)
levels(results$Category) <- c("All","Mothers","Offspring")
results$mean_worry <- 100*results$mean_worry

# plotting
p <- ggplot(data = results, aes(x = Week, y = mean_worry, color=Category)) +
  #geom_point(position=position_dodge(width=1)) +
  #geom_errorbar(aes(ymin=mean_worry-sd_worry, ymax=mean_worry+sd_worry), width=.2, position=position_dodge(width=1)) +
  geom_point(stat='summary', fun=sum, position=position_dodge(width = 0.2)) +
  stat_summary(fun=sum, geom="line") +
  #scale_x_date(date_labels="%d %b",date_breaks  ="3 day") + 
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Percent worried (%)") +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  #expand_limits(x = as.Date(c("2020-02-28", "2020-06-30"))) +
  ggtitle("Long time before resuming regular everyday life") +
  theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(plot.margin=unit(c(0.3,0.1,0.1,2),"cm"))

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/DNBC_worry_reglife.pdf", width = 6, height = 3)
p
dev.off()



# specific worries and precautions
results_all <- DNBC_worries %>%
  group_by(Week) %>%
  summarise(mean_worry = sum(cantsee == 1)/sum(cantsee == 1 | cantsee == 2))
results_all$Category <- 0
results_strat <- DNBC_worries %>%
  group_by(Week, Category) %>%
  summarise(mean_worry = sum(cantsee == 1)/sum(cantsee == 1 | cantsee == 2))
colord <- colnames(results_strat)
results_all <- results_all[colord]
results <- rbind(results_all, results_strat)
results$Category <- as.factor(results$Category)
levels(results$Category) <- c("All","Mothers","Offspring")
results$mean_worry <- 100*results$mean_worry

# plotting
p <- ggplot(data = results, aes(x = Week, y = mean_worry, color=Category)) +
  #geom_point(position=position_dodge(width=1)) +
  #geom_errorbar(aes(ymin=mean_worry-sd_worry, ymax=mean_worry+sd_worry), width=.2, position=position_dodge(width=1)) +
  geom_point(stat='summary', fun=sum, position=position_dodge(width = 0.2)) +
  stat_summary(fun=sum, geom="line") +
  #scale_x_date(date_labels="%d %b",date_breaks  ="3 day") + 
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Percent worried (%)") +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  #expand_limits(x = as.Date(c("2020-02-28", "2020-06-30"))) +
  ggtitle("Not being able to see family/friends") +
  theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(plot.margin=unit(c(0.3,0.1,0.1,2),"cm"))

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/DNBC_worry_cantsee.pdf", width = 6, height = 3)
p
dev.off()


# specific worries and precautions
results_all <- DNBC_worries %>%
  group_by(Week) %>%
  summarise(mean_worry = sum(economycrisis == 1)/sum(economycrisis == 1 | economycrisis == 2))
results_all$Category <- 0
results_strat <- DNBC_worries %>%
  group_by(Week, Category) %>%
  summarise(mean_worry = sum(economycrisis == 1)/sum(economycrisis == 1 | economycrisis == 2))
colord <- colnames(results_strat)
results_all <- results_all[colord]
results <- rbind(results_all, results_strat)
results$Category <- as.factor(results$Category)
levels(results$Category) <- c("All","Mothers","Offspring")
results$mean_worry <- 100*results$mean_worry

# plotting
p <- ggplot(data = results, aes(x = Week, y = mean_worry, color=Category)) +
  #geom_point(position=position_dodge(width=1)) +
  #geom_errorbar(aes(ymin=mean_worry-sd_worry, ymax=mean_worry+sd_worry), width=.2, position=position_dodge(width=1)) +
  geom_point(stat='summary', fun=sum, position=position_dodge(width = 0.2)) +
  stat_summary(fun=sum, geom="line") +
  #scale_x_date(date_labels="%d %b",date_breaks  ="3 day") + 
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Percent worried (%)") +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  #expand_limits(x = as.Date(c("2020-02-28", "2020-06-30"))) +
  ggtitle("A national economic crisis") +
  theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(plot.margin=unit(c(0.3,0.1,0.1,2),"cm"))

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/DNBC_worry_economycrisis.pdf", width = 6, height = 3)
p
dev.off()


# specific worries and precautions
results_all <- DNBC_worries %>%
  group_by(Week) %>%
  summarise(mean_worry = sum(inequality == 1)/sum(inequality == 1 | inequality == 2))
results_all$Category <- 0
results_strat <- DNBC_worries %>%
  group_by(Week, Category) %>%
  summarise(mean_worry = sum(inequality == 1)/sum(inequality == 1 | inequality == 2))
colord <- colnames(results_strat)
results_all <- results_all[colord]
results <- rbind(results_all, results_strat)
results$Category <- as.factor(results$Category)
levels(results$Category) <- c("All","Mothers","Offspring")
results$mean_worry <- 100*results$mean_worry

# plotting
p <- ggplot(data = results, aes(x = Week, y = mean_worry, color=Category)) +
  #geom_point(position=position_dodge(width=1)) +
  #geom_errorbar(aes(ymin=mean_worry-sd_worry, ymax=mean_worry+sd_worry), width=.2, position=position_dodge(width=1)) +
  geom_point(stat='summary', fun=sum, position=position_dodge(width = 0.2)) +
  stat_summary(fun=sum, geom="line") +
  #scale_x_date(date_labels="%d %b",date_breaks  ="3 day") + 
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Percent worried (%)") +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  #expand_limits(x = as.Date(c("2020-02-28", "2020-06-30"))) +
  ggtitle("Increased inequality after the crisis") +
  theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(plot.margin=unit(c(0.3,0.1,0.1,2),"cm"))

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/DNBC_worry_inequality.pdf", width = 6, height = 3)
p
dev.off()


# specific worries and precautions
results_all <- DNBC_worries %>%
  group_by(Week) %>%
  summarise(mean_worry = sum(handwash == 1)/sum(handwash == 1 | handwash == 2))
results_all$Category <- 0
results_strat <- DNBC_worries %>%
  group_by(Week, Category) %>%
  summarise(mean_worry = sum(handwash == 1)/sum(handwash == 1 | handwash == 2))
colord <- colnames(results_strat)
results_all <- results_all[colord]
results <- rbind(results_all, results_strat)
results$Category <- as.factor(results$Category)
levels(results$Category) <- c("All","Mothers","Offspring")
results$mean_worry <- 100*results$mean_worry

# plotting
p <- ggplot(data = results, aes(x = Week, y = mean_worry, color=Category)) +
  #geom_point(position=position_dodge(width=1)) +
  #geom_errorbar(aes(ymin=mean_worry-sd_worry, ymax=mean_worry+sd_worry), width=.2, position=position_dodge(width=1)) +
  geom_point(stat='summary', fun=sum, position=position_dodge(width = 0.2)) +
  stat_summary(fun=sum, geom="line") +
  #scale_x_date(date_labels="%d %b",date_breaks  ="3 day") + 
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Percent precaution (%)") +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  #expand_limits(x = as.Date(c("2020-02-28", "2020-06-30"))) +
  ggtitle("Increased hand washing and use of hand sanitiser") +
  theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(plot.margin=unit(c(0.3,0.1,0.1,2),"cm"))

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/DNBC_precaution_handwash.pdf", width = 6, height = 3)
p
dev.off()


# specific worries and precautions
results_all <- DNBC_worries %>%
  group_by(Week) %>%
  summarise(mean_worry = sum(distance == 1)/sum(distance == 1 | distance == 2))
results_all$Category <- 0
results_strat <- DNBC_worries %>%
  group_by(Week, Category) %>%
  summarise(mean_worry = sum(distance == 1)/sum(distance == 1 | distance == 2))
colord <- colnames(results_strat)
results_all <- results_all[colord]
results <- rbind(results_all, results_strat)
results$Category <- as.factor(results$Category)
levels(results$Category) <- c("All","Mothers","Offspring")
results$mean_worry <- 100*results$mean_worry

# plotting
p <- ggplot(data = results, aes(x = Week, y = mean_worry, color=Category)) +
  #geom_point(position=position_dodge(width=1)) +
  #geom_errorbar(aes(ymin=mean_worry-sd_worry, ymax=mean_worry+sd_worry), width=.2, position=position_dodge(width=1)) +
  geom_point(stat='summary', fun=sum, position=position_dodge(width = 0.2)) +
  stat_summary(fun=sum, geom="line") +
  #scale_x_date(date_labels="%d %b",date_breaks  ="3 day") + 
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Percent precaution (%)") +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  #expand_limits(x = as.Date(c("2020-02-28", "2020-06-30"))) +
  ggtitle("Keeping physical distance from strangers") +
  theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(plot.margin=unit(c(0.3,0.1,0.1,2),"cm"))

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/DNBC_precaution_distance.pdf", width = 6, height = 3)
p
dev.off()


# specific worries and precautions
results_all <- DNBC_worries %>%
  group_by(Week) %>%
  summarise(mean_worry = sum(contact == 1)/sum(contact == 1 | contact == 2))
results_all$Category <- 0
results_strat <- DNBC_worries %>%
  group_by(Week, Category) %>%
  summarise(mean_worry = sum(contact == 1)/sum(contact == 1 | contact == 2))
colord <- colnames(results_strat)
results_all <- results_all[colord]
results <- rbind(results_all, results_strat)
results$Category <- as.factor(results$Category)
levels(results$Category) <- c("All","Mothers","Offspring")
results$mean_worry <- 100*results$mean_worry

# plotting
p <- ggplot(data = results, aes(x = Week, y = mean_worry, color=Category)) +
  #geom_point(position=position_dodge(width=1)) +
  #geom_errorbar(aes(ymin=mean_worry-sd_worry, ymax=mean_worry+sd_worry), width=.2, position=position_dodge(width=1)) +
  geom_point(stat='summary', fun=sum, position=position_dodge(width = 0.2)) +
  stat_summary(fun=sum, geom="line") +
  #scale_x_date(date_labels="%d %b",date_breaks  ="3 day") + 
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Percent precaution (%)") +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  #expand_limits(x = as.Date(c("2020-02-28", "2020-06-30"))) +
  ggtitle("Avoiding physical contact (except co-habitation/relationship)") +
  theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(plot.margin=unit(c(0.3,0.1,0.1,2),"cm"))

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/DNBC_precaution_contact.pdf", width = 6, height = 3)
p
dev.off()



# specific worries and precautions
results_all <- DNBC_worries %>%
  group_by(Week) %>%
  summarise(mean_worry = sum(covernose == 1)/sum(covernose == 1 | covernose == 2))
results_all$Category <- 0
results_strat <- DNBC_worries %>%
  group_by(Week, Category) %>%
  summarise(mean_worry = sum(covernose == 1)/sum(covernose == 1 | covernose == 2))
colord <- colnames(results_strat)
results_all <- results_all[colord]
results <- rbind(results_all, results_strat)
results$Category <- as.factor(results$Category)
levels(results$Category) <- c("All","Mothers","Offspring")
results$mean_worry <- 100*results$mean_worry

# plotting
p <- ggplot(data = results, aes(x = Week, y = mean_worry, color=Category)) +
  #geom_point(position=position_dodge(width=1)) +
  #geom_errorbar(aes(ymin=mean_worry-sd_worry, ymax=mean_worry+sd_worry), width=.2, position=position_dodge(width=1)) +
  geom_point(stat='summary', fun=sum, position=position_dodge(width = 0.2)) +
  stat_summary(fun=sum, geom="line") +
  #scale_x_date(date_labels="%d %b",date_breaks  ="3 day") + 
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Percent precaution (%)") +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  #expand_limits(x = as.Date(c("2020-02-28", "2020-06-30"))) +
  ggtitle("Covering nose and mouth in public") +
  theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(plot.margin=unit(c(0.3,0.1,0.1,2),"cm"))

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/DNBC_precaution_covernose.pdf", width = 6, height = 3)
p
dev.off()


# specific worries and precautions
results_all <- DNBC_worries %>%
  group_by(Week) %>%
  summarise(mean_worry = sum(transport == 1)/sum(transport == 1 | transport == 2))
results_all$Category <- 0
results_strat <- DNBC_worries %>%
  group_by(Week, Category) %>%
  summarise(mean_worry = sum(transport == 1)/sum(transport == 1 | transport == 2))
colord <- colnames(results_strat)
results_all <- results_all[colord]
results <- rbind(results_all, results_strat)
results$Category <- as.factor(results$Category)
levels(results$Category) <- c("All","Mothers","Offspring")
results$mean_worry <- 100*results$mean_worry

# plotting
p <- ggplot(data = results, aes(x = Week, y = mean_worry, color=Category)) +
  #geom_point(position=position_dodge(width=1)) +
  #geom_errorbar(aes(ymin=mean_worry-sd_worry, ymax=mean_worry+sd_worry), width=.2, position=position_dodge(width=1)) +
  geom_point(stat='summary', fun=sum, position=position_dodge(width = 0.2)) +
  stat_summary(fun=sum, geom="line") +
  #scale_x_date(date_labels="%d %b",date_breaks  ="3 day") + 
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Percent precaution (%)") +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  #expand_limits(x = as.Date(c("2020-02-28", "2020-06-30"))) +
  ggtitle("Avoiding public transportation") +
  theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(plot.margin=unit(c(0.3,0.1,0.1,2),"cm"))

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/DNBC_precaution_transport.pdf", width = 6, height = 3)
p
dev.off()


# specific worries and precautions
results_all <- DNBC_worries %>%
  group_by(Week) %>%
  summarise(mean_worry = sum(travel == 1)/sum(travel == 1 | travel == 2))
results_all$Category <- 0
results_strat <- DNBC_worries %>%
  group_by(Week, Category) %>%
  summarise(mean_worry = sum(travel == 1)/sum(travel == 1 | travel == 2))
colord <- colnames(results_strat)
results_all <- results_all[colord]
results <- rbind(results_all, results_strat)
results$Category <- as.factor(results$Category)
levels(results$Category) <- c("All","Mothers","Offspring")
results$mean_worry <- 100*results$mean_worry

# plotting
p <- ggplot(data = results, aes(x = Week, y = mean_worry, color=Category)) +
  #geom_point(position=position_dodge(width=1)) +
  #geom_errorbar(aes(ymin=mean_worry-sd_worry, ymax=mean_worry+sd_worry), width=.2, position=position_dodge(width=1)) +
  geom_point(stat='summary', fun=sum, position=position_dodge(width = 0.2)) +
  stat_summary(fun=sum, geom="line") +
  #scale_x_date(date_labels="%d %b",date_breaks  ="3 day") + 
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Percent precaution (%)") +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  #expand_limits(x = as.Date(c("2020-02-28", "2020-06-30"))) +
  ggtitle("Avoiding traveling") +
  theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(plot.margin=unit(c(0.3,0.1,0.1,2),"cm"))

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/DNBC_precaution_travel.pdf", width = 6, height = 3)
p
dev.off()


# specific worries and precautions
results_all <- DNBC_worries %>%
  group_by(Week) %>%
  summarise(mean_worry = sum(cleanmore == 1)/sum(cleanmore == 1 | cleanmore == 2))
results_all$Category <- 0
results_strat <- DNBC_worries %>%
  group_by(Week, Category) %>%
  summarise(mean_worry = sum(cleanmore == 1)/sum(cleanmore == 1 | cleanmore == 2))
colord <- colnames(results_strat)
results_all <- results_all[colord]
results <- rbind(results_all, results_strat)
results$Category <- as.factor(results$Category)
levels(results$Category) <- c("All","Mothers","Offspring")
results$mean_worry <- 100*results$mean_worry

# plotting
p <- ggplot(data = results, aes(x = Week, y = mean_worry, color=Category)) +
  #geom_point(position=position_dodge(width=1)) +
  #geom_errorbar(aes(ymin=mean_worry-sd_worry, ymax=mean_worry+sd_worry), width=.2, position=position_dodge(width=1)) +
  geom_point(stat='summary', fun=sum, position=position_dodge(width = 0.2)) +
  stat_summary(fun=sum, geom="line") +
  #scale_x_date(date_labels="%d %b",date_breaks  ="3 day") + 
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Percent precaution (%)") +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  #expand_limits(x = as.Date(c("2020-02-28", "2020-06-30"))) +
  ggtitle("Cleaning more") +
  theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(plot.margin=unit(c(0.3,0.1,0.1,2),"cm"))

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/DNBC_precaution_cleanmore.pdf", width = 6, height = 3)
p
dev.off()


# specific worries and precautions
results_all <- DNBC_worries %>%
  group_by(Week) %>%
  summarise(mean_worry = sum(ventilatemore == 1)/sum(ventilatemore == 1 | ventilatemore == 2))
results_all$Category <- 0
results_strat <- DNBC_worries %>%
  group_by(Week, Category) %>%
  summarise(mean_worry = sum(ventilatemore == 1)/sum(ventilatemore == 1 | ventilatemore == 2))
colord <- colnames(results_strat)
results_all <- results_all[colord]
results <- rbind(results_all, results_strat)
results$Category <- as.factor(results$Category)
levels(results$Category) <- c("All","Mothers","Offspring")
results$mean_worry <- 100*results$mean_worry

# plotting
p <- ggplot(data = results, aes(x = Week, y = mean_worry, color=Category)) +
  #geom_point(position=position_dodge(width=1)) +
  #geom_errorbar(aes(ymin=mean_worry-sd_worry, ymax=mean_worry+sd_worry), width=.2, position=position_dodge(width=1)) +
  geom_point(stat='summary', fun=sum, position=position_dodge(width = 0.2)) +
  stat_summary(fun=sum, geom="line") +
  #scale_x_date(date_labels="%d %b",date_breaks  ="3 day") + 
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Percent precaution (%)") +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  #expand_limits(x = as.Date(c("2020-02-28", "2020-06-30"))) +
  ggtitle("Airing out and ventillating more") +
  theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(plot.margin=unit(c(0.3,0.1,0.1,2),"cm"))

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/DNBC_precaution_ventilatemore.pdf", width = 6, height = 3)
p
dev.off()


