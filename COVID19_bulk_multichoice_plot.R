#### LOAD PACKAGES & SET WORKING DIRECTORY
library(readxl)
library(knitr)
library(haven)
library(radarchart)
library(readxl)
library(wesanderson)
library(dplyr)
library(ggplot2) 
library(GDAtools)
library(tidyverse)
library(gridExtra)

setwd("C:/Users/vrw657/Dropbox/Data")

#### LOAD LATEST CITIZEN SCIENCE DATA
d_con <- read_excel("Data stripped of free text/KUSUND_1805_AJM.xlsx")
d_con_q <- d_con[1,] # We save the questions in another data set
d_con <- d_con[-1,] # Removing the line with the questions

### removing not needed variables
varkeep <- c("q1", "q2", "q5", "q21_1","q21_2","q21_3",
             "q21_4","q21_5","q21_6","q21_7","q21_9",
             "q22_1","q22_2","q22_3","q22_4","q22_5","q22_6")

d_con_selected = subset(d_con, select = varkeep )

cols_worries_eng <- c("Becoming seriously ill",
              "Someone close to you becoming seriously ill",
              "You or your family experiencing serious financial problems",
              "Losing your job",
              "Long time before resuming regular everyday life",
              "Not being able to see family/friends",
              "A national economic crisis",
              "Not concerned about the corona crisis")
cols_precautions_eng <- c("Increased hand washing and use of hand sanitiser",
                   "Keeping physical distance from strangers",
                   "Avoiding physical contact (except co-habitation/relationship)",
                   "Covering nose and mouth in public", 
                   "Avoiding public transportation",
                   "Avoiding traveling")

colnames(d_con_selected) <- c("Sex", "Age", "Education",
                              cols_worries_eng,
                              cols_precautions_eng)

# recode variables
table(d_con_selected$Sex)
d_con_selected$Sex[d_con_selected$Sex == "Mand"] <- "Male"
d_con_selected$Sex[d_con_selected$Sex == "Kvinde"] <- "Female"
d_con_selected$Sex[d_con_selected$Sex == "Andet"] <- "Other/Not specified"
d_con_selected$Sex[d_con_selected$Sex == "Ønsker ikke at oplyse"] <- "Other/Not specified"
d_con_selected$Sex <- as.factor(d_con_selected$Sex)
levels(d_con_selected$Sex)
sextable <- table(d_con_selected$Sex)
n_female <- as.numeric(sextable[1])
n_male <- as.numeric(sextable[2])
n_other <- as.numeric(sextable[3])
d_con_selected$Age <- as.numeric(d_con_selected$Age)
#table(d_con_selected$Education)
d_con_selected$Education[d_con_selected$Education == "Andet, skriv:"] <- "Short-term education/Other"
d_con_selected$Education[d_con_selected$Education == "Erhvervsuddannelse/faglært"] <- "Short-term education/Other"
d_con_selected$Education[d_con_selected$Education == "Folkeskole"] <- "Short-term education/Other"
d_con_selected$Education[d_con_selected$Education == "Gymnasial uddannelse"] <- "Short-term education/Other"
d_con_selected$Education[d_con_selected$Education == "Kort videregående uddannelse"] <- "Short-term education/Other"
d_con_selected$Education[d_con_selected$Education == "Mellemlang videregående uddannelse"] <- "Middle-term education"
d_con_selected$Education[d_con_selected$Education == "Lang videregående uddannelse"] <- "Long-term education"
d_con_selected$Education <- as.factor(d_con_selected$Education)

summary(d_con_selected)

# melting data
d_con_melt <- as.data.frame(reshape2::melt(d_con_selected, id.var = c("Sex", "Age", "Education")))

results <- d_con_melt %>%
  group_by(variable, value) %>%
  summarise(total_n = n() ) %>%
  mutate(countT = 11356) %>%
  mutate(percent = round(100*total_n/countT,2))
# keeping only Yes lines (No's are redundant as we are interested in percentages)
results_yes <- results[results$value == "Yes",] 

results_worries <- results_yes[1:8,]
results_precautions <- results_yes[9:14,]

# barplots
p1 <- ggplot(data = results_worries, aes(x = fct_rev(variable) , y = percent, fill = value)) +
  geom_bar(stat="identity", width = 0.7) +
  coord_flip() +
  xlab("Worries") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  #ggtitle("Citizen Science, Worries and Precautions") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none")

p2 <- ggplot(data = results_precautions, aes(x = fct_rev(variable) , y = percent, fill = value)) +
  geom_bar(stat="identity", width = 0.7) +
  coord_flip() +
  ylab("Percentage") + 
  xlab("Precautions") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(legend.position = "none")

gp1 <- ggplotGrob(p1)
gp2 <- ggplotGrob(p2)

maxWidth = grid::unit.pmax(gp1$widths[2:5], gp2$widths[2:5])
gp1$widths[2:5] <- as.list(maxWidth)
gp2$widths[2:5] <- as.list(maxWidth)

title1 <- grid::textGrob("Worries and Precautions in the Citizen Science cohort", gp=grid::gpar(fontsize=12,font=1))
grid.arrange(gp1, gp2, nrow = 2, ncol=1, heights=c(2,2), top=title1)

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/citizen_science_multicat.pdf", width = 6, height = 6)
grid.arrange(gp1, gp2, nrow = 2, ncol=1, heights=c(2,2), top=title1)
dev.off()




