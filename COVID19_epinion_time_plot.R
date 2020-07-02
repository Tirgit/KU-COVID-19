#### LOAD PACKAGES & SET WORKING DIRECTORY
library(knitr)
library(dplyr) 
library(ggplot2)
library(tidyverse)
library(lubridate)

# run shared .Rmd file until load of these 3 are done and save
#saveRDS(d_pop, "C:/Users/vrw657/Documents/Projects/Corona_SJPH/Data/d_pop.rds")

# merging 3 datasets
d_pop <- readRDS("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Data/d_pop.rds")

# worriedness time plot
varkeep <- c("q20", "date")
d_pop_selected <- subset(d_pop, select = varkeep )
colnames(d_pop_selected) <- c("Worried", "Date")
d_pop_selected$Worried[d_pop_selected$Worried == "1 – Ikke bekymret"] <- "1"
d_pop_selected$Worried[d_pop_selected$Worried == "10 – Meget bekymret"] <- "10"
d_pop_selected$Worried <- as.numeric(d_pop_selected$Worried)


# calculating proportions
results <- d_pop_selected %>%
  group_by(Date) %>%
  summarise(mean_worry = mean(Worried),
            sd_worry = sd(Worried))

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

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/Epinion_worry_time.pdf", width = 9, height = 3)
p
dev.off()

