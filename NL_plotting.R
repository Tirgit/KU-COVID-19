library(readxl)
library(ggplot2)
library(gridExtra)


setwd("C:/Users/vrw657/Documents/Projects/Corona_SJPH/NL_plots")

# load NL loneliness data
res_lonely <- read_excel("NL_loneliness.xlsx")
res_lonely$Category <- factor(res_lonely$Category, levels=c("All", "Gender", "Age", 
                                                            "Education", "Chronic disease",
                                                            "Mental illness"))
res_lonely <- cbind(res_lonely, correctorder = c(0,0,1,0,1,2,0,1,2,1,0,1,0))
saveRDS(res_lonely, "C:/Users/vrw657/Documents/Projects/Corona_SJPH/Data/NL_lonely.rds")


# loneliness plot
q <- ggplot(data = res_lonely, aes(x = reorder(Strata, correctorder), y = percent, fill = barcolor)) +
  facet_grid(~Category, scale = "free", space = "free") +
  geom_bar(stat="identity", width = 0.7) +
  ylab("Percentage with high loneliness") +
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_y_continuous(limits = c(0,30), expand = c(0, 0)) +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(face = c('plain','plain','plain','plain','plain',
                                            'plain','bold','plain','plain','plain',
                                            'plain','plain'))) +
  theme(axis.text.x = element_text(size = c(8,8,8,8,8,8,12,8,8,8,8,8))) + 
  theme(strip.text.x = element_text(size = 7))
#scale_x_discrete(labels=c("All"=expression(bold("All")), parse=TRUE))


pdf("NL_stratified_loneliness.pdf", width = 9, height = 2.6)
q
dev.off()


# load NL multichoice data
results <- read_excel("NL_multichoice.xlsx")
results_worries <- results[1:7,]
results_precautions <- results[8:14,]
varorder <- results_worries$variable
results_worries$variable <- factor(results_worries$variable,
                                   levels=varorder)
varorder <- results_precautions$variable
results_precautions$variable <- factor(results_precautions$variable,
                                       levels=varorder)

#multichoice plot
p1 <- ggplot(data = results_worries, aes(x = forcats::fct_rev(variable) , y = percent, fill = value)) +
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
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  theme(legend.position = "none")

p2 <- ggplot(data = results_precautions, aes(x = forcats::fct_rev(variable) , y = percent, fill = value)) +
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
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  theme(legend.position = "none")

gp1 <- ggplotGrob(p1)
gp2 <- ggplotGrob(p2)

maxWidth = grid::unit.pmax(gp1$widths[2:5], gp2$widths[2:5])
gp1$widths[2:5] <- as.list(maxWidth)
gp2$widths[2:5] <- as.list(maxWidth)

#title1 <- grid::textGrob("Worries and Precautions in the Citizen Science cohort", gp=grid::gpar(fontsize=12,font=1))
#grid.arrange(gp1, gp2, nrow = 2, ncol=1, heights=c(2,2), top=title1)

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/NL_plots/NL_multicat.pdf", width = 8, height = 6)
grid.arrange(gp1, gp2, nrow = 2, ncol=1, heights=c(2,2))
dev.off()


# load NL time data
results_time <- read_excel("NL_time.xlsx")
varorder <- results_time$week
results_time$week <- factor(results_time$week,
                            levels=varorder)
results_time$Date <- c("2020-03-30", "2020-04-02", "2020-04-12",
                       "2020-04-16", "2020-04-19", "2020-04-28",
                       "2020-05-15")
results_time$Date <- as.Date(results_time$Date)

q <- ggplot(data = results_time, aes(x = week, y = mean_worry)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean_worry-sd_worry, ymax=mean_worry+sd_worry), width=.2) +
  #scale_x_date(date_labels="%d %b",date_breaks  ="3 day") + 
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Mean levels of worries") +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0)) +
  #expand_limits(x = as.Date(c("2020-02-28", "2020-06-30"))) +
  #ggtitle("Mean levels of worriedness about the crisis") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(plot.margin=unit(c(0.3,0.1,0.1,2),"cm"))



# time plot
p <- ggplot(data = results_time, aes(x = Date, y = mean_worry)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean_worry-sd_worry, ymax=mean_worry+sd_worry), width=.2) +
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1),
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
  #ggtitle("Mean levels of worriedness about the crisis") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(plot.margin=unit(c(0.3,0.1,0.1,2),"cm"))


pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/NL_plots/NL_worry_time.pdf", width = 9, height = 3)
p
dev.off()


pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/NL_plots/NL_worry_week.pdf", width = 9, height = 3)
q
dev.off()