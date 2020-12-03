library(readxl)
library(ggplot2)

setwd("C:/Users/vrw657/Documents/Projects/Corona_SJPH/UK_plots")

# load UK loneliness data
res_lonely <- read_excel("UK_loneliness.xlsx")
res_lonely$Category <- factor(res_lonely$Category, levels=c("All", "Gender", "Age", 
                                                            "Education", "Chronic disease",
                                                            "Mental illness"))
res_lonely <- cbind(res_lonely, correctorder = c(0,0,1,0,1,2,0,1,2,1,0,1,0))
saveRDS(res_lonely, "C:/Users/vrw657/Documents/Projects/Corona_SJPH/Data/UK_lonely.rds")

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
  scale_y_continuous(limits = c(0,50), expand = c(0, 0)) +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(face = c('plain','plain','plain','plain','plain',
                                            'plain','bold','plain','plain','plain',
                                            'plain','plain'))) +
  theme(axis.text.x = element_text(size = c(8,8,8,8,8,8,12,8,8,8,8,8))) + 
  theme(strip.text.x = element_text(size = 7))
#scale_x_discrete(labels=c("All"=expression(bold("All")), parse=TRUE))


pdf("UK_stratified_loneliness.pdf", width = 9, height = 2.6)
q
dev.off()


# load UK multichoice data
results_worries <- read_excel("UK_multichoice.xlsx")
varorder <- results_worries$variable
results_worries$variable <- factor(results_worries$variable,
                                   levels=varorder)
results_worries$percent_uCI <- results_worries$percent + 1.96*(sqrt((results_worries$percent*(100-results_worries$percent))/results_worries$countT))
results_worries$percent_lCI <- results_worries$percent - 1.96*(sqrt((results_worries$percent*(100-results_worries$percent))/results_worries$countT))


#multichoice plot
p <- ggplot(data = results_worries, aes(x = forcats::fct_rev(variable) , y = percent, fill = value)) +
  geom_bar(stat="identity", width = 0.7) +
  geom_errorbar(aes(ymin=percent_lCI, ymax=percent_uCI), width=.2) +
  coord_flip() +
  ylab("Percentage") + 
  xlab("Worries") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")


pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/UK_plots/UK_multicat.pdf", width = 8, height = 6)
p
dev.off()


# load UK time data
results_time <- read_excel("UK_time.xlsx")
varorder <- results_time$week
results_time$week <- factor(results_time$week,
                            levels=varorder)

results_time$countT <- c(27752,	27948,	38846,	39384,	38465,	37242,	
                      36443,	36583,	34576,	33012,	31887,	30618,	29731)

results_time$percent_uCI <- results_time$percent + 1.96*(sqrt((results_time$percent*(100-results_time$percent))/results_time$countT))
results_time$percent_lCI <- results_time$percent - 1.96*(sqrt((results_time$percent*(100-results_time$percent))/results_time$countT))


# time plot
p <- ggplot(data = results_time, aes(x = week , y = percent, fill = "red")) +
  geom_bar(stat="identity", width = 0.7) +
  geom_errorbar(aes(ymin=percent_lCI, ymax=percent_uCI), width=.2) +
  ylab("Percentage with high anxiety") + 
  xlab("") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.text.x=element_text(angle=30, hjust=1),
        axis.line = element_line(colour = "black")) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0,30), expand = c(0, 0)) +
  theme(plot.margin=unit(c(0.3,0.1,0.1,2),"cm")) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )


pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/UK_plots/UK_anxiety_time.pdf", width = 9, height = 3)
p
dev.off()

