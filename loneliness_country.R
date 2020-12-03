library(ggplot2)

DK_lonely <- readRDS("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Data/DK_lonely.rds")
FR_lonely <- readRDS("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Data/FR_lonely.rds")
NL_lonely <- readRDS("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Data/NL_lonely.rds")
UK_lonely <- readRDS("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Data/UK_lonely.rds")

DK_lonely$country <- "Denmark"
FR_lonely$country <- "France"
FR_lonely$countT <- c(29974, 15229, 14745, 2562, 20004, 7408,
                      8068, 11596, 10310, 12257, 15767, 1083, 26941)
NL_lonely$country <- "Netherlands"
UK_lonely$country <- "United Kingdom"

DK_lonely$percent_uCI <- DK_lonely$percent + 1.96*(sqrt((DK_lonely$percent*(100-DK_lonely$percent))/DK_lonely$countT))
DK_lonely$percent_lCI <- DK_lonely$percent - 1.96*(sqrt((DK_lonely$percent*(100-DK_lonely$percent))/DK_lonely$countT))
FR_lonely$percent_uCI <- FR_lonely$percent + 1.96*(sqrt((FR_lonely$percent*(100-FR_lonely$percent))/FR_lonely$countT))
FR_lonely$percent_lCI <- FR_lonely$percent - 1.96*(sqrt((FR_lonely$percent*(100-FR_lonely$percent))/FR_lonely$countT))
NL_lonely$percent_uCI <- NL_lonely$percent + 1.96*(sqrt((NL_lonely$percent*(100-NL_lonely$percent))/NL_lonely$countT))
NL_lonely$percent_lCI <- NL_lonely$percent - 1.96*(sqrt((NL_lonely$percent*(100-NL_lonely$percent))/NL_lonely$countT))
UK_lonely$percent_uCI <- UK_lonely$percent + 1.96*(sqrt((UK_lonely$percent*(100-UK_lonely$percent))/UK_lonely$countT))
UK_lonely$percent_lCI <- UK_lonely$percent - 1.96*(sqrt((UK_lonely$percent*(100-UK_lonely$percent))/UK_lonely$countT))


keepvars <- c("percent", "Strata", "Category", "correctorder", "country",
              "percent_lCI", "percent_uCI")

DK_sel <- subset(DK_lonely, select = keepvars )
FR_sel <- subset(FR_lonely, select = keepvars )
NL_sel <- subset(NL_lonely, select = keepvars )
UK_sel <- subset(UK_lonely, select = keepvars )

lon_sel <- rbind(DK_sel, FR_sel, NL_sel, UK_sel)

# GENDER
all <- lon_sel[lon_sel$Category == "Gender",]

q <- ggplot(data = all, aes(x= Strata, y = percent, fill = correctorder)) +
  facet_grid(~country, scale = "free", space = "free") +
  geom_bar(stat="identity", width = 0.7) +
  geom_errorbar(aes(ymin=percent_lCI, ymax=percent_uCI), width=.2) +
  ylab("% with high loneliness") +
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
  theme(strip.text.x = element_blank(),
        strip.background.x = element_blank())


pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Comp_plots/gender_stratified_loneliness.pdf", width = 9, height = 2)
q
dev.off()

# AGE
all <- lon_sel[lon_sel$Category == "Age",]

q <- ggplot(data = all, aes(x= reorder(Strata, correctorder), y = percent, fill = correctorder)) +
  facet_grid(~country, scale = "free", space = "free") +
  geom_bar(stat="identity", width = 0.7) +
  geom_errorbar(aes(ymin=percent_lCI, ymax=percent_uCI), width=.2) +
  ylab("% with high loneliness") +
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_y_continuous(limits = c(0,35), expand = c(0, 0)) +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(strip.text.x = element_blank(),
        strip.background.x = element_blank())


pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Comp_plots/age_stratified_loneliness.pdf", width = 9, height = 2)
q
dev.off()

# EDUCATION
all <- lon_sel[lon_sel$Category == "Education",]

q <- ggplot(data = all, aes(x= reorder(Strata, correctorder), y = percent, fill = correctorder)) +
  facet_grid(~country, scale = "free", space = "free") +
  geom_bar(stat="identity", width = 0.7) +
  geom_errorbar(aes(ymin=percent_lCI, ymax=percent_uCI), width=.2) +
  ylab("% with high loneliness") +
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
  theme(strip.text.x = element_blank(),
        strip.background.x = element_blank())


pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Comp_plots/educ_stratified_loneliness.pdf", width = 9, height = 2)
q
dev.off()


# CHRONIC
all <- lon_sel[lon_sel$Category == "Chronic disease",]

q <- ggplot(data = all, aes(x= reorder(Strata, correctorder), y = percent, fill = correctorder)) +
  facet_grid(~country, scale = "free", space = "free") +
  geom_bar(stat="identity", width = 0.7) +
  geom_errorbar(aes(ymin=percent_lCI, ymax=percent_uCI), width=.2) +
  ylab("% with high loneliness") +
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
  theme(strip.text.x = element_blank(),
        strip.background.x = element_blank())


pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Comp_plots/chronic_stratified_loneliness.pdf", width = 9, height = 2)
q
dev.off()


# MENTAL
all <- lon_sel[lon_sel$Category == "Mental illness",]

q <- ggplot(data = all, aes(x= reorder(Strata, correctorder), y = percent, fill = correctorder)) +
  facet_grid(~country, scale = "free", space = "free") +
  geom_bar(stat="identity", width = 0.7) +
  geom_errorbar(aes(ymin=percent_lCI, ymax=percent_uCI), width=.2) +
  ylab("% with high loneliness") +
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
  theme(strip.text.x = element_blank(),
        strip.background.x = element_blank())


pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Comp_plots/mental_stratified_loneliness.pdf", width = 9, height = 2)
q
dev.off()



all <- lon_sel[lon_sel$Category == "All",]

q <- ggplot(data = all, aes(x = Strata, y = percent, fill = correctorder)) +
  facet_grid(~country, scale = "free", space = "free") +
  geom_bar(stat="identity", width = 0.7) +
  geom_errorbar(aes(ymin=percent_lCI, ymax=percent_uCI), width=.2) +
  ylab("% with high loneliness") +
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_y_continuous(limits = c(0,25), expand = c(0, 0)) +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(strip.text.x = element_text(size = 7))
#scale_x_discrete(labels=c("All"=expression(bold("All")), parse=TRUE))


pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Comp_plots/all_stratified_loneliness.pdf", width = 9, height = 2)
q
dev.off()





















