library(tidyverse)


d_pop$age <- as.numeric(d_pop$q2)
d_fam$age <- as.numeric(d_fam$q2)
d_eld$age <- as.numeric(d_eld$q2)
d_pop$q16_1_num <-recode(d_pop$q16_1_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_pop$q16_2_num <-recode(d_pop$q16_2_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_pop$q16_3_num <-recode(d_pop$q16_3_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_pop$q16_sum <- d_pop$q16_1_num + d_pop$q16_2_num + d_pop$q16_3_num
d_pop$severe_lonely <- 0
d_pop$severe_lonely[d_pop$q16_sum >= 7] <- 1
d_fam$q16_1_num <-recode(d_fam$q16_1_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_fam$q16_2_num <-recode(d_fam$q16_2_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_fam$q16_3_num <-recode(d_fam$q16_3_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_fam$q16_sum <- d_fam$q16_1_num + d_fam$q16_2_num + d_fam$q16_3_num
d_fam$severe_lonely <- 0
d_fam$severe_lonely[d_fam$q16_sum >= 7] <- 1
d_eld$q16_1_num <-recode(d_eld$q16_1_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_eld$q16_2_num <-recode(d_eld$q16_2_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_eld$q16_3_num <-recode(d_eld$q16_3_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_eld$q16_sum <- d_eld$q16_1_num + d_eld$q16_2_num + d_eld$q16_3_num
d_eld$severe_lonely <- 0
d_eld$severe_lonely[d_eld$q16_sum >= 7] <- 1

d_pop$date <- as.Date(d_pop$date)
d_fam$date <- as.Date(d_fam$date)
d_eld$date <- as.Date(d_eld$date)

d_pop_subset <- subset(d_pop, select = c("age", "severe_lonely", "date"))
d_fam_subset <- subset(d_fam, select = c("age", "severe_lonely", "date"))
d_eld_subset <- subset(d_eld, select = c("age", "severe_lonely", "date"))

d_merged <- rbind(d_pop_subset, d_fam_subset, d_eld_subset)
summary(d_merged$age)

d_merged$agecat <- "30-44 yrs"
d_merged$agecat[d_merged$age < 30] <- "18-29 yrs"
d_merged$agecat[d_merged$age < 65 & d_merged$age >=45] <- "45-64 yrs"
d_merged$agecat[d_merged$age >= 65] <- "65-98 yrs"
table(d_merged$agecat)

d_merged_1 <- d_merged[d_merged$date < "2020-06-01",]
table(d_merged_1$date)
table(d_merged_1$agecat)
d_merged_2 <- d_merged[d_merged$date > "2020-09-01" & d_merged$date < "2020-11-01",]
table(d_merged_2$date)
table(d_merged_2$agecat)
d_merged_3 <- d_merged[d_merged$date >= "2020-11-01"& d_merged$date < "2021-01-01",]
table(d_merged_3$date)
table(d_merged_3$agecat)
d_merged_4 <- d_merged[d_merged$date >= "2021-01-01"& d_merged$date < "2021-03-01",]
table(d_merged_4$date)
table(d_merged_4$agecat)
d_merged_5 <- d_merged[d_merged$date >= "2021-03-01"& d_merged$date < "2021-05-01",]
table(d_merged_5$date)
table(d_merged_5$agecat)
d_merged_6 <- d_merged[d_merged$date >= "2021-05-01",]
table(d_merged_6$date)
table(d_merged_6$agecat)

res1 <- d_merged_1 %>%
  group_by(agecat) %>%
  summarise(n = n(),
            percent_lon = 100*(sum(severe_lonely == 1)/n),
            percent_uCI = percent_lon + 1.96*(sqrt((percent_lon*(100-percent_lon))/n)),
            percent_lCI = percent_lon - 1.96*(sqrt((percent_lon*(100-percent_lon))/n)))

res2 <- d_merged_2 %>%
  group_by(agecat) %>%
  summarise(n = n(),
            percent_lon = 100*(sum(severe_lonely == 1)/n),
            percent_uCI = percent_lon + 1.96*(sqrt((percent_lon*(100-percent_lon))/n)),
            percent_lCI = percent_lon - 1.96*(sqrt((percent_lon*(100-percent_lon))/n)))

res3 <- d_merged_3 %>%
  group_by(agecat) %>%
  summarise(n = n(),
            percent_lon = 100*(sum(severe_lonely == 1)/n),
            percent_uCI = percent_lon + 1.96*(sqrt((percent_lon*(100-percent_lon))/n)),
            percent_lCI = percent_lon - 1.96*(sqrt((percent_lon*(100-percent_lon))/n)))

res4 <- d_merged_4 %>%
  group_by(agecat) %>%
  summarise(n = n(),
            percent_lon = 100*(sum(severe_lonely == 1)/n),
            percent_uCI = percent_lon + 1.96*(sqrt((percent_lon*(100-percent_lon))/n)),
            percent_lCI = percent_lon - 1.96*(sqrt((percent_lon*(100-percent_lon))/n)))

res5 <- d_merged_5 %>%
  group_by(agecat) %>%
  summarise(n = n(),
            percent_lon = 100*(sum(severe_lonely == 1)/n),
            percent_uCI = percent_lon + 1.96*(sqrt((percent_lon*(100-percent_lon))/n)),
            percent_lCI = percent_lon - 1.96*(sqrt((percent_lon*(100-percent_lon))/n)))

res6 <- d_merged_6 %>%
  group_by(agecat) %>%
  summarise(n = n(),
            percent_lon = 100*(sum(severe_lonely == 1)/n),
            percent_uCI = percent_lon + 1.96*(sqrt((percent_lon*(100-percent_lon))/n)),
            percent_lCI = percent_lon - 1.96*(sqrt((percent_lon*(100-percent_lon))/n)))


res1$agecat <- as.factor(res1$agecat)
res1$agecat <- fct_rev(res1$agecat)
res2$agecat <- as.factor(res2$agecat)
res2$agecat <- fct_rev(res2$agecat)
res3$agecat <- as.factor(res3$agecat)
res3$agecat <- fct_rev(res3$agecat)
res4$agecat <- as.factor(res4$agecat)
res4$agecat <- fct_rev(res4$agecat)
res5$agecat <- as.factor(res5$agecat)
res5$agecat <- fct_rev(res5$agecat)
res6$agecat <- as.factor(res6$agecat)
res6$agecat <- fct_rev(res6$agecat)


res1$samplesize <-  paste0("n=", res1$n)
res2$samplesize <-  paste0("n=", res2$n)
res3$samplesize <-  paste0("n=", res3$n)
res4$samplesize <-  paste0("n=", res4$n)
res5$samplesize <-  paste0("n=", res5$n)
res6$samplesize <-  paste0("n=", res6$n)


res1$perctext <-  paste0(round(res1$percent_lon,0), "%")
res2$perctext <-  paste0(round(res2$percent_lon,0), "%")
res3$perctext <-  paste0(round(res3$percent_lon,0), "%")
res4$perctext <-  paste0(round(res4$percent_lon,0), "%")
res5$perctext <-  paste0(round(res5$percent_lon,0), "%")
res6$perctext <-  paste0(round(res6$percent_lon,0), "%")


p <- ggplot(data = res1, aes(y = percent_lon, x = agecat)) +
  geom_bar(stat="identity", fill = "red", alpha = 0.5) +
  coord_flip() +
  geom_errorbar(aes(ymin=percent_lCI, ymax=percent_uCI), width=.2) +
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Age categories") +
  ylab("Percent (95% confidence interval)") +
  scale_y_continuous(limits = c(0,40), expand = c(0, 0)) +
  geom_hline(yintercept=0) +
  ggtitle("Percentage reporting severe loneliness (Mar - May 2020)") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none") + 
  geom_text(aes(y = 1.5, label=samplesize), vjust = -2.5) +
  geom_text(aes(y = percent_lon + 1, label=perctext), vjust = -2.5)
  
  
pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/Severe_loneliness_Mar_May_2020.pdf", width = 8, height = 6)
p
dev.off()

levels(res1$agecat) <- c("65-98 år", "45-64 år", "30-44 år", "18-29 år")

p <- ggplot(data = res1, aes(y = percent_lon, x = agecat)) +
  geom_bar(stat="identity", fill = "red", alpha = 0.5) +
  coord_flip() +
  geom_errorbar(aes(ymin=percent_lCI, ymax=percent_uCI), width=.2) +
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Alderskategorier") +
  ylab("Procent (95% konfidensinterval)") +
  scale_y_continuous(limits = c(0,40), expand = c(0, 0)) +
  geom_hline(yintercept=0) +
  ggtitle("Procentdel, der rapporterer svær ensomhed (mar - maj 2020)") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none") + 
  geom_text(aes(y = 1.5, label=samplesize), vjust = -2.5) +
  geom_text(aes(y = percent_lon + 1, label=perctext), vjust = -2.5)


pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/Severe_loneliness_Mar_May_2020_DAN.pdf", width = 8, height = 6)
p
dev.off()


p <- ggplot(data = res2, aes(y = percent_lon, x = agecat)) +
  geom_bar(stat="identity", fill = "red", alpha = 0.5) +
  coord_flip() +
  geom_errorbar(aes(ymin=percent_lCI, ymax=percent_uCI), width=.2) +
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Age categories") +
  ylab("Percent (95% confidence interval)") +
  scale_y_continuous(limits = c(0,40), expand = c(0, 0)) +
  geom_hline(yintercept=0) +
  ggtitle("Percentage reporting severe loneliness (Sep - Oct 2020)") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none") + 
  geom_text(aes(y = 1.5, label=samplesize), vjust = -2.5) +
  geom_text(aes(y = percent_lon + 1, label=perctext), vjust = -2.5)

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/Severe_loneliness_Sep_Oct_2020.pdf", width = 8, height = 6)
p
dev.off()

levels(res2$agecat) <- c("65-98 år", "45-64 år", "30-44 år", "18-29 år")

p <- ggplot(data = res2, aes(y = percent_lon, x = agecat)) +
  geom_bar(stat="identity", fill = "red", alpha = 0.5) +
  coord_flip() +
  geom_errorbar(aes(ymin=percent_lCI, ymax=percent_uCI), width=.2) +
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Alderskategorier") +
  ylab("Procent (95% konfidensinterval)") +
  scale_y_continuous(limits = c(0,40), expand = c(0, 0)) +
  geom_hline(yintercept=0) +
  ggtitle("Procentdel, der rapporterer svær ensomhed (sep - okt 2020)") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none") + 
  geom_text(aes(y = 1.5, label=samplesize), vjust = -2.5) +
  geom_text(aes(y = percent_lon + 1, label=perctext), vjust = -2.5)

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/Severe_loneliness_Sep_Oct_2020_DAN.pdf", width = 8, height = 6)
p
dev.off()

p <- ggplot(data = res3, aes(y = percent_lon, x = agecat)) +
  geom_bar(stat="identity", fill = "red", alpha = 0.5) +
  coord_flip() +
  geom_errorbar(aes(ymin=percent_lCI, ymax=percent_uCI), width=.2) +
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Age categories") +
  ylab("Percent (95% confidence interval)") +
  scale_y_continuous(limits = c(0,40), expand = c(0, 0)) +
  geom_hline(yintercept=0) +
  ggtitle("Percentage reporting severe loneliness (Nov - Dec 2020)") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none") + 
  geom_text(aes(y = 1.5, label=samplesize), vjust = -2.5) +
  geom_text(aes(y = percent_lon + 1, label=perctext), vjust = -2.5)

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/Severe_loneliness_Nov_Dec_2020.pdf", width = 8, height = 6)
p
dev.off()

levels(res3$agecat) <- c("65-98 år", "45-64 år", "30-44 år", "18-29 år")

p <- ggplot(data = res3, aes(y = percent_lon, x = agecat)) +
  geom_bar(stat="identity", fill = "red", alpha = 0.5) +
  coord_flip() +
  geom_errorbar(aes(ymin=percent_lCI, ymax=percent_uCI), width=.2) +
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Alderskategorier") +
  ylab("Procent (95% konfidensinterval)") +
  scale_y_continuous(limits = c(0,40), expand = c(0, 0)) +
  geom_hline(yintercept=0) +
  ggtitle("Procentdel, der rapporterer svær ensomhed (nov - dec 2020)") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none") + 
  geom_text(aes(y = 1.5, label=samplesize), vjust = -2.5) +
  geom_text(aes(y = percent_lon + 1, label=perctext), vjust = -2.5)

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/Severe_loneliness_Nov_Dec_2020_DAN.pdf", width = 8, height = 6)
p
dev.off()


p <- ggplot(data = res4, aes(y = percent_lon, x = agecat)) +
  geom_bar(stat="identity", fill = "red", alpha = 0.5) +
  coord_flip() +
  geom_errorbar(aes(ymin=percent_lCI, ymax=percent_uCI), width=.2) +
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Age categories") +
  ylab("Percent (95% confidence interval)") +
  scale_y_continuous(limits = c(0,40), expand = c(0, 0)) +
  geom_hline(yintercept=0) +
  ggtitle("Percentage reporting severe loneliness (Jan - Feb 2021)") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none") + 
  geom_text(aes(y = 1.5, label=samplesize), vjust = -2.5) +
  geom_text(aes(y = percent_lon + 1, label=perctext), vjust = -2.5)

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/Severe_loneliness_Jan_Feb_2021.pdf", width = 8, height = 6)
p
dev.off()

levels(res3$agecat) <- c("65-98 år", "45-64 år", "30-44 år", "18-29 år")

p <- ggplot(data = res4, aes(y = percent_lon, x = agecat)) +
  geom_bar(stat="identity", fill = "red", alpha = 0.5) +
  coord_flip() +
  geom_errorbar(aes(ymin=percent_lCI, ymax=percent_uCI), width=.2) +
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Alderskategorier") +
  ylab("Procent (95% konfidensinterval)") +
  scale_y_continuous(limits = c(0,40), expand = c(0, 0)) +
  geom_hline(yintercept=0) +
  ggtitle("Procentdel, der rapporterer svær ensomhed (jan - feb 2021)") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none") + 
  geom_text(aes(y = 1.5, label=samplesize), vjust = -2.5) +
  geom_text(aes(y = percent_lon + 1, label=perctext), vjust = -2.5)

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/Severe_loneliness_Jan_Feb_2021_DAN.pdf", width = 8, height = 6)
p
dev.off()


p <- ggplot(data = res5, aes(y = percent_lon, x = agecat)) +
  geom_bar(stat="identity", fill = "red", alpha = 0.5) +
  coord_flip() +
  geom_errorbar(aes(ymin=percent_lCI, ymax=percent_uCI), width=.2) +
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Age categories") +
  ylab("Percent (95% confidence interval)") +
  scale_y_continuous(limits = c(0,40), expand = c(0, 0)) +
  geom_hline(yintercept=0) +
  ggtitle("Percentage reporting severe loneliness (Mar - Apr 2021)") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none") + 
  geom_text(aes(y = 1.5, label=samplesize), vjust = -2.5) +
  geom_text(aes(y = percent_lon + 1, label=perctext), vjust = -2.5)

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/Severe_loneliness_Mar_Apr_2021.pdf", width = 8, height = 6)
p
dev.off()

levels(res3$agecat) <- c("65-98 år", "45-64 år", "30-44 år", "18-29 år")

p <- ggplot(data = res5, aes(y = percent_lon, x = agecat)) +
  geom_bar(stat="identity", fill = "red", alpha = 0.5) +
  coord_flip() +
  geom_errorbar(aes(ymin=percent_lCI, ymax=percent_uCI), width=.2) +
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Alderskategorier") +
  ylab("Procent (95% konfidensinterval)") +
  scale_y_continuous(limits = c(0,40), expand = c(0, 0)) +
  geom_hline(yintercept=0) +
  ggtitle("Procentdel, der rapporterer svær ensomhed (mar - apr 2021)") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none") + 
  geom_text(aes(y = 1.5, label=samplesize), vjust = -2.5) +
  geom_text(aes(y = percent_lon + 1, label=perctext), vjust = -2.5)

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/Severe_loneliness_Mar_Apr_2021_DAN.pdf", width = 8, height = 6)
p
dev.off()


p <- ggplot(data = res6, aes(y = percent_lon, x = agecat)) +
  geom_bar(stat="identity", fill = "red", alpha = 0.5) +
  coord_flip() +
  geom_errorbar(aes(ymin=percent_lCI, ymax=percent_uCI), width=.2) +
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Age categories") +
  ylab("Percent (95% confidence interval)") +
  scale_y_continuous(limits = c(0,40), expand = c(0, 0)) +
  geom_hline(yintercept=0) +
  ggtitle("Percentage reporting severe loneliness (May - Jun 2021)") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none") + 
  geom_text(aes(y = 1.5, label=samplesize), vjust = -2.5) +
  geom_text(aes(y = percent_lon + 1, label=perctext), vjust = -2.5)

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/Severe_loneliness_May_Jun_2021.pdf", width = 8, height = 6)
p
dev.off()

levels(res3$agecat) <- c("65-98 år", "45-64 år", "30-44 år", "18-29 år")

p <- ggplot(data = res6, aes(y = percent_lon, x = agecat)) +
  geom_bar(stat="identity", fill = "red", alpha = 0.5) +
  coord_flip() +
  geom_errorbar(aes(ymin=percent_lCI, ymax=percent_uCI), width=.2) +
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Alderskategorier") +
  ylab("Procent (95% konfidensinterval)") +
  scale_y_continuous(limits = c(0,40), expand = c(0, 0)) +
  geom_hline(yintercept=0) +
  ggtitle("Procentdel, der rapporterer svær ensomhed (maj - jun 2021)") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none") + 
  geom_text(aes(y = 1.5, label=samplesize), vjust = -2.5) +
  geom_text(aes(y = percent_lon + 1, label=perctext), vjust = -2.5)

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/Severe_loneliness_May_Jun_2021_DAN.pdf", width = 8, height = 6)
p
dev.off()
























































# plotting proportions in the general population
# d_pop_subset

# plot proportion severe lonely
res <- d_pop_subset %>%
  group_by(date) %>%
  summarise(n = n(),
            percent_lon = 100*(sum(severe_lonely == 1)/n),
            percent_uCI = percent_lon + 1.96*(sqrt((percent_lon*(100-percent_lon))/n)),
            percent_lCI = percent_lon - 1.96*(sqrt((percent_lon*(100-percent_lon))/n)))

res$n <- NULL

res$onlygenpop <- "2020 spring/summer"
res$onlygenpop[res$date > "2020-09-01"] <- "from 2020 autumn"

p <- ggplot(data = res, aes(y = percent_lon, x = date)) +
  facet_grid(~onlygenpop, scale = "free", space = "free") +
  geom_bar(stat="identity", fill = "red", alpha = 0.5, width = 2) +
  geom_errorbar(aes(ymin=percent_lCI, ymax=percent_uCI), width=.2) +
  scale_x_date(breaks = unique(res$date),
               labels = date_format("%d %b %Y")) +
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Percent (95% confidence interval)") +
  scale_y_continuous(limits = c(0,30), expand = c(0, 0)) +
  geom_hline(yintercept=0) +
  #expand_limits(x = as.Date(c("2020-03-03", "2020-12-30"))) +
  ggtitle("Percentage reporting severe loneliness in the general population") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none")

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/Proportion_severe_loneliness.pdf", width = 10, height = 4.5)
p
dev.off()

res$onlygenpop <- "forår/sommer 2020"
res$onlygenpop[res$date > "2020-09-01"] <- "fra efterår 2020"

p <- ggplot(data = res, aes(y = percent_lon, x = date)) +
  facet_grid(~onlygenpop, scale = "free", space = "free") +
  geom_bar(stat="identity", fill = "red", alpha = 0.5, width = 2) +
  geom_errorbar(aes(ymin=percent_lCI, ymax=percent_uCI), width=.2) +
  scale_x_date(breaks = unique(res$date),
               labels = date_format("%d %b %Y")) +
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Procent (95% konfidensinterval)") +
  scale_y_continuous(limits = c(0,30), expand = c(0, 0)) +
  geom_hline(yintercept=0) +
  #expand_limits(x = as.Date(c("2020-03-03", "2020-12-30"))) +
  ggtitle("Procentdel, der rapporterer svær ensomhed i befolkningen") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none")

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/Proportion_severe_loneliness_DAN.pdf", width = 10, height = 4.5)
p
dev.off()





