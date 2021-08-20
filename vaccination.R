# VACCINATION ATTITUDES
library(MASS) 

table(d_pop$date)
vac <- d_pop[d_pop$date > "2020-09-01",]
table(vac$date)
table(vac$q27_1_resp)

# WANT VACCINATED
vac$vac_yesno <- "No"
vac$vac_yesno[vac$q27_1_resp == "Ja"] <- "Yes"
table(vac$vac_yesno)

tbl <- table(vac$q27_1_resp, vac$q1)
chisq.test(tbl) 
tbl[1]/(tbl[1]+tbl[2])
tbl[3]/(tbl[3]+tbl[4])

vac$q2 <- as.numeric(vac$q2)
vac$agegroup <- "30-44 years old"
vac$agegroup[vac$q2 < 30] <- "18-29 years old"
vac$agegroup[vac$q2 < 65 & vac$q2 >= 45] <- "45-64 years old"
vac$agegroup[vac$q2 >= 65] <- "65-90 years old"
table(vac$agegroup)

tbl <- table(vac$q27_1_resp, vac$agegroup)
chisq.test(tbl) 
tbl[1]/(tbl[1]+tbl[2])
tbl[3]/(tbl[3]+tbl[4])
tbl[5]/(tbl[5]+tbl[6])

table(vac$q5)
vac$q5[vac$q5 == "Andet, skriv:"] <- "Short-term education/Other"
vac$q5[vac$q5 == "Erhvervsuddannelse/faglært"] <- "Short-term education/Other"
vac$q5[vac$q5 == "Folkeskole"] <- "Short-term education/Other"
vac$q5[vac$q5 == "Gymnasial uddannelse"] <- "Short-term education/Other"
vac$q5[vac$q5 == "Kort videregående uddannelse"] <- "Short-term education/Other"
vac$q5[vac$q5 == "Mellemlang videregående uddannelse"] <- "Middle-term education"
vac$q5[vac$q5 == "Lang videregående uddannelse"] <- "Long-term education"
table(vac$q5)

tbl <- table(vac$q27_1_resp, vac$q5)
chisq.test(tbl) 
tbl[1]/(tbl[1]+tbl[2])
tbl[3]/(tbl[3]+tbl[4])
tbl[5]/(tbl[5]+tbl[6])


# THOSE WHO WOULD NOT WANT
respno <- vac[vac$q27_1_resp == "Nej",]
# "I have already had COVID-19 and thus do not need to be vaccinated"
sum(respno$q28_1 == "Yes")/nrow(respno)
# "I am worried about side effects and thus do not want to be vaccinated"
sum(respno$q28_2 == "Yes")/nrow(respno)
# "I am not in a risk group and thus do not want to be vaccinated"
sum(respno$q28_3 == "Yes")/nrow(respno)
#  "I am generally opposed to vaccines"
sum(respno$q28_4 == "Yes")/nrow(respno)
# "I do not think the vaccine will have an effect"
sum(respno$q28_5 == "Yes")/nrow(respno)
# "Other reason"
sum(respno$q28_6 == "Yes")/nrow(respno)

onlycol <- cbind(respno$q28_1,respno$q28_2,respno$q28_3,
      respno$q28_4,respno$q28_5,respno$q28_6)

respno$q28_1_n <- 0
respno$q28_1_n[respno$q28_1 == "Yes"] <- 1
respno$q28_2_n <- 0
respno$q28_2_n[respno$q28_2 == "Yes"] <- 1
respno$q28_3_n <- 0
respno$q28_3_n[respno$q28_3 == "Yes"] <- 1
respno$q28_4_n <- 0
respno$q28_4_n[respno$q28_4 == "Yes"] <- 1
respno$q28_5_n <- 0
respno$q28_5_n[respno$q28_5 == "Yes"] <- 1
respno$q28_6_n <- 0
respno$q28_6_n[respno$q28_6 == "Yes"] <- 1

respno$totalyes <- respno$q28_1_n + respno$q28_2_n +
                       respno$q28_3_n + respno$q28_4_n +
                       respno$q28_5_n + respno$q28_5_n

table(respno$totalyes)






# load vaccination data
setwd("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Release_vaccination/")
res_lonely <- read_excel("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Release_vaccination/vacc_strat_Sep2020-Feb2021.xlsx")
res_lonely$Category <- factor(res_lonely$Category, levels=c("All", "Gender", "Age", 
                                                            "Education"))
res_lonely <- cbind(res_lonely, correctorder = c(0,0,1,0,1,2,3,0,1,2,
                                                 0,0,1,0,1,2,3,0,1,2,
                                                 0,0,1,0,1,2,3,0,1,2,
                                                 0,0,1,0,1,2,3,0,1,2))
res_lonely$Period <- factor(res_lonely$Period, levels=c("Sep 2020 - Nov 2020", 
                                                          "Dec 2020 - Feb 2021"))


no_res <- res_lonely[res_lonely$response == "No",]
notsure_res <- res_lonely[res_lonely$response != "No",]

# NO plot
p <- ggplot(data = no_res, aes(x = reorder(Strata, correctorder), y = percent, fill = Period)) +
  facet_grid(~Category, scale = "free", space = "free") +
  geom_bar(stat="identity", position = 'dodge', width = 0.7) +
  ylab("I would not get vaccinated (%)") +
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, size=12),
        axis.text = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.y = element_text(angle = 90),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        strip.text = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_y_continuous(limits = c(0,12.5), expand = c(0, 0)) +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none")
#scale_x_discrete(labels=c("All"=expression(bold("All")), parse=TRUE))

# NOTSURE plot
q <- ggplot(data = notsure_res, aes(x = reorder(Strata, correctorder), y = percent, fill = Period)) +
  facet_grid(~Category, scale = "free", space = "free") +
  geom_bar(stat="identity", position = 'dodge', width = 0.7) +
  ylab("I am not sure if I would get vaccinated (%)") +
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, size=12),
        axis.text = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.y = element_text(angle = 90),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        strip.text = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_y_continuous(limits = c(0,25), expand = c(0, 0)) +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none")
#scale_x_discrete(labels=c("All"=expression(bold("All")), parse=TRUE))




pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Release_vaccination/no_plot_Feb2021.pdf", width = 9, height = 7)
p
dev.off()

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Release_vaccination/notsure_plot_Feb2021.pdf", width = 9, height = 7)
q
dev.off()


# VACCINATION TRAJECTORIES

vac$wantvacc <- 0
vac$wantvacc[vac$q27_1_resp == "Ja"] <- 1
vac$dontwantvacc <- 0
vac$dontwantvacc[vac$q27_1_resp == "Nej"] <- 1
vac$notsurewantvacc <- 0
vac$notsurewantvacc[vac$q27_1_resp == "Ved ikke"] <- 1
vac$wgt <- as.numeric(vac$wgt)


merged <- vac %>%
  group_by(date) %>%
  summarise(yes_count = sum(wantvacc),
            yes_value = weighted.mean(wantvacc, wgt),
            yes_uCI = yes_value + 1.96*(sqrt((yes_value*(1-yes_value))/n())),
            yes_lCI = yes_value - 1.96*(sqrt((yes_value*(1-yes_value))/n())),
            no_count = sum(dontwantvacc),
            no_value = weighted.mean(dontwantvacc, wgt),
            no_uCI = no_value + 1.96*(sqrt((no_value*(1-no_value))/n())),
            no_lCI = no_value - 1.96*(sqrt((no_value*(1-no_value))/n())),
            notsure_count = sum(notsurewantvacc),
            notsure_value = weighted.mean(notsurewantvacc, wgt), 
            notsure_uCI = notsure_value + 1.96*(sqrt((notsure_value*(1-notsure_value))/n())),
            notsure_lCI = notsure_value - 1.96*(sqrt((notsure_value*(1-notsure_value))/n())),
            .groups = 'drop')

p <- ggplot(merged, aes(x=date)) +
  geom_point(aes(y=yes_value), size=3, alpha=0.5, color = "black") +
  geom_line(aes(y=yes_value), size=2, alpha=0.5, color = "black") +
  geom_errorbar(aes(ymin=yes_lCI, ymax=yes_uCI), width=.2, alpha=0.5, color = "black") +
  geom_point(aes(y=no_value), size=3, alpha=0.5, color = "red") +
  geom_line(aes(y=no_value), size=2, alpha=0.5, color = "red") +
  geom_errorbar(aes(ymin=no_lCI, ymax=no_uCI), width=.2, alpha=0.5, color = "red") +
  geom_point(aes(y=notsure_value), size=3, alpha=0.5, color = "blue") +
  geom_line(aes(y=notsure_value), size=2, alpha=0.5, color = "blue") +
  geom_errorbar(aes(ymin=notsure_lCI, ymax=notsure_uCI), width=.2, alpha=0.5, color = "blue") +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0),
                     labels = scales::percent) +
  scale_x_date(breaks = unique(vac$date),
               labels = date_format("%d %b %Y")) +
  labs(x = "", y = "Percentage of responders (%)") +
  theme(axis.title=element_text(size=20,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1,face="bold"),
        axis.text.y = element_text(face="bold"),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size=20))


tiff("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Release_vaccination/vac_trends_Jun2021_EN.tiff", units='in', width=10, height=10, res=300)
p
dev.off()

p <- ggplot(merged, aes(x=date)) +
  geom_point(aes(y=yes_value), size=3, alpha=0.5, color = "black") +
  geom_line(aes(y=yes_value), size=2, alpha=0.5, color = "black") +
  geom_errorbar(aes(ymin=yes_lCI, ymax=yes_uCI), width=.2, alpha=0.5, color = "black") +
  geom_point(aes(y=no_value), size=3, alpha=0.5, color = "red") +
  geom_line(aes(y=no_value), size=2, alpha=0.5, color = "red") +
  geom_errorbar(aes(ymin=no_lCI, ymax=no_uCI), width=.2, alpha=0.5, color = "red") +
  geom_point(aes(y=notsure_value), size=3, alpha=0.5, color = "blue") +
  geom_line(aes(y=notsure_value), size=2, alpha=0.5, color = "blue") +
  geom_errorbar(aes(ymin=notsure_lCI, ymax=notsure_uCI), width=.2, alpha=0.5, color = "blue") +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0),
                     labels = scales::percent) +
  scale_x_date(breaks = unique(vac$date),
               labels = date_format("%d %b %Y")) +
  labs(x = "", y = "Procentdel af respondere (%)") +
  theme(axis.title=element_text(size=20,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1,face="bold"),
        axis.text.y = element_text(face="bold"),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size=20))


tiff("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Release_vaccination/vac_trends_Jun2021_DK.tiff", units='in', width=10, height=10, res=300)
p
dev.off()


# GENDER - DANISH
merged <- vac %>%
  group_by(date, q1) %>%
  summarise(yes_count = sum(wantvacc),
            yes_value = weighted.mean(wantvacc, wgt),
            yes_uCI = yes_value + 1.96*(sqrt((yes_value*(1-yes_value))/n())),
            yes_lCI = yes_value - 1.96*(sqrt((yes_value*(1-yes_value))/n())),
            no_count = sum(dontwantvacc),
            no_value = weighted.mean(dontwantvacc, wgt),
            no_uCI = no_value + 1.96*(sqrt((no_value*(1-no_value))/n())),
            no_lCI = no_value - 1.96*(sqrt((no_value*(1-no_value))/n())),
            notsure_count = sum(notsurewantvacc),
            notsure_value = weighted.mean(notsurewantvacc, wgt), 
            notsure_uCI = notsure_value + 1.96*(sqrt((notsure_value*(1-notsure_value))/n())),
            notsure_lCI = notsure_value - 1.96*(sqrt((notsure_value*(1-notsure_value))/n())),
            .groups = 'drop')

merged[merged < 0] <- 0

merged$q1 <- factor(merged$q1, levels = c("Kvinde", "Mand"),
                  labels = c("Kvinde", "Mand"))

p <- ggplot(merged, aes(x=date)) +
  facet_grid(~q1) +
  geom_point(aes(y=yes_value), size=3, alpha=0.5, color = "black") +
  geom_line(aes(y=yes_value), size=2, alpha=0.5, color = "black") +
  geom_errorbar(aes(ymin=yes_lCI, ymax=yes_uCI), width=.2, alpha=0.5, color = "black") +
  geom_point(aes(y=no_value), size=3, alpha=0.5, color = "red") +
  geom_line(aes(y=no_value), size=2, alpha=0.5, color = "red") +
  geom_errorbar(aes(ymin=no_lCI, ymax=no_uCI), width=.2, alpha=0.5, color = "red") +
  geom_point(aes(y=notsure_value), size=3, alpha=0.5, color = "blue") +
  geom_line(aes(y=notsure_value), size=2, alpha=0.5, color = "blue") +
  geom_errorbar(aes(ymin=notsure_lCI, ymax=notsure_uCI), width=.2, alpha=0.5, color = "blue") +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0),
                     labels = scales::percent) +
  scale_x_date(breaks = unique(vac$date),
               labels = date_format("%d %b %Y")) +
  labs(x = "", y = "Procentdel af respondere (%)") +
  theme(axis.title=element_text(size=20,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1,face="bold"),
        axis.text.y = element_text(face="bold"),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size=20))


tiff("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Release_vaccination/vac_trends_sex_Jun2021_DK.tiff", units='in', width=10, height=7, res=300)
p
dev.off()


merged$q1 <- factor(merged$q1, levels = c("Kvinde", "Mand"),
                    labels = c("Women", "Men"))

p <- ggplot(merged, aes(x=date)) +
  facet_grid(~q1) +
  geom_point(aes(y=yes_value), size=3, alpha=0.5, color = "black") +
  geom_line(aes(y=yes_value), size=2, alpha=0.5, color = "black") +
  geom_errorbar(aes(ymin=yes_lCI, ymax=yes_uCI), width=.2, alpha=0.5, color = "black") +
  geom_point(aes(y=no_value), size=3, alpha=0.5, color = "red") +
  geom_line(aes(y=no_value), size=2, alpha=0.5, color = "red") +
  geom_errorbar(aes(ymin=no_lCI, ymax=no_uCI), width=.2, alpha=0.5, color = "red") +
  geom_point(aes(y=notsure_value), size=3, alpha=0.5, color = "blue") +
  geom_line(aes(y=notsure_value), size=2, alpha=0.5, color = "blue") +
  geom_errorbar(aes(ymin=notsure_lCI, ymax=notsure_uCI), width=.2, alpha=0.5, color = "blue") +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0),
                     labels = scales::percent) +
  scale_x_date(breaks = unique(vac$date),
               labels = date_format("%d %b %Y")) +
  labs(x = "", y = "Percentage of responders (%)") +
  theme(axis.title=element_text(size=20,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1,face="bold"),
        axis.text.y = element_text(face="bold"),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size=20))


tiff("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Release_vaccination/vac_trends_sex_Jun2021_EN.tiff", units='in', width=10, height=7, res=300)
p
dev.off()



# CHRONIC - DK
merged <- vac %>%
  group_by(date, q12) %>%
  summarise(yes_count = sum(wantvacc),
            yes_value = weighted.mean(wantvacc, wgt),
            yes_uCI = yes_value + 1.96*(sqrt((yes_value*(1-yes_value))/n())),
            yes_lCI = yes_value - 1.96*(sqrt((yes_value*(1-yes_value))/n())),
            no_count = sum(dontwantvacc),
            no_value = weighted.mean(dontwantvacc, wgt),
            no_uCI = no_value + 1.96*(sqrt((no_value*(1-no_value))/n())),
            no_lCI = no_value - 1.96*(sqrt((no_value*(1-no_value))/n())),
            notsure_count = sum(notsurewantvacc),
            notsure_value = weighted.mean(notsurewantvacc, wgt), 
            notsure_uCI = notsure_value + 1.96*(sqrt((notsure_value*(1-notsure_value))/n())),
            notsure_lCI = notsure_value - 1.96*(sqrt((notsure_value*(1-notsure_value))/n())),
            .groups = 'drop')

merged$q12 <- factor(merged$q12, levels = c("Ja", "Nej"),
                    labels = c("Kronisk sygdom", "Ingen kronisk sygdom"))



p <- ggplot(merged, aes(x=date)) +
  facet_grid(~q12) +
  geom_point(aes(y=yes_value), size=3, alpha=0.5, color = "black") +
  geom_line(aes(y=yes_value), size=2, alpha=0.5, color = "black") +
  #geom_errorbar(aes(ymin=yes_lCI, ymax=yes_uCI), width=.2, alpha=0.5, color = "black") +
  geom_point(aes(y=no_value), size=3, alpha=0.5, color = "red") +
  geom_line(aes(y=no_value), size=2, alpha=0.5, color = "red") +
  #geom_errorbar(aes(ymin=no_lCI, ymax=no_uCI), width=.2, alpha=0.5, color = "red") +
  geom_point(aes(y=notsure_value), size=3, alpha=0.5, color = "blue") +
  geom_line(aes(y=notsure_value), size=2, alpha=0.5, color = "blue") +
  #geom_errorbar(aes(ymin=notsure_lCI, ymax=notsure_uCI), width=.2, alpha=0.5, color = "blue") +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0),
                     labels = scales::percent) +
  scale_x_date(breaks = unique(vac$date),
               labels = date_format("%d %b %Y")) +
  labs(x = "", y = "Procentdel af respondere (%)") +
  theme(axis.title=element_text(size=20,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1,face="bold"),
        axis.text.y = element_text(face="bold"),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size=20))


tiff("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Release_vaccination/vac_trends_chronic_Jun2021_DK.tiff", units='in', width=10, height=7, res=300)
p
dev.off()

merged$q12 <- factor(merged$q12, levels = c("Kronisk sygdom", "Ingen kronisk sygdom"),
                     labels = c("Chronic disease", "No chronic disease"))


p <- ggplot(merged, aes(x=date)) +
  facet_grid(~q12) +
  geom_point(aes(y=yes_value), size=3, alpha=0.5, color = "black") +
  geom_line(aes(y=yes_value), size=2, alpha=0.5, color = "black") +
  #geom_errorbar(aes(ymin=yes_lCI, ymax=yes_uCI), width=.2, alpha=0.5, color = "black") +
  geom_point(aes(y=no_value), size=3, alpha=0.5, color = "red") +
  geom_line(aes(y=no_value), size=2, alpha=0.5, color = "red") +
  #geom_errorbar(aes(ymin=no_lCI, ymax=no_uCI), width=.2, alpha=0.5, color = "red") +
  geom_point(aes(y=notsure_value), size=3, alpha=0.5, color = "blue") +
  geom_line(aes(y=notsure_value), size=2, alpha=0.5, color = "blue") +
  #geom_errorbar(aes(ymin=notsure_lCI, ymax=notsure_uCI), width=.2, alpha=0.5, color = "blue") +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0),
                     labels = scales::percent) +
  scale_x_date(breaks = unique(vac$date),
               labels = date_format("%d %b %Y")) +
  labs(x = "", y = "Percentage of responders (%)") +
  theme(axis.title=element_text(size=20,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1,face="bold"),
        axis.text.y = element_text(face="bold"),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size=20))


tiff("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Release_vaccination/vac_trends_chronic_Jun2021_EN.tiff", units='in', width=10, height=7, res=300)
p
dev.off()




# AGE - DK
merged <- vac %>%
  group_by(date, agegroup) %>%
  summarise(yes_count = sum(wantvacc),
            yes_value = weighted.mean(wantvacc, wgt),
            yes_uCI = yes_value + 1.96*(sqrt((yes_value*(1-yes_value))/n())),
            yes_lCI = yes_value - 1.96*(sqrt((yes_value*(1-yes_value))/n())),
            no_count = sum(dontwantvacc),
            no_value = weighted.mean(dontwantvacc, wgt),
            no_uCI = no_value + 1.96*(sqrt((no_value*(1-no_value))/n())),
            no_lCI = no_value - 1.96*(sqrt((no_value*(1-no_value))/n())),
            notsure_count = sum(notsurewantvacc),
            notsure_value = weighted.mean(notsurewantvacc, wgt), 
            notsure_uCI = notsure_value + 1.96*(sqrt((notsure_value*(1-notsure_value))/n())),
            notsure_lCI = notsure_value - 1.96*(sqrt((notsure_value*(1-notsure_value))/n())),
            .groups = 'drop')


p <- ggplot(merged, aes(x=date)) +
  facet_grid(~agegroup) +
  geom_point(aes(y=yes_value), size=3, alpha=0.5, color = "black") +
  geom_line(aes(y=yes_value), size=2, alpha=0.5, color = "black") +
  #geom_errorbar(aes(ymin=yes_lCI, ymax=yes_uCI), width=.2, alpha=0.5, color = "black") +
  geom_point(aes(y=no_value), size=3, alpha=0.5, color = "red") +
  geom_line(aes(y=no_value), size=2, alpha=0.5, color = "red") +
  #geom_errorbar(aes(ymin=no_lCI, ymax=no_uCI), width=.2, alpha=0.5, color = "red") +
  geom_point(aes(y=notsure_value), size=3, alpha=0.5, color = "blue") +
  geom_line(aes(y=notsure_value), size=2, alpha=0.5, color = "blue") +
  #geom_errorbar(aes(ymin=notsure_lCI, ymax=notsure_uCI), width=.2, alpha=0.5, color = "blue") +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0),
                     labels = scales::percent) +
  scale_x_date(breaks = unique(vac$date),
               labels = date_format("%d %b %Y")) +
  labs(x = "", y = "Percentage of responders (%)") +
  theme(axis.title=element_text(size=20,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1,face="bold"),
        axis.text.y = element_text(face="bold"),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size=10))


tiff("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Release_vaccination/vac_trends_age_Jun2021_EN.tiff", units='in', width=10, height=7, res=300)
p
dev.off()


merged$agegroup <- factor(merged$agegroup, levels = c("18-29 years old", "30-44 years old",
                                            "45-64 years old", "65-90 years old"),
                     labels = c("18-29 år", "30-44 år",
                                "45-64 år", "65-90 år"))

p <- ggplot(merged, aes(x=date)) +
  facet_grid(~agegroup) +
  geom_point(aes(y=yes_value), size=3, alpha=0.5, color = "black") +
  geom_line(aes(y=yes_value), size=2, alpha=0.5, color = "black") +
  #geom_errorbar(aes(ymin=yes_lCI, ymax=yes_uCI), width=.2, alpha=0.5, color = "black") +
  geom_point(aes(y=no_value), size=3, alpha=0.5, color = "red") +
  geom_line(aes(y=no_value), size=2, alpha=0.5, color = "red") +
  #geom_errorbar(aes(ymin=no_lCI, ymax=no_uCI), width=.2, alpha=0.5, color = "red") +
  geom_point(aes(y=notsure_value), size=3, alpha=0.5, color = "blue") +
  geom_line(aes(y=notsure_value), size=2, alpha=0.5, color = "blue") +
  #geom_errorbar(aes(ymin=notsure_lCI, ymax=notsure_uCI), width=.2, alpha=0.5, color = "blue") +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0),
                     labels = scales::percent) +
  scale_x_date(breaks = unique(vac$date),
               labels = date_format("%d %b %Y")) +
  labs(x = "", y = "Procentdel af respondere (%)") +
  theme(axis.title=element_text(size=20,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1,face="bold"),
        axis.text.y = element_text(face="bold"),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size=10))


tiff("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Release_vaccination/vac_trends_age_Jun2021_DK.tiff", units='in', width=10, height=7, res=300)
p
dev.off()



















# TEMPORAL TRENDS IN SPECIFIC REASONS AGAINST
# THOSE WHO WOULD NOT WANT

# "I have already had COVID-19 and thus do not need to be vaccinated"
sum(respno$q28_1 == "Yes")/nrow(respno)
# "I am worried about side effects and thus do not want to be vaccinated"
sum(respno$q28_2 == "Yes")/nrow(respno)
# "I am not in a risk group and thus do not want to be vaccinated"
sum(respno$q28_3 == "Yes")/nrow(respno)
#  "I am generally opposed to vaccines"
sum(respno$q28_4 == "Yes")/nrow(respno)
# "I do not think the vaccine will have an effect"
sum(respno$q28_5 == "Yes")/nrow(respno)
# "Other reason"
sum(respno$q28_6 == "Yes")/nrow(respno)

onlycol <- as.data.frame(cbind(respno$q28_1,respno$q28_2,respno$q28_3,
                 respno$q28_4,respno$q28_5,respno$q28_6))

respno$q28_1_n <- 0
respno$q28_1_n[respno$q28_1 == "Yes"] <- 1
respno$q28_2_n <- 0
respno$q28_2_n[respno$q28_2 == "Yes"] <- 1
respno$q28_3_n <- 0
respno$q28_3_n[respno$q28_3 == "Yes"] <- 1
respno$q28_4_n <- 0
respno$q28_4_n[respno$q28_4 == "Yes"] <- 1
respno$q28_5_n <- 0
respno$q28_5_n[respno$q28_5 == "Yes"] <- 1
respno$q28_6_n <- 0
respno$q28_6_n[respno$q28_6 == "Yes"] <- 1

respno$totalyes <- respno$q28_1_n + respno$q28_2_n +
  respno$q28_3_n + respno$q28_4_n +
  respno$q28_5_n + respno$q28_5_n

table(respno$totalyes)


merged <- respno %>%
  group_by(date) %>%
  summarise(yes_count = n(),
            already_covid = sum(q28_1 == "Yes"),
            side_effects = sum(q28_2 == "Yes"),
            not_risk_group = sum(q28_3 == "Yes"),
            against_vaccines = sum(q28_4 == "Yes"),
            vaccine_ineffective = sum(q28_5 == "Yes"),
            other = sum(q28_6 == "Yes"),
            .groups = 'drop')


t(merged)

