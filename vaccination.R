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
res_lonely <- read_excel("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Vaccination_release/vacc_strat.xlsx")
res_lonely$Category <- factor(res_lonely$Category, levels=c("All", "Gender", "Age", 
                                                            "Education"))
res_lonely <- cbind(res_lonely, correctorder = c(0,0,1,0,1,2,3,0,1,2,
                                                 0,0,1,0,1,2,3,0,1,2))


no_res <- res_lonely[res_lonely$response == "No",]
notsure_res <- res_lonely[res_lonely$response != "No",]

# NO plot
p <- ggplot(data = no_res, aes(x = reorder(Strata, correctorder), y = percent, fill = barcolor)) +
  facet_grid(~Category, scale = "free", space = "free") +
  geom_bar(stat="identity", width = 0.7) +
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
q <- ggplot(data = notsure_res, aes(x = reorder(Strata, correctorder), y = percent, fill = barcolor)) +
  facet_grid(~Category, scale = "free", space = "free") +
  geom_bar(stat="identity", width = 0.7) +
  ylab("I am not sure if I would get vaccinated (%)") +
  theme(axis.title=element_text(size=8,face="bold"),
        axis.text.x = element_blank(),
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




pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Vaccination_release/no_plot.pdf", width = 9, height = 5.5)
p
dev.off()

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/Vaccination_release/notsure_plot.pdf", width = 9, height = 6.7)
q
dev.off()


