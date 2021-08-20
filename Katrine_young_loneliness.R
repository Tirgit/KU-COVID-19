# loneliness trajectories in the young
d_pop$age <- as.numeric(d_pop$q2)

# loneliness
d_pop$q16_1_num <-recode(d_pop$q16_1_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_pop$q16_2_num <-recode(d_pop$q16_2_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_pop$q16_3_num <-recode(d_pop$q16_3_resp, 'Næsten aldrig eller aldrig'=1, 'Noget af tiden'=2, 'Ofte'=3)
d_pop$q16_sum <- d_pop$q16_1_num + d_pop$q16_2_num + d_pop$q16_3_num
d_pop$severe_lonely <- 0
d_pop$severe_lonely[d_pop$q16_sum >= 7] <- 1


# quality of life
d_pop$q18 <- as.numeric(substr(d_pop$q18,0,2))
# narrow down to <30 yrs
d_pop_y <- d_pop[d_pop$age < 34,]
d_pop_y$udate <- as.Date(d_pop_y$date)
table(d_pop_y$date)

# plot loneliness
res <- d_pop_y %>%
  group_by(udate) %>%
  summarise(n = n(),
            mean_lonely = mean(q16_sum),
            sd_lonely = sd(q16_sum),
            lci_lonely = mean_lonely - 1.96*(sd_lonely/sqrt(n)),
            uci_lonely = mean_lonely + 1.96*(sd_lonely/sqrt(n)))
res$sd_lonely <- NULL
res$n <- NULL

res$onlygenpop <- "2020 spring / summer"
res$onlygenpop[res$udate > "2020-09-01"] <- "from 2020 autumn"

p <- ggplot(data = res, aes(x = udate, y = mean_lonely)) +
  facet_grid(~onlygenpop, scale = "free", space = "free") +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=lci_lonely, ymax=uci_lonely), width=.2) +
  scale_x_date(breaks = unique(res$udate),
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
  ylab("Mean levels of loneliness") +
  scale_y_continuous(limits = c(3,9), expand = c(0, 0)) +
  #expand_limits(x = as.Date(c("2020-03-03", "2020-12-30"))) +
  ggtitle("Mean levels of loneliness in those younger than 34 yrs") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none")

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/Katrine_mean_loneliness_16APR2021.pdf", width = 9, height = 4.5)
p
dev.off()


# plot QoL
res <- d_pop_y %>%
  group_by(udate) %>%
  summarise(n = n(),
            mean_qol = mean(q18),
            sd_qol = sd(q18),
            lci_qol = mean_qol - 1.96*(sd_qol/sqrt(n)),
            uci_qol = mean_qol + 1.96*(sd_qol/sqrt(n)))
res$sd_qol <- NULL
res$n <- NULL

res$onlygenpop <- "2020 spring / summer"
res$onlygenpop[res$udate > "2020-09-01"] <- "from 2020 autumn"

p <- ggplot(data = res, aes(x = udate, y = mean_qol)) +
  facet_grid(~onlygenpop, scale = "free", space = "free") +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=lci_qol, ymax=uci_qol), width=.2) +
  scale_x_date(breaks = unique(res$udate),
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
  ylab("Mean levels of quality of life") +
  scale_y_continuous(limits = c(0,10), expand = c(0, 0)) +
  #expand_limits(x = as.Date(c("2020-03-03", "2020-12-30"))) +
  ggtitle("Mean levels of quality of life in those younger than 34 yrs") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none")

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/Katrine_mean_qol_16APR2021.pdf", width = 9, height = 4.5)
p
dev.off()


# plot proportion severe lonely
res <- d_pop_y %>%
  group_by(udate) %>%
  summarise(n = n(),
            percent_lon = 100*(sum(severe_lonely == 1)/n),
            percent_uCI = percent_lon + 1.96*(sqrt((percent_lon*(100-percent_lon))/n)),
            percent_lCI = percent_lon - 1.96*(sqrt((percent_lon*(100-percent_lon))/n)))
            
res$n <- NULL

res$onlygenpop <- "2020 spring / summer"
res$onlygenpop[res$udate > "2020-09-01"] <- "from 2020 autumn"
res$percent_lCI[res$percent_lCI < 0] <- 0

p <- ggplot(data = res, aes(y = percent_lon, x = udate)) +
  facet_grid(~onlygenpop, scale = "free", space = "free") +
  geom_bar(stat="identity", width = 2) +
  geom_errorbar(aes(ymin=percent_lCI, ymax=percent_uCI), width=.2) +
  scale_x_date(breaks = unique(res$udate),
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
  ylab("Percent with severe loneliness") +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  geom_hline(yintercept=0) +
  #expand_limits(x = as.Date(c("2020-03-03", "2020-12-30"))) +
  ggtitle("Percent with severe loneliness in those younger than 34 yrs") +
  #theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5)) +
  theme(legend.position = "none")

pdf("C:/Users/vrw657/Documents/Projects/Corona_SJPH/DK_plots/Katrine_severe_loneliness_16APR2021.pdf", width = 9, height = 4.5)
p
dev.off()

