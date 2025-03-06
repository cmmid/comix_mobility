##circuit breaker figure practice 

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(zoo)
library(mgcv)
library(stringr)
library(cowplot)

#set cowplot theme
theme_set(cowplot::theme_cowplot(font_size = 10) + theme(strip.background = element_blank()))

#set data path
data_path <- paste0(here::here(), "/Data")

#load mobility data 
gm <- qs::qread(file.path(data_path, "google_mob_region.qs"))
gm2 <- gm[!duplicated(gm[, .(date, residential_percent_change_from_baseline,
                             workplaces_percent_change_from_baseline,
                             transit_stations_percent_change_from_baseline,
                             grocery_and_pharmacy_percent_change_from_baseline,
                             parks_percent_change_from_baseline,
                             retail_and_recreation_percent_change_from_baseline)],
                      fromLast = T)]
gm2 <- gm2[date <= ymd("2021-03-31")]

#turn mobility data into decimals instead of percentages
gm2[, retail_recreation := (100 + retail_and_recreation_percent_change_from_baseline) * 0.01]
gm2[, grocery_pharmacy  := (100 + grocery_and_pharmacy_percent_change_from_baseline ) * 0.01]
gm2[, parks             := (100 + parks_percent_change_from_baseline) * 0.01]
gm2[, transit_stations  := (100 + transit_stations_percent_change_from_baseline) * 0.01]
gm2[, workplaces        := (100 + workplaces_percent_change_from_baseline) * 0.01]
gm2[, residential       := (100 + residential_percent_change_from_baseline) * 0.01]

#get middate for fornight periods 
gm2[, fortnight := paste(isoyear(date), "/",
                         sprintf("%02d", ceiling(isoweek(date)/2)))]
gm2[, start_date := min(date), by = .(fortnight)]
gm2[, end_date := max(date), by = .(fortnight)]
gm2[, mid_date := start_date + floor((end_date - start_date)/2) ,
    by = .(fortnight)]

#get average
gm2 <- gm2[, .(retail_recreation_mean = mean(retail_recreation, na.rm = T),
               grocery_pharmacy_mean = mean(grocery_pharmacy, na.rm = T),
               parks_mean = mean(parks, na.rm = T),
               transit_stations_mean = mean(transit_stations, na.rm = T),
               workplaces_mean = mean(workplaces, na.rm = T),
               residential_mean = mean(residential, na.rm = T), date),
           keyby = mid_date]
gm2 <- gm2 %>%
  select(mid_date, retail_recreation_mean, grocery_pharmacy_mean, parks_mean,
         transit_stations_mean, workplaces_mean, residential_mean, date) %>%
  mutate(
    #create predictor for 'other' contacts
    predictor = retail_recreation_mean * 0.333 +
      transit_stations_mean * 0.334 +
      grocery_pharmacy_mean * 0.333
  )

gm_l1 <- gm2 %>%
  filter(date <= ymd("2020-11-04")) %>%
  mutate(days = as.numeric(difftime(mid_date, ymd("2020-03-23"), units = "days")),
         lockdown = "1") %>%
  select(-mid_date, -date)
gm_l2 <- gm2 %>%
  filter(date %in% seq(ymd("2020-06-01"), ymd("2021-01-05"), by = "days")) %>%
  mutate(days = as.numeric(difftime(mid_date, ymd("2020-11-05"), units = "days")),
         lockdown = "2") %>%
  select(-mid_date, -date)
gm_l3 <- gm2 %>%
  filter(date %in% seq(ymd("2020-12-02"), ymd("2022-02-21"), by = "days")) %>%
  mutate(days = as.numeric(difftime(mid_date, ymd("2021-01-06"), units = "days")),
         lockdown = "3") %>%
  select(-mid_date, -date)

#combine
gm_merge <- rbind(gm_l1, gm_l2, gm_l3, fill = T)
gm_wide <- melt(gm_merge, id.vars = c("lockdown", "days"))

#grocery and pharmacy mean
gm_grocery <- gm_wide[variable == "grocery_pharmacy_mean"]
grocery <- ggplot(gm_grocery) + 
  geom_line(aes(x = days, y = value, colour = lockdown)) +
  geom_vline(xintercept = 0, linetype = 2) + 
  labs(x = "Days from lockdown", y = "Google Mobility Index", 
       title = "Grocery and Pharmacy") +
  scale_colour_manual(values = c("#e17e00", "#002461", "#ff81ab"),
                      name = "Lockdown", labels = c("March 2020",
                      "November 2020", "January 2021"))

#residential mean 
gm_residential <- gm_wide[variable == "residential_mean"]
residential <- ggplot(gm_residential) + 
  geom_line(aes(x = days, y = value, colour = lockdown)) +
  geom_vline(xintercept = 0, linetype = 2) + 
  labs(x = "Days from lockdown", y = "Google Mobility Index", 
       title = "Residential") +
  scale_colour_manual(values = c("#e17e00", "#002461", "#ff81ab"),
                      name = "Lockdown", labels = c("March 2020",
                      "November 2020", "January 2021"))

#retail and recreation mean
gm_retail <- gm_wide[variable == "retail_recreation_mean"]
retail <- ggplot(gm_retail) + 
  geom_line(aes(x = days, y = value, colour = lockdown)) +
  geom_vline(xintercept = 0, linetype = 2) + 
  labs(x = "Days from lockdown", y = "Google Mobility Index", 
       title = "Retail and Recreation") +
  scale_colour_manual(values = c("#e17e00", "#002461", "#ff81ab"),
                      name = "Lockdown", labels = c("March 2020",
                      "November 2020", "January 2021"))

#transit stations mean
gm_transit <- gm_wide[variable == "transit_stations_mean"]
transit <- ggplot(gm_transit) + 
  geom_line(aes(x = days, y = value, colour = lockdown)) +
  geom_vline(xintercept = 0, linetype = 2) + 
  labs(x = "Days from lockdown", y = "Google Mobility Index", 
       title = "Transit Stations") +
  scale_colour_manual(values = c("#e17e00", "#002461", "#ff81ab"),
                      name = "Lockdown", labels = c("March 2020",
                      "November 2020", "January 2021"))

#workplaces mean
gm_workplaces <- gm_wide[variable == "workplaces_mean"]
workplaces <- ggplot(gm_workplaces) + 
  geom_line(aes(x = days, y = value, colour = lockdown)) +
  geom_vline(xintercept = 0, linetype = 2) + 
  labs(x = "Days from lockdown", y = "Google Mobility Index", 
       title = "Workplaces") + ylim(0, 1) +
  scale_colour_manual(values = c("#e17e00", "#002461", "#ff81ab"),
                      name = "Lockdown", labels = c("March 2020",
                      "November 2020", "January 2021"))

#predictor mean
gm_predictor <- gm_wide[variable == "predictor"]
predictor <- ggplot(gm_predictor) + 
  geom_line(aes(x = days, y = value, colour = lockdown)) +
  geom_vline(xintercept = 0, linetype = 2) + 
  labs(x = "Days from lockdown", y = "Google Mobility Index", 
       title = "'Other' Predictor") +
  scale_colour_manual(values = c("#e17e00", "#002461", "#ff81ab"),
                      name = "Lockdown", labels = c("March 2020",
                      "November 2020", "January 2021"))

#plot together
plot_grid(residential, grocery, retail, transit, workplaces, predictor)

##now look at contacts

#import contact data
cnts <- qs::qread(file.path(data_path, "cnts_weight_work_middate.qs"))

#filter out participants of a certain age
cnts <- cnts[sample_type == "adult"]
cnts <- cnts %>%
  filter(!is.na(part_age_group))

#order by date
cnts_date <- cnts[order(date)]
cnts_date <- cnts_date[date <= ymd("2021-03-31")]

#create data table with subset of variables
num <- cnts_date[, .(date, part_id, panel, part_age, survey_round, weekday, 
                     home = n_cnt_home, work = n_cnt_work, other = n_cnt_other, 
                     all = n_cnt_home + n_cnt_work + n_cnt_other, day_weight, 
                     social_weight = weight_raw)]
num[, t := as.numeric(date - ymd("2020-01-01"))]

#create study column
num[, study := "CoMix"]

#create sequence of dates
date <- seq(as.Date("2020-03-02"), as.Date("2022-03-02"), by = "days")
lockdowns <- as.data.table(as.Date(date))
lockdowns$lockdown_status <- 0
colnames(lockdowns) <- c("date", "status")

#create time intervals for different types of restrictions
T1 <- interval(ymd("2020-03-02"), ymd("2020-03-22"))
L1 <- interval(ymd("2020-03-23"), ymd("2020-05-31"))
T2 <- interval(ymd("2020-06-01"), ymd("2020-07-04"))
F1 <- interval(ymd("2020-07-05"), ymd("2020-09-13"))
T3 <- interval(ymd("2020-09-14"), ymd("2020-11-04"))
L2 <- interval(ymd("2020-11-05"), ymd("2020-12-01"))
T4 <- interval(ymd("2020-12-02"), ymd("2021-01-05"))
L3 <- interval(ymd("2021-01-06"), ymd("2021-03-07"))
T5 <- interval(ymd("2021-03-08"), ymd("2021-07-18"))
F2 <- interval(ymd("2021-07-19"), ymd("2021-12-07"))
T6 <- interval(ymd("2021-12-08"), ymd("2022-02-21"))

#assign value to each type of restriction
lockdowns$status <- ifelse(ymd(lockdowns$date) %within% T1, 1, 
                    ifelse(ymd(lockdowns$date) %within% L1, 2, 
                    ifelse(ymd(lockdowns$date) %within% T2, 1, 
                    ifelse(ymd(lockdowns$date) %within% T3, 1, 
                    ifelse(ymd(lockdowns$date) %within% L2, 2, 
                    ifelse(ymd(lockdowns$date) %within% T4, 1, 
                    ifelse(ymd(lockdowns$date) %within% L3, 2, 
                    ifelse(ymd(lockdowns$date) %within% T5, 1,
                    ifelse(ymd(lockdowns$date) %within% T6, 1, 0)))))))))

#create factor
lockdown_fac <- factor(lockdowns$status, levels = c(0, 1, 2, 3),
                       labels = c("No restrictions", "Some restrictions",
                                  "Lockdown", "Pre-Pandemic"))
lockdowns$status <- lockdown_fac

#merge contact data and lockdown information
cnts_l <- merge(num, lockdowns, by = "date", all.y = F)

#create second database which shifts the survey rounds and dates
num2 <- rlang::duplicate(cnts_l)
num2[, date := date + 7]
num2[, survey_round := survey_round + 1]

#merge the two 
num_merge <- rbind(cnts_l, num2) 

#calculate non home contacts
num_merge[, nonhome := all - home]

#add column for special dates 
summer <- interval(ymd("2020-08-03"), ymd("2020-08-09"))
num_merge[, special := ifelse(date == ymd("2020-12-25"), "Xmas",
                       ifelse(date == ymd("2021-12-25"), "Xmas",
                       ifelse(date == ymd("2020-12-31"), "NYE",
                       ifelse(date == ymd("2021-12-31"), "NYE",
                       ifelse(date == ymd("2020-04-13"), "Easter",
                       ifelse(date == ymd("2021-04-05"), "Easter", 
                       ifelse(date %within% summer, "Summer Hol", NA)))))))]

#get middate for fornight periods 
num_merge[, fortnight := paste(isoyear(date), "/", sprintf("%02d", ceiling(isoweek(date)/2)))]
num_merge[, start_date := min(date), by = .(fortnight)]
num_merge[, end_date := max(date), by = .(fortnight)]
num_merge[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(fortnight)]

#get weighted means by fortnight
weighted_means <- num_merge[, .(date, study, status, special,
                                work = weighted.mean(work, day_weight*social_weight),
                                other = weighted.mean(other, day_weight*social_weight),
                                nonhome = weighted.mean(nonhome, day_weight*social_weight)),
                  keyby = mid_date]

work_l1 <- weighted_means %>%
  select(date, mid_date, work) %>%
  filter(date <= ymd("2020-11-04")) %>%
  mutate(days = as.numeric(difftime(mid_date, ymd("2020-03-23"), units = "days")),
         lockdown = "1") %>%
  select(-mid_date, -date)
work_l2 <- weighted_means %>%
  select(date, mid_date, work) %>%
  filter(date %in% seq(ymd("2020-06-01"), ymd("2021-01-05"), by = "days")) %>%
  mutate(days = as.numeric(difftime(mid_date, ymd("2020-11-05"), units = "days")),
         lockdown = "2") %>%
  select(-mid_date, -date)
work_l3 <- weighted_means %>%
  select(date, mid_date, work) %>%
  filter(date %in% seq(ymd("2020-12-02"), ymd("2022-02-21"), by = "days")) %>%
  mutate(days = as.numeric(difftime(mid_date, ymd("2021-01-06"), units = "days")),
         lockdown = "3") %>%
  select(-mid_date, -date)
  
#combine
work_merge <- rbind(work_l1, work_l2, work_l3, fill = T)
work_wide <- melt(work_merge, id.vars = c("lockdown", "days"))

#workplaces mean
work <- ggplot(work_wide) + 
  geom_line(aes(x = days, y = value, colour = lockdown)) +
  geom_vline(xintercept = 0, linetype = 2) + 
  labs(x = "Days from lockdown", y = "CoMix Contacts", 
       title = "Work") + ylim(0, 1.2) +
  scale_colour_manual(values = c("#e17e00", "#002461", "#ff81ab"),
                      name = "Lockdown", labels = c("March 2020",
                      "November 2020", "January 2021"))


#import contact data
cnts <- qs::qread(file.path(data_path, "cnts_weight_other_middate.qs"))

#filter out participants of a certain age
cnts <- cnts[sample_type == "adult"]
cnts <- cnts %>%
  filter(!is.na(part_age_group))

#order by date
cnts_date <- cnts[order(date)]
cnts_date <- cnts_date[date <= ymd("2021-03-31")]

#create data table with subset of variables
num <- cnts_date[, .(date, part_id, panel, part_age, survey_round, weekday, 
                     home = n_cnt_home, work = n_cnt_work, other = n_cnt_other, 
                     all = n_cnt_home + n_cnt_work + n_cnt_other, day_weight, 
                     social_weight = weight_raw)]
num[, t := as.numeric(date - ymd("2020-01-01"))]

#create study column
num[, study := "CoMix"]

#create sequence of dates
date <- seq(as.Date("2020-03-02"), as.Date("2022-03-02"), by = "days")
lockdowns <- as.data.table(as.Date(date))
lockdowns$lockdown_status <- 0
colnames(lockdowns) <- c("date", "status")

#create time intervals for different types of restrictions
T1 <- interval(ymd("2020-03-02"), ymd("2020-03-22"))
L1 <- interval(ymd("2020-03-23"), ymd("2020-05-31"))
T2 <- interval(ymd("2020-06-01"), ymd("2020-07-04"))
F1 <- interval(ymd("2020-07-05"), ymd("2020-09-13"))
T3 <- interval(ymd("2020-09-14"), ymd("2020-11-04"))
L2 <- interval(ymd("2020-11-05"), ymd("2020-12-01"))
T4 <- interval(ymd("2020-12-02"), ymd("2021-01-05"))
L3 <- interval(ymd("2021-01-06"), ymd("2021-03-07"))
T5 <- interval(ymd("2021-03-08"), ymd("2021-07-18"))
F2 <- interval(ymd("2021-07-19"), ymd("2021-12-07"))
T6 <- interval(ymd("2021-12-08"), ymd("2022-02-21"))

#assign value to each type of restriction
lockdowns$status <- ifelse(ymd(lockdowns$date) %within% T1, 1, 
                    ifelse(ymd(lockdowns$date) %within% L1, 2, 
                    ifelse(ymd(lockdowns$date) %within% T2, 1, 
                    ifelse(ymd(lockdowns$date) %within% T3, 1, 
                    ifelse(ymd(lockdowns$date) %within% L2, 2, 
                    ifelse(ymd(lockdowns$date) %within% T4, 1, 
                    ifelse(ymd(lockdowns$date) %within% L3, 2, 
                    ifelse(ymd(lockdowns$date) %within% T5, 1,
                    ifelse(ymd(lockdowns$date) %within% T6, 1, 0)))))))))

#create factor
lockdown_fac <- factor(lockdowns$status, levels = c(0, 1, 2, 3),
                       labels = c("No restrictions", "Some restrictions",
                                  "Lockdown", "Pre-Pandemic"))
lockdowns$status <- lockdown_fac

#merge contact data and lockdown information
cnts_l <- merge(num, lockdowns, by = "date", all.y = F)

#create second database which shifts the survey rounds and dates
num2 <- rlang::duplicate(cnts_l)
num2[, date := date + 7]
num2[, survey_round := survey_round + 1]

#merge the two 
num_merge <- rbind(cnts_l, num2) 

#calculate non home contacts
num_merge[, nonhome := all - home]

#add column for special dates 
summer <- interval(ymd("2020-08-03"), ymd("2020-08-09"))
num_merge[, special := ifelse(date == ymd("2020-12-25"), "Xmas",
                       ifelse(date == ymd("2021-12-25"), "Xmas",
                       ifelse(date == ymd("2020-12-31"), "NYE",
                       ifelse(date == ymd("2021-12-31"), "NYE",
                       ifelse(date == ymd("2020-04-13"), "Easter",
                       ifelse(date == ymd("2021-04-05"), "Easter", 
                       ifelse(date %within% summer, "Summer Hol", NA)))))))]

#get middate for fornight periods 
num_merge[, fortnight := paste(isoyear(date), "/", sprintf("%02d", ceiling(isoweek(date)/2)))]
num_merge[, start_date := min(date), by = .(fortnight)]
num_merge[, end_date := max(date), by = .(fortnight)]
num_merge[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(fortnight)]

#get weighted means by fortnight
weighted_means <- num_merge[, .(date, study, status, special,
                                work = weighted.mean(work, day_weight*social_weight),
                                other = weighted.mean(other, day_weight*social_weight),
                                nonhome = weighted.mean(nonhome, day_weight*social_weight)),
                  keyby = mid_date]

other_l1 <- weighted_means %>%
  select(date, mid_date, other) %>%
  filter(date <= ymd("2020-11-04")) %>%
  mutate(days = as.numeric(difftime(mid_date, ymd("2020-03-23"), units = "days")),
         lockdown = "1") %>%
  select(-mid_date, -date)
other_l2 <- weighted_means %>%
  select(date, mid_date, other) %>%
  filter(date %in% seq(ymd("2020-06-01"), ymd("2021-01-05"), by = "days")) %>%
  mutate(days = as.numeric(difftime(mid_date, ymd("2020-11-05"), units = "days")),
         lockdown = "2") %>%
  select(-mid_date, -date)
other_l3 <- weighted_means %>%
  select(date, mid_date, other) %>%
  filter(date %in% seq(ymd("2020-12-02"), ymd("2022-02-21"), by = "days")) %>%
  mutate(days = as.numeric(difftime(mid_date, ymd("2021-01-06"), units = "days")),
         lockdown = "3") %>%
  select(-mid_date, -date)
  
#combine
other_merge <- rbind(other_l1, other_l2, other_l3, fill = T)
other_wide <- melt(other_merge, id.vars = c("lockdown", "days"))

#workplaces mean
other <- ggplot(other_wide) + 
  geom_line(aes(x = days, y = value, colour = lockdown)) +
  geom_vline(xintercept = 0, linetype = 2) + 
  labs(x = "Days from lockdown", y = "CoMix Contacts", 
       title = "Other") + ylim(0, 1.2) +
  scale_colour_manual(values = c("#e17e00", "#002461", "#ff81ab"),
                      name = "Lockdown", labels = c("March 2020",
                      "November 2020", "January 2021"))

#plot together
plot_grid(work, other, workplaces + ylim(0, 1.2),
          predictor + ylim(0, 1.2), nrow = 2)
