#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(zoo)
library(mgcv)
library(stringr)
library(cowplot)
library(readr)
library(gridExtra)

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
       title = "Workplaces") +
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

##now look at oxford stringency index
oxford <- read_csv(file.path(data_path, "OxCGRT_GBR_latest.csv")) %>%
  mutate(
    Date = ymd(Date),
  ) %>%
  select(Date, StringencyIndex_Average)
oxford <- as.data.table(oxford)

oxford <- oxford[Date <= ymd("2021-03-31")]

#get middate for fornight periods 
oxford[, fortnight := paste(isoyear(Date), "/",
                            sprintf("%02d", ceiling(isoweek(Date)/2)))]
oxford[, start_date := min(Date), by = .(fortnight)]
oxford[, end_date := max(Date), by = .(fortnight)]
oxford[, mid_date := start_date + floor((end_date - start_date)/2),
       by = .(fortnight)]

oxford <- oxford[, .(stringency_middate = mean(StringencyIndex_Average,
                                               na.rm = T), Date),
                 keyby = mid_date]

oxford_l1 <- oxford %>%
  filter(Date <= ymd("2020-11-04")) %>%
  mutate(days = as.numeric(difftime(mid_date, ymd("2020-03-23"), units = "days")),
         lockdown = "1") %>%
  select(-mid_date, -Date)

oxford_l2 <- oxford %>%
  filter(Date %in% seq(ymd("2020-06-01"), ymd("2021-01-05"), by = "days")) %>%
  mutate(days = as.numeric(difftime(mid_date, ymd("2020-11-05"), units = "days")),
         lockdown = "2") %>%
  select(-mid_date, -Date)

oxford_l3 <- oxford %>%
  filter(Date %in% seq(ymd("2020-12-02"), ymd("2022-02-21"), by = "days")) %>%
  mutate(days = as.numeric(difftime(mid_date, ymd("2021-01-06"), units = "days")),
         lockdown = "3") %>%
  select(-mid_date, -Date)

oxford_merge <- rbind(oxford_l1, oxford_l2, oxford_l3, fill = T)
oxford_wide <- melt(oxford_merge, id.vars = c("lockdown", "days"))

oxford_wide <- oxford_wide %>%
  filter(!is.na(value)) %>%
  mutate(
    value_percent = value/100,
    value_percent_invert = 1-value_percent
  )

oxford_plot <- ggplot(oxford_wide) + 
  geom_line(aes(x = days, y = value_percent_invert, colour = lockdown)) +
  geom_vline(xintercept = 0, linetype = 2) + 
  labs(x = "Days from lockdown", y = "Inverted Index (%)", 
       title = "Oxford Stringency Index") +
  scale_colour_manual(values = c("#e17e00", "#002461", "#ff81ab"),
                      name = "Lockdown", labels = c("March 2020",
                      "November 2020", "January 2021"))

(layout_matrix <- matrix(c(4, 3, 3, 4, 1, 1, 2, 2), nrow = 2, byrow = TRUE))
grid.arrange(workplaces, predictor, oxford_plot, layout_matrix = layout_matrix)
