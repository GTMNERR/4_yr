# need to run this code first
# source('R/01_load_wrangle.R')
library(readxl)
library(janitor)
library(here)
library(tidyverse)
library(lubridate)
library(broom)
library(knitr)
library(rmarkdown)
library(cowplot)
library(gridExtra)
library(patchwork)
library(scales)
library(plotly)
library(ggcorrplot)
library(kableExtra) 

# ----01 LOAD read in tolomato met file ----

met_dat <- read_xlsx(here::here("data", "TOLO", "tolomatoJuly17_Dec20.xlsx")) %>%
  janitor::clean_names()

# inspect the data file
head(met_dat)
str(met_dat)

# ----02 TIDY tolomato met data tidying, part 1----

# set 'rainfall_mm' column to numerical value, this column contains all the numerical values from the analyses
cols.num <- c("battery_v", "water_temp_c", "water_conductivity_m_s_cm",
              "water_salinity_ppt", "air_temp_c", "relative_humidity_percent",
              "barometric_pressure_in_hg", "rainfall_mm", "navd88_water_level_m")
met_dat[cols.num] <- sapply(met_dat[cols.num],as.numeric)
sapply(met_dat, class)

# pull out date information
met_dat$month <- month(met_dat$date_time_est)
met_dat$day <- day(met_dat$date_time_est)
met_dat$year <- as.character(year(met_dat$date_time_est))
rm(cols.num)

# ----03 WRANGLE subset to daily meteorological parameters----

met_dat_daily <- met_dat %>%
  group_by(year, month, day) %>%
  summarise(dailyrain = sum(rainfall_mm, na.rm = TRUE),
            daily_avg_wtemp = mean(water_temp_c, na.rm = TRUE),
            daily_avg_sal = mean(water_salinity_ppt, na.rm = TRUE),
            daily_avg_atemp = mean(air_temp_c, na.rm = TRUE)) %>%
  ungroup()
met_dat_daily$datetime <- paste(met_dat_daily$year, met_dat_daily$month,
                                met_dat_daily$day, sep="-") %>%
  ymd() %>%
  as.Date()

# ----04 WRANGLE subset to monthly meteorological parameters----

met_dat_monthly <- met_dat %>%
  group_by(year, month) %>%
  summarise(monthlyrain = sum(rainfall_mm, na.rm = TRUE),
            monthly_avg_wtemp = mean(water_temp_c, na.rm = TRUE),
            monthly_avg_sal = mean(water_salinity_ppt, na.rm = TRUE),
            monthly_avg_atemp = mean(air_temp_c, na.rm = TRUE)) %>%
  ungroup()
met_dat_monthly$day <- 1
met_dat_monthly$datetime <- paste(met_dat_monthly$year, met_dat_monthly$month,
                                  met_dat_monthly$day, sep="-") %>%
  ymd() %>%
  as.Date()




## read in rainfall data from GTM collection to merge

gtm_rain <- read_xlsx(here::here("data", "TOLO", "gtmrainfall.xlsx")) %>%
  janitor::clean_names()


### Creating a column with the month abb name not just numbers 
gtm_rain <- gtm_rain%>%
  mutate(month1 = (month.abb[month]))

met_dat_monthly <- met_dat_monthly%>%
  mutate(month1 = (month.abb[month]))


# adding year to the station_code for both rainfall sets 

gtm_rain$uniq <- paste(gtm_rain$month1, gtm_rain$year, sep = "_")

met_dat_monthly$uniq <- paste(met_dat_monthly$month1, met_dat_monthly$year, sep = "_")


# converting met data year to NUMERIC 
met_dat_monthly$year <- as.numeric(met_dat_monthly$year)

#removing 2020 from TOLO data
met_dat_new <- met_dat_daily %>% dplyr::filter(year != "2020")

### joining two datasets by uniq column 
guana_rain <- dplyr::full_join(met_dat_monthly, gtm_rain)


## converting mm of rain to inches for graphing
total_rain <- guana_rain %>%
  mutate(rain_in = monthlyrain/25.4,
         ID = row_number())


# --------- 05 graph by monthly rainfall ---------------

x <- total_rain %>%
  ggplot(aes(x = datetime, y = rain_in)) +
    geom_col()+
  theme_classic() +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text = element_text(color = 'black')) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1.0, color='black')) +
  labs(y = "Monthly Rainfall (in)",
       x = "")


ggsave(plot = x, filename = here("output", "Rain_bar.png"), dpi = 120)


##using only GTM rainfall data

y<-gtm_rain %>%
  ggplot(aes(x = datetime, y = monthlyrain_in)) +
  geom_col()+
  theme_classic() +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text = element_text(color = 'black')) +
  theme(axis.text.x = element_text( color='black')) +
  labs(y = "Monthly Rainfall (in)",
       x = "")

ggsave(plot = y, filename = here("output", "GTMRain_bar.png"), dpi = 120)

  






