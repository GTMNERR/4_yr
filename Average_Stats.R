install.packages()
## Select readxl, janitor, here, tidyverse, lubridate, broom, knitr, rmarkdown, cowplot, gridExtra, 
## patchwork, scales, plotly, ggcorrplot
## Dir refers to the directory name you'd like to work from
## setwd(dir)

## setwd('C:/Users/roorbach_o/Documents/R/win-library/4.0')
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

## ???
here::here('data')

## :: means look in this package to use this function, 
## %>%  pass the left hand side of the operator to the first argument 
## of the right hand side of the operator

##WORKS
dat <- readxl::read_xlsx(here::here('data', 'Guana_masterdata_2021.09.13.xlsx'), 
                         sheet = 'Sheet1') %>% 
  janitor::clean_names()

## change column name to work with previously written code
dat <- rename(dat, date_sampled = sample_date)

# data dictionary with site-specific information
dict <- readr::read_csv(here::here('data', 'guana_data_dictionary.csv')) %>%
  janitor::clean_names()


# inspect the data file
head(dat)
str(dat)
dplyr::glimpse(dat) # this one is my favorite to use

## remove dup samples
## cleaning up data, selecting columns we want 
# removing all but wind and secchi, all component toupper (not sure why)
dat2 <- dat %>%
  dplyr::filter(station_code != "GTMOLNUT_dup") %>%  # remove the 'duplicate' station that was only sampled for a short while
  dplyr::select(unit,
                station_code,
                component_short,
                result) %>%
  dplyr::mutate( component_short = toupper(component_short))


## Trying to get single site columns to run Average 
dat3 <- dat %>%
  dplyr::select(unit,
                station_code,
                component_short,
                result) %>%
  dplyr::filter(station_code == "GTMMKNUT") %>% #select only MK
  dplyr::filter ((component_short %in% c("SALT", "TN"))) 



result_num <- as.numeric(dat3$result, 
                         dat3$component_short)

rowSums(result_num)
rowMeans(dat3)
