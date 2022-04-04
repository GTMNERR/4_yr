# ---- 05 discharge ----
##trying to clean up this data output
## potential for discharge and rechanrge to be demarkated by lines???/



all_sites_discharge <- function(param, axis_title) {
  # param - use component_short parameter name in quotes
  # axis_title - use axis title value from 00_vis_custom.R, no quotes, or new title in quotes.
  
  #red line dam closure
  #dashed line recharge
  #solid line discharge
  #grey fill drawdown events
  
  
  p <- dat2 %>%
    dplyr::filter(component_short == param & end == "N") %>%
    ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
    geom_rect(aes(xmin = as.POSIXct("2018-03-28"),
                  xmax = as.POSIXct("2018-07-11"),
                  ymin = 0, ymax = Inf),
              fill = "grey69",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2018-08-08"),
                  xmax = as.POSIXct("2018-10-25"),
                  ymin = 0, ymax = Inf),
              fill = "grey69",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2019-02-01"),
                  xmax = as.POSIXct("2019-03-14"),
                  ymin = 0, ymax = Inf),
              fill = "grey87",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2020-02-17"),
                  xmax = as.POSIXct("2020-03-10"),
                  ymin = 0, ymax = Inf),
              fill = "grey87",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2021-02-15"),
                  xmax = as.POSIXct("2021-05-20"),
                  ymin = 0, ymax = Inf),
              fill = "grey87",
              color = NA) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    geom_vline(aes(xintercept = as.POSIXct("2018-03-28")),
               size = 1,
               colour = "red") +
    geom_vline(aes(xintercept = as.POSIXct("2018-10-25")),
               size = 1,
               colour = "red") +
    geom_vline(aes(xintercept = as.POSIXct("2017-10-05")),
               size = 1)+
    geom_vline(aes(xintercept = as.POSIXct("2018-07-26")),
               size = 1,
               linetype = "dashed") +
    geom_vline(aes(xintercept = as.POSIXct("2018-12-04")), 
               size = 1) +
    geom_vline(aes(xintercept = as.POSIXct("2019-02-01")),
               size = 1,
               linetype = "dashed")+
    geom_vline(aes(xintercept = as.POSIXct("2019-02-20")),
               size = 1)+
    geom_vline(aes(xintercept = as.POSIXct("2019-03-14")), 
               size = 1, 
               linetype = "dashed") +
    geom_vline(aes(xintercept = as.POSIXct("2019-07-09")),
               size = 1,
               linetype = "dashed")+
    geom_vline(aes(xintercept = as.POSIXct("2019-08-23")),
               size = 1)+
    geom_vline(aes(xintercept = as.POSIXct("2019-09-01")),
               size = 1,
               linetype = "dashed")+
    geom_vline(aes(xintercept = as.POSIXct("2020-02-17")),
               size = 1)+
    geom_vline(aes(xintercept = as.POSIXct("2020-03-10")),
               size = 1,
               linetype ="dashed")+
    geom_vline(aes(xintercept = as.POSIXct("2020-08-04")),
               size = 1,
               linetype = "dashed")+
    geom_vline(aes(xintercept = as.POSIXct("2020-08-12")),
               size = 1)+
    geom_vline(aes(xintercept = as.POSIXct("2020-11-12")),
               size = 1,
               linetype = "dashed")+
    geom_vline(aes(xintercept = as.POSIXct("2021-02-15")),
               size = 1,
               linetype = "dashed")+
    geom_vline(aes(xintercept = as.POSIXct("2021-03-01")),
               size = 1,
               linetype = "dashed")+
    geom_vline(aes(xintercept = as.POSIXct("2021-03-21")),
               size = 1,
               linetype = "dashed")+
    geom_vline(aes(xintercept = as.POSIXct("2021-05-26")),
               size = 1,
               linetype = "dashed")+
    geom_vline(aes(xintercept = as.POSIXct("2021-06-02")),
               size = 1)+
    scale_colour_manual(name = "Site", values = sitecolours) +
    cowplot::theme_cowplot() +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
    theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
    labs(y = axis_title,
         x = "",
         title = paste(param))
  
  p
}

all_sites_discharge("CHLA_C", chla_y_title)# corrected chlorophyll plot

all_sites_discharge("SALT", "Salinity (psu)")
all_sites_discharge("WDEPTH", "Depth (m)")



### JUST 2018 DATA LOOKING FOR ANY CHANGE DUE TO DAM CLOSURE

dat3 <- dat2 %>%
  dplyr::mutate(year = year(date_sampled)) %>%
  dplyr::filter(year == "2018")

all_sites_discharge <- function(param, axis_title) {
  # param - use component_short parameter name in quotes
  # axis_title - use axis title value from 00_vis_custom.R, no quotes, or new title in quotes.
  
  p <- dat3 %>%
    dplyr::filter(component_short == param & end == "N") %>%
    ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
    geom_rect(aes(xmin = as.POSIXct("2018-03-28"),
                  xmax = as.POSIXct("2018-07-11"),
                  ymin = 0, ymax = Inf),
              fill = "grey69",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2018-08-08"),
                  xmax = as.POSIXct("2018-10-25"),
                  ymin = 0, ymax = Inf),
              fill = "grey69",
              color = NA) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    scale_colour_manual(name = "Site", values = sitecolours) +
    cowplot::theme_cowplot() +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
    theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
    labs(y = axis_title,
         x = "",
         title = paste(param))
  
  p
}

all_sites_discharge("CHLA_C", chla_y_title)
all_sites_discharge("TN", nitro_y_title)
