# ---- 05 discharge ----

all_sites_discharge <- function(param, axis_title) {
  # param - use component_short parameter name in quotes
  # axis_title - use axis title value from 00_vis_custom.R, no quotes, or new title in quotes.
  
  #dark grey discharge light grey recharge
  # red lines are gates solid
  #solid lines are drawdown 
  #dashed lines are damn closure 
  p <- dat2 %>%
    dplyr::filter(component_short == param & end == "N") %>%
    ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
    geom_rect(aes(xmin = as.POSIXct("2017-10-05"),
                  xmax = as.POSIXct("2018-07-26"),
                  ymin = 0, ymax = Inf),
              fill = "grey69",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2018-07-26"),
                  xmax = as.POSIXct("2018-12-04"),
                  ymin = 0, ymax = Inf),
              fill = "grey87",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2018-12-04"),
                  xmax = as.POSIXct("2019-02-01"),
                  ymin = 0, ymax = Inf),
              fill = "grey69",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2019-02-01"),
                  xmax = as.POSIXct("2019-02-20"),
                  ymin = 0, ymax = Inf),
              fill = "grey87",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2019-02-20"),
                  xmax = as.POSIXct("2019-03-14"),
                  ymin = 0, ymax = Inf),
              fill = "grey69",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2019-03-14"),
                  xmax = as.POSIXct("2019-08-23"),
                  ymin = 0, ymax = Inf),
              fill = "grey87",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2019-08-23"),
                  xmax = as.POSIXct("2019-09-01"),
                  ymin = 0, ymax = Inf),
              fill = "grey69",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2019-09-01"),
                  xmax = as.POSIXct("2020-02-17"),
                  ymin = 0, ymax = Inf),
              fill = "grey87",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2020-02-17"),
                  xmax = as.POSIXct("2020-03-10"),
                  ymin = 0, ymax = Inf),
              fill = "grey69",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2020-03-10"),
                  xmax = as.POSIXct("2020-08-12"),
                  ymin = 0, ymax = Inf),
              fill = "grey87",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2020-08-12"),
                  xmax = as.POSIXct("2020-11-12"),
                  ymin = 0, ymax = Inf),
              fill = "grey69",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2020-11-12"),
                  xmax = as.POSIXct("2021-06-02"),
                  ymin = 0, ymax = Inf),
              fill = "grey87",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2021-06-02"),
                  xmax = as.POSIXct("2021-11-01"),
                  ymin = 0, ymax = Inf),
              fill = "grey69",
              color = NA) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    geom_vline(aes(xintercept = as.POSIXct("2018-03-28")),
               size = 1,
               linetype = "dashed") +
    geom_vline(aes(xintercept = as.POSIXct("2018-10-25")),
               size = 1,
               linetype = "dashed") +
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

all_sites_discharge("CHLA_C", chla_y_title) # corrected chlorophyll plot
all_sites_discharge("SALT", "Salinity (psu)")