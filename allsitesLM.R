# ----01 timeseries-all sites function -----------------------------------
# had to fix date so R would read it correctly 
dat2 <- dat2 %>% 
  mutate(DATE = lubridate::as_date(date_sampled))

all_sites_stat <- function(param, axis_title) {
  # param - use component_short parameter name in quotes
  # axis_title - use axis title value from 00_vis_custom.R, no quotes, or new title in quotes.
  
  p <- dat2 %>%
    dplyr::filter(component_short == param & end == "N") %>%
    ggplot(aes(x = DATE, y = result, color = site_friendly)) +
    scale_x_date(
          limits = as.Date(c("2017-07-01", "2021-07-01")),
          date_breaks = "1 year", date_labels = "%Y") +
    geom_point(size = 3) +
    geom_line(size = 1) +
    geom_smooth(method = "lm", color = "black") +
    ggpubr::stat_regline_equation(label.y = 100) +
    scale_colour_manual(name = "Site", values = sitecolours) +
    cowplot::theme_cowplot() +
    scale_y_continuous(expand = c(0,0)) +
    #scale_x_datetime(date_breaks = '1 year', date_labels='%Y') +
    theme(axis.text.x = element_text(angle = 0, vjust=0.3, size=12, color='black')) +
    labs(y = axis_title,
         x = "",
         title = paste(param))
  
  p
}

# use the function to create full timeseries plots of whatever parameter you want, examples below

all_sites_stat("CHLA_C", chla_y_title) 