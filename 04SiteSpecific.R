##So, you'll want to change the function variable from wbid to site and then in the code inside 
##filter() you'll change it to site_acronym == site which will then make your variable input 
##whatever site you want to select using the inputs from the site acronym column.

## have to use sitez because site plots blank because site is already a column in data set!  

sites <- function(param, sitez, axis_title) {
 # param - use component_short parameter name in quotes
 # site - use site acronymns names in the site_acronym column, in quotes
 # axis_title - use axis title value from 00_vis_custom.R, no quotes, or new title in quotes.

  p <- dat2 %>%
    dplyr::filter(component_short == param &
                    end == "N" &
                    site_acronym == sitez) %>%
    ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    scale_colour_manual(name = "Site", values = sitecolours) +
    cowplot::theme_cowplot() +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
    theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
    labs(y = axis_title,
         x = "",
         title = paste(param),
	   subtitle = paste(sitez))
	   

	p
	
}


sites('TN', 'MK', 'Total Nitrogen')


 --------------- site specific with threshold ------------------------

  sites_thres <- function(param, sitez, threshold, axis_title) {
    # param - use component_short parameter name in quotes
    # site - use site acronymns names in the site_acronym column, in quotes
    # axis_title - use axis title value from 00_vis_custom.R, no quotes, or new title in quotes.
    
    p <- dat2 %>%
      dplyr::filter(component_short == param &
                      end == "N" &
                      site_acronym == sitez) %>%
      ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
      geom_point(size = 3) +
      geom_line(size = 1) +
      geom_hline(yintercept = threshold, linetype = 'longdash', color = 'gray18', size = 1.5) +
      scale_colour_manual(name = "Site", values = sitecolours) +
      cowplot::theme_cowplot() +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_datetime(date_breaks = '1 year', date_labels='%Y') +
      theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
      labs(y = axis_title,
           x = "",
           title = paste(param),
           subtitle = paste0("Advisory = ", threshold))
    
    
    p
    
  }

sites_thres("FECCOL", 'GL2', 71, "Feccolcoliform")
sites_thres("DO_P", "LM", 42, "Dissolved Oxygen %")






--------------- site specific with lm or stats ---------------
## 
## have to use sitez because site plots blank because site is already a column in data set!  
  
##had to change formatting of DATE so R could read it right  
  dat2 <- dat2 %>% 
  mutate(DATE = lubridate::as_date(date_sampled))


  sites_stat <- function(param, sitez, axis_title) {
    # param - use component_short parameter name in quotes
    # site - use site acronymns names in the site_acronym column, in quotes
    # axis_title - use axis title value from 00_vis_custom.R, no quotes, or new title in quotes.
    
    p <- dat2 %>%
      dplyr::filter(component_short == param &
                      end == "N" &
                      site_acronym == sitez) %>%
      ggplot(aes(x = DATE, y = result, color = site_friendly)) +
      scale_x_date(
        limits = as.Date(c("2017-07-01", "2021-07-01")),
        date_breaks = "1 year", date_labels = "%Y") +
      geom_point(size = 3) +
      geom_line(size = 1) +
      geom_smooth(method = "lm", color = "black") +
      ggpubr::stat_regline_equation(label.y = 15, aes(label = ..rr.label..))+
      scale_colour_manual(name = "Site", values = sitecolours) +
      cowplot::theme_cowplot() +
      scale_y_continuous(expand = c(0,0)) +
      #scale_x_datetime(date_labels='%Y', date_breaks = '1 year', limits = c("2017-07-20" - "2021-09-07"))+
      #scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
      theme(axis.text.x = element_text(vjust=0.3, size=12, color='black')) +
      labs(y = axis_title,
           x = "",
           title = paste(param),
           subtitle = paste(sitez))
    
    
    p
    
  }

sites_stat("CHLA_C", 'GR', "Chlora")

    