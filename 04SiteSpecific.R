##So, you'll want to change the function variable from wbid to site and then in the code inside 
##filter() you'll change it to site_acronym == site which will then make your variable input 
##whatever site you want to select using the inputs from the site acronym column.



#01---------------BAR CHART BY SITE OVER TIME --------------------


## have to use sitez because site plots blank because site is already a column in data set!  

sites <- function(param, sitez, axis_title) {
 # param - use component_short parameter name in quotes
 # site - use site acronymns names in the site_acronym column, in quotes
 # axis_title - use axis title value from 00_vis_custom.R, no quotes, or new title in quotes.

  p <- dat2 %>%
    dplyr::filter(component_short == param &
                    end == "N" &
                    site_acronym == sitez) %>%
    ggplot(aes(x = date_sampled, y = result, fill = site_friendly)) +
    geom_col() +
    scale_fill_manual(values = sitecolours) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_datetime(date_breaks = '1 year', date_labels='%Y') +
    cowplot::theme_cowplot() + 
    theme(axis.text.x = element_text(vjust=0.3, size=12, color='black'),
          legend.title = element_blank(), legend.position = "none") +
    #guide = guide_legend(override.aes = list(name = "Site")) +
    labs(y = axis_title,
         x = "")
	   

	p
	
}


GR1 <- sites('DO', 'GL1', "Dissolved Oxygen") +
        geom_col(width = 0.1)
    #scale_y_continuous(breaks = c(0.0, 0.02, 0.04,0.06, 0.08, 0.1)) +
    coord_cartesian(ylim = c(-0.5 , 0.5)) +
      labs (subtitle = paste("GR1"))


x <- sites('ENTERO', 'MK', "Enterococcus MPN/100mL") +
      labs (subtitle = paste ("MK"))
  

ggsave(plot = x, filename = here("output", "GR3_ENTERO.png"), dpi = 120)

#02 --------------- site specific bar chart with threshold ------------------------

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






#03--------------- site specific with lm or stats ---------------
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




#04 -------------SITE SPECIFIC BOXPLOTS --------------------------

### NEEDS WORK THIS IS JUST ALL SITES BOXPLOT 

boxplot_all_sites <- function(param, site, axis_title) {
  # param - use component_short parameter name in quotes
  # axis_title - use axis title value from 00_vis_custom.R, no quotes
  
  p <- dat2 %>%
    dplyr::filter(component_short == param & end == "N") %>%
    ggplot(aes(x = site_friendly, y = result, fill = site_friendly)) +
    geom_boxplot(alpha = 0.8) +
    scale_fill_manual(name = "Site", values = sitecolours) +
    cowplot::theme_cowplot() +
    scale_y_continuous(expand = c(0,0)) +
    theme(axis.text.x = element_text(size=12, color='black'),
          legend.position = "none") +
    scale_x_discrete(labels = c("Micklers",
                                "GL1",
                                "GL2",
                                "Lake\nMiddle",
                                "GL4",
                                "Lake\nSouth",
                                "River\nNorth",
                                "GR1",
                                "Guana\nRiver",
                                "GR3")) +
    labs(y = axis_title,
         x = "")
  
  p
}

    

boxplot_all_sites("DO", "Dissolved Osygen")


# 05 -------------All sites bar chart over time --------------

all_sites_bar <- function(param, axis_title) {
  # param - use component_short parameter name in quotes
  # axis_title - use axis title value from 00_vis_custom.R, no quotes, or new title in quotes.
  
  p <- dat2 %>%
    dplyr::filter(component_short == param & end == "N") %>%
    ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
    geom_col() +
    scale_color_manual(name = "Site", values = sitecolours) +
    cowplot::theme_cowplot() +
    scale_y_continuous(expand = c(0,0)) +
    #scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
    #theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
    labs(y = axis_title,
         x = "",
         title = paste(param))
  
  p
}

all_sites_bar("ENTERO", "Enterococuss")
