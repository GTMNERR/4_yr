# create color palette for sites
sitecolours <- c(
  Micklers = "#F8766D",
  `GL1` = "#D89000",
  `GL2` = "#A3A500",
  `Lake Middle` = "#39B600",
  `GL4` = "#00BF7D",
  `Lake South` = "#00BFC4",
  `River North` = "#00B0F6",
  `GR1` = "#9590FF",
  `Guana River` = "#E76BF3",
  `GR3` = "#FF62BC"
)

sitecolours2 <- c(
  Micklers = "#E69F00",
  `GL1` = "#D89000",
  `GL2` = "#56B4E9",
  `Lake Middle` = "#009E73",
  `GL4` = "#00BF7D",
  `Lake South` = "#F0E442",
  `River North` = "#0072B2",
  `GR1` = "#9590FF",
  `Guana River` = "#CC79A7",
  `GR3` = "#FF62BC"
)

plot1 <- function(site, param, axis_title, cb) {
  
  df <- dat2 %>% 
    filter(station_code == {{site}} & component_short == {{param}})
  
  title <- unique(df$site_friendly)
  
  if(cb == TRUE) {
    p <- ggplot(data = df) +
      geom_col(aes(x = date_sampled, y = result, fill = site_friendly)) +
      scale_fill_manual(name = "Site", values = sitecolours2) +
      theme_classic() +
      theme(legend.position = "none",
            axis.text = element_text(size = 12, color = "black")) +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_datetime(breaks = "years", date_labels = "%Y") +
      labs(y = axis_title,
           x = "",
           title = title)
    
    p
  } else {
  p <- ggplot(data = df) +
        geom_col(aes(x = date_sampled, y = result, fill = site_friendly)) +
        scale_fill_manual(name = "Site", values = sitecolours) +
        theme_classic() +
        theme(legend.position = "none",
              axis.text = element_text(size = 12, color = "black")) +
        scale_y_continuous(expand = c(0,0)) +
    scale_x_datetime(breaks = "years", date_labels = "%Y") +
        labs(y = axis_title,
             x = "",
             title = title)
  
  p
  }
}

plot2 <- function(param, axis_title, cb, stations){
  
  if(stations == TRUE){
  
  df <- dat2 %>% 
    filter(component_short == {{param}}) 
  } else{
    df <- dat2 %>% 
      filter(component_short == {{param}} & station_code %in% c("GTMMKNUT",
                                                                "GTMGL2NUT",
                                                                "GTMLMNUT",
                                                                "GTMLSNUT",
                                                                "GTMRNNUT",
                                                                "GTMGRNUT")) 
  }
  
  if(cb == TRUE){
    
  p <- ggplot(data = df) +
    geom_boxplot(aes(x = site_acronym, y = result, fill = site_friendly)) +
    scale_fill_manual(name = "Site", values = sitecolours2) +
    theme_classic() +
    theme(legend.position = "none",
          axis.text = element_text(size = 12, color = "black")) +
    scale_y_continuous(expand = c(0,0)) +
    labs(y = axis_title,
         x = "Stations (North to South)")
  
  p
  } else {
    p <- ggplot(data = df) +
      geom_boxplot(aes(x = site_acronym, y = result, fill = site_friendly)) +
      scale_fill_manual(name = "Site", values = sitecolours) +
      theme_classic() +
      theme(legend.position = "none",
            axis.text = element_text(size = 12, color = "black")) +
      scale_y_continuous(expand = c(0,0)) +
      labs(y = axis_title,
           x = "Stations (North to South)")
    
    p
  }
}

