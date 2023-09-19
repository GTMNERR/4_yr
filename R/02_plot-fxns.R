plot1 <- function(site, param, axis_title) {
  
  df <- dat2 %>% 
    filter(station_code == {{site}} & component_short == {{param}})
  
  title <- unique(df$site_friendly)
  
  p <- ggplot(data = df) +
        geom_col(aes(x = date_sampled, y = result, fill = site_friendly)) +
        scale_fill_manual(name = "Site", values = sitecolours) +
        theme_classic() +
        theme(legend.position = "none") +
        scale_y_continuous(expand = c(0,0)) +
        labs(y = axis_title,
             x = "",
             title = title)
  
  p
  
}

plot2 <- function(param, axis_title){
  
  df <- dat2 %>% 
    filter(component_short == {{param}})
  
  p <- ggplot(data = df) +
    geom_boxplot(aes(x = site_friendly, y = result, fill = site_friendly)) +
    scale_fill_manual(name = "Site", values = sitecolours) +
    theme_classic() +
    theme(legend.position = "none") +
    scale_y_continuous(expand = c(0,0)) +
    labs(y = axis_title,
         x = "Site (North to South)")
  
  p
}

