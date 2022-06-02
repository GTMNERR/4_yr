# ----01 timeseries-all sites function -----------------------------------


all_sites <- function(param, axis_title) {
  # param - use component_short parameter name in quotes
  # axis_title - use axis title value from 00_vis_custom.R, no quotes, or new title in quotes.

p <- dat2 %>%
    dplyr::filter(component_short == param & end == "N") %>%
    ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    scale_colour_manual(name = "Site", values = sitecolours) +
    cowplot::theme_cowplot() +
    scale_y_continuous(expand = c(0,0)) +
    #scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
    #theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
    labs(y = axis_title,
         x = "",
         title = paste(param))

  p
}

# use the function to create full timeseries plots of whatever parameter you want, examples below

all_sites("CHLA_C", chla_y_title) # corrected chlorophyll plot
all_sites("TP", phos_y_title) # total phosphorus
all_sites("TN", nitro_y_title) # total nitrogen (this may be incomplete because some values may end up needing to be calculated)
all_sites("ENTERO", entero_y_title) # enterococcus
all_sites("FECCOL", fecal_y_title) # fecal coliform
all_sites("DO", "Dissolved Oxygen")
all_sites("TURBIDITY", "Turbidity")
all_sites("WTEM", "Temperature") +
  scale_x_datetime(date_breaks = 'year', date_labels = '%Y') +
  theme(axis.text.x = element_text(angle = 0, vjust=0.3, size=12, color='black'))

# ---- 01a EXAMPLES of how to further customize -----------------------------------

# not all parameters have a special y_axis title,
# if you want to label axis that isn't a value from the 00_vis_custom.R,
# you can do so using quotes. As an example:
all_sites("SALT", "Salinity (psu)")

# to change title name if you don't like default:
all_sites("CHLA_C", chla_y_title) +
  labs(title = "New title, you'll want to change me")

# you can also change the scales of the y axis
# (notice the difference between this output and the one from the previous line of code)
all_sites("CHLA_C", chla_y_title) +
  scale_y_continuous(breaks = c(25, 50, 75, 100, 125, 150, 200))

# to do both:
all_sites("CHLA_C", chla_y_title) +
  scale_y_continuous(breaks = c(25, 50, 75, 100, 125, 150, 200))+
  labs(title = "New title, you'll want to change me")




# 01.5 -------------Threshold for Do -timeseries line graph--------------------

##trying to add a function to add a threshold for all_sites graph! 
all_sites_threshold <- function(param, threshold, axis_title) 
{
  # param - use component_short parameter name in quotes
  # axis_title - use axis title value from 00_vis_custom.R, no quotes
  # threshold - as number

  allsites <- dat2 %>%
    dplyr::filter(component_short == param & end == "N") %>%
    ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    geom_hline(yintercept = threshold, linetype = 'longdash', color = 'gray18', size = 1.5) +
    scale_colour_manual(name = "Site", values = sitecolours) +
    cowplot::theme_cowplot() +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
    theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
    labs(y = axis_title,
         x = "")
         

  p <- cowplot::plot_grid(allsites, ncol = 1)


  p

}

 all_sites_threshold("DO_P", 42, "Dissolved Oxygen Percent")





# ---- 02 timeseries-split by waterbody functions -----------------------------------
# you are going to want to have lake and river split into two separate graphs

wbid_sites <- function(param, wbid, axis_title) {
  # param - use component_short parameter name in quotes
  # wbid - use wbid "Lake" or "River"
  # axis_title - use axis title value from 00_vis_custom.R, no quotes, or new title in quotes.

  if (wbid == "Lake") {

  p <- dat2 %>%
    dplyr::filter(component_short == param &
                    end == "N" &
                    wbid == "Lake") %>%
    ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    scale_colour_manual(name = "Site", values = sitecolours) +
    cowplot::theme_cowplot() +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_datetime(date_breaks = '1 year', date_labels='%Y') +
    theme(axis.text.x = element_text(vjust=0.3, size=12, color='black')) +
    labs(y = axis_title,
         x = "",
         title = paste(param),
         subtitle = "In Guana Lake")

  p

  }
  else {
    q <- dat2 %>%
      dplyr::filter(component_short == param &
                      end == "N" &
                      wbid == "River") %>%
      ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
      geom_point(size = 3) +
      geom_line(size = 1) +
      scale_colour_manual(name = "Site", values = sitecolours) +
      cowplot::theme_cowplot() +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_datetime(date_breaks = '1 year', date_labels='%Y') +
      theme(axis.text.x = element_text(vjust=0.3, size=12, color='black')) +
      labs(y = axis_title,
           x = "",
           title = paste(param),
           subtitle = "In Guana River")

    q
  }
}

wbid_sites_threshold <- function(param, lake_threshold, river_threshold, axis_title) {
  # param - use component_short parameter name in quotes
  # axis_title - use axis title value from 00_vis_custom.R, no quotes
  # lake_threshold - as number
  # river_threshold - as number

  lake <- dat2 %>%
    dplyr::filter(component_short == param & end == "N") %>%
    dplyr::filter(wbid == "Lake") %>%
    ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    geom_hline(yintercept = lake_threshold, linetype = 'longdash', color = 'gray18', size = 1.5) +
    scale_colour_manual(name = "Site", values = sitecolours1) +
    cowplot::theme_cowplot() +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_datetime(date_breaks = '1 year', date_labels='%Y') +
    theme(axis.text.x = element_text(angle = 0, vjust=0.3, size=12, color='black')) +
    labs(y = axis_title,
         x = "",
         title = "Lake",
         subtitle = paste0("Threshold =", lake_threshold))

  river <- dat2 %>%
    dplyr::filter(component_short == param & end == "N") %>%
    dplyr::filter(wbid == "River") %>%
    ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    geom_hline(yintercept = river_threshold, linetype = 'longdash', color = 'gray18', size = 1.5) +
    scale_colour_manual(name = "Site", values = sitecolours2) +
    cowplot::theme_cowplot() +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_datetime(date_breaks = '1 year', date_labels='%Y') +
    theme(axis.text.x = element_text(angle = 0, vjust=0.3, size=12, color='black')) +
    labs(y = axis_title,
         x = "",
         title = "River",
         subtitle = paste0("Threshold =", river_threshold))

  p <- cowplot::plot_grid(lake, river,
                          ncol = 1)

  p

}

# ---- 02a EXAMPLES -----------------------------------

wbid_sites("DO", wbid = "Lake", "Dissolved Oxygen mg/L")
wbid_sites("ENTERO", wbid = "Lake", "Enterocucuss")


wbid_sites_threshold("CHLA_C", 11, 6.6, chla_y_title)

# doesn't work?
wbid_sites_threshold("CHLA_C", lake_threshold = "", 6.6, chla_y_title)



# ---- 03 boxplots of all sites -----------------------------------
##!!##!! 
      #Coord_catesian line has to be determine for each parametres, not reproducable by 
      #function name alone yet!!! 

boxplot_all_sites <- function(param, axis_title) {
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

ggsave(plot = c, filename = here("output", "CHLA_box.png"), dpi = 120)

# use the function to create box plots of whatever parameter you want, examples below
c <- boxplot_all_sites("CHLA_C", chla_y_title) +
      coord_cartesian(ylim = c(0, 125)) +
      geom_hline(yintercept = 6.6, color = 'red') +
      geom_hline(yintercept = 11, linetype = 'dashed', color = 'red')

DO <- boxplot_all_sites("DO", "Dissolved Oxygen mg/L") +
       geom_hline(yintercept = 2) 
dop <- boxplot_all_sites("DO_P", "Dissolved Oxygen %")

ent <- boxplot_all_sites("ENTERO", "Enterocucuss")

PHO <- boxplot_all_sites("TP", "Total Phosphorous") +
    coord_cartesian(ylim = c(0,1))
  
    ggsave(plot = PHO, filename = here("output", "PHO_box.png"), dpi = 120)
  
n <- boxplot_all_sites("TN", "Total Nitrogen mg/L") +
      coord_cartesian(ylim = c(0 , 9))

  boxplot_all_sites("NO23F", "Nitrate+Nitrite")
  boxplot_all_sites("WTEM", "Temperature")

# this function works the same as `all_sites()` for changing titles
boxplot_all_sites("CHLA_C", chla_y_title) +
  labs(title = "New title, you'll want to change me")

# or different parameters without designated title
salt <- boxplot_all_sites("SALT", "Salinity (psu)")

boxplot_all_sites("ENTERO", "Enterococuss MPN/100mL")

ggplotly(boxplot_all_sites("ENTERO", "Enterococuss MPN/100mL"))





# ---- 04 boxplots by waterbody -----------------------------------

boxplot_wbid <- function(param, axis_title) {
  # param - use component_short parameter name in quotes
  # axis_title - use axis title value from 00_vis_custom.R, no quotes

  p <- dat2 %>%
    dplyr::filter(component_short == param & end == "N") %>%
    dplyr::filter(wbid == "Lake") %>%
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
         x = "",
         title = paste(param),
         subtitle = "Guana Lake Sites")

  q <- dat2 %>%
    dplyr::filter(component_short == param & end == "N") %>%
    dplyr::filter(wbid == "River") %>%
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
         x = "",
         title = paste(param),
         subtitle = "Guana River Sites")

  p / q

}


# use the function to create box plots of whatever parameter you want, examples below
boxplot_wbid("CHLA_C", chla_y_title)


# or different parameters without designated title
boxplot_wbid("SALT", "Salinity (psu)")










# ---- 05 discharge ----


all_sites_discharge <- function(param, axis_title) {
  # param - use component_short parameter name in quotes
  # axis_title - use axis title value from 00_vis_custom.R, no quotes, or new title in quotes.

  p <- dat2 %>%
    dplyr::filter(component_short == param & end == "N") %>%
    ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
    geom_rect(aes(xmin = as.POSIXct("2018-03-28"),
                  xmax = as.POSIXct("2018-07-11"),
                  ymin = 0, ymax = Inf),
              fill = "grey78",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2018-08-08"),
                  xmax = as.POSIXct("2018-10-25"),
                  ymin = 0, ymax = Inf),
              fill = "grey78",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2018-12-04"),
                  xmax = as.POSIXct("2019-03-14"),
                  ymin = 0, ymax = Inf),
              fill = "grey87",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2019-08-23"),
                  xmax = as.POSIXct("2019-09-01"),
                  ymin = 0, ymax = Inf),
              fill = "grey87",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2020-02-17"),
                  xmax = as.POSIXct("2020-03-10"),
                  ymin = 0, ymax = Inf),
              fill = "grey87",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2020-08-21"),
                  xmax = as.POSIXct("2020-11-12"),
                  ymin = 0, ymax = Inf),
              fill = "grey87",
              color = NA) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    geom_vline(aes(xintercept = as.POSIXct("2018-03-28")),
               size = 1,
               linetype = "dashed") +
    geom_vline(aes(xintercept = as.POSIXct("2018-10-25")),
               size = 1,
               linetype = "dashed") +
    geom_vline(aes(xintercept = as.POSIXct("2019-02-01")),
               size = 1) +
    geom_vline(aes(xintercept = as.POSIXct("2019-03-14")),
               size = 1) +
    geom_vline(aes(xintercept = as.POSIXct("2020-02-17")),
               size = 1) +
    geom_vline(aes(xintercept = as.POSIXct("2020-03-10")),
               size = 1) +
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

wbid_sites_discharge <- function(param, wbid, axis_title) {
  # param - use component_short parameter name in quotes
  # axis_title - use axis title value from 00_vis_custom.R, no quotes, or new title in quotes.
  # wbid - use wbid "Lake" or "River"

  if (wbid == "Lake") {

    p <- dat2 %>%
    dplyr::filter(component_short == param &
                    end == "N" &
                    wbid == "Lake") %>%
    ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
    geom_rect(aes(xmin = as.POSIXct("2018-03-28"),
                  xmax = as.POSIXct("2018-07-11"),
                  ymin = 0, ymax = Inf),
              fill = "grey78",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2018-08-08"),
                  xmax = as.POSIXct("2018-10-25"),
                  ymin = 0, ymax = Inf),
              fill = "grey78",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2018-12-04"),
                  xmax = as.POSIXct("2019-03-14"),
                  ymin = 0, ymax = Inf),
              fill = "grey87",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2019-08-23"),
                  xmax = as.POSIXct("2019-09-01"),
                  ymin = 0, ymax = Inf),
              fill = "grey87",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2020-02-17"),
                  xmax = as.POSIXct("2020-03-10"),
                  ymin = 0, ymax = Inf),
              fill = "grey87",
              color = NA) +
    geom_rect(aes(xmin = as.POSIXct("2020-08-21"),
                  xmax = as.POSIXct("2020-11-12"),
                  ymin = 0, ymax = Inf),
              fill = "grey87",
              color = NA) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    geom_vline(aes(xintercept = as.POSIXct("2018-03-28")),
               size = 1,
               linetype = "dashed") +
    geom_vline(aes(xintercept = as.POSIXct("2018-10-25")),
               size = 1,
               linetype = "dashed") +
    geom_vline(aes(xintercept = as.POSIXct("2019-02-01")),
               size = 1) +
    geom_vline(aes(xintercept = as.POSIXct("2019-03-14")),
               size = 1) +
    geom_vline(aes(xintercept = as.POSIXct("2020-02-17")),
               size = 1) +
    geom_vline(aes(xintercept = as.POSIXct("2020-03-10")),
               size = 1) +
    scale_colour_manual(name = "Site", values = sitecolours) +
    cowplot::theme_cowplot() +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
    theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
    labs(y = axis_title,
         x = "",
         title = paste(param),
         subtitle = "Guana Lake")

  p
  } else {
    q <- dat2 %>%
      dplyr::filter(component_short == param &
                      end == "N" &
                      wbid == "River") %>%
      ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
      geom_rect(aes(xmin = as.POSIXct("2018-03-28"),
                    xmax = as.POSIXct("2018-07-11"),
                    ymin = 0, ymax = Inf),
                fill = "grey78",
                color = NA) +
      geom_rect(aes(xmin = as.POSIXct("2018-08-08"),
                    xmax = as.POSIXct("2018-10-25"),
                    ymin = 0, ymax = Inf),
                fill = "grey78",
                color = NA) +
      geom_rect(aes(xmin = as.POSIXct("2018-12-04"),
                    xmax = as.POSIXct("2019-03-14"),
                    ymin = 0, ymax = Inf),
                fill = "grey87",
                color = NA) +
      geom_rect(aes(xmin = as.POSIXct("2019-08-23"),
                    xmax = as.POSIXct("2019-09-01"),
                    ymin = 0, ymax = Inf),
                fill = "grey87",
                color = NA) +
      geom_rect(aes(xmin = as.POSIXct("2020-02-17"),
                    xmax = as.POSIXct("2020-03-10"),
                    ymin = 0, ymax = Inf),
                fill = "grey87",
                color = NA) +
      geom_rect(aes(xmin = as.POSIXct("2020-08-21"),
                    xmax = as.POSIXct("2020-11-12"),
                    ymin = 0, ymax = Inf),
                fill = "grey87",
                color = NA) +
      geom_point(size = 3) +
      geom_line(size = 1) +
      geom_vline(aes(xintercept = as.POSIXct("2018-03-28")),
                 size = 1,
                 linetype = "dashed") +
      geom_vline(aes(xintercept = as.POSIXct("2018-10-25")),
                 size = 1,
                 linetype = "dashed") +
      geom_vline(aes(xintercept = as.POSIXct("2019-02-01")),
                 size = 1) +
      geom_vline(aes(xintercept = as.POSIXct("2019-03-14")),
                 size = 1) +
      geom_vline(aes(xintercept = as.POSIXct("2020-02-17")),
                 size = 1) +
      geom_vline(aes(xintercept = as.POSIXct("2020-03-10")),
                 size = 1) +
      scale_colour_manual(name = "Site", values = sitecolours) +
      cowplot::theme_cowplot() +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
      theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
      labs(y = axis_title,
           x = "",
           title = paste(param),
           subtitle = "Guana River")

    q

  }
}


all_sites_discharge("CHLA_C", chla_y_title) # corrected chlorophyll plot
all_sites_discharge("SALT", "Salinity (psu)")

wbid_sites_discharge("SALT", wbid = "Lake", "Salinity")
