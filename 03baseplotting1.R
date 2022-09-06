# ----01 timeseries-all sites function -----------------------------------

## LINE GRAPH ALL SITES 

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
         x = "")

  p
}

# use the function to create full timeseries plots of whatever parameter you want, examples below
wtemp <- all_sites("WTEM", "Temperature (\u00B0C)") +
  geom_line(data = PI, aes(x = date_sampled, y = wtem_n), color = "black") +
  coord_cartesian(ylim = c(10,35))
ggsave(plot = wtemp, filename = here("output", "wtemp_piref.png"), dpi = 120)


CHLA_all<-all_sites("CHLA_C", chla_y_title) +
    coord_cartesian(ylim = c(0, 220))

# corrected chlorophyll plot
all_sites("TP", phos_y_title) # total phosphorus
Nite <-all_sites("TN", nitro_y_title) +
  coord_cartesian(ylim = c(0,13))
ggsave(plot = Nite, filename = here("output", "N_all.png"), dpi = 120)


# total nitrogen (this may be incomplete because some values may end up needing to be calculated)
all_sites("ENTERO", entero_y_title) # enterococcus
all_sites("FECCOL", fecal_y_title) # fecal coliform
all_sites("DO", "Dissolved Oxygen")
tur <- all_sites("TURBIDITY", "Turbidity") +
  coord_cartesian(ylim = c(0,75))

ggplotly(all_sites("SALT", "Salinity"))
salt <- all_sites("SALT", "Salinity (psu)") +
  geom_rect(aes(xmin = as.POSIXct("2018-03-26"),
                xmax = as.POSIXct("2018-07-10"),
                ymin = 0, ymax = Inf),
            alpha = 0.4,
            fill = "grey78",
            color = NA) +
  geom_rect(aes(xmin = as.POSIXct("2018-08-08"),
                xmax = as.POSIXct("2018-10-30"),
                ymin = 0, ymax = Inf),
            fill = "grey78",
            color = NA) +
        geom_vline(aes(xintercept = as.POSIXct("2019-09-01")),
           size = 1,
           linetype = "dashed") +
        geom_vline(aes(xintercept = as.POSIXct("2019-11-23")),
             size = 1,
             linetype = "dashed") +
        geom_vline(aes(xintercept = as.POSIXct("2020-03-10")),
             size = 1,
             linetype = "dashed") +
  geom_vline(aes(xintercept = as.POSIXct("2020-08-04")),
             size = 1,
             linetype = "dashed") +
  geom_vline(aes(xintercept = as.POSIXct("2020-11-12")),
             size = 1,
             linetype = "dashed") +
  geom_vline(aes(xintercept = as.POSIXct("2021-03-01")),
             size = 1,
             linetype = "dashed") +
  geom_vline(aes(xintercept = as.POSIXct("2021-03-30")),
             size = 1,
             linetype = "dashed") +
  geom_vline(aes(xintercept = as.POSIXct("2021-07-12")),
             size = 1,
             linetype = "dashed") +
  geom_vline(aes(xintercept = as.POSIXct("2021-08-23")),
             size = 1,
             linetype = "dashed") +
  geom_vline(aes(xintercept = as.POSIXct("2022-02-24")),
             size = 1,
             linetype = "dashed") +
  geom_vline(aes(xintercept = as.POSIXct("2022-03-22")),
             size = 1,
             linetype = "dashed") +
  geom_vline(aes(xintercept = as.POSIXct("2017-09-07")),
             size = 1,
             linetype = "solid",
             color = "red") +
  geom_vline(aes(xintercept = as.POSIXct("2019-08-23")),
             size = 1,
             linetype = "solid",
             color = "red") +
  geom_vline(aes(xintercept = as.POSIXct("2020-08-03")),
             size = 1,
             linetype = "solid",
             color = "red") +
  geom_vline(aes(xintercept = as.POSIXct("2020-11-12")),
             size = 1,
             linetype = "solid",
             color = "red") +
  geom_vline(aes(xintercept = as.POSIXct("2021-07-06")),
             size = 1,
             linetype = "solid",
             color = "red") 


ggsave(plot = salt, filename = here("output", "Salt_all.png"), dpi = 120)

  scale_x_datetime(date_breaks = 'year', date_labels = '%Y') +
  theme(axis.text.x = element_text(angle = 0, vjust=0.3, size=12, color='black'))

sucra <- all_sites("SUCRA", "Sucralose ug/L")
ggsave(plot = sucra, filename = here("output", "sucra.png"), dpi = 120)

all_sites("ACETA", "Acetaminophen ug/L")

ggsave()
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
    scale_colour_manual(name = "Site", values = sitecolours1) +
    cowplot::theme_cowplot() +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_datetime(date_breaks = '1 year', date_labels='%Y') +
    theme(axis.text.x = element_text(vjust=0.3, size=12, color='black')) +
    labs(y = axis_title,
         x = "")

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
      scale_colour_manual(name = "Site", values = sitecolours2) +
      cowplot::theme_cowplot() +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_datetime(date_breaks = '1 year', date_labels='%Y') +
      theme(axis.text.x = element_text(vjust=0.3, size=12, color='black')) +
      labs(y = axis_title,
           x = "")

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
    #coord_cartesian(ylim = c(0,1500)) +
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

wbid_sites("DO_P", wbid = "Lake", "Dissolved Oxygen mg/L")
entLake <- wbid_sites("ENTERO", wbid = "Lake", "Enterocucuss")+
  coord_cartesian(ylim = c(0,1500))+
  geom_hline(yintercept = 130, linetype = 'longdash')
ggsave(plot = entLake, filename = here("output", "EntLake.png"), dpi = 120)
entRiver <- wbid_sites("ENTERO", wbid = "River", "Enterocucuss") +
  coord_cartesian(ylim = c(0,1500))+
  geom_hline(yintercept = 130, linetype = 'longdash')
ggsave(plot = entRiver, filename = here("output", "EntRiver.png"), dpi = 120)

x <-wbid_sites("SALT", wbid = "Lake", "Salinity (psu)")
x <-wbid_sites("SALT", wbid = "River", "Salinity (psu)")

ggsave(plot = x, filename = here("output", "Salt_lake.png"), dpi = 120)

Fecal<- wbid_sites("FECCOL", wbid = "Lake", "Fecal Coliform") +
  coord_cartesian(ylim = c(0,1000)) +
  geom_hline(yintercept = 43, linetype = 'longdash', color = 'gray18', size = 1.5) +
  labs(title = "Lake")
ggsave(plot = Fecal, filename = here("output", "Fecal_Lake.png"), dpi = 120)


tn <- wbid_sites("TN", wbid = "Lake", "Total Nitrogen mg/L")
ggsave(plot = tnr, filename = here("output", "TN_River.png"), dpi = 120)
tnr <- wbid_sites("TN", wbid = "River", "Total Nitrogen mg/L")

x<-wbid_sites("TP", wbid = "River", "Total Phosphorus mg/L") +
  geom_hline(yintercept = 0.105, linetype = 'longdash', color = 'black')
ggsave(plot = x, filename = here("output", "TP_river_thresh.png"), dpi = 120)


chlaSplit<-wbid_sites_threshold("CHLA_C", 11, 6.6, chla_y_title)
ggsave(plot = chlaSplit, filename = here("output", "CHLA_split.png"), dpi = 120)


fecalSplit<-wbid_sites_threshold("FECCOL", 43, 43, "Fecol Coliform")
ggsave(plot = fecalSplit, filename = here("output", "fecal_split.png"), dpi = 120)

enteroSplit<-wbid_sites_threshold("ENTERO", 130, 130, "Enterococuss") 
ggsave(plot = enteroSplit, filename = here("output", "entero_split.png"), dpi = 120)

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

ggsave(plot = FECCOL, filename = here("output", "FECCOL_rmhighValues_boxAllSites.png"), dpi = 120)

# use the function to create box plots of whatever parameter you want, examples below
c <- boxplot_all_sites("CHLA_C", chla_y_title) +
      coord_cartesian(ylim = c(0, 125)) +
      geom_hline(yintercept = 6.6, color = 'red') +
      geom_hline(yintercept = 11, linetype = 'dashed', color = 'red')

DO <- boxplot_all_sites("DO", "Dissolved Oxygen mg/L") +
       geom_hline(yintercept = 2) 
dop <- boxplot_all_sites("DO_P", "Dissolved Oxygen %") +
  coord_cartesian(ylim = c(0,150)) +
  geom_hline(yintercept = 42, color = "black")

ggsave(plot = dop, filename = here("output", "DOP_boxAllSites.png"), dpi = 120)


ent <- boxplot_all_sites("ENTERO", "Enterococcus")

PHO <- boxplot_all_sites("TP", "Total Phosphorous") +
    coord_cartesian(ylim = c(0,1))
  
nitro <- boxplot_all_sites("TN", "Total Nitrogen mg/L") +
      coord_cartesian(ylim = c(0 , 9))

boxplot_all_sites("NO23F", "Nitrate+Nitrite")
wtemp <- boxplot_all_sites("WTEM", "Temperature")

turbidity <- boxplot_all_sites("TURBIDITY", "Turbidity") +
  coord_cartesian(ylim = c(0, 60))

tknf<- boxplot_all_sites("TKNF", "Total Kjeldahl Nitrogen Dissolved mg/L") +
  coord_cartesian(ylim = c(0, 4))

FECCOL <- boxplot_all_sites("FECCOL", "Fecal Coliform CFU/100mL") +
  coord_cartesian(ylim = c(0, 1000))

sucra <- boxplot_all_sites("SUCRA", expression(paste("Surcalose (", mu, "g/L)")))
ggsave(plot = sucra, filename = here("output", "Sucra_boxAllSites.png"), dpi = 120)


boxplot_all_sites("ACETA", "Acetamenophin")

# this function works the same as `all_sites()` for changing titles
boxplot_all_sites("CHLA_C", chla_y_title) +
  labs(title = "New title, you'll want to change me")

# or different parameters without designated title
salt <- boxplot_all_sites("SALT", "Salinity (psu)")
ggsave(plot = salt, filename = here("output", "Salt_boxAllSites.png"), dpi = 120)

boxplot_all_sites("ENTERO", "Enterococuss MPN/100mL")

ggplotly(boxplot_all_sites("ENTERO", "Enterococuss MPN/100mL"))



### BOX PLOT FOR DEP PARAMETRES ONLY

boxplot_all_depsites <- function(param, axis_title) {
  # param - use component_short parameter name in quotes
  # axis_title - use axis title value from 00_vis_custom.R, no quotes
  
  p <- dat2 %>%
    dplyr::filter(component_short == param & end == "N") %>%
    ggplot(aes(x = site_friendly, y = result, fill = site_friendly)) +
    geom_boxplot(alpha = 0.8) +
    scale_fill_manual(name = "Site", values = depcolours) +
    cowplot::theme_cowplot() +
    scale_y_continuous(expand = c(0,0)) +
    theme(axis.text.x = element_text(size=12, color='black'),
          legend.position = "none") +
    scale_x_discrete(labels = c("GL1",
                                "GL2",
                                "Lake\nMiddle",
                                "GL4",
                                "GR1",
                                "Guana\nRiver",
                                "GR3")) +
    labs(y = axis_title,
         x = "")
  
  p
}

boxplot_all_depsites("SUCRA", "Sucralose (ug/L)")

x <- boxplot_all_depsites("TSS", "Total Suspended Solids") +
  coord_cartesian(ylim = c(0,130))

boxplot_all_depsites("W-TOC", "TOC")
ggsave(plot = x, filename = here("output", "TSS_allsites_box.png"), dpi = 120)


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
