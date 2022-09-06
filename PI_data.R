
# read in PI data to be able to plot on temperature line graph 

PI <- readxl::read_xlsx(here::here('data', '2022-08-29_PI-data-mo-OR_clip.xlsx'))

# renaming for joining to GTM data 
PI<-rename(PI, date_sampled = datetimestamp)
PI<-rename(PI, site_friendly = station_code)

#organizing PI data 
PI2 <- PI %>%
  select(date_sampled, wtem_n, site_friendly) %>%
  pivot_longer(cols = 2,
               names_to = "component_short",
               values_to = "result")

#organizing GTM data to match with PI data
dat2temp <- dat2 %>%
  select(date_sampled,
         component_short,
         result,
         site_friendly)

datpi <- PI2 %>%
  full_join(dat2temp, by = c("date_sampled","component_short", "result", "site_friendly"))
  
datpi %>%
  ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_colour_manual(name = "Site", values = sitecolours) +
  cowplot::theme_cowplot() +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "",
       x = "")


# getting TN values to graph
PI2 <- PI %>%
  select(date_sampled, tn, site_friendly) %>%
  ggplot(aes(x = date_sampled, y = tn)) +
  geom_line(size = 1) +
  cowplot::theme_cowplot() +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "",
       x="")

ggsave(plot = x, filename = here("output", "PItemp.png"), dpi = 120)




temp <- dat2 %>%
  left_join(PI2, by = c("station_code","date_sampled", "component_short", "result"))

temp %>%
  ggplot(aes(x = date_sampled, y = result)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_colour_manual(name = "Site", values = sitecolours) +
  cowplot::theme_cowplot() +
  scale_y_continuous() +
  labs(y = Water Temperature C,
       x = "")



PItemp <- dplyr::select (wtem_n, datetimestamp)