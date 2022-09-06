
#boxplot for ENTERO data showing MARCH 2022 highlighted sparks


## readin my data file
# ------------USE Readxlsx.R

## NOW need to select data set down to only what we want
entero_dat <- dat2 %>%
  select(site,
         date_sampled,
         component_short,
         result,
         site,
         site_friendly,
         month,
         day,
         year) %>%
  filter(component_short == 'ENTERO')


##plotting fxn for overlay of specific month of entero data 

boxplot <- ggplot(data = entero_dat, 
                  aes(x = site_friendly, y = result)) +
  geom_boxplot(data = filter(entero_dat, year < "2022"), 
               aes(fill = "2017-2021")) +
  geom_point(data = filter(entero_dat, month == 3 & year == 2022), 
             aes(color = "Mar 2022"),
             size = 4) +
  geom_point(data = filter(entero_dat, month == 4 & year == 2022),
             aes(color = "Apr 2022"),
             size = 4) +
  scale_color_manual(name = "", 
                     values = c("Mar 2022" = "red",
                                "Apr 2022" = "yellow")) +
  scale_fill_manual(name = "",
                    values = c("2017-2021" = "white")) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black")) +
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
  labs(x = '',
       y = "Enterococuss MPN/100mL")

ggplotly(boxplot)

ggsave(plot = boxplot, filename = here("output", "EnteroSpikes.png"), dpi = 120)
