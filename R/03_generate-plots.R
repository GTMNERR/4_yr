
# chla plots
plot1(site = "GTMMKNUT", param = "CHLA_C", axis_title = "Chlorophyll a (\U00B5g/L)", cb = F) %>% 
  ggsave(filename = here('output', 'mk_chla.png'), units = "in")
plot1(site = "GTMGL2NUT", param = "CHLA_C", axis_title = "Chlorophyll a (\U00B5g/L)", cb = F) %>% 
  ggsave(filename = here('output', 'gl2_chla.png'), units = "in")
plot1(site = "GTMLMNUT", param = "CHLA_C", axis_title = "Chlorophyll a (\U00B5g/L)", cb = F) %>% 
  ggsave(filename = here('output', 'lm_chla.png'), units = "in")
plot1(site = "GTMLSNUT", param = "CHLA_C", axis_title = "Chlorophyll a (\U00B5g/L)", cb = F) %>% 
  ggsave(filename = here('output', 'ls_chla.png'), units = "in")
plot1(site = "GTMRNNUT", param = "CHLA_C", axis_title = "Chlorophyll a (\U00B5g/L)", cb = F) %>% 
  ggsave(filename = here('output', 'rn_chla.png'), units = "in")
plot1(site = "GTMGRNUT", param = "CHLA_C", axis_title = "Chlorophyll a (\U00B5g/L)", cb = F) %>% 
  ggsave(filename = here('output', 'gr_chla.png'), units = "in")

plot2(param = "CHLA_C", axis_title = "Chlorophyll a (\U00B5g/L)", cb = F, stations = T) %>% 
  ggsave(filename = here('output', 'chla_boxplot.png'), units = "in")

# TN 
plot1(site = "GTMMKNUT", param = "TN", axis_title = "Total Nitrogen (mg/L)", cb = F) %>% 
  ggsave(filename = here('output', 'mk_tn.png'), units = "in")
plot1(site = "GTMGL2NUT", param = "TN", axis_title = "Total Nitrogen (mg/L)", cb = F)%>% 
  ggsave(filename = here('output', 'gl2_tn.png'), units = "in")
plot1(site = "GTMLMNUT", param = "TN", axis_title = "Total Nitrogen (mg/L)", cb = F)%>% 
  ggsave(filename = here('output', 'lm_tn.png'), units = "in")
plot1(site = "GTMLSNUT", param = "TN", axis_title = "Total Nitrogen (mg/L)", cb = F)%>% 
  ggsave(filename = here('output', 'ls_tn.png'), units = "in")
plot1(site = "GTMRNNUT", param = "TN", axis_title = "Total Nitrogen (mg/L)", cb = F)%>% 
  ggsave(filename = here('output', 'rn_tn.png'), units = "in")
plot1(site = "GTMGRNUT", param = "TN", axis_title = "Total Nitrogen (mg/L)", cb = F)%>% 
  ggsave(filename = here('output', 'gr_tn.png'), units = "in")

plot2(param = "TN", axis_title = "Total Nitrogen (mg/L)", cb = F, stations = T) %>% 
  ggsave(filename = here('output', 'tn_boxplot.png'), units = "in")

# TP 
plot1(site = "GTMMKNUT", param = "TP", axis_title = "Total Phosphorus (mg/L)", cb = F) %>% 
  ggsave(filename = here('output', 'mk_tp.png'), units = "in")
plot1(site = "GTMGL2NUT", param = "TP", axis_title = "Total Phosphorus (mg/L)", cb = F) %>% 
  ggsave(filename = here('output', 'gl2_tp.png'), units = "in")
plot1(site = "GTMLMNUT", param = "TP", axis_title = "Total Phosphorus (mg/L)", cb = F) %>% 
  ggsave(filename = here('output', 'lm_tp.png'), units = "in")
plot1(site = "GTMLSNUT", param = "TP", axis_title = "Total Phosphorus (mg/L)", cb = F) %>% 
  ggsave(filename = here('output', 'ls_tp.png'), units = "in")
plot1(site = "GTMRNNUT", param = "TP", axis_title = "Total Phosphorus (mg/L)", cb = F) %>% 
  ggsave(filename = here('output', 'rn_tp.png'), units = "in")
plot1(site = "GTMGRNUT", param = "TP", axis_title = "Total Phosphorus (mg/L)", cb = F) %>% 
  ggsave(filename = here('output', 'gr_tp.png'), units = "in")

plot2(param = "TP", axis_title = "Total Phosphorus (mg/L)", cb = F, stations = T) %>% 
  ggsave(filename = here('output', 'tp_boxplot.png'), units = "in")

# DON

# NH4
plot1(site = "GTMMKNUT", param = "NH4F", axis_title = "Ammonium (mg/L)", cb = F) %>% 
  ggsave(filename = here('output', 'mk_nh4.png'), units = "in")
plot1(site = "GTMGL2NUT", param = "NH4F", axis_title = "Ammonium (mg/L)", cb = F) %>% 
  ggsave(filename = here('output', 'gl2_nh4.png'), units = "in")
plot1(site = "GTMLMNUT", param = "NH4F", axis_title = "Ammonium (mg/L)", cb = F) %>% 
  ggsave(filename = here('output', 'lm_nh4.png'), units = "in")
plot1(site = "GTMLSNUT", param = "NH4F", axis_title = "Ammonium (mg/L)", cb = F) %>% 
  ggsave(filename = here('output', 'ls_nh4.png'), units = "in")
plot1(site = "GTMRNNUT", param = "NH4F", axis_title = "Ammonium (mg/L)", cb = F) %>% 
  ggsave(filename = here('output', 'rn_nh4.png'), units = "in")
plot1(site = "GTMGRNUT", param = "NH4F", axis_title = "Ammonium (mg/L)", cb = F) %>% 
  ggsave(filename = here('output', 'gr_nh4.png'), units = "in")

plot2(param = "NH4F", axis_title = "Ammonium (mg/L)", cb = F, stations = T) %>% 
  ggsave(filename = here('output', 'nh4f_boxplot.png'), units = "in")

# DO
plot2(param = "DO", axis_title = "Dissolved Oxygen (mg/L)", cb = F, stations = T) %>% 
  ggsave(filename = here('output', 'do_boxplot.png'), units = "in")

# PO4
p <- dat2 %>% 
  filter(component_short == "PO4" & result < 20) %>% 
ggplot() +
  geom_boxplot(aes(x = site_friendly, y = result, fill = site_friendly)) +
  scale_fill_manual(name = "Site", values = sitecolours) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Orthophosphate (mg/L)",
       x = "Site (North to South)") 
  ggsave(p, filename = here('output', 'po4_boxplot.png'), units = "in")
rm(p)

# SALT
plot2(param = "SALT", axis_title = "Salinity (psu)", cb = F, stations = T) %>% 
  ggsave(filename = here('output', 'sal_boxplot.png'), units = "in")