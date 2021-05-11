library(tidyverse)
source("R/functions.R")

##
## Data
##

daily_data <- read_csv("clean_data/daily_data.csv", col_types = cols(station = col_character())) %>% 
  mutate(study = factor(ifelse(lubridate::year(date) < 2015, "SAMBAH", "SNMP")))

# Stations, years and season for indices
index_stations <- c("1032", "1041", "1036")
index_years <- c(2011, 2012, 2017, 2018, 2019)
index_season <- 5:10 # May-Oct

# Station cooordinates for map
station_cords <- tibble(station = c("1014", "1019", "1020", "1021", "1024", 
                                    "1025", "1026", "1029", "1031", "1032", "1036", "1041"), 
                        lat = c(55.814, 55.982384, 55.873339, 55.763451, 56.0542, 55.977303, 55.832534, 
                                55.866258, 56.123757, 56.012008, 56.078047, 56.256312), 
                        long = c(15.219833, 15.479153, 15.80197, 16.122975, 15.998025, 16.319291, 16.639634, 
                                 16.898637, 16.517921, 16.83893, 17.36011, 17.564027))

##
## Map, figure 1
##
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggspatial)

coast <- st_read("shapefiles/coastline.shp")

NMB <- st_read("shapefiles/sandbank.shp") %>% 
  filter(fid == 1094) %>% st_transform("+proj=longlat +datum=WGS84") %>% 
  smoothr::smooth(method = "ksmooth")

Natura <- st_read("shapefiles/Natura2000_end2016_SE_Pp.shp") %>% 
  st_transform("+proj=longlat +datum=WGS84")

inset_map <- ggplot(ne_countries(scale = "medium", returnclass = "sf")) +
  geom_sf() +
  coord_sf(xlim = c(8, 25), ylim = c(54, 60), expand = FALSE) +
  annotate("rect", xmin = 15, xmax = 17.75, ymin = 55.5, ymax = 56.5, color = "red", fill = NA) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.length = unit(0, "mm"))

# Basic map
map_plot <- ggplot(coast) + geom_sf(fill = "lightgrey") +  
  geom_sf(data = NMB, color = NA) + geom_sf(data = Natura, fill = NA, linetype = 2, alpha = .5) +
  coord_sf(xlim = c(15, 17.9), ylim = c(55.3, 56.5), expand = FALSE) +
  geom_point(data = station_cords, 
             aes(x = long, y = lat, shape = (station %in% c("1032", "1036", "1041"))), color = "black", show.legend = FALSE) +
  theme_bw() + theme(panel.grid = element_blank()) +
  xlab("") + ylab("") +
  geom_text(data = station_cords, aes(x = long, y = lat+.05, label = station)) +
  annotation_north_arrow(location = "tl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  annotation_scale() + annotate("text", x = 17.3, y = 56.2, label = "Northern Midsea Bank", fontface = 3) +
  scale_shape_manual(values = c(1, 16))

map_basic <- cowplot::ggdraw() +
  cowplot::draw_plot(map_plot) +
  cowplot::draw_plot(inset_map, x = 0.161, y = 0.17, width = 0.25, height = 0.25)



##
## Data, figure 2
##

summer <- tibble(start = as.Date(c(paste0(c(2011, 2012, 2017, 2018, 2019), "-05-01"))), 
                 end = as.Date(c(paste0(c(2011, 2012, 2017, 2018, 2019), "-10-31"))), 
                 study = c("SAMBAH", "SAMBAH", "SNMP", "SNMP", "SNMP"))

data_fig <- daily_data %>% ggplot() + 
  geom_point(aes(x = date, y = dph), size = .7, stroke = 0, alpha = .5)  + theme_bw() +
  scale_x_date(breaks = as.Date(paste0(2010:2020, "-07-01")), 
               minor_breaks = as.Date(paste0(2010:2020, "-01-01")), date_labels = "%Y") + 
  scale_y_continuous(breaks = c(0, 5, 10), minor_breaks = NULL, limits = c(0, 13)) +
  theme_bw() +
  theme(panel.spacing = unit(0.1, "lines"), 
        axis.ticks.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.1)) + 
  xlab("") + ylab("Daily DPH") + 
  facet_grid(station~study, scales = "free_x", space = "free_x") +
  geom_rect(data = summer, aes(xmin = start, xmax = end), ymin = 0, ymax = 14, fill = "grey", alpha = .2)


##
## Seasonality, figure 3
##

fit_common <- daily_data %>% 
  filter(station %in% index_stations) %>% 
  mutate(julian_day = lubridate::yday(date),
         year = as.factor(lubridate::year(date))) %>% 
  group_by(station) %>% 
  nest() %>% 
  mutate(fit = map(data, ~mgcv::gam(dph ~ s(julian_day, bs = "cc") + year, data = .x, family =poisson())),
         season = map(fit, ~tibble(predicted = predict(.x, newdata = tibble(julian_day = 1:365, year = 2013, study = "SAMBAH"), 
                                                       type = "response"), 
                                   julian_day = 1:365)
         )
  ) %>% 
  select(station, season) %>% 
  unnest(season) %>% 
  group_by(station) %>% 
  mutate(predicted = predicted / sum(predicted), study = "Both") %>% 
  ungroup()
fit_study <- daily_data %>% 
  filter(station %in% index_stations) %>% 
  mutate(julian_day = lubridate::yday(date),
         year = as.factor(lubridate::year(date))) %>% 
  group_by(station) %>% 
  nest() %>% 
  mutate(fit = map(data, ~mgcv::gam(dph ~ s(julian_day, bs = "cc", by = study) + year, data = .x, family =poisson())),
         season = map(fit, ~tibble(predicted = predict(.x, newdata = 
                                                         bind_rows(
                                                           tibble(julian_day = 1:365, year = 2013, study = "SAMBAH"),
                                                           tibble(julian_day = 1:365, year = 2017, study = "SNMP")), 
                                                       type = "response"),
                                   julian_day = rep(1:365, 2),
                                   study = c(rep("SAMBAH", 365), rep("SNMP", 365)))
         )
  ) %>% 
  select(station, season) %>% 
  unnest(season) %>% 
  group_by(station, study) %>% 
  mutate(predicted = predicted / sum(predicted)) %>% 
  ungroup()

month_numbers <- str_pad(1:12, width = 2, side = "left", pad = "0")
xaxis_breaks <-  c(lubridate::yday(as.Date(paste0("2000-", month_numbers, "-15"))))
month_labels <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

season_fig <- ggplot() + geom_line(data = fit_common, aes(x = julian_day, y = predicted), size = 1, color = "steelblue") + 
  geom_line(data = fit_study, aes(x = julian_day, y = predicted, linetype = study), color = "steelblue") + 
  facet_wrap(~station) + theme_bw() + scale_linetype_manual(values = c(2, 3)) + 
  theme(legend.position = "none", panel.grid.major.x = element_blank(), axis.ticks = element_blank()) + 
  scale_y_continuous(breaks = 0, expand = c(0, 0), limits = c(0, NA)) + xlab("") + ylab("") +
  scale_x_continuous(breaks = xaxis_breaks, labels = month_labels)

##
## Trend, figure 4
##

indices <- c("dph", "dps", "n_clicks", "n_encounters", "n_trains")
index_data <- map(indices, ~make_indices(index_stations, index_years, index_season, response = .x))
y_labs <- c("Mean daily DPH", "Mean daily DPS", "Mean daily clicks", "Mean daily encounters", "Mean daily click trains")
trend_figs <- map2(index_data, y_labs, ~trend_fig(.x, .y))

##
## All trends, figure S5
##

all_trends_fig <- index_data %>% map_df(bind_rows) %>% 
  filter(response_type != "n_trains") %>% 
  group_by(station, response_type) %>% 
  nest(data = c(year, index)) %>% 
  mutate(fit = map(data, ~lm(log(index) ~ year, data = .x)),
         trend = map_dbl(fit, ~100*(exp(coef(.x)["year"])-1)),
         upper = map_dbl(fit, ~100*(exp(confint(.x)["year", "97.5 %"])-1)),
         lower = map_dbl(fit, ~100*(exp(confint(.x)["year", "2.5 %"])-1))
  ) %>% 
  ggplot(aes(x = response_type)) + 
  geom_point(aes(y = trend), size = 2, color = "steelblue") +
  geom_linerange(aes(ymin = lower, ymax = upper), size = 1, color = "steelblue") + 
  facet_wrap(~station) + theme_bw() + ylim(c(-50, 50)) +
  geom_abline(intercept = 0, slope = 0) +
  ylab("Yearly change (%)") + xlab("") +
  theme(panel.grid = element_blank()) +
  scale_x_discrete(labels = c("DPH", "DPS", "Clicks", "Encounters")) + coord_flip()



##
## Station proportions, figure 5
##

prop_fig <- daily_data %>% 
  mutate(year = lubridate::year(date), Study = ifelse(year < 2015, "SAMBAH", "SNMP"), 
         station = ifelse(station %in% c(1041, 1036, 1032), station, "Other"), station = as.factor(station)) %>% 
  filter(lubridate::month(date) %in% 5:10, year %in% c(2011, 2012, 2017:2019)) %>% 
  group_by(Study, year, station) %>% 
  summarise(dph = sum(dph, na.rm = TRUE)) %>% 
  mutate(dph_prop = dph/sum(dph)) %>% 
  ggplot(aes(x = year, y = dph_prop, fill = Study)) + geom_col(position = "dodge") +
  theme_bw() + scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, .9)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        legend.position = c(.1, .85), legend.title = element_blank()) + 
  scale_fill_grey() + xlab("") + ylab("Station proportions of yearly observed DPH") + facet_wrap(~station) + 
  scale_x_continuous(breaks = c(2011, 2012, 2017:2019))

#
# Imputation, fig S1-S3
#

all_days <- expand_grid(station = c("1032", "1036", "1041"), 
                        date = seq(as.Date("2011-01-01"), as.Date("2019-12-31"), by = 1)) %>% 
  left_join(daily_data, by = c("station", "date")) %>% select(station, date, dph) %>% 
  mutate(julian_day = lubridate::yday(date), year = lubridate::year(date)) %>% 
  filter(year %in% c(2011, 2012, 2017, 2018, 2019))

model_fits <- all_days %>% 
  nest_by(station) %>% 
  mutate(fitted_model = list(mgcv::gam(dph ~ s(julian_day, bs = "cc") + year, 
                                       data = data, family = "poisson")),
         data =list(mutate(data, 
                           predicted_dph = predict(fitted_model, newdata = data, type = "response"),
                           imputed_dph = ifelse(is.na(dph), predicted_dph, dph)))
  )

plot_imputed <- function(imputed_data, plot_title = NA){
  imputed_data %>% filter(year %in% c("2011", "2012", "2017", "2018", "2019"),
                          julian_day %in% 121:304) %>% 
    ggplot(aes(x = julian_day)) + 
    geom_point(aes(y = imputed_dph, color = is.na(dph)), pch = 21) +
    geom_line(aes(y = predicted_dph), alpha = .7) +
    facet_wrap(~year, ncol = 2) +
    theme_bw() + ylab("dph") + xlab("julian day") + theme(legend.position = c(.7, .15)) +
    labs(color = "Imputed") +
    ggtitle(plot_title) + scale_color_manual(values = c("steelblue", "coral"))
}

imputed_1032 <- plot_imputed(model_fits$data[[1]], "Imputed data station 1032")
imputed_1036 <- plot_imputed(model_fits$data[[2]], "Imputed data station 1036")
imputed_1041 <- plot_imputed(model_fits$data[[3]], "Imputed data station 1041")

##
## Save figs
##

ggsave(map_basic, filename = "figs/fig_1.tiff", width = 6, height = 4, dpi = 800)
ggsave(data_fig, filename = "figs/fig_2.tiff", width = 6, height = 5, dpi = 800)
ggsave(season_fig, filename = "figs/fig_3.tiff", width = 6, height = 2, dpi = 800)
ggsave(trend_figs[[1]], filename = "figs/fig_4.tiff", width = 6, height = 4, dpi = 800)
ggsave(all_trends_fig, filename = "figs/fig_S5.tiff", width = 6, height = 4, dpi = 800)
ggsave(prop_fig, filename = "figs/fig_5.tiff", width = 6, height = 4, dpi = 800)
ggsave(imputed_1032, filename = "figs/fig_S1.tiff", width = 6, height = 4, dpi = 800)
ggsave(imputed_1036, filename = "figs/fig_S2.tiff", width = 6, height = 4, dpi = 800)
ggsave(imputed_1041, filename = "figs/fig_S3.tiff", width = 6, height = 4, dpi = 800)

