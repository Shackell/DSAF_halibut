#Part1: pull in ALL NEFSC, DFO Maritimes and NF data to plot the distribution of tows and presence
#Part2: Make Appendix Figure 1: Footprint of NEFSC and DFO Maritimes, and NF  RV surveys 
#Part3: Make Appendix Figure 2: Panel plot showing the data availability by Year

library(tidyverse)
library(sf)
library(rnaturalearth)
#library(gmRi)
library(here)
library(patchwork)
source("R/Functions/enhance_r_funcs.R")

#Part 1: Bring in all tow & catch data
# Helper function to convert season names to middle-of-season dates
season_to_date <- function(year_season) {
  # Split all year_season values at once
  parts <- do.call(rbind, strsplit(year_season, "-"))
  years <- as.numeric(parts[, 1])
  seasons <- tolower(parts[, 2])
  
  # Define middle dates for each season
  season_dates <- list(
    winter = "02-15",
    spring = "05-15",
    summer = "08-15",
    fall = "11-15"
  )
  
  # Create the date strings
  date_str <- paste0(years, "-", season_dates[seasons])
  return(date_str)
}

month_season_table <- data.frame("Month" = str_pad(seq(from = 1, to = 12, by = 1), 2, "left", 0), "Season" = c("Winter", "Winter", "Spring", "Spring", "Spring", "Summer", "Summer", "Summer", "Fall", "Fall", "Fall", "Winter"))

#load unique tows
dfo_tows <- read_rds(here("Data/TrawlSurvey_data/dfo_weight_at_length.rds")) |>
  ungroup() |>
  dplyr::select(longitude, latitude, trawl_id, season, year, survey, date) |>
  distinct() |>
  mutate(
    date = lubridate::date(date),
    year = as.numeric(as.character(format(date, "%Y")))
  ) |>
  mutate(swept = 0.0405)
 
nefsc_tows <- read_rds(here("Data/TrawlSurvey_data/nefsc_both_weight_at_length.rds")) |>
  ungroup() |>
  dplyr::select(longitude, latitude, trawl_id, season, year, survey, date) |>
  distinct() |>
  mutate(date = lubridate::date(date)) |>
  mutate(swept = 0.0384)

nf_all <- get(load(here("Data/TrawlSurvey_data/abun_nl_all85to24.Rdata"))) |>
  ungroup() |>
  distinct(year, month, day, lat.start, long.start, NAFOdiv, dist.towed) |>
  mutate(trawl_id = as.character(row_number()), survey = "NF")

nf_all <- nf_all %>%
  mutate(WingSpread = ifelse(year < 1996, 0.007406042, 0.009092974)) %>% #nautical miles
  mutate(swept = dist.towed * WingSpread) %>%
  mutate(swept = swept * 3.4299)

nf_tows<- nf_all |>
  mutate(
    month = str_pad(month, 2, "left", 0),
    date = lubridate::date(paste(year, month, day, sep = "-")),
    long.start = -1 * long.start
  ) |>
  left_join(month_season_table, by = c("month" = "Month")) |>
  rename(c(longitude = long.start, latitude = lat.start, season = Season)) |>
  dplyr::select(longitude, latitude, trawl_id, season, year, survey, date, swept) |>
  distinct() 

tows_df <- rbind( dfo_tows, nefsc_tows, nf_tows) |>
  mutate(season = str_to_sentence(season), survey_season = paste(survey, season, sep = "_"))

#add catch data
dfo_catch <- read_rds(here("Data/TrawlSurvey_data/dfo_weight_at_length.rds")) |>
  ungroup() |>
  filter(scientific_name == "Hippoglossus hippoglossus") |>
  mutate(
    date = lubridate::date(date),
    year = as.numeric(as.character(format(date, "%Y")))
  ) |>
  dplyr::select(longitude, latitude, trawl_id, season, year, date, length_cm, number_at_length, weight_at_length, catchsex, survey) |>
  mutate(total_weight_at_length = number_at_length * weight_at_length) |>
  group_by(trawl_id, longitude, latitude, season, year, survey, date) |>
  summarize("total_biomass" = sum(total_weight_at_length), "total_abundance" = sum(number_at_length)) |>
  mutate(swept = 0.0405) |>
  full_join(dfo_tows)

dfo_catch$total_biomass[is.na(dfo_catch$total_biomass)] <- 0
dfo_catch$total_abundance[is.na(dfo_catch$total_abundance)] <- 0

all(dfo_catch$trawl_id %in% dfo_tows$trawl_id)
all(dfo_tows$trawl_id %in% dfo_catch$trawl_id)

nefsc_catch <- read_rds(here("Data/TrawlSurvey_data/nefsc_both_weight_at_length.rds")) |>
  ungroup() |>
  filter(scientific_name == "hippoglossus hippoglossus") |>
  mutate(date = lubridate::date(date)) |>
  dplyr::select(longitude, latitude, trawl_id, season, year, date, length_cm, number_at_length, weight_at_length, catchsex, survey) |>
  mutate(total_weight_at_length = number_at_length * weight_at_length) |>
  group_by(trawl_id, longitude, latitude, season, year, survey, date) |>
  summarize("total_biomass" = sum(total_weight_at_length), "total_abundance" = sum(number_at_length)) |>
  mutate(swept = 0.0384) |>
  full_join(nefsc_tows)

nefsc_catch$total_biomass[is.na(nefsc_catch$total_biomass)] <- 0
nefsc_catch$total_abundance[is.na(nefsc_catch$total_abundance)] <- 0

all(nefsc_catch$trawl_id %in% nefsc_tows$trawl_id)
all(nefsc_tows$trawl_id %in% nefsc_catch$trawl_id)

nf_catch <- get(load(here("Data/TrawlSurvey_data/abun_nl_all85to24.Rdata"))) |>
  ungroup() |>
  filter(spec == 893) |> #select only halibut
  filter(halibut_presence > 0) |>
  full_join(nf_all) |>
  mutate(
    month = str_pad(month, 2, "left", 0),
    date = lubridate::date(paste(year, month, day, sep = "-")),
    survey = "NF",
    total_weight_at_length = number * weight,
    long.start = -1 * long.start
  ) |>
  left_join(month_season_table, by = c("month" = "Month")) |>
  rename(c(longitude = long.start, latitude = lat.start, season = Season)) |>
  group_by(trawl_id, longitude, latitude, season, year, survey, date, swept) |>
  summarize("total_biomass" = sum(total_weight_at_length), , "total_abundance" = sum(number)) |>
  ungroup() 

nf_catch$total_biomass[is.na(nf_catch$total_biomass)] <- 0
nf_catch$total_abundance[is.na(nf_catch$total_abundance)] <- 0
str(nf_catch)
str(nf_tows)
# Double check NF
all(nf_catch$trawl_id %in% nf_tows$trawl_id)
all(nf_tows$trawl_id %in% nf_catch$trawl_id)

catch_df <- rbind(dfo_catch, nefsc_catch, nf_catch) |>
  mutate(season = str_to_sentence(season), survey_season = paste(survey, season, sep = "_"))
summary(catch_df)




#----

#Part2: Supplemental Figure 1: Footprint of NEFSC and DFO Maritimes, and NF  RV surveys ----
library(sf)
library (ggplot2)

tows_df<-subset(tows_df, tows_df$year > 1989)
tows_df<-subset(tows_df, tows_df$year < 2024)
catch_df<-subset(catch_df, catch_df$year > 1989)
catch_df<-subset(catch_df, catch_df$year < 2024)
#see the extent
extent <- tows_df |>
  distinct(longitude, latitude) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

ggplot() +
  geom_sf(data = extent)

All_region <- st_read(here::here("", "R/Shapefiles/IndexShapefiles/Full_RegionAl14.shp"))
crs <- st_crs(All_region)

#bounding box for plotting
bbox <- st_bbox(c(xmin = -78, ymin = 33, xmax = -40, ymax = 70))
bbox_polygon <- st_as_sfc(bbox)
st_crs(bbox_polygon) <- crs

#Mapping shapefiles
EEZ <- st_read(here::here("", "Data/Mapping_shapefiles/EEZ.shp"))
land <- st_read(here::here("", "Data/land_shapefile/ne_50m_land.shp"))
contours <- st_read(here::here("", "Data/Mapping_shapefiles/GEBCO_DepthContours.shp"))
NAFO <- st_read(here::here("", "Data/Mapping_shapefiles/Divisions.shp"))
Hague <- st_read(here::here("", "Data/Mapping_shapefiles/HagueLine.shp"))

EEZ <- st_transform (EEZ, crs)
land <- st_transform (land, crs)
contours <- st_transform (contours, crs)
NAFO <- st_transform (NAFO, crs)
Hague <- st_transform (Hague, crs)
sf::sf_use_s2(FALSE)   # revert to GEOS-based operations

# Clip large shapefiles shapefile to  bounding box
EEZ <- st_intersection(EEZ, bbox_polygon)
land <- st_intersection(land, bbox_polygon)#maybe comment out 
contours <- st_intersection(contours, bbox_polygon)
NAFO <- st_intersection(NAFO, bbox_polygon)

catch_df$survey <- factor(catch_df$survey, levels = c("NEFSC", "DFO", "NF") )

tows_df$survey <- factor(tows_df$survey, levels = c("NEFSC", "DFO", "NF") )
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(patchwork)
library(RColorBrewer)
library(scales)
library(grid)  # For unit() function


All_Trawls_Plot<-ggplot() +
  geom_sf(data = NAFO, color="darkblue", fill = NA) +
  geom_sf(data = Hague, color="black", size = 1) +
  geom_sf(data = EEZ, color="black", linetype = "dashed", size = 1) +
  geom_sf(data = land, fill="cornsilk") +
  geom_point (data = tows_df, aes(x= longitude , y = latitude, col= survey),  alpha = .6, size = .2, shape=19)+
  scale_color_manual(values = c("NEFSC" = "steelblue", "DFO" =  "orangered", "NF" = "darkred"))+ 
  xlim(-76, -42) + ylim(35, 67)+
  theme_bw()+
  labs(title=NULL, 
       x = NULL, 
       y = NULL, 
       fill = "Region"
       )+
  theme(legend.position = "none",
        plot.title=element_text(size=16,vjust=1.5,family="serif"),
        legend.background=element_rect(size=.9,colour="white",fill="white"),
        strip.text=element_text(size=14,family="serif",angle=0),
        panel.border = element_rect(colour = "black",fill=NA),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        strip.background=element_rect(colour="black",fill="white"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=10,family="serif"),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1,size=10,family="serif"),
        axis.title.x=element_text(size=12,hjust=0.5,vjust=-2,family="serif"),
        plot.margin=margin(20,3,3,3))+
  facet_grid(. ~ survey)

presence_only<-subset(catch_df, catch_df$total_abundance > 0)
presence_only$survey <- factor(presence_only$survey, levels = c("NEFSC", "DFO", "NF") )

All_catch_plot<-ggplot() +
  geom_sf(data = NAFO, color="darkblue", fill = NA) +
  geom_sf(data = Hague, color="black", size = 1) +
  geom_sf(data = EEZ, color="black", linetype = "dashed", size = 1) +
  geom_sf(data = land, fill="cornsilk") +
  geom_point (data = presence_only, aes(x= longitude , y = latitude, col= survey),  alpha = .6, size = .2, shape=19)+
  scale_color_manual(values = c("NEFSC" = "steelblue", "DFO" =  "orangered", "NF" = "darkred"))+ 
  xlim(-76, -42) + ylim(35, 67)+
  theme_bw()+
  labs(title=NULL, 
       x = NULL, 
       y = NULL, 
       fill = "Region"
       )+
  theme(legend.position = "none",
        plot.title=element_text(size=16,vjust=1.5,family="serif"),
        legend.background=element_rect(size=.9,colour="white",fill="white"),
        strip.text=element_text(size=14,family="serif",angle=0),
        panel.border = element_rect(colour = "black",fill=NA),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        strip.background=element_rect(colour="black",fill="white"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=10,family="serif"),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1,size=10,family="serif"),
        axis.title.x=element_text(size=12,hjust=0.5,vjust=-2,family="serif"),
        plot.margin=margin(20,3,3,3))+
  facet_grid(. ~ survey)

SurveyPlot<-grid.arrange(All_Trawls_Plot,All_catch_plot, padding = unit(0, "line"))
#add labels 
SurveyPlot_final <- ggdraw(SurveyPlot) +
  draw_plot_label(label = "(a)", x = 0.08, y = 0.998, size = 12) + 
  draw_plot_label(label = "(b)", x = 0.37, y = 0.998, size = 12) +   
  draw_plot_label(label = "(c)", x = 0.665, y = 0.998, size = 12) + 
  draw_plot_label(label = "(d)", x = 0.08, y = 0.5, size = 12) +  
  draw_plot_label(label = "(e)", x = 0.37, y = 0.5, size = 12) +  
  draw_plot_label(label = "(f)", x = 0.665, y = 0.5, size = 12)    

#SAVE plots SUPPLEMTAL FIGURE 1
ggsave(
  filename = "SurveyLocsPlot.png",
  plot = SurveyPlot_final,          # optional if it's the last plot
  path =  here::here("2025-04-23/FinalPlots"),
  width = 6.5,
  height = 7.5,
  dpi = 300
)

#PART 3
#panel plot showing the data availability by facet(.~year) 
#season by different viridis colors, 
# different symbols surveys
head(tows_df)

#fill in if missing season, remove winter
tows_df_2 <- tows_df %>%
  mutate(
    season = case_when(
      is.na(season) & month(date) %in% c(3, 4, 5)  ~ "Spring",
      is.na(season) & month(date) %in% c(6, 7, 8)  ~ "Summer",
      is.na(season) & month(date) %in% c(9, 10, 11) ~ "Fall",
      TRUE ~ season  # Keep existing SEASON values
    )
  )
tows_df_2<-subset(tows_df_2, tows_df_2$season !="Winter")

#checks
range(tows_df_2$year)
unique(tows_df_2$season)
unique(tows_df_2$survey)
range(tows_df_2$longitude)
range(tows_df_2$latitude)
sum(is.na(tows_df_2$longitude))
sum(is.na(tows_df_2$latitude))
sum(is.na(tows_df_2$season))
sum(is.na(tows_df_2$survey))

summary_table <- tows_df_2 %>%
  group_by(survey, season) %>%
  summarise(count = n()) %>%
  arrange(survey, season)

#load and clip the shapefiels for plotting 
#bounding box for plotting
bbox <- st_bbox(c(xmin = -77, ymin = 32, xmax = -42, ymax = 68))
bbox_polygon <- st_as_sfc(bbox)
st_crs(bbox_polygon) <- crs

#Mapping shapefiles
EEZ <- st_read(here::here("", "Data/Mapping_shapefiles/EEZ.shp"))
land <- st_read(here::here("", "Data/land_shapefile/ne_50m_land.shp"))
contours <- st_read(here::here("", "Data/Mapping_shapefiles/GEBCO_DepthContours.shp"))
NAFO <- st_read(here::here("", "Data/Mapping_shapefiles/Divisions.shp"))
Hague <- st_read(here::here("", "Data/Mapping_shapefiles/HagueLine.shp"))

EEZ <- st_transform (EEZ, crs)
land <- st_transform (land, crs)
contours <- st_transform (contours, crs)
NAFO <- st_transform (NAFO, crs)
Hague <- st_transform (Hague, crs)
sf::sf_use_s2(FALSE)   # revert to GEOS-based operations

# Clip large shapefiles shapefile to  bounding box
EEZ <- st_intersection(EEZ, bbox_polygon)
land <- st_intersection(land, bbox_polygon)#maybe comment out 
contours <- st_intersection(contours, bbox_polygon)
NAFO <- st_intersection(NAFO, bbox_polygon)


#plot
Annual_Pannels<-ggplot() +
  geom_sf(data = NAFO, color="darkblue", fill = NA) +
  geom_sf(data = Hague, color="black", size = 3) +
  geom_sf(data = EEZ, color="black", linetype = "dashed", size = 3) +
  geom_sf(data = land, fill="cornsilk") +
  geom_point (data = tows_df_2, 
              aes(x= longitude , y = latitude, col= season), #took out shape = survey becasue it is too small to see
              alpha = .6, size = .2)+
  scale_color_manual(
    values = c("Spring" = "#440154", "Summer" =  "#21908C", "Fall" = "#B5DE2B"))+
 # scale_shape_manual(
 #   values = c("NEFSC" = 16, "DFO" = 17, "NF" = 15))+
  xlim(-76, -43) + ylim(35, 67)+
  theme_bw()+
  labs(title=NULL, 
       x = NULL, 
       y = NULL, 
       fill = "season",
       shape = "survey")+
  guides(
    color = guide_legend(override.aes = list(size = 5, alpha = 1)),  # for color legend
    shape = guide_legend(override.aes = list(size = 5))   # for shape legend, if applicable
  )+
  theme(
        legend.background=element_rect(size=.9,colour="white",fill="white"),
        strip.text=element_text(size=8,family="serif",angle=0, margin = margin(t = 1, b = 1)),
        panel.border = element_rect(colour = "black",fill=NA),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        strip.background=element_rect(fill="white",size = 0.5,colour="black"),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust=1,size=9,family="serif"),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1,size=9,family="serif"),
        panel.spacing = unit(0, "lines"),
        legend.position = c(0.94, 0.06),
        plot.margin=margin(3,3,3,3))+
  facet_wrap(~ year, ncol = 7, nrow = 5)
Annual_Pannels

ggsave(
  filename = "AnnualSurveyLocs.png",
  plot = Annual_Pannels,          # optional if it's the last plot
  path =  here::here("2025-04-23/FinalPlots"),
  width = 6.5,
  height = 8,
  dpi = 300
)
