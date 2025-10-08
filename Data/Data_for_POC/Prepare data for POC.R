#Data handover/prep for Jake Lawlor
library(dplyr)
library(ggplot2)
library(patchwork)
#Bring in all the data and pair it down until it contains only what is needed for handover----
#1: ABUNDANCE----
  #Data from 3.1 Data_prep.R, step 1: Generated stratified abundance and standard error estimates
  abundance_ind_Region<-read.csv(here::here("2025-04-23/Output/IndexAbundance/abundance_ind_Region.csv"))  
  head(abundance_ind_Region)

  #subset to Spring and remove "all" (Canada and USA only)
abundance_ind_Region_Sp<-subset(abundance_ind_Region, abundance_ind_Region$Season=="Spring" & abundance_ind_Region$Index_Region=="Canada" | 
                                  abundance_ind_Region$Season=="Spring" & abundance_ind_Region$Index_Region=="USA")

ggplot(abundance_ind_Region_Sp, 
       aes(x = Year, y = Index_Estimate, colour = Index_Region, group = Index_Region)) +
  geom_line() +
  geom_ribbon(aes(ymin = Index_Estimate - Index_SD,
                  ymax = Index_Estimate + Index_SD,
                  fill = Index_Region), 
              alpha = 0.25, colour = NA) +
  theme_bw() +
  labs(x = "Year", y = "Index Estimate",
       colour = "Region", fill = "Region")

head(abundance_ind_Region_Sp)
#remove the columns that are not useful
abundance_ind_Region_Sp$Time <- NULL
abundance_ind_Region_Sp$Category  <- NULL
abundance_ind_Region_Sp$YearGroup  <- NULL
abundance_ind_Region_Sp$Date  <- NULL
abundance_ind_Region_Sp$Season  <- NULL
#rename some columns for clarity
names(abundance_ind_Region_Sp)[c(2,3,4)] <- c("Region", "Estimate", "SD")
#reset X column 
abundance_ind_Region_Sp$X <- 1:102
summary(abundance_ind_Region_Sp)
#save
write.csv(abundance_ind_Region_Sp,(here::here("Data/Data_for_POC/POC_Abundance.csv")), row.names = FALSE)

#2 AREA OCCUPIED----
  #Data from 3.1 Data_prep.R, steps 2&3: 
    #get the abundance estimates per grid location, Add season, Year, depth, and the area (km2) of the Stratum
      #then 8.1 Deepening.R adds a depth field
      abdest<- read.csv(here::here("2025-04-23/Output/IndexAbundance/ForShiftAnalysis/AbundanceEstimates_GridCentriods_Reg_wDepth.csv"))
      dim(abdest) 
      summary(abdest) 
      abdest$Region<-factor(abdest$Stratum)

#Select for Spring
abdest.spr<- abdest %>%
  filter(Season == "Spring")
dim(abdest.spr)
names(abdest.spr)

# Calculate total annual spring abundance by region and year
abdest.spr <- abdest.spr %>%
  group_by(Region, Year) %>%
  mutate(TotalSpringAbundance = sum(Abundance))

## Calculating Area Thresholds
#We'll calculate the area containing different percentages of abundance (50%, 75%, 90%, 95%) for each region and year:
# Function to calculate area for different abundance thresholds 
#from EAOrmarkdown.Rmd
calculate_areas <- function(data, thresholds = c(50, 75, 90, 95)) {
  result_list <- list()
  
  for (threshold in thresholds) {
    threshold_result <- data %>%
      group_by(Region, Year) %>%
      mutate(Total_Abundance = sum(Abundance)) %>%
      mutate(Total_Area = sum(Area_km2)) %>%#
      arrange(Region, Year, desc(Abundance/Area_km2)) %>%
      mutate(
        Cumulative_Abundance = cumsum(Abundance),
        Percent_Abundance = Cumulative_Abundance / Total_Abundance * 100,
        Cumulative_Area = cumsum(Area_km2)
      ) %>%
      filter(Percent_Abundance <= threshold) %>%
      summarize(
        Threshold = threshold,
        Area_Threshold = sum(Area_km2),
        Total_Area = first(Total_Area),#
        Percent_Area_Used = Area_Threshold / Total_Area * 100,#
        Total_Abundance = first(Total_Abundance),
        #Total_Area = sum(Area_km2, na.rm = TRUE),
        #Percent_Area_Used = Area_Threshold / sum(Area_km2, na.rm = TRUE) * 100,
        n_cells = n()
      )
    
    result_list[[as.character(threshold)]] <- threshold_result
  }
  
  # Combine all results
  bind_rows(result_list)
}

# Apply the function to calculate areas for different thresholds
area_thresholds <- calculate_areas(abdest.spr)
summary(area_thresholds)
area_thresholds <- area_thresholds %>%
  mutate(Area_Efficiency = Area_Threshold / Total_Abundance)


#Plot Area Occupied: The total area containing 90%,of the abundance
#subset 90%
area_thresholds_sub<-subset(area_thresholds, area_thresholds$Threshold ==90)
AO1<-ggplot(area_thresholds_sub, 
       aes(x = Year, y = Area_Threshold, colour = Region, group = Region)) +
  geom_line() +
  theme_bw() +
  labs(x = "Year", y = "Total Area occupied (90% Abundance)",
       colour = "Region", fill = "Region")+
  theme(legend.position = "none")

AO2<-ggplot(area_thresholds_sub, 
       aes(x = Year, y = Percent_Area_Used  , colour = Region, group = Region)) +
  geom_line() +
  theme_bw() +
  labs(x = "Year", y = "Percent of Area Used (90% Abundance)",
       colour = "Region", fill = "Region")+
  theme(legend.position = "none")

AO3<-ggplot(area_thresholds_sub, 
            aes(x = Year, y =Area_Efficiency , colour = Region, group = Region)) +
  geom_line() +
  theme_bw() +
  labs(x = "Year", y = "Area Efficiency (90% Abundance)",
       colour = "Region", fill = "Region")
AO1 + AO2 + AO3 + plot_layout(widths = c(1,1,1.2))

#save
write.csv(area_thresholds,(here::here("Data/Data_for_POC/POC_AreaOccupied.csv")), row.names = FALSE)

#3 ABUNDANCE-WEIGHTED DEPTH----
  #Data from 8.1Deepening.R:
    #Depth field is added to the abundance estimates per grid location (AbundanceEstimates_GridCentriods_Reg_wDepth.csv),
    # Data are grouped by year/season/region
      #calculates the mean, median, Q5, and Q95 depth, weighted by estimated abundance values 
      D_data_Reg<- read.csv(here::here("2025-04-23/Output/Shift_Indicators/Seasonal_Deepening_Reg.csv"))
      summary(D_data_Reg) 

#subset spring
  D_data_Reg<- subset(D_data_Reg, D_data_Reg$Season == "Spring")
  names(D_data_Reg)[names(D_data_Reg) == "Stratum"] <- "Region" #rename some columns for clarity
  D_data_Reg$Period  <- NULL #remove unneeded field
  D_data_Reg$Season  <- NULL #remove unneeded field
#these should all be negative values 
  D_data_Reg$Depth_Mean<- D_data_Reg$Depth_Mean*-1
  D_data_Reg$Depth_Median<- D_data_Reg$Depth_Median*-1
  D_data_Reg$Depth_Q5<- D_data_Reg$Depth_Q5*-1
  D_data_Reg$Depth_Q95<- D_data_Reg$Depth_Q95*-1

#plot
ggplot(D_data_Reg, 
       aes(x = Year, y = Depth_Median , colour = Region, group=Region)) +
  geom_line() +
  geom_ribbon(aes(ymin = Depth_Q5,
                  ymax = Depth_Q95,
                  fill = Region), 
              alpha = 0.25, colour = NA) +
  theme_bw() +
  labs(x = "Year", y = "Depth",
       colour = "Region", fill = "Region")
write.csv(D_data_Reg,(here::here("Data/Data_for_POC/POC_AWD.csv")), row.names = FALSE)

#4 RANGE EDGE----
  #from 7.1RangeEdge.R: Calculates 5th/50th/95th percentile of the spatial distribution (Weighted by abundance est,  quantile of the coordinate values)
  RE_DAT<- read.csv(here::here("R/DataforFinalFigs/Edge_df_NSreshp.csv"))

#these data area already subset to spring and represent the whole study area, but we can remove some useless fields
names(RE_DAT)
RE_DAT$YearGroup   <- NULL
RE_DAT$Season   <- NULL
write.csv(RE_DAT,(here::here("Data/Data_for_POC/POC_RangeEdge.csv")), row.names = FALSE)

#5 CENTRE OF GRAVITY----
  #data from: 5.1Centre_of Gravity.R, 
    #which takes the abundance estimates per grid location data (AbundanceEstimates_GridCentriods_Reg_wDepth.csv), from 3.1 Data_prep.R
      #and calculates the weighted mean of lon/lat based on the abundance values,
      centroid_data_Reg<- read.csv(here::here("2025-04-23/Output/Shift_Indicators/seasonal_centroid_data_region.csv"))

#subset for spring,rename some columns for clarity, and remove unneeded
centroid_data_Reg<-subset(centroid_data_Reg, centroid_data_Reg$Season=="Spring")
names(centroid_data_Reg)[names(centroid_data_Reg) == "Stratum"] <- "Region" 
centroid_data_Reg$Season  <- NULL 
write.csv(centroid_data_Reg,(here::here("Data/Data_for_POC/POC_COG.csv")), row.names = FALSE)
names(centroid_data_Reg)

#6 DISTANCE TO BORDER----















