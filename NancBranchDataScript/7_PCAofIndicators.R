library(tidyverse)
library(broom)
library(dplyr)

library(ggplot2)
library(cowplot)
library(patchwork)
library(gridExtra)
library(RColorBrewer)
library(scales)

library(grid)  # For unit() function
#theme----
theme_replace(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour="black"),
              
              strip.background=element_rect(colour="black",fill="white"))

theme_set(theme_bw())

theme_replace(legend.key =element_rect(colour="black",fill="white"),
              
              plot.margin = unit(c(1.5,3,1.5,1), "cm"),
              
              #plot.margin=margin(3,4,4,0),
              
              #plot.margin=margin(3,4,4,0),
              
              plot.title=element_text(size=16,vjust=1.5,family="serif"),
              
              legend.background=element_rect(size=.9,colour="white",fill="white"),
              
              strip.text=element_text(size=14,family="serif",angle=0),
              
              panel.border = element_rect(colour = "black",fill=NA),
              
              panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
              
              strip.background=element_rect(colour="black",fill="white"),
              
              axis.text.x = element_text(angle = 90, vjust = 0.2, hjust=1,size=12,family="serif"),
              
              axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1,size=14,family="serif"),
              
              axis.title.x=element_text(size=12,hjust=0.5,vjust=-2,family="serif"))

pd <- position_dodge(.5)
#####END theme----

POC_Abundance <- read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_Abundance.csv"))
names(POC_Abundance)[3:4]<-c("Abundance","Abundance_SD")
names(POC_Abundance)
tempAbundance<-POC_Abundance %>%select(Region,Year,Abundance)

POC_AWD <- read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_AWD.csv"))
names(POC_AWD)
tempAWD<-POC_AWD %>%select(Region,Year,Depth_Mean)

POC_AreaOccupied <- read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_AreaOccupied.csv"))
names(POC_AreaOccupied)
POC_AO<-POC_AreaOccupied %>%filter(Threshold=="90")
tempAO<-POC_AO %>%select(Region,Year,Area_Threshold)

POC_COG <- read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_COG.csv"))
names(POC_COG)
tempCOG<-POC_COG %>%select(Region,Year,centroid_latitude,centroid_longitude)
names(tempCOG)[3:4]<-c("COG_Lat","COG_Long")

POC_DtoB <- read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_DtoB.csv"))
names(POC_DtoB)
tempDtoB<-POC_DtoB %>%select(Region,Year,Dist_Mean)

POC_RangeEdge <- read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_RangeEdge.csv"))
names(POC_RangeEdge)
tempRangeEdge<-POC_RangeEdge %>%select(Year,Estimate_km_E_quantile_0.05,Estimate_km_N_quantile_0.05,
                                Estimate_km_E_quantile_0.95,Estimate_km_N_quantile_0.95)
names(tempRangeEdge)[2:5]<-c("Trailing_E","Trailing_N","Leading_E","Leading_N")

#make PCA file
PCAfile<-tempAbundance %>%
  left_join(tempAWD,by=c("Region","Year")) %>%
  left_join(tempAO,by=c("Region","Year")) %>%
  left_join(tempCOG,by=c("Region","Year")) %>%
  left_join(tempDtoB,by=c("Region","Year")) %>%
  left_join(tempRangeEdge,by=c("Year"))

write.csv(PCAfile,here::here("NancBranchDataScript/NanceDataPCA/PCAfile.csv"),row.names=FALSE)

# Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)
# Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)

# Read your data (assuming it's in a CSV file)
# data <- read.csv("your_data.csv")

# Prepare data for PCA with derived spatial metrics
data_enhanced <- PCAfile %>%
  mutate(
    # Log-transform skewed variables
    Log_Abundance = log(Abundance),
    Log_Area = log(Area_Threshold),
    # Depth per unit area (more ecologically meaningful)
    Depth_per_Area = Depth_Mean / Area_Threshold,
    # Calculate spatial extent (distance between leading and trailing edges)
    Spatial_Extent_E = Leading_E - Trailing_E,
    Spatial_Extent_N = Leading_N - Trailing_N,
    # Total spatial extent
    Total_Extent = sqrt(Spatial_Extent_E^2 + Spatial_Extent_N^2),
    # Directional metrics
    East_West_Position = (Leading_E + Trailing_E) / 2,  # Average E position
    North_South_Position = (Leading_N + Trailing_N) / 2,  # Average N position
    Directional_Bias_EW = Spatial_Extent_E / Total_Extent,  # E-W elongation (-1 to 1)
    Directional_Bias_NS = Spatial_Extent_N / Total_Extent,  # N-S elongation (-1 to 1)
    # Abundance density (log scale)
    Log_Abundance_Density = Log_Abundance - Log_Area
  )

# Select variables for PCA
pca_vars <- data_enhanced %>%
  select(Log_Abundance, Log_Area, Depth_per_Area, 
         COG_Lat, COG_Long,
         Spatial_Extent_E, Spatial_Extent_N, Total_Extent,
         East_West_Position, North_South_Position,
         Directional_Bias_EW, Directional_Bias_NS,
         Log_Abundance_Density)

# Standardize the variables (important for PCA)
pca_scaled <- scale(pca_vars)

# Perform PCA
pca_result <- prcomp(pca_scaled)

# Summary of PCA
summary(pca_result)

# Extract PC scores and add back Year and Region
pca_scores <- PCAfile %>%
  select(Region, Year) %>%
  bind_cols(as.data.frame(pca_result$x))

# View variance explained
variance_explained <- data.frame(
  PC = paste0("PC", 1:length(pca_result$sdev)),
  Variance = pca_result$sdev^2,
  Prop_Var = pca_result$sdev^2 / sum(pca_result$sdev^2),
  Cum_Var = cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))
)
print(variance_explained)

# Plot temporal trends in PC1 and PC2
p1 <- ggplot(pca_scores, aes(x = Year, y = PC1, color = Region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Temporal Trend in PC1",
       x = "Year", y = "PC1 Score") +
  theme(legend.position = "bottom")

p2 <- ggplot(pca_scores, aes(x = Year, y = PC2, color = Region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Temporal Trend in PC2",
       x = "Year", y = "PC2 Score") +
  theme(legend.position = "bottom")

# Biplot showing variable loadings
loadings_df <- as.data.frame(pca_result$rotation[, 1:2])
loadings_df$Variable <- rownames(loadings_df)

p3 <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = Region)) +
  geom_path(aes(group = Region), alpha = 0.3) +
  geom_point(aes(size = Year), alpha = 0.6) +
  geom_segment(data = loadings_df, 
               aes(x = 0, y = 0, xend = PC1 * 5, yend = PC2 * 5),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "black", inherit.aes = FALSE) +
  geom_text(data = loadings_df,
            aes(x = PC1 * 5.5, y = PC2 * 5.5, label = Variable),
            color = "black", size = 3, inherit.aes = FALSE) +
  theme_minimal() +
  labs(title = "PCA Biplot with Temporal Trajectory",
       subtitle = "Arrows show variable loadings, points show temporal progression")

# Display individual plots
print(p1)
print(p2)
print(p3)

# Analyze temporal trends using linear models
# Fit linear models for PC1 and PC2 over time by region
temporal_trends <- pca_scores %>%
  group_by(Region) %>%
  summarise(
    PC1_slope = coef(lm(PC1 ~ Year))[2],
    PC1_pval = summary(lm(PC1 ~ Year))$coefficients[2, 4],
    PC2_slope = coef(lm(PC2 ~ Year))[2],
    PC2_pval = summary(lm(PC2 ~ Year))$coefficients[2, 4]
  )

print("Temporal trends by region:")
print(temporal_trends)

# Variable loadings (contributions)
print("Variable loadings on PC1 and PC2:")
print(pca_result$rotation[, 1:2])

# Create bar plots for loadings
loadings_long <- loadings_df %>%
  pivot_longer(cols = c(PC1, PC2), names_to = "PC", values_to = "Loading")

p4 <- ggplot(loadings_long, aes(x = reorder(Variable, Loading), y = Loading, fill = Loading > 0)) +
  geom_col() +
  geom_text(aes(label = round(Loading, 3)), 
            hjust = ifelse(loadings_long$Loading > 0, -0.1, 1.1),
            size = 3) +
  coord_flip() +
  facet_wrap(~PC, scales = "free_x") +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "coral")) +
  theme_minimal() +
  labs(title = "Variable Loadings on PC1 and PC2",
       x = "Variable", y = "Loading") +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank())

print(p4)

# Combine temporal trends and loadings into one figure
library(patchwork)

combined_plot <- (p1 / p2) | p4

print(combined_plot)


# Combine temporal trends and loadings into one figure
library(patchwork)

combined_plot <- (p1 / p2) | p4

print(combined_plot)
