#---------------------------------------------------------------------------------#
#### Technical Task for ALA Interview: Visualization of Reptile Records in ACT ####
# Kajanka Jehan Mathiaparanam
# 08 March 2023
#---------------------------------------------------------------------------------#

# Getting directory
setwd("C:/Users/kajan/OneDrive/Australia/JOB APPLICATIONS/2023-03-02 - CSIRO EcoCommons/Technical Task/ALA_Reptile")
curdir <- getwd()

# Load required libraries
library(tidyverse)
library(dplyr)
library(sf)
library(ggplot2)
library(cowplot)


#### 1) Read in and clean ALA reptile data ####

# 1A) Read in ALA reptile data
ALA_data <- read_csv(str_c(curdir, "/records-2023-03-07.csv", sep = ""))
# str(ALA_data)
# View(ALA_data)


# 1B) Rename columns with alphanumerics only
ALA_colnames <- names(ALA_data)
ALA_colnames_new <- gsub("[^[:alnum:]]+", "_", ALA_colnames)
names(ALA_data) <- ALA_colnames_new


# 1C) Check if records are unique based on record ID
length(ALA_data$Record_ID) == length(unique(ALA_data$Record_ID))


# 1D) Select columns and filter data
ALA_data <- ALA_data %>% filter(!is.na(Decimal_latitude_WGS84_)
                                 & !is.na(Decimal_longitude_WGS84_)
                                 & Taxon_Rank == "species"
                                 # & !is.na(Coordinate_Uncertainty_in_Metres)
                                 # & Coordinate_Uncertainty_in_Metres <= 1000
                                 # & !is.na(Year)
                                 # & Year >= 1990
                                 # & !is.na(Individual_count)
                                 # & !Basis_Of_Record %in% c("PRESERVED_SPECIMEN", "MATERIAL_SAMPLE")
                          ) %>%
                          select(Decimal_latitude_WGS84_, Decimal_longitude_WGS84_, Year, Month,
                                 Scientific_Name_intepreted_, Order, Family, Genus, Species, Individual_count, 
                          ) %>%
                          rename(Latitude = Decimal_latitude_WGS84_,
                                 Longitude = Decimal_longitude_WGS84_,
                                 Scientific_Name = Scientific_Name_intepreted_)

  
# 1E) Convert to spatial object (sf)
ALA_data_sf <- st_as_sf(ALA_data,
                        coords = c("Longitude", "Latitude"),
                        remove = F,
                        crs = 4326) # 4326 = WGS84

# Filter data for bar chart
ALA_data_month <- ALA_data %>% filter(!is.na(Month)) %>%
                               group_by(Month) %>% 
                               summarise(Month = as.integer(first(Month)),
                                         Records = n())


#### 2) Read in spatial data and transform ####

# 2A) Read in Australian border
Aus_border <- read_sf(paste(curdir, "/Australia_Boundary", sep = ""), "Australia_Boundary")

# 2B) Get min and max values of points to crop ACT
x_min <- min(ALA_data_sf$Longitude) - 0.1
x_max <- max(ALA_data_sf$Longitude) + 0.3 # 0.1
y_min <- min(ALA_data_sf$Latitude) - 0.2 # 0.1
y_max <- max(ALA_data_sf$Latitude) + 0.2 # 0.1

# 2C) Crop ACT
ACT_cropped <- st_crop(Aus_border, xmin = x_min, ymin = y_min, xmax = x_max, ymax = y_max)


#### 3) Plot ####

# 3A) Plot ACT within Australia

# Caption
Aus_plot_caption <- "Location of surveyed area within Australia"

Aus_plot <- ggplot() +
  geom_sf(data = Aus_border, fill = "white") +
  geom_sf(data = ACT_cropped, fill = "#c0c2be") +
  # geom_sf(data = ACT_cropped, fill = "#c0c2be") + # Dark Grey
  # geom_sf(data = ACT_cropped, fill = "#A5D46A") + # Light Green
  xlab("Longitude") +
  xlim(110, 155) +
  ylab("Latitude") +
  theme(axis.title.x = element_text(size = 9), axis.title.y = element_text(size = 9)) +
  theme(axis.text.x = element_text(angle = 0, size = 7, hjust = 0.5), axis.text.y = element_text(size = 7)) +
  theme_bw() # theme_void()

# Caption
Aus_plot <- Aus_plot + labs(caption = Aus_plot_caption) +
  theme(plot.caption = element_text(size = 10, hjust = 0))


# 3B) Plot survey/data points within ACT

ALA_plot_title <- "Distribution of Reptile records in ACT\n"
ALA_plot_caption <- "\nWider distribution of Squamata compared to Testudines, across ACT."

ALA_plot <- ggplot() +
  geom_sf(data = ACT_cropped) +
  # geom_sf(data = ACT_cropped, fill = "#edf0e9") +
  # geom_sf(data = ACT_cropped, fill = "#c0c2be") + # Dark Grey
  xlab("Longitude") +
  # xlim(x_min, x_max) +
  ylab("Latitude") +
  theme_bw()

# Add ALA data points
ALA_plot <- ALA_plot + geom_sf(data = ALA_data_sf, aes(color = Order), show.legend = F) # color = "#f28b74"

# Include as facets for Orders
ALA_plot <- ALA_plot + facet_grid(rows = vars(Order), scales = "fixed") + theme(strip.text = element_text(size = 12))

# Title
ALA_plot <- ALA_plot + labs(title =  ALA_plot_title) +
  theme(plot.title = element_text(size = 12, hjust = 0))

# Caption
ALA_plot <- ALA_plot + labs(caption = ALA_plot_caption) +
  theme(plot.caption = element_text(size = 10, hjust = 0))


# 3C) Bar chart with monthly number of records

# Caption
Bar_plot_caption <- "\nSignificantly lesser records during April-September."

Monthly_plot <- ggplot(data = ALA_data_month, aes(x = Month, y = Records)) +
  geom_bar(stat = "identity", fill = "steelblue")+
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  theme_bw() # theme_minimal()

# Caption
Monthly_plot <- Monthly_plot + labs(caption = Bar_plot_caption) +
  theme(plot.caption = element_text(size = 10, hjust = 0))


# 3D) Combine Australia map and Bar chart
Plot_right <- plot_grid(Aus_plot, Monthly_plot, align = "v", axis = "l", nrow = 2) + # axis = "tblr",
  theme(plot.background = element_rect(fill = "white", colour = NA))

# 3E) Combine all plots
Main_plot <- plot_grid(ALA_plot, Plot_right, align = "v", axis = "l", ncol = 2) + # axis = "tblr",
  theme(plot.background = element_rect(fill = "white", colour = NA))

# 3F) Save plot as png
ggsave(str_c(curdir, "/Plot.png", sep = ""), plot = Main_plot, device = "png", width = 27, height = 18, units = "cm")


#### End ####