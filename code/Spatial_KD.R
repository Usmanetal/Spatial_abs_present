library(naijR)
library(cowplot)

# Get the list of all states
all_states <- states()

# Define a color for each state: light yellow for Kaduna, grey for others
state_colors <- ifelse(all_states == "Kaduna", "lightyellow", "grey")

# Plot the map with specified state colors
map_ng(region = all_states, col = state_colors, show.text = TRUE, title = "Map of Nigeria Highlighting Kaduna State")

library(readxl)
hiv_data<- read_excel("data for geospatial distribution of HCV.xlsx")
hcv_data<- read_excel("data for geospatial distribution of HCV.xlsx",sheet = "HCV RNA")
hcv_data<- hcv_data %>% rename(
  "name"=`Local Government of`,
  "present"=`POSITIVE FOR HCV RNA`
) %>%group_by(name) %>% 
  count(present) %>% mutate(name=str_replace(name,"soba","Soba"))

names(hiv_data)<-c("name","hcv_ab")
hiv_data<- hiv_data %>% 
  mutate(name=recode(name,"soba"="Soba")) %>% 
  group_by(name) %>% 
  count(hcv_ab)




library(sf)
library(ggplot2)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(patchwork)
library(ggpubr)

# Download Natural Earth data for administrative level 1 (states/provinces)
nigeria_states <- ne_states(country = "Nigeria", returnclass = "sf")
#nigeria<-ne_countries(country = "Nigeria")
# Filter the data to include only Kaduna State
kaduna_state <- nigeria_states[nigeria_states$name == "Kaduna", ]
# Merge the spatial data with the HIV data
data <- nigeria_states %>%
  left_join(hiv_data, by = "name")
# Plot Nigeria's state boundaries
p3<-ggplot() +
  geom_sf(data = nigeria_states, fill = "white", color = "black") +  # Other states
  geom_sf(data = kaduna_state, fill = "yellow", color = "black") +   # Kaduna State
  #labs(title = "Map of Nigeria Highlighting Kaduna State") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.title = element_blank(),        # Remove axis titles
    axis.text = element_blank(),         # Remove axis text
    axis.ticks = element_blank(),        # Remove axis ticks
    panel.background = element_blank(),  # Remove background
    legend.position = "right"            # Position legend on the right
  )


# Replace with the path to your extracted shapefile
nigeria_lga_shapefile <- "NGA_adm2.shp"

# Read the shapefile
nigeria_lgas <- st_read(nigeria_lga_shapefile)
kaduna_state <- nigeria_lgas %>% filter(NAME_1 == "Kaduna")

# Plot Nigeria's LGAs
ggplot(data = filter(nigeria_lgas,NAME_1=="Kaduna")) +
  geom_sf(fill = "lightyellow", color = "black") +
  geom_sf_text(aes(label = NAME_2), size = 2) +  # Adjust 'NAME_2' based on your shapefile's column name
  labs(title = "Map of Nigeria with Local Government Areas") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.title = element_blank(),        # Remove axis titles
    axis.text = element_blank(),         # Remove axis text
    axis.ticks = element_blank(),        # Remove axis ticks
    panel.background = element_blank(),  # Remove background
    legend.position = "right"            # Position legend on the right
  )

# Merge the spatial data with the HIV data
data <- nigeria_lgas[nigeria_lgas$NAME_1=="Kaduna",] %>%
  left_join(hiv_data, by = c( "NAME_2"="name"))

### hcv data result from kaduna
p1<-ggplot(data = data) +
  geom_sf(aes(fill = n), color = "black") +
  geom_sf_text(aes(label = NAME_2), size = 2) +
  scale_fill_viridis_c(name = "HCV AB Prevalence (%)", na.value = "white") +
  labs(title = "HCV AB Prevalence Rates in Kaduna") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    legend.position = "right"
  )+annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(.2, "in"), pad_y = unit(4, "in"),
                         style = north_arrow_fancy_orienteering)

#### HCV RNA prevalence data

data2 <- nigeria_lgas[nigeria_lgas$NAME_1=="Kaduna",] %>%
  left_join(hiv_data, by = c( "NAME_2"="name"))

p2<-ggplot(data = data2) +
  geom_sf(aes(fill = n), color = "black") +
  geom_sf_text(aes(label = NAME_2), size = 2) +
  scale_fill_viridis_c(name = "HCV AB Prevalence (%)", na.value = "white") +
  labs(title = "HCV AB Prevalence Rates in Kaduna") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    legend.position = "right"
  )+annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(.2, "in"), pad_y = unit(4, "in"),
                         style = north_arrow_fancy_orienteering)


# Arrange plots in a single column
combined_plot <- p1 / p2

# Combine plots with a shared legend
combined_plot <- ggarrange(p1, p2,
                           ncol = 1, nrow = 2,
                           common.legend = TRUE, legend = "bottom")

# Display the combined plot
print(combined_plot)

combined_plot+p3

# Define the bounding box of the inset map
bbox <- st_bbox(kaduna_state)

# Create the main map with a rectangle indicating the inset area
main_map_with_inset <- p3 +
  geom_rect(aes(xmin = bbox["xmin"], xmax = bbox["xmax"], ymin = bbox["ymin"], ymax = bbox["ymax"]),
            color = "red", fill = NA, size = 1)

# Combine the main map and the inset map
combined_map <- ggdraw() +
  draw_plot(main_map_with_inset,width = 0.5,height = 1) +
  draw_plot(p1, x = 0.4, y = 0.15, width = 0.8, height = 0.95) +
  draw_line(x = c(0.26, 0.6), y = c(0.6, 0.8), color = "red", size = 1, arrow = arrow())
#draw_line(x = c(0.26, 0.6), y = c(0.6, 0.4), color = "red", size = 1, arrow = arrow())
# Display the combined map
print(combined_map)
