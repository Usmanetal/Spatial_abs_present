---
  title: "geospatial_HCV"
author: "Usman"
date: "2025-03-17"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown



```{r cars}
library(sf)
library(ggplot2)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(patchwork)
library(ggpubr)
library(naijR)
library(cowplot)
```


```{r}
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


# Merge the spatial data with the HIV data
data <- nigeria_states %>%
  left_join(hiv_data, by = c("name" = "state"))

# Download Natural Earth data for administrative level 1 (states/provinces)
nigeria_states <- ne_states(country = "Nigeria", returnclass = "sf")
#nigeria<-ne_countries(country = "Nigeria")
# Filter the data to include only Kaduna State
kaduna_state <- nigeria_states[nigeria_states$name == "Kaduna", ]
# Plot Nigeria's state boundaries

# Replace with the path to your extracted shapefile
nigeria_lga_shapefile <- "NGA_adm2.shp"

# Read the shapefile
nigeria_lgas <- st_read(nigeria_lga_shapefile)
kaduna_state <- nigeria_lgas %>% filter(NAME_1 == "Kaduna")
```

## Including Plots

You can also embed plots, for example:
  
  ```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
