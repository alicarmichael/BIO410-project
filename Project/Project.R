# Load necessary libraries
library(terra)
library(ggplot2)
library(dplyr)
library(sf)
library(maps)
library(ggspatial)
library(plotly)

# Load the data as Fires
Fires <- read.csv("data/fire_data.csv")
head(Fires)

# Check column names
colnames(Fires)

# Select specific columns
fire_data_df <- Fires[, c("FireName", "FinalFireSizeAcres", "Longitude", "Latitude", "LandClass","SizeClass")]

fire_data_df
#Remove fire with no cords
fire_data_newdf <- fire_data_df[-165,]

# converts the fire data frame to a sf
fire_data_sf <- st_as_sf(fire_data_newdf, coords = c("Longitude", "Latitude"), crs = 4326)
print(fire_data_sf)


# Load Oregon county map data
oregon_map <- map_data("county", "oregon") %>% 
  select(long, lat, group, id = subregion)  # Correct column name is `region`

# Plot Oregon map with all fires
ggplot() +
  geom_polygon(data = oregon_map, aes(x = long, y = lat, group = group), 
               fill = "lightgrey", color = "white") +
  geom_sf(data = fire_data_sf, aes(color = SizeClass), size = fire_data_sf$SizeClass, alpha = 0.9) +
  scale_color_gradient(low = "yellow", high = "red", name = "Fire Size Class") + # Adjust the colors
  labs(title = "Distribution of Oregon Fires",
       x = "Longitude", 
       y = "Latitude") +
  theme_minimal() +
  coord_sf()

# Convert ggplot to a plotly object
interactive_plot <- ggplotly(gg, tooltip = "text")

# View the interactive plot
interactive_plot


# Now lets add The bee data

#reads in the OBA bee obvercation data
Bee_Observations <- read.csv("data/observations-501373.csv")
head(Bee_Observations)

#create a data frame
bee_observations_df <- Bee_Observations[, c("scientific_name","longitude", "latitude", "quality_grade")]

#turns data frame into sf
bee_observations_sf <- st_as_sf(bee_observations_df, coords = c("longitude", "latitude"), crs = 4326)
print(bee_observations_sf)

#plot bee observations on to oregon map with fires
ggplot() +
  geom_polygon(data = oregon_map, aes(x = long, y = lat, group = group), 
               fill = "lightgrey", color = "white") +
  geom_sf(data = bee_observations_sf, aes(color ="blue"), size = .1, alpha = 0.9) +
  labs(title = "Bee Species Observations in Oregon",
       x = "Longitude", 
       y = "Latitude") +
  theme_minimal() +
  coord_sf()


#plot that show Distribution of fires and bee observations
ggplot() +
  geom_polygon(data = oregon_map, aes(x = long, y = lat, group = group), 
               fill = "lightgrey", color = "white") +
  geom_sf(data = fire_data_sf, aes(color = SizeClass), size = fire_data_sf$SizeClass, alpha = 0.9) +
  scale_color_gradient(low = "yellow", high = "red", name = "Fire Size Class") + # Adjust the colors
  geom_sf(data = bee_observations_sf, color= "blue", size = .1, alpha = 0.3) +
  labs(title = "Distribution of fires and bee observations",
       x = "Longitude", 
       y = "Latitude") +
  theme_minimal() +
  coord_sf()

#Now its time to go fire by fire and determin how many bee observations are present in a 1km radius of the fire origin

# create a 1 km buffer around each fire point
fire_buffers <- st_buffer(fire_data_sf, dist = 1000)  # 1000 meters = 1 km

# spatial join to find bee observations within each buffer
bee_counts <- st_join(fire_buffers, bee_observations_sf, join = st_intersects)

# Count bee observations per fire
fire_bee_counts <- bee_counts %>%
  group_by(FireName) %>%  # Group by fire name or another identifier
  summarize(BeeCount = n())  # Counts the number of bee observations

#create df
fire_bee_counts_df <- as.data.frame(fire_bee_counts)

#list the top 10 fires with the most bee observations
top_fires <- bee_counts %>%
  group_by(FireName) %>%  # group by fire name 
  summarize(BeeCount = n()) %>%  # count the number of bee observations
  arrange(desc(BeeCount)) %>%  # now sort by BeeCount in descending order
  slice_head(n = 10)  # then select the top 10 fires

# View the top 10 fires
print(top_fires)















