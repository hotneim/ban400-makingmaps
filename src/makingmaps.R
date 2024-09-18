# Making maps --------------
# Lesson for BAN400 --------

library(tidyverse)
library(here)

# Two points
points <- tibble(x = c(5.209806, 5.205498),
                 y = c(59.92981, 59.92997))
map <- points %>% ggplot(aes(x, y)) + geom_point(size = 4, 
                                                 colour = "red", 
                                                 alpha = .5)
map

# Adding the line
map + geom_line()

# Read the data on the road and plot it
road <- here("data", "road.csv") %>% read_csv(show_col_types = F)
map + geom_point(aes(x, y), data = road, size = .7, alpha = .2)

# As a line:
map + geom_line(aes(x, y), data = road, alpha = .6)

# The distance between the points
geosphere::distm(points[1,], points[2,])

# Read the data on the shape of the island, draw the map
island <- here("data", "island.csv") %>% read_csv(col_names = c('x', 'y'), 
                                                  col_select = c(1, 2),
                                                  show_col_types = F)
island_map <-
  map + 
  geom_point(aes(x, y), data = road, size = .5, alpha = .1) +
  geom_point(aes(x, y), data = island, size = .5, alpha = .1)
island_map

# Load the sf-package
library(sf)

# Set the coordinate reference system
crs <- 4326

# Make the conversion
points_sf <- st_as_sf(points, 
                      coords = c("x", "y"),
                      crs = crs)

# This results in a data frame with one column called "geometry" and of type POINT:
points_sf

# The sf package comes with a special ggplot function that is incredibly useful:
map_sf <- points_sf %>% ggplot() + geom_sf(aes(geometry = geometry))
map_sf

# Conver the row
road_sf <- 
  # Convert to points
  st_as_sf(road, 
           coords = c("x", "y"), 
           crs = crs) %>% 
  # Merge into a single geometry (one road is one row in the data frame)
  summarise(do_union = FALSE) %>% 
  # Convert to a line
  st_cast("LINESTRING") 

# Add the road to the plot
map_sf + geom_sf(aes(geometry = geometry), data = road_sf)
# We do the same with the island

island_sf <- 
  # Convert to points
  st_as_sf(island, 
           coords = c("x", "y"), 
           crs = crs) %>% 
  # Merge into a single geometry (one road is one row in the data frame)
  summarise(do_union = FALSE) %>% 
  # Convert to a polygon
  st_cast("POLYGON") 

# Everything in one map
map_sf + 
  geom_sf(aes(geometry = geometry), data = road_sf) +
  geom_sf(aes(geometry = geometry), fill = "#00000020", data = island_sf)

# Very nice! Let us wrap up this case by finishing up the plot:
island_sf %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(fill = "gray") +
  geom_sf(aes(geometry = geometry), data = road_sf, linetype = "dotted") +
  geom_sf(aes(geometry = geometry), data = points_sf, shape = 3, size = 2) +
  theme_minimal() + 
  theme(axis.text = element_text(color = "gray"))

# Load the packages that we need for the next section
library(rnaturalearth)
library(rnaturalearthdata)

# Create the data
world <- ne_countries(scale = "medium", returnclass = "sf")

# List of the columns:
str(world)

# Notice again that there is a geometry column, that we can use to create a
# basic world map:
world %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf()

# Only Europe:
world %>% 
  filter(region_un == "Europe") %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf()

# Zooming in on Europe instead
world %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf() +
  coord_sf(xlim = c(-20, 45), ylim = c(35, 73))

# Here we clip the data instead of zooming the plot
world %>% 
  st_crop(xmin = -20, xmax = 45, ymin = 35, ymax = 73) %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf()

# Refining:
world %>% 
  st_transform(crs = crs) %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf() +
  coord_sf(xlim = c(-20, 45), ylim = c(35, 73)) + 
  theme_minimal() + 
  theme(axis.text = element_text(color = "gray")) 

# Data for plotting
library(gapminder)
head(gapminder)

# Joining the data into the map data using left_join()
gdp <- 
  world %>% 
  # The name of the country name column must match
  mutate(country = admin) %>% 
  # We try to join, keeping only the countries that we have in the gapminder data
  left_join(gapminder, by = "country") %>% 
  # We only use the latest year of data
  filter(year == 2007) %>% 
  # Drop the columns that we do not need, let us plot the GDP per capita for 2007
  select(country, gdpPercap, geometry)

# Countries with gdp data with no match in the country data:
no_match <- 
  anti_join(gapminder, gdp, by = "country") %>% 
  select(country) %>% 
  distinct()
no_match

# Using auto_merge from the countries-package instead
gdp2 <- 
  countries::auto_merge(gapminder %>% filter(year == 2007), world) %>% 
  select(name, gdpPercap, geometry)

# Works very well, did not introduce any new NAs:
gapminder %>% 
  filter(year == 2007) %>%
  select(gdpPercap) %>% 
  drop_na() %>% 
  nrow

gdp2 %>% 
  select(gdpPercap) %>% 
  drop_na() %>% 
  nrow

# Mapping gdp to the fill aesthetic:
gdp2 %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = gdpPercap))

# Finishing off the plot:
gdp2 %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = gdpPercap/1000)) + # Divide by 1000 for easier unit
  scale_fill_gradient(low = "white", high = "darkgreen", na.value = "lightgray") +
  ggtitle("GDP per Capita ($1000)") +
  theme_minimal() +
  labs(fill = "") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1, 'cm'),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) 
 
# Next example: Using official data. You need to download this yourself. 
# Check layers
st_layers(here("data", "municipalities", "Kommuner.geojson"))

# Read data
munic <- 
  st_read(here("data", "municipalities", "Kommuner.geojson"),
          layer = "Kommuner") %>% 
  st_transform(crs = crs)

# Plot the data
munic %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf()

# Make a box covering a smaller area; in principle the same operation as when
# creating the island in the beginning:
bergen_box <- 
  # First, we define just a small data frame with the coordinates of the box
  tibble(lon = c(4.5, 4.5, 6.3, 6.3), lat = c(59.7, 60.7, 60.7, 59.7)) %>% 
  # Then we convert this to a geometry columns as we did above with the island and the road:
  st_as_sf(coords = c("lon", "lat"),
           crs = crs) %>% 
  # Merge into a single geometry 
  summarise(do_union = FALSE) %>% 
  # Convert to a polygon
  st_cast("POLYGON") 

# Plot the box on top of the municipality data
munic %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf() +
  geom_sf(data = bergen_box)

# Make the intersection
munic_small <- munic %>% st_intersection(bergen_box)

# Plot the smaller municipality data
munic_small %>% 
  ggplot() +
  geom_sf()

# Load voting data
votes <- read_delim(here("data", "votes.csv"),
                    delim = ";",
                    skip = 3,
                    col_names = c("municipality", "party", "votes"), 
                    # This file contains special Norwegian characters. 
                    # Let us be specific about the encoding of this text to ensure that 
                    # they survive the reading operation. (Try without and see what you get)
                    locale = locale(encoding = "latin1")) %>% 
  filter(votes != 0)

# Split the municipality column so that we have the municipality number in a
# separate column
votes_split <-
  votes %>% 
  separate(municipality,
           sep = " ",
           into = c("number", "municipality"),
           extra = "merge") %>% 
  # Convert the municipality number to a numeric column
  mutate(number = as.integer(number))

# Here are all the unique municipality names -- looks good and I think we were smart not to rely on merging by these as they contain the sami (and in some cases other languages as well) spelling in a very specific way. 
unique(votes_split$municipality)

# Find the winning party in each municipality
winners <- 
  munic_small %>% 
  # Rename column so that we can join
  rename("number" = nummer) %>%
  left_join(votes_split, by = "number") %>% 
  # Quick way to find the winning party in each municipality: group, arrange and pick the top one
  group_by(municipality) %>%
  arrange(desc(votes)) %>% 
  slice(1)

# Basic plot
winners %>% 
  ggplot(aes(fill = party, geometry = geometry)) +
  geom_sf()

# Interesting! We see that there are four different parties who won the largest share of votes in the respective municipalities in this area. The color code is a bit off though because there is a largely accepted color code for the Norwegian political parties that we have to map manually. See here for a full list of colors recognized by R: https://r-charts.com/colors/
winners %>% 
  ggplot(aes(fill = party, geometry = geometry)) +
  geom_sf() +
  scale_fill_manual(values = c("Høyre" = "lightblue3", 
                               "Fremskrittspartiet" = "royalblue4",
                               "Arbeiderpartiet" = "coral",
                               "Senterpartiet" = "darkolivegreen4")) +
  labs(fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
