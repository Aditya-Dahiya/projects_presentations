
# Busiest Airports Dataset
# Credits: London Datastore
# https://data.london.gov.uk/dataset/busiest-airports-by-passenger-traffic  
url1 <- "https://data.london.gov.uk/download/busiest-airports-by-passenger-traffic/21e66d4f-1efc-44b9-bc7f-20cb183e2bf8/largest-global-airports-passenger-traffic.xlsx"

# Data on Airports
# R Package airportr
# https://cran.r-project.org/web/packages/airportr/vignettes/Introduction_to_Airportr.html  

library(airportr)
library(tidyverse)
library(sf)
library(rnaturalearth)

# Names of airports around the world
df <- airportr::airports |> 
  janitor::clean_names() |> 
  select(name, iata, city, country, 
         cc2 = country_code_alpha_2, latitude,
         lat = latitude, lon = longitude) |> 
  rename(airport = name) |> 
  mutate(latitude = lat, longitude = lon) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)


# List of 10 longest flights in the world
longest_flights <- tibble(
  from = c("JFK", "EWR", "AKL", "LHR", "DFW",
           "AKL", "AKL", "LAX", "BLR", "IAH"),
  to = c("SIN", "SIN", "DOH", "PER", "MEL",
         "JFK", "DXB", "SIN", "SFO", "SYD"),
  airlines = c("Singapore Airlines", "Singapore Airlines",
               "Qatar Airways", "Qantas Airways",
               "Qantas Airways", "Air New Zealand",
               "Emirates", "Singapore Airlines", 
               "Air India", "United Airlines"),
  distance = c(15332, 15329, 14526, 14499, 14468,
               14209, 14193, 14096, 13982, 13829)
)


# Extracting the busiest airports in the world
library(rvest)
# Define the URL of the Wikipedia page
url <- "https://en.wikipedia.org/wiki/List_of_busiest_airports_by_passenger_traffic"
# Read the HTML content of the page and
# Extract the table with the 2023 statistics
busiest_airports <- read_html(url) |> 
  html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>% 
  html_table(fill = TRUE) |> 
  as_tibble() |> 
  janitor::clean_names() |> 
  separate_wider_delim(
    cols = code_iata_icao,
    delim = "/",
    names = c("iata", "icao")
  ) |> 
  select(-rankchange, -percent_change) |> 
  mutate(totalpassengers = parse_number(totalpassengers))


# Plotting the longest routes in the world
longest_routes <- longest_flights |> 
  left_join(df |> select(from = iata, 
                         from_lat = latitude, 
                         from_lon = longitude) |> 
              as_tibble() |> 
              select(-geometry)) |> 
  left_join(df |> select(to = iata, 
                         to_lat = latitude, 
                         to_lon = longitude) |> 
              as_tibble() |> 
              select(-geometry))

longest_route_airports <- unique(longest_routes$from, longest_routes$to)


# Technique Credits: https://github.com/xmc811/flightplot/blob/master/R/main.R
# Credits: Mingchu Xu
# https://www.linkedin.com/in/mingchu-xu-467a0946/
# On Twitter: @xmc811
routes <- geosphere::gcIntermediate(
  p1 = longest_routes |> select(from_lon, from_lat),
  p2 = longest_routes |> select(to_lon, to_lat),
  n = 1000,
  breakAtDateLine = TRUE,
  addStartEnd = TRUE,
  sp = TRUE) |> 
  sf::st_as_sf()

ggplot() +
  geom_sf(
    data = ne_countries(returnclass = "sf") |> 
      filter(sovereignt != "Antarctica"),
    fill  = "white"
  ) +
  geom_sf(
    data = routes
  ) +
  coord_sf(crs = "ESRI:54030") +
  ggthemes::theme_map()


# Final Plot: A Composite world map with all Airports and 
# the busiest Airports in big size (with name and annual passengers)
# And, in a different colour, the 10 longest flights

# Joining Airports Dataset with the busiest airports dataset
plotdf <- df |> 
  left_join(busiest_airports) |> 
  mutate(
    busiest = iata %in% busiest_airports$iata,
    longest = iata %in% longest_route_airports,
    alpha_var = iata %in% c(busiest_airports$iata,
                        longest_route_airports)
  ) |> 
  mutate(totalpassengers = replace_na(totalpassengers, 1e7))

ggplot() +
  geom_sf(
    data = ne_countries(returnclass = "sf") |> 
      filter(sovereignt != "Antarctica"),
    fill  = "white"
  ) +
  geom_sf(
    data = plotdf,
    mapping = aes(
      colour = busiest,
      size = alpha_var,
      alpha = alpha_var
    ),
    pch = 20
  ) +
  geom_sf(
    data = routes,
    alpha = 0.5,
    linewidth = 1,
    colour = "darkgrey"
  ) +
  scale_alpha_manual(values = c(0.2, 0.5)) +
  scale_size_manual(values = c(1, 3)) +
  scale_colour_manual(values = c("black", "red")) +
  coord_sf(crs = "ESRI:54030") +
  ggthemes::theme_map()
