#==============================================================================#
# Library Load-in---------------------------------------------------------------
#==============================================================================#
library(tidyverse)      # Data Wrangling and Plotting
library(here)           # Files location and loading
library(summarytools)   # Exploratory Data Analysis
library(colorfindr)     # To get colour palettes for the Viz
library(showtext)       # Using Fonts More Easily in R Graphs
library(ggimage)        # Using Images in ggplot2
library(fontawesome)    # Social Media icons
library(ggtext)         # Markdown Text in ggplot2
library(patchwork)      # For compiling plots
library(figpatch)       # Images in patchwork
library(magick)         # Work with Images and Logos
library(ggimage)        # Background Image
library(scales)         # ggplot2 labels and scaling

#==============================================================================#
# Data Load-in------------------------------------------------------------------
#==============================================================================#

# Option 1: tidytuesdayR package 

tuesdata <- tidytuesdayR::tt_load(2024, week = 3)

polling_places <- tuesdata$polling_places
rm(tuesdata)

finaldf <- read_csv(here::here("data", "uspolling.csv"))
plotdf3 <- read_csv(here::here("data", "uspolling1.csv"))

# Data-frame / Tibble for actual visualization

plotdf4 <- plotdf3 |> 
  filter(
    (lat > 0 & lat < 90) &
      (long > -180 & long < -67)
  ) |> 
  rename(lon = long) |> 
  usmap::usmap_transform()

df <- polling_places |>
  mutate(year = year(election_date)) |> 
  distinct(year, address)

for (i in seq(2012, 2020, 2)) {
  temp <- df |> 
    filter(year == i) |> 
    pull(address) |> 
    unique()
  
  assign_name <- paste0("df_", i)
  
  assign(assign_name, temp)
  
}
rm(temp)
rm(assign_name)
rm(i)

# A tibble of only polling stations which are new from previous elections
newdf <- df |> 
  mutate(
    new_poll = case_when(
      ((year == 2014) & !(address %in% df_2012)) ~ TRUE,
      ((year == 2016) & !(address %in% c(df_2012, df_2014))) ~ TRUE,
      ((year == 2018) & !(address %in% c(df_2012, df_2014, df_2016))) ~ TRUE,
      ((year == 2020) & !(address %in% c(df_2012, df_2014, df_2016, df_2018))) ~ TRUE,
      .default = FALSE
    )
  ) |> 
  filter(new_poll) |> 
  select(-new_poll) |> 
  count(year)

rm(df_2012)
rm(df_2014)
rm(df_2016)
rm(df_2018)
rm(df_2020)

totaldf <- df |> 
  count(year)

#==============================================================================#
# Exploratory Data Analysis:  Not to Re-Run ------------------------------------
#==============================================================================#

# Aiming to geocode and reverse geocode the entries using 
# tidygeocoder R package !

# names of variables in data

names(polling_places)


# Finding number of locations each year

polling_places |> 
  mutate(year = year(election_date)) |> 
  distinct(year, address) |> 
  count(year)


#==============================================================================#
# Data Wrangling : Not to Re-Run -----------------------------------------------
#==============================================================================#
# Find new polling locations for each year
# Restrict analysis to in-person polling stations

polling_places |> 
  count(location_type)

df <- polling_places |>
  mutate(year = year(election_date)) |> 
  distinct(year, address)

for (i in seq(2012, 2020, 2)) {
  temp <- df |> 
    filter(year == i) |> 
    pull(address) |> 
    unique()
  
  assign_name <- paste0("df_", i)
  
  assign(assign_name, temp)
  
}
rm(temp)
rm(assign_name)
rm(i)

# Saving data on new polling Stations in West Virginia
# Now, using tidygeocoder to get latitude and longitude of the addresses
library(tidygeocoder)
# Using Census Data method for geocoding addresses
plotdf1 <- plotdf |> 
  geocode(
    address = address,
    method = "census",
    verbose = FALSE)

# Using Open Street Maps to Geocode Addresses
plotdf2 <- plotdf |> 
  geocode(
    address = address,
    method = "osm",
    verbose = FALSE)

finaldf <- plotdf1  |> 
  full_join(plotdf2, 
            by = c("year", "address"), 
            suffix = c("_df1", "_df2")) |> 
  mutate(lat = coalesce(lat_df1, lat_df2),
         long = coalesce(long_df1, long_df2)) |> 
  select(-lat_df1, -long_df1, -lat_df2, -long_df2) |> 
  drop_na()

# To save on computation time, save the final database results in a csv
write_csv(
  x = finaldf,
  file = here::here("data", "uspolling.csv")
)


# A tibble of only polling stations which are new from previous elections

plotdf <- df |> 
  mutate(
    new_poll = case_when(
      ((year == 2014) & !(address %in% df_2012)) ~ TRUE,
      ((year == 2016) & !(address %in% c(df_2012, df_2014))) ~ TRUE,
      ((year == 2018) & !(address %in% c(df_2012, df_2014, df_2016))) ~ TRUE,
      ((year == 2020) & !(address %in% c(df_2012, df_2014, df_2016, df_2018))) ~ TRUE,
      .default = FALSE
    )
  ) |> 
  filter(new_poll) |> 
  select(-new_poll)

# Finding Common Words in Addresses of US Polling Stations
library(tidytext)

common_words <- plotdf |> 
  unnest_tokens(output = "word",
                input = "address") |> 
  count(word, sort = TRUE) |> 
  filter(word %in% c("church", "center", "school", "hall"))

finaldf <- plotdf |> 
  mutate(address = str_to_lower(address)) |> 
  mutate(
    type = case_when(
      str_detect(address, "church") ~ "Church",
      str_detect(address, "school") ~ "School",
      .default = NA
    )
  ) |> 
  drop_na()

finaldf |> 
  count(type)

# Now, using tidygeocoder to get latitude and longitude of the addresses
library(tidygeocoder)

# Using Census Data method for geocoding addresses
plotdf1 <- finaldf |> 
  geocode(
    address = address,
    method = "census",
    verbose = FALSE)

# Using Open Street Maps to Geocode remaining Addresses
plotdf2 <- plotdf1 |> 
  filter(is.na(lat)) |> 
  geocode(
    address = address,
    method = "osm",
    verbose = FALSE)

nrow(finaldf)
nrow(plotdf1)
nrow(plotdf2)

plotdf2 <- plotdf2 |> 
  rename(lat = lat...6,
         long = long...7) |> 
  select(-c(lat...4, long...5))

plotdf1
plotdf2

plotdf3 <- plotdf1  |> 
  full_join(plotdf2, 
            by = c("year", "address"), 
            suffix = c("_df1", "_df2")) |> 
  mutate(lat = coalesce(lat_df1, lat_df2),
         long = coalesce(long_df1, long_df2)) |> 
  select(-lat_df1, -long_df1, -lat_df2, -long_df2, -type_df2) |> 
  rename(type = type_df1) |> 
  drop_na()


plotdf3

# To save on computation time, save the final database results in a csv
write_csv(
  x = plotdf3,
  file = here::here("data", "uspolling1.csv")
)

plotdf3

#==============================================================================#
# Load curated dataset, cleaned and wrangled -----------------------------------
#==============================================================================#






#==============================================================================#
# Options & Visualization Parameters--------------------------------------------
#==============================================================================#

# Load fonts
font_add_google("Sirin Stencil", 
                family = "title_font")       # Font for titles
font_add_google("Saira Extra Condensed", 
                family = "caption_font")     # Font for the caption
font_add_google("Strait", 
                family = "body_font")        # Font for plot text
showtext_auto()

# Icons to use in graph
# Credits: Used code from

# Creating a Colour Palette for the Visualization
mypal <- paletteer::paletteer_d("nord::victory_bonds")
mypal

# Define colours
school <- "#5773CCFF" 
church <- "#FFB900FF" 
bg_col <- "#F0F2F8FF"                   # Background Colour
text_col <- "#010440"                 # Colour for the text
text_hil <- "#010440"                 # Colour for highlighted text

# Define Text Size
ts = unit(20, units = "cm")                             # Text Size

# Caption stuff
sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf"))
github <- "&#xf09b"
github_username <- "aditya-dahiya"
xtwitter <- "&#xe61b"
xtwitter_username <- "@adityadahiyaias"
linkedin <- "&#xf08c"
linkedin_username <- "dr-aditya-dahiya-ias"
social_caption <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_col}'>{github_username}  </span> <span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_col}'>{xtwitter_username}</span> <span style='font-family:\"Font Awesome 6 Brands\";'>{linkedin};</span> <span style='color: {text_col}'>{linkedin_username}</span>")

# Add text to plot--------------------------------------------------------------
plot_title <- "New Polling Sites in US Elections !"

subtitle_text <- "In the last 5 elections, the United States has introduced over 94,000 new polling sites, most of them in the 2020 elections. Here, we look at these new sites located in Churches or Schools. Most of these new polling sites are concentrated in a few States. Further, more Church-based new polling sites occur in the South-East, while more School-based new polling sites lie in the North-East."
plot_subtitle <- paste(strwrap(subtitle_text, 130), collapse = "\n")

plot_caption <- paste0("**Data & Inspiration:** Center for Public Integrity | ", "**Graphics:** ", social_caption)

#==============================================================================#
# Data Visualization------------------------------------------------------------
#==============================================================================#

g <- 
  usmap::plot_usmap(
    col = "darkgrey",
    fill = "white"
  ) +
  geom_point(
    data = plotdf4,
    mapping = aes(
      x = x, 
      y = y,
      col = type,
      shape = type),
    alpha = 0.8
  ) +
  scale_color_manual(values = c(church, school)) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption,
    color = NULL,
    shape = NULL
  ) +
  theme(
  legend.position = c(0.7, 0.07),
  plot.caption =  element_textbox(family = "caption_font",
                                  hjust = 0.5,
                                  colour = text_col,
                                  size = 1.5 * ts),
  plot.title   =     element_text(hjust = 0.5,
                                  size = 6*ts,
                                  family = "title_font",
                                  face = "bold",
                                  colour = text_hil,
                                  margin = margin(4,0,2,0)),
  plot.subtitle    = element_text(hjust = 0.5,
                                  size = 1.6 * ts,
                                  family = "body_font",
                                  colour = text_col,
                                  margin = margin(5,0,2,0),
                                  lineheight = 0.35),
  plot.background =  element_rect(fill = bg_col,
                                  color = bg_col,
                                  linewidth = 0),
  plot.title.position = "plot",
  legend.text = element_text(size = 2 * ts,
                             family = "body_font",
                             colour = text_col,
                             margin = margin(0,0,0,0),
                             hjust = 0),
  legend.key = element_rect(fill = bg_col,
                            colour = bg_col),
  legend.background = element_rect(fill = bg_col)
)

#=============================================================================#
# Image Saving-----------------------------------------------------------------
#=============================================================================#


ggsave(
  filename = here::here("docs", "tidy_uspolling.png"),
  plot = g,
  width = 20, 
  height = 20, 
  units = "cm",
  bg = bg_col
)
