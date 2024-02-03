#==============================================================================#
# Library Load-in---------------------------------------------------------------
#==============================================================================#
library(tidyverse)      # Data Wrangling and Plotting
library(here)           # Files location and loading
library(showtext)       # Using Fonts More Easily in R Graphs
library(ggimage)        # Using Images in ggplot2
library(fontawesome)    # Social Media icons
library(ggtext)         # Markdown Text in ggplot2
library(patchwork)      # For compiling plots
library(magick)         # Work with Images and Logos
library(scales)         # ggplot2 labels and scaling
library(sf)             # Maps and converting coordinates

#==============================================================================#
# Data Load-in------------------------------------------------------------------
#==============================================================================#

# Option 1: tidytuesdayR package 
tuesdata <- tidytuesdayR::tt_load('2024-01-30')

object.size(tuesdata)

groundhogs <- tuesdata$groundhogs

rm(tuesdata)

# Using predictions data from 2024 (latest) with the cleaning script given 
# at https://github.com/rfordatascience/tidytuesday/blob/master/data/2024/2024-01-30/readme.md
# Credits: @tidytuesday and @jonthegeek 

predictions <- read_csv(here::here("data", "predictions2024.csv"))

#==============================================================================#
# Exploratory Data Analysis-----------------------------------------------------
#==============================================================================#

library(summarytools)
dfSummary(groundhogs) |> view()

dfSummary(predictions) |> view()

# Checking number of predictions by year (to see if animated map is an option)
predictions |>
  select(year, shadow) |>
  drop_na() |> 
  count(year, sort = TRUE) |> 
  ggplot(aes(year, n)) +
  geom_line()

#==============================================================================#
# Data Wrangling----------------------------------------------------------------
#==============================================================================#

library("rnaturalearth")
library("rnaturalearthdata")
library(usmap)
library(usmapdata)

inclregionsnames <- c(
  "New York",
  "Rhode Island",
  "Connecticut",
  "Pennsylvania",
  "Ohio",
  "West Virginia",
  "Maryland"
)

inclregions <- tibble(
  abbr = state.abb,
  full = state.name
) |> 
  filter(full %in% inclregionsnames) |> 
  pull(abbr)


# Getting a map of USA and Canada
northamerica <- ne_countries(scale = "large", 
                      returnclass = "sf",
                      continent = "North America") |> 
  filter(sovereignt %in% c("Canada", "United States of America")) |> 
  filter(gu_a3 %in% c("USA", "CAN")) |> 
  st_transform(crs = 5070) 

object.size(northamerica)

# See if I can plot a map of groundhogs

relevant_groundhogs <- predictions |> 
  filter(year == 2024) |> 
  pull(id) |> 
  unique()

df1 <- groundhogs |> 
  filter(id %in% relevant_groundhogs) |> 
  select(id, name, latitude, longitude, image, region) |> 
  left_join(predictions |> 
            filter(year == 2024)) |> 
  mutate(predict = if_else(shadow,
                           "Groundhog saw its shadow: Extended Winters",
                           "No shadow: An Early Spring!")) |> 
  select(-c(shadow, details, year)) |> 
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
    ) |> 
  st_transform(crs = 5070) |> 
  drop_na() |> 
  mutate(insetvar = region %in% inclregionsnames)

image1 <- image_read("https://img.freepik.com/free-vector/adorable-groundhog-cartoon-with-groundhog-day-banner_1308-153480.jpg")



#==============================================================================#
# Options & Visualization Parameters--------------------------------------------
#==============================================================================#

# Load fonts
font_add_google("Alfa Slab One", 
                family = "title_font")       # Font for titles
font_add_google("Saira Extra Condensed", 
                family = "caption_font")     # Font for the caption
font_add_google("Righteous", 
                family = "body_font")        # Font for plot text
showtext_auto()

# Icons to use in graph
# Credits: Used code from

# Creating a Colour Palette for the Visualization
mypal <- c("#0ab6f0", "#00990a")
mypal

# Define colours
bg_col <- "#ebfaff"                   # Background Colour
text_col <- "#4f2b00"                 # Colour for the text
text_hil <- "#c46c00"                 # Colour for highlighted text


# Define Text Size
ts = unit(40, units = "cm")                             # Text Size

# Caption stuff
sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf"))
github <- "&#xf09b"
github_username <- "aditya-dahiya"
xtwitter <- "&#xe61b"
xtwitter_username <- "@adityadahiyaias"
social_caption <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_col}'>{github_username}  </span> <span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_col}'>{xtwitter_username}</span>")

# Add text to plot--------------------------------------------------------------
plot_title <- "Groundhog Day: 2024"

subtitle_text <- "Predictions of an early Spring! A map of North America showing the locations of Groundhogs and their predictions in 2024. The inset shows the North-Eastern States - where most of these are located."
plot_subtitle <- paste(strwrap(subtitle_text, 120), collapse = "\n")

plot_caption <- paste0("**Data & Inspiration:** groundhog-day.com | ", "**Graphics:** ", social_caption)

#==============================================================================#
# Data Visualization------------------------------------------------------------
#==============================================================================#

library(patchwork)

g1 <- ggplot() +
  geom_sf(
    data = northamerica, 
    fill = "white"
  ) +
  geom_sf(
    data = df1,
    aes(color = predict),
    size = 5,
    alpha = 0.5
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption,
    size = NULL,
    color = NULL
  ) +
  scale_colour_manual(
    values = mypal
    ) +
  guides(
    colour = guide_legend(
      override.aes = list(
        size = 9
      )
    )
  ) +
  ggthemes::theme_map() +
  theme(
    legend.position = c(0, 0),
    plot.caption =  element_textbox(family = "caption_font",
                                    hjust = 0.5,
                                    colour = text_col,
                                    size = 1.5 * ts),
    plot.title   =     element_text(hjust = 0.5,
                                    size = 7.5 * ts,
                                    family = "title_font",
                                    face = "bold",
                                    colour = text_hil,
                                    margin = margin(0,0,0.5,0,
                                                    unit = "cm")),
    plot.subtitle    = element_text(hjust = 0.5,
                                    size = 1.6 * ts,
                                    family = "body_font",
                                    colour = text_col,
                                    margin = margin(0,0,0,0,
                                                    unit = "cm"),
                                    lineheight = 0.35),
    plot.background =  element_rect(fill = bg_col,
                                    colour = bg_col,
                                    linewidth = 0),
    plot.title.position = "plot",
    legend.text = element_text(size = 1.5 * ts,
                               family = "body_font",
                               colour = text_col,
                               margin = margin(0,0,0,0),
                               hjust = 0),
    legend.key = element_rect(fill = bg_col),
    legend.background = element_rect(fill = bg_col),
    legend.box = "horizontal"
    )

# Additional Inset Map of NE USA with Groundhog photos

g2 <- plot_usmap(
  include = inclregions,
  regions = "states",
  labels = TRUE,
  label_color = "grey"
  ) +
  geom_sf(
    data = df1 |> filter(insetvar) |> st_transform(crs = 2163),
    aes(
      colour = predict
      ),
    size = 5,
    alpha = 0.5
  ) +
  scale_colour_manual(
    values = mypal
  ) +
  guides(colour = "none") +
  labs(title = "North-Eastern USA") +
  theme(
    plot.title = element_text(
      size = ts * 2,
      colour = text_col,
      hjust = 0.5,
      family = "body_font",
      margin = margin(0,0,0,0)
    )
  )

g3 <- df1 |> 
  count(predict) |> 
  select(-geometry) |>
  as_tibble() |> 
  mutate(percentage = round(100 * n / sum(n), 1)) |> 
  mutate(predict = if_else(
    predict == "Groundhog saw its shadow: Extended Winters",
    "A long Winter",
    "Early Spring"
    )) |> 
  ggplot(aes(
    x = "",
    y = n,
    fill = predict,
    label = predict
  )) +
  geom_bar(
    stat = "identity",
    color = bg_col) +
  geom_text(
    aes(label = paste0(percentage, " %")),
    position = position_stack(vjust = 0.7),
    size = 18,
    family = "body_font",
    col = "white"
  ) +
  geom_text(
    aes(label = predict),
    position = position_stack(vjust = 0.4),
    size = 13,
    family = "body_font",
    col = "white"
  ) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = mypal) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Predictions so far") +
  theme(
    plot.title = element_text(
      size = ts * 2,
      colour = text_col,
      hjust = 0.5,
      family = "body_font",
      margin = margin(0,0,0,0)
    )
  )

g <- g1 +
  inset_element(
    g2,
    0, 0.09, 0.4, 0.5,
    align_to = "plot",
    ignore_tag = TRUE
  ) +
  inset_element(
    g3,
    0, 0.55, 0.4, 0.9,
    align_to = "plot",
    ignore_tag = TRUE
  ) & 
  theme(
    plot.background = element_rect(fill = bg_col,
                                   colour = bg_col,
                                   linewidth = 0)
  )
#=============================================================================#
# Image Saving-----------------------------------------------------------------
#=============================================================================#


ggsave(
  filename = here::here("docs", "tidy_groundhog.png"),
  plot = g,
  width = 40, 
  height = 31, 
  units = "cm",
  bg = bg_col
)
