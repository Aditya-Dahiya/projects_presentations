#=============== #TidyTuesday : 2023 & 2024 US Solar Eclipses =================#

#==============================================================================#
# Library Load-in---------------------------------------------------------------
#==============================================================================#

# Data Wrangling Libraries
library(tidyverse)      # Data Wrangling and Plotting
library(here)           # Files location and loading
library(janitor)        # Cleaning names etc. of messy dataset

# Mapping libraries
library(usmap)
library(sf)

# Data Visualization Libraries
library(showtext)       # Using Fonts More Easily in R Graphs
library(fontawesome)    # Social Media icons
library(ggtext)         # Markdown Text in ggplot2
library(scales)         # Scale Labels in ggplot2

# Other optional libraries
library(glue)           # To paste together text for ggtext
library(ggimage)        # Using Images in ggplot2
library(patchwork)      # For compiling plots
library(magick)         # Work with Images and Logos

#==============================================================================#
# Data Load-in------------------------------------------------------------------
#==============================================================================#

# Option 1: tidytuesdayR package 
tuesdata <- tidytuesdayR::tt_load(2024, week = 15)

ea23 <- tuesdata$eclipse_annular_2023
et24 <- tuesdata$eclipse_total_2024
ep23 <- tuesdata$eclipse_partial_2023
ep24 <- tuesdata$eclipse_partial_2024

rm(tuesdata)

# Random explorations
ep24 |> ggplot(aes(dur_pecl)) + 
  geom_histogram(bins = 200) +
  scale_x_continuous(limits = c(1990, 2200))

#==============================================================================#
# Exploratory Data Analysis & Data Wrangling -----------------------------------
#==============================================================================#

# library(summarytools)
# dfSummary(et24) |> view()

pet24 <- et24 |> 
  mutate(dur_tecl = as.numeric(eclipse_4 - eclipse_3)) |> 
  usmap_transform()

pea23 <- ea23 |> 
  mutate(dur_aecl = as.numeric(eclipse_4 - eclipse_3)) |> 
  usmap_transform()

both_eclipse <- et24 |> 
  filter(state == "TX") |> 
  inner_join(ea23, by = join_by(
    name == name,
    lat == lat,
    lon == lon
  )) |> 
  usmap_transform()

both_eclipse |> 
  mutate(
    dur2023 = eclipse_4.y - eclipse_3.y,
    dur2024 = eclipse_4.x - eclipse_3.x
  ) |> 
  ggplot(aes(dur2023, dur2024, label = name)) +
  geom_point() +
  geom_text()

#==============================================================================#
# Options & Visualization Parameters--------------------------------------------
#==============================================================================#

# Load fonts
font_add_google("Racing Sans One", 
                family = "title_font")       # Font for titles
font_add_google("Bowlby One SC",
                family = "subtext_font")     # Font for subtext
font_add_google("Saira Extra Condensed", 
                family = "caption_font")     # Font for the caption
font_add_google("Changa", 
                family = "body_font")        # Font for plot text
showtext_auto()

# Icons to use in graph
# Credits: Used code from

# Creating a Colour Palette for the Visualization
mypal <- paletteer::paletteer_d("nbapalettes::knicks_retro")
mypal

# Define colours
text_col <- "#0c2d56"               # Colour for the text
text_hil <- "#005eb9"               # Colour for highlighted text
bg_col <- "grey10"                  # Background Colour
lines_col <- "#717b85"              # Colour for lines and axes etc.

# Define Text Size
ts = unit(40, units = "cm")           # Text Size

# Caption stuff
sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf"))
github <- "&#xf09b"
github_username <- "aditya-dahiya"
xtwitter <- "&#xe61b"
xtwitter_username <- "@adityadahiyaias"
social_caption_2 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {text_col}'>{github_username}  </span>")
social_caption_1 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {text_col}'>{xtwitter_username}</span>")

# Add text to plot--------------------------------------------------------------
plot_title <- "NCAA Men's March Madness"
subtitle_text <- "Comparison of major teams' performance in NCAA Men Division-I basketball single-elimination tournament for the national championship."
plot_subtitle <- str_wrap(subtitle_text, 55)
plot_caption <- paste0("**Data & Inspiration:** Nishaan Amin (Bracketology: predicting March Madness) |  **Graphics:** ", social_caption_1, " |  **Code:**", social_caption_2)

#==============================================================================#
# Data Visualization------------------------------------------------------------
#==============================================================================#

g <- plot_usmap(
  fill = bg_col,
  colour = "grey75"
) +
  geom_sf(
    data = pet24,
    mapping = aes(
      colour = dur_tecl
    ),
    alpha = 0.5,
    size = 1.1
  ) +
  paletteer::scale_colour_paletteer_c(
    "ggthemes::Orange",
    labels = label_timespan(unit = c("secs")),
    name = "Duration of Total eclipse (2024)",
    direction = -1,
    breaks = seq(0, 300, 60),
    limits = c(0, 300)
  ) +
  ggnewscale::new_scale_colour() +
  geom_sf(
    data = pea23,
    mapping = aes(
      colour = dur_aecl
    ),
    alpha = 0.5,
    size = 1.5
  ) +
  paletteer::scale_colour_paletteer_c(
    "ggthemes::Blue-Green Sequential",
    labels = label_timespan(unit = c("secs")),
    name = "Duration of Annular eclipse (2023)",
    direction = -1,
    breaks = seq(0, 300, 60),
    limits = c(0, 300)
  ) +
  geom_sf(
    data = both_eclipse,
    shape = 1, 
    size = 5,
    colour = "grey90",
    alpha = 0.5
  )

#=============================================================================#
# Image Saving-----------------------------------------------------------------
#=============================================================================#

ggsave(
  filename = here::here("docs", "tidy_eclipse.png"),
  plot = g,
  width = 40, 
  height = 55, 
  units = "cm",
  bg = bg_col
)
  