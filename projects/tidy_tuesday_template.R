# Loading required libraries----------------------------------------------------
library(tidyverse)      # Data Wrangling and Plotting
library(sf)             # Maps and plotting
library(here)           # Files location and loading
library(showtext)       # Using Fonts More Easily in R Graphs
library(ggimage)        # Using Images in ggplot2
library(fontawesome)    # Social Media icons
library(ggtext)         # Markdown Text in ggplot2
library(patchwork)      # For compiling plots
library(figpatch)       # Images in patchwork
library(ggfittext)      # Fitting text inside boxes in ggplot2

# Loading the data--------------------------------------------------------------


# Exploratory Data Analysis-----------------------------------------------------


# Getting the data into a tidy form for plotting


# Visualization parameters------------------------------------------------------

# Load fonts
font_add_google("...", "title_font")       # Font for titles
font_add_google("...", "caption_font")     # Font for the caption
font_add_google("...", "body_font")        # Font for plot text
showtext_auto()

# Define colours
low_col <- "lightgrey"               # Heat map: low colour
hi_col <- "darkgrey"                 # Heat map: high colour
bg_col <- "white"                    # Background Colour
text_col <- "black"                  # Colour for the text
text_hil <- "red"                    # Colour for highlighted text

# Define Text Size
ts = 24                              # Text Size

# Caption stuff
sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf"))
github <- "&#xf09b"
github_username <- "aditya-dahiya"
xtwitter <- "&#xe61b"
xtwitter_username <- "@adityadahiyaias"
mastodon <- "&#xf4f6"
mastodon_username <- "@adityadahiya@mastodon.social"
social_caption <- glue::glue(
  "<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: #000000'>{github_username}  </span>
  <span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: #000000'>{xtwitter_username}</span>"
)

# Add text to plot--------------------------------------------------------------
plot_title <- "..."

plot_subtitle <- "..."

plot_caption <- paste0("...", social_caption)

# Actual Plots------------------------------------------------------------------


# Compiling Plot----------------------------------------------------------------
my_theme <- theme(
  plot.caption =  element_textbox(family = "caption_font",
                                  hjust = 1,
                                  colour = text_col),
  plot.title   =     element_text(hjust = 0.5,
                                  size = ts,
                                  family = "title_font",
                                  face = "bold",
                                  colour = text_col),
  plot.subtitle    = element_text(hjust = 0.5,
                                  size = ts/1.5,
                                  family = "body_font",
                                  colour = text_col),
  plot.background =  element_rect(fill = bg_col,
                                  color = bg_col,
                                  linewidth = 0)
)

# Saving the final image--------------------------------------------------------
ggsave(
  filename = here::here("docs", "....png"),
  plot = g,
  device = "png", dpi = "retina", 
  width = 10, height = 7, units = "cm",
  bg = bg_col)
)


