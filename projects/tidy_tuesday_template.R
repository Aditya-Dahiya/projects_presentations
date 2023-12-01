# Loading required libraries----------------------------------------------------
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

# Loading the data--------------------------------------------------------------


# Exploratory Data Analysis-----------------------------------------------------


# Getting the data into a tidy form for plotting


# Visualization parameters------------------------------------------------------

# Load fonts
font_add_google("Rowdies", 
                family = "title_font",
                bold.wt = 900)               # Font for titles
font_add_google("Saira Extra Condensed", 
                family = "caption_font")     # Font for the caption
font_add_google("Roboto", 
                family = "body_font")        # Font for plot text
showtext_auto()

# Creating a Colour Palette for the Visualization
# Image to extract
img <- "https://pbs.twimg.com/profile_images/1605297940242669568/q8-vPggS_400x400.jpg"

# Number of Colours to have in the palette
col_numbers = 5
set.seed(3)
# Colour Palette
mypal <- get_colors(img) |> 
   make_palette(n = col_numbers)
mypal

# Define colours
low_col <- mypal[2]                   # Heat map: low colour
hi_col <- mypal[3]                    # Heat map: high colour
bg_col <- mypal[1]                    # Background Colour
text_col <- mypal[4]                  # Colour for the text
text_hil <- mypal[4]                  # Colour for highlighted text

# Define Text Size
ts = 24                              # Text Size

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
plot_title <- "..."

plot_subtitle <- "..."

plot_caption <- paste0("**Data:** ...", "**Graphics:** ", social_caption)

# Actual Plots------------------------------------------------------------------
ggplot() +
  labs(title = plot_title,
       caption = plot_caption,
       subtitle = plot_subtitle) +
  theme(
  plot.caption =  element_textbox(family = "caption_font",
                                  hjust = 1,
                                  colour = text_col,
                                  size = ts/2),
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


