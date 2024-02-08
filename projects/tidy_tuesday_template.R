#=================An Attempt at Shadowed Designer Bar Chart====================#

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
library(figpatch)       # Images in patchwork
library(magick)         # Work with Images and Logos
library(rnaturalearth)  # Maps of countries
library(colorspace)     # Lighten and darken Colours


#==============================================================================#
# Data Load-in------------------------------------------------------------------
#==============================================================================#

# Option 1: tidytuesdayR package 
## install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load(2024, week = 6)
heritage <- tuesdata$heritage
rm(tuesdata)
#==============================================================================#
# Exploratory Data Analysis-----------------------------------------------------
#==============================================================================#

heritage

#==============================================================================#
# Data Wrangling----------------------------------------------------------------
#==============================================================================#

# Long format data
df <- heritage |> 
  pivot_longer(cols = -country,
               names_to = "year",
               values_to = "n_sites") |> 
  mutate(country = str_to_lower(country))

# List of coutnries to plot
plot_cons <- df |> 
  pull(country) |> 
  unique() |> 
  str_to_lower() |> 
  sort()

# Getting map polygons of the countries
map_df <- ne_countries(
  scale = "medium", 
  returnclass = "sf",
  country = plot_cons
)

df_cord <- tibble(
  country = fct(plot_cons, levels = plot_cons),
  c_code = c("DK", "NO", "SE"), 
  x = c(8, 9, 19),
  y = c(57, 62, 67),
  col = c("red", "darkblue", "yellow")
)
#==============================================================================#
# Options & Visualization Parameters--------------------------------------------
#==============================================================================#

# Load fonts
font_add_google("Graduate", 
                family = "title_font")       # Font for titles
font_add_google("Saira Extra Condensed", 
                family = "caption_font")     # Font for the caption
font_add_google("Oswald", 
                family = "body_font")        # Font for plot text
showtext_auto()

# Icons to use in graph
# Credits: Used code from

# Creating a Colour Palette for the Visualization
mypal <- paletteer::paletteer_d("nord::victory_bonds")
mypal

# Define colours
low_col <- "#fa0000"                  # Heat map: low colour
hi_col <- "#0800fa"                   # Heat map: high colour
bg_col <- "#e8e8e8"                   # Background Colour
text_col <- "#00004d"                 # Colour for the text
text_hil <- "#700000"                 # Colour for highlighted text

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
plot_title <- ""

subtitle_text <- ""
plot_subtitle <- paste(strwrap(subtitle_text, 150), collapse = "\n")

plot_caption <- paste0("**Data & Inspiration:** JLaw's R Blog | ", "**Graphics:** ", social_caption)

#==============================================================================#
# Data Visualization------------------------------------------------------------
#==============================================================================#

# UNESCO Sites Bar Graph
g1 <- df |> 
  filter(country %in% "sweden") |> 
  ggplot(aes(x = n_sites, y = year)) +
  geom_col(width = 0.2) +
  geom_text(aes(
      x = 0, 
      label = year),
    hjust = 1.1) +
  geom_text(aes(x = n_sites, label = n_sites),
            hjust = "inward") +
  theme_void()

g1 <- ggplotGrob(g1)

map_df |> 
  ggplot() +
  geom_sf(fill = "transparent") +
  geom_flag(
    data = df_cord,
    mapping = aes(
      x = x,
      y = y,
      image = c_code
    )
  ) +
  annotation_custom(
    grob = g1,
    xmin = 61,
    xmax = 61.5,
    ymin = 8,
    ymax = 11
  ) +
  coord_sf(
    xlim = c(4.5, 31),
    ylim = c(54.5, 70.5)
  ) +
  scale_x_continuous(breaks = 5:30) +
  scale_y_continuous(breaks = 55:70) +
  theme(axis.text.x = element_text(angle = 90))



g <- 
  ggplot() +
  
  theme_minimal() +
  theme(
  panel.grid = element_blank(),
  plot.caption =  element_textbox(family = "caption_font",
                                  hjust = 0.5,
                                  colour = text_col,
                                  size = 1.5 * ts),
  plot.title   =     element_text(hjust = 0.5,
                                  size = 3*ts,
                                  family = "title_font",
                                  face = "bold",
                                  colour = text_hil,
                                  margin = margin(4,0,2,0)),
  plot.subtitle    = element_text(hjust = 0,
                                  size = 1.6 * ts,
                                  family = "body_font",
                                  colour = text_col,
                                  margin = margin(5,0,2,0),
                                  lineheight = 0.35),
  plot.background =  element_rect(fill = bg_col,
                                  color = bg_col,
                                  linewidth = 0),
  axis.text = element_text(size = 1.5 * ts,
                           family = "body_font",
                           colour = text_col,
                           face = "bold",
                           margin = margin(0,0,0,0)),
  axis.title.x = element_text(size = 2 * ts,
                           family = "body_font",
                           colour = text_col,
                           margin = margin(0,0,0,0),
                           hjust = 1),
  axis.line.x = element_line(color = "grey",
                             arrow = arrow(length = unit(3, "mm"))),
  plot.title.position = "plot"
)

#=============================================================================#
# Image Saving-----------------------------------------------------------------
#=============================================================================#


ggsave(
  filename = here::here("docs", "tidy_nhl_Jan_2024.png"),
  plot = g,
  width = 20, 
  height = 20, 
  units = "cm",
  bg = bg_col
)
